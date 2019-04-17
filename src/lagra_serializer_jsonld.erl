-module(lagra_serializer_jsonld).
-export([serialize/3, serialize_impl/3]).
-export_type([jsonld_wr_opts/0]).
-include_lib("lagra/include/namespaces.hrl").

-type jsonld_wr_opts() :: #{notify => async | sync | {pid(), any()},
							context => jsx:json_term(),
							graph => lagra_model:iri(),
							native_types => boolean(),
							rdf_type => boolean(),
							ordered => boolean()}.

-spec serialize(lagra:store(), file:io_device(), map()) -> ok.
serialize(Store, File, #{notify := async} = Options) ->
	spawn_link(?MODULE, serialize_impl, [Store, File, Options]),
	ok;
serialize(Store, File, #{notify := sync} = Options) ->
	serialize_impl(Store, File, Options);
serialize(Store, File, #{notify := {Caller, Id}} = Options) ->
	spawn_link(fun (_) ->
					   serialize_impl(Store, File, Options),
					   Caller ! {serialized, Id}
			   end),
	ok;
serialize(Store, File, Options) ->
	serialize(Store, File, Options#{notify => sync}).

-spec serialize_impl(lagra:store(), file:io_device(), map()) -> ok.
serialize_impl(Store, _File, Options) ->
	Graph = maps:get(graph, Options, '_'),
	DefOptions = maps:merge(#{native_types => false,
							  rdf_type => false,
							  ordered => false},
							Options),
	Quads = lists:sort(lagra:find_all_q(Store, {'_', '_', '_', Graph})),
	%% Implement section 8.4 of https://www.w3.org/TR/json-ld11-api/
	%% Step 4: Build the graph map, which maps graph IRIs to that
	%% graph's node map.
	GraphGroups = lists:foldl(fun group_graphs/2, #{}, Quads),
	GraphMap1 = maps:map(fun (G, N) -> build_node_map(G, N, DefOptions) end,
						 GraphGroups),
	GraphMap = GraphMap1#{<<"@default">> => add_graph_nodes(GraphMap1)},
	%% Step 5: Collect the RDF lists together into actual lists.
	ReferencedOnce = build_referenced_once(GraphMap),
	ListedGraphMap = maps:map(
					   fun (Id, G) ->
							   collapse_rdf_lists(Id, G, ReferencedOnce)
					   end,
					   GraphMap),
	%% Convert our sets to lists
	FinalGraphMap = maps:map(fun sets_to_lists/2, ListedGraphMap),
	%% Steps 6-7: Apply graph identities and build the output graph
	io:format("Final graph map ~p~n", [FinalGraphMap]),
	DefaultGraph = maps:get(<<"@default">>, FinalGraphMap, #{}),
	Sorter = case DefOptions of
				 #{ordered := true} -> fun lists:sort/1;
				 #{ordered := false} -> fun (X) -> X end
			 end,
	lists:foldl(fun (X, Acc) -> build_output(X, FinalGraphMap, Acc) end,
				[],
				Sorter(maps:to_list(DefaultGraph))).

-spec group_graphs(lagra_model:quad(),
				   #{binary() => [lagra_model:quad()]}) ->
						  #{binary() => [lagra_model:quad()]}.
group_graphs(Qd = {_, _, _, {iri, <<"urn:nil">>}}, Acc) ->
	GraphQuads = maps:get(<<"@default">>, Acc, []),
	Acc#{<<"@default">> => [Qd|GraphQuads]};
group_graphs(Qd = {_, _, _, {iri, Graph}}, Acc) ->
	GraphQuads = maps:get(Graph, Acc, []),
	Acc#{Graph => [Qd|GraphQuads]}.

-spec build_node_map(binary(), [lagra_model:quad()], map()) ->
							#{binary() => #{binary() => any()}}.
build_node_map(_GraphName, Quads, Options) ->
	%% For each S, P, O in the Quads list, turn the S and (optionally)
	%% O into entries in the node map.
	%% The S node gets given entries for the P, O
	%% The O node, if non-literal, gets given a "usages" member, which
	%% contains S, P, O
	AllFragments = [build_node_fragment(Q, Options) || Q <- Quads],
	NodeMap = lists:foldl(fun merge_node_fragment/2,
						  #{}, AllFragments),
	NodeMap.

build_node_fragment({S, {iri, ?RDF("type")}, O = {OTyp, _}, _G}, Options)
  when OTyp =:= iri;
	   OTyp =:= bnode ->
	%% Using rdf:type as the predicate: append to @type element of
	%% rdf_type is false
	SIri = maybe_bnode(S),
	OIri = maybe_bnode(O),
	case Options of
		#{rdf_type := true} ->
			#{SIri => #{<<"@id">> => SIri,
						?RDF("type") => sets:from_list([#{<<"@id">> => OIri}])},
			  OIri => #{<<"@id">> => OIri}};
		_ ->
			#{SIri => #{<<"@id">> => SIri,
						<<"@type">> => sets:from_list([OIri])},
			  OIri => #{<<"@id">> => OIri}}
	end;
build_node_fragment({S, {iri, PIri}, {iri, ?RDF("nil")}, _G}, _Options) ->
	%% object is rdf:nil: end of a list; add to usages list
	SIri = maybe_bnode(S),
	#{SIri => #{<<"@id">> => SIri,
				PIri => sets:from_list([#{<<"@id">> => ?RDF("nil")}])},
	  ?RDF("nil") => #{<<"@id">> => ?RDF("nil"),
					   usages => sets:from_list([{SIri, PIri, ?RDF("nil")}])}};
build_node_fragment({S, {iri, PIri}, O = {bnode, _}, _G}, _Options) ->
	%% object is a bnode: may be part of a list; add to usages list
	SIri = maybe_bnode(S),
	OIri = maybe_bnode(O),
	#{SIri => #{<<"@id">> => SIri,
				PIri => sets:from_list([#{<<"@id">> => OIri}])},
	  OIri => #{<<"@id">> => OIri,
				usages => sets:from_list([{SIri, PIri, OIri}])}};
build_node_fragment({S, {iri, PIri}, {iri, OIri}, _G}, _Options) ->
	%% object is an IRI: process normally
	SIri = maybe_bnode(S),
	#{SIri => #{<<"@id">> => SIri,
				PIri => sets:from_list([#{<<"@id">> => OIri}])},
	  OIri => #{<<"@id">> => OIri}};
build_node_fragment({S, {iri, PIri}, O, _G},
					#{native_types := NativeTypes}) ->
	%% object is a literal: process normally
	SIri = maybe_bnode(S),
	#{SIri => #{<<"@id">> => SIri,
				PIri => sets:from_list([value_of(O, NativeTypes)])}}.

merge_node_fragment(Frag, Acc) ->
	maps:fold(fun merge_node/3, Acc, Frag).

merge_node(K, Node, Acc) ->
	case Acc of
		#{K := AccNode} ->
			NewNode = maps:fold(fun merge_node_elt/3, AccNode, Node),
			Acc#{K => NewNode};
		_ ->
			Acc#{K => Node}
	end.

merge_node_elt(<<"@id">>, Id, Node = #{<<"@id">> := Id}) ->
	Node;
merge_node_elt(<<"@id">>, Id, Node) ->
	Node#{<<"@id">> => Id};
merge_node_elt(Pred, Set, Node) ->
	case Node of
		#{Pred := AccSet} ->
			Node#{Pred => sets:union(Set, AccSet)};
		_ ->
			Node#{Pred => Set}
	end.

add_graph_nodes(GraphMap = #{<<"@default">> := DefGraph}) ->
	maps:fold(fun add_graph_node/3, DefGraph, GraphMap);
add_graph_nodes(GraphMap) ->
	add_graph_nodes(GraphMap#{<<"@default">> => #{}}).

add_graph_node(<<"@default">>, _G, DefGraph) ->
	DefGraph;
add_graph_node(Id, _G, DefGraph) ->
	case DefGraph of
		#{Id := _} ->
			DefGraph;
		_ ->
			DefGraph#{Id => #{<<"@id">> => Id}}
	end.


collapse_rdf_lists(_G, NodeMap = #{?RDF("nil") := #{usages := Usages}},
				   ReferencedOnce) ->
	io:format("NodeMap: ~p~nUsages: ~p~nReferencedOnce: ~p~n",
			  [NodeMap, sets:to_list(Usages), ReferencedOnce]),
	sets:fold(fun (X, Acc) ->
					  maybe_collapse_rdf_list(X, Acc, ReferencedOnce, [])
			  end,
			  NodeMap,
			  Usages);
collapse_rdf_lists(_G, NodeMap, _ReferencedOnce) ->
	NodeMap.

maybe_collapse_rdf_list(Triple = {<<"_:", _/binary>> = S, ?RDF("rest"), O},
						NodeMap, ReferencedOnce, MaybeList) ->
	io:format("collapse triple (bnode): ~p~n", [Triple]),
	#{S := SNode} = NodeMap,
	#{?RDF("first") := DataSet,
	  ?RDF("rest") := RestSet} = SNode,
	OtherKeys = [K || K <- maps:keys(SNode),
					  K =/= <<"@id">>, K =/= <<"@type">>, K =/= usages,
					  K =/= ?RDF("first"), K =/= ?RDF("rest")],
	Types = maps:get(<<"@type">>, SNode, sets:new()),
	io:format("first: ~p~nrest: ~p~notherkeys: ~p~ntypes ~p~n",
			  [sets:to_list(DataSet),
			   sets:to_list(RestSet),
			   OtherKeys, Types]),
	case sets:size(RestSet) =:= 1
		andalso sets:size(DataSet) =:= 1
		andalso length(OtherKeys) =:= 0
		andalso maps:is_key(S, ReferencedOnce)
		andalso maps:is_key(O, NodeMap)
		andalso sets:is_subset(Types, sets:from_list([?RDF("List")]))
	of
		true ->
			Next = maps:get(S, ReferencedOnce),
			[Data] = sets:to_list(DataSet),
			NewList = [Data | MaybeList],
			NodeMap1 = remove_node_map_triple(
						 NodeMap, Triple),
			NodeMap2 = remove_node_map_triple(
						 NodeMap1, {S, ?RDF("first"), Data}),
			NodeMap3 = remove_node_map_triple_unsafe(
						 NodeMap2, {S, <<"@type">>, ?RDF("List")}),
			io:format("new list: ~p~n", [NewList]),
			io:format("new node map: ~p~n", [NodeMap3]),
			maybe_collapse_rdf_list(Next, NodeMap3, ReferencedOnce, NewList);
		false ->
			insert_rdf_list(Triple, MaybeList, NodeMap)
	end;
maybe_collapse_rdf_list(Triple, NodeMap, _ReferencedOnce,
						MaybeList) ->
	io:format("collapse triple: ~p~n", [Triple]),
	insert_rdf_list(Triple, MaybeList, NodeMap).

remove_node_map_triple(NodeMap, {S, P, O})
  when not is_map(O) ->
	remove_node_map_triple_unsafe(NodeMap, {S, P, #{<<"@id">> => O}});
remove_node_map_triple(NodeMap, {S, P, O}) ->
	remove_node_map_triple_unsafe(NodeMap, {S, P, O}).

remove_node_map_triple_unsafe(NodeMap, {S, P, O}) ->
	case NodeMap of
		#{S := SNode = #{P := OSet}} ->
			io:format("Removing ~p~n", [{S, P, O}]),
			NewOSet = sets:del_element(O, OSet),
			NewSNode = case sets:size(NewOSet) of
						   0 ->
							   maps:remove(P, SNode);
						   _ ->
							   SNode#{P => NewOSet}
					   end,
			io:format("New SNode: ~p~n", [NewSNode]),
			NodeMap#{S => NewSNode};
		_ ->
			io:format("Triple not found ~p~n", [{S, P, O}]),
			NodeMap
	end.

insert_rdf_list({S, P, O}, List, NodeMap) ->
	%% Replace the triple {S, P, O} with {S, P, List} in the NodeMap
	#{S := SNode} = NodeMap,
	#{P := PSet} = SNode,
	PSet1 = sets:del_element(#{<<"@id">> => O}, PSet),
	PSet2 = sets:add_element(#{<<"@list">> => List}, PSet1),
	NewSNode = SNode#{P => PSet2},
	io:format("Inserted RDF list ~p~n   ~p~n   ~p~n", [S, List, NewSNode]),
	NodeMap#{S => NewSNode}.


build_referenced_once(GraphMap) ->
	BaseMap = maps:fold(fun build_referenced_once_graph/3, #{}, GraphMap),
	OnceMap = maps:filter(fun (_K, V) -> length(V) =:= 1 end, BaseMap),
	maps:map(fun (_K, [V]) -> V end, OnceMap).

build_referenced_once_graph(_Id, NodeMap, Acc) ->
	maps:fold(fun build_referenced_once_node/3, Acc, NodeMap).

build_referenced_once_node(_Id, #{usages := Usages}, Acc) ->
	sets:fold(fun map_append/2, Acc, Usages);
build_referenced_once_node(_Id, _Node, Acc) ->
	Acc.

map_append(T = {_S, _P, O}, Acc) ->
	List = maps:get(O, Acc, []),
	Acc#{O => [T|List]}.


sets_to_lists(_K, V) ->
	maps:map(fun sets_to_lists_nodemap/2, V).

sets_to_lists_nodemap(_K, V) ->
	maps:map(fun sets_to_lists_node/2, V).

sets_to_lists_node(_K, V) ->
	case sets:is_set(V) of
		true ->
			sets:to_list(V);
		false ->
			V
	end.


build_output({Subj, Node0}, GraphMap, Acc) ->
	Node1 =	case GraphMap of
				#{Subj := Graph} ->
					GraphOutput = lists:foldl(
									fun build_output_graph/2,
									[],
									lists:sort(maps:to_list(Graph))),
					Node0#{<<"@graph">> => lists:reverse(GraphOutput)};
				_ ->
					Node0
			end,
	append_if_nontrivial_node(maps:remove(usages, Node1), Acc).
	
build_output_graph({_S, N0}, Acc) ->
	N1 = maps:remove(usages, N0),
	append_if_nontrivial_node(N1, Acc).

append_if_nontrivial_node(N = #{<<"@id">> := _}, Acc)
  when map_size(N) =:= 1 ->
	Acc;
append_if_nontrivial_node(N, Acc) ->
	[N | Acc].


maybe_bnode({iri, Iri}) ->
	Iri;
maybe_bnode({bnode, Id}) ->
	<<"_:", Id/binary>>.

%% Implements §8.5 of https://www.w3.org/TR/json-ld11-api/
value_of({iri, Iri}, _) ->
	#{<<"@id">> => Iri};
value_of({bnode, Id}, _) ->
	#{<<"@id">> => <<"_:", Id/binary>>};

value_of({literal, {string, Text}}, _) ->
	#{<<"@value">> => Text};
value_of({literal, {string, Text, Lang}}, _) ->
	#{<<"@value">> => Text, <<"@language">> => Lang};

value_of({literal, {typed, Value, ?XSD("boolean")}}, true)
  when Value =:= true;
	   Value =:= false ->
	#{<<"@value">> => Value};
value_of({literal, {typed, Value, ?XSD("boolean")}}, true) ->
	#{<<"@value">> => Value, <<"@type">> => ?XSD("boolean")};

value_of({literal, {typed, Value, ?XSD(Type/binary)}}, true)
  when Type =:= <<"integer">>;
	   Type =:= <<"double">> ->
	#{<<"@value">> => Value};

value_of({literal, {typed, Value, LongType = ?XSD(Type/binary)}}, false)
  when Type =:= <<"integer">>; Type =:= <<"nonPositiveInteger">>;
	   Type =:= <<"negativeInteger">>; Type =:= <<"long">>; Type =:= <<"int">>;
	   Type =:= <<"short">>; Type =:= <<"byte">>;
	   Type =:= <<"nonNegativeInteger">>; Type =:= <<"unsignedLong">>;
	   Type =:= <<"unsignedInt">>; Type =:= <<"unsignedShort">>;
	   Type =:= <<"unsignedByte">>; Type =:= <<"positiveInteger">> ->
	#{<<"@value">> => integer_to_binary(Value), <<"@type">> => LongType};
value_of({literal, {typed, Value, LongType = ?XSD(Type/binary)}}, false)
  when Type =:= <<"float">>;
	   Type =:= <<"double">> ->
	#{<<"@value">> => double_to_canonical_binary(Value),
	  <<"@type">> => LongType};
value_of({literal, {typed, Value, LongType = ?XSD(Type/binary)}}, false)
  when Type =:= <<"decimal">> ->
	#{<<"@value">> => float_to_binary(Value, [{decimals, 15}, compact]),
	  <<"@type">> => LongType};
value_of({literal, {typed, Value, ?XSD("boolean")}}, false) ->
	#{<<"@value">> => unicode:characters_to_binary(atom_to_list(Value)),
	  <<"@type">> => ?XSD("boolean")};

value_of({literal, {typed, Value, Type}}, _) when is_float(Value) ->
	#{<<"@value">> => float_to_binary(Value, [{decimals, 15}, compact]),
	  <<"@type">> => Type};

value_of({literal, {typed, Value, Type}}, _) ->
	#{<<"@value">> => Value, <<"@type">> => Type}.
%% FIXME: This should be the lexical representation, not the term directly

double_to_canonical_binary(Value) ->
	Txt1 = float_to_binary(Value, [{scientific, 15}]),
	%% Remove trailing 0s on mantissa and + in exponent
	Txt2 = re:replace(Txt1, "(\\d)0*e\\+?", "\\1E"),
	%% Remove leading 0s on exponent
	Txt3 = re:replace(Txt2, "(E\\-?)0*", "\\1"),
	unicode:characters_to_binary(Txt3).
