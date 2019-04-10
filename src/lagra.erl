%% @author Hugo Mills <hugo@carfax.org.uk>

%% @doc The primary entry point for lagra.
%%
%%   The lagra module contains functions for parsing and serialising
%%   RDF documents, storing them, and manipulating them.
%%
%%   All of these functions operate on a <i>triplestore</i>. The only
%%   kind of triplestore currently implemented is the
%%   `trivial' triplestore, which is an in-memory store
%%   suitable only for small documents.

-module(lagra).

-export([create_store/1, create_store/2]).
-export([destroy_store/1]).

-export([parse/3, parse/4]).
-export([parse_incremental/4, parse_incremental/5]).
-export([serialize/3, serialize/4]).

-export([add/2]).

-export([find_all_t/2]).
-export([find_all_q/2]).

-export([isomorphic/4, isomorphic/2]).

-include_lib("eunit/include/eunit.hrl").

-type store() :: pid().
-type store_type() :: trivial.
-type node_map_elt() :: {grounded | ungrounded, binary()}.
-type node_map() :: #{lagra_model:rdfnode() => node_map_elt()}.
-type grouped_node() :: {node_map_elt(),
						 {[lagra_model:rdfnode()],
						  [lagra_model:rdfnode()]}}.
-type parser_type() :: ntriples | turtle.
-type incr_cb() :: fun (([lagra_model:triple()], any()) -> any()).

-export_type([store/0]).
-export_type([incr_cb/0]).

%% @equiv create_store(Type, #{})
%% @spec (Type :: trivial) -> store()
-spec create_store(store_type()) -> store().
create_store(trivial) ->
	create_store(trivial, #{}).

%% @doc Create a lagra triplestore.
%%
%% @param Type The type of the triplestore. Types defined are
%%             `trivial'.
%%
%% @param Options A map of options for the triplestore. For a
%%                `trivial' triplestore, no options are available, and
%%                the map should be empty..
%%
%% @returns A reference to the created store.
%%
%% @spec (Type :: trivial, Options :: map()) -> store()
-spec create_store(store_type(), map()) -> store().
create_store(trivial, _Options) ->
	{ok, Child} = supervisor:start_child(lagra_store_trivial_sup, []),
	Child.

%% @doc Destroy a lagra triplestore.
%%
%%   For in-memory stores, discards the store completely. For
%%   persistent disk-based stores, simply discards the handle to the
%%   store. The store can be accessed again by connecting to it using
%%   create_store/2.
%%
%% @param Store The store to destroy
%%
%% @returns `ok'
-spec destroy_store(store()) -> term().
destroy_store(Store) ->
	supervisor:terminate_child(lagra_store_trivial_sup, Store).

%% @equiv parse(Store, File, Parser, #{})
-spec parse(store(), file:io_device(), parser_type()) ->
				   ok | {error, term(), integer()}.
parse(Store, File, Parser) ->
	parse(Store, File, Parser, #{}).

%% @doc Parse an open file-like object into a triplestore.
%%
%%   Reads the contents of `File' into `Store', reading it using the
%%   `Parser', with `Options' passed to the parser.
%%
%% @spec (Store :: store(), File :: file:io_device(),
%%        Type :: parser_type(), Options :: map())
%%          -> ok | {error, term(), integer()}
%%
%% @param Store The lagra triplestore to put the triples in
%%
%% @param File An open file-like object
%%
%% @param Parser The parser to use. Defined parsers are `ntriples' and
%%   `turtle'.
%%
%% @param Options The options to pass to the parser. For `ntriples',
%% the options are:
%%
%% `allow_relative => boolean()': Allow parsing relative URIs. This is
%% forbidden by the NTriples standard, but may be useful for parsing
%% some kinds of input, such as test suites. Default: `false'.
%%
%% For `turtle', the options are:
%%
%% `base => string()': The base IRI to prefix relative IRIs with.
%%
%% @returns `ok | {error, atom(), location()}'
-spec parse(store(), file:io_device(), parser_type(), map()) ->
				   ok | {error, term(), integer()}.
parse(Store, File, ntriples, Options) ->
	lagra_parser_ntriples:parse(Store, File, Options);
parse(Store, File, turtle, Options) ->
	lagra_parser_turtle:parse(Store, File, Options).

%% @equiv parse_incremental(File, Type, Callback, State, #{})
-spec parse_incremental(file:io_device(), parser_type(),
						incr_cb(), any())
					   -> any().
parse_incremental(File, Type, Callback, State) ->
	parse_incremental(File, Type, Callback, State, #{}).

%% @doc Parse an open file-like object incrementally.
%%
%%   Reads the contents of `File', calling an accumulation function
%%   for each batch of triples read.
%%
%% @spec (File :: file:io_device(), Type :: parser_type(),
%%        Callback :: incr_cb(), State :: any(), Options :: map())
%%          -> any().
%%
%% @param File An open file-like object
%%
%% @param Parser The parser to use. Defined parsers are `ntriples' and
%%   `turtle'.
%%
%% @param Callback A function called when a batch of triples has been
%% parsed from the input.
%%
%% @param State The initial state passed to the first call of the `Callback'
%%
%% @param Options The options to pass to the parser. All parsers
%% support the following options:
%%
%% `batch => integer()': The maximum number of triples to return on
%% each call. Default: `1000'.
%%
%% For individual parsers' options, see the `Options' parameter of
%% parse/4.
%%
%% @returns The updated `State' value, or `{error, Err, State}'.
%%
%% The parser reads triples incrementally from the input file,
%% batching them up in a list. When the end of the input is reached,
%% or the list reaches the maximum batch size, the `Callback' function
%% is called with the current value of the `State'. The `Callback'
%% function should process the triples as appropriate, and then return
%% an updated value of `State'.
-spec parse_incremental(file:io_device(), parser_type(),
						incr_cb(), any(), map()) ->
							   any().
parse_incremental(File, ntriples, Callback, State, Options) ->
	lagra_parser_ntriples:parse_incremental(File, Callback, State, Options);
parse_incremental(File, turtle, Callback, State, Options) ->
	lagra_parser_turtle:parse_incremental(File, Callback, State, Options).

%% @equiv serialize(Store, File, Type, #{})
-spec serialize(store(), file:io_device(), atom()) ->
					   ok | {error, term()}.
serialize(Store, File, Type) ->
	serialize(Store, File, Type, #{}).

%% @doc Render a graph from a store to an open file
%%
%% @param Store The file store to use
%%
%% @param File A file open for writing to
%%
%% @param Type The type of file to render. Defined types are
%% `ntriples' and `turtle'.
%%
%% @param Options The options to pass to the serializer. Options
%%        common to all serializers are:
%%
%%   `notify => sync | async | {Proc :: atom() | pid(), Ref}`: Return
%%   when the serialization is complete (`sync`), or immediately
%%   (`async`, the default), or send a message `{serialized, Ref}` to
%%   the process `Proc` on completion.
%%
%% The `ntriples' serializer takes no additional options.
%%
%% For the `turtle' serializer, options are:
%%
%%   `prefixes => map(lagra_model:rdfnode() | binary() => unicode:string())':
%%                Map partial IRI prefixes (either as a lagra
%%                IRI, or as a binary) to abbreviated prefixes. e.g.::
%%
%%      Prefixes = #{<<"http://www.w3.org/1999/02/22-rdf-syntax-ns#">>
%%                    => <<"rdf">>}
%%
%% @returns `ok'

-spec serialize(store(), file:io_device(), parser_type(), map()) ->
					   ok | {error, term()}.
serialize(Store, File, ntriples, Options) ->
	lagra_serializer_ntriples:serialize(Store, File, Options);
serialize(Store, File, turtle, Options) ->
	lagra_serializer_turtle:serialize(Store, File, Options).

%% @doc Add a triple or quad to a triplestore.
%%
%% @param Store The triplestore to add the triple to.
%%
%% @param Triple The triple (or quad) to add.
%%
%% @returns `ok | {error, term()}'
-spec add(store(), lagra_model:triple() | lagra_model:quad()) ->
				 ok | {error, term()}.
add(Store, Triple) when tuple_size(Triple) =:= 3 ->
	gen_server:call(Store, {add_triple, Triple});
add(Store, Quad) when tuple_size(Quad) =:= 4 ->
	gen_server:call(Store, {add_quad, Quad}).

%% -spec find_q(store(), lagra_model:pattern()) ->
%% 					partial_result(lagra_model:quad()).
%% -spec find_t(store(), lagra_model:pattern()) ->
%% 					partial_result(lagra_model:triple()).

%% @doc Return all quads in a triplestore matching a specific pattern.
%%
%% @param Store The triplestore to search.
%%
%% @param Pattern A triple or quad pattern to match.
%%
%%   Patterns are a 3-tuple or 4-tuple, containing either an exact RDF
%%   node to match on, or the atom ``'_''' as a wildcard. A 3-tuple
%%   pattern will match triples in any graph context within the
%%   triplestore.
-spec find_all_q(store(), lagra_model:pattern()) -> [lagra_model:quad()].
find_all_q(Store, Pattern) when tuple_size(Pattern) =:= 4 ->
	gen_server:call(Store, {find_all_q, Pattern});
find_all_q(Store, {Sp, Pp, Op}) ->
	find_all_q(Store, {Sp, Pp, Op, '_'}).

%% @doc Return all quads in a triplestore matching a specific pattern.
%%
%% @param Store The triplestore to search.
%%
%% @param Pattern A triple or quad pattern to match.
%%
%%   Patterns are a 3-tuple or 4-tuple, containing either an exact RDF
%%   node to match on, or the atom ``'_''' as a wildcard. A 3-tuple
%%   pattern will match triples in any graph context within the
%%   triplestore.
-spec find_all_t(store(), lagra_model:pattern()) -> [lagra_model:triple()].
find_all_t(Store, Pattern) when tuple_size(Pattern) =:= 4 ->
	gen_server:call(Store, {find_all_t, Pattern});
find_all_t(Store, {Sp, Pp, Op}) ->
	find_all_t(Store, {Sp, Pp, Op, '_'}).

%% @doc Test whether Graph1 in Store1 is isomorphic to Graph2 in Store2
%%
%% @param Store1 The first triplestore to compare
%%
%% @param Graph1 The IRI of the graph in the first triplestore to compare
%%
%% @param Store2 The second triplestore to compare
%%
%% @param Graph2 The IRI of the graph in the second triplestore to compare
%%
%% @returns `true | false'
-spec isomorphic(store(), lagra_model:graph(), store(), lagra_model:graph())
				-> true | false.
isomorphic(Store1, Graph1, Store2, Graph2) ->
	T1 = find_all_t(Store1, {'_', '_', '_', Graph1}),
	T2 = find_all_t(Store2, {'_', '_', '_', Graph2}),
	isomorphic(T1, T2).

%% @doc Test whether two lists of triples are isomorphic to each other.
-spec isomorphic([lagra_model:triple()], [lagra_model:triple()])
				   -> true | false.
isomorphic(T1, T2) when length(T1) =/= length(T2) ->
	% If the lists of triples are different sizes, we can fail immediately
	false;
isomorphic(T1, T2) ->
	NonB1 = [T || T <- T1, not is_blank_triple(T)],
	NonB2 = [T || T <- T2, not is_blank_triple(T)],

	BaseMap1 = lists:foldl(fun build_base_node_map/2, #{}, T1),
	BaseMap2 = lists:foldl(fun build_base_node_map/2, #{}, T2),

	length(NonB1) =:= length(NonB2)
		andalso lists:sort(NonB1) =:= lists:sort(NonB2)
		andalso is_isomorphic_to_bnodes(
				  T1, update_node_map(lists:sort(T1), BaseMap1),
				  T2, update_node_map(lists:sort(T2), BaseMap2)).

%%% The algorithm:

%%% Initial, trivial checks:
%%%  - do we have the same number of triples in the graph?
%%%  - do we have the same number of fully-grounded triples in the graph,
%%%      and are they the same set?
%%%
%%% Build a node map of node -> {Grounded, Signature} for each graph
%%% If all nodes are grounded, succeed
%%% Collect together signature -> nodes for each ungrounded node in each graph
%%% Join the signature -> nodes maps for both graphs {Sig, [Nodes1], [Nodes2]}
%%% If any joined record has one node on each side, ground the nodes:
%%%    Generate a unique ID, set it to that in both node maps
%%% otherwise, try all the combinations of nodes from each side, grounding them
%%%    and recursing in
%%% If no pair results in a success from the recursion, fail

%%% A signature is:
%%%  - For a non-blank node, the node itself
%%%  - For a grounded blank node, a randomly-selected large number
%%%  - For an ungrounded blank node, the concatenation of the hashes of the
%%%    triples in sort order
%%%  - For a triple, the hash of all the grounded components of the triple
%%%
%%% Hashes of grounded nodes don't change after grounding

-spec is_isomorphic_to_bnodes([lagra_model:triple()], node_map(),
							  [lagra_model:triple()], node_map()) ->
									 boolean().
is_isomorphic_to_bnodes(T1, M1, T2, M2) ->
	Groups = group_node_maps(M1, M2),
	%% Abort if any group doesn't contain the same number of nodes
	%% from each graph.
	case lists:any(fun ({_H, {G1, G2}}) -> length(G1) =/= length(G2) end,
				   Groups)
	of
		true ->
			%% There's at least one group where the number of nodes is
			%% different: halt right now
			false;
		false ->
			%% Discard the already grounded and paired nodes
			Ungrounded = [G || {{ungrounded, _}, _}=G <- Groups],
			process_ungrounded_groups(Ungrounded, T1, M1, T2, M2)
	end.

process_ungrounded_groups([], _T1, _M1, _T2, _M2) ->
	%% No ungrounded nodes -- we must have finished, and can succeed
	true;
process_ungrounded_groups(Ungrounded, T1, M1, T2, M2) ->
	%% We need to pick a grounded and unpaired group, iterate through
	%% all the combinations and see if one succeeded.
	Trials = lists:append(
			   [[{H, {[N1], [N2]}} || N1 <- NL1, N2 <- NL2]
				|| {H, {NL1, NL2}} <- Ungrounded]),
	lists:any(fun (Trial) -> has_solution(Trial, T1, M1, T2, M2) end,
			  Trials).

-spec has_solution(grouped_node(),
				   [lagra_model:triple()], node_map(),
				   [lagra_model:triple()], node_map()) ->
						  boolean().
has_solution(Pair, T1, M1, T2, M2) ->
	{GM1, GM2, _} = maybe_ground_pair(Pair, {M1, M2, false}),
	ReGM1 = update_node_map(T1, GM1),
	ReGM2 = update_node_map(T2, GM2),
	is_isomorphic_to_bnodes(T1, ReGM1, T2, ReGM2).

%%% Given a hash-nodes-nodes group and a pair of maps, update the maps
%%% and return them, but only if the group contains precisely one node
%%% in each graph. The updated map contains the node => value for a
%%% randomly-selected value of size similar to the hashes. We don't
%%% need to use strong_rand_bytes/1, but it's an easy way of
%%% generating the binary we want.
-spec maybe_ground_pair(grouped_node(),
						{node_map(), node_map(), boolean()}) ->
							   {node_map(), node_map(), boolean()}.
maybe_ground_pair({_, {[N1], [N2]}}, {M1, M2, _}) ->
	Hash = crypto:strong_rand_bytes(32),
	{M1#{N1 := {grounded, Hash}},
	 M2#{N2 := {grounded, Hash}},
	 true};
maybe_ground_pair({_, {_, _}}, {_, _, _} = Acc) ->
	Acc.

%%% Building the base node map: blank nodes are all given the same
%%% (trivial) signature; non-blank nodes are hashed and grounded.
-spec build_base_node_map(lagra_model:triple(), node_map()) -> node_map().
build_base_node_map({S, P, O}, Acc) ->
	Acc2 = build_base_node_map_node(S, Acc),
	Acc3 = build_base_node_map_node(P, Acc2),
	build_base_node_map_node(O, Acc3).

-spec build_base_node_map_node(lagra_model:rdfnode(), node_map()) -> node_map().
build_base_node_map_node({bnode, _}=N, Acc) ->
	Acc#{N => {ungrounded, <<0>>}};
build_base_node_map_node(N, Acc) ->
	H = crypto:hash_final(
		  crypto:hash_update(
			crypto:hash_init(sha256),
			term_to_binary(N))),
	Acc#{N => {grounded, H}}.

%%% Updating a node map: Given a list of triples and a set of current
%%% node hashes, return an updated set of node hashes
-spec update_node_map([lagra_model:triple()], node_map()) -> node_map().
update_node_map(Triples, NodeMap) ->
	NodeTriples = lists:foldl(
					fun (T, Acc) ->
							build_node_triples_list(T, Acc, NodeMap)
					end,
					#{},
					Triples),
	maps:map(fun (N, Ts) -> node_signature(N, Ts, NodeMap) end,
			 NodeTriples).

-spec build_node_triples_list(lagra_model:triple(), map(), node_map()) -> map().
build_node_triples_list({S, P, O}=T, Acc, NodeMap) ->
	Acc2 = update_triples_list(S, T, Acc, NodeMap),
	Acc3 = update_triples_list(P, T, Acc2, NodeMap),
	update_triples_list(O, T, Acc3, NodeMap).

-spec update_triples_list(lagra_model:rdfnode(), lagra_model:triple(),
						  map(), node_map()) ->
								 map().
update_triples_list(N, {S, P, O}, Acc, NodeMap) ->
	List = maps:get(N, Acc, []),
	HashedTriple = {maps:get(S, NodeMap),
					maps:get(P, NodeMap),
					maps:get(O, NodeMap)},
	Acc#{N => [HashedTriple|List]}.

-spec node_signature(lagra_model:rdfnode(),
					 [lagra_model:triple()],
					 node_map()) ->
							node_map_elt().
node_signature(Node, Triples, NodeMap) ->
	case maps:get(Node, NodeMap) of
		{grounded, H} ->
			{grounded, H};
		{ungrounded, _} ->
			SortedTs = [term_to_binary(T) || T <- lists:sort(Triples)],
			HCtx = crypto:hash_init(sha256),
			HCtx2 = lists:foldl(
					  fun (Data, Ctx) -> crypto:hash_update(Ctx, Data) end,
					  HCtx,
					  SortedTs),
			H = crypto:hash_final(HCtx2),
			{ungrounded, H}
	end.

%%% Group the nodes together by hash: collect together all the nodes
%%% in the first graph with the same hash, and simultaneously the
%%% nodes in the second graph with that same hash.
-spec group_node_maps(node_map(), node_map()) -> [grouped_node()].
group_node_maps(M1, M2) ->
	Res1 = maps:fold(fun update_node_group_left/3, #{}, M1),
	Res2 = maps:fold(fun update_node_group_right/3, Res1, M2),
	maps:to_list(Res2).

-spec update_node_group_left(lagra_model:rdfnode(), node_map_elt(), node_map())
							-> map().
update_node_group_left(K, V, Acc) ->
	maps:update_with(
	  V,
	  fun ({HL1, HL2}) -> {[K|HL1], HL2} end,
	  {[K], []},
	  Acc).

-spec update_node_group_right(lagra_model:rdfnode(), node_map_elt(), node_map())
							 -> map().
update_node_group_right(K, V, Acc) ->
	maps:update_with(
	  V,
	  fun ({HL1, HL2}) -> {HL1, [K|HL2]} end,
	  {[], [K]},
	  Acc).

-spec is_blank_triple(lagra_model:triple()) -> boolean().
is_blank_triple({{bnode, _}, _, _}) -> true;
is_blank_triple({_, {bnode, _}, _}) -> true;
is_blank_triple({_, _, {bnode, _}}) -> true;
is_blank_triple(_) -> false.
