-module(lagra).

-export([create_store/1, create_store/2]).
-export([destroy_store/1]).

-export([parse/3, parse/4]).

-export([add/2]).

-export([find_all_t/2]).
-export([find_all_q/2]).

-export([isomorphic/4, isomorphic/2]).

-include_lib("eunit/include/eunit.hrl").

-type store() :: pid().
-type node_map_elt() :: {grounded | ungrounded, binary()}.
-type node_map() :: #{lagra_model:rdfnode() => node_map_elt()}.
-type grouped_node() :: {node_map_elt(),
						 {[lagra_model:rdfnode()],
						  [lagra_model:rdfnode()]}}.

-export_type([store/0]).

-spec create_store(atom()) -> store().
create_store(trivial) ->
	create_store(trivial, []).

-spec create_store(atom(), proplists:proplist()) -> store().
create_store(trivial, _Options) ->
	{ok, Child} = supervisor:start_child(lagra_store_trivial_sup, []),
	Child.

-spec destroy_store(store()) -> term().
destroy_store(Store) ->
	supervisor:terminate_child(lagra_store_trivial_sup, Store).

-spec parse(store(), file:io_device(), atom(), proplists:proplist())
		   -> ok | {error, term(), integer()}.
parse(Store, File, ntriples, Options) ->
	lagra_parser_ntriples:parse(Store, File, Options).

-spec parse(store(), file:io_device(), atom())
		   -> ok | {error, term(), integer()}.
parse(Store, File, Parser) ->
	parse(Store, File, Parser, []).

-spec add(store(), lagra_model:triple() | lagra_model:quad())
		 -> ok | {error, term()}.
add(Store, Triple) when tuple_size(Triple) =:= 3 ->
	gen_server:call(Store, {add_triple, Triple});
add(Store, Quad) when tuple_size(Quad) =:= 4 ->
	gen_server:call(Store, {add_quad, Quad}).

%% -spec find_q(store(), lagra_model:pattern()) ->
%% 					partial_result(lagra_model:quad()).
%% -spec find_t(store(), lagra_model:pattern()) ->
%% 					partial_result(lagra_model:triple()).

-spec find_all_q(store(), lagra_model:pattern()) -> [lagra_model:quad()].
find_all_q(Store, {Sp, Pp, Op}) ->
	find_all_q(Store, {Sp, Pp, Op, '_'});
find_all_q(Store, Pattern) when tuple_size(Pattern) =:= 4->
	gen_server:call(Store, {find_all_q, Pattern}).

-spec find_all_t(store(), lagra_model:pattern()) -> [lagra_model:triple()].
find_all_t(Store, {Sp, Pp, Op}) ->
	find_all_t(Store, {Sp, Pp, Op, '_'});
find_all_t(Store, Pattern) when tuple_size(Pattern) =:= 4->
	gen_server:call(Store, {find_all_t, Pattern}).

%%% Test whether Graph1 from Store1 is isomorphic to Graph2 from Store2
-spec isomorphic(store(), lagra_model:graph(), store(), lagra_model:graph())
				-> true | false.
isomorphic(Store1, Graph1, Store2, Graph2) ->
	T1 = find_all_t(Store1, {'_', '_', '_', Graph1}),
	T2 = find_all_t(Store2, {'_', '_', '_', Graph2}),
	isomorphic(T1, T2).

%%% Test whether two lists of triples are isomorphic to each other
-spec isomorphic([lagra_model:triple()], [lagra_model:triple()])
				   -> true | false.
isomorphic(T1, T2) when length(T1) =/= length(T2) ->
	% If the lists of triples are different sizes, we can fail immediately
	io:format("Different sized graphs~n"),
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
