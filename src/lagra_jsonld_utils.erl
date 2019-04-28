%% @private
-module(lagra_jsonld_utils).
-export([jsonld_object_compare/2, jsonld_object_compare/3]).

jsonld_object_compare(A, B) ->
	%% This function is pathological, and will only work properly is
	%% there is a _very_ small number of bnodes in the JSON-LD objects
	%% to be compared. This is OK, as it's not part of the public
	%% interface, and is only used in the test suite.
	jsonld_object_compare(A, B, false).

jsonld_object_compare(A, B, Ordered) ->
	Abnodes = sets:to_list(get_bnodes(A)),
	Bbnodes = sets:to_list(get_bnodes(B)),
	%io:format("compare ~n~p~n~p~n", [Abnodes, Bbnodes]),
	case {length(Abnodes), length(Bbnodes)} of
		{0, 0} ->
			jsonld_object_compare_impl(A, B, Ordered);
		{N, N} ->
			lists:any(fun (P) ->
							  BMap = maps:from_list(lists:zip(Abnodes, P)),
							  %io:format("BMap ~p~n", [BMap]),
							  jsonld_object_compare_impl(map_bnodes(A, BMap), B,
														 Ordered)
					  end,
					  perms(Bbnodes));
		{N1, N2} when N1 =/= N2 ->
			false
	end.


jsonld_object_compare_impl(A, B, _)
  when is_list(A), is_list(B), length(A) =/= length(B) ->
	%io:format("cmp1 ~p~n   = ~p~n", [A, B]),
	false;
jsonld_object_compare_impl(A, B, true)
  when is_list(A), is_list(B), length(A) =:= length(B) ->
	%io:format("cmp2 ~p~n   = ~p~n", [A, B]),
	lists:all(fun ({Ax, Bx}) ->
					  jsonld_object_compare_impl(Ax, Bx, true)
			  end,
			  lists:zip(A, B));
jsonld_object_compare_impl(A, B, false)
  when is_list(A), is_list(B), length(A) =:= length(B) ->
	%io:format("cmp3 ~p~n   = ~p~n", [A, B]),
	As = lists:sort(A),
	Bs = lists:sort(B),
	lists:all(fun ({Ax, Bx}) ->
					  jsonld_object_compare_impl(Ax, Bx, false)
			  end,
			  lists:zip(As, Bs));
jsonld_object_compare_impl(A, B, Ordered)
  when is_map(A), is_map(B), map_size(A) =:= map_size(B) ->
	%io:format("cmp4 ~p~n   = ~p~n", [A, B]),
	Aks = lists:sort(maps:keys(A)),
	Bks = lists:sort(maps:keys(B)),
	Aks =:= Bks
		andalso lists:all(
				  fun (<<"@list">> = K) ->
						  jsonld_object_compare_impl(
							maps:get(K, A), maps:get(K, B),
							true);
					  (K) ->
						  jsonld_object_compare_impl(
							maps:get(K, A), maps:get(K, B),
							Ordered)
				  end,
				  Aks);
jsonld_object_compare_impl(A, B, _) ->
	%io:format("cmp6 ~p~n   = ~p~n", [A, B]),
	A =:= B.


get_bnodes(J) when is_list(J) ->
	sets:union([get_bnodes(X) || X <- J]);
get_bnodes(J = #{<<"@id">> := <<"_:", _/binary>> = Id}) when is_map(J) ->
	sets:add_element(
	  Id,
	  sets:union([get_bnodes(X) || X <- maps:values(J)]));
get_bnodes(J) when is_map(J) ->
	sets:union([get_bnodes(X) || X <- maps:values(J)]);
get_bnodes(J = <<"_:", _/binary>>) ->
	sets:from_list([J]);
get_bnodes(_J) ->
	sets:new().


map_bnodes(J, Map) when is_list(J) ->
	[map_bnodes(X, Map) || X <- J];
map_bnodes(J, Map) when is_map(J) ->
	maps:from_list(
	  [{map_bnodes(K, Map), map_bnodes(V, Map)} || {K, V} <- maps:to_list(J)]);
map_bnodes(J, Map) ->
	case maps:get(J, Map, none) of
		none ->
			J;
		V ->
			V
	end.


perms([]) ->
	[];
perms(List) ->
	lists:append([prefix(X, List) || X <- List]).

prefix(X, [X]) ->
	[[X]];
prefix(X, List) ->
	Filtered = [Y || Y <- List,
					 Y =/= X],
	SubPerms = perms(Filtered),
	[[X|P] || P <- SubPerms].
