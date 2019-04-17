-module(lagra_jsonld_utils).
-export([jsonld_object_compare/2, jsonld_object_compare/3]).

jsonld_object_compare(A, B) ->
	jsonld_object_compare(A, B, false).

jsonld_object_compare(A, B, true)
  when is_list(A), is_list(B), length(A) =:= length(B) ->
	lists:all(fun ({Ax, Bx}) -> jsonld_object_compare(Ax, Bx, true) end,
			  lists:zip(A, B));
jsonld_object_compare(A, B, false)
  when is_list(A), is_list(B), length(A) =:= length(B) ->
	As = lists:sort(A),
	Bs = lists:sort(B),
	lists:all(fun ({Ax, Bx}) -> jsonld_object_compare(Ax, Bx, false) end,
			  lists:zip(As, Bs));
jsonld_object_compare(A, B, Ordered)
  when is_map(A), is_map(B), map_size(A) =:= map_size(B) ->
	Aks = lists:sort(maps:keys(A)),
	Bks = lists:sort(maps:keys(B)),
	Aks =:= Bks
		andalso lists:all(
				  fun (K) -> jsonld_object_compare(
							   maps:get(K, A),
							   maps:get(K, B),
							   Ordered) end,
				  Aks);
jsonld_object_compare(A, B, _) ->
	A =:= B.
