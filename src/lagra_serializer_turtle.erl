-module(lagra_serializer_turtle).
-export([serialize/3]).
-export_type([turtle_wr_opts/0]).

-type turtle_wr_opts() :: #{notify => async | sync | {pid(), any()},
							prefixes => lagra_model:prefix_map()}.

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
	serialize(Store, File, Options#{notify => async}).

-spec serialize_impl(lagra:store(), file:io_device(), map()) -> ok.
serialize_impl(Store, File, Options) ->
	Graph = maps:get(graph, Options, '_'),
	Triples = lists:sort(lagra:find_all_t(Store, {'_', '_', '_', Graph})),
	Prefixes = [prep_prefix(K, V)
				|| {K, V} <- maps:to_list(
							   maps:get(prefixes, Options, #{}))],
	[write_prefix(File, S, L) || {L, S} <- Prefixes],
	next_triple(Triples, File, maps:from_list(Prefixes), none, none).

-spec prep_prefix(unicode:string() | lagra_model:rdfnode(), unicode:string()) ->
						 {binary(), unicode:string()}.
prep_prefix({iri, Iri}, Pfx) ->
	{Iri, Pfx};
prep_prefix(IriText, Pfx) ->
	{unicode:characters_to_binary(IriText), Pfx}.

-spec write_prefix(file:io_device(), unicode:string(),
				   unicode:string() | lagra_model:rdfnode()) -> ok.
write_prefix(File, Short, Long) ->
	io:format(File, "@prefix ~ts: <~ts> .~n", [Short, Long]).


-spec next_triple([lagra_model:triple()], file:io_device(), map(),
				  lagra_model:rdfnode() | none, lagra_model:rdfnode() | none) ->
						 ok.
next_triple([], File, _Pfx, none, none) ->
	io:format(File, "~n", []);
next_triple([], File, _Pfx, _, _) ->
	io:format(File, ".~n", []);
next_triple([{S, P, O}|Triples], File, Pfx, S, P) ->
	io:format(File, ",~n\t\t~ts", [node_to_text(O, Pfx)]),
	next_triple(Triples, File, Pfx, S, P);
next_triple([{S, P, O}|Triples], File, Pfx, S, _) ->
	PTxt = node_to_text(P, Pfx),
	OTxt = node_to_text(O, Pfx),
	io:format(File, ";~n\t~ts ~ts", [PTxt, OTxt]),
	next_triple(Triples, File, Pfx, S, P);
next_triple([{S, P, O}|Triples], File, Pfx, none, none) ->
	STxt = node_to_text(S, Pfx),
	PTxt = node_to_text(P, Pfx),
	OTxt = node_to_text(O, Pfx),
	io:format(File, "~ts ~ts ~ts", [STxt, PTxt, OTxt]),
	next_triple(Triples, File, Pfx, S, P);
next_triple([{S, P, O}|Triples], File, Pfx, _, _) ->
	STxt = node_to_text(S, Pfx),
	PTxt = node_to_text(P, Pfx),
	OTxt = node_to_text(O, Pfx),
	io:format(File, ".~n~ts ~ts ~ts", [STxt, PTxt, OTxt]),
	next_triple(Triples, File, Pfx, S, P).


-define(XSD, "http://www.w3.org/2001/XMLSchema#").

-spec node_to_text(lagra_model:rdfnode(), map()) -> unicode:charlist().
node_to_text({iri, Iri}, Pfxs) ->
	case split_prefix(Iri) of
		{PreString, Id} ->
			case maps:get(PreString, Pfxs, none) of
				none ->
					["<", Iri, ">"];
				Prefix ->
					[Prefix, ":", Id]
			end;
		Iri ->
			["<", Iri, ">"]
	end;
node_to_text({bnode, Id}, _) ->
	["_:", Id];
node_to_text({literal, {string, Text}}, _) ->
	["\"", quote_text(Text), "\""];
node_to_text({literal, {string, Text, Locale}}, _) ->
	["\"", quote_text(Text), "\"@", Locale];
node_to_text({literal, {typed, Term, <<?XSD, Type/binary>>}}, _)
  when Type =:= <<"integer">>; Type =:= <<"nonPositiveInteger">>;
	   Type =:= <<"negativeInteger">>; Type =:= <<"long">>; Type =:= <<"int">>;
	   Type =:= <<"short">>; Type =:= <<"byte">>;
	   Type =:= <<"nonNegativeInteger">>; Type =:= <<"unsignedLong">>;
	   Type =:= <<"unsignedInt">>; Type =:= <<"unsignedShort">>;
	   Type =:= <<"unsignedByte">>; Type =:= <<"positiveInteger">> ->
	["\"", io_lib:format("~B", [Term]), "\"^^<", ?XSD, Type, ">"];
node_to_text({literal, {typed, Term, <<?XSD, Type/binary>>}}, _)
  when Type =:= <<"double">>;
	   Type =:= <<"decimal">>;
	   Type =:= <<"double">> ->
	["\"", io_lib:format("~g", [Term]), "\"^^<", ?XSD, Type, ">"];
node_to_text({literal, {typed, Term, <<?XSD, Type/binary>>}}, _)
  when Type =:= <<"boolean">> ->
	["\"", atom_to_list(Term), "\"^^<", ?XSD, Type, ">"];
node_to_text({literal, {typed, Term, Type}}, _) ->
	["\"", quote_text(Term), "\"^^<", Type, ">"].

-spec quote_text(unicode:charlist()) -> unicode:charlist().
quote_text(Text) ->
	lists:foldl(
	  fun replace_char/2,
	  Text,
	  [{"\\", "\\\\"},
	   {[8],  "\\b"},
	   {[9],  "\\t"},
	   {[10], "\\n"},
	   {[12], "\\f"},
	   {[13], "\\r"},
	   {"\"", "\\\""},
	   {"'",  "\\'"}]).

-spec replace_char({string(), string()}, unicode:charlist()) ->
						  unicode:charlist().
replace_char({In, Out}, T) ->
	string:replace(T, In, Out, all).

-spec split_prefix(unicode:string()) -> unicode:string() | {unicode:string(), unicode:string()}.
split_prefix(Iri) ->
	IriSplit = uri_string:parse(Iri),
	case IriSplit of
		#{fragment := Frag} ->
			NoFrag = maps:remove(fragment, IriSplit),
			Prefix = uri_string:recompose(NoFrag),
			{<<Prefix/binary, "#">>, Frag};
		#{path := Path} ->
			[PrePath, PostPath] = string:split(Path, "/", trailing),
			Prefix = uri_string:recompose(IriSplit#{path => PrePath}),
			{<<Prefix/binary, "/">>, PostPath};
		_ -> Iri
	end.
