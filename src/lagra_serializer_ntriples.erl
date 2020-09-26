%% @private
-module(lagra_serializer_ntriples).
-export([serialize/3]).
-export([serialize_impl/3]).

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
	Triples = lagra:find_all_t(Store, {'_', '_', '_', Graph}),
	[io:format(File, "~ts ~ts ~ts .~n",
			   [node_to_text(S), node_to_text(P), node_to_text(O)])
	 || {S, P, O} <- Triples],
	ok.

-define(XSD, "http://www.w3.org/2001/XMLSchema#").

-spec node_to_text(lagra_model:rdfnode()) -> unicode:charlist().
node_to_text({iri, Iri}) ->
	["<", Iri, ">"];
node_to_text({bnode, Id}) ->
	["_:", Id];
node_to_text({literal, {string, Text}}) ->
	["\"", quote_text(Text), "\""];
node_to_text({literal, {string, Text, Locale}}) ->
	["\"", quote_text(Text), "\"@", Locale];
node_to_text({literal, {typed, Term, <<?XSD, Type/binary>>}})
  when Type =:= <<"integer">>; Type =:= <<"nonPositiveInteger">>;
	   Type =:= <<"negativeInteger">>; Type =:= <<"long">>; Type =:= <<"int">>;
	   Type =:= <<"short">>; Type =:= <<"byte">>;
	   Type =:= <<"nonNegativeInteger">>; Type =:= <<"unsignedLong">>;
	   Type =:= <<"unsignedInt">>; Type =:= <<"unsignedShort">>;
	   Type =:= <<"unsignedByte">>; Type =:= <<"positiveInteger">> ->
	["\"", io_lib:format("~B", [Term]), "\"^^<", ?XSD, Type, ">"];
node_to_text({literal, {typed, Term, <<?XSD, Type/binary>>}})
  when Type =:= <<"double">>;
	   Type =:= <<"decimal">>;
	   Type =:= <<"double">> ->
	["\"", io_lib:format("~g", [Term]), "\"^^<", ?XSD, Type, ">"];
node_to_text({literal, {typed, Term, <<?XSD, Type/binary>>}})
  when Type =:= <<"boolean">> ->
	["\"", atom_to_list(Term), "\"^^<", ?XSD, Type, ">"];
node_to_text({literal, {typed, Term, Type}}) ->
	["\"", quote_text(Term), "\"^^<", Type, ">"].

-spec quote_text(unicode:charlist()) -> unicode:charlist().
quote_text(Text) ->
	lists:foldl(
	  fun replace_char/2,
	  Text,
	  [{"\\", "\\\\"},
	   {[8], "\\b"},
	   {[9], "\\t"},
	   {[10], "\\n"},
	   {[12], "\\f"},
	   {[13], "\\r"},
	   {"\"", "\\\""},
	   {"'", "\\'"}]).

-spec replace_char({string(), string()}, unicode:charlist()) ->
						  unicode:charlist().
replace_char({In, Out}, T) ->
	string:replace(T, In, Out, all).
