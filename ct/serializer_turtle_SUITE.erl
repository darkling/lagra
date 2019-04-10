-module(serializer_turtle_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile([export_all]).

all() ->
    [empty, one_triple, bnode_s, bnode_o,
	 int_literal, float_literal, string_literal, string_literal_escapes,
	 group_predicates, group_objects,
	 prefix_binary, prefix_rdfnode,
	 prefix_hash, prefix_slash, prefix_partial_slash].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(lagra),
    Config.

end_per_suite(_Config) ->
    application:stop(lagra),
    ok.

init_per_testcase(_, Config) ->
    Store1 = lagra:create_store(trivial),
    Store2 = lagra:create_store(trivial),
    [{store1, Store1}, {store2, Store2}|Config].

end_per_testcase(_, Config) ->
    Store1 = ?config(store1, Config),
    Store2 = ?config(store2, Config),
    lagra:destroy_store(Store1),
	lagra:destroy_store(Store2).

%% Test cases

empty(Config) ->          round_trip("empty", Config).
one_triple(Config) ->     round_trip("spo", Config).
bnode_s(Config) ->        round_trip("bnode-s", Config).
bnode_o(Config) ->        round_trip("bnode-o", Config).
int_literal(Config) ->    round_trip("int-literal", Config).
float_literal(Config) ->  round_trip("float-literal", Config).
string_literal(Config) -> round_trip("string-literal", Config).
string_literal_escapes(Config) ->
	round_trip("string-literal-escapes", Config).
group_predicates(Config) ->
	round_trip("group-predicates", Config).
group_objects(Config) ->
	round_trip("group-objects", Config).

prefix_binary(Config) ->
	Options = #{prefixes => #{<<"http://example.com/">> => "ex"}},
	round_trip("spo", Options, Config),
	Testfile = filename:join(?config(priv_dir, Config), "spo.ttl"),
	{ok, Data} = file:read_file(Testfile),
	[<<"@prefix ex: <http://example.com/> .">>,
	 <<"ex:s ex:p ex:o.">>] = string:lexemes(Data, "\n").

prefix_rdfnode(Config) ->
	Options = #{prefixes =>
					#{lagra_model:new_iri("http://example.com/") => "ex"}},
	round_trip("spo", Options, Config),
	Testfile = filename:join(?config(priv_dir, Config), "spo.ttl"),
	{ok, Data} = file:read_file(Testfile),
	[<<"@prefix ex: <http://example.com/> .">>,
	 <<"ex:s ex:p ex:o.">>] = string:lexemes(Data, "\n").

prefix_hash(Config) ->
	Options = #{prefixes =>
					#{lagra_model:new_iri("http://example.com/foo#") => "ex"}},
	round_trip("spo-foo", Options, Config),
	Testfile = filename:join(?config(priv_dir, Config), "spo-foo.ttl"),
	{ok, Data} = file:read_file(Testfile),
	[<<"@prefix ex: <http://example.com/foo#> .">>,
	 <<"ex:s ex:p ex:o.">>] = string:lexemes(Data, "\n").

prefix_slash(Config) ->
	Options = #{prefixes =>
					#{lagra_model:new_iri("http://example.com/foo/") => "ex"}},
	round_trip("spo-foo-slash", Options, Config),
	Testfile = filename:join(?config(priv_dir, Config), "spo-foo-slash.ttl"),
	{ok, Data} = file:read_file(Testfile),
	[<<"@prefix ex: <http://example.com/foo/> .">>,
	 <<"ex:s ex:p ex:o.">>] = string:lexemes(Data, "\n").

prefix_partial_slash(Config) ->
	Options = #{prefixes =>
					#{lagra_model:new_iri("http://example.com/foo/") => "ex"}},
	round_trip("spo-foo-partial-slash", Options, Config),
	Testfile = filename:join(?config(priv_dir, Config), "spo-foo-partial-slash.ttl"),
	{ok, Data} = file:read_file(Testfile),
	[<<"@prefix ex: <http://example.com/foo/> .">>,
	 <<"ex:s <http://example.com/foo/bar/p> <http://example.com/o>.">>]
		= string:lexemes(Data, "\n").

%% Support

round_trip(Name, Config) ->
	round_trip(Name, #{}, Config).

round_trip(Name, Options, Config) ->
    Store1 = ?config(store1, Config),
    Store2 = ?config(store2, Config),
    Filename = filename:join(?config(data_dir, Config), Name ++ ".ttl"),
    io:format("Filename = ~p~n", [Filename]),
    Testfile = filename:join(?config(priv_dir, Config), Name ++ ".ttl"),
    io:format("Testfile = ~p~n", [Testfile]),

    % Load the file
    {ok, File1} = file:open(Filename, [read, {encoding, utf8}, binary]),
    ok = lagra:parse(Store1, File1, turtle),
    file:close(File1),

    % Write it out
	{ok, File2} = file:open(Testfile, [write, {encoding, utf8}]),
	ok = lagra:serialize(Store1, File2, turtle, Options#{notify => sync}),
    file:close(File2),

	io:format("~ts~n---~n~ts~n", [os:cmd("cat "++Filename),
                                    os:cmd("cat "++Testfile)]),

    % Load it again
    {ok, File3} = file:open(Testfile, [read, {encoding, utf8}, binary]),
    ok = lagra:parse(Store2, File3, turtle),
    file:close(File3),

    % Test that they're isomorphic
    true = lagra:isomorphic(Store1, <<"urn:nil">>, Store2, <<"urn:nil">>).
