-module(parser_turtle_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() ->
	[empty_iri].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(lagra),
    Config.

end_per_suite(_Config) ->
    application:stop(lagra),
    ok.

empty_iri(Config) ->
	%% Check that we get an error when parsing a file with no base supplied
	%% and a relative IRI in it.
	Filename = filename:join(?config(data_dir, Config), "empty_iri.ttl"),
	{ok, File} = file:open(Filename, [read, {encoding, utf8}]),
	Store = lagra:create_store(trivial),
	{error, unresolvable_relative_iri, _} =
		lagra_parser_turtle:parse(Store, File, #{}),
	file:close(File).
