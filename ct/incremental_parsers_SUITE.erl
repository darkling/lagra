-module(incremental_parsers_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() ->
	[ntriples_1, ntriples_2, ntriples_fail_1, ntriples_fail_2,
	 turtle_1, turtle_2, turtle_fail_1, turtle_fail_2].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(lagra),
    Config.

end_per_suite(_Config) ->
    application:stop(lagra),
    ok.

ntriples_1(Config) ->
	{ok, File} = file:open(
				   filename:join([?config(data_dir, Config), "test_set.nt"]),
				   [read, {encoding, utf8}]),
	2 = lagra:parse_incremental(
		  File, ntriples, fun incr_test_cb_1/2, 0, #{batch => 3}),
	file:close(File).

ntriples_2(Config) ->
	{ok, File} = file:open(
				   filename:join(?config(data_dir, Config), "test_set.nt"),
				   [read, {encoding, utf8}]),
	1 = lagra:parse_incremental(
		  File, ntriples, fun incr_test_cb_2/2, 0),
	file:close(File).

ntriples_fail_1(Config) ->
	{ok, File} = file:open(
				   filename:join(?config(data_dir, Config), "test_fail.nt"),
				   [read, {encoding, utf8}]),
	{error, _, 4} = lagra:parse_incremental(
					  File, ntriples, fun incr_test_cb_0/2, 0, #{batch => 3}),
	file:close(File).

ntriples_fail_2(Config) ->
	{ok, File} = file:open(
				   filename:join(?config(data_dir, Config), "test_fail.nt"),
				   [read, {encoding, utf8}]),
	{error, _, 4} = lagra:parse_incremental(
					  File, ntriples, fun incr_test_cb_0/2, 0),
	file:close(File).

turtle_1(Config) ->
	{ok, File} = file:open(
				   filename:join([?config(data_dir, Config), "test_set.ttl"]),
				   [read, {encoding, utf8}]),
	2 = lagra:parse_incremental(
		  File, turtle, fun incr_test_cb_1/2, 0, #{batch => 3}),
	file:close(File).	

turtle_2(Config) ->
	{ok, File} = file:open(
				   filename:join(?config(data_dir, Config), "test_set.ttl"),
				   [read, {encoding, utf8}]),
	1 = lagra:parse_incremental(
		  File, turtle, fun incr_test_cb_2/2, 0),
	file:close(File).

turtle_fail_1(Config) ->
	{ok, File} = file:open(
				   filename:join(?config(data_dir, Config), "test_fail.ttl"),
				   [read, {encoding, utf8}]),
	{error, _, 4} = lagra:parse_incremental(
					  File, turtle, fun incr_test_cb_0/2, 0, #{batch => 3}),
	file:close(File).

turtle_fail_2(Config) ->
	{ok, File} = file:open(
				   filename:join(?config(data_dir, Config), "test_fail.ttl"),
				   [read, {encoding, utf8}]),
	{error, _, 4} = lagra:parse_incremental(
					  File, turtle, fun incr_test_cb_0/2, 0),
	file:close(File).

%% Null callback function
incr_test_cb_0(Triples, State) ->
	State+length(Triples).

%% Callback function validating 3-batches for test_set.*
incr_test_cb_1(
  [{{iri, <<"https://carfax.org.uk/hugo.ttl">>},
	{iri, <<"http://xmlns.com/foaf/0.1/mbox">>},
	{iri, <<"mailto:hugo@carfax.org.uk">>}},
   {{iri, <<"https://carfax.org.uk/hugo.ttl">>},
	{iri, <<"http://xmlns.com/foaf/0.1/homepage">>},
	{iri, <<"http://carfax.org.uk/">>}},
   {{iri, <<"https://carfax.org.uk/hugo.ttl">>},
	{iri, <<"http://www.w3.org/1999/02/22-rdf-syntax-ns#type">>},
	{iri, <<"http://xmlns.com/foaf/0.1/Person">>}}],
  0) ->
	1;
incr_test_cb_1(
  [{{iri, <<"https://carfax.org.uk/hugo.ttl">>},
	{iri, <<"http://xmlns.com/foaf/0.1/nick">>},
	{literal, {string, <<"darkling">>}}},
   {{iri, <<"https://carfax.org.uk/hugo.ttl">>},
	{iri, <<"http://xmlns.com/foaf/0.1/name">>},
	{literal, {string, <<"Hugo Mills">>}}}],
  1) ->
	2.

%% Callback function validating 1000-batches for test_set.*
incr_test_cb_2(
  [{{iri, <<"https://carfax.org.uk/hugo.ttl">>},
	{iri, <<"http://xmlns.com/foaf/0.1/nick">>},
	{literal, {string, <<"darkling">>}}},
   {{iri, <<"https://carfax.org.uk/hugo.ttl">>},
	{iri, <<"http://xmlns.com/foaf/0.1/name">>},
	{literal, {string, <<"Hugo Mills">>}}},
   {{iri, <<"https://carfax.org.uk/hugo.ttl">>},
	{iri, <<"http://xmlns.com/foaf/0.1/mbox">>},
	{iri, <<"mailto:hugo@carfax.org.uk">>}},
   {{iri, <<"https://carfax.org.uk/hugo.ttl">>},
	{iri, <<"http://xmlns.com/foaf/0.1/homepage">>},
	{iri, <<"http://carfax.org.uk/">>}},
   {{iri, <<"https://carfax.org.uk/hugo.ttl">>},
	{iri, <<"http://www.w3.org/1999/02/22-rdf-syntax-ns#type">>},
	{iri, <<"http://xmlns.com/foaf/0.1/Person">>}}],
  0) ->
	1.
