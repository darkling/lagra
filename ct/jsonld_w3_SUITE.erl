%%% The following module is generated by the gen-jsonld-test-suite script.
%%% Do not edit by hand.

-module(jsonld_w3_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0]).

%% The test suite is generated from the test manifest file 
%% ct/jsonld_w3_SUITE_data/manifest.jsonld

-export(['fromRDF#t0001'/1]).
-export(['fromRDF#t0002'/1]).
-export(['fromRDF#t0003'/1]).
-export(['fromRDF#t0004'/1]).
-export(['fromRDF#t0005'/1]).
-export(['fromRDF#t0006'/1]).
-export(['fromRDF#t0007'/1]).
-export(['fromRDF#t0008'/1]).
-export(['fromRDF#t0009'/1]).
-export(['fromRDF#t0010'/1]).
-export(['fromRDF#t0011'/1]).
-export(['fromRDF#t0012'/1]).
-export(['fromRDF#t0013'/1]).
-export(['fromRDF#t0014'/1]).
-export(['fromRDF#t0015'/1]).
-export(['fromRDF#t0016'/1]).
-export(['fromRDF#t0017'/1]).
-export(['fromRDF#t0018'/1]).
-export(['fromRDF#t0019'/1]).
-export(['fromRDF#t0020'/1]).
-export(['fromRDF#t0021'/1]).
-export(['fromRDF#t0022'/1]).
-export(['fromRDF#t0023'/1]).
-export(['fromRDF#t0024'/1]).
-export(['fromRDF#t0025'/1]).
-export(['fromRDF#t0026'/1]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(lagra),
    [{prefix, "${base}"}|Config].

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

'fromRDF#t0001'(Config) ->
    Store = ?config(store1, Config),
    Filename = filename:join(?config(data_dir, Config), "fromRdf/0001-in.nq"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, InFile} = file:open(Filename, [read, {encoding, utf8}, binary]),
    ok = lagra:parse(Store, InFile, nquads),
    ok = file:close(InFile),
    Output = lagra:serialize(Store, mem, jsonld, #{}),
    ExpectFile = filename:join(?config(data_dir, Config), "fromRdf/0001-out.jsonld"),
    {ok, ExpData} = file:read_file(ExpectFile),
    Expected = jsx:decode(ExpData, [return_maps]),
    io:format("Generated ~p~n", [Output]),
    io:format("Expected ~p~n", [Expected]),
    true = lagra_jsonld_utils:jsonld_object_compare(Output, Expected).

'fromRDF#t0002'(Config) ->
    Store = ?config(store1, Config),
    Filename = filename:join(?config(data_dir, Config), "fromRdf/0002-in.nq"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, InFile} = file:open(Filename, [read, {encoding, utf8}, binary]),
    ok = lagra:parse(Store, InFile, nquads),
    ok = file:close(InFile),
    Output = lagra:serialize(Store, mem, jsonld, #{}),
    ExpectFile = filename:join(?config(data_dir, Config), "fromRdf/0002-out.jsonld"),
    {ok, ExpData} = file:read_file(ExpectFile),
    Expected = jsx:decode(ExpData, [return_maps]),
    io:format("Generated ~p~n", [Output]),
    io:format("Expected ~p~n", [Expected]),
    true = lagra_jsonld_utils:jsonld_object_compare(Output, Expected).

'fromRDF#t0003'(Config) ->
    Store = ?config(store1, Config),
    Filename = filename:join(?config(data_dir, Config), "fromRdf/0003-in.nq"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, InFile} = file:open(Filename, [read, {encoding, utf8}, binary]),
    ok = lagra:parse(Store, InFile, nquads),
    ok = file:close(InFile),
    Output = lagra:serialize(Store, mem, jsonld, #{}),
    ExpectFile = filename:join(?config(data_dir, Config), "fromRdf/0003-out.jsonld"),
    {ok, ExpData} = file:read_file(ExpectFile),
    Expected = jsx:decode(ExpData, [return_maps]),
    io:format("Generated ~p~n", [Output]),
    io:format("Expected ~p~n", [Expected]),
    true = lagra_jsonld_utils:jsonld_object_compare(Output, Expected).

'fromRDF#t0004'(Config) ->
    Store = ?config(store1, Config),
    Filename = filename:join(?config(data_dir, Config), "fromRdf/0004-in.nq"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, InFile} = file:open(Filename, [read, {encoding, utf8}, binary]),
    ok = lagra:parse(Store, InFile, nquads),
    ok = file:close(InFile),
    Output = lagra:serialize(Store, mem, jsonld, #{}),
    ExpectFile = filename:join(?config(data_dir, Config), "fromRdf/0004-out.jsonld"),
    {ok, ExpData} = file:read_file(ExpectFile),
    Expected = jsx:decode(ExpData, [return_maps]),
    io:format("Generated ~p~n", [Output]),
    io:format("Expected ~p~n", [Expected]),
    true = lagra_jsonld_utils:jsonld_object_compare(Output, Expected).

'fromRDF#t0005'(Config) ->
    Store = ?config(store1, Config),
    Filename = filename:join(?config(data_dir, Config), "fromRdf/0005-in.nq"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, InFile} = file:open(Filename, [read, {encoding, utf8}, binary]),
    ok = lagra:parse(Store, InFile, nquads),
    ok = file:close(InFile),
    Output = lagra:serialize(Store, mem, jsonld, #{}),
    ExpectFile = filename:join(?config(data_dir, Config), "fromRdf/0005-out.jsonld"),
    {ok, ExpData} = file:read_file(ExpectFile),
    Expected = jsx:decode(ExpData, [return_maps]),
    io:format("Generated ~p~n", [Output]),
    io:format("Expected ~p~n", [Expected]),
    true = lagra_jsonld_utils:jsonld_object_compare(Output, Expected).

'fromRDF#t0006'(Config) ->
    Store = ?config(store1, Config),
    Filename = filename:join(?config(data_dir, Config), "fromRdf/0006-in.nq"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, InFile} = file:open(Filename, [read, {encoding, utf8}, binary]),
    ok = lagra:parse(Store, InFile, nquads),
    ok = file:close(InFile),
    Output = lagra:serialize(Store, mem, jsonld, #{}),
    ExpectFile = filename:join(?config(data_dir, Config), "fromRdf/0006-out.jsonld"),
    {ok, ExpData} = file:read_file(ExpectFile),
    Expected = jsx:decode(ExpData, [return_maps]),
    io:format("Generated ~p~n", [Output]),
    io:format("Expected ~p~n", [Expected]),
    true = lagra_jsonld_utils:jsonld_object_compare(Output, Expected).

'fromRDF#t0007'(Config) ->
    Store = ?config(store1, Config),
    Filename = filename:join(?config(data_dir, Config), "fromRdf/0007-in.nq"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, InFile} = file:open(Filename, [read, {encoding, utf8}, binary]),
    ok = lagra:parse(Store, InFile, nquads),
    ok = file:close(InFile),
    Output = lagra:serialize(Store, mem, jsonld, #{}),
    ExpectFile = filename:join(?config(data_dir, Config), "fromRdf/0007-out.jsonld"),
    {ok, ExpData} = file:read_file(ExpectFile),
    Expected = jsx:decode(ExpData, [return_maps]),
    io:format("Generated ~p~n", [Output]),
    io:format("Expected ~p~n", [Expected]),
    true = lagra_jsonld_utils:jsonld_object_compare(Output, Expected).

'fromRDF#t0008'(Config) ->
    Store = ?config(store1, Config),
    Filename = filename:join(?config(data_dir, Config), "fromRdf/0008-in.nq"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, InFile} = file:open(Filename, [read, {encoding, utf8}, binary]),
    ok = lagra:parse(Store, InFile, nquads),
    ok = file:close(InFile),
    Output = lagra:serialize(Store, mem, jsonld, #{}),
    ExpectFile = filename:join(?config(data_dir, Config), "fromRdf/0008-out.jsonld"),
    {ok, ExpData} = file:read_file(ExpectFile),
    Expected = jsx:decode(ExpData, [return_maps]),
    io:format("Generated ~p~n", [Output]),
    io:format("Expected ~p~n", [Expected]),
    true = lagra_jsonld_utils:jsonld_object_compare(Output, Expected).

'fromRDF#t0009'(Config) ->
    Store = ?config(store1, Config),
    Filename = filename:join(?config(data_dir, Config), "fromRdf/0009-in.nq"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, InFile} = file:open(Filename, [read, {encoding, utf8}, binary]),
    ok = lagra:parse(Store, InFile, nquads),
    ok = file:close(InFile),
    Output = lagra:serialize(Store, mem, jsonld, #{}),
    ExpectFile = filename:join(?config(data_dir, Config), "fromRdf/0009-out.jsonld"),
    {ok, ExpData} = file:read_file(ExpectFile),
    Expected = jsx:decode(ExpData, [return_maps]),
    io:format("Generated ~p~n", [Output]),
    io:format("Expected ~p~n", [Expected]),
    true = lagra_jsonld_utils:jsonld_object_compare(Output, Expected).

'fromRDF#t0010'(Config) ->
    Store = ?config(store1, Config),
    Filename = filename:join(?config(data_dir, Config), "fromRdf/0010-in.nq"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, InFile} = file:open(Filename, [read, {encoding, utf8}, binary]),
    ok = lagra:parse(Store, InFile, nquads),
    ok = file:close(InFile),
    Output = lagra:serialize(Store, mem, jsonld, #{}),
    ExpectFile = filename:join(?config(data_dir, Config), "fromRdf/0010-out.jsonld"),
    {ok, ExpData} = file:read_file(ExpectFile),
    Expected = jsx:decode(ExpData, [return_maps]),
    io:format("Generated ~p~n", [Output]),
    io:format("Expected ~p~n", [Expected]),
    true = lagra_jsonld_utils:jsonld_object_compare(Output, Expected).

'fromRDF#t0011'(Config) ->
    Store = ?config(store1, Config),
    Filename = filename:join(?config(data_dir, Config), "fromRdf/0011-in.nq"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, InFile} = file:open(Filename, [read, {encoding, utf8}, binary]),
    ok = lagra:parse(Store, InFile, nquads),
    ok = file:close(InFile),
    Output = lagra:serialize(Store, mem, jsonld, #{}),
    ExpectFile = filename:join(?config(data_dir, Config), "fromRdf/0011-out.jsonld"),
    {ok, ExpData} = file:read_file(ExpectFile),
    Expected = jsx:decode(ExpData, [return_maps]),
    io:format("Generated ~p~n", [Output]),
    io:format("Expected ~p~n", [Expected]),
    true = lagra_jsonld_utils:jsonld_object_compare(Output, Expected).

'fromRDF#t0012'(Config) ->
    Store = ?config(store1, Config),
    Filename = filename:join(?config(data_dir, Config), "fromRdf/0012-in.nq"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, InFile} = file:open(Filename, [read, {encoding, utf8}, binary]),
    ok = lagra:parse(Store, InFile, nquads),
    ok = file:close(InFile),
    Output = lagra:serialize(Store, mem, jsonld, #{}),
    ExpectFile = filename:join(?config(data_dir, Config), "fromRdf/0012-out.jsonld"),
    {ok, ExpData} = file:read_file(ExpectFile),
    Expected = jsx:decode(ExpData, [return_maps]),
    io:format("Generated ~p~n", [Output]),
    io:format("Expected ~p~n", [Expected]),
    true = lagra_jsonld_utils:jsonld_object_compare(Output, Expected).

'fromRDF#t0013'(Config) ->
    Store = ?config(store1, Config),
    Filename = filename:join(?config(data_dir, Config), "fromRdf/0013-in.nq"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, InFile} = file:open(Filename, [read, {encoding, utf8}, binary]),
    ok = lagra:parse(Store, InFile, nquads),
    ok = file:close(InFile),
    Output = lagra:serialize(Store, mem, jsonld, #{}),
    ExpectFile = filename:join(?config(data_dir, Config), "fromRdf/0013-out.jsonld"),
    {ok, ExpData} = file:read_file(ExpectFile),
    Expected = jsx:decode(ExpData, [return_maps]),
    io:format("Generated ~p~n", [Output]),
    io:format("Expected ~p~n", [Expected]),
    true = lagra_jsonld_utils:jsonld_object_compare(Output, Expected).

'fromRDF#t0014'(Config) ->
    Store = ?config(store1, Config),
    Filename = filename:join(?config(data_dir, Config), "fromRdf/0014-in.nq"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, InFile} = file:open(Filename, [read, {encoding, utf8}, binary]),
    ok = lagra:parse(Store, InFile, nquads),
    ok = file:close(InFile),
    Output = lagra:serialize(Store, mem, jsonld, #{}),
    ExpectFile = filename:join(?config(data_dir, Config), "fromRdf/0014-out.jsonld"),
    {ok, ExpData} = file:read_file(ExpectFile),
    Expected = jsx:decode(ExpData, [return_maps]),
    io:format("Generated ~p~n", [Output]),
    io:format("Expected ~p~n", [Expected]),
    true = lagra_jsonld_utils:jsonld_object_compare(Output, Expected).

'fromRDF#t0015'(Config) ->
    Store = ?config(store1, Config),
    Filename = filename:join(?config(data_dir, Config), "fromRdf/0015-in.nq"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, InFile} = file:open(Filename, [read, {encoding, utf8}, binary]),
    ok = lagra:parse(Store, InFile, nquads),
    ok = file:close(InFile),
    Output = lagra:serialize(Store, mem, jsonld, #{}),
    ExpectFile = filename:join(?config(data_dir, Config), "fromRdf/0015-out.jsonld"),
    {ok, ExpData} = file:read_file(ExpectFile),
    Expected = jsx:decode(ExpData, [return_maps]),
    io:format("Generated ~p~n", [Output]),
    io:format("Expected ~p~n", [Expected]),
    true = lagra_jsonld_utils:jsonld_object_compare(Output, Expected).

'fromRDF#t0016'(Config) ->
    Store = ?config(store1, Config),
    Filename = filename:join(?config(data_dir, Config), "fromRdf/0016-in.nq"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, InFile} = file:open(Filename, [read, {encoding, utf8}, binary]),
    ok = lagra:parse(Store, InFile, nquads),
    ok = file:close(InFile),
    Output = lagra:serialize(Store, mem, jsonld, #{}),
    ExpectFile = filename:join(?config(data_dir, Config), "fromRdf/0016-out.jsonld"),
    {ok, ExpData} = file:read_file(ExpectFile),
    Expected = jsx:decode(ExpData, [return_maps]),
    io:format("Generated ~p~n", [Output]),
    io:format("Expected ~p~n", [Expected]),
    true = lagra_jsonld_utils:jsonld_object_compare(Output, Expected).

'fromRDF#t0017'(Config) ->
    Store = ?config(store1, Config),
    Filename = filename:join(?config(data_dir, Config), "fromRdf/0017-in.nq"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, InFile} = file:open(Filename, [read, {encoding, utf8}, binary]),
    ok = lagra:parse(Store, InFile, nquads),
    ok = file:close(InFile),
    Output = lagra:serialize(Store, mem, jsonld, #{}),
    ExpectFile = filename:join(?config(data_dir, Config), "fromRdf/0017-out.jsonld"),
    {ok, ExpData} = file:read_file(ExpectFile),
    Expected = jsx:decode(ExpData, [return_maps]),
    io:format("Generated ~p~n", [Output]),
    io:format("Expected ~p~n", [Expected]),
    true = lagra_jsonld_utils:jsonld_object_compare(Output, Expected).

'fromRDF#t0018'(Config) ->
    Store = ?config(store1, Config),
    Filename = filename:join(?config(data_dir, Config), "fromRdf/0018-in.nq"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, InFile} = file:open(Filename, [read, {encoding, utf8}, binary]),
    ok = lagra:parse(Store, InFile, nquads),
    ok = file:close(InFile),
    Output = lagra:serialize(Store, mem, jsonld, #{native_types => true}),
    ExpectFile = filename:join(?config(data_dir, Config), "fromRdf/0018-out.jsonld"),
    {ok, ExpData} = file:read_file(ExpectFile),
    Expected = jsx:decode(ExpData, [return_maps]),
    io:format("Generated ~p~n", [Output]),
    io:format("Expected ~p~n", [Expected]),
    true = lagra_jsonld_utils:jsonld_object_compare(Output, Expected).

'fromRDF#t0019'(Config) ->
    Store = ?config(store1, Config),
    Filename = filename:join(?config(data_dir, Config), "fromRdf/0019-in.nq"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, InFile} = file:open(Filename, [read, {encoding, utf8}, binary]),
    ok = lagra:parse(Store, InFile, nquads),
    ok = file:close(InFile),
    Output = lagra:serialize(Store, mem, jsonld, #{rdf_type => true}),
    ExpectFile = filename:join(?config(data_dir, Config), "fromRdf/0019-out.jsonld"),
    {ok, ExpData} = file:read_file(ExpectFile),
    Expected = jsx:decode(ExpData, [return_maps]),
    io:format("Generated ~p~n", [Output]),
    io:format("Expected ~p~n", [Expected]),
    true = lagra_jsonld_utils:jsonld_object_compare(Output, Expected).

'fromRDF#t0020'(Config) ->
    Store = ?config(store1, Config),
    Filename = filename:join(?config(data_dir, Config), "fromRdf/0020-in.nq"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, InFile} = file:open(Filename, [read, {encoding, utf8}, binary]),
    ok = lagra:parse(Store, InFile, nquads),
    ok = file:close(InFile),
    Output = lagra:serialize(Store, mem, jsonld, #{}),
    ExpectFile = filename:join(?config(data_dir, Config), "fromRdf/0020-out.jsonld"),
    {ok, ExpData} = file:read_file(ExpectFile),
    Expected = jsx:decode(ExpData, [return_maps]),
    io:format("Generated ~p~n", [Output]),
    io:format("Expected ~p~n", [Expected]),
    true = lagra_jsonld_utils:jsonld_object_compare(Output, Expected).

'fromRDF#t0021'(Config) ->
    Store = ?config(store1, Config),
    Filename = filename:join(?config(data_dir, Config), "fromRdf/0021-in.nq"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, InFile} = file:open(Filename, [read, {encoding, utf8}, binary]),
    ok = lagra:parse(Store, InFile, nquads),
    ok = file:close(InFile),
    Output = lagra:serialize(Store, mem, jsonld, #{}),
    ExpectFile = filename:join(?config(data_dir, Config), "fromRdf/0021-out.jsonld"),
    {ok, ExpData} = file:read_file(ExpectFile),
    Expected = jsx:decode(ExpData, [return_maps]),
    io:format("Generated ~p~n", [Output]),
    io:format("Expected ~p~n", [Expected]),
    true = lagra_jsonld_utils:jsonld_object_compare(Output, Expected).

'fromRDF#t0022'(Config) ->
    Store = ?config(store1, Config),
    Filename = filename:join(?config(data_dir, Config), "fromRdf/0022-in.nq"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, InFile} = file:open(Filename, [read, {encoding, utf8}, binary]),
    ok = lagra:parse(Store, InFile, nquads),
    ok = file:close(InFile),
    Output = lagra:serialize(Store, mem, jsonld, #{}),
    ExpectFile = filename:join(?config(data_dir, Config), "fromRdf/0022-out.jsonld"),
    {ok, ExpData} = file:read_file(ExpectFile),
    Expected = jsx:decode(ExpData, [return_maps]),
    io:format("Generated ~p~n", [Output]),
    io:format("Expected ~p~n", [Expected]),
    true = lagra_jsonld_utils:jsonld_object_compare(Output, Expected).

'fromRDF#t0023'(Config) ->
    Store = ?config(store1, Config),
    Filename = filename:join(?config(data_dir, Config), "fromRdf/0023-in.nq"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, InFile} = file:open(Filename, [read, {encoding, utf8}, binary]),
    ok = lagra:parse(Store, InFile, nquads),
    ok = file:close(InFile),
    Output = lagra:serialize(Store, mem, jsonld, #{}),
    ExpectFile = filename:join(?config(data_dir, Config), "fromRdf/0023-out.jsonld"),
    {ok, ExpData} = file:read_file(ExpectFile),
    Expected = jsx:decode(ExpData, [return_maps]),
    io:format("Generated ~p~n", [Output]),
    io:format("Expected ~p~n", [Expected]),
    true = lagra_jsonld_utils:jsonld_object_compare(Output, Expected).

'fromRDF#t0024'(Config) ->
    Store = ?config(store1, Config),
    Filename = filename:join(?config(data_dir, Config), "fromRdf/0024-in.nq"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, InFile} = file:open(Filename, [read, {encoding, utf8}, binary]),
    ok = lagra:parse(Store, InFile, nquads),
    ok = file:close(InFile),
    Output = lagra:serialize(Store, mem, jsonld, #{}),
    ExpectFile = filename:join(?config(data_dir, Config), "fromRdf/0024-out.jsonld"),
    {ok, ExpData} = file:read_file(ExpectFile),
    Expected = jsx:decode(ExpData, [return_maps]),
    io:format("Generated ~p~n", [Output]),
    io:format("Expected ~p~n", [Expected]),
    true = lagra_jsonld_utils:jsonld_object_compare(Output, Expected).

'fromRDF#t0025'(Config) ->
    Store = ?config(store1, Config),
    Filename = filename:join(?config(data_dir, Config), "fromRdf/0025-in.nq"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, InFile} = file:open(Filename, [read, {encoding, utf8}, binary]),
    ok = lagra:parse(Store, InFile, nquads),
    ok = file:close(InFile),
    Output = lagra:serialize(Store, mem, jsonld, #{}),
    ExpectFile = filename:join(?config(data_dir, Config), "fromRdf/0025-out.jsonld"),
    {ok, ExpData} = file:read_file(ExpectFile),
    Expected = jsx:decode(ExpData, [return_maps]),
    io:format("Generated ~p~n", [Output]),
    io:format("Expected ~p~n", [Expected]),
    true = lagra_jsonld_utils:jsonld_object_compare(Output, Expected).

'fromRDF#t0026'(Config) ->
    Store = ?config(store1, Config),
    Filename = filename:join(?config(data_dir, Config), "fromRdf/0026-in.nq"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, InFile} = file:open(Filename, [read, {encoding, utf8}, binary]),
    ok = lagra:parse(Store, InFile, nquads),
    ok = file:close(InFile),
    Output = lagra:serialize(Store, mem, jsonld, #{}),
    ExpectFile = filename:join(?config(data_dir, Config), "fromRdf/0026-out.jsonld"),
    {ok, ExpData} = file:read_file(ExpectFile),
    Expected = jsx:decode(ExpData, [return_maps]),
    io:format("Generated ~p~n", [Output]),
    io:format("Expected ~p~n", [Expected]),
    true = lagra_jsonld_utils:jsonld_object_compare(Output, Expected).

all() ->
    ['fromRDF#t0001','fromRDF#t0002','fromRDF#t0003','fromRDF#t0004','fromRDF#t0005','fromRDF#t0006','fromRDF#t0007','fromRDF#t0008','fromRDF#t0009','fromRDF#t0010','fromRDF#t0011','fromRDF#t0012','fromRDF#t0013','fromRDF#t0014','fromRDF#t0015','fromRDF#t0016','fromRDF#t0017','fromRDF#t0018','fromRDF#t0019','fromRDF#t0020','fromRDF#t0021','fromRDF#t0022','fromRDF#t0023','fromRDF#t0024','fromRDF#t0025','fromRDF#t0026'].