%%% The following module is generated by the get-test-params.sh script.
%%% Do not edit by hand.

-module(parser_ntriples_w3_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0]).

%% The test suite is generated from the test manifest file ct/parser_ntriples_w3_SUITE_data/manifest.ttl

-export([nt_syntax_file_01/1, nt_syntax_file_02/1, nt_syntax_file_03/1, nt_syntax_uri_01/1, nt_syntax_uri_02/1, nt_syntax_uri_03/1, nt_syntax_uri_04/1, nt_syntax_string_01/1, nt_syntax_string_02/1, nt_syntax_string_03/1, nt_syntax_str_esc_01/1, nt_syntax_str_esc_02/1, nt_syntax_str_esc_03/1, nt_syntax_bnode_01/1, nt_syntax_bnode_02/1, nt_syntax_bnode_03/1, nt_syntax_datatypes_01/1, nt_syntax_datatypes_02/1, nt_syntax_bad_uri_01/1, nt_syntax_bad_uri_02/1, nt_syntax_bad_uri_03/1, nt_syntax_bad_uri_04/1, nt_syntax_bad_uri_05/1, nt_syntax_bad_uri_06/1, nt_syntax_bad_uri_07/1, nt_syntax_bad_uri_08/1, nt_syntax_bad_uri_09/1, nt_syntax_bad_prefix_01/1, nt_syntax_bad_base_01/1, nt_syntax_bad_struct_01/1, nt_syntax_bad_struct_02/1, nt_syntax_bad_lang_01/1, nt_syntax_bad_esc_01/1, nt_syntax_bad_esc_02/1, nt_syntax_bad_esc_03/1, nt_syntax_bad_string_01/1, nt_syntax_bad_string_02/1, nt_syntax_bad_string_03/1, nt_syntax_bad_string_04/1, nt_syntax_bad_string_05/1, nt_syntax_bad_string_06/1, nt_syntax_bad_string_07/1, nt_syntax_bad_num_01/1, nt_syntax_bad_num_02/1, nt_syntax_bad_num_03/1, nt_syntax_subm_01/1, comment_following_triple/1, literal_ascii_boundaries/1, literal_with_UTF8_boundaries/1, literal_all_controls/1, literal_all_punctuation/1, literal_with_squote/1, literal_with_2_squotes/1, literal/1, literal_with_dquote/1, literal_with_2_dquotes/1, literal_with_REVERSE_SOLIDUS2/1, literal_with_CHARACTER_TABULATION/1, literal_with_BACKSPACE/1, literal_with_LINE_FEED/1, literal_with_CARRIAGE_RETURN/1, literal_with_FORM_FEED/1, literal_with_REVERSE_SOLIDUS/1, literal_with_numeric_escape4/1, literal_with_numeric_escape8/1, langtagged_string/1, lantag_with_subtag/1, minimal_whitespace/1]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(lagra),
    Config.

end_per_suite(_Config) ->
    application:stop(lagra),
    ok.

init_per_testcase(_, Config) ->
    Store = lagra:create_store(trivial),
    [{store, Store}|Config].

end_per_testcase(_, Config) ->
    Store = ?config(store, Config),
    lagra:destroy_store(Store).


nt_syntax_file_01(Config) ->
    io:format("Config = ~p~n", [Config]),
    Store = ?config(store, Config),
    Filename = filename:join(?config(data_dir, Config), "nt-syntax-file-01.nt"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, File} = file:open(Filename, [read]),
    ok = lagra:parse(Store, File, ntriples).

nt_syntax_file_02(Config) ->
    io:format("Config = ~p~n", [Config]),
    Store = ?config(store, Config),
    Filename = filename:join(?config(data_dir, Config), "nt-syntax-file-02.nt"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, File} = file:open(Filename, [read]),
    ok = lagra:parse(Store, File, ntriples).

nt_syntax_file_03(Config) ->
    io:format("Config = ~p~n", [Config]),
    Store = ?config(store, Config),
    Filename = filename:join(?config(data_dir, Config), "nt-syntax-file-03.nt"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, File} = file:open(Filename, [read]),
    ok = lagra:parse(Store, File, ntriples).

nt_syntax_uri_01(Config) ->
    io:format("Config = ~p~n", [Config]),
    Store = ?config(store, Config),
    Filename = filename:join(?config(data_dir, Config), "nt-syntax-uri-01.nt"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, File} = file:open(Filename, [read]),
    ok = lagra:parse(Store, File, ntriples).

nt_syntax_uri_02(Config) ->
    io:format("Config = ~p~n", [Config]),
    Store = ?config(store, Config),
    Filename = filename:join(?config(data_dir, Config), "nt-syntax-uri-02.nt"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, File} = file:open(Filename, [read]),
    ok = lagra:parse(Store, File, ntriples).

nt_syntax_uri_03(Config) ->
    io:format("Config = ~p~n", [Config]),
    Store = ?config(store, Config),
    Filename = filename:join(?config(data_dir, Config), "nt-syntax-uri-03.nt"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, File} = file:open(Filename, [read]),
    ok = lagra:parse(Store, File, ntriples).

nt_syntax_uri_04(Config) ->
    io:format("Config = ~p~n", [Config]),
    Store = ?config(store, Config),
    Filename = filename:join(?config(data_dir, Config), "nt-syntax-uri-04.nt"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, File} = file:open(Filename, [read]),
    ok = lagra:parse(Store, File, ntriples).

nt_syntax_string_01(Config) ->
    io:format("Config = ~p~n", [Config]),
    Store = ?config(store, Config),
    Filename = filename:join(?config(data_dir, Config), "nt-syntax-string-01.nt"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, File} = file:open(Filename, [read]),
    ok = lagra:parse(Store, File, ntriples).

nt_syntax_string_02(Config) ->
    io:format("Config = ~p~n", [Config]),
    Store = ?config(store, Config),
    Filename = filename:join(?config(data_dir, Config), "nt-syntax-string-02.nt"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, File} = file:open(Filename, [read]),
    ok = lagra:parse(Store, File, ntriples).

nt_syntax_string_03(Config) ->
    io:format("Config = ~p~n", [Config]),
    Store = ?config(store, Config),
    Filename = filename:join(?config(data_dir, Config), "nt-syntax-string-03.nt"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, File} = file:open(Filename, [read]),
    ok = lagra:parse(Store, File, ntriples).

nt_syntax_str_esc_01(Config) ->
    io:format("Config = ~p~n", [Config]),
    Store = ?config(store, Config),
    Filename = filename:join(?config(data_dir, Config), "nt-syntax-str-esc-01.nt"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, File} = file:open(Filename, [read]),
    ok = lagra:parse(Store, File, ntriples).

nt_syntax_str_esc_02(Config) ->
    io:format("Config = ~p~n", [Config]),
    Store = ?config(store, Config),
    Filename = filename:join(?config(data_dir, Config), "nt-syntax-str-esc-02.nt"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, File} = file:open(Filename, [read]),
    ok = lagra:parse(Store, File, ntriples).

nt_syntax_str_esc_03(Config) ->
    io:format("Config = ~p~n", [Config]),
    Store = ?config(store, Config),
    Filename = filename:join(?config(data_dir, Config), "nt-syntax-str-esc-03.nt"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, File} = file:open(Filename, [read]),
    ok = lagra:parse(Store, File, ntriples).

nt_syntax_bnode_01(Config) ->
    io:format("Config = ~p~n", [Config]),
    Store = ?config(store, Config),
    Filename = filename:join(?config(data_dir, Config), "nt-syntax-bnode-01.nt"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, File} = file:open(Filename, [read]),
    ok = lagra:parse(Store, File, ntriples).

nt_syntax_bnode_02(Config) ->
    io:format("Config = ~p~n", [Config]),
    Store = ?config(store, Config),
    Filename = filename:join(?config(data_dir, Config), "nt-syntax-bnode-02.nt"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, File} = file:open(Filename, [read]),
    ok = lagra:parse(Store, File, ntriples).

nt_syntax_bnode_03(Config) ->
    io:format("Config = ~p~n", [Config]),
    Store = ?config(store, Config),
    Filename = filename:join(?config(data_dir, Config), "nt-syntax-bnode-03.nt"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, File} = file:open(Filename, [read]),
    ok = lagra:parse(Store, File, ntriples).

nt_syntax_datatypes_01(Config) ->
    io:format("Config = ~p~n", [Config]),
    Store = ?config(store, Config),
    Filename = filename:join(?config(data_dir, Config), "nt-syntax-datatypes-01.nt"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, File} = file:open(Filename, [read]),
    ok = lagra:parse(Store, File, ntriples).

nt_syntax_datatypes_02(Config) ->
    io:format("Config = ~p~n", [Config]),
    Store = ?config(store, Config),
    Filename = filename:join(?config(data_dir, Config), "nt-syntax-datatypes-02.nt"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, File} = file:open(Filename, [read]),
    ok = lagra:parse(Store, File, ntriples).

nt_syntax_bad_uri_01(Config) ->
    Store = ?config(store, Config),
    {ok, File} = file:open(
        filename:join(?config(data_dir, Config),
                              "nt-syntax-bad-uri-01.nt"),
                      [read]),
    {error, _, _} = lagra:parse(Store, File, ntriples).

nt_syntax_bad_uri_02(Config) ->
    Store = ?config(store, Config),
    {ok, File} = file:open(
        filename:join(?config(data_dir, Config),
                              "nt-syntax-bad-uri-02.nt"),
                      [read]),
    {error, _, _} = lagra:parse(Store, File, ntriples).

nt_syntax_bad_uri_03(Config) ->
    Store = ?config(store, Config),
    {ok, File} = file:open(
        filename:join(?config(data_dir, Config),
                              "nt-syntax-bad-uri-03.nt"),
                      [read]),
    {error, _, _} = lagra:parse(Store, File, ntriples).

nt_syntax_bad_uri_04(Config) ->
    Store = ?config(store, Config),
    {ok, File} = file:open(
        filename:join(?config(data_dir, Config),
                              "nt-syntax-bad-uri-04.nt"),
                      [read]),
    {error, _, _} = lagra:parse(Store, File, ntriples).

nt_syntax_bad_uri_05(Config) ->
    Store = ?config(store, Config),
    {ok, File} = file:open(
        filename:join(?config(data_dir, Config),
                              "nt-syntax-bad-uri-05.nt"),
                      [read]),
    {error, _, _} = lagra:parse(Store, File, ntriples).

nt_syntax_bad_uri_06(Config) ->
    Store = ?config(store, Config),
    {ok, File} = file:open(
        filename:join(?config(data_dir, Config),
                              "nt-syntax-bad-uri-06.nt"),
                      [read]),
    {error, _, _} = lagra:parse(Store, File, ntriples).

nt_syntax_bad_uri_07(Config) ->
    Store = ?config(store, Config),
    {ok, File} = file:open(
        filename:join(?config(data_dir, Config),
                              "nt-syntax-bad-uri-07.nt"),
                      [read]),
    {error, _, _} = lagra:parse(Store, File, ntriples).

nt_syntax_bad_uri_08(Config) ->
    Store = ?config(store, Config),
    {ok, File} = file:open(
        filename:join(?config(data_dir, Config),
                              "nt-syntax-bad-uri-08.nt"),
                      [read]),
    {error, _, _} = lagra:parse(Store, File, ntriples).

nt_syntax_bad_uri_09(Config) ->
    Store = ?config(store, Config),
    {ok, File} = file:open(
        filename:join(?config(data_dir, Config),
                              "nt-syntax-bad-uri-09.nt"),
                      [read]),
    {error, _, _} = lagra:parse(Store, File, ntriples).

nt_syntax_bad_prefix_01(Config) ->
    Store = ?config(store, Config),
    {ok, File} = file:open(
        filename:join(?config(data_dir, Config),
                              "nt-syntax-bad-prefix-01.nt"),
                      [read]),
    {error, _, _} = lagra:parse(Store, File, ntriples).

nt_syntax_bad_base_01(Config) ->
    Store = ?config(store, Config),
    {ok, File} = file:open(
        filename:join(?config(data_dir, Config),
                              "nt-syntax-bad-base-01.nt"),
                      [read]),
    {error, _, _} = lagra:parse(Store, File, ntriples).

nt_syntax_bad_struct_01(Config) ->
    Store = ?config(store, Config),
    {ok, File} = file:open(
        filename:join(?config(data_dir, Config),
                              "nt-syntax-bad-struct-01.nt"),
                      [read]),
    {error, _, _} = lagra:parse(Store, File, ntriples).

nt_syntax_bad_struct_02(Config) ->
    Store = ?config(store, Config),
    {ok, File} = file:open(
        filename:join(?config(data_dir, Config),
                              "nt-syntax-bad-struct-02.nt"),
                      [read]),
    {error, _, _} = lagra:parse(Store, File, ntriples).

nt_syntax_bad_lang_01(Config) ->
    Store = ?config(store, Config),
    {ok, File} = file:open(
        filename:join(?config(data_dir, Config),
                              "nt-syntax-bad-lang-01.nt"),
                      [read]),
    {error, _, _} = lagra:parse(Store, File, ntriples).

nt_syntax_bad_esc_01(Config) ->
    Store = ?config(store, Config),
    {ok, File} = file:open(
        filename:join(?config(data_dir, Config),
                              "nt-syntax-bad-esc-01.nt"),
                      [read]),
    {error, _, _} = lagra:parse(Store, File, ntriples).

nt_syntax_bad_esc_02(Config) ->
    Store = ?config(store, Config),
    {ok, File} = file:open(
        filename:join(?config(data_dir, Config),
                              "nt-syntax-bad-esc-02.nt"),
                      [read]),
    {error, _, _} = lagra:parse(Store, File, ntriples).

nt_syntax_bad_esc_03(Config) ->
    Store = ?config(store, Config),
    {ok, File} = file:open(
        filename:join(?config(data_dir, Config),
                              "nt-syntax-bad-esc-03.nt"),
                      [read]),
    {error, _, _} = lagra:parse(Store, File, ntriples).

nt_syntax_bad_string_01(Config) ->
    Store = ?config(store, Config),
    {ok, File} = file:open(
        filename:join(?config(data_dir, Config),
                              "nt-syntax-bad-string-01.nt"),
                      [read]),
    {error, _, _} = lagra:parse(Store, File, ntriples).

nt_syntax_bad_string_02(Config) ->
    Store = ?config(store, Config),
    {ok, File} = file:open(
        filename:join(?config(data_dir, Config),
                              "nt-syntax-bad-string-02.nt"),
                      [read]),
    {error, _, _} = lagra:parse(Store, File, ntriples).

nt_syntax_bad_string_03(Config) ->
    Store = ?config(store, Config),
    {ok, File} = file:open(
        filename:join(?config(data_dir, Config),
                              "nt-syntax-bad-string-03.nt"),
                      [read]),
    {error, _, _} = lagra:parse(Store, File, ntriples).

nt_syntax_bad_string_04(Config) ->
    Store = ?config(store, Config),
    {ok, File} = file:open(
        filename:join(?config(data_dir, Config),
                              "nt-syntax-bad-string-04.nt"),
                      [read]),
    {error, _, _} = lagra:parse(Store, File, ntriples).

nt_syntax_bad_string_05(Config) ->
    Store = ?config(store, Config),
    {ok, File} = file:open(
        filename:join(?config(data_dir, Config),
                              "nt-syntax-bad-string-05.nt"),
                      [read]),
    {error, _, _} = lagra:parse(Store, File, ntriples).

nt_syntax_bad_string_06(Config) ->
    Store = ?config(store, Config),
    {ok, File} = file:open(
        filename:join(?config(data_dir, Config),
                              "nt-syntax-bad-string-06.nt"),
                      [read]),
    {error, _, _} = lagra:parse(Store, File, ntriples).

nt_syntax_bad_string_07(Config) ->
    Store = ?config(store, Config),
    {ok, File} = file:open(
        filename:join(?config(data_dir, Config),
                              "nt-syntax-bad-string-07.nt"),
                      [read]),
    {error, _, _} = lagra:parse(Store, File, ntriples).

nt_syntax_bad_num_01(Config) ->
    Store = ?config(store, Config),
    {ok, File} = file:open(
        filename:join(?config(data_dir, Config),
                              "nt-syntax-bad-num-01.nt"),
                      [read]),
    {error, _, _} = lagra:parse(Store, File, ntriples).

nt_syntax_bad_num_02(Config) ->
    Store = ?config(store, Config),
    {ok, File} = file:open(
        filename:join(?config(data_dir, Config),
                              "nt-syntax-bad-num-02.nt"),
                      [read]),
    {error, _, _} = lagra:parse(Store, File, ntriples).

nt_syntax_bad_num_03(Config) ->
    Store = ?config(store, Config),
    {ok, File} = file:open(
        filename:join(?config(data_dir, Config),
                              "nt-syntax-bad-num-03.nt"),
                      [read]),
    {error, _, _} = lagra:parse(Store, File, ntriples).

nt_syntax_subm_01(Config) ->
    io:format("Config = ~p~n", [Config]),
    Store = ?config(store, Config),
    Filename = filename:join(?config(data_dir, Config), "nt-syntax-subm-01.nt"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, File} = file:open(Filename, [read]),
    ok = lagra:parse(Store, File, ntriples).

comment_following_triple(Config) ->
    io:format("Config = ~p~n", [Config]),
    Store = ?config(store, Config),
    Filename = filename:join(?config(data_dir, Config), "comment_following_triple.nt"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, File} = file:open(Filename, [read]),
    ok = lagra:parse(Store, File, ntriples).

literal_ascii_boundaries(Config) ->
    io:format("Config = ~p~n", [Config]),
    Store = ?config(store, Config),
    Filename = filename:join(?config(data_dir, Config), "literal_ascii_boundaries.nt"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, File} = file:open(Filename, [read]),
    ok = lagra:parse(Store, File, ntriples).

literal_with_UTF8_boundaries(Config) ->
    io:format("Config = ~p~n", [Config]),
    Store = ?config(store, Config),
    Filename = filename:join(?config(data_dir, Config), "literal_with_UTF8_boundaries.nt"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, File} = file:open(Filename, [read]),
    ok = lagra:parse(Store, File, ntriples).

literal_all_controls(Config) ->
    io:format("Config = ~p~n", [Config]),
    Store = ?config(store, Config),
    Filename = filename:join(?config(data_dir, Config), "literal_all_controls.nt"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, File} = file:open(Filename, [read]),
    ok = lagra:parse(Store, File, ntriples).

literal_all_punctuation(Config) ->
    io:format("Config = ~p~n", [Config]),
    Store = ?config(store, Config),
    Filename = filename:join(?config(data_dir, Config), "literal_all_punctuation.nt"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, File} = file:open(Filename, [read]),
    ok = lagra:parse(Store, File, ntriples).

literal_with_squote(Config) ->
    io:format("Config = ~p~n", [Config]),
    Store = ?config(store, Config),
    Filename = filename:join(?config(data_dir, Config), "literal_with_squote.nt"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, File} = file:open(Filename, [read]),
    ok = lagra:parse(Store, File, ntriples).

literal_with_2_squotes(Config) ->
    io:format("Config = ~p~n", [Config]),
    Store = ?config(store, Config),
    Filename = filename:join(?config(data_dir, Config), "literal_with_2_squotes.nt"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, File} = file:open(Filename, [read]),
    ok = lagra:parse(Store, File, ntriples).

literal(Config) ->
    io:format("Config = ~p~n", [Config]),
    Store = ?config(store, Config),
    Filename = filename:join(?config(data_dir, Config), "literal.nt"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, File} = file:open(Filename, [read]),
    ok = lagra:parse(Store, File, ntriples).

literal_with_dquote(Config) ->
    io:format("Config = ~p~n", [Config]),
    Store = ?config(store, Config),
    Filename = filename:join(?config(data_dir, Config), "literal_with_dquote.nt"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, File} = file:open(Filename, [read]),
    ok = lagra:parse(Store, File, ntriples).

literal_with_2_dquotes(Config) ->
    io:format("Config = ~p~n", [Config]),
    Store = ?config(store, Config),
    Filename = filename:join(?config(data_dir, Config), "literal_with_2_dquotes.nt"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, File} = file:open(Filename, [read]),
    ok = lagra:parse(Store, File, ntriples).

literal_with_REVERSE_SOLIDUS2(Config) ->
    io:format("Config = ~p~n", [Config]),
    Store = ?config(store, Config),
    Filename = filename:join(?config(data_dir, Config), "literal_with_REVERSE_SOLIDUS2.nt"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, File} = file:open(Filename, [read]),
    ok = lagra:parse(Store, File, ntriples).

literal_with_CHARACTER_TABULATION(Config) ->
    io:format("Config = ~p~n", [Config]),
    Store = ?config(store, Config),
    Filename = filename:join(?config(data_dir, Config), "literal_with_CHARACTER_TABULATION.nt"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, File} = file:open(Filename, [read]),
    ok = lagra:parse(Store, File, ntriples).

literal_with_BACKSPACE(Config) ->
    io:format("Config = ~p~n", [Config]),
    Store = ?config(store, Config),
    Filename = filename:join(?config(data_dir, Config), "literal_with_BACKSPACE.nt"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, File} = file:open(Filename, [read]),
    ok = lagra:parse(Store, File, ntriples).

literal_with_LINE_FEED(Config) ->
    io:format("Config = ~p~n", [Config]),
    Store = ?config(store, Config),
    Filename = filename:join(?config(data_dir, Config), "literal_with_LINE_FEED.nt"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, File} = file:open(Filename, [read]),
    ok = lagra:parse(Store, File, ntriples).

literal_with_CARRIAGE_RETURN(Config) ->
    io:format("Config = ~p~n", [Config]),
    Store = ?config(store, Config),
    Filename = filename:join(?config(data_dir, Config), "literal_with_CARRIAGE_RETURN.nt"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, File} = file:open(Filename, [read]),
    ok = lagra:parse(Store, File, ntriples).

literal_with_FORM_FEED(Config) ->
    io:format("Config = ~p~n", [Config]),
    Store = ?config(store, Config),
    Filename = filename:join(?config(data_dir, Config), "literal_with_FORM_FEED.nt"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, File} = file:open(Filename, [read]),
    ok = lagra:parse(Store, File, ntriples).

literal_with_REVERSE_SOLIDUS(Config) ->
    io:format("Config = ~p~n", [Config]),
    Store = ?config(store, Config),
    Filename = filename:join(?config(data_dir, Config), "literal_with_REVERSE_SOLIDUS.nt"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, File} = file:open(Filename, [read]),
    ok = lagra:parse(Store, File, ntriples).

literal_with_numeric_escape4(Config) ->
    io:format("Config = ~p~n", [Config]),
    Store = ?config(store, Config),
    Filename = filename:join(?config(data_dir, Config), "literal_with_numeric_escape4.nt"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, File} = file:open(Filename, [read]),
    ok = lagra:parse(Store, File, ntriples).

literal_with_numeric_escape8(Config) ->
    io:format("Config = ~p~n", [Config]),
    Store = ?config(store, Config),
    Filename = filename:join(?config(data_dir, Config), "literal_with_numeric_escape8.nt"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, File} = file:open(Filename, [read]),
    ok = lagra:parse(Store, File, ntriples).

langtagged_string(Config) ->
    io:format("Config = ~p~n", [Config]),
    Store = ?config(store, Config),
    Filename = filename:join(?config(data_dir, Config), "langtagged_string.nt"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, File} = file:open(Filename, [read]),
    ok = lagra:parse(Store, File, ntriples).

lantag_with_subtag(Config) ->
    io:format("Config = ~p~n", [Config]),
    Store = ?config(store, Config),
    Filename = filename:join(?config(data_dir, Config), "lantag_with_subtag.nt"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, File} = file:open(Filename, [read]),
    ok = lagra:parse(Store, File, ntriples).

minimal_whitespace(Config) ->
    io:format("Config = ~p~n", [Config]),
    Store = ?config(store, Config),
    Filename = filename:join(?config(data_dir, Config), "minimal_whitespace.nt"),
    io:format("Filename = ~p~n", [Filename]),
    {ok, File} = file:open(Filename, [read]),
    ok = lagra:parse(Store, File, ntriples).

all() ->
    [nt_syntax_file_01, nt_syntax_file_02, nt_syntax_file_03, nt_syntax_uri_01, nt_syntax_uri_02, nt_syntax_uri_03, nt_syntax_uri_04, nt_syntax_string_01, nt_syntax_string_02, nt_syntax_string_03, nt_syntax_str_esc_01, nt_syntax_str_esc_02, nt_syntax_str_esc_03, nt_syntax_bnode_01, nt_syntax_bnode_02, nt_syntax_bnode_03, nt_syntax_datatypes_01, nt_syntax_datatypes_02, nt_syntax_bad_uri_01, nt_syntax_bad_uri_02, nt_syntax_bad_uri_03, nt_syntax_bad_uri_04, nt_syntax_bad_uri_05, nt_syntax_bad_uri_06, nt_syntax_bad_uri_07, nt_syntax_bad_uri_08, nt_syntax_bad_uri_09, nt_syntax_bad_prefix_01, nt_syntax_bad_base_01, nt_syntax_bad_struct_01, nt_syntax_bad_struct_02, nt_syntax_bad_lang_01, nt_syntax_bad_esc_01, nt_syntax_bad_esc_02, nt_syntax_bad_esc_03, nt_syntax_bad_string_01, nt_syntax_bad_string_02, nt_syntax_bad_string_03, nt_syntax_bad_string_04, nt_syntax_bad_string_05, nt_syntax_bad_string_06, nt_syntax_bad_string_07, nt_syntax_bad_num_01, nt_syntax_bad_num_02, nt_syntax_bad_num_03, nt_syntax_subm_01, comment_following_triple, literal_ascii_boundaries, literal_with_UTF8_boundaries, literal_all_controls, literal_all_punctuation, literal_with_squote, literal_with_2_squotes, literal, literal_with_dquote, literal_with_2_dquotes, literal_with_REVERSE_SOLIDUS2, literal_with_CHARACTER_TABULATION, literal_with_BACKSPACE, literal_with_LINE_FEED, literal_with_CARRIAGE_RETURN, literal_with_FORM_FEED, literal_with_REVERSE_SOLIDUS, literal_with_numeric_escape4, literal_with_numeric_escape8, langtagged_string, lantag_with_subtag, minimal_whitespace].
