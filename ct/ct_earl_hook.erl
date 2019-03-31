-module(ct_earl_hook).
-include_lib("common_test/include/ct.hrl").

%% Callbacks
-export([init/2]).

-export([post_init_per_suite/4]).
-export([post_end_per_testcase/5]).

%% Always called before any other callback function. Use this to initiate
%% any common state.
init(_Id, Opts) ->
	Lines = string:split(Opts, ";", all),
	State = lists:foldl(fun parse_options/2, #{}, Lines),
	{ok, State}.

parse_options("asserted_by="++Line, Acc) ->
	OldAB = maps:get(asserted_by, Acc, []),
	ABs = [string:strip(X) || X <- string:split(Line, ",", all)],
	Acc#{asserted_by => ABs ++ OldAB};
parse_options("subject="++Line, Acc) ->
	OldSub = maps:get(subject, Acc, []),
	Subs = [string:strip(X) || X <- string:split(Line, ",", all)],
	Acc#{subject => Subs ++ OldSub}.

%% Called after init_per_suite.
post_init_per_suite(Suite, Config, Return, State) ->
	SuiteName = atom_to_list(Suite),
	Filename = filename:join(
				 [?config(priv_dir, Config),
				  "..", "..",
				  "earl-" ++ SuiteName ++ ".ttl"]),
	{ok, File} = file:open(Filename, [write, {encoding, utf8}]),
	io:format(File, "
@prefix dc: <http://purl.org/dc/terms/> .
@prefix doap: <http://usefulinc.com/ns/doap#> .
@prefix foaf: <http://xmlns.com/foaf/spec/#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix earl: <http://www.w3.org/ns/earl#> .

", []),
	file:close(File),
    {Return, State#{earlfile => Filename}}.

%% Called when finishing every test case
post_end_per_testcase(_Suite, Case, Config, EndData,
					  State = #{earlfile := Filename}) ->
	{ok, File} = file:open(Filename, [append, {encoding, utf8}]),

	Outcome = case ?config(tc_status, Config) of
				  ok        -> "earl:passed";
				  {skip, _} -> "earl:untested";
				  _         -> "earl:failed"
			  end,

	AssertedBy = lists:join(", ", maps:get(asserted_by, State)),
	Subject = lists:join(", ", maps:get(subject, State)),
	TestPrefix = proplists:get_value(prefix, Config, ""),
	Now = calendar:system_time_to_rfc3339(
			erlang:system_time(millisecond),
			[{unit, millisecond}]),
	Result = ["[
    earl:outcome ", Outcome, ";
    dc:date \"", Now, "\"^^xsd:dateTime
  ]"],

	EarlReport = ["[
  a earlAssertion;
  earl:assertedBy ", AssertedBy, ";
  earl:subject ", Subject, ";
  earl:test <", TestPrefix, atom_to_list(Case), ">;
  earl:result ", Result, ";
  earl:mode earl:automatic
]."],

	io:format(File, "~ts~n~n", [EarlReport]),
	file:close(File),
	{EndData, State}.
