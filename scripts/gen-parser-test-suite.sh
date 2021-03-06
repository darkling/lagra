#!/bin/bash

manifest="$1"
suite="$2"
base="$3"

function get_config()
{
	roqet -D "$1" -r tsv -q \
        -e 'PREFIX tm: <http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#>
            SELECT ?x ?file ?uscore ?typ ?result
            WHERE { ?x a ?typ;
                       tm:action ?file;
                       tm:name ?uscore.
                    OPTIONAL { ?x tm:result ?result. }
                   }' \
        | sed -e 1d \
		      -e 's=<file://[^>]*/ct/[^/]*/==g' \
		      -e 's=<http://www.w3.org/ns/rdftest#==' \
			  -e 's=<http://data.carfax.org.uk/def/rdftest#==' \
		      -e 's=>==g' \
		      -e 's="==g'
}
#                    BIND (REPLACE(?name, "-", "_") AS ?uscore)

# Output variables
funs=
declare -a fun_names
fun_names=()

# State variables
last_testid=
last_file=

while read testid file uscore type result; do
	case $type in
		TestNTriplesPositiveSyntax)
			funs="${funs}
'$uscore'(Config) ->
    Store = ?config(store1, Config),
    Filename = filename:join(?config(data_dir, Config), \"$file\"),
    io:format(\"Filename = ~p~n\", [Filename]),
    {ok, File} = file:open(Filename, [read, {encoding, utf8}, binary]),
    ok = lagra:parse(Store, File, ntriples).
"
			fun_names+=("'"$uscore"'")
			;;

		TestNTriplesNegativeSyntax)
			funs="${funs}
'$uscore'(Config) ->
    Store = ?config(store1, Config),
    {ok, File} = file:open(
        filename:join(?config(data_dir, Config),
                              \"$file\"),
                      [read, {encoding, utf8}, binary]),
    {error, _, _} = lagra:parse(Store, File, ntriples).
"
			fun_names+=("'"$uscore"'")
			;;

		TestNQuadsPositiveSyntax)
			funs="${funs}
'$uscore'(Config) ->
    Store = ?config(store1, Config),
    Filename = filename:join(?config(data_dir, Config), \"$file\"),
    io:format(\"Filename = ~p~n\", [Filename]),
    {ok, File} = file:open(Filename, [read, {encoding, utf8}, binary]),
    ok = lagra:parse(Store, File, nquads).
"
			fun_names+=("'"$uscore"'")
			;;

		TestNQuadsNegativeSyntax)
			funs="${funs}
'$uscore'(Config) ->
    Store = ?config(store1, Config),
    {ok, File} = file:open(
        filename:join(?config(data_dir, Config),
                              \"$file\"),
                      [read, {encoding, utf8}, binary]),
    {error, _, _} = lagra:parse(Store, File, nquads).
"
			fun_names+=("'"$uscore"'")
			;;

		TestTurtleEval)
			funs="${funs}
'$uscore'(Config) ->
    Store1 = ?config(store1, Config),
    Store2 = ?config(store2, Config),
    {ok, File1} = file:open(
        filename:join(?config(data_dir, Config),
                              \"${file}\"),
                      [read, {encoding, utf8}, binary]),
    {ok, File2} = file:open(
        filename:join(?config(data_dir, Config),
                              \"${result}\"),
                      [read, {encoding, utf8}, binary]),
    ok = lagra:parse(Store1, File1, turtle, #{base=><<\"http://www.w3.org/2013/TurtleTests/${file}\">>}),
    ok = lagra:parse(Store2, File2, ntriples, #{allow_relative=>true}),
    true = lagra:isomorphic(Store1, {iri, <<\"urn:nil\">>}, Store2, {iri, <<\"urn:nil\">>}).
"
			fun_names+=("'"$uscore"'")
			;;

		TestTurtlePositiveSyntax)
			funs="${funs}
'$uscore'(Config) ->
    Store = ?config(store1, Config),
    Filename = filename:join(?config(data_dir, Config), \"$file\"),
    io:format(\"Filename = ~p~n\", [Filename]),
    {ok, File} = file:open(Filename, [read, {encoding, utf8}, binary]),
    ok = lagra:parse(Store, File, turtle).
"
			fun_names+=("'"$uscore"'")
			;;

		TestTurtleNegativeEval|TestTurtleNegativeSyntax)
			funs="${funs}
'$uscore'(Config) ->
    Store = ?config(store1, Config),
    {ok, File} = file:open(
        filename:join(?config(data_dir, Config),
                              \"$file\"),
                      [read, {encoding, utf8}, binary]),
    {error, _, _} = lagra:parse(Store, File, turtle).
"
			fun_names+=("'"$uscore"'")
			;;

		TestIsomorphismPositive)
			if [ "${last_testid}" = "${testid}" ]; then
				funs="${funs}
'$uscore'(Config) ->
    Store1 = ?config(store1, Config),
    Store2 = ?config(store2, Config),
    {ok, File1} = file:open(
        filename:join(?config(data_dir, Config),
                              \"$file\"),
                      [read, {encoding, utf8}, binary]),
    {ok, File2} = file:open(
        filename:join(?config(data_dir, Config),
                              \"$last_file\"),
                      [read, {encoding, utf8}, binary]),
    ok = lagra:parse(Store1, File1, ntriples, #{allow_relative=>true}),
    ok = lagra:parse(Store2, File2, ntriples, #{allow_relative=>true}),
    true = lagra:isomorphic(Store1, {iri, <<\"urn:nil\">>}, Store2, {iri, <<\"urn:nil\">>}).
"
				fun_names+=("'"$uscore"'")
			else
				last_testid="${testid}"
				last_file="${file}"
			fi
			;;

		TestIsomorphismNegative)
			if [ "${last_testid}" = "${testid}" ]; then
				funs="${funs}
'$uscore'(Config) ->
    Store1 = ?config(store1, Config),
    Store2 = ?config(store2, Config),
    {ok, File1} = file:open(
        filename:join(?config(data_dir, Config),
                              \"${file}\"),
                      [read, {encoding, utf8}, binary]),
    {ok, File2} = file:open(
        filename:join(?config(data_dir, Config),
                              \"${last_file}\"),
                      [read, {encoding, utf8}, binary]),
    ok = lagra:parse(Store1, File1, ntriples, #{allow_relative=>true}),
    ok = lagra:parse(Store2, File2, ntriples, #{allow_relative=>true}),
    false = lagra:isomorphic(Store1, {iri, <<\"urn:nil\">>}, Store2, {iri, <<\"urn:nil\">>}).
"
				fun_names+=("'"$uscore"'")
			else
				last_testid="${testid}"
				last_file="${file}"
			fi
			;;

		*)
			echo Unknown test type: $type >&2
			exit 1
			;;
	esac
done < <(get_config "${manifest}")

cat <<EOF
%%% The following module is generated by the gen-parser-test-suite.sh script.
%%% Do not edit by hand.

-module($suite).
-include_lib("common_test/include/ct.hrl").
-export([all/0]).

%% The test suite is generated from the test manifest file $1

EOF

for fn in ${fun_names[@]}; do
	echo "-export([${fn}/1])."
done

cat <<EOF
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

${funs}

all() ->
EOF

IFS=,
echo "    [${fun_names[*]}]."
