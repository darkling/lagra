-module(model_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() ->
	[create_literal_01,
	 create_literal_02,
	 create_literal_03,
	 create_literal_04,
	 create_literal_05,
	 create_literal_06,
	 create_literal_07,
	 create_literal_08,
	 create_literal_09,
	 extract_literal_09].

create_literal_01(_) ->
	{literal, {string, <<"PQR">>}}
		= lagra_model:new_literal("PQR").

create_literal_02(_) ->
	{literal, {string, <<"PQR">>, <<"en_GB">>}}
		= lagra_model:new_literal("PQR", "en_GB").

create_literal_03(_) ->
	{literal, {typed, "PQR", <<"http://www.w3.org/2001/XMLSchema#foo">>}}
		= lagra_model:new_literal_typed(
			"PQR", "http://www.w3.org/2001/XMLSchema#foo").

create_literal_04(_) ->
	{literal, {typed, 1, <<"http://www.w3.org/2001/XMLSchema#integer">>}}
		= lagra_model:new_literal(1).

create_literal_05(_) ->
	{literal, {typed, 1.0, <<"http://www.w3.org/2001/XMLSchema#double">>}}
		= lagra_model:new_literal(1.0).

create_literal_06(_) ->
	{literal, {string, <<"PQR">>}}
		= lagra_model:new_literal(<<"PQR">>).

create_literal_07(_) ->
	{literal, {string, <<"PQR">>, <<"en_GB">>}}
		= lagra_model:new_literal(<<"PQR">>, <<"en_GB">>).

create_literal_08(_) ->
	{literal, {typed, <<"PQR">>, <<"http://www.w3.org/2001/XMLSchema#foo">>}}
		= lagra_model:new_literal_typed(
			<<"PQR">>, <<"http://www.w3.org/2001/XMLSchema#foo">>).

create_literal_09(_) ->
	{literal, {typed, <<"2020-09-16T12:34:56Z">>,
			   <<"http://www.w3.org/2001/XMLSchema#dateTime">>}}
		= lagra_model:new_literal({{2020, 9, 16}, {12, 34, 56}}).

extract_literal_09(_) ->
	{{2020, 9, 16}, {12, 34, 56}} = lagra_model:literal_value(
		 {literal, {typed, <<"2020-09-16T12:34:56Z">>,
					<<"http://www.w3.org/2001/XMLSchema#dateTime">>}}).
