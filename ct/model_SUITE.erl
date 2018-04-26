-module(model_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0]).
-export([create_literal_01/1, create_literal_02/1, create_literal_03/1]).
-export([create_literal_04/1, create_literal_05/1]).

all() ->
	[create_literal_01,
	 create_literal_02,
	 create_literal_03,
	 create_literal_04,
	 create_literal_05].

create_literal_01(_) ->
	{literal, {string, "PQR"}}
		= lagra_model:new_literal("PQR").

create_literal_02(_) ->
	{literal, {string, "PQR", "en_GB"}}
		= lagra_model:new_literal("PQR", "en_GB").

create_literal_03(_) ->
	{literal, {typed, "PQR", "http://www.w3.org/2001/XMLSchema#foo"}}
		= lagra_model:new_literal_typed(
			"PQR", "http://www.w3.org/2001/XMLSchema#foo").

create_literal_04(_) ->
	{literal, {typed, 1, "http://www.w3.org/2001/XMLSchema#integer"}}
		= lagra_model:new_literal(1).

create_literal_05(_) ->
	{literal, {typed, 1.0, "http://www.w3.org/2001/XMLSchema#float"}}
		= lagra_model:new_literal(1.0).
