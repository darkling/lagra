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
	{literal, {string, "PQR"}} = lagra_model:new_literal("PQR").

create_literal_02(_) ->
	{literal, {string, "PQR", "en_GB"}} = lagra_model:new_literal("PQR", "en_GB").

create_literal_03(_) ->
	{literal, {typed, "PQR", "xsd:foo"}} = lagra_model:new_literal_typed("PQR", "xsd:foo").

create_literal_04(_) ->
	{literal, {typed, 1, "xsd:integer"}} = lagra_model:new_literal(1).

create_literal_05(_) ->
	{literal, {typed, 1.0, "xsd:float"}} = lagra_model:new_literal(1.0).
