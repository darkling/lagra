%% @private
-module(lagra_parser_turtle).
-export([parse/3, parse_incremental/2]).

-spec parse(lagra:store(), file:io_device(), map())
		   -> ok | {error, atom(), {integer(), integer()}}.
parse(Store, File, Options) ->
	{ok, Sup, Parser} = lagra_parsers_sup:new_parser(turtle, File, Options),
	Result = parse_loop(Parser, Store),
	lagra_parsers_sup:stop_parser(Sup),
	Result.
	%% TermSrv = spawn_link(?MODULE, terms, [File, {"", 0, 0}]),
	%% State = #state{
	%% 		   triple={none, none, none},
	%% 		   bnodes=#{},
	%% 		   allow_relative=proplists:get_bool(allow_relative, Options)},
	%% Result = parse_subject(Store, TermSrv, next_term(TermSrv), State),
	%% stop_server(TermSrv),
	%% Result.

-spec parse_loop(pid(), lagra:store()) ->
						ok | {error, atom(), {integer(), integer()}}.
parse_loop(Parser, Store) ->
	case lagra_parser_turtle_parser:triple(Parser) of
		eof ->
			ok;
		{ok, T} ->
			ok = lagra:add(Store, T),
			parse_loop(Parser, Store);
		{error, _, _} = Err ->
			Err
	end.

-spec parse_incremental(file:io_device(), map()) ->
							   lagra:partial_result(lagra_model:triple()).
parse_incremental(File, Options) ->
	Count = maps:get(batch, Options, 1000),
	{ok, Sup, Parser} = lagra_parsers_sup:new_parser(turtle, File, Options),
	parse_incremental_loop(Parser, Sup, [], Options, Count).

-spec parse_incremental_loop(pid(), pid(),
							 [lagra_model:triple()],
							 map(), integer()) ->
									lagra:partial_result(lagra_model:triple()).
parse_incremental_loop(Parser, Sup, Acc, Options, 0) ->
	Count = maps:get(batch, Options, 1000),
	{ok, Acc,
	 fun() ->
			 parse_incremental_loop(Parser, Sup, [], Options, Count)
	 end};
parse_incremental_loop(Parser, Sup, Acc, Options, Count) ->
	case lagra_parser_turtle_parser:triple(Parser) of
		eof ->
			lagra_parsers_sup:stop_parser(Sup),
			{ok, Acc, last};
		{ok, T} ->
			parse_incremental_loop(Parser, Sup, [T|Acc], Options, Count-1);
		{error, _, _} = Err ->
			lagra_parsers_sup:stop_parser(Sup),
			{error, Err, Acc}
	end.
