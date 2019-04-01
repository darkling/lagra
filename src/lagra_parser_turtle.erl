-module(lagra_parser_turtle).
-export([parse/3, parse_incremental/4]).


%% @doc Parse an open file-like object as Turtle into a triplestore.
%%
%%   Reads the contents of `File' into `Store', reading it as Turtle,
%%   with `Options' passed to the parser.
%%
%% @param Store The lagra triplestore to put the triples in
%%
%% @param File An open file-like object
%%
%% @param Options The options to pass to the parser. The available
%% options are:
%%
%% `base => string()': The base IRI to prefix relative IRIs with.
%%
%% @returns `ok | {error, atom(), location()}'
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

%% @doc Parse an open file-like object incrementally.
%%
%%   Reads the contents of `File', calling an accumulation function
%%   for each batch of triples read.
%%
%% @param File An open file-like object
%%
%% @param Callback A function called when a batch of triples has been
%% parsed from the input.
%%
%% @param State The initial state passed to the first call of the `Callback'
%%
%% @param Options The options to pass to the parser. Supported options
%% are those of the `parse/3' function, plus:
%%
%% `batch => integer()': The maximum number of triples to return on
%% each call. Default: `1000'.
%%
%% @returns The updated `State' value, or `{error, Err, State}'.
%%
%% The parser reads triples incrementally from the input file,
%% batching them up in a list. When the end of the input is reached,
%% or the list reaches the maximum batch size, the `Callback' function
%% is called with the current value of the `State'. The `Callback'
%% function should process the triples as appropriate, and then return
%% an updated value of `State'.
-spec parse_incremental(file:io_device(), lagra:incr_cb(), any(), map()) ->
							   any().
parse_incremental(File, Callback, State, Options) ->
	Count = maps:get(batch, Options, 1000),
	{ok, Sup, Parser} = lagra_parsers_sup:new_parser(turtle, File, Options),
	parse_incremental_loop(Parser, Sup, [], Callback, State, Options, Count).

-spec parse_incremental_loop(pid(), pid(), [lagra_model:triple()],
							 lagra:incr_cb(), any(), map(), integer()) ->
									any().
parse_incremental_loop(Parser, Sup, Acc, Callback, State, Options, 0) ->
	Count = maps:get(batch, Options, 1000),
	NewState = Callback(Acc, State),
	parse_incremental_loop(Parser, Sup, [], Callback, NewState, Options, Count);
parse_incremental_loop(Parser, Sup, Acc, Callback, State, Options, Count) ->
	case lagra_parser_turtle_parser:triple(Parser) of
		eof ->
			lagra_parsers_sup:stop_parser(Sup),
			Callback(Acc, State);
		{ok, T} ->
			parse_incremental_loop(Parser, Sup, [T|Acc],
								   Callback, State, Options, Count-1);
		{error, _, _} = Err ->
			lagra_parsers_sup:stop_parser(Sup),
			NewState = Callback(Acc, State),
			{error, Err, NewState}
	end.
