%% @private
-module(lagra_parsers_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([new_parser/3]).
-export([stop_parser/1]).

-define(PARSERS_MAP, #{ntriples => {lagra_parser_ntriples_parser,
									lagra_parser_ntriples_lexer},
					   turtle => {lagra_parser_turtle_parser,
								  lagra_parser_turtle_lexer}}).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

new_parser(Type, File, Options) ->
	{ParserMod, LexerMod} = maps:get(Type, ?PARSERS_MAP),
	{ok, _Manager, _NewParser}
		= supervisor:start_child(?MODULE,
								 [[ParserMod, LexerMod, File, Options]]).

stop_parser(Manager) ->
	_ = supervisor:terminate_child(?MODULE, Manager),
	ok.

init([]) ->
	Procs = [#{id => parser,
			   start => {lagra_parser_sup, start_link, []},
			   type => supervisor}],
	{ok, {{simple_one_for_one, 1, 5}, Procs}}.
