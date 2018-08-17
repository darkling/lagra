-module(lagra_parser_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Params) ->
	{ok, Manager} = supervisor:start_link(?MODULE, Params),
	Children = supervisor:which_children(Manager),
	[ParserPid] = [Pid || {parser, Pid, _, _} <- Children],
	[LexerPid] = [Pid || {lexer, Pid, _, _} <- Children],
	gen_server:cast(ParserPid, {lexer, LexerPid}),
	{ok, Manager, ParserPid}.

init([ParserMod, LexerMod, File, Options]) ->
	Procs = [#{id => parser,
			   start => {ParserMod, start_link, [Options]},
			   restart => temporary},
			 #{id => lexer,
			   start => {LexerMod, start_link, [File, Options]},
			   restart => temporary}],
	{ok, {{one_for_all, 1, 5}, Procs}}.
