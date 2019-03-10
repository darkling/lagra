%% @private
-module(lagra_parser_ntriples_lexer).
-behaviour(gen_server).

-define(UCHAR, "\\\\u[0-9a-fA-F]{4}|\\\\U[0-9a-fA-F]{8}").
-define(ECHAR, "\\\\[tbnrf\"'\\\\]").
-define(PN_CHARS_U, "A-Za-z\\x{00C0}-\\x{00D6}\\x{00D8}-\\x{00F6}\\x{00F8}-\\x{02FF}\\x{0370}-\\x{037D}\\x{037F}-\\x{1FFF}\\x{200C}-\\x{200D}\\x{2070}-\\x{218F}\\x{2C00}-\\x{2FEF}\\x{3001}-\\x{D7FF}\\x{F900}-\\x{FDCF}\\x{FDF0}-\\x{FFFD}\\x{10000}-\\x{EFFFF}_:").
-define(PN_CHARS, ?PN_CHARS_U++"0-9\\x{00B7}\\x{0300}-\\x{036F}\\x{203F}-\\x{2040}-").

-define(
   REGEX,
   [{dot, "(\\.)"},
	{type_hats, "(\\^\\^)"},
	{langtag, "@([a-zA-Z]+(?:-[a-zA-Z]+)*)"},
	{iriref, "<((?:[^\\x00-\\x20<>\"{}|^`\\\\]|"++?UCHAR++")*)>"},
	{string_literal_quote, "\"((?:[^\\x22\\x5c\\x0a\\x0d]|"++?UCHAR++"|"++?ECHAR++")*)\""},
	{blank_node_label, "_:(["++?PN_CHARS_U++"0-9](?:(?:[."++?PN_CHARS++"])*["++?PN_CHARS++"])?)"},
	{whitespace, "([\\x09\\x20])"},
	{eol, "(\\r?\\n)"},
	{comment, "(#.*$)"}]).

%% API.
-export([start_link/2]).
-export([next_term/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {file     :: file:io_device(),
				rest     :: unicode:charlist() | eof,
				line_num :: integer(),
				char_num :: integer()
			   }).

%% API.

-spec start_link(file:io_device(), map()) -> {ok, pid()}.
start_link(File, Options) ->
	gen_server:start_link(?MODULE, [File, Options], []).

-spec next_term(pid()) -> lagra_parser_ntriples_parser:lexeme().
next_term(TermSrv) ->
	gen_server:call(TermSrv, next).

%% gen_server.

init([File, _Options]) ->
	{ok, #state{file=File,
				rest=[],
				line_num=0,
				char_num=1}}.

handle_call(next, _From, State=#state{file=File,
									  rest=Rest,
									  line_num=Line,
									  char_num=Char}) ->
	{Result, {NewRest, NewLine, NewChar}} = term(File, Rest, Line, Char),
	{reply, Result,
	 State#state{rest=NewRest, line_num=NewLine, char_num=NewChar}}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%% The lexer is a mini server for parsing terms incrementally. The
%%% server reads a line at a time from the input, and returns the next
%%% token on that line.

-spec term(file:io_device(), string() | eof, integer(), integer()) ->
				  {lagra_parser_ntriples_parser:lexeme(),
                                                     % Type, Pos, Text = Result
				   {string(), integer(), integer()}}.% Rest, Ln, Char = State
term(File, [], LnNo, ChNo) ->
	Line = case file:read_line(File) of
			   {ok, L} -> L;
			   eof -> eof;
			   {error, Reason} ->
				   throw(Reason)
		   end,
	{{eol, {LnNo, ChNo}, ""}, {Line, LnNo+1, 1}};
term(_File, eof, LnNo, ChNo) ->
	{{eof, {LnNo, ChNo}, ""}, {"", LnNo, ChNo}};
term(File, Line, LnNo, ChNo) ->
	case first_match(Line, ?REGEX) of
		notfound ->
			{{error, {LnNo, ChNo}, Line}, {tl(Line), LnNo, ChNo+1}};
		{{whitespace, Text}, Rest} ->
			term(File, Rest, LnNo, ChNo+length(Text));
		{{comment, Text}, Rest} ->
			term(File, Rest, LnNo, ChNo+length(Text));
		{{Type, Text}, Rest} ->
			{{Type, {LnNo, ChNo}, Text}, {Rest, LnNo, ChNo+length(Text)}}
	end.

-spec first_match(string(), [{atom(), string()}]) ->
						 notfound | {{atom(), string()}, string()}.
first_match(_Line, []) ->
	notfound;
first_match(Line, [{Tag, Re} | Tail]) ->
	case re:run(Line, "^"++Re++"(.*)", [{capture, all, list}, unicode]) of
		{match, [_, Found, Rest]} ->
			{{Tag, Found}, Rest};
		nomatch -> first_match(Line, Tail)
	end.
