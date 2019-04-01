%% @private
-module(lagra_parser_turtle_lexer).
-behaviour(gen_server).

%% These macros define sets of characters which can be concatenated
%% and put inside a regex character class []
-define(PN_LOCAL_ESC_CHARS, "_~.!$&'()*+,;=/?#@%-").  % Used in 172s
-define(HEX, "0-9A-Fa-f").                            % 171s
-define(PN_CHARS_BASE, "A-Za-z\\x{00C0}-\\x{00D6}\\x{00D8}-\\x{00F6}\\x{00F8}-\\x{02FF}\\x{0370}-\\x{037D}\\x{037F}-\\x{1FFF}\\x{200C}-\\x{200D}\\x{2070}-\\x{218F}\\x{2C00}-\\x{2FEF}\\x{3001}-\\x{D7FF}\\x{F900}-\\x{FDCF}\\x{FDF0}-\\x{FFFD}\\x{10000}-\\x{EFFFF}").                               % 163s
-define(PN_CHARS_U, ?PN_CHARS_BASE++"_").             % 164s
-define(PN_CHARS, ?PN_CHARS_U++"0-9\\x{00B7}\\x{0300}-\\x{036F}\\x{203F}-\\x{2040}-").                                                % 166s
-define(WS, "\\x{0020}\\x{0009}\\x{000D}\\x{000A}").  % 161s

%% These macros define full regular expressions
-define(PN_LOCAL_ESC, "\\\\["++?PN_LOCAL_ESC_CHARS++"]"). % 172s
-define(PERCENT, "%["++?HEX++"]["++?HEX++"]").            % 171s
-define(PLX, "(?:"++?PERCENT++"|"++?PN_LOCAL_ESC++")").   % 169s
-define(PN_LOCAL, "(?:[0-9:"++?PN_CHARS_U++"]|"++?PLX++")"
			   ++ "(?:(?:[.:"++?PN_CHARS++"]|"++?PLX++")*"
			   ++ "(?:[:"++?PN_CHARS++"]|"++?PLX++"))?"). % 168s
-define(PN_PREFIX, "["++?PN_CHARS_BASE++"]"
			   ++ "(?:[."++?PN_CHARS++"]*["++?PN_CHARS++"])?"). % 167s
-define(ANON, "\\[["++?WS++"]*\\]").                      % 162s
-define(ECHAR, "\\\\[tbnrf\"'\\\\]").                     % 159s
-define(UCHAR, "\\\\u["++?HEX++"]{4}"++"|\\\\U["++?HEX++"]{8}"). % 26
-define(STRING_LITERAL_LONG_QUOTE,
		"\"\"\"((?:\"{0,2}(?:[^\"\\\\]|"++?ECHAR++"|"++?UCHAR++"))*)\"\"\"").
-define(STRING_LITERAL_LONG_SINGLE_QUOTE,
		"'''((?:'{0,2}(?:[^'\\\\]|"++?ECHAR++"|"++?UCHAR++"))*)'''").
-define(STRING_LITERAL_SINGLE_QUOTE,
		"'((?:[^\\x{0027}\\x{005c}\\x{000a}\\x{000d}]|"++?ECHAR++"|"++?UCHAR++")*)'").
-define(STRING_LITERAL_QUOTE,
		"\"((?:[^\\x{0022}\\x{005c}\\x{000a}\\x{000d}]|"++?ECHAR++"|"++?UCHAR++")*)\"").
-define(EXPONENT, "[eE][+-]?[0-9]+").                     % 22
-define(DOUBLE, "([+-]?(?:[0-9]+\\.[0-9]*"++?EXPONENT
			 ++ "|\\.[0-9]+"++?EXPONENT
			 ++ "|[0-9]+"++?EXPONENT++"))").              % 21
-define(DECIMAL, "([+-]?[0-9]*\\.[0-9]+)").               % 20
-define(INTEGER, "([+-]?[0-9]+)").                        % 19
-define(LANGTAG, "@([a-zA-Z]+(?:-[a-zA-Z0-9]+)*)").       % 144s
-define(BLANK_NODE_LABEL, "_:([0-9"++?PN_CHARS_U++"](?:[."++?PN_CHARS++"]*["++?PN_CHARS++"])?)").                                         % 141s
-define(PNAME_NS, "(?:("++?PN_PREFIX++")?):").            % 139s
-define(PNAME_LN, "((?:"++?PN_PREFIX++")?:"++?PN_LOCAL++")").       % 140s
-define(IRIREF_PRINTABLE_CHARS, "<>\"{}|^`\\\\").
-define(IRIREF_CHARS, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32 | ?IRIREF_PRINTABLE_CHARS]).
-define(IRIREF, "<((?:[^\\x{0000}-\\x{0020}"++?IRIREF_PRINTABLE_CHARS++"]|"++?UCHAR++")*)>"). % 18

-define(
   REGEX,
   [{whitespace, "(["++?WS++"]+)"},
	{comment, "(#.*)$"},
	{prefix, "(@prefix|[Pp][Rr][Ee][Ff][Ii][Xx])"},
	{base, "(@base|[Bb][Aa][Ss][Ee])"},
	{iri, ?IRIREF},
	{bnode_label, ?BLANK_NODE_LABEL},
	{anon, "("++?ANON++")"},
	{pfxname_ln, ?PNAME_LN},
	{pfxname_ns, ?PNAME_NS},
	{langtag, ?LANGTAG},
	{type_hats, "(\\^\\^)"},
	{longstring, "(\"\"\")"},
	{longstring, "(''')"},
	{string, ?STRING_LITERAL_SINGLE_QUOTE},
	{string, ?STRING_LITERAL_QUOTE},
	{double, ?DOUBLE},
	{decimal, ?DECIMAL},
	{integer, ?INTEGER},
	{boolean, "(true|false)"},
	{a, "(a)"},
	{punc, "([.,;()\\x{005b}\\x{005d}])"} % \x5b = "[", \x5d = "]"
   ]).

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
				rest     :: unicode:chardata() | eof,
				line_num :: integer(),
				char_num :: integer()
			   }).

%% API.

-spec start_link(file:io_device(), map()) -> {ok, pid()}.
start_link(File, Options) ->
	gen_server:start_link(?MODULE, [File, Options], []).

-spec next_term(pid()) -> lagra_parser_turtle_parser:lexeme().
next_term(TermSrv) ->
	gen_server:call(TermSrv, next).

%% gen_server.

init([File, _Options]) ->
	{ok, #state{file=File,
				rest= <<"">>,
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

-spec term(file:io_device(), unicode:chardata() | eof, integer(), integer()) ->
				  {lagra_parser_turtle_parser:lexeme(),
                                                     % Type, Pos, Text = Result
				   {unicode:chardata(), integer(), integer()}}.
                                                     % Rest, Ln, Char = State
term(File, Empty, LnNo, _ChNo)
  when Empty =:= ""; Empty =:= <<"">> ->
	Line = case io:get_line(File, "") of
			   eof -> eof;
			   {error, Reason} ->
				   throw(Reason);
			   L -> unicode:characters_to_binary(L)
		   end,
	term(File, Line, LnNo+1, 1);
term(_File, eof, LnNo, ChNo) ->
	{{eof, {LnNo, ChNo}, <<"">>}, {<<"">>, LnNo, ChNo}};
term(File, Line, LnNo, ChNo) ->
	case first_match(Line, ?REGEX) of
		notfound ->
			{{error, {LnNo, ChNo}, Line},
			 {string:slice(Line, 1), LnNo, ChNo+1}};
		{{whitespace, Text}, Rest} ->
			term(File, Rest, LnNo, ChNo+string:length(Text));
		{{comment, Text}, Rest} ->
			term(File, Rest, LnNo, ChNo+string:length(Text));
		{{longstring, <<"\"\"\"">>}, _Rest} ->
			match_longstring(File, Line, LnNo, ChNo, 0,
							 ?STRING_LITERAL_LONG_QUOTE);
		{{longstring, <<"'''">>}, _Rest} ->
			match_longstring(File, Line, LnNo, ChNo, 0,
							 ?STRING_LITERAL_LONG_SINGLE_QUOTE);
		{{iri, Text}, Rest} ->
			QText = lagra_parser_common:replace_quoted_iri(Text),
			case string:take(QText, ?IRIREF_CHARS, true) of
				{_, <<"">>} ->
					{{iri, {LnNo, ChNo}, QText},
					 {Rest, LnNo, ChNo+string:length(Text)}};
				_ ->
					{{error, {LnNo, ChNo}, QText},
					 {Rest, LnNo, ChNo+string:length(Text)}}
			end;
		{{pfxname_ln, Text}, Rest} ->
			QText = lagra_parser_common:replace_quoted_local(Text),
			{{pfxname_ln, {LnNo, ChNo}, QText},
			 {Rest, LnNo, ChNo+string:length(Text)}};
		{{string, Text}, Rest} ->
			QText = lagra_parser_common:replace_quoted_string(Text),
			{{string, {LnNo, ChNo}, QText},
			 {Rest, LnNo, ChNo+string:length(Text)}};
		{{Type, Text}, Rest} ->
			{{Type, {LnNo, ChNo}, Text},
			 {Rest, LnNo, ChNo+string:length(Text)}}
	end.

-spec first_match(unicode:chardata(), [{atom(), unicode:chardata()}]) ->
						 notfound | {{atom(), unicode:chardata()},
									 unicode:chardata()}.
first_match(_Line, []) ->
	notfound;
first_match(Line, [{Tag, Re} | Tail]) ->
	case re:run(Line, "^"++Re++"(.*)",
				[{capture, all, binary}, unicode, dotall]) of
		{match, [_, Found, Rest]} ->
			{{Tag, Found}, Rest};
		nomatch -> first_match(Line, Tail)
	end.

-spec match_longstring(file:io_device(), unicode:chardata() | eof,
					   integer(), integer(), integer(), string()) ->
							  {lagra_parser_turtle_parser:lexeme(),
							   {unicode:chardata(), integer(), integer()}}.
match_longstring(File, Line, LnNo, ChNo, LnCount, Re) ->
	% Read lines from File until the regex matches at the start of the
	% string, or we hit EOF.
	case re:run(Line, ["^", Re, "(.*)"],
				[{capture, all, binary}, unicode, dotall]) of
		{match, [_, Found, Rest]} ->
			QText = lagra_parser_common:replace_quoted_string(Found),
			{{string, {LnNo, ChNo}, QText},
			 {Rest, LnNo+LnCount, tailchars(ChNo, LnCount, Found)}};
		nomatch ->
			case io:get_line(File, "") of
				eof ->
					{{eof, LnNo+LnCount, tailchars(ChNo, LnCount, Line)},
					 {eof, LnNo+LnCount, 1}};
				{error, Reason} ->
					throw(Reason);
				L ->
					LB = unicode:characters_to_binary(L),
					match_longstring(File, <<Line/binary, LB/binary>>,
									 LnNo, ChNo, LnCount+1, Re)
			end
	end.

-spec tailchars(integer(), integer(), unicode:chardata()) -> integer().
tailchars(ChNo, 0, Str) ->
	% Return the resulting character position after advancing by the
	% (long) string Str
	ChNo+string:length(Str)+6;
tailchars(_, _, Str) ->
	[_, LastLine] = string:split(Str, "\n", trailing),
	string:length(LastLine)+3.
