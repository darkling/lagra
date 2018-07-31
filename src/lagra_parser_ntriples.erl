-module(lagra_parser_ntriples).
-export([parse/3, terms/2]).
-export([next_term/1, term/2, first_match/2]).

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

-record(state,
		{triple :: partial_triple(),
		 bnodes :: map(),
		 allow_relative :: boolean()
		}).

-type partial_triple() :: {lagra_model:subject() | none,
						   lagra_model:predicate() | none,
						   lagra_model:object() | none}.
-type pos() :: {integer(), integer()}.
-type lexeme() :: {atom(), pos(), string()}.
-type error_value() :: {error, atom(), pos()}.
-type state() :: #state{}.

-spec parse(lagra:store(), file:io_device(), proplists:proplist())
		   -> ok | error_value().
parse(Store, File, Options) ->
	TermSrv = spawn_link(?MODULE, terms, [File, {"", 0, 0}]),
	State = #state{
			   triple={none, none, none},
			   bnodes=#{},
			   allow_relative=proplists:get_bool(allow_relative, Options)},
	Result = parse_subject(Store, TermSrv, next_term(TermSrv), State),
	stop_server(TermSrv),
	Result.

%%% The parser is basically a simple state machine, cycling through
%%% subject (iri or bnode), predicate (iri), object (iri, bnode,
%%% literal), literal annotations (lang tag, type) if appropriate, and
%%% finishing with dot and EOL.
%%% Whitespace and comments are dropped in the lexer.
%%% The parser adds triples to the store incrementally. It returns
%%% either 'ok' or {'error', ErrType, {Line, Char}}

-spec parse_subject(lagra:store(), pid(), lexeme(), state())
				   -> ok | error_value().
parse_subject(Store, TermSrv, {eol, _, _},
			  State = #state{triple={none, none, none}}) ->
	parse_subject(Store, TermSrv, next_term(TermSrv), State);
parse_subject(Store, TermSrv, {iriref, Pos, Text},
			  State = #state{triple={none, none, none}}) ->
	IRI = lagra_model:new_iri(Text),
	case lagra_model:is_absolute_iri(IRI) or State#state.allow_relative of
		true ->
			parse_predicate(Store, TermSrv, next_term(TermSrv),
							State#state{triple={IRI, none, none}});
		false ->
			{error, relative_iri, Pos}
	end;
parse_subject(Store, TermSrv, {blank_node_label, _, Text},
			  State = #state{triple={none, none, none}, bnodes=BNodes}) ->
	{NewMap, BNode} = get_bnode_or_new(Store, BNodes, Text),
	NewState = State#state{triple = {BNode, none, none},
						   bnodes = NewMap},
	parse_predicate(Store, TermSrv, next_term(TermSrv), NewState);
parse_subject(_, _, {eof, _, _}, #state{triple={none, none, none}}) ->
	ok;
parse_subject(_, _, Term={_, Pos, _}, #state{triple={none, none, none}}) ->
	io:format("Failed subject ~p~n", [Term]),
	{error, syntax, Pos}.


-spec parse_predicate(lagra:store(), pid(), lexeme(), state())
					 -> ok | error_value().
parse_predicate(Store, TermSrv, {iriref, Pos, Text},
				State = #state{triple={S, none, none}}) ->
	IRI = lagra_model:new_iri(Text),
	case lagra_model:is_absolute_iri(IRI) or State#state.allow_relative of
		true ->
			parse_object(Store, TermSrv, next_term(TermSrv),
						 State#state{triple={S, IRI, none}});
		false ->
			{error, relative_iri, Pos}
	end;
parse_predicate(_, _, {_, Pos, _}, #state{triple={_, none, none}}) ->
	{error, syntax, Pos}.


-spec parse_object(lagra:store(), pid(), lexeme(), state())
				  -> ok | error_value().
parse_object(Store, TermSrv, {iriref, Pos, Text},
			 State = #state{triple={S, P, none}}) ->
	IRI = lagra_model:new_iri(Text),
	case lagra_model:is_absolute_iri(IRI) or State#state.allow_relative of
		true ->
			parse_dot(Store, TermSrv, next_term(TermSrv),
					  State#state{triple={S, P, IRI}});
		false ->
			{error, relative_iri, Pos}
	end;
parse_object(Store, TermSrv, {blank_node_label, _, Text},
			 State = #state{triple={S, P, none}, bnodes=BNodes}) ->
	{NewMap, BNode} = get_bnode_or_new(Store, BNodes, Text),
	NewState = State#state{triple = {S, P, BNode},
						   bnodes = NewMap},
	parse_dot(Store, TermSrv, next_term(TermSrv), NewState);
parse_object(Store, TermSrv, {string_literal_quote, _, Text},
			 State = #state{triple={S, P, none}}) ->
	parse_maybe_string_annotation(
	  Store, TermSrv, next_term(TermSrv),
	  State#state{triple={S, P, {literal, {string, Text}}}});
parse_object(_, _, {_, Pos, _}, #state{triple={_, _, none}}) ->
	{error, syntax, Pos}.


-spec parse_maybe_string_annotation(lagra:store(), pid(), lexeme(), state())
								   -> ok | error_value().
parse_maybe_string_annotation(
  Store, TermSrv, {langtag, _, Lang},
  State = #state{triple={S, P, {literal, {string, Text}}}}) ->
	parse_dot(Store, TermSrv, next_term(TermSrv),
			  State#state{triple={S, P, {literal, {string, Text, Lang}}}});
parse_maybe_string_annotation(
  Store, TermSrv, {type_hats, _, _},
  State = #state{triple={_S, _P, {literal, {string, _Text}}}}) ->
	parse_type(Store, TermSrv, next_term(TermSrv), State);
parse_maybe_string_annotation(Store, TermSrv, Dot={dot, _, _}, State) ->
	parse_dot(Store, TermSrv, Dot, State);
parse_maybe_string_annotation(_, _, {_, Pos, _}, _) ->
	{error, syntax, Pos}.


-spec parse_type(lagra:store(), pid(), lexeme(), state())
				-> ok | error_value().
parse_type(Store, TermSrv, {iriref, Pos, Type},
		   State = #state{triple={S, P, {literal, {string, Text}}}}) ->
	TypeIRI = lagra_model:new_iri(Type),
	case lagra_model:is_absolute_iri(TypeIRI) or State#state.allow_relative of
		true ->
			NewLiteral = {literal, {typed, Text, Type}},
			NewTriple = {S, P, NewLiteral},
			NewState = State#state{triple=NewTriple},
			parse_dot(Store, TermSrv, next_term(TermSrv), NewState);
		false ->
			{error, relative_iri, Pos}
	end;
parse_type(_, _, {_, Pos, _}, _) ->
	{error, syntax, Pos}.


-spec parse_dot(lagra:store(), pid(), lexeme(), state())
			   -> ok | error_value().
parse_dot(Store, TermSrv, {dot, _, _}, State = #state{triple=Triple}) ->
	ok = lagra:add(Store, Triple),
	parse_eol(Store, TermSrv, next_term(TermSrv),
			  State#state{triple={none, none, none}});
parse_dot(_, _, {_, Pos, _}, _) ->
	{error, syntax, Pos}.


-spec parse_eol(lagra:store(), pid(), lexeme(), state())
			   -> ok | error_value().
parse_eol(Store, TermSrv, {eol, _, _},
		  State = #state{triple={none, none, none}}) ->
	parse_subject(Store, TermSrv, next_term(TermSrv), State);
parse_eol(_Store, _TermSrv, {eof, _, _}, #state{triple={none, none, none}}) ->
	ok;
parse_eol(_, _, {_, Pos, _}, #state{triple={none, none, none}}) ->
	{error, syntax, Pos}.

-spec get_bnode_or_new(lagra:store(), map(), string())
					  -> {map(), lagra_model:bnode()}.
get_bnode_or_new(Store, BNodes, Text) ->
	case maps:get(Text, BNodes, undef) of
		undef ->
			BNode = lagra_model:new_bnode(Store),
			{BNodes#{Text => BNode}, BNode};
		BNode ->
			{BNodes, BNode}
	end.

%%% The lexer is a mini server for parsing terms incrementally. The
%%% server reads a line at a time from the input, and returns the next
%%% token on that line on a call to next_term/1. 

%%% Call next_term/1 to get the next term, and stop_server/1 to
%%% terminate it.
-spec next_term(pid()) -> lexeme().
next_term(TermSrv) ->
	TermSrv ! {next, self()},
	receive
		{term, Term} -> Term
	after
		5000 -> {eof, none}
	end.

-spec stop_server(pid()) -> ok.
stop_server(TermSrv) ->
	TermSrv ! {halt, self()},
	ok.

-spec terms(file:io_device(), {string(), integer(), integer()}) -> ok.
terms(File, State) ->
	{Src, {Term, NewState}} =
		receive
			{next, From} -> {From, term(File, State)};
			{halt, From} -> {From, {halt, {halt, 0, 0}}}
		end,
	case NewState of
		{halt, 0, 0} ->
			ok;
		_ ->
			Src ! {term, Term},
			terms(File, NewState)
	end.

-spec term(file:io_device(), {string(), integer(), integer()}) ->
				  {lexeme(),                         % Type, Pos, Text = Result
				   {string(), integer(), integer()}}.% Rest, Ln, Char = State
term(File, {[], LnNo, ChNo}) ->
	Line = case file:read_line(File) of
			   {ok, L} -> L;
			   eof -> eof;
			   {error, Reason} ->
				   throw(Reason)
		   end,
	{{eol, {LnNo, ChNo}, ""}, {Line, LnNo+1, 1}};
term(_File, {eof, LnNo, ChNo}) ->
	{{eof, {LnNo, ChNo}, ""}, {"", LnNo, ChNo}};
term(File, {Line, LnNo, ChNo}) ->
	case first_match(Line, ?REGEX) of
		notfound ->
			{{error, {LnNo, ChNo}, Line}, {tl(Line), LnNo, ChNo+1}};
		{{whitespace, Text}, Rest} ->
			term(File, {Rest, LnNo, ChNo+length(Text)});
		{{comment, Text}, Rest} ->
			term(File, {Rest, LnNo, ChNo+length(Text)});
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
