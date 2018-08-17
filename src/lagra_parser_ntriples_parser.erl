-module(lagra_parser_ntriples_parser).
-behaviour(gen_server).

-type partial_triple() :: {lagra_model:subject() | none,
						   lagra_model:predicate() | none,
						   lagra_model:object() | none}.

-type pos() :: {integer(), integer()}.
-type lexeme() :: {atom(), pos(), string()}.
-type error_value() :: {error, atom(), pos()}.
-type parse_result() :: eof
					  | error_value()
					  | {lagra_model:triple(), state()}.

%% API.
-export([start_link/1]).
-export([triple/1]).
-export_type([lexeme/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state,
		{lexer :: pid() | undefined,
		 triple :: partial_triple(),
		 bnodes :: map(),
		 allow_relative :: boolean()
		}).
-type state() :: #state{}.

%% API.

-spec start_link(map()) -> {ok, pid()}.
start_link(Options) ->
	gen_server:start_link(?MODULE, [Options], []).

-spec triple(pid()) -> {ok, lagra_model:triple()}
						   | eof
						   | error_value().
triple(Parser) ->
	gen_server:call(Parser, triple).

%% gen_server.

init([Options]) ->
	State = #state{triple={none, none, none},
				   bnodes=#{},
				   allow_relative=maps:get(allow_relative, Options, false)},
	{ok, State}.

handle_call(triple, _From, State) ->
	Term = next_term(State),
	case parse_subject(Term, State) of
		eof ->
			{reply, eof, State};
		{error, _, _} = Err ->
			{reply, Err, State};
		{Triple, NewState} ->
			{reply, {ok, Triple}, NewState}
	end.

handle_cast({lexer, Pid}, State) ->
	{noreply, State#state{lexer=Pid}};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%% The parser is basically a simple state machine, cycling through
%%% subject (iri or bnode), predicate (iri), object (iri, bnode,
%%% literal), literal annotations (lang tag, type) if appropriate, and
%%% finishing with dot and EOL.
%%% Whitespace and comments are dropped in the lexer.
%%% The parser returns either 'eof' or a triple or an error as
%%% {'error', ErrType, {Line, Char}}

-spec parse_subject(lexeme(), state()) -> parse_result().
parse_subject({eol, _, _},
			  State = #state{triple={none, none, none}}) ->
	parse_subject(next_term(State), State);
parse_subject({iriref, Pos, Text},
			  State = #state{triple={none, none, none}}) ->
	IRI = lagra_model:new_iri(Text),
	case lagra_model:is_absolute_iri(IRI) or State#state.allow_relative of
		true ->
			parse_predicate(next_term(State),
							State#state{triple={IRI, none, none}});
		false ->
			{error, relative_iri, Pos}
	end;
parse_subject({blank_node_label, _, Text},
			  State = #state{triple={none, none, none}, bnodes=BNodes}) ->
	{NewMap, BNode} = get_bnode_or_new(BNodes, Text),
	NewState = State#state{triple = {BNode, none, none},
						   bnodes = NewMap},
	parse_predicate(next_term(NewState), NewState);
parse_subject({eof, _, _}, #state{triple={none, none, none}}) ->
	eof;
parse_subject(Term={_, Pos, _}, #state{triple={none, none, none}}) ->
	{error, syntax, Pos}.


-spec parse_predicate(lexeme(), state()) -> parse_result().
parse_predicate({iriref, Pos, Text},
				State = #state{triple={S, none, none}}) ->
	IRI = lagra_model:new_iri(Text),
	case lagra_model:is_absolute_iri(IRI) or State#state.allow_relative of
		true ->
			parse_object(next_term(State),
						 State#state{triple={S, IRI, none}});
		false ->
			{error, relative_iri, Pos}
	end;
parse_predicate({_, Pos, _}, #state{triple={_, none, none}}) ->
	{error, syntax, Pos}.


-spec parse_object(lexeme(), state()) -> parse_result().
parse_object({iriref, Pos, Text},
			 State = #state{triple={S, P, none}}) ->
	IRI = lagra_model:new_iri(Text),
	case lagra_model:is_absolute_iri(IRI) or State#state.allow_relative of
		true ->
			parse_dot(next_term(State),
					  State#state{triple={S, P, IRI}});
		false ->
			{error, relative_iri, Pos}
	end;
parse_object({blank_node_label, _, Text},
			 State = #state{triple={S, P, none}, bnodes=BNodes}) ->
	{NewMap, BNode} = get_bnode_or_new(BNodes, Text),
	NewState = State#state{triple = {S, P, BNode},
						   bnodes = NewMap},
	parse_dot(next_term(NewState), NewState);
parse_object({string_literal_quote, _, Text},
			 State = #state{triple={S, P, none}}) ->
	parse_maybe_string_annotation(
	  next_term(State),
	  State#state{triple={S, P, {literal, {string, Text}}}});
parse_object({_, Pos, _}, #state{triple={_, _, none}}) ->
	{error, syntax, Pos}.


-spec parse_maybe_string_annotation(lexeme(), state()) -> parse_result().
parse_maybe_string_annotation(
  {langtag, _, Lang},
  State = #state{triple={S, P, {literal, {string, Text}}}}) ->
	parse_dot(next_term(State),
			  State#state{triple={S, P, {literal, {string, Text, Lang}}}});
parse_maybe_string_annotation(
  {type_hats, _, _},
  State = #state{triple={_S, _P, {literal, {string, _Text}}}}) ->
	parse_type(next_term(State), State);
parse_maybe_string_annotation(Dot={dot, _, _}, State) ->
	parse_dot(Dot, State);
parse_maybe_string_annotation({_, Pos, _}, _) ->
	{error, syntax, Pos}.


-spec parse_type(lexeme(), state()) -> parse_result().
parse_type({iriref, Pos, Type},
		   State = #state{triple={S, P, {literal, {string, Text}}}}) ->
	TypeIRI = lagra_model:new_iri(Type),
	case lagra_model:is_absolute_iri(TypeIRI) or State#state.allow_relative of
		true ->
			NewLiteral = {literal, {typed, Text, Type}},
			NewTriple = {S, P, NewLiteral},
			NewState = State#state{triple=NewTriple},
			parse_dot(next_term(NewState), NewState);
		false ->
			{error, relative_iri, Pos}
	end;
parse_type({_, Pos, _}, _) ->
	{error, syntax, Pos}.


-spec parse_dot(lexeme(), state()) -> parse_result().
parse_dot({dot, _, _}, State = #state{triple=Triple}) ->
	NewState = State#state{triple={none, none, none}},
	case parse_eol(next_term(NewState), NewState) of
		{error, _, _} = Err ->
			Err;
		X when X =:= ok; X =:= eof ->
			{Triple, NewState}
	end;
parse_dot({_, Pos, _}, _) ->
	{error, syntax, Pos}.


-spec parse_eol(lexeme(), state()) -> parse_result().
parse_eol({eol, _, _}, #state{triple={none, none, none}}) ->
	ok;
parse_eol({eof, _, _}, #state{triple={none, none, none}}) ->
	eof;
parse_eol({_, Pos, _}, #state{triple={none, none, none}}) ->
	{error, syntax, Pos}.


-spec get_bnode_or_new(map(), string()) -> {map(), lagra_model:bnode()}.
get_bnode_or_new(BNodes, Text) ->
	case maps:get(Text, BNodes, undef) of
		undef ->
			BNode = lagra_model:new_bnode(),
			{BNodes#{Text => BNode}, BNode};
		BNode ->
			{BNodes, BNode}
	end.

-spec next_term(state()) -> lexeme().
next_term(State) ->
	lagra_parser_ntriples_lexer:next_term(State#state.lexer).
