%% @private
-module(lagra_parser_turtle_parser).
-behaviour(gen_server).

-type partial_triple() :: {lagra_model:subject() | none,
						   lagra_model:predicate() | none,
						   lagra_model:object() | none}.

-type pos() :: {integer(), integer()}.
-type lexeme() :: {atom(), pos(), unicode:chardata()}.
-type error_value() :: {error, atom(), pos()}.
-type parse_result() :: eof
					  | error_value()
					  | {lagra_model:triple(), state()}.
-type parser_fun1() :: fun ((state()) -> parse_result()).
-type parser_fun2() :: fun ((lexeme(), state()) -> parse_result()).
-type parser_fun() :: parser_fun1() | parser_fun2().

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
		 prev :: state() | none,
		 bnodes :: map(),
		 prefixes :: map(),
		 base :: uri_string:uri_map(),
		 ns :: unicode:chardata(),
		 next :: parser_fun1(),
		 pop :: parser_fun() | none
		}).
-type state() :: #state{}.

-define(RDF, <<"http://www.w3.org/1999/02/22-rdf-syntax-ns#">>).
-define(RDFTYPE, lagra_model:new_iri(<<?RDF/binary, "type">>)).
-define(COLL_FIRST, lagra_model:new_iri(<<?RDF/binary, "first">>)).
-define(COLL_REST, lagra_model:new_iri(<<?RDF/binary, "rest">>)).
-define(COLL_END, lagra_model:new_iri(<<?RDF/binary, "nil">>)).
-define(XSD(T), <<"http://www.w3.org/2001/XMLSchema#", T/binary>>).

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
	Base = case maps:get(base, Options, <<"">>) of
			   {iri, Text} -> Text;
			   Text ->        Text
		   end,

	State = #state{lexer=undefined,
				   triple={none, none, none},
				   prev=none,
				   bnodes=#{},
				   prefixes=#{},
				   base=uri_string:parse(Base),
				   ns= <<"">>,
				   next=fun p000s/1,
				   pop=none},
	{ok, State}.

handle_call(triple, _From, State=#state{next=NextFn}) ->
	try
		case NextFn(State) of
			eof ->
				{reply, eof, State};
			{error, _, _} = Err1 ->
				{reply, Err1, State};
			{Triple, NewState} ->
				{reply, {ok, Triple}, NewState}
		end
	catch
		throw:{error, _, _} = Err2 ->
			{reply, Err2, State}
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

%%% The parser is a push-down state machine. The state transitions are
%%% shown in the graph in the massive comment at the end of this file.

push(State = #state{triple=Triple, bnodes=BNodes}, Cont) ->
	State#state{triple=Triple,
				bnodes=BNodes,
				prev=State,
				pop=Cont}.

pop(#state{prev=PrevState,
		   bnodes=BNodes}) ->
	PrevState#state{bnodes=BNodes}.

%% p000s/p000 -- initial state, ready to parse a directive or a
%% "triples" production
-spec p000s(state()) -> parse_result().
p000s(State = #state{triple={none, none, none}}) ->
	p000(next_term(State), State);
p000s(_State) ->
	{error, state, {0, 0}}.

-spec p000(lexeme(), state()) -> parse_result().
p000({prefix, _, <<$@, _/binary>>}, State) ->
	p009(next_term(State), State, fun p014_011/2);
p000({prefix, _, _}, State) ->
	p009(next_term(State), State, fun p000/2);
p000({base, _, <<$@, _/binary>>}, State) ->
	p013(next_term(State), State, fun p014_011/2);
p000({base, _, _}, State) ->
	p013(next_term(State), State, fun p000/2);
% triples ::= subject predicateObjectlist
% subject ::= iri | blankNode | collection
p000({IriType, _, _} = Term, State)
  when IriType =:= iri;
	   IriType =:= pfxname_ln;
	   IriType =:= pfxname_ns ->
	IRI = make_iri(Term, State#state.prefixes, State#state.base),
	NewState = State#state{triple={IRI, none, none}},
	p033(next_term(NewState), NewState);
p000({bnode_label, _, Text},
	  State = #state{bnodes=BNodes}) ->
	{NewMap, BNode} = get_bnode_or_new(BNodes, Text),
	NewState = State#state{triple = {BNode, none, none},
						   bnodes = NewMap},
	p033(next_term(NewState), NewState);
p000({anon, _, _}, State) ->
	BNode = lagra_model:new_bnode(),
	NewState = State#state{triple = {BNode, none, none}},
	p033(next_term(NewState), NewState);
p000({punc, _, <<"(">>}, State) ->
	p090(next_term(State), State);
% triples ::= blankNodePropertyList predicateObjectList?
p000({punc, _, <<"[">>}, State) ->
	p091(next_term(State), State);
p000({eof, _, _}, _State) ->
	eof;
p000({_, Pos, _}, _State) ->
	{error, syntax, Pos}.

%% p033 -- collect together all the initial subjects, push the stack,
%% and descend into the predicateObjectList state machine.
-spec p033(lexeme(), state()) -> parse_result().
p033(Term, State) ->
	PState = push(State, fun p025/2),
	p100(Term, PState).

%% p025 -- return from the predicateObjectList as part of the
%% "triples" production, go round for another statement.
-spec p025(lexeme(), state()) -> parse_result().
p025({punc, _, <<".">>}, State) ->
	NewState = pop(State),
	p000(next_term(NewState), NewState);
p025({_, Pos, _}, _State) ->
	{error, syntax, Pos}.

%% p091 -- descend into a blankNodePropertyList as a subject
-spec p091(lexeme(), state()) -> parse_result().
p091(Term, State) ->
	PState = push(State, fun p023/2),
	BNode = lagra_model:new_bnode(),
	NewState = PState#state{triple={BNode, none, none}},
	p100(Term, NewState).

%% p023 -- return from a blankNodePropertyList as a subject
-spec p023(lexeme(), state()) -> parse_result().
p023({punc, _, <<"]">>}, State=#state{triple={S, _, _}}) ->
	NewState = State#state{triple={S, none, none}},
	p037(next_term(NewState), NewState);
p023({_, Pos, _}, _State) ->
	{error, syntax, Pos}.

%% p037 -- optionally end "bnode" branch of "triples" production
-spec p037(lexeme(), state()) -> parse_result().
p037({punc, _, <<".">>}, State) ->
	p000(next_term(State), State);
p037(Term, State) ->
	p033(Term, State).

%% p090 -- start a collection as subject, pushing into the "collection"
%% substate if it's not a trivial empty list
-spec p090(lexeme(), state()) -> parse_result().
p090({punc, _, <<")">>}, State) ->
	NewState = State#state{triple={?COLL_END, none, none}},
	p033(next_term(NewState), NewState);
p090(Term, State0) ->
	BNode = lagra_model:new_bnode(),
	State1 = State0#state{triple={BNode, none, none}},
	PState = push(State1, fun p028s/1),
	p500(Term, PState).

%% p028s -- return from collection as subject
-spec p028s(state()) -> parse_result().
p028s(State) ->
	NewState = pop(State),
	p033(next_term(NewState), NewState).

%% p100 -- start a predicateObjectList
-spec p100(lexeme(), state()) -> parse_result().
p100({IriType, _, _} = Term,
	 State = #state{triple={S, none, none}})
  when IriType =:= iri;
	   IriType =:= pfxname_ln;
	   IriType =:= pfxname_ns ->
	IRI = make_iri(Term, State#state.prefixes, State#state.base),
	NewState = State#state{triple={S, IRI, none}},
	p130(next_term(NewState), NewState);
p100({a, _, _},
	 State=#state{triple={S, none, none}}) ->
	NewState = State#state{triple={S, ?RDFTYPE, none}},
	p130(next_term(NewState), NewState);
p100({_, Pos, _}, #state{triple={_, none, none}}) ->
	{error, syntax, Pos}.

%% p130 -- prepare call into the "object" substate
-spec p130(lexeme(), state()) -> parse_result().
p130(Term, State) ->
	PState = push(State, fun p131/2),
	p200(Term, PState).

%% p131 -- handle return from the "object" substate: return the triple
-spec p131(lexeme(), state()) -> parse_result().
p131(Term, State=#state{triple=Triple}) ->
	NewState = pop(State),
	{Triple, NewState#state{next=fun (S) -> p102(Term, S) end}}.

%% p102 -- optionally exit the "predicateObjectList" substate
-spec p102(lexeme(), state()) -> parse_result().
p102({punc, _, <<",">>}, State=#state{triple={S, P, _}}) ->
	NewState = State#state{triple={S, P, none}},
	p130(next_term(NewState), NewState);
p102({punc, _, <<";">>}, State=#state{triple={S, _, _}}) ->
	NewState = State#state{triple={S, none, none}},
	p103(next_term(NewState), NewState);
p102(Term, State=#state{pop=Cont}) ->
	Cont(Term, State).

%% p103 -- continue a predicateObjectList, after the ";"
-spec p103(lexeme(), state()) -> parse_result().
p103({IriType, _, _} = Term,
	 State = #state{triple={S, none, none}})
  when IriType =:= iri;
	   IriType =:= pfxname_ln;
	   IriType =:= pfxname_ns ->
	IRI = make_iri(Term, State#state.prefixes, State#state.base),
	NewState = State#state{triple={S, IRI, none}},
	p138(next_term(State), NewState);
p103({a, _, _},
	 State=#state{triple={S, none, none}}) ->
	NewState = State#state{triple={S, ?RDFTYPE, none}},
	p138(next_term(NewState), NewState);
p103(Term, State=#state{triple={_, none, none}}) ->
	p102(Term, State).

%% p138 -- prepare call into the "object" substate for second or later
%% P,O of a predicateObjectList
-spec p138(lexeme(), state()) -> parse_result().
p138(Term, State) ->
	PState = push(State, fun p139/2),
	p200(Term, PState).

%% p139 -- return from call to "object" substate: return the triple
-spec p139(lexeme(), state()) -> parse_result().
p139(Term, State=#state{triple=Triple}) ->
	NewState = pop(State),
	{Triple, NewState#state{next=fun (S) -> p140(Term, S) end}}.

%% p140 -- handle repeated objects
-spec p140(lexeme(), state()) -> parse_result().
p140({punc, _, <<",">>}, State=#state{triple={S, P, _}}) ->
	NewState = State#state{triple={S, P, none}},
	p138(next_term(NewState), NewState);
p140(Term, State) ->
	p102(Term, State).


%% p200 -- entry point for "object" substate
-spec p200(lexeme(), state()) -> parse_result().
p200({IriType, _, _} = Term,
	 State = #state{triple={S, P, none},
					pop=Cont})
  when IriType =:= iri;
	   IriType =:= pfxname_ln;
	   IriType =:= pfxname_ns ->
	IRI = make_iri(Term, State#state.prefixes, State#state.base),
	NewState = State#state{triple={S, P, IRI}},
	Cont(next_term(NewState), NewState);
p200({anon, _, _},
	 State = #state{triple={S, P, none},
					pop=Cont}) ->
	BNode = lagra_model:new_bnode(),
	NewState = State#state{triple={S, P, BNode}},
	Cont(next_term(NewState), NewState);
p200({punc, _, <<"[">>},
	 State = #state{triple={_S, _P, none}}) ->
	p291(State);
p200({bnode_label, _, Text},
	 State = #state{triple={S, P, none},
					bnodes=BNodes,
					pop=Cont}) ->
	{NewMap, BNode} = get_bnode_or_new(BNodes, Text),
	NewState = State#state{triple={S, P, BNode},
						   bnodes=NewMap},
	Cont(next_term(NewState), NewState);
p200({punc, _, <<"(">>},
	 State = #state{triple={_S, _P, none}}) ->
	p290(next_term(State), State);
p200({string, _, Text},
	 State = #state{triple={S, P, none}}) ->
	p230(next_term(State),
		 State#state{triple={S, P, lagra_model:new_literal(Text)}});
p200({integer, _, Text},
	 State = #state{triple={S, P, none},
					pop=Cont}) ->
	O = lagra_model:new_literal(binary_to_integer(Text)),
	NewState = State#state{triple = {S, P, O}},
	Cont(next_term(NewState), NewState);
p200({Type, _, Text},
	 State = #state{triple={S, P, none},
					pop=Cont})
  when Type =:= double; Type =:= decimal ->
	% W3C's description of a float is wider than erlang's. We need to
	% split up the number into a few parts to add extra characters
	% where necessary so that list_to_float/1 will work right.
	F = lagra_parser_common:fixup_float_text(Text),
	TypeIRI = case Type of
				  double -> ?XSD(<<"double">>);
				  decimal -> ?XSD(<<"decimal">>)
			  end,
	O = lagra_model:new_literal_typed(binary_to_float(F), TypeIRI),
	NewState = State#state{triple = {S, P, O}},
	Cont(next_term(NewState), NewState);
p200({boolean, _, Text},
	 State = #state{triple={S, P, none},
					pop=Cont}) ->
	O = lagra_model:new_literal_typed(
		  list_to_existing_atom(unicode:characters_to_list(Text)),
		  ?XSD(<<"boolean">>)),
	NewState = State#state{triple={S, P, O}},
	Cont(next_term(NewState), NewState);
p200({_, Pos, _}, #state{triple={_, _, none}}) ->
	{error, syntax, Pos}.

%% p230: Handle string annotations, if present
-spec p230(lexeme(), state()) -> parse_result().
p230({langtag, _, Lang},
	 State = #state{triple={S, P, String},
					pop=Cont}) ->
	AnnoString = lagra_model:new_literal(
				   lagra_model:literal_value(String), Lang),
	Cont(next_term(State), State#state{triple={S, P, AnnoString}});
p230({type_hats, _, _},
	 State = #state{triple={_S, _P, _O}}) ->
	p221(next_term(State), State);
p230(Term, State=#state{pop=Cont}) ->
	Cont(Term, State).

%% p221: Handle typed literals
-spec p221(lexeme(), state()) -> parse_result().
p221({IriType, _, _} = Term,
	 State = #state{triple={S, P, O},
					prefixes=NSmap,
					pop=Cont})
  when IriType =:= iri;
	   IriType =:= pfxname_ln;
	   IriType =:= pfxname_ns ->
	TypeIriRaw = lagra_model:iri_to_text(
				   make_iri(Term, NSmap, State#state.base)),
	Text = lagra_model:literal_value(O),
	ConvertedValue = lagra_parser_common:binary_to_type(Text, TypeIriRaw),
	NewLiteral = lagra_model:new_literal_typed(ConvertedValue, TypeIriRaw),
	NewState = State#state{triple={S, P, NewLiteral}},
	Cont(next_term(NewState), NewState);
p221({_, Pos, _}, _) ->
	{error, syntax, Pos}.

%% p291 -- store the initial triple of S P [ ... ], and push down to
%% predicateObjectList for the bnode
-spec p291(state()) -> parse_result().
p291(State = #state{triple={S, P, _}}) ->
	BNode = lagra_model:new_bnode(),
	ParentTriple = {S, P, BNode},
	ParentState = State#state{triple=ParentTriple},
	PState = push(ParentState, fun p210/2),
	NewState = PState#state{triple={BNode, none, none}},
	p100(next_term(NewState), NewState).

%% p210 -- return from predicateObjectList inside a bnode
-spec p210(lexeme(), state()) -> parse_result().
p210({punc, _, <<"]">>}, State) ->
	PState = pop(State),
	Cont = PState#state.pop,
	Cont(next_term(PState), PState);
p210({_, Pos, _}, _State) ->
	{error, syntax, Pos}.

%% p290 -- push down to objectlist inside ( ... ) as an object,
%% or return a nil list
-spec p290(lexeme(), state()) -> parse_result().
p290({punc, _, <<")">>},
	 State=#state{triple={S, P, none},
				  pop=Cont}) ->
	NewTriple = {S, P, ?COLL_END},
	NewState = State#state{triple=NewTriple},
	Cont(next_term(NewState), NewState);
p290(Term, State=#state{triple={S, P, none}}) ->
	BNode = lagra_model:new_bnode(),
	PState = push(State#state{triple={S, P, BNode}}, fun p209s/1),
	NewState = PState#state{triple={BNode, none, none}},
	p500(Term, NewState).

%% p209s -- return from objectlist inside ( ... ) as an object
-spec p209s(state()) -> parse_result().
p209s(State) ->
	NewState = pop(State),
	Cont = NewState#state.pop,
	Cont(next_term(NewState), NewState).

%% p500 -- start of a non-empty collection: push down into "object"
%% substate to get the next object
-spec p500(lexeme(), state()) -> parse_result().
p500(Term, State=#state{triple={S, none, none}}) ->
	PState = push(State, fun p502/2),
	NewState = PState#state{triple={S, ?COLL_FIRST, none}},
	p200(Term, NewState).

%% p502 -- return from "object" substate. Add first "first" triple of
%% collection.
-spec p502(lexeme(), state()) -> parse_result().
p502(Term, State=#state{triple=Triple}) ->
	PState = pop(State),
	NewState = PState#state{next=fun (S) -> p503(Term, S) end},
	{Triple, NewState}.

%% p503 -- resume from sending triple(s) to caller. Look for end of
%% the collection (finish with (x :rest :nil) triple), or another
%% object (set up the next two triples, and push down to object
%% substate)
-spec p503(lexeme(), state()) -> parse_result().
p503({punc, _, <<")">>}, State=#state{triple={S, _, _}}) ->
	Triple = {S, ?COLL_REST, ?COLL_END},
	NewState = State#state{next=State#state.pop},
	{Triple, NewState};
p503(Term, State0=#state{triple={Subj, _, _}}) ->
	BNode = lagra_model:new_bnode(),
	State1 = State0#state{triple={BNode, none, none}},
	PState = push(State1, fun p504/2),
	NewState = PState#state{triple={BNode, ?COLL_FIRST, none},
							next=fun (S) -> p505(Term, S) end},
	{{Subj, ?COLL_REST, BNode}, NewState}.

-spec p505(lexeme(), state()) -> parse_result().
p505(Term, State) ->
	p200(Term, State).

%% p504 -- return from "object" substate, return two triples to
%% caller, and go back to p503.
-spec p504(lexeme(), state()) -> parse_result().
p504(Term, State=#state{triple=Triple}) ->
	PState = pop(State),
	NewState = PState#state{next=fun (S) -> p503(Term, S) end},
	{Triple, NewState}.

%% p013 -- start parsing the (@)base directive
-spec p013(lexeme(), state(), parser_fun2()) -> parse_result().
p013({iri, _, Text}, State=#state{base=OldBase}, Cont) ->
	ParsedNew = uri_string:normalize(Text, [return_map]),
	NewBase = resolve_relative_iri_map(ParsedNew, OldBase),
	Cont(next_term(State), State#state{base=NewBase});
p013({_, Pos, _}, _, _) ->
	{error, syntax, Pos}.

%% p009 -- start parsing the (@)prefix directive
-spec p009(lexeme(), state(), parser_fun2()) -> parse_result().
p009({pfxname_ns, _, Text}, State, Cont) ->
	p010(next_term(State), State#state{ns=Text}, Cont);
p009({_, Pos, _}, _, _) ->
	{error, syntax, Pos}.

%% p010 -- parse the IRI part of a prefix directive
-spec p010(lexeme(), state(), parser_fun2()) -> parse_result().
p010({iri, _, Text},
	 State = #state{prefixes=Prefixes, ns=NS},
	 Cont) ->
	NewPrefixes = Prefixes#{NS => Text},
	Cont(next_term(State),
		 State#state{prefixes=NewPrefixes, ns= <<"">>});
p010({_, Pos, _}, _, _) ->
	{error, syntax, Pos}.

%% Parse the dot at the end of the @-directives
-spec p014_011(lexeme(), state()) -> parse_result().
p014_011({punc, _, <<".">>}, State) ->
	p000(next_term(State), State);
p014_011({_, Pos, _}, _) ->
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
	lagra_parser_turtle_lexer:next_term(State#state.lexer).


-spec make_iri(lexeme(), map(), uri_string:uri_map()) -> lagra_model:iri().
make_iri(Term, NSmap, Base) ->
	IRI = resolve_prefix(Term, NSmap),
	case lagra_model:is_absolute_iri(IRI) of
		true ->
			IRI;
		false ->
			resolve_relative_iri(IRI, Base)
	end.

-spec resolve_prefix(lexeme(), map()) -> lagra_model:iri().
resolve_prefix({iri, _, Text}, _) ->
	lagra_model:new_iri(Text);
resolve_prefix({pfxname_ns, Pos, Text}, NSmap) ->
	case NSmap of
		#{Text := Prefix} ->
			lagra_model:new_iri(Prefix);
		_ ->
			throw({error, noprefix, Pos})
	end;
resolve_prefix({pfxname_ln, Pos, Text}, NSmap) ->
	[NS, Suffix] = string:split(Text, ":"),
	case NSmap of
		#{NS := Prefix} ->
			lagra_model:new_iri(<<Prefix/binary, Suffix/binary>>);
		_ ->
			throw({error, noprefix, Pos})
	end.


-spec resolve_relative_iri(lagra_model:iri(), uri_string:uri_map()) ->
								  lagra_model:iri().
resolve_relative_iri(IRI, Base) ->
	Text = lagra_model:iri_to_text(IRI),
	Parsed = uri_string:parse(Text),
	Resolved = resolve_relative_iri_map(Parsed, Base),
	NewText = uri_string:recompose(Resolved),
	lagra_model:new_iri(NewText).

%% Implemented from the algorithm in RFC 3986 Section 5.2.2
-spec resolve_relative_iri_map(uri_string:uri_map(), uri_string:uri_map()) ->
									  uri_string:uri_map().
resolve_relative_iri_map(#{scheme := _} = IRI, _Base) ->
	IRI;
resolve_relative_iri_map(#{host := _} = IRI0, Base) ->
	IRI0#{scheme => maps:get(scheme, Base)};
resolve_relative_iri_map(#{path := <<"">>} = IRI0, Base) ->
	IRI1 = IRI0#{scheme => maps:get(scheme, Base),
				 host => maps:get(host, Base),
				 path => maps:get(path, Base)},
	case maps:get(query, IRI0, maps:get(query, Base, none)) of
		none -> IRI1;
		Q ->    IRI1#{query => Q}
	end;
resolve_relative_iri_map(#{path := <<$/, _/binary>>} = IRI0, Base) ->
	IRI0#{scheme => maps:get(scheme, Base),
		  host => maps:get(host, Base)};
resolve_relative_iri_map(#{path := IRIPath} = IRI0,
						 #{path := BasePath} = Base) ->
	IRI1 = case maps:get(scheme, Base, none) of
			   none -> IRI0;
			   Scheme -> IRI0#{scheme => Scheme}
		   end,
	IRI2 = case maps:get(host, Base, none) of
			   none -> IRI1;
			   Host -> IRI1#{host => Host}
		   end,
	uri_string:normalize(
	  IRI2#{path => merge_iri_paths(IRIPath, BasePath,
									maps:get(host, Base, has_no_host))},
	  [return_map]).

-spec merge_iri_paths(unicode:chardata(), unicode:chardata(),
					  unicode:chardata() | has_no_host) ->
							 unicode:chardata().
merge_iri_paths(IRIPath, <<"">>, BaseHost)
  when BaseHost =/= has_no_host ->
	<<"/", IRIPath/binary>>;
merge_iri_paths(IRIPath, BasePath, _) ->
	case string:split(BasePath, "/", trailing) of
		[Head, _Tail] ->
			<<Head/binary, "/", IRIPath/binary>>;
		[_NoSlash] ->
			<<"/", IRIPath/binary>>
	end.

%% This graph represents the state transitions of the parser.
%% The parser is a push-down automaton.
%% Solid arrows indicate a simple state transition. Labels on those arrows
%% indicate the terminal production for that state change. Unlabelled arrows
%% indicate a default state transition.
%% Open-headed arrows indicate a push-down into one of the boxed sub-parsers,
%% and the corresponding return from it. Labels on those arrows indicate
%% which sub-parser is entered.
%%
%% digraph {
%% graph [fontname="Helvetica"];
%% node [fontname="Helvetica"];
%% edge [fontname="Helvetica"];
%%
%% subgraph cluster_directive {
%%   graph [style="invis"];
%%
%%   // Productions 3, 4, 5, 4s, 5s
%%    0 -> "9+dot" [label="'@prefix'"];
%%   "9+dot" -> "10+dot" [label="PNAME_NS"];
%%   "10+dot" -> "14+11" [label="IRIREF"];
%%   0 -> "13+dot" [label="'@base'"];
%%   "13+dot" -> "14+11" [label="IRIREF"];
%%   "14+11" -> 0. [label="'.'"];
%%    0 ->  9 [label="\"PREFIX\""];
%%    9 -> 10 [label="PNAME_NS"];
%%   10 -> 0. [label="IRIREF"];
%%    0 -> 13 [label="\"BASE\""];
%%   13 -> 0. [label="IRIREF"];
%% }
%%
%% // Production 10: Subject
%%  0 -> 90 [label="'('"];
%% 90 -> 33 [label="')'"];
%% 90 -> 28 [label="500 collection", arrowhead="empty", style="dashed"];
%%
%% // Production 6: triples
%% 28 -> 33;
%% 25 -> 0. [label="'.'"];
%%
%%  0 -> 91 [label="'['"];
%% 91 -> 23 [label="100 predicateObjectList", arrowhead="empty", style="dashed"];
%% 23 -> 37 [label="']'"];
%% 37 -> 33;
%% 37 -> 0. [label="'.'"];
%%
%% // Production 7: predicateObjectList
%% subgraph cluster_predicateObjectList {
%%   graph [shape="box", label="predicateObjectList"];
%%   100 [shape="box"];
%%   100 -> 130 [label="IRIREF"];
%%   100 -> 130 [label="PNAME_LN"];
%%   100 -> 130 [label="PNAME_NS"];
%%   100 -> 130 [label="'a'"];
%%
%%   130 -> 131 [label="200 object", arrowhead="empty", style="dashed"];
%%   131 -> 102 [style="dotted"];
%%   102 -> 130 [label="','"];
%%   102 -> "199 return";
%%   102 -> 103 [label="';'"];
%%
%%   103 -> 138 [label="'a'"];
%%   103 -> 138 [label="IRIREF"];
%%   103 -> 138 [label="PNAME_LN"];
%%   103 -> 138 [label="PNAME_NS"];
%%   103 -> 102;
  
%%   138 -> 139 [label="200 object", arrowhead="empty", style="dashed"];
%%   139 -> 140 [style="dotted"];
%%   140 -> 138 [label="','"];
%%   140 -> 102;
%%
%%   "199 return" [shape="box"];
%% }
%%
%% // Production 12: object
%% subgraph cluster_object {
%%   graph [shape="box", label="object"];
%%   200 [shape="box"];
%%   200 -> 299 [label="IRIREF"];
%%   200 -> 299 [label="PNAME_LN"];
%%   200 -> 299 [label="PNAME_NS"];
%%
%%   200 -> 299 [label="BLANK_NODE_LABEL"];
%%   200 -> 299 [label="ANON"];
%%
%%   200 -> 290 [label="'('"];
%%   290 -> 299 [label="')'"];
%%   290 -> 209 [label="500 collection", arrowhead="empty", style="dashed"];
%%   209 -> 299;
%%
%%   200 -> 291 [label="'['"];
%%   291 -> 210 [label="100 predicateObjectList", arrowhead="empty", style="dashed"];
%%   210 -> 299 [label="']'"];
%%
%%   200 -> 230 [label="STRING_LITERAL\n_QUOTE"];
%%   200 -> 230 [label="STRING_LITERAL\n_SINGLE_QUOTE"];
%%   200 -> 230 [label="STRING_LITERAL\n_LONG_SINGLE_QUOTE"];
%%   200 -> 230 [label="STRING_LITERAL\n_LONG_QUOTE"];
%%   230 -> 299 [label="LANGTAG"];
%%   230 -> 221 [label="'^^'"];
%%   221 -> 299 [label="iri"];
%%   230 -> 299;
%%
%%   200 -> 299 [label="INTEGER"];
%%   200 -> 299 [label="DECIMAL"];
%%   200 -> 299 [label="DOUBLE"];
%%
%%   200 -> 299 [label="'true'"];
%%   200 -> 299 [label="'false'"];
%%
%%   299 [shape="box"];
%% }
%%
%% // Productions 135s and 136s: iri and PrefixedName
%% // From production 10, subject
%%  0 -> 33 [label="IRIREF"];
%%  0 -> 33 [label="PNAME_LN"];
%%  0 -> 33 [label="PNAME_NS"];
%% 33 -> 25 [label="100 predicateObjectList", arrowhead="empty", style="dashed"];
%%
%% // Production 137s: BlankNode
%% // From production 10, subject
%%  0 -> 33 [label="BLANK_NODE_LABEL"];
%%  0 -> 33 [label="ANON"];
%%
%% // Production 15: collection
%% subgraph cluster_collection {
%%   graph [shape="box", label="collection"];
%%   500 [shape="box"];
%%   500 -> 502 [label="200 object", arrowhead="empty", style="dashed"];
%%   502 -> 503 [style="dotted"];
%%   503 -> 505 [style="dotted"];
%%   505 -> 504 [label="200 object", arrowhead="empty", style="dashed"];
%%   504 -> 503 [style="dotted"];
%%   503 -> 599 [label="')'", style="dotted"];
%%   599 [shape="box"];
%% }
%% }
