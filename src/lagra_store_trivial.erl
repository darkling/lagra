-module(lagra_store_trivial).
-behaviour(gen_server).

%% API.
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {quads                      :: [lagra_model:quad()],
				bnode_id = 1               :: integer(),
				default = {iri, "urn:nil"} :: lagra_model:graph()
	   }).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link(?MODULE, [], []).

%% gen_server.

init([]) ->
	{ok, #state{quads=[]}}.

handle_call({po_from_s, Subject}, _From, State) ->
	{reply, po_from_s(State, Subject), State};
handle_call({t_from_q, Pattern}, _From, State) ->
	{reply, t_from_q(State, Pattern), State};
handle_call({add_triple, Triple}, _From, State) ->
	{ok, NewState} = add(State, Triple),
	{reply, ok, NewState};
handle_call({add_quad, Quad}, _From, State) ->
	{ok, NewState} = add(State, Quad),
	{reply, ok, NewState};
handle_call({new_bnode}, _From, State = #state{bnode_id=Id}) ->
	{reply, {bnode, integer_to_list(Id)}, State#state{bnode_id=Id+1}};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Internals

-spec t_from_q(#state{}, lagra_model:quad_pattern()) ->
					  [lagra_model:triple()].
t_from_q(#state{quads=Quads}, {Ps, Pp, Po, Pg}) ->
	[lagra_model:quad_to_triple(Q)
	 || {S, P, O, G} = Q <- Quads,
		(Ps =:= '_') or (Ps =:= S),
		(Pp =:= '_') or (Pp =:= P),
		(Po =:= '_') or (Po =:= O),
		(Pg =:= '_') or (Pg =:= G)].

-spec add(#state{}, lagra_model:triple() | lagra_model:quad()) ->
				 {ok, #state{}}. %| {error, term()}.
add(State, {S, P, O}) ->
	Quad = {S, P, O, State#state.default},
	add(State, Quad);
add(State = #state{quads=Quads}, Quad = {_S, _P, _O, _G}) ->
	{ok, State#state{quads=[Quad|Quads]}}.


-spec po_from_s(#state{}, lagra_model:subject()) ->
					   [{lagra_model:predicate(), lagra_model:object()}].
po_from_s(#state{quads=Quads}, Subject) ->
	[{lagra_model:predicate(T), lagra_model:object(T)}
	 || T <- Quads, lagra_model:subject(T) =:= Subject].
