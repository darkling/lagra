%% @private
-module(lagra_bnode).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([new/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {suffix :: string(),
				serial :: integer()
			   }).
-type state() :: #state{}.

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec new() -> lagra_model:bnode().
new() ->
	gen_server:call(?MODULE, new).

%% gen_server.

init([]) ->
	{ok, #state{serial=14776336+rand:uniform(458066416),
				suffix="-"++random_string()}}.

handle_call(new, _From, State) ->
	{BNode, NewState} = new_bnode(State),
	{reply, BNode, NewState}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


-spec new_bnode(state()) -> {lagra_model:bnode(), state()}.
new_bnode(State = #state{serial=916132832}) ->  % 62^5
	new_bnode(State#state{serial=0, suffix="-"++random_string()});
new_bnode(State = #state{serial=Serial, suffix=Suffix}) ->
	BNode = {bnode, b62enc(Serial) ++ Suffix},
	NewState = State#state{serial=Serial+1},
	{BNode, NewState}.

-spec random_string() -> string().
random_string() ->
	[b62char(rand:uniform(62)-1) || _ <- lists:seq(1, 11)].

-spec b62enc(integer()) -> string().
b62enc(N) ->
	b62enc(N, []).

-spec b62enc(integer(), string()) -> string().
b62enc(N, Acc) when N < 62 ->
	[b62char(N) | Acc];
b62enc(N, Acc) ->
	b62enc(N div 62, [b62char(N rem 62) | Acc]).

-spec b62char(integer()) -> integer().
b62char(N) when  0 =< N, N < 10 -> $0 + N;
b62char(N) when 10 =< N, N < 36 -> $A + N - 10;
b62char(N) when 36 =< N, N < 62 -> $a + N - 36.
