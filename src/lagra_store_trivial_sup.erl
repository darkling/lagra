-module(lagra_store_trivial_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [#{id => store,
			   start => {lagra_store_trivial, start, []}}
			],
	{ok, {{one_for_one, 1, 5}, Procs}}.
