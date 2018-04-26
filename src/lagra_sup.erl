-module(lagra_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [#{id => manager,
			   start => {lagra_mgr, start_link, []}},
			 #{id => stores_sup,
			   start => {lagra_store_trivial_sup, start_link, []},
			   type => supervisor}
			],
	{ok, {{one_for_one, 1, 5}, Procs}}.
