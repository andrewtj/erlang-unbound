-module(unbound_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Procs = [{unbound_server,
              {unbound_server, start_link, [[{register, {local, unbound}}]]},
              permanent, 60, worker, [unbound]}],
    {ok, {{one_for_one, 1, 5}, Procs}}.
