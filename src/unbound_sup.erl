-module(unbound_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Opts = [{register, {local, unbound}}|unbound_server:defaults()],
    Procs = [{unbound_server,
              {unbound_server, start_link, [Opts]},
              permanent, 60, worker, [unbound]}],
    {ok, {{one_for_one, 1, 5}, Procs}}.
