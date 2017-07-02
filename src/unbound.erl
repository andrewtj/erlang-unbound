-module(unbound).
-export([start/0]).

-include("unbound.hrl").

start() -> start([crypto, unbound]).

start([]) -> ok;
start([App|Rest]) ->
    case application:start(App) of
        ok -> start(Rest);
        {error, {already_started, App}} -> start(Rest);
        Other -> Other
    end.
