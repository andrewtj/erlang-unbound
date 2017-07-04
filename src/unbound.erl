-module(unbound).
-export([start/0]).
-export([resolve/3, cancel/1]).

-include("unbound.hrl").

start() -> start([crypto, unbound]).

start([]) -> ok;
start([App|Rest]) ->
    case application:start(App) of
        ok -> start(Rest);
        {error, {already_started, App}} -> start(Rest);
        Other -> Other
    end.

resolve(Name, Type, Class) ->
    Q = #ub_question{name = Name, type = Type, class = Class},
    unbound_server:resolve(unbound, Q).

cancel(Ref) ->
    unbound_server:cancel(unbound, Ref).