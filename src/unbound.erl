-module(unbound).
-export([start/0, stop/0]).
-export([resolve/1, resolve/2, resolve/3, cancel/1]).

-include("internal.hrl").

start() ->
    case application:ensure_all_started(unbound) of
        {ok, _} -> ok;
        Other -> Other
    end.

stop() ->
    case application:stop(unbound) of
        {error, {not_started, unbound}} -> ok;
        Other -> Other
    end.

resolve(Name, Type) ->
    resolve(Name, Type, ?UB_CL_IN).

resolve(Name, Type, Class) ->
    resolve(#ub_question{name = Name, type = Type, class = Class}).

resolve(#ub_question{} = Q) ->
    unbound_server:resolve(unbound, Q).

cancel(Ref) ->
    unbound_server:cancel(unbound, Ref).
