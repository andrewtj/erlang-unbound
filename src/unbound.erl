-module(unbound).
-export([start/0, stop/0]).
-export([resolve/1, resolve/2, resolve/3, cancel/1]).
-export([priv_dir/0]).

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
    unbound_server:resolve(unbound, Name, Type, Class).

resolve(#ub_question{} = Q) ->
    unbound_server:resolve(unbound, Q).

cancel(Ref) ->
    unbound_server:cancel(unbound, Ref).

priv_dir() ->
    case code:priv_dir(unbound) of
        List when is_list(List) -> List;
        {error, bad_name} ->
            filename:join(filename:dirname(code:which(?MODULE)), "../priv")
    end.
