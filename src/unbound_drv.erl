-module(unbound_drv).
-include("unbound.hrl").

%% API.
-export([open/0, close/1]).
-export([load/0, unload/0]).
-export([resolve/2, cancel/2]).
-export([add_ta/2]).

-define(DRIVER_NAME, ?MODULE_STRING).

-define(DRV_RESOLVE, 1).
-define(DRV_CANCEL, 2).
-define(DRV_ADD_TA, 3).

%% API.

load() ->
    {ok, Drivers} = erl_ddll:loaded_drivers(),
    case lists:member(?DRIVER_NAME, Drivers) of
    true -> ok;
    false ->
        case erl_ddll:load(priv_dir(), ?DRIVER_NAME) of
        ok -> ok;
        {error, Error} ->
            error_logger:error_msg(
              ?MODULE_STRING ": Error loading ~p: ~p~n",
              [?DRIVER_NAME, erl_ddll:format_error(Error)]
             ),
            {error, Error}
        end
    end.

unload() ->
    case erl_ddll:unload_driver(?DRIVER_NAME) of
        ok -> ok;
        {error, Error} ->
            error_logger:error_msg(
                ?MODULE_STRING ": Error unloading ~p: ~p~n",
                [?DRIVER_NAME, erl_ddll:format_error(Error)]
            ),
            {error, Error}
    end.

open() ->
    try {ok, erlang:open_port({spawn_driver, ?DRIVER_NAME}, [binary])}
    catch error:badarg ->
        case load() of
            ok -> {ok, erlang:open_port({spawn_driver, ?DRIVER_NAME}, [binary])};
            {error, _Reason} = Error -> Error
        end
    end.

close(Port) when is_port(Port) ->
    try erlang:port_close(Port), ok
    catch error:badarg -> ok end.

resolve(Port, #ub_question{name = N, type = T, class = C}) when is_port(Port) ->
    erlang:port_call(Port, ?DRV_RESOLVE, {N, T, C}).

cancel(Port, Id) when is_port(Port) andalso is_integer(Id) ->
    erlang:port_call(Port, ?DRV_CANCEL, Id).

add_ta(Port, TA) when is_port(Port) andalso is_binary(TA) ->
    erlang:port_call(Port, ?DRV_ADD_TA, TA).

% internal

priv_dir() ->
    case code:priv_dir(unbound) of
        List when is_list(List) -> List;
        {error, bad_name} ->
            filename:join(filename:dirname(code:which(?MODULE)), "../priv")
    end.