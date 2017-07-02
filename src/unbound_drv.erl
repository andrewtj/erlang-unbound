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

-ifdef(TEST).

flush_callbacks(Port) ->
    receive #ub_callback{port = Port} -> ok
    after 0 -> ok end.

cancel_valid_test() ->
    ok = unbound:start(),
    {ok, Port} = open(),
    Q = #ub_question{name = <<"tj.id.au.">>, type = 15, class = 1},
    {ok, Id} = resolve(Port, Q),
    ok = cancel(Port, Id),
    ok = close(Port),
    flush_callbacks(Port).

cancel_invalid_test() ->
    ok = unbound:start(),
    {ok, Port} = open(),
    {error, {_Num, _String}} = cancel(Port, 101),
    ok = close(Port).

ta_test() ->
    ok = unbound:start(),
    {ok, Port1} = open(),
    {ok, _Id} = resolve(Port1, #ub_question{
        name = <<"nlnetlabs.nl.">>,
        type = 1,
        class = 1
    }),
    Result1 = receive #ub_callback{} = R1 -> R1 end,
    ?assertMatch(#ub_callback{port = Port1,
                              error = undefined,
                              result = #ub_result{secure = false}},
                              Result1),
    ok = close(Port1),
    {ok, Port2} = open(),
    ok = add_ta(Port2, <<". 3096 IN DNSKEY 257 3 8 AwEAAagAIKlVZrpC6Ia7gEzahOR+9W29euxhJhVVLOyQbSEW0O8gcCjF FVQUTf6v58fLjwBd0YI0EzrAcQqBGCzh/RStIoO8g0NfnfL2MTJRkxoX bfDaUeVPQuYEhg37NZWAJQ9VnMVDxP/VHL496M/QZxkjf5/Efucp2gaD X6RS6CXpoY68LsvPVjR0ZSwzz1apAzvN9dlzEheX7ICJBBtuA6G3LQpz W5hOA2hzCTMjJPJ8LbqF6dsV6DoBQzgul0sGIcGOYl7OyQdXfZ57relS Qageu+ipAdTTJ25AsRTAoub8ONGcLmqrAmRLKBP1dfwhYB4N7knNnulq QxA+Uk1ihz0=">>),
    {ok, _Id} = resolve(Port2, #ub_question{
        name = <<"nlnetlabs.nl.">>,
        type = 1,
        class = 1
    }),
    Result2 = receive #ub_callback{} = R2 -> R2 end,
    ?assertMatch(#ub_callback{port = Port2,
                              error = undefined,
                              result = #ub_result{secure = true}},
                              Result2),
    ok = close(Port2).

-endif.
