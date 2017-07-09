-module(unbound_drv).
-include("internal.hrl").

%% API.
-export([open/0, close/1]).
-export([load/0, unload/0]).
-export([resolve/2, cancel/2]).
-export([add_ta/2, add_ta_autr/2, add_ta_file/2]).
-export([hosts/1, hosts/2]).
-export([resolvconf/1, resolvconf/2]).
-export([set_fwd/2]).
-export([get_option/2, set_option/3]).
-export([version/1]).

-define(DRIVER_NAME, ?MODULE_STRING).

-define(DRV_RESOLVE, 1).
-define(DRV_CANCEL, 2).
-define(DRV_ADD_TA, 3).
-define(DRV_ADD_TA_AUTR, 4).
-define(DRV_ADD_TA_FILE, 5).
-define(DRV_HOSTS, 6).
-define(DRV_RESOLVCONF, 7).
-define(DRV_SET_FWD, 8).
-define(DRV_GET_OPT, 9).
-define(DRV_SET_OPT, 10).
-define(DRV_VERSION, 11).

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

add_ta_autr(Port, File) when is_port(Port) andalso is_binary(File) ->
    erlang:port_call(Port, ?DRV_ADD_TA_AUTR, File).

add_ta_file(Port, File) when is_port(Port) andalso is_binary(File) ->
    erlang:port_call(Port, ?DRV_ADD_TA_FILE, File).

hosts(Port) when is_port(Port) ->
    hosts(Port, <<>>).

hosts(Port, File) when is_port(Port) andalso is_binary(File) ->
    erlang:port_call(Port, ?DRV_HOSTS, File).

resolvconf(Port) when is_port(Port) ->
    resolvconf(Port, <<>>).

resolvconf(Port, File) when is_port(Port) andalso is_binary(File) ->
    erlang:port_call(Port, ?DRV_RESOLVCONF, File).

set_fwd(Port, Addr) when is_port(Port) andalso is_binary(Addr) ->
    erlang:port_call(Port, ?DRV_SET_FWD, Addr).

get_option(Port, Key) when is_port(Port) andalso is_binary(Key) ->
    erlang:port_call(Port, ?DRV_GET_OPT, Key).

set_option(Port, Key, Value) when is_port(Port) andalso
                                  is_binary(Key) andalso
                                  is_binary(Value) ->
    erlang:port_call(Port, ?DRV_SET_OPT, {Key, Value}).

version(Port) ->
    erlang:port_call(Port, ?DRV_VERSION, nil).

% internal

priv_dir() ->
    case code:priv_dir(unbound) of
        List when is_list(List) -> List;
        {error, bad_name} ->
            filename:join(filename:dirname(code:which(?MODULE)), "../priv")
    end.

-ifdef(TEST).

cancel_valid_test() ->
    ok = unbound:start(),
    {ok, Port} = open(),
    Q = #ub_question{name = <<"tj.id.au.">>, type = 15, class = 1},
    {ok, Id} = resolve(Port, Q),
    ok = cancel(Port, Id),
    ok = close(Port),
    receive #ub_drv_callback{port = Port} -> ok
    after 0 -> ok end.

cancel_invalid_test() ->
    ok = unbound:start(),
    {ok, Port} = open(),
    ok = cancel(Port, 101),
    ok = close(Port).

receive_callback(Port) ->
    receive #ub_drv_callback{port = Port} = C -> C
    after 5000 -> erlang:error(timeout) end.

ta_test() ->
    ok = unbound:start(),
    {ok, Port1} = open(),
    {ok, Id1} = resolve(Port1, #ub_question{
        name = <<"nlnetlabs.nl.">>,
        type = 1,
        class = 1
    }),
    Result1 = receive_callback(Port1),
    ?assertMatch(#ub_drv_callback{id = Id1,
                                  error = false,
                                  result = #ub_result{secure = false}},
                                  Result1),
    ok = close(Port1),
    {ok, Port2} = open(),
    ok = add_ta(Port2, <<". 3096 IN DNSKEY 257 3 8 AwEAAagAIKlVZrpC6Ia7gEzahOR+9W29euxhJhVVLOyQbSEW0O8gcCjF FVQUTf6v58fLjwBd0YI0EzrAcQqBGCzh/RStIoO8g0NfnfL2MTJRkxoX bfDaUeVPQuYEhg37NZWAJQ9VnMVDxP/VHL496M/QZxkjf5/Efucp2gaD X6RS6CXpoY68LsvPVjR0ZSwzz1apAzvN9dlzEheX7ICJBBtuA6G3LQpz W5hOA2hzCTMjJPJ8LbqF6dsV6DoBQzgul0sGIcGOYl7OyQdXfZ57relS Qageu+ipAdTTJ25AsRTAoub8ONGcLmqrAmRLKBP1dfwhYB4N7knNnulq QxA+Uk1ihz0=">>),
    {ok, Id} = resolve(Port2, #ub_question{
        name = <<"nlnetlabs.nl.">>,
        type = 1,
        class = 1
    }),
    Result2 = receive_callback(Port2),
    ?assertMatch(#ub_drv_callback{id = Id,
                                  error = false,
                                  result = #ub_result{secure = true}},
                                  Result2),
    ok = close(Port2).

ta_file_test() ->
    {ok, Port} = open(),
    RootKey = iolist_to_binary(filename:join(priv_dir(), "root.key")),
    ok = add_ta_file(Port, RootKey),
    {ok, Id} = resolve(Port, #ub_question{
        name = <<"nlnetlabs.nl.">>,
        type = 1,
        class = 1
    }),
    Result = receive_callback(Port),
    ?assertMatch(#ub_drv_callback{id = Id,
                                  error = false,
                                  result = #ub_result{secure = true}},
                                  Result),
    ok = close(Port).

ta_autr_test() ->
    {ok, Port} = open(),
    RootKey = iolist_to_binary(filename:join(priv_dir(), "root.key")),
    TempKey = iolist_to_binary(filename:join(priv_dir(), "root.key.eunit")),
    {ok, _} = file:copy(RootKey, TempKey),
    ok = add_ta_autr(Port, TempKey),
    {ok, Id} = resolve(Port, #ub_question{
        name = <<"nlnetlabs.nl.">>,
        type = 1,
        class = 1
    }),
    Result = receive_callback(Port),
    ?assertMatch(#ub_drv_callback{id = Id,
                                  error = false,
                                  result = #ub_result{secure = true}},
                                  Result),
    ok = close(Port),
    ok = file:delete(TempKey).

set_fwd_test() ->
    set_fwd_test(
        [{"8.8.8.8", true},        % Google Public DNS
         {"208.67.222.222", false} % OpenDNS
    ]).

set_fwd_test([]) -> ok;
set_fwd_test([{Addr, Nx}|Rest]) ->
    {ok, Port} = open(),
    ok = set_fwd(Port, list_to_binary(Addr)),
    {ok, Id} = resolve(Port, #ub_question{
        name = <<"debug.opendns.com.">>,
        type = 16,
        class = 1
    }),
    Result = receive_callback(Port),
    ?assertMatch(#ub_drv_callback{id = Id,
                                  error = false,
                                  result = #ub_result{nxdomain = Nx}},
                                  Result),
    ok = close(Port),
    set_fwd_test(Rest).

hosts_test() ->
    HostsFile = iolist_to_binary(filename:join(priv_dir(), "hosts.eunit")),
    ok = file:write_file(HostsFile, <<"1.1.1.1 not-a-real-name-yet.">>),
    {ok, Port} = open(),
    ok = hosts(Port, HostsFile),
    {ok, Id} = resolve(Port, #ub_question{
        name = <<"not-a-real-name-yet.">>,
        type = 1,
        class = 1
    }),
    Result = receive_callback(Port),
    ?assertMatch(#ub_drv_callback{id = Id,
                                  error = false,
                                  result = #ub_result{data = [<<1,1,1,1>>]}},
                                  Result),
    ok = close(Port),
    ok = file:delete(HostsFile).

resolvconf_test() ->
    ResolvConf = iolist_to_binary(filename:join(priv_dir(), "resolvconf.eunit")),
    resolvconf_test(
        ResolvConf,
        [{"8.8.8.8", true},        % Google Public DNS
         {"208.67.222.222", false} % OpenDNS
    ]).

resolvconf_test(ResolvConf, []) -> ok = file:delete(ResolvConf);
resolvconf_test(ResolvConf, [{Addr, Nx}|Rest]) ->
    ok = file:write_file(ResolvConf, "nameserver " ++ Addr),
    {ok, Port} = open(),
    ok = resolvconf(Port, ResolvConf),
    {ok, Id} = resolve(Port, #ub_question{
        name = <<"debug.opendns.com.">>,
        type = 16,
        class = 1
    }),
    Result = receive_callback(Port),
    ?assertMatch(#ub_drv_callback{id = Id,
                                  error = false,
                                  result = #ub_result{nxdomain = Nx}},
                                  Result),
    ok = close(Port),
    resolvconf_test(ResolvConf, Rest).

opt_test() ->
    {ok, Port} = open(),
    {ok, Value} = get_option(Port, <<"qname-minimisation">>),
    NewValue = case Value of
        <<"yes">> -> <<"no">>;
        <<"no">> -> <<"yes">>
    end,
    ok = set_option(Port, <<"qname-minimisation:">>, NewValue),
    {ok, NewValue} = get_option(Port, <<"qname-minimisation">>),
    ok = close(Port).

version_test() ->
    {ok, Port} = open(),
    {ok, Ver} = version(Port),
    true = is_binary(Ver),
    ok = close(Port).

-endif.
