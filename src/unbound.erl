-module(unbound).
-export([do/0]).

-define(DRV_RESOLVE, 1).
-define(DRV_CANCEL, 2).

-record(resolve, {error, result}).
-record(question, {name, type, class}).
-record(result, {
    question :: #question{},
    data,
    canonname,
    rcode,
    answer_packet,
    havedata,
    nxdomain,
    secure,
    bogus,
    why_bogus,
    ttl
}).

do() ->
    _ = application:start(crypto),
    _ = application:start(unbound),
    ok = unbound_drv:load(),
    Port = erlang:open_port({spawn_driver, "unbound_drv"},[binary]),
    Q = {<<"tj.id.au.">>, 15, 1},
    io:format("Q: ~p~n", [Q]),
    {ok, Id} = erlang:port_call(Port, ?DRV_RESOLVE, Q),
    io:format("async_id ~p~n", [Id]),
    ok = erlang:port_call(Port, ?DRV_CANCEL, Id),
    io:format("async_id ~p cancelled~n", [Id]),
    {error, {_,_} = Err} = erlang:port_call(Port, ?DRV_CANCEL, Id+1),
    io:format("error from cancelling bad async_id: ~p~n", [Err]),
    fl().

fl() -> 
    receive
        {Port, #resolve{error = E, result = R}} ->
            io:format("result from ~p~n"
                      "    error: ~p~n"
                      " response: ~p~n",
                      [Port, E, lists:zip(record_info(fields, result), tl(tuple_to_list(R)))]);
        X ->
            io:format("recv: ~p~n", [X])
    end,
    fl().
