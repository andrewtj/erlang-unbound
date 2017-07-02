-module(unbound).
-export([do/0]).

-define(DRV_RESOLVE, 1).
-define(DRV_CANCEL, 2).
-define(DRV_ADD_TA, 3).

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
    secure_test(false),
    secure_test(true),
    fl().

secure_test(AddTa) ->
    Port = erlang:open_port({spawn_driver, "unbound_drv"},[binary]),
    case AddTa of
        true -> erlang:port_call(Port, ?DRV_ADD_TA, <<". 3096 IN DNSKEY 257 3 8 AwEAAagAIKlVZrpC6Ia7gEzahOR+9W29euxhJhVVLOyQbSEW0O8gcCjF FVQUTf6v58fLjwBd0YI0EzrAcQqBGCzh/RStIoO8g0NfnfL2MTJRkxoX bfDaUeVPQuYEhg37NZWAJQ9VnMVDxP/VHL496M/QZxkjf5/Efucp2gaD X6RS6CXpoY68LsvPVjR0ZSwzz1apAzvN9dlzEheX7ICJBBtuA6G3LQpz W5hOA2hzCTMjJPJ8LbqF6dsV6DoBQzgul0sGIcGOYl7OyQdXfZ57relS Qageu+ipAdTTJ25AsRTAoub8ONGcLmqrAmRLKBP1dfwhYB4N7knNnulq QxA+Uk1ihz0=">>);
        false -> undefined
    end,
    {ok, _Id} = erlang:port_call(Port, ?DRV_RESOLVE, {<<"nlnetlabs.nl.">>, 1, 1}),
    receive
        {Port, #resolve{error = E, result = R}} ->
            io:format("secure response? ~p (~p)~n", [R#result.secure, E]);
        other ->
            io:format("unexpected response: ~p~n", [other])
    end.

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
