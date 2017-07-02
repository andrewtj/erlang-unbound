-module(unbound).
-export([do/0]).

-include("unbound.hrl").

do() ->
    _ = application:start(crypto),
    _ = application:start(unbound),
    {ok, Port} = unbound_drv:open(),
    Q = #ub_question{
        name = <<"tj.id.au.">>,
        type = 15,
        class = 1
    },
    io:format("Q: ~p~n", [Q]),
    {ok, Id} = unbound_drv:resolve(Port, Q),
    io:format("async_id ~p~n", [Id]),
    ok = unbound_drv:cancel(Port, Id),
    io:format("async_id ~p cancelled~n", [Id]),
    {error, {_,_} = Err} = unbound_drv:cancel(Port, Id+1),
    io:format("error from cancelling bad async_id: ~p~n", [Err]),
    secure_test(false),
    secure_test(true),
    {ok, _Id2} = unbound_drv:resolve(Port, Q),
    fl().

secure_test(AddTa) ->
    {ok, Port} = unbound_drv:open(),
    case AddTa of
        true -> unbound_drv:add_ta(Port, <<". 3096 IN DNSKEY 257 3 8 AwEAAagAIKlVZrpC6Ia7gEzahOR+9W29euxhJhVVLOyQbSEW0O8gcCjF FVQUTf6v58fLjwBd0YI0EzrAcQqBGCzh/RStIoO8g0NfnfL2MTJRkxoX bfDaUeVPQuYEhg37NZWAJQ9VnMVDxP/VHL496M/QZxkjf5/Efucp2gaD X6RS6CXpoY68LsvPVjR0ZSwzz1apAzvN9dlzEheX7ICJBBtuA6G3LQpz W5hOA2hzCTMjJPJ8LbqF6dsV6DoBQzgul0sGIcGOYl7OyQdXfZ57relS Qageu+ipAdTTJ25AsRTAoub8ONGcLmqrAmRLKBP1dfwhYB4N7knNnulq QxA+Uk1ihz0=">>);
        false -> undefined
    end,
    {ok, _Id} = unbound_drv:resolve(Port, #ub_question{
        name = <<"nlnetlabs.nl.">>,
        type = 1,
        class = 1
    }),
    receive
        #ub_callback{port = Port, error = E, result = R} ->
            io:format("secure response? ~p (~p)~n", [R#ub_result.secure, E]);
        other ->
            io:format("unexpected response: ~p~n", [other])
    end,
    ok = unbound_drv:close(Port).

fl() -> 
    receive
        #ub_callback{port = Port, error = E, result = R} ->
            io:format("result from ~p~n"
                      "    error: ~p~n"
                      " response: ~p~n",
                      [Port, E, lists:zip(record_info(fields, ub_result), tl(tuple_to_list(R)))]);
        X ->
            io:format("recv: ~p~n", [X])
    end,
    fl().
