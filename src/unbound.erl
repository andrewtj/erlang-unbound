-module(unbound).
-export([do/0]).

-define(DRV_RESOLVE, 1).

-record(question, {name, class, type}).
-record(result, {error, question, response}).
-record(response, {rcode, ttl, nxdomain, secure, bogus, why_bogus, message, answers}).

do() ->
    _ = application:start(crypto),
    _ = application:start(unbound),
    ok = unbound_drv:load(),
    Port = erlang:open_port({spawn_driver, "unbound_drv"},[binary]),
    Q = {<<"tj.id.au.">>, 15, 1},
    io:format("Q: ~p~n", [Q]),
    erlang:port_call(Port, ?DRV_RESOLVE, Q),
    fl().

dump(#question{} = Q) ->
    T = {question, lists:zip(record_info(fields, question), tl(tuple_to_list(Q)))},
    io:format("~p~n", [T]);
dump(#response{} = R) ->
    T = {response, lists:zip(record_info(fields, response), tl(tuple_to_list(R)))},
    io:format("~p~n", [T]).
fl() -> 
    receive
        {Port, #result{error = E, question = Q, response = R}} -> 
            io:format("result from ~p~nerror: ~p~n", [Port, E]),
            dump(Q),
            dump(R);
        X ->
            io:format("recv: ~p~n", [X])
    end,
    fl().