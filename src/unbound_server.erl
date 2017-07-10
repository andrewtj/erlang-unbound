-module(unbound_server).
-behaviour(gen_server).
-export_type([async_ref/0]).
-opaque async_ref() :: {integer(), reference()}.

-include("internal.hrl").

%% API.
-export([start_link/0, start_link/1]).
-export([resolve/2, resolve/3, resolve/4, cancel/2, cancel_noflush/2]).
-export([defaults/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(client, {ref, pid}).
-record(request, {
    q :: #ub_question{},
    async_id,
    clients :: [#client{}]
}).
-record(state, {port, requests = []}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    start_link(defaults()).

start_link(Opts) when is_list(Opts) ->
    case lists:keyfind(register, 1, Opts) of
        {register, ServerName} ->
            gen_server:start_link(ServerName, ?MODULE, Opts, []);
        false -> gen_server:start_link(?MODULE, Opts, [])
    end.

resolve(ServerRef, Name, Type) ->
    resolve(ServerRef, Name, Type, ?UB_CL_IN).

resolve(ServerRef, Name, Type, Class) ->
    resolve(ServerRef, #ub_question{name = Name, type = Type, class = Class}).

resolve(ServerRef, #ub_question{name = N} = Q) when is_list(N) ->
    resolve(ServerRef, Q#ub_question{name = iolist_to_binary(N)});
resolve(ServerRef, #ub_question{name = N, type = T, class = C} = Q)
    when is_binary(N) andalso
         is_integer(T), T > 0, T < 65535 andalso
         is_integer(C), C > 0, C < 65535 ->
    gen_server:call(ServerRef, {resolve, Q}).

cancel(ServerRef, AsyncRef) ->
    Result = cancel_noflush(ServerRef, AsyncRef),
    receive #ub_callback{ref = AsyncRef} -> Result
    after 0 -> Result end.

cancel_noflush(ServerRef, AsyncRef) ->
    gen_server:call(ServerRef, {cancel, AsyncRef}).

defaults() ->
    Args = application:get_env(unbound, server_defaults, []),
    true = is_list(Args),
    false = lists:keymember(register, 1, Args),
    Args.

%% gen_server.

init(Opts) ->
    {ok, Port} = unbound_drv:open(),
    IsMain = whereis(unbound) =:= self(),
    init(Port, IsMain, Opts).

init(Port, _IsMain, []) ->
    {ok, #state{port = Port}};
init(Port, IsMain, [Opt|Opts]) ->
    [ ok = apply(unbound_drv, Function, [Port|Args])
      || {Function, Args} <- init_drv_sig(IsMain, Opt) ],
    init(Port, IsMain, Opts).

init_drv_sig(_IsMain, {register, _ServerRef}) -> [];
init_drv_sig(IsMain, {trust_anchor, auto}) ->
    init_drv_sig(IsMain, {trust_anchor, {auto, {priv, "root.key"}}});
init_drv_sig(IsMain, {trust_anchor, read}) ->
    init_drv_sig(IsMain, {trust_anchor, {read, {priv, "root.key"}}});
init_drv_sig(true, {trust_anchor, {auto, Path}}) ->
    init_drv_sig(true, {trust_anchor, {maintain, Path}});
init_drv_sig(false, {trust_anchor, {auto, Path}}) ->
    init_drv_sig(false, {trust_anchor, {read, Path}});
init_drv_sig(_IsMain, {trust_anchor, {maintain, Path}}) ->
    [{add_ta_autr, [init_drv_sig_path(Path)]}];
init_drv_sig(_IsMain, {trust_anchor, {read, Path}}) ->
    [{add_ta_file, [init_drv_sig_path(Path)]}];
init_drv_sig(_IsMain, {trust_anchor, Data}) ->
    [{add_ta, [iolist_to_binary(Data)]}];
init_drv_sig(_IsMain, hosts) ->
    [{hosts, []}];
init_drv_sig(_IsMain, {hosts, Path}) ->
    [{hosts, [init_drv_sig_path(Path)]}];
init_drv_sig(_IsMain, resolvconf) ->
    [{resolvconf, []}];
init_drv_sig(_IsMain, {resolvconf, Path}) ->
    [{resolvconf, [init_drv_sig_path(Path)]}];
init_drv_sig(_IsMain, {forwarders, Addrs})
    when is_list(Addrs)
          andalso not is_integer(hd(Addrs)) ->
    [{set_fwd, [init_drv_addr(Addr)]} || Addr <- Addrs ];
init_drv_sig(_IsMain, {forwarders, Addr}) ->
    [{set_fwd, [init_drv_addr(Addr)]}];
init_drv_sig(IsMain, {K, V}) when is_list(K) andalso is_integer(hd(K)) ->
    init_drv_sig(IsMain, {iolist_to_binary(K), V});
init_drv_sig(IsMain, {K, V}) when is_list(V) andalso is_integer(hd(V)) ->
    init_drv_sig(IsMain, {K, iolist_to_binary(V)});
init_drv_sig(_IsMain, {K, V}) when is_binary(K) andalso is_binary(V) ->
    [{set_option, [K, V]}].

init_drv_sig_path({priv, Path}) ->
    iolist_to_binary(filename:join(unbound:priv_dir(), Path));
init_drv_sig_path(Path) ->
    iolist_to_binary(Path).

init_drv_addr(Addr) when is_tuple(Addr) ->
    S = inet:ntoa(Addr),
    true = is_list(S),
    iolist_to_binary(S);
init_drv_addr(Addr) ->
    iolist_to_binary(Addr).

handle_call({resolve, #ub_question{} = Q}, {Pid, _},
            #state{port = Port, requests = Reqs} = State) ->
    MRef = erlang:monitor(process, Pid),
    Client = #client{
        ref = MRef,
        pid = Pid
    },
    case lists:keytake(Q, #request.q, Reqs) of
        {value, #request{async_id = Id, clients = Clients} = Cur, Reqs0} ->
            Cur0 = Cur#request{clients = [Client|Clients]},
            Reqs1 = [Cur0|Reqs0],
            State0 = State#state{requests = Reqs1},
            {reply, {ok, {Id, MRef}}, State0};
        false ->
            case unbound_drv:resolve(Port, Q) of
                {ok, Id} ->
                    New = #request{q = Q, async_id = Id, clients = [Client]},
                    Reqs0 = [New|Reqs],
                    State0 = State#state{requests = Reqs0},
                    {reply, {ok, {Id, MRef}}, State0};
                {error, _} = Err ->
                    _ = erlang:demonitor(MRef, [flush]),
                    {reply, Err, State}
            end
    end;
handle_call({cancel, {Id, MRef}}, _,
            #state{port = Port, requests = Reqs} = State) ->
    _ = erlang:demonitor(MRef, [flush]),
    case lists:keytake(Id, #request.async_id, Reqs) of
        {value, #request{clients = [#client{ref = MRef}]}, Reqs0} ->
            ok = unbound_drv:cancel(Port, Id),
            State0 = State#state{requests = Reqs0},
            {reply, ok, State0};
        {value, #request{clients = Clients} = Cur, Reqs0} ->
            Clients0 = lists:keydelete(MRef, #client.ref, Clients),
            Cur0 = Cur#request{clients = Clients0},
            Reqs1 = [Cur0|Reqs0],
            State0 = State#state{requests = Reqs1},
            {reply, ok, State0};
        false ->
            {reply, ok, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', MRef, process, _Pid, _Reason},
            #state{port = Port, requests = Reqs} = State) ->
    Reqs1 = lists:filtermap(
        fun(#request{async_id = Id, clients = [#client{ref = R}]})
            when R =:= MRef ->
                ok = unbound_drv:cancel(Port, Id),
                false;
            (#request{clients = Clients} = Reqs0) ->
                Clients0 = lists:keydelete(MRef, #client.ref, Clients),
                {true, Reqs0#request{clients = Clients0}}
        end,
        Reqs
    ),
    State0 = State#state{requests = Reqs1},
    {noreply, State0};
handle_info(#ub_drv_callback{port = Port,
                             id = Id,
                             error = Error,
                             result = Result},
            #state{port = Port, requests = Reqs} = State) ->
    case lists:keytake(Id, #request.async_id, Reqs) of
        {value, #request{clients = Clients}, Reqs0} ->
            lists:foreach(fun(#client{ref = Ref, pid = Pid}) ->
                _ = erlang:demonitor(Ref, [flush]),
                Pid ! #ub_callback{
                    process = self(),
                    ref = {Id, Ref},
                    error = Error,
                    result = Result
                }
            end, Clients),
            State0 = State#state{requests = Reqs0},
            {noreply, State0};
        false ->
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{port = Port}) ->
    ok = unbound_drv:close(Port).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-ifdef(TEST).

opt_set_option_test() ->
    Cases = [{{"k", "v"}, [<<"k">>, <<"v">>]},
             {{<<"k">>, <<"v">>}, [<<"k">>, <<"v">>]}],
    opt_set_option_test(Cases).

opt_set_option_test([]) -> ok;
opt_set_option_test([{In, Out}|Cases]) ->
    Expect = [{set_option, Out}],
    Expect = init_drv_sig(false, In),
    opt_set_option_test(Cases).

opt_register_test() ->
    Cases = [{local, name}, {global, name}, {via, mod, name}],
    opt_register_test(Cases).

opt_register_test([]) -> ok;
opt_register_test([Case|Cases]) ->
    [] = init_drv_sig(false, {register, Case}),
    opt_register_test(Cases).

opt_trust_anchor_test() ->
    PrivDir = unbound:priv_dir(),
    DefaultKey = iolist_to_binary(filename:join(PrivDir, "root.key")),
    Cases = [
        {true, auto, [{add_ta_autr, [DefaultKey]}]},
        {false, auto, [{add_ta_file, [DefaultKey]}]},
        {false, read, [{add_ta_file, [DefaultKey]}]},
        {true, {auto, {priv, "root.key"}}, [{add_ta_autr, [DefaultKey]}]},
        {false, {auto, {priv, "root.key"}}, [{add_ta_file, [DefaultKey]}]},
        {false, {maintain, {priv, "root.key"}}, [{add_ta_autr, [DefaultKey]}]},
        {false, {read, {priv, "root.key"}}, [{add_ta_file, [DefaultKey]}]},
        {true, {auto, "path"}, [{add_ta_autr, [<<"path">>]}]},
        {false, {auto, "path"}, [{add_ta_file, [<<"path">>]}]},
        {false, {read, "path"}, [{add_ta_file, [<<"path">>]}]},
        {false, {maintain, "path"}, [{add_ta_autr, [<<"path">>]}]},
        {false, "data", [{add_ta, [<<"data">>]}]},
        {false, <<"data">>, [{add_ta, [<<"data">>]}]}
    ],
    opt_trust_anchor_test(Cases).

opt_trust_anchor_test([]) -> ok;
opt_trust_anchor_test([{IsMain, In, Out}|Cases]) ->
    Out = init_drv_sig(IsMain, {trust_anchor, In}),
    opt_trust_anchor_test(Cases).

opt_hosts_test() ->
    PrivDir = unbound:priv_dir(),
    PrivDirHosts = iolist_to_binary(filename:join(PrivDir, "hosts")),
    Cases = [{hosts, []},
             {{hosts, {priv, "hosts"}}, [PrivDirHosts]},
             {{hosts, "path"}, [<<"path">>]},
             {{hosts, <<"path">>}, [<<"path">>]}],
    opt_hosts_test(Cases).

opt_hosts_test([]) -> ok;
opt_hosts_test([{In, Out}|Cases]) ->
    Expect = [{hosts, Out}],
    Expect = init_drv_sig(false, In),
    opt_hosts_test(Cases).

opt_resolvconf_test() ->
    PrivDir = unbound:priv_dir(),
    PrivDirConf = iolist_to_binary(filename:join(PrivDir, "resolv.conf")),
    Cases = [{resolvconf, []},
             {{resolvconf, {priv, "resolv.conf"}}, [PrivDirConf]},
             {{resolvconf, "path"}, [<<"path">>]},
             {{resolvconf, <<"path">>}, [<<"path">>]}],
    opt_resolvconf_test(Cases).

opt_resolvconf_test([]) -> ok;
opt_resolvconf_test([{In, Out}|Cases]) ->
    Expect = [{resolvconf, Out}],
    Expect = init_drv_sig(false, In),
    opt_resolvconf_test(Cases).

opt_forwarders_test() ->
    Ipv4S = "127.0.0.1",
    Ipv4B = list_to_binary(Ipv4S),
    {ok, Ipv4T} = inet:parse_ipv4_address(Ipv4S),
    Ipv6S = "::1",
    Ipv6B = list_to_binary(Ipv6S),
    {ok, Ipv6T} = inet:parse_ipv6_address(Ipv6S),
    Ips = [Ipv4S, Ipv4B, Ipv4T, Ipv6S, Ipv6B, Ipv6T],
    IpBs = [Ipv4B, Ipv4B, Ipv4B, Ipv6B, Ipv6B, Ipv6B],
    Cases = [{Ipv4S, [Ipv4B]},
             {Ipv4B, [Ipv4B]},
             {Ipv4T, [Ipv4B]},
             {Ipv6S, [Ipv6B]},
             {Ipv6B, [Ipv6B]},
             {Ipv6T, [Ipv6B]},
             {Ips, IpBs}],
    opt_forwarders_test(Cases).

opt_forwarders_test([]) -> ok;
opt_forwarders_test([{In, Out}|Cases]) ->
    Expect = [{set_fwd, [X]} || X <- Out],
    Expect = init_drv_sig(false, {forwarders, In}),
    opt_forwarders_test(Cases).

-endif.