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
    case lists:keytake(register, 1, Opts) of
        {value, {register, ServerName}, Opts0} ->
            gen_server:start_link(ServerName, ?MODULE, Opts0, []);
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
    init(Port, Opts).

init(Port, []) ->
    {ok, #state{port = Port}};
init(Port, [Opt|Opts]) ->
    [ ok = apply(unbound_drv, Function, [Port|Args])
      || {Function, Args} <- init_drv_sig(Opt) ],
    init(Port, Opts).

init_drv_sig({trust_anchor, auto}) ->
    init_drv_sig({trust_anchor, {auto, {priv, "root.key"}}});
init_drv_sig({trust_anchor, maintain}) ->
    init_drv_sig({trust_anchor, {maintain, {priv, "root.key"}}});
init_drv_sig({trust_anchor, read}) ->
    init_drv_sig({trust_anchor, {maintain, {priv, "root.key"}}});
init_drv_sig({trust_anchor, {auto, Path}}) ->
    case whereis(unbound) =:= self() of
        true -> init_drv_sig({trust_anchor, {maintain, Path}});
        false -> init_drv_sig({trust_anchor, {read, Path}})
    end;
init_drv_sig({trust_anchor, {maintain, Path}}) ->
    [{add_ta_autr, [init_drv_sig_path(Path)]}];
init_drv_sig({trust_anchor, {read, Path}}) ->
    [{add_ta_file, [init_drv_sig_path(Path)]}];
init_drv_sig({trust_anchor, Data}) ->
    [{add_ta, [(Data)]}];
init_drv_sig(hosts) ->
    [{hosts, []}];
init_drv_sig({hosts, Path}) ->
    [{hosts, [init_drv_sig_path(Path)]}];
init_drv_sig(resolvconf) ->
    [{resolv_conf, []}];
init_drv_sig({resolvconf, Path}) ->
    [{resolv_conf, init_drv_sig_path(Path)}];
init_drv_sig({forwarders, Addrs})
    when is_list(Addrs)
          andalso not is_integer(hd(Addrs)) ->
    [{set_fwd, case is_tuple(Addr) of
        true -> iolist_to_binary(inet:ntoa(Addr));
        false -> iolist_to_binary(Addr)
      end} || Addr <- Addrs ];
init_drv_sig({forwarders, Addr}) ->
    init_drv_sig({forwarders, [Addr]}).

init_drv_sig_path({priv, Path}) ->
    PrivDir = case code:priv_dir(unbound) of
        List when is_list(List) -> List;
        {error, bad_name} ->
            filename:join(filename:dirname(code:which(?MODULE)), "../priv")
    end,
    iolist_to_binary(filename:join(PrivDir, Path));
init_drv_sig_path(Path) ->
    iolist_to_binary(Path).

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
