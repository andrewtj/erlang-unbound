-module(unbound_server).
-behaviour(gen_server).
-export_type([async_ref/0]).
-opaque async_ref() :: {integer(), reference()}.

-include("internal.hrl").

%% API.
-export([start_link/0, start_link/1]).
-export([resolve/2, cancel/2, cancel_noflush/2]).

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
    start_link([]).

start_link(Opts) when is_list(Opts) ->
    case proplists:get_value(register, Opts) of
        undefined -> gen_server:start_link(?MODULE, [], []);
        ServerName -> gen_server:start_link(ServerName, ?MODULE, [], [])
    end.

resolve(Pid, #ub_question{} = Q) when is_pid(Pid) orelse Pid =:= unbound ->
    gen_server:call(Pid, {resolve, Q}).

cancel(Pid, Ref) when is_pid(Pid) orelse Pid =:= unbound ->
    Result = cancel_noflush(Pid, Ref),
    receive #ub_callback{ref = Ref} -> Result
    after 0 -> Result end.

cancel_noflush(Pid, Ref) when is_pid(Pid) orelse Pid =:= unbound ->
    gen_server:call(Pid, {cancel, Ref}).

%% gen_server.

init([]) ->
    {ok, Port} = unbound_drv:open(),
    {ok, #state{port = Port}}.

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
