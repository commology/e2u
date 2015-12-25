-module(ppool_serv).

-behavior(gen_server).

%% API
-export([start/4, start_link/4, run/2, sync_queue/2, async_queue/2, stop/1]).

%% override
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

%% definition
-record(state,
    {
      limit = 0,
      sup,
      refs,
      queue = queue:new()
    }
).


%% implementation of API

start(Name, Limit, Sup, MFA) when is_atom(Name), is_integer(Limit) ->
    gen_server:start({local, Name}, ?MODULE, {Limit, MFA, Sup}, []).

start_link(Name, Limit, Sup, MFA) when is_atom(Name), is_integer(Limit) ->
    gen_server:start_link({local, Name}, ?MODULE, {Limit, MFA, Sup}, []).

run(Name, Args) ->
    gen_server:call(Name, {run, Args}).

sync_queue(Name, Args) ->
    gen_server:call(Name, {sync, Args}, infinity).

async_queue(Name, Args) ->
    gen_server:cast(Name, {async, Args}).

stop(Name) ->
    gen_server:call(Name, stop).


%% implementation of override

init({Limit, MFA, Sup}) ->
    self() ! {start_worker_sup, Sup, MFA},
    {ok, #state{limit = Limit, refs = gb_sets:empty()}}.


handle_call({run, Args}, _From, S = #state{limit = N, sup = Sup, refs = R}) when N > 0 ->
    {ok, Pid} = supervisor:start_child(Sup, Args),
    Ref = erlang:monitor(process, Pid),
    {reply, {ok, Pid}, S#state{limit = N-1, refs = gb_sets:add(Ref, R)}};

handle_call({run, _Args}, _From, S = #state{limit = N}) when N =< 0 ->
    {reply, noalloc, S};

handle_call({sync, Args}, _From, S = #state{limit = N, sup = Sup, refs = R}) when N > 0 ->
    {ok, Pid} = supervisor:start_child(Sup, Args),
    Ref = erlang:monitor(process, Pid),
    {reply, {ok, Pid}, S#state{limit = N-1, refs = gb_sets:add(Ref, R)}};

%% hold-on
handle_call({sync, Args}, From, S = #state{queue = Q}) ->
    {noreply, S#state{queue = queue:in({From, Args}, Q)}};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Msg, _From, State) ->
    {noreply, State}.


handle_cast({async, Args}, S = #state{limit = N, sup = Sup, refs = R}) when N > 0 ->
    {ok, Pid} = supervisor:start_child(Sup, Args),
    Ref = erlang:monitor(process, Pid),
    {noreply, S#state{limit = N-1, refs = gb_sets:add(Ref, R)}};

handle_cast({async, Args}, S = #state{limit = N, queue = Q}) when N =< 0 ->
    {noreply, S#state{queue = queue:in(Args, Q)}};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({start_worker_sup, Sup, MFA}, S = #state{}) ->
    Timeout = 10000,
    ChildSpec = {
                  worker_sup,			%% child id
                  {				%% child MFA
                    ppool_worker_sup,
                    start_link,
                    [MFA]
                  },
                  permanent,			%% restart
                  Timeout,			%% shutdown
                  supervisor,			%% type
                  [ppool_worker_sup]		%% modules
                },
    {ok, Pid} = supervisor:start_child(Sup, ChildSpec),
    {noreply, S#state{sup = Pid}};

handle_info({'DOWN', Ref, process, _Pid, _}, S = #state{refs = Refs}) ->
    io:format("received DOWN msg~n"),
    case gb_sets:is_element(Ref, Refs) of
        true ->
            handle_down_worker(Ref, S);
        false ->
            {noreply, S}
    end;

handle_info(Msg, State) ->
    io:format("Unknown msg: ~p~n", [Msg]),
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.


%% implementation staff

handle_down_worker(Ref, S = #state{limit = L, sup = Sup, refs = Refs}) ->
    case queue:out(S#state.queue) of
        {{value, {From, Args}}, Q} ->
            {ok, Pid} = supervisor:start_child(Sup, Args),
            NewRef = erlang:monitor(process, Pid),
            NewRefs = gb_sets:insert(NewRef, gb_sets:delete(Ref, Refs)),
            gen_server:reply(From, {ok, Pid}),	%% reply to the 'hold-on'
            {noreply, S#state{refs = NewRefs, queue = Q}};
        {{value, Args}, Q} ->
            {ok, Pid} = supervisor:start_child(Sup, Args),
            NewRef = erlang:monitor(process, Pid),
            NewRefs = gb_sets:insert(NewRef, gb_sets:delete(Ref, Refs)),
            {noreply, S#state{refs = NewRefs, queue = Q}};
        {empty, _} ->
            {noreply, S#state{limit = L+1, refs = gb_sets:delete(Ref, Refs)}}
    end.



