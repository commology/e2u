-module(sampler).
-vsn(1).

-export([start/2, stop/1]).
-export([call/4, install/2, uninstall/2]).

-record(state, {counter, devicePid}).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{init, 1}];
behaviour_info(_) ->
    undefined.

start(Module, InitialCounter) ->
    InitialState = #state{
        devicePid = null,
        counter = InitialCounter
    },
    spawn(fun() -> init(Module, InitialState) end).

call(SamplerPid, DevicePid, Verb, MsgBody) ->
    io:format("[~s][CALL](~p)>>> ", [
        ?MODULE,
        Verb
    ]),
    RefDeadProof = erlang:monitor(process, DevicePid),
    DevicePid ! {self(), RefDeadProof, {Verb, MsgBody}},
    receive
        {RefDeadProof, ok} ->
            io:format("successful call; demonitor ~p.~n", [DevicePid]),
            erlang:demonitor(RefDeadProof, [flush]),
            ok;
        {'DOWN', RefDeadProof, process, SamplerPid, Reason} ->
            io:format("'DOWN' event! ~p~n", [Reason]),
            {error, Reason}
    after 5000 ->
        {error, timeout}
    end.

install(SamplerPid, DevicePid) ->
    call(SamplerPid, DevicePid, install, SamplerPid),
    SamplerPid ! {install, DevicePid}.

uninstall(SamplerPid, DevicePid) ->
    call(SamplerPid, DevicePid, uninstall, SamplerPid),
    SamplerPid ! {uninstall, DevicePid}.

stop(SamplerPid) ->
    SamplerPid ! stop,
    ok.

%% ---------

init(Module, InitialState) ->
    %% overridable by specific
    OverridedInitialState = Module:init(InitialState),
    io:format("[~s/~s][INIT]{state.devicePid=~p}~n", [
        ?MODULE,
        Module,
        OverridedInitialState#state.devicePid
    ]),
    loop(OverridedInitialState).

loop(State = #state{devicePid = DevPid}) ->
    io:format("[~s][LOOP]{state.devicePid=~p}>>> ~n", [
        ?MODULE,
        DevPid
    ]),
    receive
        {install, DevicePid} ->
            NewState = State#state{devicePid = DevicePid},
            loop(NewState);
        {uninstall, DevicePid} ->
            DevicePid,
            NewState = State#state{devicePid = null},
            loop(NewState);
        stop ->
            case State#state.devicePid of
                null ->
                    ok;
                _ ->
                    io:format("STOP~n"),
                    uninstall(self(), DevPid)
            end;
        Msg ->
            io:format("{state.counter=~p}~p.~n", [State#state.counter, Msg]),
            case State#state.counter of
                0 ->
                    ok;
                _ ->
                    NewState = decrease_state_counter(State),
                    loop(NewState)
            end
    end.

%% ---------

decrease_state_counter(State = #state{}) when State#state.counter > 0 ->
    NewState = State#state{counter = State#state.counter - 1},
    NewState;
decrease_state_counter(State = #state{}) when State#state.counter < 0 ->
    NewState = State#state{counter = 0},
    NewState;
decrease_state_counter(State = #state{}) ->
    State.

%% =========
