-module(device).
-vsn(1).

-export([start/2, stop/1]).

-record(state, {
    samplers,
    sensors,
    sensorReadouts,
    initial
}).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{init, 1}];
behaviour_info(_) ->
    undefined.

start(Module, Initial) ->
    InitialState = #state{
        samplers = orddict:new(),
        sensors = orddict:new(),
        sensorReadouts = orddict:new(),
        initial = Initial
    },
    spawn(fun() -> init(Module, InitialState) end).

stop(_DevicePid) ->
    ok.

%% ---------

init(Module, InitialState) ->
    %% overridable by specific
    OverridedInitialState = Module:init(InitialState),
    io:format("[~s/~s][INIT]{state.initial=~p}~n", [
        ?MODULE,
        Module,
        OverridedInitialState#state.initial
    ]),
    loop(OverridedInitialState).

loop(State = #state{initial = Ini}) ->
    io:format("[~s][LOOP]{state.initial=~p}>>> ", [
        ?MODULE,
        Ini
    ]),
    receive
        %% ---------
        {FromPid, MsgRef, {install, SamplerPid}} ->
            SamplerRef = erlang:monitor(process, SamplerPid),
            NewSamplers = orddict:store(SamplerRef, SamplerPid, State#state.samplers),
            FromPid ! {MsgRef, ok},
            io:format("installed ~p.~n", [SamplerPid]),
            loop(State#state{samplers = NewSamplers});
        {FromPid, MsgRef, {uninstall, SamplerPid}} ->
            io:format("unintalled ~p; demonitor.~n", [SamplerPid]),
            erlang:demonitor(process, SamplerPid),
            FromPid ! {MsgRef, ok},
            loop(State);
        %% ---------
        {done, Name} ->
            Name,
            ok;
        shutdown ->
            ok;
        {'DOWN', _Ref, process, _Pid, _Reason} ->
            ok;
        code_change ->
            ok;
        Unknown ->
            io:format("Unknown message: ~p~n", [Unknown]),
            loop(State)
    end.

%% ---------


%% =========
