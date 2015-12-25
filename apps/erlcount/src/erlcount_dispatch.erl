-module(erlcount_dispatch).

-behavior(gen_fsm).

%% API
-export([start_link/0, complete/4]).

%% override
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% states
-export([dispatching/2, listening/2]).

%% definition
-define(POOL, erlcount).
-record(data,
    {
      regex = [],
      refs = []
    }
).

%% implementation of API

%% unnecessary to self-register
start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

complete(Pid, Regex, Ref, Count) ->
    gen_fsm:send_all_state_event(Pid, {complete, Regex, Ref, Count}).


%% implementation of override

init([]) ->
    {ok, Re} = application:get_env(regex),
    {ok, Dir} = application:get_env(directory),
    {ok, MaxFiles} = application:get_env(max_files),
    ppool:start_pool(?POOL, MaxFiles, {erlcount_counter, start_link, []}),
    case lists:all(fun erlcount_lib:valid_regex/1, Re) of
        true ->
            self() ! {start, Dir},
            {ok, dispatching, #data{regex = [{R,0} || R <- Re]}};
        false ->
            {stop, invalid_regex}
    end.

%% automatically into start state
handle_info({start, Dir}, State, Data) ->
    gen_fsm:send_event(self(), erlcount_lib:find_erl(Dir)),
    {next_state, State, Data}.

%% no matter current state, for all complete event
handle_event({complete, Regex, Ref, Count}, State, Data = #data{regex = Re, refs = Refs}) ->
    {Regex, OldCount} = lists:keyfind(Regex, 1, Re),
    NewRe = lists:keyreplace(Regex, 1, Re, {Regex, OldCount + Count}),
    NewData = Data#data{regex = NewRe, refs = Refs -- [Ref]},
    case State of
        dispatching ->
            {next_state, dispatching, NewData};
        listening ->
            listening(done, NewData)
    end.

handle_sync_event(Event, _From, State, Data) ->
    io:format("Unexpected event: ~p~n", [Event]),
    {next_state, State, Data}.

terminate(_Reason, _State, _Data) ->
    init:stop().

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.


%% implementation of states

dispatching({continue, File, Continuation}, Data = #data{regex = Re, refs = Refs}) ->
    F = fun({Regex, _Count}, NewRefs) ->
        Ref = make_ref(),
        ppool:async_queue(?POOL, [self(), Ref, File, Regex]),
        [Ref | NewRefs]
    end,
    NewRefs = lists:foldl(F, Refs, Re),
    gen_fsm:send_event(self(), Continuation()),
    {next_state, dispatching, Data#data{refs = NewRefs}};

dispatching(done, Data) ->
    %% This is a special case. We cannot assume that all messages have NOT been received by the time we hit 'done'.
    %% As such, we directly move to listening/2 without waiting for an external event.
    listening(done, Data).

listening(done, #data{regex = Re, refs = []}) ->	%% all received
    [io:format("Regex ~s has ~p results~n", [R, C]) || {R, C} <- Re],
    {stop, normal, done};

listening(done, Data) ->				%% entries still missing
    {next_state, listening, Data}.

