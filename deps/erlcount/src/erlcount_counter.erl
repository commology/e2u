-module(erlcount_counter).

-behavior(gen_server).

%% API
-export([start_link/4]).

%% override
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% definition
-record(state,
    {
      dispatcher,
      ref,
      file,
      re
    }
).

%% implementation of API

%% unnecessary to self-register
start_link(DispatcherPid, Ref, FileName, Regex) ->
    gen_server:start_link(?MODULE, [DispatcherPid, Ref, FileName, Regex], []).


%% implementation of override

init([DispatcherPid, Ref, FileName, Regex]) ->
    self() ! start,
    {ok, #state{
                 dispatcher = DispatcherPid,
                 ref = Ref,
                 file = FileName,
                 re = Regex
               }}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(start, S = #state{re = Re, ref = Ref}) ->
    {ok, Bin} = file:read_file(S#state.file),
    Count = erlcount_lib:regex_count(Re, Bin),
    erlcount_dispatch:complete(S#state.dispatcher, Re, Ref, Count),
    {stop, normal, S}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

