-module(erlcount).

-behavior(application).

%% override
-export([start/2, stop/1]).


%% implementation of override

start(normal, _Args) ->
    erlcount_sup:start_link().

stop(_State) ->
    ok.

