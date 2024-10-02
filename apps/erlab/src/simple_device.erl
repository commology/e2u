-module(simple_device).
-vsn(1).

-behaviour(device).
-export([init/1]).

%% ---------

init(InitialState) ->
    InitialState.

%% =========
