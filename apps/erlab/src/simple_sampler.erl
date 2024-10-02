-module(simple_sampler).
-vsn(1).

-behaviour(sampler).
-export([init/1]).

%% ---------

init(InitialState) ->
    io:format("[~s][INIT]~n", [
        ?MODULE
    ]),
    InitialState.

%% =========
