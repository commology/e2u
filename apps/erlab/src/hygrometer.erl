-module(hygrometer).
-vsn(1).

-behaviour(sensor).
-export([measure/0, measure/1, measure/2]).

-define(PERCENT, '%').

%% ---------
%% measure
%%   v/0
%%     / (null)
%%     . readout
%%   v/1
%%     / Object
%%     . readout
%%   v/2
%%     / Object
%%     / Unit
%%     . readout

measure() ->
    {round(rand:uniform() * 100), ?PERCENT}.

measure(Object) ->
    case Object of
        _ ->
            measure()
    end.

measure(Object, Unit) when Unit == ?PERCENT ->
    measure(Object).

%% =========
