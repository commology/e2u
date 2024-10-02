-module(sensor).

-export([sample/1, sample/2, sample/3]).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{measure, 0}, {measure, 1}, {measure, 2}];
behaviour_info(_) ->
    undefined.

sample(Module) ->
    Module:measure().

sample(Module, Object) ->
    Module:measure(Object).

sample(Module, Object, Unit) ->
    Module:measure(Object, Unit).

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

%% measure() ->
%%     ok.

%% measure(Object) ->
%%     Object,
%%     ok.

%% measure(Object, Unit) ->
%%     Object,
%%     Unit,
%%     ok.

%% =========
