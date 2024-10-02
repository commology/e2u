-module(thermometer).
-vsn(1).

-behaviour(sensor).
-export([measure/0, measure/1, measure/2]).

-define(KELVIN, kelvin).
-define(CELSIUS, celsius).
-define(FAHRENHEIT, fahrenheit).
-define(ABSOLUTE_ZERO_KELVIN, 0.0).
-define(ABSOLUTE_ZERO_CELSIUS, -273.15).

-define(BLACKHOLE, blackhole).

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
    {round(rand:uniform() * 100 * 10) / 10, celsius}.

measure(Object) ->
    case Object of
        blackhole ->
            io:format("BH says: ~.2f°K~n", [?ABSOLUTE_ZERO_KELVIN]),
            {?ABSOLUTE_ZERO_KELVIN, kelvin};
        _ ->
            measure()
    end.

measure(Object, Unit) ->
    {TemperatureKelvin, celsius} = measure(Object),
    temperature_unit_conversion({TemperatureKelvin, celsius}, Unit).

%% ---------

temperature_unit_conversion({FromValue, FromUnit}, ToUnit) when FromUnit /= ToUnit ->
    case {FromUnit, ToUnit} of
        {kelvin, celsius} ->
            {FromValue + ?ABSOLUTE_ZERO_CELSIUS, celsius};
        {kelvin, fahrenheit} ->
            {FromValueCelsius, celsius} = temperature_unit_conversion({FromValue, kelvin}, celsius),
            temperature_unit_conversion({FromValueCelsius, celsius}, fahrenheit);
        {celsius, kelvin} ->
            {FromValue - ?ABSOLUTE_ZERO_CELSIUS, kelvin};
        {celsius, fahrenheit} ->
            {FromValue * 1.8 + 32, fahrenheit};
        {fahrenheit, celsius} ->
            {(FromValue - 32) / 1.8, celsius}
    end;
temperature_unit_conversion({FromValue, FromUnit}, ToUnit) when FromUnit == ToUnit ->
    {FromValue, FromUnit}.

%% =========
