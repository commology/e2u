-module(erlcount_sup).

-behavior(supervisor).

%% API
-export([start_link/0]).

%% override
-export([init/1]).


%% implementation of API

%% unnecessary to self-register
start_link() ->
    supervisor:start_link(?MODULE, []).


%% implementation of override
init([]) ->
    MaxRestart = 5,
    MaxTime = 100,
    Timeout = 60000,
    ChildSpec = {
                  dispatch,			%% child id
                  {				%% child MFA
                    erlcount_dispatch,
                    start_link,
                    []
                  },
                  transient,			%% restart
                  Timeout,			%% shutdown
                  worker,			%% type
                  [erlcount_dispatch]		%% modules
                },
    {ok,
        {
          {					%% sup flags
            one_for_one,			%% strategy
            MaxRestart,
            MaxTime
          },
          [					%% child spec
            ChildSpec
          ]
        }
    }.

