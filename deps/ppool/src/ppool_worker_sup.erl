-module(ppool_worker_sup).

-behavior(supervisor).

%% API
-export([start_link/1]).

%% override
-export([init/1]).


%% implementation of API

%% unnecessary to self-register
start_link(MFA = {_, _, _}) ->
    supervisor:start_link(?MODULE, MFA).


%% implementation of override
init({M, F, A}) ->
    MaxRestart = 5,
    MaxTime = 3600,
    Timeout = 5000,
    ChildSpec = {
                  ppool_worker,			%% child id
                  {				%% child MFA
                    M, F, A
                  },
                  temporary,			%% restart
                  Timeout,			%% shutdown
                  worker,			%% type
                  [M]				%% modules
                },
    {ok,
        {
          {					%% sup flags
            simple_one_for_one,			%% strategy
            MaxRestart,
            MaxTime
          },
          [					%% child spec
            ChildSpec
          ]
        }
    }.

