-module(ppool_sup).

-behavior(supervisor).

%% API
-export([start_link/3]).

%% override
-export([init/1]).


%% Limit: number of workers the pool will accept
%% {M,F,A}: the work supervisor will need to start each worker


%% implementation of API

%% unnecessary to self-register
start_link(Name, Limit, MFA) ->
    supervisor:start_link(?MODULE, {Name, Limit, MFA}).


%% implementation of override
init({Name, Limit, MFA}) ->
    MaxRestart = 1,
    MaxTime = 3600,
    Timeout = 5000,
    ChildSpec = {
                  serv,				%% child id
                  {				%% child MFA
                    ppool_serv,
                    start_link,
                    [Name, Limit, self(), MFA]
                  },
                  permanent,			%% restart
                  Timeout,			%% shutdown
                  worker,			%% type
                  [ppool_serv]			%% modules
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

