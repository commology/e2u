-module(ppool_supersup).

-behavior(supervisor).

%% API
-export([start_link/0, stop/0, start_pool/3, stop_pool/1]).

%% override
-export([init/1]).


%% implementation of API

%% self-register as ppool
start_link() ->
    supervisor:start_link({local, ppool}, ?MODULE, []).

%% technically, a supervisor cannot be killed in an easy way. let's do it brutally!
stop() ->
    case whereis(ppool) of
        P when is_pid(P) ->
            exit(P, kill);
        _ -> ok
    end.

%% Limit: number of workers the pool will accept
%% {M,F,A}: the work supervisor will need to start each worker
start_pool(Name, Limit, MFA) ->
    Timeout = 10500,
    ChildSpec = {
                  Name,				%% child id
                  {				%% child MFA
                    ppool_sup,
                    start_link,
                    [Name, Limit, MFA]
                  },
                  permanent,			%% restart
                  Timeout,			%% shutdown
                  supervisor,			%% type
                  [ppool_sup]			%% modules
                },
    supervisor:start_child(ppool, ChildSpec).

stop_pool(Name) ->
    supervisor:terminate_child(ppool, Name),
    supervisor:delete_child(ppool, Name).


%% implementation of override
init([]) ->
    MaxRestart = 6,
    MaxTime = 3600,
    {ok,
        {
          {					%% sup flags
            one_for_one,			%% strategy
            MaxRestart,
            MaxTime
          },
          []					%% child spec
        }
    }.

