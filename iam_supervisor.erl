-module(iam_supervisor).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },
    
    Children = [
        #{
            id => iam_server,
            start => {iam_server, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [iam_server]
        },
        #{
            id => access_log_analyzer,
            start => {access_log_analyzer, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [access_log_analyzer]
        }
    ],
    
    {ok, {SupFlags, Children}}.
