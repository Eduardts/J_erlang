-module(my_app_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, _Pid} = my_app_sup:start_link(),
    start_http_server(),
    ok.

stop(_State) ->
    ok.

start_http_server() ->
    Dispatch = cowboy_router:compile([{'_', my_app_router}]),
    {ok, _} = cowboy:start_clear(my_http_listener, 100, 
            [{port, 8080}],
            #{env => #{dispatch => Dispatch}}).

