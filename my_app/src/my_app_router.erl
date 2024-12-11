-module(my_app_router).
-export([init/2]).
-include_lib("cowboy/include/cowboy.hrl").  % Include Cowboy header for necessary types


init(Req, State) ->
    % Here, you will define a response for the root path.
    % Example: respond to the path "/"
    {ok, State} = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, <<"Hello, World!">>, Req),
    {ok, State}.

