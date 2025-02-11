-module(iam_server).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2]).
-export([verify_permission/2, grant_permission/3, revoke_permission/2]).

-record(state, {
    permissions = #{},
    roles = #{},
    access_logs = []
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_call({verify_permission, EntityId, Permission}, _From, State) ->
    Result = case maps:get(EntityId, State#state.permissions, undefined) of
        undefined -> false;
        Permissions -> lists:member(Permission, Permissions)
    end,
    {reply, Result, State};

handle_call({grant_permission, EntityId, Permission, TTL}, _From, State) ->
    CurrentPerms = maps:get(EntityId, State#state.permissions, []),
    NewPerms = [Permission | CurrentPerms],
    NewState = State#state{
        permissions = maps:put(EntityId, NewPerms, State#state.permissions)
    },
    schedule_permission_expiry(EntityId, Permission, TTL),
    {reply, ok, NewState}.
