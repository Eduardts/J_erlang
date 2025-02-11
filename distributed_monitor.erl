-module(distributed_monitor).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_info/2]).

-record(state, {
    nodes = [],
    heartbeat_interval = 5000
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    net_kernel:monitor_nodes(true),
    schedule_heartbeat(),
    {ok, #state{nodes = nodes()}}.

handle_info({nodeup, Node}, State) ->
    NewState = State#state{
        nodes = [Node | State#state.nodes]
    },
    sync_state(Node),
    {noreply, NewState};

handle_info({nodedown, Node}, State) ->
    NewState = State#state{
        nodes = lists:delete(Node, State#state.nodes)
    },
    handle_node_failure(Node),
    {noreply, NewState}.

sync_state(Node) ->
    DeviceState = gen_server:call(device_manager, get_all_devices),
    IAMState = gen_server:call(iam_server, get_state),
    rpc:call(Node, state_sync, apply_state, [DeviceState, IAMState]).

%%% Application configuration
application:start(mnesia).
application:start(sasl).
application:start(device_management).

% Start distributed nodes
net_kernel:start(['node1@localhost', 'node2@localhost']).
