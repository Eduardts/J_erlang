-module(device_manager).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2]).
-export([register_device/1, update_status/2, get_device/1]).

-record(state, {
    devices = #{},  % Map of device_id to device data
    monitors = #{}, % Monitor references for connected devices
    iam_cache = #{} % Cached IAM permissions
}).

-record(device, {
    id,
    status,
    last_seen,
    permissions
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call({register_device, DeviceId}, _From, State) ->
    case maps:is_key(DeviceId, State#state.devices) of
        true ->
            {reply, {error, already_registered}, State};
        false ->
            Device = #device{
                id = DeviceId,
                status = initializing,
                last_seen = erlang:system_time(seconds),
                permissions = fetch_device_permissions(DeviceId)
            },
            NewState = State#state{
                devices = maps:put(DeviceId, Device, State#state.devices)
            },
            {reply, {ok, Device}, NewState}
    end;

handle_call({get_device, DeviceId}, _From, State) ->
    Response = case maps:get(DeviceId, State#state.devices, not_found) of
        not_found -> {error, device_not_found};
        Device -> {ok, Device}
    end,
    {reply, Response, State}.

handle_cast({update_status, DeviceId, NewStatus}, State) ->
    NewState = case maps:get(DeviceId, State#state.devices, not_found) of
        not_found ->
            State;
        Device ->
            UpdatedDevice = Device#device{
                status = NewStatus,
                last_seen = erlang:system_time(seconds)
            },
            State#state{
                devices = maps:put(DeviceId, UpdatedDevice, State#state.devices)
            }
    end,
    {noreply, NewState}.
