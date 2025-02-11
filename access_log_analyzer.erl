-module(access_log_analyzer).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2]).
-export([analyze_logs/1, get_statistics/0]).

-record(state, {
    log_buffer = [],
    statistics = #{},
    alert_threshold = 10
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    schedule_log_analysis(),
    {ok, #state{}}.

handle_cast({log_access, AccessData}, State) ->
    NewBuffer = [AccessData | State#state.log_buffer],
    NewState = case length(NewBuffer) >= State#state.alert_threshold of
        true ->
            analyze_buffer(NewBuffer, State);
        false ->
            State#state{log_buffer = NewBuffer}
    end,
    {noreply, NewState}.

analyze_buffer(Buffer, State) ->
    Statistics = analyze_access_patterns(Buffer),
    case detect_anomalies(Statistics) of
        {true, Anomalies} ->
            alert_security_team(Anomalies);
        false ->
            ok
    end,
    State#state{
        log_buffer = [],
        statistics = maps:merge(State#state.statistics, Statistics)
    }.
