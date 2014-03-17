%% @doc: Statman server, sends reports and owns the ETS tables
%%
%% Every second this gen_server sends a summary and the raw data of
%% all available statistics to the installed subscribers which can
%% further aggregate, summarize or publish the statistics.
%%
-module(statman_server).
-behaviour(gen_server).

-export([start_link/1, start_link/2, start_link/3,
         add_subscriber/1, remove_subscriber/1, report/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {counters, subscribers = [], report_interval}).
-define(COUNTERS_TABLE, statman_server_counters).

%%%===================================================================
%%% API
%%%===================================================================

start_link(ReportInterval) ->
    start_link(ReportInterval, []).

start_link(ReportInterval, StartSubscribers) ->
    start_link(ReportInterval, StartSubscribers, infinity).

start_link(ReportInterval, StartSubscribers, GcInterval) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE,
                          [ReportInterval, StartSubscribers, GcInterval], []).

add_subscriber(Ref) ->
    gen_server:call(?MODULE, {add_subscriber, Ref}).

remove_subscriber(Ref) ->
    gen_server:call(?MODULE, {remove_subscriber, Ref}).

report() ->
    ?MODULE ! report.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([ReportInterval, StartSubscribers, GcInterval]) ->
    ok = statman_counter:init(),
    ok = statman_gauge:init(),
    ok = statman_histogram:init(),

    erlang:send_after(ReportInterval, self(), report),
    case GcInterval of
        infinity -> ok;
        N when is_integer(N) ->
            erlang:send_after(GcInterval, self(), {gc, GcInterval})
    end,

    {ok, #state{counters = dict:new(),
                subscribers = StartSubscribers,
                report_interval = ReportInterval}}.

handle_call({add_subscriber, Ref}, _From, #state{subscribers = Sub} = State) ->
    {reply, ok, State#state{subscribers = [Ref | Sub]}};
handle_call({remove_subscriber, Ref}, _From, #state{subscribers = Sub} = State) ->
    {reply, ok, State#state{subscribers = lists:delete(Ref, Sub)}}.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(report, #state{report_interval = Window} = State) ->
    erlang:send_after(State#state.report_interval, self(), report),

    Stats = counters(Window) ++ histograms(Window) ++ gauges(Window),
    lists:foreach(fun (S) ->
                          gen_server:cast(S, {statman_update, Stats})
                  end, State#state.subscribers),

    {noreply, State};

handle_info({gc, GcInterval} = GcMsg, State) ->
    erlang:send_after(GcInterval, self(), GcMsg),

    _NumGCed = statman_histogram:gc(),

    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

counters(Window) ->
    lists:map(fun ({K, V}) ->
                      statman_counter:reset(K, V),
                      [{key, K}, {node, node()}, {type, counter},
                       {value, V}, {window, Window}]
              end, statman_counter:get_all()).

histograms(Window) ->
    lists:map(fun (Key) ->
                      Data = statman_histogram:get_data(Key),
                      statman_histogram:reset(Key, Data),
                      [{key, Key}, {node, node()}, {type, histogram},
                       {value, Data}, {window, Window}]
              end, statman_histogram:keys()).

gauges(Window) ->
    statman_gauge:expire(),
    lists:map(fun ({Key, Value}) ->
                      [{key, Key}, {node, node()}, {type, gauge},
                       {value, Value}, {window, Window}]
              end, statman_gauge:get_all()).

