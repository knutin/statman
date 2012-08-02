%% @doc: Statman server, sends reports and owns the ETS tables
%%
%% Every second this gen_server sends a summary and the raw data of
%% all available statistics to the installed subscribers which can
%% further aggregate, summarize or publish the statistics.
%%
-module(statman_server).
-behaviour(gen_server).

-export([start_link/1, add_subscriber/1, remove_subscriber/1, report/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {counters, subscribers = [], report_interval}).
-define(COUNTERS_TABLE, statman_server_counters).

%%%===================================================================
%%% API
%%%===================================================================

start_link(ReportInterval) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ReportInterval], []).

add_subscriber(Ref) ->
    gen_server:call(?MODULE, {add_subscriber, Ref}).

remove_subscriber(Ref) ->
    gen_server:call(?MODULE, {remove_subscriber, Ref}).

report() ->
    ?MODULE ! report.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([ReportInterval]) ->
    ok = statman_counter:init(),
    ok = statman_gauge:init(),
    ok = statman_histogram:init(),

    erlang:send_after(ReportInterval, self(), report),
    {ok, #state{counters = dict:new(), report_interval = ReportInterval}}.

handle_call({add_subscriber, Ref}, _From, #state{subscribers = Sub} = State) ->
    {reply, ok, State#state{subscribers = [Ref | Sub]}};
handle_call({remove_subscriber, Ref}, _From, #state{subscribers = Sub} = State) ->
    {reply, ok, State#state{subscribers = lists:delete(Ref, Sub)}}.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(report, State) ->
    erlang:send_after(State#state.report_interval, self(), report),

    Stats = [{node, node()},
             {window, State#state.report_interval},
             {counters, counters()},
             {histograms, histograms()},
             {gauges, gauges()}],

    lists:foreach(fun (S) ->
                          gen_server:cast(S, {statman_update, Stats})
                  end, State#state.subscribers),

    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

counters() ->
    statman_counter:get_all().

histograms() ->
    lists:map(fun (Key) ->
                      statman_histogram:get_data(Key)
              end, statman_histogram:keys()).

gauges() ->
    statman_gauge:expire(),
    lists:map(fun ({Key, Value}) ->
                      {Key, Value}
              end, statman_gauge:get_all()).

