%% @doc: Statman server, sends reports and owns the ETS tables
%%
%% Every second this gen_server sends a summary and the raw data of
%% all available statistics to the installed subscribers which can
%% further aggregate, summarize or publish the statistics.
%%
-module(statman_server).
-behaviour(gen_server).

-export([start_link/0, add_subscriber/1, remove_subscriber/1, report/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {counters, subscribers = []}).
-define(COUNTERS_TABLE, statman_server_counters).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_subscriber(Ref) ->
    gen_server:call(?MODULE, {add_subscriber, Ref}).

remove_subscriber(Ref) ->
    gen_server:call(?MODULE, {remove_subscriber, Ref}).

report() ->
    ?MODULE ! report.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ok = statman_counter:init(),
    ok = statman_gauge:init(),
    ok = statman_histogram:init(),

    ok = create_cache_tables(),

    erlang:send_after(1000, self(), report),
    {ok, #state{counters = dict:new()}}.

handle_call({add_subscriber, Ref}, _From, #state{subscribers = Sub} = State) ->
    {reply, ok, State#state{subscribers = [Ref | Sub]}};
handle_call({remove_subscriber, Ref}, _From, #state{subscribers = Sub} = State) ->
    {reply, ok, State#state{subscribers = lists:delete(Ref, Sub)}}.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(report, #state{subscribers = Subscribers} = State) ->
    erlang:send_after(1000, self(), report),

    Stats = [{node, node()},
             {rates, counter_rates()},
             {histograms, histograms()},
             {gauges, gauges()}],

    lists:foreach(fun (S) ->
                          gen_server:cast(S, {statman_update, Stats})
                  end, Subscribers),

    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

create_cache_tables() ->
    ets:new(?COUNTERS_TABLE, [named_table, private, set]),
    ok.

counter_rates() ->
    lists:flatmap(fun ({Key, Count}) ->
                          case Count - prev_count(Key) of
                              0 ->
                                  ets:insert(?COUNTERS_TABLE, {Key, Count}),
                                  [];
                              Delta ->
                                  ets:insert(?COUNTERS_TABLE, {Key, Count}),
                                  [{Key, Delta, Count}]
                          end
                  end, statman_counter:get_all()).


prev_count(Key) ->
    case ets:lookup(?COUNTERS_TABLE, Key) of
        [{Key, Count}] ->
            Count;
        [] ->
            0
    end.

histograms() ->
    lists:flatmap(fun (Key) ->
                          case statman_histogram:summary_and_raw(Key) of
                              {[], _} ->
                                  [];
                              {Summary, Raw} ->
                                  [{Key, Summary, Raw}]
                          end
              end, statman_histogram:keys()).

gauges() ->
    statman_gauge:expire(),
    lists:map(fun ({Key, Value}) ->
                      {Key, Value, Value}
              end, statman_gauge:get_all()).

