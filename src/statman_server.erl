%% @doc: Statman server, sends reports and owns the ETS tables
-module(statman_server).
-behaviour(gen_server).

-export([start_link/0, add_subscriber/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {counters, subscribers = []}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_subscriber(Ref) ->
    gen_server:call(?MODULE, {add_subscriber, Ref}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ok = statman_counter:init(),
    ok = statman_histogram:init(),
    ok = statman_gauge:init(),

    erlang:send_after(1000, self(), report),
    {ok, #state{counters = dict:new()}}.

handle_call({add_subscriber, Ref}, _From, #state{subscribers = Sub} = State) ->
    {reply, ok, State#state{subscribers = [Ref | Sub]}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(report, #state{subscribers = Subscribers} = State) ->
    erlang:send_after(1000, self(), report),

    {NewCounters, CounterRates} = counter_rate(State#state.counters),

    Json = {[{rates, CounterRates},
             {histograms, histograms()},
             {gauges, gauges()}]},

    Chunk = iolist_to_binary(["data: ", jiffy:encode(Json), "\n\n"]),
    NewSubscribers = notify_subscribers(Subscribers, Chunk),

    %% statman_counter:incr({<<"statman_internal">>, statman_reports}),
    %% statman_counter:incr({<<"/example/request">>, highscore_db}, 30),
    %% statman_counter:incr({<<"/example/request">>, user_db}, 10),
    %% [statman_histogram:record_value({<<"/example/request">>, user_db}, N) ||
    %%     N <- lists:seq(20, 50)],
    %% [statman_histogram:record_value({<<"/example/request">>, highscore_db}, N) ||
    %%     N <- lists:seq(10, 30)],

    %% statman_gauge:set({<<"rscope">>, histograms_ets_size},
    %%                 proplists:get_value(size, ets:info(statman_histograms))),

    {noreply, State#state{counters = NewCounters, subscribers = NewSubscribers}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

notify_subscribers(Subscribers, Chunk) ->
    lists:flatmap(
      fun (Sub) ->
              case elli_request:send_chunk(Sub, Chunk) of
                  ok ->
                      [Sub];
                  {error, closed} ->
                      [];
                  {error, timeout} ->
                      []
              end
      end, Subscribers).


counter_rate(Counters) ->
    AllCounters = statman_counter:get_all(),
    Rates = lists:map(
              fun ({{Id, UserKey} = Key, Count}) ->
                      PrevCount = case dict:find(Key, Counters) of
                                              {ok, C} -> C;
                                              error -> 0
                                          end,
                      {[{id, Id}, {key, UserKey}, {rate, Count - PrevCount}]}
              end, AllCounters),

    NewCounters = lists:foldl(
                    fun ({Key, Count}, D) ->
                            dict:store(Key, Count, D)
                    end, Counters, AllCounters),
    {NewCounters, Rates}.


histograms() ->
    Keys = statman_histogram:keys(),
    Result = lists:map(fun ({Id, Key} = FullKey) ->
                               {[{id, Id}, {key, Key} | statman_histogram:summary(FullKey)]}
                       end, Keys),
    lists:map(fun statman_histogram:clear/1, Keys),
    Result.

gauges() ->
    statman_gauge:expire(),
    lists:map(fun ({{Id, Key}, Value}) ->
                      {[{id, Id}, {key, Key}, {value, Value}]}
              end, statman_gauge:get_all()).
