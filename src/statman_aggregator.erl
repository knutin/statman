-module(statman_aggregator).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([merge/1, example_data/0]).

-include_lib("eunit/include/eunit.hrl").

-record(state, {
          timeseries = [],
          last_sample = [],
          nodes = []
         }).
-define(COUNTERS_TABLE, statman_aggregator_counters).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ets:new(?COUNTERS_TABLE, [named_table, protected, set]),
    {ok, #state{}}.

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast({statman_update, Metrics}, #state{nodes = Nodes} = State) ->
    io:format("aggregator got update~n"),
    Now = now_to_seconds(),
    NewNodes = lists:foldl(
                 fun (Metric, N) ->
                         case orddict:find(get_node(Metric), N) of
                             {ok, Samples} ->
                                 NewSamples = setelement(Now rem 300, Samples, Metric),
                                 orddict:store(get_node(Metric), NewSamples, N);
                             error ->
                                 Samples = setelement(Now rem 300, erlang:make_tuple(300, undefined), Metric),
                                 orddict:store(get_node(Metric), Samples, N)
                         end
                 end, Nodes, Metrics),
    {noreply, State#state{nodes = NewNodes}}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc: Merge histograms with the same key and node together. For
%% counters and gauges, we just keep the last value.
merge(TimeSeries) ->
    Metrics = lists:flatmap(fun ({_, Ms}) ->
                                    Ms
                            end, TimeSeries),
    lists:foldl(fun (Metric, Acc) ->
                        case type(Metric) of
                            histogram ->
                                case orddict:find(key(Metric), Acc) of
                                    {ok, Existing} ->
                                        orddict:store(key(Metric),
                                                      merge(Metric, Existing), Acc);
                                    error ->
                                        orddict:store(key(Metric), Metric, Acc)
                                end;
                            gauge ->
                                orddict:store(key(Metric), Metric, Acc);
                            counter ->
                                orddict:store(key(Metric), Metric, Acc)
                        end
                end, orddict:new(), Metrics).


merge(MetricA, MetricB) ->
    Values = orddict:merge(fun (_, A, B) ->
                                   A + B
                           end,
                           orddict:from_list(
                             proplists:get_value(value, MetricA)),
                           orddict:from_list(
                             proplists:get_value(value, MetricB))),

    lists:keyreplace(value, 1, MetricA, {value, Values}).


type(Metric) -> proplists:get_value(type, Metric).
get_node(Metric) -> proplists:get_value(node, Metric).

key(Metric) ->
    {proplists:get_value(node, Metric),
     proplists:get_value(key, Metric)}.

example_data() ->
    [
     [{key,{<<"/highscores">>,db_a_latency}},
      {node,[b@vm,a@vm]},
      {type,histogram},
      {value,[{1,2},
              {2,5},
              {3,1}]},
      {window,1000}],

    [{key,{<<"/highscores">>,db_b_latency}},
     {node,[b@vm,a@vm]},
     {type,histogram},
     {value,[{1,4},
             {2,2},
             {3,2}]},
     {window,1000}],

     [{key,{db,hits}},
      {node,a@vm},
      {type,counter},
      {value,3100},
      {window,1000}],

     [{key,{http,hits}},
      {node,a@vm},
      {type,counter},
      {value,155},
      {window,1000}],

     [{key,{node_a,foo}},
      {node,a@vm},
      {type,counter},
      {value,155},
      {window,1000}]
    ].


aggregate_test() ->
    {ok, S0} = init([]),

    S1 = lists:foldl(fun (_, Acc) ->
                             {noreply, NewAcc} = handle_cast(
                                                   {statman_update, example_data()},
                                                   Acc),
                             NewAcc
                     end, S0, lists:seq(1, 3)),

    %% purge

    ?assertEqual([], merge(S1#state.timeseries)).

now_to_seconds() ->
    {MegaSeconds, Seconds, _} = now(),
    MegaSeconds * 1000000 + Seconds.
