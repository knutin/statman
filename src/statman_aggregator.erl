%% @doc  Aggregate statman samples
%%
%% statman_aggregator receives metrics from statman_servers running in
%% your cluster, picks them apart and keeps a moving window of the raw
%% values. On demand, the samples are aggregated together. Metrics
%% with the same key, but from different nodes are also merged.
-module(statman_aggregator).
-behaviour(gen_server).

-export([start_link/0,
         get_window/1,
         get_window/2,
         get_merged_window/1,
         get_merged_window/2,
         get_keys/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(state, {
          subscribers = [],
          last_sample = [],
          metrics = dict:new()

         }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_window(Size) ->
    get_window(Size, 5000).

get_window(Size, Timeout) ->
    gen_server:call(?MODULE, {get_window, Size, false}, Timeout).

get_merged_window(Size) ->
    get_merged_window(Size, 5000).

get_merged_window(Size, Timeout) ->
    gen_server:call(?MODULE, {get_window, Size, true}, Timeout).

get_keys() ->
    gen_server:call(?MODULE, get_keys).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    timer:send_interval(10000, push),
    {ok, #state{metrics = dict:new()}}.

handle_call({add_subscriber, Ref}, _From, #state{subscribers = Sub} = State) ->
    {reply, ok, State#state{subscribers = [Ref | Sub]}};
handle_call({remove_subscriber, Ref}, _From, #state{subscribers = Sub} = State) ->
    {reply, ok, State#state{subscribers = lists:delete(Ref, Sub)}};


handle_call({get_window, Size, MergeNodes}, From, #state{metrics = Metrics} = State) ->
    PurgedMetrics = dict:map(fun (_, {Type, Samples}) ->
                                     {Type, purge(Samples)}
                             end, Metrics),

    spawn(fun() -> do_reply(From, PurgedMetrics, Size, MergeNodes) end),
    {noreply, State#state{metrics = PurgedMetrics}};


handle_call(get_keys, _From, State) ->
    Reply = dict:fold(fun (Key, {Type, _Samples}, Acc) ->
                              [{Key, Type} | Acc]
                      end, [], State#state.metrics),
    {reply, {ok, Reply}, State}.


handle_cast({statman_update, NewSamples}, #state{metrics = Metrics} = State) ->
    NewMetrics = lists:foldl(fun insert/2, Metrics, NewSamples),
    {noreply, State#state{metrics = NewMetrics}}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_reply(Client, Metrics, Size, MergeNodes) ->
    Aggregated = lists:map(
                     fun ({{Node, Key}, {Type, Samples}}) ->
                             {Node, Key, Type, merge_samples(Type, window(Size, Samples))}
                     end, dict:to_list(Metrics)),

    Reply = case MergeNodes of
                false ->
                    format(Size, Aggregated);
                true ->
                    format(Size, Aggregated) ++ format(Size, merge(Aggregated))
            end,
    gen_server:reply(Client, {ok, Reply}).


insert(Metric, Metrics) ->
    dict:update(nodekey(Metric),
                %% FIXME: this breaks if you have the same key for different types of metrics
                fun ({_Type, Samples}) ->
                        {type(Metric), [{now_to_seconds(), value(Metric)} | Samples]}
                end,
                {type(Metric), [{now_to_seconds(), value(Metric)}]},
                Metrics).

window(_, []) ->
    [];
window(1, [{_, Sample} | _]) ->
    [Sample];

window(Size, Samples) ->
    element(2, lists:unzip(samples_after(now_to_seconds() - Size, Samples))).

purge(Samples) ->
    samples_after(now_to_seconds() - 300, Samples).


samples_after(Threshold, Samples) ->
    lists:takewhile(fun ({Ts, _}) -> Ts >= Threshold end, Samples).



merge(Metrics) ->
    {_, Merged} =
        lists:unzip(
          orddict:to_list(
            lists:foldl(
              fun ({_, _, gauge, _}, Acc) ->
                      Acc;

                  ({Node, Key, counter, Sample}, Acc) ->
                      case orddict:find(Key, Acc) of
                          {ok, {Nodes, Key, counter, OtherSample}} ->
                              orddict:store(Key, {[Node | Nodes], Key, counter,
                                                  Sample + OtherSample}, Acc);
                          error ->
                              orddict:store(Key, {[Node], Key, counter, Sample}, Acc)
                      end;

                  ({Node, Key, Type, Samples}, Acc) ->
                      case orddict:find(Key, Acc) of
                          {ok, {Nodes, Key, Type, OtherSamples}} ->
                              Merged = merge_samples(Type, [Samples, OtherSamples]),
                              orddict:store(Key, {[Node | Nodes], Key, Type, Merged}, Acc);
                          error ->
                              orddict:store(Key, {[Node], Key, Type, Samples}, Acc)
                      end
              end, orddict:new(), Metrics))),

    lists:filter(fun ({Nodes, _, _, _}) -> length(Nodes) > 1 end, Merged).


merge_samples(histogram, Samples) ->
    lists:foldl(fun (Sample, Agg) ->
                        orddict:merge(fun (_, A, B) ->
                                              A + B
                                      end,
                                      orddict:from_list(Sample),
                                      Agg)
                end, orddict:new(), Samples);


merge_samples(counter, Samples) ->
    lists:sum(Samples);

merge_samples(gauge, []) ->
    0;
merge_samples(gauge, Samples) ->
    hd(Samples).



format(_, []) ->
    [];

format(Size, [{Nodes, Key, Type, Value} | Rest]) ->
    [
     [{key, Key},
      {node, Nodes},
      {type, Type},
      {value, Value},
      {window, Size * 1000}]

     | format(Size, Rest)].



type(Metric)  -> proplists:get_value(type, Metric).
value(Metric) -> proplists:get_value(value, Metric).

nodekey(Metric) ->
    {proplists:get_value(node, Metric),
     proplists:get_value(key, Metric)}.


now_to_seconds() ->
    {MegaSeconds, Seconds, _} = os:timestamp(),
    MegaSeconds * 1000000 + Seconds.


%%
%% TESTS
%%

-ifdef(TEST).
aggregator_test_() ->
    {foreach,
     fun setup/0, fun teardown/1,
     [?_test(expire()),
      ?_test(window()),
      ?_test(merged_window())
     ]
    }.

setup() ->
    {ok, Pid} = start_link(),
    true = unlink(Pid),
    Pid.

teardown(Pid) ->
    exit(Pid, kill),
    timer:sleep(1000),
    false = is_process_alive(Pid).

expire() ->
    gen_server:cast(?MODULE, {statman_update, [sample_histogram('a@knutin')]}),
    gen_server:cast(?MODULE, {statman_update, [sample_counter('a@knutin')]}),
    gen_server:cast(?MODULE, {statman_update, [sample_counter('a@knutin')]}),
    gen_server:cast(?MODULE, {statman_update, [sample_gauge('a@knutin', 1)]}),
    gen_server:cast(?MODULE, {statman_update, [sample_gauge('a@knutin', 3)]}),

    ?assert(lists:all(fun (M) ->
                              V = proplists:get_value(value, M, 0),
                              V =/= 0 andalso V =/= []
                      end, element(2, get_window(2)))),

    timer:sleep(3000),

    ?assert(lists:all(fun (M) ->
                              V = proplists:get_value(value, M),
                              V == 0 orelse V =:= []
                      end, element(2, get_window(2)))).

window() ->
    gen_server:cast(?MODULE, {statman_update, [sample_histogram('a@knutin')]}),
    gen_server:cast(?MODULE, {statman_update, [sample_counter('a@knutin')]}),
    gen_server:cast(?MODULE, {statman_update, [sample_counter('a@knutin')]}),
    gen_server:cast(?MODULE, {statman_update, [sample_gauge('a@knutin', 1)]}),
    gen_server:cast(?MODULE, {statman_update, [sample_gauge('a@knutin', 3)]}),
    gen_server:cast(?MODULE, {statman_update, [sample_histogram('b@knutin')]}),

    timer:sleep(1000),

    gen_server:cast(?MODULE, {statman_update, [sample_histogram('a@knutin')]}),
    gen_server:cast(?MODULE, {statman_update, [sample_histogram('a@knutin')]}),
    gen_server:cast(?MODULE, {statman_update, [sample_counter('a@knutin')]}),
    gen_server:cast(?MODULE, {statman_update, [sample_gauge('a@knutin', 2)]}),
    gen_server:cast(?MODULE, {statman_update, [sample_histogram('b@knutin')]}),

    {ok, Aggregated} = get_window(60),

    [MergedCounter, MergedGauge, MergedHistogramA, MergedHistogramB] = lists:sort(Aggregated),


    ?assertEqual([{key, {<<"/highscores">>,db_a_latency}},
                  {node, 'a@knutin'},
                  {type, histogram},
                  {value, [{1, 3}, {2, 6}, {3, 9}]},
                  {window, 60000}], MergedHistogramA),

    ?assertEqual([{key, {<<"/highscores">>,db_a_latency}},
                  {node, 'b@knutin'},
                  {type, histogram},
                  {value, [{1, 2}, {2, 4}, {3, 6}]},
                  {window, 60000}], MergedHistogramB),

    ?assertEqual([{key, {foo, bar}},
                  {node, 'a@knutin'},
                  {type, counter},
                  {value, 90},
                  {window, 60000}], MergedCounter),

    ?assertEqual([{key, {foo, baz}},
                  {node, 'a@knutin'},
                  {type, gauge},
                  {value, 2},
                  {window, 60000}], MergedGauge).

merged_window() ->
    gen_server:cast(?MODULE, {statman_update, [sample_histogram('a@knutin')]}),
    gen_server:cast(?MODULE, {statman_update, [sample_histogram('b@knutin')]}),
    gen_server:cast(?MODULE, {statman_update, [sample_counter('a@knutin')]}),
    gen_server:cast(?MODULE, {statman_update, [sample_counter('b@knutin')]}),

    timer:sleep(1000),

    gen_server:cast(?MODULE, {statman_update, [sample_histogram('a@knutin')]}),
    gen_server:cast(?MODULE, {statman_update, [sample_histogram('b@knutin')]}),
    gen_server:cast(?MODULE, {statman_update, [sample_counter('a@knutin')]}),
    gen_server:cast(?MODULE, {statman_update, [sample_counter('b@knutin')]}),

    ?assertEqual([
                  {nodekey(sample_counter('a@knutin')), counter},
                  {nodekey(sample_histogram('a@knutin')), histogram},
                  {nodekey(sample_counter('b@knutin')), counter},
                  {nodekey(sample_histogram('b@knutin')), histogram}
                 ], lists:sort(element(2, get_keys()))),

    {ok, Aggregated} = get_merged_window(60),

    [_CounterA, _CounterB, MergedCounter,
     _HistogramA, _HistogramB, MergedHistogram] = lists:sort(Aggregated),


    ?assertEqual([{key, {<<"/highscores">>,db_a_latency}},
                  {node, ['a@knutin', 'b@knutin']},
                  {type, histogram},
                  {value, [{1, 4}, {2, 8}, {3, 12}]},
                  {window, 60000}], MergedHistogram),

    ?assertEqual([{key, {foo, bar}},
                  {node, ['a@knutin', 'b@knutin']},
                  {type, counter},
                  {value, 120},
                  {window, 60000}], MergedCounter).



sample_histogram(Node) ->
    [{key,{<<"/highscores">>,db_a_latency}},
     {node,Node},
     {type,histogram},
     {value,[{1,1},
             {2,2},
             {3,3}]},
     {window,1000}].

sample_counter(Node) ->
    [{key,{foo, bar}},
     {node,Node},
     {type,counter},
     {value,30},
     {window,1000}].

sample_gauge(Node, Value) ->
    [{key,{foo, baz}},
     {node,Node},
     {type,gauge},
     {value,Value},
     {window,1000}].
-endif.
