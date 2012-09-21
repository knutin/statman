%% @doc  Aggregate statman samples
%%
%% statman_aggregator receives metrics from statman_servers running in
%% your cluster, picks them apart and keeps a moving window of the raw
%% values. On demand, the samples are aggregated together. Metrics
%% with the same key, but from different nodes are also merged.
-module(statman_aggregator).
-behaviour(gen_server).

-export([start_link/0, get_window/1, get_keys/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("eunit/include/eunit.hrl").

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
    gen_server:call(?MODULE, {get_window, Size}).

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


handle_call({get_window, Size}, _From, #state{metrics = Metrics} = State) ->
    PurgedMetrics = dict:map(fun (_, {Type, Samples}) ->
                                     {Type, purge(Samples)}
                             end, Metrics),

    Aggregated = lists:map(
                   fun ({{Node, Key}, {Type, Samples}}) ->
                           {Node, Key, Type, merge_samples(Type, window(Size, Samples))}
                   end, dict:to_list(PurgedMetrics)),

    Reply = format(Size, Aggregated) ++ format(Size, merge(Aggregated)),
    {reply, {ok, Reply}, State#state{metrics = PurgedMetrics}};


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


insert(Metric, Metrics) ->
    Samples = case dict:find(nodekey(Metric), Metrics) of
                  {ok, {_Type, M}} -> M;
                  error -> []
              end,

    dict:store(nodekey(Metric),
               {type(Metric), [{now_to_seconds(), value(Metric)} | Samples]},
               Metrics).

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
    lists:last(
      lists:filter(fun ([]) -> false;
                       (_)  -> true
                   end, Samples)).



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

window_test() ->
    {ok, P} = start_link(),

    gen_server:cast(P, {statman_update, [sample_histogram('a@knutin')]}),
    gen_server:cast(P, {statman_update, [sample_histogram('b@knutin')]}),
    gen_server:cast(P, {statman_update, [sample_counter('a@knutin')]}),
    gen_server:cast(P, {statman_update, [sample_counter('b@knutin')]}),

    timer:sleep(1000),

    gen_server:cast(P, {statman_update, [sample_histogram('a@knutin')]}),
    gen_server:cast(P, {statman_update, [sample_histogram('b@knutin')]}),
    gen_server:cast(P, {statman_update, [sample_counter('a@knutin')]}),
    gen_server:cast(P, {statman_update, [sample_counter('b@knutin')]}),

    ?assertEqual([
                  {nodekey(sample_counter('a@knutin')), counter},
                  {nodekey(sample_histogram('a@knutin')), histogram},
                  {nodekey(sample_counter('b@knutin')), counter},
                  {nodekey(sample_histogram('b@knutin')), histogram}
                 ], lists:sort(element(2, get_keys()))),

    {ok, Aggregated} = get_window(60),

    [_ACounter, _BCounter, MergedCounter,
     _AHistogram, _BHistogram, MergedHistogram] = lists:sort(Aggregated),


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
