%% @doc: Merges multiple streams
%%
%% statman_merger merges the raw data pushed from statman_server into
%% an aggregated view per metric.
-module(statman_merger).
-behaviour(gen_server).
-include_lib("eunit/include/eunit.hrl").

-export([start_link/0, add_subscriber/1, remove_subscriber/1, merge/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {subscribers = [],
                metrics = orddict:new()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_subscriber(Ref) ->
    gen_server:call(?MODULE, {add_subscriber, Ref}).

remove_subscriber(Ref) ->
    gen_server:call(?MODULE, {remove_subscriber, Ref}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    erlang:send_after(1000, self(), report),
    {ok, #state{subscribers = [], metrics = orddict:new()}}.

handle_call({add_subscriber, Ref}, _From, #state{subscribers = Sub} = State) ->
    {reply, ok, State#state{subscribers = [Ref | Sub]}};
handle_call({remove_subscriber, Ref}, _From, #state{subscribers = Sub} = State) ->
    {reply, ok, State#state{subscribers = lists:delete(Ref, Sub)}}.


handle_cast({statman_update, Updates}, #state{metrics = Metrics} = State) ->
    %%error_logger:info_msg("merger got ~p~n", [Updates]),

    NewMetrics = lists:foldl(fun (Update, Acc) ->
                                     Key = {proplists:get_value(node, Update),
                                            proplists:get_value(key, Update)},
                                     orddict:store(Key, Update, Acc)
                             end, Metrics, Updates),

    %% Histograms = lists:flatmap(fun (Update) ->
    %%                                    proplists:get_value(histograms, Update)
    %%                            end, Updates),

    %% NewSlots = lists:foldl(fun ({K, V}, Acc) ->
    %%                                orddict:store(K, V, Acc)
    %%                        end, Slots, Histograms),

    {noreply, State#state{metrics = NewMetrics}}.

handle_info(report, State) ->
    erlang:send_after(1000, self(), report),

    %% error_logger:info_msg("slots: ~p~n", [State#state.slots]),
    Merged = merge(State#state.metrics),

    KeyedMetrics = Merged ++ orddict:to_list(State#state.metrics),
    {_, Metrics} = lists:unzip(KeyedMetrics),

    %% error_logger:info_msg("merged: ~p~n", [Merged]),
    %% Updates = lists:map(fun ({_, Update}) -> Update end, State#state.slots),
    lists:foreach(fun (S) ->
                          gen_server:cast(S, {statman_update, Metrics})
                  end, State#state.subscribers),

    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

merge(Metrics) ->
    %% Find metrics with the same key
    %% Merge values if the type allows it
    %% Change node atom to node list

    orddict:fold(
      fun (_Key, Metric, Acc) ->
              case type(Metric) =:= histogram of
                  true ->
                      case orddict:find(key(Metric), Acc) of
                          {ok, OtherMetric} ->
                              orddict:store(key(Metric),
                                            do_merge(Metric, OtherMetric),
                                            Acc);
                          error ->
                              orddict:store(key(Metric),
                                            Metric,
                                            Acc)
                      end;
                  false ->
                      Acc
              end
      end, orddict:new(), Metrics).


type(Metric) -> proplists:get_value(type, Metric).
key(Metric) -> proplists:get_value(key, Metric).

do_merge(Left, Right) ->
    MergeHistogramF = fun (_Key, ValueLeft, ValueRight) ->
                              ValueLeft + ValueRight
                      end,

    orddict:merge(
      fun (node, A, Nodes) when is_list(Nodes) ->
              [A | Nodes];
          (node, A, B) ->
              [A, B];
          (value, A, B) ->
              orddict:merge(MergeHistogramF, A, B);
          (_Other, A, _) ->
              A
      end,
      Left, Right).


%%
%% TESTS
%%

example_nodedata(Node) ->
    [[{key,{db,hits}},
      {node,Node},
      {type,counter},
      {value,6240},
      {window,1000}],
     [{key,{<<"/highscores">>,db_a_latency}},
      {node,Node},
      {type,histogram},
      {value,[{2,3},
              {3,4},
              {4,1},
              {5,1}]}],

     [{key,{<<"/highscores">>,db_b_latency}},
      {node,Node},
      {type,histogram},
      {value,[{2,3},
              {3,4},
              {4,1},
              {5,1}]}],

     [{key,{db,connections}},
      {node,Node},
      {type,gauge},
      {value,7},
      {window,1000}]
     ].


%% merge_test() ->
%%     ?assertEqual([{histograms, [{{foo, bar},
%%                                  [{1,2}, {2,2}, {3,2}]}]},
%%                   {nodes, [node2, node1]}],
%%                  merge(orddict:from_list(
%%                          [{node1, example_nodedata(node1)},
%%                           {node2, example_nodedata(node2)}]))).


report_test() ->
    {ok, Init} = init([]),
    {noreply, S1} = handle_cast({statman_update, example_nodedata(foo)}, Init),
    {noreply, S2} = handle_cast({statman_update, example_nodedata(bar)}, S1),
    {noreply, S3} = handle_cast({statman_update, example_nodedata(quux)}, S2),


    ?assertEqual([{{<<"/highscores">>,db_a_latency},
                   [{key,{<<"/highscores">>,db_a_latency}},
                    {node,[quux,foo,bar]},
                    {type,histogram},
                    {value,[{2,9},{3,12},{4,3},{5,3}]}]},
                  {{<<"/highscores">>,db_b_latency},
                   [{key,{<<"/highscores">>,db_b_latency}},
                    {node,[quux,foo,bar]},
                    {type,histogram},
                    {value,[{2,9},{3,12},{4,3},{5,3}]}]}], merge(S3#state.metrics)).

