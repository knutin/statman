-module(statman_aggregator).
-behaviour(gen_server).

-export([start_link/0, get_window/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-record(state, {
          subscribers = [],
          last_sample = [],
          nodes = []
         }).
-define(COUNTERS_TABLE, statman_aggregator_counters).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_window(Size) ->
    gen_server:call(?MODULE, {get_window, Size}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ets:new(?COUNTERS_TABLE, [named_table, protected, set]),
    timer:send_interval(10000, push),
    {ok, #state{}}.

handle_call({add_subscriber, Ref}, _From, #state{subscribers = Sub} = State) ->
    {reply, ok, State#state{subscribers = [Ref | Sub]}};
handle_call({remove_subscriber, Ref}, _From, #state{subscribers = Sub} = State) ->
    {reply, ok, State#state{subscribers = lists:delete(Ref, Sub)}};

handle_call({get_window, Size}, _From, State) ->
    {reply, {ok, merged(window(Size, State#state.nodes))}, State}.



handle_cast({statman_update, Metrics}, #state{nodes = Nodes} = State) ->
    Now = now_to_seconds(),
    NewNodes = lists:foldl(
                 fun (Metric, N) ->
                         NewNode = case orddict:find(nodekey(Metric), N) of
                                          {ok, {Type, Samples}} ->
                                              {Type, setelement((Now rem 300) + 1, Samples, value(Metric))};
                                          error ->
                                           {type(Metric),
                                            setelement((Now rem 300) + 1,
                                                       erlang:make_tuple(300, []),
                                                       value(Metric))}
                                      end,

                         orddict:store(nodekey(Metric), NewNode, N)
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


window(Size, Nodes) ->
    Now = now_to_seconds(),
    lists:map(fun ({{Node, Key}, {Type, AllSamples}}) ->
                      StartIndex = (Now - 10) rem 300 + 1,
                      EndIndex = (Now rem 300) + 1,
                      Samples = get_samples(StartIndex, EndIndex, AllSamples),

                      [{node, Node},
                       {key, Key},
                       {value, merge_samples(Type, Samples)},
                       {type, Type},
                       {window, Size * 1000}]
              end, orddict:to_list(Nodes)).


merged(Metrics) ->
    {_, Merged} =
        lists:unzip(
          orddict:to_list(
            lists:foldl(
              fun (Metric, Acc) ->
                      case type(Metric) of
                          gauge ->
                              orddict:store(key(Metric), Metric, Acc);
                          Type ->
                              case orddict:find(key(Metric), Acc) of
                                  {ok, OtherMetric} ->
                                      Merged = do_merge(Type, Metric, OtherMetric),
                                      orddict:store(key(Metric), Merged, Acc);
                                  error ->
                                      orddict:store(key(Metric), Metric, Acc)
                              end
                      end
              end, orddict:new(), Metrics))),
    Merged.

do_merge(Type, Left, Right) ->
    orddict:merge(
      fun (node, A, Nodes) when is_list(Nodes) ->
              [A | Nodes];
          (node, A, B) ->
              [A, B];
          (value, A, B) ->
              case Type of
                  histogram ->
                      orddict:merge(fun (_Key, ValueLeft, ValueRight) ->
                                            ValueLeft + ValueRight
                                    end, A, B);
                  counter ->
                      A + B
              end;
          (_Other, A, _) ->
              A
      end,
      Left, Right).


get_samples(Start, End, Samples) when Start > End ->
    lists:map(fun (I) -> element(I, Samples) end,
              lists:seq(Start, 300) ++ lists:seq(1, End));
get_samples(Start, End, Samples) ->
    lists:map(fun (I) -> element(I, Samples) end,
              lists:seq(Start, End)).




merge_samples(histogram, Samples) ->
    lists:foldl(fun (Sample, Agg) ->
                        orddict:merge(fun (_, A, B) ->
                                              A + B
                                      end,
                                      orddict:from_list(Sample),
                                      Agg)
                end, orddict:new(), Samples);

merge_samples(counter, Samples) ->
    lists:sum([S || S <- Samples, S =/= []]);

merge_samples(gauge, Samples) ->
    lists:last(
      lists:filter(fun ([]) -> false;
                       (_)  -> true
                   end, Samples)).





type(Metric)  -> proplists:get_value(type, Metric).
value(Metric) -> proplists:get_value(value, Metric).
key(Metric)   -> proplists:get_value(key, Metric).

nodekey(Metric) ->
    {proplists:get_value(node, Metric),
     proplists:get_value(key, Metric)}.


now_to_seconds() ->
    {MegaSeconds, Seconds, _} = now(),
    MegaSeconds * 1000000 + Seconds.
