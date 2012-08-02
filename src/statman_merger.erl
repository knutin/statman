%% @doc: Merges multiple streams
%%
%% statman_merger merges the raw data pushed from statman_server into
%% an aggregated view per metric.
-module(statman_merger).
-behaviour(gen_server).
-include_lib("eunit/include/eunit.hrl").

-export([start_link/0, add_subscriber/1, remove_subscriber/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {subscribers = [], slots = orddict:new()}).

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
    {ok, #state{subscribers = [], slots = orddict:new()}}.

handle_call({add_subscriber, Ref}, _From, #state{subscribers = Sub} = State) ->
    {reply, ok, State#state{subscribers = [Ref | Sub]}};
handle_call({remove_subscriber, Ref}, _From, #state{subscribers = Sub} = State) ->
    {reply, ok, State#state{subscribers = lists:delete(Ref, Sub)}}.


handle_cast({statman_update, Update}, #state{slots = Slots} = State) ->
    %% We keep one update per node. When we want to report, we merge
    %% these together.
    %% error_logger:info_msg("merger got ~p~n", [Update]),
    Node = proplists:get_value(node, Update),
    {noreply, State#state{slots = orddict:store(Node, Update, Slots)}}.

handle_info(report, State) ->
    Merged = merge(State#state.slots),
    error_logger:info_msg("merged: ~p~n", [Merged]),
    lists:foreach(fun (S) ->
                          gen_server:cast(S, {statman_merged, Merged})
                  end, State#state.subscribers),

    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

merge(Updates) ->
    orddict:fold(fun (_Node, Update, Acc) ->
                         do_merge(Update, Acc)
                 end,
                 [{histograms, []}, {counters, []}],
                 Updates).



do_merge(Left, Right) ->
    MergeHistogramF = fun (_Key, LeftHistogram, RightHistogram) ->
                              orddict:merge(fun (_Value, LeftFreq, RightFreq) ->
                                                    LeftFreq + RightFreq
                                            end, LeftHistogram, RightHistogram)
                      end,
    MergeCounterF = fun (_Key, LeftValue, RightValue) ->
                            LeftValue + RightValue
                    end,

    [{histograms, orddict:merge(MergeHistogramF,
                                orddict:from_list(
                                  proplists:get_value(histograms, Left)),
                                orddict:from_list(
                                  proplists:get_value(histograms, Right)))},
     {counters, orddict:merge(MergeCounterF,
                              orddict:from_list(
                               proplists:get_value(counters, Left)),
                              orddict:from_list(
                               proplists:get_value(counters, Right)))}
                             ].



%%
%% TESTS
%%

example_nodedata(Name) ->
    [{node,Name},
     {counters,[{baz, 30}]},
     {histograms,[{{foo,bar},
                   [{1,1}, {2,1}, {3,1}]}]},
     {gauges,[{{system,run_queue},19,19},
              {{messaging,messages_in_queue},19,19},
              {{messaging,processes_with_queues},19,19}]}].

merge_test() ->
    ?assertEqual([{histograms, [{{foo, bar},
                                 [{1,2}, {2,2}, {3,2}]}]},
                  {counters, [{baz, 60}]}],
                 do_merge(example_nodedata(node1), example_nodedata(node2))).


report_test() ->
    {ok, Init} = init([]),
    {noreply, S1} = handle_cast({statman_update, example_nodedata(foo)}, Init),
    {noreply, S2} = handle_cast({statman_update, example_nodedata(bar)}, S1),
    {noreply, S3} = handle_cast({statman_update, example_nodedata(quux)}, S2),

    ?assertEqual([{histograms, [{{foo, bar},
                                 [{1,3}, {2,3}, {3,3}]}]},
                  {counters, [{baz, 90}]}],
                 merge(S3#state.slots)).
