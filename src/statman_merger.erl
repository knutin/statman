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
    erlang:send_after(1000, self(), report),
    {ok, #state{subscribers = [], slots = orddict:new()}}.

handle_call({add_subscriber, Ref}, _From, #state{subscribers = Sub} = State) ->
    {reply, ok, State#state{subscribers = [Ref | Sub]}};
handle_call({remove_subscriber, Ref}, _From, #state{subscribers = Sub} = State) ->
    {reply, ok, State#state{subscribers = lists:delete(Ref, Sub)}}.


handle_cast({statman_update, Updates}, #state{slots = Slots} = State) ->
    %% We keep one update per node. When we want to report, we merge
    %% these together.
    %% error_logger:info_msg("merger got ~p~n", [Update]),

    NewSlots = lists:foldl(fun (Update, Acc) ->
                                   Node = proplists:get_value(node, Update),
                                   orddict:store(Node, Update, Acc)
                           end, Slots, Updates),

    {noreply, State#state{slots = NewSlots}}.

handle_info(report, State) ->
    erlang:send_after(1000, self(), report),

    Merged = merge(State#state.slots),
    %% error_logger:info_msg("merged: ~p~n", [Merged]),
    Updates = lists:map(fun ({_, Update}) -> Update end, State#state.slots),
    lists:foreach(fun (S) ->
                          gen_server:cast(S, {statman_update, Updates}),
                          case proplists:get_value(histograms, Merged, []) of
                              [] ->
                                  ok;
                              _ ->
                                  gen_server:cast(S, {statman_merged, Merged})
                          end
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
                 [{histograms, []}, {nodes, []}],
                 Updates).


do_merge(Update, Acc) ->
    MergeHistogramF = fun (_Key, UpdateHistogram, AccHistogram) ->
                              orddict:merge(fun (_Value, UpdateFreq, AccFreq) ->
                                                    UpdateFreq + AccFreq
                                            end, UpdateHistogram, AccHistogram)
                      end,

    [{histograms, orddict:merge(MergeHistogramF,
                                orddict:from_list(
                                  proplists:get_value(histograms, Update)),
                                orddict:from_list(
                                  proplists:get_value(histograms, Acc)))},
     {nodes, [proplists:get_value(node, Update) | proplists:get_value(nodes, Acc)]}
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

%% merge_test() ->
%%     ?assertEqual([{histograms, [{{foo, bar},
%%                                  [{1,2}, {2,2}, {3,2}]}]},
%%                   {nodes, [node1, node2]}],
%%                  merge(orddict:from_list(
%%                          [{node1, example_nodedata(node1)},
%%                           {node2, example_nodedata(node2)}]))).


report_test() ->
    {ok, Init} = init([]),
    {noreply, S1} = handle_cast({statman_update, [example_nodedata(foo)]}, Init),
    {noreply, S2} = handle_cast({statman_update, [example_nodedata(bar)]}, S1),
    {noreply, S3} = handle_cast({statman_update, [example_nodedata(quux)]}, S2),

    ?assertEqual([{histograms, [{{foo, bar},
                                 [{1,3}, {2,3}, {3,3}]}]},
                  {nodes, [quux, foo, bar]}],
                 merge(S3#state.slots)).
