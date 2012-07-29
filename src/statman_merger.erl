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


handle_cast({statman_update, Stats}, State) ->
    error_logger:info_msg("merger got ~p~n", [Stats]),
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


rates(Stats) ->
    lists:map(fun ({FullKey, Rate, _}) ->
                      {Id, Key} = id_key(FullKey),
                      {[{id, Id}, {key, Key}, {rate, Rate}]}
              end, proplists:get_value(rates, Stats, [])).


histograms(Stats) ->
    lists:map(fun ({FullKey, Summary, _}) ->
                      {Id, Key} = id_key(FullKey),
                      {[{id, Id}, {key, Key} | Summary]}
              end, proplists:get_value(histograms, Stats, [])).

gauges(Stats) ->
    lists:map(fun ({FullKey, Value, _}) ->
                      {Id, Key} = id_key(FullKey),
                      {[{id, Id}, {key, Key}, {value, Value}]}
              end, proplists:get_value(gauges, Stats, [])).

id_key({Id, Key}) -> {Id, Key};
id_key(Key) -> {undefined, Key}.


merge(Left, Right) ->
    [{histograms, lists:map(
                    fun ({Key, _, LeftRaw}) ->
                            {Key, _, RightRaw} =
                                lists:keyfind(Key, 1, proplists:get_value(histograms, Right, [])),
                            {Summary, NewRaw} = merge_histogram(LeftRaw, RightRaw),
                            {Key, Summary, NewRaw}
                    end, proplists:get_value(histograms, Left, []))}].


merge_histogram(Left, Right) ->
    NewRaw = lists:foldl(fun ({K, V}, Acc) ->
                                 orddict:update_counter(K, V, Acc)
                         end, orddict:new(), Left ++ Right),

    {statman_histogram:do_summary(NewRaw), NewRaw}.



%%
%% TESTS
%%

merge_test() ->
    NodeData = fun(Name) ->
                       [{node,Name},
                        {rates,[]},
                        {histograms,[{{foo,bar},
                                      [{observations,10},
                                       {min,1},
                                       {median,5},
                                       {mean,5.5},
                                       {max,10},
                                       {sd,3.0276503540974917},
                                       {p95,10},
                                       {p99,10}],
                                      [{1,1}, {2,1}, {3,1},
                                       {4,1}, {5,1}, {6,1}, {7,1},
                                       {8,1}, {9,1}, {10,1}]}]},
                        {gauges,[{{system,run_queue},19,19},
                                 {{messaging,messages_in_queue},19,19},
                                 {{messaging,processes_with_queues},19,19}]}]
               end,

    ?assertEqual([], merge(NodeData(node1), NodeData(node2))).

