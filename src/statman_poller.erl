%% @doc: Statman server, sends reports and owns the ETS tables
-module(statman_poller).
-behaviour(gen_server).
-include_lib("eunit/include/eunit.hrl").

-export([start_link/0]).
-export([add_gauge/1, add_gauge/2,
         add_counter/1, add_counter/2,
         add_histogram/1, add_histogram/2]).
-export([remove_gauge/1, remove_counter/1, remove_histogram/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {timers = [], fs = []}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_gauge(F)               -> add_fun({gauge, F}, 10000).
add_gauge(F, Interval)     -> add_fun({gauge, F}, Interval).
add_counter(F)             -> add_fun({counter, F}, 10000).
add_counter(F, Interval)   -> add_fun({counter, F}, Interval).
add_histogram(F)           -> add_fun({histogram, F}, 10000).
add_histogram(F, Interval) -> add_fun({histogram, F}, Interval).

remove_gauge(F)     -> remove_fun({gauge, F}).
remove_counter(F)   -> remove_fun({counter, F}).
remove_histogram(F) -> remove_fun({histogram, F}).

remove_fun(TypedF) -> gen_server:call(?MODULE, {remove, TypedF}).

add_fun(TypedF, Interval) -> gen_server:call(?MODULE, {add, TypedF, Interval}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{fs = []}}.

handle_call({add, TypedF, Interval}, _From, #state{fs = Fs} = State) ->
    NewTimers = case orddict:find(Interval, State#state.timers) of
                    {ok, _} -> State#state.timers;
                    error ->
                        {ok, TRef} = timer:send_interval(Interval, {poll, Interval}),
                        orddict:store(Interval, TRef, State#state.timers)
                end,

    {reply, ok, State#state{timers = NewTimers, fs = [{Interval, TypedF} | Fs]}};

handle_call({remove, TypedF}, _From, #state{fs = Fs} = State) ->
    {reply, ok, State#state{fs = lists:keydelete(TypedF, 2, Fs)}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({poll, Interval}, State) ->
    Poll = fun ({I, {gauge, F}}) when I =:= Interval ->
                   [statman_gauge:set(K, V) || {K, V} <- F()];
               ({I, {counter, F}}) when I =:= Interval ->
                   [statman_counter:incr(K, V) || {K, V} <- F()];
               ({I, {histogram, F}}) when I =:= Interval ->
                   [statman_histogram:record_value(
                      K, statman_histogram:bin(V)) || {K, V} <- F()];
               (_) ->
                   ok
           end,
    spawn_link(fun () ->
                       lists:foreach(Poll, State#state.fs)
               end),
    {noreply, State};

handle_info(_, State) ->
    %% Ignore unknown messages, might come from gen calls that timed
    %% out, but response got sent anyway..
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

poller_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [
      ?_test(poller_fun()),
      ?_test(remove_poller())
     ]}.

setup() ->
    statman_counter:init(),
    statman_gauge:init(),
    statman_histogram:init().

teardown(_) ->
    [ets:delete(T) || T <- [statman_counters, statman_gauges, statman_histograms]].

poller_fun() ->
    GaugeF     = fun() -> [{gauge, 5}] end,
    CounterF   = fun () -> [{counter, 5}] end,
    HistogramF = fun () -> [{histogram, 5}, {histogram, 10}] end,

    ?assertEqual([], statman_counter:get_all()),
    ?assertEqual([], statman_gauge:get_all()),
    ?assertEqual([], statman_histogram:keys()),

    {ok, _} = start_link(),
    ok = add_counter(CounterF, 100),
    ok = add_gauge(GaugeF, 100),
    ok = add_histogram(HistogramF, 100),
    timer:sleep(200),

    ?assertEqual([histogram], statman_histogram:keys()),
    ?assertEqual([counter], statman_counter:counters()),
    ?assertMatch([{gauge, _}], statman_gauge:get_all()).


remove_poller() ->
    GaugeF = fun() -> [{gauge, 5}] end,
    {ok, Init} = init([]),

    {_, ok, Added} = handle_call({add, {gauge, GaugeF}, 1000}, undefined, Init),
    ?assertMatch(#state{fs = [{1000, {gauge, GaugeF}}]}, Added),

    {_, ok, Removed} = handle_call({remove, {gauge, GaugeF}}, undefined, Added),
    ?assertMatch(#state{fs = []}, Removed).
