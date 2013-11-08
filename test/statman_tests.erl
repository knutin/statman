-module(statman_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
statman_test_() ->
    {foreach,
        fun setup/0, fun teardown/1,
        [
         {"Start pollers",  fun test_start_pollers/0},
         {"Remove pollers", fun test_remove_pollers/0}
        ]
    }.

%% =============================================================================
setup() ->
    statman_counter:init(),
    statman_gauge:init(),
    statman_histogram:init(),
    ok.

teardown(_) ->
    [ets:delete(T) || T <- [statman_counters, statman_gauges, statman_histograms]],
    ok.

test_start_pollers() ->
    GaugeF     = fun() -> [{gauge, 5}] end,
    CounterF   = fun () -> [{counter, 5}] end,
    HistogramF = fun () -> [{histogram, 5}, {histogram, 10}] end,

    ?assertEqual([], statman_counter:get_all()),
    ?assertEqual([], statman_gauge:get_all()),
    ?assertEqual([], statman_histogram:keys()),

    {ok, _Pid} = statman_poller_sup:start_link([]),

    ok = statman_poller:add_counter(CounterF, 100),
    ok = statman_poller:add_gauge(GaugeF, 100),
    ok = statman_poller:add_histogram(HistogramF, 100),

    timer:sleep(200),

    ?assertEqual([histogram], statman_histogram:keys()),
    ?assertEqual([counter], statman_counter:counters()),
    ?assertMatch([{gauge, _}], statman_gauge:get_all()).


test_remove_pollers() ->
    {ok, _Pid} = statman_poller_sup:start_link([]),

    GaugeF = fun() -> [{gauge, 5}] end,

    ok = statman_poller:add_gauge(GaugeF, 1000),
    ?assertEqual(1, length(statman_poller:get_workers())),

    ok = statman_poller:remove_gauge(GaugeF),
    ?assertEqual([], statman_poller:get_workers()).
