-module(statman_counter).
-export([init/0, counters/0, get/1, get_all/0, reset/2]).
-export([incr/1, incr/2, decr/1, decr/2, set/2]).
-compile([{no_auto_import, [get/1]}]).
-include_lib("eunit/include/eunit.hrl").


-define(TABLE, statman_counters).


%%
%% API
%%

init() ->
    ets:new(?TABLE, [named_table, public, set, {write_concurrency, true}]),
    ok.


get(Key) ->
    case ets:match(?TABLE, {Key, '$1'}) of
        [[N]] when is_integer(N) ->
            N;
        [] ->
            error(badarg)
    end.

get_all() ->
    ets:select(?TABLE, [{ {'$1', '$2'},  [], [{{'$1', '$2'}}]}]).


incr(Key) -> incr(Key, 1).

decr(Key) -> decr(Key, 1).
decr(Key, Incr) -> incr(Key, -Incr).


counters() ->
    ets:select(?TABLE, [{ {'$1', '$2'}, [], ['$1'] }]).

reset(Key, Value) ->
    decr(Key, Value).

%%
%% INTERNAL HELPERS
%%

set(Key, Value) ->
    case catch ets:update_element(?TABLE, Key, Value) of
        {'EXIT', {badarg, _}} ->
            (catch ets:insert(?TABLE, {Key, Value})),
            ok;
        _ ->
            ok
    end.


incr(Key, Incr) when is_integer(Incr) ->
    %% If lock contention on the single key becomes a problem, we can
    %% use multiple keys and try to snapshot a value across all
    %% subkeys. See
    %% https://github.com/boundary/high-scale-lib/blob/master/src/main/java/org/cliffc/high_scale_lib/ConcurrentAutoTable.java
    case catch ets:update_counter(?TABLE, Key, Incr) of
        {'EXIT', {badarg, _}} ->
            (catch ets:insert(?TABLE, {Key, Incr})),
            ok;
        _ ->
            ok
    end;

incr(_Key, Float) when is_float(Float) ->
    error(badarg).





%%
%% TESTS
%%

counter_test_() ->
    {foreach,
     fun setup/0, fun teardown/1,
     [
      ?_test(test_operations()),
      ?_test(find_counters()),
      {timeout, 100, ?_test(benchmark())},
      ?_test(test_reset()),
      ?_test(floats())
     ]
    }.

setup() ->
    init(),
    [?TABLE].

teardown(Tables) ->
    lists:map(fun ets:delete/1, Tables).


test_operations() ->
    ?assertError(badarg, get(key)),

    ?assertEqual(ok, incr(key)),
    ?assertEqual(1, get(key)),

    ?assertEqual(ok, decr(key)),
    ?assertEqual(0, get(key)),

    ?assertEqual(ok, decr(key)),
    ?assertEqual(-1, get(key)),

    ?assertEqual(ok, set(key, 5)),
    ?assertEqual(5, get(key)),

    ?assertEqual(ok, decr(key)),
    ?assertEqual(4, get(key)).


find_counters() ->
    ?assertEqual([], counters()),
    ?assertEqual([], get_all()),

    ?assertEqual(ok, incr(foo)),
    ?assertEqual(ok, incr(bar)),
    ?assertEqual(lists:sort([bar, foo]), lists:sort(counters())),
    ?assertEqual(lists:sort([{bar, 1}, {foo, 1}]), lists:sort(get_all())).



test_reset() ->
    ?assertEqual([], counters()),

    ok = incr(foo, 5),
    ?assertEqual(5, get(foo)),

    [{foo, Count}] = get_all(),
    incr(foo, 3),
    ?assertEqual(8, get(foo)),

    ok = reset(foo, Count),
    ?assertEqual(3, get(foo)).


floats() ->
    ?assertError(badarg, get(foo)),
    ?assertError(badarg, incr(foo, 2.5)).



benchmark() ->
    do_benchmark(4, 100000),
    do_benchmark(8, 100000),
    do_benchmark(32, 100000).

do_benchmark(Processes, Writes) ->
    Start = now(),
    Parent = self(),
    Pids = [spawn(fun() ->
                          benchmark_incrementer(foo, Writes),
                          Parent ! {self(), done}
                  end) || _ <- lists:seq(1, Processes)],
    receive_all(Pids, done),
    error_logger:info_msg("~p processes, ~p writes in ~p ms~n",
                          [Processes, Writes, timer:now_diff(now(), Start) / 1000]),
    ok.

receive_all([], _) ->
    ok;
receive_all(Pids, Msg) ->
    receive
        {Pid, Msg} ->
            receive_all(lists:delete(Pid, Pids), Msg)
    end.

benchmark_incrementer(_, 0) ->
    ok;
benchmark_incrementer(Key, N) ->
    incr(Key),
    benchmark_incrementer(Key, N-1).
