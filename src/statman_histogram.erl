%% @doc: Histogram backed by ETS and ets:update_counter/3.
%%
%% Calculation of statistics is borrowed from basho_stats_histogram
%% and basho_stats_sample.

-module(statman_histogram).
-export([init/0,
         record_value/2,
         run/2,
         run/3,
         clear/1,
         keys/0,
         get_data/1,
         summary/1,
         reset/2,
         gc/0]).

-export([bin/1]).

-compile([native]).

-define(TABLE, statman_histograms).


%%
%% API
%%

init() ->
    ets:new(?TABLE, [named_table, public, set, {write_concurrency, true}]),
    ok.

record_value(UserKey, {MegaSecs, Secs, MicroSecs}) when
      is_integer(MegaSecs) andalso MegaSecs >= 0 andalso
      is_integer(Secs) andalso Secs >=0 andalso
      is_integer(MicroSecs) andalso MicroSecs >= 0 ->
    record_value(UserKey,
                 bin(timer:now_diff(now(), {MegaSecs, Secs, MicroSecs})));

record_value(UserKey, Value) when is_integer(Value) ->
    histogram_incr({UserKey, Value}, 1),
    ok.


run(Key, F) ->
    Start = os:timestamp(),
    Result = F(),
    record_value(Key, Start),
    Result.

run(Key, F, Args) ->
    Start = os:timestamp(),
    Result = erlang:apply(F, Args),
    record_value(Key, Start),
    Result.


keys() ->
    %% TODO: Maybe keep a special table of all used keys?
    lists:usort(ets:select(?TABLE, [{ { {'$1', '_'}, '_' }, [], ['$1'] }])).

gc() ->
    ets:select_delete(?TABLE, [{ {{'_', '_'}, 0}, [], [true] }]).

clear(UserKey) ->
    ets:select_delete(?TABLE, [{{{UserKey, '_'}, '_'}, [], [true]  }]).


%% @doc: Returns the raw histogram recorded by record_value/2,
%% suitable for passing to summary/1 and reset/2
get_data(UserKey) ->
    Query = [{{{UserKey, '$1'}, '$2'}, [{'>', '$2', 0}], [{{'$1', '$2'}}]}],
    lists:sort(
      ets:select(?TABLE, Query)).


%% @doc: Returns summary statistics from the raw data
summary([]) ->
    [];
summary(Data) ->
    {N, Sum, Sum2, Max} = scan(Data),

    [{observations, N},
     {min, find_quantile(Data, 0)},
     {median, find_quantile(Data, 0.50 * N)},
     {mean, Sum / N},
     {max, Max},
     {sd, sd(N, Sum, Sum2)},
     {sum, Sum},
     {sum2, Sum2},
     {p25, find_quantile(Data, 0.25 * N)},
     {p75, find_quantile(Data, 0.75 * N)},
     {p95, find_quantile(Data, 0.95 * N)},
     {p99, find_quantile(Data, 0.99 * N)},
     {p999, find_quantile(Data, 0.999 * N)}
    ].



%% @doc: Decrements the frequency counters with the current values
%% given, effectively resetting while keeping updates written during
%% our stats calculations.
reset(_UserKey, []) ->
    ok;
reset(UserKey, [{Key, Value} | Data]) ->
    ets:update_counter(?TABLE, {UserKey, Key}, -Value),
    reset(UserKey, Data).


%%
%% INTERNAL HELPERS
%%
-spec bin(integer()) -> integer().
bin(0) -> 0;
bin(N) ->
    Binner =
        if N < 10000 -> 1000;
           true ->
                %% keep 2 digits
                round(math:pow(10, trunc(math:log10(N)) - 1))
        end,
    case (N div Binner) * Binner of
        0 ->
            1;
        Bin ->
            Bin
    end.

scan(Data) ->
    scan(0, 0, 0, 0, Data).

scan(N, Sum, Sum2, Max, []) ->
    {N, Sum, Sum2, Max};
scan(N, Sum, Sum2, Max, [{Value, Weight} | Rest]) ->
    V = Value * Weight,
    scan(N + Weight,
         Sum + V,
         Sum2 + ((Value * Value) * Weight),
         max(Max, Value),
         Rest).


sd(N, _Sum, _Sum2) when N < 2 ->
    'NaN';
sd(N, Sum, Sum2) ->
    SumSq = Sum * Sum,
    math:sqrt((Sum2 - (SumSq / N)) / (N - 1)).


histogram_incr(Key, Incr) ->
    case catch ets:update_counter(?TABLE, Key, Incr) of
        {'EXIT', {badarg, _}} ->
            (catch ets:insert(?TABLE, {Key, Incr})),
            ok;
        _ ->
            ok
    end.

find_quantile(Freqs, NeededSamples) ->
    find_quantile(Freqs, 0, NeededSamples).

find_quantile([{Value, _Freq} | []], _Samples, _NeededSamples) ->
    Value;
find_quantile([{Value, Freq} | Rest], Samples, NeededSamples) ->
    Samples2 = Samples + Freq,
    if
        Samples2 < NeededSamples ->
            find_quantile(Rest, Samples2, NeededSamples);
        true ->
            Value
    end.



%%
%% TESTS
%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

histogram_test_() ->
    {foreach,
     fun setup/0, fun teardown/1,
     [
      ?_test(test_stats()),
      ?_test(test_histogram()),
      ?_test(test_samples()),
      ?_test(test_reset()),
      ?_test(test_gc()),
      ?_test(test_keys()),
      ?_test(test_binning()),
      ?_test(test_run())
     ]
    }.

setup() ->
    init(),
    [?TABLE].

teardown(Tables) ->
    lists:map(fun ets:delete/1, Tables).

test_stats() ->
    ExpectedStats = [{observations, 300},
                     {min, 1},
                     {median, 50},
                     {mean, 50.5},
                     {max, 100},
                     {sd, 28.914300774835606}, %% Checked with R
                     {sum, 15150},
                     {sum2, 1015050},
                     {p25, 25},
                     {p75, 75},
                     {p95, 95},
                     {p99, 99},
                     {p999, 100}],
    ?assertEqual(ExpectedStats, summary([{N, 3} || N <- lists:seq(1, 100)])).

test_histogram() ->
    [record_value(key, N) || N <- lists:seq(1, 100)],
    [record_value(key, N) || N <- lists:seq(1, 100)],
    [record_value(key, N) || N <- lists:seq(1, 100)],

    ExpectedStats = [{observations, 300},
                     {min, 1},
                     {median, 50},
                     {mean, 50.5},
                     {max, 100},
                     {sd, 28.914300774835606}, %% Checked with R
                     {sum, 15150},
                     {sum2, 1015050},
                     {p25, 25},
                     {p75, 75},
                     {p95, 95},
                     {p99, 99},
                     {p999, 100}],

    ?assertEqual(ExpectedStats, summary(get_data(key))),

    ?assertEqual(100, clear(key)),
    ?assertEqual([], summary(get_data(key))),

    [record_value(key, N) || N <- lists:seq(1, 100)],
    [record_value(key, N) || N <- lists:seq(1, 100)],
    [record_value(key, N) || N <- lists:seq(1, 100)],
    ?assertEqual(ExpectedStats, summary(get_data(key))).

test_gc() ->
    [record_value(key, N) || N <- lists:seq(1, 100)],
    ?assertEqual(100, proplists:get_value(observations, summary(get_data(key)))),

    ?assertEqual([{{key, 5}, 1}], ets:lookup(?TABLE, {key, 5})),
    ?assertEqual(0, gc()),

    record_value(other_key, 42),

    reset(key, get_data(key)),
    ?assertEqual(100, gc()),
    ?assertEqual(0, gc()),

    ?assertEqual([], get_data(key)),
    ?assertEqual([{42, 1}], get_data(other_key)),
    ok.

test_reset() ->
    [record_value(key, N) || N <- lists:seq(1, 100)],
    Sum = fun () ->
                  lists:sum(
                    ets:select(?TABLE, [{{{key, '_'}, '$1'}, [], ['$1']}]))
          end,
    ?assertEqual(100, Sum()),
    reset(key, get_data(key)),
    ?assertEqual(0, Sum()).


test_samples() ->
    %% In R: sd(1:100) = 29.01149
    [record_value(key, N) || N <- lists:seq(1, 100)],
    ?assertEqual(29.011491975882016,
                 proplists:get_value(sd, summary(get_data(key)))),

    %% ?assertEqual(103, clear(key)),
    %% ?assertEqual('NaN', sd(key)).
    ok.


test_keys() ->
    ?assertEqual([], keys()),

    record_value(foo, 1),
    record_value(bar, 1),
    record_value(baz, 1),

    ?assertEqual([bar, baz, foo], keys()).


test_binning() ->
    random:seed({1, 2, 3}),
    Values = [random:uniform(1000000) || _ <- lists:seq(1, 1000)],

    [record_value(foo, V) || V <- Values],
    _NormalSummary = summary(get_data(foo)),
    reset(foo, get_data(foo)),

    [record_value(foo, bin(V)) || V <- Values],
    _BinnedSummary = summary(get_data(foo)),

    ok.


bin_test() ->
    ?assertEqual(0, bin(0)),
    ?assertEqual(1, bin(1)),
    ?assertEqual(1, bin(999)),
    ?assertEqual(1000, bin(1000)),
    ?assertEqual(1000, bin(1001)),
    ?assertEqual(2000, bin(2000)),
    ?assertEqual(1000, bin(1010)),
    ?assertEqual(1000, bin(1100)),
    ?assertEqual(10000, bin(10001)),
    ?assertEqual(10000, bin(10010)),
    ?assertEqual(10000, bin(10010)),
    ?assertEqual(10000, bin(10100)),
    ?assertEqual(11000, bin(11000)),
    ?assertEqual(12000000, bin(12345678)),
    ?assertEqual(120000000, bin(123456789)).


test_run() ->
    ?assertEqual([], keys()),
    2 = run(foo, fun () -> 1 + 1 end),
    ?assertEqual([foo], keys()),

    2 = run(bar, fun (A, B) -> A + B end, [1, 1]),
    ?assertEqual([bar, foo], keys()).

-endif. %% TEST
