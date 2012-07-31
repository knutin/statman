%% @doc: Histogram backed by ETS and ets:update_counter/3.
%%
%% Calculation of statistics is borrowed from basho_stats_histogram
%% and basho_stats_sample.

-module(statman_histogram).
-export([init/0, record_value/2, clear/1, keys/0, summary/1]).
-compile([native]).
-include_lib("eunit/include/eunit.hrl").

-define(TABLE, statman_histograms).


%%
%% API
%%

init() ->
    ets:new(?TABLE, [named_table, public, set]),
    ok.

record_value(UserKey, Value) when is_integer(Value) ->
    histogram_incr({UserKey, Value}, 1),
    ok.

keys() ->
    %% TODO: Maybe keep a special table of all used keys?
    lists:usort(ets:select(?TABLE, [{ { {'$1', '_'}, '_' }, [], ['$1'] }])).


clear(UserKey) ->
    ets:select_delete(?TABLE, [{{{UserKey, '_'}, '_'}, [], [true]  }]).

%% @doc: Returns a statistical summary of the values recorded with
%% record_value/2. The data is reset at every call.
summary(UserKey) ->
    Data = get_data(UserKey),
    Summary = do_summary(Data),
    reset(UserKey, Data),
    Summary.

%%
%% INTERNAL HELPERS
%%

get_data(UserKey) ->
    Query = [{{{UserKey, '$1'}, '$2'}, [], [{{'$1', '$2'}}]}],
    lists:sort(
      ets:select(?TABLE, Query)).

%% @doc: Decrements the frequency counters with the current values
%% given, effectively resetting while keeping updates written during
%% our stats calculations.
reset(_UserKey, []) ->
    ok;
reset(UserKey, [{Key, Value} | Data]) ->
    ets:update_counter(?TABLE, {UserKey, Key}, -Value),
    reset(UserKey, Data).


do_summary([]) ->
    [];
do_summary(Data) ->
    {N, Sum, Sum2} = scan(Data),

    [{observations, N},
     {min, find_quantile(Data, 0)},
     {median, find_quantile(Data, 0.50 * N)},
     {mean, Sum / N},
     {max, find_quantile(Data, 100 * N)},
     {sd, sd(N, Sum, Sum2)},
     {p95, find_quantile(Data, 0.95 * N)},
     {p99, find_quantile(Data, 0.99 * N)}
    ].

scan(Data) ->
    scan(0, 0, 0, Data).

scan(N, Sum, Sum2, []) ->
    {N, Sum, Sum2};
scan(N, Sum, Sum2, [{Value, Weight} | Rest]) ->
    V = Value * Weight,
    scan(N + Weight, Sum + V, Sum2 + ((Value * Value) * Weight), Rest).


sd(N, _Sum, _Sum2) when N < 2 ->
    'NaN';
sd(N, Sum, Sum2) ->
    SumSq = Sum * Sum,
    math:sqrt((Sum2 - (SumSq / N)) / (N - 1)).






histogram_incr(Key, Incr) ->
    case catch ets:update_counter(?TABLE, Key, Incr) of
        {'EXIT', {badarg, _}} ->
            ets:insert(?TABLE, {Key, Incr}),
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

histogram_test_() ->
    {foreach,
     fun setup/0, fun teardown/1,
     [
      ?_test(test_stats()),
      ?_test(test_histogram()),
      ?_test(test_samples()),
      ?_test(test_reset()),
      ?_test(test_keys())
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
                     {p95, 95},
                     {p99, 99}],
    ?assertEqual(ExpectedStats, do_summary([{N, 3} || N <- lists:seq(1, 100)])).

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
                     {p95, 95},
                     {p99, 99}],

    ?assertEqual(ExpectedStats, summary(key)),

    ?assertEqual(100, clear(key)),
    ?assertEqual([], summary(key)),

    [record_value(key, N) || N <- lists:seq(1, 100)],
    [record_value(key, N) || N <- lists:seq(1, 100)],
    [record_value(key, N) || N <- lists:seq(1, 100)],
    ?assertEqual(ExpectedStats, summary(key)).


test_reset() ->
    [record_value(key, N) || N <- lists:seq(1, 100)],
    Sum = fun () ->
                  lists:sum(
                    ets:select(?TABLE, [{{{key, '_'}, '$1'}, [], ['$1']}]))
          end,
    ?assertEqual(100, Sum()),
    summary(key),
    ?assertEqual(0, Sum()).


test_samples() ->
    %% In R: sd(1:100) = 29.01149
    [record_value(key, N) || N <- lists:seq(1, 100)],
    ?assertEqual(29.011491975882016, proplists:get_value(sd, summary(key))),

    %% ?assertEqual(103, clear(key)),
    %% ?assertEqual('NaN', sd(key)).
    ok.


test_keys() ->
    ?assertEqual([], keys()),

    record_value(foo, 1),
    record_value(bar, 1),
    record_value(baz, 1),

    ?assertEqual([bar, baz, foo], keys()).
