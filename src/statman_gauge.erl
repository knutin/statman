-module(statman_gauge).
-export([init/0, set/2, expire/0, get_all/0]).
-include_lib("eunit/include/eunit.hrl").

-define(TABLE, statman_gauges).

init() ->
    ets:new(?TABLE, [named_table, public, set]),
    ok.

set(Key, Value) when is_integer(Value) orelse is_float(Value) ->
    set(Key, Value, now_to_seconds()).

set(Key, Value, Timestamp) ->
    ets:insert(?TABLE, {Key, Timestamp, Value}),
    ok.

expire() ->
    expire(now_to_seconds() - 60).

expire(Threshold) ->
    ets:select_delete(?TABLE, [{{'_', '$1', '_'}, [{'<', '$1', Threshold}], [true]}]).

get_all() ->
    ets:select(?TABLE, [{ {'$1', '_', '$2'}, [], [{{'$1', '$2'}}]}]).

now_to_seconds() ->
    {MegaSeconds, Seconds, _} = os:timestamp(),
    MegaSeconds * 1000000 + Seconds.


%%
%% TESTS
%%

gauge_test_() ->
    {foreach,
     fun setup/0, fun teardown/1,
     [
      ?_test(test_expire())
     ]
    }.

setup() ->
    init(),
    [?TABLE].

teardown(Tables) ->
    lists:map(fun ets:delete/1, Tables).


test_expire() ->
    ?assertEqual([], get_all()),
    set(foo, 30, now_to_seconds() - 3),
    ?assertEqual([{foo, 30}], get_all()),
    ?assertEqual(0, expire(now_to_seconds() - 5)),
    ?assertEqual(1, expire(now_to_seconds() - 0)),
    ?assertEqual([], get_all()).
