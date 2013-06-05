-module(statman_gauge).
-export([init/0, expire/0, get_all/0]).
-export([set/2, incr/1, incr/2, decr/1, decr/2]).
-include_lib("eunit/include/eunit.hrl").

-define(TABLE, statman_gauges).

init() ->
    ets:new(?TABLE, [named_table, public, set]),
    ok.

set(Key, Value) when is_integer(Value) orelse is_float(Value) ->
    set(Key, Value, now_to_seconds()).

set(Key, Value, Timestamp) ->
    (catch ets:insert(?TABLE, {Key, Timestamp, Value})),
    ok.

incr(Key) -> incr(Key, 1).

decr(Key) -> incr(Key, -1).
decr(Key, Decr) -> incr(Key, -Decr).


incr(Key, Incr) ->
    case catch ets:update_counter(?TABLE, Key, {3, Incr}) of
        {'EXIT', {badarg, _}} ->
            set(Key, Incr),
            ok;
        _ ->
            ets:update_element(?TABLE, Key, {2, now_to_seconds()}),
            ok
    end.

expire() ->
    expire(now_to_seconds() - 60).

%% @doc: Deletes any gauges that has not been updated since the given
%% threshold.
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
      ?_test(test_expire()),
      ?_test(test_expire_incr_decr()),
      ?_test(test_set_incr())
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

test_expire_incr_decr() ->
    ?assertEqual([], get_all()),

    ok = set(problems, 100, now_to_seconds() - 3),
    ok = decr(problems),
    ?assertEqual([{problems, 99}], get_all()),
    ?assertEqual(0, expire(now_to_seconds()-1)),
    ?assertEqual([{problems, 99}], get_all()).


test_set_incr() ->
    incr(foo, 2),
    ?assertEqual([{foo, 2}], get_all()),

    set(foo, 10),
    incr(foo),
    incr(foo),
    ?assertEqual([{foo, 12}], get_all()),

    decr(foo),
    ?assertEqual([{foo, 11}], get_all()).
