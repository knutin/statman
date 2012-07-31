-module(statman_counter).
-export([init/0, counters/0, get/1, get_all/0]).
-export([incr/1, incr/2, decr/1, decr/2, set/2]).
-compile([{no_auto_import, [get/1]}]).
-include_lib("eunit/include/eunit.hrl").


-define(TABLE, skald_counters).


%%
%% API
%%

init() ->
    ets:new(?TABLE, [named_table, public, set]),
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


%%
%% INTERNAL HELPERS
%%

set(Key, Value) ->
    case catch ets:update_element(?TABLE, Key, Value) of
        {'EXIT', {badarg, _}} ->
            ets:insert(?TABLE, {Key, Value}),
            ok;
        _ ->
            ok
    end.


incr(Key, Incr) ->
    case catch ets:update_counter(?TABLE, Key, Incr) of
        {'EXIT', {badarg, _}} ->
            ets:insert(?TABLE, {Key, Incr}),
            ok;
        _ ->
            ok
    end.




%%
%% TESTS
%%

counter_test_() ->
    {foreach,
     fun setup/0, fun teardown/1,
     [
      ?_test(test_operations()),
      ?_test(find_counters())
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
