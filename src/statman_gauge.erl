-module(statman_gauge).
-compile([export_all]).

-define(TABLE, statman_gauge).

init() ->
    ets:new(?TABLE, [named_table, public, set]),
    ok.

set(Key, Value) when is_integer(Value) orelse is_float(Value) ->
    ets:insert(?TABLE, {Key, Value}),
    ok.

get_all() ->
    ets:tab2list(?TABLE).
