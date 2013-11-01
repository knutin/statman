-module(statman_decorators).
-include_lib("eunit/include/eunit.hrl").
-compile([{parse_transform, decorators}]).


-export([runtime/3, reductions/3, memory/3, call_rate/3]).

%%
%% DECORATORS
%%

runtime(Fun, Args, Options) ->
    Key = proplists:get_value(key, Options, erlang:fun_info(Fun, name)),
    Start = os:timestamp(),
    Result = erlang:apply(Fun, Args),
    statman_histogram:record_value(Key, Start),
    Result.

reductions(Fun, Args, Options) ->
    process_info_decorator(Fun, Args, Options, reductions).

memory(Fun, Args, Options) ->
    process_info_decorator(Fun, Args, Options, memory).

call_rate(Fun, Args, Options) ->
    Key = proplists:get_value(key, Options, erlang:fun_info(Fun, name)),
    statman_counter:incr(Key),
    apply(Fun, Args).


%%
%% INTERNAL
%%

process_info_decorator(Fun, Args, Options, InfoKey) ->
    Key = proplists:get_value(key, Options, erlang:fun_info(Fun, name)),
    {InfoKey, Start} = process_info(self(), InfoKey),
    Result = erlang:apply(Fun, Args),
    {InfoKey, End} = process_info(self(), InfoKey),
    statman_histogram:record_value(Key, (End - Start)),
    Result.


-ifdef(TEST).
-decorate({statman_decorators, runtime, [{key, runtime_key}]}).
-decorate({statman_decorators, memory, [{key, memory_key}]}).
-decorate({statman_decorators, reductions, [{key, reductions_key}]}).
-decorate({statman_decorators, call_rate, [{key, rate_key}]}).
decorated_function(A, B) ->
    A + B.

decorators_test() ->
    ok = statman_histogram:init(),
    3 = decorated_function(1, 2),
    ?assertEqual([memory_key, reductions_key, runtime_key],
                 lists:sort(statman_histogram:keys())).
-endif.
