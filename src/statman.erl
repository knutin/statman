-module(statman).
-export([
         incr/1,
         incr/2,
         set_gauge/2,
         incr_gauge/1,
         decr_gauge/1,
         run/2,
         run/3,
         time/2
        ]).

incr(Key)             -> statman_counter:incr(Key).
incr(Key, Increment)  -> statman_counter:incr(Key, Increment).

set_gauge(Key, Value) -> statman_gauge:set(Key, Value).
incr_gauge(Key)       -> statman_gauge:incr(Key).
decr_gauge(Key)       -> statman_gauge:decr(Key).

run(Key, F)           -> statman_histogram:run(Key, F).
run(Key, F, Args)     -> statman_histogram:run(Key, F, Args).

time(Key, Value)      -> statman_histogram:record_value(
                           Key, statman_histogram:bin(Value)).

    








