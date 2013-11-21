%% @doc: Poller backwards compatibilty API helper
-module(statman_poller).

%% API
-export([add_gauge/1, add_gauge/2,
         add_counter/1, add_counter/2,
         add_histogram/1, add_histogram/2
        ]).
-export([remove_gauge/1, remove_counter/1, remove_histogram/1]).


%%%===================================================================
%%% API
%%%===================================================================

-spec add_gauge(fun()) -> ok.
add_gauge(F) -> add_worker({gauge, F}, 10000).

-spec add_gauge(fun(), pos_integer()) -> ok.
add_gauge(F, Interval) -> add_worker({gauge, F}, Interval).

-spec add_counter(fun()) -> ok.
add_counter(F) -> add_worker({counter, F}, 10000).

-spec add_counter(fun(), pos_integer()) -> ok.
add_counter(F, Interval) -> add_worker({counter, F}, Interval).

-spec add_histogram(fun()) -> ok.
add_histogram(F) -> add_worker({histogram, F}, 10000).

-spec add_histogram(fun(), pos_integer()) -> ok.
add_histogram(F, Interval) -> add_worker({histogram, F}, Interval).

-spec remove_gauge(fun()) -> ok.
remove_gauge(F) -> remove_worker({gauge, F}).

-spec remove_counter(fun()) -> ok.
remove_counter(F) -> remove_worker({counter, F}).

-spec remove_histogram(fun()) -> ok.
remove_histogram(F) -> remove_worker({histogram, F}).


%%%===================================================================
%%% Internal functionality
%%%===================================================================

add_worker(TypedF, Interval) ->
    {ok, _Pid} = statman_poller_sup:add_worker(TypedF, Interval),
    ok.

remove_worker(TypedF) ->
    statman_poller_sup:remove_worker(TypedF).
