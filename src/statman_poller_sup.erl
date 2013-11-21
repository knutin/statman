%% @doc: Poller supervisor provides API for starting poller
-module(statman_poller_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([init/1]).
-export([add_gauge/1, add_gauge/2,
         add_counter/1, add_counter/2,
         add_histogram/1, add_histogram/2
        ]).
-export([get_workers/0]).
-export([remove_gauge/1, remove_counter/1, remove_histogram/1]).
-export([add_worker/2, remove_worker/1]).

%% Types
-type types() :: gauge | counter | histogram.
-type typed_fun() :: {types(), fun()}.
-export_type([typed_fun/0, types/0]).


%%%===================================================================
%%% API
%%%===================================================================

-spec add_gauge(fun()) -> {ok, pid()}.
add_gauge(F) -> add_worker({gauge, F}, 10000).

-spec add_gauge(fun(), pos_integer()) -> {ok, pid()}.
add_gauge(F, Interval) -> add_worker({gauge, F}, Interval).

-spec add_counter(fun()) -> {ok, pid()}.
add_counter(F) -> add_worker({counter, F}, 10000).

-spec add_counter(fun(), pos_integer()) -> {ok, pid()}.
add_counter(F, Interval) -> add_worker({counter, F}, Interval).

-spec add_histogram(fun()) -> {ok, pid()}.
add_histogram(F) -> add_worker({histogram, F}, 10000).

-spec add_histogram(fun(), pos_integer()) -> {ok, pid()}.
add_histogram(F, Interval) -> add_worker({histogram, F}, Interval).

-spec remove_gauge(fun()) -> ok.
remove_gauge(F) -> remove_worker({gauge, F}).

-spec remove_counter(fun()) -> ok.
remove_counter(F) -> remove_worker({counter, F}).

-spec remove_histogram(fun()) -> ok.
remove_histogram(F) -> remove_worker({histogram, F}).

-spec get_workers() -> list().
get_workers() ->
    supervisor:which_children(?MODULE).

-spec start_link() -> ignore | {error, any()} | {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec add_worker(typed_fun(), pos_integer()) -> {ok, pid()}.
add_worker(TypedF, Interval) ->
    Id = get_unique_id(TypedF),
    ChildSpec = get_worker_spec(Id, TypedF, Interval),
    case supervisor:start_child(?MODULE, ChildSpec) of
        {error, Reason} ->
            throw({unable_to_start_worker, Id, Reason});
        {ok, Pid} ->
            {ok, Pid}
    end.

-spec remove_worker(typed_fun()) -> ok.
remove_worker(TypedF) ->
    Name = get_worker_name(get_unique_id(TypedF)),
    ok = supervisor:terminate_child(?MODULE, Name),
    ok = supervisor:delete_child(?MODULE, Name),
    ok.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    {ok, {{one_for_one, 5, 10}, []}}.


%%%===================================================================
%%% Internal functionality
%%%===================================================================

get_worker_spec(Id, TypedF, Interval) ->
    Name = get_worker_name(Id),
    {Name,
     {statman_poller_worker, start_link, [Name, TypedF, Interval]},
     transient, 5000, worker, [statman_poller_worker]
    }.

get_worker_name(Id) ->
    list_to_atom(
      atom_to_list(statman_poller_worker) ++ "_" ++ integer_to_list(Id)
     ).

get_unique_id(TypedF) ->
    erlang:phash2(TypedF).
