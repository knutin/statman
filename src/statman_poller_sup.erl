-module(statman_poller_sup).
-behaviour(supervisor).

%% API
-export([start_link/1, start_worker/3, delete_worker/1]).
-export([init/1]).

-define(CHILD(I, Type, Args),
        {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).


%%%===================================================================
%%% API
%%%===================================================================

start_link([]) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_worker(Id, TypedF, Interval) ->
    WorkerId = get_worker_id(Id),
    ChildSpec = get_worker(WorkerId, TypedF, Interval),
    case supervisor:start_child(?MODULE, ChildSpec) of
        {error, Reason} ->
            throw({unable_to_start_worker, Id, Reason});
        {ok, _Pid} ->
            {ok, WorkerId}
    end.

delete_worker(Id) ->
    WorkerId = get_worker_id(Id),
    ok = supervisor:terminate_child(?MODULE, WorkerId),
    ok = supervisor:delete_child(?MODULE, WorkerId).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    Children = [?CHILD(statman_poller, worker, [])],
    {ok, {{one_for_one, 5, 10}, Children}}.


%%%===================================================================
%%% Internal functionality
%%%===================================================================

get_worker(Name, TypedF, Interval) ->
    {Name,
     {statman_poller_worker, start_link, [Name, TypedF, Interval]},
     transient, 5000, worker, [statman_poller_worker]
    }.

get_worker_id(Id) ->
    list_to_atom(
      atom_to_list(statman_poller_worker) ++ "_" ++ integer_to_list(Id)
     ).
