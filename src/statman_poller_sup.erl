%% @doc: Poller supervisor provides API for starting poller
-module(statman_poller_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_worker/3, delete_worker/1]).
-export([init/1]).

-define(CHILD(I, Type, Args),
        {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).


%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_worker(Id, TypedF, Interval) ->
    ChildSpec = get_worker(Id, TypedF, Interval),
    case supervisor:start_child(?MODULE, ChildSpec) of
        {error, Reason} ->
            throw({unable_to_start_worker, Id, Reason});
        {ok, _Pid} ->
            {ok, get_worker_name(Id)}
    end.

delete_worker(Id) ->
    Name = get_worker_name(Id),
    ok = supervisor:terminate_child(?MODULE, Name),
    ok = supervisor:delete_child(?MODULE, Name).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    Children = [?CHILD(statman_poller, worker, [])],
    {ok, {{one_for_one, 5, 10}, Children}}.


%%%===================================================================
%%% Internal functionality
%%%===================================================================

get_worker(Id, TypedF, Interval) ->
    Name = get_worker_name(Id),
    {Name,
     {statman_poller_worker, start_link, [Name, TypedF, Interval]},
     transient, 5000, worker, [statman_poller_worker]
    }.

get_worker_name(Id) ->
    list_to_atom(
      atom_to_list(statman_poller_worker) ++ "_" ++ integer_to_list(Id)
     ).
