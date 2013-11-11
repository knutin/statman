%% @doc: Poller server provides API managing pollers inside of supervisor
-module(statman_poller).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([add_gauge/1, add_gauge/2,
         add_counter/1, add_counter/2,
         add_histogram/1, add_histogram/2
        ]).
-export([remove_gauge/1, remove_counter/1, remove_histogram/1]).
-export([get_worker_state/1, get_workers/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {workers :: dict()}).


%%%============================================================================
%%% API
%%%============================================================================

add_gauge(F)               -> add_fun({gauge, F}, 10000).
add_gauge(F, Interval)     -> add_fun({gauge, F}, Interval).
add_counter(F)             -> add_fun({counter, F}, 10000).
add_counter(F, Interval)   -> add_fun({counter, F}, Interval).
add_histogram(F)           -> add_fun({histogram, F}, 10000).
add_histogram(F, Interval) -> add_fun({histogram, F}, Interval).

remove_gauge(F)     -> remove_fun({gauge, F}).
remove_counter(F)   -> remove_fun({counter, F}).
remove_histogram(F) -> remove_fun({histogram, F}).

remove_fun(TypedF) ->
    gen_server:call(?MODULE, {remove, TypedF}).

add_fun(TypedF, Interval) ->
    gen_server:call(?MODULE, {add, TypedF, Interval}).

get_workers() ->
    gen_server:call(?MODULE, get_workers).

get_worker_state(Id) ->
    gen_server:call(?MODULE, {get_worker_state, Id}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%============================================================================
%% gen_server callbacks
%%============================================================================

init([]) ->
    {ok, #state{workers = dict:new()}}.

handle_call({add, TypedF, Interval}, _F, #state{workers = Workers} = State) ->
    {reply, ok, State#state{workers = add_poller(TypedF, Interval, Workers)}};
handle_call({remove, TypedF}, _F, #state{workers = Workers} = State) ->
    {reply, ok, State#state{workers = delete_poller(TypedF, Workers)}};
handle_call({get_worker_state, Id}, _F, #state{workers = Workers} = State) ->
    {reply, dict:find(Id, Workers), State};
handle_call(get_workers, _F, #state{workers = Workers} = State) ->
    {reply, dict:to_list(Workers), State};
handle_call(_Request, _F, State) ->
    {reply, ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
   {noreply, State}.

terminate(normal, _Store) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%============================================================================
%%% Internal functionality
%%%============================================================================

add_poller(TypedF, Interval, Workers) ->
    Id = get_unique_id(),
    statman_poller_sup:start_worker(Id, TypedF, Interval),
    dict:store(Id, {TypedF, Interval}, Workers).

delete_poller(TypedF, Workers) ->
    dict:fold(
      fun(Id, {TF, _I}, Acc) when TF =:= TypedF ->
              statman_poller_sup:delete_worker(Id),
              dict:erase(Id, Acc);
         (_, _, Acc) ->
              Acc
      end,
      Workers,
      Workers
     ).

get_unique_id() ->
    erlang:phash2({node(), now()}).
