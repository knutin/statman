-module(statman_poller_registry).

-behaviour(gen_server).

%% API
-export([add/2,
         get/1,
         get_by_value/1,
         get_all/0,
         delete/1,
         info/0,

         start_link/0,
         stop/0
         ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(DB_NAME,       poller_store).
-define(DB_OPTIONS,    [duplicate_bag, private]).

-record(state, {store}).


%%%============================================================================
%%% API
%%%============================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

info() ->
    gen_server:call(?MODULE, info).

get(Key) ->
    gen_server:call(?MODULE, {get, Key}).

get_by_value(Value) ->
    gen_server:call(?MODULE, {get_by_value, Value}).

get_all() ->
    gen_server:call(?MODULE, get_all).

add(Key, Value) ->
    gen_server:cast(?MODULE, {add, Key, Value}).

delete(Key) ->
    gen_server:cast(?MODULE, {delete, Key}).

%%============================================================================
%% gen_server callbacks
%%============================================================================

init([]) ->
    {ok, #state{store = ets:new(?DB_NAME, ?DB_OPTIONS)}}.

handle_call(info, _From,  #state{store = Store} = State) ->
   {reply, ets:info(Store), State};
handle_call({get, Key}, _From, #state{store = Store} = State) ->
    {reply, ets:lookup(Store, Key), State};
handle_call({get_by_value, Value}, _From,  #state{store = Store} = State) ->
    Result = [{K,V} || {K,V} <- ets:tab2list(Store), V =:= Value],
    {reply, Result, State};
handle_call(get_all, _From, #state{store = Store} = State) ->
    {reply, ets:tab2list(Store), State};
handle_call(stop, _From, State) ->
    {stop, normal, State}.

handle_info(_, State) ->
    {noreply, State}.

handle_cast({add, Key, Val}, #state{store = Store} = State) ->
   ets:insert(Store, {Key, Val}),
   {noreply, State};
handle_cast({delete, Key}, #state{store = Store} = State) ->
   ets:delete(Store, Key),
   {noreply, State}.

terminate(normal, _Store) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%============================================================================
%%% Internal functionality
%%%============================================================================
