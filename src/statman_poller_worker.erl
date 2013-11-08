-module(statman_poller_worker).
-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-record(state, {typed_fun :: tuple(),
                parameter :: any(),
                timer_ref :: term()
               }).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Name, TypedF, Interval) ->
    gen_server:start_link({local, Name}, ?MODULE, [TypedF, Interval], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([TypedF, Interval]) ->
    {ok, TRef} = timer:send_interval(Interval, poll),
    {ok, #state{typed_fun = TypedF,
                timer_ref = TRef,
                parameter = undefined}}.

handle_call(_Msg, _From, State) ->
    {reply, unknown_call, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%%TODO: do we need to spawn here?
handle_info(poll, #state{typed_fun = {gauge, F}} = State) ->
    [statman_gauge:set(K, V) || {K, V} <- F()],
    {noreply, State};
handle_info(poll, #state{typed_fun = {counter, F}} = State) ->
    [statman_counter:incr(K, V) || {K, V} <- F()],
    {noreply, State};
handle_info(poll, #state{typed_fun = {histogram, F}} = State) ->
    [statman_histogram:record_value(
       K, statman_histogram:bin(V)) || {K, V} <- F()],
    {noreply, State};
handle_info(poll, #state{typed_fun = _TypedF} = State) ->
    {noreply, State};
handle_info(_, State) ->
    %% Ignore unknown messages, might come from gen calls that timed
    %% out, but response got sent anyway..
    {noreply, State}.

terminate(_Reason, #state{timer_ref = undefined}) ->
    ok;
terminate(_Reason, #state{timer_ref = TRef}) ->
    {ok, cancel} = timer:cancel(TRef),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
