-module(statman_poller_worker).
-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-record(state, {typed_fun :: tuple(),
                fun_state :: any(),
                timer_ref :: term(),
                interval  :: integer()
               }).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(atom(), statman_poller_sup:typed_fun(), pos_integer())
                -> ignore | {error, any()} | {ok, pid()}.
start_link(Name, TypedF, Interval) ->
    gen_server:start_link({local, Name}, ?MODULE, [TypedF, Interval], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([TypedF, Interval]) ->
    {ok, #state{typed_fun = TypedF,
                timer_ref = start_timer(Interval),
                interval  = Interval,
                fun_state = undefined}}.

handle_call(_Msg, _From, State) ->
    {reply, unknown_call, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%%TODO: do we need to spawn here?
handle_info(poll, #state{typed_fun = {Type, F}, fun_state = FunState} = State) ->
    NewTimer = start_timer(State#state.interval),

    {NewFunState, Updates} = case erlang:fun_info(F, arity) of
                                 {arity, 0} -> {FunState, F()};
                                 {arity, 1} -> F(FunState)
                             end,
    case Type of
        gauge ->
            [statman_gauge:set(K, V) || {K, V} <- Updates];
        counter ->
            [statman_counter:incr(K, V) || {K, V} <- Updates];
        histogram ->
            [statman_histogram:record_value(K, statman_histogram:bin(V))
             || {K, V} <- Updates]
    end,

    {noreply, State#state{fun_state = NewFunState, timer_ref = NewTimer}};

handle_info(_, State) ->
    %% Ignore unknown messages, might come from gen calls that timed
    %% out, but response got sent anyway..
    {noreply, State}.

terminate(_Reason, #state{timer_ref = undefined}) ->
    ok;
terminate(_Reason, #state{timer_ref = TRef}) ->
    erlang:cancel_timer(TRef),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functionality
%%%===================================================================

start_timer(Interval) ->
    erlang:send_after(Interval, self(), poll).
