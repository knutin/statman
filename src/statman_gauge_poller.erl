%% @doc: Statman server, sends reports and owns the ETS tables
-module(statman_gauge_poller).
-behaviour(gen_server).

-export([start_link/0, add_gauge/1, remove_gauge/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {fs = []}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_gauge(F) ->
    gen_server:call(?MODULE, {add_gauge, F}).

remove_gauge(F) ->
    gen_server:call(?MODULE, {remove_gauge, F}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    erlang:send_after(10000, self(), report),
    {ok, #state{fs = []}}.

handle_call({add_gauge, GaugeF}, _From, #state{fs = Fs} = State) ->
    {reply, ok, State#state{fs = [GaugeF | Fs]}};

handle_call({remove_gauge, GaugeF}, _From, #state{fs = Fs} = State) ->
    {reply, ok, State#state{fs = lists:delete(GaugeF, Fs)}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(report, State) ->
    erlang:send_after(10000, self(), report),

    lists:foreach(fun ({K, V}) ->
                          statman_gauge:set(K, V)
                  end, lists:flatmap(fun (F) -> F() end, State#state.fs)),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

