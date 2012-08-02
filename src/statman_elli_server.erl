%% @doc: Subscribers to stats updates and pushes to elli chunked
%% connections once every second
-module(statman_elli_server).
-behaviour(gen_server).

-export([start_link/0, add_client/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {clients = []}).
-define(COUNTERS_TABLE, statman_elli_server_counters).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_client(Ref) ->
    gen_server:call(?MODULE, {add_client, Ref}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ets:new(?COUNTERS_TABLE, [named_table, protected, set]),
    {ok, #state{clients = []}}.

handle_call({add_client, Ref}, _From, #state{clients = Clients} = State) ->
    {reply, ok, State#state{clients = [Ref | Clients]}}.

handle_cast({statman_update, Stats}, State) ->
    Json = {[{rates, rates(Stats)},
             {histograms, histograms(Stats)},
             {gauges, gauges(Stats)}]},
    Chunk = ["data: ", jiffy:encode(Json), "\n\n"],
    NewClients = notify_subscribers(State#state.clients, Chunk),

    {noreply, State#state{clients = NewClients}}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

notify_subscribers(Subscribers, Chunk) ->
    lists:flatmap(
      fun (Sub) ->
              case elli_request:send_chunk(Sub, Chunk) of
                  ok ->
                      [Sub];
                  {error, closed} ->
                      [];
                  {error, timeout} ->
                      []
              end
      end, Subscribers).

window(Stats) ->
    proplists:get_value(window, Stats, 1000).

rates(Stats) ->
    lists:flatmap(fun ({FullKey, Count}) ->
                      {Id, Key} = id_key(FullKey),
                      case Count - prev_count(FullKey) of
                          0 ->
                              ets:insert(?COUNTERS_TABLE, {Key, Count}),
                              [];
                          Delta ->
                              ets:insert(?COUNTERS_TABLE, {Key, Count}),
                              [{[{id, Id}, {key, Key},
                                 {rate, Delta / window(Stats)}]}]
                      end
                  end, proplists:get_value(counters, Stats, [])).

prev_count(Key) ->
    case ets:lookup(?COUNTERS_TABLE, Key) of
        [{Key, Count}] ->
            Count;
        [] ->
            0
    end.

histograms(Stats) ->
    lists:map(fun ({FullKey, Raw}) ->
                      {Id, Key} = id_key(FullKey),
                      {[{id, Id}, {key, Key} | statman_histogram:summary(Raw)]}
              end, proplists:get_value(histograms, Stats, [])).

gauges(Stats) ->
    lists:map(fun ({FullKey, Value}) ->
                      {Id, Key} = id_key(FullKey),
                      {[{id, Id}, {key, Key}, {value, Value}]}
              end, proplists:get_value(gauges, Stats, [])).

id_key({Id, Key}) -> {Id, Key};
id_key(Key) -> {undefined, Key}.
