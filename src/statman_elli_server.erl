%% @doc: Subscribers to stats updates and pushes to elli chunked
%% connections once every second
-module(statman_elli_server).
-behaviour(gen_server).

-export([start_link/0, add_client/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {clients = []}).

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

rates(Stats) ->
    lists:map(fun ({FullKey, Rate, _}) ->
                      {Id, Key} = id_key(FullKey),
                      {[{id, Id}, {key, Key}, {rate, Rate}]}
              end, proplists:get_value(rates, Stats, [])).


histograms(Stats) ->
    lists:map(fun ({FullKey, Summary, _}) ->
                      {Id, Key} = id_key(FullKey),
                      {[{id, Id}, {key, Key} | Summary]}
              end, proplists:get_value(histograms, Stats, [])).

gauges(Stats) ->
    lists:map(fun ({FullKey, Value, _}) ->
                      {Id, Key} = id_key(FullKey),
                      {[{id, Id}, {key, Key}, {value, Value}]}
              end, proplists:get_value(gauges, Stats, [])).

id_key({Id, Key}) -> {Id, Key};
id_key(Key) -> {undefined, Key}.
