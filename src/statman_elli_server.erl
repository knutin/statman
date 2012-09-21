%% @doc: Subscribers to stats updates and pushes to elli chunked
%% connections once every second
-module(statman_elli_server).
-behaviour(gen_server).

-export([start_link/0, add_client/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {clients = []}).
-define(COUNTERS_TABLE, statman_elli_server_counters).
-define(a2b(A), list_to_binary(atom_to_list(A))).

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
    timer:send_interval(1000, pull),
    {ok, #state{clients = []}}.

handle_call({add_client, Ref}, _From, #state{clients = Clients} = State) ->
    io:format("adding client: ~p~n", [Ref]),
    {reply, ok, State#state{clients = [Ref | Clients]}}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(pull, State) ->
    case catch statman_aggregator:get_window(1) of
        {ok, Metrics} ->
            Json = lists:flatmap(fun metric2stats/1, Metrics),
            Chunk = ["data: ", jiffy:encode({[{metrics, Json}]}), "\n\n"],
            NewClients = notify_subscribers(State#state.clients, Chunk),

            {noreply, State#state{clients = NewClients}};
        {'EXIT', _} ->
            error_logger:info_msg("crash~n"),
            {noreply, State}
    end.

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
                      elli_request:send_chunk(Sub, <<"">>),
                      [];
                  {error, timeout} ->
                      []
              end
      end, Subscribers).

window(Metric) ->
    proplists:get_value(window, Metric, 1000) / 1000.

value(Metric) ->
    proplists:get_value(value, Metric).

get_node(Metric) ->
    proplists:get_value(node, Metric).



metric2stats(Metric) ->
    case proplists:get_value(type, Metric) of
        histogram ->
            {Id, Key} = id_key(Metric),
            Summary = statman_histogram:summary(value(Metric)),
            Num = proplists:get_value(observations, Summary, 0),
            case Num of
                0 ->
                    [];
                _ ->
                    [{[
                       {id, Id}, {key, Key},
                       {type, histogram},
                       {rate, Num / window(Metric)},
                       {node, get_node(Metric)}
                       | Summary]}]
            end;
        counter ->
            {Id, Key} = id_key(Metric),
            CounterKey = {get_node(Metric), id_key(Metric)},
            Prev = prev_count(CounterKey),

            Delta = value(Metric) - Prev,
            ets:insert(?COUNTERS_TABLE, {CounterKey, value(Metric)}),
            [{[{id, Id}, {key, Key},
               {type, counter},
               {node, get_node(Metric)},
               {rate, Delta / window(Metric)}]}];
        gauge ->
            {Id, Key} = id_key(Metric),
            [{[{id, Id}, {key, Key},
               {type, gauge},
               {node, get_node(Metric)},
               {value, value(Metric)}]}]
    end.


prev_count(Key) ->
    case ets:lookup(?COUNTERS_TABLE, Key) of
        [{Key, Count}] ->
            Count;
        [] ->
            0
    end.

id_key(Metric) ->
    case proplists:get_value(key, Metric) of
        {Id, Key} when is_binary(Id) -> {Id, key(Key)};
        Key -> {null, key(Key)}
    end.

key({A, B}) when is_atom(A) andalso is_atom(B) -> <<(?a2b(A))/binary, "/", (?a2b(B))/binary>>;
key(A) when is_atom(A) -> ?a2b(A).
