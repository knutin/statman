%% @doc: Elli middleware integrating statman
-module(statman_elli).
-behaviour(elli_handler).
-export([handle/2, handle_event/3]).
-export([start_demo/0, example_logger/0]).

-export([id_key/1]).

%%
%% ELLI CALLBACKS
%%

handle(Req, Config) ->
    case elli_request:path(Req) of
        [<<"statman">>] ->
            file:read_file(filename:join([docroot(Config), "index.html"]));

        [<<"statman">>, <<"stream">>] ->
            ok = statman_elli_server:add_client(elli_request:chunk_ref(Req)),
            {chunk, [{<<"Content-Type">>, <<"text/event-stream">>}]};

        [<<"statman">>, <<"summary">>] ->
            WindowSize = list_to_integer(
                           binary_to_list(
                             elli_request:get_arg(<<"window">>, Req, <<"300">>))),
            {ok, Metrics} = statman_aggregator:get_window(WindowSize),

            {ok, [{<<"Content-Type">>, <<"application/json">>}],
             jiffy:encode({[{metrics, lists:flatmap(fun metric2stats/1, Metrics)}]})};

        [<<"statman">>, <<"media">> | Path] ->
            Filepath = filename:join([docroot(Config) | Path]),
            valid_path(Filepath) orelse throw({403, [], <<"Permission denied">>}),
            case file:read_file(Filepath) of
                {ok, Bin} ->
                    {ok, Bin};
                {error, enoent} ->
                    {404, <<"Not found">>}
            end;
        _ ->
            rscope:identity((identity_fun(Config))(Req)),
            ignore
    end.

handle_event(request_complete, _, _) ->
    rscope:request_complete(),
    ok;
handle_event(_, _, _) ->
    ok.



%%
%% INTERNAL HELPERS
%%

identity_fun(Config) ->
    proplists:get_value(identity_fun, Config, fun (_Req) ->
                                                      <<"undefined">>
                                              end).
docroot(Config) ->
    proplists:get_value(docroot, Config, []).


valid_path(Path) ->
    case binary:match(Path, <<"..">>) of
        {_, _} -> false;
        nomatch -> true
    end.



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

            [{[{id, Id}, {key, Key},
               {type, counter},
               {node, get_node(Metric)},
               {value, value(Metric)}]}];
        gauge ->
            {Id, Key} = id_key(Metric),
            [{[{id, Id}, {key, Key},
               {type, gauge},
               {node, get_node(Metric)},
               {value, value(Metric)}]}]
    end.



id_key(Metric) ->
    case proplists:get_value(key, Metric) of
        {Id, Key} when is_binary(Id) -> {Id, key(Key)};
        Key -> {null, key(Key)}
    end.


key({A, B}) ->
    <<(key(A))/binary, "/", (key(B))/binary>>;

key(A) when is_atom(A) ->
    atom_to_binary(A, utf8);
key(B) when is_binary(B) ->
    B.


window(Metric) ->
    proplists:get_value(window, Metric, 1000) / 1000.

value(Metric) ->
    proplists:get_value(value, Metric).

get_node(Metric) ->
    proplists:get_value(node, Metric).


example_logger() ->
    Fun = fun(F) ->
                  random:seed(erlang:now()),
                  timer:sleep(random:uniform(50)),
                  statman_counter:incr({http, hits}),
                  statman_counter:incr({db, hits}, 20),
                  statman_gauge:set({db, connections}, random:uniform(10)),
                  statman_gauge:set(runners, random:uniform(10)),

                  statman_counter:incr({node(), foo}),

                  statman_histogram:record_value(
                    {<<"/highscores">>, total}, random:uniform(100) * 1000),

                  statman_histogram:record_value(
                    {<<"/highscores">>, {db, highscores}}, random:uniform(30) * 1000),

                  statman_histogram:record_value(
                    {<<"/highscores">>, {db, highscores}}, random:uniform(70) * 1000),

                  statman_histogram:record_value(
                    {<<"/highscores">>, {db, highscores}}, random:uniform(90) * 1000),

                  statman_histogram:record_value(
                    {<<"/highscores">>, {http, call_user}}, random:uniform(50) * 1000),

                  statman_histogram:record_value(
                    {<<"/highscores">>, {user, call_location}}, random:uniform(20) * 1000),

                  statman_histogram:record_value(
                    {<<"/highscores">>, {location, game_logic}}, random:uniform(10) * 1000),

                  F(F)
          end,

    spawn(fun() -> Fun(Fun) end).


%%
%% DEMO
%%

start_demo() ->
    IdentityFun =
        fun (Req) ->
                case elli_request:path(Req) of
                    [<<"favicon.ico">>] -> <<"favicon">>;
                    [<<"hello">>] -> <<"hello">>;
                    [<<"hello">>, <<"world">>] -> <<"hello/world">>;
                    [<<"statman">>, <<"example">>] -> <<"/statman/example">>;
                    _ -> <<"unknown">>
                end
        end,
    StatsConfig = [{name, elli_stats_demo},
                   {docroot, "priv/docroot"},
                   {identity_fun, IdentityFun}],

    Config = [
              {mods, [
                      {statman_elli, StatsConfig},
                      {elli_example_callback, []}
                     ]}
             ],


    A = setup(a),
    B = setup(b),

    rscope:init(),
    statman_elli_server:start_link(),
    statman_aggregator:start_link(),

    ok = rpc:call(A, statman_server, add_subscriber, [{statman_aggregator, node()}]),
    ok = rpc:call(B, statman_server, add_subscriber, [{statman_aggregator, node()}]),
    rpc:call(A, statman_gauge_poller, start_link, []),
    rpc:call(B, statman_gauge_poller, start_link, []),

    %% statman_merger:add_subscriber(statman_elli_server),
    %% statman_merger:add_subscriber(statman_aggregator),

    %% statman_gauge_poller:start_link(),


    elli:start_link([{callback, elli_middleware}, {callback_args, Config}]),

    ok.


setup(Name) ->
    error_logger:info_msg("starting ~p~n", [Name]),
    {ok, Node} = slave:start_link(list_to_atom(net_adm:localhost()), Name),

    true = rpc:call(Node, code, add_path, ["/home/knutin/git/statman/ebin"]),
    rpc:call(Node, statman_server, start_link, [1000]),
    spawn(Node, ?MODULE, example_logger, []),
    Node.

