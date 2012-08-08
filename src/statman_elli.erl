%% @doc: Elli middleware integrating statman
-module(statman_elli).
-behaviour(elli_handler).
-export([handle/2, handle_event/3]).
-export([start_demo/0, example_logger/0]).

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

example_logger() ->
    Fun = fun(F) ->
                  random:seed(erlang:now()),
                  timer:sleep(random:uniform(50)),
                  statman_counter:incr({http, hits}),
                  statman_counter:incr({db, hits}, 20),
                  statman_gauge:set({db, connections}, random:uniform(10)),
                  statman_gauge:set(runners, random:uniform(10)),

                  case node() of
                      'a@vm' ->
                          statman_counter:incr({node_a, foo});
                      'b@vm'  ->
                          statman_counter:incr({node_b, foo})
                  end,

                  statman_histogram:record_value(
                    {<<"/highscores">>, db_a_latency}, random:uniform(30)),
                  statman_histogram:record_value(
                    {<<"/highscores">>, db_b_latency}, random:uniform(30)),

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
    statman_merger:start_link(),
    statman_aggregator:start_link(),

    ok = rpc:call(A, statman_server, add_subscriber, [{statman_merger, node()}]),
    ok = rpc:call(B, statman_server, add_subscriber, [{statman_merger, node()}]),
    rpc:call(A, statman_gauge_poller, start_link, []),
    rpc:call(B, statman_gauge_poller, start_link, []),

    statman_merger:add_subscriber(statman_elli_server),
    statman_merger:add_subscriber(statman_aggregator),

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

