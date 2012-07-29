%% @doc: Elli middleware integrating statman
-module(statman_elli).
-behaviour(elli_handler).
-export([handle/2, handle_event/3]).
-export([start_demo/0]).

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

        [<<"statman">>, <<"example_logging">>] ->
            rscope:identity(<<"/statman/example_logging">>),
            example_logging(),
            {ok, [], <<"OK">>};

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

%% @doc: Creates example counters and histograms
example_logging() ->
    statman:incr(example_reqs),
    statman:incr(foo),

    statman:record_value(db_latency, 19923),
    statman:record_value(db_latency, 120).


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


    statman_elli_server:start_link(),

    statman_server:start_link(),
    statman_server:add_subscriber(statman_elli_server),

    statman_gauge_poller:start_link(),

    elli:start_link([{callback, elli_middleware}, {callback_args, Config}]).
