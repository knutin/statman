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
            ok = statman_server:add_subscriber(elli_request:chunk_ref(Req)),
            {chunk, [{<<"Content-Type">>, <<"text/event-stream">>}]};

        [<<"statman">>, <<"example">>] ->
            %% statman:request_init(),
            %% statman:identify((identity_fun(Config))(Req)),
            example_logging(Req);

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
            %% statman:request_init(),
            %% statman:identify((identity_fun(Config))(Req)),
            ignore
    end.

handle_event(request_complete, _, _) ->
    %% statman:request_complete(),
    ok;
handle_event(E, A, _) ->
    error_logger:info_msg("~p, ~p~n", [E, A]),
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
example_logging(_Req) ->
    statman:incr(example_reqs),
    statman:incr(foo),
    statman:record_value(db_latency, 100),
    statman:record_value(db_latency, 19923),
    statman:record_value(db_latency, 120),
    {ok, [], <<"Statman logged example metrics">>}.

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

    statman_server:start_link(),
    elli:start_link([{callback, elli_middleware}, {callback_args, Config}]).
