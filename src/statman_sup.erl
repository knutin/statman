-module(statman_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-define(CHILD(I, Type, Args),
        {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

start_link([]) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []);
start_link([ReportInterval, StartAggregator]) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE,
                          [ReportInterval, StartAggregator]).

init([]) ->
    Children = get_children(1000, true),
    {ok, {{one_for_one, 5, 10}, Children}};
init([ReportInterval, StartAggregator]) ->
    Children = get_children(ReportInterval, StartAggregator),
    {ok, {{one_for_one, 5, 10}, Children}}.

get_children(ReportInterval, true) ->
    [
     ?CHILD(statman_aggregator, worker, []),
     ?CHILD(statman_server, worker, [ReportInterval]),
     ?CHILD(statman_poller_registry, worker, []),
     ?CHILD(statman_poller, worker, [])
    ];
get_children(ReportInterval, false) ->
    [
     ?CHILD(statman_server, worker, [ReportInterval]),
     ?CHILD(statman_poller_registry, worker, []),
     ?CHILD(statman_poller, worker, [])
    ].
