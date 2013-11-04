-module(statman_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-define(CHILD(I, Type, Args),
        {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

start_link(ReportInterval) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ReportInterval]).

init([ReportInterval]) ->
    Children = [
                ?CHILD(statman_server, worker, [ReportInterval]),
                ?CHILD(statman_poller_registry, worker, []),
                ?CHILD(statman_poller, worker, [])
               ],
    {ok, {{one_for_one, 5, 10}, Children}}.
