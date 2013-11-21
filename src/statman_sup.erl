-module(statman_sup).
-behaviour(supervisor).

%% API
-export([start_link/1]).
-export([init/1]).

-define(CHILD(I, Type, Args),
        {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%%%===================================================================
%%% API
%%%===================================================================

start_link([]) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [1000]);
start_link([ReportInterval]) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ReportInterval]).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([ReportInterval]) ->
    Children = [
                ?CHILD(statman_server, worker, [ReportInterval]),
                ?CHILD(statman_poller_sup, supervisor, [])
               ],
    {ok, {{one_for_one, 5, 10}, Children}}.
