-module(statman_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [reporter()],
    {ok, {{one_for_one, 5, 10}, Children}}.


reporter() ->
    {statman_reporter, {statman_reporter, start_link, []},
     permanent, 2000, worker, []}.
