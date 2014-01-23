# statman - Statistics man to the rescue!

Statman makes it possible to instrument and collect statistics from
your high-traffic production Erlang systems with very low
overhead. The collected data points are aggregated in the VM and can
be sent to services like Graphite, Munin, New Relic, etc.

Statman uses in-memory ETS tables for low overhead logging and to
avoid single process bottlenecks. See "How does it work" below.

Integration options:

 * [statman_elli][]: real-time (mobile friendly) web
   dashboard. Exposes a small web app and a HTTP API where external
   tools like Munin(plugin included), Librato, etc, can pull
   aggregated stats.

 * [newrelic-erlang][]: Track web transactions happening in any Erlang
   webserver in New Relic, a hosted application monitoring service.

 * [statman_graphite][]: Push data to a Graphite instance, also works
   with hostedgraphite.com.

 * [hatman][]: Push data to stathat


## Usage

Add `statman_server` to one of your supervisors with the following
child specification. You can adjust the poll interval to your liking,
it determines how frequently metrics will be pushed to the
subscribers:


```erlang
    {statman_server, {statman_server, start_link, [1000]},
     permanent, 5000, worker, []}.
```

Statman offers three data types. Here's how to use them:

```erlang
%% Counters measure the frequency of an event
statman_counter:incr(my_queue_in).

%% A gauge is a point in time snapshot of a value
statman_gauge:set(queue_size, N).

%% Histograms show you the distribution of values
Result = statman:run({foo, bar}, fun () -> do_something() end)
```

Updates to counters, gauges and histograms involves one atomic write
in ETS.


## Decorators

You can instrument a function using one of the supplied decorators:

```erlang
-decorate({statman_decorators, call_rate}).
my_function(A, B) ->
    A + B.

-decorate({statman_decorators, runtime, [{key, {statman, key}}]}).
other_function(foo) ->
    bar.
```

## `statman_poller`

It's quite common to want to poll something at an interval, like
memory usage, reduction counts, etc. To this end, Statman includes
`statman_poller` which can run functions at intervals on your
behalf. Add the supervisor to your supervision tree with the following
child specification:

```erlang
    {statman_poller_sup, {statman_poller_sup, start_link, []},
        permanent, 5000, worker, []}]}}.
```

In your app startup, you can then create pollers, which will be
restarted if they crash and shut down together with your application:

```erlang
queue_sizes() ->
    [{my_queue_size, my_queue:get_size()},
     {other_queue, foo:queue_size()}].

app_setup() ->
    ok = statman_poller:add_gauge(fun ?MODULE:queue_sizes/0, 1000).
```

A polling function can also be "stateful". Allowing you to measure the
rate of change in an absolute number. If the function has arity 1, it
will be passed the state and expected to return a new state:

```erlang
widget_rate(undefined) ->
    TotalWidgets = count_total_widgets(),
    {TotalWidgets, []};
widget_rate(PrevTotalWidgets) ->
    TotalWidgets = count_total_widgets(),
    {TotalWidgets, [{created_widgets, TotalWidgets - PrevTotalWidgets}]}.

app_setup() ->
    ok = statman_poller:add_counter(fun ?MODULE:widget_rate/1, 1000).
```

It's important to pass a function reference rather than the function
itself, to make code upgrades smoother.

## How does it work

Using `ets:update_counter/3` we get very efficient atomic increments /
decrements of counters. With this primitive, counters, gauges and
histograms become very efficient.

A histogram is really a frequency table of values. By keeping a count
(weight) of how many times we have seen the different values, we have
enough information to calculate the mean, min, max, standard deviation
and percentiles.

Now, from this we can build something really cool:

 * The space required is proportionate to how many different values we
   have seen, not by the total number of observations. Binning values
   requires even less space.
 * Basic aggregation is done very early in the process. Binning also
   helps with this.
 * The frequency tables can easily be merged together, either to
   create an aggregate of multiple nodes to create a cluster view or
   aggregate over time to create for example 5 minute summaries.


## Clusters

In a single node application, you can collect, aggregate and push out
metrics from that single node. In bigger applications it might be
helpful to collect metrics inside of each node, but aggregate together
and view metrics for the whole cluster in one place. Having a "ops
dashboard" showing message queues in key processes, node throughput,
cluster throughput, request latency per node, request latency as a
whole, etc, is extremely useful.

## Setup

Statman has two parts, `statman_server` and `statman_aggregator`. The
server owns the ETS-tables and periodically forwards the changes to
any interested aggregator. The aggregator keeps a moving window of
metrics coming from one ore more servers. You can ask the aggregator
for the stats collected in the last N seconds.

You need to run one server under a supervisor in each node. If you
have a cluster of nodes, you can run the aggregator on just one of
them, collecting stats for the whole cluster.


[statman_elli]: https://github.com/knutin/statman_elli
[newrelic-erlang]: https://github.com/wooga/newrelic-erlang
[statman_graphite]: https://github.com/chrisavl/statman_graphite
[hatman]: https://github.com/chrisavl/hatman
