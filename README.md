# statman - super statistics

`statman` lets you to add a metric ton of instrumentation to your
Erlang code without worrying. You won't overload any processes, use
too much memory or have too expensive aggregations.

`statman` has a nice real-time web interface and can be polled from
Munin / Ganglia (when `statman_aggregator` is done). Graphite, Librato
metrics, New Relic etc can also be integrated.


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
