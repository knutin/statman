-module(statman_benchmark).
-compile([export_all]).

-define(PARETO_SHAPE, 1.5).
-define(MAX_VALUE, 1000).


histogram_run(Writes) ->
    {InsertTime, _} = timer:tc(?MODULE, do_histogram_run, [Writes]),
    {StatTime, Stats} = timer:tc(statman_histogram, summary, [foo]),
    [{insert_time, InsertTime}, {start_time, StatTime}, {stats, Stats}].

do_histogram_run(0) ->
    ok;
do_histogram_run(Writes) ->
    Value = pareto(trunc(?MAX_VALUE * 0.2), ?PARETO_SHAPE),
    statman_histogram:record_value(foo, Value),
    do_histogram_run(Writes - 1).

pareto(Mean, Shape) ->
    S1 = (-1 / Shape),
    S2 = Mean * (Shape - 1),
    U = 1 - random:uniform(),
    trunc((math:pow(U, S1) - 1) * S2).
