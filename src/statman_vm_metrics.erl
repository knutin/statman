%% @doc: Collection of functions for sending statistics from the

-module(statman_vm_metrics).
-compile([export_all]).


get_counters() ->
    {{input, InputBytes}, {output, OutputBytes}} = erlang:statistics(io),
    [{{vm, io_in_bytes}, InputBytes}, {{vm, io_out_bytes}, OutputBytes}].


get_gauges() ->
    Memory = lists:map(fun ({K, V}) ->
                               {{vm_memory, K}, V}
                       end, erlang:memory()),

    RunQueue = {{vm, run_queue}, erlang:statistics(run_queue)},
    ProcessCount = {{vm, process_count}, erlang:system_info(process_count)},

    [RunQueue, ProcessCount] ++ Memory ++ message_stats() ++ ets_stats().


message_stats() ->
    ProcessInfo = lists:flatmap(
                    fun (Pid) ->
                            case process_info(Pid, message_queue_len) of
                                undefined ->
                                    [];
                                {message_queue_len, 0} ->
                                    [];
                                {message_queue_len, Count} ->
                                    [{Count, Pid}]
                            end
                    end, processes()),
    TotalQueue = lists:sum(element(1, lists:unzip(ProcessInfo))),

    [{{vm, processes_with_queues}, length(ProcessInfo)},
     {{vm, messages_in_queue}, TotalQueue}].

ets_stats() ->
    TotalSize = lists:sum(
                  lists:map(fun (T) ->
                                    case ets:info(T, size) of
                                        N when is_integer(N) ->
                                            N;
                                        undefined ->
                                            0
                                    end
                            end, ets:all())),
    [{{vm_ets, objects}, TotalSize}].


gc(undefined) ->
    {NumGCs, _, _} = erlang:statistics(garbage_collection),
    {NumGCs, []};
gc(PrevNumGCs) ->
    {NumGCs, _, 0} = erlang:statistics(garbage_collection),
    {NumGCs, [{{vm, gcs}, NumGCs - PrevNumGCs}]}.
