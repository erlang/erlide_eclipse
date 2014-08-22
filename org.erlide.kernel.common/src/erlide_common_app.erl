-module(erlide_common_app).

-export([
         init/5
        ]).

init(JRex, Kill, HeapWarnLimit, HeapKillLimit, MaxParallelBuilds) ->
    spawn(fun () ->
                   startup(JRex, Kill, HeapWarnLimit, HeapKillLimit, MaxParallelBuilds)
          end).

startup(JRex, Kill, HeapWarnLimit, HeapKillLimit, MaxParallelBuilds)->
    erlide_jrpc:init(JRex),
    watch_eclipse(node(JRex), Kill),

    erlide_monitor:start(HeapWarnLimit, HeapKillLimit),
    erlang:system_monitor(erlang:whereis(erlide_monitor),
                          [{long_gc, 3000}, {large_heap, HeapWarnLimit*1000000 div 2}]),

    erlide_batch:start(erlide_builder, MaxParallelBuilds),
    ok.

watch_eclipse(JavaNode, Kill) ->
    spawn(fun() ->
                  monitor_node(JavaNode, true),
                  erlide_log:log({"Monitoring java node", JavaNode}),
                  write_message({"start monitoring", JavaNode, Kill}),
                  wait_nodedown(JavaNode, Kill)
          end).

shutdown() ->
    write_message("SHUTTING DOWN"),
    erlide_monitor:stop(),
    L = [V  || V = "erlide_" ++ _  <- [atom_to_list(X) || X <- registered()]],
    [exit(whereis(list_to_atom(X)), kill) || X <- L],
    write_message("FINISHED"),
    ok.

write_message(Msg) ->
    {ok, [[Home]]} = init:get_argument(home),
    {ok, F} = file:open(Home++"/erlide_debug.txt", [append, raw]),
    file:write(F, io_lib:format("~p: ~p got ~p~n", [erlang:universaltime(), node(), Msg])),
    file:sync(F),
    file:close(F),
    ok.

wait_nodedown(JavaNode, Kill) ->
    receive
        {nodedown, JavaNode}=_Msg ->
            write_message(_Msg),
            case Kill of
                true ->
                    erlang:halt(abort, [{flush, false}]),
                    ok;
                false ->
                    shutdown(),
                    ok
            end,
            ok
        after 5000 ->
            wait_nodedown(JavaNode, Kill)
    end.
