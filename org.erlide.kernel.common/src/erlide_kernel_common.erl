-module(erlide_kernel_common).

-export([
     init/4
    ]).

init(JRex, Watch, HeapWarnLimit, HeapKillLimit) ->
  spawn(fun () ->
                 startup(JRex, Watch, HeapWarnLimit, HeapKillLimit)
        end).

startup(JRex, Watch, HeapWarnLimit, HeapKillLimit)->
    erlide_jrpc:init(JRex),
    watch_eclipse(node(JRex), Watch),

	erlide_monitor:start(HeapWarnLimit, HeapKillLimit),
    erlang:system_monitor(erlang:whereis(erlide_monitor), 
						  [{long_gc, 3000}, {large_heap, HeapWarnLimit*1000000 div 2}]),

    erlide_batch:start(erlide_builder),
    ok.

watch_eclipse(JavaNode, Watch) ->
  spawn(fun() ->
          monitor_node(JavaNode, true),
          receive
            {nodedown, JavaNode} ->
              case Watch of
                true ->
                  init:stop();
                false ->
                  shutdown()
              end,
              ok
          end
      end).

shutdown() ->
  erlide_monitor:stop(),
  L = [V  || V = "erlide_" ++ _  <- [atom_to_list(X) || X <- registered()]],
  [exit(whereis(list_to_atom(X)), kill) || X <- L],
  ok.

