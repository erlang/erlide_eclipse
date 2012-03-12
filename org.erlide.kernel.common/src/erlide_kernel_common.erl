-module(erlide_kernel_common).

-export([
     init/3,
     set_monitoring/1,
     set_monitoring_interval/1
    ]).

init(JRex, Monitor, Watch) ->
  spawn(fun () ->
                 startup(JRex, Monitor, Watch)
        end).

startup(JRex, Monitor, Watch)->
    %% must be first so that only system processes are ignored
    erlide_monitor:start(),
    set_monitoring(Monitor), 

    erlide_jrpc:init(JRex),
    watch_eclipse(node(JRex), Watch),
    
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

monitor() ->
  receive
    {erlide_monitor, _Node, _Diff}=Msg ->
      erlide_log:logp("~p.", [Msg]),
      monitor();
    _ ->
      monitor()
  end.

shutdown() ->
  erlide_monitor:stop(),
  L = [V  || V = "erlide_" ++ _  <- [atom_to_list(X) || X <- registered()]],
  [exit(whereis(list_to_atom(X)), kill) || X <- L],
  ok.

set_monitoring(true) ->
    erlide_log:log("start ide monitoring"),
    Mon = spawn(fun monitor/0),
    erlide_monitor:subscribe(Mon);
set_monitoring(false) ->
    erlide_log:log("stop ide monitoring"),
    ok.

set_monitoring_interval(N) ->
    erlide_monitor:configure(poll_interval, N*1000),
    erlide_monitor ! take_snapshot.

