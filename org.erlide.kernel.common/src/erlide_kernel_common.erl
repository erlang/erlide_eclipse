-module(erlide_kernel_common).

-export([
		 init/2
		]).

init(JRex, Monitor) ->	
	spawn(fun()->
				  case Monitor of
					  true ->
						  %% must be first so that only system processes are ignored
						  Mon = spawn(fun monitor/0),
						  erlide_monitor:start(),
						  erlide_monitor:subscribe(Mon);
					  _ ->
						  ok
				  end,
				  erlide_jrpc:init(JRex),
				  watch_eclipse(node(JRex)),
				  ok
		  end),
	ok.

%% it's uncertain if this is needed anymore, but it doesn't hurt
watch_eclipse(JavaNode) ->
	spawn(fun() ->
				  monitor_node(JavaNode, true),
				  File = "safe_erlide.log",
				  file:delete(File),
				  receive
					  {nodedown, _JavaNode} ->
						  Fmt = "This file can safely be removed! ~n~n"
									++ "~p: eclipse node ~p went down  /~p~n",
						  Msg = io_lib:format(Fmt,
											  [calendar:local_time(),
											   JavaNode, _JavaNode]),
						  file:write_file(File,	Msg),
						  init:stop(),
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
