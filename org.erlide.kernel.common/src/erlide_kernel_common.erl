-module(erlide_kernel_common).

-export([
		 init/3
		]).

init(JRex, Monitor, Watch) ->	
	spawn(fun()->
				  case Monitor of
					  true ->
						  %% must be first so that only system processes are ignored
						  
						  Mon = spawn(fun monitor/0),
						  erlide_monitor:start(),
						  erlide_monitor:subscribe(Mon),
						  ok;
					  _ ->
						  ok
				  end,
				  erlide_jrpc:init(JRex),
				  watch_eclipse(node(JRex), Watch),
				  
				  erlide_batch:start(erlide_builder),
				  
				  ok
		  end),
	ok.

%% it's uncertain if this is needed anymore, but it doesn't hurt
watch_eclipse(JavaNode, Watch) ->
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
