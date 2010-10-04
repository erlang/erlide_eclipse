%% Author: piotr.dorobisz
%% Created: Sep 14, 2010
-module(proc_list).

%%
%% Exported Functions
%%
-export([list_all_processes/1]).

list_all_processes(Nodes) ->
	list_all_processes(Nodes, []).

list_all_processes([H|T], Acc) ->
	case rpc:call(H, erlang, processes, []) of
		{badrpc, nodedown} ->
			list_all_processes(T, Acc);
		Processes ->
			List = [{Pid, name(H, Pid), initial_call(H, Pid), H} || Pid <- Processes],
			list_all_processes(T, lists:append(Acc, List))
		end;
list_all_processes([], Acc) ->
	Acc.

name(Node, Pid) ->
	case rpc:call(Node, erlang, process_info, [Pid, registered_name]) of
		{registered_name, Regname} ->
			Regname;
		_ ->
			lists:flatten(io_lib:format("~p", [Pid]))
	end.

initial_call(Node, Pid) ->
	case rpc:call(Node, erlang, process_info, [Pid, initial_call]) of
		{initial_call, {M, F, A}} ->
			lists:flatten(io_lib:format("~s:~s/~p", [M, F, A]));
		_ ->
			"unknown"
	end.
