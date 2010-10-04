%%% ******************************************************************************
%%%  Copyright (c) 2004 Vlad Dumitrescu and others.
%%%  All rights reserved. This program and the accompanying materials
%%%  are made available under the terms of the Eclipse Public License v1.0
%%%  which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
%%%
%%%  Contributors:
%%%      Vlad Dumitrescu
%%% ******************************************************************************/
%%% File    : erlide_backend.erl
%%% Author  :  Vlad Dumitrescu
%%% Description :
%%% Created : 12 Aug 2004 by  Vlad Dumitrescu
%%% Version: $Revision: 1.2 $

-module(erlide_proclist).

-export([
		 init/1,
		 
		 process_list/0,
		 process_list_init/0,
		 get_process_info/1
		]).

init(_EventSinkPid) ->
	process_list_init(),
	ok.

%% taken from distel, for testing
%% original author: Luke Gorrie

process_list() ->
	[info(Pid) || Pid <- processes()].

info(Pid) ->
    {Pid, 
     name(Pid), 
     initial_call(Pid), 
     reductions(Pid), 
     messages(Pid)
    }.

name(Pid) ->
	case process_info(Pid, registered_name) of
		{registered_name, Regname} ->
			Regname;
		_ ->
			lists:flatten(io_lib:format("~p", [Pid]))
	end.

initial_call(Pid) ->
    case process_info(Pid, initial_call) of
        {initial_call, {M, F, A}} ->
            lists:flatten(io_lib:format("~s:~s/~p", [M, F, A]));
        Other ->
            lists:flatten(io_lib:format("~p", [Other]))
    end.

reductions(Pid) ->
	{reductions, NrReds} = process_info(Pid, reductions),
	NrReds.

messages(Pid) ->
	{message_queue_len, Len} = process_info(Pid, message_queue_len),
	Len.

get_process_info(Pid) ->
	case (catch erlang:process_info(Pid)) of
		{'EXIT', Reason} ->
			{error, Reason};
		Result ->
			Result
	end.

process_list_init() ->
	case get(process_list_init) of
		true ->
			ok;
		_ ->
			put(process_list_init, true),
			spawn(fun() ->
						  process_list_updater() 
				  end)
	end.

process_list_updater() ->
	receive
		stop -> ok;
		_ -> process_list_updater()
		after 5000 ->
			erlide_jrpc:event(processlist, {erlang:now(), self()}),
			process_list_updater()
	end.


%% end distel

