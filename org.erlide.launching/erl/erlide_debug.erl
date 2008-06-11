%%% ******************************************************************************
%%%  Copyright (c) 2006 Vlad Dumitrescu and others.
%%%  All rights reserved. This program and the accompanying materials
%%%  are made available under the terms of the Eclipse Public License v1.0
%%%  which accompanies this distribution, and is available at
%%%  http://www.eclipse.org/legal/epl-v10.html
%%%
%%%  Contributors:
%%%      Vlad Dumitrescu
%%% ******************************************************************************/
%%

-module(erlide_debug).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start_debug/1, line_breakpoint/2]).
-compile(export_all).

%%
%% API Functions
%%

start_debug(JPid) ->
	group_leader(whereis(init), self()),
	{ok, Pid} = erlide_dbg_mon:start(local, default),
        JPid ! {started, Pid},
	Pid.

processes(ShowSys, ShowErlide) ->
    L = erlang:processes(),
    case ShowSys of
        false ->
            L1 = lists:filter(fun(X)-> not pman_process:is_system_process(X) end, L),
            case ShowErlide of
                false ->
                    lists:filter(fun(X)-> not is_erlide_process(X) end, L1);
                true ->
                    L1
            end;
        true ->
            L
    end.

is_erlide_process(Pid) when pid(Pid)->
    Started = case erlang:process_info(Pid, initial_call) of
                  undefined -> 
                      false;
                  {initial_call, {M1, _, _}} ->
                      lists:prefix("erlide_", atom_to_list(M1))
%%                       string:equal(string:sub_string(atom_to_list(M1), 1, 7), "erlide_")
              end,
              
    Current = case erlang:process_info(Pid, current_function) of
                  undefined -> 
                      false;
                  {current_function, {M2, _, _}} ->
                      lists:prefix("erlide_", atom_to_list(M2))
%%                       string:equal(string:sub_string(atom_to_list(M2), 1, 7), "erlide_")
              end,
    
    Started or Current.

interpret(Files) ->
    erlide_dbg_mon:interpret(Files).

line_breakpoint(File, Line) ->
    io:format("before i\n", []),
    erlide_log:log("before ii\n"),
    interpret([File]),
    io:format("after i\n", []),
    erlide_log:log("after ii\n"),
    erlide_dbg_mon:line_breakpoint(File, Line).

%%
%% Local Functions
%%

foo() ->
	ok.
