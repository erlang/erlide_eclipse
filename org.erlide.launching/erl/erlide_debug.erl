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
%% -define(Debug(T), erlide_log:erlangLog(?MODULE, ?LINE, finest, T)).
%% -define(DebugStack(T), erlide_log:erlangLogStack(?MODULE, ?LINE, finest, T)).
%% -define(Info(T), erlide_log:erlangLog(?MODULE, ?LINE, info, T)).

%%
%% Exported Functions
%%
-export([start_debug/0, 
         attached/2, 
         send_started/1]).
-export([line_breakpoint/2, 
         resume/1, 
         suspend/1, 
         bindings/1, 
         step_over/1, 
         step_into/1, 
         step_return/1,
         interpret/1, 
         all_stack_frames/1, 
         eval/2, 
         set_variable_value/4, 
         
         processes/2]).

%% -compile(export_all).

%%
%% API Functions
%%

start_debug() ->
	group_leader(whereis(init), self()),
	{ok, Pid} = erlide_dbg_mon:start(local, default),
	Pid.

send_started(JPid) ->
    JPid ! {started, whereis(erlide_dbg_mon)}.

attached(Pid, JPid) ->
    %% We can't use erlide_int:attached here, because we want 
    %% to use the JPid as meta-cmd receiver
    erlide_dbg_iserver:call({attached, JPid, Pid}).

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

interpret(File) ->
%%     ?Debug({interpret, File}),
    erlide_dbg_mon:interpret([File]).

line_breakpoint(File, Line) ->
    ModuleName = filename:rootname(filename:basename(File)),
    Module = list_to_atom(ModuleName),
    Res = erlide_dbg_mon:line_breakpoint(Module, Line),
    Res.

suspend(MetaPid) ->
    erlide_dbg_mon:suspend(MetaPid).

resume(MetaPid) ->
    erlide_dbg_mon:resume(MetaPid).

bindings(MetaPid) ->
    erlide_dbg_mon:bindings(MetaPid).

eval(Expr, MetaPid) ->
    erlide_dbg_mon:eval(Expr, MetaPid).

all_stack_frames(MetaPid) ->
    erlide_dbg_mon:all_stack_frames(MetaPid).

step_over(MetaPid) ->
    erlide_dbg_mon:step_over(MetaPid).

step_into(MetaPid) ->
    erlide_dbg_mon:step_into(MetaPid).

step_return(MetaPid) ->
    erlide_dbg_mon:step_return(MetaPid).

set_variable_value(Variable, Value, SP, MetaPid) ->
    erlide_dbg_mon:set_variable_value(Variable, Value, SP, MetaPid).

    
%%
%% Local Functions
%%
