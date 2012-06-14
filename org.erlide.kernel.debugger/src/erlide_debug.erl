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
-export([start_debug/1, 
         attached/2, 
         send_started/1]).
-export([line_breakpoint/3, 
         resume/1, 
         suspend/1, 
         bindings/1, 
         step_over/1, 
         step_into/1, 
         step_return/1,
         interpret/3, 
         all_stack_frames/1,
	 all_modules_on_stack/1,
	 tracing/2,
         eval/2, 
         set_variable_value/4,
         distribute_debugger_code/1,
	 nodes/0,
	 process_info/2,
         processes/2,
	 drop_to_frame/2,
         is_erlide_process/1]).

-export([log/1]).

%%
%% API Functions
%%

%% copied from IErlDebugConstants
%% final int DISTRIBUTED_DEBUG_FLAG = 1;
%% final int ATTACH_ON_FIRST_CALL_FLAG = 2;
%% final int ATTACH_ON_BREAKPOINT_FLAG = 4;
%% final int ATTACH_ON_EXIT_FLAG = 8;

fix_flags(N) ->
    fix_flag(N, 2, init) ++ fix_flag(N, 4, break) ++ fix_flag(N, 8, exit).

fix_flag(N, F, A) when N band F =/= 0 -> [A];
fix_flag(_, _, _) -> [].

local_global(N) when N band 1 =/= 0 ->
    global;
local_global(_) ->
    local.

start_debug(Flags) ->
    group_leader(whereis(init), self()),
    {ok, Pid} = erlide_dbg_mon:start(local_global(Flags), fix_flags(Flags)),
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

is_erlide_process(Pid) -> % when is_pid(Pid)->
    Started = case (catch erlang:process_info(Pid, initial_call)) of
                  {initial_call, {M1, _, _}} ->
                      lists:prefix("erlide_", atom_to_list(M1));
                  _ -> 
                      false
              end,
    Current = case (catch erlang:process_info(Pid, current_function)) of
                  {current_function, {M2, _, _}} ->
                      lists:prefix("erlide_", atom_to_list(M2));
                  _ ->
                      false
              end,
    Started or Current.

get_dist(true) -> distributed;
get_dist(false) -> local.

interpret(File, Dist, Interpret) ->
    erlide_dbg_mon:interpret(File, get_dist(Dist), Interpret).

line_breakpoint(File, Line, Action) ->
    ModuleName = filename:rootname(filename:basename(File)),
    Module = list_to_atom(ModuleName),
    Res = erlide_dbg_mon:line_breakpoint(Module, Line, Action),
    Res.

suspend(MetaPid) ->
    erlide_dbg_mon:suspend(MetaPid).

resume(MetaPid) ->
    erlide_log:logp("dbg resume >>>> ~p", [MetaPid]),
    erlide_dbg_mon:resume(MetaPid).

bindings(MetaPid) ->
    erlide_dbg_mon:bindings(MetaPid).

eval(Expr, MetaPid) ->
    erlide_dbg_mon:eval(Expr, MetaPid).

all_stack_frames(MetaPid) ->
    erlide_dbg_mon:all_stack_frames(MetaPid).

all_modules_on_stack(MetaPid) ->
    erlide_dbg_mon:all_modules_on_stack(MetaPid).

tracing(Bool, MetaPid) ->
    erlide_dbg_mon:tracing(Bool, MetaPid).

step_over(MetaPid) ->
    erlide_dbg_mon:step_over(MetaPid).

step_into(MetaPid) ->
    erlide_dbg_mon:step_into(MetaPid).

step_return(MetaPid) ->
    erlide_dbg_mon:step_return(MetaPid).

drop_to_frame(MetaPid, FrameNum) ->
    erlide_dbg_mon:drop_to_frame(MetaPid, FrameNum).

set_variable_value(Variable, Value, SP, MetaPid) ->
    erlide_dbg_mon:set_variable_value(Variable, Value, SP, MetaPid).

distribute_debugger_code(Modules) ->
    [rpc:multicall(code, load_binary, [Module, Filename, Binary])
       || {Module, Filename, Binary} <- Modules].

nodes() ->
    [node() | erlang:nodes()].

%%     DebuggerModules = [erlide_dbg_debugged, erlide_dbg_icmd, erlide_dbg_idb,
%%                        erlide_dbg_ieval, erlide_dbg_iload, erlide_dbg_iserver,
%%                        erlide_int, int],
%%     lists:foreach(fun(Module) ->
%%                           {_Module, Binary, Filename} = 
%%                               code:get_object_code(Module),
%%                           rpc:multicall(nodes(), code, load_binary, 
%%                                         [Module, Filename, Binary])
%%                   end,
%%                   DebuggerModules).

%% log(E) ->
%%     case file:open("/Users/jakob/Desktop/log.txt", [append]) of
%%         {ok, F} ->
%%             io:format(F, "~p\n", [E]),
%%             file:close(F);
%%         _ ->
%%             ok
%%     end.
log(_) ->
    ok.

process_info(Pid, Info) ->
    Node = node(Pid),
    case node() of
	Node ->
	    erlang:process_info(Pid, Info);
	_ ->
	    rpc:call(Node, erlang, process_info, [Pid, Info], 5000)
    end.

%%
%% Local Functions
%%
