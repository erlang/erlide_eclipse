%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%% Initial module name: dbg_ui_trace
%% Modified for use with Erlide by Vlad Dumitrescu
%%
-module(erlide_dbg).

%% External exports
-export([start/2, start/3]).

-define(BACKTRACE, 100).

-record(state, {
		jpid,		% pid() the eclipse process
		pid,           % pid() Debugged process
		meta,          % pid() Meta process
		status,        % {Status,Mod,Line} ¦ {exit,Where,Reason}
		               %   Status = init ¦ idle | break
		               %      | wait_break ¦ wait_running
		               %      ¦ running
                               % Where={Mod,Line} | null

		cm,            % atom() | undefined Current module
		cm_obsolete=false, % boolean() Curr mod needs reloading

		stack,         % {Cur,Max}

		trace,         % boolean()
		stack_trace,   % all | no_tail | false
		backtrace      % integer() #call frames to fetch
	       }).

%%====================================================================
%% External exports
%%====================================================================

%%--------------------------------------------------------------------
%% start(Pid)
%% start(Pid, TraceWin, BackTrace)
%%   Pid = pid()
%%   TraceWin = [WinArea]
%%     WinArea = 'Button|Evaluator|Bindings|Trace Area'
%%   Backtrace = integer()
%%--------------------------------------------------------------------
start(Pid, JPid) -> % Used by debugger:quick/3 (no monitor)
    start(Pid, JPid, ?BACKTRACE).

start(Pid, JPid, BackTrace) ->
    %% Inform int about my existence and get the meta pid back
    log({erlide_dbg_start, {pid, Pid}, {jpid, JPid}}),
    case erlide_int:attached(Pid) of
	{ok, Meta} ->
	    init(Pid, JPid, Meta, BackTrace);
	error ->
	    ignore
    end.


%%====================================================================
%% Main loop and message handling
%%====================================================================

init(Pid, JPid, Meta, _BackTrace) ->

    %% Initial process state
    State1 = #state{jpid = JPid, pid=Pid, meta=Meta,
		    status={idle,null,null},
		    stack={1,1}},

    erlide_int:meta(Meta, trace, State1#state.trace),

    loop(State1).

loop(#state{meta=Meta} = State) ->
erlang:display({"****",Meta}),
    receive

%%   Command = ignore
%%           | stopped
%%           | MenuItem | {Menu, [MenuItem]}
%%               MenuItem = Menu = atom()
%%           | {break, Point, What}
%%               What = add | delete | {status,Status} |{trigger,Trigger}
%%           | {module, Mod, view}
%%           | {user_command, Cmd}
%%           | {edit, {Var, Val}}

	{gui, Cmd} = _Msg ->
%% 		io:format("* msg: ~p~n", [_Msg]),
	    State2 = gui_cmd(Cmd, State),
	    loop(State2);

	%% From the interpreter
	{int, Cmd} = _Msg ->
%% 		io:format("* dbg: ~p~n", [_Msg]),
	    State2 = int_cmd(Cmd, State),
	    loop(State2);

	%% From the meta process
	{Meta, Cmd} = _Msg ->
%% 		io:format("* dbg: ~p~n", [_Msg]),
	    State2 = meta_cmd(Cmd, State),
	    loop(State2);
	{NewMeta, {exit_at, Where, Reason, Cur}} = _Msg->
%% 		io:format("* dbg: ~p~n", [_Msg]),
	    State2 = meta_cmd({exit_at, Where, Reason, Cur},
			      State#state{meta=NewMeta}),
	    loop(State2);

	Msg ->
		io:format("* unknown msg in dbg: ~p~n", [Msg]),
		loop(State)
    end.

%%--Commands from the GUI---------------------------------------------

gui_cmd(ignore, State) ->
    State;
gui_cmd(stopped, State) ->
    State;

gui_cmd({gotoline, _Line}, State) ->
    State;
gui_cmd('Search...', State) ->
    State;

%% Process menu
gui_cmd(step, State) ->
    erlide_int:meta(State#state.meta, step),
    State;
gui_cmd(next, State) ->
    erlide_int:meta(State#state.meta, next),
    State;
gui_cmd(continue, State) ->
    erlide_int:meta(State#state.meta, continue),
    {Status, Mod, Line} = State#state.status,
    if
	Status==wait_break ->
	    State#state{status={wait_running,Mod,Line}};
	true ->
	    State
    end;
gui_cmd(finish, State) ->
    erlide_int:meta(State#state.meta, finish),
    State;
gui_cmd(skip, State) ->
    erlide_int:meta(State#state.meta, skip),
    State;
gui_cmd(timeout, State) ->
    erlide_int:meta(State#state.meta, timeout),
    State;
gui_cmd(stop, State) ->
    erlide_int:meta(State#state.meta, stop),
    {Status, Mod, Line} = State#state.status,
    if
	Status==wait_running ->
	    State#state{status={wait_break,Mod,Line}};
	true ->
	    State
    end;
gui_cmd(where, State) ->
    {_Cur, Max} = State#state.stack,
    Stack = {Max, Max},
    {_Status, Mod, _Line} = State#state.status,
    State#state{cm=Mod, stack=Stack};

gui_cmd(kill, State) ->
    exit(State#state.pid, kill),
    State;
gui_cmd(messages, State) ->
    State;
gui_cmd(backtrace, State) ->
    State;
gui_cmd(up, State) ->
	    State;
gui_cmd(down, State) ->
    State;

%% Break menu
gui_cmd('Line Break...', State) ->
    State;
gui_cmd('Conditional Break...', State) ->
    State;
gui_cmd('Function Break...', State) ->
    State;
gui_cmd('Enable All', State) ->
    State;
gui_cmd('Disable All', State) ->
    State;
gui_cmd(delete_all_breaks, State) ->
    erlide_int:no_break(State#state.cm),
    State;
gui_cmd({break, {Mod, Line}, What}, State) ->
    case What of
	add -> erlide_int:break(Mod, Line);
	delete -> erlide_int:delete_break(Mod, Line);
	{status, inactive} -> erlide_int:disable_break(Mod, Line);
	{status, active} -> erlide_int:enable_break(Mod, Line);
	{trigger, Action} -> erlide_int:action_at_break(Mod, Line, Action)
    end,
    State;

%% Help menu
gui_cmd('Debugger', State) ->
    State;

gui_cmd({user_command, Cmd}, State) ->
    {Status, _Mod, _Line} = State#state.status,
    if
	Status==break;
	Status==wait_break;
	Status==wait_running ->
	    Cm = State#state.cm,
	    Arg = case State#state.stack of
		      {Cur, Max} when Cur<Max -> {Cm, Cmd, Cur};
		      _Stack -> {Cm, Cmd}
		  end,

	    %% Reply will be received as {Meta, {eval_rsp, Res}}
	    erlide_int:meta(State#state.meta, eval, Arg);
	true ->
	    Str = "Commands not allowed",
	    State#state.jpid ! {message, Str}
    end,
    State;

gui_cmd({edit, {_Var, _Val}}, State) ->
    State.

%%--Commands from the interpreter-------------------------------------

int_cmd({interpret, Mod}, State) ->
    if
	Mod==State#state.cm ->
	    State#state{cm_obsolete=true};
	true ->
	    State
    end;
int_cmd({no_interpret, Mod}, State) ->
    if
	Mod==State#state.cm ->
	    State#state{cm_obsolete=true};
	true ->
	    State
    end;

int_cmd({new_break, _Break}, State) ->
    State;
int_cmd({delete_break, _Point}, State) ->
    State;
int_cmd({break_options, _Break}, State) ->
    State;
int_cmd(no_break, State) ->
    State;
int_cmd({no_break, _Mod}, State) ->
    State.

%%--Commands from the meta process------------------------------------

%% Message received when first attached to a living process
%% '_Trace' is a boolean indicating if the process is traced or not --
%% ignore this as we already have ordered tracing or not depending on if
%% the Trace Area is shown or not.
meta_cmd({attached, Mod, Line, _Trace}, State) ->
    State#state{status={init,Mod,Line}, cm=Mod};

%% Message received when returning to interpreted code
meta_cmd({re_entry, erlide_dbg_ieval, eval_fun}, State) ->
    State;
meta_cmd({re_entry, Mod, _Func}, State) ->
    Obs = State#state.cm_obsolete,
    case State#state.cm of
	Mod when Obs==true ->
	    State#state{cm_obsolete=false};
	Mod -> State;
	_Cm ->
	    State#state{cm=Mod}
    end;

%% Message received when attached to a terminated process
meta_cmd({exit_at, null, Reason, Cur}, State) ->
    Stack = {Cur, Cur},
    State#state{status={exit,null,Reason}, stack=Stack};
meta_cmd({exit_at, {Mod,Line}, Reason, Cur}, State) ->
    Stack = {Cur+1, Cur+1},
    State#state{cm=Mod,status={exit,{Mod,Line},Reason},
		stack=Stack};

meta_cmd({break_at, Mod, Line, Cur}, State) ->
    Stack = {Cur,Cur},
    State#state{cm=Mod, status={break,Mod,Line}, stack=Stack};
meta_cmd({func_at, Mod, Line, Cur}, State) ->
    Stack = {Cur,Cur},
    State#state{cm=Mod, status={idle,Mod,Line}, stack=Stack};
meta_cmd({wait_at, Mod, Line, Cur}, #state{status={Status,_,_}}=State)
  when Status/=init, Status/=break ->
    Stack = {Cur,Cur},
    State#state{status={wait_running,Mod,Line}, stack=Stack};
meta_cmd({wait_at, Mod, Line, Cur}, State) ->
    Stack = {Cur,Cur},
    State#state{cm=Mod, status={wait_break,Mod,Line},
		stack=Stack};
meta_cmd({wait_after_at, Mod, Line, Sp}, State) ->
    meta_cmd({wait_at, Mod, Line, Sp}, State);
meta_cmd(running, State) ->
    State#state{status={running,null,null}};

meta_cmd(idle, State) ->
    State#state{status={idle,null,null}, cm=undefined};

%% Message about changed trace option can be ignored, the change must
%% have been ordered by this process. (In theory, the change could have
%% been ordered by another attached process. The Debugger, though,
%% allows max one attached process per debugged process).
meta_cmd({trace, _Bool}, State) ->
    State;

meta_cmd({stack_trace, Flag}, State) ->
    {Status,_,_} = State#state.status,
    if
        Status==break; Status==wait_break ->
            ok;
        true -> ignore
    end,
    State#state{stack_trace=Flag};

meta_cmd({trace_output, Str}, State) ->
%%     dbg_ui_trace_win:trace_output(Str),
    io:format("trace_output ~p\n", [Str]),
    State;

%% Reply on a user command
meta_cmd({eval_rsp, Res}, State) ->
    Str = io_lib:print(Res),
    State#state.jpid! {message, Str},
    State.


log(_) ->
    ok.
%% erlide_debug:log(E).
