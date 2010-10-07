%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1,(the "License"); you may not use this file except in
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
-module(erlide_dbg_mon).

-include_lib("kernel/include/file.hrl").
-include("erlide.hrl").

%% External exports
-export([start/2, stop/0, interpret/3, line_breakpoint/3]).
-export([resume/1, suspend/1, bindings/1, all_stack_frames/1, step_over/1]).
-export([step_into/1, step_return/1, eval/2, set_variable_value/4]).
-export([tracing/2, drop_to_frame/2, all_modules_on_stack/1]).

-define(BACKTRACE, all).

-record(pinfo, {pid,       % pid()
                status     % break | exit | idle | running | waiting
                }).

%% Internal exports
-export([send_attached_to_java/2]).

%% TODO vi behver inget state! detta r mest till fr otp-debuggern,
%% s den sparar sig mellan varven...

-record(state, {parent, %pid() remote
                mode,      % local | global
                starter,   % bool() 'true' if int was started by me

                focus, % #pinfo()
                pinfos,    % [#pinfo{}] Debugged processes

                backtrace, % integer() Number of call frames to fetch

                attach     % false | {Flags, Function}
               }).

-define(SERVER, ?MODULE).

%%====================================================================
%% External exports
%%====================================================================

%%--------------------------------------------------------------------
%% start(Mode, SFile) -> {ok, Pid} | {error, Reason}
%%   Mode = local | global
%%   SFile = string() | default  Settings file
%%   Pid = pid()
%%   Reason = {already_started,Pid} | term()
%%--------------------------------------------------------------------
start(Mode, Flags) ->
    case whereis(?SERVER) of
	undefined ->
	    CallingPid = self(),
	    Pid = spawn(fun () ->
				 ?SAVE_CALLS,
				 init(CallingPid, Mode, Flags)
			end),
	    receive
		{initialization_complete, Pid} ->
		    {ok, Pid};
		Error ->
		    Error
	    end;
	Pid ->
	    {error, {already_started,Pid}}
    end.

%%--------------------------------------------------------------------
%% stop() -> ok
%%--------------------------------------------------------------------
stop() ->
    case whereis(?SERVER) of
        undefined ->
            ok;
        Pid ->
            Flag = process_flag(trap_exit, true),
            link(Pid),
            Pid ! stop,
            receive
                {'EXIT', Pid, stop} ->
                    process_flag(trap_exit, Flag),
                    ok
            end
    end.


%%====================================================================
%% Initialization
%%====================================================================

init(CallingPid, Mode, Flags) ->
    register(?SERVER, self()),
    %% Start Int if necessary and subscribe to information from it
    Bool = case erlide_int:start() of
               {ok, _Int} -> true;
               {error, {already_started, _Int}} -> false
           end,
    erlide_int:subscribe(),

    %% Initial process state
    State1 = #state{mode    = Mode,
                    starter = Bool,
                    pinfos  = []
                   },

    State2 = State1#state{attach = Flags},


    State3 = init_contents(erlide_int:interpreted(),   % Modules
                           erlide_int:all_breaks(),    % Breakpoints
                           erlide_int:snapshot(),      % Processes
                           State2),

    CallingPid ! {initialization_complete, self()},
    loop(State3).

init_contents(_Mods, _Breaks, Processes, State) ->
    lists:foldl(fun(PidTuple, State0) ->
                        int_cmd({new_process, PidTuple}, State0)
                end,
                State,
                Processes).


%%====================================================================
%% Main loop and message handling
%%====================================================================

loop(State) ->
    receive
        {parent, P} -> %% P is the remote mailbox
            log({parent, P}),
            erlide_int:auto_attach(State#state.attach, {?MODULE, send_attached_to_java, [P]}),
            loop(State#state{parent=P});

%%         dumpState ->
%%             io:format("dbg_mon state:: ~p~n", [State]),
%%             msg(State#state.parent, {dumpState, State, erlide_int:snapshot()}),
%%             loop(State);

        {cmd, Cmd, From} = _Msg ->
            %% 	    io:format("@ dbg_mon cmd: ~p~n", [_Msg]),
            log({gui_cmd, Cmd, From}),
            {Reply, State1} = gui_cmd(Cmd, State),
            From ! Reply,
            loop(State1);

        stop ->
            gui_cmd(stopped, State);

        %% From the interpreter process
        {int, Cmd} = Msg ->
            log({int_cmd, Cmd}),
            msg(State#state.parent, Msg),

            State2 = int_cmd(Cmd, State),
            loop(State2);

        Msg ->
            %%msg(State#state.parent, {unknown, Msg}),
            io:format("dbg_mon: unknown ~p", [Msg]),
            loop(State)

    end.

%%--Commands from the GUI---------------------------------------------
%% Act upon a command from the GUI. In most cases, it is only necessary
%% to call a relevant int-function. int will then report when the action
%% has been taken.

gui_cmd(ignore, State) ->
    {ok, State};
gui_cmd(stopped, State) ->
    if
	State#state.starter==true -> erlide_int:stop();
	true -> erlide_int:auto_attach(false)
    end,
    exit(stop);

gui_cmd(refresh, State) ->
    erlide_int:clear(),
    State2 = State#state{pinfos=[]},
    lists:foldl(fun(PidTuple, S) ->
			int_cmd({new_process,PidTuple}, S)
		end,
		State2,
		erlide_int:snapshot());

gui_cmd({attach, Jproc, Dproc}, State) ->
	erlide_int:attach(Dproc, {erlide_dbg, start, [Jproc, Dproc]}),
	State;

gui_cmd(kill_all_processes, State) ->
    lists:foreach(fun(PInfo) ->
			  case PInfo#pinfo.status of
			      exit -> ignore;
			      _Status -> exit(PInfo#pinfo.pid, kill)
			  end
		  end,
		  State#state.pinfos),
    State;

gui_cmd({interpret, {AbsBeam, Dist, true}}, State) ->
    Res = erlide_int:interpret_beam(AbsBeam, Dist),
    {Res, State};
gui_cmd({interpret, {AbsBeam, Dist, false}}, State) ->
    Res = erlide_int:n2(AbsBeam, Dist),
    {Res, State};
gui_cmd(delete_all, State) ->
    lists:foreach(fun(Mod) -> erlide_int:nn(Mod) end, erlide_int:interpreted()),
    {ok, State};
gui_cmd({module, Mod, What}, State) ->
    case What of
	delete -> erlide_int:nn(Mod)
    end,
    State;

gui_cmd(enable_all_breaks, State) ->
    Breaks = erlide_int:all_breaks(),
    lists:foreach(fun ({{Mod, Line}, _Options}) ->
			  erlide_int:enable_break(Mod, Line)
		  end,
		  Breaks),
    State;
gui_cmd(disable_all_breaks, State) ->
    Breaks = erlide_int:all_breaks(),
    lists:foreach(fun({{Mod, Line}, _Options}) ->
                          erlide_int:disable_break(Mod, Line)
                  end,
                  Breaks),
    State;

gui_cmd(delete_all_breaks, State) ->
    erlide_int:no_break(),
    State;
gui_cmd({break, {Mod, Line, What}}, State) ->
%%     io:format("break Mod Line What ~p\n", [{Mod, Line, What}]),
    Res = case What of
              add -> erlide_int:break(Mod, Line);
              delete -> erlide_int:delete_break(Mod, Line);
              {status, inactive} -> erlide_int:disable_break(Mod, Line);
              {status, active} -> erlide_int:enable_break(Mod, Line);
              {trigger, Action} -> erlide_int:action_at_break(Mod, Line, Action)
          end,
    {Res, State};

gui_cmd({resume, MetaPid}, State) ->
    Res = erlide_dbg_icmd:continue(MetaPid),
    {Res, State};
gui_cmd({suspend, MetaPid}, State) ->
    Res = erlide_dbg_icmd:stop(MetaPid),
    {Res, State};
gui_cmd({set_trace, {Bool, MetaPid}}, State) ->
    Res = erlide_dbg_icmd:set(MetaPid, trace, Bool),
    {Res, State};
gui_cmd({bindings, MetaPid}, State) ->
    Res = erlide_dbg_icmd:get(MetaPid, bindings, nostack),
    {Res, State};
gui_cmd({all_stack_frames, MetaPid}, State) ->
    Res = erlide_dbg_icmd:get(MetaPid, all_stack_frames, noargs),
    {Res, State};
gui_cmd({all_modules_on_stack, MetaPid}, State) ->
    Res = erlide_dbg_icmd:get(MetaPid, all_modules_on_stack, noargs),
    {Res, State};
gui_cmd({step_into, MetaPid}, State) ->
    Res = erlide_dbg_icmd:step(MetaPid),
    {Res, State};
gui_cmd({step_over, MetaPid}, State) ->
    Res = erlide_dbg_icmd:next(MetaPid),
    {Res, State};
gui_cmd({step_return, MetaPid}, State) ->
    Res = erlide_dbg_icmd:finish(MetaPid),
    {Res, State};
gui_cmd({set_variable_value, {Variable, Value, SP, MetaPid}}, State) ->
    log({?MODULE, ?LINE, Variable, Value}),
    Res = erlide_dbg_icmd:set_variable_value(MetaPid, Variable, Value, SP),
    log({?MODULE, ?LINE, Res}),
    {Res, State};
gui_cmd({eval, {Expr, MetaPid}}, State) ->
    Res = erlide_dbg_icmd:eval(MetaPid, {dummy_mod, Expr}),
    {Res, State};
gui_cmd({drop_to_frame, {MetaPid, StackFrameNum}}, State) ->
    Res = erlide_dbg_icmd:drop_to_frame(MetaPid, StackFrameNum),
    {Res, State};

%% Options Commands
gui_cmd({trace, JPid}, State) ->
    case State#state.attach of
	false -> ignore;
	{Flags, {erlide_dbg, start, [JPid, StartFlags]}} ->
	    case trace_function(JPid, State) of
		{_, _, StartFlags} -> ignore;
		NewFunction -> % {_, _, NewStartFlags}
		    erlide_int:auto_attach(Flags, NewFunction)
	    end;
	_AutoAttach -> ignore
    end,
    State;
gui_cmd({auto_attach, Flags}, State) ->
    erlide_int:auto_attach(Flags, {'_', '_', []}),
    State;
gui_cmd({stack_trace, [_Name]}, State) ->
%    erlide_int:stack_trace(map(Name)),
    State;
gui_cmd(backtrace_size, State) ->
    State;

gui_cmd({focus, Pid, _Win}, State) ->
    {value, PInfo} =
	lists:keysearch(Pid, #pinfo.pid, State#state.pinfos),
    State#state{focus=PInfo};
gui_cmd(default, State) ->
	State;

gui_cmd(Cmd, State) ->
	io:format("@ dbg_mon: unknown ~p~n",[Cmd]),
	State.

%%--Commands from the interpreter-------------------------------------

int_cmd({interpret, _Mod}, State) ->
    State;
int_cmd({no_interpret, _Mod}, State) ->
    State;

int_cmd({new_process, {Pid, _Function, Status, _Info}}, State) ->

    %% Create record with information about the process
    _Name = registered_name(Pid),
    PInfo = #pinfo{pid=Pid, status=Status},

    %% Store process information
    PInfos = [PInfo | State#state.pinfos],
    State#state{pinfos=PInfos};
int_cmd({new_status, Pid, Status, _Info}, State) ->

    %% Find stored information about the process
    PInfos = State#state.pinfos,
    {value, PInfo} = lists:keysearch(Pid, #pinfo.pid, PInfos),

    %% Update process information
    PInfo2 = PInfo#pinfo{status=Status},
    PInfos2 = lists:keyreplace(Pid, #pinfo.pid, PInfos, PInfo2),
    State2 = State#state{pinfos=PInfos2},

    case State2#state.focus of
	#pinfo{pid=Pid} ->
	    State2#state{focus=PInfo2};
	_ ->
	    State2
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
    State;

int_cmd({auto_attach, AutoAttach}, State) ->
    State#state{attach=AutoAttach};
int_cmd({stack_trace, _Flag}, State) ->
    State;

int_cmd(_Other, State) ->
	State.


%%====================================================================
%% Debugger API
%%====================================================================
interpret(Module, Dist, Interpret) ->
    cmd(interpret, {Module, Dist, Interpret}).

suspend(MetaPid) ->
    cmd(suspend, MetaPid).

resume(MetaPid) ->
    cmd(resume, MetaPid).

step_over(MetaPid) ->
    cmd(step_over, MetaPid).

drop_to_frame(MetaPid, StackFrameNum) ->
    cmd(drop_to_frame, {MetaPid, StackFrameNum}).

step_into(MetaPid) ->
    cmd(step_into, MetaPid).

step_return(MetaPid) ->
    cmd(step_return, MetaPid).

eval(Expr, MetaPid) ->
    cmd(eval, {Expr, MetaPid}).

set_variable_value(Variable, Value, SP, MetaPid) ->
    cmd(set_variable_value, {Variable, Value, SP, MetaPid}).

bindings(MetaPid) ->
    cmd(bindings, MetaPid).

tracing(Bool, MetaPid) ->
    cmd(set_trace, {Bool, MetaPid}).

all_stack_frames(MetaPid) ->
    cmd(all_stack_frames, MetaPid).

all_modules_on_stack(MetaPid) ->
    cmd(all_modules_on_stack, MetaPid).

line_breakpoint(Module, Line, Action) ->
    cmd(break, {Module, Line, Action}).

cmd(Cmd, Args) ->
    cmd({Cmd, Args}).

cmd(Cmd) ->
    ?SERVER ! {cmd, Cmd, self()},
    receive
        Reply ->
            Reply
    end.

%%====================================================================
%% Debugger settings
%%====================================================================

%% load_settings(SFile, State) ->
%%     case file:read_file(SFile) of
%% 	{ok, Binary} ->
%% 	    case catch binary_to_term(Binary) of
%% 		{debugger_settings, Settings} ->
%% 		    load_settings2(Settings,
%% 				   State#state{sfile=SFile,
%% 					       changed=false});
%% 		_Error -> State
%% 	    end;
%% 	{error, _Reason} -> State
%%     end.

%% load_settings(Settings, State) ->
%%     {AutoAttach, StackTrace, BackTrace, Files, Breaks} =
%% 	Settings,
%%
%%     case AutoAttach of
%% 	false -> erlide_int:auto_attach(false);
%% 	{Flags, Function} -> erlide_int:auto_attach(Flags, Function)
%%     end,
%%
%%     erlide_int:stack_trace(StackTrace),
%%
%%     case State#state.mode of
%% 	local -> lists:foreach(fun(File) -> erlide_int:i(File) end, Files);
%% 	global -> lists:foreach(fun(File) -> erlide_int:ni(File) end, Files)
%%     end,
%%     lists:foreach(fun(Break) ->
%% 			  {{Mod, Line}, [Status, Action, _, Cond]} =
%% 			      Break,
%% 			  erlide_int:break(Mod, Line),
%% 			  if
%% 			      Status==inactive ->
%% 				  erlide_int:disable_break(Mod, Line);
%% 			      true -> ignore
%% 			  end,
%% 			  if
%% 			      Action/=enable ->
%% 				  erlide_int:action_at_break(Mod,Line,Action);
%% 			      true -> ignore
%% 			  end,
%% 			  case Cond of
%% 			      CFunction when tuple(CFunction) ->
%% 				  erlide_int:test_at_break(Mod,Line,CFunction);
%% 			      null -> ignore
%% 			  end
%% 		  end,
%% 		  Breaks),
%%
%%     State#state{backtrace=BackTrace}.

%% save_settings(SFile, State) ->
%%     Settings = {erlide_int:auto_attach(),
%% 		erlide_int:stack_trace(),
%% 		State#state.backtrace,
%% 		lists:map(fun(Mod) ->
%% 				  erlide_int:file(Mod)
%% 			  end,
%% 			  erlide_int:interpreted()),
%% 		erlide_int:all_breaks()},
%%
%%     Binary = term_to_binary({debugger_settings, Settings}),
%%     case file:write_file(SFile, Binary) of
%% 	ok ->
%% 	    State#state{sfile=SFile, changed=false};
%% 	{error, _Reason} ->
%% 	    State
%%     end.


%%====================================================================
%% Other internal functions
%%====================================================================

registered_name(Pid) ->

    %% Yield in order to give Pid more time to register its name
    timer:sleep(200),

    Node = node(Pid),
    if
	Node==node() ->
	    case erlang:process_info(Pid, registered_name) of
		{registered_name, Name} -> Name;
		_ -> undefined
	    end;
	true ->
	    case rpc:call(Node,erlang,process_info,
			  [Pid,registered_name]) of
		{registered_name, Name} -> Name;
		_ -> undefined
	    end
    end.

trace_function(Jpid, State) ->
    {erlide_dbg, start, [Jpid, State#state.backtrace]}.

msg(Pid, Msg) ->
	%% Pid may be 'undefined'
%% 	io:format("SEND:: ~p~n", [Msg]),
    _Res = (catch(Pid ! Msg)),
    ok.

%% log(E) ->
%%     erlide_debug:log(E).

log(_) ->
    ok.

send_attached_to_java(P, Pid) ->
    log({attached, P, Pid}),
    msg(Pid, {attached, P}),
    log(attached_sent).




