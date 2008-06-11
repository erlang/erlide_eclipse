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


-define(Debug(T), erlide_log:erlangLog(?MODULE, ?LINE, finest, T)).
-define(DebugStack(T), erlide_log:erlangLogStack(?MODULE, ?LINE, finest, T)).
-define(Info(T), erlide_log:erlangLog(?MODULE, ?LINE, info, T)).

%% External exports
-export([start/2, stop/0, interpret/1, line_breakpoint/2]).

-define(BACKTRACE, 100).

-record(pinfo, {pid,       % pid()
                status     % break | exit | idle | running | waiting
                }).

-record(state, {parent, %pid() remote
                mode,      % local | global
                starter,   % bool() 'true' if int was started by me
                
                focus, % #pinfo()
                intdir,    % string() Default dir
                pinfos,    % [#pinfo{}] Debugged processes
                
                backtrace, % integer() Number of call frames to fetch
                
                attach,    % false | {Flags, Function}
                
                sfile,     % default | string() Settings file
                changed,   % boolean() Settings have been changed
                
                interpreted% list of interpreted filenames
               }). 

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
start(Mode, SFile) ->
    case whereis(?MODULE) of
        undefined ->
            CallingPid = self(),
            Pid = spawn(fun () -> init(CallingPid, Mode, SFile) end),
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
    case whereis(?MODULE) of
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

init(CallingPid, Mode, SFile) ->
    register(?MODULE, self()),
    init2(CallingPid, Mode, SFile).

init2(CallingPid, Mode, SFile) ->
    %% Start Int if necessary and subscribe to information from it
    Bool = case int:start() of
               {ok, _Int} -> true;
               {error, {already_started, _Int}} -> false
           end,
    int:subscribe(),

    %% Initial process state
    State1 = #state{mode    = Mode,
                    starter = Bool,
                    
                    intdir  = element(2, file:get_cwd()),
                    pinfos  = [],
                    
                    sfile   = SFile,
                    changed = false
                    },
    
    State2 = init_options(int:auto_attach(),    % Auto Attach
                          int:stack_trace(),    % Stack Trace
                          ?BACKTRACE,           % Back Trace Size
                          State1),
    
    State3 = init_contents(int:interpreted(),   % Modules
                           int:all_breaks(),    % Breakpoints
                           int:snapshot(),      % Processes
                           State2),
    
    CallingPid ! {initialization_complete, self()},
    
    if SFile==default ->
           loop(State3);
       true ->
           loop(load_settings(SFile, State3))
    end.

init_options(AutoAttach, _StackTrace, BackTrace, State) ->
    case AutoAttach of
        false -> ignore;
        {Flags, _Function} ->
            lists:foreach(fun(_Flag) ->
                                  %			  dbg_ui_mon_win:select(map(Flag), true)
                                  ok
                          end,
                          Flags)
    end,
    State#state{backtrace=BackTrace}.

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
       	    loop(State#state{parent=P});

        dumpState ->
        	io:format("dbg_mon state:: ~p~n", [State]),
        	msg(State#state.parent, {dumpState, State, int:snapshot()}),
       	    loop(State);

        {cmd, From, Args} = Msg ->
	    io:format("@ dbg_mon cmd: ~p~n", [Msg]),
	    {Reply, State1} = gui_cmd(Args, State),
            From ! Reply,
       	    loop(State1);

	stop ->
	    gui_cmd(stopped, State);

	%% From the interpreter process
	{int, Cmd} = Msg ->
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
	State#state.starter==true -> int:stop();
	true -> int:auto_attach(false)
    end,
    exit(stop);

gui_cmd(refresh, State) ->
    int:clear(),
    State2 = State#state{pinfos=[]},
    lists:foldl(fun(PidTuple, S) ->
			int_cmd({new_process,PidTuple}, S)
		end,
		State2,
		int:snapshot());

gui_cmd({attach, Jproc, Dproc}, State) ->
	int:attach(Dproc, {erlide_dbg, start, [Jproc, Dproc]}),
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

gui_cmd({interpret, Modules}, State) ->
    Res = int:i(Modules),
%    dbg_ui_interpret:start(State#state.gs, State#state.coords,
%			   State#state.intdir, State#state.mode),
    {Res, State#state{interpreted=State#state.interpreted++Modules}};
gui_cmd(delete_all, State) ->
    lists:foreach(fun(Mod) -> int:nn(Mod) end, int:interpreted()),
    {ok, State};
gui_cmd({module, Mod, What}, State) ->
    case What of
	delete -> int:nn(Mod)
    end,
    State;

gui_cmd(enable_all_breaks, State) ->
    Breaks = int:all_breaks(),
    lists:foreach(fun ({{Mod, Line}, _Options}) ->
			  int:enable_break(Mod, Line)
		  end,
		  Breaks),
    State;
gui_cmd(disable_all_breaks, State) ->
    Breaks = int:all_breaks(),
    lists:foreach(fun({{Mod, Line}, _Options}) ->
                          int:disable_break(Mod, Line)
                  end,
                  Breaks),
    State;
gui_cmd(delete_all_breaks, State) ->
    int:no_break(),
    State;
gui_cmd({break, {Mod, Line, What}}, State) ->
    Res = case What of
              delete -> int:delete_break(Mod, Line);
              {status, inactive} -> int:disable_break(Mod, Line);
              {status, active} -> int:enable_break(Mod, Line);
              {trigger, Action} -> int:action_at_break(Mod, Line, Action)
          end,
    {Res, State};

%% Options Commands
gui_cmd({trace, JPid}, State) ->
    case State#state.attach of
	false -> ignore;
	{Flags, {erlide_dbg, start, [JPid, StartFlags]}} ->
	    case trace_function(JPid, State) of
		{_, _, StartFlags} -> ignore;
		NewFunction -> % {_, _, NewStartFlags}
		    int:auto_attach(Flags, NewFunction)
	    end;
	_AutoAttach -> ignore
    end,
    State;
gui_cmd({auto_attach, _When}, State) ->
    State;
gui_cmd({stack_trace, [_Name]}, State) ->
%    int:stack_trace(map(Name)),
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
interpret(Modules) ->
    cmd(interpret, Modules).

line_breakpoint(Module, Line) ->
    cmd(break, {Module, Line, {status, active}}).

cmd(Cmd, Args) ->
    io:format("cmd ~p ~p\n", [Cmd, Args]),
    ?MODULE ! {cmd, Cmd, self(), Args},
    receive
        Reply ->
            Reply
    end.

%%====================================================================
%% Debugger settings
%%====================================================================

load_settings(SFile, State) ->
    case file:read_file(SFile) of
	{ok, Binary} ->
	    case catch binary_to_term(Binary) of
		{debugger_settings, Settings} ->
		    load_settings2(Settings,
				   State#state{sfile=SFile,
					       changed=false});
		_Error -> State
	    end;
	{error, _Reason} -> State
    end.

load_settings2(Settings, State) ->
    {AutoAttach, StackTrace, BackTrace, Files, Breaks} =
	Settings,

    case AutoAttach of
	false -> int:auto_attach(false);
	{Flags, Function} -> int:auto_attach(Flags, Function)
    end,

    int:stack_trace(StackTrace),

    case State#state.mode of
	local -> lists:foreach(fun(File) -> int:i(File) end, Files);
	global -> lists:foreach(fun(File) -> int:ni(File) end, Files)
    end,
    lists:foreach(fun(Break) ->
			  {{Mod, Line}, [Status, Action, _, Cond]} =
			      Break,
			  int:break(Mod, Line),
			  if
			      Status==inactive ->
				  int:disable_break(Mod, Line);
			      true -> ignore
			  end,
			  if
			      Action/=enable ->
				  int:action_at_break(Mod,Line,Action);
			      true -> ignore
			  end,
			  case Cond of
			      CFunction when tuple(CFunction) ->
				  int:test_at_break(Mod,Line,CFunction);
			      null -> ignore
			  end
		  end,
		  Breaks),

    State#state{backtrace=BackTrace}.

% TODO: Use of save settings
%% save_settings(SFile, State) ->
%%     Settings = {int:auto_attach(),
%% 		int:stack_trace(),
%% 		State#state.backtrace,
%% 		lists:map(fun(Mod) ->
%% 				  int:file(Mod)
%% 			  end,
%% 			  int:interpreted()),
%% 		int:all_breaks()},
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
	io:format("SEND:: ~p~n", [Msg]),
    _Res = (catch(Pid ! Msg)),
    ok.
    
