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
%%     $Id$
%%
-module(erlide_int).

%% External exports
-export([i/1, i/2, ni/1, ni/2, n/1, nn/1, n2/2, interpreted/0, file/1,
	 interpretable/1, interpret_beam/2]).
-export([auto_attach/0, auto_attach/1, auto_attach/2,
	 stack_trace/0, stack_trace/1]).
-export([break/2, delete_break/2, break_in/3, del_break_in/3,
	 no_break/0, no_break/1,
	 disable_break/2, enable_break/2,
	 action_at_break/3, test_at_break/3, get_binding/2,
	 all_breaks/0, all_breaks/1]).
-export([snapshot/0, clear/0]).
-export([continue/1, continue/3]).

%% External exports only to be used by Debugger
-export([start/0, stop/0, subscribe/0]).
-export([attach/2, step/1, next/1, finish/1]).

%% External exports only to be used by an attached process
-export([attached/1, meta/2, meta/3, contents/2, functions/1]).

%% External export only to be used by error_handler
-export([eval/3]).

%%==Erlang Interpreter================================================
%%
%% int
%% ---
%% Interface module.
%%
%% i
%% -
%% Interface module to int, retained for backwards compatibility only.
%%
%% erlide_dbg_debugged
%% ------------
%% Contains the message loops for a debugged process and is the main
%% entry point from the breakpoint handler in the error_handler module
%% (via the int module).
%%
%% When a process is debugged, most code is executed in another
%% process, called the meta process. When the meta process is
%% interpreting code, the process being debugged just waits in a
%% receive loop in erlide_dbg_debugged. However the debugged process itself
%% calls any BIFs that must execute in the correct process (such as
%% link/1 and spawn_link/1), and external code which is not
%% interpreted.
%%
%% erlide_dbg_icmd, erlide_dbg_ieval
%% -------------------
%% Code for the meta process.
%%
%% erlide_dbg_iserver
%% -----------
%% Interpreter main process, keeping and distributing information
%% about interpreted modules and debugged processes.
%%
%% erlide_dbg_idb
%% -------
%% ETS wrapper, allowing transparent access to tables at a remote node.
%%
%% erlide_dbg_iload
%% ---------
%% Code for interpreting a module.
%%====================================================================

%%====================================================================
%% External exports
%%====================================================================

%%--------------------------------------------------------------------
%% i(AbsMods) -> {module,Mod} | error | ok
%% ni(AbsMods) -> {module,Mod} | error | ok
%%   AbsMods = AbsMod | [AbsMod]
%%     AbsMod = atom() | string()
%%     Mod = atom()
%%     Options = term() ignored
%%--------------------------------------------------------------------
i(AbsMods) -> i2(AbsMods, local).
i(AbsMods, _Options) -> i2(AbsMods, local).
ni(AbsMods) -> i2(AbsMods, distributed).
ni(AbsMods, _Options) -> i2(AbsMods, distributed).

i2([AbsMod|AbsMods], Dist) when is_atom(AbsMod); is_list(AbsMod) ->
    int_mod(AbsMod, Dist),
    i2(AbsMods, Dist);
i2([AbsMod], Dist) when is_atom(AbsMod); is_list(AbsMod) ->
    int_mod(AbsMod, Dist);
i2([], _Dist) ->
    ok;
i2(AbsMod, Dist) when is_atom(AbsMod); is_list(AbsMod) ->
    int_mod(AbsMod, Dist).

interpret_beam(AbsBeam, Dist) ->
    case check_beam(AbsBeam) of
        {ok, Exp, Abst} ->
            Mod = list_to_atom(filename:basename(AbsBeam, ".beam")),
            load({Mod, AbsBeam, Exp, Abst}, Dist);
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% n(AbsMods) -> ok
%% nn(AbsMods) -> ok
%%--------------------------------------------------------------------
n(AbsMods) -> n2(AbsMods, local).
nn(AbsMods) -> n2(AbsMods, distributed).

n2([AbsMod|AbsMods], Dist) when is_atom(AbsMod); is_list(AbsMod) ->
    del_mod(AbsMod, Dist),
    n2(AbsMods, Dist);
n2([AbsMod], Dist) when is_atom(AbsMod); is_list(AbsMod) ->
    del_mod(AbsMod, Dist);
n2([], _Dist) ->
    ok;
n2(AbsMod, Dist) when is_atom(AbsMod); is_list(AbsMod) ->
    del_mod(AbsMod, Dist).

%%--------------------------------------------------------------------
%% interpreted() -> [Mod]
%%--------------------------------------------------------------------
interpreted() ->
    erlide_dbg_iserver:safe_call(all_interpreted).

%%--------------------------------------------------------------------
%% file(Mod) -> File | {error, not_loaded}
%%   Mod = atom()
%%   File = string()
%%--------------------------------------------------------------------
file(Mod) when is_atom(Mod) ->
    erlide_dbg_iserver:safe_call({file, Mod}).

%%--------------------------------------------------------------------
%% interpretable(AbsMod) -> true | {error, Reason}
%%   AbsMod = Mod | File
%%   Reason = no_src | no_beam | no_debug_info | badarg | {app, App}
%%--------------------------------------------------------------------
interpretable(AbsMod) ->
    case check(AbsMod) of
	{ok, _Res} -> true;
	Error -> Error
    end.

%%--------------------------------------------------------------------
%% auto_attach() -> false | {Flags, Function}
%% auto_attach(false)
%% auto_attach(false|Flags, Function)
%%   Flags = Flag | [Flag]
%%     Flag = init | break | exit
%%   Function = {Mod, Func} | {Mod, Func, Args}
%% Will result in calling:
%%  spawn(Mod, Func, [Dist, Pid, Meta | Args]) (living process) or
%%  spawn(Mod, Func, [Dist, Pid, Reason, Info | Args]) (dead process)
%%--------------------------------------------------------------------
auto_attach() ->
    erlide_dbg_iserver:safe_call(get_auto_attach).

auto_attach(false) ->
    erlide_dbg_iserver:safe_cast({set_auto_attach, false}).

auto_attach([], _Function) ->
    auto_attach(false);
auto_attach(Flags, {Mod, Func}) ->
    auto_attach(Flags, {Mod, Func, []});
auto_attach(Flags, {Mod, Func, Args}) when is_atom(Mod),is_atom(Func),is_list(Args) ->
    check_flags(Flags),
    erlide_dbg_iserver:safe_cast({set_auto_attach, Flags, {Mod, Func, Args}}).

check_flags([init|Flags]) -> check_flags(Flags);
check_flags([break|Flags]) -> check_flags(Flags);
check_flags([exit|Flags]) -> check_flags(Flags);
check_flags([]) -> true.

%%--------------------------------------------------------------------
%% stack_trace() -> Flag
%% stack_trace(Flag)
%%   Flag = all | true | no_tail | false
%%--------------------------------------------------------------------
stack_trace() ->
    erlide_dbg_iserver:safe_call(get_stack_trace).

stack_trace(true) ->
    stack_trace(all);
stack_trace(Flag) ->
    check_flag(Flag),
    erlide_dbg_iserver:safe_cast({set_stack_trace, Flag}).

check_flag(all) -> true;
check_flag(no_tail) -> true;
check_flag(false) -> true.

%%--------------------------------------------------------------------
%% break(Mod, Line) -> ok | {error, break_exists}
%% delete_break(Mod, Line) -> ok
%% break_in(Mod, Func, Arity) -> ok | {error, function_not_found}
%% del_break_in(Mod, Function, Arity) -> ok | {error, function_not_found}
%% no_break()
%% no_break(Mod)
%% disable_break(Mod, Line) -> ok
%% enable_break(Mod, Line) -> ok
%% action_at_break(Mod, Line, Action) -> ok
%% test_at_break(Mod, Line, Function) -> ok
%% get_binding(Var, Bindings) -> {value, Value} | unbound
%% all_breaks() -> [Break]
%% all_breaks(Mod) -> [Break]
%%   Mod = atom()
%%   Line = integer()
%%   Func = atom() function name
%%   Arity = integer()
%%   Action = enable | disable | delete
%%   Function = {Mod, Func} must have arity 1 (Bindings)
%%   Var = atom()
%%   Bindings = Value = term()
%%   Break = {Point, Options}
%%     Point = {Mod, Line}
%%     Options = [Status, Action, null, Cond]
%%       Status = active | inactive
%%       Cond = null | Function
%%--------------------------------------------------------------------
break(Mod, Line) when is_atom(Mod), is_integer(Line) ->
    erlide_dbg_iserver:safe_call({new_break, {Mod, Line},
			   [active, enable, null, null]}).

delete_break(Mod, Line) when is_atom(Mod), is_integer(Line) ->
    erlide_dbg_iserver:safe_cast({delete_break, {Mod, Line}}).

break_in(Mod, Func, Arity) when is_atom(Mod), is_atom(Func), is_integer(Arity) ->
    case erlide_dbg_iserver:safe_call({is_interpreted, Mod, Func, Arity}) of
	{true, Clauses} ->
	    Lines = first_lines(Clauses),
	    lists:foreach(fun(Line) -> break(Mod, Line) end, Lines);
	false ->
	    {error, function_not_found}
    end.

del_break_in(Mod, Func, Arity) when is_atom(Mod), is_atom(Func), is_integer(Arity) ->
    case erlide_dbg_iserver:safe_call({is_interpreted, Mod, Func, Arity}) of
	{true, Clauses} ->
	    Lines = first_lines(Clauses),
	    lists:foreach(fun(Line) -> delete_break(Mod, Line) end,
			  Lines);
	false ->
	    {error, function_not_found}
    end.

first_lines(Clauses) ->
    lists:map(fun(Clause) -> element(2, hd(element(5, Clause))) end,
	      Clauses).

no_break() ->
    erlide_dbg_iserver:safe_cast(no_break).

no_break(Mod) when is_atom(Mod) ->
    erlide_dbg_iserver:safe_cast({no_break, Mod}).

disable_break(Mod, Line) when is_atom(Mod), is_integer(Line) ->
    erlide_dbg_iserver:safe_cast({break_option, {Mod, Line}, status, inactive}).

enable_break(Mod, Line) when is_atom(Mod), is_integer(Line) ->
    erlide_dbg_iserver:safe_cast({break_option, {Mod, Line}, status, active}).

action_at_break(Mod, Line, Action) when is_atom(Mod), is_integer(Line) ->
    check_action(Action),
    erlide_dbg_iserver:safe_cast({break_option, {Mod, Line}, action, Action}).

check_action(enable) -> true;
check_action(disable) -> true;
check_action(delete) -> true.

test_at_break(Mod, Line, Function) when is_atom(Mod), is_integer(Line) ->
    check_function(Function),
    erlide_dbg_iserver:safe_cast({break_option, {Mod, Line}, condition, Function}).

check_function({Mod, Func}) when is_atom(Mod), is_atom(Func) -> true.

get_binding(Var, Bs) ->
    erlide_dbg_icmd:get_binding(Var, Bs).

all_breaks() ->
    erlide_dbg_iserver:safe_call(all_breaks).
all_breaks(Mod) when is_atom(Mod) ->
    erlide_dbg_iserver:safe_call({all_breaks, Mod}).

%%--------------------------------------------------------------------
%% snapshot() -> [{Pid, Init, Status, Info}]
%%   Pid = pid()
%%   Init = atom()  First interpreted function
%%   Status = idle | running | waiting | break | exit
%%   Info = {} | {Mod, Line} | ExitReason
%%     Mod = atom()
%%     Line = integer()
%%     ExitReason = term()
%%--------------------------------------------------------------------
snapshot() ->
    erlide_dbg_iserver:safe_call(snapshot).

%%--------------------------------------------------------------------
%% clear()
%%--------------------------------------------------------------------
clear() ->
    erlide_dbg_iserver:safe_cast(clear).

%%--------------------------------------------------------------------
%% continue(Pid) -> ok | {error, not_interpreted}
%% continue(X, Y, Z) -> ok | {error, not_interpreted}
%%--------------------------------------------------------------------
continue(Pid) when is_pid(Pid) ->
    case erlide_dbg_iserver:safe_call({get_meta, Pid}) of
	{ok, Meta} when is_pid(Meta) ->
	    erlide_dbg_icmd:continue(Meta),
	    ok;
	Error ->
	    Error
    end.

continue(X, Y, Z) when is_integer(X), is_integer(Y), is_integer(Z) ->
    continue(c:pid(X, Y, Z)).


%%====================================================================
%% External exports only to be used by Debugger
%%====================================================================

%%--------------------------------------------------------------------
%% start()
%% stop()
%% Functions for starting and stopping erlide_dbg_iserver explicitly.
%%--------------------------------------------------------------------
start() -> erlide_dbg_iserver:start().
stop() ->
    lists:foreach(
      fun(Mod) ->
	      everywhere(distributed,
			 fun() ->
				 erts_debug:breakpoint({Mod,'_','_'}, false)
			 end)
      end,
      interpreted()),
    erlide_dbg_iserver:stop().

%%--------------------------------------------------------------------
%% subscribe()
%% Subscribe to information from erlide_dbg_iserver. The process calling this
%% function will receive the following messages:
%%   {int, {interpret, Mod}}
%%   {int, {no_interpret, Mod}}
%%   {int, {new_process, Pid, Function, Status, Info}}
%%   {int, {new_status, Pid, Status, Info}}
%%   {int, {new_break, {Point, Options}}}
%%   {int, {delete_break, Point}}
%%   {int, {break_options, {Point, Options}}}
%%   {int, no_break}
%%   {int, {no_break, Mod}}
%%   {int, {auto_attach, false|{Flags, Function}}}
%%   {int, {stack_trace, Flag}}
%%--------------------------------------------------------------------
subscribe() -> erlide_dbg_iserver:cast({subscribe, self()}).

%%--------------------------------------------------------------------
%% attach(Pid, Function)
%%   Pid = pid()
%%   Function = {Mod, Func} | {Mod, Func, Args} (see auto_attach/2)
%% Tell erlide_dbg_iserver to attach to Pid using Function. Will result in:
%%   spawn(Mod, Func, [Pid, Status | Args])
%%--------------------------------------------------------------------
attach(Pid, {Mod, Func}) ->
    attach(Pid, {Mod, Func, []});
attach(Pid, Function) ->
    erlide_dbg_iserver:cast({attach, Pid, Function}).

%%--------------------------------------------------------------------
%% step(Pid)
%% next(Pid)
%% (continue(Pid))
%% finish(Pid)
%%--------------------------------------------------------------------
step(Pid) ->
    {ok, Meta} = erlide_dbg_iserver:call({get_meta, Pid}),
    erlide_dbg_icmd:step(Meta).
next(Pid) ->
    {ok, Meta} = erlide_dbg_iserver:call({get_meta, Pid}),
    erlide_dbg_icmd:next(Meta).
finish(Pid) ->
    {ok, Meta} = erlide_dbg_iserver:call({get_meta, Pid}),
    erlide_dbg_icmd:finish(Meta).


%%====================================================================
%% External exports only to be used by an attached process
%%====================================================================

%%--------------------------------------------------------------------
%% attached(Pid) -> {ok, Meta} | error
%%   Pid = Meta = pid()
%% Tell erlide_dbg_iserver that I have attached to Pid. erlide_dbg_iserver informs
%% the meta process and returns its pid. erlide_dbg_iserver may also refuse,
%% if there already is a process attached to Pid.
%%--------------------------------------------------------------------
attached(Pid) ->
    erlide_dbg_iserver:call({attached, self(), Pid}).

%%--------------------------------------------------------------------
%% meta(Meta, Cmd)
%%   Meta = pid()
%%   Cmd = step | next | continue | finish | skip | timeout | stop
%%   Cmd = messages => [Message]
%% meta(Meta, Cmd, Arg)
%%   Cmd = trace,       Arg = bool()
%%   Cmd = stack_trace  Arg = all | notail | false
%%   Cmd = stack_frame  Arg = {up|down, Sp}
%%       => {Sp, Mod, Line} | top | bottom
%%   Cmd = backtrace    Arg = integer()
%%       => {Sp, Mod, {Func, Arity}, Line}
%%   Cmd = eval        Arg = {Cm, Cmd} | {Cm, Cmd, Sp}
%%--------------------------------------------------------------------
meta(Meta, step) -> erlide_dbg_icmd:step(Meta);
meta(Meta, next) -> erlide_dbg_icmd:next(Meta);
meta(Meta, continue) -> erlide_dbg_icmd:continue(Meta);
meta(Meta, finish) -> erlide_dbg_icmd:finish(Meta);
meta(Meta, skip) -> erlide_dbg_icmd:skip(Meta);
meta(Meta, timeout) -> erlide_dbg_icmd:timeout(Meta);
meta(Meta, stop) -> erlide_dbg_icmd:stop(Meta);
meta(Meta, messages) -> erlide_dbg_icmd:get(Meta, messages, null).

meta(Meta, trace, Trace) -> erlide_dbg_icmd:set(Meta, trace, Trace);
meta(Meta, stack_trace, Flag) -> erlide_dbg_icmd:set(Meta, stack_trace, Flag);
meta(Meta, bindings, Stack) -> erlide_dbg_icmd:get(Meta, bindings, Stack);
meta(Meta, stack_frame, Arg) -> erlide_dbg_icmd:get(Meta, stack_frame, Arg);
meta(Meta, backtrace, N) -> erlide_dbg_icmd:get(Meta, backtrace, N);
meta(Meta, eval, Arg) -> erlide_dbg_icmd:eval(Meta, Arg).

%%--------------------------------------------------------------------
%% contents(Mod, Pid) -> string()
%%   Mod = atom()
%%   Pid = pid() | any
%% Return the contents of an interpreted module.
%%--------------------------------------------------------------------
contents(Mod, Pid) ->
    {ok, Bin} = erlide_dbg_iserver:call({contents, Mod, Pid}),
    binary_to_list(Bin).

%%--------------------------------------------------------------------
%% functions(Mod) -> [[Name, Arity]]
%%   Mod = Name = atom()
%%   Arity = integer()
%%--------------------------------------------------------------------
functions(Mod) ->
    lists:filter(fun([module_info, _Arity]) -> false;
		    (_Func) -> true
		 end,
		 erlide_dbg_iserver:call({functions, Mod})).


%%====================================================================
%% External exports only to be used by error_handler
%%====================================================================

eval(Mod, Func, Args) ->
    erlide_dbg_debugged:eval(Mod, Func, Args).


%%====================================================================
%% Internal functions
%%====================================================================

%%--Interpreting modules----------------------------------------------

int_mod(AbsMod, Dist) ->
    case check(AbsMod) of
	{ok, Res} -> load(Res, Dist);
	{error, {app, App}} ->
	    io:format("** Cannot interpret ~p module: ~p~n",
		      [App, AbsMod]),
	    error;
	_Error ->
	    io:format("** Invalid beam file or no abstract code: ~p\n",
		      [AbsMod]),
	    error
    end.

check(Mod) when is_atom(Mod) -> catch check_module(Mod);
check(File) when is_list(File) -> catch check_file(File).

load({Mod, Src, Beam, Exp, Abst}, Dist) ->
    everywhere(Dist,
	       fun() ->
		       code:purge(Mod),
		       erts_debug:breakpoint({Mod,'_','_'}, false),
                       %%erlang:display({load, Mod, node()}),
		       {module,Mod} = code:load_abs(filename:rootname(Beam),
						    Mod)
	       end),
    {ok, SrcBin} = file:read_file(Src),
    {ok, BeamBin} = file:read_file(Beam),
    MD5 = code:module_md5(BeamBin),
    Bin = term_to_binary({interpreter_module,Exp,Abst,SrcBin,MD5}),
    {module, Mod} = erlide_dbg_iserver:safe_call({load, Mod, Src, Bin}),
    everywhere(Dist,
	       fun() ->
		       true = erts_debug:breakpoint({Mod,'_','_'}, true) > 0
	       end),
    {module, Mod};
load({Mod, Beam, Exp, Abst}, Dist) ->
    %%X = everywhere(Dist, fun() -> erlang:display({lo2, node(), is_alive()}) end),
    %%erlang:display({x, X}),
    everywhere(Dist,
               fun() ->
                       code:purge(Mod),
                       erts_debug:breakpoint({Mod,'_','_'}, false),
                       {module,Mod} = code:load_abs(filename:rootname(Beam),
                                                    Mod),
                       %%erlang:display({load2, Mod, node()}),
                       {module,Mod}
               end),
    {ok, BeamBin} = file:read_file(Beam),
    MD5 = code:module_md5(BeamBin),
    Bin = term_to_binary({interpreter_module,Exp,Abst,<<>>,MD5}),
    {module, Mod} = erlide_dbg_iserver:safe_call({load, Mod, no_source, Bin}),
    everywhere(Dist,
	       fun() ->
		       true = erts_debug:breakpoint({Mod,'_','_'}, true) > 0
	       end),
    {module, Mod}.

check_module(Mod) ->
    case code:which(Mod) of
	Beam when is_list(Beam) ->
	    case find_src(Beam) of
		Src when is_list(Src) ->
		    check_application(Src),
		    case check_beam(Beam) of
			{ok, Exp, Abst} ->
			    {ok, {Mod, Src, Beam, Exp, Abst}};
			{error, Other} -> {error, Other}
		    end;
		error -> {error, no_src}
	    end;
	_ -> {error, badarg}
    end.

check_file(Name0) ->
    Src = case is_file(Name0) of
	      true -> Name0;
	      false ->
		  Name = Name0 ++ ".erl",
		  case is_file(Name) of
		      true -> Name;
		      false -> error
		  end
	  end,
    if
	is_list(Src) ->
	    check_application(Src),
	    Mod = scan_module_name(Src),
	    case find_beam(Mod, Src) of
		Beam when is_list(Beam) ->
		    case check_beam(Beam) of
			{ok, Exp, Abst} ->
			    {ok, {Mod, Src, Beam, Exp, Abst}};
			{error, Other} -> {error, Other}
		    end;
		error -> {error, no_beam}
	    end;
	true -> {error, badarg}
    end.

%% Try to avoid interpreting a kernel, stdlib, gs or debugger module.
check_application(Src) ->
    case lists:reverse(filename:split(filename:absname(Src))) of
	[_Mod,"src",AppS|_] ->
	    check_application2(AppS);
	_ -> ok
    end.
check_application2("kernel-"++_) -> throw({error,{app,kernel}});
check_application2("stdlib-"++_) -> throw({error,{app,stdlib}});
check_application2("gs-"++_) -> throw({error,{app,gs}});
check_application2("debugger-"++_) -> throw({error,{app,debugger}});
check_application2(_) -> ok.

find_src(Beam) ->
    Src0 = filename:rootname(Beam) ++ ".erl",
    case is_file(Src0) of
	true -> Src0;
	false ->
	    EbinDir = filename:dirname(Beam),
	    Src = filename:join([filename:dirname(EbinDir), "src",
				 filename:basename(Src0)]),
	    case is_file(Src) of
		true -> Src;
		false -> error
	    end
    end.

find_beam(Mod, Src) ->
    SrcDir = filename:dirname(Src),
    BeamFile = packages:last(Mod) ++ code_aux:objfile_extension(),
    File = filename:join(SrcDir, BeamFile),
    case is_file(File) of
	true -> File;
	false -> find_beam_1(Mod, SrcDir)
    end.

find_beam_1(Mod, SrcDir) ->
    RootDir = find_root_dir(SrcDir, packages:first(Mod)),
    EbinDir = filename:join(RootDir, "ebin"),
    CodePath = [EbinDir | code:get_path()],
    BeamFile = code_aux:to_path(Mod) ++ code_aux:objfile_extension(),
    lists:foldl(fun(_, Beam) when is_list(Beam) -> Beam;
		   (Dir, error) ->
			File = filename:join(Dir, BeamFile),
			case is_file(File) of
			    true -> File;
			    false -> error
			end
		end,
		error,
		CodePath).

find_root_dir(Dir, [_|Ss]) ->
    find_root_dir(filename:dirname(Dir), Ss);
find_root_dir(Dir, []) ->
    filename:dirname(Dir).

check_beam(Beam) ->
    case beam_lib:chunks(Beam, [abstract_code,exports]) of
	{ok,{Mod,[{abstract_code,no_abstract_code}|_]}} ->
	    {error, no_abstract_code, Mod};
	{ok,{_Mod,[{abstract_code,Abst},{exports,Exp}]}} ->
	    {ok,Exp,Abst};
	Other -> {error, Other}
    end.

is_file(Name) ->
    case filelib:is_file(Name) of
	true ->
	    not filelib:is_dir(Name);
	false ->
	    false
    end.

everywhere(distributed, Fun) ->
    case is_alive() of
	true -> rpc:multicall(erlang, apply, [Fun,[]]);
	false -> Fun()
    end;
everywhere(local, Fun) ->
    Fun().

scan_module_name(File) ->
    case file:open(File, [read]) of
	{ok, FD} ->
	    R = (catch {ok, scan_module_name_1(FD)}),
	    file:close(FD),
	    case R of
		{ok, A} when is_atom(A) -> A;
		_ -> error
	    end;
	_ ->
	    error
    end.

scan_module_name_1(FD) ->
    case io:scan_erl_form(FD, "") of
	{ok, Ts, _} ->
	    scan_module_name_2(Ts, FD);
	_ ->
	    error
    end.

scan_module_name_2([{'-',_},{atom,_,module},{'(',_} | _]=Ts, _FD) ->
    scan_module_name_3(Ts);
scan_module_name_2([{'-',_},{atom,_,_} | _], FD) ->
    scan_module_name_1(FD);
scan_module_name_2(_, _) ->
    error.

scan_module_name_3(Ts) ->
    case erl_parse:parse_form(Ts) of
	{ok, {attribute,_,module,{M,_}}} -> module_atom(M);
	{ok, {attribute,_,module,M}} -> module_atom(M);
	_ -> error
    end.

module_atom(A) when is_atom(A) -> A;
module_atom(L) when is_list(L) -> list_to_atom(packages:concat(L)).

%%--Stop interpreting modules-----------------------------------------

del_mod(AbsMod, Dist) ->
    Mod = if
	      is_atom(AbsMod) -> AbsMod;
	      is_list(AbsMod) ->
		  list_to_atom(filename:basename(AbsMod, ".beam"))
	  end,
    erlide_dbg_iserver:safe_cast({delete, Mod}),
    everywhere(Dist,
	       fun() ->
		       erts_debug:breakpoint({Mod,'_','_'}, false),
		       erlang:yield()
	       end),
    ok.



