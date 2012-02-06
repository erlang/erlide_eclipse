%% Author: jakob
%% Created: 21 aug 2008
-module(int).
-author(jakobce@gmail.com).

%% TODO Since the error_handler (in R12 and earlier) hard-codes the int
%% module, we replace it to call erlide_int
%% this should be improved in R13

%%
%% Include files
%%

%%
%% Exported Functions
%%

-export([i/1, ni/1, n/1, nn/1, interpreted/0, file/1,
	 interpretable/1, auto_attach/0, auto_attach/1, auto_attach/2,
	 stack_trace/0, stack_trace/1, break/2, delete_break/2, break_in/3,
	 del_break_in/3, no_break/0, no_break/1, disable_break/2, enable_break/2,
	 action_at_break/3, test_at_break/3, get_binding/2,
	 all_breaks/0, all_breaks/1, snapshot/0, clear/0, continue/1, continue/3]).

%%
%% Internal Export
%%
-export([eval/3]).

%%
%% API Functions
%%

eval(Mod, Func, Args) -> 
	erlide_int:eval(Mod, Func, Args).

%%
%% Local Functions
%%

i(M) -> erlide_int:i(M).
ni(M) -> erlide_int:ni(M).
n(M) -> erlide_int:n(M).
nn(M) -> erlide_int:nn(M).
interpreted() -> erlide_int:interpreted().
file(M) -> erlide_int:file(M).
interpretable(M) -> erlide_int:interpretable(M).
auto_attach() -> nyi(auto_attach).
auto_attach(_) -> nyi(auto_attach).
auto_attach(Flags, Fnk) ->erlide_int:auto_attach(Flags, Fnk).
stack_trace() -> nyi(stack_trace).
stack_trace(_) -> nyi(stack_trace).
break(Mod, Line) -> erlide_int:break(Mod, Line).
delete_break(Mod, Line) -> erlide_int:delete_break(Mod, Line).
break_in(Mod, Func, Arity) -> erlide_int:break_in(Mod, Func, Arity).
del_break_in(Mod, Func, Arity) -> erlide_int:del_break_in(Mod, Func, Arity).
no_break() -> erlide_int:no_break().
no_break(Mod) -> erlide_int:no_break(Mod).
action_at_break(_, _, _) -> nyi(action_at_break).
test_at_break(_, _, _) -> nyi(test_at_break).
get_binding(_Var, _Bs) -> nyi(get_binding).
all_breaks() -> erlide_int:all_breaks().
all_breaks(Mod) -> erlide_int:all_breaks(Mod).
disable_break(Mod, Line) -> erlide_int:disable_break(Mod, Line).
enable_break(Mod, Line) -> erlide_int:enable_break(Mod, Line).
snapshot() -> nyi(snapshot).
clear() -> nyi(clear).
continue(Pid) -> erlide_int:continue(Pid).
continue(X, Y, Z) -> erlide_int:continue(X, Y, Z).

nyi(F) -> 
    io:format("function ~p unavailable from within ErlIDE, use Eclipse debugger functions instead\n", [F]), 
    {error, unavaible}.


