-module(erlide_xref).

-compile(export_all).
-export([start/0, stop/0, function_call/1]).

start() ->
	xref:start(erlide),
	spawn(fun() ->
				  erlide_log:log("xref initialization started."),
%% 				  xref:add_release(erlide, code:lib_dir(),
%% 								   [{name, otp}, {verbose, false}]),
				  erlide_log:log("xref initialization done.")
		  end),
	ok.

stop() ->
	xref:stop(erlide).

add_project(ProjectDir) ->
	xref:add_application(erlide, ProjectDir).

update() ->
	xref:update(erlide).

analyze(Module) when is_atom(Module) ->
	xref:m(Module);
analyze(Dir) when is_list(Dir) ->
	xref:d(Dir).

module_use(Module) when is_atom(Module) ->
	xref:analyze(erlide, {module_use, Module}).

module_call(Module) when is_atom(Module) ->
	xref:analyze(erlide, {module_call, Module}).

function_use({M, F, A}) when is_atom(M), is_atom(F), is_integer(A) ->
	xref:analyze(erlide, {use, {M, F, A}}).

function_use(M, F, A) when is_atom(M), is_atom(F), is_integer(A) ->
	xref:analyze(erlide, {use, {M, F, A}}).

function_call({M, F, A}) when is_atom(M), is_atom(F), is_integer(A) ->
	xref:analyze(erlide, {call, {M, F, A}}).


