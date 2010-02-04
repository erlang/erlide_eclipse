-module(erlide_xref).

-export([start/0, 
		 stop/0,
		 add_project/1,
		 analyze/1, 
		 function_call/1, 
		 function_use/1,
		 function_call/3, 
		 function_use/3,
		 module_call/1,
		 module_use/1,
		 update/0]).

-include("erlide.hrl").

start() ->
	spawn(fun() ->
				  ?SAVE_CALLS,
				  erlang:yield(),
				  xref:start(erlide), 
				  xref:set_default(erlide, [{verbose,false},{warnings,false}]),
				  %% 				  X= xref:add_release(erlide, code:lib_dir(),
				  %% 									  [{name, otp}]),
				  ok
		  end),
	ok.

stop() ->
	xref:stop(erlide).

add_project(ProjectDir) ->
	R=xref:add_application(erlide, ProjectDir),
	R.


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
	R=xref:analyze(erlide, {use, {M, F, A}}),
	R.

function_call({M, F, A}) when is_atom(M), is_atom(F), is_integer(A) ->
	xref:analyze(erlide, {call, {M, F, A}}).

function_call(M, F, A) when is_atom(M), is_atom(F), is_integer(A) ->
	xref:analyze(erlide, {call, {M, F, A}}).


