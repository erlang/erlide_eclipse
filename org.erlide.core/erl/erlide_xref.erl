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

start() ->
	spawn(fun() ->
				  erlang:yield(),
				  erlide_log:logp(erlang:now()),
				  xref:start(erlide), 
				  erlide_log:logp(erlang:now()),
				  %erlide_log:log("-- xref initialization started."),
%% 				  X= xref:add_release(erlide, code:lib_dir(),
%% 									  [{name, otp}, {verbose, false}, {warnings,false}]),
				  %				  X=1,
				  %erlide_log:logp({"xref initialization done. ",X})
				  erlide_log:logp(erlang:now()),
				  ok
		  end),
				  erlide_log:logp({">>>",erlang:now()}),
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

function_call(M, F, A) when is_atom(M), is_atom(F), is_integer(A) ->
	xref:analyze(erlide, {call, {M, F, A}}).


