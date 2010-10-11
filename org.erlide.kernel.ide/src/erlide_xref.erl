-module(erlide_xref).

-export([start/0, 
	 stop/0,
	 add_project/1,
	 add_dirs/1,
	 analyze/1, 
	 function_call/1, 
	 function_use/1,
	 function_call/3, 
	 function_use/3,
         modules/0,
	 module_call/1,
	 module_use/1,
	 update/0]).

%-define(DEBUG, 1).

-define(XREF, erlide_xref).

-include("erlide.hrl").

start() ->
    start(whereis(?XREF)).

start(undefined) ->
%%     spawn(fun() ->
		  ?SAVE_CALLS,
		  erlang:yield(),
		  xref:start(?XREF), 
		  xref:set_default(?XREF, [{verbose,false},{warnings,false}]),
		  %% 				  X= xref:add_release(erlide, code:lib_dir(),
		  %% 									  [{name, otp}]),
%% 		  ok
%% 	  end),
    ok;
start(_) ->
    ok.

stop() ->
    xref:stop(?XREF).

add_project(ProjectDir) ->
    R=xref:add_application(?XREF, ProjectDir),
    R.

add_dirs([]) ->
	ok;
add_dirs([BeamDir | Rest]) ->
	start(),
	update(),
	?D(BeamDir),
	case xref:add_directory(?XREF, BeamDir, [{recurse, false}]) of
		{ok, _} = _R ->
			?D(_R),
			add_dirs(Rest);
		{error, xref_base, {module_clash, _}} ->
			add_dirs(Rest);
		Error ->
			?D(Error),
			Error
	end.

%% add_dir(BeamDir) ->
%% 	start(),
%%     R = xref:add_directory(?XREF, BeamDir),
%%     ?D(R),
%%     R.

update() ->
    start(),
    xref:update(?XREF, []).

modules() ->
    start(),
    xref:q(?XREF, "M").

analyze(Module) when is_atom(Module) ->
    start(),
    xref:m(Module);
analyze(Dir) when is_list(Dir) ->
	start(),
    xref:d(Dir).

module_use(Module) when is_atom(Module) ->
	start(),
    xref:analyze(?XREF, {module_use, Module}).

module_call(Module) when is_atom(Module) ->
	start(),
    xref:analyze(?XREF, {module_call, Module}).

function_use({M, F, A}) when is_atom(M), is_atom(F), is_integer(A) ->
	start(),
    xref:analyze(?XREF, {use, [{M, F, A}]}, []).

function_use(M, F, A) ->
	function_use({M, F, A}).

function_call({M, F, A}) when is_atom(M), is_atom(F), is_integer(A) ->
    xref:analyze(?XREF, {call, {M, F, A}}).

function_call(M, F, A) when is_atom(M), is_atom(F), is_integer(A) ->
    xref:analyze(?XREF, {call, {M, F, A}}).
