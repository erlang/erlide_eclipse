%% Author: Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
%% Created: 17-02-2011
%% Description: test launcher for testing cover plugin
%%----------------------------------------------
-module(testlauncher).
-behaviour(gen_server).

%%----------------------------------------------
%% Include files
%%----------------------------------------------
-include("test.hrl").
-include("coverage.hrl").

%%----------------------------------------------
%% Exported Functions
%%----------------------------------------------
-export([start/1,
		 start_link/1,
		 stop/0,
	 	 run_tests/2,
		 set_output_dir/1]).

-export([init/1,
		 handle_call/3,
		 handle_cast/2,
		 terminate/2,
         code_change/3,
         handle_info/2]).

%%----------------------------------------------
%% API Functions
%%----------------------------------------------

start(Type) ->
	gen_server:start({local, ?MODULE}, ?MODULE, [Type], []).

start_link(Type) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Type], []).

stop() ->
	gen_server:cast(?MODULE, stop).

run_tests(Type, Path) ->
	TypeAtom = list_to_atom(Type),
	gen_server:call(?MODULE, {test, TypeAtom, Path}).

% adds output dir to the code path
set_output_dir(Dir) ->
    gen_server:call(?MODULE,{output_dir,Dir}).

%%----------------------------------------------
%% Local Functions
%%----------------------------------------------

init([Type]) ->
	State = #test_state{type = Type},
	{ok, State}.

handle_call({output_dir, Dir}, _From, #test_state{output = DirOld} = State) ->
	code:del_path(DirOld),
	code:add_pathz(Dir),
	{reply, ok, State#test_state{output = Dir}};

handle_call({test, module, Path}, _From, State) ->
	Module = list_to_atom(filename:basename(Path, ".erl")),
	TestA = list_to_atom(atom_to_list(Module) ++ "_tests"),
	code:purge(TestA),
	code:load_file(TestA),
	Res = coverage:prepare(State#test_state.type, Module), 	%%should be moved
	{reply, Res, State};

handle_call({test, all, Path}, _From, State) ->
	ModulesTst = get_modules(Path),						
	Res = lists:foldl(fun(Tst, Acc) ->
						  TestA = list_to_atom(Tst),
								  code:purge(TestA),
								  code:load_file(TestA),
				  		  case {Acc, coverage:prepare(State#test_state.type, TestA)} of
							  {ok, ok} ->
								  ok;
							  _ ->
								  #cover_error{place = Path,
									 type = testing,
									 info = ""}
						  end
						  end, ok,
						  ModulesTst),
	{reply, Res, State}.

handle_cast(stop, State) ->
	{stop, normal, State}.

%terminate
terminate(normal, _State) ->
	init:stop(0);
terminate(_Reason, _State) ->
	init:stop(1).

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------
%% Helpers
%%----------------------------------------------

get_modules(Dir) ->
	filelib:fold_files(Dir,
					   ".*\.erl",
					   true,
					   fun(F, Acc) -> 
								[filename:basename(F, ".erl") | Acc]
					   end, []).



