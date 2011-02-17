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

%%----------------------------------------------
%% Exported Functions
%%----------------------------------------------
-export([start/1,
		 start_link/1,
		 stop/0,
	 	 run_tests/2]).

-export([init/1,
		 handle_call/3,
		 handle_cast/2,
		 terminate/2]).

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

%%----------------------------------------------
%% Local Functions
%%----------------------------------------------

init([Type]) ->
	State = #test_state{type = Type},
	{ok, State}.

handle_call({test, module, Path}, _From, State) ->
	Module = list_to_atom(filename:basename(Path, ".erl")),
	TestA = list_to_atom(atom_to_list(Module) ++ "_tests"),
	code:purge(TestA),
	code:load_file(TestA),
	Res = coverage:prepare(State#test_state.type, Module), 	%%should be moved
	{reply, Res, State};

handle_call({test, all, Path}, _From, State) ->
	ModulesTst = launcher:get_modules(Path),						%%!
	lists:foreach(fun(Tst) ->
						  TestA = list_to_atom(Tst),
								  code:purge(TestA),
								  code:load_file(TestA)
						  end,
						  ModulesTst),
	Res = coverage:prepare(State#test_state.type, {dir, Path}),
	{reply, ok, State}.

handle_cast(stop, State) ->
	{stop, normal, State}.

%terminate
terminate(normal, _State) ->
	init:stop(0);
terminate(_Reason, _State) ->
	init:stop(1).



