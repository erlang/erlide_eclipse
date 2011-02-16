%% Author: Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
%% Created: 21-12-2010
%% Description: cover launcher for coverage plugin
%%----------------------------------------------
-module(launcher).
-behaviour(gen_server).

%%----------------------------------------------
%% Include files
%%----------------------------------------------
-include("coverage.hrl").

%%----------------------------------------------
%% Exported Functions
%%----------------------------------------------
-export([start/1,
		 start/2,
		 start_link/1,
		 stop/0,
	 	 perform/3,
	 	 prepare_and_perform/3,
	 	 set_includes/1,
		 set_beam_dir/1]).

-export([init/1,
		 handle_call/3,
		 handle_cast/2,
		 terminate/2]).

%%----------------------------------------------
%% API Functions
%%----------------------------------------------

start(Type) ->
	gen_server:start({local, ?MODULE}, ?MODULE, [Type], []).

start(Type, Nodes) ->
	gen_server:start({local, ?MODULE}, ?MODULE, [Type, Nodes], []).

start_link(Type) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Type], []).

stop() ->
	gen_server:cast(?MODULE, stop).

%% should be called if no cover-compilation
%% over selected modules were performed (external launching)
%% NameOrPathSrc -> module Name or Path to source files
%% PathTst -> path to test files (or to module on its own)
prepare_and_perform(Type, NameOrPathSrc , PathTst) ->
	TypeAtom = list_to_atom(Type),
	case TypeAtom of
		module ->
			MName = list_to_atom(NameOrPathSrc),
			gen_server:call(?MODULE, {prep, TypeAtom, MName, PathTst});
		application ->
			AName = list_to_atom(NameOrPathSrc),
			gen_server:call(?MODULE, {prep, TypeAtom, AName , PathTst});
		_ ->
			PathSrc = NameOrPathSrc,
			gen_server:call(?MODULE, {prep, TypeAtom, PathSrc , PathTst})
	end.

%perform coverage (on cover_compiled data - after performing some tests)
perform(Type,NameOrPathSrc,PathTst) ->
    TypeAtom = list_to_atom(Type), 
	case TypeAtom of
		module ->
			MName = list_to_atom(NameOrPathSrc),
			gen_server:call(?MODULE, {perform, TypeAtom, MName, PathTst}); 
		application ->
			AName = list_to_atom(NameOrPathSrc),
			gen_server:call(?MODULE, {perform, TypeAtom, AName , PathTst});
		_ ->
			PathSrc = NameOrPathSrc,
			gen_server:call(?MODULE, {perform, TypeAtom, PathSrc , PathTst})
	end.

% set include directories
set_includes(Includes) ->
	gen_server:call(?MODULE, {includes, Includes}).

% adds beam dir to the code path
set_beam_dir(Dir) ->
	gen_server:call(?MODULE, {beam_dir, Dir}).


%%----------------------------------------------
%% Local Functions
%%----------------------------------------------

%init
%can be extendnded with the version with multible cover nodes
init([Type]) ->
	try cover:start() of
		{ok, _} ->
			State = #state{cover_type = Type},
			{ok, State};
		{error, Reason} ->
			erlide_jrpc:event(?EVENT, #cover_error{place = cover,
												   type = starting,
												   info = Reason}),
			{stop, Reason}
	catch
		_:_ ->
			erlide_jrpc:event(?EVENT, #cover_error{place = cover,
												   type = starting,
												   info = "cover error - check if it is installed"}),
			{stop, "cover error - check if it is installed"}
	end;
init([Type, Nodes]) ->
	try cover:start(Nodes) of
		{ok, _} ->
			State = #state{cover_type = Type},
			{ok, State};
		{error, Reason} ->
			erlide_jrpc:event(?EVENT, #cover_error{place = cover,
												   type = starting,
												   info = Reason}),
			{stop, Reason}
	catch
		_:_ ->
			erlide_jrpc:event(?EVENT, #cover_error{place = cover,
												   type = starting,
												   info = "cover error - check if it is installed"}),
			{stop, "cover error - check if it is installed"}
	end.


% cast 
handle_cast(stop, State) ->
	{stop, normal, State}.

% call 
handle_call({includes, Includes}, _From, State) ->
	{reply, ok, State#state{includes = Includes}};

handle_call({beam_dir, Dir}, _From, State) ->
	code:add_pathz(Dir),
	{reply, ok, State};

handle_call({perform, module, Module, _Path}, _From, State) ->
	Report = coverage:create_report(?COVER_DIR, Module),
	case Report of		
		{ok, Res} ->
			erlide_jrpc:event(?EVENT, Res);
		{error, Reason} ->
			erlide_jrpc:event(?EVENT, #cover_error{place = Module,
												   type = 'creating report',
												   info = Reason})
	end,	
	{reply, ok, State};

handle_call({perform, all, Name}, _From, State) ->
	{reply, ok, State};

handle_call({perform, application, Name, Src}, _From,  State) ->
	{reply, ok, State};


% call -> covarage with preparation
handle_call({prep, module, Module, Path}, _From, State) ->
	PathIdx = case coverage:compile(Module, Path) of
		ok ->
			TestA = list_to_atom(atom_to_list(Module) ++ "_tests"),
			code:purge(TestA),
			code:load_file(TestA),
			case coverage:prepare(State#state.cover_type, Module, Path) of
				ok ->
					Report = coverage:create_report(Module),
						 case Report of		
							{ok, Res} ->
								erlide_jrpc:event(?EVENT, Res),
								coverage:create_index([Res]);
							{error, Reason} ->
								erlide_jrpc:event(?EVENT, #cover_error{place = Module,
												   type = 'creating report',
												   info = Reason}),
								no_file
						 end;
				{error, _} ->
					no_file
			end;
		{error, _} ->
			no_file
	end,
	erlide_jrpc:event(?EVENT, ?FINISHED),
	{reply, PathIdx, State};

handle_call({prep, all, PathSrc, PathTst}, _From, State) ->
	
	Path = case coverage:compile_dir(PathSrc) of
		{ok, Comp} ->
			ModulesSrc = lists:foldl(fun(Res, Acc) ->
										case {Res, Acc} of
											{_, error} ->
												error;
											{{ok, Mod}, _} ->
												[Mod | Acc];
											{{error, Reason}, _} ->
												erlide_jrpc:event(?EVENT, #cover_error{place = Reason,
												   type = 'compilation',
												   info = ""}),
												error
										end
									end, [], Comp),
			case ModulesSrc of
				error ->
					no_file;
				_ ->
					
					ModulesTst = get_modules(PathTst),
					Res = lists:foldl(fun(Test, _) ->
								  [PathM] = search_module(PathTst, Test),
								  TestA = list_to_atom(Test),
								%  coverage:compile_test(TestA, PathM),
								  code:purge(TestA),
								  code:load_file(TestA),
								  case coverage:prepare(State#state.cover_type, TestA, PathM) of
										{error, _} ->
												[];
						  				_ ->
												coverage:create_report(ModulesSrc)
						  		  end
							end, #cover_error{place = 'the begining',
											  type = 'creating report',
											  info = 'no tests'}, ModulesTst),
	 				case Res of
						#cover_error{} ->
								erlide_jrpc:event(?EVENT, Res),
								no_file;
						_ ->
							coverage:create_index(Res)
					end
			end;
		_ ->
			no_file
	end,
	
	erlide_jrpc:event(?EVENT, ?FINISHED),
	
	{reply, Path, State};

handle_call({prep, application, Name, Src}, _From, State) ->
	{reply, ok, State};

%% cast default
handle_call(_, _From, State) ->
	{reply, ok, State}.

%terminate
terminate(normal, _State) ->
	init:stop(0);
terminate(_Reason, _State) ->
	init:stop(1).


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

search_module(Dir, Name) ->
	filelib:fold_files(Dir,
					   ".*\.erl",
					   true,
					   fun(F, Acc) -> 
							   Module = filename:basename(F, ".erl"),
							   case Module of
								   Name -> [F | Acc];
								   _ -> Acc
							   end
					   end, []).
