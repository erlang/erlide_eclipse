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
	 start_link/1,
	 restart/1,
	 stop/0,
	 prepare/1,
	 analyse/1,
	 set_includes/1,
	 set_report_dir/1]).

-export([init/1,
		 handle_call/3,
		 handle_cast/2,
		 terminate/2,
         code_change/3,
         handle_info/2]).

%%----------------------------------------------
%% API Functions
%%----------------------------------------------

start(Nodes) ->
	gen_server:start({local, ?MODULE}, ?MODULE, [Nodes], []).

start_link(Nodes) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Nodes], []).

% restarting cover
restart(Nodes) ->
	case start(Nodes) of
		{ok, _} ->
			ok;
		{error, {already_started, _}} ->
			already_started;
		_ ->
			error
	end.

stop() ->
	gen_server:cast(?MODULE, stop).

%% should be called if no cover-compilation
%% over selected modules were performed (external launching)
%% NameOrPathSrc -> module Name or Path to source files
%% PathTst -> path to test files (or to module on its own)
% deprecated
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
% deprecated
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

%prepares modules - cover compiles them
prepare(Paths) ->
	lists:foreach(fun(Path) ->
						  Mod = list_to_atom(filename:basename(Path, ".erl")),
						  gen_server:call(?MODULE, {compile, Mod, Path})
						  end, Paths).

% performes cover analysis and prepares reports
analyse(Modules) ->
	gen_server:call(?MODULE, {analyse, Modules}).

% set include directories
set_includes(Includes) ->
	gen_server:call(?MODULE, {includes, Includes}).

set_report_dir(Path) ->
	gen_server:call(?MODULE, {report_dir, Path}).

%%----------------------------------------------
%% Local Functions
%%----------------------------------------------

%init
%starts cover on multiple nodes
init([Nodes]) ->
	try cover:start(Nodes) of
		{ok, _} ->
			State = #state{},
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

handle_call({report_dir, Path}, _From, State) ->
	{reply, ok, State#state{report_dir = Path}};

handle_call({compile, Module, Path}, _From, #state{includes = Includes} = State) ->
	Res = coverage:compile(Module, Path, Includes),
	{reply, Res, State};

handle_call({analyse, Modules}, _From, State) ->
	io:format("~p~n", [Modules]),
	ModsOk = coverage:create_report(Modules, ?COVER_DIR),
	io:format("~p~n", [ModsOk]),
	Path = case ModsOk of
				#cover_error{} ->
					erlide_jrpc:event(?EVENT, ModsOk),
					no_file;
				_ ->
					coverage:create_index(ModsOk, ?COVER_DIR)
		   end,
	
	erlide_jrpc:event(?EVENT, ?FINISHED),
	
	{reply, Path, State};


%% cast default
handle_call(_, _From, State) ->
	{reply, bad_option, State}.

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


