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
		 perform/3,
		 prepare_and_perform/3,
		 get_results/0]).

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

%% should be called if no cover-compilation
%% over selected modules were performed (external launching)
prepare_and_perform(Type, Name, Path) ->
	TypeAtom = list_to_atom(Type),
	AName = list_to_atom(Name),
	io:format("prepare ~p ~n", [TypeAtom]),
	gen_server:cast(?MODULE, {prep, TypeAtom, AName, Path}).

%perform coverage (on cover_compiled data - after performing some tests)
perform(Type,Name,Path) ->
    TypeAtom = list_to_atom(Type), 
    AName = list_to_atom(Name), 
    gen_server:cast(?MODULE,{TypeAtom,AName,Path}).


%get results of coverage
get_results() ->
	%get results of coverage, gen_server:call
	ok.

%%----------------------------------------------
%% Local Functions
%%----------------------------------------------

%init
%can be extendnded with the version with multible cover nodes
init(Args) ->
	{ok, _} = cover:start(),
	[Type] = Args,
	State = #state{cover_type = Type},
	{ok, State}.

% cast -> only coverage
handle_cast({module, Module, Src}, State) ->
	io:format("inside cast1~n"),
	Report = coverage:create_report(?COVER_DIR, Module),
	case Report of		
		ok ->
			erlide_jrpc:event(?EVENT, {?OK, Report});
		{error, Reason} ->
			erlide_jrpc:event(?EVENT, ?ERROR)
	end,
	{noreply, State};

handle_cast({application, Name, Src}, State) ->
	io:format("inside cast2~n"),
	{noreply, State};
handle_cast({file, Name, Src}, State) ->
	io:format("inside cast3~n"),
	{noreply, State};
handle_cast({dir, Name, Src}, State) ->
	io:format("inside cast4~n"),
	{noreply, State};

% cast -> covarage with preparation
handle_cast({prep, module, Module, Path}, State) ->
	io:format("inside cast5, ~p ~n", [State#state.cover_type]),
	ok = coverage:prepare(State#state.cover_type, Module, Path),
	Report = coverage:create_report(?COVER_DIR, Module),
	case Report of		
		ok ->
			erlide_jrpc:event(?EVENT, {?OK, Report});
		{error, Reason} ->
			erlide_jrpc:event(?EVENT, ?ERROR)
	end,
	{noreply, State};

handle_cast({prep, application, Name, Src}, State) ->
	io:format("inside cast6~n"),
	{noreply, State};
handle_cast({prep, file, Name, Src}, State) ->
	io:format("inside cast7~n"),
	{noreply, State};
handle_cast({prep, dir, Name, Src}, State) ->
	io:format("inside cast8~n"),
	{noreply, State};
handle_cast(_, State) ->
	io:format("ioioio~n"),
	{noreply, State}.

% call
handle_call(results, _From, State) ->
	Results = new_results,
	{reply, Results, State}.

%terminate
terminate(normal, State) ->
	init:stop(0). %% ?




