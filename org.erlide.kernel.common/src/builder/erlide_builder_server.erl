%%% ******************************************************************************
%%%  Copyright (c) 2009 Vlad Dumitrescu and others.
%%%  All rights reserved. This program and the accompanying materials
%%%  are made available under the terms of the Eclipse Public License v1.0
%%%  which accompanies this distribution, and is available at
%%%  http://www.eclipse.org/legal/epl-v10.html
%%%
%%%  Contributors:
%%%      Vlad Dumitrescu
%%% ******************************************************************************/
%%% File    : erlide_builder.erl
%%% Author  :  Vlad Dumitrescu
%%% Description :

-module(erlide_builder_server).

-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%%-define(DEBUG, 1).
-include("erlide.hrl").


%% --------------------------------------------------------------------
%% External exports
-export([
		 start/0,
		 stop/0,
		 add_project/2,
		 remove_project/1
		]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {slaves=[]}).

%% ====================================================================
%% External functions
%% ====================================================================

start() ->
	gen_server:start({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:cast(?MODULE, stop).

add_project(Id, Data) ->
	gen_server:call(?MODULE, {add_project, Id, Data}).

remove_project(Id) ->
	gen_server:call(?MODULE, {remove_project, Id}).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init(_) ->
    {ok, #state{}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({add_project, Id, Data}, _From, #state{slaves=Slaves}=State) ->
	{ok, Pid} = erlide_builder_slave:start(Id, Data),
	erlang:monitor(process, Pid),
	case lists:keytake(Id, 1, Slaves) of
		{value, _, _} ->
			{reply, {error, project_exists}, State};
		false ->
			erlide_log:log({builder_server, add_project, Id}),
			{reply, {ok, Pid}, State#state{slaves=[{Id, Pid} | Slaves]}}
	end;
handle_call({remove_project, Id}, _From, #state{slaves=Slaves}=State) ->
    case lists:keytake(Id, 1, Slaves) of
		{value, {Id, Pid}, NewSlaves} ->
			erlide_log:log({builder_server, remove_project, Id}),
			erlide_builder_slave:stop(Pid),
			{reply, ok, State#state{slaves=NewSlaves}};
		false ->
			{reply, {error, no_project}, State}
	end;
handle_call(Request, _From, State) ->
    Reply = {error, unexpected, Request},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(stop, State) ->
	{stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({'DOWN', _Mref, _, Pid, _Info}, #state{slaves=Slaves}=State) ->
	NewSlaves = case lists:keytake(Pid, 2, Slaves) of
					false ->
						Slaves;
					{value, {Id, Pid}, New} ->
						erlide_log:log({builder_slave_died, Id}),
						New
				end,
	{noreply, State#state{slaves=NewSlaves}};
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------



