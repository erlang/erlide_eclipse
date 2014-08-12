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

-module(erlide_builder_slave).

-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%%-define(DEBUG, 1).
-include("erlide.hrl").


%% --------------------------------------------------------------------
%% External exports
-export([
		 start/2,
		 stop/1,
		 build_full/1,
		 build_incremental/2,
		 clean/1
		]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {id, data}).

%% ====================================================================
%% External functions
%% ====================================================================

start(Id, Configuration) ->
	gen_server:start(?MODULE, [Id, Configuration], []).

stop(Pid) ->
	gen_server:cast(Pid, stop).

build_full(Pid) ->
	gen_server:call(Pid, full_build).

build_incremental(Pid, Delta) ->
	gen_server:call(Pid, {incremental_build, Delta}).

clean(Pid) ->
	gen_server:call(Pid, clean).


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
init([Id, Data]) ->
	erlide_log:log({builder_slave, started, Id}),
    {ok, #state{id=Id, data=Data}}.

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
handle_cast(stop, #state{id=Id}=State) ->
	erlide_log:log({builder_slave, stopped, Id}),
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




