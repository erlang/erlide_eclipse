%%%-------------------------------------------------------------------
%%% File    : wrangler_undo_server.erl
%%% Author  :  <Huiqing>
%%
%% Copyright (C) 2006-2009  Huiqing Li, Simon Thompson


%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.

%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.

%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%%%-------------------------------------------------------------------
-module(wrangler_undo_server).

-behaviour(gen_server).

%% API
-export([start_undo_server/0, undo/0, add_to_history/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("../hrl/wrangler.hrl").

-record(state, {history=[]}).

%%====================================================================
%% API
%%====================================================================
%% Description: Starts the server
%%--------------------------------------------------------------------
start_undo_server() ->
    process_flag(trap_exit, true),
    gen_server:start_link({local, refactor_undo}, ?MODULE, [], []).

-spec(undo/0::() ->
	     {ok, [filename()]}).
undo() ->
    gen_server:call(refactor_undo, undo).
   
-spec(add_to_history/1::([{filename(), filename(), binary()}]) -> ok).
	     
add_to_history(Files)->
    gen_server:cast(refactor_undo, {add, Files}).
%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(undo, _From, State=#state{history=History}) ->
    case History of 
	[] ->
	    {reply, {error, "No more history to undo!"}, State};
	[H|T] -> 
	    ok = undo_files(H),
	    Modified = lists:map(fun({{OldFileName, NewFileName}, _Con})->
					 [OldFileName, NewFileName] end,H),
	    {reply,{ok, Modified}, #state{history=T}}
    end.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({add, Files},_State=#state{history=History}) ->
    History1=lists:sublist([Files|History],20),
    {noreply, #state{history=History1}}.
	
%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
undo_files(Files) -> 
    case Files of 
	[] ->
	    ok;
	[{{OldFileName,NewFileName}, Content}|T] -> 
	    case OldFileName == NewFileName of
		true ->  file:write_file(OldFileName, Content),
			 undo_files(T);
		false -> file:write_file(OldFileName, Content),
			 file:delete(NewFileName),
			 undo_files(T)
	    end
    end.
