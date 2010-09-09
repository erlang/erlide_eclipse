%% Copyright (c) 2010, Huiqing Li, Simon Thompson
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%     %% Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%     %% Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%     %% Neither the name of the copyright holders nor the
%%       names of its contributors may be used to endorse or promote products
%%       derived from this software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ''AS IS''
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
%% BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR 
%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF 
%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%-------------------------------------------------------------------
%%% File    : wrangler_undo_server.erl
%%% Author  :  <Huiqing>
%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%%%-------------------------------------------------------------------
-module(wrangler_undo_server).

-behaviour(gen_server).

%% API
-export([start_undo_server/0, undo/0, add_to_history/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("../include/wrangler.hrl").

-record(state, {history=[]}).

%%====================================================================
%% API
%%====================================================================
%% Description: Starts the server
%%--------------------------------------------------------------------
start_undo_server() ->
    process_flag(trap_exit, true),
    gen_server:start_link({local, refactor_undo}, ?MODULE, [], []).

%%-spec(undo/0::() ->
%%	     {ok, [filename()]}).
undo() ->
    gen_server:call(refactor_undo, undo).
   
%%-spec(add_to_history/1::([{filename(), filename(), binary()}]) -> ok).
	     
add_to_history({Files, LogMsg, CurFile})->
    gen_server:cast(refactor_undo, {add, {Files, LogMsg, CurFile}}).
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
	[{Files, LogMsg, CurFile}|T] -> 
	    ok = undo_files(Files),
	    Modified = lists:map(fun({{OldFileName, NewFileName,_}, _Con})->
					 [OldFileName, NewFileName] end,Files),
	    {reply,{ok, Modified, LogMsg, CurFile}, #state{history=T}}
    end.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({add, {Files, LogMsg, CurFile}},_State=#state{history=History}) ->
    History1=lists:sublist([{Files, LogMsg, CurFile}|History],20),
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
	[{{OldFileName,NewFileName, IsNew}, Content}|T] -> 
	    case OldFileName == NewFileName of
		true ->
		    case IsNew  of 
			true ->
			    file:delete(NewFileName),
			    undo_files(T);
			_ -> 
			    file:write_file(OldFileName, Content),
			    undo_files(T)
		    end;
		false ->
		    case IsNew of 
			true -> 
			    file:delete(OldFileName),
			    file:delete(NewFileName),
			    undo_files(T);
			_ ->  
			    file:write_file(OldFileName, Content),
			    file:delete(NewFileName),
			    undo_files(T)
		    end
	    end
    end.
