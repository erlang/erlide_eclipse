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
-module(wrangler_preview_server).

-behaviour(gen_server).

%% API
-export([commit/0, abort/0, add_files/1, start_preview_server/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("../include/wrangler.hrl").

-record(state, {files=[], logmsg=""}).

%%====================================================================
%% API
%%====================================================================
%% Description: Starts the server
%%--------------------------------------------------------------------
start_preview_server() ->
    process_flag(trap_exit, true),
    gen_server:start_link({local, wrangler_preview_server}, ?MODULE, [], []).
 
%%-spec(commit/0::() -> {ok, [filename()]}).
commit() ->
    gen_server:call(wrangler_preview_server, commit).

%%-spec(abort/0::() ->{ok, [filename()]}).
abort() ->
    gen_server:call(wrangler_preview_server, abort).
   
%%-spec(add_files/1::({[filename()], string()}) -> ok).
add_files({Files,LogMsg})->
    gen_server:cast(wrangler_preview_server, {add, {Files, LogMsg}}).
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

handle_call(abort, _From, #state{files=Files}) ->
    SwpFiles = lists:flatmap(fun ({{F1, F2, IsNew}, Swp}) ->
				     case IsNew of
				       false -> [Swp];
				       _ -> case F1 == F2 of
					      true -> [F1, Swp];
					      _ -> [F1, F2, Swp]
					    end
				     end
			     end, Files),
    {reply, {ok, SwpFiles}, #state{files=[], logmsg=""}};

handle_call(commit, _From, #state{files=[], logmsg=_LogMsg}) ->
    {reply, {ok, [], ""}, #state{files=[], logmsg=""}};
    
handle_call(commit, _From, #state{files=Files, logmsg=LogMsg}) ->
    OldFiles = lists:map(fun ({{F1, F2, IsNew}, _Swp}) -> {F1, F2, IsNew} end, Files),
    FilesToBackup = lists:map(fun ({F1, F2, IsNew}) ->
     				      {ok, Bin} = file:read_file(F1), {{F1, F2, IsNew}, Bin}
     			      end, OldFiles),
    wrangler_undo_server:add_to_history({FilesToBackup, LogMsg, element(1, hd(OldFiles))}),
    lists:foreach(fun ({{F1, F2, _IsNew}, Swp}) ->
     			  case file:copy(Swp, F1) of 
     			      {ok,_} -> ok;
     			      Err1 ->
				  throw(Err1)
     			  end,
     			  case F1==F2 of 
     			      true -> ok;
     			      false ->
     				  case file:rename(F1, F2) of 
     				      ok -> ok;
     				      Err2 ->
					  refac_io:format("Rename failed.\n"),
    					  throw(Err2)
     				  end
     			  end
     		  end,Files),
    Files1 = lists:map(fun ({{F1, F2, IsNew}, Swp}) -> [F1, F2, Swp, IsNew] end, Files),
    {reply, {ok, Files1, LogMsg}, #state{files=[], logmsg=""}}.
    


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({add, {Files, LogMsg}},_State=#state{files=_Files, logmsg=_LogMsg}) ->
    {noreply, #state{files=Files, logmsg=LogMsg}}.
	
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
%%-----------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

  
 
