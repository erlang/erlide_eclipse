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
-module(wrangler_preview_server).

-behaviour(gen_server).

%% API
-export([commit/0, abort/0, add_files/1, start_preview_server/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("../include/wrangler.hrl").

-record(state, {files=[]}).

%%====================================================================
%% API
%%====================================================================
%% Description: Starts the server
%%--------------------------------------------------------------------
start_preview_server() ->
    process_flag(trap_exit, true),
    gen_server:start_link({local, wrangler_preview_server}, ?MODULE, [], []).

-spec(commit/0::() ->
	     {ok, [filename()]}).
commit() ->
    gen_server:call(wrangler_preview_server, commit).

-spec(abort/0::() ->{ok, [filename()]}).

abort() ->
    gen_server:call(wrangler_preview_server, abort).
   
-spec(add_files/1::([filename()]) -> ok).
	     
add_files(Files)->
    gen_server:cast(wrangler_preview_server, {add, Files}).
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
    SwpFiles = lists:flatmap(fun({{F1, F2, IsNew},Swp}) -> 
				 case IsNew of 
				     false -> [Swp];
				     _ -> case F1==F2 of 
					      true ->[F1, Swp];
					      _ -> [F1, F2, Swp]
					  end
				 end
			 end, Files),
    {reply, {ok, SwpFiles}, #state{files=[]}};

handle_call(commit, _From, #state{files=Files}) ->
    OldFiles = lists:map(fun({{F1,F2,IsNew}, _Swp}) -> {F1,F2,IsNew} end, Files),
    FilesToBackup = lists:map(fun({F1, F2, IsNew}) ->
				      {ok, Bin} = file:read_file(F1), {{F1, F2, IsNew}, Bin}
			      end, OldFiles),
    wrangler_undo_server:add_to_history(FilesToBackup),
    lists:foreach(fun({{_F1,F2, _IsNew},Swp}) -> file:copy(Swp, F2) end, Files),
    Files1 = lists:map(fun({{F1,F2, IsNew}, Swp}) -> [F1, F2, Swp, IsNew] end, Files),
    {reply, {ok, Files1}, #state{files=[]}}.
    
%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({add, Files},_State=#state{files=_Files}) ->
    {noreply, #state{files=Files}}.
	
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

