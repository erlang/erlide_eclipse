%% Copyright (c) 2009, Huiqing Li, Simon Thompson
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
%%% File    : wrangler_callgraph_server.erl
%%% Description : A gen server manageing the callgraph of the program under refactoring.
%%% Created : 28 Aug 2008 by  <Huiqing>
%%%-------------------------------------------------------------------
-module(wrangler_callgraph_server).

-behaviour(gen_server).

-include("../include/wrangler.hrl").

%% API
-export([start_callgraph_server/0, get_callgraph/1, get_sccs_including_fun/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {callgraph=[]}).
%%====================================================================
%% API
%%====================================================================

%%-spec(start_callgraph_server/0::() -> {ok, pid()} | ignore | {error, string()}).
start_callgraph_server() ->
    gen_server:start_link({local, wrangler_callgraph_server}, ?MODULE, [], []).

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
%%-spec(init/1::(any()) ->{ok, #state{}}).
init(_Args) ->
    process_flag(trap_exit, true),
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
%%-spec(handle_call/3::({atom(), [dir()]}, any(), #state{}) ->
%%	     {reply, #callgraph{}, #state{}}).
handle_call({get, SearchPaths}, _From, State) ->
    {Reply, State1} = get_callgraph(SearchPaths, State),
    {reply, Reply, State1};
handle_call({get_fun_sccs, MFA, SearchPaths}, _From, State) ->
    {Reply, State1} = get_sccs_including_fun(MFA, SearchPaths, State),
    {reply, Reply, State1}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
%%-spec(handle_cast/2::(any(), #state{}) ->
%%	      {noreply, #state{}}).
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
%%-spec(handle_info/2::(any(), #state{}) ->
%%	      {noreply, #state{}}).
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
%%-spec(terminate/2::(any(), #state{}) -> ok).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
%%-spec(code_change/3::(any(), #state{}, any()) ->
%%	      {ok, #state{}}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%-spec(get_callgraph/1::([dir()])-> #callgraph{}).
get_callgraph(SearchPaths) ->
    gen_server:call(wrangler_callgraph_server, {get, SearchPaths}, infinity).

%%-spec(get_sccs_including_fun/2::({modulename(),functionname(), functionarity()}, [dir()]) -> scc_order()).
get_sccs_including_fun({M, F, A}, SearchPaths) ->    
    gen_server:call(wrangler_callgraph_server, {get_fun_sccs, {M,F, A}, SearchPaths}).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% Currently the State is not used because  it takes too much space;
%% Todo: find out a way to improve this.
get_callgraph(SearchPaths, State) ->
    CallGraph = refac_util:build_scc_callgraph(SearchPaths),
    {CallGraph, State}.


%% return the sccs of which {M, F, A} is a member.
get_sccs_including_fun({M, F, A}, SearchPaths, State) ->
    {#callgraph{scc_order = Sccs}, State1} = get_callgraph(SearchPaths, State),
    ResSccs = lists:filter(fun (Sc) ->
				 lists:any(fun (Elem) ->
						   case Elem of
						       {{M, F, A}, _} -> true;
						       _ -> false
						   end
					   end,
					   Sc)
			 end, Sccs),
    {ResSccs, State1}.
   

    

   
