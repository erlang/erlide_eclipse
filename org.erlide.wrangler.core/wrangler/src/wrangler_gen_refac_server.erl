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
%%%@private
%%%@hidden
-module(wrangler_gen_refac_server).

-behaviour(gen_server).

%% API
-export([set_flag/1, 
         get_flag/1, 
         delete_flag/1,
         add_change_cand/2,
         get_change_set/1,
         delete_change_set/1,
         start_gen_refac_server/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("../include/wrangler_internal.hrl").

-record(state, {flags=[], change_set=[]}).

%%====================================================================
%% API
%%====================================================================
%% Description: Starts the server
%%--------------------------------------------------------------------
start_gen_refac_server() ->
    process_flag(trap_exit, true),
    gen_server:start_link({local, wrangler_gen_refac_server}, ?MODULE, [], []).

set_flag({Pid, Selective}) ->
    case Selective of
        false ->
            gen_server:cast(wrangler_gen_refac_server, {add, {Pid, {Selective,[]}}});
        _ ->
            gen_server:cast(wrangler_gen_refac_server, {add, {Pid, Selective}})
    end.
get_flag(Pid) ->
    gen_server:call(wrangler_gen_refac_server, {get, Pid}).

delete_flag(Pid) ->
    gen_server:cast(wrangler_gen_refac_server, {delete, Pid}).

add_change_cand(Pid, ChangeCand) ->
    gen_server:cast(wrangler_gen_refac_server, {add_change_cand, {Pid, ChangeCand}}).

get_change_set(Pid) ->
     gen_server:call(wrangler_gen_refac_server, {get_change_set, Pid}).

delete_change_set(Pid) ->
    gen_server:cast(wrangler_gen_refac_server, {delete_change_set, Pid}).

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

handle_call({get,Pid}, _From, State=#state{flags=Flags}) ->
    case lists:keyfind(Pid,1, Flags) of 
        {Pid, Flag} ->
            {reply, {ok, Flag}, State};
        false ->
            {reply, {ok, {false,[]}}, State}
    end;
handle_call({get_change_set,Pid}, _From, State=#state{change_set=ChangeSet}) ->
    case lists:keyfind(Pid, 1,ChangeSet) of
        {Pid, Cands} ->
            {reply, {ok, lists:reverse(Cands)}, State};
        false ->
            {reply, {ok, []}, State}
    end.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({add, {Pid, Selective}},_State=#state{flags=Flags}) ->
    {noreply, #state{flags=[{Pid, Selective}|Flags]}};
handle_cast({add_change_cand, {Pid, ChangeCand}}, _State=#state{change_set=ChangeSet}) ->
    case lists:keyfind(Pid, 1, ChangeSet) of
        {Pid, Cand} ->
            NewCand = [ChangeCand|Cand],
            {noreply, #state{change_set=lists:keyreplace(Pid, 1, ChangeSet, {Pid, NewCand})}};
        false ->
            {noreply, #state{change_set=[{Pid, [ChangeCand]}|ChangeSet]}}
    end;
handle_cast({delete, Pid},_State=#state{flags=Flags}) ->
    {noreply, #state{flags=lists:keydelete(Pid, 1, Flags)}};
handle_cast({delete_change_set, Pid},_State=#state{change_set=ChangeSet}) ->
    {noreply, #state{change_set=lists:keydelete(Pid, 1, ChangeSet)}}.

%%-------------------------------------------------------------------
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

  
 
