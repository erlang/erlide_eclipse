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
%%% File    : wrangler_error_logger.erl
%%% Author  :  <Huiqing Li>
%%% Description : 
%%%
%%% Created : 31 Aug 2008 by  <Huiqing Li>
%%%-------------------------------------------------------------------
-module(wrangler_error_logger).

-behaviour(gen_server).

%% API
-export([start_wrangler_error_logger/0,
	 get_logged_info/0, add_to_logger/1,
	 remove_from_logger/1, remove_all_from_logger/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {errors=[]}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_wrangler_error_logger() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_wrangler_error_logger() ->
    gen_server:start_link({local, wrangler_error_logger}, ?MODULE, [], []).

get_logged_info() ->
    gen_server:call(wrangler_error_logger, get_errors).

add_to_logger(Error) ->
    gen_server:cast(wrangler_error_logger, {add, Error}).

remove_from_logger(FileName) ->
    gen_server:cast(wrangler_error_logger, {remove, FileName}).

remove_all_from_logger() ->
    gen_server:cast(wrangler_error_logger, remove_all).    
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
%% this function also reset the state of the error logger.
handle_call(get_errors, _From, _State=#state{errors=Errors}) ->
    {reply, Errors,  #state{errors=[]}}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({add, {warning, Msg}}, _State = #state{errors = Errors}) ->
    case lists:keysearch(warning, 1, Errors) of
	{value, {warning, Str}} ->
	    NewWarnMsg=Str++"\n"++Msg,
	    NewErrors =lists:keyreplace(warning, 1, Errors, {warning, NewWarnMsg}),
	    {noreply, #state{errors=NewErrors}};
	false ->
	    {noreply, #state{errors=[{warning, Msg}|Errors]}}
    end;
handle_cast({add, {FileName, Error}}, _State = #state{errors = Errors}) ->
    {noreply, #state{errors = [{FileName, Error}| lists:keydelete(FileName, 1, Errors)]}};

handle_cast({remove, FileName}, _State = #state{errors = Errors}) ->
    {noreply, #state{errors = lists:keydelete(FileName, 1, Errors)}};
handle_cast(remove_all, _State = #state{errors = _Errors}) ->
    {noreply, #state{errors = []}}.


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
