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
%%% File    : wrangler_sup.erl
%%% Author  :  <Huiqing Li>
%%% Created : 15 Aug 2008 by  <Huiqing Li>
%%%-------------------------------------------------------------------
-module(wrangler_sup).

-behaviour(supervisor).

%% API
-export([start/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------

init(_Args) ->
    ASTServer = {wrangler_ast_server, {wrangler_ast_server, start_ast_server, []},
		 permanent,100000,worker,[wrangler_ast_server]},
    CallGraphServer={wrangler_callgraph_server, {wrangler_callgraph_server, start_callgraph_server, []},
		     permanent, 10000, worker, [wrangler_callgraph_server]},
    ModuleGraphServer={wrangler_modulegraph_server, {wrangler_modulegraph_server, start_modulegraph_server, []},
		       permanent, 10000, worker, [wrangler_modulegraph_server]},
    ErrorLogger={wrangler_error_logger, {wrangler_error_logger, start_wrangler_error_logger, []},
		 permanent, 10000, worker, [wrangler_error_logger]},
    UndoServer={wrangler_undo_server, {wrangler_undo_server, start_undo_server, []}, 
		permanent, 10000, worker, [wrangler_undo_server]},
    PreviewServer={wrangler_preview_server, {wrangler_preview_server, start_preview_server, []}, 
		   permanent, 10000, worker, [wrangler_preview_server]},
    PreviewServer={wrangler_preview_server, {wrangler_preview_server, start_preview_server, []}, 
		   permanent, 10000, worker, [wrangler_preview_server]},
    {ok,{{one_for_one,3,60}, [ASTServer, CallGraphServer, ModuleGraphServer, ErrorLogger, UndoServer, PreviewServer]}}.

