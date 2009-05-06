%%%-------------------------------------------------------------------
%%% File    : wrangler_sup.erl
%%% Author  :  <Huiqing Li>
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
%%%
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

%%====================================================================
%% Internal functions
%%====================================================================
