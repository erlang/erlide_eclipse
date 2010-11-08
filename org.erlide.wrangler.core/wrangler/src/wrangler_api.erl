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
%% =====================================================================
%% Wrangler API.
%%
%% Author contact: H.Li@kent.ac.uk, Simon.J.Thompson@kent.ac.uk
%%
%% =====================================================================

%% @copyright 2006-2011 Huiqing Li, Simon Thompson
%%
%% @author Huiqing Li, Simon Thompson
%%   [http://www.cs.kent.ac.uk/projects/wrangler]

%% @version 0.1
%% @end
%%
%% @doc This module describes the refactoring commands that can be run in an Erlang shell.
%% <p>All refactorings commands should be run in the context of a Wrangler application. 
%% Use wrangler_api:start() to start a Wrangler application, and wrangler_api:stop() the 
%% application.
%% </p>
%% <p>
%% The Wrangler API is not completed yet, more API functions will be added.
%% </p>

-module(wrangler_api).

-export([rename_mod/3, rename_fun/5, move_fun/5, similar_code/7]).

-export([start/0, stop/0, undo/0]).

-include("../include/wrangler.hrl").

%% @type(modulename()::atom()).
%% @type(filename()::string()).
%% @type(arity()::integer()).


%% @doc Start a Wrangler application.
%%@spec start() -> {ok, Pid}|{error, Reason}
start() ->
    application:start(wrangler_app).

%% @doc Stop a Wrangler application.
%%@spec stop()-> ok
stop() ->
    application:stop(wrangler_app).

%% @doc Undo the previous refactoring. This only works within a Wrangler application.
%%@spec undo()-> {ok, FilesChanged::[filename()]}|{error, Reason}
undo() ->
    wrangler_undo_server:undo().

%%===================================================================================
%% @doc Rename a module.
%% <p> This refactoring affects all those modules in which the module name is used, and returns either ok with the list of files affected by this 
%%     refactoring, or an error message. </p>
%%@spec rename_mod(ModorFileName::modulename()|filename(), NewModName::modulename(), SearchPaths::[dir()]) -> 
%%			   {ok, FilesChanged::[filename()]}|{error,Reason}
rename_mod(ModOrFileName, NewModName, SearchPaths) ->
    try_apply(refac_rename_mod, rename_mod_command, [ModOrFileName, NewModName, SearchPaths]).



%%===================================================================================
%% @doc Rename a function.
%% <p> This refactoring affects all those modules in which the function renamed is 
%%     used,  and returns either ok with the list of files affected by this 
%%     refactoring, or an error message. 
%%</p>
%%@spec rename_fun(ModOrFileName::modulename()|filename(), atom(), integer(), atom(),[dir()])->
%%				       {ok, FilesChanged::[filename()]}|{error,Reason}
rename_fun(ModOrFileName, FunName, Arity, NewFunName, SearchPaths) ->
    try_apply(refac_rename_fun, rename_fun_command, 
	      [ModOrFileName, FunName, Arity, NewFunName, SearchPaths]).


%%===================================================================================
%%@doc Move a function to another module.
%% <p> This refactoring moves the function specified, together with functions that are only used 
%%     by the function to be moved, to the target module. This refactorings affects all those 
%%     modules in which the function to be moved is used, and returns either ok with the list of 
%%     files affected by this refactoring, or an error message.
%%</p>
%%@spec move_fun(FromModOrFileName::modulename()|filename(), FunName::atom(), Arity::integer(), 
%%		       ToModOrFileName::modulename()|filename(),SearchPaths::[dir()])->
%%				  {ok, FilesChanged::[filename()]}|{error, Reason}
move_fun(FromModOrFileName, FunName, Arity, ToModOrFileName, SearchPaths) ->
    try_apply(refac_move_fun, move_fun_command, 
	      [FromModOrFileName, FunName, Arity, ToModOrFileName, SearchPaths]).


%%===================================================================================
%%@doc Similar code detection.
%% <p> A similar code detector which takes an Erlang project (or just a collection of Erlang files) and
%%     a set of parameters as input, performs clone detection, and reports the clone classes found.
%%     Five parameters, which are of a conjunction relation, can be specified so that the user can 
%%     have control of the granularity of the clone classes reported. These parameters are: <br />
%%     <span style="padding-left:20px">MinLen: the minimum number of expressions included in a code clone which is a sequence of expressions;</span> <br />
%%     <span style="padding-left:20px">MinToks: the minimum number of tokens included in a code clone (minimum value: 20);</span><br />
%%     <span style="padding-left:20px">MinFreq: the minimum number of duplications of a cloned code fragment (minimum value: 2);</span><br />
%%     <span style="padding-left:20px">MaxVars: the maximum number of new parameters of the least-general common abstraction function;</span><br />
%%     <span style="padding-left:20px">SimiScore: the similarity score threshold which is between 0.1 and 1.0. </span>
%% </p>
%%@spec similar_code(DirFileList::[filename()|dir()], MinLen::integer(), MinToks::integer(),
%% 				     MinFreq::integer(),  MaxVars::integer(),SimiScore::float(), 
%% 				     SearchPaths::[dir()]) -> {ok, string()}|{error, Reason}
similar_code(DirFileList,MinLen,MinToks,MinFreq,MaxVars,SimiScore,SearchPaths) ->
    try_apply(refac_inc_sim_code, inc_sim_code_detection_command,
	      [DirFileList,MinLen,MinToks,MinFreq,MaxVars,SimiScore,SearchPaths,8]).



try_apply(Mod, Fun, Args) -> 
    try apply(Mod, Fun, Args)
    catch
	throw:Error -> 
	    Error;   
	exit:Reason->
	    {'EXIT',Reason};
	error:Reason -> 
	    {'EXIT',{Reason,erlang:get_stacktrace()}}
    end.
