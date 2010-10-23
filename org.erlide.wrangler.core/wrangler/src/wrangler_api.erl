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
%% @doc This module describes the API functions currently supported by Wrangler.
%%      All these functions can be used in command line.
%%
-module(wrangler_api).

-export([rename_mod/3]).

-export([start/0, stop/0, undo/0]).

-include("../include/wrangler.hrl").

%% All the API functions should be able to used without starting the Wrangler application. 
%% But when you run these API functions in an Erlang shell, do it within the Wrangler 
%% application is faster, and will allow you to undo refactorings if you change your mind.
 
start() ->
    application:start(wrangler_app).

stop() ->
    application:stop(wrangler_app).

undo() ->
    wrangler_undo_server:undo().

%%===================================================================================
%% @doc Rename a module.
%% <p> This refactoring affects all those modules in which the module name is used.
%%     This function returns either ok with the list of files afftected by this 
%%     refactoring, or an error message. </p>
-spec(rename_mod/3::(modulename()|filename(), modulename(), [dir()]) -> 
	     {error, string()} | {ok, [filename()]}).
rename_mod(OldFileOrModName, NewModName, SearchPaths) ->
    try_apply(refac_rename_mod, rename_mod, [OldFileOrModName, NewModName, SearchPaths]).


try_apply(Mod, Fun, Args) -> 
    try apply(Mod, Fun, Args)
    catch
	throw:Error -> 
	    Error;   
	E1:E2->
	    {E1, E2}
    end.
