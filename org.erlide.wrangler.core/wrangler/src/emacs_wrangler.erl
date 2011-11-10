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
%% Some Interface Functions to Emacs
%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%% 
%% =====================================================================

%% @private
%% @hidden
-module(emacs_wrangler).    

-include("../include/wrangler_internal.hrl").

-export([apply_refac/3, apply_refac_1/3]).

apply_refac(Fun, Args, SearchPaths) ->
    case initial_checking(SearchPaths) of
        ok ->
            Res = apply(wrangler_refacs, Fun, Args),
            output_logged_msg(),
            Res;
        {error, Reason} -> {error, Reason}
    end.

apply_refac_1(Fun, Args, SearchPaths) ->
    case check_searchpaths(SearchPaths) of
        ok ->
            apply(wrangler_refacs, Fun, Args);
        {error, Reason} -> {error, Reason}
    end.


initial_checking(SearchPaths1) ->
    case check_searchpaths(SearchPaths1) of
      ok ->
	  check_undo_process();
      {error, Reason} -> {error, Reason}
    end.

check_searchpaths(SearchPaths) ->
    InValidSearchPaths = lists:filter(fun (X) -> not filelib:is_dir(X)  andalso 
                                      not filelib:is_regular(X) end, SearchPaths),
    case InValidSearchPaths of
	[] -> ok;
	_ -> ?wrangler_io("\n===============================WARNING===============================\n",[]), 
	     ?wrangler_io("The following directories specified in the search paths do not exist:\n~s", [InValidSearchPaths]),
	     {error, "Some directories specified in the search paths do not exist!"}
    end.

			  
check_undo_process() ->
    case erlang:whereis(wrangler_undo_server) of
	undefined ->
	    {error, "The Wrangler application is not started, or is not working properly, please restart the refactorer!"};
	_ ->
	    ok
    end.

output_logged_msg() ->
    _Msg = wrangler_refacs:get_log_msg(),
    ?wrangler_io(_Msg, []).
