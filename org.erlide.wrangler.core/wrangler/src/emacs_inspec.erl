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

%% @hidden
%% @private
-module(emacs_inspec).

-include("../include/wrangler.hrl").

-export([apply_code_inspection/1, input_par_prompts/2]).

%%@doc The function called by Emacs to invoke a code inspection function. All code 
%% inspection function should have an arity of 0. 
%%@private
apply_code_inspection(Args=[ModName, FunName, CurFileName, 
                            UserInputs, SearchPaths, TabWidth]) ->
    ?wrangler_io("\nCMD: ~p:apply_code_inspection(~p).\n",
                 [?MODULE, Args]),
    M = if is_list(ModName) ->
                list_to_atom(ModName);
           true ->
                ModName
        end,
    F = if is_list(FunName) ->
                list_to_atom(FunName);
           true ->
                FunName
        end,
    Args0= #args{current_file_name=CurFileName,
                 user_inputs=UserInputs,
                 search_paths = SearchPaths,
                 tabwidth = TabWidth},
    try apply(M, F, [Args0]) of
        Res -> 
            display_search_results(Res),
            {ok, "Code inspection finished."}         
    catch
        throw:Error -> 
            Error;   
        _E1:E2->
            {error, {E2, lists:flatten(io_lib:format("~p", [erlang:get_stacktrace()]))}}
    end.

input_par_prompts(ModName, FunName) ->
    M = if is_list(ModName) ->
                list_to_atom(ModName);
           true ->
                ModName
        end,
    F = if is_list(FunName) ->
                list_to_atom(FunName);
           true ->
                FunName
        end,
    {ok, M:F(input_par_prompts)}.

%%@private
display_search_results(Res) ->
    %% case lists:all(fun(R) ->
    %%                        case R of  
    %%                            {File, {{StartLn, StartCol}, {EndLn, EndCol}}} -> 
    %%                                  filelib:is_regular(File) andalso 
    %%                                  is_integer(StartLn) andalso 
    %%                                  is_integer(StartCol) andalso 
    %%                                  is_integer(EndLn) andalso 
    %%                                  is_integer(EndCol);
    %%                                  _ -> 
    %%                                false
    %%                        end
    %%                end, Res) of 
    %%     true ->
            display_search_results_1(Res).
     %%    _ ->
    %%         ?wrangler_io("Code inspection result:\n~p\n", [Res])
    %% end.
                
display_search_results_1(Ranges) ->
    case Ranges of
	[] -> 
	    ?wrangler_io("\nNo code fragments satisfying the conditions have been found.\n", []),
	    {ok, Ranges};
	_ -> 
            Num = length(Ranges),
            if Num==1 ->
                    ?wrangler_io("\n~p code fragment satisfying the conditions has been found. \n", [Num]);
               true ->
                    ?wrangler_io("\n~p code fragments satisfying the conditions have been found. \n", [Num])
            end,
	    ?wrangler_io(compose_search_result_info(Ranges), []),
	    ?wrangler_io("\n\nNOTE: Use 'M-x compilation-minor-mode' to make the result "
			 "mouse clickable if this mode is not already enabled.\n",[]),
	    ?wrangler_io("      Use 'C-c C-w e' to remove highlights!\n", []),
	    {ok, Ranges}
    end.

compose_search_result_info(Ranges) ->
    compose_search_result_info(Ranges, "").
compose_search_result_info([], Str) ->
    Str;
compose_search_result_info([{FileName, {{StartLine, StartCol}, {EndLine, EndCol}}}|Ranges], Str) ->
    Str1 =Str ++ "\n"++FileName++io_lib:format(":~p.~p-~p.~p: ", [StartLine, StartCol, EndLine, EndCol]),
    compose_search_result_info(Ranges, Str1);
compose_search_result_info([OtherFormat|Ranges], Str) ->
    Str1 = Str ++ "\n" ++ lists:flatten(io_lib:format("~p", [OtherFormat])),
    compose_search_result_info(Ranges, Str1).

