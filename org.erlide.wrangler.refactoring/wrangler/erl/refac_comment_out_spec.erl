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

%% ============================================================================================
%% Refactoring: Introduce a  macro to a selected expression.
%%
%% Copyright (C) 2006-2008  Huiqing Li, Simon Thompson

%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%% 

-module(refac_comment_out_spec).

-export([comment_out/1]).

comment_out(Dirs) ->
    FileNames = refac_util:expand_files(Dirs, ".erl"),
    HeaderFiles = refac_util:expand_files(Dirs, ".hrl"),
    lists:foreach(fun(F) ->
			  comment_out_spec_type_1(F, Dirs) end,
		  HeaderFiles++FileNames).
    
comment_out_spec_type_1(FileName, SearchPaths) ->
    refac_io:format("Current file being processed:\n~p\n", [FileName]),
    {ok, {AnnAST, _Info}} = refac_util:parse_annotate_file(FileName,  true, SearchPaths),
    Fs = refac_syntax:form_list_elements(AnnAST), 
	Str = vertical_concat(Fs,  ""),
	file:write_file(FileName, list_to_binary(Str)).


vertical_concat([], Acc) -> Acc;
vertical_concat([F|T],Acc) ->
    Toks = case refac_syntax:type(F) of 
	       attribute ->
		   case refac_syntax:atom_value(refac_syntax:attribute_name(F)) of 
		       type ->
			   Toks1 = refac_util:get_toks(F),
			   turn_to_comments(Toks1);		       
 		       spec ->
			   Toks1 = refac_util:get_toks(F),
			   turn_to_comments(Toks1);
		       _-> refac_util:get_toks(F)
		   end;	    
	       _ ->
		   refac_util:get_toks(F)
	   end,
    vertical_concat(T, Acc ++refac_util:concat_toks(Toks)).

turn_to_comments(Toks) ->
    {Toks1, Toks2} = lists:splitwith(fun(T) ->
					     case T of 
						 {'-', _} ->
						     false;
						 _ -> true
					     end
				     end, Toks),
    Toks1 ++ [{comment, {0, 0}, "%%"}|  turn_to_comments_1(Toks2)].
turn_to_comments_1([]) -> [];
turn_to_comments_1([T]) ->[T];
turn_to_comments_1([T|Ts]) ->
    case T of 
	{whitespace, {L,_C}, '\n'} ->
	    [T, {comment, {L+1,0}, "%%"} | turn_to_comments_1(Ts)];
	_ -> [T|turn_to_comments_1(Ts)]
    end.
    
    
    
 
