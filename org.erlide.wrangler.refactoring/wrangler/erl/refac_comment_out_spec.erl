%% ============================================================================================
%% Refactoring: Introduce a  macro to a selected expression.
%%
%% Copyright (C) 2006-2008  Huiqing Li, Simon Thompson

%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.

%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.

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
    io:format("Current file being processed:\n~p\n", [FileName]),
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
    
    
    
 
