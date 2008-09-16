%% =====================================================================
%% Module-level call graph construction.
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
%%======================================================================

-module(refac_module_graph).
-export([module_graph/3]). 

-include("../hrl/wrangler.hrl").

module_graph(Files, ModuleGraphFile, SearchPaths) ->
     Ext = ".erl",
     NewFiles = refac_util:expand_files(Files, Ext),
     ModMap = refac_util:get_modules_by_file(NewFiles),
     case file:read_file(ModuleGraphFile) of 
	 {ok, Res} -> {module_graph, _, List} = analyze_all_files(ModMap, [], {ModuleGraphFile, binary_to_term(Res)}, SearchPaths),
		      List;
	 _ -> {module_graph, _, List} = analyze_all_files(ModMap, [], {ModuleGraphFile, []},SearchPaths),
	      List
     end.
	     

analyze_all_files([{Mod, Dir}|Left], Acc, {ModuleGraphFile, ModuleGraph}, SearchPaths) ->  
  ModuleGraphModifiedTime = filelib:last_modified(ModuleGraphFile),
  FileName = filename:join(Dir,Mod++".erl"),
  FileModifiedTime = filelib:last_modified(FileName),
  R = lists:keysearch({Mod, Dir},1, ModuleGraph),
  if (FileModifiedTime < ModuleGraphModifiedTime) and (R /= false) ->
	  {value, R1} = R,
	  analyze_all_files(Left, [R1|Acc], {ModuleGraphFile, ModuleGraph}, SearchPaths);
     true ->
	  case analyze_mod({Mod, Dir}, SearchPaths) of
	      {error, Reason} ->
		  erlang:error(Reason);
		  %% analyze_all_files(Left, Acc, {ModuleGraphFile, ModuleGraph}, SearchPaths);
	      {called_modules, Called} ->
		  analyze_all_files(Left, [{{Mod,Dir}, Called}|lists:keydelete({Mod, Dir}, 1, Acc)],
				    {ModuleGraphFile, ModuleGraph}, SearchPaths)
	  end
  end; 	   
  
analyze_all_files([], Acc, {ModuleGraphFile, _ModuleGraph}, _SearchPaths)->
    case file:open(ModuleGraphFile,[write,binary]) of 
	{ok, File} -> file:write_file(ModuleGraphFile, term_to_binary(Acc)),
		      file:close(File);
	{error, Reason} ->  io:format("Could not open the module graph output file, Reason ~p\n",
				      [Reason])
    end,
    {module_graph, Acc, reverse_module_graph(Acc)}.


analyze_mod({Mod, Dir}, SearchPaths) ->
    DefaultIncl1 = [".","..", "../hrl", "../incl", "../inc", "../include"],
    DefaultIncl2 = [filename:join(Dir, X) || X <- DefaultIncl1],
    Includes = SearchPaths ++ DefaultIncl2, 
    File = filename:join(Dir, Mod++".erl"),
    case refac_util:parse_annotate_file(File, false, Includes) of 
	{ok, {AnnAST, Info}} ->
	    ImportedMods = case lists:keysearch(imports,1, Info) of 
			       {value, {imports, Imps}} -> lists:map(fun({M, _Funs}) -> M end, Imps);
			       _  -> []
			   end,
	    CalledMods = collect_called_modules(AnnAST),
	    {called_modules, ImportedMods++CalledMods};
	{error, Reason} -> 
	    {error, Reason}
    end.


collect_called_modules(AnnAST) ->
    Fun = fun(T, S) -> 
 		 case refac_syntax:type(T) of 
 		     application -> 
 			 Operator = refac_syntax:application_operator(T),
 			 case refac_syntax:type(Operator) of 
			     module_qualifier ->
				 Mod  = refac_syntax:module_qualifier_argument(Operator),
				 case refac_syntax:type(Mod) of 
				     atom ->  ordsets:add_element(refac_syntax:atom_value(Mod), S);
				     _ ->  S 
				 end;
			     atom -> 
				 Op = refac_syntax:atom_value(Operator),
				 Arguments = refac_syntax:application_arguments(T),
				 Arity = length(Arguments),
				 SpecialFuns = [{apply,3},{spawn,3},{spawn,4},{spawn_link,3},{spawn_link,4}],
				 case lists:member({Op, Arity},SpecialFuns) of 
				     true ->
					 Mod = hd(Arguments),
					 Mod1 = refac_util:try_evaluation([refac_syntax:revert(Mod)]),
					 case Mod1 of 
					     {value, M} -> ordsets:add_element(M, S);
					     _ -> S
					 end;
				     _  -> S
				 end;	 
			     _ -> S
			 end;
		     _ -> S
		 end
	 end,
     lists:usort(refac_syntax_lib:fold(Fun, [], AnnAST)).


reverse_module_graph(List) ->
    reverse_module_graph_1(List,List, []).
reverse_module_graph_1([], _List,Acc) ->
    Acc;
reverse_module_graph_1([{{Mod, Dir},_Called_Mods}|T], List, Acc) ->
    Client_Modules = get_client_modules({Mod,Dir}, List),
    reverse_module_graph_1(T,List, [Client_Modules|Acc]).

get_client_modules({Mod,Dir}, List) ->
    F = fun({{M,Dir1},Called_Modules}) ->
		case lists:member(list_to_atom(Mod), Called_Modules) of 
		    true -> case filelib:is_file(filename:join(Dir1, Mod++".erl")) andalso (Dir =/=Dir1) of 
				true -> 
				    [];
				_ ->[filename:join([Dir1, M++".erl"])]
			    end;
		    false -> []
		end
	end,
    {filename:join([Dir, Mod++".erl"]), lists:flatmap(F, List)}.



    
