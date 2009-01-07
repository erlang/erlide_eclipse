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
-export([module_graph/1, collect_called_modules/1]). 

-include("../hrl/wrangler.hrl").
-spec(module_graph/1::([dir()]) -> [{filename(), [filename()]}]).
module_graph(SearchPaths) ->
    ?wrangler_io("\nStart calculating module graph ...\n",[]),
    Files = refac_util:expand_files(SearchPaths, ".erl"),
    ModMap = refac_util:get_modules_by_file(Files),
    Res =analyze_all_files(ModMap, SearchPaths),
    ?wrangler_io("\nModule graph calculation finished.\n",[]),
    Res.
    
    
analyze_all_files([], _SearchPaths)->
    Acc = ets:foldr(fun({{Mod, Dir}, CalledMods, _TimeStamp}, S) 
		       -> [{{Mod, Dir}, CalledMods}|S]
		    end, [], ?ModuleGraphTab),
    reverse_module_graph(Acc);

analyze_all_files([{Mod, Dir}|Left], SearchPaths) ->  
    FileName = filename:join(Dir,Mod++".erl"),
    FileModifiedTime = filelib:last_modified(FileName),
    R = ets:lookup(?ModuleGraphTab, {Mod, Dir}),
    case R of
	[] -> {called_modules, Called} = analyze_mod({Mod, Dir}, SearchPaths),
	      ets:insert(?ModuleGraphTab, {{Mod, Dir}, Called, filelib:last_modified(FileName)}),
	      analyze_all_files(Left, SearchPaths);
	[{{Mod, Dir}, _CalledMods, TimeStamp}] ->
	    case FileModifiedTime > TimeStamp of 
		true -> 
		    ets:delete(?ModuleGraphTab, {Mod, Dir}),
		    {called_modules, CalledMods1} = analyze_mod({Mod, Dir}, SearchPaths),
		    ets:insert(?ModuleGraphTab, {{Mod, Dir}, CalledMods1, filelib:last_modified(FileName)}),
		    analyze_all_files(Left, SearchPaths);
		false  ->
		    analyze_all_files(Left, SearchPaths)		   
	    end
  end.
		  

analyze_mod({Mod, Dir}, SearchPaths) ->
    DefaultIncl1 = [".","..", "../hrl", "../incl", "../inc", "../include"],
    DefaultIncl2 = [filename:join(Dir, X) || X <- DefaultIncl1],
    Includes = SearchPaths ++ DefaultIncl2, 
    File = filename:join(Dir, Mod++".erl"),
    {ok, {AnnAST, Info}} =refac_util:parse_annotate_file(File, true, Includes),
    ImportedMods = case lists:keysearch(imports,1, Info) of 
		       {value, {imports, Imps}} -> lists:map(fun({M, _Funs}) -> M end, Imps);
		       false  -> []
		   end,
    CalledMods = collect_called_modules(AnnAST),
    {called_modules, ImportedMods++CalledMods}.

-spec(collect_called_modules(AnnAST::syntaxTree()) ->
	     [modulename()]).
collect_called_modules(AnnAST) ->
    Fun = fun(T, S) ->
		  case refac_syntax:type(T) of 
		      application ->
			  Op = refac_syntax:application_operator(T),
			  case lists:keysearch(fun_def, 1, refac_syntax:get_ann(Op)) of 
			      {value, {fun_def, {M, F, A, _, _}}} ->
				S1 = ordsets:add_element(M, S),
				SpecialFuns1 = [{erlang,apply,3},{erlang, spawn,3},{erlang, spawn_link,3}, 
						{erlang, spawn_monitor, 3}, {erlang, spawn_opt, 4}],
				SpecialFuns2 = [{erlang, spawn, 4}, {erlang,spawn_link, 4}, {erlang, spawn_opt, 5}],
				case lists:member({M, F, A}, SpecialFuns1) of 
				    true ->
					Args = refac_syntax:application_arguments(T),
					M1 = hd(Args),
					case refac_util:try_evaluation([refac_syntax:revert(M1)]) of
					    {value, M} -> ordsets:add_element(M, S1);
					    _ -> S1
					end;
				    false ->
					case lists:member({M, F, A}, SpecialFuns2) of 
					    true ->
						Args = refac_syntax:application_arguments(T),
						M1 = element(2,list_to_tuple(Args)),
						case refac_util:try_evaluation([refac_syntax:revert(M1)]) of
						    {value, M2} -> ordsets:add_element(M2, S1);
						    _ -> S1
						end;
					    _ -> S1
					end
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



    
