%% =====================================================================
%% Refactoring: Rename a  collection of module names in batch mode.
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
%% =====================================================================
%%
%% @doc Rename a collect of module names in batch mode. 
%% <p> This refactoring is supposed to be run from the Erlang shell. For example, 
%% to rename all those module names which match the regular expression "foo_*" to 
%% "foo_*_1_0" in the directory <code> c:/distel/test </code>, just type the following command:
%% <code> refac_batch_rename_mod:batch_rename_mod("foo_*, "foo_*_1_0", ["c:/distel/test"]) </code>.
%% </p>
%% <p> This refactoring has a global effect. </p>
%% <p> The following <em> side-conditions </em> apply to this refactoring:
%% <li> The new module names should not conflict with each other, or any existing module names 
%% in the same scope which will not be renamed. </li>
%% <li> This refactorings assumes that the file basename is always the same as the module name. </li>
%% </p>
%% @end
%%%%
%% =====================================================================

%% @TODO: Add side-condition checkings.
-module(refac_batch_rename_mod).

-export([batch_rename_mod/3]).

-include("../hrl/wrangler.hrl").

%% =====================================================================
%% @spec batch_rename_mod(OldNamePattern::string(), NewNamePattern::string(), 
%%                        SearchPaths::[string()])-> ok | {error, string()}
%%   

-spec(batch_rename_mod/3::(string(), string(), [dir()])-> {ok, string()} | {error, string()}).
batch_rename_mod(OldNamePattern, NewNamePattern,SearchPaths) ->
    ?wrangler_io("\n[CMD: batch_rename_mod, ~p, ~p, ~p]\n", [OldNamePattern, NewNamePattern, SearchPaths]),
    %% Get all the erlang file which will be affected by this refactoring.
    Files = lists:append([[filename:join([Pwd,File])||
			      File <- filelib:wildcard("*.erl", Pwd)]   
			  || Pwd <- SearchPaths]),
    Mods = lists:map(fun({M, _Dir}) -> M end, refac_util:get_modules_by_file(Files)),
    ?wrangler_io("Mods:\n~p\n", [Mods]),
    Old_New_Mod_Names = lists:map(fun(M) -> {list_to_atom(M), 
					     list_to_atom(get_new_name(M, OldNamePattern, NewNamePattern))} end, Mods),
    ?wrangler_io("Old_New_Mod_Names:\n~p\n", [Old_New_Mod_Names]),
    HeaderFiles = refac_util:expand_files(SearchPaths, ".hrl"),
    %% Refactor both .erl and .hrl files, but does not change .hrl file name.
    Results = batch_rename_mod(Files++HeaderFiles, Old_New_Mod_Names),  
    refac_util:write_refactored_files(Results),
    ChangedFiles = lists:map(fun({{F, _F}, _AST}) -> F end, Results),
    ?wrangler_io("\n The following files have been changed by this refactoring:\n~p\n", [ChangedFiles]),
    {ok, "Refactoring finished."}.
    %%{ok, ChangedFiles}.


batch_rename_mod(Files, Old_New_Mod_Names) ->
    case Files of 
	[] -> [];
	[F|Fs] ->  
	    ?wrangler_io("The current file under refactoring is:\n~p\n",[F]),
	    {ok, {AnnAST, Info}} = refac_util:parse_annotate_file(F,true, []),
	    {AnnAST1, Changed} = do_rename_mod(AnnAST, Old_New_Mod_Names),
	    if Changed ->
		    NewFileName = 
			case get_module_name(Info) of 
			    {ok, OldModName} ->
				case lists:keysearch(OldModName, 1, Old_New_Mod_Names) of 
				    {value, {OldModName, NewModName}}  ->
					if OldModName == NewModName -> F;
					   true -> 
						filename:join([filename:dirname(F), atom_to_list(NewModName)++".erl"])
					end;
				    false -> {error, "Could not infer new module name"}
				end;
			    _ ->  F
			end,				   
		    [{{F,NewFileName}, AnnAST1}|batch_rename_mod(Fs, Old_New_Mod_Names)];
	       true -> batch_rename_mod(Fs, Old_New_Mod_Names)
	    end
    end.


do_rename_mod(Tree, Old_New_Mod_Names) ->
     refac_util:stop_tdTP(fun do_rename_mod_1/2, Tree, Old_New_Mod_Names).

do_rename_mod_1(Tree, Old_New_Mod_Names) ->
    case refac_syntax:type(Tree) of
        attribute -> 
 	    AttrName = refac_syntax:attribute_name(Tree),
 	    case refac_syntax:atom_value(AttrName) of 
		module -> Args = refac_syntax:attribute_arguments(Tree),
			  F = fun (Arg) -> OldModName = refac_syntax:atom_value(Arg),
					   {value, {OldModName, NewModName}} = 
					       lists:keysearch(OldModName, 1, Old_New_Mod_Names),
					   refac_syntax:copy_attrs(Arg, refac_syntax:atom(NewModName))
			      end,
			  Args1 = lists:map(F, Args),
			  Tree1 = refac_syntax:copy_attrs(Tree, refac_syntax:attribute(AttrName, Args1)),
			  {Tree1, true};
 		import -> Args = refac_syntax:attribute_arguments(Tree),
 			  case Args of 
 			      [H|T] -> OldModName = refac_syntax:atom_value(H), 
				       case lists:keysearch(OldModName, 1, Old_New_Mod_Names) of 
					   {value, {OldModName, NewModName}} ->
					       H1 = refac_syntax:copy_attrs(H, refac_syntax:atom(NewModName)),
					       Tree1 = refac_syntax:copy_attrs(Tree, 
									       refac_syntax:attribute(AttrName, [H1|T])),
					       {Tree1, true};
					   false -> {Tree, false}
				       end;
 			      _ -> {Tree, false}
 			  end;
 		_ -> {Tree, false}
 	    end;
        module_qualifier ->
 	    Mod = refac_syntax:module_qualifier_argument(Tree),
 	    Fun = refac_syntax:module_qualifier_body(Tree),
 	    case refac_syntax:type(Mod) of 
 		atom ->  OldModName = refac_syntax:atom_value(Mod),
 			 case lists:keysearch(OldModName, 1, Old_New_Mod_Names) of 
			     {value, {OldModName, NewModName}}->
 				 Mod1 = refac_syntax:copy_attrs(Mod, refac_syntax:atom(NewModName)),
 				 Tree1= refac_syntax:copy_attrs(Tree, refac_syntax:module_qualifier(Mod1, Fun)),
 				 {Tree1, true};
 			     false -> {Tree, false}
 			 end;
 		_ -> {Tree, false}
 	    end;
        _ -> {Tree, false}
     end.

get_new_name(Name, RegExp, NewRegExp) ->
    L = length(Name),
    case regexp:match(Name, regexp:sh_to_awk(RegExp)) of 
	{match, 1, L} -> Sub = get_sub(Name, RegExp),
			 get_new_name(Sub, NewRegExp);
	_ -> Name
    end.

get_new_name(Sub, NewRegExp) ->
    Index = string:str(NewRegExp, "*"),    
    case Index of 
	0 -> NewRegExp;
	N -> Prefix = string:sub_string(NewRegExp, 1, N-1), 
	     case Sub of 
		 [] -> throw({error,"Can not infer new module names, please check the new module name pattern specified!"});
		 _  -> Sub1 = hd(Sub),
		       get_new_name(tl(Sub), Prefix++Sub1++string:sub_string(NewRegExp,N+1))
	     end
    end.
	     
get_sub(Name, RegExp) ->
     get_sub(Name, RegExp, []).
get_sub(Name, RegExp, Sub) ->
      Index1 = string:str(RegExp, "*"), %% TODO: how about '?'
      case Index1 of 
  	0 -> Sub;
  	N -> SubName = string:sub_string(Name, N),
	     SubRegExp = string:sub_string(RegExp, N),
	     S = get_sub1(SubName, SubRegExp, SubName),
 	     get_sub(string:sub_string(SubName, length(S)+1), string:sub_string(SubRegExp,2), Sub++[S])
      end.

get_sub1(_SubName, _SubRegExp, []) -> [];
get_sub1(SubName, SubRegExp, Sub) ->
    Str = Sub ++ string:sub_string(SubRegExp, 2),
    L = length(SubName),
    case regexp:match(SubName, regexp:sh_to_awk(Str)) of 
	{match,1, L} -> Sub;
	nomatch -> get_sub1(SubName, SubRegExp, string:sub_string(Sub, 1, length(Sub)-1))
    end.

get_module_name(ModInfo) ->				      
    case lists:keysearch(module, 1, ModInfo) of
	{value, {module, ModName}} -> {ok, ModName};
	false ->
	    {error, "Can not get the current module name."}
    end.
