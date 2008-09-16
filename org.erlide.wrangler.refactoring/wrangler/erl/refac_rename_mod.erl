%% =====================================================================
%% Refactoring: Rename a module name.
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
%% @doc Rename a module with a user-supplied new module name.
%% <p> To apply this refactoring, point the cursor to anywhere in the module to be renamed, then select 
%% <em> Rename Module Name </em> from the <em> Refactor </em> menu, after that, the refactorer will prompt to enter 
%% the new module name in the mini-buffer.
%% </p>
%% <p> This refactoring has a global effect, i.e., it affects all those modules in which the module to be renamed is 
%% imported, or used as a module qualifier.
%% </p>
%% <p>
%% The following <em> side-conditions </em> apply to this refactoring:
%% <li> The new module name should be a fresh name. </li>
%% <li> This refactoring assume that the file basename is always the same as the module name, therefore this 
%% refactoring changes the filename as well. </li>
%% </p>
%% @end
%% =====================================================================

-module(refac_rename_mod).

-export([rename_mod/3, rename_mod_eclipse/3]).

-import(refac_rename_fun, [check_atoms/2]).
%% =====================================================================
%% @spec rename_mod(FileName::filename(), NewName::string(), SearchPaths::[string()])-> term()
%%   

rename_mod(FileName, NewName, SearchPaths) ->
    rename_mod(FileName, NewName, SearchPaths, emacs).

rename_mod_eclipse(FileName, NewName, SearchPaths) ->
    rename_mod(FileName, NewName, SearchPaths, eclipse).

rename_mod(FileName, NewName,SearchPaths, Editor) ->
    io:format("\n[CMD: rename_mod, ~p, ~p]\n", [FileName, NewName]),
    case refac_util:is_fun_name(NewName) of   %% module name and function name follow the same rules.
      true ->
          case refac_util:parse_annotate_file(FileName,true, SearchPaths) of
	    {ok, {AnnAST, Info}} ->
		  case get_module_name(Info) of 
		      {ok, OldModName} ->
			  NewModName = list_to_atom(NewName),
			  case (NewModName =/= OldModName) of
			      true -> 
				  case code:which(NewModName) of  
				      non_existing -> 
					  NewFileName = filename:dirname(FileName)++"/"++NewName++".erl",
					  case filename_exists(NewFileName, SearchPaths) of 
					      false -> 
						  io:format("The current file under refactoring is:\n~p\n",[FileName]), 
						  AnnAST1 = rename_mod_1(AnnAST, OldModName, NewModName),
						  check_atoms(AnnAST1, OldModName),
						  io:format("\nChecking client modules in the following search paths: \n~p\n",[SearchPaths]),
						  ClientFiles = refac_util:get_client_files(FileName, SearchPaths),

						  Results = rename_mod_in_client_modules(ClientFiles, 
						  		     OldModName, NewModName,SearchPaths),
						  case Editor of 
						      emacs ->
							  refac_util:write_refactored_files([{{FileName, NewFileName}, AnnAST1}|Results]),
							  ChangedClientFiles = lists:map(fun({{F, _F}, _AST}) -> F end, Results),
							  ChangedFiles = [FileName | ChangedClientFiles],
							  io:format
							    ("The following files have been changed by this refactoring:\n~p\n",
							     [ChangedFiles]),
							  {ok, ChangedFiles};
						      eclipse ->
							   Results1 =[{{FileName, NewFileName}, AnnAST1}|Results],
							   Res = lists:map(fun({{FName, NewFName}, AST}) -> {FName, NewFName, refac_prettypr:print_ast(AST)} end, Results1),
							  {ok, Res}
						  end;
					      true -> {error, "The new module/file name has been used!"}
					  end;
				      _ ->
					  {error, "The new module name has been used!"} 
				  end;
			      _ ->
				  {error, "New module name is the same as the old name."}
			  end;
		      {error, Reason} -> {error, Reason}
		  end;
	      {error, Reason} -> {error, Reason}
	  end;
      false -> {error, "Invalid new module name!"}
    end.

filename_exists(NewFileName, SearchPaths) ->
    Files = refac_util:expand_files(SearchPaths, ".erl"),
    lists:member(NewFileName, Files).
    
get_module_name(ModInfo) ->				      
    case lists:keysearch(module, 1, ModInfo) of
	{value, {module, ModName}} -> {ok, ModName};
	false ->
	    {error, "Can not get the current module name."}
    end.
	    

%% =====================================================================
%% rename_fun(Tree:syntaxTree(), oldName::{atom(),atom(), integer()}, 
%%            {DefinePos,NewName}::{{integer(),integer{},atom()
%%   
rename_mod_1(Tree, OldModName, NewModName) ->
    {Tree1, _} =refac_util:stop_tdTP(fun do_rename_mod/2, Tree, {OldModName, NewModName}),
    Tree1.

do_rename_mod(Tree, {OldModName, NewModName}) ->
    case refac_syntax:type(Tree) of
       attribute -> 
	    AttrName = refac_syntax:attribute_name(Tree),
	    case refac_syntax:atom_value(AttrName) of 
	       module -> Args = refac_syntax:attribute_arguments(Tree),
			 F = fun (Arg) -> case refac_syntax:atom_value(Arg) of 
					      OldModName -> refac_syntax:copy_attrs(Arg, refac_syntax:atom(NewModName));
					      _ -> Arg
					  end
			     end,
			 Args1 = lists:map(F, Args),
			 Tree1 = refac_syntax:copy_attrs(Tree, refac_syntax:attribute(AttrName, Args1)),
			 {Tree1, true};
		import -> Args = refac_syntax:attribute_arguments(Tree),
			  case Args of 
			      [H|T] -> case refac_syntax:atom_value(H) of 
					   OldModName -> 
					       H1 = refac_syntax:copy_attrs(H, refac_syntax:atom(NewModName)),
					       Tree1 = refac_syntax:copy_attrs(Tree, 
									       refac_syntax:attribute(AttrName, [H1|T])),
					       {Tree1, true};
					   _ -> {Tree, false}
				       end;
			      _ -> {Tree, false}
			  end;
		_ -> {Tree, false}
	    end;
       application ->
	    case application_info(Tree) of 
		{{_, apply},3} -> transform_apply_call(Tree, OldModName, NewModName);
		{{_, spawn}, 3} ->transform_spawn_call(Tree, OldModName, NewModName);
		{{_, spawn}, 4} -> transform_spawn_call(Tree, OldModName, NewModName); 
		{{_, spawn_link}, 3} ->transform_spawn_call(Tree, OldModName, NewModName);
		{{_, spawn_link}, 4} ->transform_spawn_call(Tree, OldModName, NewModName);
		_ -> {Tree, false}
	    end;
	module_qualifier ->
	    Mod = refac_syntax:module_qualifier_argument(Tree),
	    Fun = refac_syntax:module_qualifier_body(Tree),
	    case refac_syntax:type(Mod) of 
		atom ->  case refac_syntax:atom_value(Mod) of 
			     OldModName->
				 Mod1 = refac_syntax:copy_attrs(Mod, refac_syntax:atom(NewModName)),
				 Tree1= refac_syntax:copy_attrs(Tree, refac_syntax:module_qualifier(Mod1, Fun)),
				 {Tree1, true};
			     _ -> {Tree, false}
			 end;
		_ -> {Tree, false}
	    end;
	_ -> {Tree, false}
    end.
rename_mod_in_client_modules(Files, OldModName, NewModName, SearchPaths) ->
    case Files of 
        [] -> [];
        [F|Fs] ->  io:format("The current file under refactoring is:\n~p\n",[F]), 
                   case refac_util:parse_annotate_file(F,true, SearchPaths) of
                       {ok, {AnnAST, _Info}} ->
			   {AnnAST1, Changed} = rename_mod_in_client_module_1(AnnAST, OldModName, NewModName),
			   check_atoms(AnnAST1, OldModName),
			   if Changed ->
				   [{{F,F}, AnnAST1}|rename_mod_in_client_modules(Fs, OldModName, NewModName, SearchPaths)];
			      true -> rename_mod_in_client_modules(Fs, OldModName, NewModName, SearchPaths)
			   end;
  		     {error, Reason} -> {error, Reason}
  		 end 
    end.    

rename_mod_in_client_module_1(Tree, OldModName, NewModName) ->
    refac_util:stop_tdTP(fun do_rename_mod/2, Tree, {OldModName, NewModName}).


transform_apply_call(Node,OldModName, NewModName) ->
    Operator = refac_syntax:application_operator(Node),
    Arguments = refac_syntax:application_arguments(Node),
    case Arguments of 
	[Mod, Fun, Args] ->
	    Mod1 = refac_util:try_evaluation([refac_syntax:revert(Mod)]),
	    case Mod1 of 
		{value,OldModName} ->
		    Mod2 = refac_syntax:atom(NewModName),
		    {refac_syntax:copy_pos(Node,(refac_syntax:copy_attrs(Node, 
						refac_syntax:application(Operator, [Mod2, Fun, Args])))), false};
		_ -> {Node, false}
	    end;
	_ -> {Node, false}
    end.
 	     
transform_spawn_call(Node,OldModName, NewModName) ->
    Operator = refac_syntax:application_operator(Node),
    Arguments = refac_syntax:application_arguments(Node),
    [N, Mod, Fun, Args] = if length(Arguments)==4 -> Arguments;			       
				true -> [none] ++ Arguments
			     end,
    Mod1 = refac_util:try_evaluation([refac_syntax:revert(Mod)]),
    case Mod1 of 
	{value,OldModName} ->
	    Mod2 = refac_syntax:atom(NewModName),
	    App = if length(Arguments) == 4 ->
			  refac_syntax:application(Operator, [N, Mod2, Fun, Args]);
		     true -> refac_syntax:application(Operator, [Mod2, Fun, Args])
		  end,
	    {refac_syntax:copy_pos(Node,(refac_syntax:copy_attrs(Node, App))), false};
	_ -> {Node, false}
    end.


%% =====================================================================
%% @spec application_info(Tree::syntaxTree())->term()
%% ====================================================================       
application_info(Node) ->
    case refac_syntax:type(Node) of 
	application ->
	    Operator = refac_syntax:application_operator(Node),
	    Arguments = refac_syntax:application_arguments(Node),
	    Arity = length(Arguments),
	    case refac_syntax:type(Operator) of 
		atom -> Op = refac_syntax:atom_value(Operator),
			{{none,Op}, Arity}; 
		module_qualifier ->
		        Mod = refac_syntax:module_qualifier_argument(Operator),
		        Fun = refac_syntax:module_qualifier_body(Operator),
		        T1 = refac_syntax:type(Mod), 
		        T2 = refac_syntax:type(Fun),
		        case T1 of 
		  	    atom -> 
				Mod1 = refac_syntax:atom_value(Mod),
				case T2 of 
					atom -> Fun1 = refac_syntax:atom_value(Fun),
						{{Mod1, Fun1}, Arity};
				        _ ->{{Mod1, expressionfunname}, Arity}
					end;
			    _ -> case T2 of 
				     atom -> Fun1 = refac_syntax:atom_value(Fun),
					     {{expressionmodname, Fun1}, Arity};
				     _ -> {{expressionmodname,expressionfunname}, Arity}
				 end
			    end;
		_  -> {{none,expressionoperator}, Arity}
	    end;
	_ -> erlang:fault(not_an_application)
    end.



	
    
		 
		 
			     
			    
    
    

