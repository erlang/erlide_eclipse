%% =====================================================================
%% Refactoring: Rename a variable name.
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
%% @doc Rename a variable with a user-supplied new variable name.
%% <p> To apply this refactoring, point the cursor to the beginning of any occurrence of this variable, then select
%% <em> Rename Variable Name </em> from the <em> Refactor </em> menu, after that the refactorer will prompt to enter
%% the new parameter name in the mini-buffer.
%% </p>
%% <p> This refactoring has a local effect, i.e., it only affects the function in which the refactoring is initialised.
%% </p>
%% <p> The following <em> side-conditions </em> apply to this refactoring:
%% <li> The new variable name should not conflict with any of the declared variable names in the same scope;</li>
%% <li> The new name should not shadow any of the existing variables in the outer scopes, or be shadowed by any of
%% of existing variables in the inner scopes, i.e., renaming to the new name should not change the semantics of the
%% program.</li>
%% </p>
%% @end


-module(refac_rename_var).

-export([rename_var/5, rename_var_eclipse/5]).

-export([pre_cond_check/4, cond_check/3, rename/3]).

%% =====================================================================
%% @spec rename_var(FileName::filename(), Line::integer(), Col::integer(), NewName::string(),SearchPaths::[string()])-> term()
%%
rename_var(FName, Line, Col, NewName, SearchPaths) ->
    rename_var(FName, Line, Col, NewName, SearchPaths, emacs).

rename_var_eclipse(FName, Line, Col, NewName, SearchPaths) ->
    rename_var(FName, Line, Col, NewName, SearchPaths, eclipse).

rename_var(FName, Line, Col, NewName, SearchPaths, Editor) ->
    io:format("\n[CMD: rename_var, ~p, ~p, ~p, ~p, "
	      "~p]\n",
	      [FName, Line, Col, NewName, SearchPaths]),
    case refac_util:is_var_name(NewName) of
      true ->
	  case refac_util:parse_annotate_file(FName, false, SearchPaths) of
	    {ok, {AnnAST, _Info0}} ->
		NewName1 = list_to_atom(NewName), 
		  case refac_util:parse_annotate_file(FName, true, SearchPaths) of
		  {ok, {AnnAST1, _Info1}} ->
		      case refac_util:pos_to_var_name(AnnAST, {Line, Col}) of
			{ok, {VarName, DefinePos, C}} ->
			    if DefinePos == [{0, 0}] -> {error, "Renaming of a free variable is not supported!"};
			       true ->
				   if VarName /= NewName1 ->
					  case C of
					    macro_name ->
						{error,
						 "Sorry, renaming of macro names is not "
						 "supported yet."};
					    _ ->
						  case cond_check(AnnAST1, DefinePos, NewName1) of
						      {true, _} ->
							{error,
						       "New name already declared in the same "
						       "scope."};
						    {_, true} -> {error, "New name could cause name shadowing."};
						    _ ->
							  {AnnAST2, _Changed} = rename(AnnAST1, DefinePos, NewName1),
						     %%  case post_refac_check(FName, AnnAST2, SearchPaths) of
%% 							ok ->
							  case Editor of 
							      emacs ->
								  refac_util:write_refactored_files([{{FName, FName}, AnnAST2}]),
								  {ok, "Refactor succeeded"};
							      eclipse ->
								 {ok, [{FName, FName, refac_prettypr:print_ast(AnnAST2)}]}
							  end 
  
						%% 	error ->
%% 							    {error,
%% 							     "Sorry,wrangler could not rename this "
%% 							     "variable."}
%% 						      end
						  end
					  end;
				      true ->
					   case Editor of 
					       emacs ->
						   refac_util:write_refactored_files([{{FName, FName}, AnnAST1}]),
						   {ok, "Refactor succeeded"};
					       _ ->
						  {ok, [{FName, FName, refac_prettypr:print_ast(AnnAST1)}]}  
					   end
				   end
			    end;
			  {error, _Reason} -> {error, "You have not selected a variable name!"}
		      end;
		    {error, Reason} -> {error, Reason}
		end;
	      {error, Reason} -> {error, Reason}
	  end;
	false -> {error, "Invalid new variable name."}
    end.

%% =====================================================================
%% @spec cond_check(Tree::syntaxTree(), Pos::{integer(),integer()}, NewName::string())-> term()
%%   		
cond_check(Tree, Pos, NewName) ->
    Env_Bd_Fr_Vars = envs_bounds_frees(Tree),
    F_Pos = fun ({_, DefPos}) -> DefPos end,
    F_Name = fun ({Name, _}) -> Name end,
    BdVars = lists:map(fun ({_, B, _}) -> B end, Env_Bd_Fr_Vars),
    %% The new name clashes with existing bound variables.
    Clash = lists:any(fun ({bound, Bds}) ->
			      Poss = lists:map(F_Pos, Bds),
			      Names = lists:map(F_Name, Bds),
			      F_Member = fun (P) -> lists:member(P, Poss) end,
			      lists:any(F_Member, Pos) and lists:member(NewName, Names)
		      end,
		      BdVars),
    %% The new name will shadow an existing free variable within the scope.
    Shadow1 = lists:any(fun ({{env, _}, {bound, Bds}, {free, Fs}}) ->
				Poss = lists:map(F_Pos, Bds),
				Names = lists:map(F_Name, Fs),
				F_Member = fun (P) -> lists:member(P, Poss) end,
				lists:any(F_Member, Pos) and lists:member(NewName, Names)
			end,
			Env_Bd_Fr_Vars),
    %% The new name will be shadowed by an existing bound variable.
    Shadow2 = lists:any(fun ({{env, _}, {bound, Bds}, {free, Fs}}) ->
				Poss= lists:map(F_Pos, Fs),
				Names = lists:map(F_Name, Bds),
				F_Member = fun (P) -> lists:member(P, Poss) end,
				lists:any(F_Member, Pos) and lists:member(NewName, Names)
			end,
			Env_Bd_Fr_Vars),
    %%  BindingChange1 = lists:any(fun({{env, Envs}, {bound, Bds},{free, _Fs}})->
    %%  				       Poss = lists:map (F_Pos, Bds),
    %%  				       Names = lists:map(F_Name, Envs),
    %%  				       F_Member = fun (P) -> lists:member(P,Poss) end,
    %% 				       lists:any(F_Member, Pos) and lists:member(NewName, Names)
    %% 			       end, Env_Bd_Fr_Vars),
    %%     BindingChange2 = lists:any(fun({{env, Envs}, {bound, Bds}, {free, _Fs}})->
    %% 				       Poss = lists:map(F_Pos, Envs),
    %% 				       Names = lists:map(F_Name, Bds),
    %% 				       F_Member = fun (P) -> lists:member(P,Poss) end,
    %% 				       lists:any(F_Member, Pos) and lists:member(NewName, Names)
    %% 			       end, Env_Bd_Fr_Vars),
    %%     io:format("BindingChange1,2:\n~p\n",[{BindingChange1,BindingChange2}]),
    {Clash, Shadow1 or Shadow2}.  %%, (BindingChange1 or BindingChange2)}.


%% =====================================================================
%% @spec rename(Tree::syntaxTree(), DefinePos::{integer(),integer()}, NewName::string())-> term()
%%
rename(Tree, DefinePos, NewName) ->
    refac_util:stop_tdTP(fun do_rename/2, Tree, {DefinePos, NewName}).

%% =====================================================================
%%
do_rename(Tree, {DefinePos, NewName}) ->
    case refac_syntax:type(Tree) of
      variable ->
	  As = refac_syntax:get_ann(Tree),
	  case lists:keysearch(def, 1, As) of
	    {value, {def, DefinePos}} -> {refac_syntax:set_name(Tree, NewName), true};
	    _ -> {Tree, false}
	  end;
      _ -> {Tree, false}
    end.

%% ===========================================================================
%% The following functions are temporally for testing purpose only, and will be removed.
%%=============================================================================
pre_cond_check(AST, Line, Col, NewName) ->
    {ok, {VarName, DefinePos, _C}} = refac_util:pos_to_var_name(AST, {Line, Col}),
    case VarName == NewName of 
	true -> true;
	_ ->  R = cond_check(AST, DefinePos, NewName),
	      case R of
		  {false, false} -> true;
		  _ -> false
	      end
    end.

post_refac_check(FileName, AST, SearchPaths) ->
    TempFileName = filename:join([filename:dirname(FileName),
				  filename:rootname(filename:basename(FileName)) ++ "_refac_temp"]),
    file:write_file(TempFileName, list_to_binary(refac_prettypr:print_ast(AST))),
    case refac_epp:parse_file(TempFileName, true, SearchPaths) of
      {ok, Forms1} ->
	  Forms = refac_syntax:form_list(tl(Forms1)),
	  AnnAST = refac_syntax_lib:annotate_bindings(Forms, ordsets:new()),
	  case refac_util:analyze_free_vars(AnnAST) of
	    {error, Reason} -> 
		  io:format("Reason:\n~p\n",[Reason]),
		  file:delete(TempFileName), error;
	    _ -> file:delete(TempFileName), ok
	  end;
      {error, Reason} -> 
	     io:format("Reason:\n~p\n",[Reason]),
	    file:delete(TempFileName), error
    end.

%% =====================================================================
%% @spec envs_bounds_frees(Node::syntaxTree())-> {value, [{Key, [atom()}]}
%%       Key = env | bound | free
%% @doc Return the input environment of the subtree, the variables that are
%% bound as well as the variables that are free in the subtree.
envs_bounds_frees(Node) ->
    F = fun (T, B) ->
		As = refac_syntax:get_ann(T),
		EnVars = case lists:keysearch(env, 1, As) of
			   {value, {env, EnVars1}} -> EnVars1;
			   _ -> []
			 end,
		BdVars = case lists:keysearch(bound, 1, As) of
			   {value, {bound, BdVars1}} -> BdVars1;
			   _ -> []
			 end,
		FrVars = case lists:keysearch(free, 1, As) of
			   {value, {free, FrVars1}} -> FrVars1;
			   _ -> []
			 end,
		case (EnVars == []) and (BdVars == []) and (FrVars == []) of
		  true -> B;
		  _ -> [{{env, EnVars}, {bound, BdVars}, {free, FrVars}} | B]
		end
	end,
    lists:usort(refac_syntax_lib:fold(F, [], Node)).
