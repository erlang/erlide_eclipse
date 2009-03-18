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

-export([rename_var/6, rename_var_eclipse/6]).

-export([pre_cond_check/4, rename/3]).

-include("../include/wrangler.hrl").

%% =====================================================================
%% @spec rename_var(FileName::filename(), Line::integer(), Col::integer(), NewName::string(),SearchPaths::[string()])-> term()
%%

-spec(rename_var/6::(filename(), integer(), integer(), string(), [dir()], integer()) ->
	     {error, string()} | {ok, string()}).
rename_var(FName, Line, Col, NewName, SearchPaths, TabWidth) ->
    rename_var(FName, Line, Col, NewName, SearchPaths, TabWidth, emacs).

-spec(rename_var_eclipse/6::(filename(), integer(), integer(), string(), [dir()], integer()) ->
	     {error, string()} | {ok, [{filename(), filename(), string()}]}).
rename_var_eclipse(FName, Line, Col, NewName, SearchPaths, TabWidth) ->
    rename_var(FName, Line, Col, NewName, SearchPaths, TabWidth, eclipse).

rename_var(FName, Line, Col, NewName, SearchPaths, TabWidth, Editor) ->
    ?wrangler_io("\nCMD: ~p:rename_var(~p, ~p, ~p, ~p, ~p, ~p).\n", [?MODULE,FName, Line, Col, NewName, SearchPaths, TabWidth]),
    case refac_util:is_var_name(NewName) of
	true ->
	    %% {ok, {_AnnAST, _Info0}} = refac_util:parse_annotate_file(FName, false, SearchPaths, TabWidth),
	    NewName1 = list_to_atom(NewName), 
	    {ok, {AnnAST1, _Info1}}= refac_util:parse_annotate_file(FName, true, SearchPaths, TabWidth),  
	    case refac_util:pos_to_var_name(AnnAST1, {Line, Col}) of
		{ok, {VarName, DefinePos, C}} ->
		    if DefinePos == [{0, 0}] -> 
			    case C of 
				macro_name ->{error, "Renaming of a macro name is not supported by this refactoring!"};
				_ -> {error, "Renaming of a free variable is not supported by this refactoring!"}
			    end;				
		       true ->
			    if VarName /= NewName1 ->
				    case C of
					macro_name ->
					    {error, "Sorry, renaming of macro names is not supported yet."};
					_ ->
					    case cond_check(AnnAST1, DefinePos, NewName1) of
						{true, _} ->
						    {error,
						     "New name already declared in the same scope."};
							{_, true} -> {error, "New name could cause name shadowing."};
						_ ->
						    {AnnAST2, _Changed} = rename(AnnAST1, DefinePos, NewName1),
						    case Editor of 
							emacs ->
							    refac_util:write_refactored_files([{{FName, FName}, AnnAST2}]),
							    {ok, "Refactor succeeded"};
							eclipse ->
							    {ok, [{FName, FName, refac_prettypr:print_ast(refac_util:file_format(FName),AnnAST2)}]}
						    end 
					    end
				    end;
			       true ->
				    case Editor of 
					emacs ->
					    refac_util:write_refactored_files([{{FName, FName}, AnnAST1}]),
					    {ok, "Refactor succeeded"};
					_ ->
					    {ok, [{FName, FName, refac_prettypr:print_ast(refac_util:file_format(FName),AnnAST1)}]}  
				    end
			    end
		    end;
		{error, _Reason} -> {error, "You have not selected a variable name, or the variable selected does not belong to a syntactically well-formed function!"}
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
    %%     ?wrangler_io("BindingChange1,2:\n~p\n",[{BindingChange1,BindingChange2}]),
    {Clash, Shadow1 or Shadow2}.  %%, (BindingChange1 or BindingChange2)}.


%% =====================================================================
%% @spec rename(Tree::syntaxTree(), DefinePos::{integer(),integer()}, NewName::string())-> term()
%%

-spec(rename/3::(syntaxTree(), [{integer(), integer()}], atom()) ->
	     {syntaxTree(), boolean()}).
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

-spec(pre_cond_check/4::(syntaxTree(), integer(), integer(), atom())->
	     boolean()).
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
