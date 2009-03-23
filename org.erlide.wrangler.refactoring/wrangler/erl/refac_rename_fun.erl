%% =====================================================================
%% Refactoring: Rename a function name.
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
%% @doc Rename a function with a user-supplied new function name.
%% <p> To apply this refactoring, point the cursor to any occurrence of this
%% function name, then select <em> Rename Function Name </em> from the <em> Refactor </em> menu,
%% after that the refactorer will prompt to enter  the new function name in the mini-buffer.
%% </p>
%% <p>
%% When renaming an exported function name, this refactoring has a global effect, i.e.,
%% it affects all those modules in which this function is imported/used.
%% </p>
%% <p> The following <em> side-conditions </em> apply to this refactoring:
%% <li> The new function/arity should not conflict with any of the existing functions in the current module;</li>
%% <li> In the case that the function to be renamed is imported by another module, the new function name (with the same
%% arity) should be not alreay in scope (either defined or imported) in that module. </li>
%% </p>
%% @end
%% =====================================================================


-module(refac_rename_fun).

-export([rename_fun/6, rename_fun_eclipse/6]).

-export([check_atoms/2]).

%%-export([rename_fun_1/5, do_rename_fun/5]).

-include("../include/wrangler.hrl").
%% =====================================================================
%% @spec rename_fun(FileName::filename(), Line::integer(), Col::integer(), NewName::string(), SearchPaths::[string()])
%% -> term()
%%

-spec(rename_fun/6::(string(), integer(), integer(), string(), [dir()], integer()) ->
	     {error, string()} | {ok, [filename()]}).
rename_fun(FileName, Line, Col, NewName, SearchPaths, TabWidth) ->
    rename_fun(FileName, Line, Col, NewName, SearchPaths, TabWidth, emacs).

-spec(rename_fun_eclipse/6::(string(), integer(), integer(), string(), [dir()], integer()) ->
	     {error, string()} | {ok, [{filename(), filename(), string()}]}).
rename_fun_eclipse(FileName, Line, Col, NewName, SearchPaths, TabWidth) ->
    rename_fun(FileName, Line, Col, NewName, SearchPaths, TabWidth, eclipse).

rename_fun(FileName, Line, Col, NewName, SearchPaths, TabWidth, Editor) ->
    ?wrangler_io("\nCMD: ~p:rename_fun( ~p, ~p, ~p, ~p,~p, ~p).\n", [?MODULE, FileName, Line, Col, NewName, SearchPaths, TabWidth]),
    case refac_util:is_fun_name(NewName) of
      true ->
	    {ok, {AnnAST, Info}}=refac_util:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
	    NewName1 = list_to_atom(NewName),
	    {ok, ModName} = get_module_name(Info),
	    Inscope_Funs = lists:map(fun ({_M, F, A}) -> {F, A} end, refac_util:inscope_funs(Info)),
	    case refac_util:pos_to_fun_name(AnnAST, {Line, Col}) of
		{ok, {Mod, Fun, Arity, _, DefinePos}} ->
		    if Mod == ModName ->
			    case NewName1 =/= Fun of
				true ->
				    case lists:member({NewName1, Arity}, Inscope_Funs) or
					lists:member({NewName1, Arity}, refac_util:auto_imported_bifs())
					of
					true ->
					    {error,
					     NewName ++ "/" ++ integer_to_list(Arity) ++ " is already in scope."};
					_ ->
					    case is_callback_fun(Info, Fun, Arity) of
						true ->
						    {error,
						     "The refactorer does not support renaming "
						     "of callback function names."};
						_ ->
						    ?wrangler_io("The current file under refactoring is:\n~p\n", [FileName]),
						    {AnnAST1, _C} = rename_fun(AnnAST, {Mod, Fun, Arity}, {DefinePos, NewName1}),
						    %%check_atoms(AnnAST1, Fun),
						    case refac_util:is_exported({Fun, Arity}, Info) of
							true ->
							    ?wrangler_io("\nChecking client modules in the following "
								      "search paths: \n~p\n",
								      [SearchPaths]),
							    ClientFiles = refac_util:get_client_files(FileName, SearchPaths),
							    try rename_fun_in_client_modules(ClientFiles, {Mod, Fun, Arity}, NewName, SearchPaths, TabWidth) of 
								Results -> 
								    case Editor of 
									emacs ->
									    refac_util:write_refactored_files([{{FileName, FileName}, AnnAST1} | Results]),
									    ChangedClientFiles = lists:map(fun ({{F, _F}, _AST}) -> F end, Results),
									    ChangedFiles = [FileName | ChangedClientFiles],
									    ?wrangler_io("The following files have been changed "
											 "by this refactoring:\n~p\n",
											 [ChangedFiles]),
									    {ok, ChangedFiles};
									eclipse ->
									    Results1 = [{{FileName, FileName}, AnnAST1} | Results],
									    Res = lists:map(fun({{FName, NewFName}, AST}) -> {FName, NewFName, 
										       refac_prettypr:print_ast(refac_util:file_format(FName),AST)} end, Results1),
									    {ok, Res}
								    end
								catch 
								    throw:Err -> Err
								end;
							false ->
							    case Editor of 
								emacs ->
								    refac_util:write_refactored_files([{{FileName, FileName}, AnnAST1}]), {ok, [FileName]};
								eclipse ->
								    Res = [{FileName, FileName, refac_prettypr:print_ast(refac_util:file_format(FileName),AnnAST1)}],
								    {ok, Res}
							    end
						    end
					    end
				    end;
				_ -> case Editor of 
					 emacs -> refac_util:write_refactored_files([{{FileName, FileName}, AnnAST}]), {ok, []};
					 eclipse ->
 					     Res = [{FileName, FileName, refac_prettypr:print_ast(refac_util:file_format(FileName),AnnAST)}],
					     {ok, Res}
				     end
			    end;
		       true ->
			    {error,
			     "This function is not defined in this "
			     "module; please go to the module where "
			     "it is defined for renaming."}
		    end;
		{error, _Reason} -> {error, "You have not selected a function name!"}
	    end;
      false -> {error, "Invalid new function name!"}
    end.

%% =====================================================================
%% rename_fun(Tree:syntaxTree(), oldName::{atom(),atom(), integer()},
%%            {DefinePos,NewName}::{{integer(),integer{},atom()
%%
rename_fun(Tree, OldName, {DefinePos, NewName}) ->
    refac_util:stop_tdTP(fun do_rename_fun_1/2, Tree,
			 {OldName, {DefinePos, NewName}}).

do_rename_fun_1(Tree, {{M, OldName, Arity}, {DefinePos, NewName}}) ->
    case refac_syntax:type(Tree) of
      function ->
	  case get_fun_def_loc(Tree) of
	    DefinePos ->
		N = refac_syntax:function_name(Tree),
		Cs = refac_syntax:function_clauses(Tree),
		N1 = refac_syntax:copy_attrs(N, refac_syntax:atom(NewName)),
		{refac_syntax:copy_attrs(Tree, refac_syntax:function(N1, Cs)), false};
	    _ -> {Tree, false}
	  end;
      application ->
	  Operator = refac_syntax:application_operator(Tree),
	  Arguments = refac_syntax:application_arguments(Tree),
	  case application_info(Tree) of
	    {{_, apply}, 2} -> transform_apply_call(Tree, {M, OldName, Arity}, NewName);
	    {{_, apply}, 3} -> transform_apply_call(Tree, {M, OldName, Arity}, NewName);
	    {{_, spawn}, 3} -> transform_spawn_call(Tree, {M, OldName, Arity}, NewName);
	    {{_, spawn}, 4} -> transform_spawn_call(Tree, {M, OldName, Arity}, NewName);
	    {{_, spawn_link}, 3} -> transform_spawn_call(Tree, {M, OldName, Arity}, NewName);
	    {{_, spawn_link}, 4} -> transform_spawn_call(Tree, {M, OldName, Arity}, NewName);
	    _ ->
		case refac_syntax:type(Operator) of
		  atom ->
		      case get_fun_def_info(Operator) of
			{M, OldName, Arity, _} ->
			    Operator1 = refac_syntax:copy_attrs(Operator, refac_syntax:atom(NewName)),
			    Tree1 = refac_syntax:copy_attrs(Tree, refac_syntax:application(Operator1, Arguments)),
			    {Tree1, false};
			_ -> {Tree, false}
		      end;
		  module_qualifier ->
		      Mod = refac_syntax:module_qualifier_argument(Operator),
		      Fun = refac_syntax:module_qualifier_body(Operator),
		      case get_fun_def_info(Operator) of
			{M, OldName, Arity, _} ->
			    Fun1 = refac_syntax:copy_attrs(Fun, refac_syntax:atom(NewName)),
			    %% Need to change the fun_def annotation as well?
			    Operator1 = refac_syntax:copy_attrs(Tree, refac_syntax:module_qualifier(Mod, Fun1)),
			    Tree1 = refac_syntax:copy_attrs(Tree, refac_syntax:application(Operator1, Arguments)),
			    {Tree1, false};
			_ -> {Tree, false}
		      end;
		  _ -> {Tree, false}
		end
	  end;
      arity_qualifier ->
	  Fun = refac_syntax:arity_qualifier_body(Tree),
	  Fun_Name = refac_syntax:atom_value(Fun),
	  Arg = refac_syntax:arity_qualifier_argument(Tree),
	  Arg1 = refac_syntax:integer_value(Arg),
	  DefMod = get_fun_def_mod(Fun),
	  if (Fun_Name == OldName) and (Arg1 == Arity) and (DefMod == M) ->
		 {refac_syntax:copy_attrs(Tree, refac_syntax:arity_qualifier(refac_syntax:atom(NewName), Arg)),false};
	     true -> {Tree, false}
	  end;
      _ -> {Tree, false}
    end.

get_fun_def_loc(Node) ->
    As = refac_syntax:get_ann(Node),
    case lists:keysearch(fun_def, 1, As) of
      {value, {fun_def, {_M, _N, _A, _P, DefinePos}}} ->
	  DefinePos;
      _ -> ?DEFAULT_LOC
    end.

get_fun_def_mod(Node) ->
    As = refac_syntax:get_ann(Node),
    case lists:keysearch(fun_def, 1, As) of 
	{value, {fun_def, {M, _N, _A, _P, _DefinePos}}} -> M;
      _ -> '_'
    end.

rename_fun_in_client_modules(Files, {Mod, Fun, Arity}, NewName, SearchPaths, TabWidth) ->
    case Files of
      [] -> [];
      [F | Fs] ->
	    ?wrangler_io("The current file under refactoring is:\n~p\n", [F]),
	    {ok, {AnnAST, Info}}= refac_util:parse_annotate_file(F, true, SearchPaths, TabWidth),
	    {AnnAST1, Changed} = rename_fun_in_client_module_1({AnnAST, Info},{Mod, Fun, Arity}, NewName),
		%%check_atoms(AnnAST1, Fun),
	    if Changed ->
		    [{{F, F}, AnnAST1} | rename_fun_in_client_modules(Fs, {Mod, Fun, Arity}, NewName, SearchPaths, TabWidth)];
	       true ->
		    rename_fun_in_client_modules(Fs, {Mod, Fun, Arity}, NewName, SearchPaths, TabWidth)
	    end
    end.

get_fun_def_info(Node) ->
    As = refac_syntax:get_ann(Node),
    case lists:keysearch(fun_def, 1, As) of
      {value, {fun_def, {Mod, FunName, Arity, _UsePos, DefinePos}}} ->
	    {Mod, FunName, Arity, DefinePos};
	_ -> false
    end.

rename_fun_in_client_module_1({Tree, Info}, {M, OldName, Arity}, NewName) ->
    case lists:keysearch(module, 1, Info) of
      {value, {module, ClientModName}} ->
	  Inscope_Funs = lists:map(fun ({_M, F, A}) -> {F, A} end,
				   refac_util:inscope_funs(Info)),
	  case lists:member({list_to_atom(NewName), Arity}, Inscope_Funs) and
		 lists:member({OldName, Arity}, Inscope_Funs)
	      of
	    true ->
		  throw({error, "The new function name causes confliction in module '" ++ atom_to_list(ClientModName)++"'"});
	    false ->
		refac_util:stop_tdTP(fun do_rename_fun_in_client_module_1/2, Tree,
				     {{M, OldName, Arity}, NewName})
	  end;
      _ ->
	  refac_util:stop_tdTP(fun do_rename_fun_in_client_module_1/2, Tree,
			       {{M, OldName, Arity}, NewName})
    end.

do_rename_fun_in_client_module_1(Tree, {{M, OldName, Arity}, NewName}) ->
    case refac_syntax:type(Tree) of
      application ->
	  Operator = refac_syntax:application_operator(Tree),
	  Arguments = refac_syntax:application_arguments(Tree),
	  case application_info(Tree) of
	    {{_, apply}, 2} -> {Tree, true};
	    {{_, apply}, 3} -> transform_apply_call(Tree, {M, OldName, Arity}, NewName);
	    {{_, spawn}, 3} -> transform_spawn_call(Tree, {M, OldName, Arity}, NewName);
	    {{_, spawn}, 4} -> transform_spawn_call(Tree, {M, OldName, Arity}, NewName);
	    {{_, spawn_link}, 3} -> transform_spawn_call(Tree, {M, OldName, Arity}, NewName);
	    {{_, spawn_link}, 4} -> transform_spawn_call(Tree, {M, OldName, Arity}, NewName);
	    _ ->
		case refac_syntax:type(Operator) of
		  atom ->
		      case get_fun_def_info(Operator) of
			{M, OldName, Arity, _} ->
			    Operator1 = refac_syntax:copy_attrs(Operator, refac_syntax:atom(NewName)),
			    Tree1 = refac_syntax:copy_attrs(Tree, refac_syntax:application(Operator1, Arguments)),
			    {Tree1, true};
			_ -> {Tree, false}
		      end;
		  module_qualifier ->
		      Mod = refac_syntax:module_qualifier_argument(Operator),
		      Fun = refac_syntax:module_qualifier_body(Operator),
		      case get_fun_def_info(Operator) of
			{M, OldName, Arity, _} ->
			    Fun1 = refac_syntax:copy_attrs(Fun, refac_syntax:atom(NewName)),
			    %% Need to change the fun_def annotation as well?
			    Operator1 = refac_syntax:copy_attrs(Tree, refac_syntax:module_qualifier(Mod, Fun1)),
			    Tree1 = refac_syntax:copy_attrs(Tree, refac_syntax:application(Operator1, Arguments)),
			    {Tree1, true};
			_ -> {Tree, false}
		      end;
		  _ -> {Tree, false}
		end
	  end;
      arity_qualifier ->   %% is there a module name?
	  Fun = refac_syntax:arity_qualifier_body(Tree),
	  Fun_Name = refac_syntax:atom_value(Fun),
	  Arg = refac_syntax:arity_qualifier_argument(Tree),
	  Arg1 = refac_syntax:integer_value(Arg),
	  DefMod = get_fun_def_mod(Fun),
	  if (Fun_Name == OldName) and (Arg1 == Arity) and (DefMod == M) ->
		 {refac_syntax:copy_attrs(Tree, refac_syntax:arity_qualifier(refac_syntax:atom(NewName), Arg)),
		  true};
	     true -> {Tree, false}
	  end;
      _ -> {Tree, false}
    end.

get_module_name(ModInfo) ->
    case lists:keysearch(module, 1, ModInfo) of
      {value, {module, ModName}} -> {ok, ModName};
      false -> {error, "Can not get the current module name."}
    end.

is_callback_fun(ModInfo, Funname, Arity) ->
    case lists:keysearch(attributes, 1, ModInfo) of
      {value, {attributes, Attrs}} ->
	  case lists:keysearch(behaviour, 1, Attrs) of
	    {value, {behaviour, B}} ->
		lists:member({Funname, Arity},
			     refac_util:callback_funs(B));
	    _ -> false
	  end;
      _ -> false
    end.

-spec(check_atoms/2::(syntaxTree(), atom()) ->ok).	     
check_atoms(Tree, AtomName) ->
    F = fun (T) ->
		case refac_syntax:type(T) of
		  function -> collect_atoms(T, AtomName);
		  _ -> []
		end
	end,
    R = lists:flatten(lists:map(F, refac_syntax:form_list_elements(Tree))),
    R1 = lists:map(fun ({_, X}) -> X end,
		   lists:filter(fun (X) ->
					case X of
					  {atom, _X} -> true;
					  _ -> false
					end
				end,
				R)),
    R2 = lists:map(fun ({_, X}) -> X end,
		   lists:filter(fun (X) ->
					case X of
					  {atom, _X} -> false;
					  _ -> true
					end
				end,
				R)),
    UndecidableAtoms = R1 -- R2,
    case UndecidableAtoms of
      [] -> ok;
      _ ->
	  ?wrangler_io("WARNING: the refactorer could not decide "
		    "whether to rename the name at the following "
		    "positions in this file, please check "
		    "manually! {Line, Col}:\n~p\n",
		    [UndecidableAtoms])
    end.

collect_atoms(Tree, AtomName) ->
    F = fun (T, S) ->
		case refac_syntax:type(T) of
		  function ->
		      N = refac_syntax:function_name(T),
		      case refac_syntax:type(N) of
			atom ->
			    case refac_syntax:atom_value(N) of
			      AtomName -> S ++ [{function, refac_syntax:get_pos(N)}];
			      _ -> S
			    end;
			_ -> S
		      end;
		  application ->
		      Operator = refac_syntax:application_operator(T),
		      case refac_syntax:type(Operator) of
			atom ->
			    case refac_syntax:atom_value(Operator) of
			      AtomName ->
				  S ++ [{function, refac_syntax:get_pos(Operator)}];
			      _ -> S
			    end;
			_ -> S
		      end;
		  module_qualifier ->
		      Mod = refac_syntax:module_qualifier_argument(T),
		      Fun = refac_syntax:module_qualifier_body(T),
		      S1 = case refac_syntax:type(Mod) of
			     atom ->
				 case refac_syntax:atom_value(Mod) of
				   AtomName ->
				       S ++ [{module, refac_syntax:get_pos(Mod)}];
				   _ -> S
				 end;
			     _ -> S
			   end,
		      case refac_syntax:type(Fun) of
			atom ->
			    case refac_syntax:atom_value(Fun) of
			      AtomName -> S1 ++ [{function, refac_syntax:get_pos(Fun)}];
			      _ -> S1
			    end;
			_ -> S1
		      end;
		  arity_qualifier ->
		      Fun = refac_syntax:arity_qualifier_body(T),
		      case refac_syntax:atom_value(Fun) of
			AtomName -> S ++ [{function, refac_syntax:get_pos(Fun)}];
			_ -> S
		      end;
		  infix_expr ->
		      Op = refac_syntax:infix_expr_operator(T),
		      Left = refac_syntax:infix_expr_left(T),
		      case refac_syntax:operator_name(Op) of
			'!' ->
			    case refac_syntax:type(Left) of
			      atom ->
				  case refac_syntax:atom_value(Left) of
				    AtomName ->
					S ++ [{process, refac_syntax:get_pos(Left)}];
				    _ -> S
				  end;
			      _ -> S
			    end;
			_ -> S
		      end;
		  record_expr -> Type = refac_syntax:record_expr_type(T),
				 case refac_syntax:type(Type) of 
				     atom -> S ++ [{record, refac_syntax:get_pos(Type)}];
				     _ -> S
				 end;
		  record_field -> Name = refac_syntax:record_field_name(T),
				   case refac_syntax:type(Name) of 
				       atom -> S ++ [{record, refac_syntax:get_pos(Name)}];
				       _ -> S
				   end;				   
		  record_access -> Type = refac_syntax:record_access_type(T),
				   Field = refac_syntax:record_access_field(T),
				   S1 =case refac_syntax:type(Type) of 
					   atom -> S ++ [{record, refac_syntax:get_pos(Type)}];
					   _ -> S
				       end,
				   case refac_syntax:type(Field) of 
				       atom -> S1 ++ [{record, refac_syntax:get_pos(Field)}];
				       _ -> S1
				   end;
		  atom ->
		      case refac_syntax:atom_value(T) of
			AtomName ->
			    Pos = refac_syntax:get_pos(T), S ++ [{atom, Pos}];
			_ -> S
		      end;
		  _ -> S
		end
	end,
    refac_syntax_lib:fold(F, [], Tree).

transform_apply_call(Node, {ModName, FunName, Arity}, NewFunName) ->
    %%  Message = fun (Pos) -> ?wrangler_io("WARNING: function ***apply*** is used at location({line, col}):~p, and wrangler "
    %% 				    "could not decide whether this site should be refactored, please check!!!",
    %% 				     [Pos])
    %% 	      end,
    Operator = refac_syntax:application_operator(Node),
    Arguments = refac_syntax:application_arguments(Node),
    case Arguments of
      [Fun, Args] ->
	  case refac_syntax:type(Fun) of
	    implicit_fun ->
		Name = refac_syntax:implicit_fun_name(Fun),
		B = refac_syntax:atom_value(refac_syntax:arity_qualifier_body(Name)),
		A = refac_syntax:integer_value(refac_syntax:arity_qualifier_argument(Name)),
		case {B, A} of
		  {FunName, Arity} ->
		      FunName1 = refac_syntax:atom(NewFunName),
		      Fun1 = refac_syntax:implicit_fun(FunName1, refac_syntax:arity_qualifier_argument(Name)),
		      {refac_syntax:copy_attrs(Node, refac_syntax:application(Operator, [Fun1, Args])), true};
		  _ -> {Node, false}
		end;
	    _ -> {Node, false}
	  end;
      [Mod, Fun, Args] ->
	  Mod1 = refac_util:try_evaluation([refac_syntax:revert(Mod)]),
	  Fun1 = refac_util:try_evaluation([refac_syntax:revert(Fun)]),
	  %% Pos = refac_syntax:get_pos(Node),
	  case Fun1 of
	    {value, FunName} ->
		case Mod1 of
		  {value, ModName} ->
		      case refac_syntax:type(Args) of
			list ->
			    case refac_syntax:list_length(Args) of
			      Arity ->
				  Fun2 = refac_syntax:atom(NewFunName),
				  {refac_syntax:copy_pos(Node,
							 refac_syntax:copy_attrs(Node,
										 refac_syntax:application(Operator,
													  [Mod, Fun2, Args]))),
				   true};
			      _ -> {Node, false}
			    end;
			nil ->
			    if Arity == 0 ->
				   Fun2 = refac_syntax:atom(NewFunName),
				   {refac_syntax:copy_pos(Node,
							  refac_syntax:copy_attrs(Node,
										  refac_syntax:application(Operator,
													   [Mod, Fun2, Args]))),
				    true};
			       true -> {Node, false}
			    end;
			_ -> %% Message(Pos),
			    {Node, false}
		      end;
		  {value, _} -> {Node, false};
		  {error, _Reason} ->
		      case refac_syntax:type(Args) of
			list ->
			    case refac_syntax:list_length(Args) of
			      Arity ->%%  Message(Pos),
				  {Node, false};
			      _ -> {Node, false}
			    end;
			_ -> %% Message(Pos),
			    {Node, false}
		      end
		end;
	    {value, _} -> {Node, false};
	    {error, _Reason} ->
		case Mod1 of
		  {value, ModName} ->
		      case refac_syntax:type(Args) of
			list ->
			    case refac_syntax:list_length(Args) of
			      Arity -> %%Message(Pos),
				  {Node, false};
			      _ -> {Node, false}
			    end;
			_ -> %%Message(Pos),
			    {Node, false}
		      end;
		  {value, _} -> {Node, false};
		  {error, _Reason} ->
		      case refac_syntax:type(Args) of
			list ->
			    case refac_syntax:list_length(Args) of
			      Arity -> %% Message(Pos),
				  {Node, false};
			      _ -> {Node, false}
			    end;
			_ -> %% Message(Pos),
			    {Node, false}
		      end
		end
	  end;
      _ -> {Node, false}
    end.

transform_spawn_call(Node, {ModName, FunName, Arity}, NewFunName) ->
    %% Message = fun (Pos) -> ?wrangler_io("WARNING: function ***spawn*** is used at location({line, col}):~p, and wrangler "
    %% 				    "could not decide whether this site should be refactored, please check!!!",
    %% 				     [Pos])
    %% 	      end,
    Operator = refac_syntax:application_operator(Node),
    Arguments = refac_syntax:application_arguments(Node),
    [N, Mod, Fun, Args] = if length(Arguments) == 4 -> Arguments;
			     true -> [none] ++ Arguments
			  end,
    Mod1 = refac_util:try_evaluation([refac_syntax:revert(Mod)]),
    Fun1 = refac_util:try_evaluation([refac_syntax:revert(Fun)]),
    %% Pos = refac_syntax:get_pos(Node),
    case Fun1 of
      {value, FunName} ->
	  case Mod1 of
	    {value, ModName} ->
		case refac_syntax:type(Args) of
		  list ->
		      case refac_syntax:list_length(Args) of
			Arity ->
			    Fun2 = refac_syntax:atom(NewFunName),
			    App = if length(Arguments) == 4 ->
					 refac_syntax:application(Operator, [N, Mod, Fun2, Args]);
				     true -> refac_syntax:application(Operator, [Mod, Fun2, Args])
				  end,
			    {refac_syntax:copy_pos(Node, refac_syntax:copy_attrs(Node, App)), true};
			_ -> {Node, false}
		      end;
		  nil ->
		      if Arity == 0 ->
			     Fun2 = refac_syntax:atom(NewFunName),
			     App = if length(Arguments) == 4 ->
					  refac_syntax:application(Operator, [N, Mod, Fun2, Args]);
				      true -> refac_syntax:application(Operator, [Mod, Fun2, Args])
				   end,
			     {refac_syntax:copy_pos(Node, refac_syntax:copy_attrs(Node, App)), true};
			 true -> {Node, false}
		      end;
		  _ -> %%Message(Pos),
		      {Node, false}
		end;
	    {value, _} -> {Node, false};
	    {error, _Reason} ->
		case refac_syntax:type(Args) of
		  list ->
		      case refac_syntax:list_length(Args) of
			Arity -> %% Message(Pos),
			    {Node, false};
			_ -> {Node, false}
		      end;
		  _ -> %% Message(Pos),
		      {Node, false}
		end
	  end;
      {value, _} -> {Node, false};
      {error, _Reason} ->
	  case Mod1 of
	    {value, ModName} ->
		case refac_syntax:type(Args) of
		  list ->
		      case refac_syntax:list_length(Args) of
			Arity -> %% Message(Pos),
			    {Node, false};
			_ -> {Node, false}
		      end;
		  _ -> %% Message(Pos),
		      {Node, false}
		end;
	    {value, _} -> {Node, false};
	    {error, _Reason} ->
		case refac_syntax:type(Args) of
		  list ->
		      case refac_syntax:list_length(Args) of
			Arity -> %% Message(Pos),
			    {Node, false};
			_ -> {Node, false}
		      end;
		  _ -> %% Message(Pos),
		      {Node, false}
		end
	  end
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
	    atom ->
		Op = refac_syntax:atom_value(Operator), {{none, Op}, Arity};
	    module_qualifier ->
		Mod = refac_syntax:module_qualifier_argument(Operator),
		Fun = refac_syntax:module_qualifier_body(Operator),
		T1 = refac_syntax:type(Mod),
		T2 = refac_syntax:type(Fun),
		case T1 of
		  atom ->
		      Mod1 = refac_syntax:atom_value(Mod),
		      case T2 of
			atom ->
			    Fun1 = refac_syntax:atom_value(Fun), {{Mod1, Fun1}, Arity};
			_ -> {{Mod1, expressionfunname}, Arity}
		      end;
		  _ ->
		      case T2 of
			atom ->
			    Fun1 = refac_syntax:atom_value(Fun),
			    {{expressionmodname, Fun1}, Arity};
			_ -> {{expressionmodname, expressionfunname}, Arity}
		      end
		end;
	    _ -> {{none, expressionoperator}, Arity}
	  end;
      _ -> erlang:error(badarg)
    end.

