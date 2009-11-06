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
%% =====================================================================
%% Refactoring: Rename a function name.
%%
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

-export([rename_fun/6, rename_fun_1/6,  rename_fun_eclipse/6, rename_fun_1_eclipse/6]).

-export([check_atoms/4, collect_atoms/2,try_eval/4, start_atom_process/0, 
	 stop_atom_process/1,output_atom_warning_msg/3, write_files/3,
	 apply_style_funs/0, eqc_statem_callback_funs/0, is_callback_fun/3,
	 commontest_callback_funs/0,testserver_callback_funs/0]).


-include("../include/wrangler.hrl").

%%-define(DEBUG, true).

-ifdef(DEBUG).
-define(debug(__String, __Args), ?wrangler_io(__String, __Args)).
-else.

-define(debug(__String, __Args), ok).
-endif.

-spec(rename_fun/6::(string(), integer(), integer(), string(), [dir()], integer()) ->
	     {error, string()} | {warning, string()} |{ok, [filename()]}).
rename_fun(FileName, Line, Col, NewName, SearchPaths, TabWidth) ->
    rename_fun(FileName, Line, Col, NewName, SearchPaths, TabWidth, emacs).

-spec(rename_fun_eclipse/6::(string(), integer(), integer(), string(), [dir()], integer()) ->
	     {error, string()} | {warning, string()} | {ok, [{filename(), filename(), string()}]}).
rename_fun_eclipse(FileName, Line, Col, NewName, SearchPaths, TabWidth) ->
    rename_fun(FileName, Line, Col, NewName, SearchPaths, TabWidth, eclipse).

rename_fun(FileName, Line, Col, NewName, SearchPaths, TabWidth, Editor) ->
    ?wrangler_io("\nCMD: ~p:rename_fun( ~p, ~p, ~p, ~p,~p, ~p).\n",
		 [?MODULE, FileName, Line, Col, NewName, SearchPaths, TabWidth]),
    Cmd1 = "CMD: " ++ atom_to_list(?MODULE) ++ ":rename_fun(" ++ "\"" ++
	FileName ++ "\", " ++ integer_to_list(Line) ++
	", " ++ integer_to_list(Col) ++ ", " ++ "\"" ++ NewName ++ "\","
	++ "[" ++ refac_util:format_search_paths(SearchPaths) ++ "]," ++ integer_to_list(TabWidth) ++ ").",
    case refac_util:is_fun_name(NewName) of
      true -> ok;
      false -> throw({error, "Invalid new function name!"})
    end,
    {ok, {AnnAST, Info}} = refac_util:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    NewName1 = list_to_atom(NewName),
    {ok, ModName} = get_module_name(Info),
    case refac_util:pos_to_fun_name(AnnAST, {Line, Col}) of
      {ok, {Mod, Fun, Arity, _, DefinePos}} ->
	  case {ModName, NewName1} =/= {Mod, Fun} of
	    true ->
		  case pre_cond_check(FileName, Info, NewName1, Mod, Fun, Arity) of
		      ok ->
			  rename_fun_0(FileName, NewName, SearchPaths, TabWidth, 
				       Editor, AnnAST, Info, NewName1,
				       Mod, Fun, Arity, DefinePos, Cmd1);
		      Others -> Others
		  end;
	      _ ->
		  case Editor of
		      emacs -> {ok, []};
		      eclipse ->
			  FileContent = refac_prettypr:print_ast(refac_util:file_format(FileName), AnnAST),
			  {ok, [{FileName, FileName, FileContent}]}
		  end
	  end;
	{error, _Reason} ->
	    {error, "You have not selected a function name!"}
    end.

%% TODO : I would like to reorder these parameters,
rename_fun_0(FileName, NewName, SearchPaths, TabWidth, Editor, 
	     AnnAST, Info, NewName1, Mod, Fun, Arity,DefinePos, Cmd) ->
    Pid = start_atom_process(),
    ?wrangler_io("The current file under refactoring is:\n~p\n", [FileName]),
    {AnnAST1, _C} = do_rename_fun(FileName, SearchPaths, AnnAST, 
				  {Mod, Fun, Arity}, {DefinePos, NewName1}, 
				  Pid, TabWidth),
    check_atoms(FileName, AnnAST1, [Fun], Pid),
    case refac_util:is_exported({Fun, Arity}, Info) of
	true ->
	    ?wrangler_io("\nChecking possible client modules in the following search paths: \n~p\n", [SearchPaths]),
	    ClientFiles = refac_util:get_client_files(FileName, SearchPaths),
	    try
		rename_fun_in_client_modules(ClientFiles, {Mod, Fun, Arity}, 
					     NewName, SearchPaths, TabWidth, Pid)
		of
		Results ->
		    output_atom_warning_msg(Pid, not_renamed_warn_msg(Fun), renamed_warn_msg(Fun)),
		    stop_atom_process(Pid),
		    write_files(Editor, [{{FileName, FileName}, AnnAST1}| Results], Cmd)
	    catch
		throw:Err ->
		    stop_atom_process(Pid),
		    Err
	    end;
	false ->
	    output_atom_warning_msg(Pid, not_renamed_warn_msg(Fun), renamed_warn_msg(Fun)),
	    stop_atom_process(Pid),
	    write_files(Editor, [{{FileName, FileName}, AnnAST1}], Cmd)
    end.

-spec(rename_fun_1/6::(string(), integer(), integer(), string(), [dir()], integer()) ->
	     {error, string()} | {ok, [filename()]}).
rename_fun_1(FileName, Line, Col, NewName, SearchPaths, TabWidth) ->
    rename_fun_1(FileName, Line, Col, NewName, SearchPaths, TabWidth, emacs).
    
-spec(rename_fun_1_eclipse/6::(string(), integer(), integer(), string(), [dir()], integer()) ->
	     {error, string()} | {ok, [filename()]}).

rename_fun_1_eclipse(FileName, Line, Col, NewName, SearchPaths, TabWidth) ->
    rename_fun_1(FileName, Line, Col, NewName, SearchPaths, TabWidth, eclipse).
    
rename_fun_1(FileName, Line, Col, NewName, SearchPaths, TabWidth, Editor) ->
    Cmd = "CMD: " ++ atom_to_list(?MODULE) ++ ":rename_fun(" ++ "\"" ++
	FileName ++ "\", " ++ integer_to_list(Line) ++
	", " ++ integer_to_list(Col) ++ ", " ++ "\"" ++ NewName ++ "\","
	++ "[" ++ refac_util:format_search_paths(SearchPaths) ++ "]," ++ integer_to_list(TabWidth) ++ ").",    
    {ok, {AnnAST, Info}} = refac_util:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    NewName1 = list_to_atom(NewName),
    {ok, {Mod, Fun, Arity, _, DefinePos}} = refac_util:pos_to_fun_name(AnnAST, {Line, Col}),
    ?wrangler_io("The current file under refactoring is:\n~p\n", [FileName]),
    Pid = start_atom_process(),
    {AnnAST1, _C} = do_rename_fun(FileName, SearchPaths, AnnAST, {Mod, Fun, Arity}, 
				  {DefinePos, NewName1}, Pid, TabWidth),
    case refac_util:is_exported({Fun, Arity}, Info) of
	true ->
	    ?wrangler_io("\nChecking client modules in the following search paths: \n~p\n", [SearchPaths]),
	    ClientFiles = refac_util:get_client_files(FileName, SearchPaths),
	    try
		rename_fun_in_client_modules(ClientFiles, {Mod, Fun, Arity}, NewName, SearchPaths, TabWidth, Pid)
		of
		Results ->
		    output_atom_warning_msg(Pid, not_renamed_warn_msg(Fun), renamed_warn_msg(Fun)),
		    stop_atom_process(Pid),
		    write_files(Editor, [{{FileName, FileName}, AnnAST1}| Results], Cmd)
	    catch
		throw:Err -> Err
	    end;
	false ->
	    output_atom_warning_msg(Pid, not_renamed_warn_msg(Fun), renamed_warn_msg(Fun)),
	    stop_atom_process(Pid),
	    write_files(emacs, [{{FileName, FileName}, AnnAST1}], Cmd)
    end.

-spec(write_files/3::(Editor::atom(), Results::[{{filename(), filename()}, syntaxTree()}], string()) ->
	     {ok, [filename()]} | {ok, [{filename(), filename(), string()}]}).
write_files(Editor, Results, Cmd) ->    
    case Editor of 
	emacs ->
	    refac_util:write_refactored_files_for_preview(Results, Cmd),
	    ChangedFiles = [F ||{{F, _F}, _AST}<-Results],
	    ?wrangler_io("The following files are to be changed by this refactoring:\n~p\n",
			 [ChangedFiles]),
	    {ok, ChangedFiles};
	eclipse ->
	    Res= [{FName, NewFName, refac_prettypr:print_ast(refac_util:file_format(FName),AST)}
		  ||{{FName, NewFName}, AST}<-Results],	  
	    {ok, Res}
    end.

pre_cond_check(FileName, Info, NewFunName, OldFunDefMod,OldFunName, Arity) ->
    {ok, ModName} = get_module_name(Info),
    Inscope_Funs = [{F, A}||{_M, F, A}<-refac_util:inscope_funs(Info)],
    if OldFunDefMod==ModName ->
	    case lists:member({NewFunName, Arity}, Inscope_Funs) orelse
		erlang:is_builtin(erlang, NewFunName, Arity) orelse
		erl_internal:bif(erlang, NewFunName, Arity) of
		true ->
		    {error, atom_to_list(NewFunName) ++ "/" ++ 
		     integer_to_list(Arity) ++" is already in scope, "
		     "or is an auto-imported builtin function."};
		_ -> 
		    case is_callback_fun(Info,OldFunName, Arity) of
			true ->
			    {warning, "The function to be renamed is a callback function, continue?"}; 
			_ -> TestFrameWorkUsed = refac_util:test_framework_used(FileName),
			     case TestFrameWorkUsed of
				 [] -> ok;
				 _ ->
				     catch test_framework_aware_name_checking(
					     TestFrameWorkUsed, OldFunName, Arity, NewFunName)			      		     
			     end
		    end
	    end;
       true  ->
	    {error, "This function is not defined in this module; "
	     "a function can only be renamed from the module where it is defined."}
    end.


do_rename_fun(FileName, SearchPaths, Tree, {ModName, OldName, Arity}, 
	      {DefinePos, NewName}, Pid, TabWidth) ->
    refac_util:full_tdTP(fun do_rename_fun_1/2, Tree, 
			 {FileName, {ModName, OldName, Arity}, 
			  {DefinePos, NewName},SearchPaths, Pid, TabWidth}).
  
do_rename_fun_1(Tree, {FileName, {M, OldName, Arity}, 
		       {DefinePos, NewName}, SearchPaths, Pid, TabWidth}) ->
    case refac_syntax:type(Tree) of
	function ->
	    case get_fun_def_loc(Tree) of
		DefinePos ->
		    N = refac_syntax:function_name(Tree),
		    Cs = refac_syntax:function_clauses(Tree),
		    N1 = refac_syntax:copy_attrs(N, refac_syntax:atom(NewName)),
		    {rewrite(Tree, refac_syntax:function(N1, Cs)), true};
		_ -> {Tree, false}
	    end;
	application ->
	    Operator = refac_syntax:application_operator(Tree),
	    Arguments = refac_syntax:application_arguments(Tree),
	    case get_fun_def_info(Operator) of
		{Mod1, Fun1, Ari1, _} ->
		    case lists:keysearch({Mod1, Fun1, Ari1}, 1, apply_style_funs()) of
			{value, _} ->
			    transform_apply_style_calls(FileName, Tree, {M, OldName, Arity}, NewName, SearchPaths, TabWidth);
			false -> 
			    case refac_syntax:type(Operator) of
				atom ->
				    case get_fun_def_info(Operator) of
					{M, OldName, Arity, _} ->
					    Operator1 = rewrite(Operator, refac_syntax:atom(NewName)),
					    Tree1 = rewrite(Tree, refac_syntax:application(Operator1, Arguments)),
					    {Tree1, true};
					_ -> {Tree, false}
				    end;
				module_qualifier ->
				    Mod = refac_syntax:module_qualifier_argument(Operator),
				    Fun = refac_syntax:module_qualifier_body(Operator),
				    case get_fun_def_info(Operator) of
					{M, OldName, Arity, _} ->
					    Fun2 = rewrite(Fun, refac_syntax:atom(NewName)),
					    %% Need to change the fun_def annotation as well?
					    Operator1 = rewrite(Operator, refac_syntax:module_qualifier(Mod, Fun2)),
					    Tree1 = rewrite(Tree, refac_syntax:application(Operator1, Arguments)),
					    {Tree1, true};
					_ -> {Tree, false}
				    end;
				tuple ->
				    case refac_syntax:tuple_elements(Operator) of 
					[Mod, Fun] ->
					    case get_fun_def_info(Operator) of
						{M, OldName, Arity, _} ->
						    Fun2 = rewrite(Fun, refac_syntax:atom(NewName)),
						    %% Need to change the fun_def annotation as well?
						    Operator1 = rewrite(Operator, refac_syntax:tuple([Mod, Fun2])),
						    Tree1 = rewrite(Tree, refac_syntax:application(Operator1, Arguments)),
						    {Tree1, true};
						_ -> {Tree, false}
					    end;
					_ -> {Tree, false}
				    end;
				_ -> {Tree, false}
			    end
		    end
	  end;
	arity_qualifier ->
	    do_rename_fun_in_arity_qualifier(Tree, M, OldName, Arity, NewName);
	tuple -> 
	    do_rename_fun_in_tuples(Tree, {FileName, SearchPaths, M, OldName, NewName, Arity, Pid, TabWidth});
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

rename_fun_in_client_modules(Files, {Mod, Fun, Arity}, NewName, SearchPaths, TabWidth, Pid) ->
    case Files of
      [] -> [];
      [F | Fs] ->
	    ?wrangler_io("The current file under refactoring is:\n~p\n", [F]),
	    {ok, {AnnAST, Info}}= refac_util:parse_annotate_file(F, true, SearchPaths, TabWidth),
	    {AnnAST1, Changed} = rename_fun_in_client_module_1(F, {AnnAST, Info},{Mod, Fun, Arity}, NewName, SearchPaths, Pid, TabWidth),
	    check_atoms(F, AnnAST1, [Fun], Pid),
	    if Changed ->
		    [{{F, F}, AnnAST1} | rename_fun_in_client_modules(Fs, {Mod, Fun, Arity}, NewName, SearchPaths, TabWidth, Pid)];
	       true ->
		    rename_fun_in_client_modules(Fs, {Mod, Fun, Arity}, NewName, SearchPaths, TabWidth, Pid)
	    end
    end.

get_fun_def_info(Node) ->
    As = refac_syntax:get_ann(Node),
    case lists:keysearch(fun_def, 1, As) of
      {value, {fun_def, {Mod, FunName, Arity, _UsePos, DefinePos}}} ->
	    {Mod, FunName, Arity, DefinePos};
	_ -> false
    end.

rename_fun_in_client_module_1(FileName, {Tree, Info}, {M, OldName, Arity}, NewName, SearchPaths, Pid, TabWidth) ->
    case lists:keysearch(module, 1, Info) of
      {value, {module, ClientModName}} ->
	    Inscope_Funs = [{F, A} ||{_M, F, A}<-refac_util:inscope_funs(Info)],
	    case lists:member({list_to_atom(NewName), Arity}, Inscope_Funs)
		and lists:member({OldName, Arity}, Inscope_Funs) of
		true ->
		    throw({error, "The new function name causes confliction in module '" 
			   ++ atom_to_list(ClientModName)++"'"});
		_ -> ok
	    end;
	_ -> ok
    end,
    refac_util:full_tdTP(fun do_rename_fun_in_client_module_1/2, Tree, 
			 {FileName,{M, OldName, Arity}, NewName, SearchPaths, Pid,TabWidth}).
  

do_rename_fun_in_client_module_1(Tree, {FileName, {M, OldName, Arity}, NewName, SearchPaths, Pid, TabWidth}) ->
    case refac_syntax:type(Tree) of
      application ->
	  Operator = refac_syntax:application_operator(Tree),
	  Arguments = refac_syntax:application_arguments(Tree),
	  case get_fun_def_info(Operator) of
	    {Mod1, Fun1, Ari1, _} ->
		case lists:keysearch({Mod1, Fun1, Ari1}, 1, apply_style_funs()) of
		  {value, _} ->
		      transform_apply_style_calls(FileName, Tree, {M, OldName, Arity}, NewName, SearchPaths, TabWidth);
		  false -> case refac_syntax:type(Operator) of
			       atom ->
				   case get_fun_def_info(Operator) of
				       {M, OldName, Arity, _} ->
					   Operator1 = rewrite(Operator, refac_syntax:atom(NewName)),
					   Tree1 = rewrite(Tree, refac_syntax:application(Operator1, Arguments)),
					   {Tree1, true};
				       _ -> {Tree, false}
				   end;
			       module_qualifier ->
				   Mod = refac_syntax:module_qualifier_argument(Operator),
				   Fun = refac_syntax:module_qualifier_body(Operator),
				   case get_fun_def_info(Operator) of
				       {M, OldName, Arity, _} ->
					   Fun2 = rewrite(Fun, refac_syntax:atom(NewName)),
					   %% Need to change the fun_def annotation as well?
					   Operator1 = rewrite(Operator, refac_syntax:module_qualifier(Mod, Fun2)),
					   Tree1 = rewrite(Tree, refac_syntax:application(Operator1, Arguments)),
					   {Tree1, true};
				       _ -> {Tree, false}
				   end;
			       tuple ->
				   case refac_syntax:tuple_elements(Operator) of 
				       [Mod, Fun] ->
					   case get_fun_def_info(Operator) of
					       {M, OldName, Arity, _} ->
						   Fun2 = rewrite(Fun, refac_syntax:atom(NewName)),
						   %% Need to change the fun_def annotation as well?
						   Operator1 = rewrite(Operator, refac_syntax:tuple([Mod, Fun2])),
						   Tree1 = rewrite(Tree, refac_syntax:application(Operator1, Arguments)),
						   {Tree1, true};
					       _ -> {Tree, false}
					   end;
				       _ -> {Tree, false}
				   end;
			       _ -> {Tree, false}
			   end
		end;
	    _ -> {Tree, false}
	  end;
	arity_qualifier ->   %% is there a module name?
	    do_rename_fun_in_arity_qualifier(Tree, M, OldName, Arity, NewName);
	tuple -> 
	    do_rename_fun_in_tuples(Tree, {FileName, SearchPaths, M, OldName, NewName, Arity, Pid, TabWidth});
	_ -> {Tree, false}
    end.

do_rename_fun_in_arity_qualifier(Tree, M, OldName, Arity, NewName) ->
    Fun = refac_syntax:arity_qualifier_body(Tree),
    Fun_Name = refac_syntax:atom_value(Fun),
    Arg = refac_syntax:arity_qualifier_argument(Tree),
    Arg1 = refac_syntax:integer_value(Arg),
    DefMod = get_fun_def_mod(Fun),
    if (Fun_Name == OldName) and (Arg1 == Arity) and (DefMod == M) ->
	    {rewrite(Tree, refac_syntax:arity_qualifier(refac_syntax:atom(NewName), Arg)),
	    true};
       true -> {Tree, false}
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

%%-spec(check_atoms/4::(filename(), syntaxTree(), [atom()],pid()) ->ok).      
check_atoms(FileName, Tree, AtomNames, Pid) ->
    F = fun (T) ->
		case refac_syntax:type(T) of
		  function -> collect_atoms(T, AtomNames);
		  _ -> []
		end
	end,
    R = lists:usort(lists:flatten(lists:map(F, refac_syntax:form_list_elements(Tree)))),
    R1 = [X ||{atom, X, _} <- R],
    R2 = [X ||{_, X, _} <- lists:filter(fun (X) ->
					     case X of
						 {atom, _X, _} -> false;
						 _ -> true
					     end
					end,
					R)],
    UndecidableAtoms = R1 -- R2,
    case UndecidableAtoms of 
	[] -> ok;
	_ -> Pid ! {add_not_renamed, {FileName, UndecidableAtoms}}
    end.
 

collect_atoms(Tree, AtomNames) ->
    F = fun (T, S) ->
		case refac_syntax:type(T) of
		  function ->
		      N = refac_syntax:function_name(T),
		      collect_atoms_1(AtomNames, S, N, function);
		  application ->
			Operator = refac_syntax:application_operator(T),
			collect_atoms_1(AtomNames, S, Operator, function);
		  module_qualifier ->
		      Mod = refac_syntax:module_qualifier_argument(T),
		      Fun = refac_syntax:module_qualifier_body(T),
		      S1 = collect_atoms_1(AtomNames, S, Mod, module),
		      collect_atoms_1(AtomNames, S1, Fun, function);
		  arity_qualifier ->
		      Fun = refac_syntax:arity_qualifier_body(T),
		      case lists:member(refac_syntax:atom_value(Fun), AtomNames) of
			true -> S ++ [{function, refac_syntax:get_pos(Fun)}];
			false -> S
		      end;
		  infix_expr ->
		      Op = refac_syntax:infix_expr_operator(T),
		      Left = refac_syntax:infix_expr_left(T),
		      case refac_syntax:operator_name(Op) of
			'!' ->
			    case refac_syntax:type(Left) of
			      atom ->
				    AtomVal = refac_syntax:atom_value(Left),
				    case lists:member(AtomVal, AtomNames) of
					true ->
					    S ++ [{process, refac_syntax:get_pos(Left), AtomVal}];
					_ -> S
				    end;
				_ -> S
			    end;
			  _ -> S
		      end;
		    record_expr -> Type = refac_syntax:record_expr_type(T),
				   collect_atoms_1(AtomNames, S, Type, record);
		    record_field -> Name = refac_syntax:record_field_name(T),
				    case refac_syntax:type(Name) of
					atom ->
					    AtomVal = refac_syntax:atom_value(Name),
					    case lists:member(AtomVal, AtomNames) of
						true ->
						    S ++ [{record, refac_syntax:get_pos(Name), AtomVal}];
						_ -> S
					    end;
					_ -> S
				    end;
		    record_access -> Type = refac_syntax:record_access_type(T),
				     Field = refac_syntax:record_access_field(T),
				     S1 = collect_atoms_1(AtomNames, S, Type, record),
				     case refac_syntax:type(Field) of
					 atom ->
					     AtomVal = refac_syntax:atom_value(Field), 
					     case lists:member(AtomVal, AtomNames) of
						 true ->
						     S ++ [{record, refac_syntax:get_pos(Field), AtomVal}];
						 false ->
						     S
					     end;
					 _ -> S1
				     end;
		    atom ->
			AtomVal = refac_syntax:atom_value(T),
			case lists:member(AtomVal, AtomNames) of
			    true ->
				Pos = refac_syntax:get_pos(T), S ++ [{atom, Pos, AtomVal}];
			    false -> S
			end;
		    _ -> S
		end
	end,
    refac_syntax_lib:fold(F, [], Tree).

collect_atoms_1(AtomNames, S, Node, Type) ->
    F = fun(T, Acc) ->
		case refac_syntax:type(T) of
		    atom ->
			AtomVal = refac_syntax:atom_value(T),
			case lists:member(AtomVal, AtomNames) of
			    true when Type==function ->
				Ann = refac_syntax:get_ann(T),
				case lists:keysearch(fun_def, 1, Ann) of
				    {value, {fun_def, {'_', _, _, _, _}}} ->
					Acc ++[{atom, refac_syntax:get_pos(T), AtomVal}];
				    {value, {fun_def, {_,'_', _, _, _}}} ->
					Acc ++[{atom, refac_syntax:get_pos(T), AtomVal}];
				    {value, {fun_def, {_, _, '_', _, _}}} ->
					Acc ++[{atom, refac_syntax:get_pos(T), AtomVal}];
				    {value, _} ->
					Acc ++ [{function, refac_syntax:get_pos(T), AtomVal}];
				    false ->
					Acc ++[{atom, refac_syntax:get_pos(T), AtomVal}]
				end;
			    true->
				Acc ++ [{Type, refac_syntax:get_pos(T), AtomVal}];
			    false ->
				Acc
			end;
		    _ -> Acc
		end
	end,
    refac_syntax_lib:fold(F,S,Node).



transform_apply_style_calls(FileName, Node, {ModName, FunName, Arity}, NewFunName, SearchPaths, TabWidth) ->
    Operator = refac_syntax:application_operator(Node),
    Arguments = refac_syntax:application_arguments(Node),
    [N1, N2, Mod, Fun, Args] = case length(Arguments) of
				 5 -> Arguments;
				 4 -> [none| Arguments];
				 3 -> [none, none| Arguments]
			       end,
    Mod1 = try_eval(FileName, Mod, SearchPaths, TabWidth),
    Fun1 = try_eval(FileName, Fun, SearchPaths, TabWidth),
    case Fun1 of
      {value, FunName} ->
	  case Mod1 of
	    {value, ModName} ->
		  case refac_syntax:type(Args) == list andalso refac_syntax:list_length(Args) == Arity orelse
		       refac_syntax:type(Args) == nil andalso Arity == 0
		    of
		  true ->
		      Fun2 = refac_syntax:atom(NewFunName),
		      App = case length(Arguments) of
			      5 -> refac_syntax:application(Operator, [N1, N2, Mod, Fun2, Args]);
			      4 -> refac_syntax:application(Operator, [N2, Mod, Fun2, Args]);
			      3 -> refac_syntax:application(Operator, [Mod, Fun2, Args])
			    end,
		      {rewrite(Node, App), true};
		  false -> {Node, false}
		end;
	    _ ->
		{Node, false}
	  end;
      {value, _} -> {Node, false};
      {error, _Reason} -> {Node, false}
    end.

%% =====================================================================
%% @spec application_info(Tree::syntaxTree())->term()

%% Three ways that function name can be used in test data:
%% {modulename, functionname}
%% {modulename, functionname, arity}
%% {modulename, functionname, arglist}
%% {generator, modulename, functionname}
%% {call, modulename, functionname, arglist}
do_rename_fun_in_tuples(Node, {FileName, SearchPaths, ModName, OldName, NewName, Arity, Pid, TabWidth}) ->
    case refac_syntax:tuple_elements(Node) of
      [E1, E2] ->
	  case refac_syntax:type(E2) of
	      atom -> case refac_syntax:atom_value(E2) of
			  OldName ->
			      case try_eval(FileName, E1, SearchPaths, TabWidth) of
				  {value, ModName} ->
				      Pid ! {add_renamed, {FileName, Node}},
				      {rewrite(Node, refac_syntax:tuple([E1, rewrite(E2, refac_syntax:atom(NewName))])),
				       true};
				  _ -> {Node, false}
			      end;
			  _ -> {Node, false}
		      end;
	      _ -> {Node, false}
	  end;
      [E1, E2, E3] ->
	  case refac_syntax:type(E3) of
	    atom -> case refac_syntax:atom_value(E3) of
		      OldName ->
			  case try_eval(FileName, E2, SearchPaths, TabWidth) of
			    {value, ModName} ->
				Pid ! {add_renamed, {FileName, Node}},
				{rewrite(Node, refac_syntax:tuple([E1, E2, rewrite(E3, refac_syntax:atom(NewName))])),
				 true};
			    _ -> {Node, false}
			  end;
		      _ -> {Node, false}
		    end;
	    _ -> 
		  case refac_syntax:type(E3) == integer andalso refac_syntax:integer_value(E3) == Arity orelse
		      refac_syntax:type(E3) == list andalso refac_syntax:list_length(E3) == Arity orelse
		      refac_syntax:type(E3) == nil andalso Arity == 0
		      of
		   true ->
		       case refac_syntax:type(E2) of
			 atom -> case refac_syntax:atom_value(E2) of
				   OldName ->
				       case try_eval(FileName, E1, SearchPaths, TabWidth) of
					 {value, ModName} ->
					     Pid ! {add_renamed, {FileName, Node}},
					     {rewrite(Node, refac_syntax:tuple([E1, rewrite(E2, refac_syntax:atom(NewName)), E3])), true};
					 _ -> {Node, false}
				       end;
				   _ -> {Node, false}
				 end;
			 _ -> {Node, false}
		       end;
		   _ -> {Node, false}
		 end
	  end;
      [E1, E2, E3, E4] ->
	  case refac_syntax:type(E3) of
	    atom -> case refac_syntax:atom_value(E3) of
		      OldName ->
			  case try_eval(FileName, E2, SearchPaths, TabWidth) of
			    {value, ModName} ->
				  case refac_syntax:type(E4) == list andalso refac_syntax:list_length(E4) == Arity orelse
				       refac_syntax:type(E4) == nil andalso Arity == 0
				    of
				  true ->
				      Pid ! {add_renamed, {FileName, Node}},
				      {rewrite(Node, refac_syntax:tuple([E1, E2, rewrite(E3, refac_syntax:atom(NewName)), E4])),
				       true};
				  _ ->
				      {Node, false}
				end;
			    _ -> {Node, false}
			  end;
		      _ -> {Node, false}
		    end;
	    _ -> {Node, false}
	  end;
      _ -> {Node, false}
    end.


try_eval(FileName, Node, SearchPaths, TabWidth) ->
    try erl_eval:exprs([refac_syntax:revert(Node)],[]) of
	{value, Val, _} -> {value, Val}
    catch _:_ ->
	    case has_macros(Node) andalso (refac_util:get_free_vars(Node)==[]) of 
		true ->
		    Dir = filename:dirname(FileName),
		    DefaultIncl2 = [filename:join(Dir, X) || X <-refac_util:default_incls()],
		    NewSearchPaths= SearchPaths++DefaultIncl2,
		    {Ms, UMs} = case refac_epp:parse_file(FileName, NewSearchPaths, [])  of 
				    {ok, _, {Defs, Uses}} -> 
					{dict:from_list(Defs), dict:from_list(Uses)};		  	 
				    _ -> []
				end, 
		    NodeToks = get_toks(FileName, Node, TabWidth),
		    try refac_epp:expand_macros(NodeToks, {Ms, UMs}) of
			NewToks when is_list(NewToks)->
			    case refac_parse:parse_exprs(NewToks++[{dot,{999,0}}]) of 
				 {ok, Exprs} ->
				    try erl_eval:exprs(Exprs, []) of 
					{value, Val, _} -> {value, Val}
				    catch
					_:_ -> {error, no_value}
				    end;
				_ -> {error, no_value}
			    end
		    catch
			_:__ -> {error, no_value}
		    end;
		false ->
		    {error, no_value}
	    end
    end.
    

has_macros(Node) ->
    F = fun(N, _Others) ->
		case refac_syntax:type(N) of
		    macro -> {N, true};
		    _ -> {[], false}
		end
	end,			    
    {_, Res}= refac_util:once_tdTU(F, Node, []),
    Res.

get_toks(FileName, Node, TabWidth) ->
    Toks = refac_util:tokenize(FileName, false, TabWidth),
    {StartPos, EndPos} = refac_util:get_range(Node),
    Toks1 = lists:dropwhile(fun(T) ->
				    token_loc(T) < StartPos 
			    end, Toks),
    lists:takewhile(fun(T) ->
			    token_loc(T) =< EndPos 
		    end, Toks1).
    

token_loc(T) ->
    case T of
	{_, L, _V} -> L;
	{_, L1} -> L1
    end.

test_framework_aware_name_checking(UsedFrameWorks, OldFunName, Arity, NewFunName) ->
    eunit_name_checking(UsedFrameWorks, OldFunName,Arity, NewFunName),
    eqc_name_checking(UsedFrameWorks, OldFunName, Arity, NewFunName),
    testserver_name_checking(UsedFrameWorks, OldFunName, Arity, NewFunName),
    commontest_name_checking(UsedFrameWorks, OldFunName, Arity, NewFunName).


eunit_name_checking(UsedFrameWorks, OldFunName, Arity, NewFunName) ->
    case lists:member(eunit, UsedFrameWorks) of 
	true when Arity==0 ->
	    eunit_name_checking_1(atom_to_list(OldFunName), (atom_to_list(NewFunName)));
	_ -> ok
    end.
   
eunit_name_checking_1(OldFunName, NewFunName) ->
    case lists:suffix(?DEFAULT_EUNIT_TEST_SUFFIX, OldFunName) of 
	true ->
	    case lists:suffix(?DEFAULT_EUNIT_TEST_SUFFIX, NewFunName) of 
		true ->
		    ok;
		_ -> 
		    throw({warning,"Renaming will make this function no "
			   "longer an EUnit test function, continue?"})
	    end;
	_ -> case lists:suffix(?DEFAULT_EUNIT_GENERATOR_SUFFIX,OldFunName) of 
		 true ->
		     case lists:suffix(?DEFAULT_EUNIT_GENERATOR_SUFFIX, NewFunName) of 
			 true -> 
			     ok;
			 _ -> 
			     throw({warning,"Renaming will make this function no longer "
				    "an EUnit test generator function, continue?"})
		     end;
		 _ ->case lists:suffix(?DEFAULT_EUNIT_TEST_SUFFIX, NewFunName) of 
			 true -> 
			     throw({warning, "Renaming will make this function an "
				    "EUnit test function, continue?"});		     
			 _ ->
			     case lists:suffix(?DEFAULT_EUNIT_GENERATOR_SUFFIX,NewFunName) of 
				 true -> 
				     throw({warning, "Renaming will make this function an "
					    "EUnit test geneator function, continue?"});
				  _ -> 
				     ok
			     end
		     end
	     end
    end.

eqc_name_checking(UsedFrameWorks, OldFunName, Arity, NewFunName) ->
    case lists:member(eqc_statem, UsedFrameWorks) of 
	true -> 
	    eqc_statem_name_checking(OldFunName, Arity, NewFunName);
	false -> ok
    end,
    case lists:member(eqc,UsedFrameWorks) of 
	true -> 
	    eqc_name_checking(atom_to_list(OldFunName), Arity, atom_to_list(NewFunName));
	false -> ok
    end.
   
eqc_name_checking(OldFunName, _Arity, NewFunName) ->
    case lists:prefix("prop_", OldFunName) of 
	true -> 
	    case lists:prefix("prop_", NewFunName) of 
		true -> 
		    ok;
		false -> 
		    throw({warning, "Renaming will make this function no longer a quickcheck property, continue?"})
	    end;
	false ->
	    case lists:prefix("prop_", NewFunName) of 
		true ->
		    throw({warning, "Renaming will make this function a quickcheck property, continue?"});
		false -> ok
	    end
    end.
	    


eqc_statem_name_checking(OldFunName, Arity, NewFunName) ->
    case lists:member({OldFunName, Arity}, eqc_statem_callback_funs()) of 
	true ->
	    throw({warning, "The function selected is a callback function, continue?"});
	false -> 
	    case lists:member({NewFunName, Arity}, eqc_statem_callback_funs()) of 
		true ->
		    throw({warning, "The new function would be a quickcheck callback function, continue?"});
		false -> ok
	    end
    end.
		 
testserver_name_checking(UsedFrameWorks, OldFunName, Arity, NewFunName) ->
    case lists:member(testserver, UsedFrameWorks) of 
	true ->
	    testserver_name_checking(OldFunName, Arity, NewFunName);
	false -> ok
    end.

testserver_name_checking(OldFunName, Arity, NewFunName) ->
    case lists:member({OldFunName, Arity}, testserver_callback_funs()) of 
	true ->
	    throw({warning, "The function selected is mandatory in a Test Server test suite, continue?"});
	false -> 
	    case lists:member({NewFunName, Arity}, testserver_callback_funs()) of 
		true ->
		    throw({warning, "The new function would be Test Server special function, continue?"});
		false -> ok
	    end
    end. 	 
									    
commontest_name_checking(UsedFrameWorks, OldFunName, Arity, NewFunName) ->
    case lists:member(commontest, UsedFrameWorks) of 
	true ->
	    commontest_name_checking(OldFunName, Arity, NewFunName);
	false -> ok
    end.
commontest_name_checking(OldFunName, Arity, NewFunName) ->
    case lists:member({OldFunName, Arity}, commontest_callback_funs()) of 
	true ->
	    throw({warning, "The function selected is a Common Test callback function, continue?"});
	false ->
	    case lists:member({NewFunName, Arity}, commontest_callback_funs()) of 
		true ->
		    throw({warning, "The new function would be a Common Test callback function, continue?"});
		false -> ok
	    end
    end.
	    

eqc_statem_callback_funs()->
    [{initial_state, 0}, {precondition, 2}, {command,1}, {postcondition, 3}, {next_state,3}].
commontest_callback_funs() ->
    [{all,0}, {groups, 0}, {suite, 0},{init_per_suite, 1}, {end_per_suite, 1},{init_per_group,2},
     {end_per_group, 2}, {init_per_testcase,2},{end_per_testcase, 2}, {testcase, 0},{testcase, 1}].
testserver_callback_funs() ->
    [{all, 0}, {init_per_suite,1}, {end_per_suite,1},{init_per_testcase,2}, {fin_per_testcase,2}].

apply_style_funs() ->
    [{{erlang, apply, 3}, [modulenmae, functionname, arglist], term},
     {{erlang, spawn, 3}, [modulename, functionname, arglist], term},
     {{erlang, spawn, 4}, [node, modulename, functionname, arglist], term},
     {{erlang, spawn_link, 3}, [modulename, functionname, arglist], term},
     {{erlang, spawn_link, 4}, [term, modulename, functioname, arglist], term},
     {{erlang, spawn_monitor, 3}, [term, modulename,functionname, arglist], term},
     {{test_server, timecall, 3}, [modulename, functionname, arglist], term},
     {{test_server, do_times, 4}, [integer, modulename, functionname, arglist], term},
     {{test_server, call_crash, 3}, [modulename, functionname, arglist], term},
     {{test_server, call_crash, 4}, [term, modulename, functionname, arglist], term},
     {{test_server, call_crash, 5}, [term, term, modulename, functionname, arglist], term}].



renamed_warn_msg(FunName) ->
    "\n=================================================================================\n"
	"WARNING: Wrangler has renamed the uses of '"++atom_to_list(FunName)++
	"' within the following expressions while without enough "
	"syntactic/semantic information. Please check manually!\n".
     

not_renamed_warn_msg(FunName) ->
    "\n=================================================================================\n"
	"WARNING: Wrangler could not infer whether the uses of '" ++ atom_to_list(FunName)++ 
	"' at the following positions refer to the function renamed, and they are not renamed."
	" Please check manually!\n".



start_atom_process() ->
    spawn_link(fun()-> atom_loop({[],[]}) end).

stop_atom_process(Pid) ->
    Pid ! stop.

atom_loop({NotRenamed, Renamed}) ->
    receive
	{add_not_renamed, Data} ->
	    atom_loop({[Data|NotRenamed], Renamed});
	{add_renamed, {FileName, Pos}} ->
	    case Renamed of
		[] -> atom_loop({NotRenamed, [{FileName, [Pos]}]});
		[{FileName, Ps}|Others] ->
		    atom_loop({NotRenamed, [{FileName, [Pos|Ps]}|Others]});
		_ -> atom_loop({NotRenamed, [{FileName,[Pos]}|Renamed]})
	    end;		    
	{From, get} ->
	    From ! {self(), {lists:reverse(NotRenamed), lists:reverse(Renamed)}},
	    atom_loop({NotRenamed, Renamed});
	stop ->
	    ok
    end.

output_atom_warning_msg(Pid,NotRenamedWarnMsg, RenamedWarnMsg) ->
    Pid ! {self(), get},
    receive
	{Pid, {NotRenamed, Renamed}} ->
	    output_atom_warnings({NotRenamed, Renamed}, NotRenamedWarnMsg,RenamedWarnMsg);
	_ -> throw({error, "Refactoring failed because of a Wrangler error."})
    end.

    
output_atom_warnings({[], []}, _, _) ->
    ok;
output_atom_warnings({NotRenamed, Renamed}, NotRenamedMsg, RenamedMsg) ->
    output_atom_not_renamed_warnings(NotRenamed, NotRenamedMsg),
    output_atom_renamed_warnings(Renamed,  RenamedMsg).

output_atom_not_renamed_warnings([],  _Msg) -> ok;
output_atom_not_renamed_warnings(NotRenamed, Msg) ->
    ?wrangler_io(Msg,[]),
    output_not_renamed_atom_info(NotRenamed).

output_atom_renamed_warnings([],  _Msg) -> ok;
output_atom_renamed_warnings(Renamed, Msg) ->
    ?wrangler_io(Msg,[]),
    output_renamed_atom_info(Renamed).

output_not_renamed_atom_info(FileAndPositions) ->
    Fun = fun({FileName, Positions}) ->
		  lists:flatmap(fun(P) ->
					{Line, _Col} = P,
					FileName++io_lib:format(":~p: \n",[Line])
				end, Positions)
	  end,
    Msg = lists:flatmap(Fun, FileAndPositions),
    ?wrangler_io(Msg, []).

output_renamed_atom_info(FileAndExprs) ->
    Fun = fun({FileName, Exprs}) ->
		  Fun0 =fun(Expr) ->
				{Line, _Col} = refac_syntax:get_pos(Expr),
				FileName++io_lib:format(":~p: ", [Line])
				    ++ refac_prettypr:format(Expr)++"\n"
			end,		  
		  lists:flatmap(Fun0, Exprs)
	  end,
    Msg = lists:flatmap(Fun, FileAndExprs),
    ?wrangler_io(Msg, []).
    
rewrite(E1, E2) ->
    refac_syntax:copy_pos(E1, refac_syntax:copy_attrs(E1, E2)).

