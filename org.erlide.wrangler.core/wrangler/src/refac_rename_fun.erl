%% Copyright (c) 2010, Huiqing Li, Simon Thompson
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
%% Notes about being testing-framework-aware:
%% EUNIT: Issue a warning message when renaming a non-test function to a test function and vice versa;
%% EQC: Issue a warning when renaming callback functions, also handld the use of 'call' quardruples.
%% CommonTest, Issue a warning message when renaing a non-test function to a test function and vice versa.

%% @private
-module(refac_rename_fun).
 
-export([rename_fun/7, rename_fun_1/6, rename_fun_1/7,
         rename_fun_eclipse/6, rename_fun_1_eclipse/6,
         get_fun_name/5]).

-export([rename_fun_by_name/6, rename_fun_by_name/7]).

-include("../include/wrangler_internal.hrl").


%%-spec(rename_fun_eclipse/6::(string(), integer(), integer(), string(), [dir()], integer()) ->
%%				  {error, string()} | {warning, string()} | {ok, [{filename(), filename(), string()}]}).
rename_fun_eclipse(FileName, Line, Col, NewName, SearchPaths, TabWidth) ->
    rename_fun(FileName, Line, Col, NewName, SearchPaths, eclipse, TabWidth).

rename_fun_by_name(ModOrFileName, {OldFunName, Arity}, NewFunName, SearchPaths, Editor, TabWidth) ->
    rename_fun_by_name(ModOrFileName, OldFunName, Arity, NewFunName, SearchPaths, Editor, TabWidth).

%%-spec(rename_fun_command/7::(modulename()|filename(), atom(), integer(), atom(),[dir()], atom(), integer())->
%%				       {error, string()} | {ok, [filename()]}).
rename_fun_by_name(ModOrFileName, OldFunName, Arity, NewFunName, SearchPaths, Editor, TabWidth) ->
    OldFunNameStr = case is_atom(OldFunName) of
			true -> atom_to_list(OldFunName);
                        false when is_list(OldFunName) ->
                            OldFunName;
			false -> throw({error, "Original function name should be an atom."})
		    end,
    NewFunNameStr = case is_atom(NewFunName) of
			true -> atom_to_list(NewFunName);
			false when is_list(NewFunName) ->
                            NewFunName;
                        false ->
                            throw({error, "New function name should be an atom or a string."})
		    end,
    case is_integer(Arity) of
	true -> ok;
	false -> throw({error, "Arity should be an integer."})
    end,
     case api_refac:is_fun_name(NewFunNameStr) of
	true -> ok;
	false -> throw({error, "Invalid new function name!"})
    end,
    case filelib:is_file(ModOrFileName) of
	true ->
	    rename_fun_command_1(ModOrFileName, OldFunNameStr, Arity,
				 NewFunNameStr, SearchPaths, TabWidth, Editor);
	false ->
	    case is_atom(ModOrFileName) of
		true ->
		    case wrangler_misc:modname_to_filename(ModOrFileName, SearchPaths) of
			{ok, FileName} ->
			    rename_fun_command_1(FileName, OldFunNameStr, Arity,
						 NewFunNameStr, SearchPaths, TabWidth, Editor);
			{error, Msg} ->
			    {error, lists:flatten(Msg)}
		    end;
		false ->
		    {error, "Invalid parameters!"}
	    end
    end.

rename_fun_command_1(FileName, OldFunName, Arity, NewFunName, SearchPaths, TabWidth, Editor) ->
    {ok, {AnnAST, Info}} = wrangler_ast_server:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    OldFunNameAtom = list_to_atom(OldFunName),
    NewFunNameAtom = list_to_atom(NewFunName),
    {ok, ModName} = get_module_name(Info),
    case wrangler_misc:funname_to_defpos(AnnAST, {ModName, OldFunNameAtom, Arity}) of
	{ok, DefPos} ->
	    case pre_cond_check_command(Info, NewFunNameAtom, ModName, OldFunNameAtom, Arity) of
		ok ->
		    rename_fun_0(FileName, {AnnAST, Info}, {ModName, OldFunNameAtom, Arity}, {DefPos, NewFunNameAtom},
				 SearchPaths, TabWidth, Editor, "");
		Others -> Others
	    end;
	{error, Reason} ->
	    throw({error, Reason})
    end.

pre_cond_check_command(Info, NewFunNameAtom, ModName, OldFunNameAtom, Arity) ->
    DefinedFuns = [{F, A} || {M, F, A} <- api_refac:inscope_funs(Info), M == ModName],
    InscopeFuns = [{F, A} || {_M, F, A} <- api_refac:inscope_funs(Info)],
    case  not  lists:member({OldFunNameAtom, Arity}, DefinedFuns) of
	true ->
            Msg=io_lib:format("The function specified does not exist: ~p!", 
                              [{OldFunNameAtom, Arity}]),
	    {error, lists:flatten(Msg)};
	false ->
	    case lists:member({NewFunNameAtom, Arity}, InscopeFuns) orelse 
		   erl_internal:bif(erlang, NewFunNameAtom, Arity)
		of
		true ->
		    {error, lists:flatten(atom_to_list(NewFunNameAtom) ++ "/" ++ 
					    integer_to_list(Arity) ++ " is already in scope, "
								      "or is an auto-imported builtin function.")};
		false ->
		    ok
	    end
    end.

rename_fun(FileName, Line, Col, NewName, SearchPaths, Editor, TabWidth) ->
     ?wrangler_io("\nCMD: ~p:rename_fun( ~p, ~p, ~p, ~p,~p, emacs, ~p).\n",
		  [?MODULE, FileName, Line, Col, NewName, SearchPaths, TabWidth]),
     Cmd1 = "CMD: " ++ atom_to_list(?MODULE) ++ ":rename_fun(" ++ "\"" ++
	      FileName ++ "\", " ++ integer_to_list(Line) ++
	        ", " ++ integer_to_list(Col) ++ ", " ++ "\"" ++ NewName ++ "\","
      ++ "[" ++ wrangler_misc:format_search_paths(SearchPaths) ++ "]," ++ integer_to_list(TabWidth) ++ ").",
    case api_refac:is_fun_name(NewName) of
	true -> ok;
	false -> throw({error, "Invalid new function name!"})
    end,
    {ok, {AnnAST, Info}} = wrangler_ast_server:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    NewNameAtom = list_to_atom(NewName),
    {ok, ModName} = get_module_name(Info),
    case api_interface:pos_to_fun_name(AnnAST, {Line, Col}) of
	{ok, {Mod, Fun, Arity, _, DefinePos}} ->
	    case {ModName, NewNameAtom} =/= {Mod, Fun} of
		true ->
		    case pre_cond_check(FileName, Info, NewNameAtom, Mod, Fun, Arity) of
			ok ->
			    rename_fun_0(FileName, {AnnAST, Info}, {Mod, Fun, Arity}, {DefinePos, NewNameAtom},
					 SearchPaths, TabWidth, Editor, Cmd1);
			Others -> Others
		    end;
		_ ->
		    case Editor of
			emacs -> {ok, [], false};
			eclipse ->
			    FileContent = wrangler_prettypr:print_ast(wrangler_misc:file_format(FileName), AnnAST, TabWidth),
			    {ok, [{FileName, FileName, FileContent}]}
		    end
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.

rename_fun_0(FileName, {AnnAST, Info}, {Mod, OldFunNameAtom, Arity}, {DefinePos, NewNameAtom},
	     SearchPaths, TabWidth, Editor, Cmd) ->
    NewNameStr = atom_to_list(NewNameAtom),
    Pid = wrangler_atom_utils:start_atom_process(),
    ?wrangler_io("The current file under refactoring is:\n~p\n", [FileName]),
    {AnnAST1, _C} = do_rename_fun(AnnAST, {Mod, OldFunNameAtom, Arity}, {DefinePos, NewNameAtom}),
    wrangler_atom_utils:check_unsure_atoms(FileName, AnnAST1, [OldFunNameAtom], {f_atom, {Mod, OldFunNameAtom, Arity}}, Pid),
    case api_refac:is_exported({OldFunNameAtom, Arity}, Info) of
	true ->
	    ?wrangler_io("\nChecking possible client modules in the following search paths: \n~p\n", [SearchPaths]),
	    ClientFiles = wrangler_modulegraph_server:get_client_files(FileName, SearchPaths),
	    try
		rename_fun_in_client_modules(ClientFiles, {Mod, OldFunNameAtom, Arity},
					     NewNameStr, SearchPaths, TabWidth, Pid)
	    of
		Results ->
		    HasWarningMsg = wrangler_atom_utils:has_warning_msg(Pid),
		    wrangler_atom_utils:output_atom_warning_msg(Pid, not_renamed_warn_msg(OldFunNameAtom), renamed_warn_msg(OldFunNameAtom)),
		    wrangler_atom_utils:stop_atom_process(Pid),
		    Res=wrangler_write_file:write_refactored_files([{{FileName, FileName}, AnnAST1}| Results],
                                                                   HasWarningMsg, Editor, TabWidth, Cmd),
                    case Editor of 
                        composite_emacs ->
                            {Res, {name_change, [{{Mod, OldFunNameAtom, Arity}, {Mod, NewNameAtom, Arity}}]}};
                        _ -> Res
                    end
	    catch
		throw:Err ->
		    wrangler_atom_utils:stop_atom_process(Pid),
		    Err
	    end;
	false ->
	    HasWarningMsg = wrangler_atom_utils:has_warning_msg(Pid),
            wrangler_atom_utils:output_atom_warning_msg(Pid, not_renamed_warn_msg(OldFunNameAtom), renamed_warn_msg(OldFunNameAtom)),
	    wrangler_atom_utils:stop_atom_process(Pid),
	    Res=wrangler_write_file:write_refactored_files([{{FileName, FileName}, AnnAST1}],
                                                           HasWarningMsg, Editor, TabWidth, Cmd),
            case Editor of 
                composite_emacs ->
                    {Res, {name_change, [{{Mod, OldFunNameAtom, Arity}, {Mod, NewNameAtom, Arity}}]}};
                _ ->
                    Res
            end
    end.

%%-spec(rename_fun_1/6::(string(), integer(), integer(), string(), [dir()], integer()) ->
%%	     {error, string()} | {ok, [filename()]}).
rename_fun_1(FileName, Line, Col, NewName, SearchPaths, TabWidth) ->
    rename_fun_1(FileName, Line, Col, NewName, SearchPaths, emacs, TabWidth).
    
%%-spec(rename_fun_1_eclipse/6::(string(), integer(), integer(), string(), [dir()], integer()) ->
%%	     {error, string()} | {ok, [filename()]}).

rename_fun_1_eclipse(FileName, Line, Col, NewName, SearchPaths, TabWidth) ->
    rename_fun_1(FileName, Line, Col, NewName, SearchPaths, eclipse, TabWidth).

rename_fun_1(FileName, Line, Col, NewName, SearchPaths, Editor, TabWidth) ->
     Cmd = "CMD: " ++ atom_to_list(?MODULE) ++ ":rename_fun_1(" ++ "\"" ++
	     FileName ++ "\", " ++ integer_to_list(Line) ++
	       ", " ++ integer_to_list(Col) ++ ", " ++ "\"" ++ NewName ++ "\","
      ++ "[" ++ wrangler_misc:format_search_paths(SearchPaths) ++ "]," ++ integer_to_list(TabWidth) ++ ").",
    {ok, {AnnAST, Info}} = wrangler_ast_server:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    NewName1 = list_to_atom(NewName),
    {ok, {Mod, Fun, Arity, _, DefinePos}} = api_interface:pos_to_fun_name(AnnAST, {Line, Col}),
    ?wrangler_io("The current file under refactoring is:\n~p\n", [FileName]),
    Pid = wrangler_atom_utils:start_atom_process(),
    {AnnAST1, _C} = do_rename_fun(AnnAST, {Mod, Fun, Arity}, {DefinePos, NewName1}),
    case api_refac:is_exported({Fun, Arity}, Info) of
	true ->
	    ?wrangler_io("\nChecking client modules in the following search paths: \n~p\n", [SearchPaths]),
	    ClientFiles = wrangler_modulegraph_server:get_client_files(FileName, SearchPaths),
	    try
		rename_fun_in_client_modules(ClientFiles, {Mod, Fun, Arity}, NewName, SearchPaths, TabWidth, Pid)
	    of
		Results ->
		    HasWarningMsg = wrangler_atom_utils:has_warning_msg(Pid),
		    wrangler_atom_utils:output_atom_warning_msg(Pid, not_renamed_warn_msg(Fun), renamed_warn_msg(Fun)),
		    wrangler_atom_utils:stop_atom_process(Pid),
		    wrangler_write_file:write_refactored_files([{{FileName, FileName}, AnnAST1}| Results],
							       HasWarningMsg, Editor, TabWidth, Cmd)
	    catch
		throw:Err -> Err
	    end;
	false ->
	    HasWarningMsg = wrangler_atom_utils:has_warning_msg(Pid),
	    wrangler_atom_utils:output_atom_warning_msg(Pid, not_renamed_warn_msg(Fun), renamed_warn_msg(Fun)),
	    wrangler_atom_utils:stop_atom_process(Pid),
	    wrangler_write_file:write_refactored_files([{{FileName, FileName}, AnnAST1}],
						       HasWarningMsg, Editor, TabWidth, Cmd)
    end.

pre_cond_check(FileName, Info, NewFunName, OldFunDefMod, OldFunName, Arity) ->
    {ok, ModName} = get_module_name(Info),
    Inscope_Funs = [{F, A} || {_M, F, A} <- api_refac:inscope_funs(Info)],
    if OldFunDefMod == ModName ->
	   case lists:member({NewFunName, Arity}, Inscope_Funs) orelse 
		  erl_internal:bif(erlang, NewFunName, Arity)
	       of
	       true ->
		   {error, atom_to_list(NewFunName) ++ "/" ++ 
			     integer_to_list(Arity) ++ " is already in scope, "
						       "or is an auto-imported builtin function."};
	       _ ->
		   case wrangler_misc:is_callback_fun(Info, OldFunName, Arity) of
		       true ->
			   {warning, "The function to be renamed is a callback function, continue?"};
		       _ -> TestFrameWorkUsed = wrangler_misc:test_framework_used(FileName),
			    case TestFrameWorkUsed of
				[] -> ok;
				_ ->
				    catch test_framework_aware_name_checking(
					    TestFrameWorkUsed, OldFunName, Arity, NewFunName)
			    end
		   end
	   end;
       true ->
	   {error, "This function is not defined in this module; "
		   "a function can only be renamed from the module where it is defined."}
    end.

do_rename_fun(Tree, {ModName, OldName, Arity}, {DefinePos, NewName}) ->
    api_ast_traverse:full_tdTP(fun do_rename_fun_1/2, Tree,
			       {{ModName, OldName, Arity}, {DefinePos, NewName}}).

do_rename_fun_1(Tree, {{M, OldName, Arity}, {DefinePos, NewName}}) ->
    case wrangler_syntax:type(Tree) of
      function ->
	    case get_fun_def_loc(Tree) of
		DefinePos ->
		    N = wrangler_syntax:function_name(Tree),
		    Cs = wrangler_syntax:function_clauses(Tree),
		    N1 = wrangler_syntax:copy_attrs(N, wrangler_syntax:atom(NewName)),
		    {rewrite(Tree, wrangler_syntax:function(N1, Cs)), true};
		_ -> {Tree, false}
	    end;
        attribute ->
          case api_spec:is_type_spec(Tree, {M, OldName, Arity}) of
              true ->
                  NewTree=api_spec:rename_in_spec(Tree, {M, NewName, Arity}),
                  {NewTree, true};
              false ->
                  case api_spec:is_type_spec(Tree, {OldName, Arity}) of
                      true ->
                          NewTree=api_spec:rename_in_spec(Tree, {NewName, Arity}),
                          {NewTree, true};
                      _ ->
                          {Tree, false}
                  end
          end;
      application ->
     do_rename_in_fun_app(Tree, M, OldName, Arity, NewName);
      arity_qualifier ->
	  do_rename_fun_in_arity_qualifier(Tree, M, OldName, Arity, NewName);
      atom ->
	As = wrangler_syntax:get_ann(Tree),
	case lists:keysearch(type, 1, As) of
	    {value, {type, {f_atom, [M, OldName, Arity]}}} ->
		{wrangler_syntax:copy_attrs(Tree, wrangler_syntax:atom(NewName)), true};
	    _ -> {Tree, false}
	end;
      _ -> {Tree, false}
    end.

do_rename_in_fun_app(Tree, M, OldName, Arity, NewName) ->
    Operator = wrangler_syntax:application_operator(Tree),
    Arguments = wrangler_syntax:application_arguments(Tree),
    case wrangler_syntax:type(Operator) of
      atom ->
	  case get_fun_def_info(Operator) of
	    {M, OldName, Arity, _} ->
		Operator1 = rewrite(Operator, wrangler_syntax:atom(NewName)),
		Tree1 = rewrite(Tree, wrangler_syntax:application(Operator1, Arguments)),
		{Tree1, true};
	    _ -> {Tree, false}
	  end;
      module_qualifier ->
	  Mod = wrangler_syntax:module_qualifier_argument(Operator),
	  Fun = wrangler_syntax:module_qualifier_body(Operator),
	  case get_fun_def_info(Operator) of
	    {M, OldName, Arity, _} ->
		Fun2 = rewrite(Fun, wrangler_syntax:atom(NewName)),
		%% Need to change the fun_def annotation as well?
		Operator1 = rewrite(Operator, wrangler_syntax:module_qualifier(Mod, Fun2)),
		Tree1 = rewrite(Tree, wrangler_syntax:application(Operator1, Arguments)),
		{Tree1, true};
	    _ -> {Tree, false}
	  end;
      tuple ->
	  case wrangler_syntax:tuple_elements(Operator) of
	    [Mod, Fun] ->
		case get_fun_def_info(Operator) of
		  {M, OldName, Arity, _} ->
		      Fun2 = rewrite(Fun, wrangler_syntax:atom(NewName)),
		      %% Need to change the fun_def annotation as well?
		      Operator1 = rewrite(Operator, wrangler_syntax:tuple([Mod, Fun2])),
		      Tree1 = rewrite(Tree, wrangler_syntax:application(Operator1, Arguments)),
		      {Tree1, true};
		  _ -> {Tree, false}
		end;
	    _ -> {Tree, false}
	  end;
      _ -> {Tree, false}
    end.

get_fun_def_loc(Node) ->
    As = wrangler_syntax:get_ann(Node),
    case lists:keysearch(fun_def, 1, As) of
      {value, {fun_def, {_M, _N, _A, _P, DefinePos}}} ->
	  DefinePos;
      _ -> ?DEFAULT_LOC
    end.

get_fun_def_mod(Node) ->
    As = wrangler_syntax:get_ann(Node),
    case lists:keysearch(fun_def, 1, As) of
      {value, {fun_def, {M, _N, _A, _P, _DefinePos}}} -> M;
      _ -> '_'
    end.

rename_fun_in_client_modules(Files, {Mod, Fun, Arity}, NewNameStr, SearchPaths, TabWidth, Pid) ->
    case Files of
	[] -> [];
	[F| Fs] ->
	    ?wrangler_io("The current file under refactoring is:\n~p\n", [F]),
	    {ok, {AnnAST, Info}} = wrangler_ast_server:parse_annotate_file(F, true, SearchPaths, TabWidth),
	    {AnnAST1, Changed} = rename_fun_in_client_module_1({AnnAST, Info}, {Mod, Fun, Arity}, NewNameStr),
            wrangler_atom_utils:check_unsure_atoms(F, AnnAST1, [Fun], {f_atom, {Mod, Fun, Arity}}, Pid),
	    if Changed ->
		   [{{F, F}, AnnAST1}| rename_fun_in_client_modules(
					 Fs, {Mod, Fun, Arity}, NewNameStr, SearchPaths, TabWidth, Pid)];
	       true ->
		   rename_fun_in_client_modules(
		     Fs, {Mod, Fun, Arity}, NewNameStr, SearchPaths, TabWidth, Pid)
	    end
    end.

get_fun_def_info(Node) ->
    As = wrangler_syntax:get_ann(Node),
    case lists:keysearch(fun_def, 1, As) of
      {value, {fun_def, {Mod, FunName, Arity, _UsePos, DefinePos}}} ->
	  {Mod, FunName, Arity, DefinePos};
      _ -> false
    end.

rename_fun_in_client_module_1({Tree, Info}, {M, OldName, Arity}, NewNameStr) ->
    NewNameAtom = list_to_atom(NewNameStr),
    case lists:keysearch(module, 1, Info) of
	{value, {module, ClientModName}} ->
	    Inscope_Funs = [{F, A} || {_M, F, A} <- api_refac:inscope_funs(Info)],
	    case lists:member({NewNameAtom, Arity}, Inscope_Funs)
		    and lists:member({OldName, Arity}, Inscope_Funs)
		of
		true ->
		    throw({error, "The new function name causes confliction in module '"
				     ++ atom_to_list(ClientModName) ++ "'"});
		_ -> ok
	    end;
	_ -> ok
    end,
    api_ast_traverse:full_tdTP(fun do_rename_fun_in_client_module_1/2, Tree,
			       {{M, OldName, Arity}, NewNameAtom}).

do_rename_fun_in_client_module_1(Tree,  {{M, OldName, Arity}, NewName}) ->
    case wrangler_syntax:type(Tree) of
        attribute ->
            case api_spec:is_type_spec(Tree, {M, OldName, Arity}) of
                true ->
                    NewTree=api_spec:rename_in_spec(Tree, {M, NewName, Arity}),
                    {NewTree, true};
                false ->
                    {Tree, false}
            end;
        application -> do_rename_in_fun_app(Tree, M, OldName, Arity, NewName);
        arity_qualifier ->   %% is there a module name?
            do_rename_fun_in_arity_qualifier(Tree, M, OldName, Arity, NewName);
        atom ->
            As = wrangler_syntax:get_ann(Tree),
            case lists:keysearch(type, 1, As) of
                {value, {type, {f_atom, [M, OldName, Arity]}}} ->
                    {wrangler_syntax:copy_attrs(Tree, wrangler_syntax:atom(NewName)), true};
                _ -> {Tree, false}
            end;
        _ -> {Tree, false}
    end.

do_rename_fun_in_arity_qualifier(Tree, M, OldName, Arity, NewName) ->
    Fun = wrangler_syntax:arity_qualifier_body(Tree),
    Fun_Name = wrangler_syntax:atom_value(Fun),
    Arg = wrangler_syntax:arity_qualifier_argument(Tree),
    Arg1 = wrangler_syntax:integer_value(Arg),
    DefMod = get_fun_def_mod(Fun),
    if
      (Fun_Name == OldName) and (Arg1 == Arity) and (DefMod == M) ->
	  {rewrite(Tree, wrangler_syntax:arity_qualifier(wrangler_syntax:atom(NewName), Arg)),
	   true};
      true -> {Tree, false}
    end.

get_module_name(ModInfo) ->
    case lists:keysearch(module, 1, ModInfo) of
      {value, {module, ModName}} -> {ok, ModName};
      false -> throw({error, "Wrangler could not infer the current module name; "
		      "please check whether the module name is consistent with file name."})
    end.

test_framework_aware_name_checking(UsedFrameWorks, OldFunName, Arity, NewFunName) ->
    eunit_name_checking(UsedFrameWorks, OldFunName, Arity, NewFunName),
    eqc_name_checking(UsedFrameWorks , OldFunName, Arity, NewFunName),
    testserver_name_checking(UsedFrameWorks, OldFunName, Arity, NewFunName),
    commontest_name_checking(UsedFrameWorks, OldFunName, Arity, NewFunName).

eunit_name_checking(UsedFrameWorks, OldFunName, Arity, NewFunName) ->
    case lists:member(eunit, UsedFrameWorks) of
      true when Arity == 0 ->
	  eunit_name_checking_1(atom_to_list(OldFunName), atom_to_list(NewFunName));
      _ -> ok
    end.

eunit_name_checking_1(OldFunName, NewFunName) ->
    case lists:suffix(?DEFAULT_EUNIT_TEST_SUFFIX, OldFunName) of
      true ->
	  case lists:suffix(?DEFAULT_EUNIT_TEST_SUFFIX, NewFunName) of
	    true ->
		ok;
	    _ ->
		throw({warning, "Renaming will make this function no "
				"longer an EUnit test function, continue?"})
	  end;
      _ -> case
	     lists:suffix(?DEFAULT_EUNIT_GENERATOR_SUFFIX, OldFunName)
	       of
	     true ->
		 case
		   lists:suffix(?DEFAULT_EUNIT_GENERATOR_SUFFIX, NewFunName)
		     of
		   true ->
		       ok;
		   _ ->
		       throw({warning, "Renaming will make this function no longer "
				       "an EUnit test generator function, continue?"})
		 end;
	     _ -> case
		    lists:suffix(?DEFAULT_EUNIT_TEST_SUFFIX, NewFunName)
		      of
		    true ->
			throw({warning, "Renaming will make this function an "
					"EUnit test function, continue?"});
		    _ ->
			case
			  lists:suffix(?DEFAULT_EUNIT_GENERATOR_SUFFIX, NewFunName)
			    of
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
    case lists:member(eqc_statem, UsedFrameWorks) andalso  not  lists:member(eqc_fsm, UsedFrameWorks) of
	true ->
	    eqc_callback_name_checking(OldFunName, Arity, NewFunName, wrangler_misc:eqc_statem_callback_funs());
	false -> ok
    end,
    case lists:member(eqc_fsm, UsedFrameWorks) of
	true ->
	    eqc_callback_name_checking(OldFunName, Arity, NewFunName, wrangler_misc:eqc_fsm_callback_funs());
	false -> ok
    end,
    case lists:member(eqc, UsedFrameWorks) of
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

eqc_callback_name_checking(OldFunName, Arity, NewFunName, CallBacks) ->
    case lists:member({OldFunName, Arity}, CallBacks) of
      true ->
	  throw({warning, "The function selected is a callback function, continue?"});
      false ->
	  case lists:member({NewFunName, Arity}, CallBacks) of
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
    case lists:member({OldFunName, Arity}, wrangler_misc:testserver_callback_funs()) of
	true ->
	    throw({warning, "The function selected is mandatory in a Test Server test suite, continue?"});
	false ->
	    case lists:member({NewFunName, Arity}, wrangler_misc:testserver_callback_funs()) of
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
    case lists:member({OldFunName, Arity}, wrangler_misc:commontest_callback_funs()) of
	true ->
	    throw({warning, "The function selected is a Common Test callback function, continue?"});
	false ->
	    case lists:member({NewFunName, Arity}, wrangler_misc:commontest_callback_funs()) of
		true ->
		    throw({warning, "The new function would be a Common Test callback function, continue?"});
		false -> ok
	    end
    end.

renamed_warn_msg(FunName) ->
    "\n=================================================================================\n"
    "WARNING: Wrangler has renamed the uses of '" ++ atom_to_list(FunName) ++
      "' within the following expressions while without enough "
      "syntactic/semantic information. Please check manually!\n".

not_renamed_warn_msg(FunName) ->
    "\n=================================================================================\n"
    "WARNING: Wrangler could not infer whether the uses of '" ++ atom_to_list(FunName) ++
      "' at the following position(line)s refer to the function renamed, and they are not renamed."
      " Please check manually!\n".

rewrite(E1, E2) ->
    wrangler_syntax:copy_pos(E1, wrangler_syntax:copy_attrs(E1, E2)).


%%-spec(get_fun_name/5::(string(), integer(), integer(), [dir()], integer()) -> string().
get_fun_name(FileName, Line, Col, SearchPaths, TabWidth) ->
    {ok, {AnnAST, _Info}} = wrangler_ast_server:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    case api_interface:pos_to_fun_name(AnnAST, {Line, Col}) of
        {ok, {_Mod, Fun, _Arity, _, _DefinePos}} ->
            atom_to_list(Fun);
        _ ->
            ""
    end.
