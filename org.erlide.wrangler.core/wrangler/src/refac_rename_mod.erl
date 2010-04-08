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
%% Refactoring: Rename a module name.
%%
%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%% 
%% =====================================================================
%% @doc Rename a module with a user-supplied new module name.
%% <p> To apply this refactoring, point the cursor to anywhere in the module
%% to be renamed, then select  <em> Rename Module Name </em> from the 
%% <em> Refactor </em> menu, after that, the refactorer will prompt to enter 
%% the new module name in the mini-buffer.
%% </p>
%% <p> This refactoring has a global effect, i.e., it affects all those modules 
%% in which the module to be renamed is imported, or used as a module qualifier.
%% </p>
%% <p>
%% The following <em> side-conditions </em> apply to this refactoring:
%% <li> The new module name should be a fresh name. </li>
%% <li> This refactoring assume that the file basename is always the same as the 
%%  module name, therefore this refactoring changes the filename as well. </li>
%% </p>
%% @end
%% =====================================================================
%% Notes relating to test frameworks: this refactoring check the usage of Quickcheck, 
%% Eunit and CommonTest.
%% QuickCheck: no restrictions on the usage of module name, but need to check the 
%%             usage of 'call' Quadruples.
%% Eunit: Need to issue a warning message when a non-test module name is renamed to 
%%        a test module name; or vice versa; 
%%        Also need to check the existence of the test module, and ask the user whether 
%%        to rename it as well.
%%CommonTest: Need to issue a warning message when a non-test module name is renamed to
%%            a test module namel or vice versa.

-module(refac_rename_mod).

-export([rename_mod/4, rename_mod_1/5, rename_mod_eclipse/4, rename_mod_1_eclipse/5]).

-import(refac_atom_utils, [output_atom_warning_msg/3,check_atoms/4,start_atom_process/0,stop_atom_process/1]).

-include("../include/wrangler.hrl").

-spec(rename_mod/4::(filename(), string(), [dir()], integer()) -> 
	     {error, string()} | {question, string()} |{ok, [filename()], boolean()}).
rename_mod(FileName, NewName, SearchPaths, TabWidth) ->
    rename_mod(FileName, NewName, SearchPaths, TabWidth, emacs).

-spec(rename_mod_eclipse/4::(filename(), string(), [dir()], integer()) ->
	     {error, string()} | {question, string()} | {warning, string()} |
		 {ok, [{filename(), filename(), string()}]}).
rename_mod_eclipse(FileName, NewName, SearchPaths, TabWidth) ->
    rename_mod(FileName, NewName, SearchPaths, TabWidth, eclipse).


-spec(rename_mod_1/5::(filename(), string(), [dir()], integer(),boolean()) ->
			    {ok, [filename()], boolean()} |  {ok, [{filename(), filename(), string()}]}).
rename_mod_1(FileName, NewName, SearchPaths, TabWidth, RenameTestMod) ->
    rename_mod_1(FileName, NewName, SearchPaths, TabWidth, RenameTestMod, emacs).

-spec(rename_mod_1_eclipse/5::(filename(), string(), [dir()], integer(),boolean()) ->
				    {ok, [{filename(), filename(), string()}]}).
rename_mod_1_eclipse(FileName, NewName, SearchPaths, TabWidth, RenameTestMod) ->
    rename_mod_1(FileName, NewName, SearchPaths, TabWidth, RenameTestMod, eclipse).

rename_mod(FileName, NewName, SearchPaths, TabWidth, Editor) ->
    ?wrangler_io("\nCMD: ~p:rename_mod(~p, ~p,~p, ~p).\n",
		 [?MODULE, FileName, NewName, SearchPaths, TabWidth]),
    Cmd = "CMD: " ++ atom_to_list(?MODULE) ++ ":rename_mod(" ++ "\"" ++
	    FileName ++ "\", " ++ NewName ++ "\"," ++ "[" ++
	      refac_misc:format_search_paths(SearchPaths) ++ "]," ++ integer_to_list(TabWidth) ++ ").",
    case refac_misc:is_fun_name(NewName) of
      true ->
	  {ok, {AnnAST, Info}} = refac_util:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
	  case lists:keysearch(module, 1, Info) of
	    {value, {module, OldModName}} ->
		NewModName = list_to_atom(NewName),
		TestFrameWorkUsed = refac_util:test_framework_used(FileName),
		case pre_cond_check(FileName, OldModName, NewModName, TestFrameWorkUsed, SearchPaths) of
		  ok -> do_rename_mod(FileName, [{OldModName, NewModName}],
				      AnnAST, SearchPaths, Editor, TabWidth, Cmd);
		  Other -> Other
		end;
	    false -> {error, "Wrangler could not infer the current module name."}
	  end;
      false -> {error, "Invalid new module name!"}
    end.

parse_file_with_type_ann(FileName, SearchPaths, TabWidth) ->
    {ok, {AnnAST0, Info}} = refac_util:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    {AnnAST0, Info}.


pre_cond_check(FileName, OldModName, NewModName, TestFrameWorkUsed, SearchPaths) ->
    case NewModName =/= OldModName of
	true ->
	    ok;
	false ->
	    throw({error, "New module name is the same as the old name."})
    end,
    NewFileName = filename:dirname(FileName) ++ "/" ++ atom_to_list(NewModName) ++ ".erl",
    case filelib:is_file(NewFileName) of
      false ->
	  ok;
      true ->
	  throw({error, "The new module/file name has been used!"})
    end,
    case TestFrameWorkUsed of
      [] -> ok;
      _ ->
	  pre_cond_test_file_checking(OldModName, NewModName, TestFrameWorkUsed, SearchPaths)
    end.

find_eunit_test_file(ModName, SearchPaths) ->
    TestFileName = atom_to_list(ModName)++"_tests",
    Files = refac_util:expand_files(SearchPaths, ".erl"),
    [F || F<-Files, filename:basename(F, ".erl")==TestFileName,
                   lists:member(eunit, refac_util:test_framework_used(F))].
    
	
pre_cond_test_file_checking(OldModName, NewModName,TestFrameWorkUsed, SearchPaths) ->
    ok =test_framework_aware_name_checking(TestFrameWorkUsed, OldModName, NewModName),
    EUnitTestFiles = find_eunit_test_file(OldModName, SearchPaths),
    case EUnitTestFiles of 
	[] -> ok;
	[F] ->
	    NewTestFileName = filename:dirname(F) ++ "/" ++ atom_to_list(NewModName) ++ "_tests.erl",
	    case filelib:is_file(NewTestFileName) of
		false ->
		    {question, "Also rename the test module: " ++ F ++ "?"};
		true -> {warning, "This module has a test module, but Wrangler cannot rename it"
			 " because the new test module name is already in use, still continue?"}
	    end;
	_ -> {warning, "There are multiple EUnit test files for this module, "++output_filenames(EUnitTestFiles)++
	      ", and Wrangler will not rename them, still continue?"}
    end.

    
rename_mod_1(FileName, NewName, SearchPaths, TabWidth, RenameTestMod, Editor) ->
    ?wrangler_io("\nCMD: ~p:rename_mod_1(~p, ~p,~p, ~p, ~p, ~p).\n",
		 [?MODULE, FileName, NewName, SearchPaths, TabWidth, RenameTestMod, Editor]),
    Cmd = "CMD: " ++ atom_to_list(?MODULE) ++ ":rename_mod(" ++ "\"" ++
	    FileName ++ "\", " ++ NewName ++ "\"," ++ "[" ++
	      refac_misc:format_search_paths(SearchPaths) ++ "]," ++ integer_to_list(TabWidth) ++ ").",
    {AnnAST, Info} = parse_file_with_type_ann(FileName, SearchPaths, TabWidth),
    {value, {module, OldModName}} = lists:keysearch(module, 1, Info),
    NewModName = list_to_atom(NewName),
    TestFrameWorkUsed = refac_util:test_framework_used(FileName),
    case lists:member(eunit, TestFrameWorkUsed) andalso RenameTestMod of
      true ->
	  TestModName = list_to_atom(atom_to_list(OldModName) ++ "_tests"),
	  NewTestModName = list_to_atom(NewName ++ "_tests"),
	  do_rename_mod(FileName, [{OldModName, NewModName}, {TestModName, NewTestModName}],
			AnnAST, SearchPaths, Editor, TabWidth, Cmd);
      false ->
	  do_rename_mod(FileName, [{OldModName, NewModName}], AnnAST, SearchPaths, Editor, TabWidth, Cmd)
    end.

do_rename_mod(FileName, OldNewModPairs, AnnAST, SearchPaths, Editor, TabWidth, Cmd) ->
    {OldModName, NewModName} = hd(OldNewModPairs),
    NewFileName = filename:dirname(FileName) ++ "/" ++ atom_to_list(NewModName) ++ ".erl",
    {TestFileName, NewTestFileName} =
	case find_eunit_test_file(OldModName, SearchPaths) of
	  [F] -> {F, filename:dirname(F) ++ "/" ++ atom_to_list(NewModName) ++ "_tests.erl"};
	  _ -> {none, none}
	end,
    Pid = start_atom_process(),
    ?wrangler_io("The current file under refactoring is:\n~p\n", [FileName]),
    {AnnAST1, _C1} = do_rename_mod_1(AnnAST, {FileName, OldNewModPairs, Pid}),
    OldModNames = element(1, lists:unzip(OldNewModPairs)),
    check_atoms(FileName, AnnAST1, OldModNames, Pid),
    TestModRes = case length(OldNewModPairs) of
		   2 -> ?wrangler_io("The current file under refactoring is:\n~p\n", [TestFileName]),
			{TestAnnAST, _Info} = parse_file_with_type_ann(TestFileName, SearchPaths, TabWidth),
			{TestAnnAST1, _C2} = do_rename_mod_1(TestAnnAST, {FileName, OldNewModPairs, Pid}),
			check_atoms(FileName, TestAnnAST1, OldModNames, Pid),
			[{{TestFileName, NewTestFileName}, TestAnnAST1}];
		   _ -> []
		 end,
    
    ?wrangler_io("\nChecking client modules in the following search paths: \n~p\n", [SearchPaths]),
    ClientFiles1 = refac_util:get_client_files(FileName, SearchPaths),
    ClientFiles2 = case length(OldNewModPairs) == 2 of
		     true -> refac_util:get_client_files(TestFileName, SearchPaths);
		     _ -> []
		   end,
    ClientFiles = case length(OldNewModPairs) of
		    2 -> lists:usort(ClientFiles1 ++ ClientFiles2) -- [FileName, TestFileName];
		    _ -> ClientFiles1
		  end,
    Results = rename_mod_in_client_modules(ClientFiles, OldModName, OldNewModPairs, SearchPaths, TabWidth, Pid),
    case Editor of
      emacs ->
	  HasWarningMsg = refac_atom_utils:has_warning_msg(Pid),
	  case HasWarningMsg of
	    true ->
		output_atom_warning_msg(Pid, not_renamed_warn_msg(OldModNames), renamed_warn_msg(OldModNames));
	    false ->
		ok
	  end,
	  stop_atom_process(Pid),
	  refac_util:write_refactored_files_for_preview([{{FileName, NewFileName}, AnnAST1}| TestModRes ++ Results], Cmd),
	  ChangedClientFiles = lists:map(fun ({{F, _F}, _AST}) -> F end, Results),
	  ChangedFiles = case length(OldNewModPairs) of
			   2 -> [FileName, TestFileName| ChangedClientFiles];
			   1 -> [FileName| ChangedClientFiles]
			 end,
	  ?wrangler_io("The following files are to be changed by this refactoring:\n~p\n", [ChangedFiles]),
	  {ok, ChangedFiles, HasWarningMsg};
      eclipse ->
	  Results1 = [{{FileName, NewFileName}, AnnAST1}| TestModRes ++ Results],
	  Res = lists:map(fun ({{FName, NewFName}, AST}) -> {FName, NewFName,
							     refac_prettypr:print_ast(refac_util:file_format(FName), AST)}
			  end, Results1),
	  {ok, Res}
    end.
  
 
   
do_rename_mod_1(Tree, {FileName, OldNewModPairs, Pid}) ->
    TestFrameWorkUsed = refac_util:test_framework_used(FileName),
    {AnnAST1, C1} = ast_traverse_api:full_tdTP(fun do_rename_mod_2/2, Tree, {FileName, OldNewModPairs, Pid}),
    {AnnAST2, C2} = case lists:member(eunit, TestFrameWorkUsed) of
		      true -> do_rename_mod_in_eunit_funs(FileName, AnnAST1, OldNewModPairs, Pid);
		      _ -> {AnnAST1, C1}
		    end,
    {AnnAST2, C1 or C2}.

do_rename_mod_2(Tree, {_FileName, OldNewModPairs, _Pid}) ->
    case refac_syntax:type(Tree) of
      attribute ->
	  AttrName = refac_syntax:attribute_name(Tree),
	  case refac_syntax:atom_value(AttrName) of
	    module -> Args = refac_syntax:attribute_arguments(Tree),
		      F = fun (Arg) -> case lists:keysearch(refac_syntax:atom_value(Arg), 1, OldNewModPairs) of
					 {value, {_OldModName, NewModName}} ->
					     copy_pos_attrs(Arg, refac_syntax:atom(NewModName));
					 _ -> Arg
				       end
			  end,
		      Args1 = lists:map(F, Args),
		      Tree1 = copy_pos_attrs(Tree, refac_syntax:attribute(AttrName, Args1)),
		      {Tree1, Tree =/= Tree1};
	    import -> Args = refac_syntax:attribute_arguments(Tree),
		      case Args of
			[H| T] ->
			    case refac_syntax:type(H) of
			      atom ->
				  M = refac_syntax:atom_value(H),
				  case lists:keysearch(M, 1, OldNewModPairs) of
				    {value, {_OldModName, NewModName}} ->
					H1 = copy_pos_attrs(H, refac_syntax:atom(NewModName)),
					Tree1 = copy_pos_attrs(Tree, refac_syntax:attribute(AttrName, [H1| T])),
					{Tree1, true};
				    _ -> {Tree, false}
				  end;
			      qualified_name ->
				  M = list_to_atom(packages:concat(
						     [refac_syntax:atom_value(A)
						      || A <- refac_syntax:qualified_name_segments(H)])),
				  case lists:keysearch(M, 1, OldNewModPairs) of
				    {value, {_OldModName, NewModName}} ->
					H1 = copy_pos_attrs(H, refac_syntax:qualified_name(
								 [refac_syntax:atom(NewModName)])),
					Tree1 = copy_pos_attrs(Tree, refac_syntax:attribute(AttrName, [H1| T])),
					{Tree1, true};
				    _ -> {Tree, false}
				  end;
			      _ -> {Tree, false}
			    end;
			_ -> {Tree, false}
		      end;
	    _ -> {Tree, false}
	  end;
      module_qualifier ->
	  Mod = refac_syntax:module_qualifier_argument(Tree),
	  Fun = refac_syntax:module_qualifier_body(Tree),
	  case refac_syntax:type(Mod) of
	    atom ->
		case lists:keysearch(refac_syntax:atom_value(Mod), 1, OldNewModPairs) of
		  {value, {_OldModName, NewModName}} ->
		      Mod1 = copy_pos_attrs(Mod, refac_syntax:atom(NewModName)),
		      Tree1 = copy_pos_attrs(Tree, refac_syntax:module_qualifier(Mod1, Fun)),
		      {Tree1, true};
		  _ -> {Tree, false}
		end;
	    _ -> {Tree, false}
	  end;
      atom ->
	  As = refac_syntax:get_ann(Tree),
	  case lists:keysearch(type, 1, As) of
	    {value, {type, m_atom}} ->
		case lists:keysearch(refac_syntax:atom_value(Tree), 1, OldNewModPairs) of
		  {value, {_OldModName, NewModName}} ->
		      Tree1 = copy_pos_attrs(Tree, refac_syntax:atom(NewModName)),
		      {Tree1, true};
		  _ -> {Tree, false}
		end;
	    _ ->
		{Tree, false}
	  end;
	_ -> {Tree, false}
    end.
rename_mod_in_client_modules(Files, OldModName, OldNewModPairs, SearchPaths, TabWidth, Pid) ->
    case Files of 
        [] -> [];
        [F|Fs] -> 
	    ?wrangler_io("The current file under refactoring is:\n~p\n",[F]), 
	    {AnnAST,Info} =  parse_file_with_type_ann(F, SearchPaths, TabWidth),
	    case lists:keysearch(module,1, Info) of 
		{value, {module, OldModName}} ->
		    throw({error, "The same module name, "++atom_to_list(OldModName)
			   ++", is also used by file: "++F++"."});
		_ -> ok
	    end,
	    {AnnAST1, Changed} = do_rename_mod_1(AnnAST, {F, OldNewModPairs,Pid}),
	    OldModNames = element(1, lists:unzip(OldNewModPairs)),
	    check_atoms(F, AnnAST1, OldModNames, Pid),
	    if Changed ->
		    [{{F,F}, AnnAST1}|rename_mod_in_client_modules(
					Fs, OldModName, OldNewModPairs, SearchPaths, TabWidth, Pid)];
	       true -> 
		    rename_mod_in_client_modules(
		      Fs, OldModName, OldNewModPairs, SearchPaths, TabWidth,Pid)
	    end
    end.    

copy_pos_attrs(E1, E2) ->
    refac_syntax:copy_pos(E1, refac_syntax:copy_attrs(E1, E2)).

do_rename_mod_in_eunit_funs(FileName, AnnAST, OldNewModPairs, Pid) ->
    ast_traverse_api:full_tdTP(fun do_rename_mod_in_eunit_funs_1/2, AnnAST, {FileName, OldNewModPairs, Pid}).
do_rename_mod_in_eunit_funs_1(Node, Others) ->
    case refac_syntax:type(Node) of
      function ->
	  FunName = refac_syntax:data(refac_syntax:function_name(Node)),
	  Arity = refac_syntax:function_arity(Node),
	  case (Arity == 0) and lists:suffix(?DEFAULT_EUNIT_GENERATOR_SUFFIX, atom_to_list(FunName)) of
	    true ->
		ast_traverse_api:full_tdTP(fun do_rename_mod_in_eunit_funs_3/2, Node, Others);
	    _ -> {Node, false}
	  end;
      _ -> {Node, false}
    end.
do_rename_mod_in_eunit_funs_3(Node, {FileName,OldNewModPairs,Pid}) ->
    case refac_syntax:type(Node) of 
	tuple ->
	    case refac_syntax:tuple_elements(Node) of 
		[E1, E2, E3] ->
		    case refac_syntax:type(E1)==atom andalso
			refac_syntax:atom_value(E1)==generator andalso
			refac_syntax:type(E2) == atom of 
			true ->
			    case lists:keysearch(refac_syntax:atom_value(E2), 1, OldNewModPairs) of 
				{value, {_OldModName, NewModName}} ->
				    Pid ! {add_renamed, {FileName, Node}},
				    {copy_pos_attrs(Node, refac_syntax:tuple(
					     [E1, copy_pos_attrs(E2, refac_syntax:atom(NewModName)), E3])),
				     true};
				false-> {Node, false}
			    end;
			false ->
			    {Node, false}
		    end;
		[E1,E2] -> case refac_syntax:type(E1) of 
			       atom ->
				   case refac_syntax:atom_value(E1) of 
				       module ->
					   case refac_syntax:type(E2) of 
					       atom ->
						   case lists:keysearch(refac_syntax:atom_value(E2), 1, OldNewModPairs) of 
						       {value, {_OldModName, NewModName}} ->
							   Pid ! {add_renamed, {FileName, Node}},
							   {copy_pos_attrs(Node, refac_syntax:tuple([E1, copy_pos_attrs(E2, refac_syntax:atom(NewModName))])),
							    false};
						       _-> {Node, false}
						   end;
					       _ ->  {Node, false}
					   end;
				       file ->
					   {Node, false}; %% TODO: NEED TO CHANGE HERE;				       
				       Val -> case lists:keysearch(Val, 1, OldNewModPairs) of
						  {value, {_OldModName, NewModName}} ->
						      Pid ! {add_renamed, {FileName, Node}},
						      {copy_pos_attrs(Node,refac_syntax:tuple(
									     [copy_pos_attrs(E1, refac_syntax:atom(NewModName)), E2])), false};
						  false ->
						      {Node, false}
					      end
				   end;
			       _ -> {Node, false}
			   end;
		atom ->  case lists:keysearch(refac_syntax:atom_value(Node),1, OldNewModPairs) of 
			     {value, {_OldModName, NewModName}} ->
				 Pid ! {add_renamed, {FileName, Node}},
				 {copy_pos_attrs(Node, refac_syntax:atom(NewModName)),true};
			     false-> {Node, false}
			 end;
		_ -> {Node, false}
	    end;
	_ -> {Node, false}
    end.


test_framework_aware_name_checking(UsedFrameWorks, OldModName, NewModName) ->
    eunit_name_checking(UsedFrameWorks, OldModName, NewModName),
    eqc_name_checking(UsedFrameWorks, OldModName, NewModName),
    testserver_name_checking(UsedFrameWorks, OldModName, NewModName).
 
eunit_name_checking(UsedFrameWorks, OldModName, NewModName) ->
    case lists:member(eunit, UsedFrameWorks) of
	true ->
	    case lists:suffix(?DEFAULT_EUNIT_TESTMODULE_SUFFIX, atom_to_list(OldModName)) of 
		true ->
		    throw({warning, "The module to be renamed is an EUnit test module, "
			   "do you really want to continue the refactoring?"});
		false ->
		    case lists:suffix(?DEFAULT_EUNIT_TESTMODULE_SUFFIX, atom_to_list(NewModName)) of 
			true ->
			    throw({warning, "Module names ending with '" ++ ?DEFAULT_EUNIT_TESTMODULE_SUFFIX ++ 
				   "' are used by EUnit as test module names, continue?"});
			_ -> ok
		    end
	    end;	
	_ -> ok
    end.

eqc_name_checking(_UsedFrameWorks, _OldModName, _NewModName)->
    ok.   %% are there any contraints on module names imposed by quickcheck?

testserver_name_checking(UsedFrameWorks, OldModName, NewModName) ->
    case lists:suffix(?DEFAULT_TS_MODULE_SUFFIX, atom_to_list(OldModName)) of
      true -> case lists:suffix(?DEFAULT_TS_MODULE_SUFFIX, atom_to_list(NewModName)) of
		true -> ok;
		_ ->
		    case lists:member(testserver, UsedFrameWorks) orelse
			lists:member(commontest, UsedFrameWorks) of
		      true ->
			  throw({warning, "A Test Server/Common Test test suite module "
				 "should have a name on the form '*_SUITE.erl', still continue?"});
		      false ->
			  throw({warning, "A module with a name of the form '*_SUITE.erl' could "
				 "be a Test Server/Common Test test suite module, continue?"})
		    end
	      end;
      _ -> case lists:suffix(?DEFAULT_TS_MODULE_SUFFIX, atom_to_list(NewModName)) of
	     false -> ok;
	     _ ->
		 throw({warning, "A module name on the form '*_SUITE.erl' is"
			" usually treated as a Test Server/Common Test test suite module, continue?"})
	   end
    end.
    
  
renamed_warn_msg(OldModNames) ->
    case OldModNames of 
	[M] ->
	    "\n=================================================================================\n"
		"WARNING: Wrangler has renamed the uses of '"++atom_to_list(M)++
		"' within the following expressions while without enough "
		"syntactic/semantic information. Please check manually!\n";
	[M1,M2] ->
	    "\n=================================================================================\n"
		"WARNING: Wrangler has renamed the uses of "++atom_to_list(M1)++", or"++atom_to_list(M2)++ 
		", within the following expressions while without enough "
		"syntactic/semantic information. Please check manually!\n"
    end.

not_renamed_warn_msg(OldModNames) ->
    case OldModNames of 
	[M] ->"\n=================================================================================\n"
		  "WARNING: Wrangler could not infer whether the uses of '"++atom_to_list(M)++"' at the following positions "
		  "refer to the module renamed, and they are not renamed. Please check manually!\n";
	[M1, M2] ->"\n=================================================================================\n"		 
		       "WARNING: Wrangler could not infer whether the uses of '"++atom_to_list(M1)++"', or'" 
		       ++atom_to_list(M2)++"', at the following positions "
		       "refer to the module renamed, and they are not renamed. Please check manually!\n"
    end.

output_filenames([F|T]) ->
    output_filenames(T, F).
output_filenames([], Acc) ->
    Acc;
output_filenames([F|T], Acc) ->
    output_filenames(T, Acc++", "++F).

	
    
		 
		 
			     
			    
    
    

