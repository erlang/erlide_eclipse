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
%% Refactoring: Rename a module name.
%%
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

-export([rename_mod/4, rename_mod_1/5, rename_mod_eclipse/4, rename_mod_1_eclipse/5]).

-import(refac_rename_fun, [check_atoms/4, start_atom_process/0, 
			   output_atom_warning_msg/3, stop_atom_process/1]).

-include("../include/wrangler.hrl").
%% =====================================================================
%% @spec rename_mod(FileName::filename(), NewName::string(), SearchPaths::[string()])-> term()
%%   

-spec(rename_mod/4::(filename(), string(), [dir()], integer()) -> 
	     {error, string()} | {question, string()} | {warning, string()} |{ok, [filename()]}).
rename_mod(FileName, NewName, SearchPaths, TabWidth) ->
    rename_mod(FileName, NewName, SearchPaths, TabWidth, emacs).

-spec(rename_mod_eclipse/4::(filename(), string(), [dir()], integer()) ->
	     {error, string()} | {question, string()} | {warning, string()} |
		 {ok, [{filename(), filename(), string()}]}).
rename_mod_eclipse(FileName, NewName, SearchPaths, TabWidth) ->
    rename_mod(FileName, NewName, SearchPaths, TabWidth, eclipse).


-spec(rename_mod_1/5::(filename(), string(), [dir()], integer(),bool()) ->{ok, [filename()]}).
rename_mod_1(FileName, NewName, SearchPaths, TabWidth, RenameTestMod) ->
    rename_mod_1(FileName, NewName, SearchPaths, TabWidth, RenameTestMod, emacs).

-spec(rename_mod_1_eclipse/5::(filename(), string(), [dir()], integer(),bool()) ->{ok, [{filename(), filename(), string()}]}).
rename_mod_1_eclipse(FileName, NewName, SearchPaths, TabWidth, RenameTestMod) ->
    rename_mod_1(FileName, NewName, SearchPaths, TabWidth, RenameTestMod, eclipse).

rename_mod(FileName, NewName,SearchPaths, TabWidth, Editor) ->
    ?wrangler_io("\nCMD: ~p:rename_mod(~p, ~p,~p, ~p).\n", [?MODULE, FileName, NewName, SearchPaths, TabWidth]),
    Cmd = "CMD: " ++ atom_to_list(?MODULE) ++ ":rename_mod(" ++ "\"" ++
	FileName ++ "\", " ++ NewName ++ "\"," ++ "[" ++ 
	refac_util:format_search_paths(SearchPaths) ++ "]," ++ integer_to_list(TabWidth) ++ ").",
    case refac_util:is_fun_name(NewName) of   %% module name and function name follow the same rules.
	true ->
	    {ok, {AnnAST, Info}}= refac_util:parse_annotate_file(FileName,true, SearchPaths, TabWidth),
	    case lists:keysearch(module, 1, Info) of 
		{value, {module, OldModName}} ->
		    NewModName = list_to_atom(NewName),
		    TestFrameWorkUsed = refac_util:test_framework_used(FileName),
		    case pre_cond_check(FileName, OldModName, NewModName, TestFrameWorkUsed) of 
			ok -> do_rename_mod(FileName, [{OldModName, NewModName}], AnnAST, SearchPaths, Editor, TabWidth, Cmd);
			Other -> Other
		    end;
		false -> {error, "Wrangler could not infer the current module name."}
	    end;
	false -> {error, "Invalid new module name!"}
    end.


pre_cond_check(FileName, OldModName, NewModName, TestFrameWorkUsed) ->
    case NewModName =/= OldModName of 
	true ->
	    NewFileName = filename:dirname(FileName)++"/"++atom_to_list(NewModName)++".erl",
	    %% Here we assume the application file and test file are in the same dir; maybe to restrictive?
	    EunitTestFileName= filename:dirname(FileName)++"/"++atom_to_list(OldModName)++"_tests.erl",  
	    case filelib:is_file(NewFileName) of
		false -> 
		    TestFrameWorkUsed = refac_util:test_framework_used(FileName),
		    case TestFrameWorkUsed of 
			[] -> ok;
			_ -> case (catch test_framework_aware_name_checking(TestFrameWorkUsed, OldModName, NewModName)) of 
				 ok ->
				     case filelib:is_file(EunitTestFileName) of 
					 true -> case lists:member(eunit,refac_util:test_framework_used(EunitTestFileName)) of 
						     true ->
							 NewTestFileName = filename:dirname(FileName)++"/"++atom_to_list(NewModName)++"_tests.erl",
							 case filelib:is_file(NewTestFileName) of 
							     false ->
								{question, "Also rename the test module: "++ atom_to_list(OldModName)++"_tests.erl" ++ "?"};
							     true -> {warning, "This module has a test module, but Wrangler cannot rename it"
								      " because the new test module name is already in use, still continue?"}
							 end;
						     false -> ok
						 end;
					 _->  ok
				     end;
				 Others -> Others
			     end		    
		    end;
		true ->{error, "The new module/file name has been used!"}
	    end;
	false ->
	    {error, "New module name is the same as the old name."}
    end.


rename_mod_1(FileName, NewName, SearchPaths, TabWidth, RenameTestMod, Editor) ->  
    Cmd = "CMD: " ++ atom_to_list(?MODULE) ++ ":rename_mod(" ++ "\"" ++
	FileName ++ "\", " ++ NewName ++ "\"," ++ "[" ++ 
	refac_util:format_search_paths(SearchPaths) ++ "]," ++ integer_to_list(TabWidth) ++ ").",    
    {ok, {AnnAST, Info}}= refac_util:parse_annotate_file(FileName,true, SearchPaths),
    {value, {module, OldModName}} = lists:keysearch(module, 1, Info), 
    NewModName = list_to_atom(NewName),
    TestFrameWorkUsed = refac_util:test_framework_used(FileName), 
    case lists:member(eunit, TestFrameWorkUsed) andalso RenameTestMod of 
	true ->
	    TestModName = list_to_atom(atom_to_list(OldModName) ++ "_tests"),
	    NewTestModName = list_to_atom(NewName++"_tests"),
	    do_rename_mod(FileName, [{OldModName, NewModName}, {TestModName, NewTestModName}], 
			  AnnAST, SearchPaths, Editor, TabWidth, Cmd);
	false ->
	    do_rename_mod(FileName, [{OldModName, NewModName}], AnnAST, SearchPaths, Editor, TabWidth, Cmd)
    end.

do_rename_mod(FileName, OldNewModPairs, AnnAST, SearchPaths,Editor, TabWidth, Cmd) ->
    {OldModName, NewModName} = hd(OldNewModPairs),
    NewFileName = filename:dirname(FileName)++"/"++ atom_to_list(NewModName)++".erl",
    TestFileName = filename:dirname(FileName)++"/"++ atom_to_list(OldModName)++"_tests.erl",
    NewTestFileName = filename:dirname(FileName)++"/"++atom_to_list(NewModName)++"_tests.erl",
    Pid = start_atom_process(),
    ?wrangler_io("The current file under refactoring is:\n~p\n",[FileName]), 
    {AnnAST1, _C1}=do_rename_mod_1(AnnAST, {FileName,OldNewModPairs,Pid}),
    OldModNames = element(1, lists:unzip(OldNewModPairs)),
    check_atoms(FileName, AnnAST1, OldModNames, Pid),
    TestModRes = case length(OldNewModPairs) of 
	      2 ->  ?wrangler_io("The current file under refactoring is:\n~p\n",[TestFileName]), 
		    {ok, {TestAnnAST, _Info}}= refac_util:parse_annotate_file(TestFileName,true, SearchPaths, TabWidth),
		    {TestAnnAST1, _C2} =do_rename_mod_1(TestAnnAST, {FileName,OldNewModPairs, Pid}), 
		    check_atoms(FileName,TestAnnAST1, OldModNames, Pid),
		    [{{TestFileName, NewTestFileName}, TestAnnAST1}];
	      _ -> []
	  end,

    ?wrangler_io("\nChecking client modules in the following search paths: \n~p\n",[SearchPaths]),
    ClientFiles1 = refac_util:get_client_files(FileName, SearchPaths),
    ClientFiles2 = case length(OldNewModPairs)==2 of 
		      true ->  refac_util:get_client_files(TestFileName, SearchPaths);
		      _ -> []
		  end,
    ClientFiles = ClientFiles1++ClientFiles2 -- [FileName, TestFileName],
    Results = rename_mod_in_client_modules(ClientFiles, OldNewModPairs,SearchPaths, TabWidth, Pid),
    case Editor of 
	emacs ->
	    output_atom_warning_msg(Pid, not_renamed_warn_msg(OldModNames), renamed_warn_msg(OldModNames)),
	    stop_atom_process(Pid),
	    refac_util:write_refactored_files_for_preview([{{FileName, NewFileName}, AnnAST1}|(TestModRes++Results)], Cmd),
	    ChangedClientFiles = lists:map(fun({{F, _F}, _AST}) -> F end, Results),
	    ChangedFiles = case length(OldNewModPairs) of 
			       2 ->[FileName, TestFileName| ChangedClientFiles];
			       1 ->[FileName|ChangedClientFiles]
			   end,
	    ?wrangler_io("The following files are to be changed by this refactoring:\n~p\n",[ChangedFiles]),
	    {ok, ChangedFiles};
	eclipse ->
	    Results1 =[{{FileName, NewFileName}, AnnAST1}|(TestModRes++Results)],
	    Res = lists:map(fun({{FName, NewFName}, AST}) -> {FName, NewFName, 
			      refac_prettypr:print_ast(refac_util:file_format(FName),AST)} end, Results1),
	    {ok, Res}
    end.
  
 
   
do_rename_mod_1(Tree, {FileName, OldNewModPairs,Pid}) ->
    {AnnAST1, C1}=refac_util:full_tdTP(fun do_rename_mod_2/2, Tree, {FileName,OldNewModPairs, Pid}),
    {AnnAST2, C2} = case lists:member(eunit, refac_util:test_framework_used(FileName)) of 
			true ->do_rename_mod_in_eunit_funs(FileName, AnnAST1, OldNewModPairs,Pid);
			_ -> {AnnAST1, C1}
		    end,
    {AnnAST2, C1 or C2}.

do_rename_mod_2(Tree, {FileName,OldNewModPairs, Pid}) ->
    case refac_syntax:type(Tree) of
       attribute -> 
	    AttrName = refac_syntax:attribute_name(Tree),
	    case refac_syntax:atom_value(AttrName) of 
	       module -> Args = refac_syntax:attribute_arguments(Tree),
			 F = fun (Arg) -> case lists:keysearch(refac_syntax:atom_value(Arg),1, OldNewModPairs) of 
					      {value, {_OldModName, NewModName}} -> 
						  copy_pos_attrs(Arg, refac_syntax:atom(NewModName));
					      _ -> Arg
					  end
			     end,
			 Args1 = lists:map(F, Args),
			 Tree1 = copy_pos_attrs(Tree, refac_syntax:attribute(AttrName, Args1)),
			 {Tree1, Tree=/=Tree1};
		import -> Args = refac_syntax:attribute_arguments(Tree),
			  case Args of 
			      [H|T] -> case lists:keysearch(refac_syntax:atom_value(H),1, OldNewModPairs) of 
					   {value, {_OldModName, NewModName}} ->
					       H1 = copy_pos_attrs(H, refac_syntax:atom(NewModName)),
					       Tree1 = copy_pos_attrs(Tree,refac_syntax:attribute(AttrName, [H1|T])),
					       {Tree1, true};
					   _ -> {Tree, false}
				       end;
			      _ -> {Tree, false}
			  end;
		_ -> {Tree, false}
	    end;
       application ->
	    Op = refac_syntax:application_operator(Tree),
	    As = refac_syntax:get_ann(Op),
	    case lists:keysearch(fun_def, 1, As) of 
		{value, {fun_def, {Mod1, Fun1, Arity1, _, _}}} ->
		    case lists:keysearch({Mod1, Fun1, Arity1}, 1, mod_name_as_pars_1()) of
			{value, _} ->
			    Tree1 =transform_mod_name_as_pars(Tree, OldNewModPairs, 1),
			    {Tree1, Tree1=/=Tree};
			false ->
			    case lists:keysearch({Mod1, Fun1, Arity1}, 1, mod_name_as_pars_2()) of
				{value, _} ->
				    Tree1 = transform_mod_name_as_pars(Tree, OldNewModPairs,2),
				    {Tree1, Tree1=/=Tree};
				false ->
				    case lists:keysearch({Mod1, Fun1, Arity1}, 1, mod_name_as_pars_3()) of
					{value, _} ->
					    Tree1=transform_mod_name_as_pars(Tree, OldNewModPairs,3),
					    {Tree1, Tree1=/=Tree};
					_ -> {Tree, false}
				    end
			    end
		    end;
		_ -> {Tree, false}
	    end;
	module_qualifier ->
	    Mod = refac_syntax:module_qualifier_argument(Tree),
	    Fun = refac_syntax:module_qualifier_body(Tree),
	    case refac_syntax:type(Mod) of 
		atom ->  
		    case lists:keysearch(refac_syntax:atom_value(Mod),1, OldNewModPairs) of 
			{value, {_OldModName, NewModName}} ->
			    Mod1 = copy_pos_attrs(Mod,refac_syntax:atom(NewModName)),
			    Tree1= copy_pos_attrs(Tree, refac_syntax:module_qualifier(Mod1, Fun)),
			    {Tree1, true};
			_ -> {Tree, false}
		    end;
		_ -> {Tree, false}
	    end;
	tuple -> do_rename_mod_in_tuples(Tree, {FileName, OldNewModPairs, Pid});
	_ -> {Tree, false}
    end.
rename_mod_in_client_modules(Files, OldNewModPairs, SearchPaths, TabWidth, Pid) ->
    case Files of 
        [] -> [];
        [F|Fs] ->  ?wrangler_io("The current file under refactoring is:\n~p\n",[F]), 
                   {ok, {AnnAST, _Info}}= refac_util:parse_annotate_file(F,true, SearchPaths, TabWidth),
		   {AnnAST1, Changed} = do_rename_mod_1(AnnAST, {F, OldNewModPairs,Pid}),
		   OldModNames = element(1, lists:unzip(OldNewModPairs)),
		   check_atoms(F, AnnAST1, OldModNames, Pid),
		   if Changed ->
			   [{{F,F}, AnnAST1}|rename_mod_in_client_modules(Fs, OldNewModPairs, SearchPaths, TabWidth, Pid)];
		      true -> rename_mod_in_client_modules(Fs, OldNewModPairs, SearchPaths, TabWidth,Pid)
		   end
	end.    

transform_mod_name_as_pars(Node, OldNewModPairs, Nth) ->
    Op = refac_syntax:application_operator(Node),
    Args = refac_syntax:application_arguments(Node),
    ModArg = lists:nth(Nth, Args),
    Args1 = case Nth of 
		1 -> [];
		_ -> lists:sublist(Args, 1, Nth-1)
	    end,
    Args2 = lists:sublist(Args, Nth+1, length(Args)),
    case refac_syntax:type(ModArg) of 
	atom -> case lists:keysearch(refac_syntax:atom_value(ModArg),1,OldNewModPairs) of
		    {value, {_OldModName, NewModName}} ->
			NewModArg = copy_pos_attrs(ModArg, refac_syntax:atom(NewModName)),
			copy_pos_attrs(Node,  refac_syntax:application(Op, Args1++[NewModArg]++Args2));
		    false -> Node
		end;
	_ -> Node
    end.

copy_pos_attrs(E1, E2) ->
    refac_syntax:copy_pos(E1, refac_syntax:copy_attrs(E1, E2)).

do_rename_mod_in_tuples(Tuple, {FileName, OldNewModPairs, Pid}) ->
    case refac_syntax:tuple_elements(Tuple) of 
	[E1, E2, E3, E4] ->
	    case {refac_syntax:type(E1), refac_syntax:type(E2), refac_syntax:type(E3)} of 
		{atom, atom, atom} ->
		    case {refac_syntax:atom_value(E1), refac_syntax:atom_value(E2), refac_syntax:atom_value(E3)} of 
			{call, ModName, _FunName} ->  %% More check about FunName to be addeed;
			    case lists:keysearch(ModName, 1, OldNewModPairs) of 
				{value, {_OldModName, NewModName}} ->
				    Pid ! {add_renamed, {FileName, Tuple}},
				    {copy_pos_attrs(Tuple, refac_syntax:tuple([E1, copy_pos_attrs(E2, refac_syntax:atom(NewModName)), E3, E4])), true};
				false -> {Tuple, false}
			    end;
			_->{Tuple, false}
		    end;
		_ -> {Tuple, false}
	    end;
	[E1,E2, E3] ->
	    case {refac_syntax:type(E1), refac_syntax:type(E2)} of 
		{atom, atom} ->
		    {ModName, _FunName}= {refac_syntax:atom_value(E1), refac_syntax:atom_value(E2)},
		    case lists:keysearch(ModName, 1, OldNewModPairs) of 
			{value, {_OldModName, NewModName}} ->
			    Pid ! {add_renamed, {FileName, Tuple}},
			    {copy_pos_attrs(Tuple, refac_syntax:tuple([copy_pos_attrs(E1, refac_syntax:atom(NewModName)), E2, E3])), true};
			false -> {Tuple, false}
		    end;
		_ -> {Tuple, false}
	    end;
	[E1, E2] ->
	    As = refac_syntax:get_ann(Tuple),
	    case lists:keysearch(fun_def,1,As) of
		{value, {fun_def, {ModName, _, _, _,_}}} ->
		    case lists:keysearch(ModName, 1, OldNewModPairs) of 
			{value, {_OldModName, NewModName}} ->
			    {copy_pos_attrs(Tuple, refac_syntax:tuple([copy_pos_attrs(E1, refac_syntax:atom(NewModName)), E2])), true};
			false -> {Tuple, false}
		    end;
		_ -> {Tuple, false}
	    end;
	_ -> {Tuple, false}
    end.

do_rename_mod_in_eunit_funs(FileName, AnnAST, OldNewModPairs, Pid) ->
    refac_util:full_tdTP(fun do_rename_mod_in_eunit_funs_1/2, AnnAST, {FileName, OldNewModPairs, Pid}).
do_rename_mod_in_eunit_funs_1(Node, Others) ->
    case refac_syntax:type(Node) of 
	function ->
	    FunName = refac_syntax:data(refac_syntax:function_name(Node)),
	    Arity =refac_syntax:function_arity(Node),
	    case (Arity==0) and (lists:suffix(?DEFAULT_EUNIT_GENERATOR_SUFFIX, atom_to_list(FunName))) of 
		true ->
		    refac_util:full_tdTP(fun do_rename_mod_in_eunit_funs_3/2, Node, Others);
		_ -> {Node, false}
	    end;
	_ -> {Node, false}
    end.
do_rename_mod_in_eunit_funs_3(Node, {FileName,OldNewModPairs,Pid}) ->
    case refac_syntax:type(Node) of 
	tuple ->
	    case refac_syntax:tuple_elements(Node) of 
		[E1, E2, E3] ->
		    case refac_syntax:type(E1) of 
			atom ->
			    case refac_syntax:atom_value(E1) of 
				generator ->
				    case refac_syntax:type(E2) of 
					atom ->
					    case lists:keysearch(refac_syntax:atom_value(E2), 1, OldNewModPairs) of 
						{value, {_OldModName, NewModName}} ->
						    Pid ! {add_renamed, {FileName, Node}},
						    {copy_pos_attrs(Node, refac_syntax:tuple([E1, copy_pos_attrs(E2, refac_syntax:atom(NewModName)), E3])),
						     true};
						false-> {Node, false}
					    end;
					_ -> {Node, false}
				    end;
				_ -> {Node, false}
			    end;
			_ -> {Node, false}
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
						      {copy_pos_attrs(Node,refac_syntax:tuple([copy_pos_attrs(E1, refac_syntax:atom(NewModName)), E2])), false};
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
		    throw({warning, "The module to be renamed is an EUnit test module, do you really want to continue the refactoring?"});
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
 			case lists:member(testserver, UsedFrameWorks) or 
			    lists:member(commontest, UsedFrameWorks) of 
			    true ->
 				throw({warning, "A Test Server/Common Test test suite module should have a name on the form '*_SUITE.erl', still continue?"});
			    false ->
				throw({warning, "A module with a name of the form '*_SUITE.erl' could be a Test Server/Common Test test suite module, continue?"})
			end
		end;
	_ -> case (lists:suffix(?DEFAULT_TS_MODULE_SUFFIX, atom_to_list(NewModName))) of 
		 false -> ok;
		 _ ->
		     throw({warning, "A module name on the form '*_SUITE.erl' is"
			    " usually treated as a Test Server/Common Test test suite module, continue?"})
	     end
    end.
    
 
mod_name_as_pars_1() ->
    [{{erlang, apply, 3}, [modulenmae, functionname, arglist], term},
     {{erlang, spawn, 3}, [modulename, functionname, arglist], term},
     {{erlang, spawn_link, 3}, [modulename, functionname, arglist], term},
     {{erlang, spawn_opt, 4}, [modulename, functionname, arglist, term], term},
     {{eqc, module, 1}, [modulename], term},
     {{eqc_ct, compile, 1}, [modulename], term},
     {{eqc_ct, compile, 2}, [mdoulename, term], term},
     {{eqc_ct, module, 1}, [modulename], term},
     {{eqc_statem, commands, 1}, [modulename], term},
     {{eqc_statem, commands, 2}, [modulename, term], term},
     {{eqc_statem, postconditions, 3}, [modulename, term, term], term},
     {{eqc_statem, run_commands, 2}, [modulename, term], term},
     {{eqc_statem, run_commands, 3}, [modulename, term, term], term},
     {{test_server, timecall, 3}, [modulename, functionname, arglist], term},
     {{test_server, call_crash, 3}, [modulename, functionname, arglist], term},
     {{test_server, is_native, 1}, [modulename], term},
     {{test_server_ctrl, add_module, 1}, [modulename], term},
     {{test_server_ctrl, add_case, 2}, [modulename, functionname], term},
     {{test_server_ctrl, add_cases, 2}, [modulename, [functionname]], term},
     {{rs, r, 2}, [modulename, functionname], term},
     {{rs, r, 3}, [modulename, functionname, term], term}].

mod_name_as_pars_2() ->
    [ {{erlang, spawn, 4}, [node, modulename, functionname, arglist], term},
      {{erlang, spawn_link, 4}, [term, modulename, functioname, arglist], term},
      {{erlang, spawn_monitor, 3}, [term, modulename,functionname, arglist], term},			       
      {{eralng, spawn_opt, 4}, [term, modulename, functionname, arglist, term], term},
      {{test_server, do_times, 4}, [integer, modulename, functionname, arglist], term},
      {{test_server, call_crash, 4}, [term, modulename, functionname, arglist], term},
      {{test_server_ctrl, add_case, 3}, [term, modulename, functionname], term},
      {{test_server_ctrl, add_cases,3}, [term, modulename, [functionname]], term},
      {{ts, run, 2}, [term, modulename], term},
      {{ts, run, 3}, [term, modulename, functionname], term},
      {{ts, run, 4}, [term, modulename, functionname, term], term}].
      
mod_name_as_pars_3() ->
    [{{test_server, call_crash, 5}, [term, term, modulename, functionname, arglist], term}].
    

    
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



	
    
		 
		 
			     
			    
    
    

