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
%%% =====================================================================
%% Refactoring: Move a function definition, or a collection of functions, from one module to another.
%%
%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%% 
%% =====================================================================
%% @doc Move a function definition from its current module to another module.
%% <p> To apply this refactoring, point the cursor at the function definition, then 
%% select <em> Move Definition to Another Module</em> from the <em> Refactor </em> menu, 
%% after that the refactorer will prompt to enter the target module name in the mini-buffer. 
%% </p>
%% <p> This refactoring has a global effect, i.e., it affects all the modules in which 
%%     the function is imported/used.
%% </p>
%% <p> This refactoring assumes that an erlang module name always matches it file name.
%% </p>
%% <p> Suppose we move functin <em> foo/n </em> from its current module <em> M </em> 
%%     to module <em> N </em>, then the following <em> side-conditions </em> apply to 
%%     this refactoring: 
%% <li> If <em> foo/n </em> is already in scope in module <em> N </em>, then its defining 
%%      module should be  <em> M </em>.(Note: move a collection of modules together to another 
%%      module will be supported by another refactoring.)
%% </li>
%% </p>
%% <p>
%% Known problems: this refactoring does not move the include/include_lib attributes, and type 
%% declarations on which the functions to be moved depend; In the case that some other internal
%% functions are solely depended by the functions to be moved, they are moved together with the functions
%% specified only if the pre-conditions are not violated. Therefore manual inspection is 
%% needed after the refactorings.
%% </p>
%% @end
%% @author Huiqing Li <hl@kent.ac.uk>
%% @version 0.1

%% @private
-module(refac_move_fun).

-export([move_fun/7, move_fun_1/8, move_fun_eclipse/6, move_fun_1_eclipse/6,
         move_fun_by_name/6]).

-export([analyze_file/3]).

-import(wrangler_atom_utils, [output_atom_warning_msg/3, check_unsure_atoms/5,
			      stop_atom_process/1, start_atom_process/0]).

-include("../include/wrangler_internal.hrl").

-record(modinfo, {filename :: filename(),
                  modname  :: atom(),
                  inscope_funs ::[{atom(), atom(), integer()}],
                  macro_defs :: [{atom(), any()}],
                  record_defs ::[{atom(), any()}],
                  includes ::[filename()],
                  ast :: syntaxTree(),
                  info:: [{key(), any()}]}).
%==========================================================================================

-spec(move_fun_eclipse/6::(filename(),integer(),integer(), string(),[dir()], integer())
        ->  {ok, [{filename(), filename(), string()}]} | {question, string()}).
move_fun_eclipse(FName, Line, Col, TargetModorFileName, SearchPaths, TabWidth) ->
    move_fun(FName, Line, Col, TargetModorFileName, SearchPaths, eclipse, TabWidth).


%% THIS interface need to be changed; and should inform George of the changes.
-spec(move_fun_1_eclipse/6::(filename(),integer(),integer(), string(),[dir()], integer())
        ->  {ok, [{filename(), filename(), string()}]}).
move_fun_1_eclipse(FName, Line, Col, TargetModorFileName, SearchPaths, TabWidth) ->
    move_fun_1(FName, Line, Col, TargetModorFileName, true, SearchPaths, eclipse, TabWidth).

-spec(move_fun_by_name/6::(modulename()|filename(), {atom(), integer()}, modulename()|filename(),
                           [dir()], atom(), integer())->
                                {error, string()} | {ok, [filename()]}).
move_fun_by_name(ModorFileName, {FunName, Arity}, TargetModorFileName, SearchPaths, Editor, TabWidth) ->
    move_fun_by_name_1(ModorFileName, FunName, Arity,TargetModorFileName, SearchPaths, Editor, TabWidth).

move_fun_by_name_1(ModorFileName, FunName, Arity,TargetModorFileName, SearchPaths, Editor, TabWidth) ->
    case get_file_name(ModorFileName, SearchPaths) of
	{ok, OriginalFileName} ->
	    case get_file_name(TargetModorFileName, SearchPaths) of
		{ok, TargetFileName} ->
		    {ok, {AnnAST, Info}} = wrangler_ast_server:parse_annotate_file(
                                             OriginalFileName, true, SearchPaths, TabWidth),
                    ModName = get_module_name(Info),
		    Res=wrangler_misc:funname_to_defpos(AnnAST, {ModName, FunName, Arity}),
                    case Res of
			{ok, Pos} ->
			    case Pos of
				{Line, Col} ->
                                    case Editor of 
                                        composite_emacs ->
                                            {ok, OriginalFileName, Line, Col, TargetFileName, SearchPaths};
                                        _ ->
                                            move_fun_1(OriginalFileName, Line, Col, TargetFileName, true, 
                                                       SearchPaths, Editor, TabWidth)
                                    end;
                                _ -> {error, "Wrangler could not infer the location of "
                                      "the function in the program."}
                            end;
			{error, Reason} ->
                            throw({error, Reason})
		    end;
		{error, Reason} ->
		    throw({error, Reason})
	    end;
	{error, Reason} ->
	    throw({error, Reason})
    end.

-spec(move_fun/7::(filename(),integer(),integer(), string(),[dir()], atom(), integer())
                  ->  {ok, [{filename(), filename(), string()}]} | {question, string()}).
move_fun(FName, Line, Col, TargetModorFileName, SearchPaths, Editor, TabWidth) ->
     ?wrangler_io("\nCMD: ~p:move_fun(~p, ~p, ~p, ~p, ~p,  ~p, ~p).\n",
		  [?MODULE, FName, Line, Col, TargetModorFileName, SearchPaths, Editor, TabWidth]),
     TargetFName = get_target_file_name(FName, TargetModorFileName),
     case TargetFName of
	 FName -> throw({error, "The target module is the same as the current module."});
	 _ -> ok
     end,
     {ok, {AnnAST, _Info}} = wrangler_ast_server:parse_annotate_file(FName, true, SearchPaths, TabWidth),
     case api_interface:pos_to_fun_def(AnnAST, {Line, Col}) of
	 {ok, _Def} ->
	     ok;
	 {error, _Reason} ->
	     case pos_to_export(AnnAST, {Line, Col}) of
		 {ok,_ExpAttr} ->
		     ok;
		 {error, _} ->
		     throw({error, "You have not selected a well-formed function definition or an export attribute."})
	     end
     end,
     case filelib:is_file(TargetFName) of
	 true ->
             move_fun_1(FName, Line, Col, TargetFName, true, SearchPaths, Editor, TabWidth);
         false ->
             {question, "Target file " ++ TargetFName ++ " does not exist, create it?"}
     end.

-spec(move_fun_1/8::(filename(),integer(),integer(), string(), boolean(),[dir()], atom(), integer())
                    ->  {ok, [{filename(), filename(), string()}]}).
move_fun_1(FName, Line, Col, TargetModorFileName, CondCheck, SearchPaths, Editor, TabWidth) ->
     Cmd = "CMD: " ++ atom_to_list(?MODULE) ++ ":move_fun_1(" ++ "\"" ++
	     FName ++ "\", " ++ integer_to_list(Line) ++
         ", " ++ integer_to_list(Col) ++ ", " ++ "\"" ++ TargetModorFileName ++ "\", " ++ "\""
        ++atom_to_list(CondCheck)++"\", "
        ++ "[" ++ wrangler_misc:format_search_paths(SearchPaths) ++ "]," ++ atom_to_list(Editor)
        ++ integer_to_list(TabWidth) ++ ").",
    CurModInfo = analyze_file(FName, SearchPaths, TabWidth),
    AnnAST = CurModInfo#modinfo.ast,
    case api_interface:pos_to_fun_def(AnnAST, {Line, Col}) of
	{ok, Def} ->
	    {value, {fun_def, {ModName, FunName, Arity, _Pos1, _Pos2}}} =
		lists:keysearch(fun_def, 1, wrangler_syntax:get_ann(Def)),
	    move_fun_2(CurModInfo, [{ModName, FunName, Arity}], TargetModorFileName,
		       CondCheck, SearchPaths, TabWidth, Editor, Cmd);
	{error, _Reason} ->
	    {ok, ExpAttr} = pos_to_export(AnnAST, {Line, Col}),
	    MFAs = get_exported_funs(CurModInfo#modinfo.modname, ExpAttr),
	    move_fun_2(CurModInfo, MFAs, TargetModorFileName,
		       CondCheck, SearchPaths, TabWidth, Editor, Cmd)
    end.

move_fun_2(CurModInfo, MFAs, TargetModorFileName, CheckCond, SearchPaths, TabWidth, Editor, Cmd) ->
    FName =CurModInfo#modinfo.filename,
    TargetFName = get_target_file_name(FName, TargetModorFileName),
    TargetModName = list_to_atom(filename:basename(TargetFName, ".erl")),
    NewTargetFile = case not filelib:is_file(TargetFName) of
			true -> create_new_file(TargetFName, TargetModName),
				true;
			_ -> false
		    end,
    Forms = wrangler_syntax:form_list_elements(CurModInfo#modinfo.ast),
    FunDefs = [{get_fun_mfa(F), F} || F <- Forms, defines(F, MFAs)],
    case FunDefs of
	[] ->  throw({error, "You have not selected a well-formed function definition or an export attribute."});
	_ -> ok
    end,
    TargetModInfo = analyze_file(TargetFName, SearchPaths, TabWidth),
    {UnDefinedMs, UnDefinedRs} = side_cond_check(FunDefs, CurModInfo, TargetModInfo, NewTargetFile, CheckCond),
    CG = wrangler_callgraph_server:gen_digraph_callgraph(FName),
    {MoveableDepFuns, UnDefinedMs1, UnDefinedRs1} =
	get_moveable_dependent_funs(MFAs, CurModInfo, TargetModInfo, CG),
    NewMFAs = MFAs ++ MoveableDepFuns,
    Info=CurModInfo#modinfo.info,
    FunsToExportInTargetMod = get_funs_to_export_in_target_mod(NewMFAs, CG, Info),
    FunsToExportInCurMod = local_funs_to_be_exported_in_cur_module(NewMFAs, CG, Info),
    digraph:delete(CG),
    NewUnDefinedMs = lists:usort(UnDefinedMs ++ UnDefinedMs1),
    NewUnDefinedRs = lists:usort(UnDefinedRs ++ UnDefinedRs1),
    AtomsToCheck=[FunName || {_M, FunName, _A} <- MFAs],
    move_fun_3(CurModInfo, TargetModInfo, NewMFAs, {NewUnDefinedMs, NewUnDefinedRs},
	       FunsToExportInCurMod, FunsToExportInTargetMod, 
	       AtomsToCheck, NewTargetFile,SearchPaths, TabWidth, Editor, Cmd).

move_fun_3(CurModInfo, TargetModInfo, MFAs, {UnDefinedMs, UnDefinedRs},
	   FunsToExportInCurMod, FunsToExportInTargetMod,
	   AtomsToCheck, NewTargetFile, SearchPaths, TabWidth, Editor, Cmd) ->
    Pid = start_atom_process(),
    {AnnAST1, TargetAnnAST1} =
	do_transformation(CurModInfo, TargetModInfo, MFAs, {UnDefinedMs, UnDefinedRs},
			  FunsToExportInCurMod, FunsToExportInTargetMod, Pid,
			  SearchPaths, TabWidth),
    FName = CurModInfo#modinfo.filename,
    ModName = CurModInfo#modinfo.modname,
    TargetFName = TargetModInfo#modinfo.filename,
    TargetModName = TargetModInfo#modinfo.modname,
    check_unsure_atoms(FName, AnnAST1, AtomsToCheck, f_atom, Pid),
    check_unsure_atoms(TargetFName, TargetAnnAST1, AtomsToCheck, f_atom, Pid),
    ExportedMFAs = exported_funs(MFAs, CurModInfo#modinfo.info),
    
    Results = case ExportedMFAs/=[] of
		  true ->
		      ?wrangler_io("\nChecking client modules in the following search paths: \n~p\n", [SearchPaths]),
		      ClientFiles = lists:delete(TargetFName, wrangler_modulegraph_server:get_client_files(FName, SearchPaths)),
		      refactor_in_client_modules(ClientFiles, ExportedMFAs, TargetModName, SearchPaths, TabWidth, Pid);
		  false ->
		      []
	      end,
    output_atom_warning_msg(Pid, not_renamed_warn_msg(AtomsToCheck), renamed_warn_msg(ModName)),
    stop_atom_process(Pid),
    FinalResults = case lists:member(Editor, [emacs, composite_emacs]) of
		       true ->
			   [{{FName, FName}, AnnAST1},
			    {{TargetFName, TargetFName, NewTargetFile}, TargetAnnAST1}| Results];
                       _ ->
			   [{{FName, FName}, AnnAST1}, {{TargetFName, TargetFName}, TargetAnnAST1}| Results]
		   end,
    wrangler_write_file:write_refactored_files(FinalResults, Editor, TabWidth, Cmd).
 

do_transformation(CurModInfo, TargetModInfo, MFAs, {UnDefinedMs, UnDefinedRs},
		  FunsToExportInCurMod, FunsToExportInTargetMod, Pid,
		  SearchPaths, TabWidth)->
    FileName=CurModInfo#modinfo.filename,
    Forms = wrangler_syntax:form_list_elements(CurModInfo#modinfo.ast),
    AttrsToAdd = get_attrs(Forms, UnDefinedMs, UnDefinedRs, TargetModInfo#modinfo.includes),
    GroupedForms = group_forms(Forms),
    %% FormsToBeMoved = [F || F <- Forms, defines(F, MFAs) orelse type_specifies(F, MFAs)],
    FormsToBeMoved=lists:append([Fs||Fs<-GroupedForms, one_of_defines(Fs, MFAs) orelse
					 one_of_type_specifies(Fs, MFAs)]),
    TargetModName=TargetModInfo#modinfo.modname,
    InScopeFunsInTargetMod = TargetModInfo#modinfo.inscope_funs,
    Args={FileName,MFAs, TargetModName,InScopeFunsInTargetMod, SearchPaths, TabWidth, Pid},
    FormsToBeMoved1 = transform_forms_to_be_moved(FormsToBeMoved, Args), 
    Args1={FileName, MFAs, FunsToExportInCurMod,TargetModName, SearchPaths, TabWidth, Pid},
    AnnAST1 = do_remove_fun(Forms,FormsToBeMoved, Args1),
    TargetAnnAST1 = do_add_fun(TargetModInfo, FormsToBeMoved1, AttrsToAdd,
     			       MFAs,FunsToExportInTargetMod, SearchPaths, TabWidth, Pid),
    {AnnAST1, TargetAnnAST1}.


%%==============================================================================
%% Functions defined in the current module, but used by the function to be moved.
%%===============================================================================


local_funs_to_be_exported_in_cur_module(MFAs = [{ModName,_,_}| _T], CG, Info) ->
    Ns = [digraph:out_neighbours(CG, {M,F,A}) || {M,F,A} <- MFAs],
    Ns1 = lists:usort(lists:append(Ns)) --MFAs,
    [{F,A} || {M, F, A} <- Ns1, M==ModName,
	       not api_refac:is_exported({F,A}, Info)].

get_funs_to_export_in_target_mod(MFAs, CG, Info) ->
    [{F, A} || {M, F, A} <- MFAs,
	       api_refac:is_exported({F, A}, Info) orelse
		 digraph:in_neighbours(CG, {M, F, A}) -- MFAs /= []].


%%======================================================================================
%% Transform the function to be moved.
%%======================================================================================

transform_forms_to_be_moved(Forms, Args) ->
    [transform_a_form_to_be_moved(Form, Args)||Form<-Forms].

transform_a_form_to_be_moved(Form, Args) ->
    element(1, api_ast_traverse:full_tdTP(fun do_transform_fun/2, Form, Args)).


do_transform_fun(Node, {FileName, MFAs = [{ModName, _, _}| _T], TargetModName,
			InScopeFunsInTargetMod, SearchPaths, TabWidth, Pid}) ->
    case wrangler_syntax:type(Node) of
	application -> transform_application_node(FileName, Node, MFAs, TargetModName, 
						  InScopeFunsInTargetMod, SearchPaths, TabWidth);
	implicit_fun -> transform_implicit_fun_node(Node, MFAs, ModName, TargetModName);
	tuple -> do_rename_fun_in_tuples(Node, {FileName, SearchPaths, MFAs, TargetModName, Pid, TabWidth});
	_ -> {Node, false}
    end.

transform_implicit_fun_node(Node, MFAs, ModName, TargetModName) ->
    Name = wrangler_syntax:implicit_fun_name(Node),
    case wrangler_syntax:type(Name) of
      arity_qualifier -> 
	    transform_arity_qualifier_node(Node, MFAs, ModName, Name);
      module_qualifier ->
	  Name = wrangler_syntax:implicit_fun_name(Node),
	  Mod = wrangler_syntax:module_qualifier_argument(Name),
	  Body = wrangler_syntax:module_qualifier_body(Name),
	  case wrangler_syntax:type(Mod) of
	    atom ->
		case wrangler_syntax:atom_value(Mod) of
		  ModName ->
		      B = wrangler_syntax:arity_qualifier_body(Body),
		      A = wrangler_syntax:arity_qualifier_argument(Body),
		      case {wrangler_syntax:type(B), wrangler_syntax:type(A)} of
			{atom, integer} ->
			    {F1, A1} = {wrangler_syntax:atom_value(Body), wrangler_syntax:integer_value(A)},
			    case lists:member({ModName, F1, A1}, MFAs) of
			      true ->
				  {copy_pos_attrs(Node,
						  wrangler_syntax:implicit_fun(
						       copy_pos_attrs(
						         Name, wrangler_syntax:module_qualifier(
							            copy_pos_attrs(Mod, wrangler_syntax:atom(TargetModName)), Body)))),
				   true};
			      false -> {Node, false}
			    end;
			_ -> {Node, false}
		      end;
		  _ -> {Node, false}
		end;
	    _ -> {Node, false}
	  end;
      _ -> {Node, false}
    end.

transform_arity_qualifier_node(Node, MFAs, ModName, Name) ->
    Body = wrangler_syntax:arity_qualifier_body(Name),
    A = wrangler_syntax:arity_qualifier_argument(Name),
    case {wrangler_syntax:type(Body), wrangler_syntax:type(A)} of
      {atom, integer} ->
	  {F1, A1} = {wrangler_syntax:atom_value(Body), wrangler_syntax:integer_value(A)},
	  case lists:member({ModName, F1, A1}, MFAs) of
	    true ->
		{Node, false};
	    false ->
		{copy_pos_attrs(Node,
				wrangler_syntax:implicit_fun(
				     copy_pos_attrs(Name, wrangler_syntax:module_qualifier(
							       wrangler_syntax:atom(ModName), Name)))), true}
	  end;
      _ -> {Node, false}
    end.

transform_application_node(FileName, Node, MFAs, TargetModName,
			   InScopeFunsInTargetMod, SearchPaths, TabWidth) ->
    Op = wrangler_syntax:application_operator(Node),
    Args = wrangler_syntax:application_arguments(Node),
    case lists:keysearch(fun_def, 1, wrangler_syntax:get_ann(Op)) of
	{value, {fun_def, {M, F, A, _, _}}} ->
	    case wrangler_syntax:type(Op) of
		atom ->
		    case lists:keysearch({M, F, A}, 1, wrangler_misc:apply_style_funs()) of
			{value, _} ->
			    transform_apply_style_calls(FileName, Node, MFAs, TargetModName, SearchPaths, TabWidth);
			false ->
			    case lists:member({M, F, A}, InScopeFunsInTargetMod) orelse  erl_internal:bif(M, F, A) orelse lists:member({M, F, A}, MFAs)
				of
				true ->
				    {Node, false};
				_ when M =/= '_' ->
				    Op1 = copy_pos_attrs(Op, wrangler_syntax:module_qualifier(wrangler_syntax:atom(M), Op)),
				    {copy_pos_attrs(Node, wrangler_syntax:application(Op1, Args)), true};
				_ ->
				    {Line, Col} = wrangler_syntax:get_pos(Op),
				    Str = "Wrangler could not infer where the function " ++ atom_to_list(F) ++ "/" ++ integer_to_list(A) ++ 
					    ", used at location {" ++ integer_to_list(Line) ++ "," ++ integer_to_list(Col) ++ "} in the current module, is defined.",
				    throw({error, Str})
			    end
		    end;
		_ ->
		    case M == TargetModName of
			true ->
			    Node1 = wrangler_syntax:application(
				         copy_pos_attrs(Op, wrangler_syntax:atom(F)),
				         Args),
			    {copy_pos_attrs(Node, Node1), true};
			_ ->
			    {Node, false}
		    end
	    end;
	_ -> {Node, false}
    end.
				

%%=========================================================================
%% Remove the function from the current module. 
%%=========================================================================
do_remove_fun(Forms, FormsToBemoved, Args={_,_,FunsToBeExported,_,_,_,_}) ->
    Forms0 =Forms -- FormsToBemoved,
    {Forms1, _C}= lists:unzip([process_a_form_in_original_mod(Form, Args)|| Form<-Forms0]),
    Forms2 = lists:append(Forms1),
    NewForms = case FunsToBeExported of 
		   [] -> Forms2;
		   [_H|_T] ->  Export = make_export(FunsToBeExported),
			       insert_export_form([Export], Forms2)
	       end,
    wrangler_syntax:form_list(NewForms).

%%=======================================================================
%% Add the function to the target module.
%%=======================================================================
do_add_fun(TargetModInfo, FormsToAdd, AttrsToAdd, MFAs = [{ModName, _, _}| _T],
	   FunsToExport, SearchPaths, TabWidth, Pid) ->
    FileName = TargetModInfo#modinfo.filename,
    TargetModName = TargetModInfo#modinfo.modname,
    Forms = wrangler_syntax:form_list_elements(TargetModInfo#modinfo.ast),
    Info = TargetModInfo#modinfo.info,
    TargetMFAs = [get_fun_mfa(F) || F <- Forms, wrangler_syntax:type(F) == function],
    NewFormsToAdd = [F || F <- FormsToAdd,  not  defines(F, [{ModName, F1, A1} || {_M,F1,A1} <- TargetMFAs])],
    Args = {FileName, MFAs, TargetModName, SearchPaths, TabWidth, Pid},
    ProcessedForms = lists:append([process_a_form_in_target_module(Form, Args) || Form <- Forms]),
    IsExported = [{F, A} || {_M, F, A} <- MFAs,
			    api_refac:is_exported({F, A}, Info)],
    FunsToExport1 = FunsToExport--IsExported,
    NewForms = case FunsToExport1 of
		   [] ->
		       insert_export_form(AttrsToAdd, ProcessedForms)++ NewFormsToAdd;
		   _ ->
		       Export = make_export(FunsToExport1),
                       insert_export_form([Export| AttrsToAdd], ProcessedForms) ++ NewFormsToAdd
	       end,
    wrangler_syntax:form_list(NewForms).


%%============================================================================
%% Refactoring in the target module.
%%============================================================================

process_a_form_in_target_module(Form, Args)->
    {FileName, MFAs=[{ModName, _,_}|_T],TargetModName, SearchPaths, TabWidth, Pid}=Args,
    case wrangler_syntax:type(Form) of
	function -> [remove_module_qualifier(FileName, Form, MFAs, TargetModName, SearchPaths, TabWidth, Pid)];
	attribute -> 
	    Name = wrangler_syntax:attribute_name(Form),
	    case wrangler_syntax:type(Name) of
		atom -> case wrangler_syntax:atom_value(Name) of
			    import ->
				[H, L] = wrangler_syntax:attribute_arguments(Form),
				case wrangler_syntax:atom_value(H) of
				    ModName ->
					Es = wrangler_syntax:list_elements(L),
					Es1 = [E || E <- Es,
						    F1 <- [wrangler_syntax:atom_value(wrangler_syntax:arity_qualifier_body(E))],
						    A1 <- [wrangler_syntax:integer_value(wrangler_syntax:arity_qualifier_argument(E))],
						    not lists:member({ModName, F1,A1}, MFAs)],
					case length(Es1) == 0 of
					    true -> [];
					    _ -> [copy_pos_attrs(Form,wrangler_syntax:attribute(Name, [H, wrangler_syntax:list(Es1)]))]
					end;
				    _ -> [Form]
				end;
				    _ -> [Form]
			end;
		_ -> [Form]
	    end;
	_ -> [Form]
    end.


%%============================================================================
%% Remove module qualifier.
%%============================================================================

remove_module_qualifier(FileName, Form, MFAs, TargetModName, SearchPaths, TabWidth, Pid) ->
    element(1, api_ast_traverse:full_tdTP(fun do_remove_module_qualifier/2, Form,
					  {FileName, MFAs, TargetModName, SearchPaths, TabWidth, Pid})).

do_remove_module_qualifier(Node, {FileName, MFAs, TargetModName, SearchPaths, TabWidth, Pid}) ->
    case wrangler_syntax:type(Node) of
	application ->
	    Op = wrangler_syntax:application_operator(Node),
	    Args = wrangler_syntax:application_arguments(Node),
	    case application_info(Node) of
		{{none, _FunName}, _Arity} -> {Node, true};
		{{ModName, FunName}, Arity} ->
		    case lists:member({ModName, FunName, Arity}, MFAs) of
			true ->
                            case wrangler_syntax:type(Op) of
                                module_qualifier ->
                                    Op1 = wrangler_syntax:module_qualifier_body(Op),
                                    Node1 = copy_pos_attrs(Node, wrangler_syntax:application(Op1, Args)),
                                    {Node1, true};
                                _ ->
                                    {Node, false}
                            end;
			_ ->
			    case lists:keysearch({ModName, FunName, Arity}, 1, wrangler_misc:apply_style_funs()) of
				{value, _} ->
				    transform_apply_style_calls(FileName, Node, MFAs, TargetModName, SearchPaths, TabWidth);
				false -> {Node, false}
			    end
		    end
	    end;
	implicit_fun -> process_implicit_fun(Node, MFAs, TargetModName);
	tuple -> do_rename_fun_in_tuples(Node, {FileName, SearchPaths, MFAs, TargetModName, Pid, TabWidth});
	_ -> {Node, false}
    end.

process_implicit_fun(Node, MFAs, TargetModName) ->
    Name = wrangler_syntax:implicit_fun_name(Node),
    case wrangler_syntax:type(Name) of
      arity_qualifier ->
	  {Node, false};
      module_qualifier ->
	    Mod = wrangler_syntax:module_qualifier_argument(Name),
	    Body = wrangler_syntax:module_qualifier_body(Name),
	    F= wrangler_syntax:arity_qualifier_body(Body),
	    A = wrangler_syntax:arity_qualifier_argument(Body),
	    case {wrangler_syntax:type(Mod), wrangler_syntax:type(F),
		  wrangler_syntax:type(A)} of
		{atom, atom, integer} ->
		    M1 =wrangler_syntax:atom_value(Mod),
		    F1 =wrangler_syntax:atom_value(F),
		    A1 =wrangler_syntax:integer_value(A),
		    case lists:member({M1,F1,A1}, MFAs) of
			true ->
			    {copy_pos_attrs(Node,
					    wrangler_syntax:implicit_fun(
					         copy_pos_attrs(
						   Name,wrangler_syntax:module_qualifier(
						             copy_pos_attrs(Mod,wrangler_syntax:atom(TargetModName)), Body)))), true};
			_ -> {Node, false}
		    end;
		_ -> {Node, false}
	    end;
	_ -> {Node, false}
    end.


insert_export_form(Attrs, Forms) ->
    {Forms11, Forms12} = lists:splitwith(fun (F) -> 
                                                 wrangler_syntax:type(F) == attribute orelse
                                                     wrangler_syntax:type(F) == comment
					 end, Forms),
    {Forms111, Forms112} =case lists:splitwith(fun(F) -> 
                                                       not is_attribute(F, export) 
                                               end, lists:reverse(Forms11)) of
                              {_, []} ->
                                  lists:splitwith(fun(F) -> 
                                                          wrangler_syntax:type(F) == comment
                                                  end, lists:reverse(Forms11));
                              {Fs1, Fs2} ->
                                  {Fs1, Fs2}
                          end,
    lists:reverse(Forms112)++Attrs ++ lists:reverse(Forms111) ++ Forms12.
    
   
%%=========================================================================
%% Refacotoring in the current module. 
%%========================================================================
process_a_form_in_original_mod(Form, Args) ->
    {FileName,MFAs,_, TargetModName, SearchPaths, TabWidth, Pid}=Args,
    FAs =[{F,A}||{_M,F,A}<-MFAs],
    case wrangler_syntax:type(Form) of
	function -> 
	    As = wrangler_syntax:get_ann(Form),
	    {value, {fun_def, {M, F, A, _, _}}} =
		lists:keysearch(fun_def,1, As),
	    case lists:member({M,F,A}, MFAs) of 
		true ->
		    {[], false};
		_ ->
	    	    {Form1, C}=add_change_module_qualifier(
				 Form, FileName, MFAs,TargetModName, SearchPaths, TabWidth, Pid),
		    {[Form1],C}
	    end;		 
	attribute -> 
	    Name = wrangler_syntax:attribute_name(Form),
	    case wrangler_syntax:type(Name) of
		atom ->
		    case wrangler_syntax:atom_value(Name) of
			export ->
			    [L] = wrangler_syntax:attribute_arguments(Form),
			    Es = wrangler_syntax:list_elements(L),
			    Es1 =[E|| E<-Es, 
				      F1 <- [wrangler_syntax:atom_value(wrangler_syntax:arity_qualifier_body(E))],
				      A1 <- [wrangler_syntax:integer_value(wrangler_syntax:arity_qualifier_argument(E))],
				      not lists:member({F1, A1},FAs)],
			    case length(Es1) == 0 of
				true -> 
				    {[],false} ;
				false -> 
				    case length(Es1) == length(Es) of 
					true -> {[Form], false};
					_ -> {[copy_pos_attrs(Form,wrangler_syntax:attribute(Name,[copy_pos_attrs(L, wrangler_syntax:list(Es1))]))], true}
				    end
			    end;
			'spec' -> case type_specifies(Form, MFAs) of 
				    true ->
					{[],true};
				    false ->
					{[Form], false}
				end;
			_  -> {[Form], false}
		    end;
		_  -> {[Form], false}
	    end;
	_-> {[Form], false}
    end.


%%============================================================================
%% Add module qualifier.
%%============================================================================
add_change_module_qualifier(Form,FileName,MFAs, TargetModName, SearchPaths, TabWidth, Pid) ->
    api_ast_traverse:full_tdTP(fun do_add_change_module_qualifier/2,
			       Form, {FileName, MFAs, TargetModName, SearchPaths, TabWidth, Pid}).

do_add_change_module_qualifier(Node, {FileName, MFAs = [{ModName,_,_}| _], TargetModName, SearchPaths, TabWidth, Pid}) ->
    case wrangler_syntax:type(Node) of
	application ->
	    Operator = wrangler_syntax:application_operator(Node),
	    Args = wrangler_syntax:application_arguments(Node),
	    case lists:keysearch(fun_def, 1, wrangler_syntax:get_ann(Operator)) of
		{value, {fun_def, {Mod1, Fun1, Ari1, _, _}}} ->
		    case lists:keysearch({Mod1, Fun1, Ari1}, 1, wrangler_misc:apply_style_funs()) of
			{value, _} ->
			    transform_apply_style_calls(FileName, Node, MFAs, TargetModName, SearchPaths, 8);
			false ->
			    case lists:member({Mod1, Fun1, Ari1}, MFAs) of
				true ->
				    case wrangler_syntax:type(Operator) of
					tuple ->
					    {rename_fun_in_tuple_op(Node, Operator, Args, TargetModName), true};
					_ ->
					    Node2 = copy_pos_attrs(Node, rename_in_qualified_app(Operator, Fun1, Args, TargetModName)),
					    {Node2, true}
				    end;
				_ -> {Node, false}
			    end
		    end;
		_ -> {Node, false}
	    end;
	implicit_fun ->
	    Name = wrangler_syntax:implicit_fun_name(Node),
	    case wrangler_syntax:type(Name) of
		arity_qualifier ->
		    Body = wrangler_syntax:arity_qualifier_body(Name),
		    A = wrangler_syntax:integer_value(wrangler_syntax:arity_qualifier_argument(Name)),
		    case wrangler_syntax:type(Body) of
			atom -> B = wrangler_syntax:atom_value(Body),
				case lists:member({ModName, B, A}, MFAs) of
				    true ->
					FunName1 = copy_pos_attrs(Name,
								  wrangler_syntax:module_qualifier(
								       copy_pos_attrs(
								         Name, wrangler_syntax:atom(TargetModName)), Name)),
					{copy_pos_attrs(Node, wrangler_syntax:implicit_fun(FunName1)), true};
				    _ -> {Node, false}
				end;
			_ -> {Node, false}
		    end;
		module_qualifier ->
		    Mod = wrangler_syntax:module_qualifier_argument(Name),
		    Body = wrangler_syntax:module_qualifier_body(Name),
		    case wrangler_syntax:type(Mod) of
			atom ->
			    case wrangler_syntax:atom_value(Mod) of
				ModName ->
				    B = wrangler_syntax:arity_qualifier_body(Body),
				    A = wrangler_syntax:arity_qualifier_argument(Body),
				    case {wrangler_syntax:type(B), wrangler_syntax:type(A)} of
					{atom, integer} ->
					    B1 = wrangler_syntax:atom_value(B),
					    A1 = wrangler_syntax:integer_value(A),
					    case lists:member({ModName, B1, A1}, MFAs) of
						true ->
						    {copy_pos_attrs(
						       Node,wrangler_syntax:implicit_fun(
							         copy_pos_attrs(
								   Name,wrangler_syntax:module_qualifier(
								             copy_pos_attrs(Mod,wrangler_syntax:atom(TargetModName)), Body)))), true};
						_ -> {Node, false}
					    end;
					_ -> {Node, false}
				    end;
				_ -> {Node, false}
			    end;
			_ -> {Node, false}
		    end
	    end;
	tuple -> do_rename_fun_in_tuples(Node, {FileName, SearchPaths, MFAs, TargetModName, Pid, TabWidth});
	_ -> {Node, false}
    end.

rename_in_qualified_app(Operator1, FunName1, Arguments1, TargetModName1) ->
    Operator2 = copy_pos_attrs(Operator1, 
			       wrangler_syntax:module_qualifier(wrangler_syntax:atom(TargetModName1),
							        wrangler_syntax:atom(FunName1))),
    wrangler_syntax:application(Operator2, Arguments1).


%% ====================================================================================
%%  Processing Client modules.
%%====================================================================================


refactor_in_client_modules(ClientFiles, MFAs, TargetModName, SearchPaths, TabWidth, Pid) ->
    case ClientFiles of
	[] -> [];
	[F| Fs] ->
	    ?wrangler_io("The current file under refactoring is:\n~p\n", [F]),
	    {ok, {AnnAST, Info}} = wrangler_ast_server:parse_annotate_file(F, true, SearchPaths, TabWidth),
	    {AnnAST1, Changed} = refactor_in_client_module_1(F, {AnnAST, Info}, MFAs, TargetModName, SearchPaths, TabWidth, Pid),
	    AtomsToCheck = lists:usort([FunName || {_M, FunName, _A} <- MFAs]),
	    check_unsure_atoms(F, AnnAST1, AtomsToCheck, f_atom, Pid),
	    if Changed ->
		   [{{F, F}, AnnAST1}| refactor_in_client_modules(Fs, MFAs, TargetModName, SearchPaths, TabWidth, Pid)];
	       true -> refactor_in_client_modules(Fs, MFAs, TargetModName, SearchPaths, TabWidth, Pid)
	    end
    end.
refactor_in_client_module_1(FileName,{AnnAST, _Info}, MFAs, TargetModName, SearchPaths, TabWidth,Pid) ->
    Forms  = wrangler_syntax:form_list_elements(AnnAST),
    {Forms1, C} = lists:unzip([process_in_client_module(FileName, Form, MFAs, TargetModName, SearchPaths, TabWidth, Pid)||Form <-Forms]),
    Forms2 = lists:append(Forms1),
    {copy_pos_attrs(AnnAST, wrangler_syntax:form_list(Forms2)), lists:member(true, C)}.

process_in_client_module(FileName, Form, MFAs = [{ModName, _, _}| _T], TargetModName, SearchPaths, TabWidth, Pid) ->
    case wrangler_syntax:type(Form) of
      function ->
	  {Form1, C} = change_module_qualifier(Form, FileName,MFAs, TargetModName, SearchPaths, TabWidth, Pid),
	  {[Form1], C};
      attribute ->
	  Name = wrangler_syntax:attribute_name(Form),
	  case wrangler_syntax:type(Name) of
	    atom -> case wrangler_syntax:atom_value(Name) of
		      import ->
			  case wrangler_syntax:attribute_arguments(Form) of
			    [H, L] ->
				case wrangler_syntax:atom_value(H) of
				  ModName ->
				      Es = wrangler_syntax:list_elements(L),
				      Es1 = [E || E <- Es,
						  F1 <- [wrangler_syntax:atom_value(wrangler_syntax:arity_qualifier_body(E))],
						  A1 <- [wrangler_syntax:integer_value(wrangler_syntax:arity_qualifier_argument(E))],
						  not lists:member({ModName, F1, A1}, MFAs)],
				      Es2 = Es -- Es1,
				      case length(Es1) == length(Es) of
					true ->
					    {[Form], false};
					false ->
					    Imp1 = copy_pos_attrs(Form, wrangler_syntax:attribute(
									     Name, [H, copy_pos_attrs(L,wrangler_syntax:list(Es1))])),
					    Imp2 = wrangler_syntax:attribute(wrangler_syntax:atom(import),
									     [wrangler_syntax:atom(TargetModName),
									      wrangler_syntax:list(Es2)]),
					    {[Imp1, Imp2], true}
				      end;
				  _ -> {[Form], false}
				end;
			    _ -> {[Form], false}
			  end;
		      _ -> {[Form], false}
		    end;
	    _ -> {[Form], false}
	  end;
      _ -> {[Form], false}
    end.


change_module_qualifier(Form, FileName, MFAs, TargetModName, SearchPaths, TabWidth, Pid) ->
    api_ast_traverse:full_tdTP(fun do_change_module_qualifier/2,
			       Form, {FileName, MFAs, TargetModName, SearchPaths, TabWidth, Pid}).

do_change_module_qualifier(Node, {FileName, MFAs, TargetModName, SearchPaths, TabWidth, Pid}) ->
    case wrangler_syntax:type(Node) of
	application ->
	    Op = wrangler_syntax:application_operator(Node),
	    Args = wrangler_syntax:application_arguments(Node),
	    case lists:keysearch(fun_def, 1, wrangler_syntax:get_ann(Op)) of
		{value, {fun_def, {Mod1, Fun1, Ari1, _, _}}} ->
		    case lists:keysearch({Mod1, Fun1, Ari1}, 1, wrangler_misc:apply_style_funs()) of
			{value, _} ->
			    transform_apply_style_calls(FileName, Node, MFAs, TargetModName, SearchPaths, TabWidth);
			false ->
			    case lists:member({Mod1, Fun1, Ari1}, MFAs) of
				true ->
				    case wrangler_syntax:type(Op) of
					module_qualifier ->
					    Node2 = copy_pos_attrs(Node, rename_in_qualified_app(Op, Fun1, Args, TargetModName)),
					    {Node2, true};
					tuple -> {rename_fun_in_tuple_op(Node, Op, Args, TargetModName), true};
					_ -> {Node, false}
				    end;
				false ->
				    {Node, false}
			    end
		    end;
		_ -> {Node, false}
	    end;
	implicit_fun -> process_implicit_fun(Node, MFAs, TargetModName);
	tuple -> do_rename_fun_in_tuples(Node, {FileName, SearchPaths, MFAs,
						TargetModName, Pid, TabWidth});
	_ -> {Node, false}
    end.

rename_fun_in_tuple_op(App,Op, Args, TargetModName) ->
    [M, F] = wrangler_syntax:tuple_elements(Op),
    Op1 = copy_pos_attrs(Op, wrangler_syntax:tuple([copy_pos_attrs(M, wrangler_syntax:atom(TargetModName)), F])),
    copy_pos_attrs(App, wrangler_syntax:application(Op1, Args)).

exported_funs(MFAs, Info) ->
    lists:filter(fun ({_M,F,A}) ->
			 api_refac:is_exported({F, A}, Info)
		 end, MFAs).
      
%% =====================================================================
%% @spec application_info(Tree::syntaxTree())->term()
%%
application_info(Node) ->
    case wrangler_syntax:type(Node) of
	application ->
	    Operator = wrangler_syntax:application_operator(Node),
	    Arguments = wrangler_syntax:application_arguments(Node),
	    Arity = length(Arguments),
	    case wrangler_syntax:type(Operator) of
		atom -> Op = wrangler_syntax:atom_value(Operator),
			{{none,Op}, Arity}; 
		module_qualifier ->
		        Mod = wrangler_syntax:module_qualifier_argument(Operator),
		        Fun = wrangler_syntax:module_qualifier_body(Operator),
		        T1 = wrangler_syntax:type(Mod),
		        T2 = wrangler_syntax:type(Fun),
		        case T1 of 
		  	    atom -> 
				Mod1 = wrangler_syntax:atom_value(Mod),
				case T2 of 
					atom -> Fun1 = wrangler_syntax:atom_value(Fun),
						{{Mod1, Fun1}, Arity};
				        _ ->{{Mod1, expressionfunname}, Arity}
					end;
			    _ -> case T2 of 
				       atom -> Fun1 = wrangler_syntax:atom_value(Fun),
					       {{expressionmodname, Fun1}, Arity};
				       _ -> {{expressionmodname,expressionfunname}, Arity}
				   end
			end;
		_  -> {{none,expressionoperator}, Arity}
	    end;
	_ -> erlang:error(badarg)
    end.

transform_apply_style_calls(FileName, Node, MFAs, NewModName, SearchPaths, TabWidth) ->
    Op = wrangler_syntax:application_operator(Node),
    Arguments = wrangler_syntax:application_arguments(Node),
    [N1, N2, Mod, Fun, Args] = case length(Arguments) of
				   5 -> Arguments;
				   4 -> [none| Arguments];
				   3 -> [none, none| Arguments]
			       end,
    M1 = wrangler_misc:try_eval(FileName, Mod, SearchPaths, TabWidth),
    F1 = wrangler_misc:try_eval(FileName, Fun, SearchPaths, TabWidth),
    case F1 of
	{value, FunName} ->
	    case M1 of
		{value, ModName} ->
		    A1 = case wrangler_syntax:type(Args) of
			     list ->
				 wrangler_syntax:list_length(Args);
			     nil -> 0;
			     _ -> '_'
			 end,
		    case lists:member({ModName, FunName, A1}, MFAs) of
			true ->
			    Mod2 = copy_pos_attrs(Mod, wrangler_syntax:atom(NewModName)),
			    App = case length(Arguments) of
				      5 -> wrangler_syntax:application(Op, [N1, N2, Mod2, Fun, Args]);
				      4 -> wrangler_syntax:application(Op, [N2, Mod2, Fun, Args]);
				      3 -> wrangler_syntax:application(Op, [Mod2, Fun, Args])
				  end,
			    {copy_pos_attrs(Node, App), true};
			false -> {Node, false}
		    end;
		_ ->
		    {Node, false}
	    end;
	_ -> {Node, false}
    end.

do_rename_fun_in_tuples(Node, {FileName, SearchPaths, MFAs, TargetModName, Pid, TabWidth}) ->
    Es = wrangler_syntax:tuple_elements(Node),
    case length(Es) >= 3 of
	true ->
	    [E3, E2, E1| _T] = lists:reverse(Es),
	    case wrangler_misc:try_eval(FileName, E1, SearchPaths, TabWidth) of
		{value, ModName} ->
		    case wrangler_syntax:type(E2) of
			atom ->
			    F = wrangler_syntax:type(E2),
			    case wrangler_syntax:type(E3) of
				list ->
				    A = wrangler_syntax:list_length(E3),
				    do_rename_fun_in_tuples_1(Node, FileName, MFAs, TargetModName,
							      Pid, Es, {ModName, F, A});
				nil ->
				    do_rename_fun_in_tuples_1(Node, FileName, MFAs, TargetModName,
							      Pid, Es, {ModName, F, 0});
				integer ->
				    A = wrangler_syntax:integer_value(E3),
				    do_rename_fun_in_tuples_1(Node, FileName, MFAs, TargetModName,
							      Pid, Es, {ModName, F, A});
				_ -> {Node, false}
			    end;
			_ ->
			    {Node, false}
		    end;
		_ ->
		    {Node, false}
	    end;
	_ -> {Node, false}
    end.


do_rename_fun_in_tuples_1(Node, FileName, MFAs, TargetModName,
			  Pid, Es, {M, F, A}) ->
    [E3, E2, E1| T] = lists:reverse(Es),
    NewE1 = copy_pos_attrs(E1, wrangler_syntax:atom(TargetModName)),
    case lists:member({M, F, A}, MFAs) of
	true ->
	    Pid ! {add_renamed, {FileName, Node}},
	    Node1 = wrangler_syntax:tuple(lists:reverse([E3, E2, NewE1| T])),
	    {copy_pos_attrs(Node, Node1), true};
	false ->
	    {Node, false}
    end.


is_the_same_fun(FunDef1, FunDef2) ->
    Ann1 = wrangler_syntax:get_ann(FunDef1),
    {value, {fun_def, {M1, F1, A1, _, _}}}=lists:keysearch(fun_def,1,Ann1),
    Ann2 = wrangler_syntax:get_ann(FunDef2),
    {value, {fun_def, {M2, F2, A2, _, _}}} =lists:keysearch(fun_def,1,Ann2),
    FunDef11= reset_attrs(FunDef1, {M1, F1, A1}),
    FunDef21=reset_attrs(FunDef2, {M2, F2, A2}),
    FunDef11 == FunDef21.

reset_attrs(Node, {M, F, A}) ->
    Fun = fun (T) ->
		  T1 = case wrangler_syntax:type(T) of
			   module_qualifier ->
			       {value, {fun_def, DefInfo}} = lists:keysearch(fun_def,1,wrangler_syntax:get_ann(T)),
			       wrangler_syntax:add_ann({fun_def, DefInfo}, wrangler_syntax:module_qualifier_body(T));
			   variable ->
			       wrangler_syntax:default_literals_vars(T, wrangler_syntax:variable_name(T));
			   integer ->
			       wrangler_syntax:default_literals_vars(T, wrangler_syntax:integer_value(T));
			   float ->
			       wrangler_syntax:default_literals_vars(T, wrangler_syntax:float_value(T));
			   char ->
			       wrangler_syntax:default_literals_vars(T, wrangler_syntax:char_value(T));
			   string ->
			       wrangler_syntax:default_literals_vars(T, wrangler_syntax:string_value(T));
			   atom ->
			       wrangler_syntax:default_literals_vars(T, wrangler_syntax:atom_value(T));
			   nil -> wrangler_syntax:default_literals_vars(T, nil);
			   underscore -> wrangler_syntax:default_literals_vars(T, '_');
			   _ -> T
		       end,
		  case lists:keysearch(fun_def,1,wrangler_syntax:get_ann(T1)) of
		      {value, {fun_def, {ModName, FunName, Arity, _, _}}} ->
			  case {ModName, FunName, Arity} of
			      {M, F, A} -> wrangler_syntax:set_pos(wrangler_syntax:remove_comments(wrangler_syntax:set_ann(T1, [])), {0,0});
			      _ -> wrangler_syntax:set_pos(
				        wrangler_syntax:remove_comments(
				             wrangler_syntax:set_ann(T1,[{fun_def, {ModName, FunName, Arity, {0,0}, {0,0}}}])),{0,0})
			  end;
		      _ -> wrangler_syntax:set_pos(wrangler_syntax:remove_comments(wrangler_syntax:set_ann(T1, [])), {0,0})
		  end
	  end,
    api_ast_traverse:map(Fun, Node).




%%================================================================================
%%              Side Condition Checking. 
%%================================================================================

-spec(analyze_file/3::(FName::filename(), SearchPaths::[dir()|filename()], integer()) ->
                            #modinfo{}).
analyze_file(FName, SearchPaths, TabWidth) ->
    Dir = filename:dirname(FName),
    DefaultIncls = [filename:join(Dir, X) || X <- wrangler_misc:default_incls()],
    NewSearchPaths = SearchPaths ++ DefaultIncls,
    case wrangler_epp:parse_file(FName, NewSearchPaths, [], TabWidth,
			         wrangler_misc:file_format(FName))
    of
	{ok, AST, {MDefs, _MUses1}} ->
	    MacroDefs = get_macro_defs(MDefs),
	    ModInfo = get_mod_info_from_parse_tree(AST),
            RecordDefs = case lists:keyfind(records, 1, ModInfo) of
                              {records, RecordDefsInFile} ->
	         		 [{Name, lists:keysort(1, [{F, prettyprint(FDef)} || {F, FDef} <- Fields])}
	         		  || {Name, Fields} <- RecordDefsInFile];
	         	     false -> []
	         	 end,
            {ok, {AnnAST, Info}} = wrangler_ast_server:parse_annotate_file(FName, true, SearchPaths, TabWidth),
            Forms = wrangler_syntax:form_list_elements(AnnAST),
	    Includes = lists:append([lists:flatmap(fun (A) ->
							   case wrangler_syntax:type(A) of
							       string -> [wrangler_syntax:string_value(A)];
							       _ -> []
							   end
	 					   end, Args)
				     || F <- Forms, is_attribute(F, include) orelse is_attribute(F, include_lib),
					Args <- [wrangler_syntax:attribute_arguments(F)]]),
	    InscopeFuns = api_refac:inscope_funs(ModInfo),
            #modinfo{filename = FName,
			 modname = get_module_name(Info),
			 inscope_funs = InscopeFuns,
			 macro_defs = MacroDefs,
			 record_defs = RecordDefs,
			 includes = Includes,
			 ast = AnnAST,
			 info = Info};
	_ ->
	    throw({error, "Refactoring failed because Wrangler could not parse the target module."})
    end.

get_macro_defs(MDefs) -> 
    lists:flatmap(fun get_macro_def_1/1, MDefs).
get_macro_def_1({{_,Name}, {Args, Toks}})->
     [{Name, {Args, wrangler_misc:concat_toks(Toks)}}];
get_macro_def_1({{_, Name}, ArgToks}) when is_list(ArgToks) ->
    [{Name, {Args, wrangler_misc:concat_toks(Toks)}} || {_Arity, {Args, Toks}} <- ArgToks];
get_macro_def_1({{_, _Name}, _ArgToks}) ->
    [].
 
side_cond_check(FunDefs, CurModInfo, TargetModInfo, NewTargetFile, CheckCond) ->
    try side_cond_check(FunDefs, CurModInfo, TargetModInfo, CheckCond) 
    catch
	throw:E2 -> throw(E2);
	E1:E2 ->
	    case NewTargetFile of 
		true -> 
		    TargetFName=TargetModInfo#modinfo.filename,
		    file:delete(TargetFName);
		false-> ok
	    end,
	    throw({E1,E2})
    end.
side_cond_check(FunDefs, CurModInfo, TargetModInfo, CheckCond) ->
    case CheckCond of
      true ->
	  check_fun_name_clash(FunDefs, TargetModInfo);
      false ->
	  ok
    end,
    check_macros_records(FunDefs, CurModInfo, TargetModInfo, CheckCond).

-spec(check_macros_records/4::(FunDefs::[any()], #modinfo{}, #modinfo{}, boolean()) ->
                                   {[atom()], [atom()]}).
check_macros_records(FunDefs, CurModInfo, TargetModInfo, CheckCond) ->
    CurRecordDefs=CurModInfo#modinfo.record_defs,
    TargetRecordDefs=TargetModInfo#modinfo.record_defs,
    UnDefinedRecords = check_records(FunDefs, CurRecordDefs, TargetRecordDefs, CheckCond),
    CurMacroDefs=CurModInfo#modinfo.macro_defs,
    TargetMacroDefs=TargetModInfo#modinfo.macro_defs,
    UnDefinedMacros = check_macros(FunDefs,CurMacroDefs, TargetMacroDefs, CheckCond),
    {UnDefinedMacros, UnDefinedRecords}.


check_fun_name_clash(FunDefs=[{{ModName, _, _},_}|_T],TargetModInfo)->
    TargetInscopeFuns=TargetModInfo#modinfo.inscope_funs,
    TargetForms=wrangler_syntax:form_list_elements(TargetModInfo#modinfo.ast),
    FAs = [{F, A}||{{_M, F, A},_}<-FunDefs],
    Clash = lists:filter(fun ({M, F, A}) ->
				 lists:member({F,A}, FAs) andalso ModName =/= M
			 end, TargetInscopeFuns),
    case Clash of
	[] ->true;
	_ ->
	    SameFuns= [get_fun_mfa(Form)|| 
			  Form <- TargetForms, 
			  defines(Form, Clash),
			  {_M,F,A}<-[get_fun_mfa(Form)],
			  is_the_same_fun(
			    Form, hd([Def||{{M1, F1,A1}, Def}<-FunDefs,
					   {M1,F1,A1}=={ModName, F,A}]))],
	    Clash1=Clash--SameFuns, 
	    case Clash1 of 
		[] -> true;
		_ ->
		    throw({error, "Moving function(s): "++format_funs(Clash1)++ 
			   "will cause confliction in the target module."})
	    end
    end.

-spec(check_records/4::(FunDefs::[any()], [{atom(), any()}], [{atom(), any()}], boolean()) -> [atom()]).
check_records(FunDefs,CurRecordDefs,TargetRecordDefs, CheckCond) ->
    UsedRecords = lists:usort(lists:append([wrangler_misc:collect_used_records(FunDef) || {_, FunDef} <- FunDefs])),
    UsedRecordDefs = [{Name, lists:keysort(1, Fields)} || {Name, Fields} <- CurRecordDefs, lists:member(Name, UsedRecords)],
    CurUnDefinedUsedRecords = UsedRecords -- [Name || {Name, _Fields} <- UsedRecordDefs],
    UsedRecordDefsInTargetFile = [{Name, lists:keysort(1, Fields)}
				  || {Name, Fields} <- TargetRecordDefs,
				     lists:member(Name, UsedRecords)],
    RecordsWithDiffDefs = [R || {R, Def} <- UsedRecordDefs,
				case lists:keysearch(R, 1, UsedRecordDefsInTargetFile) of
				    {value, {R, Def1}} ->
					Def /= Def1;
				    false ->
					false
				end],
    UnDefinedRecords = UsedRecords-- element(1, lists:unzip(UsedRecordDefsInTargetFile)),
    case CheckCond of 
        true -> 
            case RecordsWithDiffDefs of
                [] ->
                    ok;
                [_] ->
                    Msg = format("Record: ~p, used by the function(s) to be moved, "
                                 "is defined differently in the target module.",RecordsWithDiffDefs),
                    throw({error, Msg});
                _ ->
                    Msg = "The following records: "++format_names(RecordsWithDiffDefs)++" used by the function(s) to be moved,"
                        "are defined differently in the target module.",
                    throw({error, Msg})
            end;
        false ->
            ok
    end,
    case CheckCond of 
        true ->
            case CurUnDefinedUsedRecords of
                [] ->
                    ok;
                [_R] ->
                    Msg1 = format("Record: ~p, is used by the function(s) to be moved, but not defined in the current module.",
                                  CurUnDefinedUsedRecords),
                    throw({error, Msg1});
                _ ->
                    Msg1 = format("The following records: ~p are used by the function(s) to be moved, but not defined in the current module",
                                  [CurUnDefinedUsedRecords]),
                    throw({error, Msg1})
            end;
        false ->
            ok
    end,
    UnDefinedRecords.

-spec(check_macros/4::([any()], [{atom(), any()}], [{atom(), any()}], boolean()) -> [atom()]).
check_macros(FunDefs, CurMacroDefs, TargetMacroDefs, CheckCond) ->
    UsedMacros = lists:usort(lists:append([wrangler_misc:collect_used_macros(FunDef) || {_, FunDef} <- FunDefs])),
    UsedMacroDefs = [{Name, {Args, Def}}
		     || {Name, {Args, Def}} <- CurMacroDefs,
			lists:member(Name, UsedMacros)],
    UsedMacroDefsInTargetFile =
	[{Name, {Args, Def}} || {Name, {Args, Def}} <- TargetMacroDefs, lists:member(Name, UsedMacros)],
    UnDefinedMacros = UsedMacros-- element(1, lists:unzip(UsedMacroDefsInTargetFile)),
    MsWithDiffDefs = [M || {M, Def} <- UsedMacroDefs,
			   case lists:keysearch(M, 1, UsedMacroDefsInTargetFile) of
			       {value, {M, Def1}} ->
				   Def /= Def1;
			       false ->
				   false
			   end],
    case CheckCond of 
        true -> case UnDefinedMacros of
                    [] -> ok;
                    [_] -> Msg = format("Macro: ~p, is used by the function(s) to be moved, but not defined in the current module.",
                                        UnDefinedMacros),
                           throw({error, Msg});
                    _ -> Msg = format("The following macros: ~p are used by the function(s) to be moved, but not defined in the current module.",
                                      [UnDefinedMacros]),
                         throw({error, Msg})
                end;
        false ->
            ok
    end,
    case CheckCond of 
        true -> case MsWithDiffDefs of
                    [] ->  ok;
                    [_] -> Msg1 = format("Macro: ~p, used by the function(s) to be moved "
                                         "is defined differently in the target module", MsWithDiffDefs),
                           throw({warning, Msg1 ++ ", still continue?"});
                    _ -> Msg1 = "Macros: "++format_names(MsWithDiffDefs)++" used by the function(s) to be moved "
                             "are defined differently in the target module",
                         throw({warning, Msg1 ++ ", still continue?"})
                end;
        false ->
            ok
    end,
    UnDefinedMacros.

-spec(get_mod_info_from_parse_tree/1::([any()]) ->module_info()).    
get_mod_info_from_parse_tree(AST) ->
    AST1 = lists:filter(fun (F) ->
				case F of
				    {attribute, _, file, _} -> false;
				    {attribute, _, type, {{record, _}, _, _}} -> false;
				    _ -> true
				end
			end, AST),
    SyntaxTree = wrangler_recomment:recomment_forms(AST1, []),
    wrangler_syntax_lib:analyze_forms(SyntaxTree).

%%================================================================================
%%              Some Utility Functions 
%%================================================================================

get_attrs(Forms, Ms,Rs, TargetIncludes) ->
    Attrs=[F || F<-Forms, defines(F, Ms, Rs)],
    case length(Attrs)==length(Ms++Rs) of
	true ->
	    Attrs;
	false ->
	    Includes=get_includes(Forms, TargetIncludes),
	    Includes++Attrs
    end.

defines(Form, Ms, Rs) ->
    case wrangler_syntax:type(Form) of
	attribute ->
	    Name = wrangler_syntax:attribute_name(Form),
	    case wrangler_syntax:type(Name) of
		atom ->
		    AttrName = wrangler_syntax:atom_value(Name),
		    case AttrName == record orelse AttrName == define of
			true ->
			    Type = hd(wrangler_syntax:attribute_arguments(Form)),
			    case wrangler_syntax:type(Type) of
				atom ->
				    TypeName = wrangler_syntax:atom_value(Type),
				    AttrName == record andalso lists:member(TypeName, Rs) orelse
					AttrName == define andalso lists:member(TypeName, Ms);
				variable ->
				    TypeName= wrangler_syntax:variable_name(Type),
				    AttrName == define andalso lists:member(TypeName, Ms);
				application ->
				    Op = wrangler_syntax:application_operator(Type),
				    case wrangler_syntax:type(Op) of
					atom ->
					    TypeName= wrangler_syntax:atom_value(Op),
					    AttrName == define andalso lists:member(TypeName, Ms); 
					variable ->
					    TypeName= wrangler_syntax:variable_name(Op),
					    AttrName == define andalso lists:member(TypeName, Ms);
					_ -> false
				    end;
				_ -> false
			    end;
			_ -> false
		    end;
		_ ->
		    false
	    end;
	_ -> false
    end.

get_includes(Forms, Incs) ->
    [F||F<-Forms,
	is_attribute(F, include) orelse is_attribute(F, include_lib),
	Arg <- wrangler_syntax:attribute_arguments(F),
	case wrangler_syntax:type(Arg) of
	    string -> not (lists:member(wrangler_syntax:string_value(Arg), Incs));
	    _ -> false
	end].


type_specifies(Form, MFAs) ->
    FAs =[{F,A}||{_M,F,A}<-MFAs],
    case wrangler_syntax:type(Form) of
	attribute ->
	    Name = wrangler_syntax:attribute_name(Form),
	    case wrangler_syntax:type(Name) of
		atom -> case wrangler_syntax:atom_value(Name) of
			    'spec' ->
				FA = hd(wrangler_syntax:attribute_arguments(Form)),
				case wrangler_syntax:type(FA) of
				    tuple ->
					[A1, F1|_T] = lists:reverse(wrangler_syntax:tuple_elements(FA)),
					lists:member({wrangler_syntax:atom_value(F1),
						      wrangler_syntax:integer_value(A1)},FAs);
				    _ -> false
				end;
			    _ -> false
			end;
		_ -> false
	    end;
	_ -> false
    end.




get_moveable_dependent_funs(MFAs, CurModInfo, TargetModInfo, CG) ->
    Info = CurModInfo#modinfo.info,
    DepMFAs=get_dependent_funs(Info, MFAs, CG),
    case DepMFAs of 
	[] -> {[],[],[]};
	_ ->
	    Forms = wrangler_syntax:form_list_elements(CurModInfo#modinfo.ast),
	    DepDefs = [{get_fun_mfa(F), F} || F <- Forms, defines(F, DepMFAs)],
	    check_dependent_funs(DepMFAs, DepDefs, CurModInfo, TargetModInfo,CG)
    end.

get_dependent_funs(Info, MFAs = [{ModName, _, _}| _], CG) ->
    Reachable = digraph_utils:reachable(MFAs, CG),
    Reachable1 = [{M, F, A} || {M, F, A} <- Reachable, M == ModName,
			        not api_refac:is_exported({F, A}, Info)],
    Funs = get_closed_dependent_funs(Reachable1, MFAs, CG),
    Funs -- MFAs.

get_closed_dependent_funs([], MFAs, _CG) ->
    MFAs;
get_closed_dependent_funs(Vs, MFAs, CG) ->
    AllVs = lists:usort(Vs++MFAs),
    Vs1=[V||V<-Vs, not lists:member(V, MFAs),
		     digraph:in_neighbours(CG, V)--AllVs/=[]],
    case Vs1 of
	[] ->
	    Vs;
	_ -> get_closed_dependent_funs(Vs--Vs1, MFAs, CG)
    end.
     
check_dependent_funs(MFAs=[{ModName,_,_}|_T], FunDefs, CurModInfo, TargetModInfo, CG) ->
    TargetInScopeFuns = TargetModInfo#modinfo.inscope_funs,
    FAs = [{F, A}||{_M, F, A}<-MFAs],
    Clash = [{ModName, F, A}||{_M,F,A}<-TargetInScopeFuns, 
                              lists:member({F,A}, FAs)],
    NewMFAs = MFAs--Clash,
    NewFunDefs=[{F, Def}||{F, Def}<-FunDefs, lists:member(F, NewMFAs)],
    FMRs=lists:append([try check_macros_records([{F,FunDef}], CurModInfo,TargetModInfo, false) of
	     {Ms, Rs} ->
		   [{F, Ms,Rs}]
	  catch
	      _E1:_E2 ->
		  []
	  end ||{F,FunDef}<-NewFunDefs]),
    NewMFAs1=element(1, lists:unzip3(FMRs)),
    ClosedNewMFAs=get_closed_dependent_funs(NewMFAs1, MFAs, CG),
    FMRs1=[{F, Ms, Rs}||{F,Ms,Rs}<-FMRs,
			 lists:member(F,ClosedNewMFAs)],
    {Fs, Ms1, Rs1} =lists:unzip3(FMRs1),
    {Fs, lists:append(Ms1), lists:append(Rs1)}.

format_funs([]) ->
   "";
format_funs([{_M, F, A}|Fs]) ->
    lists:flatten(io_lib:format("~p/~p,", [F, A])++format_funs(Fs)).

format_names([])->
    "";
format_names([N|Ns]) ->
    lists:flatten(io_lib:format("~p,", [N])++format_names(Ns)).

get_target_file_name(CurrentFName, TargetModorFileName) ->
    ErrMsg = {error, "Illegal target module/file name."},
    TargetModName = case filename:extension(TargetModorFileName) of
			".erl" -> filename:basename(TargetModorFileName, ".erl");
			[] -> filename:basename(TargetModorFileName, ".erl");
			_ -> throw(ErrMsg)
		    end,
    TargetFileName = filename:join([filename:dirname(CurrentFName), filename:dirname(TargetModorFileName),
				    TargetModName ++ ".erl"]),
    case api_refac:is_fun_name(TargetModName) of
	true ->
	    TargetFileName;
	_ -> throw(ErrMsg)
    end.


create_new_file(TargetFName, TargetModName) ->
    S = "-module("++atom_to_list(TargetModName)++").",
    case file:write_file(TargetFName, list_to_binary(S)) of 
	ok -> ok;
	{error, Reason} ->
	    Msg = io_lib:format("Wrangler could not write to file ~s: ~w \n",
				[TargetFName, Reason]),
	    throw({error, Msg})
    end.

defines(Form, MFAs) ->
    case wrangler_syntax:type(Form) of
	function ->
	  case lists:keysearch(fun_def, 1, wrangler_syntax:get_ann(Form)) of
	      {value, {fun_def, {M, F, A, _, _}}} ->
		  lists:member({M,F,A}, MFAs);
	      _ -> false
	  end;
	_ -> false
    end.

one_of_defines(Forms, MFAs) ->
    lists:any(fun(F) ->
		      defines(F, MFAs)
	      end, Forms).

one_of_type_specifies(Forms, MFAs) ->
    lists:any(fun(F) ->
		      type_specifies(F, MFAs)
	      end, Forms).

group_forms(Forms) ->
    group_forms(lists:reverse(Forms), []).

group_forms([], Acc)->
    Acc;
group_forms([F|Fs], Acc) ->
    case wrangler_syntax:type(F) == function orelse
	is_attribute(F, 'spec') of
	true ->
	    {Fs1, Fs2} = lists:splitwith(fun(Form)->
						 wrangler_syntax:type(Form) == comment
					 end, Fs),
	    group_forms(Fs2, [lists:reverse([F|Fs1])|Acc]);
	_ -> 
	    group_forms(Fs, [[F]|Acc])
    end.

  
get_fun_mfa(Form) ->
    case wrangler_syntax:type(Form) of
	function ->
	    {value, {fun_def, {M, F, A, _, _}}} =
		lists:keysearch(fun_def, 1, wrangler_syntax:get_ann(Form)),
	    {M,F,A};
	_ -> throw({error, "error in function refac_move_fun:get_fun_mfa/1"})
    end.

pos_to_export(AnnAST, Pos) ->
    Forms = wrangler_syntax:form_list_elements(AnnAST),
    Fs = [F || F <- Forms,
	       is_export_attribute(F),
	       {Start, End} <- [wrangler_misc:start_end_loc(F)],
	       Start=<Pos,
	       End>=Pos],
    case Fs of
	[F] ->
	    {ok,F};
	_ -> {error,"No export attribute has been selected."}
    end.
    
is_export_attribute(Form) ->
    case wrangler_syntax:type(Form) of
	attribute ->
	    Name = wrangler_syntax:attribute_name(Form),
	    wrangler_syntax:type(Name) == atom andalso
		wrangler_syntax:atom_value(Name) == 'export';
	_ -> false
    end.


get_exported_funs(ModName,ExportAttr) ->
    [L]=wrangler_syntax:attribute_arguments(ExportAttr),
    Es = wrangler_syntax:list_elements(L),
    [{ModName,F,A}|| E<-Es, 
	       F <- [wrangler_syntax:atom_value(wrangler_syntax:arity_qualifier_body(E))],
	       A <- [wrangler_syntax:integer_value(wrangler_syntax:arity_qualifier_argument(E))]].


make_export(Names) ->
    Es = [wrangler_syntax:arity_qualifier(wrangler_syntax:atom(F),wrangler_syntax:integer(A))
	  || {F, A} <- Names],
    wrangler_syntax:attribute(wrangler_syntax:atom('export'), [wrangler_syntax:list(Es)]).

copy_pos_attrs(E1, E2) ->
    wrangler_syntax:copy_pos(E1, wrangler_syntax:copy_attrs(E1, E2)).    

    
is_attribute(F, Name) ->
    wrangler_syntax:type(F) == attribute andalso
      wrangler_syntax:type(wrangler_syntax:attribute_name(F)) == atom andalso
	wrangler_syntax:atom_value(wrangler_syntax:attribute_name(F)) == Name.

prettyprint(none) ->
    none;
prettyprint(Node) -> 
    wrangler_prettypr:format(Node).


renamed_warn_msg(ModName) ->
    "\n=================================================================================\n"
     "WARNING: Wrangler has renamed the uses of '"++atom_to_list(ModName) ++ "' to the target "
     "module name within the following expressions while without enough "
     "syntactic/semantic information.\n Please check manually!\n".
  
not_renamed_warn_msg(FunNames) ->
    "\n=================================================================================\n"
     "WARNING: Wrangler could not infer whether the uses of '" ++format_names(FunNames) ++"' at the following positions "
     "refer to the function moved.\n Please check manually for necessary module name changes!\n".
format(Format, Options) ->
    lists:flatten(io_lib:format(Format, Options)).

get_module_name(ModInfo) ->
    case lists:keysearch(module, 1, ModInfo) of
      {value, {module, ModName}} -> ModName;
      false -> throw({error, "Wrangler could not infer the current module name."})
    end.

get_file_name(ModorFileName, SearchPaths) ->
    case filelib:is_file(ModorFileName) of
	true ->
	    {ok, ModorFileName};
	false ->
	    wrangler_misc:modname_to_filename(ModorFileName, SearchPaths)
    end.
