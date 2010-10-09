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

-module(refac_move_fun).

-export([move_fun/6, move_fun_1/7, move_fun_eclipse/6, move_fun_1_eclipse/6]).

-export([analyze_file/3]).
-import(refac_atom_utils, [output_atom_warning_msg/3, check_unsure_atoms/5, 
			   stop_atom_process/1, start_atom_process/0]).

-include("../include/wrangler.hrl").

-record(module_info, {filename, 
		      modname,
		      inscope_funs, 
		      macro_defs, 
		      record_defs, 
		      includes, 
		      ast, 
		      info}).
%==========================================================================================
%%-spec(move_fun/6::(filename(),integer(),integer(), string(), [dir()], integer())->
%%	     {ok, [filename()]} | {question, string()}).
%%==========================================================================================
move_fun(FName, Line, Col, TargetModorFileName, SearchPaths, TabWidth) ->
    move_fun(FName, Line, Col, TargetModorFileName, SearchPaths, TabWidth, emacs).

%%-spec(move_fun_1/7::(filename(),integer(),integer(), string(),boolean(), [dir()], integer())->
%%	     {ok, [filename()]}).
move_fun_1(FName, Line, Col, TargetModorFileName, CheckCond, SearchPaths, TabWidth) ->
    move_fun_1(FName, Line, Col, TargetModorFileName, CheckCond, SearchPaths, TabWidth, emacs).


%%-spec(move_fun_eclipse/6::(filename(),integer(),integer(), string(),[dir()], integer())
%%        ->  {ok, [{filename(), filename(), string()}]} | {question, string()}).

move_fun_eclipse(FName, Line, Col, TargetModorFileName, SearchPaths, TabWidth) ->
    move_fun(FName, Line, Col, TargetModorFileName, SearchPaths, TabWidth, eclipse).


%% THIS interface need to be changed; and should inform George of the changes.
%%-spec(move_fun_1_eclipse/6::(filename(),integer(),integer(), string(),[dir()], integer())
%%        ->  {ok, [{filename(), filename(), string()}]}).

move_fun_1_eclipse(FName, Line, Col, TargetModorFileName, SearchPaths, TabWidth) ->
    move_fun_1(FName, Line, Col, TargetModorFileName, true, SearchPaths, TabWidth, eclipse).


move_fun(FName, Line, Col, TargetModorFileName, SearchPaths, TabWidth, Editor) ->
    ?wrangler_io("\nCMD: ~p:move_fun(~p, ~p, ~p, ~p, ~p, ~p).\n",
		 [?MODULE, FName, Line, Col, TargetModorFileName, SearchPaths, TabWidth]),
    TargetFName = get_target_file_name(FName, TargetModorFileName),
    case TargetFName of
	FName -> throw({error, "The target module is the same as the current module."});
	_ -> ok
    end,
    {ok, {AnnAST, _Info}} = refac_util:parse_annotate_file(FName, true, SearchPaths, TabWidth),
    case interface_api:pos_to_fun_def(AnnAST, {Line, Col}) of
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
	true -> move_fun_1(FName, Line, Col, TargetFName, true, SearchPaths, TabWidth, Editor);
	false -> {question, "Target file "++ TargetFName ++ " does not exist, create it?"}
    end.
  
move_fun_1(FName, Line, Col, TargetModorFileName, CondCheck, SearchPaths, TabWidth, Editor) ->
    Cmd = "CMD: " ++ atom_to_list(?MODULE) ++ ":move_fun_1(" ++ "\"" ++
	FName ++ "\", " ++ integer_to_list(Line) ++
	", " ++ integer_to_list(Col) ++ ", " ++ "\"" ++ TargetModorFileName ++ "\", " ++ "\""
	++atom_to_list(CondCheck)++"\", "
	++ "[" ++ refac_misc:format_search_paths(SearchPaths) ++ "]," ++ integer_to_list(TabWidth) ++ ").",
    CurModInfo = analyze_file(FName, SearchPaths, TabWidth),
    AnnAST=CurModInfo#module_info.ast,
    case interface_api:pos_to_fun_def(AnnAST, {Line, Col}) of
	{ok, Def} ->
	    {value, {fun_def, {ModName, FunName, Arity, _Pos1, _Pos2}}} =
		lists:keysearch(fun_def, 1, refac_syntax:get_ann(Def)),
	    move_fun_2(CurModInfo, [{ModName, FunName, Arity}], TargetModorFileName,
		       CondCheck, SearchPaths, TabWidth, Editor, Cmd);
	{error, _Reason} ->
	    {ok, ExpAttr}=pos_to_export(AnnAST, {Line, Col}), 
	    MFAs = get_exported_funs(CurModInfo#module_info.modname, ExpAttr),
	    move_fun_2(CurModInfo, MFAs, TargetModorFileName,
		      CondCheck, SearchPaths, TabWidth, Editor, Cmd)
    end.

move_fun_2(CurModInfo, MFAs, TargetModorFileName, CheckCond, SearchPaths, TabWidth, Editor, Cmd) ->
    FName =CurModInfo#module_info.filename,
    TargetFName = get_target_file_name(FName, TargetModorFileName),
    TargetModName = list_to_atom(filename:basename(TargetFName, ".erl")),
    NewTargetFile = case not filelib:is_file(TargetFName) of
			true -> create_new_file(TargetFName, TargetModName),
				true;
			_ -> false
		    end,
    Forms = refac_syntax:form_list_elements(CurModInfo#module_info.ast),
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
    Info=CurModInfo#module_info.info,
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
	   AtomsToCheck, NewTargetFile,SearchPaths, TabWidth, Editor, Cmd) ->
    Pid = start_atom_process(),
    {AnnAST1, TargetAnnAST1} =
	do_transformation(CurModInfo, TargetModInfo, MFAs, {UnDefinedMs, UnDefinedRs},
			  FunsToExportInCurMod, FunsToExportInTargetMod, Pid,
			 SearchPaths, TabWidth),
    FName =CurModInfo#module_info.filename,
    ModName=CurModInfo#module_info.modname,
    TargetFName = TargetModInfo#module_info.filename,
    TargetModName=TargetModInfo#module_info.modname,
    check_unsure_atoms(FName, AnnAST1, AtomsToCheck, f_atom, Pid),
    check_unsure_atoms(TargetFName, TargetAnnAST1, AtomsToCheck, f_atom, Pid),
    ExportedMFAs = exported_funs(MFAs, CurModInfo#module_info.info),
    Results = case ExportedMFAs/=[] of
		  true ->
		      ?wrangler_io("\nChecking client modules in the following search paths: \n~p\n", [SearchPaths]),
		      ClientFiles = lists:delete(TargetFName, refac_util:get_client_files(FName, SearchPaths)),
		      refactor_in_client_modules(ClientFiles, ExportedMFAs, TargetModName, SearchPaths, TabWidth, Pid);
		  false ->
		      []
	      end,
    output_atom_warning_msg(Pid, not_renamed_warn_msg(AtomsToCheck), renamed_warn_msg(ModName)),
    stop_atom_process(Pid),
    output_refactor_results(FName, TargetFName, Editor, Cmd, NewTargetFile, AnnAST1, TargetAnnAST1, Results).

do_transformation(CurModInfo, TargetModInfo, MFAs, {UnDefinedMs, UnDefinedRs},
		  FunsToExportInCurMod, FunsToExportInTargetMod, Pid,
		  SearchPaths, TabWidth)->
    FileName=CurModInfo#module_info.filename,
    Forms = refac_syntax:form_list_elements(CurModInfo#module_info.ast),
    AttrsToAdd = get_attrs(Forms, UnDefinedMs, UnDefinedRs, TargetModInfo#module_info.includes),
    GroupedForms = group_forms(Forms),
    %% FormsToBeMoved = [F || F <- Forms, defines(F, MFAs) orelse type_specifies(F, MFAs)],
    FormsToBeMoved=lists:append([Fs||Fs<-GroupedForms, one_of_defines(Fs, MFAs) orelse
					 one_of_type_specifies(Fs, MFAs)]),
    TargetModName=TargetModInfo#module_info.modname,
    InScopeFunsInTargetMod = TargetModInfo#module_info.inscope_funs,
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

local_funs_to_be_exported_in_cur_module(MFAs=[{ModName,_,_}|_T], CG, Info) ->
    Ns=[digraph:out_neighbours(CG, {M,F,A})||{M,F,A}<-MFAs],
    Ns1 =lists:usort(lists:append(Ns)) --MFAs,
    [{F,A} || {M,F, A} <- Ns1,M==ModName, 
		not refac_misc:is_exported({F,A}, Info)].

get_funs_to_export_in_target_mod(MFAs, CG, Info) ->
    [{F, A} || {M, F, A} <- MFAs,
		  refac_misc:is_exported({F, A}, Info) orelse
		      digraph:in_neighbours(CG, {M, F, A}) -- MFAs /= []].


%%======================================================================================
%% Transform the function to be moved.
%%======================================================================================

transform_forms_to_be_moved(Forms, Args) ->
    [transform_a_form_to_be_moved(Form, Args)||Form<-Forms].

transform_a_form_to_be_moved(Form, Args) ->
    element(1, ast_traverse_api:full_tdTP(fun do_transform_fun/2, Form, Args)).


do_transform_fun(Node, {FileName, MFAs = [{ModName, _, _}| _T], TargetModName,
			InScopeFunsInTargetMod, SearchPaths, TabWidth, Pid}) ->
    case refac_syntax:type(Node) of
	application -> transform_application_node(FileName, Node, MFAs, TargetModName, 
						  InScopeFunsInTargetMod, SearchPaths, TabWidth);
	implicit_fun -> transform_implicit_fun_node(Node, MFAs, ModName, TargetModName);
	tuple -> do_rename_fun_in_tuples(Node, {FileName, SearchPaths, MFAs, TargetModName, Pid, TabWidth});
	_ -> {Node, false}
    end.

transform_implicit_fun_node(Node, MFAs, ModName, TargetModName) ->
    Name = refac_syntax:implicit_fun_name(Node),
    case refac_syntax:type(Name) of
      arity_qualifier -> 
	    transform_arity_qualifier_node(Node, MFAs, ModName, Name);
      module_qualifier ->
	  Name = refac_syntax:implicit_fun_name(Node),
	  Mod = refac_syntax:module_qualifier_argument(Name),
	  Body = refac_syntax:module_qualifier_body(Name),
	  case refac_syntax:type(Mod) of
	    atom ->
		case refac_syntax:atom_value(Mod) of
		  ModName ->
		      B = refac_syntax:arity_qualifier_body(Body),
		      A = refac_syntax:arity_qualifier_argument(Body),
		      case {refac_syntax:type(B), refac_syntax:type(A)} of
			{atom, integer} ->
			    {F1, A1} = {refac_syntax:atom_value(Body), refac_syntax:integer_value(A)},
			    case lists:member({ModName, F1, A1}, MFAs) of
			      true ->
				  {copy_pos_attrs(Node,
						  refac_syntax:implicit_fun(
						    copy_pos_attrs(
						      Name, refac_syntax:module_qualifier(
							      copy_pos_attrs(Mod, refac_syntax:atom(TargetModName)), Body)))),
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
    Body = refac_syntax:arity_qualifier_body(Name),
    A = refac_syntax:arity_qualifier_argument(Name),
    case {refac_syntax:type(Body), refac_syntax:type(A)} of
      {atom, integer} ->
	  {F1, A1} = {refac_syntax:atom_value(Body), refac_syntax:integer_value(A)},
	  case lists:member({ModName, F1, A1}, MFAs) of
	    true ->
		{Node, false};
	    false ->
		{copy_pos_attrs(Node,
				refac_syntax:implicit_fun(
				  copy_pos_attrs(Name, refac_syntax:module_qualifier(
							 refac_syntax:atom(ModName), Name)))), true}
	  end;
      _ -> {Node, false}
    end.

transform_application_node(FileName, Node, MFAs, TargetModName, 
			   InScopeFunsInTargetMod, SearchPaths, TabWidth) ->
    Op = refac_syntax:application_operator(Node),
    Args = refac_syntax:application_arguments(Node),
    case lists:keysearch(fun_def, 1, refac_syntax:get_ann(Op)) of
      {value, {fun_def, {M, F, A, _, _}}} ->
	  case refac_syntax:type(Op) of
	    atom ->
		  case lists:keysearch({M, F, A}, 1, refac_misc:apply_style_funs()) of 
		      {value, _} ->
			  transform_apply_style_calls(FileName, Node, MFAs, TargetModName, SearchPaths, TabWidth);
		      false ->
			  case lists:member({M, F, A}, InScopeFunsInTargetMod) orelse
			      erl_internal:bif(M, F, A) orelse lists:member({M, F, A}, MFAs)
			  of
			      true ->
				  {Node, false};
			      _ when M =/= '_' ->
				  Op1 = copy_pos_attrs(Op, refac_syntax:module_qualifier(refac_syntax:atom(M), Op)),
				  {copy_pos_attrs(Node, refac_syntax:application(Op1, Args)), true};
			      _ ->
				  {Line, Col} = refac_syntax:get_pos(Op),
				  Str = "Wrangler could not infer where the function " ++ atom_to_list(F) ++ "/" ++ integer_to_list(A) ++
				      ", used at location {" ++ integer_to_list(Line) ++ "," ++ integer_to_list(Col) ++ "} in the current module, is defined.",
				  throw({error, Str})
			  end
		  end;
	    _ ->
		case M == TargetModName of
		  true ->
			Node1 = refac_syntax:application(
				  copy_pos_attrs(Op, refac_syntax:atom(F)),
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
    refac_syntax:form_list(NewForms).

%%=======================================================================
%% Add the function to the target module.
%%=======================================================================
do_add_fun(TargetModInfo, FormsToAdd, AttrsToAdd, MFAs=[{ModName, _,_}|_T],
	   FunsToExport, SearchPaths, TabWidth, Pid) ->
    FileName=TargetModInfo#module_info.filename,
    TargetModName=TargetModInfo#module_info.modname,
    Forms = refac_syntax:form_list_elements(TargetModInfo#module_info.ast),
    Info = TargetModInfo#module_info.info,
    TargetMFAs = [get_fun_mfa(F) || F <- Forms, refac_syntax:type(F)==function],
    NewFormsToAdd = [F || F <- FormsToAdd, not defines(F, [{ModName, F1,A1}||{_M,F1,A1}<-TargetMFAs])],
    Args= {FileName, MFAs, TargetModName, SearchPaths, TabWidth, Pid},
    ProcessedForms = lists:append([process_a_form_in_target_module(Form, Args)|| Form <- Forms]),
    IsExported = [{F, A} || {_M, F, A} <- MFAs,
			    refac_misc:is_exported({F, A}, Info)],
    FunsToExport1=FunsToExport--IsExported,
    NewForms = case FunsToExport1 of
		   [] ->
		       insert_export_form(AttrsToAdd, ProcessedForms)++ NewFormsToAdd;
		   _ ->
		       Export = make_export(FunsToExport1),
		       insert_export_form([Export|AttrsToAdd], ProcessedForms) ++ NewFormsToAdd
	       end,
    refac_syntax:form_list(NewForms).


%%============================================================================
%% Refactoring in the target module.
%%============================================================================

process_a_form_in_target_module(Form, Args)->
    {FileName, MFAs=[{ModName, _,_}|_T],TargetModName, SearchPaths, TabWidth, Pid}=Args,
    case refac_syntax:type(Form) of
	function -> [remove_module_qualifier(FileName, Form, MFAs, TargetModName, SearchPaths, TabWidth, Pid)];
	attribute -> 
	    Name = refac_syntax:attribute_name(Form),
	    case refac_syntax:type(Name) of
		atom -> case refac_syntax:atom_value(Name) of
			    import ->
				[H, L] = refac_syntax:attribute_arguments(Form),
				case refac_syntax:atom_value(H) of 
				    ModName ->
					Es = refac_syntax:list_elements(L),
					Es1 = [E || E <- Es,
						    F1<-[refac_syntax:atom_value(refac_syntax:arity_qualifier_body(E))],
						    A1<-[refac_syntax:integer_value(refac_syntax:arity_qualifier_argument(E))],
						    not lists:member({ModName, F1,A1}, MFAs)],
					case length(Es1) == 0 of
					    true -> [];
					    _ -> [copy_pos_attrs(Form,refac_syntax:attribute(Name, [H, refac_syntax:list(Es1)]))]
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
    element(1, ast_traverse_api:full_tdTP(fun do_remove_module_qualifier/2, Form,
					  {FileName, MFAs, TargetModName, SearchPaths, TabWidth, Pid})).


do_remove_module_qualifier(Node, {FileName, MFAs, TargetModName, SearchPaths, TabWidth, Pid}) ->
    case refac_syntax:type(Node) of
      application ->
	  Op = refac_syntax:application_operator(Node),
	  Args = refac_syntax:application_arguments(Node),
	  case application_info(Node) of
	      {{none, _FunName}, _Arity} -> {Node, true};
	      {{ModName, FunName}, Arity} ->
		  case lists:member({ModName, FunName, Arity}, MFAs) of 
		      true ->
			  Op1 = copy_pos_attrs(Op, refac_syntax:atom(FunName)),
			  Node1 = copy_pos_attrs(Node, refac_syntax:application(Op1, Args)),
			  {Node1, true};
		      _ ->
			  case lists:keysearch({ModName, FunName, Arity}, 1, refac_misc:apply_style_funs()) of
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
    Name = refac_syntax:implicit_fun_name(Node),
    case refac_syntax:type(Name) of
      arity_qualifier ->
	  {Node, false};
      module_qualifier ->
	    Mod = refac_syntax:module_qualifier_argument(Name),
	    Body = refac_syntax:module_qualifier_body(Name),
	    F= refac_syntax:arity_qualifier_body(Body),
	    A = refac_syntax:arity_qualifier_argument(Body),
	    case {refac_syntax:type(Mod), refac_syntax:type(F), 
		  refac_syntax:type(A)} of
		{atom, atom, integer} ->
		    M1 =refac_syntax:atom_value(Mod),
		    F1 =refac_syntax:atom_value(F),
		    A1 =refac_syntax:integer_value(A),
		    case lists:member({M1,F1,A1}, MFAs) of
			true ->
			    {copy_pos_attrs(Node,
					    refac_syntax:implicit_fun(
					      copy_pos_attrs(
						Name,refac_syntax:module_qualifier(
						       copy_pos_attrs(Mod,refac_syntax:atom(TargetModName)), Body)))), true};
			_ -> {Node, false}
		    end;
		_ -> {Node, false}
	    end;
	_ -> {Node, false}
    end.


insert_export_form(Attrs, Forms) ->
    {Forms11, Forms12} = lists:splitwith(fun (F) -> 
						 is_attribute(F, module) orelse
					         is_attribute(F, export) orelse
					         is_attribute(F, import) orelse 
						 is_attribute(F, include) orelse
						 is_attribute(F, include_lib) orelse
					         refac_syntax:type(F) == comment
					 end, Forms),
    {Forms111, Forms112}= lists:splitwith(fun(F) -> 
						  refac_syntax:type(F)==comment 
					  end, lists:reverse(Forms11)),
    lists:reverse(Forms112)++Attrs ++ lists:reverse(Forms111) ++ Forms12.
    
   
%%=========================================================================
%% Refacotoring in the current module. 
%%========================================================================
process_a_form_in_original_mod(Form, Args) ->
    {FileName,MFAs,_, TargetModName, SearchPaths, TabWidth, Pid}=Args,
    FAs =[{F,A}||{_M,F,A}<-MFAs],
    case refac_syntax:type(Form) of 
	function -> 
	    As = refac_syntax:get_ann(Form),
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
	    Name = refac_syntax:attribute_name(Form),
	    case refac_syntax:type(Name) of 
		atom ->
		    case refac_syntax:atom_value(Name) of
			export ->
			    [L] = refac_syntax:attribute_arguments(Form),
			    Es = refac_syntax:list_elements(L),
			    Es1 =[E|| E<-Es, 
				      F1<-[refac_syntax:atom_value(refac_syntax:arity_qualifier_body(E))],
				      A1<-[refac_syntax:integer_value(refac_syntax:arity_qualifier_argument(E))],
				      not lists:member({F1, A1},FAs)],
			    case length(Es1) == 0 of
				true -> 
				    {[],false} ;
				false -> 
				    case length(Es1) == length(Es) of 
					true -> {[Form], false};
					_ -> {[copy_pos_attrs(Form,refac_syntax:attribute(Name,[refac_syntax:list(Es1)]))], true}
				    end
			    end;
			spec -> case type_specifies(Form, MFAs) of 
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
    ast_traverse_api:full_tdTP(fun do_add_change_module_qualifier/2,
			       Form, {FileName,MFAs, TargetModName, SearchPaths, TabWidth, Pid}).

do_add_change_module_qualifier(Node, {FileName, MFAs=[{ModName,_,_}|_], TargetModName, SearchPaths, TabWidth, Pid}) ->
    case refac_syntax:type(Node) of
      application ->
	  Operator = refac_syntax:application_operator(Node),
	  Args = refac_syntax:application_arguments(Node),
	  case lists:keysearch(fun_def, 1, refac_syntax:get_ann(Operator)) of
	    {value, {fun_def, {Mod1, Fun1, Ari1, _, _}}} ->
		case lists:keysearch({Mod1, Fun1, Ari1}, 1, refac_misc:apply_style_funs()) of
		  {value, _} ->
		      transform_apply_style_calls(FileName, Node, MFAs, TargetModName, SearchPaths, 8);  
		  false ->
		      case lists:member({Mod1, Fun1, Ari1}, MFAs) of
			  true ->
			      case refac_syntax:type(Operator) of
				  tuple -> 
				      {rename_fun_in_tuple_op(Node, Operator, Args, TargetModName), true};
				  _ ->
				      Node2=copy_pos_attrs(Node, rename_in_qualified_app(Operator, Fun1, Args, TargetModName)),				      
				      {Node2, true}
			      end;
			  _ -> {Node, false}
		      end
		end;
	      _ -> {Node, false}
	  end;
      implicit_fun ->
	    Name = refac_syntax:implicit_fun_name(Node),
	    case refac_syntax:type(Name) of
		arity_qualifier ->
		    Body = refac_syntax:arity_qualifier_body(Name),
		    A = refac_syntax:integer_value(refac_syntax:arity_qualifier_argument(Name)),
		    case refac_syntax:type(Body) of
			atom -> B = refac_syntax:atom_value(Body),
				case lists:member({ModName, B, A}, MFAs) of
				    true ->
					FunName1 = copy_pos_attrs(Name, 
								  refac_syntax:module_qualifier(
								    copy_pos_attrs(
								      Name, refac_syntax:atom(TargetModName)), Name)),
					{copy_pos_attrs(Node, refac_syntax:implicit_fun(FunName1)), true};
				    _ -> {Node, false}
				end;
			_ -> {Node, false}
		    end;
	    module_qualifier ->
		Mod = refac_syntax:module_qualifier_argument(Name),
		Body = refac_syntax:module_qualifier_body(Name),
		case refac_syntax:type(Mod) of
		  atom ->
		      case refac_syntax:atom_value(Mod) of
			  ModName ->
			    B = refac_syntax:arity_qualifier_body(Body),
			    A = refac_syntax:arity_qualifier_argument(Body),
			    case {refac_syntax:type(B), refac_syntax:type(A)} of
			      {atom, integer} ->
				    B1 = refac_syntax:atom_value(B),
				    A1 = refac_syntax:integer_value(A),
				    case lists:member({ModName, B1, A1}, MFAs) of
					true ->
					    {copy_pos_attrs(
					       Node,refac_syntax:implicit_fun(
						      copy_pos_attrs(
							Name,refac_syntax:module_qualifier(
							       copy_pos_attrs(Mod,refac_syntax:atom(TargetModName)), Body)))), true};
				    _ -> {Node, false}
				  end;
			      _ -> {Node, false}
			    end;
			_ -> {Node, false}
		      end
		end
	  end;
	tuple -> do_rename_fun_in_tuples(Node, {FileName, SearchPaths, MFAs, TargetModName, Pid, TabWidth});
	_ -> {Node, false}
    end.

rename_in_qualified_app(Operator1, FunName1, Arguments1, TargetModName1) ->
    Operator2 = copy_pos_attrs(Operator1, 
			       refac_syntax:module_qualifier(refac_syntax:atom(TargetModName1),
							     refac_syntax:atom(FunName1))),
    refac_syntax:application(Operator2, Arguments1).


%% ====================================================================================
%%  Processing Client modules.
%%====================================================================================

refactor_in_client_modules(ClientFiles, MFAs, TargetModName, SearchPaths, TabWidth, Pid) ->
    case ClientFiles of
      [] -> [];
      [F| Fs] ->
	  ?wrangler_io("The current file under refactoring is:\n~p\n", [F]),
	  {ok, {AnnAST, Info}} = refac_util:parse_annotate_file(F, true, SearchPaths, TabWidth),
	  {AnnAST1, Changed} = refactor_in_client_module_1(F, {AnnAST, Info}, MFAs, TargetModName, SearchPaths, TabWidth, Pid),
	  AtomsToCheck = lists:usort([FunName || {_M, FunName, _A} <- MFAs]),
	  check_unsure_atoms(F, AnnAST1, AtomsToCheck, f_atom, Pid),
	  if Changed ->
		 [{{F, F}, AnnAST1}| refactor_in_client_modules(Fs, MFAs, TargetModName, SearchPaths, TabWidth, Pid)];
	     true -> refactor_in_client_modules(Fs, MFAs, TargetModName, SearchPaths, TabWidth, Pid)
	  end
    end.
refactor_in_client_module_1(FileName,{AnnAST, _Info}, MFAs, TargetModName, SearchPaths, TabWidth,Pid) ->
    Forms  = refac_syntax:form_list_elements(AnnAST),
    {Forms1, C} = lists:unzip([process_in_client_module(FileName, Form, MFAs, TargetModName, SearchPaths, TabWidth, Pid)||Form <-Forms]),
    Forms2 = lists:append(Forms1),
    {copy_pos_attrs(AnnAST, refac_syntax:form_list(Forms2)), lists:member(true, C)}.

process_in_client_module(FileName, Form, MFAs = [{ModName, _, _}| _T], TargetModName, SearchPaths, TabWidth, Pid) ->
    case refac_syntax:type(Form) of
      function ->
	  {Form1, C} = change_module_qualifier(Form, FileName,MFAs, TargetModName, SearchPaths, TabWidth, Pid),
	  {[Form1], C};
      attribute ->
	  Name = refac_syntax:attribute_name(Form),
	  case refac_syntax:type(Name) of
	    atom -> case refac_syntax:atom_value(Name) of
		      import ->
			  case refac_syntax:attribute_arguments(Form) of
			    [H, L] ->
				case refac_syntax:atom_value(H) of
				  ModName ->
				      Es = refac_syntax:list_elements(L),
				      Es1 = [E || E <- Es,
						  F1 <- [refac_syntax:atom_value(refac_syntax:arity_qualifier_body(E))],
						  A1 <- [refac_syntax:integer_value(refac_syntax:arity_qualifier_argument(E))],
						  not lists:member({ModName, F1, A1}, MFAs)],
				      Es2 = Es -- Es1,
				      case length(Es1) == length(Es) of
					true ->
					    {[Form], false};
					false ->
					    Imp1 = copy_pos_attrs(Form, refac_syntax:attribute(
									  Name, [H, refac_syntax:list(Es1)])),
					    Imp2 = refac_syntax:attribute(refac_syntax:atom(import),
									  [refac_syntax:atom(TargetModName),
									   refac_syntax:list(Es2)]),
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
    ast_traverse_api:full_tdTP(fun do_change_module_qualifier/2,
			       Form, {FileName, MFAs, TargetModName, SearchPaths, TabWidth, Pid}).

do_change_module_qualifier(Node, {FileName, MFAs, TargetModName, SearchPaths, TabWidth, Pid}) ->
    case refac_syntax:type(Node) of
	application ->
	    Op = refac_syntax:application_operator(Node),
	    Args = refac_syntax:application_arguments(Node),
	    case lists:keysearch(fun_def, 1, refac_syntax:get_ann(Op)) of
		{value, {fun_def, {Mod1, Fun1, Ari1, _, _}}} ->
		    case lists:keysearch({Mod1, Fun1, Ari1}, 1, refac_misc:apply_style_funs()) of
			{value, _} ->
			    transform_apply_style_calls(FileName, Node, MFAs, TargetModName, SearchPaths, TabWidth);
			false ->
			    case lists:member({Mod1, Fun1, Ari1}, MFAs) of
				true ->
				    case refac_syntax:type(Op) of
					module_qualifier ->
					    Node2=copy_pos_attrs(Node, rename_in_qualified_app(Op, Fun1, Args, TargetModName)),
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
						TargetModName,Pid, TabWidth});
	_ -> {Node, false}
    end.

rename_fun_in_tuple_op(App,Op, Args, TargetModName) ->
    [M, F] = refac_syntax:tuple_elements(Op),
    Op1 = copy_pos_attrs(Op, refac_syntax:tuple([copy_pos_attrs(M, refac_syntax:atom(TargetModName)), F])),
    copy_pos_attrs(App, refac_syntax:application(Op1, Args)).
    

exported_funs(MFAs, Info) ->  
    lists:filter(fun({_M,F,A})->
			 refac_misc:is_exported({F, A}, Info)
		 end, MFAs).
      
%% =====================================================================
%% @spec applicaation_info(Tree::syntaxTree())->term()
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
	_ -> erlang:error(badarg)
    end.

transform_apply_style_calls(FileName, Node, MFAs, NewModName, SearchPaths, TabWidth) ->
    Op = refac_syntax:application_operator(Node),
    Arguments = refac_syntax:application_arguments(Node),
    [N1, N2, Mod, Fun, Args] = case length(Arguments) of
				 5 -> Arguments;
				 4 -> [none| Arguments];
				 3 -> [none, none| Arguments]
			       end,
    M1 = refac_misc:try_eval(FileName, Mod, SearchPaths, TabWidth),
    F1 = refac_misc:try_eval(FileName, Fun, SearchPaths, TabWidth),
    case F1 of
	{value, FunName} ->
	    case M1 of
		{value, ModName} ->
		    A1 = case refac_syntax:type(Args) of
			     list ->
				 refac_syntax:list_length(Args);
			     nil -> 0;
			     _ -> '_'
			 end,
		    case lists:member({ModName,FunName, A1}, MFAs) of
			true ->
			    Mod2 = copy_pos_attrs(Mod, refac_syntax:atom(NewModName)),
			    App = case length(Arguments) of
				      5 -> refac_syntax:application(Op, [N1, N2, Mod2, Fun, Args]);
				      4 -> refac_syntax:application(Op, [N2, Mod2, Fun, Args]);
				      3 -> refac_syntax:application(Op, [Mod2, Fun, Args])
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
    Es = refac_syntax:tuple_elements(Node),
    case length(Es) >= 3 of
      true ->
	  [E3, E2, E1| _T] = lists:reverse(Es),
	  case refac_misc:try_eval(FileName, E1, SearchPaths, TabWidth) of
	    {value, ModName} ->
		case refac_syntax:type(E2) of
		  atom ->
		      F = refac_syntax:type(E2),
		      case refac_syntax:type(E3) of
			list ->
			    A = refac_syntax:list_length(E3),
			    do_rename_fun_in_tuples_1(Node, FileName, MFAs, TargetModName,
						      Pid, Es, {ModName, F, A});
			nil ->
			    do_rename_fun_in_tuples_1(Node, FileName, MFAs, TargetModName,
						      Pid, Es, {ModName, F, 0});
			integer ->
			    A = refac_syntax:integer_value(E3),
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
    NewE1 = copy_pos_attrs(E1, refac_syntax:atom(TargetModName)),
    case lists:member({M, F, A}, MFAs) of
	true ->
	    Pid ! {add_renamed, {FileName, Node}},
	    Node1 = refac_syntax:tuple(lists:reverse([E3, E2, NewE1| T])),
	    {copy_pos_attrs(Node, Node1), true};
	false ->
	    {Node, false}
    end.


is_the_same_fun(FunDef1, FunDef2) ->
    Ann1 = refac_syntax:get_ann(FunDef1),
    {value, {fun_def, {M1, F1, A1, _, _}}}=lists:keysearch(fun_def,1,Ann1),
    Ann2 = refac_syntax:get_ann(FunDef2),
    {value, {fun_def, {M2, F2, A2, _, _}}} =lists:keysearch(fun_def,1,Ann2),
    FunDef11= reset_attrs(FunDef1, {M1, F1, A1}),
    FunDef21=reset_attrs(FunDef2, {M2, F2, A2}),
    FunDef11 == FunDef21.

reset_attrs(Node, {M, F, A}) ->
    Fun = fun(T) -> 
		  T1 = case refac_syntax:type(T) of 
			   module_qualifier ->
			       {value, {fun_def, DefInfo}} = lists:keysearch(fun_def,1,refac_syntax:get_ann(T)),
			       refac_syntax:add_ann({fun_def, DefInfo}, refac_syntax:module_qualifier_body(T));
			   variable ->
			       refac_syntax:default_literals_vars(T, refac_syntax:variable_name(T));
			   integer ->
			       refac_syntax:default_literals_vars(T, refac_syntax:integer_value(T));
			   float ->
			       refac_syntax:default_literals_vars(T, refac_syntax:float_value(T));
			   char ->
			       refac_syntax:default_literals_vars(T, refac_syntax:char_value(T));
			   string ->
			       refac_syntax:default_literals_vars(T, refac_syntax:string_value(T));
			   atom ->
			       refac_syntax:default_literals_vars(T, refac_syntax:atom_value(T));
			   nil -> refac_syntax:default_literals_vars(T, nil);
			   underscore ->refac_syntax:default_literals_vars(T, '_');
			   _ -> T
		       end,
		  case lists:keysearch(fun_def,1,refac_syntax:get_ann(T1)) of
		      {value, {fun_def, {ModName, FunName, Arity, _, _}}} ->
			  case {ModName, FunName, Arity} of 
			      {M, F, A} -> refac_syntax:set_pos(refac_syntax:remove_comments(refac_syntax:set_ann(T1, [])), {0,0});
			      _ -> refac_syntax:set_pos(
				     refac_syntax:remove_comments(
				       refac_syntax:set_ann(T1,[{fun_def, {ModName, FunName, Arity, {0,0},{0,0}}}])),{0,0})
			  end;	      
		      _ -> refac_syntax:set_pos(refac_syntax:remove_comments(refac_syntax:set_ann(T1, [])), {0,0})
		  end
	  end,
    refac_syntax_lib:map(Fun, Node).




%%================================================================================
%%              Side Condition Checking. 
%%================================================================================

analyze_file(FName, SearchPaths, TabWidth) ->   
    Dir = filename:dirname(FName),
    DefaultIncls = [filename:join(Dir, X) || X <- refac_misc:default_incls()],
    NewSearchPaths = SearchPaths ++ DefaultIncls,
    case refac_epp:parse_file(FName, NewSearchPaths, [], TabWidth,
			      refac_util:file_format(FName)) of
	{ok, TargetAST, {MDefs, _MUses1}} ->
	    MacroDefs= [{Name, {Args, refac_util:concat_toks(Toks)}}
			|| {{_, Name}, {Args, Toks}} <- MDefs],
	    TargetModInfo = get_mod_info_from_parse_tree(TargetAST),
	    RecordDefs=case lists:keysearch(records,1, TargetModInfo) of
			   {value, {records, RecordDefsInTargetFile}} ->
			       [{Name, lists:keysort(1, [{F, prettyprint(FDef)} || {F, FDef} <- Fields])}
				|| {Name, Fields} <- RecordDefsInTargetFile];
			   _ -> []
		       end,
	    {ok, {AnnAST, Info}} = refac_util:parse_annotate_file(FName, true, SearchPaths, TabWidth),
	    Forms = refac_syntax:form_list_elements(AnnAST),
	    Includes=lists:append([lists:flatmap(fun (A) -> 
					    case refac_syntax:type(A) of
						string-> [refac_syntax:string_value(A)];
						_ -> []
					    end
				    end, Args)
		      ||F<-Forms, is_attribute(F, include) orelse is_attribute(F, include_lib),
			Args<-[refac_syntax:attribute_arguments(F)]]),		       
	    InscopeFuns = refac_misc:inscope_funs(TargetModInfo),
	    #module_info{filename=FName,
			 modname=get_module_name(Info),
			 inscope_funs=InscopeFuns, 
			 macro_defs=MacroDefs, 
			 record_defs=RecordDefs,
			 includes=Includes, 
			 ast=AnnAST,
			 info=Info};
	_ ->
	    throw({error, "Refactoring failed because Wrangler could not parse the target module."})
    end.

side_cond_check(FunDefs, CurModInfo, TargetModInfo, NewTargetFile, CheckCond) ->
    try side_cond_check(FunDefs, CurModInfo, TargetModInfo, CheckCond) 
    catch
	throw:E2 -> throw(E2);
	E1:E2 ->
	    case NewTargetFile of 
		true -> 
		    TargetFName=TargetModInfo#module_info.filename,
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
    check_macros_records(FunDefs, CurModInfo, TargetModInfo).

check_macros_records(FunDefs, CurModInfo, TargetModInfo) ->
    CurRecordDefs=CurModInfo#module_info.record_defs,
    TargetRecordDefs=TargetModInfo#module_info.record_defs,
    UnDefinedRecords = check_records(FunDefs, CurRecordDefs, TargetRecordDefs),
    CurMacroDefs=CurModInfo#module_info.macro_defs,
    TargetMacroDefs=TargetModInfo#module_info.macro_defs,
    UnDefinedMacros = check_macros(FunDefs,CurMacroDefs, TargetMacroDefs),
    {UnDefinedMacros, UnDefinedRecords}.


check_fun_name_clash(FunDefs=[{{ModName, _, _},_}|_T],TargetModInfo)->
    TargetInscopeFuns=TargetModInfo#module_info.inscope_funs,
    TargetForms=refac_syntax:form_list_elements(TargetModInfo#module_info.ast),
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

check_records(FunDefs,CurRecordDefs,TargetRecordDefs) -> 
    UsedRecords = lists:usort(lists:append([refac_misc:collect_used_records(FunDef)
					    || {_, FunDef} <- FunDefs])),
    UsedRecordDefs=[{Name, lists:keysort(1, Fields)}
		    || {Name, Fields} <- CurRecordDefs, lists:member(Name, UsedRecords)],
    CurUnDefinedUsedRecords = UsedRecords -- [Name || {Name, _Fields} <- UsedRecordDefs],
    UsedRecordDefsInTargetFile = [{Name, lists:keysort(1, Fields)}
				  || {Name, Fields} <- TargetRecordDefs,
				     lists:member(Name, UsedRecords)],
    RecordsWithDiffDefs=[R||{R, Def}<-UsedRecordDefs,
			   case lists:keysearch(R, 1, UsedRecordDefsInTargetFile) of
			   {value, {R, Def1}} ->
			       Def /= Def1;
			   false ->
			       false 
		       end],
    UnDefinedRecords = UsedRecords-- element(1, lists:unzip(UsedRecordDefsInTargetFile)),
    case RecordsWithDiffDefs of
	[] ->
	    ok;
	[_] ->
	    Msg = format("Record: ~p, used by the function(s) to be moved, "
			 "is defined differently in the target module.",RecordsWithDiffDefs),
	    throw({error, Msg});
	_ ->
	    Msg= "The following records: "++format_names(RecordsWithDiffDefs)++" used by the function(s) to be moved,"
		"are defined differently in the target module.",
	    throw({error, Msg})
    end,
    case CurUnDefinedUsedRecords of 
	[] -> 
	    UnDefinedRecords;
	[_R] ->
	    Msg1=format("Record: ~p, is used by the function(s) to be moved, but not defined in the current module.",
			CurUnDefinedUsedRecords),
	    throw({error, Msg1});
	_->
	    Msg1=format("The following records: ~p are used by the function(s) to be moved, but not defined in the current module",
			[CurUnDefinedUsedRecords]),
	    throw({error, Msg1})
    end.   


check_macros(FunDefs, CurMacroDefs,TargetMacroDefs) ->
    UsedMacros = lists:usort(lists:append([refac_misc:collect_used_macros(FunDef) || {_, FunDef} <- FunDefs])),
    UsedMacroDefs =[{Name, {Args, Def}}
		    || {Name, {Args, Def}} <- CurMacroDefs,
		       lists:member(Name, UsedMacros)],
    UnDefinedUsedMacros =  UsedMacros -- [Name || {Name, _Def} <- UsedMacroDefs],
    UsedMacroDefsInTargetFile =
	[{Name, {Args, Def}}
	 || {Name, {Args, Def}} <- TargetMacroDefs, lists:member(Name, UsedMacros)],
    UnDefinedMacros = UsedMacros-- element(1, lists:unzip(UsedMacroDefsInTargetFile)),
    MsWithDiffDefs=[M||{M, Def}<-UsedMacroDefs,
		       case lists:keysearch(M, 1,UsedMacroDefsInTargetFile) of
			   {value, {M, Def1}} ->
			       Def /= Def1;
			   false ->
			       false
		       end],
    case UnDefinedUsedMacros of
	[] -> ok;
	[_] ->Msg=format("Macro: ~p, is used by the function(s) to be moved, but not defined in the current module",
			 UnDefinedUsedMacros),
	      throw({error, Msg});	       
	_ -> Msg =format("The following macros: ~p are used by the function(s) to be moved, but not defined in the current module",
			 [UnDefinedUsedMacros]),
	     throw({error, Msg})
    end,
    case MsWithDiffDefs of
	[] -> UnDefinedMacros;
	[_] -> Msg1 = format("Macro: ~p, used by the function(s) to be moved "
			     "is defined differently in the target module", MsWithDiffDefs),
	       throw({warning, Msg1 ++ ", still continue?"});
	_ -> Msg1 = "Macros: "++format_names(MsWithDiffDefs)++" used by the function(s) to be moved "
		 "are defined differently in the target module",		      
	     throw({warning, Msg1 ++ ", still continue?"})
    end.


get_mod_info_from_parse_tree(AST) ->
    AST1 = lists:filter(fun (F) ->
				case F of
				    {attribute, _, file, _} -> false;
				    {attribute, _, type, {{record, _}, _, _}} -> false;
				    _ -> true
				end
			end, AST),
    SyntaxTree = refac_recomment:recomment_forms(AST1, []),
    refac_syntax_lib:analyze_forms(SyntaxTree).

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
    case refac_syntax:type(Form) of
	attribute ->
	    Name = refac_syntax:attribute_name(Form),
	    case refac_syntax:type(Name) of
		atom ->
		    AttrName = refac_syntax:atom_value(Name),
		    case AttrName == record orelse AttrName == define of
			true ->
			    Type = hd(refac_syntax:attribute_arguments(Form)),
			    case refac_syntax:type(Type) of
				atom ->
				    TypeName = refac_syntax:atom_value(Type),
				    AttrName == record andalso lists:member(TypeName, Rs) orelse
					AttrName == define andalso lists:member(TypeName, Ms);
				variable ->
				    TypeName= refac_syntax:variable_name(Type),
				    AttrName == define andalso lists:member(TypeName, Ms);
				application ->
				    Op = refac_syntax:application_operator(Type),
				    case refac_syntax:type(Op) of
					atom ->
					    TypeName= refac_syntax:atom_value(Op),
					    AttrName == define andalso lists:member(TypeName, Ms); 
					variable ->
					    TypeName= refac_syntax:variable_name(Op),
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
	Arg<-refac_syntax:attribute_arguments(F),
	case refac_syntax:type(Arg) of
	    string -> not(lists:member(refac_syntax:string_value(Arg), Incs));
	    _ -> false
	end
    ].


type_specifies(Form, MFAs) ->
    FAs =[{F,A}||{_M,F,A}<-MFAs],
    case refac_syntax:type(Form) of
	attribute ->
	    Name = refac_syntax:attribute_name(Form),
	    case refac_syntax:type(Name) of
		atom -> case refac_syntax:atom_value(Name) of
			    'spec' ->
				FA = hd(refac_syntax:attribute_arguments(Form)),
				case refac_syntax:type(FA) of
				    tuple ->
					[A1, F1|_T] = lists:reverse(refac_syntax:tuple_elements(FA)),
					lists:member({refac_syntax:atom_value(F1),
						      refac_syntax:integer_value(A1)},FAs);
				    _ -> false
				end;
			    _ -> false
			end;
		_ -> false
	    end;
	_ -> false
    end.




get_moveable_dependent_funs(MFAs, CurModInfo, TargetModInfo, CG) ->
    Info = CurModInfo#module_info.info,
    DepMFAs=get_dependent_funs(Info, MFAs, CG),
    case DepMFAs of 
	[] -> {[],[],[]};
	_ ->
	    Forms = refac_syntax:form_list_elements(CurModInfo#module_info.ast),
	    DepDefs = [{get_fun_mfa(F), F} || F <- Forms, defines(F, DepMFAs)],
	    check_dependent_funs(DepMFAs, DepDefs, CurModInfo, TargetModInfo,CG)

    end.
	    

  
get_dependent_funs(Info, MFAs = [{ModName, _, _}| _], CG) ->
    Reachable = digraph_utils:reachable(MFAs, CG),
    Reachable1 = [{M, F, A} || {M, F, A} <- Reachable, M == ModName,
			       not refac_misc:is_exported({F, A}, Info)],
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
    TargetInScopeFuns = TargetModInfo#module_info.inscope_funs,
    FAs = [{F, A}||{_M, F, A}<-MFAs],
    Clash = lists:filter(fun ({M, F, A}) ->
				 lists:member({F,A}, FAs) andalso ModName =/= M
			 end, TargetInScopeFuns),
    NewMFAs = MFAs--Clash,
    NewFunDefs=[{F, Def}||{F, Def}<-FunDefs, lists:member(F, NewMFAs)],
    FMRs=lists:append([try check_macros_records([{F,FunDef}], CurModInfo,TargetModInfo) of
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
    case refac_misc:is_fun_name(TargetModName) of
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
    case refac_syntax:type(Form) of
	function ->
	  case lists:keysearch(fun_def, 1, refac_syntax:get_ann(Form)) of
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
    case refac_syntax:type(F)==function orelse
	is_attribute(F, 'spec') of
	true ->
	    {Fs1, Fs2} = lists:splitwith(fun(Form)->
						 refac_syntax:type(Form)==comment
					 end, Fs),
	    group_forms(Fs2, [lists:reverse([F|Fs1])|Acc]);
	_ -> 
	    group_forms(Fs, [[F]|Acc])
    end.

  
get_fun_mfa(Form) ->
    case refac_syntax:type(Form) of
	function ->
	    {value, {fun_def, {M, F, A, _, _}}} =
		lists:keysearch(fun_def,1, refac_syntax:get_ann(Form)),
	    {M,F,A};
	_ -> throw({error, "error in function refac_move_fun:get_fun_mfa/1"})
    end.
	
output_refactor_results(FName, TargetFName, Editor, Cmd, NewTargetFile, AnnAST1, TargetAnnAST1, Results) ->
    case Editor of
	emacs ->
	    refac_util:write_refactored_files_for_preview([{{FName, FName}, AnnAST1},
							   {{TargetFName, TargetFName, NewTargetFile}, TargetAnnAST1}| Results], Cmd),
	    ChangedClientFiles = [F ||{{F, _F}, _AST} <- Results],
	    ChangedFiles = [FName, TargetFName| ChangedClientFiles],
	    ?wrangler_io("The following files are to be changed by this refactoring:\n~p\n", [ChangedFiles]),
	    {ok, ChangedFiles};
      eclipse ->
	    Results1 = [{{FName, FName}, AnnAST1}, {{TargetFName, TargetFName}, TargetAnnAST1}| Results],
	    Res = [{FName1, NewFName1, refac_prettypr:print_ast(refac_util:file_format(FName1), AST)} ||			    
		      {{FName1, NewFName1}, AST}<-Results1],
	    {ok, Res}
    end.

pos_to_export(AnnAST, Pos) ->
    Forms = refac_syntax:form_list_elements(AnnAST),
    Fs=[F||F<-Forms,
	   is_export_attribute(F),
	   {Start, End} <-[refac_misc:get_start_end_loc(F)],
	   Start=<Pos,
	   End>=Pos],
    case Fs of 
	[F] ->
	    {ok,F};
	_ ->{error,"No export attribute has been selected."}
    end.
    
is_export_attribute(Form) ->
    case refac_syntax:type(Form) of
	attribute ->
	    Name = refac_syntax:attribute_name(Form),
	    refac_syntax:type(Name)==atom andalso
		refac_syntax:atom_value(Name)=='export';
	_ -> false
    end.


get_exported_funs(ModName,ExportAttr) ->
    [L]=refac_syntax:attribute_arguments(ExportAttr),
    Es = refac_syntax:list_elements(L),
    [{ModName,F,A}|| E<-Es, 
	       F<-[refac_syntax:atom_value(refac_syntax:arity_qualifier_body(E))],
	       A<-[refac_syntax:integer_value(refac_syntax:arity_qualifier_argument(E))]].


make_export(Names) ->
    Es = [refac_syntax:arity_qualifier(refac_syntax:atom(F),refac_syntax:integer(A))
	  || {F, A} <- Names],
    refac_syntax:attribute(refac_syntax:atom('export'), [refac_syntax:list(Es)]).

copy_pos_attrs(E1, E2) ->
    refac_syntax:copy_pos(E1, refac_syntax:copy_attrs(E1, E2)).    

    
is_attribute(F, Name) ->
    refac_syntax:type(F) == attribute andalso
      refac_syntax:type(refac_syntax:attribute_name(F)) == atom andalso
	refac_syntax:atom_value(refac_syntax:attribute_name(F)) == Name.

prettyprint(none) ->
    none;
prettyprint(Node) -> 
    refac_prettypr:format(Node).


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
