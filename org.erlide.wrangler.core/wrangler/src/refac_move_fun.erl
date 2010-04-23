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
%% Refactoring: Move a function definition from one module to another.
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
%% Known problems: this refactoring does not move the include attributes, type/record/macro definitions
%% on which the function to be moved depends; while it tries to move the type specification associated 
%% with the function, it does not move the type definitions on which the type specification depends, therefore,
%% manual inspection is needed after the refactoring.
%% </p>
%% @end
%% @author Huiqing Li <hl@kent.ac.uk>
%% @version 0.1

-module(refac_move_fun).

-export([move_fun/6, move_fun_1/6, move_fun_eclipse/6, move_fun_1_eclipse/6]).

-import(refac_atom_utils, [output_atom_warning_msg/3, check_atoms/4, 
			   stop_atom_process/1, start_atom_process/0]).

-include("../include/wrangler.hrl").


%%TODO: The current implementation will be simplified when the type info for atoms is avaialble.

%==========================================================================================
-spec(move_fun/6::(filename(),integer(),integer(), string(), [dir()], integer())->
	     {ok, [filename()]} | {question, string()}| {error, string()}).
%%==========================================================================================
move_fun(FName, Line, Col, TargetModorFileName, SearchPaths, TabWidth) ->
    move_fun(FName, Line, Col, TargetModorFileName, SearchPaths, TabWidth, emacs).

-spec(move_fun_1/6::(filename(),integer(),integer(), string(), [dir()], integer())->
	     {ok, [filename()]} | {error, string()}).
move_fun_1(FName, Line, Col, TargetModorFileName, SearchPaths, TabWidth) ->
    move_fun_1(FName, Line, Col, TargetModorFileName, SearchPaths, TabWidth, emacs).


-spec(move_fun_eclipse/6::(filename(),integer(),integer(), string(),[dir()], integer())
        ->  {ok, [{filename(), filename(), string()}]} | {question, string()} |{error, string()}).

move_fun_eclipse(FName, Line, Col, TargetModorFileName, SearchPaths, TabWidth) ->
    move_fun(FName, Line, Col, TargetModorFileName, SearchPaths, TabWidth, eclipse).


-spec(move_fun_1_eclipse/6::(filename(),integer(),integer(), string(),[dir()], integer())
        ->  {ok, [{filename(), filename(), string()}]} | {error, string()}).

move_fun_1_eclipse(FName, Line, Col, TargetModorFileName, SearchPaths, TabWidth) ->
    move_fun_1(FName, Line, Col, TargetModorFileName, SearchPaths, TabWidth, eclipse).


move_fun(FName, Line, Col, TargetModorFileName, SearchPaths, TabWidth, Editor) ->
    ?wrangler_io("\nCMD: ~p:move_fun(~p, ~p, ~p, ~p, ~p, ~p).",
		 [?MODULE, FName, Line, Col, TargetModorFileName, SearchPaths, TabWidth]),
    TargetFName = get_target_file_name(FName, TargetModorFileName),
    case TargetFName of
	FName -> throw({error, "The target module is the same as the current module."});
	_ -> ok
    end,
    case filelib:is_file(TargetFName) of
	true -> move_fun_1(FName, Line, Col, TargetFName,SearchPaths, TabWidth, Editor);
	false -> {question, "Target file "++ TargetFName ++ " does not exist, create it?"}
    end.
  
move_fun_1(FName, Line, Col, TargetModorFileName, SearchPaths, TabWidth, Editor) ->
    Cmd = "CMD: " ++ atom_to_list(?MODULE) ++ ":move_fun_1(" ++ "\"" ++
	    FName ++ "\", " ++ integer_to_list(Line) ++
	      ", " ++ integer_to_list(Col) ++ ", " ++ "\"" ++ TargetModorFileName ++ "\","
		++ "[" ++ refac_misc:format_search_paths(SearchPaths) ++ "]," ++ integer_to_list(TabWidth) ++ ").",
    {ok, {AnnAST, Info}} = refac_util:parse_annotate_file(FName, true, SearchPaths, TabWidth),
    case interface_api:pos_to_fun_def(AnnAST, {Line, Col}) of
      {ok, Def} ->
	  {value, {fun_def, {ModName, FunName, Arity, _Pos1, _Pos2}}} =
	      lists:keysearch(fun_def, 1, refac_syntax:get_ann(Def)),
	  TargetFName = get_target_file_name(FName, TargetModorFileName),
	  TargetModName = list_to_atom(filename:basename(TargetFName, ".erl")),
	  NewTargetFile = case not filelib:is_file(TargetFName) of
			    true -> create_new_file(TargetFName, TargetModName),
				    true;
			    _ -> false
			  end,
	  case side_cond_check({FName, ModName, FunName, Arity}, TargetFName,
			       TargetModName, Def, SearchPaths, TabWidth)
	      of
	    {error, Reason} ->
		case NewTargetFile of
		  true -> file:delete(TargetFName);
		  _ -> ok
		end,
		throw({error, Reason});
	    true -> ok
	  end,
	  Pid = start_atom_process(),
	  {ok, {TargetAnnAST, TargetInfo}} = refac_util:parse_annotate_file(TargetFName, true, SearchPaths, TabWidth),
	  {AnnAST1, TargetAnnAST1} =
	      do_transformation(FName, {AnnAST, Info}, {TargetAnnAST, TargetInfo}, {ModName, FunName, Arity},
				TargetModName, SearchPaths, TabWidth, Pid),
	  check_atoms(FName, AnnAST1, [FunName], Pid),
	  check_atoms(TargetFName, TargetAnnAST1, [FunName], Pid),
	  Results = case refac_misc:is_exported({FunName, Arity}, Info) of
		      true ->
			  ?wrangler_io("\nChecking client modules in the following search paths: \n~p\n", [SearchPaths]),
			  ClientFiles = lists:delete(TargetFName, refac_util:get_client_files(FName, SearchPaths)),
			  refactor_in_client_modules(ClientFiles, {ModName, FunName, Arity}, TargetModName, SearchPaths, TabWidth, Pid);
		      false ->
			  []
		    end,
	  output_atom_warning_msg(Pid, not_renamed_warn_msg(FunName), renamed_warn_msg(ModName)),
	  stop_atom_process(Pid),
	  output_refactor_results(FName, TargetFName, Editor, Cmd, NewTargetFile, AnnAST1, TargetAnnAST1, Results);
      {error, Reason} -> {error, Reason}
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
    file:write_file(TargetFName, list_to_binary(S)).

side_cond_check({FileName, ModName, FunName, Arity}, TargetFileName, TargetModName, FunDef, SearchPaths, TabWidth) ->
    {ok, {AnnAST, Info}} = refac_util:parse_annotate_file(TargetFileName, true, SearchPaths, TabWidth),
    InscopeFuns = refac_misc:inscope_funs(Info),
    check_macros_records(FileName, TargetFileName, FunDef, SearchPaths, TabWidth),
    Clash = lists:any(fun ({M, F, A}) ->
			      {FunName, Arity} == {F, A} andalso ModName =/= M
		      end, InscopeFuns),
    case Clash of
      true ->
	  Forms = refac_syntax:form_list_elements(AnnAST),
	  FunWithSameName = [F || F <- Forms, defines(F, {TargetModName, FunName, Arity})],
	  case FunWithSameName of
	    [] ->
		{error, "Moving this function will cause confliction in the target module."};
	    [F] -> case is_the_same_fun(FunDef, F) of
		     true -> true;
		     _ -> {error, "The same function name, with a possibly different definition, is already defined in the target module."}
		   end;
	    _ -> {error, "The same function name/arity has been defined more than once in the target module."}
	  end;
      false -> true
    end.

defines(Form, {M, F, A}) ->
    case refac_syntax:type(Form) of
	function ->
	  case lists:keysearch(fun_def, 1, refac_syntax:get_ann(Form)) of
	      {value, {fun_def, {M, F, A, _, _}}} ->
		  true;
	      _ -> false
	  end;
	_ -> false
    end.
 

is_the_same_fun(FunDef1, FunDef2) ->
    Ann1 = refac_syntax:get_ann(FunDef1),
    {value, {fun_def, {M1, F1, A1, _, _}}}=lists:keysearch(fun_def,1,Ann1),
    Ann2 = refac_syntax:get_ann(FunDef2),
    {value, {fun_def, {M2, F2, A2, _, _}}} =lists:keysearch(fun_def,1,Ann2),
    FunDef11= reset_attrs(FunDef1, {M1, F1, A1}),
    FunDef21=reset_attrs(FunDef2, {M2, F2, A2}),
    FunDef11 == FunDef21.


check_macros_records(FileName, TargetFileName, FunDef, SearchPaths, TabWidth) ->
    UsedMacros = refac_misc:collect_used_macros(FunDef),
    UsedRecords = refac_misc:collect_used_records(FunDef),
    case UsedMacros == [] andalso UsedRecords == [] of
      true -> true;
      _ ->
	  Dir = filename:dirname(FileName),
	  DefaultIncls = [filename:join(Dir, X) || X <- refac_misc:default_incls()],
	  NewSearchPaths = SearchPaths ++ DefaultIncls,
	  case refac_epp:parse_file(FileName, NewSearchPaths, [], TabWidth, refac_util:file_format(FileName)) of
	    {ok, AST, {MDefs, _MUses}} ->
		  {UsedMacroDefs, UsedRecordDefs} = check_macros_records_in_current_file(UsedMacros, UsedRecords, AST, MDefs),
		  check_marcos_records_in_target_file(TargetFileName, UsedMacros, UsedRecords, 
						      UsedMacroDefs, UsedRecordDefs,NewSearchPaths,TabWidth);
	      _ -> throw({error, "Refactoring failed because Wrangler could not parse the current module."})
	  end
    end.

check_macros_records_in_current_file(UsedMacros, UsedRecords, AST, MDefs) ->
    UsedMacroDefs =
	[{Name, {Args, refac_util:concat_toks(Toks)}}
	 || {{_, Name}, {Args, Toks}} <- MDefs,
	    lists:member(Name, UsedMacros)],
    UsedRecordDefs =
	case UsedRecords of
	  [] -> [];
	  _ ->
	      Info = get_mod_info_from_parse_tree(AST),
	      case lists:keysearch(records, 1, Info) of
		{value, {records, RecordDefs}} ->
		    [{Name, lists:keysort(1, [{F, prettyprint(FDef)} || {F, FDef} <- Fields])}
		     || {Name, Fields} <- RecordDefs, lists:member(Name, UsedRecords)];
		_ -> []
	      end
	end,
    check_macros_in_current_file(UsedMacros, UsedMacroDefs),
    check_records_in_current_file(UsedRecords, UsedRecordDefs),
    {UsedMacroDefs, UsedRecordDefs}.


check_macros_in_current_file(UsedMacros, UsedMacroDefs) ->
    case length(UsedMacros) > length(UsedMacroDefs) of
      true ->
	  UnDefinedUsedMacros = UsedMacros -- [Name || {Name, _Def} <- UsedMacroDefs],
	  Msg = format("Macro(s) used by the function selected, but not defined: ~p.",
		       [UnDefinedUsedMacros]),
	  ?wrangler_io("\n"++Msg, []),
	  throw({error, Msg});
      false -> ok
    end.

check_records_in_current_file(UsedRecords, UsedRecordDefs) ->
    case length(UsedRecords) > length(UsedRecordDefs) of
      true ->
	  UnDefinedUsedRecords = UsedRecords -- [Name || {Name, _Fields} <- UsedRecordDefs],
	  Msg1 = format("Record(s) used by the function selected, but not defined: ~p.",
			       [UnDefinedUsedRecords]),
	  ?wrangler_io("\n"++Msg1, []),
	  throw({error, Msg1});
      false -> ok
    end.


check_marcos_records_in_target_file(TargetFileName, UsedMacros, UsedRecords,
				    UsedMacroDefs, UsedRecordDefs, NewSearchPaths, TabWidth) ->
    case refac_epp:parse_file(TargetFileName, NewSearchPaths, [], TabWidth,
			      refac_util:file_format(TargetFileName))
	of
      {ok, TargetAST, {MDefs1, _MUses1}} ->
	  UsedMacroDefsInTargetFile =
	      [{Name, {Args, refac_util:concat_toks(Toks)}}
	       || {{_, Name}, {Args, Toks}} <- MDefs1, lists:member(Name, UsedMacros)],
	  check_macros_in_target_file(UsedMacros, UsedMacroDefs, UsedMacroDefsInTargetFile),
	  TargetModInfo = get_mod_info_from_parse_tree(TargetAST),
	  UsedRecordDefsInTargetFile = case lists:keysearch(records, 1, TargetModInfo) of
					 {value, {records, RecordDefsInTargetFile}} ->
					     [{Name, lists:keysort(1, [{F, prettyprint(FDef)} || {F, FDef} <- Fields])}
					      || {Name, Fields} <- RecordDefsInTargetFile,
						 lists:member(Name, UsedRecords)];
					 _ -> []
				       end,
	  check_records_in_target_file(UsedRecords, UsedRecordDefs, UsedRecordDefsInTargetFile);
      _ -> throw({error, "Refactoring failed because Wrangler could not parse the target module."})
    end.


check_macros_in_target_file(UsedMacros, UsedMacroDefs, UsedMacroDefsInTargetFile) ->
    case length(UsedMacros) > length(UsedMacroDefsInTargetFile) of
	true ->
	    Ms =UsedMacros -- element(1, lists:unzip(UsedMacroDefsInTargetFile)),
	    Msg = format("Macros used by the function selected "
			 "are not defined in the target module: ~p.",[Ms]),
	    ?wrangler_io("\n"++Msg,[]),
	    throw({error, Msg});
	false ->
	    case length(UsedMacros) == length(UsedMacroDefsInTargetFile) of
		true ->
		    case lists:keysort(1, UsedMacroDefs) == lists:keysort(1, UsedMacroDefsInTargetFile) of
			true -> ok;
			_ ->
			    Ms=element(1, lists:unzip(lists:keysort(1, UsedMacroDefs)
						       -- lists:keysort(1, UsedMacroDefsInTargetFile))),
			    Msg1 = format("Macro(s) used by the function selected "
					  "are defined differently in the target module: ~p", [Ms]),
			    ?wrangler_io("\n"++Msg1 ++ ".", []),
			    throw({warning, Msg1 ++ ", still continue?"})
		    end;
		_ -> ok
	    end
    end.

check_records_in_target_file(UsedRecords, UsedRecordDefs, UsedRecordDefsInTargetFile) ->
    case length(UsedRecords) > length(UsedRecordDefsInTargetFile) of
      true ->
	    Ms = UsedRecords -- element(1, lists:unzip(UsedRecordDefsInTargetFile)),
	    Msg = format("Records used by the function selected "
			 "are not defined in the target module: ~p.", [Ms]),
	    ?wrangler_io("\n"++Msg, []),
	    throw({error, Msg});
      _ ->
	  case lists:keysort(1, UsedRecordDefs) == lists:keysort(1, UsedRecordDefsInTargetFile) of
	    true -> true;
	      _ ->
		  Ms1= element(1, lists:unzip(lists:keysort(1, UsedRecordDefs)
					     -- lists:keysort(1, UsedRecordDefsInTargetFile))),
		  Msg1 = format("Records used by the function selected "
				"are defined differently in the target module: ~p.",[Ms1]),
		  ?wrangler_io("\n"++Msg1, []),
		  throw({error, Msg1})
	  end
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
    


do_transformation(FileName, {AnnAST, Info}, {TargetAnnAST, Info1}, {ModName, FunName, Arity}, TargetModName, SearchPaths, TabWidth, Pid) ->
    Forms = refac_syntax:form_list_elements(AnnAST),
    FormsToBeMoved = [F || F <- Forms, defines(F, {ModName, FunName, Arity}) orelse type_specifies(F, {FunName, Arity})],
    FunToBeMoved = hd([F || F <- FormsToBeMoved, refac_syntax:type(F) == function]),
    SpecToBeMove = [F || F <- FormsToBeMoved, refac_syntax:type(F) == attribute],
    FunsToBeExported = local_funs_to_be_exported(FunToBeMoved, {ModName, FunName, Arity}, Info),
    InScopeFunsInTargetMod = refac_misc:inscope_funs(Info1),
    FunToBeMoved1 = transform_fun(FileName, FunToBeMoved, {ModName, FunName, Arity}, TargetModName, InScopeFunsInTargetMod, SearchPaths, TabWidth, Pid),
    {AnnAST1, FunIsUsed} = do_remove_fun(FileName, AnnAST, {ModName, FunName, Arity}, FunsToBeExported, TargetModName, SearchPaths, TabWidth, Pid),
    IsExported = refac_misc:is_exported({FunName, Arity}, Info),
    Export = FunIsUsed or IsExported,
    TargetAnnAST1 = do_add_fun(FileName, {TargetAnnAST, Info1}, SpecToBeMove ++ [FunToBeMoved1],
			       {ModName, FunName, Arity}, TargetModName, Export, SearchPaths, TabWidth, Pid),
    {AnnAST1, TargetAnnAST1}.

type_specifies(F, {FunName, Arity}) ->
    case refac_syntax:type(F) of
	attribute ->
	    Name = refac_syntax:attribute_name(F),
	    case refac_syntax:type(Name) of
		atom -> case refac_syntax:atom_value(Name) of
			    'spec' ->
				FA = hd(refac_syntax:attribute_arguments(F)),
				case refac_syntax:type(FA) of
				    tuple ->
					case refac_syntax:tuple_elements(FA) of
					    [_M1, F1, A1] -> 
						{FunName, Arity}=={refac_syntax:atom_value(F1),
								   refac_syntax:integer_value(A1)};						
					    [F1, A1] -> 
						{FunName, Arity}=={refac_syntax:atom_value(F1),
								   refac_syntax:integer_value(A1)};
					    _ -> false
					end;
				    _ -> false
				end;
			    _ -> false
			end;
		_ -> false
	    end;
	_ -> false
    end.

%%==============================================================================
%% Functions defined in the current module, but used by the function to be moved.
%%===============================================================================
local_funs_to_be_exported(Node, {ModName, FunName, Arity}, ModInfo) ->
    Exports = case lists:keysearch(exports, 1, ModInfo) of
		{value, {exports, ExportList}} ->
		    ExportList;
		_ -> []
	      end,
    CalledFuns = wrangler_callgraph_server:called_funs(Node),
    CalledLocalFuns = lists:flatmap(fun ({M, F, A}) ->
					    case M == ModName andalso {F, A} =/= {FunName, Arity}
						of
					      true -> [{F, A}];
					      _ -> []
					    end
				    end, CalledFuns),
    lists:usort(CalledLocalFuns) -- Exports.

%%======================================================================================
%% Transform the function to be moved.
%%======================================================================================
transform_fun(FileName, Form, {ModName, FunName, Arity}, TargetModName, InScopeFunsInTargetMod, SearchPaths, TabWidth, Pid) ->
    element(1, ast_traverse_api:full_tdTP(fun do_transform_fun/2, Form,
					  {FileName, {ModName, FunName, Arity},
					   TargetModName, InScopeFunsInTargetMod,
					   SearchPaths, TabWidth, Pid})).
  
do_transform_fun(Node, {FileName, {ModName, FunName, Arity}, TargetModName, InScopeFunsInTargetMod, SearchPaths, TabWidth, Pid}) ->
    case refac_syntax:type(Node) of
      application ->
	  Op = refac_syntax:application_operator(Node),
	  Args = refac_syntax:application_arguments(Node),
	  case lists:keysearch(fun_def, 1, refac_syntax:get_ann(Op)) of
	    {value, {fun_def, {M, F, A, _, _}}} ->
		case refac_syntax:type(Op) of
		  atom ->
			case lists:member({M, F, A}, InScopeFunsInTargetMod) orelse
			    erl_internal:bif(M, F, A) orelse {M, F, A} == {ModName, FunName, Arity}
			of
			    true ->
				{Node, false};
			    _ when M =/= '_' ->
				{copy_pos_attrs(Node, refac_syntax:application(
							copy_pos_attrs(Op, refac_syntax:module_qualifier(refac_syntax:atom(M), Op)), Args)),
			     true};
			    _ ->
				{Line, Col} = refac_syntax:get_pos(Op),
				Str = "Wrangler could not infer where the function " ++ atom_to_list(F) ++ "/" ++ integer_to_list(A) ++
				    ", used at location {" ++ integer_to_list(Line) ++ "," ++ integer_to_list(Col) ++ "} in the current module, is defined.",
				throw({error, Str})
			end;
		  _ ->
		      case M == TargetModName of
			true ->
			    {copy_pos_attrs(Node, refac_syntax:application(
						    copy_pos_attrs(Op, refac_syntax:atom(F)), Args)),
			     true};
			_ ->
			    {Node, false}
		      end
		end;
	    _ -> {Node, false}
	  end;
      implicit_fun ->
	  Name = refac_syntax:implicit_fun_name(Node),
	  case refac_syntax:type(Name) of
	    arity_qualifier ->
		Body = refac_syntax:arity_qualifier_body(Name),
		A = refac_syntax:arity_qualifier_argument(Name),
		case {refac_syntax:type(Body), refac_syntax:type(A)} of
		  {atom, integer} ->
		      case {refac_syntax:atom_value(Body), refac_syntax:integer_value(A)} of
			{FunName, Arity} -> {Node, false};
			_ -> {copy_pos_attrs(Node, refac_syntax:implicit_fun(
						     copy_pos_attrs(Name,refac_syntax:module_qualifier(refac_syntax:atom(ModName), Name)))), true}
		      end;
		  _ -> {Node, false}
		end;
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
				  case {refac_syntax:atom_value(B), refac_syntax:integer_value(A)} of
				    {FunName, Arity} ->
					{copy_pos_attrs(Node,
							refac_syntax:implicit_fun(copy_pos_attrs(Name,
												 refac_syntax:module_qualifier(copy_pos_attrs(Mod,
																	      refac_syntax:atom(TargetModName)), Body)))), true};
				    _ -> {Node, false}
				  end;
			      _ -> {Node, false}
			    end;
			_ -> {Node, false}
		      end;
		  _ -> {Node, false}
		end;
	    _ -> {Node, false}
	  end;
      tuple -> do_rename_fun_in_tuples(Node, {FileName, SearchPaths, ModName, FunName, TargetModName, Arity, Pid, TabWidth});
      _ -> {Node, false}
    end.
				

%%=========================================================================
%% Remove the function from the current module. 
%%=========================================================================
do_remove_fun(FileName, AnnAST, {ModName, FunName, Arity}, FunsToBeExported, TargetModName, SearchPaths, TabWidth, Pid) ->
    Forms  = refac_syntax:form_list_elements(AnnAST),
    {Forms1, C}= lists:unzip([process_a_form_in_original_mod(FileName, Form, {ModName, FunName, Arity}, 
								      TargetModName, SearchPaths, TabWidth, Pid)
			      || Form<-Forms]),
    Forms2 = lists:append(Forms1),
    Fun_is_Used = lists:member(true, C),
    NewForms = case FunsToBeExported of 
		   [] -> Forms2;
		   [_H|_T] ->  Export = make_export(FunsToBeExported),
			       insert_export_form(Export, Forms2)
	       end,
    {copy_pos_attrs(AnnAST, refac_syntax:form_list(NewForms)), Fun_is_Used}.

%%=======================================================================
%% Add the function to the target module.
%%=======================================================================
do_add_fun(FileName, {TargetAnnAST, Info}, FunToBeMoved, {ModName, FunName, Arity}, TargetModName, ToBeExported, SearchPaths, TabWidth, Pid) ->
    Forms = refac_syntax:form_list_elements(TargetAnnAST),
    FunWithSameName = [F || F <- Forms, defines(F, {TargetModName, FunName, Arity})],
    NewFun = case FunWithSameName of
	       [] -> FunToBeMoved;
	       _ -> []
	     end,
    Forms1 = lists:append([process_a_form_in_target_module(FileName, Form, {ModName, FunName, Arity}, TargetModName, SearchPaths, TabWidth, Pid)
			   || Form <- Forms]),
    IsExported = refac_misc:is_exported({FunName, Arity}, Info),
    NewForms = case ToBeExported andalso not IsExported of
		 false -> Forms1 ++ NewFun;
		 true ->
		     Export = make_export([{FunName, Arity}]),
		     insert_export_form(Export, Forms1) ++ NewFun
	       end,
    copy_pos_attrs(TargetAnnAST, refac_syntax:form_list(NewForms)).


insert_export_form(Export, Forms) ->
    {Forms11, Forms12} = lists:splitwith(fun (F) -> 
						 is_attribute(F, module) orelse
					         is_attribute(F, export) orelse
					         is_attribute(F, import) orelse 
					         refac_syntax:type(F) == comment
					 end, Forms),
    {Forms111, Forms112}= lists:splitwith(fun(F) -> 
						  refac_syntax:type(F)==comment 
					  end, lists:reverse(Forms11)),
    lists:reverse(Forms112)++Export ++ lists:reverse(Forms111) ++ Forms12.
    
   
%%=========================================================================
%% Refacotoring in the current module. 
%%========================================================================
process_a_form_in_original_mod(FileName,Form, {ModName, FunName, Arity}, TargetModName, SearchPaths, TabWidth, Pid) ->
    case refac_syntax:type(Form) of 
	function -> 
	    As = refac_syntax:get_ann(Form),
	    case lists:keysearch(fun_def,1, As) of 
		{value, {fun_def, {ModName, FunName, Arity, _, _}}} ->
		    {[], false};
		_ ->
		    {Form1, C}=add_change_module_qualifier(FileName, Form, {ModName, FunName, Arity}, 
							   TargetModName, SearchPaths, TabWidth, Pid),
		    {[Form1],C}
	    end;		    
	attribute -> Name = refac_syntax:attribute_name(Form),
		     case refac_syntax:type(Name) of 
			 atom ->case refac_syntax:atom_value(Name) of
				    export -> [L] = refac_syntax:attribute_arguments(Form),
					      Es = refac_syntax:list_elements(L),
					      Es1 =[E  || E<-Es, 
							  {refac_syntax:atom_value(refac_syntax:arity_qualifier_body(E)),
							   refac_syntax:integer_value(refac_syntax:arity_qualifier_argument(E))}
							      =/={FunName, Arity}],
					      case length(Es1) == 0 of
						  true -> 
						      {[],false} ;
						  false -> 
						      case length(Es1) == length(Es) of 
							  true -> {[Form], false};
							  _ -> {[copy_pos_attrs(Form,refac_syntax:attribute(Name,[refac_syntax:list(Es1)]))], true}
						      end
					      end;
				    spec -> FA = hd(refac_syntax:attribute_arguments(Form)),
					    case refac_syntax:type(FA) of
						tuple ->
						    case refac_syntax:tuple_elements(FA) of 
							[_M1, F1, A1] ->case {refac_syntax:atom_value(F1), refac_syntax:integer_value(A1)} of
									    {FunName, Arity} -> {[],true};
									    _ -> {[Form], false}
									    end;
							[F1, A1] ->case {refac_syntax:atom_value(F1), refac_syntax:integer_value(A1)} of
								       {FunName, Arity} -> {[],true};
								       _ -> {[Form],false}
								   end;
							_ -> {[Form],false}
						    end;
						_ ->{[Form], false}
					    end;
				    _  -> {[Form], false}
				end;
			 _  -> {[Form], false}
		     end;
	_-> {[Form], false}
    end.

%%============================================================================
%% Refactoring in the target module.
%%============================================================================
process_a_form_in_target_module(FileName, Form, {ModName, FunName, Arity}, TargetModName, SearchPaths, TabWidth, Pid) ->
    case refac_syntax:type(Form) of
	function -> [remove_module_qualifier(FileName, Form, {ModName, FunName, Arity}, TargetModName, SearchPaths, TabWidth, Pid)];
	attribute -> Name = refac_syntax:attribute_name(Form),
		     case refac_syntax:type(Name) of
			 atom -> case refac_syntax:atom_value(Name) of
				     import ->
					 [H, L] = refac_syntax:attribute_arguments(Form),
					 case refac_syntax:atom_value(H) of 
					     ModName ->
						 Es = refac_syntax:list_elements(L),
						 Es1 = [E || E <- Es,
							     {refac_syntax:atom_value(refac_syntax:arity_qualifier_body(E)),
							      refac_syntax:integer_value(refac_syntax:arity_qualifier_argument(E))}
								 =/= {FunName, Arity}],
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
%% Add/remove module qualifier.
%%============================================================================
remove_module_qualifier(FileName, Form, {ModName, FunName, Arity}, TargetModName, SearchPaths, TabWidth, Pid) ->
    element(1, ast_traverse_api:full_tdTP(fun do_remove_module_qualifier/2, Form,
					  {FileName, {ModName, FunName, Arity},
					   TargetModName, SearchPaths, TabWidth, Pid})).
  

do_remove_module_qualifier(Node, {FileName, {ModName, FunName, Arity}, TargetModName, SearchPaths, TabWidth, Pid}) ->
    case refac_syntax:type(Node) of
      application ->
	  Op = refac_syntax:application_operator(Node),
	  Args = refac_syntax:application_arguments(Node),
	  case application_info(Node) of
	    {{none, FunName}, Arity} -> {Node, true};
	    {{ModName, FunName}, Arity} ->
		case refac_syntax:type(Op) of
		  tuple -> {rename_fun_in_tuple_op(Node, Op, Args, TargetModName), true};
		  _ ->
		      Op1 = copy_pos_attrs(Op, refac_syntax:atom(FunName)),
		      Node1 = copy_pos_attrs(Node, refac_syntax:application(Op1, Args)),
		      {Node1, true}
		end;
	    {{Mod1, Fun1}, Ari1} ->
		case lists:keysearch({Mod1, Fun1, Ari1}, 1, refac_misc:apply_style_funs()) of
		  {value, _} ->
		      transform_apply_style_calls(FileName, Node, {ModName, FunName, Arity}, TargetModName, SearchPaths, TabWidth);
		  false -> {Node, false}
		end
	  end;
      implicit_fun -> process_implicit_fun(Node, ModName, FunName, Arity, TargetModName);
      tuple -> do_rename_fun_in_tuples(Node, {FileName, SearchPaths, ModName, FunName, TargetModName, Arity, Pid, TabWidth});
      _ -> {Node, false}
    end.

process_implicit_fun(Node, ModName, FunName, Arity, TargetModName) ->
    Name = refac_syntax:implicit_fun_name(Node),
    case refac_syntax:type(Name) of
      arity_qualifier ->
	  {Node, false};
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
			    case
			      {refac_syntax:atom_value(B), refac_syntax:integer_value(A)}
				of
			      {FunName, Arity} ->
				  {copy_pos_attrs(Node,
					 refac_syntax:implicit_fun(copy_pos_attrs(Name,
					  refac_syntax:module_qualifier(copy_pos_attrs(Mod,
					  refac_syntax:atom(TargetModName)), Body)))), true};
			      _ -> {Node, false}
			    end;
			_ -> {Node, false}
		      end;
		  _ -> {Node, false}
		end;
	      _ ->{Node, false}
	  end
    end.
add_change_module_qualifier(FileName, Form, {ModName, FunName, Arity}, TargetModName, SearchPaths, TabWidth, Pid) ->
    ast_traverse_api:full_tdTP(fun do_add_change_module_qualifier/2,
			       Form, {FileName, {ModName, FunName, Arity}, TargetModName, SearchPaths, TabWidth, Pid}).

do_add_change_module_qualifier(Node, {FileName, {ModName, FunName, Arity}, TargetModName, SearchPaths, TabWidth, Pid}) ->
    MakeApp = fun (Node1, Operator1, Arguments1, TargetModName1, FunName1) ->
		      Operator2 = copy_pos_attrs(Operator1, refac_syntax:module_qualifier(refac_syntax:atom(TargetModName1),
											  refac_syntax:atom(FunName1))),
		      Node2 = copy_pos_attrs(Node1, refac_syntax:application(Operator2, Arguments1)),
		      {Node2, true}
	      end,
    case refac_syntax:type(Node) of
      application ->
	  Operator = refac_syntax:application_operator(Node),
	  Args = refac_syntax:application_arguments(Node),
	  case lists:keysearch(fun_def, 1, refac_syntax:get_ann(Operator)) of
	    {value, {fun_def, {Mod1, Fun1, Ari1, _, _}}} ->
		case lists:keysearch({Mod1, Fun1, Ari1}, 1, refac_misc:apply_style_funs()) of
		  {value, _} ->
		      transform_apply_style_calls(FileName, Node, {ModName, FunName, Arity}, TargetModName, SearchPaths, 8);  %% To Change
		  false ->
		      case {Mod1, Fun1, Ari1} of
			{ModName, FunName, Arity} ->
			    case refac_syntax:type(Operator) of
			      tuple -> {rename_fun_in_tuple_op(Node, Operator, Args, TargetModName), true};
			      _ ->
				  MakeApp(Node, Operator, Args, TargetModName, FunName)
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
			  case {B, A} of
			    {FunName, Arity} ->
				FunName1 = copy_pos_attrs(Name, refac_syntax:module_qualifier(copy_pos_attrs(Name,
													     refac_syntax:atom(TargetModName)), Name)),
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
				  case {refac_syntax:atom_value(B), refac_syntax:integer_value(A)} of
				    {FunName, Arity} ->
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
		      end
		end
	  end;
      tuple -> do_rename_fun_in_tuples(Node, {FileName, SearchPaths, ModName, FunName, TargetModName, Arity, Pid, TabWidth});
      _ -> {Node, false}
    end.
  

%% ====================================================================================
%%  Processing Client modules.
%%====================================================================================

refactor_in_client_modules(ClientFiles,{ModName, FunName, Arity}, TargetModName, SearchPaths, TabWidth, Pid) ->
    case ClientFiles of 
	[] -> [];
	[F | Fs] ->
	    ?wrangler_io("The current file under refactoring is:\n~p\n", [F]),
	    {ok, {AnnAST, Info}} =refac_util:parse_annotate_file(F,true, SearchPaths, TabWidth),     
	    {AnnAST1, Changed} = refactor_in_client_module_1(F, {AnnAST, Info}, {ModName, FunName, Arity}, TargetModName, SearchPaths, TabWidth, Pid),
	    check_atoms(F, AnnAST1, [FunName], Pid),
	    if Changed ->
		    [{{F,F}, AnnAST1} | refactor_in_client_modules(Fs, {ModName, FunName, Arity}, TargetModName, SearchPaths, TabWidth, Pid)];
	       true -> refactor_in_client_modules(Fs, {ModName, FunName, Arity}, TargetModName, SearchPaths, TabWidth, Pid)
	    end
    end.
refactor_in_client_module_1(FileName,{AnnAST, _Info}, {ModName, FunName, Arity}, TargetModName, SearchPaths, TabWidth,Pid) ->
    Forms  = refac_syntax:form_list_elements(AnnAST),
    {Forms1, C} = lists:unzip([process_in_client_module(FileName, Form, {ModName, FunName, Arity}, TargetModName, SearchPaths, TabWidth, Pid)||Form <-Forms]),
    Forms2 = lists:append(Forms1),
    {copy_pos_attrs(AnnAST, refac_syntax:form_list(Forms2)), lists:member(true, C)}.

process_in_client_module(FileName, Form, {ModName, FunName, Arity}, TargetModName,SearchPaths, TabWidth, Pid) ->
     NewImport = refac_syntax:attribute(refac_syntax:atom('import'),
					[refac_syntax:atom(TargetModName),
					 refac_syntax:list([refac_syntax:arity_qualifier(refac_syntax:atom(FunName),
								       refac_syntax:integer(Arity))])]),
    case refac_syntax:type(Form) of
	function -> {Form1, C} =change_module_qualifier(FileName, Form, {ModName, FunName, Arity}, TargetModName, SearchPaths, TabWidth, Pid),
		    {[Form1], C};
	attribute -> Name = refac_syntax:attribute_name(Form),
		     case refac_syntax:type(Name) of
			 atom -> case refac_syntax:atom_value(Name) of
				     import ->
					 case refac_syntax:attribute_arguments(Form) of 
					     [H, L] ->
						 case refac_syntax:atom_value(H) of 
						     ModName ->
							 Es = refac_syntax:list_elements(L),
							 Es1 = [E || E <- Es,
								     {refac_syntax:atom_value(refac_syntax:arity_qualifier_body(E)),
								      refac_syntax:integer_value(refac_syntax:arity_qualifier_argument(E))}
									 =/= {FunName, Arity}],
							 case length(Es1) of 
							     0 ->
								 {[NewImport], true};
							     _ -> case length(Es) == length(Es1) of 
								      true -> {[Form], false};
								      _ ->
									  {[copy_pos_attrs(Form, refac_syntax:attribute(Name, [H, refac_syntax:list(Es1)])),
									    NewImport],true}
								  end
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


change_module_qualifier(FileName, Form, {ModName, FunName, Arity}, TargetModName, SearchPaths, TabWidth, Pid) ->
    ast_traverse_api:full_tdTP(fun do_change_module_qualifier/2,
			       Form, {FileName, {ModName, FunName, Arity}, TargetModName, SearchPaths, TabWidth, Pid}).

do_change_module_qualifier(Node, {FileName, {ModName, FunName, Arity}, TargetModName, SearchPaths, TabWidth, Pid}) ->
    MakeApp = fun (Node1, Operator1, Arguments1, TargetModName1, FunName1) ->
		      Operator2 = copy_pos_attrs(Operator1, refac_syntax:module_qualifier(refac_syntax:atom(TargetModName1),
											  refac_syntax:atom(FunName1))),
		      Node2 = copy_pos_attrs(Node1, refac_syntax:application(Operator2, Arguments1)),
		      {Node2, true}
	      end,
    case refac_syntax:type(Node) of
      application ->
	  Op = refac_syntax:application_operator(Node),
	  Args = refac_syntax:application_arguments(Node),
	  case lists:keysearch(fun_def, 1, refac_syntax:get_ann(Op)) of
	    {value, {fun_def, {Mod1, Fun1, Ari1, _, _}}} ->
		case lists:keysearch({Mod1, Fun1, Ari1}, 1, refac_misc:apply_style_funs()) of
		  {value, _} ->
		      transform_apply_style_calls(FileName, Node, {ModName, FunName, Arity}, TargetModName, SearchPaths, TabWidth);
		  false ->
		      case {Mod1, Fun1, Ari1} of
			{ModName, FunName, Arity} ->
			    case refac_syntax:type(Op) of
			      module_qualifier ->
				  MakeApp(Node, Op, Args, TargetModName, FunName);
			      tuple -> {rename_fun_in_tuple_op(Node, Op, Args, TargetModName), true};
			      _ -> {Node, false}
			    end;
			_ -> {Node, false}
		      end
		end;
	    _ -> {Node, false}
	  end;
      implicit_fun -> process_implicit_fun(Node, ModName, FunName, Arity, TargetModName);
      tuple -> do_rename_fun_in_tuples(Node, {FileName, SearchPaths, ModName,
					      FunName, TargetModName, Arity, Pid, TabWidth});
      _ -> {Node, false}
    end.

rename_fun_in_tuple_op(App,Op, Args, TargetModName) ->
    [M, F] = refac_syntax:tuple_elements(Op),
    Op1 = copy_pos_attrs(Op, refac_syntax:tuple([copy_pos_attrs(M, refac_syntax:atom(TargetModName)), F])),
    copy_pos_attrs(App, refac_syntax:application(Op1, Args)).
    
  
%%================================================================================
%%              Some Utility Functions 
%%================================================================================

make_export(Names) ->
    Es = [refac_syntax:arity_qualifier(refac_syntax:atom(F),refac_syntax:integer(A))
	  || {F, A} <- Names],
    [refac_syntax:attribute(refac_syntax:atom('export'), [refac_syntax:list(Es)])].

copy_pos_attrs(E1, E2) ->
    refac_syntax:copy_pos(E1, refac_syntax:copy_attrs(E1, E2)).    

    
is_attribute(F, Name) ->
    refac_syntax:type(F) == attribute andalso
      refac_syntax:type(refac_syntax:attribute_name(F)) == atom andalso
	refac_syntax:atom_value(refac_syntax:attribute_name(F)) == Name.
       
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
	_ -> erlang:error(badarg)
    end.

transform_apply_style_calls(FileName, Node, {ModName, FunName, Arity}, NewModName, SearchPaths, TabWidth) ->
    Operator = refac_syntax:application_operator(Node),
    Arguments = refac_syntax:application_arguments(Node),
    [N1, N2, Mod, Fun, Args] = case length(Arguments) of
				 5 -> Arguments;
				 4 -> [none| Arguments];
				 3 -> [none, none| Arguments]
			       end,
    Mod1 = refac_misc:try_eval(FileName, Mod, SearchPaths, TabWidth),
    Fun1 = refac_misc:try_eval(FileName, Fun, SearchPaths, TabWidth),
    case Fun1 of
      {value, FunName} ->
	  case Mod1 of
	    {value, ModName} ->
		case refac_syntax:type(Args) == list andalso refac_syntax:list_length(Args) == Arity orelse
		       refac_syntax:type(Args) == nil andalso Arity == 0
		    of
		  true ->
		      Mod2 = copy_pos_attrs(Mod, refac_syntax:atom(NewModName)),
		      App = case length(Arguments) of
			      5 -> refac_syntax:application(Operator, [N1, N2, Mod2, Fun, Args]);
			      4 -> refac_syntax:application(Operator, [N2, Mod2, Fun, Args]);
			      3 -> refac_syntax:application(Operator, [Mod2, Fun, Args])
			    end,
		      {copy_pos_attrs(Node, App), true};
		  false -> {Node, false}
		end;
	    _ ->
		{Node, false}
	  end;
      _ -> {Node, false}
    end.


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


do_rename_fun_in_tuples(Node, {FileName, SearchPaths, ModName, FunName, TargetModName, Arity, Pid, TabWidth}) ->
    case refac_syntax:tuple_elements(Node) of
	[E1, E2, E3] ->
	  case refac_misc:try_eval(FileName, E1, SearchPaths, TabWidth) of
	    {value, ModName} ->
		case refac_syntax:type(E2) == atom andalso refac_syntax:atom_value(E2) == FunName of
		  true -> case refac_syntax:type(E3) == list andalso refac_syntax:list_length(E3) == Arity of
			    true ->
				Pid ! {add_renamed, {FileName, Node}},
				{refac_syntax:tuple([copy_pos_attrs(E1, refac_syntax:atom(TargetModName)), E2, E3]), true};
			    false ->
				case refac_syntax:type(E3) == nil andalso Arity == 0 of
				  true ->
				      Pid ! {add_renamed, {FileName, Node}},
				      {refac_syntax:tuple([copy_pos_attrs(E1, refac_syntax:atom(TargetModName)), E2, E3]), true};
				  false ->
				      case refac_syntax:type(E3) == integer andalso refac_syntax:integer_value(E3) == Arity of
					true ->
					    Pid ! {add_renamed, {FileName, Node}},
					    {refac_syntax:tuple([copy_pos_attrs(E1, refac_syntax:atom(TargetModName)), E2, E3]), true};
					false ->
					    {Node, false}
				      end
				end
			  end;
		  false -> {Node, false}
		end;
	    _ ->
		{Node, false}
	  end;
      [E0, E1, E2, E3] ->
	  case refac_misc:try_eval(FileName, E1, SearchPaths, TabWidth) of
	    {value, ModName} ->
		case refac_syntax:type(E2) == atom andalso refac_syntax:atom_value(E2) == FunName of
		  true -> case refac_syntax:type(E3) == list andalso refac_syntax:list_length(E3) == Arity of
			    true ->
				Pid ! {add_renamed, {FileName, Node}},
				{refac_syntax:tuple([E0, copy_pos_attrs(E1, refac_syntax:atom(TargetModName)), E2, E3]), true};
			    false ->
				case refac_syntax:type(E3) == nil andalso Arity == 0 of
				  true ->
				      Pid ! {add_renamed, {FileName, Node}},
				      {refac_syntax:tuple([E0, copy_pos_attrs(E1, refac_syntax:atom(TargetModName)), E2, E3]), true};
				  false ->
				      case refac_syntax:type(E3) == integer andalso refac_syntax:integer_value(E3) == Arity of
					true ->
					    Pid ! {add_renamed, {FileName, Node}},
					    {refac_syntax:tuple([E0, copy_pos_attrs(E1, refac_syntax:atom(TargetModName)), E2, E3]), true};
					false ->
					    {Node, false}
				      end
				end
			  end;
		  false -> {Node, false}
		end;
	    _ ->
		{Node, false}
	  end;
      _ -> {Node, false}
    end.

prettyprint(none) ->
    none;
prettyprint(Node) -> 
    refac_prettypr:format(Node).


renamed_warn_msg(ModName) ->
    "\n=================================================================================\n"
     "WARNING: Wrangler has renamed the uses of '"++atom_to_list(ModName) ++ "' to the target "
     "module name within the following expressions while without enough "
     "syntactic/semantic information.\n Please check manually!\n".
  
not_renamed_warn_msg(FunName) ->
    "\n=================================================================================\n"
     "WARNING: Wrangler could not infer whether the uses of '" ++atom_to_list(FunName) ++"' at the following positions "
     "refer to the function moved.\n Please check manually for necessary module name changes!\n".
    

format(Format, Options) ->
    lists:flatten(io_lib:format(Format, Options)).
