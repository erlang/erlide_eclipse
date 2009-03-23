%%% =====================================================================
%% Refactoring: Move a function definition from one module to another.
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
%% <li> Function <em> foo/n </em> should not contain any uses of <em> implicit fun expressions </em>.
%% </li>
%% </p>
%% @end
%% @author Huiqing Li <hl@kent.ac.uk>
%% @version 0.1

-module(refac_move_fun).

-export([move_fun/7, move_fun_eclipse/7]).

-define(COMMENT_PREFIX, "% ").

-include("../include/wrangler.hrl").

%% @TODO If a new module needs to be created, 1) Wrangler should check that the new module name does not conflict with 
%% libary modules. 2) the undo process should remove the newly created file.
%%  
%% =====================================================================
-spec(move_fun/7::(filename(),integer(),integer(), string(), atom(),[dir()], integer())
        -> {ok, [filename()]} | {error, string()}).

move_fun(FName, Line, Col, TargetModorFileName, CreateNewFile, SearchPaths, TabWidth) ->
    move_fun(FName, Line, Col, TargetModorFileName, CreateNewFile, SearchPaths, TabWidth, emacs).


-spec(move_fun_eclipse/7::(filename(),integer(),integer(), string(), atom(),[dir()], integer())
        ->  {ok, [{filename(), filename(), string()}]} | {error, string()}).

move_fun_eclipse(FName, Line, Col, TargetModorFileName, CreateNewFile, SearchPaths, TabWidth) ->
    move_fun(FName, Line, Col, TargetModorFileName, CreateNewFile, SearchPaths, TabWidth, eclipse).

move_fun(FName, Line, Col, TargetModorFileName, CreateNewFile, SearchPaths, TabWidth, Editor) ->
    ?wrangler_io("\nCMD: ~p:move_fun(~p, ~p, ~p, ~p, ~p, ~p, ~p).",
		 [?MODULE, FName, Line, Col, TargetModorFileName, CreateNewFile, SearchPaths, TabWidth]),
    {TargetFName, TargetModName} = get_target_file_mod_name(FName, TargetModorFileName),
    case TargetFName of 
	FName -> {error, "The target module is the same as the current module."};
	{error, Reason} -> {error, Reason};
	_ -> case filelib:is_file(TargetFName) orelse CreateNewFile == t orelse CreateNewFile == true of
		 true ->
		     {ok, {AnnAST, Info}} = refac_util:parse_annotate_file(FName, true, SearchPaths, TabWidth),
		     case refac_util:pos_to_fun_def(AnnAST, {Line, Col}) of
			 {ok, Def} ->
			     {value, {fun_def, {ModName, FunName, Arity, _Pos1, _Pos2}}} =
				 lists:keysearch(fun_def, 1, refac_syntax:get_ann(Def)),
			     case not filelib:is_file(TargetFName) andalso (CreateNewFile == t orelse CreateNewFile == true) of
				 true -> create_new_file(TargetFName, TargetModName);
				 _ -> ok
			     end,
			     R = side_cond_check({FName, ModName, FunName, Arity, Def}, TargetFName, list_to_atom(TargetModName), Def, SearchPaths, TabWidth),
			     case R of
				 true ->
				     {ok, {TargetAnnAST, Info1}} = refac_util:parse_annotate_file(TargetFName, true, SearchPaths, TabWidth),  
				     {AnnAST1, TargetAnnAST1} =
					 do_transformation({AnnAST, Info}, {TargetAnnAST, Info1},
							   {ModName, FunName, Arity}, TargetModName),
				     case refac_util:is_exported({FunName, Arity}, Info) of
					 true ->
					     ?wrangler_io("\nChecking client modules in the following search paths: \n~p\n", [SearchPaths]),
					     ClientFiles = lists:delete(TargetFName,
									refac_util:get_client_files(FName, SearchPaths)),
					     Results = refactor_in_client_modules(ClientFiles,
										  {ModName, FunName, Arity}, TargetModName, SearchPaths, TabWidth),
					     case Editor of
						 emacs ->
						     refac_util:write_refactored_files([{{FName, FName}, AnnAST1},
											{{TargetFName, TargetFName}, TargetAnnAST1}| Results]),
						     ChangedClientFiles =
							 lists:map(fun ({{F, _F}, _AST}) -> F end, Results),
						     ChangedFiles = [FName, TargetFName| ChangedClientFiles],
						     ?wrangler_io("The following files have been changed "
								  "by this refactoring:\n~p\n", [ChangedFiles]),
						     {ok, ChangedFiles};
						 eclipse ->
						     Results1 = [{{FName, FName}, AnnAST1},
								 {{TargetFName, TargetFName}, TargetAnnAST1}| Results],
						     Res = lists:map(fun ({{FName1, NewFName1}, AST}) ->
									     {FName1, NewFName1, refac_prettypr:print_ast(refac_util:file_format(FName1),AST)}
								     end,
								     Results1),
						     {ok, Res}
					     end;
					 false ->
					     case Editor of
						 emacs ->
						     refac_util:write_refactored_files([{{FName, FName}, AnnAST1},
											{{TargetFName, TargetFName}, TargetAnnAST1}]),
						     {ok, [FName, TargetFName]};
						 eclipse ->
						     Results1 = [{{FName, FName}, AnnAST1}, {{TargetFName, TargetFName}, TargetAnnAST1}],
						     Res = lists:map(fun ({{FName1, NewFName1}, AST}) ->
									     {FName1, NewFName1, refac_prettypr:print_ast(refac_util:file_format(FName1),AST)}
								     end,
								     Results1),
						     {ok, Res}
					     end
				     end;
				 {error, Reason} -> {error, Reason}
			     end;
			 {error, Reason} -> {error, Reason}
		     end;
		 false -> {error, " Target module/file does not exist."}
	     end
    end.

get_target_file_mod_name(CurrentFName, TargetModorFileName) ->
    ErrMsg = {error, "Illegal target module/file name."},
    TargetModName = case filename:extension(TargetModorFileName) of 
			".erl" -> filename:basename(TargetModorFileName, ".erl");
			[] -> TargetModorFileName;
			_ -> "IllegalModName"			
		    end,
    case is_mod_name(TargetModName) of 
	true -> {filename:join([filename:dirname(CurrentFName), TargetModName++".erl"]), TargetModName};
	_  -> ErrMsg
    end.
    
  
create_new_file(TargetFName, TargetModName) ->
     S = "-module("++TargetModName++").",
     file:write_file(TargetFName, list_to_binary(S)).

side_cond_check({FileName, ModName, FunName, Arity, Node}, TargetFileName, TargetModName, FunDef, SearchPaths, TabWidth) ->
    case filelib:is_file(TargetFileName) of
      true -> {ok, {AnnAST, Info}} = refac_util:parse_annotate_file(TargetFileName, true, SearchPaths, TabWidth),
	      InscopeFuns = refac_util:inscope_funs(Info),
	      check_macros(FileName, TargetFileName, FunDef, SearchPaths, TabWidth),
	      check_records(FileName, TargetFileName, FunDef, SearchPaths, TabWidth),
	      Clash = lists:any(fun ({ModName1, FunName1, Arity1}) ->
					(FunName == FunName1) and (Arity == Arity1) and (ModName =/= ModName1)
				end, InscopeFuns),
	      ImplicitFunCall = has_implicit_fun_call(Node),
	      case not Clash of
		true -> case not ImplicitFunCall of
			  true -> true;
			  false -> {error,
				    "Moving a function definiton containing implicit fun expressions "
				    "is not supported by this refactoring."}
			end;
		false ->
		    Forms = refac_syntax:form_list_elements(AnnAST),
		    FunWithSameName = [F || F <- Forms, not is_not_the_fun(F, {TargetModName, FunName, Arity})],
		    case FunWithSameName of
		      [] ->
			  {error, "Moving this function will cause confliction in the target module."};
		      [F] -> case is_the_same_fun(FunDef, F) of
			       true -> true;
			       _ -> {error, "The same function name, with a different definition, is already defined in the target module."}
			     end
		    end
	      end;
      false -> true
    end.


is_the_same_fun(FunDef1, FunDef2) ->
    Ann1 = refac_syntax:get_ann(FunDef1),
    {value, {fun_def, {M1, F1, A1, _, _}}}=lists:keysearch(fun_def,1,Ann1),
    Ann2 = refac_syntax:get_ann(FunDef2),
    {value, {fun_def, {M2, F2, A2, _, _}}} =lists:keysearch(fun_def,1,Ann2),
    FunDef11= reset_attrs(FunDef1, {M1, F1, A1}),
    FunDef21=reset_attrs(FunDef2, {M2, F2, A2}),
    FunDef11 == FunDef21.

check_macros(FileName, TargetFileName, FunDef, SearchPaths, TabWidth) ->
    UsedMacros = collect_used_macros(FunDef),
    case UsedMacros of 
	[] ->
	    true;
	_ -> 
	    Dir = filename:dirname(FileName),
	    DefaultIncl1 = [".","..", "../hrl", "../incl", "../inc", "../include"],
	    DefaultIncl2 = [filename:join(Dir, X) || X <-DefaultIncl1],
	    NewSearchPaths= SearchPaths++DefaultIncl2,
	    case refac_epp:parse_file(FileName, NewSearchPaths, [], TabWidth, refac_util:file_format(FileName))  of 
		{ok, _, {MDefs, _MUses}} -> 
		    UsedMacroDefs = [{Name, {Args, refac_util:concat_toks(Toks)}} || {{_, Name}, {Args, Toks}} <-MDefs, lists:member(Name, UsedMacros)],
		    case length(UsedMacros) > length(UsedMacroDefs) of 
			true ->  UnDefinedUsedMacros = UsedMacros -- [Name || {Name, _Def} <-UsedMacroDefs],
				 ?wrangler_io("\nThe following macros are used by the function to be moved, but not defined:~p\n", [UnDefinedUsedMacros]),
				 throw({error, "There are undefined macros used by the function to be moved."});
			_ ->  case refac_epp:parse_file(TargetFileName, NewSearchPaths, [], TabWidth, refac_util:file_format(TargetFileName)) of 
				  {ok, _, {MDefs1, _MUses1}} ->
				      UsedMacroDefsInTargetFile = [{Name, {Args, refac_util:concat_toks(Toks)}} 
								   || {{_, Name}, {Args, Toks}} <-MDefs1, lists:member(Name, UsedMacros)],
				      case  length(UsedMacros) > length(UsedMacroDefsInTargetFile) of 
					  true ->
					      UnDefinedUsedMacrosInTargetFile = UsedMacros -- [Name || {Name, _Def} <-UsedMacroDefsInTargetFile], 
					      ?wrangler_io("\nThe following macros are used by the function to be moved, but not defined in the target module:~p\n", 
							   [UnDefinedUsedMacrosInTargetFile]),
					      throw({error, "Some macros used by the function to be moved are not defined in the target module."});
					  _ ->
					      case lists:keysort(1,UsedMacroDefs) == lists:keysort(1,UsedMacroDefsInTargetFile) of 
						  true -> true;
						  _ -> throw({error, "Moving this function could change program semantics because of different macro definitions."})
					      end
				      end;
				  _ -> throw({error, "Refactoring failed because the target file does not compile."})
			      end
		    end;
		_ -> throw({error, "Refactoring failed because the current file does not compile."})
	    end
    end.
	    
collect_used_macros(FunDef) ->
    F = fun(T, S) ->
		case refac_syntax:type(T) of
		    macro ->
			Name = refac_syntax:macro_name(T),
			case refac_syntax:type(Name) of 
			    variable -> [refac_syntax:variable_name(Name)|S];
			    atom -> [refac_syntax:atom_value(Name) |S]
			end;
		    _  -> S
		end
	end,
    lists:usort(refac_syntax_lib:fold(F, [], FunDef)).


check_records(FileName, TargetFileName, FunDef, SearchPaths, TabWidth) ->
    UsedRecords = collect_used_records(FunDef),
    case UsedRecords of 
	[] -> true;
	_ ->
	    case refac_util:parse_annotate_file(FileName,false, SearchPaths, TabWidth) of 
		{ok,{_, Info}} ->
		    case lists:keysearch(records, 1, Info) of
		      {value, {records, RecordDefs}} -> 
			    UsedRecordDefs = [{Name, lists:keysort(1,[{F, format(FDef)} || {F, FDef} <-Fields])}
					    || {Name, Fields} <-RecordDefs, lists:member(Name, UsedRecords)],
			    case length(UsedRecords) > length(UsedRecordDefs) of 
				true ->
				    UnDefinedUsedRecords = UsedRecords -- [Name || {Name, _Fields} <- UsedRecordDefs],
				    ?wrangler_io("\nThe following records are used by the function to be moved, but not defined:~p\n", [UnDefinedUsedRecords]),
				    throw({error, "There are undefined records used by the function to be moved."});
				_ -> case refac_util:parse_annotate_file(TargetFileName, false, SearchPaths, TabWidth) of 
					 {ok, {_, Info1}} ->
					     case lists:keysearch(records,1, Info1) of 
						 {value, {records, RecordDefsInTargetFile}} ->
						     UsedRecordDefsInTargetFile = [{Name, lists:keysort(1,[{F, format(FDef)} || {F, FDef} <-Fields])}
										   || {Name, Fields} <-RecordDefsInTargetFile,
										      lists:member(Name, UsedRecords)],
						     case length(UsedRecords) > length(UsedRecordDefsInTargetFile) of 
							 true -> throw({error, "Some records used by the function are not defined in the taret file."});
							 _ -> case lists:keysort(1, UsedRecordDefs) == lists:keysort(1, UsedRecordDefsInTargetFile) of 
								  true ->true;
								  _  -> throw({error,  "Moving this function could change program semantics because of different record definitions."})
							      end
							 end;
						 _ ->throw({error, "Some record(s) used by the function to be moved are not defined in the target file."})
					     end;
					 _ ->throw({error, "Refactoring failed because the target file does not compile."})
				     end
			    end;
			_ ->
			    throw({error, "Refactoring failed because of incomplete information about the record(s) used by this function."})
		    end;
		_ -> throw({error, "Refactring failed because the current file does not compile."})
	    end
    end.
					
	    
format(none) ->
    none;
format(Node) -> refac_prettypr:format(Node).
	    
collect_used_records(FunDef) ->
    F = fun(T, S) ->
		case refac_syntax:type(T) of 
		    record_expr ->
			Type = refac_syntax:record_expr_type(T),
			[refac_syntax:atom_value(Type) |S];
		    record_access ->
			Type = refac_syntax:record_access_type(T),
			[refac_syntax:atom_value(Type)|S];
		    _ -> S
		end
	end,
    lists:usort(refac_syntax_lib:fold(F,[], FunDef)).
		
			
		



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
			      {M, F, A} -> set_default_ann(T1);
			      _ -> refac_syntax:set_pos(
				     refac_syntax:remove_comments(
				       refac_syntax:set_ann(T1, 
							    [{fun_def, {ModName, FunName, Arity, {0,0},{0,0}}}])),{0,0})
			  end;	      
		      _ -> set_default_ann(T1)
		  end
	  end,
    refac_syntax_lib:map(Fun, Node).
				       
set_default_ann(Node) ->
    refac_syntax:set_pos(refac_syntax:remove_comments(refac_syntax:set_ann(Node, [])), {0,0}).


has_implicit_fun_call(Tree) ->    
    F=fun(T,S) ->
	      case refac_syntax:type(T) of 
		  arity_qualifier-> S++[true];
		  _  -> S
	      end
      end,
    R = refac_syntax_lib:fold(F, [], Tree),
    lists:member(true, R).

do_transformation({AnnAST, Info}, {TargetAnnAST, Info1}, {ModName, FunName, Arity}, TargetModName) ->
    Forms = refac_syntax:form_list_elements(AnnAST),
    FunToBeMoved = hd([F || F <- Forms, not is_not_the_fun(F, {ModName, FunName, Arity})]),
    FunsToBeExported = local_funs_to_be_exported(FunToBeMoved, {ModName, FunName, Arity}, Info),
    InScopeFunsInTargetMod = refac_util:inscope_funs(Info1),
    FunToBeMoved1 = transform_fun(FunToBeMoved, {ModName, FunName, Arity}, TargetModName, InScopeFunsInTargetMod),
    {AnnAST1, FunIsUsed} = do_remove_fun(AnnAST, {ModName, FunName, Arity}, FunsToBeExported, TargetModName),
    IsExported = refac_util:is_exported({FunName, Arity}, Info),
    Export = FunIsUsed or IsExported,
    TargetAnnAST1 = do_add_fun({TargetAnnAST, Info1}, FunToBeMoved1, {ModName, FunName, Arity}, TargetModName, Export),
    {AnnAST1, TargetAnnAST1}.

%%==============================================================================
%% Functions defined in the current module, but used by the function to be moved.
%%===============================================================================
local_funs_to_be_exported(Node, {ModName, FunName, Arity}, Info) ->
    Funs = fun (T, S) ->
		   case refac_syntax:type(T) of
		     application ->
			 Operator = refac_syntax:application_operator(T),
			 case application_info(T) of
			   {{none, F}, A} ->
			       case get_fun_def_info(Operator) of
				 {ModName, F, A, _P} when {F, A} =/= {FunName, Arity} ->
				     ordsets:add_element({F, A}, S);
				 _ -> S
			       end;
			   _ -> S
			 end;
		     _ -> S
		   end
	   end,
    Fs = refac_syntax_lib:fold(Funs, ordsets:new(), Node),
    [F || F <- Fs, not refac_util:is_exported(F, Info)].


%%======================================================================================
%% Transform the function to be moved.
%%======================================================================================
transform_fun(Form, {ModName, FunName, Arity}, TargetModName, InScopeFunsInTargetMod) -> 
      {Form1, _} =refac_util:stop_tdTP(fun do_transform_fun/2, Form, {{ModName, FunName, Arity},
							  TargetModName, InScopeFunsInTargetMod}),
      Form1.

do_transform_fun(Node, {{ModName, FunName, Arity}, TargetModName, InScopeFunsInTargetMod}) ->
    MakeApp = fun (Node1, Operator1, Arguments1,ModName1, FunName1) ->
 		     Operator2 =refac_syntax:copy_attrs(Operator1,
 					     refac_syntax:module_qualifier(refac_syntax:atom(ModName1),
 									   refac_syntax:atom(FunName1))),
 		     Node2= refac_syntax:copy_attrs(Node1,
 						     refac_syntax:application(Operator2,Arguments1)),
 		     {Node2, false}
 	     end,
    case refac_syntax:type(Node) of 
 	  application ->
	    Operator = refac_syntax:application_operator(Node),
	    Arguments = refac_syntax:application_arguments(Node),
	    case application_info(Node) of 
		  {{none, F}, A} ->
  		      case {F, A} == {FunName, Arity} of 
  			  true -> {Node, false};
  			  false ->  case refac_syntax:type(Operator) of 
				       atom ->{M, F, A, _P} = get_fun_def_info(Operator),
					       case lists:member({M, F, A}, InScopeFunsInTargetMod) of 
						  true -> {Node, false};
						  _  ->MakeApp(Node, Operator, Arguments, M, F)
					      end;
				       _ -> {Node, false}
				   end				   
  		      end;
 	 	  {{ModName, FunName}, Arity} ->MakeApp(Node,Operator, Arguments, TargetModName, FunName);
		  _ -> {Node, false}
 	      end;
        _  -> {Node, false}
    end.


%%=========================================================================
%% Remove the function from the current module. 
%%=========================================================================
do_remove_fun(AnnAST, {ModName, FunName, Arity}, FunsToBeExported, TargetModName) ->
    Forms  = refac_syntax:form_list_elements(AnnAST),
    Res = [process(Form, {ModName, FunName, Arity}, TargetModName)||Form <-Forms, 
								       is_not_the_fun(Form, {ModName, FunName, Arity})],
    Forms1 = lists:map(fun({F, _M}) -> F end, Res),
    Fun_is_Used = lists:member(true, lists:map(fun({_F, M}) -> M end, Res)),
    NewForms = case FunsToBeExported of 
		   [] -> Forms1;
		   [_H|_T] ->  Export = make_export(FunsToBeExported),
			       {Forms11, Forms12} = lists:splitwith(fun(F) -> refac_syntax:type(F)==attribute 
								  orelse refac_syntax:type(F) == comment end, Forms1),
			       Forms11++Export++Forms12
	       end,
    {refac_syntax:copy_attrs(AnnAST, refac_syntax:form_list(NewForms)), Fun_is_Used}.

%%=======================================================================
%% Add the function to the target module.
%%=======================================================================
do_add_fun({TargetAnnAST, Info}, FunToBeMoved, {ModName, FunName, Arity}, TargetModName, ToBeExported) ->
    Forms = refac_syntax:form_list_elements(TargetAnnAST),
    FunWithSameName = [F || F <- Forms, not is_not_the_fun(F, {list_to_atom(TargetModName), FunName, Arity})],
    NewFun = case FunWithSameName of
	       [] -> [FunToBeMoved];
	       _ -> []
	     end,
    Forms1 = [process_forms_in_target_module(Form, {ModName, FunName, Arity}, TargetModName) || Form <- Forms],
    IsExported = refac_util:is_exported({FunName, Arity}, Info),
    NewForms = case ToBeExported andalso not IsExported of
		 false -> Forms1 ++ NewFun;
		 true -> Export = make_export([{FunName, Arity}]),
			 {Forms11, Forms12} = lists:splitwith(fun (F) -> refac_syntax:type(F) == attribute
									   orelse refac_syntax:type(F) == comment
							      end, Forms1),
			 Forms11 ++ Export ++ Forms12 ++ NewFun
	       end,
    refac_syntax:copy_attrs(TargetAnnAST, refac_syntax:form_list(NewForms)).

%%=========================================================================
%% Refacotoring in the current module. 
%%========================================================================
process(Form, {ModName, FunName, Arity}, TargetModName) ->
    case refac_syntax:type(Form) of 
	function -> add_module_qualifier(Form, {ModName, FunName, Arity}, TargetModName);
	attribute -> Name = refac_syntax:attribute_name(Form),
		     case refac_syntax:type(Name) of 
			 atom ->case refac_syntax:atom_value(Name) of
				    export -> [L] = refac_syntax:attribute_arguments(Form),
					      Es = refac_syntax:list_elements(L),
					      Es1 =[E  || E<-Es, 
						  {refac_syntax:atom_value(refac_syntax:arity_qualifier_body(E)),
						   refac_syntax:integer_value(refac_syntax:arity_qualifier_argument(E))}
						   =/={FunName, Arity}],
					      case length(Es1) == 0  of
						  true -> {comment([""]), false};
						  _ ->
						      {refac_syntax:copy_attrs(Form, 
							  refac_syntax:attribute(Name,[refac_syntax:list(Es1)])), false}
						  end;
				    _  -> {Form, false}
				end;
			 _  -> {Form,false}
		     end;
	_-> {Form,false}
    end.

%%============================================================================
%% Refactoring in the target module.
%%============================================================================
process_forms_in_target_module(Form, {ModName, FunName, Arity}, TargetModName) ->
    case refac_syntax:type(Form) of
      function -> remove_module_qualifier(Form, {ModName, FunName, Arity}, TargetModName);
      attribute -> Name = refac_syntax:attribute_name(Form),
		   case refac_syntax:type(Name) of
		     atom -> case refac_syntax:atom_value(Name) of
			       import ->
				   [H, L] = refac_syntax:attribute_arguments(Form),
				   Es = refac_syntax:list_elements(L),
				   Es1 = [E || E <- Es,
					       {refac_syntax:atom_value(refac_syntax:arity_qualifier_body(E)),
						refac_syntax:integer_value(refac_syntax:arity_qualifier_argument(E))}
						 =/= {FunName, Arity}],
				   case length(Es1) == 0 of
				     true -> comment([""]);
				     _ -> refac_syntax:copy_attrs(Form,
								  refac_syntax:attribute(Name, [H, refac_syntax:list(Es1)]))
				   end;
			       _ -> Form
			     end;
		     _ -> Form
		   end;
      _ -> Form
    end.

%%============================================================================
%% Add/remove module qualifier.
%%============================================================================
remove_module_qualifier(Form, {ModName, FunName, Arity}, TargetModName) ->    
    {Form1, _} = refac_util:stop_tdTP(fun do_remove_module_qualifier/2, Form, {{ModName, FunName, Arity}, TargetModName}),
    Form1.

do_remove_module_qualifier(Node, {{ModName, FunName, Arity}, TargetModName}) ->
    case refac_syntax:type(Node) of 
	application ->
	    Op = refac_syntax:application_operator(Node),
	    Args =refac_syntax:application_arguments(Node),
	    case application_info(Node) of 
		{{none, FunName}, Arity} -> {Node, true};
		{{ModName, FunName}, Arity} -> Op1 = refac_syntax:copy_attrs(Op,refac_syntax:atom(FunName)),
					       Node1 = refac_syntax:copy_attrs(Node, refac_syntax:application(Op1, Args)),
					       {Node1, true};
		{{_, apply},2} -> transform_apply_call(Node,{ModName, FunName, Arity}, TargetModName);
		{{_, apply},3} -> transform_apply_call(Node, {ModName, FunName, Arity}, TargetModName);
		{{_, spawn}, 3} ->transform_spawn_call(Node, {ModName, FunName, Arity}, TargetModName);
		{{_, spawn}, 4} -> transform_spawn_call(Node, {ModName, FunName, Arity}, TargetModName); 
		{{_, spawn_link}, 3} ->transform_spawn_call(Node, {ModName, FunName, Arity}, TargetModName);
		{{_, spawn_link}, 4} ->transform_spawn_call(Node, {ModName, FunName, Arity}, TargetModName);
		_ -> {Node, true}
	    end;		
	_ ->{Node, false}
    end.
	    
add_module_qualifier(Form, {ModName, FunName, Arity}, TargetModName) ->
      refac_util:stop_tdTP(fun do_add_module_qualifier/2,
				Form, {{ModName, FunName, Arity}, TargetModName}).
   

do_add_module_qualifier(Node, {{ModName, FunName, Arity}, TargetModName}) ->
   MakeApp = fun (Node1, Operator1, Arguments1,TargetModName1, FunName1) ->
		     Operator2 =refac_syntax:copy_attrs(Operator1,
					     refac_syntax:module_qualifier(refac_syntax:atom(TargetModName1),
									   refac_syntax:atom(FunName1))),
		     Node2= refac_syntax:copy_attrs(Node1,
						     refac_syntax:application(Operator2,Arguments1)),
		     {Node2, false}
	     end,
   case refac_syntax:type(Node) of 
	  application ->
	      Operator = refac_syntax:application_operator(Node),
	      Arguments = refac_syntax:application_arguments(Node),
	      case application_info(Node) of 
		  {{none, FunName}, Arity} -> MakeApp(Node, Operator, Arguments, TargetModName, FunName);	      
		  {{ModName, FunName}, Arity} ->MakeApp(Node,Operator, Arguments, TargetModName, FunName);
		  {{_, apply},2} -> transform_apply_call(Node,{ModName, FunName, Arity}, TargetModName);
		  {{_, apply},3} -> transform_apply_call(Node, {ModName, FunName, Arity}, TargetModName);
		  {{_, spawn}, 3} ->transform_spawn_call(Node, {ModName, FunName, Arity}, TargetModName);
		  {{_, spawn}, 4} -> transform_spawn_call(Node, {ModName, FunName, Arity}, TargetModName); 
		  {{_, spawn_link}, 3} ->transform_spawn_call(Node, {ModName, FunName, Arity}, TargetModName);
		  {{_, spawn_link}, 4} ->transform_spawn_call(Node, {ModName, FunName, Arity}, TargetModName);
		   _ -> {Node, false}
	      end;
       implicit_fun ->
	   Name = refac_syntax:implicit_fun_name(Node),
	   B = refac_syntax:atom_value(refac_syntax:arity_qualifier_body(Name)),
	   A = refac_syntax:integer_value(refac_syntax:arity_qualifier_argument(Name)),
	   case {B, A} of
	       {FunName, Arity} ->
		   FunName1 = refac_syntax:module_qualifier(refac_syntax:atom(TargetModName),
							    refac_syntax:atom(FunName)),
		   Node1 = refac_syntax:implicit_fun(FunName1, refac_syntax:arity_qualifier_argument(Name)),
		   {refac_syntax:copy_attrs(Node, Node1), true};
	       _ -> {Node, false}
	   end;
       _  -> {Node, false}
   end.
%% ====================================================================================
%%  Processing Client modules.
%%====================================================================================

refactor_in_client_modules(ClientFiles,{ModName, FunName, Arity}, TargetModName, SearchPaths, TabWidth) ->
    case ClientFiles of 
	[] -> [];
	[F | Fs] ->
	    ?wrangler_io("The current file under refactoring is:\n~p\n", [F]),
	    {ok, {AnnAST, Info}} =refac_util:parse_annotate_file(F,true, SearchPaths, TabWidth),      %% level=1
	    {AnnAST1, Changed} = 
				refactor_in_client_module_1({AnnAST, Info}, {ModName, FunName, Arity}, TargetModName),
		if Changed ->
		    [{{F,F}, AnnAST1} | refactor_in_client_modules(Fs, {ModName, FunName, Arity}, TargetModName, SearchPaths, TabWidth)];
	       true -> refactor_in_client_modules(Fs, {ModName, FunName, Arity}, TargetModName, SearchPaths, TabWidth)
	    end
	end.
refactor_in_client_module_1({AnnAST, _Info}, {ModName, FunName, Arity}, TargetModName) ->
    Forms  = refac_syntax:form_list_elements(AnnAST),
    Res = [process_in_client_module(Form, {ModName, FunName, Arity}, TargetModName)||Form <-Forms],
    Forms1 = lists:map(fun({F, _M}) -> F end, Res),
    Changed = lists:member(true, lists:map(fun({_F, M}) -> M end, Res)),
    {refac_syntax:copy_attrs(AnnAST, refac_syntax:form_list(Forms1)), Changed}.

process_in_client_module(Form, {ModName, FunName, Arity}, TargetModName) ->
    case refac_syntax:type(Form) of
      function -> add_module_qualifier(Form, {ModName, FunName, Arity}, TargetModName);
      attribute -> Name = refac_syntax:attribute_name(Form),
		   case refac_syntax:type(Name) of
		     atom -> case refac_syntax:atom_value(Name) of
			       import ->
				   [H, L] = refac_syntax:attribute_arguments(Form),
				   Es = refac_syntax:list_elements(L),
				   Es1 = [E || E <- Es,
					       {refac_syntax:atom_value(refac_syntax:arity_qualifier_body(E)),
						refac_syntax:integer_value(refac_syntax:arity_qualifier_argument(E))}
						 =/= {FunName, Arity}],
				   {refac_syntax:copy_attrs(Form,
							    refac_syntax:attribute(Name, [H, refac_syntax:list(Es1)])),
				    length(Es) =/= length(Es1)};
			       _ -> {Form, false}
			     end;
		     _ -> {Form, false}
		   end;
      _ -> {Form, false}
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
	_ -> erlang:error(bagarg)
    end.
%%================================================================================
%%              Some Utility Functions 
%%================================================================================


make_export(Names) ->
    Es = [refac_syntax:arity_qualifier(refac_syntax:atom(F),
				       refac_syntax:integer(A))
	  || {F, A} <- Names],
   %% comment(["** The following export is added by Wrangler. **"]),
    [erl_syntax:attribute(erl_syntax:atom('export'), [erl_syntax:list(Es)])].

is_not_the_fun(Form, {ModName, FunName, Arity}) ->
    case refac_syntax:type(Form) of 
	function -> 
	    As = refac_syntax:get_ann(Form),
	    case lists:keysearch(fun_def,1,As) of 
		{value, {fun_def, {ModName, FunName, Arity, _Pos1, _Pos2}}} ->
		    false;
		_ -> true
	    end;	    
	_ -> true
    end.

transform_apply_call(Node, {ModName, FunName, Arity}, TargetModName) ->
    Message = fun
		(Pos) -> ?wrangler_io("WARNING: function ***apply*** is used at location({line, col}):~p, and wrangler "
				      "could not decide whether this site should be refactored, please check!!!\n",
				      [Pos])
	      end,
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
		      FunName1 = refac_syntax:module_qualifier(refac_syntax:atom(TargetModName),
							       refac_syntax:atom(FunName)),
		      Fun1 = refac_syntax:implicit_fun(FunName1, refac_syntax:arity_qualifier_argument(Name)),
		      {refac_syntax:copy_attrs(Node, refac_syntax:application(Operator, [Fun1, Args])), true};
		  _ -> {Node, false}
		end;
	    _ -> {Node, false}
	  end;
      [Mod, Fun, Args] ->
	  Mod1 = refac_util:try_evaluation([refac_syntax:revert(Mod)]),
	  Fun1 = refac_util:try_evaluation([refac_syntax:revert(Fun)]),
	  Pos = refac_syntax:get_pos(Node),
	  case Fun1 of
	    {value, FunName} ->
		case Mod1 of
		  {value, ModName} ->
		      case refac_syntax:type(Args) of
			list ->
			    case refac_syntax:list_length(Args) of
			      Arity ->
				  Mod2 = refac_syntax:atom(TargetModName),
				  {refac_syntax:copy_pos(Node, refac_syntax:copy_attrs(Node,
										       refac_syntax:application(Operator, [Mod2, Fun, Args]))), true};
			      _ -> {Node, false}
			    end;
			nil -> if Arity == 0 ->
				      Mod2 = refac_syntax:atom(TargetModName),
				      {refac_syntax:copy_pos(Node, refac_syntax:copy_attrs(Node,
											   refac_syntax:application(Operator, [Mod2, Fun, Args]))), true};
				  true -> {Node, false}
			       end;
			_ -> Message(Pos),
			     {Node, false}
		      end;
		  {value, _} -> {Node, false};
		  {error, _Reason} ->
		      case refac_syntax:type(Args) of
			list -> case refac_syntax:list_length(Args) of
				  Arity -> Message(Pos),
					   {Node, false};
				  _ -> {Node, false}
				end;
			_ -> Message(Pos),
			     {Node, false}
		      end
		end;
	    {value, _} -> {Node, false};
	    {error, _Reason} ->
		case Mod1 of
		  {value, ModName} ->
		      case refac_syntax:type(Args) of
			list -> case refac_syntax:list_length(Args) of
				  Arity -> Message(Pos),
					   {Node, false};
				  _ -> {Node, false}
				end;
			_ -> Message(Pos),
			     {Node, false}
		      end;
		  {value, _} -> {Node, false};
		  {error, _Reason} -> case refac_syntax:type(Args) of
					list -> case refac_syntax:list_length(Args) of
						  Arity -> Message(Pos),
							   {Node, false};
						  _ -> {Node, false}
						end;
					_ -> Message(Pos),
					     {Node, false}
				      end
		end
	  end;
      _ -> {Node, false}
    end.

transform_spawn_call(Node, {ModName, FunName, Arity}, TargetModName) ->
    Message = fun
		(Pos) -> ?wrangler_io("WARNING: function ***spawn*** is used at location({line, col}):~p, and wrangler "
				      "could not decide whether this site should be refactored, please check!!!\n",
				      [Pos])
	      end,
    Operator = refac_syntax:application_operator(Node),
    Arguments = refac_syntax:application_arguments(Node),
    [N, Mod, Fun, Args] = if length(Arguments) == 4 -> Arguments;
			     true -> [none] ++ Arguments
			  end,
    Mod1 = refac_util:try_evaluation([refac_syntax:revert(Mod)]),
    Fun1 = refac_util:try_evaluation([refac_syntax:revert(Fun)]),
    Pos = refac_syntax:get_pos(Node),
    case Fun1 of
      {value, FunName} ->
	  case Mod1 of
	    {value, ModName} ->
		case refac_syntax:type(Args) of
		  list ->
		      case refac_syntax:list_length(Args) of
			Arity ->
			    Mod2 = refac_syntax:atom(TargetModName),
			    App = if length(Arguments) == 4 ->
					 refac_syntax:application(Operator, [N, Mod2, Fun, Args]);
				     true -> refac_syntax:application(Operator, [Mod2, Fun, Args])
				  end,
			    {refac_syntax:copy_pos(Node, refac_syntax:copy_attrs(Node, App)), true};
			_ -> {Node, false}
		      end;
		  nil -> if Arity == 0 ->
				Mod2 = refac_syntax:atom(TargetModName),
				App = if length(Arguments) == 4 ->
					     refac_syntax:application(Operator, [N, Mod2, Fun, Args]);
					 true -> refac_syntax:application(Operator, [Mod2, Fun, Args])
				      end,
				{refac_syntax:copy_pos(Node, refac_syntax:copy_attrs(Node, App)), true};
			    true -> {Node, false}
			 end;
		  _ -> Message(Pos),
		       {Node, false}
		end;
	    {value, _} -> {Node, false};
	    {error, _Reason} ->
		case refac_syntax:type(Args) of
		  list -> case refac_syntax:list_length(Args) of
			    Arity -> Message(Pos),
				     {Node, false};
			    _ -> {Node, false}
			  end;
		  _ -> Message(Pos),
		       {Node, false}
		end
	  end;
      {value, _} -> {Node, false};
      {error, _Reason} ->
	  case Mod1 of
	    {value, ModName} ->
		case refac_syntax:type(Args) of
		  list -> case refac_syntax:list_length(Args) of
			    Arity -> Message(Pos),
				     {Node, false};
			    _ -> {Node, false}
			  end;
		  _ -> Message(Pos),
		       {Node, false}
		end;
	    {value, _} -> {Node, false};
	    {error, _Reason} -> case refac_syntax:type(Args) of
				  list -> case refac_syntax:list_length(Args) of
					    Arity -> Message(Pos),
						     {Node, false};
					    _ -> {Node, false}
					  end;
				  _ -> Message(Pos),
				       {Node, false}
				end
	  end
    end.

get_fun_def_info(Node) ->
    As = refac_syntax:get_ann(Node),
    case lists:keysearch(fun_def, 1, As) of
      {value,
       {fun_def, {Mod, FunName, Arity, _UsePos, DefinePos}}} ->
	  {Mod, FunName, Arity, DefinePos};
      _ -> false
    end.


is_mod_name(A) ->
    refac_util:is_fun_name(A).


comment(Txt) ->
    comment(Txt, ?COMMENT_PREFIX).

comment(Txt, Prefix) ->
    erl_syntax:comment(prefix_lines(split_lines(Txt), Prefix)).

prefix_lines([L | Ls], Prefix) ->
    [Prefix ++ L | prefix_lines(Ls, Prefix)];
prefix_lines([], _) ->
    [].

split_lines(Ls) ->
    split_lines(Ls, []).

split_lines([L | Ls], Ls1) ->
    split_lines(Ls, split_lines(L, [], Ls1));
split_lines([], Ls1) ->
    lists:reverse(Ls1).

%% split_lines([$\r, $\n | Cs], Cs1, Ls) ->
%%     split_lines_1(Cs, Cs1, Ls);
%% split_lines([$\r | Cs], Cs1, Ls) ->
%%     split_lines_1(Cs, Cs1, Ls);
%% split_lines([$\n | Cs], Cs1, Ls) ->
%%     split_lines_1(Cs, Cs1, Ls);
%% split_lines([C | Cs], Cs1, Ls) ->
%%     split_lines(Cs, [C | Cs1], Ls);
split_lines([], Cs, Ls) ->
    [lists:reverse(Cs) | Ls].

%% split_lines_1(Cs, Cs1, Ls) ->
%%     split_lines(Cs, [], [lists:reverse(Cs1) | Ls]).
