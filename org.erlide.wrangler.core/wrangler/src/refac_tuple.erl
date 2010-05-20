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
%%==============================================================================
%% @doc Group a consecutive sequence of arguments of a function into a tuple.
%% <p> To apply this refactoring, highlight the arguments to be grouped into a tuple from 
%% the function definition, then select <em> Tuple Function Arguments </em> from 
%%the <em> Refactor </em> menu.
%% </p>
%% <p>
%% When tupling an exported function parameters, this refactoring has a global 
%% effect, i.e., it affects all those modules in which this function is 
%% imported/used.
%% </p>
%% <p> The following <em> side-conditions </em> apply to this refactoring:
%% <li> The new function arity should not cause confliction with any of the 
%% functions which are in scope in the current module;</li>
%% <li> In the case that the function is imported by another module, 
%% the new function arity and the same name should not be already in scope 
%% (either defined or imported) in that module. </li>
%% </p>
%% =============================================================================

-module(refac_tuple).

-export([tuple_funpar/5, tuple_funpar_1/5]).
-export([tuple_funpar_eclipse/5, tuple_funpar_1_eclipse/5]).

-export([tuple_funpar/7]).  %% For testing purpose.

-import(refac_misc, [try_eval/4, commontest_callback_funs/0,
		     eqc_statem_callback_funs/0,testserver_callback_funs/0,
		     apply_style_funs/0]).

-include("../include/wrangler.hrl").

-spec(tuple_funpar/5::(filename(), pos(), pos(), [dir()], integer()) ->
	     {error, string()} | {ok, [filename()]}).
tuple_funpar(FileName, StartLoc, EndLoc, SearchPaths, TabWidth)->
    tuple_funpar(FileName, StartLoc, EndLoc, SearchPaths, TabWidth, emacs).


-spec(tuple_funpar_eclipse/5::(filename(), pos(), pos(), [dir()], integer()) ->
	     {error, string()} | {ok, [{filename(), filename(), string()}]}).
tuple_funpar_eclipse(FileName, StartLoc, EndLoc,  SearchPaths, TabWidth)->
    tuple_funpar(FileName, StartLoc, EndLoc, SearchPaths, TabWidth, eclipse).


-spec(tuple_funpar_1/5::(filename(), pos(), pos(), [dir()], integer()) ->
	     {error, string()} | {ok, [filename()]}).
tuple_funpar_1(FileName, StartLoc, EndLoc, SearchPaths, TabWidth)->
    tuple_funpar_1(FileName, StartLoc, EndLoc, SearchPaths, TabWidth, emacs).


-spec(tuple_funpar_1_eclipse/5::(filename(), pos(), pos(), [dir()], integer()) ->
	     {error, string()} | {ok, [filename()]}).
tuple_funpar_1_eclipse(FileName, StartLoc, EndLoc, SearchPaths, TabWidth)->
    tuple_funpar_1(FileName, StartLoc, EndLoc, SearchPaths, TabWidth, emacs).


tuple_funpar(FileName, Line, Col, Index, Num, SearchPaths, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:tuple_funpar(~p, ~p,~p,~p,~p, ~p, ~p).\n",
		 [?MODULE, FileName, Line, Col, Index, Num, SearchPaths, TabWidth]),
    {ok, {AnnAST, Info}} = refac_util:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    {ok, FunDef} = interface_api:pos_to_fun_def(AnnAST, {Line, Col}),
    FunName = refac_syntax:data(refac_syntax:function_name(FunDef)),
    FunArity = refac_syntax:function_arity(FunDef),
    NewArity = FunArity - Num + 1,
    ok = pre_cond_check(FileName, FunName, FunArity, NewArity, Info),
    tuple_par_0(FileName, AnnAST, Info, FunName, FunArity,
		Index, Num, SearchPaths, TabWidth, emacs, "").

tuple_funpar_1(FileName, StartLoc = {StartLine, StartCol}, EndLoc = {EndLine, EndCol}, SearchPaths, TabWidth, Editor) ->
    Cmd = "CMD: " ++ atom_to_list(?MODULE) ++ ":tuple_funpar(" ++ "\"" ++
	    FileName ++ "\", {" ++ integer_to_list(StartLine) ++ ", " ++ integer_to_list(StartCol) ++ "}," ++
	      "{" ++ integer_to_list(EndLine) ++ ", " ++ integer_to_list(EndCol) ++ "}, ["
		++ refac_misc:format_search_paths(SearchPaths) ++ "]," ++ integer_to_list(TabWidth) ++ ").",
    {ok, {AnnAST, Info}} = refac_util:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    {FunName, FunArity, Index, Num} = pos_to_pars(AnnAST, StartLoc, EndLoc),
    tuple_par_0(FileName, AnnAST, Info, FunName, FunArity,
		Index, Num, SearchPaths, TabWidth, Editor, Cmd).

tuple_funpar(FileName, StartLoc = {StartLine, StartCol}, EndLoc = {EndLine, EndCol},
	     SearchPaths, TabWidth, Editor) ->
    ?wrangler_io("\nCMD: ~p:tuple_funpar(~p, {~p,~p}, {~p,~p}, ~p, ~p).\n",
		 [?MODULE, FileName, StartLine, StartCol, EndLine, EndCol, SearchPaths, TabWidth]),
    Cmd = "CMD: " ++ atom_to_list(?MODULE) ++ ":tuple_funpar(" ++ "\"" ++
	    FileName ++ "\", {" ++ integer_to_list(StartLine) ++ ", " ++ integer_to_list(StartCol) ++ "}," ++
	      "{" ++ integer_to_list(EndLine) ++ ", " ++ integer_to_list(EndCol) ++ "}, ["
		++ refac_misc:format_search_paths(SearchPaths) ++ "]," ++ integer_to_list(TabWidth) ++ ").",
    {ok, {AnnAST, Info}} = refac_util:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    {FunName, FunArity, Index, Num} = pos_to_pars(AnnAST, StartLoc, EndLoc),
    NewArity = FunArity - Num + 1,
    ok = pre_cond_check(FileName, FunName, FunArity, NewArity, Info),
    tuple_par_0(FileName, AnnAST, Info, FunName, FunArity,
		Index, Num, SearchPaths, TabWidth, Editor, Cmd).

tuple_par_0(FileName, AnnAST, Info, FunName, Arity, Index, Num, SearchPaths, TabWidth, Editor, Cmd) ->
    ?wrangler_io("The current file under refactoring is:\n~p\n", [FileName]),
    {ok, FunDefMod} = get_module_name(FileName, Info),
    AnnAST1 = tuple_pars(FileName, AnnAST, FunDefMod, FunName, Arity, Index, Num, Info, SearchPaths, TabWidth),
    case refac_misc:is_exported({FunName, Arity}, Info) of
      true ->
	  ?wrangler_io("\nChecking client modules in the following search paths: \n~p\n", [SearchPaths]),
	  ClientFiles = refac_util:get_client_files(FileName, SearchPaths),
	  try
	    tuple_pars_in_client_modules(ClientFiles, FunDefMod, FunName, Arity, Index, Num, SearchPaths, TabWidth)
	  of
	    Results ->
		refac_util:write_refactored_files([{{FileName, FileName}, AnnAST1}| Results], Editor, Cmd)
	  catch
	    throw:Err ->
		Err
	  end;
      false ->
	  refac_util:write_refactored_files([{{FileName, FileName}, AnnAST1}], Editor, Cmd)
    end.


pre_cond_check(FileName, FunName, OldArity, NewArity, Info) ->
    Inscope_Funs = [{F, A} || {_M, F, A} <- refac_misc:inscope_funs(Info)],
    case lists:member({FunName, NewArity}, Inscope_Funs) orelse
	erl_internal:bif(erlang, FunName, NewArity)
	of
      true ->
	  throw({error, atom_to_list(FunName) ++ "/" ++
			  integer_to_list(NewArity) ++ " is already in scope, "
						       "or is an auto-imported builtin function."});
      false ->
	  case refac_misc:is_callback_fun(Info, FunName, OldArity) of
	    true -> throw({warning, "The function to be renamed is"
				    "a callback function, continue?"});
	    false ->
		test_framework_aware_name_checking(
		  FileName, FunName, OldArity, NewArity)
	  end
    end.


test_framework_aware_name_checking(FileName, FunName, OldArity, NewArity) ->
    UsedTestFrameWorks = refac_util:test_framework_used(FileName),
    eunit_name_checking(UsedTestFrameWorks, FunName, OldArity, NewArity),
    eqc_name_checking(UsedTestFrameWorks, FunName, OldArity,  NewArity),
    testserver_name_checking(UsedTestFrameWorks, FunName, OldArity, NewArity),
    commontest_name_checking(UsedTestFrameWorks, FunName, OldArity, NewArity).
 

eunit_name_checking(_, _, _, _) ->
    ok.
    
eqc_name_checking(UsedFrameWorks, FunName, OldArity, NewArity) ->
    case lists:member(eqc_statem, UsedFrameWorks) of 
	true -> 
	    eqc_statem_name_checking(FunName, OldArity, NewArity);
	false -> ok
    end.

eqc_statem_name_checking(FunName, OldArity, NewArity) ->
    case lists:member({FunName, OldArity}, eqc_statem_callback_funs()) of 
	true ->
	    throw({warning, "The function selected is a callback function, continue?"});
	false -> 
	    case lists:member({FunName, NewArity}, eqc_statem_callback_funs()) of 
		true ->
		    throw({warning, "The new function would be a "
		     "QuickCheck callback function, continue?"});
		false -> ok
	    end
    end.

testserver_name_checking(UsedFrameWorks, FunName, OldArity, NewArity) ->
    case lists:member(testserver,UsedFrameWorks) of 
	true ->
	    testserver_name_checking(FunName, OldArity, NewArity);
	false -> ok
    end.

testserver_name_checking(FunName, OldArity, NewArity) ->
    case lists:member({FunName, OldArity}, testserver_callback_funs()) of 
	true ->
	    throw({warning, "The function selected is a Test Server callback function, "
		   "continue?"});
	false -> 
	    case lists:member({FunName, NewArity}, testserver_callback_funs()) of 
		true ->
		    throw({warning, "The new function would be a Test Server "
			   "special function, continue?"});
		false -> ok
	    end
    end. 	 

commontest_name_checking(UsedFrameWorks, FunName, OldArity, NewArity) ->
    case lists:member(commontest,UsedFrameWorks) of 
	true ->
	    commontest_name_checking(FunName, OldArity, NewArity);
	false -> ok
    end.
commontest_name_checking(FunName, OldArity,NewArity) ->
    case lists:member({FunName, OldArity}, commontest_callback_funs()) of 
	true ->
	    throw({warning, "The function selected is a Common Test "
		   "callback function, continue?"});
	false ->
	    case lists:member({FunName, NewArity}, commontest_callback_funs()) of 
		true ->
		    throw({warning, "The new function would be a Common Test "
			   "callback function, continue?"});
		false -> ok
	    end
    end.

pos_to_pars(AnnAST, StartLoc, EndLoc) ->
    case interface_api:pos_to_fun_def(AnnAST, EndLoc) of
      {ok, FunDef} ->
	  FunName = refac_syntax:data(refac_syntax:function_name(FunDef)),
	  FunArity = refac_syntax:function_arity(FunDef),
	  Cs = refac_syntax:function_clauses(FunDef),
	  C = [C || C <- Cs, {StartLoc1, EndLoc1} <- [refac_misc:get_start_end_loc(C)],
		    StartLoc1 =< StartLoc, EndLoc =< EndLoc1],
	  case C of
	    [] -> throw({error, "You have not selected a sequence parameters,"
				"or the function containing the parameters selected does not parse."});
	    [C1| _] ->
		Pars = refac_syntax:clause_patterns(C1),
		{Pars1, Pars2} = lists:splitwith(
				   fun (P) ->
					   {S, _E} = refac_misc:get_start_end_loc(P),
					   S < StartLoc
				   end, Pars),
		{Pars21, _Pars22} = lists:splitwith(
				      fun (P) ->
					      {_S, E} = refac_misc:get_start_end_loc(P),
					      E =< EndLoc
				      end, Pars2),
		case Pars21 of
		  [] -> throw({error, "You have not selected a sequence of parameters,"
				      "or the function containing the parameters selected does not parse."});
		  [_H] -> throw({error, "Tupling one argument is not supported by Wrangler."});
		  _ -> {FunName, FunArity, length(Pars1) + 1, length(Pars21)}
		end
	  end;
      {error, _Reason} -> throw({error, "You have not selected a sequence parameters,"
					"or the function containing the parameters selected does not parse."})
    end.



collect_implicit_funs(AnnAST, {FunName, Arity}) ->
    F = fun (Node, _) ->
		case refac_syntax:type(Node) of
		  implicit_fun ->
		      Name = refac_syntax:implicit_fun_name(Node),
		      case refac_syntax:type(Name) of
			arity_qualifier ->
			    Body = refac_syntax:arity_qualifier_body(Name),
			    Arg = refac_syntax:arity_qualifier_argument(Name),
			    F = refac_syntax:atom_value(Body),
			    A = refac_syntax:integer_value(Arg),
			    case {F, A} of
			      {FunName, Arity} ->
				  {[Node], true};
			      _ -> {[], false}
			    end;
			_ ->
			    {[], false}
		      end;
		  _ -> {[], false}
		end
	end,
    element(1, ast_traverse_api:once_tdTU(F, AnnAST, {})).
    
tuple_pars(FileName, AnnAST, ModName, FunName, Arity, Index, Num, Info, SearchPaths, TabWidth) ->
    Forms = refac_syntax:form_list_elements(AnnAST),
    Args = {FileName, ModName, ModName, FunName, Arity, Index,Num, SearchPaths, TabWidth},
    Forms1 = [F1 || F <- Forms, F1 <- do_tuple_fun_pars(F, Args)],
    AnnAST1 = refac_syntax:form_list(Forms1),
    case refac_misc:is_exported({FunName, Arity}, Info) of
      true ->
	  AnnAST1;
      false ->
	  case collect_implicit_funs(AnnAST1, {FunName, Arity}) of
	    [] ->
		Forms2 = [F || F <- Forms1, not defines(F, {FunName, Arity})],
		refac_syntax:form_list(Forms2);
	    _ ->
		AnnAST1
	  end
    end.

defines(F, {FunName, Arity}) ->
    case refac_syntax:type(F) of
	function ->
	    refac_syntax:data(refac_syntax:function_name(F))==FunName andalso
		refac_syntax:function_arity(F)==Arity;
	    _ -> false
    end.
    
		     

		     
   

do_tuple_fun_pars(Form, Args) ->
    case refac_syntax:type(Form) of
	function ->
	    tuple_pars_in_function(Form, Args);
	attribute ->
	    tuple_pars_in_attribute(Form, Args);
	_ -> [Form]    
    end.


tuple_pars_in_function(Form, Args = {_FileName, _CurModName, _FunDefMod, FunName, Arity,
					Index, Num, _SearchPaths, _TabWidth}) ->
    Fun1 = refac_syntax:function_name(Form),
    FunName1 = refac_syntax:data(Fun1),
    FunArity1 = refac_syntax:function_arity(Form),
    case {FunName1, FunArity1} of
      {FunName, Arity} ->
	  Cs = refac_syntax:function_clauses(Form),
	  NewCs = lists:map(
		    fun (C) ->
			    Body = refac_syntax:clause_body(C),
			    Guard = refac_syntax:clause_guard(C),
			    Pats = refac_syntax:clause_patterns(C),
			    NewPats = process_pars(Pats, Index, Num),
			    NewBody = [tuple_actual_pars(B, Args) || B <- Body],
			    refac_misc:rewrite(C, refac_syntax:clause(NewPats, Guard, NewBody))
		    end, Cs),
	  NewForm = refac_misc:rewrite(Form, refac_syntax:function(Fun1, NewCs)),
	  Cs1 = refac_syntax:function_clauses(Form),
	  NewCs1 = lists:map(
		     fun (C) ->
			     Pats = refac_syntax:clause_patterns(C),
			     Pats1 = [P1 || P <- Pats, {P1, _}
							   <- [ast_traverse_api:full_tdTP(
								 fun do_replace_underscore/2, P, [])]],
			     G = refac_syntax:clause_guard(C),
			     Op = refac_syntax:atom(FunName),
			     Pats2 = process_pars(Pats1, Index, Num),
			     Body = [refac_syntax:application(Op, Pats2)],
			     refac_syntax:clause(Pats, G, Body)
		     end, Cs1),
	  NewForm1 = refac_syntax:function(refac_syntax:atom(FunName), NewCs1),
	  [NewForm1, NewForm];
      _ -> [tuple_actual_pars(Form, Args)]
    end.


tuple_pars_in_attribute(Form, Args = {_FileName, _CurModName, _FunDefMod, FunName, Arity,
					 _Index, Num, _SearchPaths, _TabWidth}) ->
    AttrName = refac_syntax:attribute_name(Form),
    case refac_syntax:type(AttrName) of
      atom ->
	  Name = refac_syntax:atom_value(AttrName),
	  AttrArgs0 = refac_syntax:attribute_arguments(Form),
	  case Name of
	    export when AttrArgs0 /= none ->
		
		AttrArgs = hd(AttrArgs0),
		NewAttrArgs = lists:flatmap(
				fun (A) ->
					Fun = refac_syntax:arity_qualifier_body(A),
					FunName1 = refac_syntax:atom_value(Fun),
					Arity1 = refac_syntax:arity_qualifier_argument(A),
					ArityVal = refac_syntax:integer_value(Arity1),
					case {FunName1, ArityVal} of
					  {FunName, Arity} ->
					      NewArity = Arity - Num + 1,
					      NewArity1 = refac_misc:rewrite(Arity1, refac_syntax:integer(NewArity)),
					      [A, refac_syntax:arity_qualifier(Fun, NewArity1)];
					  _ -> [A]
					end
				end, refac_syntax:list_elements(AttrArgs)),
		NewAttrArgs1 = [refac_misc:rewrite(AttrArgs, refac_syntax:list(NewAttrArgs))],
		NewAttr = refac_misc:rewrite(Form, refac_syntax:attribute(AttrName, NewAttrArgs1)),
		[NewAttr];
	    _ ->
		[tuple_actual_pars(Form, Args)]
	  end;
      _ ->
	  [tuple_actual_pars(Form, Args)]
    end.


   
process_pars(Pars, Index, Num) ->
    Pars1 = lists:sublist(Pars, Index - 1),
    Pars2 = lists:sublist(Pars, Index, Num),
    Pars21 = [refac_misc:rewrite(hd(Pars2), refac_syntax:tuple(Pars2))],
    Pars3 = lists:nthtail(Index + Num - 1, Pars),
    Pars1 ++ Pars21 ++ Pars3.


tuple_actual_pars(Node, Args) ->
    element(1, ast_traverse_api:full_tdTP(fun do_tuple_actual_pars/2, Node, Args)).
 

do_tuple_actual_pars(Node, Others = {_FileName, CurModName, FunDefMod, FunName,
				     Arity, Index, Num, _SearchPaths, _TabWith}) ->
    case refac_syntax:type(Node) of
      application ->
	  Op = refac_syntax:application_operator(Node),
	  Args = refac_syntax:application_arguments(Node),
	  case get_fun_def_info(Op) of
	    {FunDefMod, FunName, Arity} ->
		NewArgs = process_pars(Args, Index, Num),
		Node1 = refac_syntax:application(Op, NewArgs),
		{refac_misc:rewrite(Node, Node1), true};
	    {erlang, apply, 2} ->
		transform_apply_with_arity_of_2(
		  Node, CurModName, FunDefMod, FunName, Arity, Index, Num);
	    {M1, F1, A1} ->
		case lists:keysearch({M1, F1, A1}, 1, apply_style_funs()) of
		  {value, _} ->
		      transform_apply_style_calls(Node, Others);
		  false ->
		      {Node, false}
		end;
	    false -> {Node, false}
	  end;
      _ -> {Node, false}
    end.


transform_apply_with_arity_of_2(Tree, CurModName, FunDefMod, FunName, Arity, Index, Num) ->
    Op = refac_syntax:application_operator(Tree),
    Args = refac_syntax:application_arguments(Tree),
    [Fun, Pars] = Args,
    NewArity = Arity - Num + 1,
    case refac_syntax:type(Fun) of
      implicit_fun ->
	  Name = refac_syntax:implicit_fun_name(Fun),
	  case refac_syntax:type(Name) of
	    arity_qualifier ->
		Body = refac_syntax:arity_qualifier_body(Name),
		Arg = refac_syntax:arity_qualifier_argument(Name),
		F = refac_syntax:atom_value(Body),
		A = refac_syntax:integer_value(Arg),
		case {CurModName, F, A} of
		  {FunDefMod, FunName, Arity} ->
		      case refac_syntax:type(Pars) of
			list ->
			    Pars0 = refac_syntax:list_elements(Pars),
			    case length(Pars0) of
			      Arity ->
				  NewPars = refac_misc:rewrite(
					      Pars, refac_syntax:list(process_pars(Pars0, Index, Num))),
				  NewArg = refac_misc:rewrite(Arg, refac_syntax:integer(NewArity)),
				  NewName = refac_misc:rewrite(
					      Name, refac_syntax:arity_qualifier(Body, NewArg)),
				  NewFun = refac_misc:rewrite(
					     Fun, refac_syntax:implicit_fun(NewName)),
				  Tree1 = refac_syntax:application(Op, [NewFun, NewPars]),
				  {refac_misc:rewrite(Tree, Tree1), true};
			      _ -> {Tree, false}
			    end;
			_ -> {Tree, false}
		      end;
		  _ -> {Tree, false}
		end;
	    module_qualifier ->
		Mod = refac_syntax:module_qualifier_argument(Name),
		MBody = refac_syntax:module_qualifier_body(Name),
		ABody = refac_syntax:arity_qualifier_body(MBody),
		Arg = refac_syntax:arity_qualifier_argument(MBody),
		case refac_syntax:atom_value(Mod) of
		  FunDefMod ->
		      B = refac_syntax:atom_value(ABody),
		      A = refac_syntax:integer_value(Arg),
		      case {B, A} of
			{FunName, Arity} ->
			    case refac_syntax:type(Pars) of
			      list ->
				  Pars0 = refac_syntax:list_elements(Pars),
				  case length(Pars0) of
				    Arity ->
					NewPars = process_pars(Pars0, Index, Num),
					NewArg = refac_misc:rewrite(Arg, refac_syntax:integer(NewArity)),
					NewMBody = refac_misc:rewrite(
						     MBody, refac_syntax:arity_qualifier(ABody, NewArg)),
					NewName = refac_misc:rewrite(
						    Name, refac_syntax:module_qualifier(Mod, NewMBody)),
					Fun1 = refac_misc:rewrite(
						 Fun, refac_syntax:implicit_fun(NewName)),
					Tree1 = refac_misc:rewrite(
						  Tree, refac_syntax:application(Op, [Fun1, NewPars])),
					{Tree1, true};
				    _ -> {Tree, false}
				  end;
			      _ -> {Tree, false}
			    end;
			_ -> {Tree, false}
		      end;
		  _ -> {Tree, false}
		end
	  end;
      _ -> {Tree, false}
    end.

transform_apply_style_calls(Node, {FileName, _ModName, FunDefMod, FunName, Arity,
				   Index, Num, SearchPaths, TabWidth}) ->
    Op = refac_syntax:application_operator(Node),
    Args = refac_syntax:application_arguments(Node),
    [N1, N2, Mod, Fun, Pars] = case length(Args) of
				 5 -> Args;
				 4 -> [none| Args];
				 3 -> [none, none| Args]
			       end,
    Mod1 = try_eval(FileName, Mod, SearchPaths, TabWidth),
    Fun1 = try_eval(FileName, Fun, SearchPaths, TabWidth),
    NewApp = fun () ->
		     Pars0 = refac_syntax:list_elements(Pars),
		     NewPars = refac_misc:rewrite(
				 Pars, refac_syntax:list(process_pars(Pars0, Index, Num))),
		     NewArgs = case length(Args) of
				 5 ->
				     [N1, N2, Mod, Fun, NewPars];
				 4 ->
				     [N2, Mod, Fun, NewPars];
				 3 ->
				     [Mod, Fun, NewPars]
			       end,
		     refac_misc:rewrite(
		       Node, refac_syntax:application(Op, NewArgs))
	     end,
    case Fun1 of
      {value, FunName} ->
	  case Mod1 of
	    {value, FunDefMod} ->
		case refac_syntax:type(Pars) of
		  list ->
		      case refac_syntax:list_length(Pars) of
			Arity ->
			    {NewApp(), true};
			_ -> {Node, false}
		      end;
		  _ -> {Node, false}
		end;
	    _ -> {Node, false}
	  end;
      _ -> {Node, false}
    end.


tuple_pars_in_client_modules(ClientFiles,FunDefMod, FunName,Arity, Index, Num, SearchPaths, TabWidth) ->
    case ClientFiles of
	[] -> [];
	[F | Fs] ->
	    ?wrangler_io("The current file under refactoring is:\n~p\n", [F]),
	    {ok, {AnnAST, ModInfo}}= refac_util:parse_annotate_file(F, true, [], TabWidth),
	    {ok, CurModName} = get_module_name(F, ModInfo),
	    {AnnAST1, Modified} = tuple_pars_in_client_modules_1(
				    F,AnnAST, CurModName,FunDefMod, FunName, Arity, Index, Num, SearchPaths, TabWidth),
	    case Modified of
		true ->
		    [{{F, F}, AnnAST1} | 
		     tuple_pars_in_client_modules(
		       Fs, FunDefMod, FunName,Arity, Index, Num, SearchPaths, TabWidth)];
		false ->
		    tuple_pars_in_client_modules(
		      Fs, FunDefMod, FunName,Arity, Index, Num, SearchPaths, TabWidth)
	    end
    end.


tuple_pars_in_client_modules_1(F, AnnAST, CurModName, FunDefMod, FunName, Arity, Index, Num, SearchPaths, TabWidth) ->
    Args = {F, CurModName, FunDefMod, FunName, Arity, Index, Num, SearchPaths, TabWidth},
    ast_traverse_api:full_tdTP(fun do_tuple_actual_pars/2, AnnAST, Args).
    

    
get_fun_def_info(Node) ->
    As = refac_syntax:get_ann(Node),
    case lists:keysearch(fun_def, 1, As) of
      {value, {fun_def, {Mod, FunName, Arity, _, _}}} ->
	    {Mod, FunName, Arity};
	_ -> false
    end.

get_module_name(FileName, ModInfo) ->				      
    case lists:keysearch(module, 1, ModInfo) of
	{value, {module, ModName}} -> 
	    {ok, ModName};
	false ->
	    case lists:suffix(".hrl", FileName) of
		true -> {ok, none};
		_ -> throw({error, "Wrangler could not infer the name "++
			    "of the module defined in "++ FileName++"."})
	    end
    end.

do_replace_underscore(Tree, _Others) ->
    case refac_syntax:type(Tree) of 
	underscore ->
	    {refac_syntax:atom(undefined), true};
	_ -> {Tree,false}
    end.
