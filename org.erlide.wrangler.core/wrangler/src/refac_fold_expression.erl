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
%% ============================================================================================
%% Refactoring: Fold expression(s) against a function clause definition.

%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%% 
%% =============================================================================================
%% @doc This refactoring replaces instances of the right-hand side of a function clause definition by
%% the corresponding left-hand side with necessary parameter substitutions.

%% <p> To apply this refactoring, move the cursor to the function clause against with expressions 
%% will be folded, then select <em> Fold Expression Against Function</em> from the <em> Refactor</em>
%% menu, after that the refactor will search the current module for expressions which are instances 
%% of the right-hand side of the selected function clause.
%%
%% <p> If no candidate expression has been found, a message will be given, and the refactoring 
%% finishes; otherwise, Wrangler will go through the found candidate expressions one by one asking 
%% the user whether she/he wants to replace the expression with an application of selected function.
%% If the user answers 'yes' to one instance,  that instance will be replaced by function application,
%% otherwise it will remain unchanged.
%%
%% <p> In the case that a candidate expression/expression sequence  need to export some variables with 
%% are used by the following code, that expression/expression sequence will be replaced by a match 
%% expression, whose left-hand side it the exported variable(s), and right-hand side is the function
%% application.
%% 
%% <p> This refactoring does not support folding against function clauses with guard expressions, and 
%% function clauses with complex formal parameters, such as tuples, lists, or records.
%% =============================================================================================
%%@private
-module(refac_fold_expression).

-export([fold_expr_by_loc/6, 
         fold_expr_by_loc_eclipse/5, 
	 fold_expr_1_eclipse/5,
	 do_fold_expression/5,
         do_fold_expression/6,
         fold_expr_by_name/8,
         fold_expr_by_name_eclipse/7]).

-export([fold_expression_1/5]).  %% used by tests.

-include("../include/wrangler_internal.hrl").

%%-spec(fold_expr_by_loc/5::(filename(), integer(), integer(), [dir()], atom(), integer())->
%%	     {ok, [{integer(), integer(), integer(), integer(), syntaxTree(), 
%%		    {filename(), atom(), syntaxTree(), integer()}}], string()}).
fold_expr_by_loc(FileName, Line, Col, SearchPaths, Editor, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:fold_expr_by_loc(~p, ~p,~p,~p,~p, ~p).\n", 
		 [?MODULE, FileName, Line, Col, SearchPaths, Editor, TabWidth]),
    fold_expression(FileName, Line, Col, SearchPaths, Editor, TabWidth).

%%-spec(fold_expr_by_loc_eclipse/5::(filename(), integer(), integer(), [dir()], integer()) ->
%%	     {ok,  {syntaxTree(),[{{{integer(), integer()}, {integer(), integer()}}, syntaxTree(),syntaxTree()}]}}).
fold_expr_by_loc_eclipse(FileName, Line, Col, SearchPaths, TabWidth) ->
    fold_expression(FileName, Line, Col, SearchPaths, eclipse, TabWidth).

fold_expression(FileName, Line, Col, SearchPaths, Editor, TabWidth) ->
     Cmd = "CMD: " ++ atom_to_list(?MODULE) ++ ":fold_expression(" ++ "\"" ++
	     FileName ++ "\", " ++ integer_to_list(Line) ++
	       ", " ++ integer_to_list(Col) ++ ", " ++ "[" ++ 
        wrangler_misc:format_search_paths(SearchPaths) ++ "], "
        ++ atom_to_list(Editor) ++ ", "++integer_to_list(TabWidth) ++ ").",
    {ok, {AnnAST, _Info}} = wrangler_ast_server:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    case pos_to_fun_clause(AnnAST, {Line, Col}) of
	{ok, {Mod, FunName, _Arity, FunClauseDef, _ClauseIndex}} ->
	    side_condition_analysis(FunClauseDef),
	    Candidates = search_candidate_exprs(AnnAST, {Mod, Mod}, FunName, FunClauseDef),
	    fold_expression_0(FileName, Candidates, FunClauseDef, Cmd, Editor, SearchPaths, TabWidth);
	{error, _Reason} -> throw({error, "No function clause has been selected!"})
    end.

fold_expression_0(FileName, Candidates, FunClauseDef, Cmd, Editor, SearchPaths, TabWidth)->
    case Candidates of
	[] when Editor /=composite_emacs->
	    throw({error, "No expressions that are suitable for folding "
		   "against the selected function have been found!"});
	_ -> ok
    end,
    FunClauseDef1 = term_to_list(FunClauseDef),
    Regions = [{SLine, SCol, ELine, ECol, term_to_list(Expr), 
                  term_to_list(NewExp), FunClauseDef1}
                 || {{{SLine, SCol}, {ELine, ECol}}, Expr, NewExp} <- Candidates],
    case Editor of
        emacs ->
            {ok, Regions, Cmd};
        eclipse -> {ok, {FunClauseDef, Candidates}};
        command ->
            do_fold_expression(FileName, Regions, SearchPaths, command, TabWidth, "");
        composite_emacs ->
            {ok, Regions, Cmd}
            %% do_fold_expression(FileName, Regions, SearchPaths, composite_emacs, TabWidth, "")
    end.

fold_expr_by_name(FileName, ModName, FunName, Arity, ClauseIndex,SearchPaths, Editor, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:fold_expression_by_name(~p,~p,~p,~p,~p,~p, ~p, ~p).\n",
                    [?MODULE, FileName, ModName, FunName, Arity, ClauseIndex, SearchPaths, Editor,TabWidth]),
    fold_by_name_par_checking(ModName, FunName, Arity, ClauseIndex),
    fold_expr_by_name_1(FileName, list_to_atom(ModName), list_to_atom(FunName),
		        list_to_integer(Arity), list_to_integer(ClauseIndex),
		        SearchPaths, Editor, TabWidth).

%%-spec(fold_expr_by_name_eclipse/7::(filename(), string(), string(), integer(), integer(), [dir()], integer())
%%				   -> {ok, {syntaxTree(), [{{{integer(), integer()}, {integer(), integer()}}, syntaxTree(),syntaxTree()}]}}).
fold_expr_by_name_eclipse(FileName, ModName, FunName, Arity, ClauseIndex, SearchPaths, TabWidth) ->
    fold_expr_by_name_1(FileName, list_to_atom(ModName), list_to_atom(FunName), Arity,
		        ClauseIndex, SearchPaths, eclipse, TabWidth).

fold_expr_by_name_1(FileName, ModName, FunName, Arity, ClauseIndex, SearchPaths, Editor, TabWidth) ->
    Cmd = "CMD: " ++ atom_to_list(?MODULE) ++ ":fold_expression(" ++ "\"" ++
        FileName ++ "\", " ++ atom_to_list(ModName) ++ ", " ++ atom_to_list(FunName) ++
        ", " ++ integer_to_list(Arity) ++ ", " ++ integer_to_list(ClauseIndex) ++ ", ["
        ++ wrangler_misc:format_search_paths(SearchPaths) ++ "]," ++ integer_to_list(TabWidth) ++ ").",
    {ok, {AnnAST, Info}} = wrangler_ast_server:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    {value, {module, CurrentModName}} = lists:keysearch(module, 1, Info),
    FileName1 = get_file_name(ModName, SearchPaths),
    {ok, {AnnAST1, _Info1}} = wrangler_ast_server:parse_annotate_file(FileName1, true, SearchPaths, TabWidth),
    case api_refac:is_exported({FunName, Arity}, FileName1) of 
        true ->
            case get_fun_clause_def(AnnAST1, FunName, Arity, ClauseIndex) of
                {ok, {Mod, _FunName, _Arity, FunClauseDef}} ->
                    side_condition_analysis(FunClauseDef),
                    Candidates = search_candidate_exprs(AnnAST, {Mod, CurrentModName}, FunName, FunClauseDef),
                    fold_expression_0(FileName, Candidates, FunClauseDef, Cmd, Editor, SearchPaths, TabWidth);
                {error, _Reason} ->
                    throw({error, "The specified funcion clause does not exist!"})
            end;
        false ->
            throw({error, "The function is not exported by its defining module!"})
    end.

get_file_name(ModName, SearchPaths) ->
    Files = wrangler_misc:expand_files(SearchPaths, ".erl"),
    FileNames = lists:filter(fun (F) ->
				     list_to_atom(filename:basename(F, ".erl")) == ModName
			     end, Files),
    case FileNames of
	[] ->
	    throw({error, "Wrangler could not find the file " ++ atom_to_list(ModName) ++ ".erl" ++ 
			    " following Wrangler's SearchPaths!"});
	[FileName] -> FileName;
	_ -> throw({error, "Wrangler found more than one file defining the module, " ++ atom_to_list(ModName)
                    ++ ", folloing the SearchPaths  specified!"})
    end.

%%-spec(fold_expr_1_eclipse/5::(filename(), syntaxTree(),
%%			      [{{{integer(), integer()}, {integer(), integer()}}, syntaxTree()}],
%%			      [dir()], integer()) -> {ok, [{filename(), filename(), string()}]}).
fold_expr_1_eclipse(FileName, FunClauseDef, RangeNewExpList, SearchPaths, TabWidth) ->
    {ok, {AnnAST, _Info}} = wrangler_ast_server:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    Body = wrangler_syntax:clause_body(FunClauseDef),
    AnnAST1 = fold_expression_1_eclipse_1(AnnAST, Body, RangeNewExpList),
    FileContent = wrangler_prettypr:print_ast(wrangler_misc:file_format(FileName), AnnAST1, TabWidth),
    {ok, [{FileName, FileName, FileContent}]}.

fold_expression_1_eclipse_1(AnnAST, _Body, []) ->
    AnnAST;
fold_expression_1_eclipse_1(AnnAST, Body, [Cand| Tail]) ->
    {AnnAST1, _} = api_ast_traverse:stop_tdTP(fun do_replace_expr_with_fun_call/2,
					      AnnAST, {Body, Cand}),
    fold_expression_1_eclipse_1(AnnAST1, Body, Tail).

do_fold_expression(FileName,  CandidatesToFold, SearchPaths, TabWidth, LogMsg) ->
    do_fold_expression(FileName,  CandidatesToFold, SearchPaths, emacs, TabWidth, LogMsg).
do_fold_expression(FileName, CandidatesToFold, SearchPaths, Editor, TabWidth, LogMsg) ->
    {ok, {AnnAST, _Info}} = wrangler_ast_server:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    AnnAST1 = fold_expression_1_1(AnnAST, CandidatesToFold),
    wrangler_write_file:write_refactored_files([{{FileName, FileName}, AnnAST1}], Editor, TabWidth, LogMsg),
    {ok, [FileName]}.

fold_expression_1_1(AnnAST, []) ->
    AnnAST;
fold_expression_1_1(AnnAST, [{StartLine, StartCol, EndLine, EndCol, Expr0, FunApp0, FunClauseDef0}| Tail]) ->
    Expr = list_to_term(Expr0),
    FunClauseDef = list_to_term(FunClauseDef0),
    FunApp = list_to_term(FunApp0),
    Body = wrangler_syntax:clause_body(FunClauseDef),
    {AnnAST1, _} = api_ast_traverse:stop_tdTP(
		     fun do_replace_expr_with_fun_call/2,
		     AnnAST, {Body, {{{StartLine, StartCol}, {EndLine, EndCol}}, Expr, FunApp}}),
    fold_expression_1_1(AnnAST1, Tail).

%% =============================================================================================
%% Side condition analysis.
%% =============================================================================================   

side_condition_analysis(FunClauseDef) ->
    Fun = fun(P) ->
		  Type = wrangler_syntax:type(P),
		  %%TODO:  any others?
		  SimpleTypes =[variable,atom,operator,char,
				integer,string,underscore,nil],
		  lists:member(Type, SimpleTypes)
	  end,
    case wrangler_syntax:clause_guard(FunClauseDef) of
 	none -> 
	    Pats = wrangler_syntax:clause_patterns(FunClauseDef),
	    AllSimplePats = lists:all(Fun, Pats), 
	    case AllSimplePats of 
		true -> ok;
		_ -> throw({error, "Wrangler does not support folding against "
			    "functions with complex parameters."})
	    end;
	_  -> throw({error, "Wrangler does not support "
		     "folding against functions with guards."})
    end.


%% ==========================================================================================================================
%% Replace an expression/expression sequence with a function call/match expression whose right-hand side is the function call.
%% ==========================================================================================================================
 
do_replace_expr_with_fun_call(Tree, {ExprList, {Range, Expr, FunApp}}) ->
    case ExprList of
      [E] ->
	  case wrangler_syntax:type(E) /= match_expr of
	    true ->
		do_replace_expr_with_fun_call_1(Tree, {Range, Expr, FunApp});
	    false ->
		do_replace_expr_with_fun_call_2(Tree, {Range, Expr, FunApp})
	  end;
      _ ->
	  do_replace_expr_with_fun_call_2(Tree, {Range, Expr, FunApp})
    end.

do_replace_expr_with_fun_call_1(Tree, {Range, Expr, NewExp}) ->
    case get_start_end_locations(Tree) of
	Range ->
	    case
		Tree== Expr  %% This is necessary due to the inaccuracy of Range.
		of
		true ->
		    {wrangler_misc:rewrite_with_wrapper(Tree, NewExp), true};
		false ->
		    {Tree, false}
	    end;
	_ -> {Tree, false}
    end.

do_replace_expr_with_fun_call_2(Tree, {{StartLoc, EndLoc}, _Expr, NewExp}) ->
    case wrangler_syntax:type(Tree) of
	clause ->
	    Exprs = wrangler_syntax:clause_body(Tree),
	    {NewBody, Modified} = do_replace_expr(Exprs, {StartLoc, EndLoc}, NewExp),
	    Pats = wrangler_syntax:clause_patterns(Tree),
	    G = wrangler_syntax:clause_guard(Tree),
	    {wrangler_misc:rewrite(Tree, wrangler_syntax:clause(Pats, G, NewBody)), Modified};
	block_expr ->
	    Exprs = wrangler_syntax:block_expr_body(Tree),
	    {NewBody, Modified} = do_replace_expr(Exprs, {StartLoc, EndLoc}, NewExp),
	    {wrangler_misc:rewrite(Tree, wrangler_syntax:block_expr(NewBody)), Modified};
	try_expr ->
	    Exprs = wrangler_syntax:try_expr_body(Tree),
	    {NewBody, Modified} = do_replace_expr(Exprs, {StartLoc, EndLoc}, NewExp),
	    Cs = wrangler_syntax:try_expr_clauses(Tree),
	    Handlers = wrangler_syntax:try_expr_handlers(Tree),
	    After = wrangler_syntax:try_expr_after(Tree),
	    Tree1 = wrangler_misc:rewrite(Tree, wrangler_syntax:try_expr(NewBody, Cs, Handlers, After)),
	    {Tree1, Modified};
	_ -> {Tree, false}
    end.

do_replace_expr(Exprs, {StartLoc, EndLoc}, NewExp) ->
    {Exprs1, Exprs2} =
	lists:splitwith(
	  fun (E) ->
		  element(1, get_start_end_locations(E)) =/= StartLoc
	  end, Exprs),
    case Exprs2 of
	[] -> {Exprs, false};
	_ ->
	    {Exprs21, Exprs22} =
		lists:splitwith(
		  fun (E) ->
			  element(2, get_start_end_locations(E)) =/= EndLoc
		  end, Exprs2),
	    case Exprs22 of
		[] -> {Exprs, false};  %% THIS SHOULD NOT HAPPEN.
		_ ->
                    NewExp1 = wrangler_misc:rewrite_with_wrapper(Exprs21 ++ [hd(Exprs22)], NewExp),
                    {Exprs1 ++ [NewExp1| tl(Exprs22)], true}
	    end
    end.

%% =============================================================================================
%% Search expression/expression sequence with are instances of of the selected function clause.
%% ============================================================================================= 

search_candidate_exprs(AnnAST, {FunDefMod, CurrentMod}, FunName,FunClauseDef) ->
    Body = wrangler_syntax:clause_body(FunClauseDef),
    Pats = wrangler_syntax:clause_patterns(FunClauseDef),
    Fun = fun({Range, Expr, Subst, none}) -> 
		  {Range, Expr, make_fun_call({FunDefMod, CurrentMod}, FunName, Pats, Subst)};
	     ({Range, Expr, Subst, Pattern}) -> 
		  {Range, Expr, make_match_expr({FunDefMod, CurrentMod}, FunName, Pats, Subst, Pattern)}
	  end,
    Res = do_search_candidate_exprs(AnnAST,Body),
    [Fun(R)|| R <- Res].

do_search_candidate_exprs(AnnAST, ExpList) ->
    case ExpList of
      [E] ->
	    case wrangler_syntax:type(E) of
		match_expr ->
		    search_for_expr_list(AnnAST, ExpList);
		_ ->
		    search_for_single_not_match_expr(AnnAST, E)
	    end;
	_ -> search_for_expr_list(AnnAST, ExpList)
    end.

search_for_single_not_match_expr(AnnAST, Exp) ->
    OpRanges = collect_op_ranges(AnnAST),
    Fun = fun (T, S) ->
		  case api_refac:is_expr(T) of
		      true->
			  case T =/= Exp of
			      true ->
				  R = get_start_end_locations(T),
				  case lists:member(R, OpRanges) of
				      false ->
                                          case wrangler_unification:expr_unification_extended(Exp, T) of
					      {true, Subst} ->
						  S ++ [{get_start_end_locations(T), T, Subst, none}];
					      _ ->
						  S
					  end;
				      _ -> S
				  end;
			      _ -> S
			  end;
		      false -> S
		  end
	  end,
    api_ast_traverse:fold(Fun, [], AnnAST).

collect_op_ranges(Tree) ->
    F = fun (T, S) ->
		case wrangler_syntax:type(T) of
		    application ->
			Operator = wrangler_syntax:application_operator(T),
			Range = get_start_end_locations(Operator),
			S ++ [Range];
		    _ -> S
		end
	end,
    api_ast_traverse:fold(F, [], Tree).

search_for_expr_list(AnnAST, ExpList) ->
    Fun = fun (T, S) ->
		  case wrangler_syntax:type(T) of
		      clause ->
			  Exprs = wrangler_syntax:clause_body(T),
			  CandidateExprs = get_candidate_exprs(ExpList, Exprs),
			  S ++ CandidateExprs;
		      block_expr ->
			  Exprs = wrangler_syntax:block_expr_body(T),
			  CandidateExprs = get_candidate_exprs(ExpList, Exprs),
			  S ++ CandidateExprs;
		      try_expr ->
			  Exprs = wrangler_syntax:try_expr_body(T),
			  CandidateExprs = get_candidate_exprs(ExpList, Exprs),
			  S ++ CandidateExprs;
		      _ -> S
		  end
	  end,
    api_ast_traverse:fold(Fun, [], AnnAST).

get_candidate_exprs(FoldFunBodyExprList, CurExprList)->
    Len = length(FoldFunBodyExprList),
    SubExprs = sublists(CurExprList, Len),
    Res=[check_a_candidate_expr_list(FoldFunBodyExprList, CurExprList, Es)
	 ||Es<-SubExprs, Es/=FoldFunBodyExprList],
    [R||R<-lists:append(Res), R/=false].


check_a_candidate_expr_list(FoldFunBodyExprList, CurExprList, SubExprs) ->
    LastBodyExpr = lists:last(FoldFunBodyExprList),
    case wrangler_syntax:type(LastBodyExpr) of
      match_expr ->
	  check_expr_list_ends_with_match(FoldFunBodyExprList, CurExprList, SubExprs);
      _ ->
	  check_expr_list_not_ends_with_match(FoldFunBodyExprList, CurExprList, SubExprs)
    end.

check_expr_list_ends_with_match(FoldFunBodyExprList, CurExprList, SubExprs) ->
    case wrangler_unification:expr_unification_extended(FoldFunBodyExprList, SubExprs) of
	{true, Subst} ->
	    VarsToExport = vars_to_export(CurExprList, SubExprs),
	    case VarsToExport of
		[] ->
		    [{get_start_end_locations(SubExprs), SubExprs, Subst, none}];
		_ ->
		    [Last| _Es] = lists:reverse(SubExprs),
		    VarsToExportByLastExpr = vars_to_export(CurExprList, [Last]),
		    case VarsToExport -- VarsToExportByLastExpr of
			[] ->
			    Pat = wrangler_syntax:match_expr_pattern(Last),
			    [{get_start_end_locations(SubExprs), SubExprs, Subst, Pat}];
			_ ->
			    [false]
		    end
	    end;
	_ ->
	    check_expr_list_minus_last_expr(FoldFunBodyExprList, CurExprList, SubExprs)
    end.

check_expr_list_not_ends_with_match(FoldFunBodyExprList, CurExprList, SubExprs) ->
    [Last| Es] = lists:reverse(SubExprs),
    VarsToExport = vars_to_export(CurExprList, SubExprs),
    case wrangler_syntax:type(Last) of
	match_expr ->
	    VarsToExportByLastExpr = vars_to_export(CurExprList, [Last]),
	    case VarsToExport -- VarsToExportByLastExpr of
		[] ->
		    Body = wrangler_syntax:match_expr_body(Last),
		    Pats = wrangler_syntax:match_expr_pattern(Last),
		    SubExprs1 = lists:reverse([Body| Es]),
		    case wrangler_unification:expr_unification_extended(FoldFunBodyExprList, SubExprs1) of
			{true, Subst} ->
			    [{get_start_end_locations(SubExprs), SubExprs, Subst, Pats}];
			_ ->
			    check_expr_list_minus_last_expr(FoldFunBodyExprList,CurExprList, SubExprs)
		    end;
		_ ->
		    check_expr_list_minus_last_expr(FoldFunBodyExprList, CurExprList,SubExprs)
	    end;
	_ ->
	    case wrangler_unification:expr_unification_extended(FoldFunBodyExprList, SubExprs) of
		{true, Subst} when VarsToExport == [] ->
		    [{get_start_end_locations(SubExprs), SubExprs, Subst, none}];
		{true, Subst} ->
		    FreeVars = element(1, lists:unzip(api_refac:free_vars(Last))),
		    case is_simple_expr(Last) andalso VarsToExport--FreeVars==[] of
			true ->
			    [{get_start_end_locations(SubExprs), SubExprs, Subst, Last}];
			false ->
			    [false]
		    end;
		_ ->
		    check_expr_list_minus_last_expr(FoldFunBodyExprList, CurExprList,SubExprs)
	    end
    end.
    
check_expr_list_not_ends_with_match_2(FoldFunBodyExprList, CurExprList, Es) ->
    VarsToExport = vars_to_export(CurExprList, Es),
    Len = length(FoldFunBodyExprList),
    Res = wrangler_unification:expr_unification_extended(lists:sublist(FoldFunBodyExprList, Len - 1), Es),
    case Res of
      {true, Subst} ->
	    LastExp = lists:last(FoldFunBodyExprList),
	    case make_pattern(LastExp, VarsToExport, Subst) of
		false -> 
		    false;
		Pattern ->
		    {get_start_end_locations(Es), Es, Subst, Pattern}
	    end;
	_ -> false
    end.

check_expr_list_minus_last_expr(FoldFunBodyExprList, CurExprList, SubExprs) ->
    LastBodyExpr = lists:last(FoldFunBodyExprList),
    case is_simple_expr(LastBodyExpr) of
	true ->
	    [check_expr_list_not_ends_with_match_2(FoldFunBodyExprList,CurExprList,SubEs)
	     || SubEs <- sublists(SubExprs,length(SubExprs)-1)];
	_ -> [false]
    end.



%% ==================================================
%% Variables defined and exported by an expression list in
%% the context of a larger expression list containing it.                      
%% ==================================================
vars_to_export(WholeExpList, SubExpList) ->
    AllVars = lists:usort(
		lists:flatmap(
		  fun (E) -> wrangler_misc:collect_var_source_def_pos_info(E) end,
		  WholeExpList)),
    SubExpListBdVars = lists:flatmap(
			 fun (E) ->
				 As = wrangler_syntax:get_ann(E),
				 case lists:keysearch(bound, 1, As) of
				     {value, {bound, BdVars1}} -> BdVars1;
				     _ -> []
				 end
			 end, SubExpList),
    SubExpListBdVarPoses = [Pos || {_Var, Pos} <- SubExpListBdVars],
    SubExpListEndPos = element(2, get_start_end_locations(lists:last(SubExpList))),
    lists:usort([V || {V, SourcePos, DefPos} <- AllVars,
		      SourcePos > SubExpListEndPos,
		      lists:subtract(DefPos, SubExpListBdVarPoses) == []]).


%% ==================================================
%% Order variables in the pattern                     
%% ==================================================
make_pattern(Expr, VarsToExport, Subst) ->
    ExprElems = case wrangler_syntax:type(Expr) of
		  tuple -> wrangler_syntax:tuple_elements(Expr);
		  _ -> [Expr]
		end,
    Fun = fun (Elem) ->
		  case wrangler_syntax:type(Elem) of
		    variable ->
			VarName = wrangler_syntax:variable_name(Elem),
			case lists:keysearch(VarName, 1, Subst) of
			  false ->
			      '_';  %% This variable is not needed.
			  {value, {VarName, SubstVar}} ->
			      SubstVarName = wrangler_syntax:variable_name(SubstVar),
			      case lists:member(SubstVarName, VarsToExport) of
				true ->
				    wrangler_syntax:variable_name(SubstVar);
				_ -> '_'  %% variable not needed.
			      end
			end;
		    _ -> '_'
		  end
	  end,
    Pats = lists:map(Fun, ExprElems),
    case VarsToExport -- Pats of
      [] ->
	  case [V || V <- Pats, V /= '_'] of
	    [] -> none;
	    _ ->
		PatVars = [wrangler_syntax:variable(V) || V <- Pats],
		case PatVars of
		  [P] -> P;
		  _ -> wrangler_syntax:tuple(PatVars)
		end
	  end;
      _ ->
	  false
    end.

%% ==================================================
%% Compose a function application expression.                      
%% ==================================================
make_fun_call({FunDefMod, CurrentMod}, FunName, Pats, Subst) ->
    Fun = fun (P) ->
		  case wrangler_syntax:type(P) of
		      variable ->
			  PName = wrangler_syntax:variable_name(P),
			  case lists:keysearch(PName, 1, Subst) of
			      {value, {PName, Par}} -> Par;
			      _ -> wrangler_syntax:atom(undefined)
			  end;
		      underscore ->
			  wrangler_syntax:atom(undefined);
		      _ -> P
		  end
	  end,
    Pars = lists:map(Fun, Pats),
    Op = case FunDefMod == CurrentMod of
	     true -> wrangler_syntax:atom(FunName);
	     _ -> wrangler_syntax:module_qualifier(
		       wrangler_syntax:atom(FunDefMod), wrangler_syntax:atom(FunName))
	 end,
    wrangler_syntax:application(Op, [wrangler_misc:reset_attrs(P) || P <- Pars]).
  
%% =============================================================================
%% Compose a match expression of a function application when the pattern is none.                      
%% =============================================================================
make_match_expr({FunDefMod, CurrentMod}, FunName, Pats, Subst,Pattern) ->
    FunCall = make_fun_call({FunDefMod, CurrentMod},FunName, Pats, Subst),
    case Pattern of 
	none -> 
	    FunCall;
	_ -> 
	    wrangler_syntax:match_expr(Pattern, FunCall)
    end.

%% ==================================================
%% Name to function clause.                      
%% ==================================================
get_fun_clause_def(Node, FunName, Arity, ClauseIndex) ->
    case
      api_ast_traverse:once_tdTU(fun get_fun_def_1/2, Node, {FunName, Arity, ClauseIndex})
	of
      {_, false} -> {error, none};
      {R, true} -> {ok, R}
    end.

get_fun_def_1(Node, {FunName, Arity, ClauseIndex}) ->
    case wrangler_syntax:type(Node) of
	function ->
	    As = wrangler_syntax:get_ann(Node),
	    case lists:keysearch(fun_def, 1, As) of 
		{value, {fun_def, {Mod, FunName, Arity, _Pos1, _Pos2}}} ->
		    C = lists:nth(ClauseIndex, wrangler_syntax:function_clauses(Node)),
		    {{Mod,FunName, Arity, C}, true};
		_ -> {[], false}
	    end;
	_ ->
	    {[], false}
    end.

%% ==================================================
%% position to function clause                      
%% ==================================================
pos_to_fun_clause(Node, Pos) ->
    case
      api_ast_traverse:once_tdTU(fun pos_to_fun_clause_1/2, Node, Pos)
	of
      {_, false} -> {error, none};
      {R, true} -> {ok, R}
    end.

pos_to_fun_clause_1(Node, Pos) ->
    case wrangler_syntax:type(Node) of
      function ->
	  {S, E} = get_start_end_locations(Node),
	  if (S =< Pos) and (Pos =< E) ->
		 Cs = wrangler_syntax:function_clauses(Node),
		 NoOfCs = length(Cs),
		 [{Index, C}] = [{I1, C1}
				 || {I1, C1} <- lists:zip(lists:seq(1, NoOfCs), Cs),
				    {S1, E1} <- [get_start_end_locations(C1)], S1 =< Pos,
				    Pos =< E1],
		 As = wrangler_syntax:get_ann(Node),
		 case lists:keysearch(fun_def, 1, As) of
		   {value, {fun_def, {Mod, FunName, Arity, _P1, _P2}}} ->
		       {{Mod, FunName, Arity, C, Index}, true};
		   _ -> {[], false}
		 end;
	     true -> {[], false}
	  end;
      _ ->
	  {[], false}
    end.

%% ==================================================
%% sanity checking of parameter inputs                      
%% ==================================================
fold_by_name_par_checking(ModName, FunName, Arity, ClauseIndex) ->
    case ModName of
	[] -> throw({error, "Invalid module name!"});
      _ -> ok
    end, 
    case FunName of
      [] -> throw({error, "Invalid function name!"});
      _ -> ok
    end,
    try
      list_to_integer(Arity)
    of
      _ -> ok
    catch
      _:_ -> throw({error, "Invalid arity!"})
    end,
    try
      list_to_integer(ClauseIndex)
    of
      _ ->
	  ok
    catch
      _:_ -> throw({error, "Invalid function clause index!"})
    end.


%% ==================================================
%%  Some utility functions                      
%% ==================================================
sublists(List, Len) ->
    L = length(List),
    case Len > length(List) of
	true ->
	    [];
	_ -> [lists:sublist(List, Index, Len)
	      || Index<-lists:seq(1, L-Len+1)]
    end.

is_simple_expr(Expr) ->
    case wrangler_syntax:is_literal(Expr) of
	true -> true;
	false -> 
	    case wrangler_syntax:type(Expr) of
		variable -> true;
		tuple ->
		    lists:all(fun (E) ->
				      wrangler_syntax:type(E) == variable orelse
					  wrangler_syntax:is_literal(E)
			      end,
			      wrangler_syntax:tuple_elements(Expr));
		_ -> false
	    end
    end.

get_start_end_locations(E) when is_list(E) ->
    {SLoc1, _ELoc1} = get_start_end_locations(hd(E)),
    {_SLoc2, ELoc2} = get_start_end_locations(lists:last(E)),
    {SLoc1, ELoc2};
get_start_end_locations(E) ->
    wrangler_misc:start_end_loc(E).

term_to_list(Term) ->
    binary_to_list(term_to_binary(Term)).

list_to_term(List)->
    binary_to_term(list_to_binary(List)).

%%-spec(fold_expression_1/5::(filename(), atom(), integer(), [dir()], integer()) -> 
%%	     {syntaxTree(), moduleInfo()} | {error, string()}).
fold_expression_1(FileName, FunName, Arity, SearchPaths, TabWidth) ->
    {ok, {AnnAST, Info}} = wrangler_ast_server:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    {value, {module, ModName}} = lists:keysearch(module, 1, Info),
    FunClauseDef = name_to_fun_clause(AnnAST, FunName, Arity),
    Candidates = search_candidate_exprs(AnnAST, {ModName, ModName}, FunName, FunClauseDef),
    Body = wrangler_syntax:clause_body(FunClauseDef),
    AnnAST1 = fold_expression_1_eclipse_1(AnnAST, Body, Candidates),
    {AnnAST1, Info}.

name_to_fun_clause(AnnAST, FunName, Arity) ->
    Forms = wrangler_syntax:form_list_elements(AnnAST),
    F = fun (Form) ->
		case wrangler_syntax:type(Form) of
		    function ->
			FunName1 = wrangler_syntax:data(wrangler_syntax:function_name(Form)),
			Arity1 = wrangler_syntax:function_arity(Form),
			FunName1 == FunName andalso Arity == Arity1;
		    _ -> false
		end
	end,
    Fun = hd(lists:filter(F, Forms)),
    hd(wrangler_syntax:function_clauses(Fun)).




