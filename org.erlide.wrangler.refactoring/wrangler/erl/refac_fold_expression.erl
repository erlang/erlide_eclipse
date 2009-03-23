%% ============================================================================================
%% Refactoring: Fold expression(s) against a function clause definition.
%%
%% Copyright (C) 2006-2009  Huiqing Li, Simon Thompson

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
-module(refac_fold_expression).

-export([fold_expr_by_loc/5, fold_expression_1/9,
	 fold_expr_by_loc_eclipse/5, fold_expression_1_eclipse/5,
	 fold_expression_2_eclipse/7, fold_expression_1/5,
	 fold_expr_by_name/7, fold_expr_by_name_eclipse/7,
	 cursor_at_fun_clause/5]).

-export([expr_unification/2, fold_expression/6]).
-include("../include/wrangler.hrl").
%% =============================================================================================
%% @spec fold_expression(FileName::filename(), Line::integer(), Col::integer())-> term()
%% =============================================================================================        

-spec(fold_expr_by_loc/5::(filename(), integer(), integer(), [dir()], integer())->
			 {ok, [{integer(), integer(), integer(), integer(), syntaxTree(), {filename(), atom(), syntaxTree(), integer()}}]}
							 | {error, string()}).

fold_expr_by_loc(FileName, Line, Col, SearchPaths, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:fold_expression(~p, ~p,~p,~p).\n", [?MODULE, FileName, Line, Col, TabWidth]),
    fold_expression(FileName, Line, Col, SearchPaths, TabWidth, emacs).

-spec(fold_expr_by_loc_eclipse/5::(filename(), integer(), integer(), [dir()], integer()) -> {ok,  {syntaxTree(),
										  [{{{integer(), integer()}, {integer(), integer()}}, syntaxTree()}]}}
										     | {error, string()}).

fold_expr_by_loc_eclipse(FileName, Line, Col, SearchPaths, TabWidth) ->
    fold_expression(FileName, Line, Col, SearchPaths, TabWidth, eclipse).

fold_expression(FileName, Line, Col, SearchPaths, TabWidth, Editor) ->
    {ok, {AnnAST, Info}} =refac_util:parse_annotate_file(FileName,true, SearchPaths, TabWidth),
    {value, {module, CurrentModName}} = lists:keysearch(module, 1, Info),
    case pos_to_fun_clause(AnnAST, {Line, Col}) of 
	{ok, {Mod, FunName, _Arity, FunClauseDef, ClauseIndex}} ->
	    case side_condition_analysis(FunClauseDef) of 
		ok ->			    
		    Candidates = search_candidate_exprs(AnnAST, {Mod, Mod}, FunName, FunClauseDef),
		    case Candidates of 
			[] -> {error, "No expressions that are suitable for folding against the selected function have been found!"};	
			_ -> case Editor of 
				 emacs ->
				     FunClauseDef1 = binary_to_list(term_to_binary(FunClauseDef)),
				     Regions = lists:map(fun({{{StartLine, StartCol}, {EndLine, EndCol}},NewExp}) ->
								 NewExp1 = binary_to_list(term_to_binary(NewExp)),
								 {StartLine, StartCol, EndLine,EndCol, NewExp1, {FileName, CurrentModName, FunClauseDef1, ClauseIndex}} end, 
							 Candidates),
				     {ok, Regions};
				 eclipse ->  {ok, {FunClauseDef, Candidates}}
			     end			      
		    end;				 
		{error, Reason} -> {error, Reason}
	    end;
	{error, _} ->
	    {error, "You have not selected a function definition."}
    end.


-spec(fold_expr_by_name/7::(filename(), string(), string(), string(), string(), [dir()], integer()) ->
	     {ok, [{integer(), integer(), integer(), integer(), syntaxTree(), {filename(), atom(), syntaxTree(), integer()}}]}
		 | {error, string()}).

fold_expr_by_name(FileName, ModName, FunName, Arity, ClauseIndex, SearchPaths, TabWidth) ->
    case ModName of 
	[] -> {error, "Invalid module name!"};
	_  -> case FunName of 
		  [] -> {error, "Invalid function name!"};
		  _ -> case (Arity==[]) orelse (list_to_integer(Arity) <0) of   
			   true -> {error, "Invalid arity!"};
			   _ -> case (ClauseIndex==[]) orelse (list_to_integer(ClauseIndex) <1) of 
				    true -> {error, "Invalid function clause index!"};
				    _ ->				
					fold_expr_by_name(FileName, list_to_atom(ModName), list_to_atom(FunName), 
							  list_to_integer(Arity), list_to_integer(ClauseIndex), SearchPaths,  TabWidth, emacs)
				end
		       end
	      end
    end.
 
-spec(fold_expr_by_name_eclipse/7::(filename(), string(), string(), string(), string(), [dir()], integer()) ->
	     {ok, [{{{integer(), integer()}, {integer(), integer()}}, syntaxTree()}]} 
		 | {error, string()}).

fold_expr_by_name_eclipse(FileName, ModName, FunName, Arity, ClauseIndex, SearchPaths, TabWidth) ->
	case ModName of 
	    [] -> {error, "Invalid module name!"};
	    _  -> case FunName of 
		      [] -> {error, "Invalid function name!"};
		      _ -> case (Arity==[]) orelse (list_to_integer(Arity) <0) of   
			       true -> {error, "Invalid arity!"};
			       _ -> case (ClauseIndex==[]) orelse (list_to_integer(ClauseIndex) <1) of 
					true -> {error, "Invalid function clause index!"};
					_ ->				
					    fold_expr_by_name(FileName, list_to_atom(ModName), list_to_atom(FunName), 
							      list_to_integer(Arity), list_to_integer(ClauseIndex), SearchPaths, TabWidth,  eclipse)
				    end
			   end
		  end
	end.
   

fold_expr_by_name(FileName, ModName, FunName, Arity, ClauseIndex, SearchPaths, TabWidth,  Editor) ->
    ?wrangler_io("\nCMD: ~p:fold_expression(~p,~p,~p,~p,~p,~p).\n", [?MODULE, FileName, ModName, FunName, Arity, ClauseIndex, TabWidth]),
    {ok, {AnnAST, Info}} = refac_util:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    {value, {module, CurrentModName}} = lists:keysearch(module, 1, Info),
    Files = refac_util:expand_files(SearchPaths, ".erl"),
    FileNames = lists:filter(fun (F) -> list_to_atom(filename:basename(F, ".erl")) == ModName end, Files),
    case FileNames of
      [] ->
	  {error, "Wrangler could not find the file " ++ atom_to_list(ModName) ++ ".erl" ++ " following Wrangler's SearchPaths!"};
      _ ->
	  FileName1 = hd(FileNames),
	  {ok, {AnnAST1, _Info1}} = refac_util:parse_annotate_file(FileName1, true, SearchPaths, TabWidth),
	  case get_fun_clause_def(AnnAST1, FunName, Arity, ClauseIndex) of
	    {ok, {Mod, _FunName, _Arity, FunClauseDef}} ->
		case side_condition_analysis(FunClauseDef) of
		  ok ->
		      Candidates = search_candidate_exprs(AnnAST, {Mod, CurrentModName}, FunName, FunClauseDef),
		      case Candidates of
			[] ->
			    {error,
			     "No expressions that are suitable for folding against the selected function have been found!"};
			_ ->
			    Regions = case Editor of
					emacs ->
					    FunClauseDef1 = binary_to_list(term_to_binary(FunClauseDef)),
					    lists:map(fun ({{{StartLine, StartCol}, {EndLine, EndCol}}, NewExp}) ->
							      NewExp1 = binary_to_list(term_to_binary(NewExp)),
							      {StartLine, StartCol, EndLine, EndCol, NewExp1, {FileName1, Mod, FunClauseDef1, ClauseIndex}}
						      end,
						      Candidates);
					eclipse -> Candidates
				      end,
			    {ok, Regions}
		      end;
		  {error, Reason} -> {error, Reason}
		end;
	    {error, _Reason} ->
		{error, "The specified funcion clause does not exist!"}
	  end
    end.

    
%% =============================================================================================
%% @spec fold_expression_1(FileName::filename(), StartLine::integer(), StartCol::integer(),
%%                        (EndLine::integer(), EndCol::integer(), NewExp::term(),
%%                        {FunClauseDef, ClauseIndex}::{term(), integer()) -> term()
%% =============================================================================================  

-spec(fold_expression_1_eclipse/5::(filename(), syntaxTree(), [{{{integer(), integer()}, {integer(), integer()}}, syntaxTree}], [dir()], integer()) ->
	     {ok, [{filename(), filename(), string()}]}).
fold_expression_1_eclipse(FileName, FunClauseDef, RangeNewExpList, SearchPaths, TabWidth) ->   %% RangeNewExpList [{{{StartLine, EndCol}, {EndLine, EndCol}}, NewExp}]
    {ok, {AnnAST, _Info}} = refac_util:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    Body = refac_syntax:clause_body(FunClauseDef),
    AnnAST1= fold_expression_1_eclipse_1(AnnAST, Body, RangeNewExpList),
    Res = [{FileName, FileName, refac_prettypr:print_ast(refac_util:file_format(FileName),AnnAST1)}],
    {ok, Res}.


fold_expression_1_eclipse_1(AnnAST, _Body,  []) -> 
    AnnAST;
fold_expression_1_eclipse_1(AnnAST, Body, [{{StartLoc, EndLoc}, Exp}|Tail]) ->
    {AnnAST1,_} = refac_util:stop_tdTP(fun do_replace_expr_with_fun_call_eclipse/2, AnnAST, {Body, {{StartLoc, EndLoc}, Exp}}),
    fold_expression_1_eclipse_1(AnnAST1, Body, Tail).
    
    

-spec(fold_expression_1/9::(filename(), integer(), integer(), integer(), integer(), syntaxTree(), {filename(),atom(), syntaxTree(), integer()}, [dir()], integer()) ->
	     {ok, [{integer(), integer(), integer(), integer(), syntaxTree(), {filename(), atom(), syntaxTree(), integer()}}]}).
fold_expression_1(FileName, StartLine, StartCol, EndLine, EndCol, NewExp0, {FunDefFileName, FunDefMod, FunClauseDef0, ClauseIndex}, SearchPaths, TabWidth) -> 
    NewExp= binary_to_term(list_to_binary(NewExp0)),
    FunClauseDef = binary_to_term(list_to_binary(FunClauseDef0)),
    {ok, {AnnAST, Info}} = refac_util:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    {value, {module, CurrentMod}} = lists:keysearch(module, 1, Info),
    FunCall = case refac_syntax:type(NewExp) of 
		  application -> NewExp;
		  match_expr -> refac_syntax:match_expr_body(NewExp)
	      end,
    Op = refac_syntax:application_operator(FunCall),
    FunName = case refac_syntax:type(Op) of 
		  atom ->refac_syntax:atom_value(Op);
		  module_qualifier ->
		       refac_syntax:atom_value(refac_syntax:module_qualifier_body(Op))
	      end,
    Arity = length(refac_syntax:application_arguments(FunCall)),		   
    Body = refac_syntax:clause_body(FunClauseDef),
    {AnnAST1,_} = refac_util:stop_tdTP(fun do_replace_expr_with_fun_call/2, AnnAST, {Body, NewExp, {{StartLine, StartCol}, {EndLine, EndCol}}}),
    refac_util:write_refactored_files([{{FileName, FileName}, AnnAST1}]),  
    {ok, {AnnAST2, _Info2}} = refac_util:parse_annotate_file(FileName,true, SearchPaths, TabWidth),
    {ok, {AnnAST3, _Info3}} = refac_util:parse_annotate_file(FunDefFileName,true, SearchPaths, TabWidth),
    case get_fun_clause_def(AnnAST3, FunName, Arity, ClauseIndex) of 
	{ok, {_Mod, _FunName, _Arity, FunClauseDef1}} ->
	    Candidates = search_candidate_exprs(AnnAST2, {FunDefMod, CurrentMod}, FunName, FunClauseDef1),
	    FunClauseDef2 = binary_to_list(term_to_binary(FunClauseDef1)),
	    Regions = [{StartLine1, StartCol1, EndLine1, EndCol1, binary_to_list(term_to_binary(FunCall1)), 
			{FunDefFileName, FunDefMod, FunClauseDef2, ClauseIndex}} 
		       || {{{StartLine1, StartCol1}, {EndLine1, EndCol1}}, FunCall1}<-Candidates,
			  StartLine1 >= StartLine],
	    {ok,  Regions};
	{error, _Reason} ->
	    {error, "You have not selected a function definition."}  %% THIS SHOULD NOT HAPPEN.
    end.
  


-spec(fold_expression_2_eclipse/7::(filename(), atom(),integer(), integer(), integer(), [dir()], integer()) -> 
	     {ok, [{integer(), integer(), integer(), integer(), syntaxTree(), {syntaxTree(), integer()}}]}
             | {error, string()}).
fold_expression_2_eclipse(FileName, FunName, Arity, ClauseIndex, StartLine, SearchPaths, TabWidth ) ->
    {ok, {AnnAST2, _Info1}} = refac_util:parse_annotate_file(FileName,true, SearchPaths, TabWidth),
    case get_fun_clause_def(AnnAST2, FunName, Arity, ClauseIndex) of 
	{ok, {Mod, _FunName, _Arity, FunClauseDef1}} ->
	    Candidates = search_candidate_exprs(AnnAST2, {Mod, Mod}, FunName, FunClauseDef1),
	    Regions = [{StartLine1, StartCol1, EndLine1, EndCol1, FunCall1, {FunClauseDef1, ClauseIndex}} 
		       || {{{StartLine1, StartCol1}, {EndLine1, EndCol1}}, FunCall1}<-Candidates,
			  StartLine1 >= StartLine],
	    {ok, Regions};
	{error, _Reason} ->
	    {error, "You have not selected a function definition."}  %% THIS SHOULD NOT HAPPEN.
    end.



%% =============================================================================================
%% Side condition analysis.
%% =============================================================================================   
    
side_condition_analysis(FunClauseDef) ->
    G = refac_syntax:clause_guard(FunClauseDef),
    case G of 
	none -> 
	    Pats = refac_syntax:clause_patterns(FunClauseDef),
	    AllSimplePats = lists:all(fun(P) -> case refac_syntax:type(P) of 
						    variable -> true;
						    atom -> true;
						    operator  -> true;
						    char -> true;
						    integer -> true;
						    string -> true;
						    underscore -> true;
						    nil -> true;
						    _ -> false
						end
				      end, Pats), %% TODO: OTHER SIMPLE PARAMETERS SHOULD ALSO Be ALLOWED.
	    case AllSimplePats of 
		true -> ok;
		_ -> {error, "Wrangler does not support folding against functions with complex parameters."}
	    end;
	_  -> {error, "Wrangler does not support folding against functions with guards."}
    end.


%% ==========================================================================================================================
%% Replace an expression/expression sequence with a function call/match expression whose right-hand side is the function call.
%% ==========================================================================================================================
 

do_replace_expr_with_fun_call_eclipse(Tree, {Expr, {Range, NewExp}})->
    case length(Expr) of 
	1 -> do_replace_expr_with_fun_call_eclipse_1(Tree, {Range, NewExp});
	_  -> do_replace_expr_with_fun_call_eclipse_2(Tree, {Range, NewExp})
    end.
						   


do_replace_expr_with_fun_call_eclipse_1(Tree, {Range, NewExp}) ->
    case refac_util:get_range(Tree) of 
 	Range ->
	     {NewExp, true};
 	_  -> {Tree, false}
     end.
 	
do_replace_expr_with_fun_call_eclipse_2(Tree, {{StartLoc, EndLoc}, NewExp}) ->
      case refac_syntax:type(Tree) of
	clause ->
	    Exprs = refac_syntax:clause_body(Tree),
	    {Exprs1, Exprs2} = lists:splitwith(fun(E) -> element(1,refac_util:get_range(E)) =/= StartLoc end, Exprs),
	    {NewBody, Modified} = case Exprs2 of 
				      [] -> {Exprs, false};
				      _ -> {_Exprs21, Exprs22} = lists:splitwith(fun(E) -> element(2, refac_util:get_range(E)) =/= EndLoc end, Exprs),
					   case Exprs22 of 
					       [] -> {Exprs, false};  %% THIS SHOULD NOT HAPPEN.
					       _ -> {Exprs1++[NewExp|tl(Exprs22)], true}
					   end
				  end,
	    Pats = refac_syntax:clause_patterns(Tree),
	    G = refac_syntax:clause_guard(Tree),
	    {refac_syntax:copy_pos(Tree, refac_syntax:copy_attrs(Tree, refac_syntax:clause(Pats, G, NewBody))), Modified};
	block_expr ->
	    Exprs = refac_syntax:block_expr_body(Tree),
	    {Exprs1, Exprs2} = lists:splitwith(fun(E) -> element(1,refac_util:get_range(E)) =/= StartLoc end, Exprs),
	    {NewBody, Modified} = case Exprs2 of 
				      [] -> {Exprs, false};
				      _ -> {_Exprs21, Exprs22} = lists:splitwith(fun(E) -> element(2, refac_util:get_range(E)) =/= EndLoc end, Exprs),
					   case Exprs22 of 
					       [] -> {Exprs, false};  %% THIS SHOULD NOT HAPPEN.
					       _ -> {Exprs1++[NewExp|tl(Exprs22)], true}
					   end
				  end,
	    {refac_syntax:copy_pos(Tree, refac_syntax:copy_attrs(Tree, refac_syntax:block_expr(NewBody))), Modified};	 
	_  -> {Tree, false}
    end.
    

do_replace_expr_with_fun_call(Tree, {Expr,NewExp, Range})->
    case length(Expr) of 
	1 -> do_replace_expr_with_fun_call_1(Tree, {NewExp, Range});
	_  -> do_replace_expr_with_fun_call_2(Tree, {NewExp, Range})
    end.
	    

do_replace_expr_with_fun_call_1(Tree, {NewExp, Range}) ->
     case refac_util:get_range(Tree) of 
 	Range ->
	    {NewExp, true};
 	_  -> {Tree, false}
     end.
    
do_replace_expr_with_fun_call_2(Tree, {NewExp, {StartLoc, EndLoc}}) -> 
   case refac_syntax:type(Tree) of
	clause ->
	    Exprs = refac_syntax:clause_body(Tree),
	    {Exprs1, Exprs2} = lists:splitwith(fun(E) -> element(1,refac_util:get_range(E)) =/= StartLoc end, Exprs),
	    {NewBody, Modified} = case Exprs2 of 
				      [] -> {Exprs, false};
				      _ -> {_Exprs21, Exprs22} = lists:splitwith(fun(E) -> element(2, refac_util:get_range(E)) =/= EndLoc end, Exprs),
					   case Exprs22 of 
					       [] -> {Exprs, false};  %% THIS SHOULD NOT HAPPEN.
					       _ -> {Exprs1++[NewExp|tl(Exprs22)], true}
					   end
				  end,
	    Pats = refac_syntax:clause_patterns(Tree),
	    G = refac_syntax:clause_guard(Tree),
	    {refac_syntax:copy_pos(Tree, refac_syntax:copy_attrs(Tree, refac_syntax:clause(Pats, G, NewBody))), Modified};
	block_expr ->
	    Exprs = refac_syntax:block_expr_body(Tree),
	    {Exprs1, Exprs2} = lists:splitwith(fun(E) -> element(1,refac_util:get_range(E)) =/= StartLoc end, Exprs),
	    {NewBody, Modified} = case Exprs2 of 
				      [] -> {Exprs, false};
				      _ -> {_Exprs21, Exprs22} = lists:splitwith(fun(E) -> element(2, refac_util:get_range(E)) =/= EndLoc end, Exprs),
					   case Exprs22 of 
					       [] -> {Exprs, false};  %% THIS SHOULD NOT HAPPEN.
					       _ -> {Exprs1++[NewExp|tl(Exprs22)], true}
					   end
				  end,
	    {refac_syntax:copy_pos(Tree, refac_syntax:copy_attrs(Tree, refac_syntax:block_expr(NewBody))), Modified};	 
	_  -> {Tree, false}
    end.

%% =============================================================================================
%% Search expression/expression sequence with are instances of of the selected function clause.
%% ============================================================================================= 
search_candidate_exprs(AnnAST, {FunDefMod, CurrentMod}, FunName,FunClauseDef) ->
    Body = refac_syntax:clause_body(FunClauseDef),
    Pats = refac_syntax:clause_patterns(FunClauseDef),
    Res = do_search_candidate_exprs(AnnAST,Body),
    lists:map(fun(R) -> case R of 
			    {Range, Subst} -> {Range, make_fun_call({FunDefMod, CurrentMod}, FunName, Pats, Subst)};
			    {Range, Subst, VarsToExport} -> {Range, make_match_expr({FunDefMod, CurrentMod}, FunName, Pats, Subst, VarsToExport)}
			end
	      end, Res).


do_search_candidate_exprs(AnnAST, ExpList) ->
    case length(ExpList) of 
	 1 ->
	    do_search_candidate_exprs_1(AnnAST, hd(ExpList));
	_  ->do_search_candidate_exprs_2(AnnAST, ExpList)
    end.


do_search_candidate_exprs_1(AnnAST, Exp) ->
    PrimExprRanges=collect_prime_expr_ranges(AnnAST),
    Fun = fun(T,S) ->
		 As = refac_syntax:get_ann(T),
		 case lists:keysearch(category, 1, As) of 
		     {value, {category, expression}} ->
			 case T=/=Exp of 
			     true -> 
				 R = refac_util:get_range(T),
				 case lists:member(R, PrimExprRanges) of
				     false ->  case expr_unification(Exp, T) of 
						   {true, Subst} -> 
						       S++[{refac_util:get_range(T), Subst}];
						   _ -> S
					       end;
				     _ -> S
				 end;
			     _ -> S
			 end;
		      _  -> S
		 end		     
	 end,
    refac_syntax_lib:fold(Fun, [], AnnAST).
 

do_search_candidate_exprs_2(AnnAST, ExpList) ->
    Len = length(ExpList),
    LastExp = lists:last(ExpList),
    HasExportExp = case refac_syntax:type(LastExp) of 
		      variable -> true;
		       tuple -> lists:all(fun(E) -> refac_syntax:type(E) == variable end, 
					  refac_syntax:tuple_elements(LastExp));
		       _  -> false
		   end,
    
    Fun = fun(T, S) ->
		  case refac_syntax:type(T) of 
		      clause ->
			  Exprs = refac_syntax:clause_body(T),
			  SubExprs = sublists(Exprs, Len),
			  CandidateExprs1 = 
			      lists:map(fun(E) -> case ExpList =/= E of 
   						      true ->case expr_unification(ExpList, E) of 
   								 {true,Subst} -> {StartLoc1, _EndLoc1} = refac_util:get_range(hd(E)),
   										 {_StartLoc2, EndLoc2} = refac_util:get_range(lists:last(E)),
   										 {{StartLoc1, EndLoc2}, Subst};
   								 _ ->
								     false
   							     end;
   						      _ -> false
   						  end							       
   					end, SubExprs),
 			  CandidateExprs2 = 
			      case HasExportExp of 
				  true ->
				      SubExprs1 = sublists(Exprs, Len-1),
				      lists:map(fun(E) -> case hd(ExpList) =/= hd(E) of 
							      true -> VarsToExport = vars_to_export_1(Exprs, E),
								      Res =  expr_unification(lists:sublist(ExpList, Len-1), E),
								      case Res of 
									  {true, Subst} ->
									      case reorder_vars_to_export(LastExp, VarsToExport, Subst) of 
										  {true, VarsToExport1} ->
										      {StartLoc1, _EndLoc1} = refac_util:get_range(hd(E)),
										      {_StartLoc2, EndLoc2} = refac_util:get_range(lists:last(E)),
										      {{StartLoc1, EndLoc2}, Subst, VarsToExport1};
										  _ -> false
									      end;
									  _ -> false
								      end;
							      _ -> false
							  end
						end, SubExprs1);
				  _ -> []
			      end,
			  S ++ lists:filter(fun(C) -> C =/= false end, CandidateExprs1++CandidateExprs2);
		      block_expr ->
			  Exprs = refac_syntax:block_expr_body(T),
			  SubExprs = sublists(Exprs, Len),
			  CandidateExprs = lists:map(fun(E) -> case ExpList =/= E of 
								   true ->case expr_unification(ExpList, E) of 
									      {true, Subst} -> {StartLoc1, _EndLoc1} = refac_util:get_range(hd(E)),
											       {_StartLoc2, EndLoc2} = refac_util:get_range(lists:last(E)),
											       {{StartLoc1, EndLoc2}, Subst};
									      _ -> false
									  end;
								   _ -> false
							       end
						     end, SubExprs),
			  S ++ lists:filter(fun(C) -> C =/= false end, CandidateExprs);
		      _  -> S
		  end
	  end,
     refac_syntax_lib:fold(Fun,[], AnnAST).
    

-spec(expr_unification/2::(syntaxTree(), syntaxTree()) ->
	     {true, [{atom(), syntaxTree()}]} | false).
expr_unification(Exp1, Exp2) ->
    Res1 = case {is_list(Exp1), is_list(Exp2)} of 
	       {true, true} ->   %% both are list of expressions
		   case length(Exp1) == length(Exp2) of
		       true -> Res = lists:map(fun({E1,E2}) ->			      
						       expr_unification(E1,E2)						
					       end, lists:zip(Exp1, Exp2)),
			       Unifiable = lists:all(fun(E) -> case E of 
								   {true, _} -> true;
								   _ -> false
							       end
						     end, Res),
			       Substs = lists:usort(lists:flatmap(fun(E) -> case E of 
										{true,S} -> S;
										_ -> []
									    end
								  end,Res)),
			       case Unifiable of 
				   true -> {true, Substs};
				   _ -> false
			       end;
		       _ -> false
		   end;
	       {false, false} ->  %% both are single expressions.
		   T1 = refac_syntax:type(Exp1),
		   T2 = refac_syntax:type(Exp2),
		   case T1 == T2 of 
		       true -> 
			   case T1 of 
			       variable -> 
				   case lists:keysearch(category, 1, refac_syntax:get_ann(Exp1)) of 
				       {value, {category, macro_name}} ->
					   case lists:keysearch(category, 1, refac_syntax:get_ann(Exp2)) of 
					       {value, {category, macro_name}} ->
						   case refac_syntax:variable_name(Exp1) == refac_syntax:variable_name(Exp2) of 
						       true ->
							   {true, []};
					       _ ->false
						   end;
					       _ -> false
					   end;
				       _ -> {true, [{refac_syntax:variable_name(Exp1), set_default_ann(Exp2)}]} 
				   end;
			       atom -> case refac_syntax:atom_value(Exp1) == refac_syntax:atom_value(Exp2) of 
					   true -> {true, []};
					   _ -> false
				       end;
			       operator -> case refac_syntax:atom_value(Exp1) == refac_syntax:atom_value(Exp2) of
					       true -> {true, []};
					       _ -> false
					   end;
			       char -> case refac_syntax:char_value(Exp1) == refac_syntax:char_value(Exp2) of 
					   true -> {true, []};
					   _ -> false
				       end;
			       integer -> case refac_syntax:integer_value(Exp1) ==refac_syntax:integer_value(Exp2) of 
					      true -> {true, []};
					      _ -> false
					  end;
			       string -> case refac_syntax:string_value(Exp1) == refac_syntax:string_value(Exp2) of 
					     true -> {true, []};
					     _ -> false
					 end;
			       float -> case refac_syntax:float_value(Exp1) == refac_syntax:float_value(Exp2) of 
					    true -> {true, []}
					end;
			       underscore -> {true, []};
			       nil -> {true, []};
			       _ -> 
				   SubTrees1 = erl_syntax:subtrees(Exp1),
				   SubTrees2 = erl_syntax:subtrees(Exp2),
				   case length(SubTrees1) == length(SubTrees2) of 
				       true -> 
					   expr_unification(SubTrees1, SubTrees2);				    
				       _ -> false
				   end 
			   end;
		       _ -> case T1 of 
				variable -> case T2 of 
						match_expr -> false;  %% ANY OTHER CASES?
						_ -> 
						    case lists:keysearch(category, 1, refac_syntax:get_ann(Exp2)) of 
							{value, {category, application_op}} ->
							    case lists:keysearch(fun_def, 1, refac_syntax:get_ann(Exp2)) of 
								{value, {fun_def, {_M, _N, A, _P1, _P2}}} ->
								    {true, [{refac_syntax:variable_name(Exp1), 
									     set_default_ann(refac_syntax:implicit_fun(Exp2, refac_syntax:integer(A)))}]};
								_ -> false
							    end;
							_ -> {true, [{refac_syntax:variable_name(Exp1), set_default_ann(Exp2)}]}
						    end
					    end;
				_ -> false
			    end
		   end;
	       {true, false} -> %% Exp1 is a list, but Exp2 is not.
		   false;
	       {false, true} ->  %% Exp1 is a single expression, but Exp2 is not.
		   false      %% an actual parameter cannot be a list of expressions.
	   end,
    case Res1 of 
	{true, Subst} ->
	    Subst1 = lists:usort(lists:map(fun({E1,E2}) -> 
						   {E1, refac_prettypr:format(E2)} end, Subst)),
	    Len1 = length(lists:usort(lists:map(fun({E1,_E2}) -> E1 end, Subst1))),
	    case Len1 == length(Subst1) of 
		true -> {true, Subst};
		_ -> false
	    end;
	_ -> false
    end.
	    

%% =============================================================================================
%% Some Utility Functions.
%% =============================================================================================   

make_fun_call({FunDefMod, CurrentMod}, FunName, Pats, Subst) -> 
    Pars = lists:map(fun(P) -> case refac_syntax:type(P) of 
				   variable -> PName = refac_syntax:variable_name(P), 
					       case lists:keysearch(PName, 1, Subst) of 
						   {value, {PName, Par}} -> Par;
						   _ -> refac_syntax:atom(undefined)
					       end;
				   underscore -> 
				       refac_syntax:atom(undefined);
				   _  -> P
			       end
		     end, Pats),
    Op = case FunDefMod == CurrentMod of 
	     true -> refac_syntax:atom(FunName);
	     _ -> refac_syntax:module_qualifier(refac_syntax:atom(FunDefMod), refac_syntax:atom(FunName))
	 end,
    FunCall = refac_syntax:application(Op, Pars),
    FunCall.

make_match_expr({FunDefMod, CurrentMod}, FunName, Pats, Subst, VarsToExport) ->
    FunCall = make_fun_call({FunDefMod, CurrentMod},FunName, Pats, Subst),
    case VarsToExport of 
	[] ->
	    FunCall;
	[V] -> P = refac_syntax:variable(V),
	       refac_syntax:match_expr(P, FunCall);
	[_V|_VS] -> P =refac_syntax:tuple([refac_syntax:variable(V) || V <-VarsToExport]),
		    refac_syntax:match_expr(P, FunCall)
    end.
	    
 		
 
sublists(List, Len) ->
    L = length(List),
    case Len > length(List) of
	true ->
	    [];
	_ -> [lists:sublist(List, Index, Len)|| Index<-lists:seq(1, L-Len+1)]
    end.


collect_prime_expr_ranges(Tree) ->
     F= fun(T, S) ->
 		   case refac_syntax:type(T) of 
 		       application ->
			   Operator = refac_syntax:application_operator(T),
			   Range = refac_util:get_range(Operator),
		           S++[Range];
		       _ -> S
		   end
	end,
    refac_syntax_lib:fold(F, [], Tree).


set_default_ann(Node) ->
    refac_syntax:set_pos(refac_syntax:remove_comments(refac_syntax:set_ann(Node, [])), {0,0}).


vars_to_export_1(WholeExpList, SubExpList) ->
    F1= fun(T, S) ->
	       case refac_syntax:type(T) of 
		   variable ->
		       SourcePos = refac_syntax:get_pos(T),
		       case lists:keysearch(def, 1, refac_syntax:get_ann(T)) of 
			   {value, {def, DefinePos}} ->
			       VarName = refac_syntax:variable_name(T),
			       S ++ [{VarName, SourcePos, DefinePos}];
			   _ ->
			       S
		       end;
		   _  -> S
	       end
	end,
    AllVars = lists:usort(lists:flatmap(fun(E)->refac_syntax_lib:fold(F1, [], E) end,  WholeExpList)),
    SubExpListBdVars = lists:flatmap(fun(E) -> As = refac_syntax:get_ann(E),
							    case lists:keysearch(bound,1, As) of
								{value, {bound, BdVars1}} -> BdVars1;
								_ -> []
							    end
						  end, SubExpList),
    SubExpListBdVarPoses = lists:map(fun({_Var, Pos}) -> Pos end, SubExpListBdVars),
    SubExpListEndPos = element(2, refac_util:get_range(lists:last(SubExpList))),
    VarsToExport = lists:usort([V || {V, SourcePos, DefPos} <- AllVars,
			      SourcePos > SubExpListEndPos,
			      lists:subtract(DefPos, SubExpListBdVarPoses) == []]),
    VarsToExport.


reorder_vars_to_export(LastExp, VarsToExport, Subst) ->
    VarsOfLastExp = case refac_syntax:type(LastExp) of 
		      variable -> [LastExp];
		      tuple -> refac_syntax:tuple_elements(LastExp);
		      _  -> []
		  end,
    ReOrderedExportList = lists:map(fun(V) ->
					    VarName = refac_syntax:variable_name(V),
					    case lists:keysearch(VarName,1, Subst) of 
					     false ->
						  '_';
					     {value, {VarName, SubstVar}} ->
						    case lists:member(refac_syntax:variable_name(SubstVar), VarsToExport) of 
							true ->
							    refac_syntax:variable_name(SubstVar);
							_ -> '_'
						    end
					 end
				 end, VarsOfLastExp),
    case VarsToExport -- ReOrderedExportList of 
	[] ->
	    {true, ReOrderedExportList};
	UnexportedVars ->
	    Subst1 = lists:flatmap(fun({S1,S2}) -> case refac_syntax:type(S2) of 
						      variable -> [{S1, refac_syntax:variable_name(S2)}];
						       _ -> []
						   end
				   end, Subst),
	    Vars = lists:flatmap(fun(V) -> case lists:keysearch(V, 2, Subst1) of 
					       false -> [];
					       {value, {Var, V}} -> [Var]
					   end
				 end, UnexportedVars),
	    ?wrangler_io("Warning: some expressions could have been folded if the function also exported the following variable(s):~p\n", Vars),
	    false
    end.
    
get_fun_clause_def(Node, FunName, Arity, ClauseIndex) ->
    case refac_util:once_tdTU(fun get_fun_def_1/2, Node, {FunName, Arity, ClauseIndex}) of
	{_, false} -> {error, none};
	{R, true} -> {ok, R}
    end.

get_fun_def_1(Node, {FunName, Arity, ClauseIndex}) ->
    case refac_syntax:type(Node) of 
	function ->
	    As = refac_syntax:get_ann(Node),
	    case lists:keysearch(fun_def, 1, As) of 
		{value, {fun_def, {Mod, FunName, Arity, _Pos1, _Pos2}}} ->
		    C = lists:nth(ClauseIndex, refac_syntax:function_clauses(Node)),
		    {{Mod,FunName, Arity, C}, true};
		_ -> {[], false}
	    end;
	_ ->
	    {[], false}
    end.


-spec(cursor_at_fun_clause/5::(filename(), integer(), integer(), [dir()], integer()) ->
	     true |false).
cursor_at_fun_clause(FileName, Line, Col, SearchPaths, TabWidth) ->    
    {ok, {AnnAST, _Info}} =refac_util:parse_annotate_file(FileName,true, SearchPaths, TabWidth),
    case pos_to_fun_clause(AnnAST, {Line, Col}) of 
	{ok, {_Mod, _FunName, _Arity, _FunClauseDef, _ClauseIndex}} -> true;
	_  -> false
    end.
    
	
pos_to_fun_clause(Node, Pos) ->
    case refac_util:once_tdTU(fun pos_to_fun_clause_1/2, Node, Pos) of
	{_, false} -> {error, none};
	{R, true} -> {ok, R}
    end.

pos_to_fun_clause_1(Node, Pos) ->
    case refac_syntax:type(Node) of 
	function ->
	    {S, E} = refac_util:get_range(Node),
	    if (S=<Pos) and (Pos =< E) ->
		    Cs  = refac_syntax:function_clauses(Node),
		    NoOfCs = length(Cs),
		    [{Index, C}] = [{I1,C1} || {I1, C1} <- lists:zip(lists:seq(1,NoOfCs), Cs), {S1, E1} <- [refac_util:get_range(C1)],S1=<Pos, Pos=<E1],
		     As = refac_syntax:get_ann(Node),
		    case lists:keysearch(fun_def, 1, As) of 
			{value, {fun_def, {Mod, FunName, Arity, _Pos1, _Pos2}}} ->
			    {{Mod,FunName, Arity, C, Index}, true};
			_ -> {[], false}
		    end;
	       true -> {[], false}
	    end;
	_ ->
	    {[], false}
    end.


-spec(fold_expression_1/5::(filename(), atom(), integer(), [dir()], integer()) -> {syntaxTree(), moduleInfo()} | {error, string()}).
fold_expression_1(FileName, FunName, Arity, SearchPaths, TabWidth) ->
    {ok, {AnnAST, Info}} =refac_util:parse_annotate_file(FileName,true, SearchPaths, TabWidth),
    {value, {module, ModName}} = lists:keysearch(module, 1, Info),
    FunClauseDef= name_to_fun_clause(AnnAST, FunName, Arity),
    Candidates = search_candidate_exprs(AnnAST, {ModName, ModName}, FunName, FunClauseDef),
    Body = refac_syntax:clause_body(FunClauseDef),
    AnnAST1= fold_expression_1_eclipse_1(AnnAST, Body, Candidates),
    {AnnAST1, Info}.

name_to_fun_clause(AnnAST, FunName, Arity) ->
    Forms = refac_syntax:form_list_elements(AnnAST),
    F = fun (Form) ->
		case refac_syntax:type(Form) of
		  function ->
		      FunName1 = refac_syntax:data(refac_syntax:function_name(Form)),
		      Arity1 = refac_syntax:function_arity(Form),
		      FunName1 == FunName andalso Arity == Arity1;
		  _ -> false
		end
	end,
    Fun = hd(lists:filter(F, Forms)),
    hd(refac_syntax:function_clauses(Fun)).
