-module(anti_unification).

-export([anti_unification/2,anti_unification_with_score/3,generate_anti_unifier/3,
	generate_anti_unifier_and_num_of_new_vars/3,subst_sanity_check/2]).

-include("../include/wrangler.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
%%                                                                      %%
%%  Try to find the anti-unifier to two expression/expression sequences %%
%%                                                                      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%-spec anti_unification([syntaxTree()]|syntaxTree(), [syntaxTree()]|syntaxTree()) ->
%%			      [{syntaxTree(), syntaxTree()}] |none.
anti_unification(Expr1, Expr2) ->
    try do_anti_unification(Expr1, Expr2) of 
	Res ->
	    Res
    catch
	throw:_E2 ->
	    none
    end.
       
	
do_anti_unification(Exprs1, Exprs2) when is_list(Exprs1) andalso is_list(Exprs2) ->
    case length(Exprs1) == length(Exprs2) of
	true ->
	    case Exprs1 of
		[] ->
		    [];
		_ ->
		    ZippedExprs = lists:zip(Exprs1, Exprs2),
		    Res = [do_anti_unification(E1, E2) || {E1, E2} <- ZippedExprs],
		    lists:append(Res)
	    end;
	false ->
	    ?debug("Does not anti-unify 1:\n~p\n", [{Exprs1, Exprs2}]),
	    throw(not_anti_unifiable)
    end;
do_anti_unification(Expr1, _Expr2) when is_list(Expr1) ->
    ?debug("Does not anti-unify 2:n~p\n", [{Expr1, _Expr2}]),
    throw(not_anti_unifiable);
do_anti_unification(_Expr1, Expr2) when is_list(Expr2) ->
    ?debug("Does not anti-unify 3:\n~p\n", [{_Expr1, Expr2}]),
    throw(not_anti_unifiable);
do_anti_unification(Expr1, Expr2) ->
    T1 = refac_syntax:type(Expr1),
    T2 = refac_syntax:type(Expr2),
    case T1 == T2 of
	false -> 
	    anti_unfication_different_type(Expr1, Expr2);
	true ->
	    anti_unification_same_type(Expr1, Expr2)
    end.

anti_unfication_different_type(Expr1, Expr2) ->
    case refac_code_search_utils:generalisable(Expr1) 
	andalso refac_code_search_utils:generalisable(Expr2) of
	true ->
	  [{Expr1, Expr2}];
      false ->
	    ?debug("Does not anti-unify 4:\n~p\n", [{Expr1, Expr2}]),
	    throw(not_anti_unifiable)
    end.

anti_unification_same_type(Expr1, Expr2) ->
    case refac_syntax:is_literal(Expr1) andalso refac_syntax:is_literal(Expr2) of
      true ->
	  case refac_syntax:concrete(Expr1) == refac_syntax:concrete(Expr2) of
	    true ->
		case refac_syntax:type(Expr1) of
		  atom ->
			Ann1 = refac_syntax:get_ann(Expr1),
			Ann2 = refac_syntax:get_ann(Expr2),
			case lists:keysearch(fun_def, 1, Ann1) of
			    {value, {fun_def, {M, F, A, _, _}}} ->
				case lists:keysearch(fun_def, 1, Ann2) of
				    {value, {fun_def, {M1, F1, A1, _, _}}} ->
					case {M, F, A} == {M1, F1, A1} of
					    true -> [];
					    false ->
						[{Expr1, Expr2}]
					end;
				    false ->
					[{Expr1, Expr2}]
				end;
			    false ->
				case lists:keysearch(fun_def, 1, Ann2) of
				    {value, _} ->
					[{Expr1, Expr2}];
				    false ->
					[]
				end
			end;
		    _ ->
			[]
		end;
	    _ ->
		case generalisable(Expr1, Expr2) of
		  true ->
		      case lists:keysearch(fun_def, 1, refac_syntax:get_ann(Expr1)) of
			{value, {fun_def, {M, F, A, _, _}}} ->
			    case lists:keysearch(fun_def, 1, refac_syntax:get_ann(Expr2)) of
			      {value, {fun_def, {M, F, A, _, _}}} ->
				  [];
			      _ -> [{Expr1, Expr2}]
			    end;
			_ ->
			    [{Expr1, Expr2}]
		      end;
		  false -> throw(non_unificable)
		end
	  end;
	_ ->
	    case refac_syntax:type(Expr1) of
		variable ->
		    case {is_macro_name(Expr1), is_macro_name(Expr2)} of
			{true, true} ->
			    case has_same_name(Expr1, Expr2) of
				true -> [];
				false -> throw(not_anti_unifiable)
			    end;
			{false, false} -> [{Expr1, Expr2}];
			_ ->
			    ?debug("Does not anti-unify 5:\n~p\n", [{Expr1, Expr2}]),
			    throw(not_anti_unifiable)
		end;
	    operator -> case refac_syntax:operator_name(Expr1) == refac_syntax:operator_name(Expr2) of
			  true -> [];
			  false ->
			      ?debug("Does not anti-unify 6:\n~p\n", [{Expr1, Expr2}]),
			      throw(not_anti_unifiable)
			end;
	      underscore -> [];
	      macro -> 
		  MacroName1 = refac_syntax:macro_name(Expr1),
		  MacroName2 = refac_syntax:macro_name(Expr2),
		  case not has_same_name(MacroName1, MacroName2) of
		      true ->
			  case  generalisable(Expr1, Expr2) of
			      true ->
				  [{Expr1, Expr2}];
			      false ->
				  anti_unification_same_type_1(Expr1, Expr2)
			  end;
		      _ -> anti_unification_same_type_1(Expr1, Expr2)
		     end;
	      _ -> 
		  case refac_syntax:is_leaf(Expr1) of
		      true -> case generalisable(Expr1, Expr2) of
				  true ->
				      [{Expr1, Expr2}];
				  _ ->
				      ?debug("Does not anti-unify 7:\n~p\n", [{Expr1, Expr2}]),
				      throw(not_anti_unifiable)
			      end;
		      false -> anti_unification_same_type_1(Expr1, Expr2)
		  end
	  end
    end.

anti_unification_same_type_1(E1, E2) ->
    SubExprs1 = refac_syntax:subtrees(E1),
    SubExprs2 = refac_syntax:subtrees(E2),
    try
      do_anti_unification(SubExprs1, SubExprs2)
    of
      Subst -> Subst
    catch
      throw:_ -> case generalisable(E1, E2) of
		   true ->
		       [{E1, E2}];
		   _ -> throw(non_unificable)
		 end
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
%%                                                                      %%
%%  Try to find an anti-unifier satisfying a similarity score           %%
%%                                                                      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%-spec anti_unification_with_score([syntaxTree()]|syntaxTree(), [syntaxTree()]|syntaxTree(), float()) ->
%%					 [{syntaxTree(), syntaxTree()}]|none.
anti_unification_with_score(Expr1, Expr2, SimiScore) ->
    try
	do_anti_unification(Expr1, Expr2)
    of
	SubSt ->
	    {SubExprs1, SubExprs2} = lists:unzip(SubSt),
	    Score1 = simi_score(Expr1, SubExprs1),
	    Score2 = simi_score(Expr2, SubExprs2),
	    case Score1 >= SimiScore andalso Score2 >= SimiScore of
		true ->
		    case subst_sanity_check(Expr1, SubSt) of
			false ->
			    none;
			_ ->
			    SubSt
		    end;
		_ -> none
	    end
    catch
	throw:_ -> none
    end.


simi_score(Expr, SubExprs) ->
    case no_of_nodes(Expr) of
      0 -> 0;
      ExprSize ->
	  NonVarExprs = [E || E <- SubExprs, refac_syntax:type(E) =/= variable],
	  NoOfNewVars = length(NonVarExprs),
	  Res = 1 - (no_of_nodes(SubExprs) - length(SubExprs)
		     + NoOfNewVars * (NoOfNewVars + 1) / 2) / ExprSize,
	  %% Res =1 -((no_of_nodes(SubExprs)-length(SubExprs))/ExprSize),
	  Res
    end.

subst_sanity_check(Expr1, SubSt) ->
    BVs = refac_misc:get_bound_vars(Expr1),
    F = fun ({E1, E2}) ->
		case refac_syntax:type(E1) of
		  variable ->
		      case is_macro_name(E1) of
			true ->
			    false;
			_ ->
			    {value, {def, DefPos}} = lists:keysearch(def, 1, refac_syntax:get_ann(E1)),
			    %% local vars should have the same substitute.
			    not lists:any(fun ({E11, E21}) ->
						  refac_syntax:type(E11) == variable andalso
						    {value, {def, DefPos}} == lists:keysearch(def, 1, refac_syntax:get_ann(E11))
						      andalso
						      refac_prettypr:format(reset_attrs(E2))
							=/= refac_prettypr:format(reset_attrs(E21))
					  end, SubSt)
		      end;
		  _ ->
		      %% the expression to be replaced should not contain local variables.
		      BVs -- refac_misc:get_free_vars(E1) == BVs
		end
	end,
    lists:all(F, SubSt).
    


no_of_nodes(Nodes) when is_list(Nodes) ->
    lists:sum([no_of_nodes(N)||N<-Nodes]);
no_of_nodes(Node) ->
    case refac_syntax:is_leaf(Node) of
	true -> 1;
	_ ->
	    lists:sum([no_of_nodes(T)||
			  T<-refac_syntax:subtrees(Node)])
    end.

    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
%%                                                                      %%
%%  Generate anti_unifier                                               %%
%%                                                                      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%TODO: refactor the third argument.

%%-spec generate_anti_unifier([syntaxTree()], [{syntaxTree(), syntaxTree()}], 
%%			     {[atom()], [atom()]}) -> syntaxTree().

generate_anti_unifier(Exprs, Subst, ExportVars) ->
    {AU, _} = generate_anti_unifier_and_num_of_new_vars(Exprs, Subst, ExportVars),
    AU.

generate_anti_unifier_and_num_of_new_vars(Exprs, Subst, ExportVars) ->
    FunName = refac_syntax:atom(new_fun),
    BVs = refac_misc:get_bound_vars(Exprs),
    FVs = lists:ukeysort(2, refac_misc:get_free_vars(Exprs)),
    {NewExprs, NewExportVars} = generalise_expr_1(Exprs, Subst, ExportVars),
    NewExprs1 = case NewExportVars of
		    [] -> NewExprs;
		    [V]-> NewExprs++[refac_syntax:variable(V)];
		    _ -> LastExpr =refac_syntax:tuple([refac_syntax:variable(V) 
						       || V <- NewExportVars]),
			 NewExprs++[LastExpr]
		end,
    %% BVs: [{name, pos}]. FVs: [{name, pos}]
    Pars = refac_misc:collect_var_names(NewExprs) -- element(1, lists:unzip(BVs)),
    FVPars = [V || {V, _} <- FVs, lists:member(V, Pars)],
    NewVarPars = refac_misc:remove_duplicates(Pars -- FVPars),
    Pars1 = [refac_syntax:variable(V) || V <- FVPars] ++ 
	[refac_syntax:variable(V) || V <- NewVarPars],
    C = refac_syntax:clause(refac_misc:remove_duplicates(Pars1), none, NewExprs1),
    {refac_syntax:function(FunName, [C]), length(NewVarPars)}.

generalise_expr_1(Exprs, Subst, ExportVars) when is_list(Exprs) ->
    BlockExpr = refac_syntax:block_expr(Exprs),
    FVs = refac_misc:get_free_vars(Exprs),
    {E, NewExportVars} = generalise_expr_2(BlockExpr, Subst, FVs, ExportVars),
    {refac_syntax:block_expr_body(E), NewExportVars};
generalise_expr_1(Expr, Subst, ExportVars) ->
    FVs = refac_misc:get_free_vars(Expr),
    {E, NewExportVars} = generalise_expr_2(Expr, Subst, FVs, ExportVars),
    {[E], NewExportVars}.

generalise_expr_2(Expr, Subst, ExprFreeVars, {ExportVars1, ExportVars2}) ->
    case lists:all(fun (S) -> S == [] end, Subst) of
	true -> 
	    %% Nothing to generalise; therefore no new 
	    %% variable is needed.
	    {Expr, ExportVars1};  
	_ ->
	    %% New variables needed.
	    UsedVarNames = sets:from_list(refac_misc:collect_var_names(Expr)),
	    Pid = refac_code_search_utils:start_counter_process(UsedVarNames),
	    ExportVars3 = [E || E <- ExportVars2, refac_syntax:type(E) =/= variable],
	    ExprNewVarPairs=generate_new_var_names(Subst,Pid),
	    {Expr1, _} = ast_traverse_api:stop_tdTP(fun do_replace_expr_with_var_1/2, Expr,
						    {ExprNewVarPairs, Subst,ExprFreeVars, Pid, ExportVars3}),
	    NewVarsToExport = refac_code_search_utils:get_new_export_vars(Pid),
	    refac_code_search_utils:stop_counter_process(Pid),
	    VarsToExport1 = ExportVars1 ++
		[refac_syntax:variable_name(E)
		 || E <- ExportVars2, refac_syntax:type(E) == variable] ++
		NewVarsToExport,
	    VarsToExport = refac_misc:remove_duplicates(VarsToExport1),
	    {Expr1, VarsToExport}
    end.

do_replace_expr_with_var_1(Node, {ExprNewVarPairs, SubSt, ExprFreeVars, Pid, ExportExprs}) ->
    F = fun (S, Name) -> Es = [refac_prettypr:format(E2)
			       || {E1, E2} <- S,
				  refac_syntax:type(E1) == variable,
				  refac_syntax:variable_name(E1) == Name],
			 length(sets:to_list(sets:from_list(Es))) == 1
	end,
    ExprsToReplace = [E||{E,_}<-ExprNewVarPairs],
    case lists:member(Node, ExprsToReplace) of
	true ->
	    FVs = refac_misc:get_free_vars(Node),
	    case refac_syntax:type(Node) of
		variable ->
		    case FVs of
			[] -> {Node, false};
			_ -> case FVs -- ExprFreeVars of
				 [] ->
				     Name = refac_syntax:variable_name(Node),
				     case lists:all(fun (S) -> F(S, Name) end, SubSt) of
					 true -> {Node, false};
					 _ -> NewVar = get_new_var_name(Node, ExprNewVarPairs, Pid),
					      case lists:member(Node, ExportExprs) of
						  true ->
						      refac_code_search_utils:add_new_export_var(Pid, NewVar);
						  _ -> ok
					      end,
					      {refac_misc:rewrite(Node, refac_syntax:variable(NewVar)), true}
				     end;
				 _ -> {Node, false}
			     end
		    end;
		_ ->
		    NewVar = get_new_var_name(Node, ExprNewVarPairs, Pid),
		    case lists:member(Node, ExportExprs) of
			true ->
			    refac_code_search_utils:add_new_export_var(Pid, NewVar);
			_ -> ok
		    end,
		    {refac_misc:rewrite(Node, refac_syntax:variable(NewVar)), true}
	    end;
	_ -> {Node, false}
    end.

get_new_var_name(Node, ExprNewVarPairs, NewVarGenPid)->
    case  lists:keysearch(Node, 1, ExprNewVarPairs) of
	{value, {Node, NewVar}} when NewVar/=none->
	    NewVar;
	_ ->
	    refac_code_search_utils:gen_new_var_name(NewVarGenPid)
    end.
	    
    
generate_new_var_names(Subst, NewVarGenPid) ->
    ExprsToBeReplaced =lists:append([element(1, lists:unzip(S))||S<-Subst]),
    SortedExprsToBeReplacedByLoc = lists:usort(fun(E1,E2) ->
						       refac_syntax:get_pos(E1) =<
							   refac_syntax:get_pos(E2)
					       end, ExprsToBeReplaced),
    CompleteSubst=[[{E, case lists:keysearch(E, 1, S) of
			    {value, {E, E1}} ->
				E1;
			    false ->
				E
			end}||E<-SortedExprsToBeReplacedByLoc]||S<-Subst],
    ZippedSubst=zip_subst(CompleteSubst),
    GroupedSubst=refac_misc:group_by(2, ZippedSubst),
    GroupedExprsToBeReplaced=[element(1, lists:unzip(G))||G<-GroupedSubst],
    SortedGroupedExprsToBeReplacedByLoc=lists:sort(fun(G1,G2) ->
							min_src_pos(G1)=<min_src_pos(G2)
						   end, GroupedExprsToBeReplaced),
    lists:append([[{E, NewVar}||E<-Group]||
		     Group<-SortedGroupedExprsToBeReplacedByLoc,
		     NewVar <-[case refac_syntax:type(hd(Group)) of
			 variable -> none;
			 _ ->refac_code_search_utils:gen_new_var_name(NewVarGenPid)
		     end]]).

min_src_pos(Es) ->
    hd(lists:sort([refac_syntax:get_pos(E)||E<-Es])).
    
zip_subst(ListOfSubstLists) ->    
    zip_subst_1(ListOfSubstLists, []).
zip_subst_1([[]|_T], Acc)  ->
    lists:reverse(Acc);
zip_subst_1(ListOfLists, Acc)->
    Hd=[hd(L)|| L  <- ListOfLists],
    Tl=[tl(L) || L <-ListOfLists],
    {Es1, Es2} = lists:unzip(Hd),
    zip_subst_1(Tl, [{hd(Es1), [gen_binding_info(E)||E<-[hd(Es1)|Es2]]}|Acc]).


%% TO BE COMPLETEd.
gen_binding_info(E)->
    refac_prettypr:format(E).

   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
%%                                                                      %%
%%  Some utility functions                                              %%
%%                                                                      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

has_same_name(Expr1, Expr2) ->
    refac_code_search_utils:identifier_name(Expr1) ==
      refac_code_search_utils:identifier_name(Expr2).

generalisable(E1, E2) ->
    refac_code_search_utils:generalisable(E1) andalso 
	refac_code_search_utils:generalisable(E2).

reset_attrs(Node) ->
    ast_traverse_api:full_buTP(fun (T, _Others) ->
				       T1 = refac_syntax:set_ann(T, []),
				       refac_syntax:remove_comments(T1)
			       end,
			       Node, {}).
is_macro_name(Exp) ->
    case lists:keysearch(category, 1, refac_syntax:get_ann(Exp)) of
	 {value, {category, {macro_name,_,_}}} ->
	    true;
	_ -> false
    end.
