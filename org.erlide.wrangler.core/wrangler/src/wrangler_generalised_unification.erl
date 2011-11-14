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
%%@hidden
%%@private
-module(wrangler_generalised_unification).

-export([expr_match/3, expr_match/2]).

-compile(export_all).

-include("../include/wrangler_internal.hrl").

expr_match(Exp1, Exp2) ->
    expr_match(Exp1, Exp2, fun(_) ->true end).

-spec(expr_match/3::(syntaxTree()|[syntaxTree()], syntaxTree()|[syntaxTree()], function()) ->
                          {true, [{atom(), syntaxTree()|[syntaxTree()]}]} | false).
expr_match(Exp1, Exp2, Cond) ->
    Res = unification(Exp1, Exp2),
    PossibleMatches=[case static_semantics_check(Subst) of 
                         false -> [];
                         {true, S} ->
                             case Cond(S) of
                                 true ->
                                     [{true, S}];
                                 false ->
                                     [];
                                 _ ->
                                     throw({error, lists:flatten(
                                                     io_lib:format
                                                       ("Condition checking of rule/collector "
                                                        "returns non-boolean value. Template being matched: ~s.",
                                                        [wrangler_prettypr:format(Exp1)]))})
                             end
                     end||{true,Subst}<-Res],
    case lists:append(PossibleMatches) of
        [] ->
            false;
        [{true, Subst}|_] ->
            {true, Subst}
    end.


static_semantics_check(Subst) ->
    Subst1 = [{case wrangler_syntax:type(V) of
                   variable -> wrangler_syntax:variable_name(V);
                   atom -> wrangler_syntax:atom_value(V)
               end, Exp} 
              ||{V, Exp}<-Subst],
    GroupedSubst = group_by_index(1, Subst1),
    Res =[static_semantics_check_1(G)||G<-GroupedSubst],
    case lists:member(false, Res) of 
        true -> 
            false;
        false ->
            {true,lists:append([S || {true, S} <- Res])}
    end.

static_semantics_check_1([]) ->
    {true,[]};
static_semantics_check_1([{V,S}]) ->
    {true, [{V,S}]};
static_semantics_check_1(Subst=[{V, S}|_]) ->
    SubstEs = [E2||{_E1, E2} <- Subst],
    EStrs = [format(E)||E<-SubstEs],
    case lists:usort(EStrs) of
        [_] ->
            B=[var_binding_structure(E)||E<-SubstEs],
            case lists:usort(B) of 
                [_] ->
                    {true, [{V,S}]};
                _ -> false
            end;
        _ ->
            false 
    end.
        
-spec(unification/2::(syntaxTree()|[syntaxTree()], syntaxTree()|[syntaxTree()]) ->
                           [{true, [tuple()]}]|[false]).
unification(Exp1, Exp2) ->
    case {is_list(Exp1), is_list(Exp2)} of
        {true, true} ->   
            %%both are list of expressions
            list_unification_1(Exp1, Exp2);
        {false, false} -> 
            %% both are single expressions.
	    T1 = wrangler_syntax:type(Exp1),
	    T2 = wrangler_syntax:type(Exp2),
	    case T1 == T2 of
		true ->
                    same_type_unification(Exp1, Exp2);
                _ -> non_same_type_unification(Exp1, Exp2)
	    end;
        {false, true}-> 
            case is_meta_list_variable(Exp1) of 
                true ->
                    %% make sure a meta list variable is mapped to a list of exprs.
                    [{true, [{Exp1, Exp2}]}];
                false->
                    [false]
            end;
        _ -> [false]
    end.

-spec(list_unification_1/2::([syntaxTree()|[syntaxTree()]], [syntaxTree()|[syntaxTree()]]) ->
                                  [{true, [tuple()]}]|[false]).
list_unification_1(Exp1, Exp2)->
    LEs1 = [E||E<-Exp1,is_list(E)],
    LEs2 = [E||E<-Exp2,is_list(E)],
    case LEs1==[] andalso LEs2==[] andalso 
        has_meta_list(Exp1) of 
        false ->
            list_unification_2(Exp1, Exp2);
        true ->
            list_unification_3(Exp1, Exp2)
    end.

has_meta_list(NodeList) ->
    [E ||E<-NodeList, is_meta_list(E)]/=[].



-spec(list_unification_2/2::([syntaxTree()|[syntaxTree()]], [syntaxTree()|[syntaxTree()]]) ->
                           [{true, [tuple()]}]|[false]).
list_unification_2(Exp1, Exp2) ->
    case length(Exp1) == length(Exp2) of
        true ->
            Res = [unification(E1, E2)|| 
                      {E1, E2} <- lists:zip(Exp1, Exp2)],
            zip_unification_results(Res);
        _ -> [false]
    end.

zip_unification_results([]) ->
    [];
zip_unification_results(Res=[R|Rs]) ->
    case lists:member([false], Res) of 
        true ->
            [false];
        false ->      
            case Rs of 
                [] ->
                    [{true, E}||{true, E}<-R];
                _  when R==[]->
                    zip_unification_results(Rs);
                _ ->
                    [{true, E1++E2}||{true,E1}<-R, 
                                     {true,E2}<-zip_unification_results(Rs)]
            end
    end.
    
          
           
-spec(list_unification_3/2::([syntaxTree()], [syntaxTree()]) ->
                               [{true, [tuple()]}]|[false]).
list_unification_3([], []) ->
    [];
list_unification_3(T, []) ->
    NonMetaList=[T1||T1<-T, not is_meta_list(T1)],
    case NonMetaList of 
        [] ->
            lists:append([create_sub(T1,[])||T1<-T]);
        _ -> 
            [false]
    end;
list_unification_3([], _) ->
    [false];
list_unification_3(_List1=[H1|T1], List2=[H2|T2]) ->
    case is_meta_list(H1) of
        true ->
            T11= [T||T<-T1, not is_meta_list(T)],
            Len1 = length(T11),
            Len2 = length(List2),
            case Len1 > Len2 of 
                true -> [false];
                _->
                    Res=[begin
                             H1Sub = create_sub(H1, lists:sublist(List2, I)),
                             Res=list_unification_1(T1, lists:nthtail(I, List2)),
                             combine_unification_results(H1Sub, Res)
                         end||I<-lists:seq(0,Len2-Len1)],
                    lists:append(Res)
            end;
        false ->
            Res= unification(H1, H2),
            Ms = list_unification_3(T1, T2),
            combine_unification_results(Res, Ms)
    end.

combine_unification_results([], []) ->
    [];
combine_unification_results(Sub1,[]) ->
    Sub1;
combine_unification_results([], Sub2) ->
    Sub2;
combine_unification_results([false],_) ->
    [false];
combine_unification_results(_, [false]) ->
    [false];
combine_unification_results(Sub1,Sub2) ->
    [{true, M++R}||{true,M}<-Sub1, {true,R}<-Sub2].



    

create_sub(T1, T2List)->
    case is_meta_list_variable(T1) of 
        true ->
            [{true,[{T1, T2List}]}];
        false ->
            case is_meta_function_arity_list(T1) of
                true ->
                    case lists:all(fun(L) -> 
                                           wrangler_syntax:type(L) == arity_qualifier
                                   end, T2List) of 
                        true ->
                            create_sub_for_arity_qualifier(T1, T2List);
                        false ->
                            [false]
                    end;
                false ->
                    create_sub_for_clause(T1, T2List)
            end
    end.


         
create_sub_for_arity_qualifier(MetaAQList, AQList) ->
    MetaFunName=wrangler_syntax:arity_qualifier_body(MetaAQList),
    MetaArity = wrangler_syntax:arity_qualifier_argument(MetaAQList),
    {FunNames, Arities} = lists:unzip([{wrangler_syntax:arity_qualifier_body(T),
                                        wrangler_syntax:arity_qualifier_argument(T)}
                                       ||T <- AQList]),
    [{true, [{MetaFunName, FunNames}, {MetaArity, Arities}]}].
                                        
    
create_sub_for_clause(MetaClause, ClauseList) ->
    T= wrangler_syntax:type(MetaClause),
    case lists:member(T, [clause, function_clause]) of
        false ->
            [false];
        true ->
            T11=case wrangler_syntax:type(MetaClause) of
            function_clause ->
                        wrangler_syntax:function_clause(MetaClause);
            _ -> MetaClause
                end,
            [Pat]= wrangler_syntax:clause_patterns(T11),
            T2List1=[case wrangler_syntax:type(T2) of
                         function_clause ->
                             wrangler_syntax:function_clause(T2);
                         _ -> T2
                     end || T2 <- ClauseList],
            case clause_guard(T11) of
                [[]] ->
                    T2Gs=[G2||T2 <- T2List1, G2 <- clause_guard(T2), G2 /= []],
                    case T2Gs of
                        [] ->
                            [Body]= wrangler_syntax:clause_body(T11),
                            T2Pats=[wrangler_syntax:clause_patterns(T2)||T2 <- T2List1],
                            T2Body=[wrangler_syntax:clause_body(T2)||T2 <- T2List1],
                            [{true, [{Pat, T2Pats},{Body, T2Body}]}];
                        _ -> [false]
                    end;
                [[Guard]] ->
                    [Body]= wrangler_syntax:clause_body(T11),
                    T2Pats=[wrangler_syntax:clause_patterns(T2)||T2 <- T2List1],
                    T2Gs=[G2||T2 <- T2List1, G2 <- clause_guard(T2)],
                    T2Body=[wrangler_syntax:clause_body(T2)||T2 <- T2List1],
                    [{true, [{Pat, T2Pats}, {Guard, T2Gs}, {Body, T2Body}]}]
            end
    end.
          
-spec(same_type_unification/2::(syntaxTree(), syntaxTree()) ->
                                     [{true, [tuple()]}]|[false]).
same_type_unification(Exp1, Exp2) ->
    T1 = wrangler_syntax:type(Exp1),
    case T1 of
	variable ->
            Exp1Name = wrangler_syntax:variable_name(Exp1),
	    Exp2Name = wrangler_syntax:variable_name(Exp2),
            case is_macro_name(Exp1) andalso is_macro_name(Exp2) of
                true ->
                    if Exp1Name==Exp2Name ->
                            [{true, []}];
                       true -> [false]
                    end;
                false -> 
                    case is_object_variable(Exp1) of 
                        true when Exp1Name==Exp2Name->
                            [{true, []}];
                        true ->
                            [false];
                        false ->
                            [{true, [{Exp1, Exp2}]}]
                    end
	    end;
	atom ->
            Exp1Val = wrangler_syntax:atom_value(Exp1),
            Exp2Val = wrangler_syntax:atom_value(Exp2),
	    case Exp1Val == Exp2Val of
		true ->
                    Ann1 = wrangler_syntax:get_ann(Exp1),
                    Ann2 = wrangler_syntax:get_ann(Exp2),
                    case lists:keysearch(fun_def,1,Ann1) of
                        {value, {fun_def, {M, F, A, _, _}}} ->
                            case lists:keysearch(fun_def,1,Ann2) of
		         	{value, {fun_def, {M1,F1,A1,_,_}}} ->
		         	    case {M, F, A}=={M1, F1, A1} of
		        		true -> [{true, []}];
		         		false ->
		         		    [false]
		         	    end;
		         	false ->
		         	    [false]
		            end;
                        false ->
                            [{true, []}]
                    end;
		_ -> 
                    case is_meta_atom(Exp1) of
                        true ->
                            [{true, [{Exp1, Exp2}]}];
                        false ->
                            [false]
                    end
	    end;
	operator ->
	    case wrangler_syntax:operator_name(Exp1)
               == wrangler_syntax:operator_name(Exp2) of
		true -> [{true, []}];
		_ -> [false]
	    end;
        char ->
	    case wrangler_syntax:char_value(Exp1)
               == wrangler_syntax:char_value(Exp2) of
		true -> [{true, []}];
		_ -> [false]
	    end;
	integer ->
	    case wrangler_syntax:integer_value(Exp1)
               == wrangler_syntax:integer_value(Exp2) of
		true -> [{true, []}];
		_ -> [false]
	    end;
	string ->
	    case wrangler_syntax:string_value(Exp1)
               == wrangler_syntax:string_value(Exp2) of
		true -> [{true, []}];
		_ -> [false]
	    end;
	float ->
	    case wrangler_syntax:float_value(Exp1)
               == wrangler_syntax:float_value(Exp2) of
		true -> [{true, []}]
	    end;
	underscore -> [{true, []}];
	nil -> [{true, []}];
        list ->
            case wrangler_syntax:is_proper_list(Exp1) andalso
                wrangler_syntax:is_proper_list(Exp2) of
                true ->
                    Es1 = wrangler_syntax:list_elements(Exp1),
                    Es2 = wrangler_syntax:list_elements(Exp2),
                    list_unification_1(Es1, Es2);
                false ->
                    SubTrees1 = subtrees(Exp1),
                    SubTrees2 = subtrees(Exp2),
                    case length(SubTrees1) == length(SubTrees2) of
                        true ->
                            unification(SubTrees1, SubTrees2);
                        _ -> [false]
                    end
            end;
        _ ->
	    SubTrees1 = subtrees(Exp1),
            SubTrees2 = subtrees(Exp2),
            case length(SubTrees1) == length(SubTrees2) of
		true ->
                    unification(SubTrees1, SubTrees2);
                _ -> [false]
	    end
    end.


non_same_type_unification(Exp1, Exp2) ->
    T1 = wrangler_syntax:type(Exp1),
    case T1 of
        variable ->
            case is_object_variable(Exp1) of
                true -> [false];
                _ ->
                    [{true, [{Exp1, Exp2}]}]
            end;
        _ ->
            [false]
    end.
       
var_binding_structure(AST) when not is_list(AST) ->
    var_binding_structure([AST]);
var_binding_structure(ASTList) ->
    VarLocs = lists:keysort(2, wrangler_misc:collect_var_source_def_pos_info(ASTList)),
    case VarLocs of
	[] ->
	    [];
	_ -> 
            [DefLoc|| {_Name, _SrcLoc, DefLoc} <- VarLocs]
    end.

is_meta_list(Node) ->
    is_meta_list_variable(Node) orelse is_meta_clause_list(Node) orelse
        is_meta_function_arity_list(Node).

is_meta_clause_list(C) ->
    T = wrangler_syntax:type(C),
    case lists:member(T, [clause, function_clause]) of 
        true ->
            C1 =case T of 
                    function_clause ->
                        wrangler_syntax:function_clause(C);
                    _ -> C
                end,
            Pat = wrangler_syntax:clause_patterns(C1),
            Body = wrangler_syntax:clause_body(C1),
            Guard = clause_guard(C1),
            case Pat of 
                [P] ->
                    case Guard of 
                        [[]] ->
                            case Body of 
                                [B] ->
                                    is_meta_meta_list_variable(P) andalso 
                                        is_meta_meta_list_variable(B);  
                                _ -> false
                            end;
                        [[G]] ->
                            case Body of 
                                [B] ->
                                    is_meta_meta_list_variable(P) andalso 
                                        is_meta_meta_list_variable(G) andalso
                                        is_meta_meta_list_variable(B);  
                                _ -> false
                            end;
                        _ -> false
                    end;
                _ ->
                    false
            end;
        _ -> false
    end.

is_meta_function_arity_list(Node) ->
    case wrangler_syntax:type(Node) of
        arity_qualifier ->
            Body = wrangler_syntax:arity_qualifier_body(Node),
            Arity = wrangler_syntax:arity_qualifier_argument(Node),
            is_meta_list_variable(Body) andalso
                is_meta_list_variable(Arity);
        _  ->
            false
    end. 
    
is_object_variable(Var) ->
    case wrangler_syntax:type(Var) of
        variable ->
            VarName = atom_to_list(wrangler_syntax:variable_name(Var)),
            not (lists:prefix("@", lists:reverse(VarName)));
        _ ->
          false
    end.

is_meta_atom(Node) ->
    case wrangler_syntax:type(Node) of
        atom ->
            AtomName = atom_to_list(wrangler_syntax:atom_value(Node)),
            lists:prefix("@", lists:reverse(AtomName));
        _ ->
            false
    end.
        
is_meta_list_variable(Var) ->
    case wrangler_syntax:type(Var) of
        variable ->
            VarName = wrangler_syntax:variable_name(Var),
            lists:prefix("@@", lists:reverse(atom_to_list(VarName)));
        _ ->
            false
    end.

is_meta_meta_list_variable(Var) ->
    case wrangler_syntax:type(Var) of
        variable ->
            VarName = wrangler_syntax:variable_name(Var),
            lists:prefix("@@@", lists:reverse(atom_to_list(VarName)));
        _ ->
            false
    end.

group_by_index(N, TupleList) ->
    group_by_1(N, lists:keysort(N, TupleList)).

group_by_1(_N, []) -> [];
group_by_1(N, TupleList=[E|_Es]) ->
    NthEle = element(N, E),
    {E1,E2} = lists:splitwith(fun(T) -> element(N,T) == NthEle end, TupleList),
    [E1 | group_by_1(N, E2)].
    
format(Es)when is_list(Es) ->
    [format(E)||E<-Es];
format(E) ->
    wrangler_prettypr:format(
         wrangler_misc:reset_ann_and_pos(
              rm_comments(E))).

rm_comments(Node) ->
    wrangler_syntax:remove_comments(Node).

is_macro_name(Exp) ->
    Ann = wrangler_syntax:get_ann(Exp),
    {value, {syntax_path, macro_name}} == 
        lists:keysearch(syntax_path, 1, Ann).

clause_guard(C) ->
    revert_clause_guard(wrangler_syntax:clause_guard(C)).

revert_clause_guard(none) -> [[]];
revert_clause_guard(E)->
    case  wrangler_syntax:type(E) of
        disjunction -> wrangler_syntax:revert_clause_disjunction(E);
        conjunction ->
            %% Only the top level expression is
            %% unfolded here; no recursion.
            [wrangler_syntax:conjunction_body(E)];
        _ ->
            [[E]]       % a single expression
    end.

subtrees(T) ->
    case wrangler_syntax:type(T) of
        clause ->
            case wrangler_syntax:clause_guard(T) of
                none -> [wrangler_syntax:clause_patterns(T), [[]],
                         wrangler_syntax:clause_body(T)];
                G -> [wrangler_syntax:clause_patterns(T),
                      revert_clause_guard(G), 
                      wrangler_syntax:clause_body(T)]
            end;
        _ ->
            wrangler_syntax:subtrees(T)
    end.


collect_meta_vars_and_atoms(Tree) when is_list(Tree) ->
    lists:usort(lists:append([collect_meta_vars_and_atoms(T)||T<-Tree]));
collect_meta_vars_and_atoms(Tree) ->
    F=fun(Node, Acc) ->
              case erl_syntax:type(Node) of
                  variable ->
                      VarName = erl_syntax:variable_name(Node),
                      case is_meta_name(VarName) of 
                          true ->
                              [VarName|Acc];
                          _ ->
                              Acc
                      end;
                  atom ->
                      AtomValue = erl_syntax:atom_value(Node),
                      case is_meta_name(AtomValue) of 
                          true ->
                              [AtomValue|Acc];
                          _ ->
                              Acc
                      end;
                  _ -> Acc
              end
      end,
    lists:usort(api_ast_traverse:fold(F, [], Tree)).

is_meta_name(VarName) ->
    lists:prefix("@", lists:reverse(atom_to_list(VarName))).
