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
%% ===========================================================================================
%% Refactoring: Search an user-selected expression/expression sequence from the current buffer.
%%
%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%% 
-module(refac_sim_expr_search).

-export([sim_expr_search/6, normalise_record_expr/5]).

-export([get_start_end_loc/1, start_counter_process/0, 
	 stop_counter_process/1, gen_new_var_name/1,
	 variable_replaceable/1, find_anti_unifier/4,
	generalise_expr/3, vars_to_export/3]).

-import(refac_duplicated_code, [collect_vars/1]).
-import(refac_expr_search, [compose_search_result_info/2]).

-include("../include/wrangler.hrl").

-define(DefaultSimiScore, 0.8).

%%-define(DEBUG, true).

-ifdef(DEBUG).
-define(debug(__String, __Args), ?wrangler_io(__String, __Args)).
-else.
-define(debug(__String, __Args), ok).
-endif.

%% ================================================================================================
%% @doc Search expressions that are 'similar' to a user-selected expression or a sequence of expressions in the current buffer.
%%
%% <p> Given expression selected by the user, A say, expression B is similar to A if there exist a least general common abstation, C, 
%% such that, substitution C(Args1) = A, and C(Arg2) == B, and Size(C) /Size(A) > the threshold specified (0.8 by default).
%% </p>
%% <p> When the selected code contains multiple, but non-continuous sequence of, expressions, the first
%% continuous sequence of expressions is taken as the user-selected expression. A continuous sequence of
%% expressions is a sequence of expressions separated by ','.
%% <p>

%%-spec(sim_expr_search/6::(filename(), pos(), pos(), string(),[dir()],integer()) 
%%      -> {ok, [{integer(), integer(), integer(), integer()}]} | {error, string()}).    
sim_expr_search(FName, Start = {Line, Col}, End = {Line1, Col1}, SimiScore0, SearchPaths, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:sim_expr_search(~p, {~p,~p},{~p,~p},~p, ~p, ~p).\n", 
		 [?MODULE, FName, Line, Col, Line1, Col1, SimiScore0, SearchPaths, TabWidth]),
     SimiScore1 = try 
		     case SimiScore0 of 
			 [] -> ?DefaultSimiScore;
			 _ -> list_to_float(SimiScore0)
		     end
		 catch
		     V -> V;
		       _:_ -> throw({error, "Parameter input is invalid."})
		 end,
    SimiScore = case (SimiScore1>=0.1) andalso (SimiScore1 =<1.0) of 
		    true ->SimiScore1;
		    _ -> ?DefaultSimiScore
		end,    
    {ok, {AnnAST, _Info}} = refac_util:parse_annotate_file(FName, true, [], TabWidth),
    case refac_util:pos_to_fun_def(AnnAST, Start) of
	{ok, FunDef} -> FunDef;
	{error, _} -> throw({error, "You have not selected an expression!"}),
		      FunDef=[]
    end,
    Exprs= refac_util:pos_to_expr_list(FunDef, Start, End),
    case Exprs of 
	[] -> throw({error, "You have not selected an expression!"});
	_ -> ok
    end,
    RecordInfo =get_module_record_info(FName, SearchPaths, TabWidth),
    Exprs1 = case Exprs of 
		 [E] ->normalise_expr(E, RecordInfo);
		 [_|_] ->
		     normalise_expr(Exprs, RecordInfo)
	     end,    
    Res =do_search_similar_expr(AnnAST, RecordInfo, Exprs1, SimiScore),
    {Ranges, ExportVars, SubSt} = lists:unzip3(Res),
    SE = get_start_end_loc(Exprs),
    Ranges1 = [SE| Ranges--[SE]],
    ExportVars1 = {element(1,lists:unzip(vars_to_export(FunDef, End, Exprs1))), lists:usort(lists:append(ExportVars))},
    AntiUnifier = generalise_expr(Exprs1,SubSt, ExportVars1),
   %% AntiUnifier = denormalise_expr(generalise_expr(Exprs1,SubSt, ExportVars1),RecordInfo),
    Num = length(Ranges1), 
    case Num =<1 of 
	true -> ?wrangler_io("No similar expression has been found.\n",[]); 
	false-> ?wrangler_io("~p expressions (including the expression selected)"
			     " which are similar to the expression selected have been found. \n", [Num]),
		?wrangler_io(compose_search_result_info(FName, Ranges1),[]),
		?wrangler_io("\nThe generalised expression would be:\n\n~s\n\n", [refac_prettypr:format(AntiUnifier)]),
		?wrangler_io("\nUse 'C-c C-e' to remove highlights!\n",[]),
		{ok, Ranges1}
    end.
   


do_search_similar_expr(AnnAST, RecordInfo, Exprs, SimiScore) when is_list(Exprs) ->
    F0 = fun(FunNode, Acc)->
		 F = fun(T, Acc1) ->
			     case refac_syntax:type(T) of
				 clause -> Exprs1 = refac_syntax:clause_body(T),
					   do_search_similar_expr_1(Exprs, Exprs1, RecordInfo, SimiScore, FunNode)++Acc1;
				 block_expr -> Exprs1 = refac_syntax:block_expr_body(T),
					       do_search_similar_expr_1(Exprs, Exprs1, RecordInfo, SimiScore, FunNode)++Acc1;
				 try_expr -> Exprs1 = refac_syntax:try_expr_body(T),
					     do_search_similar_expr_1(Exprs, Exprs1, RecordInfo, SimiScore, FunNode)++Acc1;
				 _  -> Acc1
			     end
		     end,
		 refac_syntax_lib:fold(F, Acc, FunNode)
	 end,
    F1 =fun(Node, Acc) ->
		case refac_syntax:type(Node) of 
		    function -> F0(Node, Acc);
		    _ -> Acc
		end
	end,
    lists:reverse(refac_syntax_lib:fold(F1, [], AnnAST));
   
do_search_similar_expr(AnnAST, RecordInfo, Expr, SimiScore) ->
    {EStart, EEnd} = get_start_end_loc(Expr),
    F0 = fun(FunNode, Acc) ->
		 F = fun(Node, Acc1) ->
			     case refac_util:is_expr(Node) of 
				 true ->
				     {NStart, NEnd} = get_start_end_loc(Node),
				     case (EStart =< NStart andalso  NEnd =< EEnd) orelse
					 (NStart =< EStart andalso EEnd =< NEnd) of 
					 true -> Acc1;
					 _ ->
					     Node1 = normalise_expr(Node, RecordInfo),
					     ExportVars = vars_to_export(FunNode, NEnd, Node),
					     find_anti_unifier(Expr, Node1, SimiScore, ExportVars) ++ Acc1
				     end;
				 _ -> Acc1
			     end
		     end,
		 refac_syntax_lib:fold(F, Acc, FunNode)
	 end,
    F1 =fun(Node, Acc) ->
		case refac_syntax:type(Node) of 
		    function -> F0(Node,Acc);
		    _ -> Acc
		end
	end,
    lists:reverse(refac_syntax_lib:fold(F1, [], AnnAST)).
   

 
do_search_similar_expr_1(Exprs1, Exprs2, RecordInfo, SimiScore, FunNode) ->
    Len1 = length(Exprs1),
    Len2 = length(Exprs2),
    case Len1 =< Len2 of 
	true ->  Exprs21= lists:sublist(Exprs2, Len1),
		 {S1, E1} = get_start_end_loc(Exprs1),
		 {S2, E2} = get_start_end_loc(Exprs21),
		 case (S1 =< S2 andalso E2 =< E1) orelse (S2 =< S1 andalso E1=< E2) of 
		     true -> [];
		     _ ->
			 ExportVars =  vars_to_export(FunNode, E2, Exprs21),
			 find_anti_unifier(Exprs1, normalise_expr(Exprs21, RecordInfo), SimiScore, ExportVars)
			     ++ do_search_similar_expr_1(Exprs1, tl(Exprs2), RecordInfo, SimiScore, FunNode)
		 end;
	_ -> []
    end.


find_anti_unifier(Expr1, Expr2, SimiScore, Expr2ExportVars) ->
    try
      do_find_anti_unifier(Expr1, Expr2)
    of
      SubSt ->
	    {SubExprs1, SubExprs2} = lists:unzip(SubSt),
	    Score1 = simi_score(Expr1, SubExprs1),
	    Score2 = simi_score(Expr2, SubExprs2),
	    case Score1>= SimiScore andalso Score2>=SimiScore of
		true ->
		    case subst_check(Expr1, SubSt) of
			false ->
			    [];
			_ ->
			    {SLoc, ELoc} = get_start_end_loc(Expr2),
			    EVs1 = [E1 || {E1, E2} <- SubSt, refac_syntax:type(E2)== variable,
					  lists:member({refac_syntax:variable_name(E2), get_var_define_pos(E2)}, Expr2ExportVars)],
			    [{{SLoc, ELoc}, EVs1, SubSt}]
		    end;
		_ -> []
	    end
    catch
	_ -> []
    end.

simi_score(Expr, SubExprs) ->
    case no_of_nodes(Expr) of
	0 -> 0;
	ExprSize-> 
	    NonVarExprs = [E || E<-SubExprs, refac_syntax:type(E)=/=variable],
	    NoOfNewVars = length(NonVarExprs),
	    Res =1-((no_of_nodes(SubExprs)-length(SubExprs)+NoOfNewVars*(NoOfNewVars+1)/2)/ExprSize),
	   %% Res =1 -((no_of_nodes(SubExprs)-length(SubExprs))/ExprSize),
	    Res
    end.

subst_check(Expr1, SubSt)->
    BVs = refac_util:get_bound_vars(Expr1),
    case BVs of
	[] ->
	    true;
	_ ->  lists:all(fun({E1, E2}) ->     
		 case refac_syntax:type(E1) of
		     variable -> 
			 case is_macro_name(E1) of
			     true -> 
				 false;
			     _ ->{value, {def, DefPos}} = lists:keysearch(def,1, refac_syntax:get_ann(E1)),
				 %% local vars should have the same substitute.
				 not(lists:any(fun({E11, E21}) ->
					       (refac_syntax:type(E11) == variable) andalso
					   ({value, {def, DefPos}} == lists:keysearch(def,1, refac_syntax:get_ann(E11))) andalso
					   (refac_prettypr:format(reset_attrs(E2)) 
					    =/= refac_prettypr:format(reset_attrs(E21)))
					       end, SubSt))
			 end;
		     _ ->
			 %% the expression to be replaced should not contain local variables.
			 (BVs -- refac_util:get_free_vars(E1) == BVs)   
		 end
	 end,  SubSt)
       end.
    
reset_attrs(Node) ->
    refac_util:full_buTP(fun (T, _Others) -> 
				 refac_syntax:remove_comments(refac_syntax:set_ann(T, [])) end, 
			 Node, {}).

   
do_find_anti_unifier(Exprs1, Exprs2) when is_list(Exprs1) andalso is_list(Exprs2)->
    case length(Exprs1) == length(Exprs2) of
	true ->
	    case Exprs1 of 
		[] -> [];
		_ -> 
		    lists:append([do_find_anti_unifier(E1, E2) || 
				     {E1, E2} <-lists:zip(Exprs1, Exprs2)])
	    end;
	false ->
	    ?debug("Does not unify 1:\n~p\n", [{Exprs1,Exprs2}]), 
	    throw(non_unifiable)
    end;
do_find_anti_unifier(Expr1, _Expr2) when is_list(Expr1) ->
    ?debug("Does not unify 2:n~p\n", [{Expr1,_Expr2}]), 
    throw(non_unifiable);
do_find_anti_unifier(_Expr1, Expr2) when is_list(Expr2) ->
    ?debug("Does not unify 3:\n~p\n", [{_Expr1,Expr2}]), 
    throw(non_unifiable);
do_find_anti_unifier(Expr1, Expr2) ->
    F =fun(E1, E2) ->
	       SubExprs1 = refac_syntax:subtrees(E1),
	       SubExprs2 = refac_syntax:subtrees(E2),
	       try do_find_anti_unifier(SubExprs1, SubExprs2) of
		   Subst -> Subst
	       catch
		   _ -> case variable_replaceable(E1) andalso 
			    variable_replaceable(E2) of 
			    true ->
				[{E1,E2}];
			    _ -> throw(non_unificable)
			end
	       end       
       end,
    T1 = refac_syntax:type(Expr1),
    T2 = refac_syntax:type(Expr2),
    case T1==T2 of
	false  -> case variable_replaceable(Expr1) andalso 
		   variable_replaceable(Expr2) of 
		   true ->[{Expr1, Expr2}];
		   false -> 
			 ?debug("Does not unify 4:\n~p\n", [{Expr1,Expr2}]), 
			 throw(non_unifiable)
	       end;
	true -> case refac_syntax:is_literal(Expr1) andalso refac_syntax:is_literal(Expr2) of 
		 true ->
			case refac_syntax:concrete(Expr1) ==refac_syntax:concrete(Expr2) of 
			    true ->
				[];
			    _ ->
				case variable_replaceable(Expr1) andalso
				    variable_replaceable(Expr2) of
				    true ->
					[{Expr1, Expr2}];
				    false -> throw(non_unificable)
				end
			end;			
		_ -> case T1 of 
			 variable ->
			      case {is_macro_name(Expr1),is_macro_name(Expr2)} of
				  {true, true} ->
				      case macro_name_value(Expr1)==
					  macro_name_value(Expr2) of 
					  true -> [];
					  false -> throw(non_unifiable)
				      end;
				  {false, false} ->[{Expr1, Expr2}];
				  _ -> 
				      ?debug("Does not unify 5:\n~p\n", [{Expr1,Expr2}]),
				      throw(non_unifiable)
			      end;
			 operator -> case refac_syntax:operator_name(Expr1) == 
					 refac_syntax:operator_name(Expr2) of 
					 true -> [];
					 false ->
					     ?debug("Does not unify 6:\n~p\n", [{Expr1,Expr2}]),
					     throw(non_unifiable)
				     end;
			 underscore ->[];
			 macro -> MacroName1 = refac_syntax:macro_name(Expr1),
				  MacroName2 = refac_syntax:macro_name(Expr2),
				  case macro_name_value(MacroName1) =/=
				      macro_name_value(MacroName2) of
				      true -> [{Expr1, Expr2}];
				      false -> F(Expr1, Expr2)
				  end;
			 _ -> case refac_syntax:is_leaf(Expr1) of
				  true -> case variable_replaceable(Expr1)  andalso 
 					      variable_replaceable(Expr2) of
				      
					      true -> 
						  [{Expr1, Expr2}];
					      _ ->
						  ?debug("Does not unify 7:\n~p\n", [{Expr1,Expr2}]),
						  throw(non_unifiable)
					  end;
				  false -> F(Expr1,Expr2)
			      end
		     end
	     end
    end.
    
is_macro_name(Exp) ->
    {value, {category, macro_name}} == 
	lists:keysearch(category, 1, refac_syntax:get_ann(Exp)).

macro_name_value(Exp) ->
    case refac_syntax:type(Exp) of 
	    atom ->
		refac_syntax:atom_value(Exp);
	    variable ->
		refac_syntax:variable_name(Exp)
    end.
  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generalise_expr(Exprs, SearchRes, ExportVars) ->
    FunName = refac_syntax:atom(new_fun),
    BVs = refac_util:get_bound_vars(Exprs),
    FVs = lists:ukeysort(2, refac_util:get_free_vars(Exprs)),
    {NewExprs, NewExportVars} = generalise_expr_1(Exprs, SearchRes, ExportVars),
    NewExprs1 = case NewExportVars of
		    [] -> NewExprs;
		    [V] -> NewExprs++[refac_syntax:variable(V)];
		    _ ->E = refac_syntax:tuple([refac_syntax:variable(V) || V <- NewExportVars]),
			NewExprs ++ [E]
		end,
    Pars = collect_vars(NewExprs)--element(1, lists:unzip(BVs)),
    FVPars = [V || {V, _} <-FVs, lists:member(V, Pars)],
    NewVarPars = Pars --FVPars,
    Pars1 = [refac_syntax:variable(V)|| V <- FVPars] ++ 
	[refac_syntax:variable(V) || V <-NewVarPars],    
    C = refac_syntax:clause(refac_util:remove_duplicates(Pars1), none, NewExprs1),
    refac_syntax:function(FunName, [C]).
   
    
generalise_expr_1(Exprs, Subst, ExportVars) when is_list(Exprs)->
    {E, NewExportVars} = generalise_expr_2(refac_syntax:block_expr(Exprs), Subst, refac_util:get_free_vars(Exprs), ExportVars),
    {refac_syntax:block_expr_body(E), NewExportVars};
generalise_expr_1(Expr, Subst, ExportVars) ->
    {E, NewExportVars}=generalise_expr_2(Expr, Subst, refac_util:get_free_vars(Expr), ExportVars),
    {[E], NewExportVars}.
  
generalise_expr_2(Expr, Subst, ExprFreeVars, {ExportVars1, ExportVars2}) ->
    case lists:all(fun(S) -> S==[] end, Subst) of 
	true -> {Expr, ExportVars1};
	_ ->
	    Pid = start_counter_process(sets:from_list(collect_vars(Expr))),
	    ExportVars3 = [E || E<-ExportVars2, refac_syntax:type(E) =/= variable],
	    {Expr1, _}= refac_util:stop_tdTP(fun do_replace_expr_with_var_1/2, Expr, {Subst, ExprFreeVars, Pid, ExportVars3}),
	    NewVarsToExport = get_new_export_vars(Pid),
	    stop_counter_process(Pid),
	    VarsToExport = refac_util:remove_duplicates(ExportVars1++[refac_syntax:variable_name(E)||E<-ExportVars2, 
						     refac_syntax:type(E)==variable]++NewVarsToExport),
	    {Expr1, VarsToExport}						  
    end.

do_replace_expr_with_var_1(Node, {SubSt, ExprFreeVars, Pid, ExportExprs }) ->
    F= fun(S, Name) ->Es =[refac_prettypr:format(E2)
			   ||{E1, E2}<-S, 
			     refac_syntax:type(E1)==variable, 
			     refac_syntax:variable_name(E1) == Name],
		      length(sets:to_list(sets:from_list(Es)))==1
       end,	       
    ExprsToReplace = sets:from_list([E1|| S<-SubSt, {E1, _E2}<-S]),
    case sets:is_element(Node, ExprsToReplace) of
	true ->
	    FVs = refac_util:get_free_vars(Node),
	    case refac_syntax:type(Node) of
		variable -> 
		    case FVs of
				[] -> {Node, false};
				_ -> case FVs --ExprFreeVars of
					 [] -> 
					     Name = refac_syntax:variable_name(Node),
					     case lists:all(fun(S) ->F(S, Name) end, SubSt) of
						 true ->{Node, false};
						 _ ->NewVar = gen_new_var_name(Pid),
						     case lists:member(Node, ExportExprs) of
							 true ->
							     add_new_export_var(Pid, NewVar);
							 _ -> ok
						     end,
						     {refac_util:rewrite(Node, refac_syntax:variable(NewVar)),true}
					     end;
					 _ -> {Node, false}
				     end
		    end;
		_ -> NewVar = gen_new_var_name(Pid),
		     case lists:member(Node, ExportExprs) of
			 true ->
			     add_new_export_var(Pid, NewVar);
			 _ -> ok
		     end,	     
		     {refac_util:rewrite(Node, refac_syntax:variable(NewVar)),true}
	    end;
	_ -> {Node, false}
    end.


%% expressions which should not be replaced by a variable.
%% how about expressions has side effects?
variable_replaceable(Exp) ->
    case lists:keysearch(category,1, refac_syntax:get_ann(Exp)) of 
	{value, {category, record_field}} -> false;
	{value, {category, record_type}} -> false;	 
	{value, {category, guard_expression}} -> false;
	{value, {category, macro_name}} -> false;
	{value, {category, pattern}} -> 
	    case refac_syntax:is_literal(Exp) orelse
		refac_syntax:type(Exp)==variable of 
		true ->
		     true;
		_ -> false
	    end;
	_ -> T = refac_syntax:type(Exp),
	     (not (lists:member(T, [match_expr, operator]))) andalso
	             (refac_util:get_var_exports(Exp)==[])
    end.


add_new_export_var(Pid, VarName) ->
    Pid ! {add, VarName}.

get_new_export_vars(Pid) ->
    Pid !{self(), get},
    receive
	{Pid, Vars} ->
	     Vars
    end.
	
gen_new_var_name(Pid) -> 
    Pid ! {self(), next},
    receive
	{Pid, V} ->
	     V
    end.
    
start_counter_process() ->
    start_counter_process(sets:new()).

start_counter_process(UsedNames) ->               
    spawn_link(fun() -> counter_loop({1, UsedNames,[]}) end).
             

stop_counter_process(Pid) ->
    Pid!stop.

counter_loop({SuffixNum, UsedNames, NewExportVars}) ->
    receive
	{From, next} ->
	    {NewSuffixNum, NewName} = make_new_name(SuffixNum, UsedNames),
	    From ! {self(), NewName},
	    counter_loop({NewSuffixNum, sets:add_element(NewName, UsedNames), NewExportVars});
	{add, Name} ->
	    counter_loop({SuffixNum, UsedNames, [Name|NewExportVars]});
	{From, get}->
	    From ! {self(), lists:reverse(NewExportVars)},
	    counter_loop({SuffixNum, UsedNames, NewExportVars});
	stop ->
	    ok
    end.

make_new_name(SuffixNum, UsedNames) ->
    NewName = "NewVar_"++integer_to_list(SuffixNum),
    case sets:is_element(NewName, UsedNames) of 
	true ->
	    make_new_name(SuffixNum+1, UsedNames);
	_ -> {SuffixNum, NewName}
    end.

normalise_expr(Exprs, RecordInfo) ->
    normalise_record_expr(Exprs, RecordInfo).
  %%  do_normalise_fun_calls(Exprs1).
  
normalise_record_expr(Exprs, RecordInfo) when is_list(Exprs)->
     [refac_util:full_buTP(fun do_normalise_record_expr_1/2, E, {RecordInfo, true})|| E<-Exprs];

normalise_record_expr(Expr, RecordInfo) ->
     refac_util:full_buTP(fun do_normalise_record_expr_1/2, Expr, {RecordInfo,true}).

get_start_end_loc(Exprs) when is_list(Exprs) ->
    E1= hd(Exprs),
    En = lists:last(Exprs),
    {S, _E} = refac_util:get_range(E1),
    {_S, E} = refac_util:get_range(En),
    {S, E};
get_start_end_loc(Expr) ->
    refac_util:get_range(Expr).
    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Refactoring: Normalise record expression.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%-spec(normalise_record_expr/5::(filename(), pos(), bool(), [dir()], integer()) -> {error, string()} | {ok, [filename()]}).
normalise_record_expr(FName, Pos={Line, Col}, ShowDefault, SearchPaths, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:normalise_record_expr(~p, {~p,~p},~p, ~p, ~p).\n", 
		 [?MODULE, FName, Line, Col, ShowDefault, SearchPaths,TabWidth]),
    Cmd = "CMD: " ++ atom_to_list(?MODULE) ++ ":normalise_record_expr(" ++ "\"" ++
	FName ++ "\", {" ++ integer_to_list(Line) ++", " ++ integer_to_list(Col) ++ "},"
	 ++ atom_to_list(ShowDefault)++ " [" ++ refac_util:format_search_paths(SearchPaths)
	++ "]," ++ integer_to_list(TabWidth) ++ ").",
    {ok, {AnnAST, _Info}} =refac_util:parse_annotate_file(FName,true, [], TabWidth),
    RecordExpr =pos_to_record_expr(AnnAST, Pos),
    case refac_syntax:type(refac_syntax:record_expr_type(RecordExpr)) of
	atom -> ok;
	_ -> throw({error, "Wrangler can only normalise a record expression with an atom as the record name."})
    end,
    {AnnAST1, _Changed} = normalise_record_expr_1(FName, AnnAST,Pos,ShowDefault, SearchPaths, TabWidth),
    refac_util:write_refactored_files_for_preview([{{FName, FName}, AnnAST1}], Cmd),
    {ok, [FName]}.


normalise_record_expr_1(FName, AnnAST,Pos, ShowDefault, SearchPaths, TabWidth) ->
    RecordInfo = get_module_record_info(FName, SearchPaths, TabWidth),
    refac_util:stop_tdTP(fun do_normalise_record_expr/2, AnnAST, {Pos,RecordInfo, ShowDefault}).
    

do_normalise_record_expr(Node, {Pos, RecordInfo, ShowDefault}) ->
    case refac_syntax:type(Node) of 
	record_expr ->
	    {S, E} = refac_util:get_range(Node), 
	    case (S =<Pos) andalso (Pos =< E) of 
		true -> 
		    {refac_util:full_buTP(fun do_normalise_record_expr_1/2,
                 	  Node, {RecordInfo, ShowDefault}), true};
		_ -> {Node, false}
	    end;
	_ -> {Node, false}
    end.

do_normalise_record_expr_1(Node, {RecordInfo, ShowDefault}) ->
    Fun = fun({FName, FVal}, Fields) ->
		  R =[F||F<-Fields, refac_syntax:type(refac_syntax:record_field_name(F))==atom,
			   refac_syntax:concrete(refac_syntax:record_field_name(F))== FName],
		  case R of
		      [F] when ShowDefault ->[F];
		      [F] -> V = refac_syntax:record_field_value(F),
			     Cond =(refac_syntax:type(V)==atom andalso refac_syntax:concrete(V)==undefined) orelse
				 (FVal =/= none andalso (refac_prettypr:format(V) == refac_prettypr:format(FVal))),
			     case Cond of 
				 true -> []; 
				 false -> [F]
			     end;
		      [] ->
			  Fs=[F||F<-Fields, refac_syntax:type(refac_syntax:record_field_name(F))== underscore],
			  case Fs of 
			      [F] ->
				  [refac_syntax:record_field(refac_syntax:atom(FName), refac_syntax:record_field_value(F))];
			      [] when ShowDefault->		
				  case FVal of 
				      none -> [refac_syntax:record_field(
						 refac_syntax:atom(FName), set_random_pos(refac_syntax:atom(undefined)))];
				      _ ->[refac_syntax:record_field(refac_syntax:atom(FName), set_random_pos(FVal))]
				  end;
			      _ ->[]
			  end
		  end
	  end,
    case refac_syntax:type(Node) of 
	record_expr ->
	    Arg = refac_syntax:record_expr_argument(Node),
	    Type = refac_syntax:record_expr_type(Node), 
	    Fields = refac_syntax:record_expr_fields(Node),
	    case refac_syntax:type(Type) of 
		atom -> 
		    case lists:keysearch(refac_syntax:concrete(Type), 1, RecordInfo) of 
			{value, {_, Fields1}} ->
			    Fields2 = lists:append([Fun(F, Fields) || F <- Fields1]),
			    refac_util:rewrite(Node, refac_syntax:record_expr(Arg, Type, Fields2));
			_ -> 
			    Node
		    end;
		_ -> Node
	    end;
	_ ->Node
    end.

set_random_pos(Node) ->    
    refac_syntax:set_pos(Node, {(-random:uniform(200)), -(random:uniform(200))}).
 
pos_to_record_expr(Tree, Pos) ->
    case refac_util:once_tdTU(fun pos_to_record_expr_1/2, Tree, Pos) of 
	{_, false} ->
	     throw({error, "You have not selected a record expression, "
		   "or the function containing the record expression selected does not parse."});
	{R, true} -> 
	    R
    end.

pos_to_record_expr_1(Node, Pos) ->
    case refac_syntax:type(Node) of 
	record_expr ->
	    {S, E} = refac_util:get_range(Node), 
	    case (S =<Pos) andalso (Pos =< E) of 
		true -> {Node, true};
		_ -> {[], false}
	    end;
	_ -> {[], false}
    end.


get_module_record_info(FName, SearchPaths, TabWidth) ->
    Dir = filename:dirname(FName),
    DefaultIncl = [filename:join(Dir, X) || X <-refac_util:default_incls()],
    Includes = SearchPaths++DefaultIncl,
    case refac_epp:parse_file(FName, Includes,[], TabWidth, refac_util:file_format(FName)) of 
	{ok, Forms, _} -> Forms1 =[F || F <-Forms, case F of 
						       {attribute, _, file, _} -> false;
						       {attribute, _, type, {{record, _}, _, _}} -> false;
						       _ -> true
						   end],
			  SyntaxTree = refac_recomment:recomment_forms(Forms1,[]),
			  Info = refac_syntax_lib:analyze_forms(SyntaxTree),
			  case lists:keysearch(records,1, Info) of 
			      {value, {records, Records}} -> Records;
			      _ ->[]
			  end;
	{error, _Reason} -> []
    end.


no_of_nodes(Nodes) when is_list(Nodes) ->
    lists:sum([no_of_nodes(N)||N<-Nodes]);
no_of_nodes(Node) ->
    case refac_syntax:is_leaf(Node) of
	true -> 1;
	_ ->
	    lists:sum([no_of_nodes(T)||
			  T<-refac_syntax:subtrees(Node)])
    end.


vars_to_export(Fun,ExprEndPos, Expr) ->
    F= fun(T, S) ->
	       case refac_syntax:type(T) of 
 		       variable ->
		       SourcePos = refac_syntax:get_pos(T),
		       case lists:keysearch(def, 1, refac_syntax:get_ann(T)) of
			   {value, {def, DefinePos}} ->
			       VarName = refac_syntax:variable_name(T),
			       S++[{VarName, SourcePos, DefinePos}];
			   _ -> S
		       end;
		   _  -> S
	       end
	    end,
    AllVars = refac_syntax_lib:fold(F, [], Fun),
    ExprBdVarsPos = [Pos || {_Var, Pos}<-refac_util:get_bound_vars(Expr)],
    [{V, DefPos} || {V, SourcePos, DefPos} <- AllVars,
		    SourcePos > ExprEndPos,
		    lists:subtract(DefPos, ExprBdVarsPos) == []].
  

get_var_define_pos(V) ->
    {value, {def, DefinePos}} = lists:keysearch(def,1, refac_syntax:get_ann(V)),
    DefinePos.




