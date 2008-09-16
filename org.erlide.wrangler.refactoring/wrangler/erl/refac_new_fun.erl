%% ============================================================================================
%% Refactoring: Introduce a new function definition to represent a selected expression sequence.
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
%% =============================================================================================

%% =============================================================================================
-module(refac_new_fun).

-export([fun_extraction/4, fun_extraction_eclipse/4]).

%% =============================================================================================
%% @spec new_fun(FileName::filename(), Start::Pos, End::Pos, NewFunName::string())-> term()
%%         
fun_extraction(FileName, Start, End, NewFunName) ->
    fun_extraction(FileName, Start, End, NewFunName, emacs).

fun_extraction_eclipse(FileName, Start, End, NewFunName) ->
    fun_extraction(FileName, Start, End, NewFunName, eclipse).


fun_extraction(FileName, Start, End, NewFunName,Editor) ->
    io:format("\n[CMD: fun_extraction, ~p, ~p, ~p, ~p]\n", [FileName, Start, End, NewFunName]),
    case refac_util:is_fun_name(NewFunName) of 
	true ->
	    case refac_util:parse_annotate_file(FileName,true, []) of 
		{ok, {AnnAST, Info}} ->
		    case pos_to_expr(FileName, AnnAST, {Start, End}) of 
			[] -> {error, "You have not selected an expression!"};
			ExpList ->
			     {ok,Fun} = refac_util:expr_to_fun(AnnAST, hd(ExpList)),
			     case side_cond_analysis(Info, Fun, ExpList, list_to_atom(NewFunName)) of 
				 {ok, {BdVars, FrVars}} ->
				     FunName = refac_syntax:atom_value(refac_syntax:function_name(Fun)),
				     FunArity = refac_syntax:function_arity(Fun),
				     VarsToExport=vars_to_export(Fun, End, BdVars, ExpList), 
				     AnnAST1=do_fun_extraction(AnnAST,ExpList, NewFunName, FrVars, VarsToExport, FunName, FunArity),
				     case Editor of 
					 emacs ->
					     refac_util:write_refactored_files([{{FileName,FileName}, AnnAST1}]),
					     {ok, "Refactor succeeded"};
					 eclipse ->
					      Res = [{FileName, FileName, refac_prettypr:print_ast(AnnAST1)}],
					     {ok, Res}
				     end;
				 {error, Reason} -> {error, Reason}
			       end
		    end;
		{error, Reason} -> {error, Reason}
	    end;
	false  -> {error, "Invalid function name!"}
    end.  

side_cond_analysis(Info, Fun, ExpList, NewFunName) ->
    FrBdVars = lists:map(fun(E)-> envs_bounds_frees(E) end, ExpList),
    BdVars = lists:usort(lists:concat(lists:map(fun({{bound, Vars}, _}) -> Vars end, FrBdVars))),
    FrVars1 = lists:usort(lists:concat(lists:map(fun({_, {free, Vars}}) -> Vars end, FrBdVars))),
    FrVars = lists:map(fun({VarName, _Pos}) -> VarName end, lists:subtract(FrVars1, BdVars)),
    InscopeFuns = lists:map(fun({_M, F, A}) ->
				    {F, A} end, refac_util:inscope_funs(Info)),
    case lists:member({NewFunName, length(FrVars)}, InscopeFuns) of
	true ->
	    {error, "The given function name has been used by this module, please choose another name!"};
	_ ->
	    case length(ExpList) ==1  of 
		true ->
		    Exp = hd(ExpList),
		    case is_guard_expr(Exp) of 
			true -> {error, "The selected guard expression cannot be replaced by a function call!"};
			_ ->{StartPos, EndPos} = refac_util:get_range(Exp),
			    Ranges = collect_prime_expr_ranges(Fun),
			    Res = lists:any(fun({StartPos1, EndPos1}) ->  
						    (StartPos >= StartPos1) andalso (EndPos =<EndPos1) end, Ranges),
			    case Res of 
				true -> {error, "The selected expression cannot be replaced by a function call!"};
				_ -> 
				    {ok, {BdVars, FrVars}}
			    end
		    end;				 
		_ ->
		    ExpList1 = filter_exprs_via_ast(Fun, ExpList),
		    case ExpList1 of 
			[] ->{ok, {BdVars, FrVars}};
			_ -> {error, "The selected expression sequence canot be replaced by  a function call!"}
		    end
	    end
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


is_guard_expr(Node) ->
    As = refac_syntax:get_ann(Node),
    case lists:keysearch(category,1, As) of
	{value, {category, guard_expression}} ->
	    true;
	_  -> false
    end.

do_fun_extraction(AnnAST, ExpList, NewFunName, ParNames, VarsToExport, EnclosingFunName, EnclosingFunArity) ->
    NewFunName1 = refac_syntax:atom(NewFunName),
    Pars = lists:map(fun(P) ->refac_syntax:variable(P) end, ParNames),
    ExpList1 = case VarsToExport of
		   [] -> ExpList;
		   [V] -> E = refac_syntax:variable(V),
			  ExpList++[E];
		   [_V|_Vs] -> E = refac_syntax:tuple([refac_syntax:variable(V)||V<-VarsToExport]),
			       ExpList ++ [E]
	       end,
    Clause = refac_syntax:clause(Pars, [], ExpList1),
    NewFun = refac_syntax:function(NewFunName1, [Clause]),
    Forms = refac_syntax:form_list_elements(AnnAST),
    Fun = fun(Form) ->
		  case refac_syntax:type(Form) of 
		      function -> Name = refac_syntax:atom_value(refac_syntax:function_name(Form)),
				  Arity = refac_syntax:function_arity(Form),
				  case {Name, Arity} == {EnclosingFunName, EnclosingFunArity} of
				      true -> 
					  Form1 = replace_expr_with_fun_call(Form, ExpList, NewFunName, ParNames, VarsToExport), 
					  [Form1, NewFun];
				      _ -> [Form]
				  end;
		      _ -> [Form]
		  end
	  end,
    refac_syntax:form_list([F||Form<-Forms, F <-Fun(Form)]).


replace_expr_with_fun_call(Form, ExpList, NewFunName, ParNames, VarsToExport) ->
    Op = refac_syntax:operator(NewFunName),
    Pars = [refac_syntax:variable(P)|| P <-ParNames],
    FunCall= refac_syntax:application(Op, Pars),
    NewExpr = case length(VarsToExport) of 
		  0  ->  FunCall;
		  1 -> Pats = refac_syntax:variable(hd(VarsToExport)),
		       refac_syntax:match_expr(Pats, FunCall);
		  _ -> Pats = refac_syntax:tuple([refac_syntax:variable(V) || V <- VarsToExport]),
		       refac_syntax:match_expr(Pats, FunCall)
	      end,
    case (length(ExpList)==1) andalso (refac_syntax:type(hd(ExpList))=/=match_expr) of
	true -> {Form1, _} =refac_util:stop_tdTP(fun do_replace_expr_with_fun_call_1/2, Form, {NewExpr, hd(ExpList)}),
		Form1;
	_ ->    {Form1, _} =refac_util:stop_tdTP(fun do_replace_expr_with_fun_call_2/2, Form, {NewExpr, ExpList}),
		Form1

    end.
    
do_replace_expr_with_fun_call_1(Tree, {NewExpr, Expr}) ->
    Range = refac_util:get_range(Expr),
    case refac_util:get_range(Tree) of
	Range -> {NewExpr, true};
	_  -> {Tree, false}
    end.
    
	    
do_replace_expr_with_fun_call_2(Tree, {NewExpr, ExpList}) ->
    Range1 = refac_util:get_range(hd(ExpList)),
    %%{StartPos1, _EndPos} = Range1,
    Range2 = refac_util:get_range(lists:last(ExpList)),
    %%{_StartPos2, EndPos2} = Range2,
    %%Range3 = {StartPos1, EndPos2},
    case refac_syntax:type(Tree) of
	clause ->
	    Exprs = refac_syntax:clause_body(Tree),
	    {Exprs1, Exprs2} = lists:splitwith(fun(E) -> refac_util:get_range(E) =/= Range1 end, Exprs),
	    {NewBody, Modified} = case Exprs2 of 
				      [] -> {Exprs, false};
				      _ -> {_Exprs21, Exprs22} = lists:splitwith(fun(E) -> refac_util:get_range(E) =/= Range2 end, Exprs2),
					   case Exprs22 of 
					       [] -> {Exprs, false}; %% THIS SHOULD NOT HAPPEN.
					       _ -> {Exprs1 ++ [NewExpr|tl(Exprs22)], true}
					   end
				  end,
	    Pats = refac_syntax:clause_patterns(Tree),
	    G    = refac_syntax:clause_guard(Tree),
	    {refac_syntax:copy_pos(Tree, refac_syntax:copy_attrs(Tree, refac_syntax:clause(Pats, G, NewBody))), Modified};
	block_expr -> 
	    Exprs = refac_syntax:block_expr_body(Tree),
	    {Exprs1, Exprs2} = lists:splitwith(fun(E) -> refac_util:get_range(E) =/= Range1 end, Exprs),
	    {NewBody, Modified} = case Exprs2 of 
				      [] -> {Exprs, false};
				      _ -> {_Exprs21, Exprs22} = lists:splitwith(fun(E) -> refac_util:get_range(E) =/= Range2 end, Exprs2),
					   case Exprs22 of 
					       [] -> {Exprs, false}; %% THIS SHOULD NOT HAPPEN.
					       _ -> {Exprs1 ++ [NewExpr|tl(Exprs22)], true}
					   end
				  end,
	    {refac_syntax:copy_pos(Tree, refac_syntax:copy_attrs(Tree, refac_syntax:block_expr(NewBody))), Modified};	    
	_ -> {Tree, false}
    end.

envs_bounds_frees(Node) ->
    As = refac_syntax:get_ann(Node),
    BdVars = case lists:keysearch(bound,1,As) of
		 {value, {bound, BdVars1}} ->
		     BdVars1;
		 _ -> []
	     end,
    FrVars = case lists:keysearch(free,1, As) of 
		 {value, {free, FrVars1}} ->
		     FrVars1;
		 _ -> []
	     end,
    {{bound, BdVars},{free, FrVars}}.


vars_to_export(Fun,ExprEndPos, ExprBdVars, _ExpList) ->
   %% LastExpr = lists:last(ExpList),
    AllVars = collect_vars(Fun),
    ExprBdVarsPos = lists:map(fun({_Var, Pos}) -> Pos end, ExprBdVars),
    VarsToExport = lists:usort([V || {V, SourcePos, DefPos} <- AllVars,
			      SourcePos > ExprEndPos,
			      lists:subtract(DefPos, ExprBdVarsPos) == []]),
   %% io:format("VarsToExport:\n~p\n",[VarsToExport]),
    VarsToExport.

collect_vars(Tree) ->
     F= fun(T, S) ->
 		   case refac_syntax:type(T) of 
 		       variable ->
			   SourcePos = refac_syntax:get_pos(T),
			   {value, {def, DefinePos}} = lists:keysearch(def, 1, refac_syntax:get_ann(T)),
			   VarName = refac_syntax:variable_name(T),
			   S++[{VarName, SourcePos, DefinePos}];
		       _  -> S
 		   end
	    end,
    refac_syntax_lib:fold(F, [], Tree).



%% The following functions should be combined with those in 'refac_expr_search.erl'

filter_exprs_via_ast(Tree, ExpList) ->
    F = fun(T, Acc) ->
		case refac_syntax:type(T) of
		    clause -> Exprs = refac_syntax:clause_body(T), 
			      Acc ++ [Exprs];
		    block_expr -> Exprs = refac_syntax:block_expr_body(T),
				  Acc++ [Exprs];    
		    _  -> Acc
		end
	end,
    AllExprSeqs = lists:flatten(refac_syntax_lib:fold(F, [], Tree)),
    case lists:subtract(ExpList, AllExprSeqs) of 
	[] ->  [];
	_  -> ExpList
    end.
		    

%% get the list sequence of expressions contained in Tree between locations Start and End.
pos_to_expr(FName, Tree, {Start, End}) ->
    {ok, Toks} = refac_epp:scan_file(FName, [], []),
    Exprs = pos_to_expr(Tree, {Start, End}),
    filter_exprs_via_toks(Toks, Exprs).
 
filter_exprs_via_toks(_Toks, []) ->
    [];
filter_exprs_via_toks(_Toks, [E]) ->
    [E];
filter_exprs_via_toks(Toks, [E1,E2|Es]) ->
    {_StartLoc, EndLoc} = refac_util:get_range(E1),
    {StartLoc1, _EndLoc1} = refac_util:get_range(E2),
    Toks1 = lists:dropwhile(fun(T) ->
				    token_loc(T) =< EndLoc end, Toks),
    Toks2 = lists:takewhile(fun(T) ->
				    token_loc(T) < StartLoc1 end, Toks1),
    case lists:any(fun(T) -> token_val(T) =/= ',' end, Toks2) of 
	false ->
	    [E1]++ filter_exprs_via_toks(Toks, [E2|Es]);
	_  -> [E1]
    end.
	
%% get the list of expressions contained in Tree between locations Start and End.		
pos_to_expr(Tree, {Start, End}) ->
    {S, E} = refac_util:get_range(Tree),
    if (S >= Start) and (E =< End) ->
	    case refac_util:is_expr(Tree) of
		true -> [Tree];
		_ ->
		    Ts = refac_syntax:subtrees(Tree),
		    R0 = [[pos_to_expr(T, {Start, End}) || T <- G]
			  || G <- Ts],
		    lists:flatten(R0)
	    end;
       (S > End) or (E < Start) -> [];
       (S < Start) or (E > End) ->
	    Ts = refac_syntax:subtrees(Tree),
	    R0 = [[pos_to_expr(T, {Start, End}) || T <- G]
		  || G <- Ts],
	    lists:flatten(R0);
       true -> []
    end.

token_loc(T) ->
      case T of 
	{_, L, _V} -> L;
	{_, L1} -> L1
      end.

%% get the value of a token.
token_val(T) ->
    case T of 
	{_, _, V} -> V;
	{V, _} -> V
    end.
