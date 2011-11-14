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
%%       names of its contributors may be used to endoorse or promote products
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
%%@doc Unfold a function application to an instance of the function's body.
%% <p> This refactoring replaces a function application with an instance of the function body.
%% With the current implementation, Wrangler unfolds a function application only if the function 
%% is defined in the same module, and Wrangler could work out which function clause to use (in case 
%% the function definition contains multiple function clauses).
%% </p>
%% Some remarks about the implementation:
%% 1) This refactoring does automatic variable renaming of variables decared in the function 
%% to be inlined if there would be a name capture/conflict without doing so;
%% 2) In the case that the function definition has multiple clause, Wrangler tries to work out 
%% which function clause to inline by matching the actual and formal parameters; but only do 
%% the unfolding if Wrangler is certain which clause to unfold;
%% 3) Because Erlang is a strict language, Wrangler needs to make sure that actual parameters 
%% are evaluated only once.
%%
%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%% =============================================================================================

%% =============================================================================================
%% @private
-module(refac_unfold_fun_app).

-export([unfold_fun_app/5, 
         unfold_fun_app_eclipse/4]).


-import(wrangler_code_search_utils, [identifier_name/1]).

-include("../include/wrangler_internal.hrl").

%% =============================================================================================
%% <p>
%% Usage: Point the cursor to the function name in the function application to unfold, then 
%% select <em>Unfold Function Application</em> from <em>Refactor</em>.
%% </p>

%%-spec(unfold_fun_app_eclipse/4::(FileName::filename(), Pos::pos(), SearchPaths::[dir()], TabWidth::integer())
%%      ->{ok, [{filename(), filename(), string()}]}).
unfold_fun_app_eclipse(FileName,Pos,SearchPaths, TabWidth) ->
    unfold_fun_app(FileName, Pos, SearchPaths, eclipse, TabWidth).

unfold_fun_app(FName, Pos = {Line, Col}, SearchPaths, Editor, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:unfold_fun_app(~p, {~p,~p}, ~p, ~p).\n",
		 [?MODULE, FName, Line, Col, SearchPaths, TabWidth]),
    Cmd = "CMD: " ++ atom_to_list(?MODULE) ++ ":unfold_fun_app(" ++ "\"" ++ 
	    FName ++ "\", {" ++ integer_to_list(Line) ++ ", " ++ integer_to_list(Col) ++ "}," ++ 
	      "[" ++ wrangler_misc:format_search_paths(SearchPaths) ++ "]," ++ integer_to_list(TabWidth) ++ ").",
    {ok, {AnnAST, Info}} = wrangler_ast_server:parse_annotate_file(FName, true, SearchPaths, TabWidth),
    {value, {module, ModName}} = lists:keysearch(module, 1, Info),
    case pos_to_fun_clause_app(AnnAST, Pos) of
	{ok, {Clause, App}} ->
	    {ok, {FunClause, {Subst, MatchExprs}}} = side_cond_analysis(ModName, AnnAST, App),
	    SubstLocs = [Loc || {_, Loc, _} <- Subst],
	    Subst1 = [{Loc, P2} || {_P1, Loc, P2} <- Subst],
	    {FunClause1, MatchExprs1} = auto_rename_vars({FunClause, MatchExprs}, {Clause, App}, SubstLocs),
	    UsedRecords = wrangler_misc:collect_used_records(FunClause),
	    fun_inline_1(FName, AnnAST, Pos, {FunClause1, Subst1, MatchExprs1},
			 {Clause, App}, UsedRecords, Editor, TabWidth, Cmd);
	{error, _} -> throw({error, "You have not selected a function application, "
				    "or the function containing the function application selected does not parse."})
    end.


side_cond_analysis(ModName, AnnAST, App) ->
    Op = wrangler_syntax:application_operator(App),
    Args = wrangler_syntax:application_arguments(App),
    Arity = length(Args),
    case lists:keysearch(fun_def, 1, wrangler_syntax:get_ann(Op)) of
      {value, {fun_def, {ModName, FunName, Arity, _, _}}} ->
	  Fs = wrangler_syntax:form_list_elements(AnnAST),
	  Res = [F || F <- Fs, wrangler_syntax:type(F) == function,
		      case lists:keysearch(fun_def, 1, wrangler_syntax:get_ann(F)) of
			{value, {fun_def, {ModName, FunName, Arity, _, _}}} -> true;
			_ -> false
		      end],
	    case Res of
	      [FunDef] -> side_cond_analysis_1(FunDef, App, AnnAST);
	      [] ->
		  throw({error, "The function to be inlined is not defined."});
	      [_| _] ->
		  throw({error, "The function to be inlined has been defined more than once."})
	  end;
	{value, {fun_def, {'_', _, _, _, _}}} ->
	    throw({error, "The function to be inlined is not defined."});
	{value, {fun_def, {_M, _F, _A, _, _}}} ->
	    throw({error, "Inlining a function defined in another module is not supported yet."});
      _ -> throw({error, "Sorry, Wrangler could not figure out where the function to be inlined is defined."})
    end.

side_cond_analysis_1(FunDef, App, AnnAST) ->
    Fs = wrangler_syntax:form_list_elements(AnnAST),
    Args = wrangler_syntax:application_arguments(App),
    Cs = wrangler_syntax:function_clauses(FunDef),
    try
	find_matching_clause(Cs, Args)
    of
	none -> throw({error, "The function to be inlined has multiple clauses, "
			      "and Wrangler could not figure out which function clause to inline."});
	{C, {Subst, MatchExprs}} ->
	    UsedMacros = wrangler_misc:collect_used_macros(C),
	    CPos = wrangler_syntax:get_pos(C),
	    AppPos = wrangler_syntax:get_pos(App),
	    case check_macro_defs(Fs, UsedMacros, lists:min([CPos, AppPos]), lists:max([CPos, AppPos])) of
		[] ->
		    {ok, {C, {Subst, MatchExprs}}};
		Ms -> return_error_msg(Ms)
	    end
    catch
	throw:E2 ->
	    throw(E2)
    end.
 
return_error_msg(Ms) ->
    MsStr = format(Ms),
    case length(Ms) of
	1 ->
	    Str = "Macro " ++ MsStr ++ " used by the function clause to be inlined is defined/undefined "
		"between the function application to be unfolded and the function clause.",
	    throw({error, Str});
      _ ->
	    Str = "Macros: " ++ MsStr ++ ", used by the function clause to be inlined is defined/undefined "
				       "between the function application to be unfolded and the function clause.",
	    throw({error, Str})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Find the function clause to be inlined by matching actual and formal parameters.
find_matching_clause([], _Ps) -> none;
find_matching_clause([C], Ps) ->
    case find_matching_clause_1(C, Ps, true) of 
	no_match -> 
	    none;
	no_more_match -> none;
	Res ->
	    case scrutinse_subst(Res) of 
		none ->
		    none;
		{Subst, MatchExprs} ->
		    {C, {Subst, MatchExprs}}
	    end
    end;
find_matching_clause([C|Cs], Ps) ->
    case find_matching_clause_1(C, Ps, false) of
	no_match ->
	    find_matching_clause(Cs, Ps);
	no_more_match ->
	    none;
	Res ->
	    case scrutinse_subst(Res) of
		none -> 
		    find_matching_clause(Cs, Ps);
		{Subst, MatchExprs} ->
		    {C, {Subst, MatchExprs}}
	    end		
    end.

%% to handle the case when the same pattern occurs more than once in the format parameters;
%% in this case, Wrangler needs to make sure all the occurrences are bound to the same thing.
scrutinse_subst(Res) ->
    SubSt = [{P1, P2} || {P1, _Loc, P2} <- Res],
    SubSt1 = [{P1, Loc, P2} || {P1, Loc, P2} <- Res],
    MatchExprs = Res -- SubSt1,
    MatchExprs1 = [{wrangler_syntax:match_expr_pattern(M),
		    wrangler_syntax:match_expr_body(M)}
		   || M <- MatchExprs],
    StrRep = lists:usort([{wrangler_prettypr:format(E1), wrangler_prettypr:format(E2)}
			  || {E1, E2} <- SubSt ++ MatchExprs1]),
    Fst = lists:usort(element(1, lists:unzip(StrRep))),
    case length(Fst) < length(StrRep) of
      true ->
	  none;
      _ ->
	  {SubSt1, MatchExprs}
    end.



find_matching_clause_1(C, AppPs, IsLastClause) ->
    DefPs = wrangler_syntax:clause_patterns(C),
    G = wrangler_syntax:clause_guard(C),
    case G of
      none -> match_patterns(DefPs, AppPs, IsLastClause);
      _ when IsLastClause ->
	  throw({error, "Inlining of a function clause with guard expression(s) is not supported yet."});
      _ -> no_more_match
    end.

match_patterns(DefPs, AppPs, IsLastClause) ->
    try
      do_match_patterns(DefPs, AppPs, IsLastClause)
    of
      Subst -> Subst
    catch
      throw:E2 -> E2
    end.

do_match_patterns(DefP, AppP, IsLastClause)
    when is_list(DefP) andalso is_list(AppP) ->
    case length(DefP) == length(AppP) of
      false ->
	  throw(no_match);
      true ->
	  case DefP of
	    [] ->
		[];
	    _ ->
		lists:append([do_match_patterns(P1, P2, IsLastClause)
			      || {P1, P2} <- lists:zip(DefP, AppP)])
	  end
    end;

do_match_patterns(DefP, _AppP, _) when is_list(DefP) ->
    throw(no_more_match);
do_match_patterns(_DefP, AppP, _) when is_list(AppP) ->
    throw(no_more_match);
do_match_patterns(DefP, AppP, IsLastClause) ->
    T1 = wrangler_syntax:type(DefP),
    T2 = wrangler_syntax:type(AppP),
    case T1 == T2 of
	false -> match_pattern_of_same_type(DefP, AppP, IsLastClause);
	true -> match_pattern_of_different_type(DefP, AppP,IsLastClause)
    end.

match_pattern_of_same_type(DefP, AppP, IsLastClause) ->
    case wrangler_syntax:type(DefP) of
	variable ->
	    case is_non_reducible_term(AppP) of
		true ->
		    Ann = wrangler_syntax:get_ann(DefP),
		    case lists:keysearch(def, 1, Ann) of
			{value, {def, DefinePos}} ->
			    [{DefP, DefinePos, AppP}];
			_ -> [wrangler_syntax:match_expr(DefP, AppP)]
		    end;
		false ->
		    [wrangler_syntax:match_expr(DefP, AppP)]
	    end;
	underscore ->
	    [];
	_
	  when
	      IsLastClause   %% The last function clause; do an enforced match;
	                   ->
	    [wrangler_syntax:match_expr(DefP, AppP)];
        _ -> case wrangler_syntax:is_literal(AppP) of
		 true -> throw(no_match);
		 _ -> throw(no_more_match) %% stop here; no more further match needed.
	     end
    end.

match_pattern_of_different_type(DefP, AppP, IsLastClause) ->
    T1 = wrangler_syntax:type(DefP),
    case wrangler_syntax:is_literal(DefP) andalso wrangler_syntax:is_literal(AppP) of
	true ->
	    case
		wrangler_syntax:concrete(DefP) == wrangler_syntax:concrete(AppP)
	    of
		true ->
		    [];
		_ -> throw(no_match)   %% should continue matching the next clause;
	    end;
	false ->
	    case T1 of
		variable ->
		    case {is_macro_name(DefP), is_macro_name(AppP)} of
			{true, true} ->
			    case identifier_name(DefP) == identifier_name(AppP) of
				true ->
				    [];
				false -> throw(no_match)  %% should continue matching the next clause;
			    end;
			{false, false}  %% both are variables;
			               ->
                            Ann = wrangler_syntax:get_ann(DefP),
			    case lists:keysearch(def, 1, Ann) of
				{value, {def, DefinePos}} ->
				    [{DefP, DefinePos, AppP}];
				%% this should not happen;
				_ -> [wrangler_syntax:match_expr(DefP, AppP)]
			    end
		    end;
		underscore -> [];
		_ -> case wrangler_syntax:is_leaf(DefP) of
			 true ->
			     throw(no_match);  %% should continue matching the next clause;
			 _ -> do_match_a_pattern(DefP, AppP, IsLastClause)
		     end
	    end
    end.

do_match_a_pattern(P1, P2, IsLastClause) ->
    SubPats1 = wrangler_syntax:subtrees(P1),
    SubPats2 = wrangler_syntax:subtrees(P2),
    try
	do_match_patterns(SubPats1, SubPats2, IsLastClause)
    of
	Subst -> Subst
    catch
	throw:E ->
	    throw(E)
    end.


is_non_reducible_term(T) ->
    lists:member(wrangler_syntax:type(T), [variable, fun_expr])
       orelse wrangler_syntax:is_literal(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


fun_inline_1(FName, AnnAST, Pos, {FunClauseToInline, Subst, MatchExprsToAdd}, {Clause, App},
	     UsedRecords, Editor, TabWidth, Cmd) ->
    B = wrangler_syntax:clause_body(FunClauseToInline),
    {SubstedBody1, _} = lists:unzip([api_ast_traverse:stop_tdTP(fun do_subst/2, E, Subst) || E <- B]),
    SubstedBody = MatchExprsToAdd ++ SubstedBody1,
    Fs = wrangler_syntax:form_list_elements(AnnAST),
    RecordDefs = collect_record_defs(Fs, UsedRecords, Pos),
    Fs0 = Fs -- RecordDefs,
    Fs1 = lists:append([do_inline(F, Pos, Clause, App, SubstedBody, RecordDefs) || F <- Fs0]),
    AnnAST1 = wrangler_misc:rewrite(AnnAST, wrangler_syntax:form_list(Fs1)),
    wrangler_write_file:write_refactored_files([{{FName,FName}, AnnAST1}], Editor, TabWidth, Cmd).

do_inline(Form, Pos, _Clause, App, SubstedBody, RecordDefs) ->
    SubstedBody1 = wrangler_misc:reset_ann_and_pos(SubstedBody),
    {S, E} = wrangler_misc:start_end_loc(Form),
    if (S =< Pos) and (Pos =< E) ->
            {NewForm, _} = api_ast_traverse:stop_tdTP(
                            fun do_inline_1/2, Form, {App, SubstedBody1}),
            case length(SubstedBody) > 1 of
                true ->
		   {NewForm1, _} = api_ast_traverse:stop_tdTP(
                                     fun remove_begin_end/2, NewForm, SubstedBody1),
                   RecordDefs ++ [NewForm1];
                _ -> RecordDefs ++ [NewForm]
            end;
       true ->
            [Form]
    end.

do_inline_1(Node, {App, SubstedBody}) ->
    case Node of
	App ->
            case SubstedBody of 
                [B] ->
                    {wrangler_misc:rewrite_with_wrapper(App, B), true};
                [B|Bs] ->
                        {wrangler_misc:rewrite_with_wrapper(
                             App, wrangler_syntax:block_expr([B|Bs])),
                         true}
            end;
	_ -> {Node, false}
    end.

remove_begin_end(Node, BlockBody) ->
    Fun = fun (E) ->
		  case wrangler_syntax:type(E) of
                      fake_parentheses ->
                          B=wrangler_syntax:fake_parentheses_body(E),
                          case wrangler_syntax:type(B) of
                              block_expr ->
                                  [B1|Bs] = wrangler_syntax:block_expr_body(B),
                                  [wrangler_misc:rewrite_with_wrapper(E, B1)
                                   |Bs];
                              _ -> [E]
                          end;
  		      match_expr ->
			  Ps = match_expr_patterns(E),
			  B = match_expr_body(E),
                        case wrangler_syntax:type(B) of
                            fake_parentheses ->
                                FB=wrangler_syntax:fake_parentheses_body(B),
                                case wrangler_syntax:type(FB) of
                                    block_expr ->
                                        [FB1|FBs] = wrangler_syntax:block_expr_body(FB),
                                        Last = lists:last(FBs),
                                        NewLast = make_match_expr([wrangler_misc:reset_ann(P) || P <- Ps], Last),
                                        NewFB1 =wrangler_misc:rewrite_with_wrapper(Ps, FB1),
                                        FBs1 = lists:reverse(FBs),
                                        [NewFB1|lists:reverse([NewLast|tl(FBs1)])];
                                    _ -> [E]
                                end;
                            _ -> [E]
                        end;
                      _ -> [E]
		  end
	  end,
    case wrangler_syntax:type(Node) of
	clause ->
	    P = wrangler_syntax:clause_patterns(Node),
	    G = wrangler_syntax:clause_guard(Node),
	    B = wrangler_syntax:clause_body(Node),
	    B1 = lists:append([Fun(E) || E <- B]),
	    {wrangler_misc:rewrite(Node, wrangler_syntax:clause(P, G, B1)),
	     length(B) =/= length(B1)};
	block_expr ->
	    Es = wrangler_syntax:block_expr_body(Node),
	    case Es of
		BlockBody ->
		    {Node, false};
		_ ->
		    Es1 = lists:append([Fun(E) || E <- Es]),
		    {wrangler_misc:rewrite(Node, wrangler_syntax:block_expr(Es1)),
		     length(Es) =/= length(Es1)}
	    end;
	try_expr ->
	    B = wrangler_syntax:try_expr_body(Node),
	    B1 = lists:append([Fun(E) || E <- B]),
	    Cs = wrangler_syntax:try_expr_clauses(Node),
	    Handlers = wrangler_syntax:try_expr_handlers(Node),
	    After = wrangler_syntax:try_expr_after(Node),
	    {wrangler_misc:rewrite(Node, wrangler_syntax:try_expr(B1, Cs, Handlers, After)),
	     length(B) =/= length(B1)};
	_ ->
	    {Node, false}
    end.

do_subst(Node, Subst) ->
    case wrangler_syntax:type(Node) of
	variable ->
	    As = wrangler_syntax:get_ann(Node),
	    case lists:keysearch(def, 1, As) of
		{value, {def, DefinePos}} ->
		    case lists:keysearch(DefinePos, 1, Subst) of
			{value, {DefinePos, Expr}} ->
			    {wrangler_misc:rewrite_with_wrapper(Node, Expr), true};
			_ -> {Node, false}
		    end;
		_ -> {Node, false}
	    end;
	_ -> {Node, false}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%% From source postion to the function name part in a function application.
pos_to_fun_clause_app(Node, Pos) ->
    case
      api_ast_traverse:once_tdTU(fun pos_to_fun_clause_app_1/2, Node, Pos)
	of
      {_, false} -> {error, none};
      {{C, App}, true} -> {ok, {C, App}}
    end.

pos_to_fun_clause_app_1(Node, Pos) ->
    case wrangler_syntax:type(Node) of
	function ->
	    {S, E} = wrangler_misc:start_end_loc(Node),
	    if (S =< Pos) and (Pos =< E) ->
		   Cs = wrangler_syntax:function_clauses(Node),
		   [C] = [C1 || C1 <- Cs,
				{S1, E1} <- [wrangler_misc:start_end_loc(C1)],
				S1 =< Pos, Pos =< E1],
		   case pos_to_fun_app(C, Pos) of
		       {_, false} -> throw({error, "You have not selected a function application, "
                                            "or the function containing the function "
                                            "application selected does not parse."});
		       {App, true} ->
			   {{C, App}, true}
		   end;
	       true -> {[], false}
	    end;
	_ ->
	    {[], false}
    end.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pos_to_fun_app(Node, Pos) ->
    api_ast_traverse:once_tdTU(fun pos_to_fun_app_1/2, Node, Pos).

pos_to_fun_app_1(Node, Pos) ->
    case wrangler_syntax:type(Node) of
	application ->
	    Op = wrangler_syntax:application_operator(Node),
	    {S, E} = wrangler_misc:start_end_loc(Op),
	    if (S =< Pos) and (Pos =< E) ->
		   {Node, true};
	       true -> {[], false}
	    end;
	_ -> {[], false}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
auto_rename_vars({ClauseToInline, MatchExprs}, {Clause, App}, SubStLocs) ->
    VarName = 'WRANGLER_TEMP_VAR',
    NewVarPat = wrangler_syntax:copy_pos(App, wrangler_syntax:copy_pos(App, wrangler_syntax:variable(VarName))),
    MatchExpr = wrangler_syntax:copy_pos(App, wrangler_syntax:match_expr(NewVarPat, wrangler_syntax:atom(ok))),
    {Clause1, _} = api_ast_traverse:stop_tdTP(fun do_replace_app_with_match/2, Clause, {App, MatchExpr}),
    Clause2 = wrangler_syntax_lib:var_annotate_clause(wrangler_misc:reset_ann(Clause1), [], {[],[]}, []),
    BdsInFunToInline = get_bound_vars(ClauseToInline),
    NewNames = [{Name, DefinePos} || {Name, DefinePos} <- BdsInFunToInline,
				      not  lists:member(DefinePos, SubStLocs)],
    Pos = wrangler_syntax:get_pos(App),
    VarsToRename = get_vars_to_rename(Clause2, [Pos], VarName, NewNames, ClauseToInline),
    UsedVarNames = ordsets:from_list(wrangler_misc:collect_var_names(Clause)),
    do_rename_var({ClauseToInline, MatchExprs}, lists:usort(VarsToRename), UsedVarNames).

do_replace_app_with_match(Node, {App, MatchExpr}) ->
    case Node of
	App ->
	    {wrangler_misc:rewrite_with_wrapper(App, MatchExpr), true};
	_ -> {Node, false}
    end.

get_vars_to_rename(Clause, Pos, VarName, NewNames, ClauseToInline) ->
    [{Name, DefinePos} || {Name, P} <- NewNames,
			  refac_rename_var:cond_check(Clause, Pos, VarName, Name) =/= {false, false, false},
			  {ok, {_, DefinePos}} <- [api_interface:pos_to_var_name(ClauseToInline, P)]].

do_rename_var({Node, MatchExprs}, [], _UsedVarNames) ->
    {Node, [wrangler_misc:reset_ann(M) || M <- MatchExprs]};
do_rename_var({Node, MatchExprs}, [V| Vs], UsedVarNames) ->
    {Node1, MatchExprs1} = do_rename_var_1({Node, MatchExprs}, V, UsedVarNames),
    do_rename_var({Node1, MatchExprs1}, Vs, UsedVarNames).

do_rename_var_1({Node, MatchExprs}, {VarName, DefLoc}, UsedVarNames) ->
    UsedVarNames1 = ordsets:union(ordsets:from_list(wrangler_misc:collect_var_names(Node)), UsedVarNames),
    NewVarName = api_refac:make_new_name(VarName, UsedVarNames1),
    {Node1, _} = refac_rename_var:rename(Node, DefLoc, NewVarName),
    MatchExprs1 = do_rename_in_match_exprs(MatchExprs, DefLoc, NewVarName),
    {Node1, MatchExprs1}.

do_rename_in_match_exprs(MatchExprs, DefLoc, NewVarName) ->
    [do_rename_in_match_expr_1(M, DefLoc, NewVarName) || M <-MatchExprs].

do_rename_in_match_expr_1(MatchExpr, DefLoc, NewVarName) ->
    P = wrangler_syntax:match_expr_pattern(MatchExpr),
    B = wrangler_syntax:match_expr_body(MatchExpr),
    {P1, _} = refac_rename_var:rename(P, DefLoc, NewVarName),
    wrangler_syntax:match_expr(P1, B).
    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Note that In Erlang a match expression in the form of  'P1 = P2 ... = Pn = E' is allowed.
match_expr_patterns(E) ->
    case wrangler_syntax:type(E) of
	match_expr ->
	    P = wrangler_syntax:match_expr_pattern(E),
	    B = wrangler_syntax:match_expr_body(E),
	    [P|match_expr_patterns(B)];
	_ ->[]
    end.
match_expr_body(E) ->
    case wrangler_syntax:type(E) of
	match_expr ->
	    B = wrangler_syntax:match_expr_body(E),
	    match_expr_body(B);
	_ -> E
    end.

make_match_expr(Ps, Body) ->	
    make_match_expr_1(lists:reverse(Ps), Body).

make_match_expr_1([], Body) -> Body;
make_match_expr_1([P], Body) ->
    wrangler_syntax:match_expr(P, Body);
make_match_expr_1([P|Ps], Body) ->
    make_match_expr_1(Ps, wrangler_syntax:match_expr(P, Body)).
			  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

collect_record_defs(Fs, RecordTypes, Loc) ->
    [F || F<-Fs, collect_record_defs_1(F, RecordTypes, Loc)].

collect_record_defs_1(F, RecordTypes, Loc) ->
    Pos = wrangler_syntax:get_pos(F),
    case Pos < Loc of
	true ->
	    false;
	false ->
	    case wrangler_syntax:type(F) of
		attribute ->
		    Name = wrangler_syntax:attribute_name(F),
		    case wrangler_syntax:type(Name) == atom andalso
			wrangler_syntax:atom_value(Name) == record of
			true -> 
			    Type = hd(wrangler_syntax:attribute_arguments(F)),
			    case wrangler_syntax:type(Type) of
				atom -> 
				    lists:member(wrangler_syntax:atom_value(Type), RecordTypes);
				_ -> false
			    end;
			_ -> false
		    end;
		_ -> false
	    end
    end.
  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_macro_defs(_Fs, [], _StartLoc, _EndLoc) -> 
    [];
check_macro_defs(Fs, UsedMacros, StartLoc, EndLoc) ->
    Res =[check_macro_defs_1(F, UsedMacros, StartLoc, EndLoc) || F<-Fs],
    lists:usort([M|| {true, Ms}<-Res, M<-Ms]).

check_macro_defs_1(F, UsedMacros, StartLoc, EndLoc) ->
    Pos = wrangler_syntax:get_pos(F),
    case Pos =< StartLoc orelse Pos >= EndLoc of
	true ->
	    false;
	false ->
	    case is_attribute(F, define) orelse is_attribute(F, undef) of
		true ->
		    Args = wrangler_syntax:attribute_arguments(F),
		    MacroHead = wrangler_misc:ghead("refac_unfold_fun_app:check_macro_defs_1", Args),
		    MacroHead1 = case wrangler_syntax:type(MacroHead) of
				     application ->
					 wrangler_syntax:application_operator(MacroHead);
				     _ ->
					 MacroHead
				 end,
		    Name = case wrangler_syntax:type(MacroHead1) of
			       atom -> wrangler_syntax:atom_value(MacroHead1);
			       variable -> wrangler_syntax:variable_name(MacroHead1);
			       _ -> '_'
			   end,
		    case lists:member(Name, UsedMacros) of
			true ->
			    {true, [Name]};
			false -> false
		    end;
		_ -> false
	    end
    end.

is_attribute(F, Name) ->
    wrangler_syntax:type(F) == attribute andalso
      wrangler_syntax:type(wrangler_syntax:attribute_name(F)) == atom andalso
	wrangler_syntax:atom_value(wrangler_syntax:attribute_name(F)) == Name.
	    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
format([]) ->	   
     "";
format([M]) ->
    atom_to_list(M);
format([M|Ms]) ->
    atom_to_list(M) ++ ","++ format(Ms).
	
	    
get_bound_vars(Node) ->
    get_bound_vars_1(wrangler_syntax:get_ann(Node)).

get_bound_vars_1([{bound, B} | _Bs]) -> B;
get_bound_vars_1([_ | Bs]) -> get_bound_vars_1(Bs);
get_bound_vars_1([]) -> [].


is_macro_name(Exp) ->
    Ann = wrangler_syntax:get_ann(Exp),
    {value, {syntax_path, macro_name}} == 
        lists:keysearch(syntax_path, 1, Ann).


