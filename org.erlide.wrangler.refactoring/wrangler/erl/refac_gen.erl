%% Copyright (c) 2009, Huiqing Li, Simon Thompson
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
%% Refactoring: Generalise a function definition.
%%
%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%% 
%% =====================================================================
%%
%% @doc Generalise a function by selecting a sub-expression of the right-hand side and 
%% making this the value of a new argument added to the definition of the function. The 
%% sub-expression becomes the actual parameter at the call sites. 
%%
%% <p> To apply this refactoring, highlight the expression first, then  select 
%% <em> Generalise Function Definition </em> from the <em>Refactor</em> menu, after 
%% that the refactorer will prompt to enter the parameter name in the mini-buffer. </p>
%% 
%% <p> Here is an example of generalisation, in which the function <code> add_one </code> defined 
%% on  the left-hand side is generalised on the expression <code> 1 </code>, and the result is 
%% shown on the right-hand side. 
%%
%%        ```    -module (test).                          -module (test). 
%%               -export([f/1]).                          -export([f/1]).
%%        
%%               add_one ([H|T]) ->                       add_one (N, [H|T]) ->
%%                  [H+1 | add_one(T)];                      [H+N | add_one(N,T)];
%%               add_one ([]) -> [].                      add_one (N, []) -> [].
%%
%%               f(X) -> add_one(X).                      f(X) -> add_one(1,X)
%%        ''' 
%%  </p>
%% <p> In the case that the selected expression has a side-effect, the refactorer will wrap this expression 
%% in an function expression before passing it at the actual parameter to the call-sites. This is illustrated 
%% in the following example, in which function <code>repeat/1</code> is generalised on the expression 
%% <code>io:format("Hello\n")</code>.
%% 
%%         ```   -module (test).                          -module (test).                          
%%               -export([f/0]).                          -export([f/0]).
%%
%%               repeat(0) -> ok;                         repeat(A, 0) -> ok;
%%               repeat(N) ->                             repeat(A, N) ->
%%                 io:format("Hello\n"),                    A( ),
%%                 repeat(N-1).                             repeat(A,N-1).
%%
%%               f() -> repeat(5).                        f( ) -> 
%%                                                           repeat (fun( )->io:format ("Hello\n") end, 5).
%%          '''
%% </p>
%%
%% <p> This refactoring <em>only </em> affects the module in which the refactoring is initialised. In the case that 
%% the generalised function is exported by the module, an auxiliary function will be created 
%% to wrap the generalised function up, so that the module's interface is not changed.
%% </p>
%% <p> The following <em> side-condtions </em> apply to this refactoring:
%% <li> Supporse the function to be generalised is <code>foo/n </code>, then <code>foo/n+1</code> should not  
%% be in scope before the generalisation;</li>
%% <li> The selected expression should not contain locally declared variable(s), unless the selected expression 
%% has side effect; </li>
%% <li> The user-provided parameter name should not conflict with the existing parameters or
%% change the semantics of the function to be generalised; </li>
%% </p>
%% <p>
%% NOTE THAT in Erlang some literal expressions can not be replaced by variables. For example, the atom ```fields'''
%% in the experssion ```record_info(fields, Record)''' should not be replaced by a variable or other expressions.
%% This kind of checking is NOT supported by Wrangler yet.
%% 
%% </p>
%% @end
%% =====================================================================

-module(refac_gen).

-include("../include/wrangler.hrl").

-import(refac_rename_fun,[apply_style_funs/0, try_eval/4, collect_atoms/2]).

-export([generalise/6, gen_fun_1/11, gen_fun_clause/10]).

-export([generalise_eclipse/6, gen_fun_1_eclipse/11, gen_fun_clause_eclipse/10]).

-define(DEFAULT_RANGE, {?DEFAULT_LOC, ?DEFAULT_LOC}).
%% =====================================================================
-spec generalise(FileName::filename(),Start::pos(), End::pos(),ParName::string(),
		 SearchPaths::[dir()], TabWidth::integer()) ->
	     {ok, [filename()]}
		 |{multiple_instances, {atom(), atom(), integer(), pos(), syntaxTree(), boolean(),[{pos(), pos()}], string()}}
		 |{unknown_side_effect, {atom(), atom(),integer(), pos(), syntaxTree(), integer(),
					 [{pos(), pos()}], [{pos(),pos()}], string()}}
		 |{more_than_one_clause, {atom(), atom(), integer(), pos(), syntaxTree(), boolean(),
					  [{pos(), pos()}], [{pos(),pos()}], string()}}. 

generalise(FileName, Start, End, ParName, SearchPaths, TabWidth) ->
    generalise(FileName, Start, End, ParName, SearchPaths, TabWidth, emacs).

%% TO CHECK: Dialyzer says this function also returns {ok, [filename()]}, but it really shouldn't.
%% I don't know where goes wrong.
-spec generalise_eclipse(FileName::filename(), Start::pos(), End::pos(), ParName::string(),
	                         SearchPaths::[dir()], TabWidth::integer()) ->
        {ok, [filename()]} |
	{ok, [{filename(), filename(), string()}]} |
	{multiple_instances,  {ParName:: atom(), FunName::atom(), Arity::integer(),
			       FunDefPos::pos(), Exp::syntaxTree(), SideEffect::boolean(),
			       DupsInFun::[{pos(), pos()}], Cmd::string()}} |
	{unknown_side_effect, {ParName::atom(), FunName::atom(), Arity::integer(),
			       FunDefPos::pos(), Exp::syntaxTree(), NoOfClauses::integer(),
			       DupsInFun::[{pos(), pos()}], DupsInClause::[{pos(), pos()}],
			       Cmd::string()}} |
	{more_than_one_clause, {ParName::atom(), FunName::atom(), Arity::integer(),
				FunDefPos::pos(), Exp::syntaxTree(), SideEffect::boolean(),
				DupsInFun::[{pos(), pos()}], DupsInClause::[{pos(), pos()}],
				Cmd::string()}}.
generalise_eclipse(FileName, Start, End, ParName, SearchPaths, TabWidth) ->
    generalise(FileName, Start, End, ParName, SearchPaths, TabWidth, eclipse).
    
generalise(FileName, Start = {Line, Col}, End = {Line1, Col1}, ParName, SearchPaths, TabWidth, Editor) ->
    ?wrangler_io("\nCMD: ~p:generalise(~p, {~p,~p}, {~p,~p}, ~p,~p,~p).\n",
		 [?MODULE, FileName, Line, Col, Line1, Col1, ParName, SearchPaths, TabWidth]),
    Cmd = "CMD: " ++ atom_to_list(?MODULE) ++ ":generalise(" ++ "\"" ++
	FileName ++ "\", {" ++ integer_to_list(Line) ++	", " ++ integer_to_list(Col) ++ "},"++
	"{" ++ integer_to_list(Line1) ++ ", " ++ integer_to_list(Col1) ++ "},"  ++ "\"" ++ ParName ++ "\","
	++ "[" ++ refac_util:format_search_paths(SearchPaths) ++ "]," ++ integer_to_list(TabWidth) ++ ").",
    case refac_util:is_var_name(ParName) of
      false -> throw({error, "Invalid parameter name!"});
      true -> ok
    end,
    {ok, {AnnAST, Info}} = refac_util:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    {ok, ModName} = get_module_name(Info),
    case refac_util:pos_to_expr(AnnAST, Start, End) of
      {ok, Exp} -> Exp;
      {error, _} -> throw({error, "You have not selected an expression, "
				  "or the function containing the expression does not parse."}),
		    Exp = none
    end,
    case refac_util:expr_to_fun(AnnAST, Exp) of
      {ok, Fun} -> Fun;
      {error, _} -> throw({error, "You have not selected an expression within a function."}),
		    Fun = none
    end,
    NoOfClauses = length(refac_syntax:function_clauses(Fun)),
    FunName = refac_syntax:data(refac_syntax:function_name(Fun)),
    FunArity = refac_syntax:function_arity(Fun),
    Inscope_Funs = [{F, A} || {_M, F, A} <- refac_util:inscope_funs(Info)],
    NewArity = FunArity +1, 
    case lists:member({FunName, NewArity}, Inscope_Funs)  orelse
	erlang:is_builtin(erlang, FunName, NewArity) orelse
	erl_internal:bif(erlang, FunName, NewArity)
	of
	true -> throw({error, "Function " ++
		       atom_to_list(FunName) ++ "/" ++ 
		       integer_to_list(NewArity) 
		       ++ " is already in scope!"});
	false -> ok
    end,
    FunDefPos = get_fun_def_loc(Fun),
    ParName1 = list_to_atom(ParName),
    case gen_cond_analysis(Fun, Exp, ParName1) of
      {error, Reason} -> throw({error, Reason});
      ok -> ok
    end,
    SideEffect = check_side_effect(FileName, Exp, SearchPaths),
    DupsInFun = search_duplications(Fun, Exp),
    DupsInClause = search_duplications(expr_to_fun_clause(Fun, Exp), Exp),
    case SideEffect of
	unknown ->
	    {unknown_side_effect, {ParName1, FunName, FunArity,
				   FunDefPos, Exp, NoOfClauses, DupsInFun, DupsInClause, Cmd}};
	_ ->
	    case NoOfClauses > 1 of
		true ->
		    {more_than_one_clause,
		     {ParName1, FunName, FunArity, FunDefPos, Exp, SideEffect, DupsInFun, DupsInClause, Cmd}};
		_ ->
		    case DupsInFun of
			[_| _] ->
			    {multiple_instances,
			     {ParName1, FunName, FunArity, FunDefPos, Exp, SideEffect, DupsInFun, Cmd}};
			_ ->
			    {AnnAST1, _} = gen_fun(FileName, ModName, AnnAST, ParName1, FunName,
						   FunArity, FunDefPos, Info, Exp, SideEffect, [], SearchPaths, TabWidth),
			    case Editor of
				emacs ->
				    refac_util:write_refactored_files_for_preview([{{FileName, FileName}, AnnAST1}], Cmd),
				    {ok, [FileName]};
				eclipse ->
				    Content = refac_prettypr:print_ast(refac_util:file_format(FileName), AnnAST1),
				    {ok, [{FileName, FileName, Content}]}
			    end
		    end
	    end
    end.


-spec(gen_fun_1/11::(SideEffect::boolean(), FileName::filename(),ParName::atom(), FunName::atom(),
		     Arity::integer(), FunDefPos::pos(), Exp::syntaxTree(), SearchPaths::[dir()],
		     TabWidth::integer(), Dups::[{pos(), pos()}], LogCmd::string())
      -> {ok, [filename()]}).
gen_fun_1(SideEffect, FileName,ParName, FunName, Arity, DefPos, Exp, SearchPaths,TabWidth, Dups, LogCmd) ->
    gen_fun_1(SideEffect, FileName,ParName, FunName, Arity, DefPos, Exp, SearchPaths,TabWidth,Dups, emacs, LogCmd).

-spec(gen_fun_1_eclipse/11::(SideEffect::boolean(), FileName::filename(),ParName::atom(), FunName::atom(), 
			    Arity::integer(), FunDefPos::pos(), Expr::syntaxTree(), SearchPaths::[dir()],
			    TabWidth::integer(), Dups::[{pos(), pos()}], LogCmd::string()) 
      -> {ok, [{filename(), filename(),string()}]}).
gen_fun_1_eclipse(SideEffect, FileName,ParName, FunName, Arity, DefPos, Exp, SearchPaths,TabWidth, Dups, LogCmd) ->
    gen_fun_1(SideEffect, FileName,ParName, FunName, Arity, DefPos, Exp, SearchPaths, TabWidth, Dups, eclipse,LogCmd).

gen_fun_1(SideEffect, FileName,ParName, FunName, Arity, DefPos, Exp, SearchPaths,TabWidth, Dups, Editor, LogCmd) ->
    {ok, {AnnAST, Info}} = refac_util:parse_annotate_file(FileName,true, [],TabWidth),  
    {ok, ModName} = get_module_name(Info),
    AnnAST1 = case to_keep_original_fun(FileName, AnnAST, ModName, FunName, Arity, Exp, Info) of
		  true -> add_function(ModName, AnnAST, FunName,DefPos, Exp, SideEffect);
		  false -> AnnAST
	      end,		       
    ActualPar =  make_actual_parameter(ModName, Exp, SideEffect),
    {AnnAST2, _} = refac_util:stop_tdTP(fun do_gen_fun/2, AnnAST1, 
					{FileName, ParName, FunName, Arity, DefPos,Info,
					 Exp, ActualPar, SideEffect, Dups, SearchPaths, TabWidth}),
    case Editor of 
	emacs ->
	    refac_util:write_refactored_files_for_preview([{{FileName,FileName}, AnnAST2}], LogCmd),
	    {ok, [FileName]};
	eclipse ->
	    Content = refac_prettypr:print_ast(refac_util:file_format(FileName),AnnAST2),
	    {ok, [{FileName, FileName, Content}]}
    end.


-spec(gen_fun_clause_eclipse/10::(FileName::filename(), ParName::atom(), FunName::atom(), Arity::integer(), DefPos::pos(), 
				 Exp::syntaxTree(), TabWidth::integer(), SideEffect::boolean(),  Dups::[{pos(), pos()}], LogCmd::string()) ->
					{ok, [{filename(), filename(), string()}]}).
gen_fun_clause_eclipse(FileName, ParName, FunName, Arity, DefPos, Exp, TabWidth, SideEffect, Dups, LogCmd) ->
    gen_fun_clause_1(FileName, ParName, FunName, Arity, DefPos, Exp, [], TabWidth, SideEffect, Dups, eclipse, LogCmd).

-spec(gen_fun_clause/10::(FileName::filename(), ParName::atom(), FunName::atom(), Arity::integer(), DefPos::pos(), 
			 Exp::syntaxTree(), TabWidth::integer(), SideEffect::boolean(), 
			 Dups::[{{integer(), integer()},{integer(), integer()}}], LogCmd::string()) ->{ok, [filename()]}).

gen_fun_clause(FileName, ParName, FunName, Arity, DefPos, Exp, TabWidth, SideEffect, Dups, LogCmd) ->
    gen_fun_clause_1(FileName, ParName, FunName, Arity, DefPos, Exp, [], TabWidth, SideEffect, Dups,emacs, LogCmd).

gen_fun_clause_1(FileName, ParName, FunName, _Arity, DefPos, Exp, SearchPaths, TabWidth, SideEffect, Dups, Editor, LogCmd) ->
    {ok, {AnnAST, Info}} = refac_util:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    {ok, ModName} = get_module_name(Info),
    Forms = refac_syntax:form_list_elements(AnnAST),
    Exp1 = make_actual_parameter(ModName, Exp, SideEffect),
    F = fun (Form) ->
		case refac_syntax:type(Form) of
		    function ->
			case get_fun_def_loc(Form) of
			DefPos ->
				NewPar = refac_syntax:variable(ParName),
				Cs = refac_syntax:function_clauses(Form),
				Cs1 = [replace_clause_body(C, FunName,Exp, Exp1) || C <- Cs],
				Form1 = refac_util:rewrite(Form, refac_syntax:function(refac_syntax:atom(FunName), Cs1)),
				ClauseToGen = hd(lists:filter(fun (C) -> {EStart, EEnd} = refac_util:get_range(Exp),
									 {CStart, CEnd} = refac_util:get_range(C),
									 CStart =< EStart andalso EEnd =< CEnd
							      end, Cs)),
				ClauseToGen1 = replace_exp_with_var(ClauseToGen, {ParName, Exp, SideEffect, Dups}),						
				ClauseToGen2 = add_parameter(ClauseToGen1, NewPar),
				NewForm = refac_syntax:function(refac_syntax:atom(FunName), [ClauseToGen2]),
				[Form1, NewForm];
			    _ -> [Form]
			end;
		    _ -> [Form]
		end
	end,
    AnnAST1 = refac_syntax:form_list([T || Form <- Forms, T <- F(Form)]),
    case Editor of
	emacs ->
	    refac_util:write_refactored_files_for_preview([{{FileName, FileName}, AnnAST1}], LogCmd),
	    {ok, [FileName]};
	eclipse ->
	    Content = refac_prettypr:print_ast(refac_util:file_format(FileName), AnnAST1),
	    Res = [{FileName, FileName, Content}],
	    {ok, Res}
    end.


make_actual_parameter(ModName, Exp, SideEffect) ->
    FreeVars = [V || {V, _} <- refac_util:get_free_vars(Exp)],
    case FreeVars of
      [] ->
	  case refac_syntax:type(Exp) of
	    fun_expr -> Exp;
	    implicit_fun -> Exp;
	    _ ->
		case lists:keysearch(fun_def, 1, refac_syntax:get_ann(Exp)) of
		  {value, {fun_def, {M, _N, A, _P1, _P2}}} ->
		      case refac_syntax:type(Exp) of
			atom ->
			    case M == ModName of
			      true ->
				  refac_syntax:implicit_fun(Exp, refac_syntax:integer(A));
			      _ ->
				  Exp1 = refac_syntax:module_qualifier(refac_syntax:atom(M), Exp),
				  refac_syntax:implicit_fun(Exp1, refac_syntax:integer(A))
			    end;
			module_qualifier ->
			    refac_syntax:implicit_fun(Exp, refac_syntax:integer(A));
			_ ->
			    throw({error,
				   "Wrangler failed to perform this refactoring, "
				   "because it could not get enough information "
				   "about the expression selected."})
		      end;
		  _ -> case SideEffect of
			 true ->
			     C = refac_syntax:clause([], [], [Exp]),
			     refac_util:rewrite(Exp, refac_syntax:fun_expr([C]));
			 _ -> Exp
		       end
		end
	  end;
      [_|_] ->
	    Pars = [refac_syntax:variable(P) || P <- FreeVars],
	    C = refac_syntax:clause(Pars, [], [Exp]),
	    refac_util:rewrite(Exp, refac_syntax:fun_expr([C]))
    end.
	    
gen_cond_analysis(Fun, Exp, ParName) ->
    Cs = refac_syntax:function_clauses(Fun),
    case lists:keysearch(category,1, refac_syntax:get_ann(Exp)) of 
	{value, {category, record_field}} ->
	    throw({error, "Record field cannot be replaced by a variable."});
        {value, {category, record_type}} -> 
	    throw({error, "Record type cannot be replaced by a variable."});	 
        {value, {category, guard_expression}} -> 
	    throw({error, "Generalisation over a guard expression is not supported."});
	{value, {category, application_op}} -> 
	    GuardRanges=[refac_util:get_range(G) || 
			    C <-Cs, 
			    G <-[refac_syntax:clause_guard(C)], 
			    G=/=none],
	    {Start, End} = refac_util:get_range(Exp),
	    case [{S,E}||{S, E}<- GuardRanges, S=<Start, End=<E] of
		[] -> ok;
		_ ->throw({error, "Generalisation over a function application "
			   "in a guard expression is not supported."})
	    end;
	_ -> ok     
    end,
    Exp_Free_Vars = refac_util:get_free_vars(Exp),
    Exp_Export_Vars =refac_util:get_var_exports(Exp),
    case Exp_Export_Vars of
	[_|_] ->
	    throw({error, "Wrangler does not support generalisation "
		   "over an expression that exports variables(s)!"});
	_ -> ok
    end,
    F= fun(Node, Acc) ->
	       refac_util:get_bound_vars(Node)++Acc
       end,
    Vars0 = lists:foldl(fun(C,Acc)->
				refac_syntax_lib:fold(F, [], C)++Acc
			end, [], Cs),
    Vars = Vars0++Exp_Free_Vars,
    case [X ||{X,_Y}<-Vars, X==ParName] of
	[] -> ok;
	_ ->
	    {error, "The given parameter name conflicts with "
	     "the existing parameters, or will change the "
	     "semantics of the function to be generalised!"}
    end.

  
gen_fun(FileName, ModName, Tree, ParName, FunName, Arity, DefPos,Info, Exp, SideEffect, Dups, SearchPaths, TabWidth) ->
    Tree1 = case to_keep_original_fun(FileName, Tree, ModName, FunName, Arity, Exp, Info) of
		true  -> 
		    add_function(ModName, Tree, FunName,DefPos, Exp, SideEffect);
		false -> Tree
	    end,		  
    ActualPar =  make_actual_parameter(ModName, Exp, SideEffect),
    refac_util:stop_tdTP(fun do_gen_fun/2, Tree1, 
			 {FileName, ParName, FunName, Arity, DefPos,Info, Exp, ActualPar, SideEffect, Dups, SearchPaths, TabWidth}).



%% =====================================================================
%% @spec add_function(Tree::syntaxTree(),FunName::atom(),DefPos::Pos, Exp::expression()) ->syntaxTree()
%%
add_function(ModName, Tree, FunName, DefPos, Exp, SideEffect) -> 				
    Forms = refac_syntax:form_list_elements(Tree),
    MakeClause = 
	fun(C, Expr, Name) ->
		Pats = refac_syntax:clause_patterns(C),
		Fun = fun(P)->
			      {P1, _} =refac_util:stop_tdTP(fun do_replace_underscore/2, P, []),
			      P1
		      end,
		Pats1 = lists:map(Fun, Pats),
		G =  refac_syntax:clause_guard(C),
		Op = refac_syntax:operator(Name),    
		Args = Pats1 ++ [refac_util:reset_attrs(Expr)],
		Body = [refac_syntax:application(Op, Args)],
		refac_syntax:clause(Pats, G, Body)
	end,
    F = fun(Form) ->
		case refac_syntax:type(Form) of 
		    function ->
			case get_fun_def_loc(Form) of 
			    DefPos -> 
				Exp1 = make_actual_parameter(ModName, Exp, SideEffect),
				NewCs =[MakeClause(C, Exp1,FunName) 
					|| C <- refac_syntax:function_clauses(Form)],
				NewForm = refac_syntax:function(refac_syntax:atom(FunName),NewCs),
				[NewForm, Form];
			    _ -> [Form]
			end;
		    _ -> [Form] 
		end
	end,		
    refac_syntax:form_list([T|| Form<-Forms, T <- F(Form)]).

%% =====================================================================
%%
do_gen_fun(Tree, {FileName, ParName, FunName, Arity, DefPos,Info, Exp, 
		  ActualPar, SideEffect,Dups, SearchPaths,TabWidth}) ->    
    case  refac_syntax:type(Tree) of 
	function -> 
	    case get_fun_def_loc(Tree) of 
		DefPos -> 
		    A1 = refac_syntax:function_arity(Tree),
		    if A1 == Arity -> 
			    Name = refac_syntax:function_name(Tree),
			    NewPar= refac_syntax:variable(ParName),
			    Cs=[add_parameter(C1, NewPar) 
				||C1 <-[add_actual_parameter(replace_exp_with_var(C, {ParName, Exp, SideEffect, Dups}),
							     {FileName, FunName, Arity , NewPar, Info, SearchPaths, TabWidth})
					|| C <- refac_syntax:function_clauses(Tree)]],
			    {refac_util:rewrite(Tree, refac_syntax:function(Name, Cs)), true};
		       true -> 		    
			    {add_actual_parameter(Tree, {FileName, FunName, 
							 Arity,ActualPar, Info, SearchPaths, TabWidth}), true}
		    end;
		_ -> {add_actual_parameter(Tree, {FileName,FunName, Arity, 
						  ActualPar, Info, SearchPaths, TabWidth}), true}
	    end;
	_ -> {Tree, false}
    end.


replace_clause_body(C, FunName,Exp, ActualPar ) ->
    {EStart, EEnd} = refac_util:get_range(Exp),
    {CStart, CEnd} = refac_util:get_range(C),
    case (CStart =< EStart) andalso (EEnd =< CEnd) of
	true ->
	    Pats = refac_syntax:clause_patterns(C),
	    G = refac_syntax:clause_guard(C),
	    Body = [refac_syntax:application(refac_syntax:atom(FunName), Pats++[ActualPar])],
	    Body1 = [B1||B<-Body,
			      {B1,_} <-[refac_util:stop_tdTP(fun do_replace_underscore/2, B, [])]],
	    refac_util:rewrite(C, refac_syntax:clause(Pats, G, Body1));
	_ -> C %% add_actual_parameter(C, {FileName, FunName, Arity, Exp1, Info, SearchPaths, TabWidth})
    end.


replace_exp_with_var(Tree, {ParName, Exp, SideEffect, Dups}) ->
    {Tree1, _} =refac_util:stop_tdTP(fun do_replace_exp_with_var/2, 
				     Tree, {ParName, Exp, SideEffect, Dups}),
    Tree1.

do_replace_exp_with_var(Tree, {ParName, Exp, SideEffect, Dups}) ->
    Range = refac_util:get_range(Exp),
    case lists:member(refac_util:get_range(Tree),[Range|Dups]) of 
	true ->
	    FreeVars = [V||{V,_}<-refac_util:get_free_vars(Exp)],
	    Pars  = [refac_syntax:variable(P)||P<-FreeVars],
	    case SideEffect of 
		false -> 
		    case FreeVars==[] of 
			true ->
			    {refac_syntax:variable(ParName), true};
			_ ->
			    Op1 = refac_syntax:operator(ParName),
			    {refac_syntax:application(Op1, Pars), true}
		    end;
	        _ -> 
		    Op1 = refac_syntax:operator(ParName),
		    {refac_syntax:application(Op1, Pars),true}
	    end;
	_ -> {Tree, false}
    end.
   
		
add_actual_parameter(Tree, Args={_FileName, _FunName, _Arity, _Exp, _Info, _SearchPaths, _TabWidth})->
   {Tree1, _} =refac_util:stop_tdTP(fun do_add_actual_parameter/2,Tree, Args),
    Tree1.
    

do_add_actual_parameter(Tree, Others={_FileName, FunName, Arity, Exp, Info, _SearchPaths, _TabWidth}) ->
    {ok, ModName} = get_module_name(Info),
    case refac_syntax:type(Tree) of
      application ->
	    Op = refac_syntax:application_operator(Tree),
	    Args = refac_syntax:application_arguments(Tree),
	    case get_fun_def_info(Op) of
		{M1, F1, A1} ->
		    case lists:keysearch({M1, F1, A1}, 1, apply_style_funs()) of
			{value, _} ->
			    transform_apply_style_calls(Tree, Others);
			false ->
			    case {M1, F1, A1} of
				{ModName, FunName, Arity} ->
				    Exp1 = refac_util:update_ann(Exp, {range, ?DEFAULT_RANGE}),
				    Args1 = Args ++ [reset_attr(Exp1, fun_def)],
				    Op1 = refac_util:update_ann(Op, {fun_def, {}}),
				    Tree1 = refac_syntax:application(Op1, Args1),
				    {refac_util:update_ann(refac_util:rewrite(Tree, Tree1), {fun_def, {}}), false};
				{erlang, apply, 2} ->
				    transform_apply_with_arity_of_2(Tree, ModName, FunName, Arity, Exp);
				_ ->
				    {Tree, false}
			    end
		    end;
		false ->
		    {Tree, false}
	    end;
      %% leave implicit_funs unchanged because it is hard to their parameters.
     %% implicit_fun -> transform_implicit_fun(Tree, ModName, FunName, Arity);
      _ -> {Tree, false}
    end.

transform_apply_with_arity_of_2(Tree, ModName, FunName, Arity, Exp) ->
    Op = refac_syntax:application_operator(Tree),
    Args = refac_syntax:application_arguments(Tree),
    [Fun, Pars] = Args,
    case refac_syntax:type(Fun) of
      implicit_fun ->
	  Name = refac_syntax:implicit_fun_name(Fun),
	  case refac_syntax:type(Name) of
	    arity_qualifier ->
		F = refac_syntax:atom_value(refac_syntax:arity_qualifier_body(Name)),
		A = refac_syntax:integer_value(refac_syntax:arity_qualifier_argument(Name)),
		case {F, A} of
		    {FunName, Arity} ->
			Exp1 = refac_util:update_ann(Exp, {range, ?DEFAULT_RANGE}),
			case refac_syntax:type(Pars) of
			    list ->
				Pars0 = refac_syntax:list(refac_syntax:list_elements(Pars) ++ [Exp1]),
				Pars1 = refac_util:rewrite(Pars, Pars0);
			    _ -> Op1 = refac_syntax:operator('++'),
				 L = refac_syntax:list([Exp1]),
				 Pars0 = refac_syntax:infix_expr(Pars, Op1, L),
				 Pars1 = refac_util:rewrite(Pars, Pars0)
			end,
			Tree1 = refac_syntax:implicit_fun(refac_syntax:atom(FunName),
							  refac_syntax:integer(Arity + 1)),
			Tree2 = refac_syntax:application(Op, [Tree1, Pars1]),
			{refac_util:rewrite(Tree, Tree2), false};
		    _ -> {Tree, false}
		end;
	      module_qualifier ->
		  Mod = refac_syntax:module_qualifier_argument(Name),
		  Body = refac_syntax:module_qualifier_body(Name),
		  case refac_syntax:type(Mod) == atom andalso 
		      refac_syntax:atom_value(Mod) == ModName of
		      true ->
			  B = refac_syntax:atom_value(refac_syntax:arity_qualifier_body(Body)),
			  A = refac_syntax:integer_value(refac_syntax:arity_qualifier_argument(Body)),
			  case {B, A} of
			      {FunName, Arity} ->
				  Exp1 = refac_util:update_ann(Exp, {range, ?DEFAULT_RANGE}),
				  case refac_syntax:type(Pars) of
				      list -> 
					  Pars0 = refac_syntax:list(refac_syntax:list_elements(Pars) ++ [Exp1]),
					  Pars1 = refac_util:rewrite(Pars, Pars0);
				      _ -> Op1 = refac_syntax:operator('++'),
					   L = refac_syntax:list([Exp1]),
					   Pars0 = refac_syntax:infix_expr(Pars, Op1, L),
					   Pars1 = refac_util:rewrite(Pars, Pars0)
				  end,
				  Fun0 = refac_util:rewrite(Body, refac_syntax:arity_qualifier(
								    refac_syntax:atom(FunName),
								    refac_syntax:integer(Arity + 1))),
				  Fun1 = refac_util:rewrite(Fun,refac_syntax:implicit_fun(
								  refac_syntax:module_qualifier(Mod, Fun0))),
				  {refac_util:rewrite(Tree,refac_syntax:application(
							     Op, [Fun1, Pars1])), false};
			      _ -> {Tree, false}
			  end;
		      _ -> {Tree, false}
		  end
	  end;
	_ -> {Tree, false}
    end.

transform_apply_style_calls(Node, {FileName, FunName, Arity, Exp, Info, SearchPaths, TabWidth}) ->
    {ok, ModName} = get_module_name(Info),
    Op = refac_syntax:application_operator(Node),
    Args = refac_syntax:application_arguments(Node),
    [N1, N2, Mod, Fun, Pars] = case length(Args) of
				   5 -> Args;
				   4 -> [none| Args];
				   3 -> [none, none| Args]
			       end,
    Mod1 = try_eval(FileName, Mod, SearchPaths, TabWidth),
    Fun1 = try_eval(FileName, Fun, SearchPaths, TabWidth),
    NewApp = fun() ->
		     Exp1 = refac_util:update_ann(Exp, {range, ?DEFAULT_RANGE}),
		     Pars0 = refac_syntax:list(refac_syntax:list_elements(Pars) ++ [Exp1]),
		     Pars1 = refac_util:rewrite(Pars, Pars0),
		     App = case length(Args) of
			       5 -> refac_syntax:application(Op, [N1, N2, Mod, Fun, Pars1]);
			       4 -> refac_syntax:application(Op, [N2, Mod, Fun, Pars1]);
			       3 -> refac_syntax:application(Op, [Mod, Fun, Pars1])
			   end,
		     refac_util:rewrite(Node, App)
	     end,
    case Fun1 of
	{value, FunName} ->
	    case Mod1 of
		{value, ModName} ->
		    case refac_syntax:type(Pars) of
			list ->
			    case refac_syntax:list_length(Pars) of
				Arity ->
				    {NewApp(), true};
				_ -> {Node, false}
			    end;
			nil -> 
			    if Arity == 0 ->
				    {NewApp(), true};
			       true -> {Node, false}
			    end;
			_ -> {Node, false}
		    end;
		_ -> {Node, false}
	    end;
	_ -> {Node, false}
    end.

add_parameter(C, NewPar) ->       
    Pats = refac_syntax:clause_patterns(C),
    G    = refac_syntax:clause_guard(C),
    Body = refac_syntax:clause_body(C),
    Pats1 = Pats ++ [NewPar],
    refac_util:rewrite(C, refac_syntax:clause(Pats1, G, Body)).



to_keep_original_fun(FileName, AnnAST, ModName, FunName, Arity, _Exp, Info) ->
    refac_util:is_exported({FunName, Arity}, Info) orelse
	is_eunit_special_function(FileName, atom_to_list(FunName), Arity) orelse
       	check_atoms(AnnAST, [FunName]) orelse
	check_implicit_and_apply_style_calls(AnnAST, ModName, FunName, Arity).

    
is_eunit_special_function(FileName, FunName, Arity) ->
    UsedTestFrameWorks = refac_util:test_framework_used(FileName),
    case lists:member(eunit, UsedTestFrameWorks) of 
	true ->
	    (Arity==0)
		andalso  
		  (lists:suffix(?DEFAULT_EUNIT_TEST_SUFFIX, FunName) orelse
		   lists:suffix(?DEFAULT_EUNIT_GENERATOR_SUFFIX, FunName) orelse
		   FunName=="test");
	_ -> false
    end.

check_atoms(AnnAST, AtomNames) ->
    F = fun (T) ->
		case refac_syntax:type(T) of
		    function -> collect_atoms(T, AtomNames);
		    _ -> []
		end
	end,
    R = lists:flatmap(F, refac_syntax:form_list_elements(AnnAST)),
    R1 = [X ||{atom, X, _} <- R],
    R2 = [X ||{_, X, _} <- lists:filter(fun (X) ->
					     case X of
						 {atom, _X,_} -> false;
						 _ -> true
					     end
				     end,
				     R)],
    R1 -- R2 =/= [].
   
check_implicit_and_apply_style_calls(AnnAST, ModName, FunName, Arity) ->
    F = fun(Node, _Others) ->
		case refac_syntax:type(Node) of 
		    application ->
			Op = refac_syntax:application_operator(Node),
			case get_fun_def_info(Op) of
			    {M1, F1, A1} ->
				case lists:keysearch({M1, F1, A1}, 1, apply_style_funs()) of
				    {value, _} ->
					{Node, true};
				    _ ->
					{[], false}
				end;
			    _ -> {[], false}
			end;
		    implicit_fun ->
			Name = refac_syntax:implicit_fun_name(Node),
			case refac_syntax:type(Name) of
			    arity_qualifier ->
				FunName1 = refac_syntax:atom_value(refac_syntax:arity_qualifier_body(Name)),
				Arity1 = refac_syntax:integer_value(refac_syntax:arity_qualifier_argument(Name)),
				case {FunName1, Arity1} of
				    {FunName, Arity} ->{Node, true};
				    _ ->{[], false}
				end;
			    module_qualifier ->
				Mod = refac_syntax:module_qualifier_argument(Name),
				Body = refac_syntax:module_qualifier_body(Name),
				case refac_syntax:type(Mod) == atom andalso 
				    refac_syntax:atom_value(Mod) == ModName of
				    true ->
					FunName1 = refac_syntax:atom_value(refac_syntax:arity_qualifier_body(Body)),
					Arity1 = refac_syntax:integer_value(refac_syntax:arity_qualifier_argument(Body)),
					case {FunName1, Arity1} of
					    {FunName, Arity} -> {Node, true};
					    _ -> {[], false}
					end;
				    false ->{[], false}
				end
			end;
		    _ -> {[], false}
		end
	end,
    element(2, refac_util:once_tdTU(F, AnnAST, [])).


check_side_effect(FileName, Exp, SearchPaths) ->
    case refac_util:has_side_effect(FileName, Exp, SearchPaths) of
	unknown -> case refac_util:get_free_vars(Exp) of
		       [] -> unknown;
		       _ -> true
		   end;
	V -> V
    end.

get_fun_def_loc(Node) ->
    As = refac_syntax:get_ann(Node),
    case lists:keysearch(fun_def, 1, As) of 
	{value, {fun_def, {_M, _N, _A, _P, DefPos}}} ->
	    DefPos;
	_ -> false
    end.

get_fun_def_info(Node) ->
    As = refac_syntax:get_ann(Node),
    case lists:keysearch(fun_def, 1, As) of
      {value, {fun_def, {Mod, FunName, Arity, _, _}}} ->
	    {Mod, FunName, Arity};
	_ -> false
    end.


get_module_name(ModInfo) ->				      
    case lists:keysearch(module, 1, ModInfo) of
	{value, {module, ModName}} -> {ok, ModName};
	false ->
	    {error, "Can not get the current module name."}
    end.


expr_to_fun_clause(FunDef, Expr) ->
    Cs = refac_syntax:function_clauses(FunDef),
    {EStart, EEnd} = refac_util:get_range(Expr),
    Res =[C||C<-Cs, {CStart, CEnd}<-[refac_util:get_range(C)],
	     CStart=<EStart, EEnd =< CEnd],
    case Res of
	[] ->
	     none;
	_ ->hd(Res)
    end.
     

search_duplications(Tree, Exp) ->
    F = fun(Node, Acc)-> 
		As = refac_syntax:get_ann(Node),
		case lists:keysearch(category, 1, As) of 
		    {value, {category, expression}} ->
			case  refac_syntax:is_literal(Node)andalso
			    Node =/= Exp of
			    true ->
				case refac_syntax:concrete(Node) ==
				    refac_syntax:concrete(Exp) of
				    true -> [Node|Acc];
				    _ -> Acc
				end;
			    false -> Acc
			end;
		    _ -> Acc
		end
	end,
    case refac_syntax:is_literal(Exp) of
	true ->
	    Es =lists:reverse(refac_syntax_lib:fold(F, [],Tree)),
	    [refac_util:get_range(E) || E<-Es];
	_ -> []
    end.  
	
do_replace_underscore(Tree, _Others) ->
    case refac_syntax:type(Tree) of 
	underscore ->
	    {refac_syntax:atom(undefined), true};
	_ -> {Tree,false}
    end.
    

reset_attr(Node, Key) ->
    refac_util:full_buTP(fun (T, _Others) ->
				 Ann = refac_syntax:get_ann(T), 
				 NewAnn = lists:keydelete(Key, 1, Ann),
				 refac_syntax:set_ann(T, NewAnn)
			 end, Node, {}).
