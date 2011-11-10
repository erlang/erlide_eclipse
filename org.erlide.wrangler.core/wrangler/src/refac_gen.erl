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

%% @private
-module(refac_gen).

-include("../include/wrangler_internal.hrl").

-export([generalise/7, gen_fun_1/12, gen_fun_clause/11]).

-export([generalise_eclipse/6, gen_fun_1_eclipse/11, gen_fun_clause_eclipse/10]).

%%-spec generalise_eclipse(FileName::filename(), Start::pos(), End::pos(), ParName::string(),
%%	                         SearchPaths::[dir()], TabWidth::integer()) ->
%%        {ok, [filename()]} |
%%	{ok, [{filename(), filename(), string()}]} |
%%	{multiple_instances,  {ParName:: atom(), FunName::atom(), Arity::integer(),
%%			       FunDefPos::pos(), Exp::syntaxTree(), SideEffect::boolean(),
%%			       DupsInFun::[{pos(), pos()}], Cmd::string()}} |
%%	{unknown_side_effect, {ParName::atom(), FunName::atom(), Arity::integer(),
%%			       FunDefPos::pos(), Exp::syntaxTree(), NoOfClauses::integer(),
%%			       DupsInFun::[{pos(), pos()}], DupsInClause::[{pos(), pos()}],
%%			       Cmd::string()}} |
%%	{more_than_one_clause, {ParName::atom(), FunName::atom(), Arity::integer(),
%%				FunDefPos::pos(), Exp::syntaxTree(), SideEffect::boolean(),
%%				DupsInFun::[{pos(), pos()}], DupsInClause::[{pos(), pos()}],
%%				Cmd::string()}}.
generalise_eclipse(FileName, Start, End, ParName, SearchPaths, TabWidth) ->
    generalise(FileName, Start, End, ParName, SearchPaths, eclipse, TabWidth).

%% =====================================================================
%%-spec generalise(FileName::filename(),Start::pos(), End::pos(),ParName::string(),
%%		 SearchPaths::[dir()], atom(), TabWidth::integer()) ->
%%	     {ok, [filename()]}
%%		 |{multiple_instances, {atom(), atom(), integer(), pos(), syntaxTree(), boolean(),[{pos(), pos()}], string()}}
%%		 |{unknown_side_effect, {atom(), atom(),integer(), pos(), syntaxTree(), integer(),
%%					 [{pos(), pos()}], [{pos(),pos()}], string()}}
%%		 |{more_than_one_clause, {atom(), atom(), integer(), pos(), syntaxTree(), boolean(),
%%					  [{pos(), pos()}], [{pos(),pos()}], string()}}. 
generalise(FileName, Start = {Line, Col}, End = {Line1, Col1}, ParName, SearchPaths, Editor, TabWidth) ->
     ?wrangler_io("\nCMD: ~p:generalise(~p, {~p,~p}, {~p,~p}, ~p,~p,~p, ~p).\n",
		  [?MODULE, FileName, Line, Col, Line1, Col1, ParName, SearchPaths, Editor, TabWidth]),
     Cmd = "CMD: " ++ atom_to_list(?MODULE) ++ ":generalise(" ++ "\"" ++
	     FileName ++ "\", {" ++ integer_to_list(Line) ++ ", " ++ integer_to_list(Col) ++ "}," ++
	       "{" ++ integer_to_list(Line1) ++ ", " ++ integer_to_list(Col1) ++ "}," ++ "\"" ++ ParName ++ "\","
      ++ "[" ++ wrangler_misc:format_search_paths(SearchPaths) ++ "]," ++ integer_to_list(TabWidth) ++ ").",
    case api_refac:is_var_name(ParName) of
	false -> throw({error, "Invalid parameter name!"});
	true -> ok
    end,
    {ok, {AnnAST, Info}} = wrangler_ast_server:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    {ok, ModName} = get_module_name(Info),
    case api_interface:pos_to_expr(AnnAST, Start, End) of
	{ok, Exp} ->
	    Exp;
	{error, _} -> throw({error, "You have not selected an expression, "
				    "or the function containing the expression does not parse."}),
		      Exp = none
    end,
    case api_interface:expr_to_fun(AnnAST, Exp) of
	{ok, Fun} ->
	    Fun;
	{error, _} -> throw({error, "You have not selected an expression within a function."}),
		      Fun = none
    end,
    NoOfClauses = length(wrangler_syntax:function_clauses(Fun)),
    FunName = wrangler_syntax:data(wrangler_syntax:function_name(Fun)),
    FunArity = wrangler_syntax:function_arity(Fun),
    Inscope_Funs = [{F, A} || {_M, F, A} <- api_refac:inscope_funs(Info)],
    NewArity = FunArity + 1,
    case lists:member({FunName, NewArity}, Inscope_Funs) orelse erl_internal:bif(erlang, FunName, NewArity) of
	true -> throw({error, "Function " ++ 
				atom_to_list(FunName) ++ "/" ++ integer_to_list(NewArity) ++ " is already in scope!"});
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
    Exp1 = case Editor of
	       emacs -> term_to_list(Exp);
               composite_emacs -> term_to_list(Exp);
	       _ -> Exp
	   end,
    case SideEffect of
	unknown ->
	    {unknown_side_effect, {ParName1, FunName, FunArity,
				   FunDefPos, Exp1, NoOfClauses, DupsInFun, DupsInClause, Cmd}};
	_ ->
	    case NoOfClauses > 1 of
		true ->
		    {more_than_one_clause,
		     {ParName1, FunName, FunArity, FunDefPos, Exp1, SideEffect, DupsInFun, DupsInClause, Cmd}};
		_ ->
		    case DupsInFun of
			[_| _] ->
			    {multiple_instances,
			     {ParName1, FunName, FunArity, FunDefPos, Exp1, SideEffect, DupsInFun, Cmd}};
			_ ->
			    {AnnAST1, _} = gen_fun(FileName, ModName, AnnAST, ParName1, FunName,
						   FunArity, FunDefPos, Info, Exp, SideEffect, [], SearchPaths, TabWidth),
			    wrangler_write_file:write_refactored_files([{{FileName,FileName}, AnnAST1}], Editor, TabWidth, Cmd)
		    end
	    end
    end.

%%-spec(gen_fun_1_eclipse/11::(SideEffect::boolean(), FileName::filename(),ParName::atom(), FunName::atom(), 
%%			    Arity::integer(), FunDefPos::pos(), Expr::syntaxTree(), SearchPaths::[dir()],
%%			    TabWidth::integer(), Dups::[{pos(), pos()}], LogCmd::string()) 
%%      -> {ok, [{filename(), filename(),string()}]}).
gen_fun_1_eclipse(SideEffect, FileName,ParName, FunName, Arity, DefPos, Exp, SearchPaths,TabWidth, Dups, LogCmd) ->
    gen_fun_1(SideEffect, FileName,ParName, FunName, Arity, DefPos, Exp, SearchPaths, TabWidth, Dups, eclipse,LogCmd).

gen_fun_1(SideEffect, FileName, ParName, FunName, Arity, DefPos, Exp0, SearchPaths, TabWidth, Dups, Editor, LogCmd) ->
    {ok, {AnnAST, Info}} = wrangler_ast_server:parse_annotate_file(FileName, true, [], TabWidth),
    {ok, ModName} = get_module_name(Info),
    Exp = case Editor of
	      emacs -> list_to_term(Exp0);
              composite_emacs ->
                  list_to_term(Exp0);
	      _ -> Exp0
	  end,
    AnnAST1 = case to_keep_original_fun(FileName, AnnAST, ModName, FunName, Arity, Exp, Info) of
		  true -> add_function(ModName, AnnAST, FunName, Arity, Exp, SideEffect);
		  false -> AnnAST
	      end,
    ActualPar = make_actual_parameter(ModName, Exp, SideEffect),
    {AnnAST2, _} = api_ast_traverse:stop_tdTP(fun do_gen_fun/2, AnnAST1,
					      {FileName, ParName, FunName, Arity, DefPos, Info,
					       Exp, ActualPar, SideEffect, Dups, SearchPaths, TabWidth}),
    wrangler_write_file:write_refactored_files([{{FileName,FileName}, AnnAST2}], Editor, TabWidth, LogCmd).


%%-spec(gen_fun_clause_eclipse/10::(FileName::filename(), ParName::atom(), FunName::atom(), Arity::integer(), DefPos::pos(), 
%%				 Exp::syntaxTree(), TabWidth::integer(), SideEffect::boolean(),  Dups::[{pos(), pos()}], LogCmd::string()) ->
%%					{ok, [{filename(), filename(), string()}]}).
gen_fun_clause_eclipse(FileName, ParName, FunName, Arity, DefPos, Exp, TabWidth, SideEffect, Dups, LogCmd) ->
    gen_fun_clause_1(FileName, ParName, FunName, Arity, DefPos, Exp, [], TabWidth, SideEffect, Dups, eclipse, LogCmd).

%%-spec(gen_fun_clause/10::(FileName::filename(), ParName::atom(), FunName::atom(), Arity::integer(), DefPos::pos(), 
%%			 Exp::syntaxTree(), TabWidth::integer(), SideEffect::boolean(), 
%%			 Dups::[{{integer(), integer()},{integer(), integer()}}], LogCmd::string()) ->{ok, [filename()]}).

gen_fun_clause(FileName, ParName, FunName, Arity, DefPos, Exp, TabWidth, SideEffect, Dups, Editor, LogCmd) ->
    gen_fun_clause_1(FileName, ParName, FunName, Arity, DefPos, Exp, [], TabWidth, SideEffect, Dups, Editor, LogCmd).

gen_fun_clause_1(FileName, ParName, FunName, _Arity, DefPos, Exp0, SearchPaths, TabWidth, SideEffect, Dups, Editor, LogCmd) ->
    {ok, {AnnAST, Info}} = wrangler_ast_server:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    {ok, ModName} = get_module_name(Info),
    Exp = case Editor of
	      emacs -> list_to_term(Exp0);
              composite_emacs -> term_to_list(Exp0);
	      _ -> Exp0
	  end,
    Forms = wrangler_syntax:form_list_elements(AnnAST),
    Exp1 = make_actual_parameter(ModName, Exp, SideEffect),
    F = fun (Form) ->
		case wrangler_syntax:type(Form) of
		    function ->
			case get_fun_def_loc(Form) of
			    DefPos ->
				NewPar = wrangler_syntax:variable(ParName),
				Cs = wrangler_syntax:function_clauses(Form),
				Cs1 = [replace_clause_body(C, FunName, Exp, Exp1) || C <- Cs],
				Form1 = wrangler_misc:rewrite(Form, wrangler_syntax:function(wrangler_syntax:atom(FunName), Cs1)),
				ClauseToGen = hd(lists:filter(fun (C) -> {EStart, EEnd} = wrangler_misc:start_end_loc(Exp),
									 {CStart, CEnd} = wrangler_misc:start_end_loc(C),
									 CStart =< EStart andalso EEnd =< CEnd
							      end, Cs)),
				ClauseToGen1 = replace_exp_with_var(ClauseToGen, {ParName, Exp, SideEffect, Dups}),
				ClauseToGen2 = add_parameter(ClauseToGen1, NewPar),
                                NewForm = wrangler_syntax:function(wrangler_syntax:function_name(Form), [ClauseToGen2]),
                                NewForm1 =wrangler_misc:rewrite(Form, NewForm),
				[Form1, NewForm1];
			    _ -> [Form]
			end;
		    _ -> [Form]
		end
	end,
    AnnAST1 = wrangler_syntax:form_list([T || Form <- Forms, T <- F(Form)]),
    wrangler_write_file:write_refactored_files([{{FileName, FileName}, AnnAST1}], Editor, TabWidth, LogCmd).

make_actual_parameter(ModName, Exp, SideEffect) ->
    FreeVars = [V || {V, _} <- api_refac:free_vars(Exp)],
    case FreeVars of
	[] ->
	    case wrangler_syntax:type(Exp) of
		fun_expr -> Exp;
		implicit_fun -> Exp;
		_ ->
		    case lists:keysearch(fun_def, 1, wrangler_syntax:get_ann(Exp)) of
			{value, {fun_def, {M, _N, A, _P1, _P2}}} ->
			    case wrangler_syntax:type(Exp) of
				atom ->
				    case M == ModName orelse M=='_' of
					true ->
					    wrangler_syntax:implicit_fun(Exp, wrangler_syntax:integer(A));
					_ ->
					    Exp1 = wrangler_syntax:module_qualifier(wrangler_syntax:atom(M), Exp),
					    wrangler_syntax:implicit_fun(Exp1, wrangler_syntax:integer(A))
				    end;
				module_qualifier ->
				    wrangler_syntax:implicit_fun(Exp, wrangler_syntax:integer(A));
				_ ->
				    throw({error,
					   "Wrangler failed to perform this refactoring, "
					   "because it could not get enough information "
					   "about the expression selected."})
			    end;
			_ -> case SideEffect of
				 true ->
				     C = wrangler_syntax:clause([], [], [Exp]),
				     wrangler_misc:rewrite(Exp, wrangler_syntax:fun_expr([C]));
				 _ -> Exp
			     end
		    end
	    end;
	[_| _] ->
	    Pars = [wrangler_syntax:variable(P) || P <- FreeVars],
	    C = wrangler_syntax:clause(Pars, [], [Exp]),
	    wrangler_misc:rewrite(Exp, wrangler_syntax:fun_expr([C]))
    end.

gen_cond_analysis(Fun, Exp, ParName) ->
    Cs = wrangler_syntax:function_clauses(Fun),
    Ann = wrangler_syntax:get_ann(Exp),
    case lists:keysearch(category, 1, Ann) of
        {value, {category, guard_expression}} ->
	    case lists:keysearch(fun_def,1,Ann) of
		{value, {fun_def,_}} ->
		    throw({error, "Generalisation over a function application "
                           "in a guard expression is not supported."});
		_ ->
		    case api_refac:free_vars(Exp) of
			[] -> ok;
			_ ->
			    throw({error,"Generalisation over an expression with free variables in "
                                   " a guard expression is not supported."})
		    end
	    end;
	_ -> ok
    end,
    Exp_Free_Vars = api_refac:free_vars(Exp),
    Exp_Export_Vars = wrangler_misc:exported_vars(Exp),
    case Exp_Export_Vars of
	[_| _] ->
	    throw({error, "Wrangler does not support generalisation "
			  "over an expression that exports variables(s)!"});
	_ -> ok
    end,
    F = fun (Node, Acc) ->
		api_refac:bound_vars(Node) ++ Acc
	end,
    Vars0 = lists:foldl(fun (C, Acc) ->
				api_ast_traverse:fold(F, [], C) ++ Acc
			end, [], Cs),
    Vars = Vars0 ++ Exp_Free_Vars,
    case [X || {X, _Y} <- Vars, X == ParName] of
	[] -> ok;
	_ ->
	    {error, "The given parameter name conflicts with "
		    "the existing parameters, or will change the "
		    "semantics of the function to be generalised!"}
    end.

  
gen_fun(FileName, ModName, Tree, ParName, FunName, Arity, DefPos, Info, Exp, SideEffect, Dups, SearchPaths, TabWidth) ->
    Tree1 = case to_keep_original_fun(FileName, Tree, ModName, FunName, Arity, Exp, Info) of
	      true ->
		  add_function(ModName, Tree, FunName, Arity, Exp, SideEffect);
	      false -> Tree
	    end,
    ActualPar = make_actual_parameter(ModName, Exp, SideEffect),
    api_ast_traverse:stop_tdTP(fun do_gen_fun/2, Tree1,
			       {FileName, ParName, FunName, Arity, DefPos, Info, Exp, ActualPar, SideEffect, Dups, SearchPaths, TabWidth}).

%% =====================================================================
%% @spec add_function(ModName::atom(),Tree::syntaxTree(),FunName::atom(),DefPos::Pos,Exp::expression(),SideEffect::boolean()) ->syntaxTree()
%%
add_function(ModName, Tree, FunName, Arity, Exp, SideEffect) ->
    Forms = wrangler_syntax:form_list_elements(Tree),
    MakeClause =
	fun (C, Expr, Name) ->
		Pats = wrangler_syntax:clause_patterns(C),
		Fun = fun (P) ->
			      {P1, _} = api_ast_traverse:stop_tdTP(fun do_replace_underscore/2, P, []),
			      P1
		      end,
		Pats1 = lists:map(Fun, Pats),
		G = wrangler_syntax:clause_guard(C),
		Op = wrangler_syntax:operator(Name),
		Args = Pats1 ++ [wrangler_misc:reset_ann_and_pos(Expr)],
		Body = [wrangler_syntax:application(Op, Args)],
		wrangler_syntax:clause(Pats, G, Body)
	end,
    {Forms1, [Form|Forms2]} = lists:splitwith(
                                fun(F)->
                                       not (api_refac:fun_define_info(F)=={ModName, FunName, Arity})
                                end, Forms),
    Exp1 = make_actual_parameter(ModName, Exp, SideEffect),
    NewCs = [MakeClause(C, Exp1, FunName)
             || C <- wrangler_syntax:function_clauses(Form)],
    NewForm=wrangler_syntax:function(wrangler_syntax:atom(FunName), NewCs),
    {Fs1, [F1|Fs2]} = lists:splitwith(fun(F) ->
                                               not (lists:member(wrangler_syntax:type(F), [function, attribute]))
                                       end,lists:reverse(Forms1)),
    case api_spec:is_type_spec(F1, {FunName, Arity}) orelse
        api_spec:is_type_spec(F1, {ModName, FunName, Arity}) of
        true ->
            wrangler_syntax:form_list(
              lists:reverse(Fs2)++[NewForm]++[F1]++lists:reverse(Fs1)++[Form|Forms2]);
        false ->
            wrangler_syntax:form_list(Forms1 ++ [NewForm, Form |Forms2])
    end.
 
%% =====================================================================
%%
do_gen_fun(Tree, {FileName, ParName, FunName, Arity, DefPos, Info, Exp,
		  ActualPar, SideEffect, Dups, SearchPaths, TabWidth}) ->
    case wrangler_syntax:type(Tree) of
	function ->
	    case get_fun_def_loc(Tree) of
		DefPos ->
		    A1 = wrangler_syntax:function_arity(Tree),
		    if A1 == Arity ->		
                            Name = wrangler_syntax:function_name(Tree),
			    NewPar = wrangler_syntax:variable(ParName),
			    Cs = [add_parameter(C1, NewPar)
				  || C1 <- [add_actual_parameter(replace_exp_with_var(C, {ParName, Exp, SideEffect, Dups}),
								 {FileName, FunName, Arity, NewPar, Info, SearchPaths, TabWidth})
					    || C <- wrangler_syntax:function_clauses(Tree)]],
			    {wrangler_misc:rewrite(Tree, wrangler_syntax:function(Name, Cs)), true};
		       true ->
			   {add_actual_parameter(Tree, {FileName, FunName,
							Arity, ActualPar, Info, SearchPaths, TabWidth}), true}
		    end;
		_ -> {add_actual_parameter(Tree, {FileName, FunName, Arity,
						  ActualPar, Info, SearchPaths, TabWidth}), true}
	    end;
        attribute ->
            M = list_to_atom(filename:basename(FileName, ".erl")),
            case api_spec:is_type_spec(Tree, {M, FunName, Arity}) orelse
                api_spec:is_type_spec(Tree, {FunName, Arity}) of
                true ->
                    NewArgType ={type, {0, 0}, any, []}, %% this will be generated automatically.
                    NewTree=api_spec:add_arg_type_to_spec(
                         Tree, NewArgType, Arity),
                    {NewTree, true};
                false ->
                    {Tree, true}
            end;
	_ -> {Tree, false}
    end.

replace_clause_body(C, FunName, Exp, ActualPar) ->
    {EStart, EEnd} = wrangler_misc:start_end_loc(Exp),
    {CStart, CEnd} = wrangler_misc:start_end_loc(C),
    case CStart =< EStart andalso EEnd =< CEnd of
	true ->
	    Pats = wrangler_syntax:clause_patterns(C),
	    G = wrangler_syntax:clause_guard(C),
	    Body = [wrangler_syntax:application(wrangler_syntax:atom(FunName), 
                                                Pats ++ [wrangler_misc:reset_ann_and_pos(ActualPar)])],
	    Body1 = [B1 || B <- Body,
			   {B1, _} <- [api_ast_traverse:stop_tdTP(fun do_replace_underscore/2, B, [])]],
	    wrangler_misc:rewrite(C, wrangler_syntax:clause(Pats, G, Body1));
	_ -> C %% add_actual_parameter(C, {FileName, FunName, Arity, Exp1, Info, SearchPaths, TabWidth})
    end.


replace_exp_with_var(Tree, {ParName, Exp, SideEffect, Dups}) ->
    {Tree1, _} = api_ast_traverse:stop_tdTP(fun do_replace_exp_with_var/2,
					    Tree, {ParName, Exp, SideEffect, Dups}),
    Tree1.

do_replace_exp_with_var(Tree, {ParName, Exp, SideEffect, Dups}) ->
    Range = wrangler_misc:start_end_loc(Exp),
    case lists:member(wrangler_misc:start_end_loc(Tree), [Range| Dups]) of
	true ->
	    FreeVars = [V || {V, _} <- api_refac:free_vars(Exp)],
	    Pars = [wrangler_syntax:variable(P) || P <- FreeVars],
	    case SideEffect of
		false ->
		    case FreeVars == [] of
			true ->
			    {wrangler_misc:rewrite(Tree, wrangler_syntax:variable(ParName)), true};
			_ ->
			    Op1 = wrangler_syntax:operator(ParName),
			    {wrangler_misc:rewrite(Tree, wrangler_syntax:application(Op1, Pars)), true}
		    end;
		_ ->
		    Op1 = wrangler_syntax:operator(ParName),
		    {wrangler_misc:rewrite(Tree, wrangler_syntax:application(Op1, Pars)), true}
	    end;
	_ -> {Tree, false}
    end.
   
add_actual_parameter(Tree, Args = {_FileName, _FunName, _Arity, _Exp, _Info, _SearchPaths, _TabWidth}) ->
    {Tree1, _} = api_ast_traverse:stop_tdTP(fun do_add_actual_parameter/2, Tree, Args),
    Tree1.

do_add_actual_parameter(Tree, Others = {_FileName, FunName, Arity, Exp, Info, _SearchPaths, _TabWidth}) ->
    {ok, ModName} = get_module_name(Info),
    case wrangler_syntax:type(Tree) of
	application ->
	    Op = wrangler_syntax:application_operator(Tree),
	    Args = wrangler_syntax:application_arguments(Tree),
	    case get_fun_def_info(Op) of
		{M1, F1, A1} ->
		    case lists:keysearch({M1, F1, A1}, 1, wrangler_misc:apply_style_funs()) of
			{value, _} ->
			    transform_apply_style_calls(Tree, Others);
			false ->
			    case {M1, F1, A1} of
				{ModName, FunName, Arity} ->
				    Exp1 = wrangler_misc:reset_ann_and_pos(Exp),
				    Args1 = Args ++ [reset_attr(Exp1, fun_def)],
				    Op1 = wrangler_misc:update_ann(Op, {fun_def, {}}),
				    Tree1 = wrangler_syntax:application(Op1, Args1),
				    {wrangler_misc:update_ann(wrangler_misc:rewrite(Tree, Tree1), {fun_def, {}}), false};
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

%% The following two function should be refactored.
transform_apply_with_arity_of_2(Tree, ModName, FunName, Arity, Exp) ->
    Op = wrangler_syntax:application_operator(Tree),
    Args = wrangler_syntax:application_arguments(Tree),
    [Fun, Pars] = Args,
    case wrangler_syntax:type(Fun) of
	implicit_fun ->
	    Name = wrangler_syntax:implicit_fun_name(Fun),
	    case wrangler_syntax:type(Name) of
		arity_qualifier ->
		    F = wrangler_syntax:atom_value(wrangler_syntax:arity_qualifier_body(Name)),
		    A = wrangler_syntax:integer_value(wrangler_syntax:arity_qualifier_argument(Name)),
		    case {F, A} of
			{FunName, Arity} ->
			    Exp1 = wrangler_misc:reset_ann_and_pos(Exp),
			    case wrangler_syntax:type(Pars) of
				list ->
				    Pars0 = wrangler_syntax:list(list_elements(Pars) ++ [Exp1]),
				    Pars1 = wrangler_misc:rewrite(Pars, Pars0);
				_ -> Op1 = wrangler_syntax:operator('++'),
				     L = wrangler_syntax:list([Exp1]),
				     Pars0 = wrangler_syntax:infix_expr(Pars, Op1, L),
				     Pars1 = wrangler_misc:rewrite(Pars, Pars0)
			    end,
			    Tree1 = wrangler_syntax:implicit_fun(wrangler_syntax:atom(FunName),
							         wrangler_syntax:integer(Arity + 1)),
			    Tree2 = wrangler_syntax:application(Op, [Tree1, Pars1]),
			    {wrangler_misc:rewrite(Tree, Tree2), false};
			_ -> {Tree, false}
		    end;
		module_qualifier ->
		    Mod = wrangler_syntax:module_qualifier_argument(Name),
		    Body = wrangler_syntax:module_qualifier_body(Name),
		    case wrangler_syntax:type(Mod) == atom andalso
			   wrangler_syntax:atom_value(Mod) == ModName
			of
			true ->
			    B = wrangler_syntax:atom_value(wrangler_syntax:arity_qualifier_body(Body)),
			    A = wrangler_syntax:integer_value(wrangler_syntax:arity_qualifier_argument(Body)),
			    case {B, A} of
				{FunName, Arity} ->
				    Exp1 = wrangler_misc:reset_ann_and_pos(Exp),
				    case wrangler_syntax:type(Pars) of
					list ->
					    Pars0 = wrangler_syntax:list(list_elements(Pars) ++ [Exp1]),
					    Pars1 = wrangler_misc:rewrite(Pars, Pars0);
					_ -> Op1 = wrangler_syntax:operator('++'),
					     L = wrangler_syntax:list([Exp1]),
					     Pars0 = wrangler_syntax:infix_expr(Pars, Op1, L),
					     Pars1 = wrangler_misc:rewrite(Pars, Pars0)
				    end,
				    Fun0 = wrangler_misc:rewrite(Body, wrangler_syntax:arity_qualifier(
								            wrangler_syntax:atom(FunName),
								            wrangler_syntax:integer(Arity + 1))),
				    Fun1 = wrangler_misc:rewrite(Fun, wrangler_syntax:implicit_fun(
								           wrangler_syntax:module_qualifier(Mod, Fun0))),
				    {wrangler_misc:rewrite(Tree, wrangler_syntax:application(
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
    Op = wrangler_syntax:application_operator(Node),
    Args = wrangler_syntax:application_arguments(Node),
    [N1, N2, Mod, Fun, Pars] = case length(Args) of
				   5 -> Args;
				   4 -> [none| Args];
				   3 -> [none, none| Args]
			       end,
    Mod1 = wrangler_misc:try_eval(FileName, Mod, SearchPaths, TabWidth),
    Fun1 = wrangler_misc:try_eval(FileName, Fun, SearchPaths, TabWidth),
    NewApp = fun () ->
		     Exp1 = wrangler_misc:reset_ann_and_pos(Exp),
		     Pars0 = wrangler_syntax:list(list_elements(Pars) ++ [Exp1]),
		     Pars1 = wrangler_misc:rewrite(Pars, Pars0),
		     App = case length(Args) of
			       5 -> wrangler_syntax:application(Op, [N1, N2, Mod, Fun, Pars1]);
			       4 -> wrangler_syntax:application(Op, [N2, Mod, Fun, Pars1]);
			       3 -> wrangler_syntax:application(Op, [Mod, Fun, Pars1])
			   end,
		     wrangler_misc:rewrite(Node, App)
	     end,
    case Fun1 of
	{value, FunName} ->
	    case Mod1 of
		{value, ModName} ->
		    case wrangler_syntax:type(Pars) of
			list ->
			    case wrangler_syntax:list_length(Pars) of
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
    Pats = wrangler_syntax:clause_patterns(C),
    G = wrangler_syntax:clause_guard(C),
    Body = wrangler_syntax:clause_body(C),
    Pats1 = Pats ++ [NewPar],
    wrangler_misc:rewrite(C, wrangler_syntax:clause(Pats1, G, Body)).

to_keep_original_fun(FileName, AnnAST, ModName, FunName, Arity, Exp, Info) ->
    api_refac:is_exported({FunName, Arity}, Info) orelse
      is_eunit_special_function(FileName, atom_to_list(FunName), Arity) orelse 
	has_unsure_atoms(AnnAST, [FunName], f_atom) orelse 
	  check_implicit_and_apply_style_calls(AnnAST, ModName, FunName, Arity) orelse 
	    generalise_recursive_function_call(Exp, ModName, FunName, Arity) orelse 
	      has_multiple_definitions(AnnAST, ModName, FunName, Arity).

is_eunit_special_function(FileName, FunName, Arity) ->
    UsedTestFrameWorks = wrangler_misc:test_framework_used(FileName),
    case lists:member(eunit, UsedTestFrameWorks) of
	true ->
	    Arity == 0 andalso 
	      (lists:suffix(?DEFAULT_EUNIT_TEST_SUFFIX, FunName) orelse 
		 lists:suffix(?DEFAULT_EUNIT_GENERATOR_SUFFIX, FunName) orelse 
		   FunName == "test");
	_ -> false
    end.

has_unsure_atoms(AnnAST, AtomNames, AtomType) ->
    wrangler_atom_utils:collect_unsure_atoms_in_file(
         AnnAST, AtomNames, AtomType) /= [].

check_implicit_and_apply_style_calls(AnnAST, ModName, FunName, Arity) ->
    F = fun (Node, _Others) ->
		case wrangler_syntax:type(Node) of
		    application ->
			Op = wrangler_syntax:application_operator(Node),
			case get_fun_def_info(Op) of
			    {M1, F1, A1} ->
				case lists:keysearch({M1, F1, A1}, 1, wrangler_misc:apply_style_funs()) of
				    {value, _} ->
					{Node, true};
				    _ ->
					{[], false}
				end;
			    _ -> {[], false}
			end;
		    implicit_fun ->
			Name = wrangler_syntax:implicit_fun_name(Node),
			case wrangler_syntax:type(Name) of
			    arity_qualifier ->
				FunName1 = wrangler_syntax:atom_value(wrangler_syntax:arity_qualifier_body(Name)),
				Arity1 = wrangler_syntax:integer_value(wrangler_syntax:arity_qualifier_argument(Name)),
				case {FunName1, Arity1} of
				    {FunName, Arity} -> {Node, true};
				    _ -> {[], false}
				end;
			    module_qualifier ->
				Mod = wrangler_syntax:module_qualifier_argument(Name),
				Body = wrangler_syntax:module_qualifier_body(Name),
				case wrangler_syntax:type(Mod) == atom andalso wrangler_syntax:atom_value(Mod) == ModName of
				    true ->
					FunName1 = wrangler_syntax:atom_value(wrangler_syntax:arity_qualifier_body(Body)),
					Arity1 = wrangler_syntax:integer_value(wrangler_syntax:arity_qualifier_argument(Body)),
					case {FunName1, Arity1} of
					    {FunName, Arity} -> {Node, true};
					    _ -> {[], false}
					end;
				    false -> {[], false}
				end
			end;
		    _ -> {[], false}
		end
	end,
    element(2, api_ast_traverse:once_tdTU(F, AnnAST, [])).

generalise_recursive_function_call(Exp, ModName, FunName, Arity) ->
    Fun = fun (T, S) ->
		  case lists:keysearch(fun_def, 1, wrangler_syntax:get_ann(T)) of
		      {value, {fun_def, {ModName, FunName, Arity, _P1, _P2}}} ->
			  [true| S];
		      _ ->
			  S
		  end
	  end,
    lists:member(true, api_ast_traverse:fold(Fun, [], Exp)).

has_multiple_definitions(AnnAST, ModName, FunName, Arity) ->
    Fun=fun(F) ->
		case  lists:keysearch(fun_def, 1, wrangler_syntax:get_ann(F)) of
		    {value, {fun_def, {ModName, FunName,Arity, _, _}}} ->
			true;
		    _ -> false
		end
	end,
    Forms = wrangler_syntax:form_list_elements(AnnAST),
    Fs =[F||F<-Forms, Fun(F)],
    length(Fs)>1.

check_side_effect(FileName, Exp, SearchPaths) ->
    case wrangler_side_effect:has_side_effect(FileName, Exp, SearchPaths) of
	unknown -> case api_refac:free_vars(Exp) of
		       [] -> unknown;
		       _ -> true
		   end;
	V -> V
    end.

get_fun_def_loc(Node) ->
    As = wrangler_syntax:get_ann(Node),
    case lists:keysearch(fun_def, 1, As) of 
	{value, {fun_def, {_M, _N, _A, _P, DefPos}}} ->
	    DefPos;
	_ -> false
    end.

get_fun_def_info(Node) ->
    As = wrangler_syntax:get_ann(Node),
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
    Cs = wrangler_syntax:function_clauses(FunDef),
    {EStart, EEnd} = wrangler_misc:start_end_loc(Expr),
    Res = [C || C <- Cs, {CStart, CEnd} <- [wrangler_misc:start_end_loc(C)],
		CStart =< EStart, EEnd =< CEnd],
    case Res of
	[] ->
	    none;
	_ -> hd(Res)
    end.

search_duplications(Tree, Exp) ->
    F = fun (Node, Acc) ->
		As = wrangler_syntax:get_ann(Node),
		case lists:keysearch(category, 1, As) of
		    {value, {category, expression}} ->
			case wrangler_syntax:is_literal(Node) andalso Node =/= Exp of
			    true ->
				case wrangler_syntax:concrete(Node) ==
				       wrangler_syntax:concrete(Exp)
				    of
				    true -> [Node| Acc];
				    _ -> Acc
				end;
			    false -> Acc
			end;
		    _ -> Acc
		end
	end,
    case wrangler_syntax:is_literal(Exp) of
	true ->
	    Es = lists:reverse(api_ast_traverse:fold(F, [], Tree)),
	    [wrangler_misc:start_end_loc(E) || E <- Es];
	_ -> []
    end.
	
do_replace_underscore(Tree, _Others) ->
    case wrangler_syntax:type(Tree) of
	underscore ->
	    {wrangler_syntax:atom(undefined), true};
	_ -> {Tree,false}
    end.
    

reset_attr(Node, Key) ->
    api_ast_traverse:full_buTP(fun (T, _Others) ->
				       Ann = wrangler_syntax:get_ann(T),
				       NewAnn = lists:keydelete(Key, 1, Ann),
				       wrangler_syntax:set_ann(T, NewAnn)
			       end, Node, {}).

list_elements(Node) ->
    lists:reverse(list_elements(Node, [])).

list_elements(Node, As) ->
    case wrangler_syntax:type(Node) of
	list ->
	    As1 = lists:reverse(wrangler_syntax:list_prefix(Node)) ++ As,
	    case wrangler_syntax:list_suffix(Node) of
		none -> As1;
		Tail -> list_elements(Tail, As1)
	    end;
	_ -> As  %% not necessary to be a proper list here.
    end.

term_to_list(Term) ->
    binary_to_list(term_to_binary(Term)).

list_to_term(List)->
    binary_to_term(list_to_binary(List)).

