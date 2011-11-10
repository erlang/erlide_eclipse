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
%% Some program slicing algorithms.
%%
%% @author Huiqing Li <hl@kent.ac.uk>
%%   [http://www.cs.kent.ac.uk/projects/forse]

%% @version  0.1
%%
%% @doc Forward and backward slicing of Erlang programs. The current implementation does 
%%      not handle message passing, and is not fully tested yet.
%% @end


%%THIS MODULE IS NOT COMPLETE YET, and will be rewritten.
%% @private
-module(wrangler_slice). 

-export([forward_slice/5, backward_slice/5]). 

-include("../include/wrangler_internal.hrl").

%%% TOdo: rename process name to 'wrangler_forward_slicer'.
%% TODO: extend the slicer to go into the inner of case/receive/if exprs.


%% @spec forward_slice(Files::[filename()], AnnAST::syntaxTree(), ModName::atom(), FunDef::syntaxTree(), Expr::syntaxTree()) -> [syntaxTree()]
%% @doc Forward slice the program with expression Expr, which in contained in function FunDef, as the slicing criterion.                     
 
%%-spec(forward_slice/5::([filename()], syntaxTree(), atom(), syntaxTree(), syntaxTree())->
%%	     [syntaxTree()]).
forward_slice(Files, AnnAST, ModName, FunDef, Expr)  ->
    start_slice_env_process(),
    Res = forward_slice_1(Files, AnnAST, ModName, {FunDef, Expr}),
    stop_slice_env_process(),
    Res.

forward_slice_1(Files, AnnAST, ModName, {FunDef, Expr}) ->
    FunName = wrangler_syntax:function_name(FunDef),
    FunName1 = wrangler_syntax:data(FunName),
    Arity = wrangler_syntax:function_arity(FunDef),
    FunClauses = wrangler_syntax:function_clauses(FunDef),
    NewFunClauses = [process_a_clause(Files, AnnAST, ModName, FunName1, Arity, C, Expr)||C<-FunClauses],
    NewFunDef = wrangler_syntax:copy_attrs(FunDef, wrangler_syntax:function(FunName, NewFunClauses)),
    sliced_funs ! {add, {{ModName, FunName1, Arity, wrangler_misc:start_end_loc(Expr)}, NewFunDef}},
    case returns_undefined(NewFunDef) of
	true ->    %% None of the variables depending on the selected expression is exported.
	    get_all_sliced_funs();
	false ->
	    CallerFuns = get_caller_funs(Files, {ModName, FunName1, Arity}),
	    F = fun (T, Acc) -> case wrangler_syntax:type(T) of
				    application ->
					Op = wrangler_syntax:application_operator(T),
					Ann = wrangler_syntax:get_ann(Op),
					case lists:keysearch(fun_def, 1, Ann) of
					    {value, {fun_def, {ModName, FunName1, Arity, _, _}}} ->
						[T| Acc];
					    _ -> Acc
					end;
				    _ -> Acc
				end
		end,
	    case CallerFuns of
		[] -> get_all_sliced_funs();
		_ -> SliceCriterion = lists:flatmap(fun (FunDef1) -> AppExprs = api_ast_traverse:fold(F, [], FunDef1),
								     lists:map(fun (E) -> {FunDef1, E} end, AppExprs)
						    end, CallerFuns),
		     lists:flatmap(fun (SC) -> forward_slice_1(Files, AnnAST, ModName, SC) end, SliceCriterion)
	    end
    end.

returns_undefined(FunDef) ->
   F= fun(C) ->
	       Body = wrangler_syntax:clause_body(C),
	       Expr = lists:last(Body),
	       case wrangler_syntax:type(Expr) of
		   atom -> wrangler_syntax:atom_value(Expr) == undefined;
		   _ -> false
	       end
       end,
   FunClauses = wrangler_syntax:function_clauses(FunDef),
   lists:all(fun(C) -> F(C) end, FunClauses).
    


get_caller_funs(Files, {ModName, FunName, Arity}) ->
    CallGraph = wrangler_callgraph_server:build_callercallee_callgraph(Files),
    lists:flatmap(fun ({{_Caller, CallerDef}, Callee}) ->
			  case lists:member({ModName, FunName, Arity}, Callee) of
			    true -> [CallerDef];
			    _ -> []
			  end
		  end, CallGraph).
    

start_slice_env_process() ->    
    case erlang:whereis(sliced_funs) of 
	undefined -> ok;
	_         -> erlang:unregister(sliced_funs)
    end,
    register(sliced_funs, spawn(fun()->sliced_funs([]) end)).

stop_slice_env_process() ->
    sliced_funs!stop.

get_all_sliced_funs() ->
    sliced_funs!{self(), get_all},
    receive 
	{sliced_funs, State} ->
	    State
    end.
	
sliced_funs(State) ->
    receive
	{From, get, Key} ->
	    Res = case lists:keysearch(Key, 1, State) of
		      {value, {Key, Value}} ->
			   {sliced_funs, value, Value};
		      false -> {sliced_funs, false}
		  end,
	    From ! Res,
	    sliced_funs(State);
	{add, {Key, Value}} ->
	    case lists:keysearch(Key, 1, State) of 
		    {value, {Key, _}} ->  %% This should not happen.
			State1 =lists:keyreplace(Key,1, State, {Key, Value}), 
			sliced_funs(State1);
		    false -> sliced_funs([{Key, Value}|State])
		end;
	 {From, get_all} ->
	    From ! {sliced_funs, State},
	    sliced_funs(State);
	 stop ->
		ok
	end.

process_a_clause(Files, AnnAST, ModName, FunName, Arity, C, Expr) ->
    ExportedVars = wrangler_misc:exported_vars(Expr),
    Patterns = wrangler_syntax:clause_patterns(C),
    Guard = wrangler_syntax:clause_guard(C),
    Body = wrangler_syntax:clause_body(C),
    Body1 = rm_unrelated_exprs(Files, AnnAST, ModName, FunName, Arity, Body, Expr, ExportedVars),
    wrangler_syntax:clause(Patterns, Guard, Body1).

rm_unrelated_exprs(_Files, _AnnAST, _ModName, _FunName, _Arity, [], _Expr, _Vars) ->
    [];
rm_unrelated_exprs(Files, AnnAST, ModName, FunName, Arity, [E| Exprs], Expr, Vars) ->
    FreeVars = api_refac:free_vars(E),
    ExportedVars = wrangler_misc:exported_vars(E),
    case ExportedVars -- Vars =/= ExportedVars of
	true ->
	    [E| rm_unrelated_exprs(Files, AnnAST, ModName, FunName, Arity, Exprs, Expr, lists:usort(Vars ++ ExportedVars))];
	false -> case FreeVars -- Vars =/= FreeVars of
		     true ->
			 Env = api_refac:env_vars(E),
			 E1 = process_fun_applications(Files, AnnAST, ModName, FunName, Arity, E, Vars),
			 E2 = wrangler_syntax_lib:annotate_bindings(reset_attrs(E1), Env),
			 FreeVars1 = api_refac:free_vars(E2),
			 case FreeVars1 -- Vars =/= FreeVars1 of
			     true ->
				 [E2| rm_unrelated_exprs(Files, AnnAST, ModName, FunName, Arity, Exprs, Expr, lists:sort(Vars ++ wrangler_misc:exported_vars(E2)))];
			     _ ->
				 {Start1, End1} = wrangler_misc:start_end_loc(Expr),
				 {Start2, End2} = wrangler_misc:start_end_loc(E2),
				 case Start2 =< Start1 andalso End1 =< End2 of
				     true -> [E2| rm_unrelated_exprs(Files, AnnAST, ModName, FunName, Arity, Exprs, Expr, lists:sort(Vars ++ wrangler_misc:exported_vars(E2)))];
				     _ -> case Exprs of
					      [] -> [wrangler_syntax:atom(undefined)];
					      _ -> rm_unrelated_exprs(Files, AnnAST, ModName, FunName, Arity, Exprs, Expr, Vars)
					  end
				 end
			 end;
		     _ ->
			 {Start1, End1} = wrangler_misc:start_end_loc(Expr),
			 {Start2, End2} = wrangler_misc:start_end_loc(E),
			 case Start2 =< Start1 andalso End1 =< End2 of
			     true -> [E| rm_unrelated_exprs(Files, AnnAST, ModName, FunName, Arity, Exprs, Expr, lists:sort(Vars ++ wrangler_misc:exported_vars(E)))];
			     _ -> rm_unrelated_exprs(Files, AnnAST, ModName, FunName, Arity, Exprs, Expr, Vars)
			 end
		 end
    end.

    
reset_attrs(Node) ->
    api_ast_traverse:full_buTP(fun (T, _Others) ->
				       As = wrangler_syntax:get_ann(T),
				       As0 = lists:keydelete(free, 1, As),
				       As1 = lists:keydelete(bound, 1, As0),
				       As2 = lists:keydelete(env, 1, As1),
				       wrangler_syntax:set_ann(T, As2)
			       end, Node, {}).

	    
	    
  
intra_fun_forward_slice(Files, AnnAST, ModName, FunDef, PatIndex) ->
    FunName = wrangler_syntax:function_name(FunDef),
    Arity = wrangler_syntax:function_arity(FunDef),
    Cs = wrangler_syntax:function_clauses(FunDef),
    FunName1 = wrangler_syntax:data(FunName),
    Cs1 = [process_a_clause_1(Files, AnnAST, ModName, FunName1, Arity,C, PatIndex)||C<-Cs],
    wrangler_syntax:copy_attrs(FunDef, wrangler_syntax:function(FunName, Cs1)).

process_a_clause_1(Files, AnnAST, ModName, FunName, Arity, C, PatIndex) ->
    Patterns = wrangler_syntax:clause_patterns(C),
    Body = wrangler_syntax:clause_body(C),
    Vars = lists:flatmap(fun (I) -> wrangler_misc:exported_vars(lists:nth(I, Patterns)) end, PatIndex),
    Body1 = process_fun_body(Files, AnnAST, ModName, FunName, Arity, Body, Vars),
    wrangler_syntax:clause(Patterns, none, Body1).

process_fun_body(_Files, _AnnAST, _ModName, _FunName, _Arity, [], _Vars) ->
    [];
process_fun_body(Files, AnnAST, ModName, FunName, Arity, [E], Vars) ->
    E1 = process_fun_applications(Files, AnnAST, ModName, FunName, Arity, E, Vars),
    FreeVars = api_refac:free_vars(E1),
    ExportedVars = wrangler_misc:exported_vars(E1),
    case ExportedVars -- Vars =/= ExportedVars of
	true -> [E];
	false -> case FreeVars -- Vars =/= FreeVars of
		     true ->
			 %% check free/exported vars again?
			 [E1];
		     _ ->
			 [wrangler_syntax:atom(undefined)]
		 end
    end;
process_fun_body(Files, AnnAST, ModName, FunName, Arity, [E| Exprs], Vars) ->
    E1 = process_fun_applications(Files, AnnAST, ModName, FunName, Arity, E, Vars),
    FreeVars = api_refac:free_vars(E1),
    ExportedVars = wrangler_misc:exported_vars(E1),
    case ExportedVars -- Vars =/= ExportedVars of
	true -> [E| process_fun_body(Files, AnnAST, ModName, FunName, Arity, Exprs, lists:usort(Vars ++ ExportedVars))];
	false -> case FreeVars -- Vars =/= FreeVars of
		     true ->
			 [E1| process_fun_body(Files, AnnAST, ModName, FunName, Arity, Exprs, lists:sort(Vars ++ wrangler_misc:exported_vars(E1)))];
		     _ ->
			 process_fun_body(Files, AnnAST, ModName, FunName, Arity, Exprs, Vars)
		 end
    end.


process_fun_applications(Files, AnnAST, ModName, FunName, Arity, E, Vars) ->
    api_ast_traverse:full_buTP(fun do_process_fun_applications/2, E, {Files, AnnAST, ModName, FunName, Arity, Vars}).

do_process_fun_applications(Node, {Files, AnnAST, ModName, FunName, Arity, Vars}) ->
    case wrangler_syntax:type(Node) of
	application ->
	    FreeVars = api_refac:free_vars(Node),
	    case
		FreeVars -- Vars =/= FreeVars   %% the function application makes use of some of the variables in Vars;
		of
		true -> Operator = wrangler_syntax:application_operator(Node),
			Ann = wrangler_syntax:get_ann(Operator),
			Args = wrangler_syntax:application_arguments(Node),
			IndexedArgs = lists:zip(lists:seq(1, length(Args)), Args),
			FilteredIndexedArgs = lists:filter(fun ({_, Arg}) ->
								   FVars = api_refac:free_vars(Arg),
								   FVars -- Vars =/= FVars
							   end, IndexedArgs),
			FilteredIndex = element(1, lists:unzip(FilteredIndexedArgs)),
			case lists:keysearch(fun_def, 1, Ann) of
			    {value, {fun_def, {M, F, A, _, DefPos}}} ->
				case {M, F, A} of
				    {ModName, FunName, Arity} -> Node;
				    _ ->
					sliced_funs ! {self(), get, {M, F, A, FilteredIndex}},
					receive
					    {sliced_funs, value, {{M, F, A, FilteredIndex}, FunDef1}} ->
						case returns_undefined(FunDef1) of
						    true -> wrangler_syntax:atom(undefined);
						    _ -> Node
						end;
					    _ ->
						FileName1 = lists:filter(fun (F1) -> list_to_atom(filename:basename(F1, ".erl")) == M
									 end, Files),
						case FileName1 of
						    [] ->
							Node;
						    _ -> FileName = hd(FileName1),
							 {ok, {AnnAST1, _Info}} = wrangler_ast_server:parse_annotate_file(FileName, true, Files),
							 case
							     api_interface:pos_to_fun_def(AnnAST1, DefPos)    %% TO check: how to you have this DefPos.
							     of
							     {ok, FunDef} ->
								 FunDef1 = intra_fun_forward_slice(Files, AnnAST, ModName, FunDef, FilteredIndex),
								 sliced_funs ! {add, {{M, F, A, FilteredIndex}, FunDef1}},
								 case returns_undefined(FunDef1) of
								     true -> wrangler_syntax:atom(undefined);
								     _ -> Node
								 end;
							     _ -> Node
							 end
						end
					end
				end;
			    _ -> Node   %% no fun_def annotation. This should not happen.
			end;
		_ -> wrangler_syntax:atom(undefined)
	    end;
	_ -> Node
    end.

%%=========================================================================================================
%% TODO: Need to think what term() returned by backward_slice() really is
%% @spec backward_slice(Files::[filename()], AnnAST::syntaxTree(), ModName::atom(), FunDef::syntaxTree(), Expr::syntaxTree()) -> term()
%% @doc Backward slice the program with expression Expr, which is contained in function FunDef, as the slicing criterion.      


%%-spec(backward_slice/5::([filename()], syntaxTree(), atom(), syntaxTree(), syntaxTree())->[any()]).  %% any needs to be refined here.
backward_slice(Files, AnnAST, ModName, FunDef, Expr) ->
    FunName = wrangler_syntax:data(wrangler_syntax:function_name(FunDef)),
    Arity = wrangler_syntax:function_arity(FunDef),
    NewFunDef1 = backward_slice(Expr, FunDef),
    NewFunDef2 = unfold_fun_defs(Files, AnnAST, ModName, NewFunDef1),
    C = hd(wrangler_syntax:function_clauses(NewFunDef2)),
    Body = wrangler_syntax:clause_body(C),
    {_, FreeVarsInBody} = get_bound_free_vars(Body),
    case FreeVarsInBody of
	[] ->
	    [wrangler_syntax:block_expr(Body)];
	_ ->
	    Patterns = wrangler_syntax:clause_patterns(C),
	    NewPatterns = lists:map(fun (P) ->
					    BdVars = api_refac:bound_vars(P),
					    case FreeVarsInBody -- BdVars =/= FreeVarsInBody of
						true -> P;
						_ -> wrangler_syntax:underscore()
					    end
				    end, Patterns),
	    C1 = wrangler_syntax:clause(NewPatterns, wrangler_syntax:clause_guard(C), wrangler_syntax:clause_body(C)),
	    SlicePoints = collect_app_sites(AnnAST, ModName, FunName, Arity),
	    case SlicePoints of
		[] -> [wrangler_syntax:block_expr(Body)]; %% could not find any use sites of this function.
		_ -> Slices = lists:map(fun ({Fun, S}) ->
						PsCs = lists:zip(NewPatterns, S),
						lists:flatmap(fun ({P, E}) ->
								      case wrangler_syntax:type(P) of
									  underscore -> [wrangler_syntax:atom('_')];
									  _ -> backward_slice(Files, AnnAST, ModName, Fun, E)
								      end
							      end, PsCs)
					end, SlicePoints),
		     FunExpr = wrangler_syntax:fun_expr([C1]),
		     Result = lists:map(fun (S) -> wrangler_syntax:application(FunExpr, S) end, Slices),
		     Result
	    end
    end.

collect_app_sites(AnnAST, ModName, FunName, Arity) ->
    HandleSpecialFuns = fun (Args) ->
				Args1 = list_to_tuple(lists:reverse(Args)),
				A = element(1, Args1),
				F = element(2, Args1),
				M = element(3, Args1),
				case {wrangler_syntax:type(M), wrangler_syntax:type(F), wrangler_syntax:type(A)} of
				    {atom, atom, list} ->
					case {wrangler_syntax:atom_value(M), wrangler_syntax:atom_value(F), wrangler_syntax:list_length(A)} of
					    {ModName, FunName, Arity} -> [wrangler_syntax:list_elements(A)];
					    _ -> []
					end;
				    _ -> []
				end
			end,
    F1 = fun (T,Acc) ->
		 case wrangler_syntax:type(T) of
		     application ->
			 Op = wrangler_syntax:application_operator(T),
			 Args = wrangler_syntax:application_arguments(T),
			 Ann = wrangler_syntax:get_ann(Op),
			 case lists:keysearch(fun_def, 1, Ann) of
			     {value, {fun_def, {M, F, A, _, _}}} ->
				 case {M, F, A} of
				     {ModName, FunName, Arity} ->
					 case Args of
					     [] -> Acc;
					     _ -> Acc ++ [Args]
					 end;
				     {erlang, apply, 3} -> HandleSpecialFuns(Args)++Acc;
				     {erlang, spawn, 3} -> HandleSpecialFuns(Args)++Acc;
				     {erlang, spawn, 4} -> HandleSpecialFuns(Args)++Acc;
				     {erlang, spawn_link, 3} -> HandleSpecialFuns(Args)++Acc;
				     {erlang, spawn_link, 4} -> HandleSpecialFuns(Args)++Acc;
				     _ -> Acc
				 end;
			     _ -> Acc
			 end;
		     _ -> Acc
		 end
	 end,
    F = fun (T, Acc) ->
		case wrangler_syntax:type(T) of
		    function ->
			Acc1 = api_ast_traverse:fold(F1, [], T),
			case Acc1 of
			    [] -> Acc;
			    _ -> Acc ++ lists:map(fun (E) -> {T, E} end, Acc1)
			end;
		    _ -> Acc
		end
	end,
    api_ast_traverse:fold(F, [], AnnAST).
unfold_fun_defs(_Files, AnnAST, ModName, FunDef %% How about recursive functions?
                                               ) ->
    F = fun (Node, _Others) ->
		case wrangler_syntax:type(Node) of
		  application ->
		      Operator = wrangler_syntax:application_operator(Node),
		      Ann = wrangler_syntax:get_ann(Operator),
		      case lists:keysearch(fun_def, 1, Ann) of
			{value, {fun_def, {ModName, _F, _A, _, DefPos}}}  %% TOCHANGE: temporaly assume the function is local.
			                                                 ->
                            case api_interface:pos_to_fun_def(AnnAST, DefPos) of
			      {ok, Def} ->
				  Cs = wrangler_syntax:function_clauses(Def),
				  FunExpr = wrangler_syntax:fun_expr(Cs),
				  Args = wrangler_syntax:application_arguments(Node),
				  {wrangler_syntax:application(FunExpr, Args), true};
			      _ -> {Node, false}
			    end;
			_ -> {Node, false}
		      end;
		  _ -> {Node, false}
		end
	end,
    {FunDef1, _} = api_ast_traverse:stop_tdTP(F, FunDef, []),
    FunDef2 = wrangler_syntax_lib:annotate_bindings(reset_attrs(FunDef1), []),
    FunDef2.

%% backward slice within a single function.
backward_slice(Expr, FunDef) ->
    FunName = wrangler_syntax:function_name(FunDef),
    {S, E} = wrangler_misc:start_end_loc(Expr),
    FunClauses = wrangler_syntax:function_clauses(FunDef),
    Pred = fun (Node) ->
		   {StartPos, EndPos} = wrangler_misc:start_end_loc(Node),
		   S >= StartPos andalso E =< EndPos
	   end,
    %% Get the function clause to which the expression belongs.
    C = hd(lists:filter(fun (Clause) -> Pred(Clause) end, FunClauses)),
    Patterns = wrangler_syntax:clause_patterns(C),
    C1 = process_a_clause(C, Expr),
    NewFun = wrangler_syntax:function(FunName, C1),
    %% to keep the annotation info correct.
    NewFun1 = wrangler_syntax_lib:annotate_bindings(reset_attrs(NewFun), []),
    Body = wrangler_syntax:clause_body(hd(wrangler_syntax:function_clauses(NewFun1))),
    Body1 = rm_unused_exprs(Body),  %%Qn: how about the guard expression?
    NewFun2 = wrangler_syntax:function(FunName, [wrangler_syntax:clause(Patterns, none, Body1)]),
    %%?wrangler_io(refac_prettypr:format(NewFun),[]),
    NewFun2.

process_a_clause(C, Expr) ->
    Patterns = wrangler_syntax:clause_patterns(C),
    Body = wrangler_syntax:clause_body(C),
    NewBody = process_body(Body, Expr),
    FreeVars = api_refac:free_vars(Expr),
    case NewBody == [wrangler_syntax:tuple([wrangler_syntax:atom(error), wrangler_syntax:atom("Error with evaluation")])] of
	true -> [];
	_ ->
	    BoundVars = lists:flatmap(fun (P) -> api_refac:bound_vars(P) end, Patterns),
	    case FreeVars -- BoundVars =/= FreeVars of
		true  %% Expr uses some of the vars declared in Patterns.
		     ->
                    C1 = wrangler_syntax:clause(Patterns, none, NewBody),
		    {Bound1, Free1} = lists:foldl(fun (P, {Bd, Fr}) ->
							  {Bd1, Fr1} = {api_refac:bound_vars(P), api_refac:free_vars(P)},
							  {ordsets:union(Bd, Bd1), ordsets:union(Fr, Fr1)}
						  end,
						  {[], []}, Patterns),
		    {Bound2, Free2} = get_bound_free_vars(NewBody),
		    Bound = ordsets:union(Bound1, Bound2),
		    Free = ordsets:union(Free1, ordsets:subtract(Free2, Bound1)),
		    C2 = wrangler_misc:update_ann(wrangler_misc:update_ann(C1, {bound, Bound}), {free, Free}),
		    [C2];
		_ -> %% Expr does not use any of the vars declared in Patterns.
		    {Bound, Free} = get_bound_free_vars(NewBody),
		    C1 = wrangler_syntax:clause([wrangler_syntax:underscore()], none, NewBody),  %% replace patterns with undersocre.
		    C2 = wrangler_misc:update_ann(wrangler_misc:update_ann(C1, {bound, Bound}), {free, Free}),
		    [C2]
	    end
    end.

%% If Expr belongs to Body, then remove those expressions that will be evaluated after Expr, since 
%% those expressions do not contribute to the value of Expr.
process_body(Body, Expr) ->
    {S, E} = wrangler_misc:start_end_loc(Expr),
    FreeVars = api_refac:free_vars(Expr),
    FstExp = hd(Body),
    LstExp = lists:last(Body),
    {S1, _} = wrangler_misc:start_end_loc(FstExp),
    {_, E1} = wrangler_misc:start_end_loc(LstExp),
    case S1 =< S andalso E =< E1 of
	true ->
	    %% Expr is part of body.
	    case FreeVars of
		[] -> [Expr];        %% The selected expr does not has any free vars.
		_ ->
		    FreeVarDefLocs = lists:map(fun ({_V, DefLoc}) -> DefLoc end, FreeVars),
		    LastLoc = lists:last(lists:sort(FreeVarDefLocs)),
		    Exprs1 = lists:takewhile(fun (BodyExpr) ->
						     {StartPos, EndPos} = wrangler_misc:start_end_loc(BodyExpr),
						     (EndPos =< S) or (S >= StartPos andalso E =< EndPos)
					     end,
					     Body),
		    LastExpr1 = lists:last(Exprs1), %% The expression that contains Expr.
		    {LastExprStartPos, _} = wrangler_misc:start_end_loc(LastExpr1),
		    LastExpr = case LastLoc >= LastExprStartPos of
				   false -> Expr;  %% The last expr does not declare any free vars of Expr
				   true -> %% some of the free vars in Expr are introduced in the LastExpr1.
				       %% This function needs to make sure Expr or {error, error with evalution} is 
				       %% the lasted expression to evaluate in the processed LastExpr1.
				       process_expr(LastExpr1, Expr)  %% LastExpr1 is a complex expr, such as case/if/receive exprs.
			       end,
		    NewExprs = lists:reverse(tl(lists:reverse(Exprs1))) ++ [LastExpr],
		    rm_unused_exprs(NewExprs)
	    end;
	%% Expr is not part of Body.
	false -> [wrangler_syntax:tuple([wrangler_syntax:atom(error), wrangler_syntax:atom("Error with evaluation")])]
    end.

%% Expr is part of LastExpr. This function tries to simplify LastExpr to remove those parts that do not 
%% affect the value of Expr.
%% IMPORTANT:
%% 1) Make sure that the last evaluated expression is either Expr or {error, "Error with evaluation"}.
%% 2) The slicing process should not change the binding structure of variables.
process_expr(LastExpr, Expr) ->
    GetExprBody = fun (E) ->
			  case wrangler_syntax:type(E) of
			      match_expr -> get_match_expr_body(E);
			      _ -> E
			  end
		  end,
    E = GetExprBody(LastExpr),
    case wrangler_syntax:type(E) of
	case_expr ->
	    Args = wrangler_syntax:case_expr_argument(E),
	    {Bound1, Free1} = {api_refac:bound_vars(Args), api_refac:free_vars(Args)},
	    Clauses = wrangler_syntax:case_expr_clauses(E),
	    NewClauses = lists:flatmap(fun (C) -> process_a_clause(C, Expr)
				       end, Clauses), %% process each case clause.
	    {Bound2, Free2} = lists:foldl(fun (C, {Bd, Fr}) ->
						  {Bd1, Fr1} = {api_refac:bound_vars(C), api_refac:free_vars(C)},
						  {ordsets:intersection(Bd, Bd1), ordsets:union(Fr, Fr1)}
					  end,
					  {[], []}, NewClauses),
	    Bound = ordsets:union(Bound1, Bound2),
	    Free = ordsets:union(Free1, Free2),
	    E1 = wrangler_syntax:case_expr(Args, NewClauses),
	    %% updated the annotation.
	    E2 = wrangler_misc:update_ann(wrangler_misc:update_ann(E1, {bound, Bound}), {free, Free}),
	    E2;
	block_expr ->
	    Body = wrangler_syntax:block_expr_body(E),
	    NewBody = process_body(Body, Expr),
	    {Bound, Free} = get_bound_free_vars(NewBody),
	    BE = wrangler_syntax:block_expr(NewBody),
	    wrangler_misc:update_ann(wrangler_misc:update_ann(BE, {bound, Bound}), {free, Free});
	if_expr ->
	    Clauses = wrangler_syntax:if_expr_clauses(E),
	    NewClauses = lists:flatmap(fun (C) -> process_a_clause(C, Expr) end, Clauses),
	    {Bound, Free} = lists:foldl(fun (C, {Bd, Fr}) ->
						{Bd1, Fr1} = {api_refac:bound_vars(C), api_refac:free_vars(C)},
						{ordsets:intersection(Bd, Bd1), ordsets:union(Fr, Fr1)}
					end,
					{[], []}, NewClauses),
	    IE = wrangler_syntax:if_expr(NewClauses),
	    wrangler_misc:update_ann(wrangler_misc:update_ann(IE, {bound, Bound}), {free, Free});
	%%	receive_expr -> LastExpr;
	%%fun_expr ->  %% IMPORTANT: fun exprs need more attection, as it is a function closure. 
	%% lists comprehension is another problem. (find the example !!)
	%% catch_expr ->
	%% Any other possibilities?
	_ -> wrangler_syntax:tuple([wrangler_syntax:atom(error), wrangler_syntax:atom("Error with evaluation")])
    end.

%% this is the function that does the backward slicing.
rm_unused_exprs([]) -> [];
rm_unused_exprs(Exprs) ->
    LastExpr = lists:last(Exprs),
    FreeVars = api_refac:free_vars(LastExpr),
    ReversedPrevExprs = tl(lists:reverse(Exprs)),
    Res = rm_unused_exprs_1(ReversedPrevExprs, FreeVars, [LastExpr]),
    Res.

rm_unused_exprs_1([], _FreeVars, Acc) -> Acc;
rm_unused_exprs_1([E| Exprs], FreeVars, Acc) ->
    ExportedVars = wrangler_misc:exported_vars(E),
    case FreeVars -- ExportedVars =/= FreeVars of
	true ->
	    FreeVarsInE = api_refac:free_vars(E),
	    NewFreeVars = lists:usort((FreeVars -- ExportedVars) ++ FreeVarsInE),
	    rm_unused_exprs_1(Exprs, NewFreeVars, [E| Acc]);
	false -> rm_unused_exprs_1(Exprs, FreeVars, Acc)
    end.

get_match_expr_body(E) ->
    Body = wrangler_syntax:match_expr_body(E),
    case Body of
      match_expr -> get_match_expr_body(Body);
      _ -> Body
    end.

get_bound_free_vars(Body) ->
    lists:foldl(fun (E, {Bd, Fr}) ->
			{Bd1, Fr1} = {api_refac:bound_vars(E), api_refac:free_vars(E)},
			{ordsets:union(Bd, Bd1), ordsets:union(Fr, ordsets:subtract(Fr1, Bd))}
		end,
		{[], []}, Body).
