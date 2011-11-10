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
%% Refactoring: Annotate the AST representation of an Erlang program with process information.

%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%%================================================================================================
%% This module trys to annotate AST with the spawn information associated with a process identifer. But
%% because complex pattern binding and message passing are not considered yet, the annotation information
%% is only partial.


%% TODO: Summarise the limitations of this module.
%%@private
-module(wrangler_annotate_pid).

-export([ann_pid_info/2]).

-include("../include/wrangler_internal.hrl").

%%-spec(ann_pid_info/2::([dir()], integer())->ok).
ann_pid_info(DirList, TabWidth) ->
    Files = wrangler_misc:expand_files(DirList, ".erl"),
    SortedFuns = sort_funs(DirList),
    start_counter_process(),
    Pid = start_fun_typesig_process([]),            %% Refactor this USING WRANGLER:  register a process, and remove the uses of Pid.
    SortedFuns1 = do_ann_pid_info(SortedFuns, Pid),
    stop_counter_process(),
    lists:foreach(fun (File) -> {File, update_function(File, SortedFuns1, DirList, TabWidth)} end, Files),
    ok.
    

do_ann_pid_info(Funs, Pid) ->
    Funs1 = bottom_up_ann(Funs, Pid),
    fixpoint(Funs1, Pid).

bottom_up_ann(Funs, TypeSigPid) ->
    lists:map(fun (F) -> bottom_up_ann_1(F, TypeSigPid) end,
	      Funs).

bottom_up_ann_1({{ModName, FunName, Arity}, FunDef}, TypeSigPid) ->
    EnvPid = start_env_process(),
    FunDef1 = annotate_special_fun_apps({{ModName, FunName, Arity}, FunDef}, EnvPid),
    FunDef2 = api_ast_traverse:full_buTP(fun annotate_within_fun/2, FunDef1, {ModName, FunName, Arity, EnvPid, TypeSigPid}),
    EnvPid ! stop,
    {{ModName, FunName, Arity}, FunDef2}.


do_topdown_prop(Funs, Pid) ->
    Funs1=lists:map(fun ({F, FunDef}) -> 
			    EnvPid = start_env_process(),
			    FunDef1 = prop_from_calls(FunDef, Pid),
			    EnvPid ! stop, 
			    {F, FunDef1} 
		    end,
		    lists:reverse(Funs)),
    lists:map(fun (F) -> annotate_within_fun_1(F, Pid) end, Funs1).
    

fixpoint(Funs, TypeSigPid) ->
     TypeSigPid ! {self(), getenv},
     receive
 	{TypeSigPid, Env} ->
 	    Env
     end,
     Funs1 =do_topdown_prop(Funs, TypeSigPid),
     TypeSigPid ! {self(), getenv},
     receive
 	{TypeSigPid, Env1} ->
 	    Env1
     end,
     case Env==Env1 of
 	true ->
 	    TypeSigPid!stop,
 	    Funs1;
 	_ ->
 	    fixpoint(Funs1, TypeSigPid)
     end.

update_function(File, FunList, DirList, TabWidth) ->
    {ok, {AnnAST, Info}} = wrangler_ast_server:parse_annotate_file(File, true, DirList, TabWidth),
    ModName = case lists:keysearch(module, 1, Info) of
		  {value, {module, Mod}} -> Mod;
		  _ -> list_to_atom(filename:basename(File, ".erl"))
	      end,
    F = fun (Node, []) ->
		case wrangler_syntax:type(Node) of
		    function ->
			FunName = wrangler_syntax:data(wrangler_syntax:function_name(Node)),
			Arity = wrangler_syntax:function_arity(Node),
			case lists:keysearch({ModName, FunName, Arity}, 1, FunList) of
			    {value, {{ModName, FunName, Arity}, FunDef}} ->
				{FunDef, true};
			    _ -> {Node, false}
			end;
		    _ -> {Node, false}
		end
	end,
    {AnnAST1, _} = api_ast_traverse:stop_tdTP(F, AnnAST, []),
    wrangler_ast_server:update_ast({File, true, DirList, TabWidth, wrangler_misc:file_format(File)}, 
                                   {AnnAST1, Info, wrangler_misc:filehash(File)}),
    ok.

annotate_within_fun(Node, {_ModName, FunName, Arity, EnvPid, TypeSigPid}) ->
    case wrangler_syntax:type(Node) of
	variable ->
	    Ann = wrangler_syntax:get_ann(Node),
	    case lists:keysearch(def, 1, Ann) of
		{value, {def, DefinePos}} ->
		    EnvPid ! {self(), get, {def, DefinePos}},
		    receive
			{EnvPid, value, Value} ->
			    wrangler_misc:update_ann(Node, Value);
			{EnvPid, false} -> Node
		    end;
		_ -> Node
	    end;
	application ->
	    Operator = wrangler_syntax:application_operator(Node),
	    Args = wrangler_syntax:application_arguments(Node),
	    Ann = wrangler_syntax:get_ann(Operator),
	    case lists:keysearch(fun_def, 1, Ann) of
		{value, {fun_def, {M1, F1, A1, _P1, _2}}} ->
		    TypeSigPid ! {self(), get, {M1, F1, A1}},
		    receive
			{TypeSigPid, value, {ParSig, RtnSig}} ->
			    F = fun ({A, S}) ->
					case S of
					    {p_name, _}    %% Can you do this to Pid?
						       ->
                                                wrangler_misc:update_ann(A, S);
					    _ -> A
					end
				end,
			    Args1 = lists:map(fun ({A, S}) -> F({A, S}) end, lists:zip(Args, ParSig)),
			    Node1 = wrangler_syntax:copy_attrs(Node, wrangler_syntax:application(Operator, Args1)),
			    case RtnSig of
				any -> Node1;
				Pid -> Node2 = wrangler_misc:update_ann(Node1, Pid),
				       Node2
			    end;
			{TypeSigPid, false} ->
			    Node
		    end;
		_ -> Node
	    end;
	match_expr ->
	    P = wrangler_syntax:match_expr_pattern(Node),
	    B = wrangler_syntax:match_expr_body(Node),
	    Ann = wrangler_syntax:get_ann(B),
	    case lists:keysearch(pid, 1, Ann) of
		{value, {pid, Value}} ->
		    P1 = wrangler_misc:update_ann(P, {pid, Value}),
		    case wrangler_syntax:type(P) of
			variable ->
			    Ann1 = wrangler_syntax:get_ann(P),
			    {value, {def, DefinePos}} = lists:keysearch(def, 1, Ann1),
			    EnvPid ! {add, {{def, DefinePos}, {pid, Value}}};
			_ -> ok                %%% What about the complex pattern matches?
		    end,
		    wrangler_syntax:copy_attrs(Node, wrangler_syntax:match_expr(P1, B));
		_ ->
		    case lists:keysearch(p_name, 1, Ann) of
			{value, {p_name, Value}} ->
			    P1 = wrangler_misc:update_ann(P, {p_name, Value}),
			    case wrangler_syntax:type(P) of
				variable ->
				    Ann1 = wrangler_syntax:get_ann(P),
				    {value, {def, DefinePos}} = lists:keysearch(def, 1, Ann1),
				    EnvPid ! {add, {{def, DefinePos}, {p_name, Value}}};
				_ -> ok                %%% What about the complex pattern matches?
			    end,
			    wrangler_syntax:copy_attrs(Node, wrangler_syntax:match_expr(P1, B));
			_ ->
			    Node %% The body is not a pid
		    end
	    end;
	function ->
	    Ann = wrangler_syntax:get_ann(Node),
	    {value, {fun_def, {Mod, FunName, Arity, _, _}}} = lists:keysearch(fun_def, 1, Ann),
	    Cs = wrangler_syntax:function_clauses(Node),
	    case length(Cs) of
		1 ->  %% only handle when the function has only one clause.
		    C = hd(Cs),
		    Ps = wrangler_syntax:clause_patterns(C),
		    LastExp = lists:last(wrangler_syntax:clause_body(C)),
		    Res = case wrangler_syntax:type(LastExp) of
			      match_expr -> wrangler_syntax:match_expr_pattern(LastExp);
			      _ -> LastExp
			  end,
		    ResInfo = case lists:keysearch(pid, 1, wrangler_syntax:get_ann(Res)) of
				  {value, {pid, Value}} -> {pid, Value};
				  _ -> case lists:keysearch(p_name, 1, wrangler_syntax:get_ann(Res)) of
					   {value, {p_name, Value}} -> {p_name, Value};
					   _ -> any
				       end
			      end,
		    F = fun ({P, _T}) ->
				Ann1 = wrangler_syntax:get_ann(P),
				case lists:keysearch(pid, 1, Ann1) of
				    {value, {pid, Value1}} ->
					{pid, Value1};
				    _ -> case lists:keysearch(p_name, 1, Ann1) of
					     {value, {p_name, V}} ->
						 {p_name, V};
					     _ -> any
					 end
				end
			end,
		    TypeSigPid ! {self(), get, {Mod, FunName, Arity}},
		    receive
			{TypeSigPid, value, {ParSig, RtnSig}} ->
			    ParSig1 = lists:map(fun ({P, T}) -> F({P, T}) end, lists:zip(Ps, ParSig)),
			    case RtnSig =/= ResInfo of
				true -> Info = {{Mod, FunName, Arity}, {ParSig1, ResInfo}},
					TypeSigPid ! {add, Info};
				_ -> ok
			    end;
			{TypeSigPid, false} ->
			    ArgsInfo = lists:duplicate(Arity, any),
			    ArgsInfo1 = lists:map(fun ({P, T}) -> F({P, T}) end, lists:zip(Ps, ArgsInfo)),
			    Info = {{Mod, FunName, Arity}, {ArgsInfo1, ResInfo}},
			    TypeSigPid ! {add, Info}
		    end;
		_ -> ok
	    end,
	    Node;
	_ -> Node
    end.


prop_from_calls(FunDef, TypeSigPid) ->
    F = fun (Pat, Typ) ->
		Ann = wrangler_syntax:get_ann(Pat),
		case lists:keysearch(pid, 1, Ann) of
		  {value, {pid, Value}} ->
		      case Typ of
			{pid, Vs} -> {pid, lists:usort(Value ++ Vs)};
			_ -> {pid, Value}
		      end;
		  false ->
		      case Typ of
			any -> Typ;
			%% Special case: some application sites are deciable pids, some are undeciable.
			{pid, Vs} -> {pid, lists:usort([any| Vs])};
			{p_name, V} -> {p_name, V}
		      end
		end
	end,
    F2 = fun (Node, _Others) ->
		 case wrangler_syntax:type(Node) of
		   application    %% propagate from call sites to function.
		               ->
		       Operator = wrangler_syntax:application_operator(Node),
                       Ann = wrangler_syntax:get_ann(Operator),
		       case lists:keysearch(fun_def, 1, Ann) of
			 {value, {fun_def, {M1, F1, A1, _P1, _P2}}} ->
			     Args = wrangler_syntax:application_arguments(Node),
			     TypeSigPid ! {self(), get, {M1, F1, A1}},
			     receive
			       {TypeSigPid, value, {ParSig, RtnSig}} ->
				   ArgsInfo = lists:map(fun ({A, P}) -> F(A, P) end, lists:zip(Args, ParSig)),
				   Info = {{M1, F1, A1}, {ArgsInfo, RtnSig}}, %% any is for the return type of the function.
				   TypeSigPid ! {add, Info};
			       {TypeSigPid, false} ->
				   ParSig = lists:duplicate(A1, any),
				   ArgsInfo = lists:map(fun ({A, P}) -> F(A, P) end, lists:zip(Args, ParSig)),
				   case ArgsInfo == ParSig of
				     true -> ok;
				     _ ->
					 Info = {{M1, F1, A1}, {ArgsInfo, any}},
					 TypeSigPid ! {add, Info}
				   end
			     end,
			     Node;
			 _ -> Node
		       end;
		   _ -> Node
		 end
	 end,
    api_ast_traverse:full_buTP(F2, FunDef, []).

annotate_within_fun_1({{ModName, FunName, Arity}, FunDef}, TypeSigPid) ->
    EnvPid = start_env_process(),
    TypeSigPid ! {self(), get, {ModName, FunName, Arity}},
    FunDef1 = receive
		  {TypeSigPid, value, {ParSig, _RtnSig}} ->
		      FunName1 = wrangler_syntax:function_name(FunDef),
		      Cs = wrangler_syntax:function_clauses(FunDef),
		      Cs1 = lists:map(fun (C) ->
					      Ps = wrangler_syntax:clause_patterns(C),
					      B = wrangler_syntax:clause_body(C),
					      G = wrangler_syntax:clause_guard(C),
					      Ps1 = lists:map(fun ({P, T}  %% don't care about complex parameters.
                                                                         )
                                                                  
								           ->
                                                                      case wrangler_syntax:type(P) of
									  variable ->
									      case T of
										  any -> P;
										  Pid ->
										      Ann1 = wrangler_syntax:get_ann(P),
										      {value, {def, DefinePos}} = lists:keysearch(def, 1, Ann1),
										      EnvPid ! {add, {{def, DefinePos}, Pid}},
										      wrangler_misc:update_ann(P, Pid)
									      end;
									  _ -> P
								      end
							      end,
							      lists:zip(Ps, ParSig)),
					      wrangler_syntax:copy_attrs(C, wrangler_syntax:clause(Ps1, G, B))
				      end, Cs),
		      FunDef0 = wrangler_syntax:copy_attrs(FunDef, wrangler_syntax:function(FunName1, Cs1)),
		      api_ast_traverse:full_buTP(fun annotate_within_fun/2, FunDef0, {ModName, FunName, Arity, EnvPid, TypeSigPid});
		  
		  _ -> %% refac_util:full_buTP(fun annotate_within_fun/2, FunDef, {ModName, FunName, Arity, EnvPid, TypeSigPid})
		      FunDef
	      end,
    EnvPid ! stop,
    {{ModName, FunName, Arity}, FunDef1}.
    
start_counter_process() ->               
    Pid = spawn_link(fun() -> counter_loop({1,1}) end),
    register(counter1, Pid).            %% REFACTOR THIS USING WRANGLER: RENAME counter1 to counter.

stop_counter_process() ->
    counter1!stop.

init_counter() ->
    counter1 ! init.

counter_loop({Spawn, Self}) ->
    receive
	{From, next_spawn} ->
	    From ! {counter1, Spawn},
	    counter_loop({Spawn+1, Self});
	{From, next_self} ->
	    From ! {counter1, Self},
	    counter_loop({Spawn, Self+1});
	init ->
	    counter_loop({1,1});
	stop ->
	    ok
    end.

%%% TO think: any other problems/restrictions with thus fun?
annotate_special_fun_apps({CurrentFun, FunDef}, EnvPid) ->
    init_counter(),
    {FunDef1, _} = api_ast_traverse:stop_tdTP(fun do_annotate_special_fun_apps_pid/2, FunDef, {CurrentFun, EnvPid}),
    api_ast_traverse:full_buTP(fun do_annotate_special_fun_apps_pname/2, FunDef1, EnvPid).

do_annotate_special_fun_apps_pid(Node, {CurrentFun, EnvPid}) ->
    case wrangler_syntax:type(Node) of
	application ->
	    case wrangler_misc:is_spawn_app(Node)   %% TODO:How about meta application of spawn?
		of
		true ->
		    Op = wrangler_syntax:application_operator(Node),
		    Args = wrangler_syntax:application_arguments(Node),
		    Args1 = case Args of
				[A] -> {A1, _} = api_ast_traverse:stop_tdTP(fun do_annotate_special_fun_apps_pid/2, A, {wrangler_prettypr:format(A), EnvPid}),
				       [A1];
				[Node, A] ->
				    {A1, _} = api_ast_traverse:stop_tdTP(fun do_annotate_special_fun_apps_pid/2, A, {wrangler_prettypr:format(A), EnvPid}),
				    [Node, A1];
				_ -> Args
			    end,
		    counter1 ! {self(), next_spawn},
		    receive {counter1, N} -> N end,
		    Node1 = wrangler_syntax:copy_attrs(Node, wrangler_syntax:application(Op, Args1)),
		    Node2 = wrangler_misc:update_ann(Node1,
						     {pid, [{spawn, CurrentFun, N}]}),
		    {Node2, true};
		_ ->
		    Op = wrangler_syntax:application_operator(Node),
		    OpAnn = wrangler_syntax:get_ann(Op),
		    case lists:keysearch(fun_def, 1, OpAnn) of
			{value, {fun_def, {erlang, self, 0, _, _}}} ->
			    Node1 = wrangler_misc:update_ann(Node, {pid, [{self, CurrentFun}]}),
			    {Node1, true};
			_ -> {Node, false}
		    end
	    end;
	_ -> {Node, false}
    end.

do_annotate_special_fun_apps_pname(Node, EnvPid) ->
    case wrangler_syntax:type(Node) of
	application ->
	    Op = wrangler_syntax:application_operator(Node),
	    OpAnn = wrangler_syntax:get_ann(Op),
	    case lists:keysearch(fun_def, 1, OpAnn) of
		{value, {fun_def, {erlang, register, 2, _, _}}} ->
		    [Arg1, Arg2] = wrangler_syntax:application_arguments(Node),
		    PidInfo = case lists:keysearch(pid, 1, wrangler_syntax:get_ann(Arg2)) of
				  {value, {pid, PidInfo1}} -> PidInfo1;
				  _ -> []
			      end,
		    Arg11 = wrangler_misc:update_ann(Arg1, {p_name, PidInfo}),
		    Node1 = wrangler_syntax:copy_attrs(Node, wrangler_syntax:application(Op, [Arg11, Arg2])),
		    case lists:keysearch(def, 1, wrangler_syntax:get_ann(Arg1)) of
			{value, {def, DefinePos}} ->  %% the process name is a variable.
			    EnvPid ! {add, {{def, DefinePos}, {p_name, PidInfo}}};
			_ -> case wrangler_syntax:type(Arg1) of
				 atom -> EnvPid ! {add, {{name, wrangler_syntax:atom_value(Arg1)}, {p_name, PidInfo}}};
				 _ -> ok
			     end
		    end,
		    Node1;
		{value, {fun_def, {erlang, unregister, 1, _, _}}} ->
		    [Arg1] = wrangler_syntax:application_arguments(Node),
		    case lists:keysearch(def, 1, wrangler_syntax:get_ann(Arg1)) of
			{value, {def, DefinePos}} ->
			    EnvPid ! {self(), get, {def, DefinePos}},
			    receive
				{EnvPid, value, Value} ->
				    Arg11 = wrangler_misc:update_ann(Arg1, Value),
				    wrangler_syntax:copy_attrs(Node, wrangler_syntax:application(Op, [Arg11]));
				{EnvPid, false} ->
				    Arg11 = wrangler_misc:update_ann(Arg1, {p_name, []}), %% keep an empty list for information extension.
				    EnvPid ! {add, {{def, DefinePos}, {p_name, []}}},
				    wrangler_syntax:copy_attrs(Node, wrangler_syntax:application(Op, [Arg11]))
			    end;
			_ -> Arg11 = wrangler_misc:update_ann(Arg1, {p_name, []}),
			     wrangler_syntax:copy_attrs(Node, wrangler_syntax:application(Op, [Arg11]))
		    end;
		{value, {fun_def, {erlang, whereis, 1, _, _}}} ->
		    [Arg1] = wrangler_syntax:application_arguments(Node),
		    case lists:keysearch(def, 1, wrangler_syntax:get_ann(Arg1)) of
			{value, {def, DefinePos}} ->
			    EnvPid ! {self(), get, {def, DefinePos}},
			    receive
				{EnvPid, value, Value} ->
				    Arg11 = wrangler_misc:update_ann(Arg1, Value),
				    wrangler_syntax:copy_attrs(Node, wrangler_syntax:application(Op, [Arg11]));
				{EnvPid, false} ->
				    Arg11 = wrangler_misc:update_ann(Arg1, {p_name, []}),
				    EnvPid ! {add, {{def, DefinePos}, {p_name, []}}},
				    wrangler_syntax:copy_attrs(Node, wrangler_syntax:application(Op, [Arg11]))
			    end;
			_ ->
			    Arg11 = wrangler_misc:update_ann(Arg1, {p_name, []}),
			    wrangler_syntax:copy_attrs(Node, wrangler_syntax:application(Op, [Arg11]))
		    end;
		{value, {fun_def, {erlang, send, 2, _, _}}} ->
		    [Arg1, Arg2] = wrangler_syntax:application_arguments(Node),
		    Arg11 = case wrangler_syntax:type(Arg1) of
				atom -> wrangler_misc:update_ann(Arg1, {p_name, []});
				_ -> Arg1
			    end,
		    Node1 = wrangler_syntax:copy_attrs(Node, wrangler_syntax:application(Op, [Arg11, Arg2])),
		    case lists:keysearch(def, 1, wrangler_syntax:get_ann(Arg1)) of
			{value, {def, DefinePos}} ->
			    EnvPid ! {add, {{def, DefinePos}, {p_name, []}}};
			_ -> ok
		    end,
		    Node1;
		_ ->
		    Node
	    end;
	infix_expr -> Op = wrangler_syntax:infix_expr_operator(Node),
		      case wrangler_syntax:operator_name(Op) of
			  '!' ->
			      Left = wrangler_syntax:infix_expr_left(Node),
			      Right = wrangler_syntax:infix_expr_right(Node),
			      case wrangler_syntax:type(Left) of
				  atom ->
				      Left1 = wrangler_misc:update_ann(Left, {p_name, []}),
				      wrangler_syntax:copy_attrs(Node, wrangler_syntax:infix_expr(Left1, Op, Right));
				  _ -> Node
			      end;
			  _ -> Node
		      end;
	_ -> Node
    end.

is_send_expr(Tree) ->
    SendFuns = [{erlang, send, 2}, {erlang, send, 3}, {erlang, send_after, 3}, {erlang, send_nosuspend, 2},
		{erlang, send_nosuspend, 3}],
    case wrangler_syntax:type(Tree) of
      infix_expr ->
	  Op = wrangler_syntax:infix_expr_operator(Tree),
	  case wrangler_syntax:type(Op) of
	    operator -> wrangler_syntax:operator_name(Op) == '!';
	    _ -> false
	  end;
      application ->
	  Operator = wrangler_syntax:application_operator(Tree),
	  Ann = wrangler_syntax:get_ann(Operator),
	  case lists:keysearch(fun_def, 1, Ann) of
	    {value, {fun_def, {Mod, Fun, Arity, _, _}}} -> lists:member({Mod, Fun, Arity}, SendFuns);
	    _ -> false
	  end;
      _ -> false
    end.

%% sort functions according to calling relationship and remove functions which are not process related.
sort_funs(DirList) ->
    CallGraph = wrangler_callgraph_server:get_callgraph(DirList),
    TrimmedSccs = trim_scc(CallGraph#callgraph.scc_order, CallGraph#callgraph.callercallee, [], []),
    lists:append(TrimmedSccs).

trim_scc([], _CallerCallee, _PFunAcc, Acc) -> lists:reverse(Acc);
trim_scc([Scc | Sccs], CallerCallee, PFunAcc, Acc) ->
    SccFuns = lists:map(fun ({Fun, _FunDef}) -> Fun end, Scc),
    IsProcessScc = lists:any(fun ({_Fun, FunDef}) -> is_process_related_fun(FunDef) end, Scc),
    CalledFuns = lists:usort(lists:flatmap(fun (Fun) ->
						   case lists:keysearch(Fun, 1, CallerCallee) of
						     {value, {Fun, Called}} -> Called;
						     _ -> []
						   end
					   end,
					   SccFuns)),
    PFunsCalled = length(lists:subtract(CalledFuns, PFunAcc)) < length(CalledFuns),
    case IsProcessScc orelse PFunsCalled of
      true -> trim_scc(Sccs, CallerCallee, SccFuns ++ PFunAcc, [Scc | Acc]);
      _ -> trim_scc(Sccs, CallerCallee, PFunAcc, Acc)
    end.

is_process_related_fun(FunDef) ->
    ProcessFuns = [{erlang, register, 2}, {erlang, self, 0}, {erlang, spawn, 1}, {erlang, spawn, 2}, {erlang, spawn, 3}, {erlang, process_info, 1},
		   {erlang, spawn, 4}, {erlang, spawn_link, 1}, {erlang, spawn_link, 2}, {erlang, spawn_link, 3}, {erlang, spawn_link, 4},
		   {erlang, send, 2}, {erlang, send, 3}, {erlang, send_after, 3}, {erlang, send_nosuspend, 2}, {erlang, send_nosuspend, 3}],
    F = fun (Node, _Others) ->
		case wrangler_syntax:type(Node) of
		  infix_expr ->
		      case is_send_expr(Node) of
			true -> {true, true};
			_ -> {[], false}
		      end;
		  receive_expr -> {true, true};
		  application ->
		      Operator = wrangler_syntax:application_operator(Node),
		      Arity = length(wrangler_syntax:application_arguments(Node)),
		      case wrangler_syntax:type(Operator) of
			atom ->
			    Op = wrangler_syntax:atom_value(Operator),
			    {value, {fun_def, {M, Op, A, _, _}}} = lists:keysearch(fun_def, 1, wrangler_syntax:get_ann(Operator)),
			    case lists:member({M, Op, A}, ProcessFuns) of
			      true -> {true, true};
			      _ -> {[], false}
			    end;
			module_qualifier ->
			    Mod = wrangler_syntax:module_qualifier_argument(Operator),
			    Body = wrangler_syntax:module_qualifier_body(Operator),
			    case {wrangler_syntax:type(Mod), wrangler_syntax:type(Body)} of
			      {atom, atom} ->
				  M = wrangler_syntax:atom_value(Mod),
				  Op = wrangler_syntax:atom_value(Body),
				  case lists:member({M, Op, Arity}, ProcessFuns) of
				    true -> {true, true};
				    _ -> {[], false}
				  end;
			      _ -> {[], false}
			    end;
			_ -> {[], false}
		      end;
		  _ -> {[], false}
		end
	end,
    case api_ast_traverse:once_tdTU(F, FunDef, []) of
      {_, false} -> false;
      {_R, true} -> true
    end.

start_fun_typesig_process(State) ->
    spawn(fun() ->fun_typesig_loop(State) end).
    

fun_typesig_loop(State) ->
    receive
 	{From, get, Fun} ->
 	    case lists:keysearch(Fun, 1, State) of
 		{value, {Fun, TypeSig}} -> 
 		    From ! {self(), value, TypeSig};
 		false -> 
		    From ! {self(), false}
 	    end, 
	    fun_typesig_loop(State);
 	{add, {Fun, TypeSig}} ->
	    State1 = case lists:keysearch(Fun, 1, State) of
		       {value, {Fun, _}} ->
			   lists:keyreplace(Fun, 1, State, {Fun, TypeSig});
		       false -> 
			  [{Fun,TypeSig}|State]
		   end,
	    fun_typesig_loop(State1);
 	{From, getenv} -> 
	    From ! {self(), State},
	    fun_typesig_loop(State);
 	stop ->  
 	   %% ?wrangler_io("typesig env:\n~p\n", [State]),
 	    ok
     end.

   
start_env_process() ->
    Pid = spawn_link(fun() -> env_loop([]) end), Pid.

env_loop(Env) ->
    receive
	{From, get, Key} ->
	    case lists:keysearch(Key, 1, Env) of
		{value, {Key, Value}} -> From ! {self(), value, Value};
		false -> From ! {self(), false}
	    end,
	    env_loop(Env);
	{add, {Key, Value}} ->
	    Env1 =   case lists:keysearch(Key, 1, Env) of
			 {value, {Key, _}} ->
			     lists:keyreplace(Key, 1, Env, {Key, Value});
			 false -> [{Key, Value} | Env]
		     end,
	   env_loop(Env1);
	stop ->
	    ok
    end.



