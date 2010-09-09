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
-module(refac_annotate_pid).

-export([ann_pid_info/2]).

-include("../include/wrangler.hrl").

%%-spec(ann_pid_info/2::([dir()], integer())->ok).
ann_pid_info(DirList, TabWidth) ->
    Files = refac_util:expand_files(DirList, ".erl"),
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
    FunDef2 = ast_traverse_api:full_buTP(fun annotate_within_fun/2, FunDef1, {ModName, FunName, Arity, EnvPid, TypeSigPid}),
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
    {ok, {AnnAST, Info}} = refac_util:parse_annotate_file(File, true, DirList, TabWidth),
    ModName = case lists:keysearch(module, 1, Info) of
		{value, {module, Mod}} -> Mod;
		_ -> list_to_atom(filename:basename(File, ".erl"))
	      end,
    F = fun (Node, []) ->
		case refac_syntax:type(Node) of
		  function ->
		      FunName = refac_syntax:data(refac_syntax:function_name(Node)),
		      Arity = refac_syntax:function_arity(Node),
		      case lists:keysearch({ModName, FunName, Arity}, 1, FunList) of
			{value, {{ModName, FunName, Arity}, FunDef}} ->
			    {FunDef, true};
			_ -> {Node, false}
		      end;
		  _ -> {Node, false}
		end
	end,
    {AnnAST1, _} = ast_traverse_api:stop_tdTP(F, AnnAST, []),
    wrangler_ast_server:update_ast({File, true, DirList, TabWidth, refac_util:file_format(File)}, {AnnAST1, Info, filelib:last_modified(File)}),
    ok.

annotate_within_fun(Node, {_ModName, FunName, Arity, EnvPid, TypeSigPid}) ->
    case refac_syntax:type(Node) of
      variable ->
	  Ann = refac_syntax:get_ann(Node),
	  case lists:keysearch(def, 1, Ann) of
	    {value, {def, DefinePos}} ->
		EnvPid ! {self(), get, {def, DefinePos}},
		receive
		  {EnvPid, value, Value} ->
		      refac_misc:update_ann(Node, Value);
		  {EnvPid, false} -> Node
		end;
	    _ -> Node
	  end;
      application ->
	  Operator = refac_syntax:application_operator(Node),
	  Args = refac_syntax:application_arguments(Node),
	  Ann = refac_syntax:get_ann(Operator),
	  case lists:keysearch(fun_def, 1, Ann) of
	    {value, {fun_def, {M1, F1, A1, _P1, _2}}} ->
		TypeSigPid ! {self(), get, {M1, F1, A1}},
		receive
		  {TypeSigPid, value, {ParSig, RtnSig}} ->
		      F = fun ({A, S}) ->
				  case S of
				    {pname, _} ->    %% Can you do this to Pid?
					refac_misc:update_ann(A, S);
				    _ -> A
				  end
			  end,
		      Args1 = lists:map(fun ({A, S}) -> F({A, S}) end, lists:zip(Args, ParSig)),
		      Node1 = refac_syntax:copy_attrs(Node, refac_syntax:application(Operator, Args1)),
		      case RtnSig of
			any -> Node1;
			Pid -> Node2 = refac_misc:update_ann(Node1, Pid),
			       Node2
		      end;
		  {TypeSigPid, false} ->
		      Node
		end;
	    _ -> Node
	  end;
      match_expr ->
	  P = refac_syntax:match_expr_pattern(Node),
	  B = refac_syntax:match_expr_body(Node),
	  Ann = refac_syntax:get_ann(B),
	  case lists:keysearch(pid, 1, Ann) of
	    {value, {pid, Value}} ->
		P1 = refac_misc:update_ann(P, {pid, Value}),
		case refac_syntax:type(P) of
		  variable ->
		      Ann1 = refac_syntax:get_ann(P),
		      {value, {def, DefinePos}} = lists:keysearch(def, 1, Ann1),
		      EnvPid ! {add, {{def, DefinePos}, {pid, Value}}};
		  _ -> ok                %%% What about the complex pattern matches?
		end,
		refac_syntax:copy_attrs(Node, refac_syntax:match_expr(P1, B));
	    _ ->
		case lists:keysearch(pname, 1, Ann) of
		  {value, {pname, Value}} ->
		      P1 = refac_misc:update_ann(P, {pname, Value}),
		      case refac_syntax:type(P) of
			variable ->
			    Ann1 = refac_syntax:get_ann(P),
			    {value, {def, DefinePos}} = lists:keysearch(def, 1, Ann1),
			    EnvPid ! {add, {{def, DefinePos}, {pname, Value}}};
			_ -> ok                %%% What about the complex pattern matches?
		      end,
		      refac_syntax:copy_attrs(Node, refac_syntax:match_expr(P1, B));
		  _ ->
		      Node %% The body is not a pid
		end
	  end;
      function ->
	  Ann = refac_syntax:get_ann(Node),
	  {value, {fun_def, {Mod, FunName, Arity, _, _}}} = lists:keysearch(fun_def, 1, Ann),
	  Cs = refac_syntax:function_clauses(Node),
	  case length(Cs) of
	    1 ->  %% only handle when the function has only one clause.
		C = hd(Cs),
		Ps = refac_syntax:clause_patterns(C),
		LastExp = lists:last(refac_syntax:clause_body(C)),
		Res = case refac_syntax:type(LastExp) of
			match_expr -> refac_syntax:match_expr_pattern(LastExp);
			_ -> LastExp
		      end,
		ResInfo = case lists:keysearch(pid, 1, refac_syntax:get_ann(Res)) of
			    {value, {pid, Value}} -> {pid, Value};
			    _ -> case lists:keysearch(pname, 1, refac_syntax:get_ann(Res)) of
				   {value, {pname, Value}} -> {pname, Value};
				   _ -> any
				 end
			  end,
		F = fun ({P, _T}) ->
			    Ann1 = refac_syntax:get_ann(P),
			    case lists:keysearch(pid, 1, Ann1) of
			      {value, {pid, Value1}} ->
				  {pid, Value1};
			      _ -> case lists:keysearch(pname, 1, Ann1) of
				     {value, {pname, V}} ->
					 {pname, V};
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
		Ann = refac_syntax:get_ann(Pat),
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
			{pname, V} -> {pname, V}
		      end
		end
	end,
    F2 = fun (Node, _Others) ->
		 case refac_syntax:type(Node) of
		   application ->    %% propagate from call sites to function.
		       Operator = refac_syntax:application_operator(Node),
		       Ann = refac_syntax:get_ann(Operator),
		       case lists:keysearch(fun_def, 1, Ann) of
			 {value, {fun_def, {M1, F1, A1, _P1, _P2}}} ->
			     Args = refac_syntax:application_arguments(Node),
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
    ast_traverse_api:full_buTP(F2, FunDef, []).


annotate_within_fun_1({{ModName, FunName, Arity}, FunDef}, TypeSigPid) ->
    EnvPid = start_env_process(),
    TypeSigPid ! {self(), get, {ModName, FunName, Arity}},
    FunDef1 = receive
		{TypeSigPid, value, {ParSig, _RtnSig}} ->
		    FunName1 = refac_syntax:function_name(FunDef),
		    Cs = refac_syntax:function_clauses(FunDef),
		    Cs1 = lists:map(fun (C) ->
					    Ps = refac_syntax:clause_patterns(C),
					    B = refac_syntax:clause_body(C),
					    G = refac_syntax:clause_guard(C),
					    Ps1 = lists:map(fun ({P, T}) ->  %% don't care about complex parameters.
								    case refac_syntax:type(P) of
								      variable ->
									  case T of
									    any -> P;
									    Pid ->
										Ann1 = refac_syntax:get_ann(P),
										{value, {def, DefinePos}} = lists:keysearch(def, 1, Ann1),
										EnvPid ! {add, {{def, DefinePos}, Pid}},
										refac_misc:update_ann(P, Pid)
									  end;
								      _ -> P
								    end
							    end,
							    lists:zip(Ps, ParSig)),
					    refac_syntax:copy_attrs(C, refac_syntax:clause(Ps1, G, B))
				    end, Cs),
		    FunDef0 = refac_syntax:copy_attrs(FunDef, refac_syntax:function(FunName1, Cs1)),
		    ast_traverse_api:full_buTP(fun annotate_within_fun/2, FunDef0, {ModName, FunName, Arity, EnvPid, TypeSigPid});
		
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
    {FunDef1, _} = ast_traverse_api:stop_tdTP(fun do_annotate_special_fun_apps_pid/2, FunDef, {CurrentFun, EnvPid}),
    ast_traverse_api:full_buTP(fun do_annotate_special_fun_apps_pname/2, FunDef1, EnvPid).
   


do_annotate_special_fun_apps_pid(Node, {CurrentFun, EnvPid}) ->
    case refac_syntax:type(Node) of
      application ->
	  case refac_register_pid:is_spawn_app(Node)   %% TODO:How about meta application of spawn?
	      of
	    true ->
		Op = refac_syntax:application_operator(Node),
		Args = refac_syntax:application_arguments(Node),
		Args1 = case Args of
			  [A] -> {A1, _} = ast_traverse_api:stop_tdTP(fun do_annotate_special_fun_apps_pid/2, A, {refac_prettypr:format(A), EnvPid}),
				 [A1];
			  [Node, A] ->
			      {A1, _} = ast_traverse_api:stop_tdTP(fun do_annotate_special_fun_apps_pid/2, A, {refac_prettypr:format(A), EnvPid}),
			      [Node, A1];
			  _ -> Args
			end,
		counter1 ! {self(), next_spawn},
		receive {counter1, N} -> N end,
		Node1 = refac_syntax:copy_attrs(Node, refac_syntax:application(Op, Args1)),
		Node2 = refac_misc:update_ann(Node1,
					      {pid, [{spawn, CurrentFun, N}]}),
		{Node2, true};
	    _ ->
		Op = refac_syntax:application_operator(Node),
		OpAnn = refac_syntax:get_ann(Op),
		case lists:keysearch(fun_def, 1, OpAnn) of
		  {value, {fun_def, {erlang, self, 0, _, _}}} ->
		      Node1 = refac_misc:update_ann(Node, {pid, [{self, CurrentFun}]}),
		      {Node1, true};
		  _ -> {Node, false}
		end
	  end;
      _ -> {Node, false}
    end.


do_annotate_special_fun_apps_pname(Node, EnvPid) ->
    case refac_syntax:type(Node) of
      application ->
	  Op = refac_syntax:application_operator(Node),
	  OpAnn = refac_syntax:get_ann(Op),
	  case lists:keysearch(fun_def, 1, OpAnn) of
	    {value, {fun_def, {erlang, register, 2, _, _}}} ->
		[Arg1, Arg2] = refac_syntax:application_arguments(Node),
		PidInfo = case lists:keysearch(pid, 1, refac_syntax:get_ann(Arg2)) of
			    {value, {pid, PidInfo1}} -> PidInfo1;
			    _ -> []
			  end,
		Arg11 = refac_misc:update_ann(Arg1, {pname, PidInfo}),
		Node1 = refac_syntax:copy_attrs(Node, refac_syntax:application(Op, [Arg11, Arg2])),
		case lists:keysearch(def, 1, refac_syntax:get_ann(Arg1)) of
		  {value, {def, DefinePos}} ->  %% the process name is a variable.
		      EnvPid ! {add, {{def, DefinePos}, {pname, PidInfo}}};
		  _ -> case refac_syntax:type(Arg1) of
			 atom -> EnvPid ! {add, {{name, refac_syntax:atom_value(Arg1)}, {pname, PidInfo}}};
			 _ -> ok
		       end
		end,
		Node1;
	    {value, {fun_def, {erlang, unregister, 1, _, _}}} ->
		[Arg1] = refac_syntax:application_arguments(Node),
		case lists:keysearch(def, 1, refac_syntax:get_ann(Arg1)) of
		  {value, {def, DefinePos}} ->
		      EnvPid ! {self(), get, {def, DefinePos}},
		      receive
			{EnvPid, value, Value} ->
			    Arg11 = refac_misc:update_ann(Arg1, Value),
			    refac_syntax:copy_attrs(Node, refac_syntax:application(Op, [Arg11]));
			{EnvPid, false} ->
			    Arg11 = refac_misc:update_ann(Arg1, {pname, []}), %% keep an empty list for information extension.
			    EnvPid ! {add, {{def, DefinePos}, {pname, []}}},
			    refac_syntax:copy_attrs(Node, refac_syntax:application(Op, [Arg11]))
		      end;
		  _ -> Arg11 = refac_misc:update_ann(Arg1, {pname, []}),
		       refac_syntax:copy_attrs(Node, refac_syntax:application(Op, [Arg11]))
		end;
	    {value, {fun_def, {erlang, whereis, 1, _, _}}} ->
		[Arg1] = refac_syntax:application_arguments(Node),
		case lists:keysearch(def, 1, refac_syntax:get_ann(Arg1)) of
		  {value, {def, DefinePos}} ->
		      EnvPid ! {self(), get, {def, DefinePos}},
		      receive
			{EnvPid, value, Value} ->
			    Arg11 = refac_misc:update_ann(Arg1, Value),
			    refac_syntax:copy_attrs(Node, refac_syntax:application(Op, [Arg11]));
			{EnvPid, false} ->
			    Arg11 = refac_misc:update_ann(Arg1, {pname, []}),
			    EnvPid ! {add, {{def, DefinePos}, {pname, []}}},
			    refac_syntax:copy_attrs(Node, refac_syntax:application(Op, [Arg11]))
		      end;
		  _ ->
		      Arg11 = refac_misc:update_ann(Arg1, {pname, []}),
		      refac_syntax:copy_attrs(Node, refac_syntax:application(Op, [Arg11]))
		end;
	    {value, {fun_def, {erlang, send, 2, _, _}}} ->
		[Arg1, Arg2] = refac_syntax:application_arguments(Node),
		Arg11 = case refac_syntax:type(Arg1) of
			  atom -> refac_misc:update_ann(Arg1, {pname, []});
			  _ -> Arg1
			end,
		Node1 = refac_syntax:copy_attrs(Node, refac_syntax:application(Op, [Arg11, Arg2])),
		case lists:keysearch(def, 1, refac_syntax:get_ann(Arg1)) of
		  {value, {def, DefinePos}} ->
		      EnvPid ! {add, {{def, DefinePos}, {pname, []}}};
		  _ -> ok
		end,
		Node1;
	    _ ->
		Node
	  end;
      infix_expr -> Op = refac_syntax:infix_expr_operator(Node),
		    case refac_syntax:operator_name(Op) of
		      '!' ->
			  Left = refac_syntax:infix_expr_left(Node),
			  Right = refac_syntax:infix_expr_right(Node),
			  case refac_syntax:type(Left) of
			    atom ->
				Left1 = refac_misc:update_ann(Left, {pname, []}),
				refac_syntax:copy_attrs(Node, refac_syntax:infix_expr(Left1, Op, Right));
			    _ -> Node
			  end;
		      _ -> Node
		    end;
      _ -> Node
    end.

is_send_expr(Tree) ->
    SendFuns = [{erlang, send, 2}, {erlang, send, 3}, {erlang, send_after, 3}, {erlang, send_nosuspend, 2},
		{erlang, send_nosuspend, 3}],
    case refac_syntax:type(Tree) of
      infix_expr ->
	  Op = refac_syntax:infix_expr_operator(Tree),
	  case refac_syntax:type(Op) of
	    operator -> refac_syntax:operator_name(Op) == '!';
	    _ -> false
	  end;
      application ->
	  Operator = refac_syntax:application_operator(Tree),
	  Ann = refac_syntax:get_ann(Operator),
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
		case refac_syntax:type(Node) of
		  infix_expr ->
		      case is_send_expr(Node) of
			true -> {true, true};
			_ -> {[], false}
		      end;
		  receive_expr -> {true, true};
		  application ->
		      Operator = refac_syntax:application_operator(Node),
		      Arity = length(refac_syntax:application_arguments(Node)),
		      case refac_syntax:type(Operator) of
			atom ->
			    Op = refac_syntax:atom_value(Operator),
			    {value, {fun_def, {M, Op, A, _, _}}} = lists:keysearch(fun_def, 1, refac_syntax:get_ann(Operator)),
			    case lists:member({M, Op, A}, ProcessFuns) of
			      true -> {true, true};
			      _ -> {[], false}
			    end;
			module_qualifier ->
			    Mod = refac_syntax:module_qualifier_argument(Operator),
			    Body = refac_syntax:module_qualifier_body(Operator),
			    case {refac_syntax:type(Mod), refac_syntax:type(Body)} of
			      {atom, atom} ->
				  M = refac_syntax:atom_value(Mod),
				  Op = refac_syntax:atom_value(Body),
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
    case ast_traverse_api:once_tdTU(F, FunDef, []) of
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



