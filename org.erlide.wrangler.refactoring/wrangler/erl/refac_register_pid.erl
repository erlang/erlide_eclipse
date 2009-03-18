%% =====================================================================
%% Refactoring: Register a process.
%%
%% Copyright (C) 2006-2009  Huiqing Li, Simon Thompson
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.


%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%%
%% =====================================================================
%%
%% @doc This refactoring register a process id, Pid say, with a name, regname say, and replace
%% the uses of Pid ! Msg with  regname ! Msg if possible. To initiate this refactoring, the 
%% user should select a match expression whose left-hand side is a process indentifier, and right-hand 
%% side is a spawn expression.
%%
%% Rationale for this refactoring: 
%% If we want to send a message to a process, then we need to know its PID. This is often inconvenient 
%% since the PID has to be sent to all the processes in the system that want to comminicate with this process. 
%% Registering a process with a name allows any process in the system to communicate with this process 
%% without knowing its PID.
%%
%% Side-conditions:
%%  1. The process name provided by the user should be lexically valid.
%%  2. The name provided by the user should not have been used as a process name. 
%%  3. The process under consideration should not have been registered.
%%  4. Only one process spawned by the spawn expression selected should exist anytime during the running of the system.
%% 
%% Transformation:
%% To perform the transformation, Wrangler needs to know, for each Pid!Msg expression, where the Pid is spawned. 
%% A Pid is replacable only if Wrangler is sure that it is only associated with the spawn expression selected.
%% @end

-module(refac_register_pid).

-export([register_pid/6, register_pid_eclipse/6, register_pid_1/9, register_pid_2/8]).

-include("../include/wrangler.hrl").

%% ==============================================================================================================
%% @spec register_pid(FileName::filename(), Start::Pos, End::Pos, RegName::string(),SearchPaths::[dir()])-> term()
%% @doc This function associates a name, which must be an atom, with a pid, and replaces the uses of this pid in 
%% send expressions with the name.

-spec(register_pid(FileName::filename(), Start::pos(), End::pos(), RegName::string(),SearchPaths::[dir()], TabWidth::integer())-> 
	     {error, string()} |{ok, [filename()]}).
register_pid(FName, Start, End, RegName,  SearchPaths, TabWidth) ->
    register_pid(FName, Start, End, RegName, SearchPaths, TabWidth, emacs).


-spec(register_pid_eclipse(FileName::filename(), Start::pos(), End::pos(), RegName::string(),SearchPaths::[dir()], TabWidth::integer())
      -> {error, string()} |{ok, [{filename(), filename(), string()}]}).
register_pid_eclipse(FName, Start, End, RegName, SearchPaths, TabWidth) ->
    register_pid(FName, Start, End, RegName, SearchPaths, TabWidth, eclipse).

register_pid(FName, Start={Line1, Col1}, End={Line2, Col2}, RegName, SearchPaths, TabWidth, Editor) ->
    ?wrangler_io("\nCMD: ~p:register_pid(~p, {~p,~p}, {~p,~p}, ~p,~p, ~p)\n",  [?MODULE, FName, Line1, Col1, Line2, Col2, RegName, SearchPaths, TabWidth]),
    case is_process_name(RegName) of
		true ->	{ok, {AnnAST,Info}}= refac_util:parse_annotate_file(FName, true, SearchPaths, TabWidth), 
				case pos_to_spawn_match_expr(AnnAST, Start, End) of
					{ok, _MatchExpr1} ->
					{value, {module, ModName}} = lists:keysearch(module, 1, Info),
					RegName1 = list_to_atom(RegName), 
					_Res=refac_annotate_pid:ann_pid_info(SearchPaths, TabWidth),
					%% get the AST with pid information.
					{ok, {AnnAST1,_Info}}= refac_util:parse_annotate_file(FName, true, SearchPaths, TabWidth), 
					case pos_to_spawn_match_expr(AnnAST1, Start, End) of 
					    {ok, MatchExpr} ->
						case pre_cond_check(ModName,AnnAST1, Start, MatchExpr, RegName1, Info, SearchPaths, TabWidth) of 
						    ok -> 
							Pid = refac_syntax:match_expr_pattern(MatchExpr),
							case do_register(FName, AnnAST1, MatchExpr, Pid, RegName1, SearchPaths, TabWidth) of 
							    {ok, Results} ->
								case Editor of 
								    emacs ->
									refac_util:write_refactored_files(Results),
									ChangedFiles = lists:map(fun ({{F, _F}, _AST}) -> F end, Results),
									?wrangler_io("The following files have been changed by this refactoring:\n~p\n",
										     [ChangedFiles]),
									{ok, ChangedFiles};
								    eclipse ->
									Res = lists:map(fun({{OldFName, NewFName}, AST}) -> 
												{OldFName, NewFName, 
												 refac_prettypr:print_ast(refac_util:file_format(OldFName),AST)} end, Results),
									{ok, Res}
								end;
							    {error, Reason} -> {error, Reason}
							end;	
						    {unknown_pnames, _UnKnownPNames, RegPids} -> {unknown_pnames, RegPids};
						    {unknown_pids, UnKnownPids} ->{unknown_pids, UnKnownPids};
						    {error, Reason} -> {error, Reason}
						end;
					    {error, Reason} -> {error, Reason}
					end;
				    {error, Reason} -> {error, Reason} 
				end;
	false -> {error, "Invalid process name."}
    end.


-spec(register_pid_1(FName::filename(), StartLine::integer(), StartCol::integer(),EndLine::integer(), EndCol::integer(), 
		     RegName::string(), RegPids::[{{atom(), atom(), integer()}, syntaxTree()}],
		     SearchPaths::[dir()], TabWidth::integer())-> {error, string()} |{ok, [filename()]} | {unknown_pids, [{{atom(),atom(),atom()},syntaxTree()}]}).
register_pid_1(FName, StartLine, StartCol, EndLine, EndCol, RegName, RegPids, SearchPaths, TabWidth) ->
    {Start, End} = {{StartLine, StartCol}, {EndLine, EndCol}},
    {ok, {AnnAST, _Info}} = refac_util:parse_annotate_file(FName, true, SearchPaths, TabWidth),
    {ok, MatchExpr} = pos_to_spawn_match_expr(AnnAST, Start, End),
    Pid = refac_syntax:match_expr_pattern(MatchExpr),
    RegName1 = list_to_atom(RegName), 
    Res = check_registration(MatchExpr, SearchPaths, RegPids), 
    case Res of 
 	 ok ->  case do_register(FName, AnnAST, MatchExpr, Pid, RegName1, SearchPaths, TabWidth) of 
		    {ok, Results} ->
			ChangedFiles = lists:map(fun ({{F, _F}, _AST}) -> F end, Results),
			?wrangler_io("The following files have been changed by this refactoring:\n~p\n",
				  [ChangedFiles]),
			{ok, ChangedFiles};
		    {error, Reason} -> {error, Reason}
		end;
  	{registered, RegExpr} -> {{Line,_Col}, _} = refac_util:get_range(RegExpr),
 				 {error, "The selected process is already registered at line "++ integer_to_list(Line)};
 	{unknown_pids, RegExprs} ->
 	    ?wrangler_io("\nWrangler could not decide the process(s) registered by the following expression(s), please check!\n",[]),
	    lists:foreach(fun({{M, F, A},PidExpr}) -> {{Ln,_},_} = refac_util:get_range(PidExpr),
 				  ?wrangler_io("Location: module:~p, function: ~p/~p, line: ~p\n ", [M, F, A, Ln]),
 				  ?wrangler_io(refac_prettypr:format(PidExpr)++"\n",[]) 
		      end, RegExprs),
 	    {unknown_pids, RegExprs}
    end.

-spec(register_pid_2(FName::filename(), StartLine::integer(), StartCol::integer(), EndLine::integer(),EndCol::integer(), RegName::string(),
		     SearchPaths::[dir()],TabWidth::integer())-> {error, string()} |{ok, [filename()]}).    
register_pid_2(FName, StartLine, StartCol, EndLine, EndCol, RegName, SearchPaths, TabWidth) ->
    {Start, End} = {{StartLine, StartCol}, {EndLine, EndCol}},
    {ok, {AnnAST, _Info}} = refac_util:parse_annotate_file(FName, true, SearchPaths, TabWidth),
    {ok, MatchExpr} = pos_to_spawn_match_expr(AnnAST, Start, End),
    Pid = refac_syntax:match_expr_pattern(MatchExpr),
    RegName1 = list_to_atom(RegName),
    case do_register(FName,AnnAST, MatchExpr, Pid, RegName1, SearchPaths, TabWidth) of 
	{ok, Results} ->
	    refac_util:write_refactored_files(Results),
	    ChangedFiles = lists:map(fun ({{F, _F}, _AST}) -> F end, Results),
	    ?wrangler_io("The following files have been changed by this refactoring:\n~p\n",
		      [ChangedFiles]),
	    {ok, ChangedFiles};
	{error, Reason} -> {error, Reason}
    end.



%% 3. At any time during the running of the system, only one process can be associated with a particular process name.  Since statically we 
%% cannot decide when a process starts/dies, we use a rather stronger condition, that is the function containing the registration is only 
%% called once. 
%%   3.1) the function should not be a recursive function either directly or indirectly. (done).
%%   3.2) the function is only called at one place of the program.
%%   3.3) the function should not be used in a map/fold.
%%   3.4) the registeration should not be in a receive expression, list comprehension (done)
%% Why do I need slicing? mostly for reduced the number of unclear registration expressions.

%% Side condition checking:
%% So far, this cond-checking still cannot guarantee that only one process spawned by the 
%% expression seleted exist during anytime of the running of the system.
pre_cond_check(ModName, AnnAST, Start, MatchExpr, RegName, _Info, SearchPaths, TabWidth) ->		 
    {ok, FunDef} = refac_util:pos_to_fun_def(AnnAST, Start),
    FunName = refac_syntax:data(refac_syntax:function_name(FunDef)),
    Arity = refac_syntax:function_arity(FunDef),
    case is_recursive_fun(SearchPaths, {ModName, FunName, Arity, FunDef}) of 
	true -> {error, "The function containing the spawn  expression is a recursive function"};
	_ -> case pos_to_receive_expr(FunDef, Start) of 
		 true -> {error, "Wrangler do not support registering a process spawned in a received expression\n"};
		 _ -> case pos_to_list_comp_expr(FunDef, Start) of
			  true -> {error, "The spawn expression selected in part of a list comprehension expression\n"};
			  _ -> {RegPids, {ExistingProcessNames, UnKnowns}} = collect_registered_names_and_pids(SearchPaths, TabWidth),
			     %%   ?wrangler_io("registeredd:\n~p\n", [{ExistingProcessNames, UnKnowns}]),
			       case lists:member(RegName, ExistingProcessNames) of 
				   true -> {error, "The process name provided is already in use, please choose another name."};
				   _ -> case UnKnowns of 
					    [] ->
						Res = check_registration(MatchExpr, SearchPaths, RegPids), 
						case Res of 
						    ok -> ok;
						    {registered, RegExprs1} ->
								{{_M, F, A}, _R} = hd(RegExprs1),
							{error, "The process is already registered in function "++ atom_to_list(F)++"/"++integer_to_list(A)++"\n"};
						    {unknown_pids, RegExprs} -> 
							?wrangler_io("Wrangler could not decide the processe(s) registered by the followling expression(s):\n",[]),
							 lists:foreach(fun({{M, F, A},PidExpr}) -> 
									   {{Ln,_},_} = refac_util:get_range(PidExpr),
									   ?wrangler_io("Location: module:~p, function: ~p/~p, line: ~p\n ", [M, F, A, Ln])
									  %% ?wrangler_io(refac_prettypr:format(PidExpr)++"\n") 
								   end, RegExprs),
							{unknown_pids, RegExprs}
						end;
					    _ -> ?wrangler_io("Wrangler could not decide the process name(s) used by the following register expression(s):\n",[]),
						 UnKnowns1 = lists:map(fun({_, V}) -> V end, UnKnowns),
						 lists:foreach(fun({M, F,A, {L,_}}) -> ?wrangler_io("Location: module: ~p, function:~p/~p, line:~p\n", [M, F, A, L])
							  end, UnKnowns1),
						 {unknown_pnames, UnKnowns, RegPids}
				       end
			      end
		     end
	     end
    end.

    
   

check_registration(MatchExpr, SearchPaths, RegPids) ->
    Pid = refac_syntax:match_expr_pattern(MatchExpr),
    {value, {pid, PidInfo}} = lists:keysearch(pid,1, refac_syntax:get_ann(Pid)),
    SpawnExpr = refac_syntax:match_expr_body(MatchExpr),
    %% functions reached from the intial function of the spawn expression.
    ReachedFuns = reached_funs(SpawnExpr, SearchPaths),
    F = fun({{M, F, A}, PidExpr}, {RegAcc, UnKnownAcc}) ->
		Ann = refac_syntax:get_ann(PidExpr),
		case lists:keysearch(pid,1, Ann) of 
		    {value, {pid, PidInfo1}} ->
			case PidInfo--PidInfo1=/= PidInfo of 
			    true -> {[{{M,F, A}, PidExpr}|RegAcc], UnKnownAcc};
			    false -> case lists:any(fun(P1) ->
							    case P1 of
								{self, Fun1} -> lists:member(Fun1, ReachedFuns);
								_ -> false
							    end
						    end, PidInfo1) of 
					 true -> {[{{M,F, A}, PidExpr}|RegAcc], UnKnownAcc};
					 false -> {RegAcc, UnKnownAcc}
				     end
			end;
		    _ ->{RegAcc, [{{M,F, A}, PidExpr}|UnKnownAcc]}
		end
	end,
    {Regs, UnKnowns} =lists:foldl(F, {[],[]}, RegPids),
    case Regs of 
	[] -> case UnKnowns of 
		  [] -> ok;
		  _ -> {unknown_pids, UnKnowns}
	      end;
	_ -> {registered, Regs}
    end.

 
reached_funs(SpawnExpr, SearchPaths) ->
     Args = refac_syntax:application_arguments(SpawnExpr),
     Arity = length(Args),
     InitialFuns = case (Arity==1) or (Arity==2) of 
		       true -> Expr = lists:last(Args), 
			       funs_called(Expr);			       
		       _ ->  Args1 = list_to_tuple(lists:reverse(Args)),
			     {M, F, A}= {element(3, Args1),element(2, Args1), element(1, Args1)},
			     [{M, F, A}]
		   end,
    CallGraph = wrangler_callgraph_server:get_callgraph(SearchPaths), 
    InitialFuns++reached_funs_1(CallGraph#callgraph.callercallee, InitialFuns).
	 
reached_funs_1(CallerCallee, Acc) ->
    Res = lists:usort(lists:concat(lists:map(fun({Mod, Fun, Args}) ->
					 case lists:keysearch({Mod, Fun, Args}, 1, CallerCallee) of 
					     {value, {{Mod, Fun, Args}, CalledFuns}} ->
						 CalledFuns;
					     _ ->[]
					 end
				 end, Acc))),
     case lists:usort(Res++Acc) == Acc of 
	true -> Res;
	_ -> reached_funs_1(CallerCallee, lists:usort(Res++Acc)) 
    end.	      
		
    
is_direct_recursive_fun(ModName, FunName, Arity, FunDef) ->
    F = fun(Node, {ModName1, FunName1, Arity1}) ->
		case refac_syntax:type(Node) of 
		    application ->
			Op = refac_syntax:application_operator(Node),
			case lists:keysearch(fun_def, 1, refac_syntax:get_ann(Op)) of 
			    {value, {fun_def, {ModName1, FunName1, Arity1, _, _}}} ->
				{true, true};
			    _ -> {[],false}
			end;
		    _ -> {[], false}
		end
	end,	   
    R = refac_util:once_tdTU(F,  FunDef, {ModName, FunName, Arity}),
    case R of 
	{_, true} ->
	     true;
	_ -> false
    end.
is_recursive_fun(Files, {ModName, FunName, Arity, FunDef}) ->
    case is_direct_recursive_fun(ModName, FunName, Arity, FunDef) of 
	true -> 
	    true;
	false ->
	    CallGraph = wrangler_callgraph_server:get_callgraph(Files),
	    Sccs = CallGraph#callgraph.scc_order,
	    Sccs1 =[[Fun||{Fun, _FunDef}<-Scc]||Scc<-Sccs],
	    lists:any(fun(E)-> (length(E)>1) andalso (lists:member({ModName, FunName, Arity}, E)) end,
		      Sccs1)   
    end.
	
   
%% The only way to register a process is to use register/2.
%% This function checks all the applications of 'register/2'.
%% -spec(collect_registered_names_and_pids/1::([dir()])->
%%               {[{{modulename(), functionname(), arity()},syntaxTree()}], [atom()], [{unknown, {modulename(), functionname(), arity(), pos()}}]}).
 
collect_registered_names_and_pids(DirList, TabWidth) ->
    Files = refac_util:expand_files(DirList, ".erl"),
    F = fun(File, FileAcc) ->
		{ok, {AnnAST, Info}} = refac_util:parse_annotate_file(File, true, DirList, TabWidth),
		{value, {module, ModName}} = lists:keysearch(module, 1, Info),
		F1 = fun(Node, ModAcc) ->
			     case refac_syntax:type(Node) of 
				 function ->
				     FunName = refac_syntax:data(refac_syntax:function_name(Node)),
				     Arity = refac_syntax:function_arity(Node),
				     F2= fun (Node1, FunAcc) ->    
						 case refac_syntax:type(Node1) of 
						     application ->
							 case is_register_app(Node1) of 
							     true -> 
								 [RegName, Pid] = refac_syntax:application_arguments(Node1),
								 RegNameValues = evaluate_expr(Files, ModName, AnnAST, Node, RegName),
								 RegNameValues1 = lists:map(fun(R) -> {pname, R} end, RegNameValues),
								 [{pid, {{ModName, FunName, Arity}, Pid}}|RegNameValues1++FunAcc];
							     _ -> FunAcc
							 end; 
						     _ ->  FunAcc
						 end
					 end,
				     refac_syntax_lib:fold(F2, [], Node)++ModAcc;
				 _-> ModAcc 
			     end
		     end,
		refac_syntax_lib:fold(F1, [], AnnAST) ++ FileAcc
	   end,			 
    Acc =lists:foldl(F, [], Files),
    PNameAcc = lists:flatmap(fun({P,A}) -> if  P ==pname -> [A]; 
					       true -> []
					   end 
			     end, Acc), 
    PidAcc = lists:flatmap(fun({P,A}) -> if P == pid -> [A];
					true -> [] 
				     end 
		       end, Acc), 
    {Names, UnKnowns} = lists:partition(fun({Tag,_V})-> Tag==value end, PNameAcc),
    {PidAcc, {lists:usort(lists:map(fun({value, P}) -> P end, Names)), lists:usort(UnKnowns)}}.
    

is_register_app(T) ->
     case refac_syntax:type(T) of
       application ->
 	  Operator = refac_syntax:application_operator(T),
 	  Ann = refac_syntax:get_ann(Operator),
 	  case lists:keysearch(fun_def, 1, Ann) of
 	    {value, {fun_def, {erlang, register, 2, _, _}}} -> true;
 	    _ -> false
 	  end;
       _ -> false
     end.
   


do_register(FName, AnnAST, MatchExpr, Pid, RegName, SearchPaths, TabWidth) ->
    Ann = refac_syntax:get_ann(Pid),
   {value, PidInfo} = lists:keysearch(pid, 1, Ann),
   {AnnAST1, Modified} = add_register_expr(AnnAST, MatchExpr, RegName),
   case Modified of 
       true ->  Res = refactor_send_exprs(FName, AnnAST1, PidInfo, RegName, SearchPaths, TabWidth),
	       {ok, Res};	    
       _ -> {error, "Wrangler failed to add the registration expression."}
   end.
 


refactor_send_exprs(FName, AnnAST, PidInfo, RegName, SearchPaths, TabWidth) ->
    {AnnAST1,_} = refac_util:stop_tdTP(fun do_refactor_send_exprs/2, AnnAST, {PidInfo, RegName}),
    %%This can be refined to check the client and parent modules of the current module.
    Files = refac_util:expand_files(SearchPaths, ".erl") -- [FName],
    Results = lists:flatmap(fun(File) ->
				    ?wrangler_io("The current file under refactoring is:\n~p\n",[File]),
				    {ok, {AnnAST2, _Info}} = refac_util:parse_annotate_file(File, true, SearchPaths, TabWidth),
				    {AnnAST3, Changed} = refac_util:stop_tdTP(fun do_refactor_send_exprs/2, AnnAST2, {PidInfo, RegName}),
				    if Changed ->
					    [{{File, File}, AnnAST3}];
				       true -> []
				    end
			    end, Files),
    [{{FName, FName}, AnnAST1}|Results].

do_refactor_send_exprs(Node, {PidInfo, RegName}) ->
     case refac_syntax:type(Node) of 
	infix_expr -> Dest = refac_syntax:infix_expr_left(Node),
		      Op = refac_syntax:infix_expr_operator(Node),
		      Msg = refac_syntax:infix_expr_right(Node),
		      Ann = refac_syntax:get_ann(Dest),
		      case lists:keysearch(pid, 1, Ann) of 
			  {value, PidInfo} ->
			      Node1 = refac_syntax:infix_expr(refac_syntax:atom(RegName), Op, Msg),
			      {refac_syntax:copy_attrs(Node, Node1), true};
			  _ -> {Node, false}
		      end;
	application -> Operator = refac_syntax:application_operator(Node),
		       Ann = refac_syntax:get_ann(Operator),
		       case lists:keysearch(fun_def,1,Ann) of 
			   {value, {fun_def, {erlang, send, 2, _, _}}} ->
			       [ReceiverPid, Msg]=refac_syntax:application_arguments(Node),
			       Ann = refac_syntax:get_ann(ReceiverPid),
			       case lists:keysearch(pid,1,Ann) of 
				   {value, PidInfo} ->
				       Node1 = refac_syntax:application(Operator, [refac_syntax:atom(RegName), Msg]),
				       {refac_syntax:copy_attrs(Node, Node1), true};
				   _ -> {Node,false}
			       end;
			   _ -> {Node,false}
		       end;
	_  -> {Node, false}
    end.
			  
	    
    

add_register_expr(AnnAST, MatchExpr,RegName) ->
    Pid = refac_syntax:match_expr_pattern(MatchExpr),
    RegExpr = refac_syntax:application(refac_syntax:atom(register),
				       [refac_syntax:atom(RegName), Pid]),
    refac_util:stop_tdTP(fun do_add_register_expr/2, AnnAST, {MatchExpr, RegExpr}).
    
    


do_add_register_expr(Node, {MatchExpr, RegExpr}) ->
    F = fun(Body) ->
		lists:flatmap(fun(E) -> case E == MatchExpr of 
					   true -> [E, RegExpr];
					    _ -> [E]
					end
			      end, Body)
	end,
    case refac_syntax:type(Node) of 
	clause -> 
	    P = refac_syntax:clause_patterns(Node),
	    B = refac_syntax:clause_body(Node),
	    G = refac_syntax:clause_guard(Node),
	    B1 = F(B),
	    case length(B1) == length(B) of
		true -> {Node, false};
		_ -> Node1 = refac_syntax:clause(P,G, B1),
		     {Node1, true}
	    end;
	_  -> {Node, false}
    end.	  
	    
    
is_spawn_app(Tree) ->
    SpawnFuns1 = [{erlang, spawn, 1}, {erlang, spawn, 2}, {erlang, spawn, 3}, {erlang, spawn, 4},
		  {erlang, spawn_link, 1}, {erlang, spawn_link, 2}, {erlang, spawn_link, 3}, {erlang, spawn_link, 4},
		  {erlang, spawn_opt, 3}, {erlang, spawn_opt, 5}],
    %% SpawnFuns2 = [{erlang, spawn_monitor, 1}, {erlang, spawn_monitor, 3}, {erlang, spawn_opt, 2},
%% 		  {erlang, spawn_opt, 4}],
    case refac_syntax:type(Tree) of
      application ->
	  Operator = refac_syntax:application_operator(Tree),
	  Ann = refac_syntax:get_ann(Operator),
	  case lists:keysearch(fun_def, 1, Ann) of
	    {value, {fun_def, {Mod, Fun, Arity, _, _}}} -> lists:member({Mod, Fun, Arity}, SpawnFuns1);
	    _ -> false
	  end;
      _ -> false
    end.



evaluate_expr(Files, ModName, AnnAST, FunDef, Expr) ->
    F = fun(E) ->
		Es = [refac_syntax:revert(E)],
		case catch erl_eval:exprs(Es, []) of 
		    {value, V, _} -> {value, V};
		    _ ->
			FunName = refac_syntax:data(refac_syntax:function_name(FunDef)),
			Arity = refac_syntax:function_arity(FunDef),
			{StartPos, _} = refac_util:get_range(Expr),
			{unknown, {ModName, FunName, Arity, StartPos}}
		end
	end,
    Exprs = case refac_util:get_free_vars(Expr) of 
		[] -> [Expr];
		_ ->  refac_slice:backward_slice(Files, AnnAST, ModName, FunDef, Expr)
	    end,
    Values = lists:map(F, Exprs),
    Values.


funs_called(Node) ->
    HandleSpecialFuns = fun (Arguments, S) ->
				case Arguments of
				    [M, F, A] ->
					case {refac_syntax:type(M), refac_syntax:type(F), refac_syntax:type(A)} of
					    {atom, atom, list} ->
						ModName = refac_syntax:atom_value(M),
						FunName = refac_syntax:atom_value(F),
						Arity = refac_syntax:list_length(A),
						ordsets:add_element({ModName, FunName, Arity}, S);
					    _ -> S
					end;
				    [M, F, A, _O] ->
					case {refac_syntax:type(M), refac_syntax:type(F), refac_syntax:type(A)} of
					    {atom, atom, list} ->
						ModName = refac_syntax:atom_value(M),
						FunName = refac_syntax:atom_value(F),
						Arity = refac_syntax:list_length(A),
						ordsets:add_element({ModName, FunName, Arity}, S);
					    _ -> S
					end
				end
			end,
    F2 = fun (T, S) ->
		 case refac_syntax:type(T) of
		     application ->
			 Op = refac_syntax:application_operator(T),
			 Args= refac_syntax:application_arguments(T),
			 case lists:keysearch(fun_def,1, refac_syntax:get_ann(Op)) of 
			     {value, {fun_def, {M, F, A, _, _}}} ->
				 S1 = ordsets:add_element({M, F, A}, S),
				 case {M, F, A} of
					{erlang, apply, 3} -> HandleSpecialFuns(Args, S1);
					{erlang, spawn, 3} -> HandleSpecialFuns(Args, S1);
					{erlang, spawn, 4} -> HandleSpecialFuns(Args, S1);
					{erlang, spawn_link, 3} -> HandleSpecialFuns(Args, S1);
					{eralng, spawn_link, 4} -> HandleSpecialFuns(Args, S1);
				     _ -> S1
				 end;
			     _ -> S
			 end;
		     _ -> S
		 end
	 end,
    lists:usort(refac_syntax_lib:fold(F2, [], Node)).



pos_to_spawn_match_expr(AnnAST, Start, End) ->
    Message = "You have not selected a match expression whose left-hand side is a PID, and right-hand side is a spawn expression!",
    case refac_util:pos_to_expr(AnnAST, Start, End) of 
	{ok, Expr} ->
	    case refac_syntax:type(Expr) of 
		match_expr -> 
		    P = refac_syntax:match_expr_pattern(Expr),
		    B = refac_syntax:match_expr_body(Expr),
		    case {is_spawn_app(B), refac_syntax:type(P) == variable} of 
			{true, true} ->
			    {ok, Expr};
			_ -> {error, Message}
		    end;
		_ -> {error, Message}
	    end;
	_ -> {error, Message}    
    end.


%% TODO: REFACTOR THE FOLLOWING TWO FUNCTIONS.
pos_to_receive_expr(FunDef, Start) ->
    F = fun(T, Acc) ->
		case refac_syntax:type(T)==receive_expr of 
		    true -> [T|Acc];
		    _ -> Acc
		end
	end,
    ReceiveExprs = refac_syntax_lib:fold(F, [], FunDef),
    lists:any(fun(E) ->
		      {Start1, End1} = refac_util:get_range(E),
		      (Start1 =< Start) andalso (Start =< End1)
	      end, ReceiveExprs).
    

pos_to_list_comp_expr(FunDef, Start) ->
    F = fun(T, Acc) ->
		case refac_syntax:type(T) of 
		    list_comp -> [T|Acc];
		    _ -> Acc
		end
	end,
    ReceiveExprs = refac_syntax_lib:fold(F, [], FunDef),
    lists:any(fun(E) ->
		      {Start1, End1} = refac_util:get_range(E),
		      (Start1 =< Start) andalso (Start =< End1)
	      end, ReceiveExprs).



is_process_name(Name) ->
    refac_util:is_fun_name(Name) and (list_to_atom(Name) =/= undefined).
