
%% ============================================================================================
%% Refactoring: Instrument the program to trace process communication information.
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
%% Qn: is it possible to get more static infomration? How to balance the usages of staic and dynamic info?
%% How do we know the existing trace info is uptodate?
%% what about the spawn expression has receive expressions?

%% Is theory, it is possible that a  receiver function is shared by different processes (processse with different 
%% entries); it practice, this rarely happen. We ignore this case for now.

%% special functions involving Pids:
%% spawn, link, register, send, !, exit ...
%% =============================================================================================
-module(refac_add_a_tag).

-export([add_a_tag/5, add_a_tag/6,send_expr_to_region/5]).

-export([counter/0, collector/0,process_trace_info/1, start_collector_process/0]).

-export([trim_callgraph/1]).

-compile(export_all).

-include("../hrl/wrangler.hrl").


%% =============================================================================================
add_a_tag(FileName, Line, Col, Tag, SearchPaths) ->
    io:format("\n[CMD: add_a_tag, ~p, ~p, ~p, ~p,~p]\n", [FileName, Line, Col, Tag, SearchPaths]),
    case is_atom(list_to_atom(Tag)) of 
	true -> case refac_util:parse_annotate_file(FileName, true, SearchPaths) of 
		    {ok, {AnnAST, Info}} ->
			 case pos_to_receive_fun(AnnAST, {Line, Col}) of 
			     {ok, FunDef} ->
				 {value, {module, ModName}} = lists:keysearch(module, 1, Info),
				 FunName = refac_syntax:data(refac_syntax:function_name(FunDef)),
				 FunArity = refac_syntax:function_arity(FunDef),
				 CallGraph = callgraph(AnnAST, Info, FileName),
				 ReceiveFuns = collect_receive_funs(AnnAST, Info),
				 InitialFuns = collect_process_initial_funs(AnnAST, Info),
				 ReachedReceiveFuns = reached_receive_funs(InitialFuns, CallGraph, ReceiveFuns, Info),
				 %% get the affected initials by analysing the receiving side.
				 AffectedInitials = get_affected_initials(ReachedReceiveFuns, {ModName, FunName, FunArity}) ,
				 %%io:format("AffectInitials (initial result):\n~p\n", [AffectedInitials]),
				 ProcessedTraceRes =process_trace_info("c:/cygwin/home/hl/distel-wrangler-0.2/test/wrangler_trace_cache"),
				 FinalAffectedInitials = get_affected_funs_from_send(ProcessedTraceRes, AffectedInitials, ReceiveFuns),
				 %% io:format("Affected initial functions:\n~p\n", [FinalAffectedInitials]),
				 ReachedReceiveFuns1 = get_reached_receive_funs_1(FinalAffectedInitials, ReachedReceiveFuns),
				 %% io:format("Affected receive functions:\n~p\n", [ReachedReceiveFuns1]),
				 start_collector_process(),
				 {AnnAST1, Modified} = do_add_a_tag(AnnAST, Tag,ModName, FinalAffectedInitials, ReachedReceiveFuns1, ProcessedTraceRes),
				 send_collector ! {get, self()},
			         receive
				     {collector, S} -> S
				 end,
				 send_collector!stop,
				 if Modified ->
					 refac_util:write_refactored_files([{{FileName, FileName}, AnnAST1}]),
					{ok, S};
				    true ->{ok, []}
				 end;
			     _ -> {error, "You have not selected a function containing receive expressions!"}
			 end;
		    {error, Reason} -> {error, Reason}
		end;
	false  -> {error, "The tag to add should be an atom!"}
    end.

add_a_tag(FileName, TagName, Line1, Col1, Line2, Col2) ->
    F = fun(Node, {Tag, Range}) ->
		case refac_syntax:type(Node) of 
		    infix_expr ->
			case is_send_expr(Node) of 
			    true ->
				case refac_util:get_range(Node) of 
				    Range ->
					ReceiverPid = refac_syntax:infix_expr_left(Node),
					Msg = refac_syntax:infix_expr_right(Node),
					Op  = refac_syntax:infix_expr_operator(Node),
					Msg1 = case refac_syntax:type(Msg) of 
						   tuple ->
						       refac_syntax:tuple([Tag | refac_syntax:tuple_elements(Msg)]);
						   _ -> refac_syntax:tuple([Tag, Msg])
					       end,
					{refac_syntax:infix_expr(ReceiverPid, Op, Msg1), true};
				    _ ->
					{Node, false}
				end;
			    _ -> {Node, false}
			end;
		    _ -> {Node, false}
		end
	end,
    case refac_util:parse_annotate_file(FileName, true, []) of  %% Need to fill in the searchpaths.
	{ok, {AnnAST, _Info}} ->
	    {AnnAST1, _Modified} = refac_util:stop_tdTP(F, AnnAST, {refac_syntax:atom(TagName), {{Line1, Col1}, {Line2, Col2}}}),
	    refac_util:write_refactored_files([{{FileName, FileName}, AnnAST1}]),
	    {ok, []};
	{error, Reason} ->
	    {error, Reason}
    end.
    
test(AnnAST) ->
    F = fun(Node, _Others) ->
		case refac_syntax:type(Node) of 
		    function ->
			io:format("is_process_related_fun:\n~p\n", [is_process_related_fun(Node)]),
			{Node, true};
		    _ ->
			{Node, false}
		end
	end,
    refac_util:stop_tdTP(F, AnnAST, []).

annotate_pids(DirList) ->
    Files = refac_util:expand_files(DirList, ".erl"),
    Sccs= trim_callgraph(DirList),
    Sccs2 = lists:concat(Sccs),
    Funs = lists:map(fun({Fun, _FunDef}) -> Fun end, Sccs2),
    Pid = start_fun_typesig_process(Funs),
    Sccs3 = lists:map(fun(S) -> annotate_a_fun(S, Pid) end, Sccs2),
    ASTs=lists:map(fun(F) -> update_function(F, Sccs3) end,  Files),
    Res = refac_util:full_buTP(fun add_a_tag_to_send_exprs/2, hd(ASTs), []),   
    Sccs4 = fixpoint(lists:reverse(Sccs3), Pid),
   %% io:format("callgraph:\n~p\n", [lists:map(fun({Fun, _FunDef})-> Fun end, lists:reverse(Sccs4))]),
    Pid ! stop,
    ok.
	

add_a_tag_to_send_exprs(Node, _TagName) ->
     case refac_syntax:type(Node) of 
 	infix_expr ->
 	    case is_send_expr(Node) of 
 		true ->
 		    ReceiverPid = refac_syntax:infix_expr_left(Node),
 		    %% io:format("Node:\n~p\n", [ReceiverPid]),
 		    Node;
 		_ -> Node
 	    end;
 	_ -> Node
     end.

fixpoint(Sccs, Pid) ->
    Pid ! {self(), getenv},
    receive 
	{Pid, Env} ->
	    Env
    end,
    Sccs4 = lists:map(fun(S) -> topdown_propagate(S, Pid) end, Sccs),
    Pid ! {self(), getenv},
    receive 
	{Pid, Env1} ->
	    Env1
    end,
    io:format("Env0:\n~p\n",[Env]),
    io:format("Env1:\n~p\n",[Env1]),
    case Env==Env1 of 
	true -> 
	    Pid!stop,
	    Sccs4;
	_ ->
	    fixpoint(Sccs4, Pid)
    end.

   

annotate_a_fun({{ModName, FunName, Arity}, FunDef}, TypeSigPid) ->
    EnvPid = start_env_process(),
    FunDef1 = annotate_special_fun_apps({{ModName, FunName, Arity}, EnvPid, FunDef}),
    %% Note
    FunDef2 = refac_util:full_buTP(fun annotate_within_fun/2, FunDef1, {ModName, FunName, Arity, EnvPid, TypeSigPid}), 
    FunDef3 = refac_util:full_buTP(fun annotate_within_fun/2, FunDef2, {ModName, FunName, Arity, EnvPid, TypeSigPid}),
    EnvPid ! stop,
    {{ModName, FunName, Arity}, FunDef3}.

update_function(File, FunList) ->
    {ok, {AnnAST, Info}} = refac_util:parse_annotate_file(File, true, []),
    {value, {module, ModName}} = lists:keysearch(module, 1, Info),
    F = fun(Node, []) ->
		case refac_syntax:type(Node) of 
		    function -> FunName = refac_syntax:data(refac_syntax:function_name(Node)),
				Arity = refac_syntax:function_arity(Node),
				case lists:keysearch({ModName, FunName, Arity}, 1, FunList) of 
				    {value, {{ModName, FunName, Arity}, FunDef}} -> {FunDef, true};
				    _ -> {Node, false}
				end;				    
		    _ -> {Node, false}
		end
	end,
    {AnnAST1, _} = refac_util:stop_tdTP(F, AnnAST, []),
    AnnAST1.
     

%% annotate_sccs([], Acc, Env) ->
%%      {Acc, Env};
%% annotate_sccs([Scc|Sccs], Acc, Env) ->
%%      {Scc1, Env1}= annotate_a_scc(Scc,[],Env),
%%      annotate_sccs(Sccs, [Scc1|Acc], Env1).

%% annotate_a_scc([],  Acc, Env) ->
%%     {lists:reverse(Acc), Env};
%% annotate_a_scc([{Fun, FunDef}|Fs], Acc, Env) ->
%%     {FunDef1, Env1} = annotate_a_fun({Fun, FunDef}), 
%%     annotate_a_scc(Fs, [{Fun, FunDef1}|Acc], Env1).




annotate_within_fun(Node, {_ModName, FunName, Arity, Pid, TypeSigPid}) ->
    case refac_syntax:type(Node) of 
	variable -> 
	    Ann = refac_syntax:get_ann(Node),
	    case lists:keysearch(def, 1, Ann) of
		{value, {def, DefinePos}} ->
		    Pid ! {self(), get, {def,DefinePos}},
		    receive
			{Pid, value, Value} -> 
			    refac_util:update_ann(Node, Value);
			{Pid, false} ->    
			    Node
		    end;
		_ -> Node
	    end;
	 match_expr->
	    P = refac_syntax:match_expr_pattern(Node),
	    B = refac_syntax:match_expr_body(Node),
	    Ann = refac_syntax:get_ann(B),
	    case lists:keysearch(process, 1, Ann) of 
		{value, {process, Value}} ->
		    P1 = refac_util:update_ann(P, {process, Value}),
		    case refac_syntax:type(P) of 
			variable -> Ann1 = refac_syntax:get_ann(P),
				    {value, {def, DefinePos}} = lists:keysearch(def, 1, Ann1),
				    Pid ! {add,{{def, DefinePos}, {process, Value}}};
			_ -> ok                %%% What about the complex pattern matches?
		    end,
		    refac_syntax:match_expr(P1, B);
		_ -> Node
	    end;
	application ->
	    Operator = refac_syntax:application_operator(Node),
	    Ann = refac_syntax:get_ann(Operator),
	    case lists:keysearch(fun_def,1, Ann) of 
		{value, {fun_def, {Mod, F, A, _, _}}}  -> 
		    Args = refac_syntax:application_arguments(Node),
		    TypeSigPid!{self(), get, {Mod,F, A}},
		    Args1 = receive 
				{TypeSigPid, value, {ParSig, _RtnSig}} ->
				    lists:map(fun({Arg, T}) ->
						      case T of 
							  {process, Value} ->
							      case refac_syntax:type(Arg) of 
								  variable ->
								      Ann1 = refac_syntax:get_ann(Arg),
								      case lists:keysearch(process, 1, Ann1) of
									  {value, {process, _Value1}} ->
									      Node;
									  false ->
									      {value, {def, DefinePos}} = lists:keysearch(def,1, Ann1),
									      Pid ! {add, {{def, DefinePos}, {process, Value}}},
									      refac_util:update_ann(Arg, {process, Value})
								      end;
								  _ ->Ann1 = refac_syntax:get_ann(Arg),  
								      case lists:keysearch(process, 1, Ann1) of 
									  {value, {process, _Value}} ->
									      Node;
									  false ->
									      refac_util:update_ann(Arg, {process, Value})
								      end
							      end;
							  _ -> Arg						      
						      end
					      end, lists:zip(Args, ParSig));
				{TypeSigPid, false} -> Args
			    end,
		    refac_syntax:copy_attrs(Node, refac_syntax:application(Operator, Args1));		    		    
		_ ->
		    Node
	    end;
	function ->
	    Ann = refac_syntax:get_ann(Node),
	    {value, {fun_def, {Mod, FunName, Arity, _, _}}} = lists:keysearch(fun_def, 1, Ann),
	    Cs = refac_syntax:function_clauses(Node),
	    lists:map(fun(C) ->
			      Ps = refac_syntax:clause_patterns(C),
			      ArgsInfo = lists:map(fun(P) ->
							   Ann1 = refac_syntax:get_ann(P),
							   case lists:keysearch(process,1, Ann1) of 
							       {value, {process, Value}} ->
								   {process, Value};
							       _  -> any
							   end
						   end, Ps),
			      Info = {{Mod, FunName, Arity}, {ArgsInfo, any}},
			      TypeSigPid ! {add, Info}
		      end, Cs),
	    Node;
	_ -> Node
    end.

topdown_propagate({{ModName, FunName, Arity}, FunDef}, TypeSigPid) ->
    EnvPid = start_env_process(),
    FunDef2 = do_topdown_propagate(FunDef, {ModName, FunName, Arity, EnvPid, TypeSigPid}),
    FunDef3 = refac_util:full_buTP(fun annotate_within_fun_1/2, FunDef2, {ModName, FunName, Arity, EnvPid, TypeSigPid}),
    EnvPid ! stop,
    {{ModName, FunName, Arity}, FunDef3}.

do_topdown_propagate(FunDef, {_ModName, FunName, Arity, EnvPid, TypeSigPid}) ->
    F = fun(Pat, Typ) ->
	       Ann = refac_syntax:get_ann(Pat),
	       case lists:keysearch(process,1, Ann) of 
		   {value, {process, Value}} ->
		       case Typ of 
			   any -> {process, Value};
			   {process, []} -> {process, Value};
			   {process, Value1} -> {process, lists:usort(Value++Value1)}
		       end;
		   false ->
		       Typ
	       end
	end,
    F2 = fun(Node, _Others) ->
		 case refac_syntax:type(Node) of 
		     application ->
			 Operator = refac_syntax:application_operator(Node),
			 Ann = refac_syntax:get_ann(Operator),
			 case lists:keysearch(fun_def,1, Ann) of 
			     {value, {fun_def, {M1, F1, A1, _P1, _P2}}}  -> 
				 Args = refac_syntax:application_arguments(Node),
				 TypeSigPid ! {self(), get, {M1, F1, A1}},
				 receive
				     {TypeSigPid, value, {ParSig, _RtnSig}} ->
					 ArgsInfo = lists:map(fun({A, P}) -> F(A,P) end, lists:zip(Args, ParSig)),
					 Info = {{M1, F1, A1}, {ArgsInfo, any}}, %% any is for the return type of the function.
					 
					 TypeSigPid!{add, Info};
				     {TypeSigPid, false} ->
					 ok
				 end,
				 Node;
			     _ ->
				 Node
			 end;
		     variable ->
			 Ann = refac_syntax:get_ann(Node),
			 case lists:keysearch(def, 1, Ann) of
			     {value, {def, DefinePos}} ->
				 EnvPid ! {self(), get, {def,DefinePos}},
				 receive
				     {EnvPid, value, Value} -> 
					 refac_util:update_ann(Node, Value);
				     {EnvPid, false} ->    
					 Node
				 end;
			     _ -> Node
			 end;
		     _  -> 
			 Node
		 end
	 end,
    Ann = refac_syntax:get_ann(FunDef),
    {value, {fun_def, {Mod, FunName, Arity, _, _}}} = lists:keysearch(fun_def,1, Ann),
    TypeSigPid ! {self(), get, {Mod, FunName, Arity}},
    receive
	{TypeSigPid, value, {ParsSig, _RtnSig}} -> 
	    FunName1 = refac_syntax:function_name(FunDef),
	    Cs = refac_syntax:function_clauses(FunDef),
	    Cs1 = lists:map(fun(C) -> Ps = refac_syntax:clause_patterns(C),
				      B = refac_syntax:clause_body(C),
				      G = refac_syntax:clause_guard(C),
				      Ps1 = lists:map(fun({P,T}) ->  %% what about complex parameters.
							      case refac_syntax:type(P) of 
								  variable ->
								      case T of 
									  {process, Value} -> 
									      Ann1 = refac_syntax:get_ann(P),
									      {value, {def, DefinePos}} = lists:keysearch(def, 1,Ann1),
									      EnvPid ! {add, {{def, DefinePos}, {process, Value}}},
									      refac_util:update_ann(P, {process, Value});
									  _ -> P
								      end;
								  _  -> P
							      end
						      end, lists:zip(Ps, ParsSig)),
				      C1 = refac_syntax:copy_attrs(C, refac_syntax:clause(Ps1, G, B)),
				      refac_util:full_buTP(F2, C1, [])
			    end, Cs),
  		    refac_syntax:copy_attrs(FunDef, refac_syntax:function(FunName1, Cs1));
	{TypeSigPid, false} -> FunDef
    end.


annotate_within_fun_1(Node, {_ModName, FunName, Arity, Pid, TypeSigPid}) ->
    case refac_syntax:type(Node) of 
	variable -> 
	    Ann = refac_syntax:get_ann(Node),
	    case lists:keysearch(def, 1, Ann) of
		{value, {def, DefinePos}} ->
		    Pid ! {self(), get, {def,DefinePos}},
		    receive
			{Pid, value, Value} -> 
			    refac_util:update_ann(Node, Value);
			{Pid, false} ->    
			    Node
		    end;
		_ -> Node
	    end;
	 match_expr->
	    P = refac_syntax:match_expr_pattern(Node),
	    B = refac_syntax:match_expr_body(Node),
	    Ann = refac_syntax:get_ann(B),
	    case lists:keysearch(process, 1, Ann) of 
		{value, {process, Value}} ->
		    P1 = refac_util:update_ann(P, {process, Value}),
		    case refac_syntax:type(P) of 
			variable -> Ann1 = refac_syntax:get_ann(P),
				    {value, {def, DefinePos}} = lists:keysearch(def, 1, Ann1),
				    Pid ! {add,{{def, DefinePos}, {process, Value}}};
			_ -> ok                %%% What about the complex pattern matches?
		    end,
		    refac_syntax:match_expr(P1, B);
		_ -> Node
	    end;
%% 	application ->
%% 	    Operator = refac_syntax:application_operator(Node),
%% 	    Ann = refac_syntax:get_ann(Operator),
%% 	    case lists:keysearch(fun_def,1, Ann) of 
%% 		{value, {fun_def, {Mod, F, A, _, _}}}  -> 
%% 		    Args = refac_syntax:application_arguments(Node),
%% 		    TypeSigPid!{self(), get, {Mod,F, A}},
%% 		    Args1 = receive 
%% 				{TypeSigPid, value, {ParSig, _RtnSig}} ->
%% 				    lists:map(fun({Arg, T}) ->
%% 						      case T of 
%% 							  {process, Value} ->
%% 							      case refac_syntax:type(Arg) of 
%% 								  variable ->
%% 								      Ann1 = refac_syntax:get_ann(Arg),
%% 								      case lists:keysearch(process, 1, Ann1) of
%% 									  {value, {process, _Value1}} ->
%% 									      Node;
%% 									  false ->
%% 									      {value, {def, DefinePos}} = lists:keysearch(def,1, Ann1),
%% 									      Pid ! {add, {{def, DefinePos}, {process, Value}}},
%% 									      refac_util:update_ann(Arg, {process, Value})
%% 								      end;
%% 								  _ ->Ann1 = refac_syntax:get_ann(Arg),  
%% 								      case lists:keysearch(process, 1, Ann1) of 
%% 									  {value, {process, _Value}} ->
%% 									      Node;
%% 									  false ->
%% 									      refac_util:update_ann(Arg, {process, Value})
%% 								      end
%% 							      end;
%% 							  _ -> Arg						      
%% 						      end
%% 					      end, lists:zip(Args, ParSig));
%% 				{TypeSigPid, false} -> Args
%% 			    end,
%% 		    refac_syntax:copy_attrs(Node, refac_syntax:application(Operator, Args1));	
		
%% 		_ ->
%% 		    Node
%% 	    end;
%% 	function ->
%% 	    Ann = refac_syntax:get_ann(Node),
%% 	    {value, {fun_def, {Mod, FunName, Arity, _, _}}} = lists:keysearch(fun_def, 1, Ann),
%% 	    Cs = refac_syntax:function_clauses(Node),
%% 	    lists:map(fun(C) ->
%% 			      Ps = refac_syntax:clause_patterns(C),
%% 			      ArgsInfo = lists:map(fun(P) ->
%% 							   Ann1 = refac_syntax:get_ann(P),
%% 							   case lists:keysearch(process,1, Ann1) of 
%% 							       {value, {process, Value}} ->
%% 								   {process, Value};
%% 							       _  -> any
%% 							   end
%% 						   end, Ps),
%% 			      Info = {{Mod, FunName, Arity}, {ArgsInfo, any}},
%% 			      TypeSigPid ! {add, Info}
%% 		      end, Cs),
%% 	    Node;
	_ -> Node
    end.

annotate_special_fun_apps({{ModName, FunName, Arity}, EnvPid, FunDef}) ->
    refac_util:full_buTP(fun do_annotate_special_fun_apps/2,FunDef, {ModName, FunName, Arity, EnvPid}).

do_annotate_special_fun_apps(Node, {ModName, FunName, Arity, EnvPid}) ->
    F = fun(Args) ->
		case Args of 
		     [M, F, A] ->
			ModName = refac_syntax:atom_value(M),
			FunName = refac_syntax:atom_value(F),
			Arity = refac_syntax:list_length(A),
			{ModName, FunName, Arity};
		    [_N, M, F, A] ->
			ModName = refac_syntax:atom_value(M),
			FunName = refac_syntax:atom_value(F),
			Arity = refac_syntax:list_length(A),
			{ModName, FunName, Arity};
		    [Fun] -> refac_prettypr:format(Fun);
		    [_N, Fun] -> refac_prettypr:format(Fun)
		end
	end,
    case refac_syntax:type(Node) of 
	application ->
	    case is_spawn_app(Node) of 
		true ->
		    Arguments = refac_syntax:application_arguments(Node),
		    InitialFun = F(Arguments),
		    Node1 = refac_util:update_ann(Node, {process, [{pid,InitialFun}]}),
		    Node1;
		_ ->case is_self_app(Node) of
			true ->
			    CurrentFun = {ModName, FunName, Arity},
			    %% TODO: NEED TO FIND OUT THE REAL INITIAL FUN.
			    Node1 = refac_util:update_ann(Node, {process, [{pid, CurrentFun}]}),  
			    Node1;
			_ -> case is_register_app(Node) of 
				 true -> 
				     Operator = refac_syntax:application_operator(Node),
				     [RegName, Pid]= refac_syntax:application_arguments(Node),
				     case lists:keysearch(process, 1, refac_syntax:get_ann(Pid)) of 
					 {value, {process, [{pid, Value}]}} ->
					     RegName1 = refac_util:update_ann(RegName,{process, [{pname, Value}]}),
					     Node1 =refac_syntax:application(Operator, [RegName1, Pid]),
					     case refac_syntax:type(RegName) of 
						 variable -> 
						     Ann1 = refac_syntax:get_ann(RegName), 
						     {value, {def, DefinePos}} = lists:keysearch(def, 1, Ann1),
						     EnvPid ! {add, {{def, DefinePos}, {process, [{pname, Value}]}}},
						     refac_syntax:copy_attrs(Node, Node1);
						 atom -> 
						     EnvPid ! {add, {refac_syntax:atom_value(RegName), {process, [{pname, Value}]}}},
						     refac_syntax:copy_atts(Node, Node1)
					     end;
					 _ ->  Node
				     end;
				 _ -> Node
			     end
		    end
	    end;
	infix_expr ->
 	    case is_send_expr(Node) of 
		true -> Dest = refac_syntax:infix_expr_left(Node),
			Ann = refac_syntax:get_ann(Dest),
			case lists:keysearch(process, 1, Ann) of 
			    false ->
				case refac_syntax:type(Dest) of 
				    variable ->
					{value, {def, DefinePos}} = lists:keysearch(def, 1, Ann),
					EnvPid ! {add, {{def, DefinePos}, {process, []}}},
					refac_util:update_ann(Node,{process, []});
				    atom ->
					PName = refac_syntax:atom_value(Dest),
					EnvPid ! {add, PName, {process, [{pname, any}]}},
					refac_util:update_ann(Node, {process, [{pname, any}]});
				    _ -> Node			
				end;
			    _ -> Node
			end
	    end;
	_  -> Node
    end.


is_register_app(T) ->
    case refac_syntax:type(T) of 
	application ->
	    Operator = refac_syntax:application_operator(T),
	    Ann = refac_syntax:get_ann(Operator),
	    case lists:keysearch(fun_def, 1, Ann) of 
		{value, {fun_def, {erlang, register,2, _, _}}} ->
		    true;
		_  ->
		    false
	    end;
	_ -> false
    end.
    

is_self_app(T) ->
    case refac_syntax:type(T) of 
	application ->
	    Operator = refac_syntax:application_operator(T),
	    Ann = refac_syntax:get_ann(Operator),
	    case lists:keysearch(fun_def, 1, Ann) of 
		{value, {fun_def, {erlang, self, 0, _, _}}} ->
		    true;
		_  ->
		    false
	    end;
	_ -> false
    end.

is_spawn_app(T) ->
    SpawnFuns = [{erlang, spawn, 1}, {erlang, spawn,2}, {erlang, spawn, 3},
		 {erlang, spawn, 4}, {erlang, spawn_link, 1}, {erlang, spawn_link, 2}, {erlang, spawn_link, 3},
		 {erlang, spawn_link, 4}],
    case refac_syntax:type(T) of 
	application ->
	    Operator = refac_syntax:application_operator(T),
	    Ann = refac_syntax:get_ann(Operator),
	    case lists:keysearch(fun_def,1,Ann) of 
		{value, {fun_def, {Mod, Fun, Arity, _, _}}} ->
		    lists:member({Mod, Fun, Arity}, SpawnFuns);
		_ -> false
	    end;
	_ -> false
    end.


is_send_expr(Tree) ->
    case refac_syntax:type(Tree) of 
	infix_expr ->
	    Op = refac_syntax:infix_expr_operator(Tree),
	    case refac_syntax:type(Op) of 
		operator ->   %% TODO: should also check the uses of erlang:send/2, erlang:send/3 and other variants of send.
		    refac_syntax:operator_name(Op) == '!';
		_ -> false
	    end;
	_ -> false
    end.



trim_callgraph(DirList) ->
    Files = refac_util:expand_files(DirList, ".erl"),
    CallGraph = refac_util:build_call_graph(Files, []),
    #callgraph{scc_order = Sccs, external_calls = _E} = refac_callgraph:construct(CallGraph),
    CallerCallee = lists:map(fun({{Caller, _CallerDef}, Callee}) ->
					{Caller, Callee} end, CallGraph),
    Res = trim_scc(Sccs, CallerCallee, [], []),
    %% Sccs1 =[[Fun||{Fun, _FunDef}<-Scc]||Scc<-Res],
    Res.
    

trim_scc([], _CallerCallee, _PFunAcc, Acc) -> lists:reverse(Acc);
trim_scc([Scc|Sccs], CallerCallee, PFunAcc, Acc) ->
    SccFuns = lists:map(fun({Fun, _FunDef}) -> Fun end, Scc),
    IsProcessScc = lists:any(fun({_Fun, FunDef}) ->is_process_related_fun(FunDef) end, Scc),
    CalledFuns = lists:usort(lists:flatmap(fun(Fun) ->
						   case lists:keysearch(Fun, 1, CallerCallee) of 
						       {value, {Fun, Called}} ->
							   Called;
						       _ -> []
						   end
					   end, SccFuns)),
    PFunsCalled = length(lists:subtract(CalledFuns, PFunAcc))<length(CalledFuns),
    case  IsProcessScc orelse PFunsCalled of 
	true ->
	    trim_scc(Sccs, CallerCallee,SccFuns++PFunAcc,  [Scc|Acc]);
	_ -> trim_scc(Sccs, CallerCallee, PFunAcc, Acc)
    end. 

is_process_related_fun(FunDef) ->
    ProcessFuns = [{erlang, register, 2}, {erlang, self, 0}, {erlang, spawn, 1}, {erlang, spawn,2}, {erlang, spawn, 3},
		   {erlang, spawn, 4}, {erlang, spawn_link, 1}, {erlang, spawn_link, 2}, {erlang, spawn_link, 3},
		   {erlang, spawn_link, 4}],
    F = fun(Node, _Others) ->
		case refac_syntax:type(Node) of
		    infix_expr->
			case is_send_expr(Node) of
			    true -> {true, true};
			    _ -> {[], false}
			end;
		    receive_expr ->
			{true, true};
		    application ->
			Operator = refac_syntax:application_operator(Node),
			Arity = length(refac_syntax:application_arguments(Node)),
			case refac_syntax:type(Operator) of 
			    atom ->
				Op = refac_syntax:atom_value(Operator),
				{value, {fun_def, {M, Op, A, _, _}}} = lists:keysearch(fun_def,1, refac_syntax:get_ann(Operator)),
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
					case  lists:member({M, Op, Arity}, ProcessFuns) of 
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
    case refac_util:once_tdTU(F, FunDef, []) of 
	{_, false} ->
	    false;
	{_R, true}  -> 
	    true
    end.
		    

send_expr_to_region(FileName, _ModName, FunName, Arity, Index) ->

    F1 = fun(Node, Acc) ->
		case is_send_expr(Node) of 
		    true -> Acc++[Node];
		    _ -> Acc
		end
	 end,
    F = fun(Node, _Others) ->
	   case refac_syntax:type(Node) of 
	       function ->
		   FunName1 = refac_syntax:data(refac_syntax:function_name(Node)),
		   Arity1 = refac_syntax:function_arity(Node),
		   case {FunName1, Arity1} of 
		       {FunName, Arity} -> SendExprs = refac_syntax_lib:fold(F1, [], Node),
					   SendExpr = lists:nth(Index, SendExprs),
					   {{Line1, Col1},{Line2, Col2}} = refac_util:get_range(SendExpr),
					   {{Line1, Col1, Line2, Col2}, true};
		       _ -> {[], false}
		   end;
	       _ -> {[], false}
	   end
	end,
    case refac_util:parse_annotate_file(FileName, true, []) of   %% SearchPaths temporally empty; Need to be modifed.
	{ok, {AnnAST, _Info}} -> case refac_util:once_tdTU(F, AnnAST, []) of
 				    {Res, true} -> {ok, Res};
				    {_, false} -> {error, "Send expression not found!"}
				end;
	{error, Reason} -> {error, Reason}
    end.

do_add_a_tag(AnnAST, Tag, ModName, InitialFuns,ReceiveFuns,Trace) ->
    refac_util:stop_tdTP(fun do_add_a_tag_1/2, AnnAST, {refac_syntax:atom(Tag), ModName, InitialFuns, ReceiveFuns, Trace}).

do_add_a_tag_1(Node, {Tag, ModName, InitialFuns, ReceiveFuns, Trace}) ->
    case refac_syntax:type(Node) of 
	function ->
	    FunName = refac_syntax:data(refac_syntax:function_name(Node)),
	    Arity = refac_syntax:function_arity(Node),
	    Node1 = refac_util:full_buTP(fun do_add_a_tag_2/2, Node, 
 				 {Tag,lists:member({ModName, FunName, Arity}, ReceiveFuns)}),
	    Pid = start_counter_process(),
	    Res = refac_util:stop_tdTP(fun do_add_a_tag_3/2, Node1, {{ModName, FunName, Arity}, Tag, InitialFuns, Pid,Trace}),
	    Pid ! stop,
	    Res;
	_ -> {Node, false}
    end.
				 
do_add_a_tag_2(Node, {Tag, AffectedReceiveFun}) ->
    F = fun(C) ->
		P = refac_util:ghead("do_add_a_tag_2", refac_syntax:clause_patterns(C)),
		G = refac_syntax:clause_guard(C),
		B = refac_syntax:clause_body(C),
		P1 = case refac_syntax:type(P) of 			 
			 tuple -> 
			     Es=[Tag|refac_syntax:tuple_elements(P)],
			     refac_syntax:tuple(Es);
			 _ -> refac_syntax:tuple([Tag, P]) %%WHEN THE MESSAGE IS NOT A TUPLE.
		     end,
		refac_syntax:clause([P1], G, B)
	end,
    case refac_syntax:type(Node) of 
	receive_expr ->
	    if AffectedReceiveFun 
	       -> Cs = refac_syntax:receive_expr_clauses(Node),
		  Cs1 = lists:map(F, Cs),
		  refac_syntax:receive_expr(Cs1);
	       true -> Node
	    end;
	_ -> Node
    end. 

do_add_a_tag_3(Node, {{ModName, FunName, Arity}, Tag, InitialFuns, Pid, Trace}) ->		    
    case refac_syntax:type(Node) of
	infix_expr ->
	    case is_send_expr(Node) of 
		true ->
		    Pid ! {self(), next},
		    receive
			{Pid, N} -> N
		    end,
		    Fs = get_initial_funs({ModName, FunName, Arity, N}, Trace),
		    case Fs of 
			[] ->  %% WARNING: NOT COVERED BY THE TRACE INFO.
			    send_collector ! {add, {ModName, FunName, Arity, N}},
			    {Node, true};
			_ -> case length(Fs) > length(lists:subtract(Fs, InitialFuns)) of 
				 true ->  %% Covered by trace, and IS affected by the refactoring.
				     ReceiverPid = refac_syntax:infix_expr_left(Node),
				     Msg = refac_syntax:infix_expr_right(Node),
				     Op  = refac_syntax:infix_expr_operator(Node),
				     Msg1 = case refac_syntax:type(Msg) of 
						tuple ->
						    refac_syntax:tuple([Tag | refac_syntax:tuple_elements(Msg)]);
						_ -> refac_syntax:tuple([Tag, Msg])
					    end,
				     {refac_syntax:infix_expr(ReceiverPid, Op, Msg1), true};
				 false -> {Node, true}   %% Covered by trace, but is not affected by the refactoring.
			     end
		    end;  
		_ -> 
		    {Node, false}
	    end;
	_ ->
	    {Node, false}
    end.

get_reached_receive_funs_1(Inits, InitReceivers) ->
    F = fun({Int, Receivers}) ->
		case Int of 
		    {Mod, Fun, Arity} ->
			case lists:member({Mod, Fun, Arity}, Inits) of 
			    true -> Receivers;
			    _ -> []
			end;
		    {Mod, Fun, Arity, Index, _Def} ->
			case lists:member({Mod, Fun, Arity, Index}, Inits) of
			    true -> Receivers;
			    _ -> []
			end
		end
	end,
     lists:usort(lists:flatmap(F, InitReceivers)).

process_trace_info(CacheFile) ->
    F = fun(E, Spawns) ->
		Pids = lists:map(fun(S) ->element(3,S) end, Spawns),
		case E of 
		    {send, CurrentFun, ReceiverPid, {initial_call, {erlang, apply,2}}, ReceiverCurrentFun} ->
			case lists:member(ReceiverPid, Pids) of 
			    true -> 
				case lists:keysearch(ReceiverPid, 3, Spawns) of
				    {value, {spawn, Initial, ReceiverPid}} ->
					[{send, CurrentFun, ReceiverPid, {initial_call, Initial}, ReceiverCurrentFun}];
				    _ -> [E]
				end;
			    false -> [E]
			end;
		    {spawn, _, _} -> [];
		    _ -> [E]
		end
	end,
    case dets:open_file(CacheFile) of
      {ok, Tab} ->
	    Trace = lists:concat(dets:match(Tab, '$1')),
	    dets:close(Tab),
	    Spawns = lists:filter(fun(E) -> element(1, E) == spawn end, Trace),
	    lists:concat(lists:map(fun(E) -> F(E, Spawns) end, Trace));					      
      {error, Reason} -> erlang:error(Reason)
    end.

get_initial_funs(Send, Trace) ->
    Res = lists:filter(fun(T) -> element(2, T) == Send end, Trace),
    lists:flatmap(fun({_, _, _, {initial_call, F1}, {current_function, F2}}) 
		 -> [F1, F2] end, Res).
		       

get_affected_funs_from_send(Trace, AffectedInitials, ReceiveFuns) ->
    GroupedTrace = group_by(2, Trace),
    Res = get_affected_funs_from_send_1(GroupedTrace, AffectedInitials, ReceiveFuns),
    case Res == AffectedInitials of 
	true ->
	    Res;
	%% otherwise, repeat until a fix-point has been reached.
	_ -> get_affected_funs_from_send(Trace, Res, ReceiveFuns)
    end.

get_affected_funs_from_send_1([], AffectedInitials, _ReceiveFuns) ->
    AffectedInitials;
get_affected_funs_from_send_1([G|Gs], AffectedInitials, ReceiveFuns) ->
   F = fun({send, _, _, {initial_call, F1}, {current_function, F2}}) ->
	       case F1 of 
		   {erlang, apply, 2} ->
		       case lists:member(F2, ReceiveFuns) of 
			   true ->
			       [F2];
			   _  -> []
		       end;
		   _ -> [F1]
	       end
       end,
   Funs =lists:usort(lists:flatmap(F, G)),
   Len = length(lists:subtract(Funs, AffectedInitials)),
   case (Len <  length(Funs)) andalso (0<Len)  of 
       true -> get_affected_funs_from_send_1(Gs, lists:usort(Funs++AffectedInitials), ReceiveFuns);
       _  -> get_affected_funs_from_send_1(Gs, AffectedInitials, ReceiveFuns)
   end.


group_by(N, TupleList) ->
    group_by_1(N, lists:keysort(N, TupleList)).

group_by_1(_N, []) -> [];
group_by_1(N, TupleList=[E|_Es])->
    NthEle = element(N, E),
    {E1,E2} = lists:splitwith(fun(T) -> element(N,T) == NthEle end, TupleList),
    [E1 | group_by_1(N, E2)].
    
pos_to_receive_fun(AnnAST, Pos) ->
    case refac_util:pos_to_fun_def(AnnAST, Pos) of 
	{ok, FunDef} ->
	    case has_receive_expr(FunDef) of 
		true -> {ok, FunDef};
		_ -> {error, none}
	    end;	    
	_  -> {error, none}
    end.

has_receive_expr(Node) ->
    case refac_util:once_tdTU(fun has_receive_expr/2, Node, []) of 
	{_, false} ->
	     false;
	{_R, true} -> true
    end.

has_receive_expr(Node, []) ->
    case refac_syntax:type(Node) of 
	receive_expr ->
	    {Node, true};
	_  ->{[], false}
    end.


collect_receive_funs(AnnAST, Info) ->
    {value, {module, ModName}} = lists:keysearch(module, 1, Info),
    F = fun(T, Acc) ->
		case refac_syntax:type(T) of 
		    function ->
			case has_receive_expr(T) of 
			    true ->  FunName = refac_syntax:data(refac_syntax:function_name(T)),
				     Arity = refac_syntax:function_arity(T),
				     [{ModName, FunName, Arity}|Acc];
			    _ -> Acc
			end;
		    _ -> Acc
		end
	end,
    refac_syntax_lib:fold(F, [], AnnAST).

collect_process_initial_funs(AnnAST, Info) ->  %% TODO: THINK ABOUT MULTIPLE MODULES.
    {value, {module, ModName}} = lists:keysearch(module, 1, Info),
    HandleSpecialFuns = fun(Arguments) ->
				case Arguments of
				    [M, F, A] ->
				           ModName = refac_syntax:atom_value(M),
					    FunName = refac_syntax:atom_value(F),
					    Arity = refac_syntax:list_length(A),
					    {ModName, FunName, Arity};
				    [_N, M, F, A] ->
				            ModName = refac_syntax:atom_value(M),
					    FunName = refac_syntax:atom_value(F),
					    Arity = refac_syntax:list_length(A),
					    {ModName, FunName, Arity}	   
				    end
			end,
    Fun= fun(Node, Acc) ->			       
		 case refac_syntax:type(Node) of 
		     function ->
			 FunName = refac_syntax:data(refac_syntax:function_name(Node)),
			 FunArity = refac_syntax:function_arity(Node),
			 Pid = start_counter_process(),
			 F = fun(T, S) ->
				     case refac_syntax:type(T) of
					 application ->
					     Operator = refac_syntax:application_operator(T),
					     Arguments = refac_syntax:application_arguments(T),
					     Arity = length(Arguments),
					     case refac_syntax:type(Operator) of 
						 atom ->
						     Op = refac_syntax:atom_value(Operator),  % Todo: Need to think about user-defined spawn.
						     case {Op, Arity} of                      %%TODO: ADD OTHER VARIANTS OF SPAWN.
							 {spawn, 1} -> Pid ! {self(), next},
								       receive 
									   {Pid, N} -> N
								       end,
								       [{ModName, FunName, FunArity, N,hd(Arguments)}| S];
							 {spawn, 3} ->  [HandleSpecialFuns(Arguments)|S];
							 _ -> S
						     end;
						 _ -> S
					     end;
					 _ -> S
				     end
			     end,
			 Res = refac_syntax_lib:fold(F, [],  Node),
			 Pid ! stop,
			 lists:usort(Res ++ Acc);
		     _ -> Acc
		 end
	 end,
    refac_syntax_lib:fold(Fun, [], AnnAST).
		 

start_fun_typesig_process(Funs)->
    spawn_link(refac_add_a_tag, fun_typesig, [Funs]).


fun_typesig(Funs) ->
    InitialEnv = lists:map(fun({Mod, Fun, Arity}) ->
				   {{Mod, Fun, Arity}, {lists:duplicate(Arity, any), any}} end, Funs), 
    fun_typesig_loop(InitialEnv).

fun_typesig_loop(Env) ->
    F = fun(T1, T2) ->
		case T1 of 
		    any -> T2;
		    {process, V1} ->
			case T2 of 
			    any -> T1;
			    {process, V2} ->
				{process, lists:usort(V1++V2)}
			end
		end
	end,
    receive
	{From, get, Fun} ->
	    case  lists:keysearch(Fun,1, Env) of 
		{value, {Fun, TypeSig}} ->
		    From! {self(), value, TypeSig};
		false  -> From!{self(), false}
	    end,
	    fun_typesig_loop(Env);
	{add, {Fun, {Args, Ret}}} ->
	    case lists:keysearch(Fun, 1, Env) of 
		{value, {Fun, {Args1, Ret1}}} ->
		    NewArgs = lists:map(fun({S, S1}) ->
					F(S, S1)
					end, lists:zip(Args, Args1)),
		    NewEnv=lists:keyreplace(Fun,1, Env, {Fun, {NewArgs, F(Ret, Ret1)}}),
		    fun_typesig_loop(NewEnv);
		false ->
		    fun_typesig_loop(Env)
	    end;
	{From, getenv} ->
	    From ! {self(), Env},
	    fun_typesig_loop(Env);
	stop ->
	    io:format("Types:\n~p\n", [Env]),
	    ok
    end.
	    
	      


start_env_process() ->
     spawn_link(refac_add_a_tag, env, []).

env() ->
     env_loop([]).

env_loop(Env) ->
    receive
	{From, get, Key} ->
	    case lists:keysearch(Key, 1, Env) of 
		{value, {Key, Value}} -> From!{self(), value, Value};
		false -> From!{self(), false}
	    end,
	    env_loop(Env);
	{add, {Key, Value}} ->
	    case lists:keysearch(Key, 1, Env) of 
		{value, {Key, _}} ->
		    lists:keyreplace(Key,1, Env, {Key, Value}),
		    env_loop(Env);
		false ->
		    env_loop([{Key, Value} | Env])
	    end;   
	stop ->
	   %%io:format("EnvP:\n~p\n", [Env]),
	    ok
    end.


start_counter_process() ->
     spawn_link(refac_add_a_tag, counter, []).

counter() ->
     loop(1).

loop(N) ->
    receive
	{From, next} ->
	    From ! {self(), N},
	    loop(N+1);
	stop ->
	    ok
    end.

start_collector_process() ->
    register(send_collector, spawn(refac_add_a_tag, collector, [])).
    

collector() ->
    collector_loop([]).

collector_loop(S) ->
    receive
	{add, E} ->
	    collector_loop([E|S]);
	{get, From} ->
	    From ! {collector, S},
	    collector_loop(S);
	stop ->
	    ok
    end.
    
	    

remove_spawn_pars(Node) ->   
    refac_util:stop_tdTP(fun do_remove_spawn_pars/2, Node, []).

do_remove_spawn_pars(Node, []) ->
    case refac_syntax:type(Node) of 
	application ->
	    Operator = refac_syntax:application_operator(Node),
	    case refac_syntax:type(Operator) of
		atom ->
		    case refac_syntax:atom_value(Operator) of 
			spawn ->
			    Node1 = refac_syntax:application(Operator, []),
			    {Node1, true};
			_ -> {Node, false}
		    end;
		_ -> {Node, false}
	    end;
	_ -> {Node, false}
    end.
		    

    
   
reached_receive_funs([], _Callgraph, _ReceiveFuns, _Info) -> [];
reached_receive_funs(InitialFuns, CallGraph, ReceiveFuns, Info) ->
    {value, {module, ModName}} = lists:keysearch(module, 1, Info),
     Inscope_Funs = [{erlang, Fun, Arity} || {Fun, Arity} <- refac_util:auto_imported_bifs()] ++
		     refac_util:inscope_funs(Info),  %% NOTE: orders matters here.
     F2 = fun (T, S) ->
		 case refac_syntax:type(T) of
		   application ->
		       Operator = refac_syntax:application_operator(T),
		       Arguments = refac_syntax:application_arguments(T),
		       Arity = length(Arguments),
		       case refac_syntax:type(Operator) of
			 atom ->
			     Op = refac_syntax:atom_value(Operator),
			     R = lists:filter(fun ({_M, F, A}) -> (F == Op) and (A == Arity) end, Inscope_Funs),
			     if R == [] ->
				    ordsets:add_element({unknown, Op, Arity},
							S);  %% Should we give an error message here?
				true ->
				    {M, Op, Arity} = hd(R),
				     ordsets:add_element({M, Op, Arity}, S)
				end;
			   module_qualifier ->
			       Mod = refac_syntax:module_qualifier_argument(Operator),
			       Body = refac_syntax:module_qualifier_body(Operator),
			       case {refac_syntax:type(Mod), refac_syntax:type(Body)} of
				   {atom, atom} ->
				       Mod1 = refac_syntax:atom_value(Mod),
				       Op = refac_syntax:atom_value(Body),
				       ordsets:add_element({Mod1, Op, Arity}, S);
				   _ -> S
			       end
			   end;
		     arity_qualifier ->
			 Fun = refac_syntax:arity_qualifier_body(T),
			 A = refac_syntax:arity_qualifier_argument(T),
			 case {refac_syntax:type(Fun), refac_syntax:type(A)} of
			     {atom, integer} ->
				 FunName = refac_syntax:atom_value(Fun),
				 Arity = refac_syntax:integer_value(A),
				 ordsets:add_element({ModName, FunName, Arity}, S);
			     _ -> S
			 end;
		     _ -> S
		 end
	  end,
    F  = fun(InitialFun) ->
		 case InitialFun of 
		     {Mod, Fun, Arity} -> [{Mod,Fun, Arity}];
		     {_Mod, _Fun, _Arity, _Index, FunExpr} ->
			 lists:usort(refac_syntax_lib:fold(F2, [], FunExpr))
		 end
	 end,		 
    ReachedFuns = lists:zip(InitialFuns, lists:map(fun(InitialFun) -> 
							   reached_funs(CallGraph, F(InitialFun)) end, 
						   InitialFuns)),

    FunsReachedFromInitials=lists:map(fun({Initial, Reached}) -> 
					      {Initial, lists:subtract(ReceiveFuns, lists:subtract(ReceiveFuns, Reached))} end, ReachedFuns),
    StandAloneReceivers  = lists:subtract(ReceiveFuns, lists:concat(lists:map(fun({_Initial, Receivers})->
										       Receivers end, FunsReachedFromInitials))),
    ReachedByStandAlones =  lists:zip(StandAloneReceivers, lists:map(fun(InitialFun) -> 
							   lists:usort([InitialFun |reached_funs(CallGraph, F(InitialFun))]) end, 
						   StandAloneReceivers)),
    ReachedByStandAlones1=lists:map(fun({Initial, Reached}) -> 
					      {Initial, lists:subtract(ReceiveFuns, lists:subtract(ReceiveFuns, Reached))} end, ReachedByStandAlones),
    FunsReachedFromInitials++ReachedByStandAlones1.


 
reached_funs(CallGraph, Acc) ->
    Res = lists:usort(lists:concat(lists:map(fun({Mod, Fun, Args}) ->
					 case lists:keysearch({Mod, Fun, Args}, 1, CallGraph) of 
					     {value, {{Mod, Fun, Args}, CalledFuns}} ->
						 CalledFuns;
					     _ ->[]
					 end
				 end, Acc))),
     case lists:usort(Res++Acc) == Acc of 
	true -> Res;
	_ -> reached_funs(CallGraph, lists:usort(Res++Acc)) 
    end.
		       
get_affected_initials(ReachInfo, {ModName, FunName, Arity}) ->
    Initials = lists:filter(fun({_Entry, Receivers}) ->
			 lists:member({ModName, FunName, Arity}, Receivers) end, ReachInfo),
    lists:map(fun(I) ->
		      case I of 
			 {{M, F, A, Index, _Def}, _Receivers} ->
			       {M, F, A, Index};
			  {{M, F, A}, _Receivers} -> 
			      {M, F, A}
		      end
	      end, Initials).
   
			

callgraph(AnnAST, Info, FileName) ->
    {AnnAST1,_} = remove_spawn_pars(AnnAST),
    CallGraph = build_call_graph(AnnAST1, Info, FileName),
    lists:map(fun({{Caller,_CallerDef}, Called})->
		      {Caller, Called} end, CallGraph).

    
build_call_graph(Node, Info, _FileName) ->
    {value, {module, ModName}} = lists:keysearch(module, 1, Info),
    Inscope_Funs = [{erlang, Fun, Arity} || {Fun, Arity} <- refac_util:auto_imported_bifs()] ++
		     refac_util:inscope_funs(Info),  %% NOTE: orders matters here.
    HandleSpecialFuns = fun (Arguments, S) ->
				case Arguments of
				  [F, A] ->
				      case {refac_syntax:type(F), refac_syntax:type(A)} of
					{atom, list} ->
					    FunName = refac_syntax:atom_value(F),
					    Arity = refac_syntax:list_length(A),
					    ordsets:add_element({ModName, FunName, Arity}, S);
					_ -> S
				      end;
				  [M, F, A] ->
				      case {refac_syntax:type(M), refac_syntax:type(F), refac_syntax:type(A)} of
					{atom, atom, list} ->
					    ModName = refac_syntax:atom_value(M),
					    FunName = refac_syntax:atom_value(F),
					    Arity = refac_syntax:list_length(A),
					    ordsets:add_element({ModName, FunName, Arity}, S);
					_ -> S
				      end;
				  [_N, M, F, A] ->
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
		       Operator = refac_syntax:application_operator(T),
		       Arguments = refac_syntax:application_arguments(T),
		       Arity = length(Arguments),
		       case refac_syntax:type(Operator) of
			 atom ->
			     Op = refac_syntax:atom_value(Operator),
			     R = lists:filter(fun ({_M, F, A}) -> (F == Op) and (A == Arity) end, Inscope_Funs),
			     if R == [] ->
				    ordsets:add_element({unknown, Op, Arity},
							S);  %% Should we give an error message here?
				true ->
				    {M, Op, Arity} = hd(R),
				    S1 = ordsets:add_element({M, Op, Arity}, S),
				    case {Op, Arity} of
				      {apply, 2} -> HandleSpecialFuns(Arguments, S1);
				      {apply, 3} -> HandleSpecialFuns(Arguments, S1);
				      _ -> S1
				    end
			     end;
			 module_qualifier ->
			     Mod = refac_syntax:module_qualifier_argument(Operator),
			     Body = refac_syntax:module_qualifier_body(Operator),
			     case {refac_syntax:type(Mod), refac_syntax:type(Body)} of
			       {atom, atom} ->
				   Mod1 = refac_syntax:atom_value(Mod),
				   Op = refac_syntax:atom_value(Body),
				   S1 = ordsets:add_element({Mod1, Op, Arity}, S),
				   case {Mod1, Op, Arity} of
				     {erlang, apply, 2} -> HandleSpecialFuns(Arguments, S1);
				     {erlang, apply, 3} -> HandleSpecialFuns(Arguments, S1);
				     _ -> S1
				   end;
			       _ -> S
			     end;
			 _ -> S
		       end;
		   arity_qualifier ->
		       Fun = refac_syntax:arity_qualifier_body(T),
		       A = refac_syntax:arity_qualifier_argument(T),
		       case {refac_syntax:type(Fun), refac_syntax:type(A)} of
			 {atom, integer} ->
			     FunName = refac_syntax:atom_value(Fun),
			     Arity = refac_syntax:integer_value(A),
			     ordsets:add_element({ModName, FunName, Arity}, S);
			 _ -> S
		       end;
		   _ -> S
		 end
	 end,
    F1 = fun (T, S) ->
		 case refac_syntax:type(T) of
		   function ->
		       FunName = refac_syntax:data(refac_syntax:function_name(T)),
		       Arity = refac_syntax:function_arity(T),
		       Caller = {{ModName, FunName, Arity}, T},
		       CalledFuns = lists:usort(refac_syntax_lib:fold(F2, [], T)),
		       ordsets:add_element({Caller, CalledFuns}, S);
		   _ -> S
		 end
	 end,
    lists:usort(refac_syntax_lib:fold(F1, [], Node)).


    
