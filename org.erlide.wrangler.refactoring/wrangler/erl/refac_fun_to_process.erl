%% =====================================================================
%% Refactoring: From function to process
%%
%% Copyright (C) 2006-2009  Huiqing Li, Simon Thompson


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
%% =====================================================================

-module(refac_fun_to_process).

-export([fun_to_process/6, fun_to_process_eclipse/6, fun_to_process_1/6, fun_to_process_1_eclipse/6]).

-include("../include/wrangler.hrl").


%% =====================================================================
%% @spec rename_var(FileName::filename(), Line::integer(), Col::integer(), NewName::string(),SearchPaths::[string()])-> term()
%%
-spec(fun_to_process/6::(filename(), integer(), integer(), string(), [dir()], integer()) -> {ok, [filename()]} |{undecidables, string()}| {error, string()}).	     
fun_to_process(FName, Line, Col, ProcessName, SearchPaths, TabWidth) ->
    fun_to_process(FName, Line, Col, ProcessName, SearchPaths, TabWidth, emacs).


-spec(fun_to_process_1/6::(filename(), integer(), integer(), string(), [dir()], integer()) -> {ok, [filename()]}).	     
fun_to_process_1(FName, Line, Col, ProcessName, SearchPaths, TabWidth) ->
    fun_to_process_1(FName, Line, Col, ProcessName, SearchPaths, TabWidth, emacs).

-spec(fun_to_process_eclipse/6::(filename(), integer(), integer(), string(), [dir()], integer()) -> {ok, [{filename(), filename(), string()}]} | 
											     {undecidables, string()} | {error, string()}).
fun_to_process_eclipse(FName, Line, Col, ProcessName, SearchPaths, TabWidth) ->
    fun_to_process(FName, Line, Col, ProcessName, SearchPaths, TabWidth, eclipse).

fun_to_process(FName, Line, Col, ProcessName, SearchPaths, TabWidth, Editor) ->
    ?wrangler_io("\nCMD: ~p:fun_to_process(~p, ~p, ~p, ~p,~p, ~p)\n",  [?MODULE, FName,  Line, Col, ProcessName, SearchPaths, TabWidth]),
    case is_process_name(ProcessName) of
	true ->
	    _Res =refac_annotate_pid:ann_pid_info(SearchPaths, TabWidth),
	    {ok, {AnnAST,Info}}= refac_util:parse_annotate_file(FName, true, SearchPaths, TabWidth), 
	    {value, {module, ModName}} = lists:keysearch(module, 1, Info),
	    ProcessName1 = list_to_atom(ProcessName), 
	    case refac_util:pos_to_fun_name(AnnAST, {Line,Col}) of
		{ok, {Mod, FunName, Arity, _, DefinePos}} ->
		    if Mod == ModName -> 
			    case pre_cond_check(AnnAST, {Line, Col}, ModName,FunName,Arity, ProcessName1, SearchPaths) of 
				ok -> AnnAST2 = do_fun_to_process(AnnAST, Info, ModName, DefinePos, FunName, Arity, ProcessName1),
				     case refac_util:is_exported({FunName, Arity}, Info) of
					 true ->
					     ?wrangler_io("\nChecking client modules in the following search paths: \n~p\n",
						       [SearchPaths]),
					     ClientFiles = refac_util:get_client_files(FName, SearchPaths),
					     Results = fun_to_process_in_client_modules(ClientFiles, ModName, FunName, Arity, ProcessName1, SearchPaths),
					     case Editor of 
						 emacs ->
						     refac_util:write_refactored_files([{{FName, FName}, AnnAST2} | Results]),
						     ChangedClientFiles = lists:map(fun ({{F, _F}, _AST}) -> F end, Results),
						     ChangedFiles = [FName | ChangedClientFiles],
						     ?wrangler_io("The following files have been changed by this refactoring:\n~p\n",
							       [ChangedFiles]),
						     {ok, ChangedFiles};
						 eclipse ->
						     Results1 = [{{FName, FName}, AnnAST2} | Results],
						     Res = lists:map(fun({{FName1, NewFName1}, AST}) ->
									     {FName1, NewFName1, refac_prettypr:print_ast(refac_util:file_format(FName1),AST)} end, Results1),
						     {ok, Res}
					     end;
					 false ->
					     case Editor of 
						 emacs ->
						     refac_util:write_refactored_files([{{FName, FName}, AnnAST2}]), {ok, [FName]};
	 					 eclipse ->
						     Res = [{FName, FName, refac_prettypr:print_ast(refac_util:file_format(FName),AnnAST2)}],
						     {ok, Res}
					     end
				     end;
				{error, Reason}-> {error, Reason};
				undecidables -> {undecidables, "There are undeciable cases."}
				end;
		       true ->
			    {error, "This function is not defined in this module; please initiate this refactoring from the  module where it is defined."}
		    end;
		{error, Reason} -> {error, Reason}
	    end;
	false -> {error, "Invalid process name."}
    end.

-spec(fun_to_process_1_eclipse/6::(filename(), integer(), integer(), string(), [dir()], integer()) -> {ok, [{filename(), filename(), string()}]}).
fun_to_process_1_eclipse(FName, Line, Col, ProcessName, SearchPaths, TabWidth) ->
    fun_to_process_1(FName, Line, Col, ProcessName, SearchPaths, TabWidth, eclipse).

fun_to_process_1(FName, Line, Col, ProcessName, SearchPaths, TabWidth, Editor) ->
    {ok, {AnnAST,Info}}= refac_util:parse_annotate_file(FName, true, SearchPaths, TabWidth), 
    {value, {module, ModName}} = lists:keysearch(module, 1, Info),
    ProcessName1 = list_to_atom(ProcessName), 
    {ok, {ModName, FunName, Arity, _, DefinePos}}=refac_util:pos_to_fun_name(AnnAST, {Line,Col}),
    AnnAST2 = do_fun_to_process(AnnAST, Info, ModName, DefinePos, FunName, Arity, ProcessName1),
    case refac_util:is_exported({FunName, Arity}, Info) of
	true ->
	    ?wrangler_io("\nChecking client modules in the following search paths: \n~p\n",
		      [SearchPaths]),
	    ClientFiles = refac_util:get_client_files(FName, SearchPaths),
	    Results = fun_to_process_in_client_modules(ClientFiles, ModName, FunName, Arity, ProcessName1, SearchPaths),
	    case Editor of 
		emacs ->
		    refac_util:write_refactored_files([{{FName, FName}, AnnAST2} | Results]),
		    ChangedClientFiles = lists:map(fun ({{F, _F}, _AST}) -> F end, Results),
		    ChangedFiles = [FName | ChangedClientFiles],
		    ?wrangler_io("The following files have been changed by this refactoring:\n~p\n",
			      [ChangedFiles]),
		    {ok, ChangedFiles};
		eclipse ->
		    Results1 = [{{FName, FName}, AnnAST2} | Results],
		    Res = lists:map(fun({{FName1, NewFName1}, AST}) ->
					    {FName1, NewFName1, refac_prettypr:print_ast(refac_util:file_format(FName1),AST)} end, Results1),
		    {ok, Res}
	    end;
	false ->
	    case Editor of 
		emacs ->
		    refac_util:write_refactored_files([{{FName, FName}, AnnAST2}]), 
		    {ok, [FName]};		     
		eclipse ->
		    Res = [{FName, FName, refac_prettypr:print_ast(refac_util:file_format(FName),AnnAST2)}],
		    {ok, Res}
	    end
    end.


%% Side conditios:
%% 1. This process name provided by the user should be lexically legal, and not conflict with existing process names.
%%    The process name could also be generated by Wangler automatically when 'renaming process name' is supported.
%% 2. The function should not be a recursive function, either directly or indirectly.
%% 3. The function of functions called by this function should not register the Pid returned by self().
%%    (but registering a pid returned by a spawn expression is OK).
%% 4. The function or functions called by this function should not have receive expressions. (Is this correct?).
%% 
%% Wrangler generates the new function name and the rpc function name automatically, but the user could always rename it afterwards.
%% Support the original function is f/n, then the new function name would be f/0 and the rpc function name would be f_rpc/2; if 
%% any conflicts occur, '_i' will be attached to the end of the function name where i is a smallest number that make the name fresh.
%% 
pre_cond_check(AnnAST, Pos, ModName,FunName,Arity, ProcessName, SearchPaths)->
    {ok, FunDef} = refac_util:pos_to_fun_def(AnnAST, Pos),    
    case is_recursive_fun({ModName, FunName, Arity, FunDef}, SearchPaths) of  
	true ->
	    {error, "The function is a recursive (direct or indirect) function.\n"};
	false -> {SelfApps, _Pids, {PNames, UnKnowns}} = collect_registration_and_self_apps(SearchPaths), 
		 case lists:member(ProcessName, PNames) of 
		     true -> {error, "The process name provided is already in use, please choose another name."};
		     _ ->
			 SelfRes = check_self_exprs(SelfApps, {ModName,FunName, Arity}, SearchPaths),
			 case UnKnowns of 
			     [] -> case SelfRes of 
				       ok -> ok;
				       _ -> ?wrangler_io("\n*************************************Warning****************************************\n",[]),
					    ?wrangler_io("The value returned by 'self()', which is used at the location(s) listed below, will be changed "
						      " by this refactoring, and this could possibly change the behaviour of the program!\n",[]),
					    lists:foreach(fun({{Mod, Fun, Ari}, SelfExpr, _}) ->
								  {{Line,_}, _} = refac_util:get_range(SelfExpr),
								  ?wrangler_io("Location: moldule: ~p, function: ~p/~p, line: ~p\n", [Mod, Fun, Ari, Line])
							  end, SelfRes),
					   undecidables
				   end;
			     _ -> ?wrangler_io("\n*************************************Warning****************************************\n",[]),
				 ?wrangler_io("Wrangler could not decide whether the process name provided conflicts with the process name(s) "
					    "used by the following registeration expression(s):\n",[]),
				  UnKnowns1 = lists:map(fun({_, V}) -> V end, UnKnowns),
				  lists:foreach(fun({M, F,A, {L,_}}) -> ?wrangler_io("Location: module: ~p, function:~p/~p, line:~p\n", [M, F, A, L])
						end, UnKnowns1),
				  case SelfRes of 
				      ok -> ok;
				      _ -> ?wrangler_io("\n*************************************Warning****************************************\n",[]),
					   ?wrangler_io("The value returned by 'self()', which is used at the location(s) listed below, will be changed "
						     " by this refactoring, and this could possibly change the behaviour of the program!\n",[]),
					   lists:foreach(fun({{Mod, Fun, Ari}, SelfExpr, _}) ->
								 {{Line,_}, _} = refac_util:get_range(SelfExpr),
								 ?wrangler_io("Location: moldule: ~p, function: ~p/~p, line: ~p\n", [Mod, Fun, Ari, Line])
							 end, SelfRes)
				  end,
				  undecidables
			 end
		 end
    end.
		 
	    


%% This refactoring changes the value returned by those 'self()' which are reachable from the function under consideration,
%% and there is a possiblity that this will change the program's behaviour when, for example, the value returned by 'self()' is 
%% used as part of a message sent/received between processes.
check_self_exprs([], _, _SearchPaths) ->
    ok;
check_self_exprs(SelfApps, InitialFun={_ModName, _FunName, _Arity},SearchPaths) ->
    Files = refac_util:expand_files(SearchPaths, ".erl"),
    CallGraph= wrangler_callgraph_server:get_callgraph(SearchPaths), 
    CallerCallee = CallGraph#callgraph.callercallee,
    ReachedFuns = [InitialFun|reached_funs_1(CallerCallee, [InitialFun])],
    SelfApps1 = lists:filter(fun({{M, F, A}, {_File, _FunDef, _SelfExpr}}) ->
				     lists:member({M,F, A}, ReachedFuns)
			     end, SelfApps),
    F1 = fun(Node, {Regs, Recs, Sends}) ->
		 case refac_syntax:type(Node) of 
		     application ->
			 case is_register_app(Node) of 
			     true ->
				 {[Node|Regs], Recs, Sends};
			     _ ->
				 {Regs, Recs, Sends}
			 end;
		     receive_expr ->
			 {Regs, [Node|Recs], Sends};
		     send_expr ->
			 {Regs, Recs, [Node|Sends]};
		     _ -> {Regs, Recs, Sends}
		 end
	 end,
    F = fun({{Mod, Fun, Arity}, {File, FunDef, SelfExpr}}) ->
		{ok, {AnnAST, _Info}} = refac_util:parse_annotate_file(File, true, SearchPaths),
		Res = refac_slice:forward_slice(Files, AnnAST, Mod, FunDef, SelfExpr),
	        Res1 = lists:map(fun({_,FunDef1}) -> FunDef1 end, Res),
		{RegAcc, RecAcc, SendAcc} = lists:unzip3(lists:map(fun(FunDef1) ->
									   refac_syntax_lib:fold(F1, {[],[],[]}, FunDef1) end, Res1)),
		{{Mod,Fun,Arity}, SelfExpr,{RegAcc, RecAcc, SendAcc}}
	end,
    Res = lists:filter(fun({_, _, {Regs, Recs, Sends}}) ->
			       (Regs=/=[]) or (Recs=/=[]) or (Sends =/= [])
		       end,
		       lists:map(F, SelfApps1)),
    case Res of 
	[] ->
	     ok;
	_ -> Res
    end.
		       
   
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

do_fun_to_process(AnnAST, Info,ModName, DefPos, FunName, Arity, ProcessName) ->
    InScopeFuns = lists:map(fun({_M, F, A}) ->
				    {F, A} end, refac_util:inscope_funs(Info)),
    RpcFunName = new_fun_name(atom_to_list(FunName)++ "_rpc", 2, 0, InScopeFuns),
    NewFunName = new_fun_name(atom_to_list(FunName), 0, 0, InScopeFuns--[{FunName, Arity}]),
     {AnnAST1,_}=refac_util:stop_tdTP(fun fun_call_to_rpc/2, AnnAST, {ModName,FunName, Arity, ProcessName, RpcFunName}),
    AnnAST2 = do_fun_to_process_1(AnnAST1, DefPos, ProcessName, NewFunName, RpcFunName),
    AnnAST2.

fun_call_to_rpc(Node, {ModName, FunName, Arity, ProcessName, RpcFunName}) ->
    Message = fun (Pos) -> ?wrangler_io("WARNING: function ***apply*** is used at location({line, col}):~p, and wrangler " 
				     "could not decide whether this site should be refactored, please check manually!\n",
				     [Pos])
	      end,
    F1 = fun(T, _Others) ->
		case refac_syntax:type(T) of
		    arity_qualifier -> 
			Fun = refac_syntax:arity_qualifier_body(T),
			FunName1 = refac_syntax:atom_value(Fun),
			Arg = refac_syntax:arity_qualifier_argument(T),
			Arg1 = refac_syntax:integer_value(Arg),
			DefMod = get_fun_def_mod(Fun),
			if (FunName1 == FunName) and (Arg1 == Arity) and (DefMod == ModName) ->
				 refac_syntax:copy_attrs(T,refac_syntax:arity_qualifier(refac_syntax:atom(RpcFunName), refac_syntax:integer(2)));			
			   true -> T
			end;
		    _ -> T
		end
	end,
    case refac_syntax:type(Node) of 
	attribute -> Node1=refac_util:full_buTP(F1, Node, {}),
		     {Node1, true};
	application ->
	    Op = refac_syntax:application_operator(Node),
	    case refac_syntax:type(Op) of 
		atom ->
		    Ann = refac_syntax:get_ann(Op),
		    Arguments = refac_syntax:application_arguments(Node),
		    {value, {fun_def, {Mod, F, A, _, _}}} = lists:keysearch(fun_def,1, Ann), 
		    case {Mod, F, A} of 
			{ModName, FunName, Arity} ->
			    Op1 =  refac_syntax:atom(RpcFunName),
			    Arg1 = refac_syntax:atom(ProcessName),
			    Arg2 =  refac_syntax:tuple(Arguments),			   			       
			    Node1 = refac_syntax:application(Op1, [Arg1, Arg2]),
			    {Node1, true};
			{erlang, apply, 2} ->
			    Arg1 = lists:nth(1, Arguments),
			    Arg2 = lists:nth(2, Arguments),
			    case refac_syntax:type(Arg1) of 
				implicit_fun ->
				    Name = refac_syntax:implicit_fun_name(Arg1),
				    B1 = refac_syntax:atom_value(refac_syntax:arity_qualifier_body(Name)),
				    A1 = refac_syntax:integer_value(refac_syntax:arity_qualifier_argument(Name)),
				    case {B1, A1} of 
					{FunName, Arity} ->
					    F2 = refac_syntax:implicit_fun(refac_syntax:atom(RpcFunName), refac_syntax:integer(2)),
					    P = refac_syntax:atom(ProcessName),
					    T2 = case refac_syntax:type(Arg2) of 
						     list -> refac_syntax:tuple(refac_syntax:list_elements(Arg2));
						     _  -> refac_syntax:application(refac_syntax:atom(list_to_tuple), [Arg2])						   
						 end,
					    T3 = refac_syntax:list([P, T2]),
					    {refac_syntax:copy_attrs(Node, refac_syntax:application
								     (Op, [F2,T3])), true};
					
				_ -> {Node, false}
				    end;
				_ -> {Node, false}
			    end;
			{erlang, apply, 3} ->
			    [Mod1,Fun1,Args1] = Arguments,
			    Mod2 = refac_util:try_evaluation([refac_syntax:revert(Mod1)]),  %% TODO: add backward slicing.
			    Fun2 = refac_util:try_evaluation([refac_syntax:revert(Fun1)]),
			    Pos = refac_syntax:get_pos(Node),
			    case Fun2 of 
				{value, FunName} ->
				    case Mod2 of 
					{value,ModName} ->
					    case refac_syntax:type(Args1) of 
						list ->
						    case refac_syntax:list_length(Args1) of 
							Arity ->
							    Op1 =  refac_syntax:atom(RpcFunName),
							    Arg1 = refac_syntax:atom(ProcessName),
							    Arg2 =  refac_syntax:tuple(refac_syntax:list_elements(Args1)),			   			       
							    Node1 = refac_syntax:application(Op1, [Arg1, Arg2]),
							    {Node1, true};
							_ -> {Node, true}
						    end;
						_ -> Message(Pos),
						     {Node, true}
					    end;
					{value, _}-> {Node, true};
					{error, _Reason} -> case refac_syntax:type(Args1) of 
								list -> case refac_syntax:list_length(Args1) of 
									    Arity -> Message(Pos),
										     {Node, true};
									    _ -> {Node, true}
									end;
								_ -> Message(Pos),
								     {Node, true}
							    end
				    end;
				{value, _} -> {Node, true};
				{error, _Reason} ->  
				    case Mod2 of 
					{value, ModName} ->
					    case refac_syntax:type(Args1) of 
						list -> case refac_syntax:list_length(Args1) of 
							    Arity -> Message(Pos),
								     {Node, true};
							    _ -> {Node, true}
							end;
						_ -> Message(Pos),
						     {Node, true}
					    end;
					{value, _} -> {Node, true};
					{error, _Reason} -> case refac_syntax:type(Args1) of 
								list-> case refac_syntax:list_length(Args1) of 
									   Arity -> Message(Pos),
										    {Node, true};
									   _ -> {Node, true}
								       end;
								_  -> Message(Pos),
								      {Node, true}
							    end
				    end
			    end;     
			_ ->
			    {Node, false}
		    end;
		_ -> {Node, false}
	    end;
	_ ->{Node, false}
    end.
	

new_fun_name(BaseName, Arity, Index, InScopeFuns) ->
    NewName  = case Index =< 0 of 
		   true -> BaseName;
		   _ -> BaseName++"_"++integer_to_list(Index)
	       end,		 
    case lists:member({list_to_atom(NewName), Arity}, InScopeFuns) of 
	true ->new_fun_name(BaseName, Arity, Index+1, InScopeFuns);
	_ -> list_to_atom(NewName)
    end.
	    
    
rpc_fun(NewFunName, RpcFunName) ->
    RpcFun=atom_to_list(RpcFunName)++"(RegName, Request) ->
                           Fun = fun() ->
                                    try register(RegName, self())
                                    catch 
                                         true ->
                                               "++atom_to_list(NewFunName)++"();
                                         error:_-> already_running
                                    end
                                 end,
                            spawn(Fun),
                            RegName ! {self(), Request},
		            receive
				  {RegName, Response} -> Response
			    end.",
    {ok, Toks, _} = refac_scan:string(RpcFun),
    {ok, Form} =erl_parse:parse_form(Toks),
    FunDef= hd(refac_syntax:form_list_elements(refac_recomment:recomment_forms([Form], []))),
    FunDef.


%% rpc_fun(NewFunName, RpcFunName) ->
%%      RpcFun=atom_to_list(RpcFunName)++"(RegName, Request) ->
%%  			   case whereis(RegName) of 
%%  				 undefined -> register(RegName, spawn_link(fun "++atom_to_list(NewFunName)++"/0));
%%  				 _ -> ok
%%  			   end,
%%  		           RegName ! {self(), Request},
%%  		           receive
%%  				 {RegName, Response} -> Response
%%  			   end.",
%%      {ok, Toks, _} = refac_scan:string(RpcFun),
%%      {ok, Form} =erl_parse:parse_form(Toks),
%%      FunDef= hd(refac_syntax:form_list_elements(refac_recomment:recomment_forms([Form], []))),
%%      FunDef.

do_fun_to_process_1(AnnAST, DefPos, ProcessName, NewFunName, RpcFunName) -> 				
    Forms = refac_syntax:form_list_elements(AnnAST),
    F = fun(Form) ->
 		case refac_syntax:type(Form) of 
 		    function -> case get_fun_def_loc(Form) of 
 				    DefPos -> 
 					[rpc_fun(NewFunName, RpcFunName), do_fun_to_process_2(Form, NewFunName, ProcessName)];
 				    _ -> [Form]
				end;
 		    _ -> [Form] 
 		end
 	end,		
    refac_syntax:form_list([T|| Form<-Forms, T <- F(Form)]).

do_fun_to_process_2(FunDef, NewFunName, ProcessName)->
    NewFunName1= refac_syntax:atom(NewFunName),
    Cs = refac_syntax:function_clauses(FunDef),
    Cs1 = lists:map(fun(C) -> Ps = refac_syntax:clause_patterns(C), 
			      Guard = refac_syntax:clause_guard(C),
			      Body = refac_syntax:clause_body(C),
			      LastE = lists:last(Body),
			      Msg = refac_syntax:tuple([refac_syntax:atom(ProcessName), LastE]),
			      Dest = refac_syntax:variable('From'),
			      SendExp = refac_syntax:infix_expr(Dest, refac_syntax:operator('!'), Msg),
			      RecExp = refac_syntax:application(NewFunName1, []),
			      Body1 = lists:reverse([RecExp,SendExp | tl(lists:reverse(Body))]),
			     %% Ps1 = refac_syntax:tuple(Ps),	
			      P = refac_syntax:tuple([refac_syntax:variable('From') |Ps]),
			      refac_syntax:clause([P], Guard, Body1)
		    end, Cs),					  
    ReceiveExp = refac_syntax:receive_expr(Cs1),
    C = refac_syntax:clause(none, [ReceiveExp]),		    
    NewFun = refac_syntax:function(NewFunName1, [C]),
    NewFun.

get_fun_def_loc(Node) ->
     As = refac_syntax:get_ann(Node),
     case lists:keysearch(fun_def, 1, As) of 
	  {value, {fun_def, {_M, _N, _A, _P, DefinePos}}} -> DefinePos;
	 _ -> false
     end.

fun_to_process_in_client_modules(Files, ModName, FunName, Arity, ProcessName, SearchPaths) ->
    case Files of 
	[] ->
	     [];
	[F | Fs] ->
	    ?wrangler_io("The current file under refactoring is:\n~p\n", [F]),
	    {ok, {AnnAST, _Info}} = refac_util:parse_annotate_file(F, true, SearchPaths),
	    {AnnAST1, Changed} = fun_to_process_in_client_modules_1(AnnAST, ModName, FunName, Arity, ProcessName),			  
	    if Changed ->
		    [{{F, F}, AnnAST1} | fun_to_process_in_client_modules(Fs, ModName,FunName, Arity, ProcessName, SearchPaths)];
	       true ->
		    fun_to_process_in_client_modules(Fs, ModName, FunName, Arity, ProcessName, SearchPaths)
	    end
	end.


fun_to_process_in_client_modules_1(AnnAST, ModName, FunName, Arity, ProcessName) ->
    RpcFunName = atom_to_list(FunName)++ "_rpc",
    refac_util:stop_tdTP(fun fun_call_to_rpc/2, AnnAST, {ModName, FunName, Arity, ProcessName, RpcFunName}).
  

   

get_fun_def_mod(Node) ->
    As = refac_syntax:get_ann(Node),
    case lists:keysearch(fun_def, 1, As) of
      {value, {fun_def, {M, _N, _A, _P, _DefinePos}}} -> M;
      _ -> false
    end.


		   
collect_registration_and_self_apps(DirList) ->
    Files = refac_util:expand_files(DirList, ".erl"),
    F = fun(File, FileAcc) ->
		{ok, {AnnAST, Info}} = refac_util:parse_annotate_file(File, true, DirList),
		{value, {module, ModName}} = lists:keysearch(module, 1, Info),
		F1 = fun(Node, ModAcc) ->
			     case refac_syntax:type(Node) of 
				 function ->
				     FunDef = Node,
				     FunName = refac_syntax:data(refac_syntax:function_name(Node)),
				     Arity = refac_syntax:function_arity(Node),
				     F2= fun (Node1, FunAcc) ->    
						 case refac_syntax:type(Node1) of 
						     application ->
							 case is_register_app(Node1) of 
							     true -> 
								 [RegName, Pid] = refac_syntax:application_arguments(Node1),
								 RegNameValues = evaluate_expr(Files, ModName, AnnAST, Node, RegName),
								 %%?wrangler_io("RegNameValue:\n~p\n", [RegNameValues]),
								 RegNameValues1 = lists:map(fun(R) -> {pname, R} end, RegNameValues),
								 [{pid, {{ModName, FunName, Arity}, Pid}}|RegNameValues1++FunAcc];
							     _ -> case is_self_app(Node1) of 
								      true -> [{self, {{ModName, FunName, Arity}, {File, FunDef, Node1}}}|FunAcc];
								      false -> FunAcc
								  end
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
    SelfApps = lists:flatmap(fun({P,A}) -> if P == self -> [A];
					     true -> [] 
					   end 
			    end, Acc), 
    {Names, UnKnowns} = lists:partition(fun({Tag,_V})-> Tag==value end, PNameAcc),
    %%?wrangler_io("NamesUnKnowns:\n~p\n", [{Names, UnKnowns}]),
    {SelfApps, PidAcc, {lists:usort(lists:map(fun({value, P}) -> P end, Names)), lists:usort(UnKnowns)}}.
    

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


is_recursive_fun({ModName, FunName, Arity, FunDef}, SearchPaths) ->
    case is_direct_recursive_fun(ModName, FunName, Arity, FunDef) of 
	true -> 
	    true;
	false ->
	    CallGraph = wrangler_callgraph_server:get_callgraph(SearchPaths),
	    Sccs1 =[[Fun||{Fun, _FunDef}<-Scc]||Scc<-CallGraph#callgraph.scc_order],
	    lists:any(fun(E)-> (length(E)>1) andalso (lists:member({ModName, FunName, Arity}, E)) end,
		      Sccs1)
    
    end.
	

is_direct_recursive_fun(ModName, FunName, Arity, FunDef) ->
    F = fun(Node, {Mod, Fun, Ari}) ->
		case refac_syntax:type(Node) of 
		    application ->
			Op = refac_syntax:application_operator(Node),
			case lists:keysearch(fun_def, 1, refac_syntax:get_ann(Op)) of 
			    {value, {fun_def, {Mod, Fun, Ari, _, _}}} ->
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
   
is_self_app(T) ->
     case refac_syntax:type(T) of
       application ->
 	  Operator = refac_syntax:application_operator(T),
 	  Ann = refac_syntax:get_ann(Operator),
 	  case lists:keysearch(fun_def, 1, Ann) of
 	    {value, {fun_def, {erlang, self, 0, _, _}}} -> true;
 	    _ -> false
 	  end;
       _ -> false
     end.

is_process_name(Name) ->
    refac_util:is_fun_name(Name) and (list_to_atom(Name) =/= undefined).
