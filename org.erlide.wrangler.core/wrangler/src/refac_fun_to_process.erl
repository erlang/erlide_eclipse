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
%% Refactoring: From function to process
%%
%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%%
%% =====================================================================

-module(refac_fun_to_process).

-export([fun_to_process/6, fun_to_process_eclipse/6, 
	 fun_to_process_1/7, fun_to_process_1_eclipse/6]).

-include("../include/wrangler.hrl").


%%-spec(fun_to_process/6::(filename(), integer(), integer(), string(), [dir()], integer()) 
%%      -> {ok, [filename()]}).      
fun_to_process(FName, Line, Col, ProcessName, SearchPaths, TabWidth) ->
    fun_to_process(FName, Line, Col, ProcessName, SearchPaths, TabWidth, emacs).


%%-spec(fun_to_process_1/7::(filename(), integer(), integer(), string(), [dir()], integer(), string()) 
%%      -> {ok, [filename()]}).      
fun_to_process_1(FName, Line, Col, ProcessName, SearchPaths, TabWidth, LogMsg) ->
    fun_to_process_1(FName, Line, Col, ProcessName, SearchPaths, TabWidth, emacs, LogMsg).

%%-spec(fun_to_process_eclipse/6::(filename(), integer(), integer(), string(), [dir()], integer()) -> 
%%	     {ok, [{filename(), filename(), string()}]}).
fun_to_process_eclipse(FName, Line, Col, ProcessName, SearchPaths, TabWidth) ->
    fun_to_process(FName, Line, Col, ProcessName, SearchPaths, TabWidth, eclipse).

fun_to_process(FName, Line, Col, ProcessName, SearchPaths, TabWidth, Editor) ->
    ?wrangler_io("\nCMD: ~p:fun_to_process(~p, ~p, ~p, ~p,~p, ~p).\n",
		 [?MODULE, FName, Line, Col, ProcessName, SearchPaths, TabWidth]),
    Cmd = "CMD: " ++ atom_to_list(?MODULE) ++ ":fun_to_process(" ++ "\"" ++
	    FName ++ "\", " ++ integer_to_list(Line) ++
	      ", " ++ integer_to_list(Col) ++ ", " ++ "\"" ++ ProcessName ++ "\","
		++ "[" ++ refac_misc:format_search_paths(SearchPaths) ++ "]," ++ integer_to_list(TabWidth) ++ ").",
    case is_process_name(ProcessName) of
      true -> ok;
      false -> throw({error, "Invalid process name."})
    end,
    _Res = refac_annotate_pid:ann_pid_info(SearchPaths, TabWidth),
    {ok, {AnnAST, Info}} = refac_util:parse_annotate_file(FName, true, SearchPaths, TabWidth),
    {value, {module, ModName}} = lists:keysearch(module, 1, Info),
    ProcessName1 = list_to_atom(ProcessName),
    case interface_api:pos_to_fun_def(AnnAST, {Line, Col}) of
      {ok, FunDef} ->
	  {value, {fun_def, {ModName, FunName, Arity, _Pos1, DefinePos}}} =
	      lists:keysearch(fun_def, 1, refac_syntax:get_ann(FunDef)),
	  pre_cond_check(AnnAST, {Line, Col}, ModName, FunName, Arity, ProcessName1, SearchPaths, TabWidth, Cmd),
	  AnnAST2 = do_fun_to_process(AnnAST, Info, DefinePos, FunName, Arity, ProcessName1),
	  case Editor of
	    emacs ->
		refac_util:write_refactored_files_for_preview([{{FName, FName}, AnnAST2}], TabWidth, Cmd),
		?wrangler_io("The following files are to be changed by this refactoring:\n~p\n",
			     [FName]),
		{ok, [FName]};
	    eclipse ->
		Content = refac_prettypr:print_ast(refac_util:file_format(FName), AnnAST2, TabWidth),
		Res = [{FName, FName, Content}],
		{ok, Res}
	  end;
      _ -> throw({error, "You have not selected a function definition, "
			 "or the function definition selected does not parse."})
    end.

%%-spec(fun_to_process_1_eclipse/6::(filename(), integer(), integer(), string(), [dir()], integer())
%%      -> {ok, [{filename(), filename(), string()}]}).
fun_to_process_1_eclipse(FName, Line, Col, ProcessName, SearchPaths, TabWidth) ->
    fun_to_process_1(FName, Line, Col, ProcessName, SearchPaths, TabWidth, eclipse, "").

fun_to_process_1(FName, Line, Col, ProcessName, SearchPaths, TabWidth, Editor, LogMsg) ->
    {ok, {AnnAST, Info}} = refac_util:parse_annotate_file(FName, true, SearchPaths, TabWidth),
    {value, {module, ModName}} = lists:keysearch(module, 1, Info),
    ProcessName1 = list_to_atom(ProcessName),
    {ok, FunDef} = interface_api:pos_to_fun_def(AnnAST, {Line, Col}),
    {value, {fun_def, {ModName, FunName, Arity, _Pos1, DefinePos}}} =
	lists:keysearch(fun_def, 1, refac_syntax:get_ann(FunDef)),
    AnnAST1 = do_fun_to_process(AnnAST, Info, DefinePos, FunName, Arity, ProcessName1),
    case Editor of
      emacs ->
	  Res = [{{FName, FName}, AnnAST1}],
	  refac_util:write_refactored_files_for_preview(Res, TabWidth, LogMsg),
	  {ok, [FName]};
      eclipse ->
	  Content = refac_prettypr:print_ast(refac_util:file_format(FName), AnnAST1, TabWidth),
	  Res = [{FName, FName, Content}],
	  {ok, Res}
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
pre_cond_check(AnnAST, Pos, ModName, FunName, Arity, ProcessName, SearchPaths, TabWidth, Cmd) ->
    {ok, FunDef} = interface_api:pos_to_fun_def(AnnAST, Pos),
    case is_recursive_fun({ModName, FunName, Arity, FunDef}, SearchPaths) of
      true ->
	  throw({error, "The function is a recursive (direct or indirect) function.\n"});
      false -> ok
    end,
    {SelfApps, _Pids, PNames, UnKnowns} = collect_registration_and_self_apps(SearchPaths, TabWidth),
    case lists:member(ProcessName, PNames) of
      true -> throw({error, "The process name provided is already in use, please choose another name."});
      _ -> ok
    end,
    SelfRes = check_self_exprs(SelfApps, {ModName, FunName, Arity}, SearchPaths),
    case {UnKnowns, SelfRes} of
      {[], []} -> ok;
      {[], _} ->
	  ?wrangler_io("\n*************************************Warning****************************************\n", []),
	  ?wrangler_io("The value returned by 'self()', which is used at the location(s) listed below, will be changed "
		       " by this refactoring, and this could possibly change the behaviour of the program!\n", []),
	  lists:foreach(fun ({{File, _Fun, _Ari}, SelfExpr, _}) ->
				{{Line, _}, _} = refac_misc:get_start_end_loc(SelfExpr),
				Msg = File ++ io_lib:format(":~p: \n", [Line]),
				?wrangler_io(Msg, [])
			end, SelfRes),
	  throw({undecidables, "there are undecidable cases.", Cmd});
      {_, []} ->
	  ?wrangler_io("\n*************************************Warning****************************************\n", []),
	  ?wrangler_io("Wrangler could not decide whether the process name provided conflicts with the process name(s) "
		       "used by the following registeration expression(s):\n", []),
	  lists:foreach(fun ({File, L}) ->
				Msg = File ++ io_lib:format(":~p: \n", [L]),
				?wrangler_io(Msg, [])
			end, UnKnowns),
	  throw({undecidables, "there are undecidable cases.", Cmd});
      _ ->
	  ?wrangler_io("\n*************************************Warning****************************************\n", []),
	  ?wrangler_io("Wrangler could not decide whether the process name provided conflicts with the process name(s) "
		       "used by the following registeration expression(s):\n", []),
	  lists:foreach(fun ({File, L}) ->
				Msg = File ++ io_lib:format(":~p: \n", [L]),
				?wrangler_io(Msg, [])
			end, UnKnowns),
	  ?wrangler_io("\n*************************************Warning****************************************\n", []),
	  ?wrangler_io("The value returned by 'self()', which is used at the location(s) listed below, will be changed "
		       " by this refactoring, and this could possibly change the behaviour of the program!\n", []),
	  lists:foreach(fun ({{File, _Fun, _Ari}, SelfExpr, _}) ->
				{{Line, _}, _} = refac_misc:get_start_end_loc(SelfExpr),
				Msg = File ++ io_lib:format(":~p: \n", [Line]),
				?wrangler_io(Msg, [])
			end, SelfRes),
	  throw({undecidables, "there are undecidable cases.", Cmd})
    end.
  
%% This refactoring changes the value returned by those 'self()' which are reachable from the function under consideration,
%% and there is a possiblity that this will change the program's behaviour when, for example, the value returned by 'self()' is 
%% used as part of a message sent/received between processes.
check_self_exprs([], _, _SearchPaths) ->
    [];
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
	        Res1 = [FunDef1||{_,FunDef1}<-Res],
		{RegAcc, RecAcc, SendAcc} = lists:unzip3(
					      lists:map(fun(FunDef1) ->
								refac_syntax_lib:fold(F1, {[],[],[]}, FunDef1)
							end,Res1)),
		{{Mod,Fun,Arity}, SelfExpr,{RegAcc, RecAcc, SendAcc}}
	end,
    lists:filter(fun({_, _, {Regs, Recs, Sends}}) ->
			(Regs=/=[]) or (Recs=/=[]) or (Sends =/= [])
		end,
		lists:map(F, SelfApps1)).
   		       
   
reached_funs_1(CallerCallee, Acc) ->
    Res = lists:flatmap(fun({Mod, Fun, Args}) ->
				case lists:keysearch({Mod, Fun, Args}, 1, CallerCallee) of 
				    {value, {{Mod, Fun, Args}, CalledFuns}} ->
					CalledFuns;
				    _ ->[]
				end
			end, Acc),
    case lists:usort(Res++Acc) == Acc of 
	true -> Res;
	_ ->
	    reached_funs_1(CallerCallee, lists:usort(Res++Acc)) 
    end.

do_fun_to_process(AnnAST, Info, DefPos, FunName, Arity, ProcessName) ->
    InScopeFuns = [{F, A} || {_M, F, A} <- refac_misc:inscope_funs(Info)],
    RpcFunName = new_fun_name(atom_to_list(FunName) ++ "_rpc", 2, 0, InScopeFuns),
    NewFunName = new_fun_name(atom_to_list(FunName), 0, 0, InScopeFuns -- [{FunName, Arity}]),
    do_fun_to_process_1(AnnAST, DefPos, ProcessName, FunName, NewFunName, RpcFunName).
    

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
    %%NOTE: is looks like the following implementation still cause race conditions.
    RpcFun=atom_to_list(RpcFunName)++
	"(RegName, Request) ->
                Sender = self(),
                   Fun = fun() ->
                     try register(RegName, self()) of
                         true ->
                               Sender ! {started, self()},
                               "++atom_to_list(NewFunName)++"()
                      catch 
                         error:_ -> 
                              Sender ! {already_running, self()},
                              already_running
                       end
                    end,
                    Pid = spawn(Fun),
                    receive 
                     {_, Pid} -> ok
                    end, 
                    RegName ! {self(), Request},
		    receive
		      {RegName, Response} -> Response
		 end.",
    {ok, Toks, _} = refac_scan:string(RpcFun),
    {ok, Form} =erl_parse:parse_form(Toks),
    hd(refac_syntax:form_list_elements(
	 refac_recomment:recomment_forms([Form], []))).
  

do_fun_to_process_1(AnnAST, DefPos, ProcessName, FunName, NewFunName, RpcFunName) -> 				
    Forms = refac_syntax:form_list_elements(AnnAST),
    F = fun(Form) ->
 		case refac_syntax:type(Form) of 
 		    function -> 
			case get_fun_def_loc(Form) of 
			    DefPos -> 
				do_fun_to_process_2(Form, FunName, NewFunName, RpcFunName, ProcessName) ++
				    [rpc_fun(NewFunName, RpcFunName)];
			    _ -> [Form]
			end;
 		    _ -> [Form] 
 		end
 	end,		
    refac_syntax:form_list([T|| Form<-Forms, T <- F(Form)]).

do_fun_to_process_2(FunDef, FunName, NewFunName, RpcFunName, ProcessName)->
    NewFunName1= refac_syntax:atom(NewFunName),
    Cs = refac_syntax:function_clauses(FunDef),
    Cs1 = lists:map(
	    fun(C) -> 
		    Ps = refac_syntax:clause_patterns(C), 
		    Guard = refac_syntax:clause_guard(C),
		    Body = refac_syntax:clause_body(C),
		    LastE = lists:last(Body),
		    Msg = refac_syntax:tuple([refac_syntax:atom(ProcessName), LastE]),
		    Dest = refac_syntax:variable('From'),
		    SendExp = refac_syntax:infix_expr(Dest, refac_syntax:operator('!'), Msg),
		    RecExp = refac_syntax:application(NewFunName1, []),
		    Body1 = lists:reverse([RecExp,SendExp | tl(lists:reverse(Body))]),
		    P = refac_syntax:tuple([refac_syntax:variable('From'), refac_syntax:tuple(Ps)]),
		    refac_syntax:clause([P], Guard, Body1)
	    end, Cs),	
    Cs2 = lists:map(
	    fun(C) -> 
		    Ps = refac_syntax:clause_patterns(C), 
		    Guard = refac_syntax:clause_guard(C),
		    NewBody = refac_syntax:application(
				refac_syntax:atom(RpcFunName),
				[refac_syntax:atom(ProcessName),refac_syntax:tuple(Ps)]),
		    refac_syntax:clause(Ps, Guard, [NewBody])
	    end, Cs),
    ReceiveExp = refac_syntax:receive_expr(Cs1),
    C = refac_syntax:clause(none, [ReceiveExp]),		    
    [refac_syntax:function(refac_syntax:atom(FunName), Cs2),
     refac_syntax:function(NewFunName1, [C])].
 
  
collect_registration_and_self_apps(DirList, TabWidth) ->
    Files = refac_util:expand_files(DirList, ".erl"),
    F = fun(File, FileAcc) ->
		{ok, {AnnAST, Info}} = refac_util:parse_annotate_file(File, true, DirList),
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
								 RegNameValues = evaluate_expr(File, RegName, DirList, TabWidth),
								 RegNameValues1 = [{pname, R} || R<- RegNameValues],
								 [{pid, {{ModName, FunName, Arity}, Pid}}|RegNameValues1++FunAcc];
							    false ->
								 case is_self_app(Node1) of 
								     true -> 
									 [{self, {{File, FunName, Arity},
										  {File, Node, Node1}}}|FunAcc];
								     false ->
									 FunAcc
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
    PNameAcc  =[A||{pname, A}<- Acc],
    PidAcc = [A||{pid, A} <- Acc],
    SelfApps = [A||{self, A} <-Acc],
    Names = lists:usort([V||{value, V}<-PNameAcc]),
    UnKnowns = lists:usort([V||{unknown, V}<-PNameAcc]),
    {SelfApps, PidAcc, Names, UnKnowns}.
    

evaluate_expr(FileName, Expr, SearchPaths, TabWidth) ->
    Val = refac_misc:try_eval(FileName, Expr, SearchPaths, TabWidth),
    case Val of
      {value, V} -> [{value, V}];
      _ -> {{StartLine, _StartCol}, _} = refac_misc:get_start_end_loc(Expr),
	   [{unknown, {FileName, StartLine}}]
    end.
   

is_recursive_fun({ModName, FunName, Arity, FunDef}, SearchPaths) ->
    case is_direct_recursive_fun(ModName, FunName, Arity, FunDef) of
      true ->
	  true;
      false ->
	  CallGraph = wrangler_callgraph_server:get_callgraph(SearchPaths),
	  Sccs1 = [[Fun || {Fun, _FunDef} <- Scc] || Scc <- CallGraph#callgraph.scc_order],
	  lists:any(fun (E) -> length(E) > 1 andalso
				 lists:member({ModName, FunName, Arity}, E)
		    end,
		    Sccs1)
    end.
is_direct_recursive_fun(ModName, FunName, Arity, FunDef) ->
    F = fun (Node, {Mod, Fun, Ari}) ->
		case refac_syntax:type(Node) of
		  application ->
		      Op = refac_syntax:application_operator(Node),
		      case lists:keysearch(fun_def, 1, refac_syntax:get_ann(Op)) of
			{value, {fun_def, {Mod, Fun, Ari, _, _}}} ->
			    {true, true};
			_ -> {[], false}
		      end;
		  _ -> {[], false}
		end
	end,
    R = ast_traverse_api:once_tdTU(F, FunDef, {ModName, FunName, Arity}),
    element(2, R).
   

get_fun_def_loc(Node) ->
     As = refac_syntax:get_ann(Node),
     case lists:keysearch(fun_def, 1, As) of 
	  {value, {fun_def, {_M, _N, _A, _P, DefinePos}}} -> DefinePos;
	 _ -> false
     end.

is_register_app(T) ->
     case refac_syntax:type(T) of
       application ->
 	  Operator = refac_syntax:application_operator(T),
 	  Ann = refac_syntax:get_ann(Operator),
 	  case lists:keysearch(fun_def, 1, Ann) of
 	    {value, {fun_def, {erlang, register, 2, _, _}}} ->
		  true;
	      _ ->
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
		 _ ->
		     false
	     end;
	 _ -> false
     end.

is_process_name(Name) ->
    refac_misc:is_fun_name(Name) andalso
      list_to_atom(Name) =/= undefined.
