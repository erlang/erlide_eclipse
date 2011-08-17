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
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMIATTED TO, PROCUREMENT OF 
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR 
%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF 
%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


%%---------------------------------------------------------------------------%%
%% Atoms in Erlang have multiple roles. An atom could be a module name, a function 
%% name, a process name, or just an atom literal. While in most cases, it is possible 
%% to infer the role played by an atom syntactically, in some cases it is not so obvious. 
%% This module provides functionalities that try to infer the role of an atom played using
%% some type inference techniques. We distinguish those atoms that representing modules names, 
%% function names, and process names. For an atom that represents a function name, we also 
%% try to infer the function's defining module and the function'a arity.


%%NOTE: THIS MODULE IS STILL UNDERDEVELOPEMENT!

-module(refac_type_annotation).

-export([type_ann_ast/5]).

-include("../include/wrangler.hrl").

type_ann_ast(FileName, Info, AnnAST, SearchPaths, TabWidth) ->
    case lists:keysearch(module, 1, Info) of
	false -> AnnAST; %% this should only happen for .hrl files.
	{value, {module, ModName}} ->
	    TestFrameWorkUsed = refac_util:test_framework_used(FileName),
	    Pid = start_type_env_process(),
	    Fs = refac_syntax:form_list_elements(AnnAST),
	    Funs = wrangler_callgraph_server:get_sorted_funs(ModName, AnnAST),
	    Funs1 = lists:map(fun ({{M, F, A}, FunDef}) ->
				      {{M, F, A}, do_type_ann(FileName, FunDef, TestFrameWorkUsed, SearchPaths, TabWidth, Pid)}
			      end, Funs),
	    Fs1 = lists:map(fun (Form) ->
				    case refac_syntax:type(Form) of
					function -> find_fun(Funs1, Form);
					_ -> Form
				    end
			    end, Fs),
	    AnnAST1 = refac_util:rewrite(AnnAST, refac_syntax:form_list(Fs1)),
	    case get_all_type_info(Pid) of
		[] ->
		    stop_type_env_process(Pid),
		    AnnAST1;
		TypeInfo ->
		    ?debug("Typeinfo:\n~p\n", [TypeInfo]),
		    stop_type_env_process(Pid),
		    prop_type_info(AnnAST1, TypeInfo)
	    end
    end.

find_fun(Funs, Form) ->
    Ann = refac_syntax:get_ann(Form),
    Pos = refac_syntax:get_pos(Form),
    {value, {fun_def, {M, F, A, _, _}}} = lists:keysearch(fun_def, 1, Ann),
    {value, {{M, F, A}, FunDef}} = lists:keysearch({M, F, A}, 1, Funs),
    Pos1 = refac_syntax:get_pos(FunDef),   
    case Pos==Pos1 of  
	true ->
	    FunDef;
	false ->
	    Form
    end.

do_type_ann(FileName, Form, TestFrameWorkUsed, SearchPaths, TabWidth, Pid) ->
    case refac_syntax:type(Form) of
	function ->
	    Ann = refac_syntax:get_ann(Form),
	    {value, {fun_def, {M, F, A, _, _}}} = lists:keysearch(fun_def, 1, Ann),
	    Name = refac_syntax:function_name(Form),
	    Name1 = refac_syntax:add_ann({type, {f_atom, [M, F, A]}}, Name),
	    Cs = [ast_traverse_api:full_buTP(fun do_type_ann_clause/2, C,
					     {FileName, refac_syntax:clause_patterns(C),
					      TestFrameWorkUsed, SearchPaths, TabWidth, Pid})
		  || C <- refac_syntax:function_clauses(Form)],
	    CsPats = [refac_syntax:clause_patterns(C) || C <- refac_syntax:function_clauses(Form)],
	    TypeInfo = get_all_type_info(Pid),
	    CsPatsTypes = [[get_pat_type(P, TypeInfo) || P <- CPats] || CPats <- CsPats],
	    ZippedCsPatsTypes = zip_list(CsPatsTypes),
	    PatTypes = [lists:usort(Ts) || Ts <- ZippedCsPatsTypes],
	    case lists:all(fun (T) -> length(T) == 1 end, PatTypes) andalso 
		   lists:any(fun (T) -> T /= [any] end, PatTypes)
		of
		true ->
		    Ts = {{M, F, A}, {[hd(PT) || PT <- PatTypes], any}},
		    add_to_type_env(Pid, [Ts]);
		_ -> ok
	    end,
	    refac_util:rewrite(Form, refac_syntax:function(Name1, Cs));
	_ ->
	    Form
    end.

do_type_ann_clause(Node, {FileName, Pats, TestFrameWorkUsed, SearchPaths, TabWidth, Pid}) ->
    case refac_syntax:type(Node) of
	application ->
	    Op = refac_syntax:application_operator(Node),
	    Args = refac_syntax:application_arguments(Node),
	    case lists:keysearch(fun_def, 1, refac_syntax:get_ann(Op)) of
		{value, {fun_def, {M, F, A, _, _}}} when M =/= '_' andalso F =/= '_' ->
		    Args1 = do_type_ann_args({M, F, A}, map_args(Pats, Args), Args, Pid),
		    Node1 = refac_util:rewrite(Node, refac_syntax:application(Op, Args1)),
		    do_type_ann_op(FileName, Node1, SearchPaths, TabWidth, Pid);
		_ -> %% Either module name or function name is not an atom;
		    do_type_ann_op(FileName, Node, SearchPaths, TabWidth, Pid)
	    end;
	tuple ->
	    case refac_syntax:tuple_elements(Node) of
		[E1, E2, E3, E4] ->
		    case refac_syntax:type(E1) == atom andalso refac_syntax:atom_value(E1) == call
								  andalso lists:member(eqc, TestFrameWorkUsed)
			of
			true ->
			    NewE2 = add_type_info({type, m_atom}, E2, Pid),
			    NewE3 = add_type_info({type, {f_atom, [try_eval(FileName, E2, SearchPaths, TabWidth, fun is_atom/1),
								   try_eval(FileName, E3, SearchPaths, TabWidth, fun is_atom/1),
								   try_eval_length(E4)]}}, E3, Pid),
			    refac_util:rewrite(Node, refac_syntax:tuple([E1, NewE2, NewE3, E4]));
			false ->
			    Node
		    end;
		_ -> Node
	    end;
	_ -> Node
    end.

do_type_ann_op(FileName, Node, SearchPaths, TabWidth, Pid) ->
    Op = refac_syntax:application_operator(Node),
    Args = refac_syntax:application_arguments(Node),
    Arity = length(Args),
    case refac_syntax:type(Op) of
	atom ->
	    As = refac_syntax:get_ann(Op),
	    {value, {fun_def, {Mod, FunName, Ari, _, _}}} = lists:keysearch(fun_def, 1, As),
	    Op1 = refac_syntax:add_ann({type, {f_atom, [Mod, FunName, Ari]}}, Op),
	    refac_util:rewrite(Node, refac_syntax:application(Op1, Args));
	variable ->
	    Op1 = add_type_info({type, {f_atom, ['_', try_eval(FileName, Op, SearchPaths, TabWidth,
							       fun is_atom/1), Arity]}}, Op, Pid),
	    refac_util:rewrite(Node, refac_syntax:application(Op1, Args));
	module_qualifier ->
	    M = refac_syntax:module_qualifier_argument(Op),
	    F = refac_syntax:module_qualifier_body(Op),
	    M1 = add_type_info({type, m_atom}, M, Pid),
	    F1 = add_type_info({type, {f_atom, [try_eval(FileName, M, SearchPaths, TabWidth, fun is_atom/1),
						try_eval(FileName, F, SearchPaths, TabWidth, fun is_atom/1), Arity]}}, F, Pid),
	    Op1 = refac_util:rewrite(Op, refac_syntax:module_qualifier(M1, F1)),
	    refac_util:rewrite(Node, refac_syntax:application(Op1, Args));
	tuple ->
	    case refac_syntax:tuple_elements(Op) of
		[M, F] ->
		    M1 = add_type_info({type, m_atom}, M, Pid),
		    F1 = add_type_info({type, {f_atom, [try_eval(FileName, M, SearchPaths, TabWidth, fun is_atom/1),
							try_eval(FileName, M, SearchPaths, TabWidth, fun is_atom/1), Arity]}}, F, Pid),
		    Op1 = refac_util:rewrite(Op, refac_syntax:tuple([M1, F1])),
		    refac_util:rewrite(Node, refac_syntax:application(Op1, Args));
		_ ->
		    Node
	    end;
	_ -> Node
    end.

do_type_ann_args({M, F, A}, MappedArgs, Args, Pid) ->
    case type(M, F, A) of 
	none -> 
	    case get_type_info(Pid, {M, F, A}) of 
		none -> Args;
		{ParTypes, _RtnTypes} ->
		    ?debug("do_type_ann_args:\n~p\n", [ParTypes]),
		    do_type_ann_args_1(ParTypes, MappedArgs, Args, Pid)
	    end;
	{ParTypes, _RtnType} ->
	    do_type_ann_args_1(ParTypes, MappedArgs, Args, Pid)	    
    end.

do_type_ann_args_1(ParTypes, MappedArgs, Args, Pid) ->
    ZippedParTypeArgs = lists:zip(ParTypes, Args),
    lists:map(fun ({ParType, Arg}) ->
		      ?debug("AT:\n~p\n", [{Arg, ParType}]),
		      case ParType of
			any ->
			    Arg;
			_ when is_function(ParType) ->
			      add_type_info({type, ParType(MappedArgs)}, Arg, Pid);
			{f_atom, [M, F, Arity]} ->
			      M1 = case is_function(M) of
				     true -> M(MappedArgs);
				     _ -> M
				 end,
			      F1 = case is_function(F) of
				     true ->
					   F(MappedArgs);
				     _ -> M
				 end,
			      Arity1 = case is_function(Arity) of
					 true ->
					   Arity(MappedArgs);
					 _ -> Arity
				     end,
			      add_type_info({type, {f_atom, [M1, F1, Arity1]}}, Arg, Pid);
			  _ ->
			      add_type_info({type, ParType}, Arg, Pid)
		      end
	      end, ZippedParTypeArgs).


add_type_info({type, Type}, Node, Pid) ->
    As =refac_syntax:get_ann(Node),
    Ps = [{Pos, {type, Type}}|| {value, {_,  Pos}} <- As],
    Ps1 =lists:append([[{Pos, {type, Type}}|| Pos<-Poss] ||{def, Poss} <-As]),
    add_to_type_env(Pid, Ps++Ps1),
    refac_syntax:add_ann({type, Type}, Node).
   

map_args(Pats, ActualArgs) ->
    Fun = fun (ActualArg) ->
		  case refac_syntax:type(ActualArg) of
		      literal ->
			  ActualArg;
		      variable ->
			  As = refac_syntax:get_ann(ActualArg),
			  case lists:keysearch(def, 1, As) of
			      {value, {def, DefinePos}} ->
				  {Ps1, _Ps2} = lists:splitwith(
					      fun (P) -> case refac_syntax:type(P) of
							     variable ->
								 As1 = refac_syntax:get_ann(P),
								 case lists:keysearch(def, 1, As1) of
								     {value, {def, DefinePos}} ->
									 false;
								     _ -> true
								 end;
							     _ -> true
							 end
					      end, Pats),
				  case length(Ps1) == length(Pats) of
				      true ->
					  ActualArg;
				      _ ->
					  ?debug("nth:\n~p\n", [length(Ps1) + 1]),
					  fun (P) -> lists:nth(length(Ps1) + 1, P) end
				  end;
			      _ ->
				  ActualArg
			  end;
		      _ -> ActualArg
		  end
	  end,
    Res = lists:map(Fun, ActualArgs),
    ?debug("Res:\n~p\n", [Res]),
    Res.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

get_pat_type(P, TypeInfo) ->
    case refac_syntax:type(P) of
	variable ->
	    As = refac_syntax:get_ann(P),
	    case lists:keysearch(def, 1, As) of
		{value, {def, [DefinePos]}} ->
		    case lists:keysearch(DefinePos, 1, TypeInfo) of
			{value, {DefinePos, {type, T}}} ->
			    T;
			_ -> any
		    end;
		_ -> any
	    end;
	_ -> any
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                       %%
%%                 Propagate Type Information to Atoms                   %%
%%                                                                       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prop_type_info(AnnAST, TypeEnv) ->
    element(1, ast_traverse_api:stop_tdTP(fun do_prop_type_info/2, AnnAST, TypeEnv)).
  
do_prop_type_info(Node, TypeEnv) ->
    case refac_syntax:type(Node) of 
	atom ->
	    Pos = refac_syntax:get_pos(Node),
	    Ts =lists:usort([Type||{P, Type} <-TypeEnv, P==Pos]),
	    case Ts of
		[] ->
		    {Node, true};
		_ ->
		    Type = element(2, lists:unzip(Ts)),
		    Node1 = refac_syntax:add_ann({type, Type}, Node),
		    {Node1, true}
	    end;	    
	_ -> {Node, false}
    end.
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                       %%
%%                 Type Info Server                                      %%
%%                                                                       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_type_env_process() ->
    spawn_link(fun() -> type_env_loop([]) end).
 
stop_type_env_process(Pid) ->
    Pid ! stop.


get_all_type_info(Pid) ->
    Pid ! {self(), get_all},
    receive 
	{Pid, Res} ->
	    Res
    end.

get_type_info(Pid, Key) ->    
    Pid ! {self(), get, Key},
    receive
	{Pid, Res} ->
	    Res
    end.
    
add_to_type_env(_Pid,[]) ->
    [];
add_to_type_env(Pid, PosTypes) ->
    Pid ! {add, PosTypes}.

type_env_loop(Env) ->
    receive
	{add, PosTypes} ->
	    Env1 = PosTypes++Env,
	    type_env_loop(Env1);
	{From, get, Key} ->
	    case lists:keysearch(Key,1, Env) of 
		{value, {Key, Type}} -> 
		    From ! {self(), Type};
		false ->
		    From ! {self(), none}
	    end,
	    type_env_loop(Env);
	{From, get_all} ->
	    From ! {self(), Env},
	    type_env_loop(Env);
	stop ->
	    ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                       %%
%%                 Utility Functions                                     %%
%%                                                                       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

try_eval(Expr, Cond) when is_function(Expr) ->
    fun (X) -> try_eval(Expr(X), Cond) end;
try_eval(Expr, Cond) ->
    try_eval(none, Expr, [], 8, Cond).

try_eval(FileName, E, SearchPaths, TabWidth, Cond) ->
    As = refac_syntax:get_ann(E),
    case lists:keysearch(value, 1, As) of
	{value, {value, {{_, V}, _DefPos}}} ->
	    case Cond(V) of
		true ->
		    ?debug("V:\n~p\n", [V]),
		    V;
		_ -> '_'
	    end;
	_ ->
	    case refac_util:try_eval(FileName, E, SearchPaths, TabWidth) of
		{value, V} ->
		    ?debug("V:\n~p\n", [V]),
		    case Cond(V) of
			true -> V;
			_ -> '_'
		    end;
		{error, _} -> '_'
	    end
    end.
		    
try_eval_length(Expr) when is_function(Expr) ->
    fun (X) -> try_eval_length(Expr(X)) end;
try_eval_length(Expr) ->
    E = refac_syntax:revert(mk_length_app(Expr)),
    try
      erl_eval:expr(E, [])
    of
      {value, V, _} ->
	  V
    catch
      _E1:_E2 ->
	  As = refac_syntax:get_ann(Expr),
	  ?debug("As:\n~p\n", [As]),
	  case lists:keysearch(value, 1, As) of
	    {value, {value, {{list, V}, _DefPos}}} ->
		V;
	    _ ->
		?debug("Expr:\n~p\n", [Expr]),
		case refac_syntax:type(Expr) of
		  list -> refac_syntax:list_length(Expr);
		  _ -> '_'
		end
	  end
    end.

mk_length_app(Expr) ->
    refac_syntax:set_pos(refac_syntax:application(
			   refac_syntax:set_pos(
			     refac_syntax:atom(length), {1, 1}), [Expr]), {1, 1}).


zip_list(ListOfLists) ->    
    zip_list_1(ListOfLists, []).
		      
zip_list_1([[]|_T], Acc)  ->
    Acc;
zip_list_1(ListOfLists, Acc)->      
    zip_list_1([tl(L) || L <-ListOfLists], Acc ++ [[hd(L)|| L  <- ListOfLists]]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                       %%
%%                 Hard Coded Type Info                                  %%
%%                                                                       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
type(erlang, apply, 3) ->
   mfa_type();
type(erlang, check_process_code, 2) ->
    {[any, m_atom], any};
type(erlang, delete_module, 1) ->
    {[m_atom], any};
type(erlang, function_exported, 3) ->
	    mfa_type();
type(erlang, hibernate, 3) ->
    mfa_type();
type(erlang, is_builtin, 3) ->
    mfa_type();
type(erlang, load_module, 2) ->
    {[m_atom, any],any};
type(erlang, module_loaded, 1) ->
    {[m_atom], any};
type(erlang, purge_module, 1) ->
    {[m_atom], any};
type(erlang, spawn, 3) ->
    mfa_type();
type(erlang, spawn, 4) ->
    {[any, m_atom, f_atom_type(), any], any};
type(erlang, spawn_link, 3) ->
    mfa_type();
type(erlang, spawn_link, 4) ->
     {[any, m_atom, f_atom_type(), any], any};
type(erlang, spawn_monitor, 3) -> 
    mfa_type();
type(erlang, spawn_opt, 4) ->
    {[any, m_atom, f_atom_type(), any, any], any};
type(erlang, spawn_opt, 5) ->
    {[any, m_atom, f_atom_type(), any, any], any};
type(erlang, whereis, 1) ->
    {[p_atom], any};

%% type(gen_server, start_link, 3) ->
%%     {[m_atom, any, any], any};
%% type(gen_server, start_link, 4) ->
%%     {[{atom, p_atom}, m_atom, any, any], any};
%% type(gen_server, start, 3) ->
%%     {[m_atom, any, any], any};
%% type(gen_server, start, 4) ->
%%     {[{atom, p_atom}, m_atom, any, any], any};
%% type(gen_server, call, 2) ->
%%     {[{any, any}], any};
%% type(gen_server, call, 3) ->
%%     {[any, any, any], any};

type(eqc, module, 1) ->
    {[m_atom], any};
type(eqc_c, start, 1) ->
    {[m_atom], any};
type(eqc_c, start, 2) ->
    {[m_atom, any], any};
type(eqc_ct, compile, 1) ->
     {[m_atom], any};
type(eqc_ct, compile, 2) ->
     {[m_atom, any], any};
type(eqc_ct, module, 1) ->
     {[m_atom], any};
%% type(eqc_ct, compile_mods, 1) ->
%%      {[m_atom], any};

type(eqc_statem, commands, 1) ->
    {[m_atom], any};
type(eqc_statem, commands,2)->
    {[m_atom, any], any};
type(eqc_statem, run_commands, 2) ->
    {[m_atom, any], any};
type(eqc_statem, run_commands,3) ->
    {[m_atom, any, any],any};
type(eqc_statem, postconditions, 3) ->
    {[m_atom, any, any], any};
type(eqc_fsm, analyze, 1) ->
    {[m_atom], any};
type(eqc_fsm, automate_weights, 1) ->
     {[m_atom], any};
type(eqc_fsm, commands, 1) ->
    {[m_atom], any};
type(eqc_fsm, commands, 2) ->
    {[m_atom, any], any};
type(eqc_fsm, dot, 1) ->
    {[m_atom], any};
type(eqc_fsm, run_commands, 2) ->
    {[m_atom, any], any};
type(eqc_fsm, run_commands,3) ->
    {[m_atom, any, any],any};
type(eqc_fsm, visualize, 1) ->
    {[m_atom], any};
type(eqc_fsm, visualize, 2) ->
    {[m_atom, any], any};
type(test_server, call_crash, 5)->
    {[any, any, m_atom, f_atom_type(), any], any};
type(_, _, _) -> 
    none.

mfa_type() ->
    {[m_atom, f_atom_type(), any],any}.

f_atom_type() ->
    fun(Args)->
	    [A1, A2, A3] = Args, 
	    {f_atom, [try_eval(A1, fun is_atom/1), 
		      try_eval(A2, fun is_atom/1),
		      try_eval_length(A3)]}
    end.
   

%% mod_name_as_pars_1() ->
%%     [{{erlang, apply, 3}, [modulenmae, functionname, arglist], term},
%%      {{erlang, spawn, 3}, [modulename, functionname, arglist], term},
%%      {{erlang, spawn_link, 3}, [modulename, functionname, arglist], term},
%%      {{erlang, spawn_opt, 4}, [modulename, functionname, arglist, term], term},
%%      {{eqc, module, 1}, [modulename], term},
%%      {{eqc_ct, compile, 1}, [modulename], term},
%%      {{eqc_ct, compile, 2}, [mdoulename, term], term},
%%      {{eqc_ct, module, 1}, [modulename], term},
%%      {{eqc_statem, commands, 1}, [modulename], term},
%%      {{eqc_statem, commands, 2}, [modulename, term], term},
%%      {{eqc_statem, postconditions, 3}, [modulename, term, term], term},
%%      {{eqc_statem, run_commands, 2}, [modulename, term], term},
%%      {{eqc_statem, run_commands, 3}, [modulename, term, term], term},
%%      {{test_server, timecall, 3}, [modulename, functionname, arglist], term},
%%      {{test_server, call_crash, 3}, [modulename, functionname, arglist], term},
%%      {{test_server, is_native, 1}, [modulename], term},
%%      {{test_server_ctrl, add_module, 1}, [modulename], term},
%%      {{test_server_ctrl, add_case, 2}, [modulename, functionname], term},
%%      {{test_server_ctrl, add_cases, 2}, [modulename, [functionname]], term},
%%      {{rs, r, 2}, [modulename, functionname], term},
%%      {{rs, r, 3}, [modulename, functionname, term], term}].

%% mod_name_as_pars_2() ->
%%     [ {{erlang, spawn, 4}, [node, modulename, functionname, arglist], term},
%%       {{erlang, spawn_link, 4}, [term, modulename, functioname, arglist], term},
%%       {{erlang, spawn_monitor, 3}, [term, modulename,functionname, arglist], term},			       
%%       {{eralng, spawn_opt, 4}, [term, modulename, functionname, arglist, term], term},
%%       {{test_server, do_times, 4}, [integer, modulename, functionname, arglist], term},
%%       {{test_server, call_crash, 4}, [term, modulename, functionname, arglist], term},
%%       {{test_server_ctrl, add_case, 3}, [term, modulename, functionname], term},
%%       {{test_server_ctrl, add_cases,3}, [term, modulename, [functionname]], term},
%%       {{ts, run, 2}, [term, modulename], term},
%%       {{ts, run, 3}, [term, modulename, functionname], term},
%%       {{ts, run, 4}, [term, modulename, functionname, term], term}].
      
%% mod_name_as_pars_3() ->
%%     [{{test_server, call_crash, 5}, [term, term, modulename, functionname, arglist], term}].
    
%% =====================================================================
%% @spec application_info(Tree::syntaxTree())->term()

%% Three ways that function name can be used in test data:
%% {modulename, functionname}
%% {modulename, functionname, arity}
%% {modulename, functionname, arglist}
%% {generator, modulename, functionname}
%% {call, modulename, functionname, arglist}
