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

%% Roles of atom in Erlang program:
%% function name (f_atom) / module name (m_atom) / process name (p_atom)/ macro name (ma_atom} 
%% / record name (rn_atom) / record field name (rf_atom) / attribute name (attr_atom) /literal

%% For each atom node in the AST, if Wrangler could infer the role of that atom, then 
%% annotate it with a tuple of the format {type, {.._atom, ...}}/{type, .._atom}. 
%% For an atom node without a 'type' annotation, there is not guaranttee that it represents a 
%% literal; it only tells you that Wrangler cound not infer its role.


-module(refac_atom_annotation).

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
	    NewFuns = [do_type_ann(FileName, F, TestFrameWorkUsed, SearchPaths, TabWidth, Pid)
		       || F<-Funs],
	    NewFs = [update_a_form(F, NewFuns)|| F<-Fs],
	    AnnAST1 = refac_misc:rewrite(AnnAST, refac_syntax:form_list(NewFs)),
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

update_a_form(Form, SortedFuns) ->
    refac_io:format("Type:\n~p\n", [refac_syntax:type(Form)]),
    case refac_syntax:type(Form) of
      function -> find_fun(Form, SortedFuns);
      attribute -> do_atom_annotation_in_attr(Form);
      _ -> Form
    end.

do_atom_annotation_in_attr(Form) ->
    element(1, ast_traverse_api:full_tdTP(fun do_atom_annotation_in_attr/2, Form, {})).


do_atom_annotation_in_attr(Node, _Others) ->
    case refac_syntax:type(Node) of 
	attribute ->
	    Name = refac_syntax:attribute_name(Node), 
	    Name1 = refac_misc:update_ann(Name, {type, attr_atom}),
	    Args = refac_syntax:attribute_arguments(Node),
	    Args1=case refac_syntax:atom_value(Name) of
		      module ->
			  [refac_misc:update_ann(A, {type, m_atom})||A<-Args];
		      import ->
			  case Args of
			      [H| T] ->
				  case refac_syntax:type(H) of
				      atom ->
					  [refac_misc:update_ann(H, {type, m_atom})|T];
				      qualified_name ->
					  [refac_syntax:qualified_name([refac_misc:update_ann(A, {type, m_atom})
					   || A <- refac_syntax:qualified_name_segments(H)])|T]
				  end
			  end;
		      _ -> Args
		  end,
	    {refac_misc:rewrite(Node, refac_syntax:attribute(Name1, Args1)), true};
	atom ->
	    As = refac_syntax:get_ann(Node),
	    case lists:keysearch(type, 1, As) of
		{value, _} ->
		    {Node, true};
		false ->
		    case lists:keysearch(fun_def, 1, As) of
			{value, {fun_def, {Mod, FunName, Ari, _, _}}} ->
			    {refac_misc:update_ann(Node, {type, {f_atom, [Mod, FunName, Ari]}}), true};
			false ->
			    {Node, false}
		end
	    end;
	_ ->
	    {Node, false}
    end.
			 

find_fun(Form, Funs) ->
    Ann = refac_syntax:get_ann(Form),
    refac_io:format("Form:\n~p\n", [Form]),
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


do_type_ann(FileName, {{M, F, A}, Form}, TestFrameWorkUsed, SearchPaths, TabWidth, Pid) ->
    case refac_syntax:type(Form) of
      function ->
	  Name = refac_syntax:function_name(Form),
	  Name1 = refac_syntax:add_ann({type, {f_atom, [M, F, A]}}, Name),
	  Cs = [do_atom_annotation(FileName, C, TestFrameWorkUsed, SearchPaths, TabWidth, Pid)
		|| C <- refac_syntax:function_clauses(Form)],
	  CsPats = [refac_syntax:clause_patterns(C) || C <- refac_syntax:function_clauses(Form)],
	  TypeInfo = get_all_type_info(Pid),
	  CsPatsTypes = [[get_pat_type(P, TypeInfo) || P <- CPats] || CPats <- CsPats],
	  ZippedCsPatsTypes = zip_list(CsPatsTypes),
	  PatTypes = [lists:usort(Ts) || Ts <- ZippedCsPatsTypes],
	  ?debug("MFA:\n~p\n", [{M, F, A}]),
	  ?debug("ParTypes:\n~p\n", [[hd(PT) || PT <- PatTypes]]),
	  case lists:all(fun (T) -> length(T) == 1
			 end, PatTypes) andalso
		 lists:any(fun (T) -> T /= [any] end, PatTypes)
	      of
	    true ->
		Ts = {{M, F, A}, {[hd(PT) || PT <- PatTypes], any}},
		add_to_type_env(Pid, [Ts]);
	    _ -> ok
	  end,
	  Form1 = refac_misc:rewrite(Form, refac_syntax:function(Name1, Cs)),
	  {{M, F, A}, Form1};
      _ ->
	  {{M, F, A}, Form}
    end.

do_atom_annotation(FileName, C, TestFrameWorkUsed, SearchPaths, TabWidth, Pid) ->
    {C1, _} = ast_traverse_api:full_tdTP(fun do_atom_annotation/2, C,
					 {FileName, refac_syntax:clause_patterns(C),
					  TestFrameWorkUsed, SearchPaths, TabWidth, Pid}),
    C1.

do_atom_annotation(Node, {FileName, Pats, TestFrameWorkUsed, SearchPaths, TabWidth, Pid}) ->
    case refac_syntax:type(Node) of
      attribute ->
	    refac_io:format("Name:\n~p\n", [Node]),
	    Name = refac_syntax:attribute_name(Node),
	    Args = refac_syntax:attribute_arguments(Node),
	    Name1 = refac_misc:update_ann(Name, {type,attr_atom}),
	    {refac_misc:rewrite(Node, refac_syntax:attribute(Name1, Args)), true};
      application ->
	  Op = refac_syntax:application_operator(Node),
	  Args = refac_syntax:application_arguments(Node),
	  Arity = length(Args),
	  Op1 = case refac_syntax:type(Op) of
		  variable ->
		      add_type_info({type, {f_atom, ['_', try_eval(FileName, Op, SearchPaths, TabWidth,
								   fun is_atom/1), Arity]}}, Op, Pid);
		  _ -> Op
		end,
	  Args = refac_syntax:application_arguments(Node),
	  case lists:keysearch(fun_def, 1, refac_syntax:get_ann(Op1))
	      of
	    {value, {fun_def, {M, F, A, _, _}}}
		when M =/= '_' andalso F =/= '_' ->
		Args1 = do_type_ann_args({M, F, A}, map_args(Pats, Args), Args, Pid),
		{refac_misc:rewrite(Node, refac_syntax:application(Op1, Args1)), true};
	    false ->
		{Node, false}
	  end;
      module_qualifier ->
	  Ann = refac_syntax:get_ann(Node),
	  {value, {fun_def, {_, _, Arity, _, _}}} = lists:keysearch(fun_def, 1, Ann),
	  Arg = refac_syntax:module_qualifier_argument(Node),
	  Body = refac_syntax:module_qualifier_body(Node),
	  Arg1 = add_type_info({type, m_atom}, Arg, Pid),
	  Body1 = add_type_info({type, {f_atom, [try_eval(FileName, Arg, SearchPaths, TabWidth, fun is_atom/1),
						 try_eval(FileName, Body, SearchPaths, TabWidth, fun is_atom/1), Arity]}}, Body, Pid),
	  {refac_misc:rewrite(Node, refac_syntax:module_qualifier(Arg1, Body1)), true};
      tuple ->
	  case refac_syntax:tuple_elements(Node) of
	    [E1, E2, E3, E4] ->
		case
		  refac_syntax:type(E1) == atom andalso refac_syntax:atom_value(E1) == call
		    andalso lists:member(eqc, TestFrameWorkUsed)
		    of
		  true ->
		      NewE2 = add_type_info({type, m_atom}, E2, Pid),
		      NewE3 = add_type_info({type, {f_atom, [try_eval(FileName, E2, SearchPaths, TabWidth, fun is_atom/1),
							     try_eval(FileName, E3, SearchPaths, TabWidth, fun is_atom/1),
							     try_eval_length(E4)]}}, E3, Pid),
		      {refac_misc:rewrite(Node, refac_syntax:tuple([E1, NewE2, NewE3, E4])), true};
		  false ->
		      {Node, false}
		end;
	    _ -> {Node, false}
	  end;
      infix_expr ->
	  Op = refac_syntax:infix_expr_operator(Node),
	  Left = refac_syntax:infix_expr_left(Node),
	  Right = refac_syntax:infix_expr_right(Node),
	  case refac_syntax:operator_name(Op) of
	    '!' ->
		case refac_syntax:type(Left) of
		  atom ->
		      Left1 = refac_misc:update_ann(Left, {type, p_atom}),
		      {refac_misc:rewrite(Node, refac_syntax:infix_expr(Left1, Op, Right)), true};
		  _ -> {Node, false}
		end;
	    _ -> {Node, false}
	  end;
      record_expr ->
	  Type = refac_syntax:record_expr_type(Node),
	  Type1 = add_type_info({type, rt_atom}, Type, Pid),
	  Arg = refac_syntax:record_expr_argument(Node),
	  Fields = refac_syntax:record_expr_fields(Node),
	  {refac_misc:rewrite(Node, refac_syntax:record_expr(Arg, Type1, Fields)), true};
      record_field ->
	  Name = refac_syntax:record_field_name(Node),
	  Name1 = add_type_info({type, rf_atom}, Name, Pid),
	  Value = refac_syntax:record_field_value(Node),
	  {refac_misc:rewrite(Node, refac_syntax:record_field(Name1, Value)), true};
      record_access ->
	  Arg = refac_syntax:record_access_argument(Node),
	  Type = refac_syntax:record_access_type(Node),
	  Field = refac_syntax:record_access_field(Node),
	  Type1 = add_type_info({type, rt_atom}, Type, Pid),
	  Field1 = add_type_info({type, rf_atom}, Field, Pid),
	  {refac_misc:rewite(Node, refac_syntax:record_access(Arg, Type1, Field1)), true};
      atom ->
	  As = refac_syntax:get_ann(Node),
	  case lists:keysearch(type, 1, As) of
	    {value, _} ->
		{Node, true};
	    false ->
		case lists:keysearch(fun_def, 1, As) of
		  {value, {fun_def, {Mod, FunName, Ari, _, _}}} ->
		      {refac_syntax:update_ann(Node, {type, {f_atom, [Mod, FunName, Ari]}}), true};
		  false ->
		      {Node, false}
		end
	  end;
      _ -> {Node, false}
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
			    ?debug("MappedArgS:\n~p\n", [MappedArgs]),
			    ?debug("TMappedArgs:\n~p\n", [ParType(MappedArgs)]),
			    add_type_info({type, ParType(MappedArgs)}, Arg, Pid);
			{f_atom, [M, F, Arity]} ->
			    ?debug("T:\n~p\n", [ParType]),
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
			    ?debug("M1:\n~p\n", [M1]),
			    ?debug("F1:\n~p\n", [F1]),
			    ?debug("Arity1:\n~p\n", [Arity1]),
			    add_type_info({type, {f_atom, [M1, F1, Arity1]}}, Arg, Pid);
			_ ->
			    ?debug("T:\n~p\n", [ParType]),
			    ?debug("A:\n~p\n", [Arg]),
			    add_type_info({type, ParType}, Arg, Pid)
		      end
	      end, ZippedParTypeArgs).


add_type_info({type, Type}, Node, Pid) ->
    As =refac_syntax:get_ann(Node),
    Ps = [{Pos, {type, Type}}|| {value, {_,  Pos}} <- As],
    Ps1 =lists:append([[{Pos, {type, Type}}|| Pos<-Poss] ||{def, Poss} <-As]),
    add_to_type_env(Pid, Ps++Ps1),
    refac_misc:update_ann(Node,{type, Type}).
   

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
	  case refac_misc:try_eval(FileName, E, SearchPaths, TabWidth) of
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
