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

%%@hidden
%%@private
-module(wrangler_annotate_ast).


-export([add_fun_define_locations/2]).

-import(wrangler_misc,[rewrite/2, update_ann/2]).

-include("../include/wrangler_internal.hrl").

%%-spec(add_fun_define_locations/2::(syntaxTree(), moduleInfo()) -> syntaxTree()).
add_fun_define_locations(AST, ModInfo) ->
    case lists:keysearch(module, 1, ModInfo) of
	{value, {module, ModName}} -> ModName;
	_ -> ModName = '_'
    end,
    Funs = fun (T, S) ->
		   case wrangler_syntax:type(T) of
		       function ->
			   FunName = wrangler_syntax:data(wrangler_syntax:function_name(T)),
			   Arity = wrangler_syntax:function_arity(T),
			   Pos = wrangler_syntax:get_pos(T),
			   ordsets:add_element({{ModName, FunName, Arity}, Pos}, S);
		       _ -> S
		   end
	   end,
    DefinedFuns = api_ast_traverse:fold(Funs, ordsets:new(), AST),
    ImportedFuns = case lists:keysearch(imports, 1, ModInfo) of
		       {value, {imports, I}} ->
			   lists:append([[{{M, F, A}, ?DEFAULT_LOC}
					  || {F, A} <- Fs] || {M, Fs} <- I]);
		       _ -> []
		   end,
    Fs = wrangler_syntax:form_list_elements(AST),
    Fs1 = [add_fun_def_info(F, ModName, DefinedFuns, ImportedFuns) || F <- Fs],
    rewrite(AST, wrangler_syntax:form_list(Fs1)).


add_fun_def_info(F, ModName, DefinedFuns, ImportedFuns) ->
    case wrangler_syntax:type(F) of
	attribute ->
	    Name = wrangler_syntax:attribute_name(F),
	    case wrangler_syntax:type(Name) of
		atom ->
		    case wrangler_syntax:atom_value(Name) of
	  		import ->
			    add_fun_def_info_in_import(F);
			export ->
		 	    add_fun_def_info_in_export(F, ModName, DefinedFuns);
			'spec' ->
			     add_fun_def_info_in_spec(F, ModName, DefinedFuns);
			_ ->
			    add_fun_def_info_in_form(F,ModName, DefinedFuns, ImportedFuns ) 
		    end;
		_ -> 
		    add_fun_def_info_in_form(F,ModName, DefinedFuns, ImportedFuns)
	    end;
	_ ->
	    add_fun_def_info_in_form(F, ModName,DefinedFuns, ImportedFuns)
    end.
 
add_fun_def_info_in_import(F) ->
    Name = wrangler_syntax:attribute_name(F),
    Args = wrangler_syntax:attribute_arguments(F),
    case Args of 
	[_M] -> F;
	[M, L] ->
	    case wrangler_syntax:type(M) of
		atom ->
		    M1 = wrangler_syntax:atom_value(M),
		    L1 = [add_fun_def_info_to_export_import_elems(E, M1,[]) ||
			      E <- wrangler_syntax:list_elements(L)],
		    rewrite(F, wrangler_syntax:attribute(Name, [M, rewrite(L, wrangler_syntax:list(L1))]));
		_ -> F
	    end;					    
	_ -> F
    end.

add_fun_def_info_in_export(F, ModName, DefinedFuns) ->
    Name = wrangler_syntax:attribute_name(F),
    Args = wrangler_syntax:attribute_arguments(F),
    case Args of
	[L] ->
	    L1 = [add_fun_def_info_to_export_import_elems(E, ModName, DefinedFuns) 
		  || E <- wrangler_syntax:list_elements(L)],
	    rewrite(F, wrangler_syntax:attribute(Name, [rewrite(L, wrangler_syntax:list(L1))]));
	_ -> F
    end.

add_fun_def_info_in_spec(Form, ModName, DefinedFuns) ->
    Name = wrangler_syntax:attribute_name(Form),
    Args = wrangler_syntax:attribute_arguments(Form),
    [H|T] = Args, 
    case wrangler_syntax:type(H) of
	tuple ->
	    case wrangler_syntax:tuple_elements(H) of
		[F, A] ->
		    case {wrangler_syntax:type(F), wrangler_syntax:type(A)} of
			{atom, integer} ->
			    FunName = wrangler_syntax:atom_value(F),
			    Arity = wrangler_syntax:integer_value(A),
			    Pos = wrangler_syntax:get_pos(F),
			    case lists:keysearch({ModName, FunName, Arity}, 1, DefinedFuns) of
				{value, {{ModName, FunName, Arity}, DefinePos}} ->
				    Ann ={fun_def, {ModName, FunName, Arity, Pos, DefinePos}};
				false ->
				    Ann = {fun_def, {ModName, FunName, Arity, Pos, ?DEFAULT_LOC}}
			    end,
			    F1 = update_ann(F, Ann),
			    H1 =rewrite(H, wrangler_syntax:tuple([F1,A])),
			    rewrite(Form, wrangler_syntax:attribute(Name, [H1|T]));
			_ -> Form
		    end;
		_ -> Form
	    end;
	_ -> Form
    end.

add_fun_def_info_to_export_import_elems(Node, DefMod, DefinedFuns) ->
    B = wrangler_syntax:arity_qualifier_body(Node),
    A = wrangler_syntax:arity_qualifier_argument(Node),
    Pos = wrangler_syntax:get_pos(B),
    FunName = case wrangler_syntax:type(B) of
		  atom -> wrangler_syntax:atom_value(B);
		  _ -> '_'
	      end,
    Arity = case wrangler_syntax:type(A) of
		integer -> wrangler_syntax:integer_value(A);
		_ -> '_'
	    end,
    case lists:keysearch({DefMod,FunName,Arity},1,DefinedFuns) of
	{value,{{DefMod,FunName,Arity},DefinePos}} ->
	    Ann = {fun_def,{DefMod,FunName,Arity,Pos,DefinePos}};
	false ->
	    Ann = {fun_def,{DefMod,FunName,Arity,Pos,?DEFAULT_LOC}}
    end,
    B1 = update_ann(B,Ann),
    update_ann(rewrite(Node,wrangler_syntax:arity_qualifier(B1,A)),Ann).
    




add_fun_def_info_in_form(F, ModName, DefinedFuns, ImportedFuns) ->
    {F1, _} = api_ast_traverse:full_tdTP(fun add_fun_def_info_1/2, F, {ModName, DefinedFuns, ImportedFuns}),
    F1.

								  
add_fun_def_info_1(T, {ModName, DefinedFuns, ImportedFuns}) ->
    T1 = add_fun_def_info_2(T, {ModName, DefinedFuns, ImportedFuns}),
    {T1, true}.

add_fun_def_info_2(T, {ModName, DefinedFuns, ImportedFuns}) ->
    DefineModLoc = 
	fun (Name, Arity) ->
		Fs = ordsets:to_list(ordsets:union(DefinedFuns, ImportedFuns)),
		Fs1 =[{{M, F, A}, Pos} || {{M,F,A},Pos}<-Fs, F==Name, A==Arity],
		case Fs1 of
		    [] -> 
			case erl_internal:bif(Name, Arity) of
			    true ->
				{erlang, ?DEFAULT_LOC};
			    false ->
				{'_', ?DEFAULT_LOC}
			end;
		    [{{M, _, _}, Pos}| _] -> {M, Pos}
		end
	end,
    case wrangler_syntax:type(T) of
	function -> 
	    add_fun_def_info_in_function(T, ModName);
	application ->
	    add_fun_def_info_in_apps(T, ModName, DefineModLoc);
	implicit_fun ->
	    add_fun_def_info_in_implicit_funs(T, ModName, DefineModLoc);
	_ -> T
    end.


add_fun_def_info_in_function(T, ModName) ->
    Name = wrangler_syntax:function_name(T),
    Fun_Name = wrangler_syntax:atom_value(Name),
    Arity = wrangler_syntax:function_arity(T),
    Pos = wrangler_syntax:get_pos(T),
    T1 = [begin  
	      CPos = wrangler_syntax:get_pos(C),
	      Ann = {fun_def, {ModName, Fun_Name, Arity, CPos, Pos}},
	      update_ann(C, Ann) 
	  end|| C <- wrangler_syntax:function_clauses(T)],
    Ann = {fun_def, {ModName, Fun_Name, Arity, Pos, Pos}},
    Name1 = update_ann(Name, Ann),
    T2 = rewrite(T, wrangler_syntax:function(Name1, T1)),
    update_ann(T2, Ann).


add_fun_def_info_in_apps(T, ModName, DefineModLoc) ->
    Op = wrangler_syntax:application_operator(T),
    Args = wrangler_syntax:application_arguments(T),
    Pos = wrangler_syntax:get_pos(Op),
    case wrangler_syntax:type(Op) of
	atom ->
	    OpVal = wrangler_syntax:atom_value(Op),
	    Arity = length(Args),
	    {DefMod, DefLoc} = DefineModLoc(OpVal, Arity),
	    Ann = {fun_def, {DefMod, OpVal, Arity, Pos, DefLoc}},
	    Op1 = update_ann(Op,Ann),
	    rewrite(T, wrangler_syntax:application(Op1, Args));
	module_qualifier ->
	    Mod = wrangler_syntax:module_qualifier_argument(Op),
	    Fun = wrangler_syntax:module_qualifier_body(Op),
	    {M, FunName, Arity} = get_mod_fun_arg_info(Mod, Fun, Args, ModName),
	    DefLoc = if M == ModName ->
			     element(2, DefineModLoc(FunName, Arity));
			true -> ?DEFAULT_LOC
		     end,
	    Ann ={fun_def, {M, FunName, Arity, Pos, DefLoc}},
	    Op1 = update_ann(Op, Ann),
	    rewrite(T, wrangler_syntax:application(Op1, Args));
	tuple ->
	    case wrangler_syntax:tuple_elements(Op) of
		[Mod, Fun] ->
		    {M, FunName, Arity} = get_mod_fun_arg_info(Mod, Fun, Args, ModName),
		    DefLoc = if M == ModName ->
				     element(2, DefineModLoc(FunName, Arity));
				true -> ?DEFAULT_LOC
			     end,
		    Ann = {fun_def, {M, FunName, Arity, Pos, DefLoc}},
		    Fun1 = update_ann(Fun, Ann),
		    Op1 = rewrite(Op, wrangler_syntax:tuple([Mod, Fun1])),
		    Op2 = update_ann(Op1, Ann),
		    rewrite(T, wrangler_syntax:application(Op2, Args));
		_ ->
		    Arity = length(Args),
		    Ann ={fun_def, {'_', '_', Arity, Pos, ?DEFAULT_LOC}},
		    Op1 = update_ann(Op, Ann),
		    rewrite(T, wrangler_syntax:application(Op1, Args))
	    end;
	implicit_fun ->
	    T;
	_ -> Arity = length(Args),
	     Ann ={fun_def, {'_', '_', Arity, Pos, ?DEFAULT_LOC}},
	     Op1 = update_ann(Op, Ann),
	     rewrite(T, wrangler_syntax:application(Op1, Args))
    end.


add_fun_def_info_in_implicit_funs(T, ModName, DefineModLoc) ->
    Name = wrangler_syntax:implicit_fun_name(T),
    case wrangler_syntax:type(Name) of
	module_qualifier ->
	    Mod = wrangler_syntax:module_qualifier_argument(Name),
	    Body = wrangler_syntax:module_qualifier_body(Name),
	    case wrangler_syntax:type(Body) of
		arity_qualifier ->
		    Fun = wrangler_syntax:arity_qualifier_body(Body),
		    Pos =wrangler_syntax:get_pos(Fun),
		    A = wrangler_syntax:arity_qualifier_argument(Body),
		    {DefMod, FunName, Arity} = get_mod_fun_arg_info(Mod, Fun, A, ModName),
		    DefLoc = if DefMod == ModName ->
				     element(2, DefineModLoc(FunName, Arity));
				true -> ?DEFAULT_LOC
			     end,
		    Ann = {fun_def, {DefMod, FunName, Arity, Pos, DefLoc}},
		    Fun1 = update_ann(Fun, Ann),
		    Body1 = update_ann(rewrite(Body, wrangler_syntax:arity_qualifier(Fun1, A)), Ann),
		    Name1=update_ann(rewrite(Name, wrangler_syntax:module_qualifier(Mod, Body1)), Ann),
		    update_ann(rewrite(T, wrangler_syntax:implicit_fun(Name1)), Ann);
		_ -> T
	    end;
	arity_qualifier ->
	    Fun = wrangler_syntax:arity_qualifier_body(Name),
	    A = wrangler_syntax:arity_qualifier_argument(Name),
	    FunName = wrangler_syntax:atom_value(Fun),
	    Arity = wrangler_syntax:integer_value(A),
	    Pos =wrangler_syntax:get_pos(Fun),
	    {DefMod, DefLoc} = DefineModLoc(FunName, Arity),
	    Ann ={fun_def, {DefMod, FunName, Arity, Pos, DefLoc}},
	    Fun1 = update_ann(Fun, Ann),
	    Name1 =update_ann(rewrite(Name, wrangler_syntax:arity_qualifier(Fun1, A)), Ann),
	    update_ann(rewrite(T, wrangler_syntax:implicit_fun(Name1)), Ann);
	_ -> T
    end.


get_mod_fun_arg_info(Mod, Fun, Args, CurModName) ->
    M = case wrangler_syntax:type(Mod) of
	  atom -> wrangler_syntax:atom_value(Mod);
	  macro -> MacroName = wrangler_syntax:macro_name(Mod),
		   case wrangler_syntax:type(MacroName) of
		     variable ->
			 case wrangler_syntax:variable_name(MacroName) of
			   'MODULE' -> CurModName;
			   _ -> '_'
			 end;
		     _ -> '_'
		   end;
	  _ -> '_'
	end,
    F = case wrangler_syntax:type(Fun) of
		 atom -> wrangler_syntax:atom_value(Fun);
		 _ -> '_'
	       end,
    A =case is_list(Args) of 
	   true -> length(Args);
	   _ -> case wrangler_syntax:type(Args) of
		    integer ->
			wrangler_syntax:integer_value(Args);
		    _ -> '_'
		end
       end,
    {M, F, A}.

