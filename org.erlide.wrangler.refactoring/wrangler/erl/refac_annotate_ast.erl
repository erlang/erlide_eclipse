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

-module(refac_annotate_ast).

-import(refac_util, [rewrite/2,update_ann/2]).
-export([add_fun_define_locations/2]).

-include("../include/wrangler.hrl").

add_fun_define_locations(AST, ModInfo) ->
    {value, {module, ModName}} = lists:keysearch(module, 1, ModInfo),
    Funs = fun (T, S) ->
		   case refac_syntax:type(T) of
		     function ->
			 FunName = refac_syntax:data(refac_syntax:function_name(T)),
			 Arity = refac_syntax:function_arity(T),
			 Pos = refac_syntax:get_pos(T),
			 ordsets:add_element({{ModName, FunName, Arity}, Pos}, S);
		     _ -> S
		   end
	   end,
    DefinedFuns = refac_syntax_lib:fold(Funs, ordsets:new(), AST),
    ImportedFuns = case lists:keysearch(imports, 1, ModInfo) of
		     {value, {imports, I}} ->
			 lists:append([[{{M, F, A}, ?DEFAULT_LOC}
					|| {F, A} <- Fs] || {M, Fs} <- I]);
		     _ -> []
		   end,
    Fs = refac_syntax:form_list_elements(AST),
    Fs1 = [add_fun_def_info(F, ModName, DefinedFuns, ImportedFuns) || F <- Fs],
    rewrite(AST, refac_syntax:form_list(Fs1)).


add_fun_def_info(F, ModName, DefinedFuns, ImportedFuns) ->
    case refac_syntax:type(F) of
	attribute ->
	    Name = refac_syntax:attribute_name(F),
	    case refac_syntax:type(Name) of
		atom ->
		    case refac_syntax:atom_value(Name) of
	  		import ->
			    add_fun_def_info_in_import(F);
			export ->
		 	    add_fun_def_info_in_export(F, ModName, DefinedFuns);
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
    Fun = fun(Node, DefMod) ->
		  B = refac_syntax:arity_qualifier_body(Node),
		  A = refac_syntax:arity_qualifier_argument(Node),
		  Pos = refac_syntax:get_pos(B),
		  FunName =case refac_syntax:type(B) of
			       atom -> refac_syntax:atom_value(B);
			       _ -> '_'
			   end,
		  Arity = case refac_syntax:type(A) of
			      integer -> refac_syntax:integer_value(A);
			      _ -> '_'
			  end,
		  Ann = {fun_def, {DefMod, FunName, Arity, Pos, ?DEFAULT_LOC}},
		  B1 = update_ann(B, Ann),
		  update_ann(rewrite(Node, refac_syntax:arity_qualifier(B1, A)), Ann)
	  end,
    Name = refac_syntax:attribute_name(F),
    Args = refac_syntax:attribute_arguments(F),
    case Args of 
	[_M] -> F;
	[M, L] ->
	    case refac_syntax:type(M) of
		atom ->
		    M1 = refac_syntax:atom_value(M),
		    L1 = [Fun(E, M1) || E<-refac_syntax:list_elements(L)],
		    rewrite(F, refac_syntax:attribute(Name, [M, rewrite(L, refac_syntax:list(L1))]));
		_ -> F
	    end;					    
	_ -> F
    end.
 
add_fun_def_info_in_export(F, ModName, DefinedFuns) ->
    Fun = fun(Node) ->
		  B = refac_syntax:arity_qualifier_body(Node),
		  A = refac_syntax:arity_qualifier_argument(Node),
		  Pos = refac_syntax:get_pos(B),
		  FunName =case refac_syntax:type(B) of
			       atom -> refac_syntax:atom_value(B);
			       _ -> '_'
			   end,
		  Arity = case refac_syntax:type(A) of
			      integer -> refac_syntax:integer_value(A);
			      _ -> '_'
			  end,
		  case lists:keysearch({ModName, FunName, Arity}, 1, DefinedFuns) of
		      {value, {{ModName, FunName, Arity}, DefinePos}} ->
			  Ann ={fun_def, {ModName, FunName, Arity, Pos, DefinePos}};
		      false ->
			  Ann = {fun_def, {ModName, FunName, Arity, Pos, ?DEFAULT_LOC}}
		  end,
		  B1 = update_ann(B, Ann),
		  update_ann(rewrite(Node, refac_syntax:arity_qualifier(B1, A)), Ann)
	  end, 
    Name = refac_syntax:attribute_name(F),
    Args = refac_syntax:attribute_arguments(F),
    case Args of 
	[L] ->
	    L1 = [Fun(E) || E<-refac_syntax:list_elements(L)],
	    rewrite(F, refac_syntax:attribute(Name, [rewrite(L, refac_syntax:list(L1))]));
	_ -> F
    end.

add_fun_def_info_in_form(F, ModName, DefinedFuns, ImportedFuns) ->
    {F1, _} =refac_util:full_tdTP(fun add_fun_def_info_1/2, F, {ModName, DefinedFuns, ImportedFuns}),
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
    case refac_syntax:type(T) of
	function -> 
	    add_fun_def_info_in_function(T, ModName);
	application ->
	    add_fun_def_info_in_apps(T, ModName, DefineModLoc);
	implicit_fun ->
	    add_fun_def_info_in_implicit_funs(T, ModName, DefineModLoc);
	_ -> T
    end.

add_fun_def_info_in_function(T, ModName) ->
    Name = refac_syntax:function_name(T),
    Fun_Name = refac_syntax:atom_value(Name),
    Arity = refac_syntax:function_arity(T),
    Pos = refac_syntax:get_pos(T),
    Ann = {fun_def, {ModName, Fun_Name, Arity, Pos, Pos}},
    T1 = [update_ann(C, Ann) || C <- refac_syntax:function_clauses(T)],
    Name1 = update_ann(Name, Ann),
    T2 = rewrite(T, refac_syntax:function(Name1, T1)),
    update_ann(T2, Ann).


add_fun_def_info_in_apps(T, ModName, DefineModLoc) ->
    Op = refac_syntax:application_operator(T),
    Args = refac_syntax:application_arguments(T),
    Pos = refac_syntax:get_pos(Op),
    case refac_syntax:type(Op) of
	atom ->
	    OpVal = refac_syntax:atom_value(Op),
	    Arity = length(Args),
	    {DefMod, DefLoc} = DefineModLoc(OpVal, Arity),
	    Ann = {fun_def, {DefMod, OpVal, Arity, Pos, DefLoc}},
	    Op1 = update_ann(Op,Ann),
	    rewrite(T, refac_syntax:application(Op1, Args));
	module_qualifier ->
	    Mod = refac_syntax:module_qualifier_argument(Op),
	    Fun = refac_syntax:module_qualifier_body(Op),
	    {M, FunName, Arity} = get_mod_fun_arg_info(Mod, Fun, Args, ModName),
	    DefLoc = if M == ModName ->
			     element(2, DefineModLoc(FunName, Arity));
			true -> ?DEFAULT_LOC
		     end,
	    Ann ={fun_def, {M, FunName, Arity, Pos, DefLoc}},
	    Op1 = update_ann(Op, Ann),
	    rewrite(T, refac_syntax:application(Op1, Args));
	tuple ->
	    case refac_syntax:tuple_elements(Op) of
		[Mod, Fun] ->
		    {M, FunName, Arity} = get_mod_fun_arg_info(Mod, Fun, Args, ModName),
		    DefLoc = if M == ModName ->
				     element(2, DefineModLoc(FunName, Arity));
				true -> ?DEFAULT_LOC
			     end,
		    Ann = {fun_def, {M, FunName, Arity, Pos, DefLoc}},
		    Fun1 = update_ann(Fun, Ann),
		    Op1 = rewrite(Op, refac_syntax:tuple([Mod, Fun1])),
		    Op2 = update_ann(Op1, Ann),
		    rewrite(T, refac_syntax:application(Op2, Args));
		_ ->
		    Arity = length(Args),
		    Ann ={fun_def, {'_', '_', Arity, Pos, ?DEFAULT_LOC}},
		    Op1 = update_ann(Op, Ann),
		    rewrite(T, refac_syntax:application(Op1, Args))
	    end;
	implicit_fun ->
	    T;
	_ -> Arity = length(Args),
	     Ann ={fun_def, {'_', '_', Arity, Pos, ?DEFAULT_LOC}},
	     Op1 = update_ann(Op, Ann),
	     rewrite(T, refac_syntax:application(Op1, Args))
    end.


add_fun_def_info_in_implicit_funs(T, ModName, DefineModLoc) ->
    Name = refac_syntax:implicit_fun_name(T),
    case refac_syntax:type(Name) of
	module_qualifier ->
	    Mod = refac_syntax:module_qualifier_argument(Name),
	    Body = refac_syntax:module_qualifier_body(Name),
	    case refac_syntax:type(Body) of
		arity_qualifier ->
		    Fun = refac_syntax:arity_qualifier_body(Body),
		    Pos =refac_syntax:get_pos(Fun),
		    A = refac_syntax:arity_qualifier_argument(Body),
		    {DefMod, FunName, Arity} = get_mod_fun_arg_info(Mod, Fun, A, ModName),
		    DefLoc = if DefMod == ModName ->
				     element(2, DefineModLoc(FunName, Arity));
				true -> ?DEFAULT_LOC
			     end,
		    Ann = {fun_def, {DefMod, FunName, Arity, Pos, DefLoc}},
		    Fun1 = update_ann(Fun, Ann),
		    Body1 = update_ann(rewrite(Body, refac_syntax:arity_qualifier(Fun1, A)), Ann),
		    Name1=update_ann(rewrite(Name, refac_syntax:module_qualifier(Mod, Body1)), Ann),
		    update_ann(rewrite(T, refac_syntax:implicit_fun(Name1)), Ann);		
		_ -> T
	    end;
	arity_qualifier ->
	    Fun = refac_syntax:arity_qualifier_body(Name),
	    A = refac_syntax:arity_qualifier_argument(Name),
	    FunName = refac_syntax:atom_value(Fun),
	    Arity = refac_syntax:integer_value(A),
	    Pos =refac_syntax:get_pos(Fun),
	    {DefMod, DefLoc} = DefineModLoc(FunName, Arity),
	    Ann ={fun_def, {DefMod, FunName, Arity, Pos, DefLoc}},
	    Fun1 = update_ann(Fun, Ann),
	    Name1 =update_ann(rewrite(Name, refac_syntax:arity_qualifier(Fun1, A)), Ann),
	    update_ann(rewrite(T, refac_syntax:implicit_fun(Name1)), Ann);
	_ -> T
    end.


get_mod_fun_arg_info(Mod, Fun, Args, CurModName) ->
    M = case refac_syntax:type(Mod) of
	  atom -> refac_syntax:atom_value(Mod);
	  macro -> MacroName = refac_syntax:macro_name(Mod),
		   case refac_syntax:type(MacroName) of
		     variable ->
			 case refac_syntax:variable_name(MacroName) of
			   'MODULE' -> CurModName;
			   _ -> '_'
			 end;
		     _ -> '_'
		   end;
	  _ -> '_'
	end,
    F = case refac_syntax:type(Fun) of
		 atom -> refac_syntax:atom_value(Fun);
		 _ -> '_'
	       end,
    A =case is_list(Args) of 
	   true -> length(Args);
	   _  ->case refac_syntax:type(Args) of 
		    integer ->
			refac_syntax:integer_value(Args);
		    _ -> '_'
		end
       end,
    {M, F, A}.

