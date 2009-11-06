%% Copyright (c) 2009, Huiqing Li, Simon Thompson
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
%%       names of its contributors may be used to endoorse or promote products
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
%% Refactoring: Unfold a function application.
%%
%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%% 
%% =============================================================================================

%% =============================================================================================
-module(refac_inline_fun).

-export([inline_fun/4, inline_fun_eclipse/4]).

-include("../include/wrangler.hrl").

%% =============================================================================================
%%-spec(inline_fun/4::(FileName::filename(), Pos::pos(), SearchPaths::[dir()], TabWidth::integer)
%%      ->{'ok', [string()]}).
inline_fun(FileName, Pos, SearchPaths, TabWidth) ->
    inline_fun(FileName, Pos, SearchPaths, TabWidth, emacs).

%%-spec(inline_fun_eclipse/4::(FileName::filename(), Pos::pos(), SearchPaths::[dir()], TabWidth::integer)
%%      ->{ok, [{filename(), filename(), string()}]}).
inline_fun_eclipse(FileName,Pos,SearchPaths, TabWidth) ->
    inline_fun(FileName, Pos, SearchPaths, TabWidth, eclipse).

inline_fun(FileName, Pos={Line, Col}, SearchPaths, TabWidth, Editor) ->
    ?wrangler_io("\nCMD: ~p:inline_fun(~p, {~p,~p}, ~p, ~p).\n",
		 [?MODULE, FileName, Line, Col, SearchPaths, TabWidth]),
    {ok, {AnnAST, Info}} = refac_util:parse_annotate_file(FileName, true, [], TabWidth),
    {value, {module, ModName}} = lists:keysearch(module, 1, Info),
    case pos_to_fun_app(AnnAST, Pos) of 
	{ok, App} -> App;
	{error, Reason} -> App=none,
			   throw({error, Reason})
    end,
    {ok, FunDef}=side_cond_analysis(ModName, AnnAST, App),
    fun_inline_1(FileName, AnnAST, Pos, FunDef, App, Editor).

side_cond_analysis(ModName, AnnAST, App) ->
    FunName = refac_syntax:atom_value(refac_syntax:application_operator(App)),
    Args = refac_syntax:application_arguments(App),
    Arity = length(Args),
    Fs = refac_syntax:form_list_elements(AnnAST),
    Res = [F || F <- Fs, refac_syntax:type(F) == function,
		case lists:keysearch(fun_def, 1, refac_syntax:get_ann(F)) of
		    {value, {fun_def, {ModName, FunName, Arity, _, _}}} -> true;
		    _ -> false
		end],
    case Res of
	[FunDef] -> 
	    Cs = refac_syntax:function_clauses(FunDef),
	    case length(Cs) >1 of
		true -> throw({error, "Inlining a function with multiple function clauses is not supported yet."});
		false-> C = hd(Cs),
			_B = refac_syntax:clause_body(C),
			Ps = refac_syntax:clause_patterns(C),
			G = refac_syntax:clause_guard(C),
			case G of 
			    none ->
				case lists:all(fun(P) ->
						       refac_syntax:type(P)==variable orelse
							   refac_syntax:is_literal(P) 
					       end, Ps) of
				    true -> {ok, FunDef};
				    false ->
					throw({error, "Inlining a function with complex parameters is not supported yet."})
				end;
			    _ ->throw({error, "Inlining a function with guard expressions is not supported yet."})
			end
	    end;
	[] -> 
	    throw({error, "The function to be inlined is not defined in this module."});
	[_|_]->
	    throw({error, "The function has been defined more than once."})
    end.

fun_inline_1(FileName, AnnAST, Pos, FunDef, App, Editor) ->
    Args = refac_syntax:application_arguments(App),
    C = hd(refac_syntax:function_clauses(FunDef)),
    B = refac_syntax:clause_body(C),
    Ps = refac_syntax:clause_patterns(C),
    %% TOCHANGE: there is not def for literal patterns.
    PsDefPoss = lists:map(fun(P) ->
				  Ann = refac_syntax:get_ann(P),
				  {value, {def, DefinePos}}=lists:keysearch(def, 1, Ann),
				  DefinePos
			  end, Ps),
    Subst= lists:zip(PsDefPoss, Args),
    {SubstedBody, _} = lists:unzip([refac_util:stop_tdTP(fun do_subst/2, E, Subst) || E <-B]),
    Fs = refac_syntax:form_list_elements(AnnAST),
    Fs1 = [do_inline(F, Pos, App, SubstedBody) || F<-Fs],
    AnnAST1 = refac_util:rewrite(AnnAST, refac_syntax:form_list(Fs1)),
    case Editor of 
	emacs ->
	    refac_util:write_refactored_files_for_preview([{{FileName, FileName}, AnnAST1}]),
	    {ok, [FileName]};
	eclipse ->
	    Content = refac_prettypr:print_ast(refac_util:file_format(FileName),AnnAST1),
	    {ok, [{FileName, FileName,Content}]}
    end.


do_inline(Form, Pos, App, SubstedBody) ->
    {S, E} = refac_util:get_range(Form),
    if  (S =< Pos) and (Pos =< E) ->
	    {F1, _} = refac_util:stop_tdTP(
			fun do_inline_1/2, Form, {App, SubstedBody}),
	    F1;
	true ->
	    Form
    end.
do_inline_1(Node, {App, SubstedBody}) ->
    case Node of
	App ->
	    case SubstedBody of 
		[B] ->
		    {B, true};
		[_|_] ->
		    {refac_syntax:block_expr(SubstedBody), true}
	    end;
	_ -> {Node, false}
    end.
    
    
    
do_subst(Node, Subst) ->
    case refac_syntax:type(Node) of
	variable ->
	    As = refac_syntax:get_ann(Node),
	    case lists:keysearch(def,1, As) of 
		{value, {def, DefinePos}} ->
		    case lists:keysearch(DefinePos,1, Subst) of
			{value, {DefinePos, Expr}} ->
			    {Expr, true};
			_ ->{Node, false}
		    end;
		_ -> {Node, false}
	    end;
	_ ->{Node,false}
    end.
	
	    
    
pos_to_fun_app(Node, Pos) ->
    case refac_util:once_tdTU(fun pos_to_fun_app_1/2, Node, Pos) of
	{_, false} -> {error, "You have not selected a function application, "
		       "or the function containing the function application does not parse."};
	{R, true} -> {ok, R}
    end.

pos_to_fun_app_1(Node, Pos) ->
    case refac_syntax:type(Node) of
	application ->
	    {S, E} = refac_util:get_range(Node),
	    if (S =< Pos) and (Pos =< E) ->
		    {Node, true};
	       true -> {[], false}
	    end;
	_ -> {[], false}
    end.

