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
%% Refactoring: Introduce a ?LET.
%%
%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%% 
%% =============================================================================================

%% =============================================================================================
-module(refac_new_let).

-export([new_let/6,  new_let_1/7, new_let_eclipse/6, new_let_1_eclipse/6,
	 merge_let/3, merge_let_1/5, merge_let_eclipse/3, merge_let_1_eclipse/4,
	 merge_forall/3, merge_forall_1/5,merge_forall_eclipse/3, merge_forall_1_eclipse/4]).

-include("../include/wrangler.hrl").


%% =============================================================================================
%%-spec(new_let/6::(filename(), pos(), pos(), string(), [dir()], integer()) ->
%%	     {'ok', [filename()]} | {question, string(), list(), list(), string()}).
new_let(FileName, Start, End, NewPatName, SearchPaths, TabWidth) ->
    new_let(FileName, Start, End, NewPatName, SearchPaths, TabWidth, emacs).


%%-spec(new_let_eclipse/6::(filename(), pos(), pos(), string(), [dir()], integer()) ->
%%	     {'ok', [{filename(), filename(),string()}]} | {question, string(), {list(), list()}}).
new_let_eclipse(FileName, Start, End, NewPatName, SearchPaths, TabWidth) ->
    new_let(FileName, Start, End, NewPatName, SearchPaths, TabWidth, eclipse).

new_let(FileName, Start = {Line, Col}, End = {Line1, Col1}, NewPatName, SearchPaths, TabWidth, Editor) ->
    ?wrangler_io("\nCMD: ~p:new_let(~p, {~p,~p}, {~p,~p}, ~p, ~p,~p).\n",
		 [?MODULE, FileName, Line, Col, Line1, Col1, NewPatName, SearchPaths, TabWidth]),
    Cmd = "CMD: " ++ atom_to_list(?MODULE) ++ ":new_let(" ++ "\"" ++ 
	    FileName ++ "\", {" ++ integer_to_list(Line) ++ ", " ++ integer_to_list(Col) ++ "}," ++ 
	      "{" ++ integer_to_list(Line1) ++ ", " ++ integer_to_list(Col1) ++ "}," ++ "\"" ++ NewPatName ++ "\","
														 ++ integer_to_list(TabWidth) ++ ").",
    {ok, {AnnAST, Info}} = wrangler_ast_server:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    case is_quickcheck_used(Info) of
	true -> ok;
	false -> throw({error, "QuickCheck is not used by this module."})
    end,
    case interface_api:pos_to_fun_def(AnnAST, Start) of
	{ok, FunDef} ->
	    case interface_api:pos_to_expr(FunDef, Start, End) of
		{ok, Expr} ->
		    ?debug("Expr:\n~p\n", [Expr]),
		    case side_cond_analysis(FunDef, Expr, NewPatName) of
			{ok, {ParentExpr, LetMacro}} ->
			    new_let_2(FileName, AnnAST, NewPatName, Expr, ParentExpr, LetMacro, Editor, Cmd, TabWidth);
			{question, Msg, ParentExpr} ->
			    case Editor of
				emacs ->
				    {question, Msg, term_to_list(Expr), term_to_list(ParentExpr), Cmd};
				eclipse ->
				    {question, Msg, {Expr, ParentExpr}}
			    end
		    end;
		{error, Reason} -> throw({error, Reason})
	    end;
	{error, _Reason} -> throw({error, "You have not selected an expresison."})
    end.

%%-spec(new_let_1/7::(filename(), string(), list(), list(), [dir()], integer(), string()) ->			 
%%			 {ok,[filename()]}).
new_let_1(FileName, NewPatName, Expr, ParentExpr, SearchPaths, TabWidth, Cmd) ->
    {ok, {AnnAST, _Info}} = wrangler_ast_server:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    Expr1 = list_to_term(Expr),
    ParentExpr1 = list_to_term(ParentExpr),
    new_let_2(FileName, AnnAST, NewPatName, Expr1, ParentExpr1, none, emacs, Cmd, TabWidth).

%%-spec(new_let_1_eclipse/6::(filename(), string(), syntaxTree(), syntaxTree(), [dir()], integer()) ->	
%%			 {'ok', [{filename(), filename(),string()}]}).		
new_let_1_eclipse(FileName, NewPatName, Expr, ParentExpr, SearchPaths, TabWidth) ->
    {ok, {AnnAST, _Info}} = wrangler_ast_server:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    new_let_2(FileName, AnnAST, NewPatName, Expr, ParentExpr, none, eclipse, "", TabWidth).

new_let_2(FileName, AnnAST, NewPatName, Expr, ParentExpr, LetMacro, Editor, Cmd, TabWidth) ->
    AnnAST1 = do_intro_new_let(AnnAST, Expr, list_to_atom(NewPatName), ParentExpr, LetMacro),
    case Editor of
	emacs ->
	    Res = [{{FileName, FileName}, AnnAST1}],
	    refac_write_file:write_refactored_files_for_preview(Res, TabWidth, Cmd),
	    {ok, [FileName]};
	eclipse ->
	    FileContent = refac_prettypr:print_ast(refac_util:file_format(FileName), AnnAST1, TabWidth),
	    {ok, [{FileName, FileName, FileContent}]}
    end.

side_cond_analysis(FunDef, Expr, NewPatName) ->
    case refac_util:is_var_name(NewPatName) of
	true -> ok;
	_ -> throw({error, "Invalid pattern variable name."})
    end,
    case get_parent_expr(FunDef, Expr) of
	{ok, ParentExpr} ->
	    FrVars = refac_util:get_free_vars(ParentExpr),
	    BdVars = refac_util:get_bound_vars(ParentExpr),
	    EnvVars = refac_util:get_env_vars(ParentExpr),
	    Vars = element(1, lists:unzip(FrVars ++ BdVars ++ EnvVars)),
	    case lists:member(list_to_atom(NewPatName), Vars) of
		true ->
		    throw({error, "The new pattern variable chould cause name shadow or semantics change."});
		false ->
		    ok
	    end,
	    ?debug("Parent Expr:\n~p\n", [ParentExpr]),
	    case enclosing_macro(FunDef, ParentExpr, 'LET', 3) of
		none ->
		    ?debug("Enclosing Let Macro: none\n", []),
		    case is_generator(Expr) of
			true -> {ok, {ParentExpr, none}};
			false -> throw({error, "The expression selected is not a QuickCheck generator."});
			unknown -> {question, "Is the expression selected a QuickCheck generator?", ParentExpr}
		    end;
		{ok, LetMacro} ->
		    ?debug("Enclosing Let Macro:~p\n", [LetMacro]),
		    {ok, {ParentExpr, LetMacro}}
	    end;
	{error, _} ->
	    case is_generator(Expr) of
		false -> throw({error, "The expression selected is not a QuickCheck generator."});
		_ -> {ok, {Expr, none}}
	    end
    end.

    
get_parent_expr(Node, Exp) ->
    case
      ast_traverse_api:once_tdTU(fun get_parent_expr_1/2, Node, Exp)
	of
      {_, false} ->
	  {error, none};
      {R, true} ->
	  {ok, R}
    end.

get_parent_expr_1(Node, Exp) ->
    case refac_syntax:type(Node) of 
	tuple ->
	    Es = refac_syntax:tuple_elements(Node),
	    case lists:member(Exp, Es) of
		true ->
		    {Node, true};
		false ->
		    {[], false}
	    end;
	list ->
	    Es = refac_syntax:list_elements(Node),
	    case lists:member(Exp, Es) of
		true ->
		    {Node, true};
		false ->
		    {[], false}
	    end;
	record_expr ->
	    Fs = refac_syntax:record_expr_fields(Node),
	    F1 =[F||F<-Fs, refac_syntax:record_field_value(F)==Exp],
	    case F1 of
		[_F] ->
		    {Node,true};
		[] ->
		    {[], false}
	    end;
	_ -> {[], false}
    end.


enclosing_macro(Node, Expr, MacroName, Nth) ->
    case
      ast_traverse_api:once_tdTU(fun get_enclosing_macro/2, Node, {Expr, MacroName, Nth})
	of
      {_, false} ->
	  none;
      {R, true} ->
	  {ok, R}
    end.


get_enclosing_macro(Node, {Expr, Macro, Nth}) ->
    case refac_syntax:type(Node) of
	macro ->
	    Name = refac_syntax:macro_name(Node),
	    Args = refac_syntax:macro_arguments(Node),
	    case refac_syntax:type(Name) of
		variable when is_list(Args)->
		    M = refac_syntax:variable_name(Name),
		    Arity = length(Args),
		    case {M, Arity} of
			{Macro, 3} ->
			    B = element(Nth, list_to_tuple(Args)),
			    case B of
				Expr ->
				    {Node, true};
				_ -> {[], false}
			  end;
		      _ ->
			  {[], false}
		  end;
	      _ -> {[], false}
	  end;
	_ ->
	    {[], false}
    end.

do_intro_new_let(Node, Exp, NewPatName, ParentExpr, LetMacro) ->
    element(1, ast_traverse_api:stop_tdTP(fun do_intro_new_let/2, Node, {Exp, NewPatName, ParentExpr, LetMacro})).

do_intro_new_let(Node, {Expr, NewPatName, ParentExpr, LetMacro}) ->
    case Node of
	LetMacro when LetMacro =/= none ->
	    Args = list_to_tuple(refac_syntax:macro_arguments(LetMacro)),
	    Pats = element(1, Args),
	    G1 = element(2, Args),
	    BdVars = refac_util:get_bound_vars(Pats),
	    FrVars = refac_util:get_free_vars(Expr),
	    case FrVars -- BdVars of
		FrVars ->
		    ParentExpr1 = replace_expr_with_var(ParentExpr, {Expr, NewPatName}),
		    NewPat = refac_syntax:variable(NewPatName),
		    {NewPats, NewG1} =
			case {refac_syntax:type(Pats), refac_syntax:type(G1)} of
			    {tuple, tuple} ->
				Ps = refac_syntax:tuple_elements(Pats),
				Gs = refac_syntax:tuple_elements(G1),
				{refac_util:rewrite_with_wrapper(Pats, refac_syntax:tuple(Ps ++ [NewPat])),
				 refac_util:rewrite_with_wrapper(G1, refac_syntax:tuple(Gs ++ [Expr]))};
			    _ ->
				{refac_syntax:tuple([Pats, NewPat]),
				 refac_syntax:tuple([G1, Expr])}
			end,
		    NewArgs = [NewPats, NewG1, ParentExpr1],
		    {refac_syntax:macro(refac_syntax:variable('LET'), NewArgs), true};
		_ -> {Node, false}
	    end;
	ParentExpr ->
	    ParentExpr1 = replace_expr_with_var(ParentExpr, {Expr, NewPatName}),
	    Args = [refac_syntax:variable(NewPatName),
		    Expr, ParentExpr1],
	    {refac_syntax:macro(refac_syntax:variable('LET'), Args), true};
	_ -> {Node, false}
    end.

replace_expr_with_var(Node, {Expr, Var}) ->
    element(1, ast_traverse_api:stop_tdTP(fun do_replace_expr_with_var/2, Node, {Expr, Var})).

do_replace_expr_with_var(Node, {Expr, Var}) ->
    case Node of
	Expr ->
	    {refac_util:rewrite_with_wrapper(Expr, refac_syntax:variable(Var)), true};
	_ -> {Node, false}
    end.

is_generator(Expr) ->
    case refac_syntax:is_literal(Expr) of
      true -> false;
      false ->
	  case refac_syntax:type(Expr) of
	    tuple ->
		  Es = refac_syntax:tuple_elements(Expr), 
		  all_are_generators(Es);
	    list ->
		  Es = refac_syntax:list_elements(Expr),
		  all_are_generators(Es);
	      record_expr ->
		  Fs = refac_syntax:record_expr_fields(Expr),
		  Vs =[refac_syntax:record_field_value(F) || F <-Fs],
		  all_are_generators(Vs);
	      application ->
		  As = refac_syntax:get_ann(refac_syntax:application_operator(Expr)),
		  case lists:keysearch(fun_def, 1, As) of
		      {value, {fun_def, {Mod, Fun, Arity, _, _}}} ->
			  case returns_gen({Mod, Fun, Arity}) of
			      true -> true;
			      _ -> unknown
			  end;
		      _ -> unknown
		  end;
	    _ -> unknown
	  end
    end.

all_are_generators(Es) ->
    case lists:all(fun (E) -> is_generator(E) == true end, Es) of
      true ->
	    true;
	_ ->
	    case lists:all(fun (E) -> is_generator(E) == false end, Es) of
		true ->
		    false;
		_ ->
		    unknown
	    end
    end.
	
returns_gen({eqc_gen, binary, 0}) ->
    true;
returns_gen({eqc_gen, binary,1}) ->
    true;
returns_gen({eqc_gen, bool,1}) ->
    true;
returns_gen({eqc_gen, char, 0}) ->
    true;
returns_gen({eqc_gen, choose, 2}) ->
    true;
returns_gen({eqc_gen, default, 2}) ->
    true;
returns_gen({eqc_gen, fault, 2}) ->
    true;
returns_gen({eqc_gen, frequency,1}) ->
    true;
returns_gen({eqc_gen, function0, 1}) ->
    true;
returns_gen({eqc_gen, function1, 1}) ->
    true;
returns_gen({eqc_gen, function2, 1}) ->
    true;
returns_gen({eqc_gen, function3, 1}) ->
    true;
returns_gen({eqc_gen, function4, 1}) ->
    true;
returns_gen({eqc_gen, growingelements, 1}) ->
    true;
returns_gen({eqc_gen, int, 0}) ->
    true;
returns_gen({eqc_gen, list, 1}) ->
    true;
returns_gen({eqc_gen, maybe, 1}) ->
    true;
returns_gen({eqc_gen, more_faulty, 2}) ->
    true;
returns_gen({eqc_gen, nat, 0}) ->
    true;
returns_gen({eqc_gen, no_faults, 1}) ->
    true;
returns_gen({eqc_gen, noshrink, 1}) ->
    true;
returns_gen({eqc_gen, oneof, 1}) ->
    true;
returns_gen({eqc_gen, open, 1}) ->
    true;
returns_gen({eqc_gen, orderedlist, 1}) ->
    true;
returns_gen({eqc_gen, parameter, 1}) ->
    true;
returns_gen({eqc_gen, parameter, 2}) ->
    true;
returns_gen({eqc_gen, real, 0}) ->
    true;
returns_gen({eqc_gen, resize, 2}) ->
    true;
returns_gen({eqc_gen, return, 1}) ->
    true;
returns_gen({eqc_gen, seal, 1}) ->
    true;
returns_gen({eqc_gen, shrink_int, 3}) ->
    true;
returns_gen({eqc_gen, shrink_list, 1}) ->
    true;
returns_gen({eqc_gen, shrink_without_duplicates, 1}) ->
    true;
returns_gen({eqc_gen, timeout, 2}) ->
    true;
returns_gen({eqc_gen, vextor, 2}) ->
    true;
returns_gen({eqc_gen, weighted_default, 2}) ->
    true;
returns_gen({eqc_gen, with_parameter, 3}) ->
    true;
returns_gen({eqc_gen, with_parameter, 2}) ->
    true;
returns_gen(_) ->
    false.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Merge LET expressions. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%-spec(merge_let/3::(FileName::filename(), SearchPaths::[dir()], TabWidth::integer()) ->
%%	     {not_found, string()} |{ok, [{{integer(), integer(), integer(), integer()}, string()}], string()}).
merge_let(FileName, SearchPaths, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:merge_let(~p,~p,~p).\n",
		 [?MODULE, FileName, SearchPaths, TabWidth]),
    merge(FileName, 'LET', SearchPaths, TabWidth, emacs).


%%-spec(merge_let_eclipse/3::(FileName::filename(), SearchPaths::[dir()], TabWidth::integer()) ->
%%	     {not_found, string()} |{ok, [{{integer(), integer(), integer(), integer()}, syntaxTree()}]}).
merge_let_eclipse(FileName, SearchPaths, TabWidth) ->
    merge(FileName, 'LET', SearchPaths, TabWidth, eclipse).

%%-spec(merge_forall/3::(FileName::filename(), SearchPaths::[dir()], TabWidth::integer()) ->
%%			    {not_found, string()} |{ok, [{{{integer(), integer()}, {integer(), integer()}}, syntaxTree()}]}).
merge_forall(FileName, SearchPaths, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:merge_forall(~p,~p,~p).\n",
		 [?MODULE, FileName, SearchPaths, TabWidth]),
    merge(FileName, 'FORALL', SearchPaths, TabWidth, emacs).


%%-spec(merge_forall_eclipse/3::(FileName::filename(), SearchPaths::[dir()], TabWidth::integer()) ->
%%	     {not_found, string()} |{ok, [{{{integer(), integer()}, {integer(), integer()}}, syntaxTree()}]}).
merge_forall_eclipse(FileName, SearchPaths, TabWidth) ->
    merge(FileName, 'FORALL', SearchPaths, TabWidth, eclipse).

merge(FileName, MacroName, SearchPaths, TabWidth, Editor) ->
    Cmd = "CMD: " ++ atom_to_list(?MODULE) ++ ":merge_let(" ++ "\"" ++ 
	    FileName ++ "\"," ++ atom_to_list(MacroName) ++ ", " ++ "["
								       ++ refac_util:format_search_paths(SearchPaths) ++ "]," ++ integer_to_list(TabWidth) ++ ").",
    {ok, {AnnAST, Info}} = wrangler_ast_server:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    case is_quickcheck_used(Info) of
	true -> ok;
	false -> throw({error, "QuickCheck is not used by this module."})
    end,
    Candidates = search_merge_candiates(AnnAST, MacroName),
    case Candidates of
	[] ->
	    {not_found, "No ?" ++ atom_to_list(MacroName) ++ " applications to merge."};
	_ ->
	    case Editor of
		emacs ->
		    Regions = lists:keysort(1, lists:map(fun ({{{StartLine, StartCol}, {EndLine, EndCol}}, NewLetApp}) ->
								 NewLetApp1 = term_to_list(NewLetApp),
								 {{StartLine, StartCol, EndLine, EndCol}, NewLetApp1}
							 end,
							 Candidates)),
		    {ok, Regions, Cmd};
		eclipse ->
		    {ok, Candidates}
	    end
    end.

%%-spec(merge_let_1/5::(FileName::filename(), Candidates::[{{integer(), integer(), integer(), integer()}, string()}],
%%		      SearchPaths::[dir()], TabWidth::integer(), Cmd::string()) -> {ok, [filename()]}).
merge_let_1(FileName, Candidates, SearchPaths, TabWidth, Cmd) ->
    merge_1(FileName, Candidates, SearchPaths, TabWidth, Cmd, emacs, TabWidth).


%%-spec(merge_forall_1/5::(FileName::filename(), Candidates::[{{integer(), integer(), integer(), integer()}, string()}],
%%		      SearchPaths::[dir()], TabWidth::integer(), Cmd::string()) -> {ok, [filename()]}).
merge_forall_1(FileName, Candidates, SearchPaths, TabWidth, Cmd) ->
    merge_1(FileName, Candidates, SearchPaths, TabWidth, Cmd, emacs, TabWidth).



%%-spec(merge_let_1_eclipse/4::(FileName::filename(), Candidates::[{{{integer(), integer()}, {integer(), integer()}}, syntaxTree()}],
%%			      SearchPaths::[dir()], TabWidth::integer()) ->
%%				   {'ok', [{filename(), filename(),string()}]}).
merge_let_1_eclipse(FileName, Candidates, SearchPaths, TabWidth) ->
    Candidates1 = [{{StartLine, StartCol, EndLine, EndCol}, NewLetApp}||
		      {{{StartLine, StartCol}, {EndLine, EndCol}}, NewLetApp}<-Candidates],
    merge_1(FileName, Candidates1, SearchPaths, TabWidth, "", eclipse, TabWidth).


%%-spec(merge_forall_1_eclipse/4::(FileName::filename(), Candidates::[{{{integer(), integer()}, {integer(), integer()}}, syntaxTree()}],
%%				 SearchPaths::[dir()], TabWidth::integer()) -> 
%%				      {'ok', [{filename(), filename(),string()}]}).
merge_forall_1_eclipse(FileName, Candidates, SearchPaths, TabWidth) ->
    Candidates1 = [{{StartLine, StartCol, EndLine, EndCol}, NewLetApp}||
		      {{{StartLine, StartCol}, {EndLine, EndCol}}, NewLetApp}<-Candidates],
    merge_1(FileName, Candidates1, SearchPaths, TabWidth, "", eclipse,TabWidth).

merge_1(FileName, Candidates, SearchPaths, TabWidth, Cmd, Editor, TabWidth) ->
    {ok, {AnnAST, _Info}} = wrangler_ast_server:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    Candidates1 = case Editor of
		      emacs -> [{SE, list_to_term(NewLetApp)} || {SE, NewLetApp} <- Candidates];
		      eclipse -> Candidates
		  end,
    AnnAST1 = do_merge(AnnAST, Candidates1),
    case Editor of
	emacs ->
	    Res = [{{FileName, FileName}, AnnAST1}],
	    refac_write_file:write_refactored_files_for_preview(Res, TabWidth, Cmd),
	    {ok, [FileName]};
	eclipse ->
	    FileContent = refac_prettypr:print_ast(refac_util:file_format(FileName), AnnAST1, TabWidth),
	    {ok, [{FileName, FileName, FileContent}]}
    end.


do_merge(AnnAST, []) ->
    AnnAST;
do_merge(AnnAST, Candidates) ->
    element(1, ast_traverse_api:stop_tdTP(fun do_merge_1/2, AnnAST, Candidates)).

do_merge_1(Tree, Candidates) ->
    {{StartLine, StartCol}, {EndLine, EndCol}} = refac_util:get_start_end_loc(Tree),
    case lists:keysearch({StartLine, StartCol, EndLine, EndCol}, 1, Candidates) of
	{value, {_, NewLetApp}} ->
	    {refac_util:rewrite_with_wrapper(Tree, NewLetApp), true};
	_ -> {Tree, false}
    end.

search_merge_candiates(AnnAST, MacroName) ->
    F = fun (Node, Acc) ->
		case is_macro_app(Node, MacroName) of
		    true ->
			collect_mergeable_lets_or_foralls(Node, MacroName) ++ Acc;
		    _ -> Acc
		end
	end,
    ast_traverse_api:fold(F, [], AnnAST).

is_macro_app(Node, MacroName) ->
    case refac_syntax:type(Node) of
      macro ->
	  Name = refac_syntax:macro_name(Node),
	  case refac_syntax:type(Name) of
	    variable ->
		M = refac_syntax:variable_name(Name),
		Args = refac_syntax:macro_arguments(Node),
		case is_list(Args) of 
		    true ->
			Arity = length(Args),
			case {M, Arity} of
			    {MacroName, 3} ->
				true;
			    _ -> false
			end;
		    false ->
			false
		end;
	    _ -> false
	  end;
      _ -> false
    end.

collect_mergeable_lets_or_foralls(Node, MacroName) ->
    Args = refac_syntax:macro_arguments(Node),
    [P, G1, G2] = Args,
    Res = collect_mergeable_lets_or_foralls_1([P, G1, G2], MacroName),
    case Res == [P, G1, G2] of
	true ->
	    [];
	_ -> [{refac_util:get_start_end_loc(Node),
	       refac_util:reset_attrs(refac_syntax:macro(refac_syntax:variable(MacroName), Res))}]
    end.

collect_mergeable_lets_or_foralls_1(Res = [P, G1, G2], MacroName) ->
    case is_macro_app(G2, MacroName) of
	true ->
	    Args1 = refac_syntax:macro_arguments(G2),
	    [P1, G11, G12] = Args1,
	    BdVars = refac_util:get_bound_vars(P),
	    FrVars = refac_util:get_free_vars(G11),
	    case FrVars -- BdVars of
		FrVars ->
		    {Ps, Gs} = get_pats_gens(P, G1),
		    {Ps1, Gs1} = get_pats_gens(P1, G11),
		    NewP = refac_syntax:tuple(Ps ++ Ps1),
		    NewG1 = refac_syntax:tuple(Gs ++ Gs1),
		    collect_mergeable_lets_or_foralls_1([NewP, NewG1, G12], MacroName);
		_ -> Res
	    end;
	false ->
	    Res
    end.

get_pats_gens(Pat, Gen) ->
     case {refac_syntax:type(Pat), refac_syntax:type(Gen)} of
	 {tuple, tuple} ->
	     {refac_syntax:tuple_elements(Pat),
	      refac_syntax:tuple_elements(Gen)};
	 _ -> {[Pat], [Gen]}
     end.
    

is_quickcheck_used(ModuleInfo) ->
    case lists:keysearch(imports, 1, ModuleInfo) of 
	{value, {imports, Imps}} ->
	    Ms = [M || {M, _} <-Imps],
	    lists:member(eqc, Ms) andalso lists:member(eqc_gen, Ms);
	false ->
	    false
    end.

term_to_list(T) -> binary_to_list(term_to_binary(T)).

list_to_term(T) -> binary_to_term(list_to_binary(T)).

