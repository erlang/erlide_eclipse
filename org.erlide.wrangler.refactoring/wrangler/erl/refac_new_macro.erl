%% ============================================================================================
%% Refactoring: Introduce a  macro to a selected expression.
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
%% This refactoring allows the user define a macro to represent a sequence of expression/patterns;
%% While by definition, the body of macro definition can be just a sequence of toks, this refactoring
%% only works with well-formed expression/patterns; after all using a macro to represent an 
%% arbitrary sequence of toks is not a programming style we should recommend.
%% =============================================================================================

-module(refac_new_macro).

-export([new_macro/6, new_macro_eclipse/6]).

-export([replace_expr_with_macro/3]).

-include("../include/wrangler.hrl").

%% =============================================================================================

-spec(new_macro/6::(filename(), pos(), pos(), string(), [dir()], integer()) ->
	      {error, string()} | {ok, string()}).
new_macro(FileName, Start, End, NewMacroName, SearchPaths, TabWidth) ->
    new_macro(FileName, Start, End, NewMacroName, SearchPaths, TabWidth, emacs).

-spec(new_macro_eclipse/6::(filename(), pos(), pos(), string(), [dir()], integer()) ->
	     {error, string()} | {ok, [{filename(), filename(), string()}]}).
new_macro_eclipse(FileName, Start, End, NewMacroName, SearchPaths, TabWidth) ->
    new_macro(FileName, Start, End, NewMacroName, SearchPaths, TabWidth, eclipse).


new_macro(FileName, Start={SLine, SCol}, End={ELine, ECol}, NewMacroName, SearchPaths, TabWidth, Editor) ->
    ?wrangler_io("\nCMD: ~p:new_macro(~p, {~p,~p}, {~p,~p}, ~p, ~p,~p).\n", 
				 [refac_new_macro, FileName, SLine, SCol, ELine, ECol, NewMacroName, SearchPaths, TabWidth]),
     case pre_cond_check(FileName, NewMacroName, Start, End, SearchPaths, TabWidth) of
		 {ok, AnnAST, Sel} ->
			 AnnAST1 = do_intro_new_macro(AnnAST, NewMacroName, Sel),
			 case Editor of
				 emacs ->
					 refac_util:write_refactored_files([{{FileName, FileName}, AnnAST1}]),
					 {ok, "Refactor succeeded"};
				 eclipse -> Res = [{FileName, FileName, refac_prettypr:print_ast(refac_util:file_format(FileName),AnnAST1)}], {ok, Res}
			 end;
		 {error, Reason} -> {error, Reason}
	 end.


pre_cond_check(FileName, NewMacroName, Start, End, SearchPaths, TabWidth) ->    
    case refac_util:is_fun_name(NewMacroName) orelse refac_util:is_var_name(NewMacroName) of 
	true ->
	    Ms = existing_macros(FileName, SearchPaths, TabWidth),
	    case lists:member(list_to_atom(NewMacroName), Ms) of 
		true ->
		     {error, "Macro name provided is already in use!"};
		_ ->
		    {ok, {AnnAST, _Info}} = refac_util:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
		    Sel = refac_util:pos_to_syntax_units(FileName, AnnAST, Start, End, fun is_expr_or_pat/1, TabWidth),
		    case Sel of
			 [] -> {error, "You have not selected a sequence of expressions/patterns!"};
			 _ ->
			      {ok, AnnAST, hd(Sel)}
		     end
	    end;
	_ -> {error, "Invalid macro name!"}
    end.


do_intro_new_macro(AnnAST, MacroName, SelExpList) ->
    Vars = lists:usort(lists:append(lists:map(fun(E)-> vars(E) end, SelExpList))),
    MName = case refac_util:is_var_name(MacroName) of 
		true -> refac_syntax:variable(list_to_atom(MacroName));
		_ -> refac_syntax:atom(list_to_atom(MacroName))
	    end,
    MDef =case Vars of 
	      [] -> refac_syntax:attribute(refac_syntax:atom(define), [MName|SelExpList]);
	      _ ->  
		  Args = lists:map(fun(P) -> refac_syntax:variable(P) end, Vars), 
		  MApp = refac_syntax:application(MName, Args),
		  refac_syntax:attribute(refac_syntax:atom(define), [MApp|SelExpList])
	  end,
    Forms = refac_syntax:form_list_elements(AnnAST),
    {Forms1, Forms2} = lists:splitwith(fun(F) -> (refac_syntax:type(F)==attribute) orelse 
						     (refac_syntax:type(F) == comment) end, Forms),
    {S1, _} = refac_util:get_range(hd(SelExpList)),
    {_, E1}= refac_util:get_range(lists:last(SelExpList)),
    MApp1 = case Vars of 
	       [] ->
		   refac_syntax:macro(MName);
	       _ ->
		   Args1 =lists:map(fun(P) -> refac_syntax:variable(P) end, Vars), 
		   refac_syntax:macro(MName, Args1)
	   end,
    Fun = fun(F) ->
		  {S, E} = refac_util:get_range(F),
		  case (S=<S1) and (E1=<E) of 
		      true -> replace_expr_with_macro(F, {SelExpList, S1, E1}, MApp1);
		      _ -> F
		  end
	  end,		     
    Forms11 = lists:map(fun(F) -> Fun(F) end, Forms1),
    Forms21 = lists:map(fun(F) -> Fun(F) end, Forms2),
    refac_syntax:form_list(Forms11++[MDef]++Forms21).



-spec(replace_expr_with_macro/3::(syntaxTree(), {[syntaxTree()], pos(), pos()}, syntaxTree()) ->
	     syntaxTree()).
replace_expr_with_macro(Form, {ExpList, SLoc, ELoc},  MApp) ->
    case (length(ExpList)==1) of 
	true ->
	    {Form1, _} = refac_util:stop_tdTP(fun do_replace_expr_with_macro_app_1/2, Form, {MApp, SLoc, ELoc}),
	    Form1;
	_ ->{Form1, _} = refac_util:stop_tdTP(fun do_replace_expr_with_macro_app_2/2, Form, {MApp, SLoc, ELoc}),
	    Form1
    end.

do_replace_expr_with_macro_app_1(Tree, {MApp, SLoc, ELoc}) ->
    case refac_util:get_range(Tree) of 
	{SLoc, ELoc} ->
	    {MApp, true};
	_ -> {Tree, false}
    end.

    
do_replace_expr_with_macro_app_2(Tree, {MApp, SLoc, ELoc}) ->
    case refac_syntax:type(Tree) of
	clause ->
	    Body = refac_syntax:clause_body(Tree),
	    Pats = refac_syntax:clause_patterns(Tree),
	    {NewBody, Modified} = process_exprs(Body, {MApp, SLoc, ELoc}),
	    {NewPats, Modified1}= process_exprs(Pats, {MApp, SLoc, ELoc}),
	    case Modified or Modified1 of 
		true ->
		    G    = refac_syntax:clause_guard(Tree),
		    {refac_syntax:copy_pos(Tree, 
					   refac_syntax:copy_attrs(Tree, 
								   refac_syntax:clause(NewPats, G, NewBody))),  true};
		_ ->
		     {Tree, false}
	    end;
	application ->
	    Args= refac_syntax:application_arguments(Tree),
	    {NewArgs, Modified} = process_exprs(Args, {MApp, SLoc, ELoc}),
	    case Modified of 
		true -> Op  = refac_syntax:application_operator(Tree),
			{refac_syntax:copy_pos(Tree, refac_syntax:copy_attrs(Tree, 
									     refac_syntax:application(Op, NewArgs))), true};
		false -> {Tree, false}
	    end;
	tuple ->
	    Elems = refac_syntax:tuple_elements(Tree),
	    {NewElems, Modified} = process_exprs(Elems, {MApp, SLoc, ELoc}),
	    case Modified of 
		true ->
		    {refac_syntax:copy_pos(Tree, refac_syntax:copy_attrs(Tree, refac_syntax:tuple(NewElems))), true};
		false -> {Tree, false}
	    end;
	block_expr ->
	    Exprs = refac_syntax:block_expr_body(Tree),
	    {NewExprs, Modified} = process_exprs(Exprs, {MApp,SLoc, ELoc}),
	    case Modified of 
		true ->{refac_syntax:copy_pos(Tree, refac_syntax:copy_attrs(Tree, refac_syntax:block_expr(NewExprs))), true};
		_ -> {Tree, false}
	    end;
	_ ->
	    {Tree, false}
    end.

process_exprs(Exprs, {MApp, SLoc, ELoc}) ->    
    {Exprs1, Exprs2} = lists:splitwith(fun(E) -> 
						 {SLoc1, _} =refac_util:get_range(E),
						 SLoc1 =/= SLoc 
				       end, Exprs),
    case Exprs2 of 
	[] -> {Exprs, false};
	_ -> {_Exprs21, Exprs22} = lists:splitwith(fun(E) -> 
							   {_, ELoc1} = refac_util:get_range(E),
							   ELoc1=/= ELoc 
						   end, Exprs2),
	     case Exprs22 of 
		 [] -> {Exprs, false}; %% THIS SHOULD NOT HAPPEN.
		 _ -> {Exprs1 ++ [MApp|tl(Exprs22)], true}
	     end
    end.
    
 
is_expr_or_pat(Node) ->
    refac_util:is_expr(Node) orelse refac_util:is_pattern(Node).


existing_macros(FileName, SearchPaths, TabWidth) -> 
    Dir = filename:dirname(FileName),
    DefaultIncl1 = [".","..", "../hrl", "../incl", "../inc", "../include"],
    DefaultIncl2 = [filename:join(Dir, X) || X <-DefaultIncl1],
    NewSearchPaths= SearchPaths++DefaultIncl2,
    case refac_epp:parse_file(FileName, NewSearchPaths, [], TabWidth, refac_util:file_format(FileName))  of 
	{ok, _, {MDefs, MUses}} -> 
	    lists:usort(lists:map(fun({{_,Name}, _Def}) -> Name end, MDefs++MUses));	 
	_ -> {error, "The current file does not compile!"}
    end.
	    
	

vars(Node) ->
    F = fun(T,S) ->
		case refac_syntax:type(T) of 
		    variable ->
			case lists:keysearch(category, 1, refac_syntax:get_ann(T)) of
			    {value, {category, macro_name}} ->
				 S;
			    _ ->		
				[refac_syntax:variable_name(T)|S]
			end;
		    _ -> S
		end
	end,
    lists:usort(refac_syntax_lib:fold(F, [], Node)).
  
