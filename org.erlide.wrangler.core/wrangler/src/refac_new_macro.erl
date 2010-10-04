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

%% ============================================================================================
%% Refactoring: Introduce a  macro to a selected expression.
%%
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

%%-spec(new_macro/6::(filename(), pos(), pos(), string(), [dir()], integer()) ->
%%	       {ok, [filename()]}).
new_macro(FileName, Start, End, NewMacroName, SearchPaths, TabWidth) ->
    new_macro(FileName, Start, End, NewMacroName, SearchPaths, TabWidth, emacs).

%%-spec(new_macro_eclipse/6::(filename(), pos(), pos(), string(), [dir()], integer()) ->
%%				 {ok, [{filename(), filename(), string()}]}).
new_macro_eclipse(FileName, Start, End, NewMacroName, SearchPaths, TabWidth) ->
    new_macro(FileName, Start, End, NewMacroName, SearchPaths, TabWidth, eclipse).


new_macro(FileName, Start = {SLine, SCol}, End = {ELine, ECol}, NewMacroName, SearchPaths, TabWidth, Editor) ->
    ?wrangler_io("\nCMD: ~p:new_macro(~p, {~p,~p}, {~p,~p}, ~p, ~p,~p).\n",
		 [?MODULE, FileName, SLine, SCol, ELine, ECol, NewMacroName, SearchPaths, TabWidth]),
    Cmd = "CMD: " ++ atom_to_list(?MODULE) ++ ":new_macro(" ++ "\"" ++
	    FileName ++ "\", {" ++ integer_to_list(SLine) ++ ", " ++ integer_to_list(SCol) ++ "}," ++
	      "{" ++ integer_to_list(ELine) ++ ", " ++ integer_to_list(ECol) ++ "}," ++ "\"" ++ NewMacroName ++ "\","
		++ "[" ++ refac_misc:format_search_paths(SearchPaths) ++ "]," ++ integer_to_list(TabWidth) ++ ").",
    {ok, {AnnAST, _Info}} = refac_util:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    case pre_cond_check(FileName, AnnAST, NewMacroName, Start, End, SearchPaths, TabWidth) of
      {ok, AnnAST, Sel, NeedBracket} ->
	  AnnAST1 = do_intro_new_macro(AnnAST, NewMacroName, Sel, NeedBracket),
	  return_refac_result(FileName, AnnAST1, Editor, Cmd);
      {error, Reason} -> throw({error, Reason})
    end.

pre_cond_check(FileName, AnnAST, NewMacroName, Start, End, SearchPaths, TabWidth) ->
    case refac_misc:is_fun_name(NewMacroName) orelse refac_misc:is_var_name(NewMacroName) of
      true ->
	  Ms = existing_macros(FileName, SearchPaths, TabWidth),
	  MsToAvoid = collect_names_to_avoid(AnnAST),
	  case lists:member(list_to_atom(NewMacroName), Ms ++ MsToAvoid) of
	    true ->
		{error, "Macro name provided is already in use!"};
	    _ ->
		case interface_api:pos_to_fun_def(AnnAST, Start) of
		  {ok, FunDef} ->
		      Sel = interface_api:pos_to_expr_or_pat_list(AnnAST, Start, End),
		      case Sel of
			[] -> {error, "You have not selected a sequence of expressions/patterns!"};
			_ ->
			    ExprList = [hd(Sel)],
			    NeedBracket = need_bracket(refac_misc:get_toks(FunDef), ExprList),
			    {ok, AnnAST, [hd(Sel)], NeedBracket}
		      end;
		  _ -> {error, "You have not selected a sequence of expressions/patterns!"}
		end
	  end;
      _ -> {error, "Invalid macro name!"}
    end.
do_intro_new_macro(AnnAST, MacroName, SelExpList, NeedBracket) ->
    SelExpList1 = case NeedBracket of
		    true -> [refac_syntax:parentheses(hd(SelExpList))];
		    _ -> SelExpList
		  end,
    Vars = refac_misc:collect_var_names(SelExpList),
    Args = [refac_syntax:variable(P) || P <- Vars],
    MName = mk_macro_name(MacroName),
    MDef = mk_macro_def(MName, Args, SelExpList1),
    Forms = refac_syntax:form_list_elements(AnnAST),
    {Forms1, Forms2} = lists:splitwith(fun (F) -> refac_syntax:type(F) == attribute orelse
						    refac_syntax:type(F) == comment
				       end, Forms),
    {S1, E1} = refac_misc:get_start_end_loc(SelExpList),
    MApp = mk_macro_app(MName, Args),
    Fun = fun (F) ->
		  {S, E} = refac_misc:get_start_end_loc(F),
		  case (S =< S1) and (E1 =< E) of
		    true -> replace_expr_with_macro(F, {SelExpList, S1, E1}, MApp);
		    _ -> F
		  end
	  end,
    Forms11 = [Fun(F) || F <- Forms1],
    Forms21 = [Fun(F) || F <- Forms2],
    refac_syntax:form_list(Forms11 ++ [MDef] ++ Forms21).

mk_macro_name(MacroName) ->
    case refac_misc:is_var_name(MacroName) of
      true -> refac_syntax:variable(list_to_atom(MacroName));
      _ -> refac_syntax:atom(list_to_atom(MacroName))
    end.

mk_macro_def(MName, Args, Exps) ->
    case Args of
      [] -> refac_syntax:attribute(refac_syntax:atom(define), [MName| Exps]);
      _ ->
	  MApp = refac_syntax:application(MName, Args),
	  refac_syntax:attribute(refac_syntax:atom(define), [MApp| Exps])
    end.

mk_macro_app(MName, Args) ->
    case Args of
      [] ->
	  refac_syntax:macro(MName);
      _ ->
	  refac_syntax:macro(MName, Args)
    end.

 

%%-spec(replace_expr_with_macro/3::(syntaxTree(), {[syntaxTree()], pos(), pos()}, syntaxTree()) ->
%%				       syntaxTree()).
replace_expr_with_macro(Form, {ExpList, SLoc, ELoc}, MApp) ->
    case length(ExpList) == 1 of
      true ->
	  element(1, ast_traverse_api:stop_tdTP(fun replace_single_expr_with_macro_app/2, Form, {MApp, SLoc, ELoc}));
      _ ->
	  element(1, ast_traverse_api:stop_tdTP(fun replace_expr_list_with_macro_app/2, Form, {MApp, SLoc, ELoc}))
    end.

replace_single_expr_with_macro_app(Tree, {MApp, SLoc, ELoc}) ->
    case refac_misc:get_start_end_loc(Tree) of
      {SLoc, ELoc} ->
	  {MApp, true};
      _ -> {Tree, false}
    end.

    
replace_expr_list_with_macro_app(Tree, {MApp, SLoc, ELoc}) ->
    case refac_syntax:type(Tree) of
      clause ->
	  Body = refac_syntax:clause_body(Tree),
	  Pats = refac_syntax:clause_patterns(Tree),
	  {NewBody, Modified} = process_exprs(Body, {MApp, SLoc, ELoc}),
	  {NewPats, Modified1} = process_exprs(Pats, {MApp, SLoc, ELoc}),
	  case Modified or Modified1 of
	    true ->
		G = refac_syntax:clause_guard(Tree),
		{refac_syntax:copy_pos(
		   Tree, refac_syntax:copy_attrs(
			   Tree, refac_syntax:clause(NewPats, G, NewBody))), true};
	    _ ->
		{Tree, false}
	  end;
      application ->
	  Args = refac_syntax:application_arguments(Tree),
	  {NewArgs, Modified} = process_exprs(Args, {MApp, SLoc, ELoc}),
	  case Modified of
	    true -> Op = refac_syntax:application_operator(Tree),
		    {refac_syntax:copy_pos(
		       Tree, refac_syntax:copy_attrs(
			       Tree, refac_syntax:application(Op, NewArgs))), true};
	    false -> {Tree, false}
	  end;
      tuple ->
	  Elems = refac_syntax:tuple_elements(Tree),
	  {NewElems, Modified} = process_exprs(Elems, {MApp, SLoc, ELoc}),
	  case Modified of
	    true ->
		{refac_syntax:copy_pos(Tree, refac_syntax:copy_attrs(
					       Tree, refac_syntax:tuple(NewElems))), true};
	    false -> {Tree, false}
	  end;
      block_expr ->
	  Exprs = refac_syntax:block_expr_body(Tree),
	  {NewExprs, Modified} = process_exprs(Exprs, {MApp, SLoc, ELoc}),
	  case Modified of
	    true -> {refac_syntax:copy_pos(Tree, refac_syntax:copy_attrs(
						   Tree, refac_syntax:block_expr(NewExprs))), true};
	    _ -> {Tree, false}
	  end;
      _ ->
	  {Tree, false}
    end.

process_exprs(Exprs, {MApp, SLoc, ELoc}) ->
    {Exprs1, Exprs2} = lists:splitwith(fun (E) ->
					       {SLoc1, _} = refac_misc:get_start_end_loc(E),
					       SLoc1 =/= SLoc
				       end, Exprs),
    case Exprs2 of
      [] -> {Exprs, false};
      _ -> {_Exprs21, Exprs22} = lists:splitwith(fun (E) ->
							 {_, ELoc1} = refac_misc:get_start_end_loc(E),
							 ELoc1 =/= ELoc
						 end, Exprs2),
	   case Exprs22 of
	     [] -> {Exprs, false}; %% THIS SHOULD NOT HAPPEN.
	     _ -> {Exprs1 ++ [MApp| tl(Exprs22)], true}
	   end
    end.
    

existing_macros(FileName, SearchPaths, TabWidth) ->
    Dir = filename:dirname(FileName),
    DefaultIncl = [filename:join(Dir, X) || X <- refac_misc:default_incls()],
    NewSearchPaths = SearchPaths ++ DefaultIncl,
    case refac_epp:parse_file(FileName, NewSearchPaths, [], TabWidth, refac_util:file_format(FileName)) of
      {ok, _, {MDefs, MUses}} ->
	  lists:usort([Name || {{_, Name}, _Def} <- MDefs ++ MUses]);
      _ -> {error, "The current file does not compile!"}
    end.
need_bracket(Toks, Exprs) ->
    case Exprs of
      [E] ->
	  {Start, End} = refac_misc:get_start_end_loc(E),
	  Toks1 = lists:reverse(lists:takewhile(
				  fun (B) ->
					  element(2, B) =/= Start
				  end, Toks)),
	  Toks2 = lists:dropwhile(fun (B) ->
					  case B of
					    {whitespace, _, _} -> true;
					    _ -> false
					  end
				  end, Toks1),
	  Toks3 = lists:dropwhile(fun (B) ->
					  element(2, B) =< End orelse
					    element(1, B) == whitespace
				  end, Toks),
	  case Toks2 of
	    [{'(', _}| _] ->
		case Toks3 of
		  [{')', _}| _] ->
		      true;
		  _ -> false
		end;
	    _ -> false
	  end;
      _ -> false
    end.

collect_names_to_avoid(AnnAST) ->
    lists:append([collect_names_to_avoid_1(F)||F<-refac_syntax:form_list_elements(AnnAST),
					       refac_syntax:type(F)==attribute]).

collect_names_to_avoid_1(F) ->
    ArrName =refac_syntax:atom_value(refac_syntax:attribute_name(F)),
    case lists:member(ArrName, [ifdef, ifndef]) of
	true ->
	    Args =refac_syntax:attribute_arguments(F),
	    [list_to_atom(refac_prettypr:format(A))||A<-Args];
	_ -> []
    end.
    


return_refac_result(FileName, AnnAST, Editor, Cmd) ->
    case Editor of
      emacs ->
	  refac_util:write_refactored_files_for_preview([{{FileName, FileName}, AnnAST}], Cmd),
	  {ok, [FileName]};
      eclipse ->
	  Src = refac_prettypr:print_ast(refac_util:file_format(FileName), AnnAST),
	  Res = [{FileName, FileName, Src}],
	  {ok, Res}
    end.
