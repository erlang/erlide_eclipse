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
%%	      {error, string()} | {ok, string()}).
new_macro(FileName, Start, End, NewMacroName, SearchPaths, TabWidth) ->
    new_macro(FileName, Start, End, NewMacroName, SearchPaths, TabWidth, emacs).

%%-spec(new_macro_eclipse/6::(filename(), pos(), pos(), string(), [dir()], integer()) ->
%%	     {error, string()} | {ok, [{filename(), filename(), string()}]}).
new_macro_eclipse(FileName, Start, End, NewMacroName, SearchPaths, TabWidth) ->
    new_macro(FileName, Start, End, NewMacroName, SearchPaths, TabWidth, eclipse).


new_macro(FileName, Start={SLine, SCol}, End={ELine, ECol}, NewMacroName, SearchPaths, TabWidth, Editor) ->
    ?wrangler_io("\nCMD: ~p:new_macro(~p, {~p,~p}, {~p,~p}, ~p, ~p,~p).\n", 
				 [?MODULE, FileName, SLine, SCol, ELine, ECol, NewMacroName, SearchPaths, TabWidth]),
    Cmd = "CMD: " ++ atom_to_list(?MODULE) ++ ":new_macro(" ++ "\"" ++
	FileName ++ "\", {" ++ integer_to_list(SLine) ++", " ++ integer_to_list(SCol) ++ "},"++
	"{" ++ integer_to_list(ELine) ++ ", " ++ integer_to_list(ECol) ++ "},"  ++ "\"" ++ NewMacroName ++ "\","
	++ "[" ++ refac_util:format_search_paths(SearchPaths) ++ "]," ++ integer_to_list(TabWidth) ++ ").",
     case pre_cond_check(FileName, NewMacroName, Start, End, SearchPaths, TabWidth) of
		 {ok, AnnAST, Sel} ->
			 AnnAST1 = do_intro_new_macro(AnnAST, NewMacroName, Sel),
			 case Editor of
				 emacs ->
					 refac_util:write_refactored_files_for_preview([{{FileName, FileName}, AnnAST1}], Cmd),
					 {ok, [FileName]};
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
		    Sel = refac_util:pos_to_expr_or_pat_list(AnnAST, Start, End),
		    case Sel of
			 [] -> {error, "You have not selected a sequence of expressions/patterns!"};
			 _ ->
			      {ok, AnnAST, [hd(Sel)]}
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



%%-spec(replace_expr_with_macro/3::(syntaxTree(), {[syntaxTree()], pos(), pos()}, syntaxTree()) ->
%%	     syntaxTree()).
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
    

existing_macros(FileName, SearchPaths, TabWidth) -> 
    Dir = filename:dirname(FileName),
    DefaultIncl = [filename:join(Dir, X) || X <-refac_util:default_incls()],
    NewSearchPaths= SearchPaths++DefaultIncl,
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
  
