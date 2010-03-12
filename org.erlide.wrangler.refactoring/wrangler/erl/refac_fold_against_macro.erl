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
%% Refactoring: Fold expressions/patterns again a macro definition.
%%
%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk

-module(refac_fold_against_macro).

-export([fold_against_macro/5, fold_against_macro_1/5, 
	 fold_against_macro_eclipse/5, fold_against_macro_1_eclipse/5]).


-export([fold_against_macro/7]).

-include("../include/wrangler.hrl").

%% =============================================================================================
%% @doc This refactoring replaces instances of the macro body by the corresponding macro head 
%% with necessary paramter substitutions.

%% <p> To apply this refactoring, move the cursor to the macro definition against whith expressions/patterns
%% will be folded, then select <em> Fold Against Macro Definition </em> from the <em> Refactor </em>
%% menu, after that the refactor will search the current module for expressions which are instances
%% of the macro body.

%% <p> If no candiate expressions/patterns have been found, a message will be given, and the refactoring
%% process finished; otherwise, Wrangler will go through the candidates found one by one asking
%% the user whether she/he wants to replace the candidate with an macro application. If the user
%% answers 'yes' to a instance, that instance will be replaced by macro application, otherwise 
%% it will remain unchaged.

%% <p> NOTE: 1) The current implmentation of this refactoring does not hanld condition compilatiion yet.
%% 2) This refactoring can only fold expressions/patterns again macro definitions that are wellformed.

%%=============================================================================================

-spec(fold_against_macro/5::(filename(), integer(), integer(), [dir()], integer()) ->
	 {error, string()} | {ok, [{integer(), integer(), integer(), integer(), 
				    syntaxTree(), syntaxTree()}], string()}).
fold_against_macro(FileName, Line, Col, SearchPaths, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:fold_against_macro(~p, ~p,~p, ~p,~p).\n",
		 [?MODULE, FileName, Line, Col, SearchPaths, TabWidth]),
    Cmd = "CMD: " ++ atom_to_list(?MODULE) ++ ":fold_against_macro(" ++ "\"" ++
	    FileName ++ "\", " ++ integer_to_list(Line) ++
	      ", " ++ integer_to_list(Col) ++ ", "
		++ "[" ++ refac_misc:format_search_paths(SearchPaths) ++ "]," ++ integer_to_list(TabWidth) ++ ").",
    fold_against_macro(FileName, Line, Col, SearchPaths, TabWidth, emacs, Cmd).


fold_against_macro_eclipse(FileName, Line, Col,  SearchPaths, TabWidth) ->
    fold_against_macro(FileName, Line, Col, SearchPaths, TabWidth, eclipse, "").

fold_against_macro(FileName, Line, Col, SearchPaths, TabWidth, Editor, Cmd) ->
    {ok, {AnnAST, _Info}} = refac_util:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    case pos_to_macro_define(AnnAST, {Line, Col}) of 
	{ok, MacroDef} ->
	    Candidates = search_candidate_exprs(AnnAST, MacroDef),
	    case Candidates of 
		[] -> {error, "No syntax phrase that is suitable for folding against the selected macro definition has been found!"};
		_  -> case Editor of 
			  emacs ->
			      MacroDef1 = binary_to_list(term_to_binary(MacroDef)),
			      Regions  = lists:map(fun({{{StartLine, StartCol}, {EndLine, EndCol}}, MacroApp}) ->
							   MacroApp1 = binary_to_list(term_to_binary(MacroApp)),
							   {StartLine, StartCol, EndLine, EndCol, MacroApp1, MacroDef1} end,
						   Candidates),
			      {ok, Regions, Cmd};
			  eclipse ->
			      {ok, Candidates, MacroDef}
		      end
	    end;
	{error, _} ->
	    {error, "You have not selected a macro definition, or the selected macro definition does not have a syntactially well-formed body!"}
    end.



-spec(fold_against_macro_1_eclipse/5::(filename(), [{{{integer(), integer()}, {integer(), integer()}}, syntaxTree()}], syntaxTree(), 
				       [dir()], integer()) ->
					     {ok, [{filename(), filename(), string()}]}).
fold_against_macro_1_eclipse(FileName, CandidatesToFold, MacroDef, SearchPaths, TabWidth) ->
    {ok, {AnnAST, _Info}} = refac_util:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    CandidatesToFold1 =[{StartLine, StartCol, EndLine, EndCol, MacroApp, MacroDef} ||
			   {{{StartLine, StartCol}, {EndLine, EndCol}}, MacroApp} <- CandidatesToFold],
    AnnAST1 = fold_against_macro_1_1(AnnAST, CandidatesToFold1),
    Src = refac_prettypr:print_ast(refac_util:file_format(FileName), AnnAST1),
    Res = [{FileName, FileName, Src}],
    {ok, Res}.
 

-spec(fold_against_macro_1/5::(filename(), [{integer(), integer(), integer(), integer(), syntaxTree(), syntaxTree()}],
			       [dir()], integer(), string()) ->
				    {ok, [filename()]}).
fold_against_macro_1(FileName, CandidatesToFold, SearchPaths, TabWidth, Cmd) ->
    {ok, {AnnAST, _Info}} = refac_util:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    AnnAST1 = fold_against_macro_1_1(AnnAST, CandidatesToFold),
    refac_util:write_refactored_files_for_preview([{{FileName, FileName}, AnnAST1}], Cmd),
    {ok, [FileName]}.
   
fold_against_macro_1_1(AnnAST, []) ->
    AnnAST;
fold_against_macro_1_1(AnnAST, [{StartLine, StartCol, EndLine, EndCol,MacroApp0, MacroDef0}|Tail] ) ->
    MacroApp = binary_to_term(list_to_binary(MacroApp0)),
    MacroDef = binary_to_term(list_to_binary(MacroDef0)),
    Args = refac_syntax:attribute_arguments(MacroDef),
    MacroBody = tl(Args),
    TMacroApp=transform(MacroApp),
    AnnAST1 = refac_new_macro:replace_expr_with_macro(AnnAST, {MacroBody, {StartLine, StartCol}, {EndLine, EndCol}}, TMacroApp),
    fold_against_macro_1_1(AnnAST1, Tail).


search_candidate_exprs(AnnAST, MacroDef) ->
    Args = refac_syntax:attribute_arguments(MacroDef),
    MacroHead = refac_misc:ghead("refac_fold_against_macro:search_candiate_exprs", Args),
    MacroParNames = case refac_syntax:type(MacroHead) of
		      application ->
			  Pars = refac_syntax:application_arguments(MacroHead),
			  [refac_syntax:variable_name(A) || A <- Pars];
		      _ -> []
		    end,
    MacroBody = tl(Args),
    Res = do_search_candidate_exprs(AnnAST, MacroBody, MacroParNames),
    case MacroBody of
      [] ->
	  [];
      _ ->
	  [{Range, make_macro_app(MacroHead, Subst)} || {Range, Subst} <- Res]
    end.



do_search_candidate_exprs(AnnAST,MacroBody, MacroParNames) ->
    case length(MacroBody) of 
	1 ->
	    do_search_candidate_exprs_1(AnnAST, hd(MacroBody), MacroParNames);
	_ ->
	    do_search_candidate_exprs_2(AnnAST,MacroBody, MacroParNames)
    end.


do_search_candidate_exprs_1(AnnAST, MacroBody, MacroParNames) ->
    Fun = fun (T, S) ->
		  case is_expr_or_pat(T) of
		    true ->
			case T =/= MacroBody of
			  true ->
			      case expr_unification(MacroBody, T, MacroParNames) of
				{true, Subst} ->
				    S ++ [{refac_misc:get_start_end_loc(T), Subst}];
				_ -> S
			      end;
			  _ -> S
			end;
		    false ->
			S
		  end
	  end,
    refac_syntax_lib:fold(Fun, [], AnnAST).

do_search_candidate_exprs_2(AnnAST, MacroBody, MacroParNames) ->
    Len = length(MacroBody),
    Fun = fun (T, S) ->
		  case refac_syntax:type(T) of
		      clause ->
			  Exprs = refac_syntax:clause_body(T),
			  CandidateExprs = get_candidate_exprs(Exprs, Len, MacroBody, MacroParNames),
			  S ++ lists:filter(fun (C) -> C =/= false end, CandidateExprs);
		      block_expr ->
			  Exprs = refac_syntax:block_expr_body(T),
			  CandidateExprs = get_candidate_exprs(Exprs, Len, MacroBody, MacroParNames),
			  S ++ lists:filter(fun (C) -> C =/= false end, CandidateExprs);
		      appication ->
			  Args = refac_syntax:application_arguments(T),
			  CandidateExprs = get_candidate_exprs(Args, Len, MacroBody, MacroParNames),
			  S ++ lists:filter(fun (C) -> C =/= false end, CandidateExprs);
		      tuple ->
			  Elems = refac_syntax:tuple_elements(T),
			  CandidateExprs = get_candidate_exprs(Elems, Len, MacroBody, MacroParNames),
			  S ++ lists:filter(fun (C) -> C =/= false end, CandidateExprs);
		      _ -> S
		  end
	  end,
    refac_syntax_lib:fold(Fun, [], AnnAST).

get_candidate_exprs(Exprs, Len, MacroBody, MacroParNames) ->
    SubExprs = sublists(Exprs, Len),
    lists:map(fun (E) ->
		      case MacroBody =/= E of
			true ->
			    case expr_unification(MacroBody, E, MacroParNames) of
			      {true, Subst} ->
				  {StartLoc1, _EndLoc1} = refac_misc:get_start_end_loc(hd(E)),
				  {_StartLoc2, EndLoc2} = refac_misc:get_start_end_loc(lists:last(E)),
				  {{StartLoc1, EndLoc2}, Subst};
			      _ -> false
			    end;
			_ -> false
		      end
	      end,
	      SubExprs).



make_macro_app(MacroHead, Subst) ->
    case refac_syntax:type(MacroHead) of
      application ->
	  Op = refac_syntax:application_operator(MacroHead),
	  Args1 = refac_syntax:application_arguments(MacroHead),
	  Args = [refac_syntax:variable_name(A) || A <- Args1],
	  Pars = lists:map(fun (P) -> case lists:keysearch(P, 1, Subst) of
					{value, {P, Par}} ->
					    refac_misc:reset_attrs(Par);
					_ -> refac_syntax:atom(undefined)
				      end
			   end, Args),
	  
	  refac_syntax:macro(Op, Pars);
      _ ->
	  refac_syntax:macro(MacroHead)
    end.

%%=====================================================================================
expr_unification(Exp1, Exp2, MacroParNames) ->
    case unification:expr_unification(Exp1, Exp2) of
      false -> false;
      {true, Subst} ->
	  Subst1 = lists:usort([{E1, refac_prettypr:format(E2)}||{E1,E2}<-Subst]),
	  Vars = [E1||{E1, _E2}<-Subst1],
	  SVars = lists:usort(Vars),
	  case length(Vars) == length(SVars) of
	    true ->
		case lists:subtract(SVars, MacroParNames) of
		  [] -> {true, Subst};
		  Res ->
		      ResSubst = lists:filter(fun ({E1, _E2}) ->
						      lists:member(E1, Res)
					      end, Subst),
		      case lists:all(fun ({E1, E2}) ->
					     list_to_atom(refac_prettypr:format(E2)) == E1
				     end, ResSubst)
			  of
			true -> {true, Subst};
			_ -> false
		      end
		end;
	    _ -> false
	  end
    end.

sublists(List, Len) ->
    L = length(List),
    case Len > length(List) of
	true ->
	    [];
	_ -> [lists:sublist(List, Index, Len)|| Index<-lists:seq(1, L-Len+1)]
    end.

%%==========================================================================
pos_to_macro_define(AnnAST, Pos) ->
    case
      ast_traverse_api:once_tdTU(fun pos_to_macro_define_1/2, AnnAST, Pos)
	of
      {_, false} ->
	  {error, none};
      {R, true} ->
	  {ok, R}
    end.

pos_to_macro_define_1(Node, Pos) ->
    case refac_syntax:type(Node) of
      attribute -> case refac_syntax:atom_value(refac_syntax:attribute_name(Node)) of
		     define ->
			 {S, E} = refac_misc:get_start_end_loc(Node),
			 case (S =< Pos) and (Pos =< E) of
			   true ->
			       {Node, true};
			   _ -> {[], false}
			 end;
		     _ -> {[], false}
		   end;
      _ ->
	  {[], false}
    end.

is_expr_or_pat(Node) ->
    refac_misc:is_expr(Node) orelse refac_misc:is_pattern(Node).

%%==========================================================================

%% Distel seems turn {wrapper, nil, ...} into {wrapper, [], ...}
%% We have to turn it back.
transform(Node) ->
    ast_traverse_api:full_buTP(fun (T, _Others) ->
				       case T of
					 {wrapper, [], A, {[], B}} ->
					     {wrapper, nil, A, {nil, B}};
					 _ -> T
				       end
			       end, Node, {}).
