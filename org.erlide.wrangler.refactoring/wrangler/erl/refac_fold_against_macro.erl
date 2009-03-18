%% ============================================================================================
%% Refactoring: Fold expressions/patterns again a macro definition.
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

-module(refac_fold_against_macro).

-export([fold_against_macro/5, fold_against_macro_1/9, fold_against_macro_eclipse/5]).


-export([fold_against_macro/6]).

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
	      {error, string()} | {ok, [{integer(), integer(), integer(), integer(), syntaxTree(), syntaxTree()}]}).

fold_against_macro(FileName, Line, Col,  SearchPaths, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:fold_aginst_macro(~p, ~p,~p, ~p,~p).\n", [?MODULE, FileName, Line, Col, SearchPaths, TabWidth]),
    fold_against_macro(FileName, Line, Col, SearchPaths, TabWidth, emacs).

fold_against_macro_eclipse(FileName, Line, Col,  SearchPaths, TabWidth) ->
    fold_against_macro(FileName, Line, Col, SearchPaths, TabWidth, eclipse).

fold_against_macro(FileName, Line, Col, SearchPaths, TabWidth, Editor) ->
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
			      {ok, Regions};
			  eclipse ->
			      {ok, Candidates}
		      end
	    end;
	{error, _} ->
	    {error, "You have not selected a macro definition, or the selected macro definition does not have a syntactially well-formed body!"}
    end.

-spec(fold_against_macro_1/9::(filename(), integer(), integer(), integer(), integer(), syntaxTree(), syntaxTree(), [dir()], integer()) ->
	     {ok, [{integer(), integer(), integer(), integer(), syntaxTree(), syntaxTree()}]}).
fold_against_macro_1(FileName, StartLine, StartCol, EndLine, EndCol, MacroApp1, MacroDef1, SearchPaths, TabWidth) ->
    MacroApp = binary_to_term(list_to_binary(MacroApp1)),
    MacroDef = binary_to_term(list_to_binary(MacroDef1)),
    {ok, {AnnAST, _Info}} = refac_util:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    Args = refac_syntax:attribute_arguments(MacroDef),
    MacroBody = tl(Args),
    TMacroApp=transform(MacroApp),
    AnnAST1 = refac_new_macro:replace_expr_with_macro(AnnAST, {MacroBody, {StartLine, StartCol}, {EndLine, EndCol}}, TMacroApp),
    refac_util:write_refactored_files([{{FileName, FileName}, AnnAST1}]),
    {ok, {AnnAST2, _Info2}} = refac_util:parse_annotate_file(FileName, true, SearchPaths, TabWidth),    
    Candidates = search_candidate_exprs(AnnAST2, MacroDef),
    Regions = [{StartLine1, StartCol1, EndLine1, EndCol1, MacroApp2, MacroDef}
	       || {{{StartLine1, StartCol1}, {EndLine1, EndCol1}}, MacroApp2} <- Candidates,
		  StartLine1 >= StartLine],
    {ok, Regions}.


search_candidate_exprs(AnnAST, MacroDef) ->
    Args = refac_syntax:attribute_arguments(MacroDef),
    MacroHead = refac_util:ghead("refac_fold_against_macro:search_candiate_exprs", Args),
    MacroParNames = case refac_syntax:type(MacroHead) of 
		   application ->
		       Pars = refac_syntax:application_arguments(MacroHead),
		       lists:map(fun(A) ->refac_syntax:variable_name(A) end, Pars);
		   _ -> []
	       end,	    
    MacroBody = tl(Args),
    Res = do_search_candidate_exprs(AnnAST, MacroBody, MacroParNames),
    case MacroBody of 
	[] ->[];
	_ -> lists:map(fun({Range, Subst}) ->
			       {Range, make_macro_app(MacroHead, Subst)}
		       end, Res)
    end.



do_search_candidate_exprs(AnnAST,MacroBody, MacroParNames) ->
    case length(MacroBody) of 
	1 ->
	    do_search_candidate_exprs_1(AnnAST, hd(MacroBody), MacroParNames);
	_ ->
	    do_search_candidate_exprs_2(AnnAST,MacroBody, MacroParNames)
    end.


do_search_candidate_exprs_1(AnnAST, MacroBody, MacroParNames) ->
    Fun = fun(T, S) ->
		  case is_expr_or_pat(T) of 
		      true ->
			  case T=/=MacroBody of 
			      true ->
				  case expr_unification(MacroBody, T, MacroParNames) of 
				      {true, Subst} ->
					  S ++ [{refac_util:get_range(T), Subst}];
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
				      {StartLoc1, _EndLoc1} = refac_util:get_range(hd(E)),
				      {_StartLoc2, EndLoc2} = refac_util:get_range(lists:last(E)),
				      {{StartLoc1, EndLoc2}, Subst};
				  _ -> false
			      end;
			  _ -> false
		      end
	      end,
	      SubExprs).



make_macro_app(MacroHead,Subst) ->
    case refac_syntax:type(MacroHead) of 
	application ->
	    Op  = refac_syntax:application_operator(MacroHead),
	    Args1 = refac_syntax:application_arguments(MacroHead),
	    Args= lists:map(fun(A) -> refac_syntax:variable_name(A) end, Args1),
	    Pars = lists:map(fun(P) -> case lists:keysearch(P, 1, Subst) of 
					   {value, {P, Par}} -> Par;
					   _ -> refac_syntax:atom(undefined)
				       end
			     end, Args),

	    refac_syntax:macro(Op, Pars); 
	_->
	    refac_syntax:macro(MacroHead)
    end.

%%=====================================================================================
expr_unification(Exp1, Exp2, MacroParNames) ->
    case refac_fold_expression:expr_unification(Exp1, Exp2) of
      false -> false;
      {true, Subst} ->
	  Subst1 = lists:usort(lists:map(fun ({E1, E2}) -> {E1, refac_prettypr:format(E2)} end, Subst)),
	  Vars = lists:map(fun ({E1, _E2}) -> E1 end, Subst1),
	  SVars = lists:usort(Vars),
	  case length(Vars) == length(SVars) of
	      true ->
		  case lists:subtract(SVars, MacroParNames) of
		      [] ->{true, Subst};
		      Res ->
			  ResSubst = lists:filter(fun({E1,_E2}) -> 
							  lists:member(E1, Res) 
						  end, Subst),
			  case lists:all(fun({E1,E2}) ->
						 list_to_atom(refac_prettypr:format(E2)) == E1
					 end, ResSubst) of 
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
    case refac_util:once_tdTU(fun pos_to_macro_define_1/2, AnnAST, Pos) of 
	{_, false} ->
	    {error, none};
	{R, true} ->
	    {ok, R}
    end.

pos_to_macro_define_1(Node, Pos) ->
    case refac_syntax:type(Node) of 
	attribute -> case refac_syntax:atom_value(refac_syntax:attribute_name(Node)) of
			 define ->
			     {S, E} = refac_util:get_range(Node),
			     case (S=<Pos) and (Pos =< E)  of 
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
    refac_util:is_expr(Node) orelse refac_util:is_pattern(Node).

%%==========================================================================

%% Distel seems turn {wrapper, nil, ...} into {wrapper, [], ...}
%% We have to turn it back.
transform(Node) ->
    refac_util:full_buTP(fun(T, _Others) ->
				 case T of 
				     {wrapper, [], A, {[], B}} ->
					 {wrapper, nil, A, {nil, B}};
				     _  -> T
				 end
			 end, Node, {}).
