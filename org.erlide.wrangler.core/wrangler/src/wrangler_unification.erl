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
%% =====================================================================
%% @hidden
%% @private
-module(wrangler_unification).

-export([expr_unification/2, expr_unification_extended/2, expr_match/2]).


-include("../include/wrangler_internal.hrl").


%%TODO: should expr_unification(Exp1, Exp2) == expr_unification(Exp2, Exp1)?

expr_match(Exp1, Exp2) ->
    expr_unification(Exp1, Exp2, syntax, false).

-spec(expr_unification/2::(syntaxTree()|[syntaxTree()], syntaxTree()|[syntaxTree()]) ->
				{true, [{atom(), syntaxTree()|[syntaxTree()]}]} | false).
expr_unification(Exp1, Exp2) ->
    expr_unification(Exp1, Exp2, syntax, true).
expr_unification_extended(Exp1, Exp2) ->
    expr_unification(Exp1, Exp2, semantics, true).

expr_unification(Exp1, Exp2, Type, CheckGen) ->
    case expr_unification_1(Exp1, Exp2, Type, CheckGen) of
	{true, Subst} ->
            Subst1 = lists:usort([{E1, format(E2)} || {E1, E2} <- Subst]),
	    Subst2 = lists:usort([E1 || {E1, _E2} <- Subst1]),
	    case length(Subst2) == length(Subst1) of
		true -> {true, Subst};
		_ ->
		    false
	    end;
	_ -> 
	    false
    end.

%% This algorithm should be extended to work with exprs from different modules.
expr_unification_1(Exp1, Exp2, Type, CheckGen) ->
    case {is_list(Exp1), is_list(Exp2)} of
      {true, true} ->   %% both are list of expressions
	    case length(Exp1) == length(Exp2) of
	    true ->
		    Res = [expr_unification(E1, E2, Type, CheckGen) || {E1, E2} <- lists:zip(Exp1, Exp2)],
                    Unifiable = not lists:member(false, [false || false <- Res]),
		    case Unifiable of
			true ->
			    {true, lists:usort(lists:append([S || {true, S} <- Res]))};
			_ -> false
		    end;
	      _ -> false
	  end;
      {false, false}  %% both are single expressions.
	             ->
	  T1 = wrangler_syntax:type(Exp1),
	  T2 = wrangler_syntax:type(Exp2),
	  case T1 == T2 of
              true ->
                  same_type_expr_unification(Exp1, Exp2, Type, CheckGen);
	      _ -> non_same_type_expr_unification(Exp1, Exp2, Type, CheckGen)
	  end;
      {true, false} %% Exp1 is a list, but Exp2 is not.
	            ->
	  false;
      {false, true}  %% Exp1 is a single expression, but Exp2 is not.
                    ->
          false      %% an actual parameter cannot be a list of expressions.
    end.

same_type_expr_unification(Exp1, Exp2, Type,CheckGen) ->
    T1 = wrangler_syntax:type(Exp1),
    case T1 of
	variable ->
            Exp1Name = wrangler_syntax:variable_name(Exp1),
	    Exp2Name = wrangler_syntax:variable_name(Exp2),
            case is_macro_name(Exp1) andalso is_macro_name(Exp2) of
                true ->
                    if Exp1Name==Exp2Name ->
                            {true, []};
                       true -> false
                    end;
		_ -> {true, [{Exp1Name, rm_comments(Exp2)}]}
	    end;
	atom ->
	    case wrangler_syntax:atom_value(Exp1) == wrangler_syntax:atom_value(Exp2) of
		true ->
		    Ann1 = wrangler_syntax:get_ann(Exp1),
		    Ann2 = wrangler_syntax:get_ann(Exp2),
		    case lists:keysearch(fun_def,1,Ann1) of
			{value, {fun_def, {M, F, A, _, _}}} ->
			    case lists:keysearch(fun_def,1,Ann2) of
				{value, {fun_def, {M1,F1,A1,_,_}}} ->
				    case {M, F, A}=={M1, F1, A1} of
					true -> {true, []};
					false ->
					    false
				    end;
				false ->
				    false
			    end;
			false ->
			    case lists:keysearch(fun_def, 1, Ann2) of
				{value, _} ->
				    false;
				false ->
				    {true, []}
			    end
		    end;
		_ -> false
	    end;
	operator ->
	    case wrangler_syntax:operator_name(Exp1) == wrangler_syntax:operator_name(Exp2) of
		true -> {true, []};
		_ -> false
	    end;
	char ->
	    case wrangler_syntax:char_value(Exp1) == wrangler_syntax:char_value(Exp2) of
		true -> {true, []};
		_ -> false
	    end;
	integer ->
	    case wrangler_syntax:integer_value(Exp1) == wrangler_syntax:integer_value(Exp2) of
		true -> {true, []};
		_ -> false
	    end;
	string ->
	    case wrangler_syntax:string_value(Exp1) == wrangler_syntax:string_value(Exp2) of
		true -> {true, []};
		_ -> false
	    end;
	float ->
	    case wrangler_syntax:float_value(Exp1) == wrangler_syntax:float_value(Exp2) of
		true -> {true, []}
	    end;
	underscore -> {true, []};
	nil -> {true, []};
	application when Type==semantics ->
	    Op = wrangler_syntax:application_operator(Exp1),
	    Args1 = wrangler_syntax:application_arguments(Exp1),
	    Args2 = wrangler_syntax:application_arguments(Exp2),
	    case wrangler_syntax:type(Op) == variable andalso Args1 == [] andalso
		           Args2 /= [] andalso api_refac:free_vars(Exp2) == []
	    of
		true ->
		    OpName = wrangler_syntax:variable_name(Op),
		    {true, [{OpName, wrangler_syntax:fun_expr([wrangler_syntax:clause([], none, [Exp2])])}]};
		_ ->
		    SubTrees1 = erl_syntax:subtrees(Exp1),
		    SubTrees2 = erl_syntax:subtrees(Exp2),
		    case length(SubTrees1) == length(SubTrees2) of
			true ->
			    expr_unification(SubTrees1, SubTrees2, Type, CheckGen);
			_ -> false
		    end
	    end;
	_ ->
	    SubTrees1 = erl_syntax:subtrees(Exp1),
	    SubTrees2 = erl_syntax:subtrees(Exp2),
            case length(SubTrees1) == length(SubTrees2) of
		true ->
                    expr_unification(SubTrees1, SubTrees2, Type, CheckGen);
		_ -> false
	    end
    end.

   
non_same_type_expr_unification(Exp1, Exp2,_Type, CheckGen) ->
    T1 = wrangler_syntax:type(Exp1),
    case T1 of
      variable ->
            case CheckGen andalso not (wrangler_code_search_utils:generalisable(Exp2)) of
                true ->
                    false;
                false ->
                    Exp2Ann = wrangler_syntax:get_ann(Exp2),
		    Exp1Name = wrangler_syntax:variable_name(Exp1),
		    case lists:keysearch(syntax_path, 1, Exp2Ann) of
                        {value, {syntax_path, application_operator}} ->
                            case lists:keysearch(fun_def, 1, Exp2Ann) of
                                {value, {fun_def, {_M, _N, A, _P1, _P2}}} ->
                                    {true, [{Exp1Name, rm_comments(
                                                         wrangler_syntax:implicit_fun(
                                                              Exp2, wrangler_syntax:integer(A)))}]};
                                _ -> 
                                    %% this is the function name part of a M:F. 
                                    {true, [{Exp1Name, rm_comments(Exp2)}]}
                            end;
                        _ ->
                            case wrangler_side_effect:has_side_effect(Exp2) of
                                false ->
                                    {true, [{Exp1Name, rm_comments(Exp2)}]};
                                _ ->
                                    {true, [{Exp1Name, rm_comments(Exp2)}]}
                                    %% C = refac_syntax:clause([],[], [rm_comments(Exp2)]),
                                    %% {true, [{Exp1Name, refac_syntax:fun_expr([C])}]}
                            end
                    end
            end;
      _ -> T2 = wrangler_syntax:type(Exp2),
	   case {T1, T2} == {atom, module_qualifier} orelse
                {T1,T2} == {module_qualifier, atom} of
	       true ->
		   Ann1=wrangler_syntax:get_ann(Exp1),
		   Ann2=wrangler_syntax:get_ann(Exp2),
		   case lists:keysearch(fun_def,1,Ann1) of
		       {value, {fun_def, {M, F, A, _, _}}} ->
			   case lists:keysearch(fun_def,1,Ann2) of
			       {value, {fun_def, {M1,F1,A1,_,_}}} ->
				   case {M, F, A} == {M1, F1, A1} of
				       true -> {true, []};
				       false ->
					   false
				   end;
			       false ->
				   false
			   end;
		       false ->
			   false
		   end;
	       _ -> false
	   end
    end.

rm_comments(Node) ->
    wrangler_syntax:remove_comments(Node).

format(Es)when is_list(Es) ->
    [wrangler_prettypr:format(wrangler_misc:reset_ann_and_pos(E))||E <- Es];
format(E) ->
    wrangler_prettypr:format(wrangler_misc:reset_ann_and_pos(E)).

is_macro_name(Exp) ->
    Ann = wrangler_syntax:get_ann(Exp),
    {value, {syntax_path, macro_name}} == 
        lists:keysearch(syntax_path, 1, Ann).
