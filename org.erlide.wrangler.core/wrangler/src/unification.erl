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
-module(unification).

-export([expr_unification/2]).


-include("../include/wrangler.hrl").

-spec(expr_unification/2::(syntaxTree(), syntaxTree()) ->
				{true, [{atom(), syntaxTree()}]} | false).
expr_unification(Exp1, Exp2) ->
    case expr_unification_1(Exp1, Exp2) of
	{true, Subst} ->
	    Subst1 = lists:usort([{E1, refac_prettypr:format(E2)} || {E1, E2} <- Subst]),
	    Subst2 = lists:usort([E1 || {E1, _E2} <- Subst1]),
	    case length(Subst2) == length(Subst1) of
		true -> {true, Subst};
		_ -> false
	    end;
	_ -> false
    end.

%% This algorithm should be extended to work with exprs from different modules.
expr_unification_1(Exp1, Exp2) ->
    case {is_list(Exp1), is_list(Exp2)} of
      {true, true} ->   %% both are list of expressions
	  case length(Exp1) == length(Exp2) of
	    true ->
		  Res = [expr_unification(E1, E2) || {E1, E2} <- lists:zip(Exp1, Exp2)],
		  Unifiable = not lists:member(false, [false || false <- Res]),
		  case Unifiable of
		      true ->
			  {true, lists:usort(lists:append([S || {true, S} <- Res]))};
		      _ -> false
		  end;
	      _ -> false
	  end;
	{false, false} ->  %% both are single expressions.
	    T1 = refac_syntax:type(Exp1),
	    T2 = refac_syntax:type(Exp2),
	    case T1 == T2 of
		true -> same_type_expr_unification(Exp1, Exp2);
		_ -> non_same_type_expr_unification(Exp1, Exp2)
	    end;
	{true, false} -> %% Exp1 is a list, but Exp2 is not.
	    false;
	{false, true} ->  %% Exp1 is a single expression, but Exp2 is not.
	    false      %% an actual parameter cannot be a list of expressions.
    end.

same_type_expr_unification(Exp1, Exp2) ->
    T1 = refac_syntax:type(Exp1),
    case T1 of
      variable ->
	  Exp1Ann = refac_syntax:get_ann(Exp1),
	  Exp2Ann = refac_syntax:get_ann(Exp2),
	  Exp1Name = refac_syntax:variable_name(Exp1),
	  Exp2Name = refac_syntax:variable_name(Exp2),
	  case lists:keysearch(category, 1, Exp1Ann) of
	    {value, {category, macro_name}} ->
		  case lists:keysearch(category, 1, Exp2Ann) of
		  {value, {category, macro_name}} ->
		      case Exp1Name of
			Exp2Name -> {true, []};
			_ -> false
		      end;
		  _ -> false
		end;
	    _ -> {true, [{Exp1Name, rm_commments(Exp2)}]}
	  end;
      atom ->
	    case refac_syntax:atom_value(Exp1) == refac_syntax:atom_value(Exp2) of
		true ->
		    Ann1=refac_syntax:get_ann(Exp1),
		    Ann2=refac_syntax:get_ann(Exp2),
		    case lists:keysearch(fun_def,1,Ann1) of
			{value, {fun_def, {M,F, A, _, _}}} ->
			  case lists:keysearch(fun_def,1,Ann2) of
			      {value, {fun_def, {M1,F1,A1, _,_}}} ->
				  case {M, F,A}=={M1, F1, A1} of
				      true-> {true, []};
				      false ->
					  false
				  end;
			      false->
				  false
			  end;
			false ->
			    case lists:keysearch(fun_def,1, Ann2) of
				{value, _} ->
				    false;
				false ->
				  {true, []}
			    end
		    end;
		_ -> false
	  end;
      operator ->
	  case refac_syntax:operator_name(Exp1) == refac_syntax:operator_name(Exp2) of
	    true -> {true, []};
	    _ -> false
	  end;
      char ->
	  case refac_syntax:char_value(Exp1) == refac_syntax:char_value(Exp2) of
	    true -> {true, []};
	    _ -> false
	  end;
      integer ->
	  case refac_syntax:integer_value(Exp1) == refac_syntax:integer_value(Exp2) of
	    true -> {true, []};
	    _ -> false
	  end;
      string ->
	  case refac_syntax:string_value(Exp1) == refac_syntax:string_value(Exp2) of
	    true -> {true, []};
	    _ -> false
	  end;
      float ->
	  case refac_syntax:float_value(Exp1) == refac_syntax:float_value(Exp2) of
	    true -> {true, []}
	  end;
      underscore -> {true, []};
      nil -> {true, []};
      _ ->
	  SubTrees1 = erl_syntax:subtrees(Exp1),
	  SubTrees2 = erl_syntax:subtrees(Exp2),
	  case length(SubTrees1) == length(SubTrees2) of
	    true ->
		expr_unification(SubTrees1, SubTrees2);
	    _ -> false
	  end
    end.

non_same_type_expr_unification(Exp1, Exp2) ->
    T1 = refac_syntax:type(Exp1),
    case T1 of
      variable ->
	  case refac_misc:variable_replaceable(Exp2) of
	    false ->
		false;
	    true ->
		  Exp2Ann = refac_syntax:get_ann(Exp2),
		  Exp1Name = refac_syntax:variable_name(Exp1),
		case lists:keysearch(category, 1, Exp2Ann) of
		  {value, {category, application_op}} ->
		      case lists:keysearch(fun_def, 1, Exp2Ann) of
			{value, {fun_def, {_M, _N, A, _P1, _P2}}} ->
			    {true, [{Exp1Name, rm_commments(
						 refac_syntax:implicit_fun(
						   Exp2, refac_syntax:integer(A)))}]};
			_ -> %% this is the function name part of a M:F. 
			    {true, [{Exp1Name, rm_commments(Exp2)}]}
		      end;
		  _ ->
		      {true, [{Exp1Name, rm_commments(Exp2)}]}
		end
	  end;
      _ -> false
    end.

rm_commments(Node) ->
    refac_syntax:remove_comments(Node).
