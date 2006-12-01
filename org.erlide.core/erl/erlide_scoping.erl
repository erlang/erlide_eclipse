%%% ******************************************************************************
%%%  Copyright (c) 2004 Vlad Dumitrescu and others.
%%%  All rights reserved. This program and the accompanying materials
%%%  are made available under the terms of the Eclipse Public License v1.0
%%%  which accompanies this distribution, and is available at
%%%  http://www.eclipse.org/legal/epl-v10.html
%%%
%%%  Contributors:
%%%      Vlad Dumitrescu
%%% ******************************************************************************/
%%
-module(erlide_scoping).

-export([bar/0, ok/1]).

-include_lib("stdlib/include/erl_bits.hrl").


bar() ->
    {ok, B} = file:read_file("cc.erl"),
    S = binary_to_list(B),
    {ok, T, _} = erl_scan:string(S),
    {ok, Code} = parse(T),

    forms(Code).


ok({ok, R}) ->
    R;
ok(X) ->
    X.

parse(Toks) ->
    Parts = split_dot(Toks),
    Fun = fun(E) ->
		  case erl_parse:parse(E) of
		      {ok, X} ->
			  X;
		      Err ->
			  Err
		  end
	  end,
    Res = lists:map(Fun, Parts),
    {ok, Res}.

split_dot(L) ->
    split_dot(L, [], []).

split_dot([], R, []) ->
    lists:reverse(R);
split_dot([], R, V) ->
    lists:reverse([V|R]);
split_dot([{eof}|T], R, V) ->
    split_dot(T, R, V);
split_dot([{dot, _}=H|T], R, V) ->
    split_dot(T, [lists:reverse([H|V])|R], []);
split_dot([H|T], R, V) ->
    split_dot(T, R, [H|V]).

forms(Forms) ->
    {NewTree, Res} = xform(Forms),
    [erl_syntax:revert(T) || T <- lists:flatten(NewTree)],
    compact(lists:reverse(Res)).

xform(Forms) ->
    Bef = fun(function, Form, _) ->
		  %%io:format("++ > ~p~n", [erl_syntax_lib:analyze_function(Form)]),
                  {Form, erl_syntax_lib:analyze_function(Form), []};
	     (fun_expr, Form, _) ->
		  %%io:format("  ++ > fun~n", []),
                  {Form, 'fun', []};
             (_, Form, State) ->
                  {Form, State, []}
          end,
    Aft = fun(variable, Form, _Context, _SubAcc, Acc) ->
		  %%io:format("** var = ~p~n", [Form]),
                  {Form, [Form|Acc]};
	     (clause, Form, _Context, SubAcc, Acc) ->
		  %%io:format("  !! ~p -> (~p)~n", [Context, SubAcc]),
		  {Form, lists:flatten([SubAcc|Acc])};
	     (function, Form, _Context, SubAcc, Acc) ->
		  {Form, [{erl_syntax_lib:analyze_function(Form), SubAcc} | Acc]};
	     (fun_expr, Form, _Context, SubAcc, Acc) ->
		  {Form, [{'fun', lists:flatten([SubAcc|Acc])}]};
             (_, Form, _Context, SubAcc, Acc) ->
                  {Form, lists:flatten([SubAcc|Acc])}
          end,
    transform(Forms, Bef, Aft, top, []).

transform(Forms, Before, After, Context, Acc) ->
    F1 = fun(Form, Acc0) ->
		 Type = erl_syntax:type(Form),
		 {Form1, Context1, InitSubAcc} =
		     try Before(Type, Form, Context)
		     catch
			 error:Reason ->
			     error(Reason, 'before', Before,
				   [{type, Type},
				    {context, Context},
				    {acc, Acc},
				    {form, Form}])
		     end,
		 {Form2, SubAcc2} =
		     case erl_syntax:subtrees(Form1) of
			 [] ->
			     {Form1, InitSubAcc};
			 List ->
			     {NewList, NewSubAcc} =
				 transform(List, Before, After, Context1, InitSubAcc),
			     NewForm = erl_syntax:update_tree(Form,NewList),
			     {NewForm, NewSubAcc}
		     end,
		 Type2 = erl_syntax:type(Form2),
		 {_Form3, _Acc3} =
		     try After(Type2, Form2, Context, SubAcc2, Acc0)
		     catch
			 error:Reason2 ->
			     error(Reason2, 'after', After,
				   [{type, Type2},
				    {context, Context},
				    {sub_acc, SubAcc2},
				    {acc, Acc0},
				    {form, Form2}])
		     end
	 end,
    F2 = fun(List, St) when is_list(List) ->
                 lists:mapfoldl(F1, St, List);
            (Form, St) ->
                 F1(Form, St)
         end,
    lists:mapfoldl(F2, Acc, Forms).

error(Reason, BeforeOrAfter, Fun, Info) ->
    Fmt = lists:flatten(
            ["*** ERROR in transform function:~n"
             "*** Reason     = ~p~n"
             "*** applying ~w fun (~p)~n",
             ["*** ~10w = ~p~n" || _ <- Info]]),
    Args = [Reason, BeforeOrAfter, Fun |
            lists:foldr(
              fun({K,V}, Acc) ->
                      [K, V | Acc]
              end, [], Info)],
    io:format(Fmt, Args),
    erlang:error(Reason).

compact(L) when is_list(L) ->
    [compact(E) || E <- L];
compact({{F, A}, L}) ->
    {{F, A}, compact_1(L)}.

compact_1(L1) ->
    Fun = fun({var, L, N}, Acc) ->
		  case lists:keysearch(N, 1, Acc) of
		      {value, {N, Ls}} ->
			  [{N, [L|Ls]}|lists:keydelete(N, 1, Acc)];
		      _ ->
			  [{N, [L]}|Acc]
		  end;
	     ({'fun', L}, Acc)  ->
		  [{'fun', compact_1(L)}|Acc]
	  end,
    lists:sort(lists:foldl(Fun, [], L1)).
