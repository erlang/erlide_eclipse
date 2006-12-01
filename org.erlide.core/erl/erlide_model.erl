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
-module(erlide_model).

%% TODO? use ets to store the parsed content

-compile(export_all).

%-define(DEBUG, 1).

-include("../include/erlide.hrl").
-include("../include/erlide_scanner.hrl").


comments(String) ->
    case erlide_comment_scan:string(String) of
        L when is_list(L) ->
            {ok, [{Pos, lists:flatten(T)} || {Pos, _, _, T} <- L]};
        Err ->
            Err
    end.

%%%%%%%%%%%%%%%%%%%%%%%%

function_header_pattern() ->
    [{ws, "\n"}, atom, {ws}, '('].

attribute_pattern() ->
    ['-', {ws}, atom].

match_token({N, V}, #token{kind=N, value=V}) ->
    true;
match_token({N, V}, #token{kind=N, text=V}) ->
    true;
match_token(N, #token{kind=N}) when is_atom(N) ->
    true;
match_token(_, _) ->
    false.

match(P, L) ->
    match(P, L, []).

match([], L, Res) ->
    {lists:reverse(Res), L};
match(_, [], _Res) ->
    false;
match([{P}|Ps]=Pat, [H|T]=L, Res) ->
    case match_token(P, H) of
        true ->
            match(Pat, T, [H|Res]);
        false ->
            match(Ps, L, Res)
    end;
match([P|Pat], [H|T], Res) ->
    case match_token(P, H) of
        true ->
            match(Pat, T, [H|Res]);
        false ->
            false
    end.

find_match(P, L) ->
    find_match(P, L, []).

find_match(_, [], _Res) ->
    no_match;
find_match(P, [H|T]=L, Before) ->
    case match(P, L) of
        {X, Y} ->
            {lists:reverse(Before), X, Y};
        false ->
            find_match(P, T, [H|Before])
    end.

split(L) ->
    L1 = split_dot(L),
    L2 = lists:flatmap(fun split_attr/1, L1),
    L3 = [erlide_scanner:filter_ws(lists:flatten(X)) || X<-L2],
    L3.

split_dot(L) ->
    split_dot(L, [], []).

split_dot([], R, []) ->
    lists:reverse(R);
split_dot([], R, V) ->
    lists:reverse([V|R]);
split_dot([#token{kind=eof}|T], R, V) ->
    split_dot(T, R, V);
split_dot([#token{kind=dot}=H|T], R, V) ->
    split_dot(T, [lists:reverse([H|V])|R], []);
split_dot([H|T], R, V) ->
    split_dot(T, R, [H|V]).

split_attr(L) ->
    case find_match(attribute_pattern(), L) of
        no_match ->
            split_fun(L);
        _ ->
            [L]
    end.


split_fun(L) ->
    split_fun(L, []).

split_fun([], R) ->
    [lists:reverse(R)];
split_fun(L, R) ->
    case find_match(function_header_pattern(), L) of
        no_match ->
            lists:reverse([L|R]);
        {Pre, Hdr, Rest} ->
            {Rest1, Rest2} = split_arrow(Rest),
            split_fun(Rest2, [Hdr++Rest1, Pre|R])
    end.

split_arrow(L) ->
    split_arrow(L, []).

split_arrow([], R) ->
    {lists:reverse(R), []};
split_arrow([#token{kind='->'}=H|T], R) ->
    {lists:reverse([H|R]), T};
split_arrow([H|T], R) ->
    split_arrow(T, [H|R]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse(String, Name) ->
  TN = list_to_atom("_erlide_model_"++Name),
    erlide_scanner:create(TN),
    erlide_scanner:insertText(TN,1,String),
    Toks = erlide_scanner:getWsTokens(TN),
    erlide_scanner:destroy(TN),

    Toks1 = filter_comments(Toks),
    Parts = split(Toks1),
 % io:format("*> ~p~n", [Parts]),
    Fun = fun([]) ->
                [];
             (E) ->
                case erlide_parse:parse([erlide_scanner:revert_token(X) || X <- E]) of
                    {ok, X} ->
                        [X];
                    Err ->
                        [Err]
                end
              end,
    Res = lists:flatmap(Fun, Parts),
  Res1 = join_funs(Res),
    {ok, Res1}.

filter_comments(L) ->
    Fun = fun(#token{kind=comment}) -> false;
             (_) -> true
          end,
    lists:filter(Fun, L).

revert(L) when is_list(L) ->
    [revert(X) || X <-L];
revert(T) when is_tuple(T), element(1, T)==tree ->
    case catch erlide_syntax:revert(T) of
        {'EXIT', _} ->
            T;
        T1 ->
            T1
    end;
revert(X) ->
    X.

%%%%

get_exported(M, Prefix) when is_atom(M), is_list(Prefix) ->
    case catch M:module_info(exports) of
        {'EXIT', _} ->
            error;
        Val ->
            Fun = fun({N,_A}) -> lists:prefix(Prefix,atom_to_list(N)) end,
            lists:filter(Fun, Val)
         end.

join_funs(L) ->
    L1 = join_clauses(L ,[]),
    join_funs(L1, [], []).

join_clauses([], R) ->
    lists:reverse(R);
join_clauses([{clause_head, P1, H1, H2, H3},{clause_body, B}|T], R) ->
    join_clauses(T, [{clause, P1, H1, H2, H3, B}|R]);
join_clauses([{clause_head, P1, H1, H2, H3},{error, _}=B|T], R) ->
    join_clauses(T, [{clause, P1, H1, H2, H3, B}|R]);
join_clauses([H|T], R) ->
    join_clauses(T, [H|R]).

join_funs([], R, []) ->
    lists:reverse(R);
join_funs([], R, V) ->
    lists:reverse([build_fun(V)|R]);
join_funs([{clause, _, _, _, _, _}=C|T], R, []) ->
     join_funs(T, R, [C]);
join_funs([{clause, _, N, A1, _, _}=C|T], R, [{clause, _, N, A2, _, _}|_]=V) ->
     case length(A1) == length(A2) of
    true ->
       join_funs(T, R, [C|V]);
      false ->
         join_funs(T, [build_fun(V)|R], [C])
  end;
join_funs([{clause, _, _, _, _, _}=C|T], R, [{clause, _, _, _, _, _}|_]=V) ->
     join_funs(T, [build_fun(V)|R], [C]);
join_funs([H|T], R, []) ->
     join_funs(T, [H|R], []);
join_funs([H|T], R, V) ->
     join_funs(T, [H, build_fun(V)|R], []).

build_fun(L) ->
    L1 = lists:reverse(L),
    {clause, P1, N, AL, _, _} = hd(L1),
    A = length(AL),
    {function, P1, N, A, L1}.


