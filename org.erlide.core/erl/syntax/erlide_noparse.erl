-module(erlide_noparse).

-compile(export_all).

-export([parse/2]).

-include("erlide.hrl").
-include("erlide_scanner.hrl").

-record(function, {pos, name, arity, clauses}).
-record(clause, {pos, name, args, guards, code}).
-record(attribute, {pos, name, args}).
-record(other, {pos, name, tokens}).

parse(String, Name) ->
    try
        {Toks, Comments} = scan(String, Name),
        F = split_after_dots(Toks, [], []),
        C = [split_clauses(I, [], []) || I <- F],
        {ok, [classify_and_collect(I) || I <- C], Comments}
    catch
        error:Reason ->
            {error, Reason}
    end.        

classify_and_collect([C1 | _] = C) ->
    cac(check_class(C1), C).

cac(function, ClauseList) ->
    Clauses = [#clause{pos={{Line, Offset}, Length},
                       name=Name, args=get_args(R), guards=get_guards(R), code=[]}
               || [#token{kind=atom, value=Name, line=Line,
                         offset=Offset, length=Length} | R] <- ClauseList],
    [#clause{pos=P, name=N, args=A} | _] = Clauses,
    Arity = erlide_text:guess_arity(A),
    #function{pos=P, name=N, arity=Arity, clauses=Clauses};
cac(attribute, [Attribute]) ->
    [_, #token{kind=atom, value=Name, line=Line,
               offset=Offset, length=Length},
     _, #token{value=Args} | _] = Attribute,
    #attribute{pos={{Line, Offset}, Length},
                name=Name, args=Args};
cac(other, [#token{value=Name, line=Line,
                         offset=Offset, length=Length} | _]) ->
    #other{pos={{Line, Offset}, Length}, name=Name}.

check_class([#token{kind = atom}, #token{kind = '('} | _]) ->
    function;
check_class([#token{kind = '-'}, #token{kind = atom} | _]) ->
    attribute;
check_class([_]) ->
    other.

get_args(T) ->
    get_between_pars(T).

get_guards(T) ->
    get_between(T, 'when', '->'). 

get_between_pars(T) ->
    get_between(T, '(', ')').


get_between([], _A, _B) ->
    [];
get_between([#token{kind = A} | Rest], A, B) ->
    get_between2(Rest, B, []);
get_between([_ | Rest], A, B) ->
    get_between(Rest, A, B).

get_between2([], _B, Acc) ->
    lists:reverse(Acc);
get_between2([#token{kind = B} | _], B, Acc) ->
    get_between2([], B, Acc);
get_between2([T | Rest], B, Acc) ->
    get_between2(Rest, B, [T | Acc]).

reverse2(L) ->
    lists:foldl(fun(E, Acc) ->
                        [lists:reverse(E) | Acc]
                end, [], L).

split_after_dots([], Acc, []) ->
    reverse2(Acc);
split_after_dots([], Acc, FunAcc) ->
    split_after_dots([], [FunAcc | Acc], []);
split_after_dots([#token{kind = eof} | _], Acc, FunAcc) ->
    split_after_dots([], Acc, FunAcc);
split_after_dots([T = #token{kind = dot} | TRest], Acc, FunAcc) ->
    split_after_dots(TRest, [[T | FunAcc] | Acc], []);
split_after_dots([T | TRest], Acc, FunAcc) ->
    split_after_dots(TRest, Acc, [T | FunAcc]).

check_clause([#token{kind = ';'} | Rest]) ->
    check_class(Rest) == function;
check_clause(_) ->
    false.

split_clauses([], Acc, []) ->
    reverse2(Acc);
split_clauses([], Acc, ClAcc) ->
    split_clauses([], [ClAcc | Acc], []);
split_clauses([T | TRest] = Tokens, Acc, ClAcc) ->
    case check_clause(Tokens) of
        false ->
            split_clauses(TRest, Acc, [T | ClAcc]);
        true ->
            split_clauses(TRest, [[T | ClAcc] | Acc], [])
    end.



scan(String, Name) ->
    TN = list_to_atom("_erlide_model_"++Name),
    erlide_scanner:create(TN),
    erlide_scanner:insertText(TN,1,String),
    Toks = erlide_scanner:getTokens(TN),
    erlide_scanner:destroy(TN),
    lists:partition(fun(#token{kind=comment}) -> false;
                             (_) -> true end, Toks).

%% %% fixa in line-offset i tokens
%% %% invariant: first-line-offset alltid =< tokenoffset
%% %% offset ut ett-baserat (som tokens)
%% fix_tokens_w_line_offset(Toks, S) ->
%%     L = lists:reverse(erlide_text:split_lines(S)),
%%     [{FirstLineOffset, _} | Rest] = L,
%%     fix_twlo(Toks, Rest, FirstLineOffset, []).

%% fix_twlo([], _, _, Acc) ->    
%%     lists:reverse(Acc);
%% fix_twlo([T = #token{offset=TOffset} | TRest], [], CurLOffset, Acc) ->
%%     fix_twlo(TRest, [], CurLOffset, [{TOffset - CurLOffset, T} | Acc]);
%% fix_twlo([#token{offset=TOffset} | _] = Tokens, [{LOffset, _} | LRest], _, Acc)
%%   when TOffset >= LOffset ->
%%     fix_twlo(Tokens, LRest, LOffset, Acc);
%% fix_twlo([T = #token{offset=TOffset} | TRest], Lines, CurLOffset, Acc) ->
%%     fix_twlo(TRest, Lines, CurLOffset, [{TOffset - CurLOffset, T} | Acc]).
