%% Author: jakob
%% Created: Mar 23, 2006
%% Description: TODO: Add description to erlide_open

-module(erlide_noparse).

%%
%% Exported Functions
%%

-export([parse/2]). 
-compile(export_all).

%%
%% Include files
%%

-include("erlide.hrl").
-include("erlide_scanner.hrl").

-record(function, {pos, name, arity, clauses, name_pos}).
-record(clause, {pos, name, args, guards, code, name_pos}).
-record(attribute, {pos, name, args}).
-record(other, {pos, name, tokens}).

parse(String, ModuleName) ->
    try
        Toks = scan(String, ModuleName),
        {UncommentToks, Comments} = extract_comments(Toks),
        F = split_after_dots(UncommentToks, [], []),
        C = split_clauses(F),
        Collected = [classify_and_collect(I) || I <- C],
        {ok, Collected, Comments}
    catch
        error:Reason ->
            {error, Reason}
    end.

classify_and_collect([C1 | _] = C) ->
    cac(check_class(C1), C).

cac(function, ClauseList) ->
    Clauses = [fix_clause(C) || C <- ClauseList],
    [#clause{pos=P, name=N, args=A, name_pos=NP} | _] = Clauses,
    Arity = erlide_text:guess_arity(A),
    #function{pos=P, name=N, arity=Arity, clauses=Clauses, name_pos=NP};
cac(attribute, [Attribute]) ->
    [_, #token{kind=atom, value=Name, line=Line,
               offset=Offset, length=Length},
     _, #token{value=Args} | _] = Attribute,
    #attribute{pos={{Line, Offset}, Length},
                name=Name, args=Args};
cac(other, [[#token{value=Name, line=Line,
                         offset=Offset, length=Length} | _] | _]) ->
    #other{pos={{Line, Offset}, Length}, name=Name}.

check_class([#token{kind = atom}, #token{kind = '('} | _]) ->
    function;
check_class([#token{kind = '-'}, #token{kind = atom} | _]) ->
    attribute;
check_class(_) ->
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

split_clauses(F) ->
    [split_clauses(I, [], []) || I <- F].

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

fix_clause([#token{kind=atom, value=Name, line=Line, offset=Offset, length=Length} | Rest]) ->
    #token{offset=LastOffset, length=LastLength} = lists:last(Rest),
    PosLength = LastOffset - Offset + LastLength,
    #clause{pos={{Line, Offset}, PosLength},
            name=Name, args=get_args(Rest), guards=get_guards(Rest), code=[],
            name_pos={{Line, Offset}, Length}}.

scan(String, Name) ->
    case erlide_scanner:isScanned(Name) of
        false ->
		    erlide_scanner:create(Name),
		    erlide_scanner:insertText(Name, 1, String);
        true ->
            ok
    end,
    erlide_scanner:getTokens(Name).

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

extract_comments(Tokens) ->
    extract_comments(Tokens, -1, [], []).

extract_comments([], _, TAcc, CAcc) ->
    {lists:reverse(TAcc), lists:reverse(CAcc)};
extract_comments([#token{kind=comment, offset=ONext, length=LNext, line=NNext,
                         value=VNext}
                  | Rest], NNext, TAcc,
                 [#token{kind=comment, offset=O, value=V}=C | CAcc]) ->
    NewComment = C#token{offset=O, length=ONext-O+LNext, value=V++"\n"++VNext},
    extract_comments(Rest, NNext+1, TAcc, [NewComment | CAcc]);
extract_comments([C = #token{kind=comment, line=N} | Rest], _, TAcc, CAcc) ->
    extract_comments(Rest, N+1, TAcc, [C | CAcc]);
extract_comments([T | Rest], _, TAcc, CAcc) ->
    extract_comments(Rest, -1, [T | TAcc], CAcc).























