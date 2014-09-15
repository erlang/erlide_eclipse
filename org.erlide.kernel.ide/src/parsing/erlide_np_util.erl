%% Author: jakob
%% Created: 7 nov 2010
%% Description: TODO: Add description to erlide_np_util
-module(erlide_np_util).

%% -define(DEBUG, 1).

-include("erlide.hrl").
-include("erlide_noparse.hrl").
-include("erlide_token.hrl").

-export([extract_comments/1, split_after_dots/1, skip_to/2,
         get_between_outer_pars/3, compact_model/1, get_top_level_comments/2]).

%% @doc extract comments from tokens, and concatenate multiline comments to one token
-spec extract_comments([token()]) -> {[token()],[token()]}.
%%
extract_comments(Tokens) ->
    extract_comments(Tokens, -1, [], []).

extract_comments([], _, TAcc, CAcc) ->
    {lists:reverse(TAcc), lists:reverse(CAcc)};
extract_comments([#token{kind=comment, offset=ONext, length=LNext, line=NNext,
                         text=VNext}
                            | Rest], NNext, TAcc,
                 [#token{kind=comment, offset=O, text=V}=C | CAcc]) ->
    NewComment = C#token{offset=O, length=ONext-O+LNext, text = <<V/binary,$\n,VNext/binary>>,
                         last_line=NNext-1},
    extract_comments(Rest, NNext+1, TAcc, [NewComment | CAcc]);
extract_comments([C = #token{kind=comment, line=N} | Rest], _, TAcc, CAcc) ->
    extract_comments(Rest, N+1, TAcc, [C | CAcc]);
extract_comments([T | Rest], _, TAcc, CAcc) ->
    extract_comments(Rest, -1, [T | TAcc], CAcc).

%% @doc split tokens list at 'dot' tokens
-spec split_after_dots([token()]) -> [[token()]].
%%
split_after_dots(D) ->
    split_after_dots(D, [], []).

split_after_dots([], Acc, []) ->
    erlide_util:reverse2(Acc);
split_after_dots([], Acc, FunAcc) ->
    split_after_dots([], [FunAcc | Acc], []);
split_after_dots([#token{kind = eof} | _], Acc, FunAcc) ->
    split_after_dots([], Acc, FunAcc);
split_after_dots([T = #token{kind = dot} | TRest], Acc, FunAcc) ->
    split_after_dots(TRest, [[T | FunAcc] | Acc], []);
split_after_dots([T | TRest], Acc, FunAcc) ->
    split_after_dots(TRest, Acc, [T | FunAcc]).

%% @doc drop tokens until delimiter
-spec skip_to([token()], atom()) -> [token()].
%%
skip_to([], _Delim) ->
    [];
skip_to([#token{kind=Delim} | _] = L, Delim) ->
    L;
skip_to([_ | Rest], Delim) ->
    skip_to(Rest, Delim).

%%
-spec get_between_outer_pars([token()], atom(), atom()) -> [token()].
%%
get_between_outer_pars(T, L, R) ->
    case skip_to(T, L) of
        [] ->
            [];
        [_ | S] ->
            {RL, _Rest} = gbop(S, L, R),
            lists:reverse(tl(lists:reverse(RL)))
    end.

gbop([], _L, _R) ->
    {[], []};
gbop([eof | _], _L, _R) ->
    {[], []};
gbop([#token{kind=R}=T | Rest], _L, R) ->
    {[T], Rest};
gbop([#token{kind=L}=T | Rest], L, R) ->
    {R1, Rest1} = gbop(Rest, L, R),
    {R2, Rest2} = gbop(Rest1, L, R),
    {[T] ++ R1 ++ R2, Rest2};
gbop([T | Rest], L, R) ->
    {LR, Rest1} = gbop(Rest, L, R),
    {[T] ++ LR, Rest1}.

%% change model to more compact to miminize terms from erlang to java
-spec compact_model(model()) -> model().
%%
compact_model(#model{forms=Forms, comments=Comments}) ->
    FixedComments = compact_tokens(Comments),
    FixedForms = compact_forms(Forms),
    #model{forms=FixedForms, comments=FixedComments}.

compact_forms(Forms) ->
    [compact_form(Form) || Form <- Forms].

compact_tokens(Tokens) ->
    [compact_token(Token) || Token <- Tokens].

compact_token(#token{value=Value} = Token) when is_list(Value) ->
    Token#token{value=to_binary_with_unicode(Value)};
compact_token(#token{text=Text} = Token) when is_list(Text) ->
    Token#token{value=to_binary_with_unicode(Text)};
compact_token(Token) ->
    Token.

list_of_binaries(Args) when is_list(Args) ->
    [unicode:characters_to_binary(A) || A <- Args];
list_of_binaries(_) ->
    [].

compact_form(#function{clauses=Clauses, args=Args} = Function) ->
    Function#function{clauses=compact_forms(Clauses), args=list_of_binaries(Args)};
compact_form(#clause{head=Head, args=Args} = Clause) ->
    Clause#clause{head=to_binary_with_unicode(Head), args=list_of_binaries(Args)};
compact_form(Other) ->
    Other.

to_binary_with_unicode(Comment) when is_list(Comment) ->
    unicode:characters_to_binary(Comment);
to_binary_with_unicode(Other) ->
    Other.

%% @doc retrieve top level comments
-spec get_top_level_comments([tuple()], [token()]) -> [token()].
%%
get_top_level_comments(Forms, Comments) ->
    get_top_level_comments(Forms, Comments, []).

get_top_level_comments(_Forms, [], Acc) ->
    lists:reverse(Acc);
get_top_level_comments([], Comments, Acc) ->
    lists:reverse(Acc, Comments);
get_top_level_comments([Form | FormRest] = Forms,
                       [#token{offset=CommentOffset, length=CommentLength}=Comment | CommentRest] = Comments,
                       Acc) ->
    {{_Line, _LastLine, FormOffset}, FormLength} = get_form_pos(Form),
    case relative_pos(CommentOffset, CommentLength, FormOffset, FormLength) of
        within ->
            get_top_level_comments(FormRest, CommentRest, Acc);
        before ->
            get_top_level_comments(Forms, CommentRest, [Comment | Acc]);
        'after' ->
            get_top_level_comments(FormRest, Comments, Acc);
        overlapping ->
            get_top_level_comments(FormRest, CommentRest, Acc)
    end.

get_form_pos(#function{pos=Pos}) -> Pos;
get_form_pos(#attribute{pos=Pos}) -> Pos;
get_form_pos(#clause{pos=Pos}) -> Pos;
get_form_pos(#other{pos=Pos}) -> Pos.

relative_pos(Offset1, Length1, Offset2, Length2)
  when Offset1 >= Offset2, Offset1 + Length1 =< Offset2 + Length2 ->
    within;
relative_pos(Offset1, Length1, Offset2, _Length2)
  when Offset1+Length1 =< Offset2 ->
    before;
relative_pos(Offset1, _Length1, Offset2, Length2)
  when Offset1 >= Offset2+Length2 ->
    'after';
relative_pos(_, _, _, _) ->
    overlapping.
