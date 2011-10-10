%% Author: jakob
%% Created: 7 nov 2010
%% Description: TODO: Add description to erlide_np_util
-module(erlide_np_util).

%%
%% Include files
%%

%% -define(DEBUG, 1).
%% -define(IO_FORMAT_DEBUG, 1).

-include("erlide.hrl").
-include("erlide_noparse.hrl").
-include("erlide_scanner.hrl").

%%
%% Exported Functions
%%
-export([extract_comments/1, get_function_comments/2, split_after_dots/1, skip_to/2,
	 get_between_outer_pars/3]).

%%
%% API Functions
%%

%% @spec (Tokens::tokens()) -> {tokens(), tokens()}
%% @type tokens() = [#token]
%%
%% @doc extract comments from tokens, and concatenate multiline comments to one token

extract_comments(Tokens) ->
    extract_comments(Tokens, -1, [], []).

get_function_comments(Forms, Comments0) ->
    Comments = get_top_level_comments(Forms, Comments0),
    lists:map(fun(#function{} = F) ->
		      get_function_comment(F, Comments);
		 (Other) ->
		      Other
	      end, Forms).

get_top_level_comments(Forms, Comments) ->
    get_top_level_comments(Forms, Comments, []).

get_top_level_comments(_Forms, [], Acc) ->
    lists:reverse(Acc);
get_top_level_comments([], _Comments, Acc) ->
    lists:reverse(Acc);
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

split_after_dots(D) ->
    split_after_dots(D, [], []).

skip_to([], _Delim) ->
    [];
skip_to([#token{kind=Delim} | _] = L, Delim) ->
    L;
skip_to([_ | Rest], Delim) ->
    skip_to(Rest, Delim).

get_between_outer_pars(T, L, R) ->
    case skip_to(T, L) of
        [] ->
            [];
        [_ | S] ->
            {RL, _Rest} = gbop(S, L, R),
            lists:reverse(tl(lists:reverse(RL)))
    end.

%%
%% Local Functions
%%


extract_comments([], _, TAcc, CAcc) ->
    {lists:reverse(TAcc), lists:reverse(CAcc)};
extract_comments([#token{kind=comment, offset=ONext, length=LNext, line=NNext,
                         value=VNext}
                  | Rest], NNext, TAcc,
				 [#token{kind=comment, offset=O, value=V}=C | CAcc]) ->
    NewComment = C#token{offset=O, length=ONext-O+LNext, value=V++"\n"++VNext,
                         last_line=NNext},
    extract_comments(Rest, NNext+1, TAcc, [NewComment | CAcc]);
extract_comments([C = #token{kind=comment, line=N} | Rest], _, TAcc, CAcc) ->
    extract_comments(Rest, N+1, TAcc, [C | CAcc]);
extract_comments([T | Rest], _, TAcc, CAcc) ->
    extract_comments(Rest, -1, [T | TAcc], CAcc).

get_function_comment(F, []) ->
    F;
get_function_comment(#function{name_pos={{Line, _}, _}}=F, 
                     [#token{last_line=LastLine, value=Value} | _])
  when LastLine+1=:=Line; LastLine+2=:=Line; LastLine+3=:=Line->
    F#function{comment=erlide_text:strip_percents_and_spaces(Value)};
get_function_comment(#function{name_pos={{Line, _}, _}}=F,
                     [#token{line=LastLine, last_line=u, value=Value} | _])
  when LastLine+1=:=Line; LastLine+2=:=Line; LastLine+3=:=Line->
    F#function{comment=erlide_text:strip_percents_and_spaces(Value)};
get_function_comment(F, [_ | Rest]) ->
    get_function_comment(F, Rest).


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
