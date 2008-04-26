%% Author: jakob
%% Created: Mar 23, 2006
%% Description: TODO: Add description to erlide_open

-module(erlide_noparse).

%%
%% Exported Functions
%%

-export([initial_parse/4, reparse/1]). 

%%
%% Include files
%%

%-define(DEBUG, 1).

%-define(SCANNER, erlide_scanner).
-define(SCANNER, erlide_scanner2).

-include("erlide.hrl").
-include("erlide_scanner.hrl").

-record(function, {pos, name, arity, clauses, name_pos}).
-record(clause, {pos, name, args, guards, code, name_pos}).
-record(attribute, {pos, name, args}).
-record(other, {pos, name, tokens}).

initial_parse(ScannerName, ModuleFileName, InitalText, StateDir) ->
    try
    	?D({StateDir, ModuleFileName}),
		Renew = fun(_F) -> do_parse(ScannerName, ModuleFileName, InitalText, StateDir) end,
    	CacheFileName = filename:join(StateDir, atom_to_list(ScannerName) ++ ".noparse"),
        ?D(CacheFileName),
		Res = erlide_util:check_cached(ModuleFileName, CacheFileName, Renew),
        {ok, Res}
    catch
        error:Reason ->
            {error, Reason}
    end.

reparse(ScannerName) ->
    try
        Res = do_parse(ScannerName, "", "", ""),
        {ok, Res}
    catch
        error:Reason ->
            {error, Reason}
    end.

do_parse(ScannerName, ModuleFileName, InitalText, StateDir) ->
    ?Info({noparse, ScannerName}),
    Toks = scan(ScannerName, ModuleFileName, InitalText, StateDir),
    ?D(Toks),
    {UncommentToks, Comments} = extract_comments(Toks),
    Functions = split_after_dots(UncommentToks, [], []),
    ?D(Functions),
    Collected = [classify_and_collect(I) || I <- Functions],
    ?D(Collected),
    {Collected, Comments, Toks}.

classify_and_collect(C) ->
    cac(check_class(C), C).

cac(function, Tokens) ->
    ClauseList = split_clauses(Tokens),
    Clauses = [fix_clause(C) || C <- ClauseList],
    [#clause{pos=P, name=N, args=A, name_pos=NP} | _] = Clauses,
    Arity = erlide_text:guess_arity(A),
    #function{pos=P, name=N, arity=Arity, clauses=Clauses, name_pos=NP};
cac(attribute, Attribute) ->
    case Attribute of
        [#token{kind='-', offset=Offset, line=Line}, 
         #token{kind=atom, value=Name, line=_Line, offset=_Offset},
         _, #token{value=Args} | _] = Attribute ->
            #token{line=LastLine, offset=LastOffset, 
                   length=LastLength} = lists:last(Attribute),
            PosLength = LastOffset - Offset + LastLength,
            #attribute{pos={{Line, LastLine, Offset}, PosLength},
                       name=Name, args=Args};
        [_, #token{kind=atom, value=Name, line=Line, offset=Offset} | _] ->
            #token{line=LastLine, offset=LastOffset, 
                   length=LastLength} = lists:last(Attribute),
            PosLength = LastOffset - Offset + LastLength,
            #attribute{pos={{Line, LastLine, Offset}, PosLength},
                       name=Name, args=[]}
    end;
cac(other, [#token{value=Name, line=Line,
                         offset=Offset, length=Length} | _]) ->
    #other{pos={{Line, Line, Offset}, Length}, name=Name}.

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
    lists:reverse([lists:reverse(A) || A <- L]).
%%     lists:foldl(fun(E, Acc) ->
%%                         [lists:reverse(E) | Acc]
%%                 end, [], L).

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
    split_clauses(F, [], []).

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
    #token{line=LastLine, offset=LastOffset, length=LastLength} = lists:last(Rest),
    PosLength = LastOffset - Offset + LastLength,
    #clause{pos={{Line, LastLine, Offset}, PosLength},
            name=Name, args=get_args(Rest), guards=get_guards(Rest), code=[],
            name_pos={{Line, Offset}, Length}}.

%% scan(String, Name) ->
%%     case erlide_scanner:isScanned(Name) of
%%         false ->
%% 		    erlide_scanner:create(Name),
%% 		    erlide_scanner:insertText(Name, 1, String);
%%         true ->
%%             ok
%%     end,
scan(ScannerName, ModuleFileName, InitialText, StateDir) ->
    ?D(ets:info(ScannerName)),
    ?SCANNER:initialScan(ScannerName, ModuleFileName, InitialText, StateDir),
    S = ?SCANNER:getTokens(ScannerName),
    ?D(ets:info(ScannerName)),
    S.

extract_comments(Tokens) ->
    extract_comments(Tokens, -1, [], []).

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























