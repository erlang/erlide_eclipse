%% Author: jakob
%% Created: 25 okt 2007
-module(erlide_util).

%% -define(DEBUG, 1).

%%
%% Include files
%%

-include("erlide.hrl").
-include_lib("kernel/include/file.hrl").

%%
%% Exported Functions
%%
-export([check_and_renew_cached/5, check_and_renew_cached/6, check_cached/3, renew_cached/4, read_cache_date_and_version/1, read_cache/1]).
-export([pack/1, unpack/1, join/2]).
-export([reverse2/1]).
-export([get_between_strs/3, get_all_between_strs/3, get_from_str/2, get_upto_str/2 ,split_lines/1]).
-export([get_auto_imported/1, add_auto_imported/1]).

%%
%% API Functions
%%

-define(SEP, ";").

unpack(F) ->
    string:tokens(F, ?SEP).

pack(L) ->
    join(L, ?SEP).

check_cached(SourceFileName, CacheFileName, Version) ->
    ?D({SourceFileName, CacheFileName}),
    SourceModDate = case file:read_file_info(SourceFileName) of
                        {ok, Info} ->
                            Info#file_info.mtime;
                        {error, enoent} ->
                            {{1900, 1, 1}, {0, 0, 0}}
                    end,
    ?D(SourceModDate),
    SV = read_cache_date_and_version(CacheFileName),
    ?D(SV),
    case same_date_and_version(SV, {SourceModDate, Version}) of
        true ->
            {cache, read_cache(CacheFileName)};
        _ ->
            {no_cache, SourceModDate}
    end.
    
same_date_and_version({{Date, {H, M, S1}}, V}, {{Date, {H, M, S2}}, V}) ->
    S1 div 2 =:= S2 div 2;
same_date_and_version(_, _) ->
    false.

reverse2(L) when is_list(L) ->
    lists:reverse([lists:reverse(A) || A <- L]).

renew_cached(SourceFileName, CacheFileName, Version, Term) ->
    SourceModDate = case file:read_file_info(SourceFileName) of
                        {ok, Info} ->
                            Info#file_info.mtime;
                        {error, enoent} ->
                            {{1900, 1, 1}, {0, 0, 0}}
                    end,
    ?D(SourceFileName),
    renew_cache(SourceModDate, Version, CacheFileName, Term).                    

check_and_renew_cached(SourceFileName, CacheFileName, Version, 
		       RenewFun, UseCache) ->
    check_and_renew_cached(SourceFileName, CacheFileName, Version,
			   RenewFun, fun(D) -> D end, UseCache).

check_and_renew_cached(SourceFileName, _CacheFileName, _Version,
                       RenewFun, _CachedFun, false) ->
    Term = RenewFun(SourceFileName),
    {dont_use_cache, Term};
check_and_renew_cached(SourceFileName, CacheFileName, 
		       Version, RenewFun, CachedFun, 
                       true) ->
    ?D(check_and_renew_cached),
    case check_cached(SourceFileName, CacheFileName, Version) of
        {cache, Cached} ->
            ?D({from_cache, CacheFileName}),
            R = {cached, CachedFun(Cached)},
	    ?D(got_cached),
	    R;
        {no_cache, SourceModDate} ->
	    ?D(SourceModDate),
            Term = RenewFun(SourceFileName),
            ?D({renewing, CacheFileName, UpdateCache}),
		    renew_cache(SourceModDate, Version, CacheFileName, Term),
            {renewed, Term}
    end.

get_from_str(Text, Start) ->
    case string:str(Text, Start) of
	0 ->
	    Text;
	N ->
	    string:substr(Text, N + length(Start))
    end.

get_between_strs(Text, Start, End) ->
    get_upto_str(get_from_str(Text, Start), End).

get_all_between_strs(Text, Start, End) ->
    {One, Next} = split_at(get_from_str(Text, Start), End),
    case Next of
	"" ->
	    [One];
	_ ->
	    [One | get_all_between_strs(Next, Start, End)]
    end.

get_upto_str(Text, End) ->
    case string:rstr(Text, End) of
	0 ->
	    Text;
	N ->
	    string:substr(Text, 1, N-1)
    end.

split_at(Text, End) ->
    case string:str(Text, End) of
	0 ->
	    {Text, ""};
	N ->
	    {string:substr(Text, 1, N-1), string:substr(Text, N+length(End))}
    end.

split_lines(<<B/binary>>) ->
    split_lines(binary_to_list(B));
split_lines(L) when is_list(L) ->
    split_lines(L, [], []).

split_lines([], [], Acc) ->
    lists:reverse(Acc);
split_lines([], LineAcc, Acc) ->
    split_lines([], [], [lists:reverse(LineAcc) | Acc]);
split_lines([$\n, $\r | Rest], LineAcc, Acc) ->
	split_lines(Rest, [], [lists:reverse(LineAcc) | Acc]);
split_lines([$\n | Rest], LineAcc, Acc) ->
	split_lines(Rest, [], [lists:reverse(LineAcc) | Acc]);
split_lines([$\r | Rest], LineAcc, Acc) ->
	split_lines(Rest, [], [lists:reverse(LineAcc) | Acc]);
split_lines([C | Rest], LineAcc, Acc) ->
	split_lines(Rest, [C | LineAcc], Acc).

join([], Sep) when is_list(Sep) ->
    [];
join([H|T], Sep) ->
    H ++ lists:append([Sep ++ X || X <- T]).

add_auto_imported(Imports) ->
    [{erlang, get_auto_imported("")} | Imports].

get_auto_imported(Prefix) when is_list(Prefix) ->
    case catch erlang:module_info(exports) of
        Val when is_list(Val) ->
            lists:filter(fun({N, A}) ->
                                 lists:prefix(Prefix, atom_to_list(N)) andalso
                                     erl_internal:bif(N, A)
                         end, Val);
        _Error ->
            ?D(_Error),
            error
    end.

%%
%% Local Functions
%%

renew_cache(SourceFileModDate, Version, CacheFileName, Term) ->
    ?D(SourceFileModDate),
    BinDate = date_to_bin(SourceFileModDate),
    B = term_to_binary(Term, [compressed]),
    _Delete = file:delete(CacheFileName),
    _Write = file:write_file(CacheFileName, <<BinDate/binary, Version:16/integer-big, B/binary>>),
    ?D(_Write),
    ?D(CacheFileName).

bin_to_date(<<Y:15/integer-big, Mo:4, D:5, H:5, M:6, S:5>>) ->
    {{Y, Mo, D}, {H, M, S*2}}.
date_to_bin({{Y, Mo, D}, {H, M, S}}) ->
    <<Y:15/integer-big, Mo:4, D:5, H:5, M:6, (S div 2):5>>.

read_cache_date_and_version(CacheFileName) ->
    case file:open(CacheFileName, [read, binary]) of
        {ok, F} ->
            {ok, BinDateAndVersion} = file:read(F, 7),
            file:close(F),
            <<BinDate:5/binary, Version:16/integer-big>> = BinDateAndVersion,
            {bin_to_date(BinDate), Version};
        _ ->
            {{{0, 0, 0}, {0, 0, 0}}, 0}
    end.

read_cache(CacheFileName) ->
    {ok, B} = file:read_file(CacheFileName),
    ?D(CacheFileName),
    <<_:5/binary, _:16/integer-big, BinTerm/binary>> = B,
    binary_to_term(BinTerm).
