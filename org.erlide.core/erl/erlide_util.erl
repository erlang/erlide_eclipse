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
-export([check_and_renew_cached/4, check_and_renew_cached/5, check_cached/3, renew_cached/4]).
-export([pack/1, unpack/1]).
-export([get_between_strs/3, get_all_between_strs/3, get_from_str/2, get_upto_str/2 ,split_lines/1]).

-ifdef(DEBUG).
-compile(export_all).
-endif.

%%
%% API Functions
%%

-define(SEP, ";").

unpack(F) ->
    string:tokens(F, ?SEP).

pack([]) ->
    [];
pack([A]) ->
    A;
pack([A | [_ | _] = Rest]) ->
    A++?SEP++pack(Rest).

check_cached(SourceFileName, CacheFileName, Version) ->
    ?D({SourceFileName, CacheFileName}),
    SourceModDate = case file:read_file_info(SourceFileName) of
                        {ok, Info} ->
                            Info#file_info.mtime;
                        {error, enoent} ->
                            {{1900, 1, 1}, {0, 0, 0}}
                    end,
    ?D(SourceModDate),
    case read_cache_date_and_version(CacheFileName) of
        {SourceModDate, Version} ->
            {ok, read_cache(CacheFileName)};
        _ ->
            {no_cache, SourceModDate}
    end.
    
renew_cached(SourceFileName, CacheFileName, Version, Term) ->
    SourceModDate = case file:read_file_info(SourceFileName) of
                        {ok, Info} ->
                            Info#file_info.mtime;
                        {error, enoent} ->
                            {{1900, 1, 1}, {0, 0, 0}}
                    end,
    renew_cache(SourceModDate, Version, CacheFileName, Term).                   

check_and_renew_cached(SourceFileName, CacheFileName, Version, RenewFun) ->
    check_and_renew_cached(SourceFileName, CacheFileName, Version, RenewFun, fun(D) -> D end).

check_and_renew_cached(SourceFileName, CacheFileName, Version, RenewFun, CachedFun) ->
    case check_cached(SourceFileName, CacheFileName, Version) of
        {ok, Cached} ->
            ?D(from_cache),
            CachedFun(Cached);
        {no_cache, SourceModDate} ->
            Term = RenewFun(SourceFileName),
            ?D(renewing),
            renew_cache(SourceModDate, Version, CacheFileName, Term),
            Term
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



%%
%% Local Functions
%%

renew_cache(SourceFileModDate, Version, CacheFileName, Term) ->
    BinDate = date_to_bin(SourceFileModDate),
    B = term_to_binary(Term),
    _Delete = file:delete(CacheFileName),
    _Write = file:write_file(CacheFileName, <<BinDate/binary, Version:16/integer-big, B/binary>>).

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
    <<_:5/binary, _:16/integer-big, BinTerm/binary>> = B,
    binary_to_term(BinTerm).
