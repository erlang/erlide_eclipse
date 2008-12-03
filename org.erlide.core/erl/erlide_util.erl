%% Author: jakob
%% Created: 25 okt 2007
%% Description: TODO: Add description to erlide_util
-module(erlide_util).

%-define(DEBUG, 1).

%%
%% Include files
%%

-include("erlide.hrl").
-include_lib("kernel/include/file.hrl").

%%
%% Exported Functions
%%
-export([check_and_renew_cached/4, check_and_renew_cached/5, check_cached/3, renew_cached/4]).

%%
%% API Functions
%%

check_cached(SourceFileName, CacheFileName, Version) ->
    SourceModDate = case file:read_file_info(SourceFileName) of
                        {ok, Info} ->
                            Info#file_info.mtime;
                        {error, enoent} ->
                            {{1900, 1, 1}, {0, 0, 0}}
                    end,
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
            CachedFun(Cached);
        {no_cache, SourceModDate} ->
            Term = RenewFun(SourceFileName),
            renew_cache(SourceModDate, Version, CacheFileName, Term),
            Term
    end.

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

