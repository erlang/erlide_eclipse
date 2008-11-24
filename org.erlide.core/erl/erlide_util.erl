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
%% -export([newer_file/2]).
-export([check_cached/4, check_cached/5]).

%%
%% API Functions
%%

check_cached(SourceFileName, CacheFileName, Version, RenewFun) ->
    check_cached(SourceFileName, CacheFileName, Version, RenewFun, fun(D) -> D end).

check_cached(SourceFileName, CacheFileName, Version, RenewFun, CacheFun) ->
    SourceModDate = case file:read_file_info(SourceFileName) of
                        {ok, Info} ->
                            Info#file_info.mtime;
                        {error, enoent} ->
                            {{1900, 1, 1}, {0, 0, 0}}
                    end,
    case read_cache_date_and_version(CacheFileName) of
        {SourceModDate, Version} ->
            read_cache(CacheFileName, CacheFun);
        _ ->
            renew_cache(SourceFileName, SourceModDate, Version, CacheFileName, RenewFun)
    end.

%%
%% Local Functions
%%

renew_cache(SourceFileName, SourceFileModDate, Version, CacheFileName, RenewFun) ->
    ?D({SourceFileModDate, CacheFileName, Version}),
    BinDate = date_to_bin(SourceFileModDate),
    ?D({Version, BinDate}),
    T = RenewFun(SourceFileName),
    %?D(T),
    B = term_to_binary(T),
    ?D({Version, B}),
    _Delete = file:delete(CacheFileName),
    ?D({Version, _Delete}),
    _Write = file:write_file(CacheFileName, <<BinDate/binary, Version:16/integer-big, B/binary>>),
    ?D({Version, _Write}),
    T.

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

read_cache(CacheFileName, CacheFun) ->
    {ok, B} = file:read_file(CacheFileName),
    <<_:5/binary, _:16/integer-big, BinTerm/binary>> = B,
    CacheFun(binary_to_term(BinTerm)).

