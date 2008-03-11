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
-export([check_cached/3]).

%%
%% API Functions
%%

%% newer_file(F1, F2) when is_list(F1), is_list(F2) ->
%%     case file:read_file_info(F1) of
%%         {ok, Info1} ->
%%             case file:read_file_info(F2) of
%%                 {ok, Info2} ->
%%                     ?D({Info1#file_info.mtime, Info2#file_info.mtime, Info1#file_info.mtime > Info2#file_info.mtime}),
%%                     Info1#file_info.mtime > Info2#file_info.mtime;
%%                 _ ->
%%                     true
%%             end;
%%         _ ->
%%             false
%%     end;
%% newer_file(Module, F2) when is_atom(Module) ->
%%     case lists:keysearch(time, 1, Module:module_info(compile)) of
%%         {value, time, ModTime} ->
%%             case file:read_file_info(F2) of
%%                 {ok, Info2} ->
%%                     ModTime > Info2#file_info.mtime;
%%                 _ ->
%%                     true
%%             end;
%%         _ ->
%%             false
%%     end.

%% check_cached(SourceFileName, CacheFileName, Renew) ->
%%     case erlide_util:newer_file(SourceFileName, CacheFileName)
%%          orelse erlide_util:newer_file(?MODULE, CacheFileName) of
%%         true ->
%%             renew_cache(SourceFileName, CacheFileName, Renew);
%%         _ ->
%%             read_cache(CacheFileName)
%%     end.

check_cached(SourceFileName, CacheFileName, Renew) ->
    {ok, Info} = file:read_file_info(SourceFileName),
    SourceModDate = Info#file_info.mtime,
    case read_cache_date(CacheFileName) of
        SourceModDate ->
            read_cache(CacheFileName);
        _ ->
            renew_cache(SourceFileName, SourceModDate, CacheFileName, Renew)
    end.

%%
%% Local Functions
%%

renew_cache(SourceFileName, SourceFileModDate, CacheFileName, Renew) ->
    BinDate = date_to_bin(SourceFileModDate),
    T = Renew(SourceFileName),
    B = term_to_binary(T),
    file:delete(CacheFileName),
    file:write_file(CacheFileName, <<BinDate/binary, B/binary>>),
    T.

bin_to_date(<<Y:16/integer-big, Mo, D, H, M, S>>) ->
    {{Y, Mo, D}, {H, M, S}}.
date_to_bin({{Y, Mo, D}, {H, M, S}}) ->
    <<Y:16/integer-big, Mo, D, H, M, S>>.

read_cache_date(CacheFileName) ->
    case file:open(CacheFileName, [read, binary]) of
        {ok, F} ->
            {ok, BinDate} = file:read(F, 7),
            file:close(F),
            bin_to_date(BinDate);
        _ ->
            {{0, 0, 0}, {0, 0, 0}}
    end.

read_cache(CacheFileName) ->
    {ok, B} = file:read_file(CacheFileName),
    <<_:7/binary, BinTerm/binary>> = B,
    binary_to_term(BinTerm).

