%% Author: jakob
%% Created: 25 okt 2007
%% Description: TODO: Add description to erlide_util
-module(erlide_util).

-define(DEBUG, 1).

%%
%% Include files
%%

-include("erlide.hrl").
-include_lib("kernel/include/file.hrl").

%%
%% Exported Functions
%%
-export([newer_file/2, check_cached/3]).

%%
%% API Functions
%%

newer_file(F1, F2) when is_list(F1), is_list(F2) ->
    case file:read_file_info(F1) of
        {ok, Info1} ->
            case file:read_file_info(F2) of
                {ok, Info2} ->
                    ?D({Info1#file_info.mtime, Info2#file_info.mtime, Info1#file_info.mtime > Info2#file_info.mtime}),
                    Info1#file_info.mtime > Info2#file_info.mtime;
                _ ->
                    true
            end;
        _ ->
            false
    end;
newer_file(Module, F2) when is_atom(Module) ->
    case lists:keysearch(time, 1, Module:module_info(compile)) of
        {value, time, ModTime} ->
            case file:read_file_info(F2) of
                {ok, Info2} ->
                    ModTime > Info2#file_info.mtime;
                _ ->
                    true
            end;
        _ ->
            false
    end.

check_cached(SourceFileName, CacheFileName, Renew) ->
    case erlide_util:newer_file(SourceFileName, CacheFileName)
         orelse erlide_util:newer_file(?MODULE, CacheFileName) of
        true ->
            renew_cache(SourceFileName, CacheFileName, Renew);
        _ ->
            read_cache(CacheFileName)
    end.


%%
%% Local Functions
%%

renew_cache(SourceFileName, CacheFileName, Renew) ->
    T = Renew(SourceFileName),
    B = term_to_binary(T),
    file:delete(CacheFileName),
    file:write_file(CacheFileName, B),
    T.

read_cache(CacheFileName) ->
    {ok, B} = file:read_file(CacheFileName),
    binary_to_term(B).
