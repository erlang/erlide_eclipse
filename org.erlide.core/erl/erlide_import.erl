%% Author: jakob
%% Created: 30 maj 2008
%% Description: TODO: Add description to erlide_import
-module(erlide_import).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([import/2]).
-compile(export_all).

%%
%% API Functions
%%

%%
%% TODO: Add description of import/function_arity
%%

import(L) ->
    import("", L).

import(Prefix0, L) ->
    M = filter_out_dotnames(L),
    Prefix = case lists:last(Prefix0) of
                 $/ ->
                     Prefix0;
                 _ ->
                     Prefix0++"/"
             end,
	import(Prefix, M, [], [], []).

%%
%% Local Functions
%%

dot_name("") -> false;
dot_name([C | _]) -> C == $. .

filter_out_dotnames(L) ->
    lists:filter(fun(Name) ->
                         not lists:any(fun([$. | _]) -> true;
                                          (_) -> false end,
                                       filename:split(Name))
                 end, L).
  
import(_Prefix, [], Files, SourceDirs, IncludeDirs) ->
    {lists:reverse(Files), lists:reverse(SourceDirs), lists:reverse(IncludeDirs)};
import(Prefix, [File | Rest], Files, SourceDirs, IncludeDirs) ->
    Dir = filename:dirname(File),
    case filename:extension(File) of
        ".hrl" ->
            import(Prefix, Rest, [File | Files], SourceDirs, add(remove_prefix(Prefix, Dir), IncludeDirs));
        ".erl" ->
            import(Prefix, Rest, [File | Files], add(remove_prefix(Prefix, Dir), SourceDirs), IncludeDirs);
        ".beam" ->
            import(Prefix, Rest, Files, SourceDirs, IncludeDirs);
        _ ->
            import(Prefix, Rest, [File | Files], SourceDirs, IncludeDirs)
    end.

add(E, L) ->
    case lists:member(E, L) of
        true ->
            L;
        false ->
            [E | L]
    end.

remove_prefix(Prefix, Dir) ->
    case lists:prefix(Prefix, Dir) of
        true ->
            string:substr(Dir, length(Prefix)+1);
        false ->
            Dir
    end.


         
         