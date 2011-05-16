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
%% -compile(export_all).

%%
%% API Functions
%%

%% import(L) ->
%%     import("", L).

import(Prefix0, L) ->
    M0 = filter_out_dotnames(L),
    M = filter_out_tilde(M0),
    Prefix = case lists:last(Prefix0) of
                 $/ ->
                     Prefix0;
                 _ ->
                     Prefix0++"/"
             end,
	import(Prefix, M, new_empty(), new_empty(), new_empty(), none, new_empty()).

%%
%% Local Functions
%%

%% dot_name("") -> false;
%% dot_name([C | _]) -> C == $. .

filter_out_dotnames(L) ->
    lists:filter(fun(Name) ->
                         not lists:any(fun([$. | _]) -> true;
                                          (_) -> false end,
                                       filename:split(Name))
                 end, L).

filter_out_tilde(L) ->
    lists:filter(fun(Name) ->
                         case lists:reverse(Name) of
                             [$~ | _] -> false;
                             _ -> true
                         end
                 end, L).
                         

import(_Prefix, [], Files, SourceDirs, IncludeDirs, BeamDir, AllDirs) ->
    {to_list(Files), to_list(SourceDirs), to_list(IncludeDirs), BeamDir, to_list(AllDirs)};
import(Prefix, [File | Rest], Files, SourceDirs, IncludeDirs, BeamDir, AllDirs0) ->
    Dir = filename:dirname(File),
	AllDirs = add_elem(remove_prefix(Prefix, Dir), AllDirs0),
    case filename:extension(File) of
        ".hrl" ->
            import(Prefix, Rest, [File | Files], SourceDirs, add_elem(remove_prefix(Prefix, Dir), IncludeDirs), BeamDir, AllDirs);
        ".erl" ->
            import(Prefix, Rest, [File | Files], add_elem(remove_prefix(Prefix, Dir), SourceDirs), IncludeDirs, BeamDir, AllDirs);
        ".yrl" ->
            import(Prefix, Rest, [File | Files], add_elem(remove_prefix(Prefix, Dir), SourceDirs), IncludeDirs, BeamDir, AllDirs);
        ".beam" ->
            import(Prefix, Rest, Files, SourceDirs, IncludeDirs, remove_prefix(Prefix, Dir), AllDirs);
        _ ->
            import(Prefix, Rest, [File | Files], SourceDirs, IncludeDirs, BeamDir, AllDirs)
    end.

new_empty() ->
    ordsets:new().

add_elem(E, L) ->
    ordsets:add_element(E, L).

to_list(L) ->
    ordsets:to_list(L).

remove_prefix(Prefix, Dir) ->
    case lists:prefix(Prefix, Dir) of
        true ->
            string:substr(Dir, length(Prefix)+1);
        false ->
            Dir
    end.


         
         