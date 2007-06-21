%% Author: jakob
%% Created: Mar 23, 2006
%% Description: TODO: Add description to erlide_open
-module(erlide_open).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([open_included/1,
         open_info/2,
         find_first_var/2,
         get_source_from_module/1]).

%% -define(DEBUG, 1).

-include("erlide.hrl").

-include("erlide_scanner.hrl").

%%
%% API Functions
%%

open_included(Text) ->
        catch open_include(Text).

open_include(Text) ->
    {ok, Tokens, _} = erlide_scan:string(Text),
    ?D({scan, Tokens}),
    case lists:keysearch(include, 3, Tokens) of
        {value, _} ->
            get_include(Tokens);
        false ->
            get_include_lib(Tokens)
    end.

get_include(Tokens) ->
    {value, {string, _, S, _}} = lists:keysearch(string, 1, Tokens),
    S.

find_lib_dir(Dir) ->
    [Lib | Rest] = filename:split(Dir),
    {code:lib_dir(Lib), Rest}.

get_include_lib(Tokens) ->
    {value, {string, _, S, _}} = lists:keysearch(string, 1, Tokens),
    ?D({str, S}),
    {LibDir, Rest} = find_lib_dir(S),
     ?D({libdir,LibDir,Rest}),
    R = filename:join([LibDir | Rest]),
    ?D({fn,R}),
    R.

open_info(L, W) ->
    ?D({open_info, W, L}),
    {CL, CW} = erlide_text:clean_tokens(L, W),
    ?D({open_info, CW, CL}),
    case erlide_text:check_function_call(CL, CW) of
        {ok, M, F, Rest} ->
             {external, {M, F, erlide_text:guess_arity(Rest), get_source_from_module(M)}};
        {ok, F, Rest} ->
            {local, {F, erlide_text:guess_arity(Rest)}};
        _ ->
            case erlide_text:check_variable_macro_or_record(CL, CW) of
                {ok, M, R} ->
                    {M, {R}};
                _ ->
                    none
            end
    end.

get_source_from_module(Mod) ->
    case catch get_source(Mod) of
        {'EXIT', _} ->
            not_found;
        [] ->
            not_found;
        Other ->
            Other
    end.

get_source(Mod) ->
    L = Mod:module_info(compile),
    {value, {source, Path}} = lists:keysearch(source, 1, L),
    case filelib:is_regular(Path) of
        true ->
            Path;
        false ->
            get_source_ebin(Mod)
    end.

find_first_var(Var, S) ->
    case catch get_var(Var, S) of
        {'EXIT', _} ->
            error;
        Other ->
            {ok, Other}
    end.

%% Local Functions
%%

get_source_ebin(Mod) ->
    EbinPath = code:which(Mod),
    BeamF = filename:basename(EbinPath),
    ErlF = filename:rootname(BeamF) ++ ".erl",
    SrcPath = filename:join([filename:dirname(filename:dirname(EbinPath)), "src", ErlF]),
    SrcPath.

get_var(Var, S) ->
    {ok, T, _} = erlide_scan:string(S),
    {var, {{_Line, Offset}, Length}, _Var} = find_var(T, Var),
    {Offset, Length}.

find_var([], _) ->
    not_found;
find_var([{var, _, Var} = T | _], Var) ->
    T;
find_var([_ | Rest], Var) ->
    find_var(Rest, Var).



