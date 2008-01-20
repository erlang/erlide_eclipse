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
-export([open_info/4,
         find_first_var/2,
         get_source_from_module/3]).

%%-define(DEBUG, 1).

-include("erlide.hrl").

-include("erlide_scanner.hrl").

%%
%% API Functions
%%

get_include(Tokens, T) ->
    case lists:keysearch(string, #token.kind, Tokens) of
    	{value, Token} ->
            {T, Token#token.value};
		_ ->
            none
    end.

find_lib_dir(Dir) ->
    [Lib | Rest] = filename:split(Dir),
    {code:lib_dir(Lib), Rest}.
    
%%     ?D({str, S}),
%%     {LibDir, Rest} = find_lib_dir(S),
%%      ?D({libdir,LibDir,Rest}),
%%     R = filename:join([LibDir | Rest]),
%%     ?D({fn,R}),
%%     R.

check_include(Tokens) ->
    case lists:keymember(include, #token.value, Tokens) of
        true ->
            get_include(Tokens, include);
        false -> 
            case lists:keymember(include_lib, #token.value, Tokens) of
                true ->
                    get_include(Tokens, include);
                false ->
                    none
            end
    end.  

open_info(L, W, ExternalModules, PathVars) ->
    ?D({open_info, W, L}),
    {CL, CW} = erlide_text:clean_tokens(L, W),
    ?D({open_info, CW, CL}),
    case check_include(CL) of
        {include, F} ->
            ?D(F),
            {include, F};
        {include_lib, D, F} ->
            ?D({D,F}),
            {include, filename:join(find_lib_dir(D), F)};
        {ok, F, Rest} -> {local, {F, erlide_text:guess_arity(Rest)}};
        _ ->
            case erlide_text:check_function_call(CL, CW) of
                {ok, M, F, Rest} = _Xx ->
                    ?D(_Xx),
                    {external, {M, F, erlide_text:guess_arity(Rest), 
                                get_source_from_module(M, ExternalModules, PathVars)}};
                {ok, F, Rest} -> 
                    ?D(F),
                    {local, {F, erlide_text:guess_arity(Rest)}};
                _ ->
                    ?D(CL),
                    case erlide_text:check_variable_macro_or_record(CL, CW) of
                        {ok, M, R} -> 
                            {M, {R}};
                        _ ->
                            none
                    end
            end
    end.

get_source_from_module(Mod, ExternalModules, PathVars) ->
    case catch get_source(Mod) of
        {'EXIT', _} ->
            get_source_from_external_modules(Mod, ExternalModules, PathVars);
        [] ->
            get_source_from_external_modules(Mod, ExternalModules, PathVars);
        Other ->
            Other
    end.

get_external_modules_file(FileName, PathVars) ->
    get_external_modules_file(FileName, PathVars, []).

replace_path_var(FileName, PathVars) ->
    case filename:split(FileName) of
        ["/" | _] ->
            FileName;
        [Var | Rest] ->
            filename:join([replace_path_var_aux(Var, PathVars) | Rest])
    end.

replace_path_var_aux(Var, PathVars) ->
    case lists:keysearch(Var, 1, PathVars) of
        {value, {Var, Value}} ->
            Value;
        _ ->
            Var
    end.

get_external_modules_file(FileName0, PathVars, Acc) ->
    FileName = replace_path_var(FileName0, PathVars),
    case file:read_file(FileName) of
        {ok, B} ->
            get_ext_aux(split_lines(B), PathVars, Acc);
        _ ->
            Acc
    end.

get_ext_aux([], _PathVars, Acc) ->
    Acc;
get_ext_aux([L | Rest], PathVars, Acc0) ->
     case filename:extension(L) of
         ".erlidex" ->
             Acc = get_external_modules_file(L, PathVars, Acc0),
             get_ext_aux(Rest, PathVars, Acc);
         _ ->
             get_ext_aux(Rest, PathVars, [L | Acc0])
     end.

get_source_from_external_modules(Mod, ExternalModules, PathVars) ->
    ?D(ExternalModules),
    ?D(PathVars),
    L = get_external_modules_file(ExternalModules, PathVars),
    select_external(L, atom_to_list(Mod)).

select_external([], _) ->
    not_found;
select_external([P | Rest], Mod) ->
	case filename:rootname(filename:basename(P)) of
        Mod ->
            P;
        _ ->
            select_external(Rest, Mod)
    end.

split_lines(<<B/binary>>) ->
    split_lines(binary_to_list(B), [], []).

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



