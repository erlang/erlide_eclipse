%% Author: jakob
%% Created: Mar 23, 2006
%% Description: TODO: Add description to erlide_open
-module(erlide_open).
-author(jakobce@gmail.com).

%%
%% Exported Functions
%%
-export([open/4,
         open_info/4,
         find_first_var/2,
         get_source_from_module/3,
         get_include_lib/1]).

%%
%% Include files
%%

%% -define(DEBUG, 1).

-include("erlide.hrl").
-include("erlide_scanner.hrl").

%%
%% API Functions
%%


open(Mod, Offset, ExternalModules, PathVars) ->
    ?D({Mod, Offset, PathVars}),
    try
        {TokensWComments, BeforeReversed} =
            erlide_scanner2:getTokenWindow(Mod, Offset, 5, 50),
        ?D({TokensWComments, BeforeReversed}),
        try_open(Mod, Offset, TokensWComments, BeforeReversed,
                 ExternalModules, PathVars),
        error
    catch
        throw:{open, Res} ->
            Res;
        throw:T ->
            {error, T};
        error:E ->
            {error, E}
    end.

try_open(Mod, Offset, TokensWComments, BeforeReversed, ExternalModules, PathVars) ->
    ?D(TokensWComments),
    Tokens = strip_comments(TokensWComments),
	?D(Tokens),
    o_tokens(Tokens, ExternalModules, PathVars, BeforeReversed),
	case BeforeReversed of
		[] ->
			not_found;
		[B | Rest] ->
			try_open(Mod, Offset, [B | TokensWComments], Rest, ExternalModules, PathVars)
    end.

consider_local([]) ->
	true;
consider_local([#token{kind=':'} | _]) ->
	false;
consider_local([#token{kind=comment} | More]) ->
	consider_local(More);
consider_local(_) ->
    true.

strip_comments(Tokens) ->
    [T || T <- Tokens, T#token.kind =/= comment].

o_tokens([#token{kind=atom, value=include} | Rest], _, _, [#token{kind='-'} | _]) ->
    o_include(Rest);
o_tokens([#token{kind=atom, value=include_lib} | Rest], _, _, [#token{kind='-'} | _]) ->
    o_include_lib(Rest);
o_tokens([#token{kind=macro, value=Value} | _], _, _, _) ->
    o_macro(Value);
o_tokens([#token{kind='#'}, #token{kind=atom, value=Value} | _], _, _, _) ->
    o_record(Value);
o_tokens([#token{kind=atom, value=Module}, #token{kind=':'}, #token{kind=atom, value=Function} | Rest],
         ExternalModules, PathVars, _) ->
    o_external(Module, Function, Rest, ExternalModules, PathVars);
o_tokens([#token{kind=atom, value=Function}, #token{kind='/'}, #token{kind=integer, value=Arity} | _], _, _, _) ->
    throw({open, {local, Function, Arity}});
o_tokens([#token{kind='/'}, #token{kind=integer, value=Arity} | _], _, _, [#token{kind=atom, value=Function} | _]) ->
    throw({open, {local, Function, Arity}});
o_tokens([#token{kind=atom, value=Function}, #token{kind='('} | Rest], _, _, BeforeReversed) ->
	case consider_local(BeforeReversed) of
        true ->
            ?D(Rest),
            throw({open, {local, Function, erlide_text:guess_arity(Rest)}});
        false ->
            continue
    end;
o_tokens(_, _, _, _) ->
    no.

o_include([#token{kind='('}, #token{kind=string, value=File} | _]) ->
    throw({open, {include, File}});
o_include(_) ->
    no.

o_include_lib([#token{kind='('}, #token{kind=string, value=Path} | _]) ->
    {include, File} = get_include_lib(Path),
    throw({open, {include, File}});
o_include_lib(_) ->
	no.

o_macro(Value) ->
    throw({open, {macro, Value}}).

o_record(Value) ->
    throw({open, {record, Value}}).

o_external(Module, Function, [_ | ParameterListTokens], ExternalModules, PathVars) ->
    ?D({Module, Function, ParameterListTokens}),
    N = erlide_text:guess_arity(ParameterListTokens),
    ?D(N),
    P = get_source_from_module(Module, ExternalModules, PathVars),
    throw({open, {external, Module, Function, N, P}}).

get_include_lib(Path) ->
    {Lib, Rest} = find_lib_dir(Path),
    {include, filename:join([Lib | Rest])}.

find_lib_dir(Dir) ->
    [Lib | Rest] = filename:split(Dir),
    {code:lib_dir(Lib), Rest}.


open_info(L, W, ExternalModules, PathVars) ->
    ?D({open_info, W, L}),
    {CL, CW} = erlide_text:clean_tokens(L, W),
    ?D({open_info, CW, CL}),
    case erlide_text:check_variable_macro_or_record(CL, CW) of
        {ok, M, R} ->
            {M, {R}};
        _ ->
            case erlide_text:check_function_call(CL, CW) of
                {ok, M, F, Rest} = _Xx ->
                    ?D(_Xx),
                    {external, {M, F, erlide_text:guess_arity(Rest),
                                get_source_from_module(M, ExternalModules, PathVars)}};
                {ok, F, Rest}=_Zz ->
                    ?D(_Zz),
                    {local, {F, erlide_text:guess_arity(Rest)}};
                _ ->
                    ?D(CL),
                    none
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
         [Var | Rest] ->
            filename:join([replace_path_var_aux(Var, PathVars) | Rest]);
         _ ->
             FileName
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
    ?D({ExternalModules, PathVars}),
    L = lists:append([get_external_modules_file(EM, PathVars) || EM <- string:tokens(ExternalModules, ";")]),
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
            Other
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



