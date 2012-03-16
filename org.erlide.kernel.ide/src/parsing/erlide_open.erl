%% Author: jakob
%% Created: Mar 23, 2006
%% Description: TODO: Add description to erlide_open
-module(erlide_open).
-author(jakobce@gmail.com).

%%
%% Exported Functions
%%
-export([open/3,
         find_first_var/2,
         get_source_from_module/2,
         get_external_modules/2,
         get_external_module/2,
         get_external_module_tree/1,
         get_external_include/2,
		 get_external_1/3,
         get_lib_dirs/0,
         get_lib_src_include/1,
         get_lib_files/1
        ]).

%% TODO (JC) there are some code duplication in external modules (and includes) handling

%%
%% Include files
%%

%% -define(DEBUG, 1).
%%-define(IO_FORMAT_DEBUG, 1).

-include("erlide.hrl").
-include("erlide_scanner.hrl").

-compile(export_all).

-record(open_context, {externalModules,
                       externalIncludes,
                       pathVars,
                       extraSourcePaths,
                       imports}).


%%
%% API Functions
%%

open(Mod, Offset, #open_context{imports=Imports0}=Context) ->
    ?D({Mod, Offset, Context}),
    Imports = erlide_util:add_auto_imported(Imports0),
    try
        {TokensWComments, BeforeReversed} =
            erlide_scanner_server:getTokenWindow(Mod, Offset, 45, 100),
        ?D({TokensWComments, BeforeReversed}),
        try_open(Offset, TokensWComments, BeforeReversed,
                 Context#open_context{imports=Imports}),
        error
    catch
        throw:{open, Res} ->
            Res;
        throw:T ->
            {error, T};
        error:E ->
            {error, E}
    end.

open_info(S, #open_context{}=Context) when is_list(S); is_binary(S) ->
    try
        TokensWComments = erlide_scanner:scan_string(S),
        ?D({open_info, S, TokensWComments}),
        try_open(0, TokensWComments, [], Context),
        error
    catch
        throw:{open, Res} ->
            Res;
        throw:T ->
            {error, T};
        error:E ->
            {error, E}
    end.

get_external_include(FilePath, #open_context{externalIncludes=ExternalIncludes, 
                                             pathVars=PathVars}) ->
    ?D(FilePath),
    ExtIncPaths = get_external_modules_files(ExternalIncludes, PathVars),
    get_ext_inc(ExtIncPaths, FilePath).

get_lib_dirs() ->
    CodeLibs = [D || D <- code:get_path(), D =/= "."],
    LibDir = code:lib_dir(),
    Libs = lists:filter(fun(N) -> lists:prefix(LibDir, N) end, CodeLibs),
    {ok, Libs}.

get_lib_src_include(Dir) ->
    Dirs = ["src", "include"],
    R = get_dirs(Dirs, get_lib_dir(Dir), []),
    {ok, R}.

get_dirs([], _, Acc) ->
    lists:reverse(Acc);
get_dirs([Dir | Rest], Base, Acc) ->
    D = filename:join(Base, Dir),
    case filelib:is_dir(D) of
        true ->
            get_dirs(Rest, Base, [D | Acc]);
        false ->
            get_dirs(Rest, Base, Acc)
    end.

get_lib_files(Dir) ->
    case file:list_dir(Dir) of
        %% TODO should we filter for erlang source-files here?
        {ok, SrcFiles} ->
            Files = [filename:join(Dir, SrcFile) || SrcFile <- SrcFiles],
            {ok, lists:filter(fun(F) -> filelib:is_regular(F) end, Files)};
        _ ->
            {ok, []}
    end.

get_includes_in_dir(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            {ok, filter_includes(Files)};
        _ ->
            {ok, []}
    end.

%%
%% Local Functions
%%

filter_includes(Files) ->
    filter_includes(Files, []).

filter_includes([], Acc) ->
    lists:reverse(Acc);
filter_includes([Filename | Rest], Acc) ->
    case filename:extension(Filename) of
        ".hrl" ->
            filter_includes(Rest, [Filename | Acc]);
        _ ->
            filter_includes(Rest, Acc)
    end.

get_lib_dir(Dir) ->
    B = filename:basename(Dir),
    case B of
        "ebin" ->
            filename:dirname(Dir);
        _ ->
            Dir
    end.
                
    
try_open(Offset, TokensWComments, BeforeReversedWComments, Context) ->
    Tokens = erlide_text:strip_comments(TokensWComments),
    BeforeReversed = erlide_text:strip_comments(BeforeReversedWComments),
    try_open_aux(Offset, Tokens, BeforeReversed, Context).

try_open_aux(Offset, Tokens, BeforeReversed, Context) ->
    case Tokens of
        [#token{offset=O} | _] = Tokens when O =< Offset ->
            ?D(Tokens),
            o_tokens(Tokens, Offset, Context, BeforeReversed),
            case BeforeReversed of
                [] ->
                    not_found;
                [B | Rest] ->
                    try_open_aux(Offset, [B | Tokens], Rest, Context)
            end;
        _ ->
            ok
    end.

has_prefix(Prefix, FileName) ->
    lists:prefix(Prefix, filename:basename(FileName)).

has_name(Name, FileName) ->
    Name == filename:rootname(filename:basename(FileName)).

get_external_modules(Prefix, #open_context{externalModules=ExternalModulesFiles, pathVars=PathVars}) ->
    ExternalModules = get_external_modules_files(ExternalModulesFiles, PathVars),
    {ok, [XM || XM <- ExternalModules, has_prefix(Prefix, XM)]}.

get_external_module_tree(#open_context{externalModules=ExternalModulesFiles, pathVars=PathVars}) ->
    {ok, get_external_module_tree(ExternalModulesFiles, PathVars)}.

get_external_module(Name, #open_context{externalModules=ExternalModulesFiles, pathVars=PathVars}) ->
    ExternalModules = get_external_modules_files(ExternalModulesFiles, PathVars),
    case [XM || XM <- ExternalModules, has_name(Name, XM)] of
        [Path | _] ->
            {ok, Path};
        _ ->
            not_found
    end.

get_external_module_tree(PackedFileNames, PathVars) ->
    Fun = fun(Parent, FileName, Acc) -> [{Parent, replace_path_var(FileName, PathVars), module} | Acc] end,
    Fun2 = fun(Parent, FileName, Acc) -> [{Parent, replace_path_var(FileName, PathVars), entry} | Acc] end,
    FileNames = erlide_util:unpack(PackedFileNames),
    R = fold_externals(Fun, Fun2, FileNames, PathVars),
    R.

consider_local([]) ->
    true;
consider_local([#token{kind=':'} | _]) ->
    false;
consider_local(_) ->
    true.

consider_macro_def([#token{kind=atom, value=define}, #token{kind='-'} | _]) ->
    true;
consider_macro_def([#token{kind='('} | Rest]) ->
    consider_macro_def(Rest);
consider_macro_def(_) ->
    false.

%% TODO: rewrite this with some kind of table, and make it possible to
%% add new items, e.g. gen_server calls

o_tokens([#token{kind=atom, value=include} | Rest], _, _, [#token{kind='-'} | _]) ->
    o_include(Rest);
o_tokens([#token{kind=atom, value=include_lib} | Rest], _, _, [#token{kind='-'} | _]) ->
    o_include_lib(Rest);
o_tokens([#token{kind=atom, value=define} | Rest], _, _, _) ->
    o_macro_def(Rest);
o_tokens([#token{kind=atom, value=record} | Rest], Offset, _, [#token{kind='-'} | _]) ->
    o_record_def(Rest, Offset);
o_tokens([#token{kind='#'}, #token{kind=atom, value=Value} | _] = Tokens, Offset, _, _) ->
    o_record(Tokens, Offset, Value);
o_tokens([#token{kind='#'}, #token{kind=macro, value=Value} | _] = Tokens, Offset, _, _) ->
    o_record(Tokens, Offset, Value);
o_tokens([#token{kind=atom, value=Module}, #token{kind=':'}, #token{kind=atom, value=Function},
          #token{kind='/'}, #token{kind=integer, value=Arity} | _], _, Context, _) ->
    o_external(Module, Function, Arity, Context);
o_tokens([#token{kind=atom, value=Module}, #token{kind=':'}, #token{kind=atom, value=Function} | Rest],
         _, Context, _) ->
    o_external(Module, Function, Rest, Context);
o_tokens([#token{kind=macro, value=Module}, #token{kind=':'}, #token{kind=atom, value=Function} | Rest],
         _, Context, _) ->
    o_external(Module, Function, Rest, Context);
o_tokens([#token{kind=atom, value=Function}, #token{kind='/'}, #token{kind=integer, value=Arity} | _],
         _, Context, [#token{kind=':'}, #token{kind=atom, value=Module} | _]) ->
    o_external(Module, Function, Arity, Context);
o_tokens([#token{kind=atom, value=Function}, #token{kind='/'}, #token{kind=integer, value=Arity} | _],
         _, Context, _BeforeReversed) ->
    o_local(Function, Arity, Context);
o_tokens([#token{kind='/'}, #token{kind=integer, value=Arity} | _],
         _, Context, [#token{kind=atom, value=Function} | _]) ->
    o_local(Function, Arity, Context);
o_tokens([#token{kind=macro, value=Value} | _] = Tokens, Offset, _, [#token{kind='#'} | _]) ->
    o_record(Tokens, Offset, Value);
o_tokens([#token{kind=macro, value=Value} | _], _, _, _) ->
    o_macro(Value);
o_tokens([#token{kind=atom, value=Function}, #token{kind='('} | Rest],
         _, Context, BeforeReversed) ->
    case consider_local(BeforeReversed) of
        true ->
            ?D(Rest),
            o_local(Function, erlide_text:guess_arity(Rest),
                    Context);
        false ->
            continue
    end;
o_tokens([#token{kind=var, value=VarName} | _], _, _, BeforeReversed) ->
    case consider_macro_def(BeforeReversed) of
        true ->
            throw({open, {macro_def, VarName}});
        false ->
            throw({open, {variable, VarName}})
    end;
o_tokens(_, _, _, _) ->
    no.

o_include([#token{kind='('}, #token{kind=string, value=File} | _]) ->
    throw({open, {include, File}});
o_include(_) ->
    no.

o_include_lib([#token{kind='('}, #token{kind=string, value=Path} | _]) ->
    ?D(Path),
    IncludeLib = get_otp_include_lib(Path),
    throw({open, IncludeLib});
o_include_lib(_) ->
    no.

o_macro(Value) ->
    throw({open, {macro, Value}}).

o_macro_def([#token{kind='('}, #token{kind=var, value=Value} | _]) ->
    throw({open, {macro, Value}});
o_macro_def([#token{kind='('}, #token{kind=atom, value=Value} | _]) ->
    throw({open, {macro, Value}}).

o_record(Tokens, Offset, Value) ->
    {State, _Name, Prefix, _Fields} =
        erlide_content_assist:check_record_tokens(upto_offset(Tokens, Offset)),
    case State of
        record_want_field -> throw({open, {field, Value, Prefix}});
        record_want_dot_field -> throw({open, {field, Value, Prefix}});
        record_dot_field -> throw({open, {field, Value, Prefix}});
        record_field -> throw({open, {field, Value, Prefix}});
        _ -> throw({open, {record, Value}})
    end.

upto_offset([#token{offset=O, length=L}=T | Rest], Offset) when Offset>=O+L ->
    [T | upto_offset(Rest, Offset)];
upto_offset([], _) ->
    [];
upto_offset([T | _], _) ->
    [T].

o_record_def([#token{kind='('}, #token{value=Value}, #token{kind=','} | Tokens], Offset) ->
    Between = erlide_np_util:get_between_outer_pars(Tokens, '{', '}'),
    o_record_def_aux(Between, Offset, Value, want_field).

o_record_def_aux([], _Offset, Record, _) ->
    throw({open, {record, Record}});
o_record_def_aux([#token{offset=O, length=L, value=Field} | _], Offset, Record, want_field) when Offset<O+L ->
    throw({open, {field, Record, Field}});
o_record_def_aux([#token{value=V} | _]=Tokens, Offset, Record, _) when V=:='('; V=:='{'; V=:='['; V=:='<<' ->
    Rest = erlide_text:skip_expr(Tokens),
    o_record_def_aux(Rest, Offset, Record, want_comma);
o_record_def_aux([#token{value=','} | Rest], Offset, Record, want_comma) ->
    o_record_def_aux(Rest, Offset, Record, want_field);
o_record_def_aux([_ | Rest], Offset, Record, W) ->
    o_record_def_aux(Rest, Offset, Record, W).

o_external(Module, Function, [_ | ParameterListTokens], Context) ->
    ?D({Module, Function, ParameterListTokens}),
    A = erlide_text:guess_arity(ParameterListTokens),
    ?D(A),
    P = get_source_from_module(Module, Context),
    throw({open, {external, Module, Function, A, P}});
o_external(Module, Function, Arity, Context) when is_integer(Arity) ->
    ?D({Module, Function, Arity}),
    P = get_source_from_module(Module, Context),
    throw({open, {external, Module, Function, Arity, P}}).

o_local(Function, Arity, #open_context{imports=Imports}=Context) ->
    case get_imported(Imports, {Function, Arity}) of
        false ->
            throw({open, {local, Function, Arity}});
        Module ->
            P = get_source_from_module(Module, Context),
            throw({open, {external, Module, Function, Arity, P}})
    end.

get_imported([], _) ->
    false;
get_imported([{Mod, Funcs} | Rest], Func) ->
    case lists:member(Func, Funcs) of
        true ->
            Mod;
        false ->
            get_imported(Rest, Func)
    end.

get_otp_include_lib(Path) ->
    {Lib, Rest} = find_lib_dir(Path),
    FileName = filename:basename(Rest),
    {include_lib, FileName, filename:join([Lib | Rest])}.

find_lib_dir(Dir) ->
    [Lib | Rest] = filename:split(Dir),
    ?D(Lib),
    {code:lib_dir(list_to_atom(Lib)), Rest}.

get_source_from_module(Mod, Context) ->
    case catch get_source(Mod) of
        {'EXIT', _E} ->
            ?D({get_source, _E}),
            case catch select_external(get_erl_from_dirs(Context#open_context.extraSourcePaths),
                                       atom_to_list(Mod)) of
                Path when is_list(Path) ->
                    Path;
                _ ->
                    get_source_from_external_modules(Mod, Context)
            end;
        Other ->
            Other
    end.

get_external_modules_files(PackedFileNames, PathVars) ->
    ?D(PackedFileNames),
    Fun = fun(_Parent, FileName, Acc) -> [replace_path_var(FileName, PathVars) | Acc] end,
    Fun2 = fun(_Parent, _FileName, Acc) -> Acc end,
    FileNames = erlide_util:unpack(PackedFileNames),
    R = fold_externals(Fun, Fun2, FileNames, PathVars), 
    %%?D(R),
    R.

replace_path_vars(FileNames, PathVars) ->
    [replace_path_var(F, PathVars) || F <- FileNames].

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

get_external_1(FileName0, PathVars, IsRoot) ->
    FileName = replace_path_var(FileName0, PathVars),
    FileNames = case IsRoot orelse filename:extension(FileName) == ".erlidex" of
                    true ->
                        case file:read_file(FileName) of
                            {ok, B} ->
                                erlide_util:split_lines(B);
                            _ ->
                                [FileName]
                        end;
                    false ->
                        [FileName]
                end,
    R = replace_path_vars(FileNames, PathVars),
    {ok, R}.

fold_externals(Fun, Fun2, FileNames, PathVars) ->
    {_Done, Acc} = fx(FileNames, Fun, Fun2, PathVars, "root", [], []),
    lists:reverse(Acc).

fx([], _Fun, _Fun2, _PathVars, _Parent, Done, Acc) ->
    {Done, Acc};
fx([FN0 | Rest], Fun, Fun2, PathVars, Parent, Done, Acc) ->
    FN = replace_path_var(FN0, PathVars),
    case lists:member(FN, Done) of
        true ->
            fx(Rest, Fun, Fun2, PathVars, Parent, Done, Acc);
        false ->
            case Parent=:="root" orelse filename:extension(FN) == ".erlidex" of
                true ->
                    {NewDone, NewAcc} = fx2(FN, Fun, Fun2, PathVars, Parent, Done, Acc),
                    fx(Rest, Fun, Fun2, PathVars, Parent, NewDone, NewAcc);
                false ->
                    fx(Rest, Fun, Fun2, PathVars, Parent, [FN | Done], Fun(Parent, FN, Acc))
            end
    end.

fx2(FN, Fun, Fun2, PathVars, Parent, Done, Acc) ->
    NewAcc = Fun2(Parent, FN, Acc),
    case file:read_file(FN) of
        {ok, B} ->
            Lines = erlide_util:split_lines(B),
            fx(Lines, Fun, Fun2, PathVars, FN, [FN | Done], NewAcc);
        _ ->
            {Done, Acc}
    end.

get_source_from_external_modules(Mod, #open_context{externalModules=ExternalModules,
                                                    pathVars=PathVars,
                                                    extraSourcePaths=ExtraSources}=_Context) ->
    ?D(_Context),
    L = get_external_modules_files(ExternalModules, PathVars),
    %%?D(lists:flatten(io_lib:format(">> ~p~n", [L]))),
    ?D({get_external_modules_files, length(L)}),
    _Extra = get_erl_from_dirs(ExtraSources),
    ?D({extra, _Extra}),
    select_external(L, atom_to_list(Mod)).

select_external([], _) ->
    not_found;
select_external([P | Rest], Mod) ->
    Name = filename:rootname(filename:basename(P)),
    %%?D({select_external, Name, Mod, P}),
    case Name of
        Mod ->
            P;
        _ ->
            select_external(Rest, Mod)
    end.

get_erl_from_dirs(undefined) ->
    [];
get_erl_from_dirs(L) ->
    ?D({get_erl_from_dirs, L}),
    lists:flatmap(fun(X) -> get_erl_from_dir(X) end,
                  L).

get_erl_from_dir(D) ->
    case file:list_dir(D) of
        {ok, Fs} ->
            [filename:join(D, F) || F<-Fs, filename:extension(F)==".erl"];
        _ ->
            []
    end.

get_source(Mod) ->
    L = Mod:module_info(compile),
    {value, {source, Path}} = lists:keysearch(source, 1, L),
    case filelib:is_regular(Path) of
        true ->
            ?D(Path),
            Path;
        false ->
            ?D(false),
            get_source_ebin(Mod)
    end.

find_first_var(Var, S) ->
    case catch get_var(Var, S) of
        {'EXIT', _} ->
            error;
        Other ->
            Other
    end.

get_ext_inc([], _) ->
    "";
get_ext_inc([P | Rest], FilePath) ->
    S = filename:join(P, FilePath),
    case filelib:is_regular(S) of
        true ->
            {ok, S};
        false ->
            get_ext_inc(Rest, FilePath)
    end.

get_source_ebin(Mod) ->
    EbinPath = code:which(Mod),
    BeamF = filename:basename(EbinPath),
    ErlF = filename:rootname(BeamF) ++ ".erl",
    SrcPath = filename:join([filename:dirname(filename:dirname(EbinPath)), "src", ErlF]),
    SrcPath.

get_var(Var, S) ->
    ?D({Var, S}),
    {ok, T, _} = erlide_scan:string(S),
    ?D(T),
    FV = find_var(T, Var),
    ?D(FV),
    {var, {{_Line, Offset}, Length}, _Var} = FV,
    {Offset, Length}.

find_var([], _) ->
    not_found;
find_var([{var, _, Var} = T | _], Var) ->
    T;
find_var([_ | Rest], Var) ->
    find_var(Rest, Var).



