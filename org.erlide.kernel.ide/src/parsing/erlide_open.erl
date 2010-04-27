%% Author: jakob
%% Created: Mar 23, 2006
%% Description: TODO: Add description to erlide_open
-module(erlide_open).
-author(jakobce@gmail.com).

%%
%% Exported Functions
%%
-export([open/3,
         open_info/3,
         find_first_var/2,
         get_source_from_module/2,
         get_include_lib/1,
         get_external_modules/2,
         get_external_module/2,
         get_external_include/2]).

%% TODO (JC) there are some code duplication in external modules (and includes) handling

%%
%% Include files
%%

%%-define(DEBUG, 1).
%%-define(IO_FORMAT_DEBUG, 1).

-include("erlide.hrl").
-include("erlide_scanner.hrl").

-compile(export_all).

-record(open_context, {externalModules, externalIncludes, pathVars, extraSourcePaths}).


%%
%% API Functions
%%


open(Mod, Offset, #open_context{}=Context) ->
    ?D({Mod, Offset, Context}),
    try
        {TokensWComments, BeforeReversed} =
            erlide_scanner_server:getTokenWindow(Mod, Offset, 5, 100),
        ?D({TokensWComments, BeforeReversed}),
        try_open(Mod, Offset, TokensWComments, BeforeReversed,
                 Context),
        error
    catch
        throw:{open, Res} ->
            Res;
        throw:T ->
            {error, T};
        error:E ->
            {error, E}
    end.

try_open(Mod, Offset, TokensWComments, BeforeReversed, Context) ->
    case erlide_text:strip_comments(TokensWComments) of
	[#token{offset=O} | _] = Tokens when O =< Offset ->
	    ?D(Tokens),
	    o_tokens(Tokens, Context, BeforeReversed),
	    case BeforeReversed of
		[] ->
		    not_found;
		[B | Rest] ->
		    try_open(Mod, Offset, [B | TokensWComments], Rest, Context)
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

get_external_module(Name, #open_context{externalModules=ExternalModulesFiles, pathVars=PathVars}) ->
    ExternalModules = get_external_modules_files(ExternalModulesFiles, PathVars),
    case [XM || XM <- ExternalModules, has_name(Name, XM)] of
        [Path | _] ->
            {ok, Path};
        _ ->
            not_found
    end.

consider_local([]) ->
    true;
consider_local([#token{kind=':'} | _]) ->
    false;
consider_local([#token{kind=comment} | More]) ->
    consider_local(More);
consider_local(_) ->
    true.

%% TODO: rewrite this with some kind of table, and make it possible to
%% add new items, e.g. gen_server calls

o_tokens([#token{kind=atom, value=include} | Rest], _, [#token{kind='-'} | _]) ->
    o_include(Rest);
o_tokens([#token{kind=atom, value=include_lib} | Rest], _, [#token{kind='-'} | _]) ->
    o_include_lib(Rest);
o_tokens([#token{kind=macro, value=Value} | _], _, _) ->
    o_macro(Value);
o_tokens([#token{kind='#'}, #token{kind=atom, value=Value} | _], _, _) ->
    o_record(Value);
o_tokens([#token{kind=atom, value=Module}, #token{kind=':'}, #token{kind=atom, value=Function},
	  #token{kind='/'}, #token{kind=integer, value=Arity} | _],
         Context, _) ->
    o_external(Module, Function, Arity, Context);
o_tokens([#token{kind=atom, value=Module}, #token{kind=':'}, #token{kind=atom, value=Function} | Rest],
         Context, _) ->
    o_external(Module, Function, Rest, Context);
o_tokens([#token{kind=atom, value=Function}, #token{kind='/'}, #token{kind=integer, value=Arity} | _],
	 Context, [#token{kind=':'}, #token{kind=atom, value=Module} | _]) ->
    o_external(Module, Function, Arity, Context);
o_tokens([#token{kind=atom, value=Function}, #token{kind='/'}, #token{kind=integer, value=Arity} | _], _, _) ->
    throw({open, {local, Function, Arity}});
o_tokens([#token{kind='/'}, #token{kind=integer, value=Arity} | _], _, [#token{kind=atom, value=Function} | _]) ->
    throw({open, {local, Function, Arity}});
o_tokens([#token{kind=atom, value=Function}, #token{kind='('} | Rest], _, BeforeReversed) ->
    case consider_local(BeforeReversed) of
	true ->
	    ?D(Rest),
	    throw({open, {local, Function, erlide_text:guess_arity(Rest)}});
	false ->
	    continue
    end;
o_tokens([#token{kind=var, value=VarName} | _], _, _) ->
    throw({open, {variable, VarName}});
o_tokens(_, _, _) ->
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

o_external(Module, Function, [_ | ParameterListTokens], Context) ->
    ?D({Module, Function, ParameterListTokens}),
    N = erlide_text:guess_arity(ParameterListTokens),
    ?D(N),
    P = get_source_from_module(Module, Context),
    throw({open, {external, Module, Function, N, P}});
o_external(Module, Function, Arity, Context) when is_integer(Arity) ->
    ?D({Module, Function, Arity}),
    P = get_source_from_module(Module, Context),
    throw({open, {external, Module, Function, Arity, P}}).

get_include_lib(Path) ->
    {Lib, Rest} = find_lib_dir(Path),
    {include, filename:join([Lib | Rest])}.

find_lib_dir(Dir) ->
    [Lib | Rest] = filename:split(Dir),
    {code:lib_dir(list_to_atom(Lib)), Rest}.


open_info(Mod, Offset, #open_context{}=Context) ->
    CW = 5,
    {TokensWComments, BeforeReversed} =
	erlide_scanner_server:getTokenWindow(Mod, Offset, CW, 50),
    CL = lists:reverse(BeforeReversed, TokensWComments),
    ?D({open_info, CW, CL}),
    case erlide_text:check_variable_macro_or_record(CL, CW) of
        {ok, M, R} ->
            {M, {R}};
        _ ->
            case erlide_text:check_function_call(CL, CW) of
                {ok, M, F, Rest} = _Xx ->
                    ?D(_Xx),
                    {external, {M, F, erlide_text:guess_arity(Rest),
                                get_source_from_module(M, Context)}};
                {ok, F, Rest}=_Zz ->
                    ?D(_Zz),
                    {local, {F, erlide_text:guess_arity(Rest)}};
                _ ->
                    ?D(CL),
                    none
            end
    end.

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
    {_, R} = get_external_modules_files(erlide_util:unpack(PackedFileNames), PathVars, true, [], []),
    R.

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

get_external_modules_files(Filenames, PathVars, Top, Done, Acc) ->
    Fun = fun(Filename0, {Done0, Acc0}) ->
		  Filename = replace_path_var(Filename0, PathVars),
		  case lists:member(Filename, Done0) of
		      true ->
			  {Done0, Acc0};
		      false ->
			  Done1 = [Filename | Done0],
			  case Top orelse filename:extension(Filename) == ".erlidex" of
			      true ->
				  case file:read_file(Filename) of
				      {ok, B} ->
					  get_external_modules_files(
					    erlide_util:split_lines(B), 
					    PathVars, false, Done1, Acc0);
				      _ ->
					  {Done1, Acc0}
				  end;
			      false ->
				  {Done1, [Filename | Acc0]}
			  end
		  end
	  end,
    lists:foldl(Fun, {Done, Acc}, Filenames).

get_source_from_external_modules(Mod, #open_context{externalModules=ExternalModules, 
													pathVars=PathVars, 
													extraSourcePaths=ExtraSources}=_Context) ->
    ?D(_Context),
    L = get_external_modules_files(ExternalModules, PathVars),
    %%?D(lists:flatten(io_lib:format(">> ~p~n", [L]))),
	?D({get_external_modules_files, length(L)}),
	Extra = get_erl_from_dirs(ExtraSources),
	?D({extra, Extra}),
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
	lists:flatmap(fun(X) -> get_erl_from_dir(X) end, 
				  L).
   
get_erl_from_dir(D) ->
	{ok, Fs} = file:list_dir(D),
	[filename:join(D, F) || F<-Fs, filename:extension(F)==".erl"] .

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

get_external_include(FilePath, #open_context{externalIncludes=ExternalIncludes, pathVars=PathVars}) ->
    ExtIncPaths = get_external_modules_files(ExternalIncludes, PathVars),
    get_ext_inc(ExtIncPaths, FilePath).

%% Local Functions
%%

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



