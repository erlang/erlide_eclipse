%% otp_doc - extract documentation from otp HTML
%%

-module(erlide_otp_doc).

-export([check_all/0,
         get_doc/3,
         get_doc_from_fun_arity_list/3,
         get_all_links_to_other/0,
         get_exported/2,
         get_modules/3,
         get_proposals/3,
         get_all_doc_dirs/0]).

-export([fix_proposals/3]).

-include_lib("kernel/include/file.hrl").

%% -define(DEBUG, 1).

-include("erlide.hrl").

-define(CACHE_VERSION, 2).

%% recursively return tags for which Fun returns true
%%
%% recu_find_tags(L, Fun) ->
%%     lists:reverse(recu_find_tags(L, Fun, [])).

get_exported(M, Prefix) when is_atom(M), is_list(Prefix) ->
    case catch M:module_info(exports) of
        Val when is_list(Val) ->
            lists:filter(fun({N,_A}) ->
                                 lists:prefix(Prefix,atom_to_list(N))
                         end, Val);
    	_Error ->
            ?D(_Error),
            error
    end.

get_modules(Prefix, Modules, includes) when is_list(Prefix), is_list(Modules) ->
    get_modules(Prefix, Modules);
get_modules(Prefix, Modules, modules) ->
    LoadedModules = [atom_to_list(I) || {I, _} <- code:all_loaded()],
    get_modules(Prefix, Modules ++ LoadedModules).

get_modules(Prefix, Modules) when is_list(Prefix), is_list(Modules) ->
    L = [I || I <- Modules, lists:prefix(Prefix, I)],
    lists:usort(L).

find_tags(L, Fun) ->
     lists:filter(Fun, L).

%% return true for tags which is either <div> or <a name=...>
%% (used for extracting those with recu_find_tags in extract_from_file)
%%
%% is_a_slash_name_or_div_tag({_TagType, T, _Attrs, _Children, _Offset} = Tag) ->
is_a_slash_name_or_div_tag({_TagType, T, _Attrs, _Offset} = Tag) ->
    case T of
        "div" ->
            true;
        "a" ->
            is_a_slash_name_tag(Tag);
        _ ->
            false
    end.

extract_from_file(F) ->
    {ok, B} = file:read_file(F),
    P = scan(B), %% tags_to_tuples(B),
    E = find_tags(P, fun(Tag) -> is_a_slash_name_or_div_tag(Tag) end),
    T = get_functions_with_offsets(E),
    T.

%% from a list of a tags and div tags, collect list of list of
%% functions and offsets
%% get_functions_with_offsets(Tags) -> [[{f, 1}, ...], 100, 200]
get_functions_with_offsets(E) ->
    get_functions_with_offsets(E, [], 0, false, []).

get_functions_with_offsets([], [], _, _, Acc) ->
    lists:reverse(Acc); % done, return it
get_functions_with_offsets([], Functions, Offset, _, Acc) ->
    get_functions_with_offsets([], [], Offset, true,
                               [{lists:reverse(Functions), Offset, 1000000} | Acc]);
get_functions_with_offsets([{_TagType, "a", Attrs, Offs} | Rest],
                           [], _, _, Acc) -> % first func in first group, keep offset
    F = attrs_to_fn(Attrs),
    get_functions_with_offsets(Rest, [F], Offs, false, Acc);
get_functions_with_offsets([{_TagType, "a", Attrs, Offs} | Rest],
                           Functions, Offset, true, Acc) ->
    F = attrs_to_fn(Attrs), % first func in new group, keep offset, save old
    get_functions_with_offsets(Rest, [F], Offs, false,
                               [{lists:reverse(Functions), Offset, Offs - Offset} | Acc]);
get_functions_with_offsets([{_TagType, "a", Attrs, _} | Rest],
                           Functions, Offset, _, Acc) -> % func in group, add to functions
    F = attrs_to_fn(Attrs),
    get_functions_with_offsets(Rest, [F | Functions], Offset, false, Acc);
get_functions_with_offsets([_ | Rest], Functions, Offset, _, Acc) ->
    get_functions_with_offsets(Rest, Functions, Offset, true, Acc).

%% check for slash in name attribute, or minus, since edoc
%% of some reason uses minus in anchor tags instead of slash
%%
is_a_slash_name_tag({_TagType, "a", ["NAME=" ++ Name | _ ], _}) ->
    is_slash_int(Name);
is_a_slash_name_tag({_TagType, "a", ["name=" ++ Name | _ ], _}) ->
    is_slash_int(Name);
is_a_slash_name_tag(_) ->
    false.

split_name(Name) ->
    Nm = string:strip(Name, both, $"),
    case string:tokens(Nm, "/") of
        [A, B] -> [A, B];
        _ -> string:tokens(Nm, "-")
    end.

is_slash_int(Name) ->
    case split_name(Name) of
        [_N, I] ->
            I -- "0123456789" == "";
        _ ->
            false
    end.

%% if a string contains : remove all upto and including :
%% "mod:fun" -> "fun"
remove_module(A) ->
    string:substr(A, string:rchr(A, $:)+1).

%% convert name attribute (as first element in list) to
%% a {Function, Arity} tuple
%%
attrs_to_fn(["NAME=" ++ Name | _ ]) ->
    attrs_to_fn_x(Name);
attrs_to_fn(["name=" ++ Name | _ ]) ->
    attrs_to_fn_x(Name).

attrs_to_fn_x(Name0) ->
    Name1 = string:strip(Name0, both, $"),
    [Name2, ArityS] = split_name(Name1),
    Name3 = remove_module(Name2),
    Arity = list_to_integer(ArityS),
    {Name3, Arity}.

%% scan(S) -> [{tag, Name, Attr, Offset}]
%% scan a tagged text (such as HTML) and return a list of tags
%% and offsets
%%
scan(S) when is_list(S) ->
    scan(S, 0, []);
scan(B) when is_binary(B) ->
    scan(binary_to_list(B)).

scan([$< | Rest0], I, Acc) -> % tag start, scan tag
    {ok, Rest1, Tag, D} = scan_tag(Rest0, 0, ""),
    scan(Rest1, I+D+1, [{Tag, I} | Acc]);
scan([_ | Rest], I, Acc) -> % just text w/o tag, skip
    scan(Rest, I+1, Acc);
scan([], _I, Acc) -> % done, let's split tags on attribute
    lists:map(fun({Tag, Offset}) ->
                  split_tag(Tag, Offset)
              end, lists:reverse(Acc)).

%% scan tag to end of tag, considering / and !
scan_tag([$! | S], I0, Acc) ->
    {Rest, Tag, I1} = scan_s(S, $>, I0+1, Acc),
    {ok, Rest, {comment_tag, Tag}, I1};
scan_tag([$/ | S], I0, Acc) ->
    {Rest, Tag, I1} = scan_s(S, $>, I0+1, Acc),
    {ok, Rest, {end_tag, Tag}, I1};
scan_tag(S, I0, Acc) ->
    {Rest, Tag, I1} = scan_s(S, $>, I0, Acc),
    {ok, Rest, {tag, Tag}, I1}.

scan_s([End, End | Rest], End, I, Acc) ->
    scan_s([End | Rest], End, I+1, Acc);
scan_s([End | Rest], End, I, Acc) ->
    {Rest, lists:reverse(Acc), I+1};
scan_s([$\\, C | Rest], End, I, Acc) ->
    scan_s(Rest, End, I+2, [C | Acc]);
scan_s([34 | Rest0], End, I, Acc) ->
    {Rest1, S, D} = scan_s(Rest0, 34, 0, ""),
    scan_s(Rest1, End, I+D+1, "\"" ++ lists:reverse(S) ++ "\"" ++ Acc);
scan_s([A | Rest], End, I, Acc) ->
    scan_s(Rest, End, I+1, [A | Acc]);
scan_s([], _End, I, Acc) ->
    {"", lists:reverse(Acc), I}.

%% split tag string to tag tuple with attribute list
split_tag({TagType, S}, Offset) ->
    {Rest, Tag, _} = scan_s(S, $  , 0, ""),
    {Tag, Attrs} = split_tag(Rest, Tag, []),
    LTag = string:to_lower(Tag),
    {tag_type(TagType, LTag), LTag, Attrs, Offset}.

split_tag("", Tag, Acc) ->
    {Tag, lists:reverse(Acc)};
split_tag(S, Tag, Acc) ->
    {Rest, R, _} = scan_s(S, $  , 0, ""),
    split_tag(Rest, Tag, [R | Acc]).

%% convert list of tags to hierarchy, grouping begin_tag and end_tag
%% into a tuple
%%
%% tags_to_tuples(X) ->
%%     Scan = scan(X),
%%     tags_to_tuples(Scan, [], [], 0, 0, []).
%%
%% tags_to_tuples([], _CurTag, _CurAttrs, _Lvl, _BeginOffset, Acc) ->
%%     lists:reverse(Acc);
%% tags_to_tuples([{begin_tag, Tag, Attrs, Offset} | Rest0], CurTag, CurAttrs,
%%                Lvl, BeginOffset, Acc) ->
    %%io:format("begin_tag ~p Lvl ~p\n", [Tag, Lvl]),
%%     {Rest1, Children} = tags_to_tuples(Rest0, Tag, Attrs, Lvl+1, Offset, []),
%%     tags_to_tuples(Rest1, CurTag, CurAttrs, Lvl, BeginOffset, [Children | Acc]);
%% tags_to_tuples([{end_tag, CurTag, [], _Offset} | Rest], CurTag, CurAttrs, _Lvl,
%%                BeginOffset, Acc) ->
    %%io:format("end_tag ~p Lvl ~p length(Rest) ~p\n", [CurTag, _Lvl, length(Rest)]),
%%     {Rest, {tag, CurTag, CurAttrs, lists:reverse(Acc), BeginOffset}};
%% tags_to_tuples([S | Rest], CurTag, CurAttrs, Lvl, BeginOffset, Acc) ->
%%     tags_to_tuples(Rest, CurTag, CurAttrs, Lvl, BeginOffset, [S | Acc]).
%%
%% add_attrs([]) ->
%%     "";
%% add_attrs(A) ->
%%     add_attrs(A, []).
%%
%% add_attrs([A], Acc) ->
%%     [" ", Acc | A];
%% add_attrs([A | Rest], Acc) ->
%%     add_attrs(Rest, [Acc, A | ","]).
%%
%% to_lower(N) ->
%%     httpd_util:to_lower(N).

%% return begin_tag if this is a tag that requires a slash-tag
%% otherwise return tag
tag_type(tag, "p") -> tag;
tag_type(tag, "br") -> tag;
tag_type(tag, "hr") -> tag;
tag_type(tag, "img") -> tag;
tag_type(tag, "link") -> tag;
tag_type(tag, Tag) ->
    case lists:last(Tag) of
        $/ -> tag;
        _ -> begin_tag
    end;
tag_type(T, _) -> T.

listify(A) when is_atom(A) ->
    atom_to_list(A);
listify(L) when is_list(L) ->
    L.

combine_docs([]) ->
    [];
combine_docs([D | Rest]) ->
    combine_docs(Rest, [D]).

combine_docs([], Acc) ->
    lists:flatten(Acc);
combine_docs([D | Rest], Acc) ->
    combine_docs(Rest, [Acc, "<p>", D]).

get_doc(_DocFileName, []) ->
    "";
get_doc(DocFileName, PosLens) ->
    ?D({get_doc, DocFileName, PosLens}),
    {ok, F} = file:open(DocFileName, [read, raw]),
    PL = lists:flatten(PosLens),
    {ok, AppendedDocs} = file:pread(F, PL),
    file:close(F),
    Docs = unappend(AppendedDocs, PosLens),
    R = [combine_docs(D) || D <- Docs],
    R.

get_doc_dir(Module) ->
    BeamFile = code:where_is_file(Module ++ ".beam"),
    ModDir = hack_erts_bad_doc_location(BeamFile),
    ?D(ModDir),
    filename:join([ModDir, "doc", "html"]).

hack_erts_bad_doc_location(BeamFile) ->
    ?D(BeamFile),
    Dir0 = filename:dirname(filename:dirname(BeamFile)),
    ?D(Dir0),
    Dir1 = filename:basename(Dir0),
    ?D(Dir1),
    case Dir1 of
	"erts"++_ ->
	    filename:join([filename:dirname(filename:dirname(Dir0)), Dir1]);
	_ ->
	    Dir0
    end.    

extract_doc_for_func(Doc, Func) ->
    extract_doc_for_func(Doc, Func, []).

extract_doc_for_func([], _, Acc) ->
    lists:reverse(Acc);
extract_doc_for_func([{Funcs, Pos, Len} | Rest], Func, Acc0) ->
    Acc1 = case has_func(Funcs, Func) of
               true ->
                   [{Pos, Len} | Acc0];
               false ->
                   Acc0
           end,
    extract_doc_for_func(Rest, Func, Acc1).

extract_doc_for_funcs(Doc, FuncList) ->
    [extract_doc_for_func(Doc, {listify(F), A}) || {F, A} <- FuncList].

has_func([], _) ->
    false;
has_func([{F, A} | _], {F, A}) ->
    true;
has_func([{F, _} | _], {F, -1}) ->
    true;
has_func([_ | Rest], Func) ->
    has_func(Rest, Func).


get_all_doc_dirs() ->
    Paths = code:get_path(),
    OtpPaths = lists:filter(fun(Path) ->
                                    string:str(Path, "otp") =/= 0
                            end, Paths),
    Dirs0 = lists:map(fun(D) ->
                             ModDir = filename:dirname(D),
                             {filename:join([ModDir, "doc", "html"]), D}
                     end, OtpPaths),
    lists:filter(fun({D,_}) -> filelib:is_dir(D) end, Dirs0).

remove_ext(F) ->
    remove_ext_x(lists:reverse(F)).

remove_ext_x("." ++ Rest) ->
    lists:reverse(Rest);
remove_ext_x("") ->
    "";
remove_ext_x([_ | Rest]) ->
    remove_ext_x(Rest).

get_mod_doc_files(DocDir, EbinDir) ->
    Dl = filelib:wildcard("*.html", DocDir),
    El = filelib:wildcard("*.beam", EbinDir),
    D0 = [remove_ext(D) || D <- Dl],
    E0 = [remove_ext(E) || E <- El],
    D1 = sets:from_list(D0),
    E1 = sets:from_list(E0),
    Intersection = sets:intersection(D1, E1),
    sets:to_list(Intersection).

get_all_docs_for_mod(Mod) ->
    E0 = Mod:module_info(exports),
    Exports = lists:filter(fun({module_info, _}) -> false;
                              (_) -> true
                           end, E0),
    Docs = [case get_doc_for_external(".", Mod, [{Func, Arity}]) of
                [] -> {Mod, Func, Arity};
                _ -> []
            end || {Func, Arity} <- Exports],
    Docs.

check_all() ->
    DocDirs = get_all_doc_dirs(),
    Mods = [list_to_atom(M) || {D, E} <- DocDirs,
                               M <- get_mod_doc_files(D, E)],
    NotFound = [get_all_docs_for_mod(Mod) || Mod <- Mods],
    lists:flatten(NotFound).

%% report_on_file(FileName) ->
%%     Missing = check_all(),
%%     file:delete(FileName),
%%     {ok, F} = file:open(FileName, [write, delayed_write]),
%%     lists:foreach(fun({Mod, Fun, Arity}) ->
%%                       io:format(F, "~p:~p/~p\n", [Mod, Fun, Arity])
%%                   end, Missing),
%%     file:close(F).

get_all_links_to_other() ->
%%     DocDirs = get_all_doc_dirs(),
%%     _Mods = [list_to_atom(M) || {D, E} <- DocDirs,
%%                                 M <- get_mod_doc_files(D, E)],
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_doc(Module, Input, StateDir) ->
    try
        ?D(Input),
        case Input of
            {external, M, F, A, _Path} = External ->
                ?D({open, External}),
                case get_doc_for_external(StateDir, M, [{F, A}]) of
                    D when is_list(D) ->
                        {ok, lists:flatten(D), External};
                    _Error ->
                        External
                end;
            {local, F, A} = Local ->
                case get_doc_for_external(StateDir, Module, [{F, A}]) of
                    D when is_list(D) ->
                        {ok, lists:flatten(D), Local};
                    _Error ->
                        Local
                end;
            {macro, Macro} ->
                {macro, Macro};
            {record, Record} ->
                {record, Record};
            {field, Record, Field} ->
                {field, Record, Field};
            Error ->
                ?D(Error),
                {error, Error}
        end
    catch
        error:E ->
            {error, E};
        exit:E ->
            {error, E}
    end.

e(E) ->
    E.

get_doc_for_external(StateDir, Mod, FuncList) ->
    try
        Module = listify(Mod),
        ?D(Module),
        OutDir = get_doc_dir(Module),
        ?D(OutDir),
        DocFileName = filename:join(OutDir, Module ++ ".html"),
        IndexFileName = filename:join([StateDir, "erlide_doc",
                                       Module ++ ".erlide_doc_x"]),
        filelib:ensure_dir(IndexFileName),
        Renew = fun(F) -> extract_from_file(F) end,
		?D({DocFileName, IndexFileName, Renew}),
        {_Cached, Doc} = erlide_util:check_and_renew_cached(DocFileName, IndexFileName, ?CACHE_VERSION, Renew, true),
        ?D({doc, _Cached, Doc, FuncList}),
        PosLens = extract_doc_for_funcs(Doc, FuncList),
        get_doc(DocFileName, PosLens)
    catch
	exit:E ->
	    ?D(E),
	    e(E),
	    E;
	error:E ->
	    ?D(E),
	    e(E),
	    E
    end.

get_doc_from_fun_arity_list(Mod, List, StateDir) ->
    get_doc_for_external(StateDir, Mod, List).

unappend(Flat, L) ->
    unappend(L, Flat, []).

unappend([], [], Acc) ->
    lists:reverse(Acc);
unappend([L | Rest], Flat, Acc) ->
    {F, FRest} = get_sublist(L, Flat),
    unappend(Rest, FRest, [F | Acc]).

get_sublist(L, Flat) ->
    get_sublist(L, Flat, []).

get_sublist([], Flat, Acc) ->
    {lists:reverse(Acc), Flat};
get_sublist([_ | Rest], [F | FRest], Acc) ->
    get_sublist(Rest, FRest, [F | Acc]).


%% Get exported functions with documentation
%% [{FunWithArity, FunWithParameters, [{Offset, Length}, Doc]}]

get_proposals(Mod0, Prefix, StateDir) ->
    {Mod, Functions} = case Mod0 of
		    '<auto_imported>' ->
			{erlang, erlide_util:get_auto_imported(Prefix)};
		    _ ->
			{Mod0, get_exported(Mod0, Prefix)}
		end,
    case Functions of
	L when is_list(L) ->
	    DocList = case get_doc_from_fun_arity_list(Mod, L, StateDir) of
			  S when is_list(S) ->
			      S;
			  _ ->
			      lists:duplicate(length(L), "")
		      end,
	    fix_proposals(L, DocList, length(Prefix));
	Error ->
	    Error
    end.

fix_proposals(FunArityList, DocList, PrefixLength) ->
%%     ?Debug({prefixLength, PrefixLength}),
    fix_proposals(FunArityList, DocList, PrefixLength, []).

fix_proposals([], _, _, Acc) ->
    lists:reverse(Acc);
fix_proposals([{FunctionName, Arity} | FALRest], [Doc | DLRest], PrefixLength, Acc) ->
    FunName = atom_to_list(FunctionName),
    FunWithArity = FunName++"/"++integer_to_list(Arity),
    Offset = length(FunName)+1-PrefixLength,
    {TextPars, Pars} = extract_pars(FunctionName, Arity, Offset, Doc),
    ?D(TextPars),
    ?D(Pars),
    FunWithParameters = FunName++"("++TextPars++")",
    fix_proposals(FALRest, DLRest, PrefixLength,
                  [{FunWithArity, FunWithParameters, Pars, Doc} | Acc]).

extract_pars(FunctionName, Arity, Offset, Doc) ->
    Sub1 = erlide_util:get_all_between_strs(Doc, "<CODE>", "</CODE>"),
    Sub2 = erlide_util:get_all_between_strs(Doc, "<a name =", "</span>"),
    Sub3 = erlide_util:get_all_between_strs(Doc, "<a name=", "</span>"),
    try_make_pars(Sub1++Sub2++Sub3, FunctionName, Arity, Offset).

try_make_pars([], _, Arity, Offset) ->
    {make_parameters(Arity), make_par_offs_length(0, Arity, Offset)};
try_make_pars([Sub | Rest], FunctionName, Arity, Offset) ->
    case erl_scan:string(Sub) of
	{ok, Tokens, _} ->
	    ?D(Tokens),
	    case make_pars_from_tokens(Tokens, FunctionName, Arity, Offset) of
		bad_tokens ->
		    try_make_pars(Rest, FunctionName, Arity, Offset);
		T ->
		    ?D(T),
		    T
	    end;
	_E ->
	    ?D(_E),
	    try_make_pars(Rest, FunctionName, Arity, Offset)
    end.

skip_lpar([{'(', _} | Rest]) ->
    Rest;
skip_lpar([]) ->
    [];
skip_lpar([_ | Rest]) ->
    skip_lpar(Rest).

make_pars_from_tokens([{atom, _, FunctionName} | Rest], FunctionName, Arity, Offset) ->
    ?D(FunctionName),
    Pars = get_vars_upto_rpar(skip_lpar(Rest)),
    ?D(Pars),
    ?D(Arity),
    case length(Pars) of
	Arity ->
	    {make_parameters(Pars), make_par_offs_length(0, Arity, Pars, Offset)};
	_ ->
	    bad_tokens
    end;
make_pars_from_tokens([_ | Rest], FunctionName, Arity, Offset) ->
    make_pars_from_tokens(Rest, FunctionName, Arity, Offset);
make_pars_from_tokens(_, _, _, _) ->
    bad_tokens.

get_vars_upto_rpar(Tokens) ->
    get_vars_upto_rpar(Tokens, []).

get_vars_upto_rpar([], Acc) ->
    lists:reverse(Acc);
get_vars_upto_rpar([{')', _} | _], Acc) ->
    lists:reverse(Acc);
get_vars_upto_rpar([{var, _, Par} | Rest], Acc) ->
    get_vars_upto_rpar(Rest, [atom_to_list(Par) | Acc]);
get_vars_upto_rpar([_ | Rest], Acc) ->
    get_vars_upto_rpar(Rest, Acc).

make_parameters(0) ->
    "";
make_parameters(1) ->
    "_";
make_parameters(N) when is_integer(N) ->
    "_, " ++ make_parameters(N-1);
make_parameters([]) ->
    "";
make_parameters([Par]) ->
    Par;
make_parameters([Par | Rest]) ->
    Par ++ ", " ++ make_parameters(Rest).

make_par_offs_length(N, N, _) ->
    [];
make_par_offs_length(I, N, Offset) ->
    [{Offset, 1} | make_par_offs_length(I+1, N, Offset + 3)].

make_par_offs_length(N, N, _ParTokens, _Offset) ->
    [];
make_par_offs_length(I, N, [Par | Rest], Offset) ->
    Len = length(Par),
    [{Offset, Len} | make_par_offs_length(I+1, N, Rest, Offset + Len + 2)].






