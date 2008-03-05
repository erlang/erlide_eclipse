%% otp_doc - extract documentation from otp HTML
%%

-module(erlide_otp_doc).

-export([check_all/0,
         get_doc_from_scan_tuples/4,
         get_doc_from_fun_arity_list/3,
         get_all_links_to_other/0,
         get_exported/2,
         get_modules/2]).

-include_lib("kernel/include/file.hrl").

%-define(DEBUG, 1). 

-include("erlide.hrl").
-include("erlide_scanner.hrl").

%% recursively return tags for which Fun returns true
%%
%% recu_find_tags(L, Fun) ->
%%     lists:reverse(recu_find_tags(L, Fun, [])).

get_exported(M, Prefix) when is_atom(M), is_list(Prefix) ->
    case catch M:module_info(exports) of
        Val when is_list(Val) ->
            Fun = fun({N,_A}) -> lists:prefix(Prefix,atom_to_list(N)) end,
            lists:filter(Fun, Val);
    	_ ->
            error
    end.

get_modules(Prefix, Modules) when is_list(Prefix), is_list(Modules) ->
    M = Modules++[atom_to_list(I) || {I, _} <- code:all_loaded()],
	L = [I || I <- M, lists:prefix(Prefix, I)],
    lists:usort(L).

find_tags(L, Fun) ->
     lists:filter(Fun, L).        

%%recu_find_tags([], _Tags, Acc) ->
%%     Acc;
%% recu_find_tags([{_,_,_,L,_}=Tag | Rest], Fun, Acc0) ->
%%     Acc1 = recu_find_tags(L, Fun, Acc0),
%%     Acc2 = case Fun(Tag) of
%%                true ->
%%                    [Tag | Acc1];
%%                false ->
%%                    Acc1
%%            end,
%%     recu_find_tags(Rest, Fun, Acc2);
%% recu_find_tags([_ | Rest], Fun, Acc) ->
%%     recu_find_tags(Rest, Fun, Acc).

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
    ?D({'Docs', Docs}),
    R = [combine_docs(D) || D <- Docs],
    ?D({'R', R}),
    R.

get_doc_dir(Module) ->
    BeamFile = code:where_is_file(Module ++ ".beam"),
    ModDir = filename:dirname(filename:dirname(BeamFile)),
    filename:join([ModDir, "doc", "html"]).

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
        io:format("~p\n", [Mod]),
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
    DocDirs = get_all_doc_dirs(),
    _Mods = [list_to_atom(M) || {D, E} <- DocDirs, 
                                M <- get_mod_doc_files(D, E)],
    ok.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_doc_from_scan_tuples(Module, Offset, Imports, StateDir) ->
    Window = 12,
    List = erlide_scanner:getTokenWindow(Module, Offset, Window),
    ?D({get_doc, List}),
    D = case erlide_text:check_function_call(List, Window) of
            {ok, M, F, Rest} ->
                get_doc_for_external(StateDir, M, [{F, erlide_text:guess_arity(Rest)}]);
            {ok, F, Rest} ->
                ?D({F, Rest}),
                 get_doc_for_local(StateDir, F, erlide_text:guess_arity(Rest),
                                                Imports);
            _ ->
                {_Left, Right} = lists:split(2, List),
                [Token | _] = Right,
                [Token]
        end,
    lists:flatten(D).

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
        Doc = erlide_util:check_cached(DocFileName, IndexFileName, Renew),
        ?D({doc, Doc, FuncList}),
        PosLens = extract_doc_for_funcs(Doc, FuncList),
        get_doc(DocFileName, PosLens)
    catch
        exit:E -> E
    end.

get_doc_for_local(StateDir, F, A, Imports) ->
    ?D({F, A, Imports}),
    Mod = case erl_internal:bif(F, A) of
              true ->
                  erlang;
              false ->
                  get_imported(Imports, {F, A})
          end,
    get_doc_for_external(StateDir, Mod, [{F, A}]).

get_imported([], _) ->
    exit({error, doc_not_found});
get_imported([{Mod, Funcs} | Rest], Func) ->
    case lists:member(Func, Funcs) of
        true ->
            Mod;
        false ->
            get_imported(Rest, Func)
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
