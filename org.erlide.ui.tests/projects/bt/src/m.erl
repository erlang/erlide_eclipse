%% Description: Split source files into functions and clauses and collects references
%% Author: jakob
%% Created: Mar 23, 2006

-module(erlide_noparse).

%%
%% Exported Functions
%%

%% called from Java 
-export([initial_parse/5, reparse/1, remove_cache_files/2]).

%% called from Erlang
-export([read_module_refs/3]).

%%
%% Include files
%%

%% -define(DEBUG, 1).
%% -define(IO_FORMAT_DEBUG, 1).

-define(CACHE_VERSION, 25).
-define(SERVER, erlide_noparse).

-include("erlide.hrl").
-include("erlide_noparse.hrl").
-include("erlide_scanner.hrl").
-include("erlide_search_server.hrl").

%% special arity flags
-define(ARI_TYPESPEC, -2).
-define(ARI_ATTRIBUTE, -3).
-define(ARI_RECORD_DEF, -4).
-define(ARI_MACRO_DEF, -5).
-define(ARI_INCLUDE, -6).
-define(ARI_RECORD_FIELD_DEF, -7).

-define(X(X), erlide_log:log({X, erlang:now()})).


%%
%% API Functions äöå
%%

initial_parse(ScannerName, ModuleFileName, StateDir, UseCache,
              UpdateSearchServer) ->
    try ini
        %%         ?D({StateDir, ModuleFileName}),
        BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
        RefsFileName = BaseName ++ ".refs",
        RenewFun = fun(_F) ->
                           do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer)
                   end,
        CacheFileName = BaseName ++ ".noparse",
        ?D(CacheFileName),
        {Cached, Res} = erlide_util:check_and_renew_cached(
                          ModuleFileName, CacheFileName, ?CACHE_VERSION, 
                          RenewFun, UseCache),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        ?D(updated),
        {ok, Res, Cached}
    catch
        error:Reason ->
            {error, Reason}
    end.

reparse(ScannerName) ->
    try
        Res = do_parse(ScannerName, "", "", true),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        {ok, Res, unused}
    catch
        error:Reason ->
            {error, Reason}
    end.

read_module_refs(ScannerName, ModulePath, StateDir) ->
    ?D(ScannerName),
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    RefsFileName = BaseName ++ ".refs",
    ?D(RefsFileName),
    case file:read_file(RefsFileName) of
        {ok, Binary} ->
            ?D(byte_size(Binary)),
            binary_to_term(Binary);
        _ ->
            ?D(ModulePath),
            {ok, InitialTextBin} = file:read_file(ModulePath),
            ?D(byte_size(InitialTextBin)),
            InitialText = binary_to_list(InitialTextBin),
            _D = erlide_scanner_server:initialScan(
                   ScannerName, ModulePath, InitialText, StateDir, true),
            ?D(_D),
            initial_parse(ScannerName, ModulePath, StateDir, true, false),
            ?D(RefsFileName),
            {ok, Binary} = file:read_file(RefsFileName),
            ?D(byte_size(Binary)),
            binary_to_term(Binary)
    end.

remove_cache_files(ScannerName, StateDir) ->
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    ScannerCacheFileName = BaseName ++ ".scan",
    file:delete(ScannerCacheFileName),
    RefsFileName = BaseName ++ ".refs",
    file:delete(RefsFileName),
    CacheFileName = BaseName ++ ".noparse",
    file:delete(CacheFileName).

%%
%% Internal functions
%%

do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer) ->
    Toks = erlide_scanner_server:getTokens(ScannerName),
    do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer).

do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer) ->
    ?D({do_parse, ScannerName, length(Toks)}),
    ?X(111),
    {UncommentToks, Comments} = erlide_np_util:extract_comments(Toks),
    ?X(222),
    ?D({length(UncommentToks), length(Comments)}),
    ?D({UncommentToks}),
    Functions = erlide_np_util:split_after_dots(UncommentToks),
    ?X(333),
    ?D(length(Functions)),
    AutoImports = erlide_util:add_auto_imported([]),
    ?X(444),
%%     ?D(AutoImports),
    {Collected, Refs} = classify_and_collect(Functions, [], [], [], AutoImports),
    ?X(555),
    ?D({'>>',length(Collected)}),
    CommentedCollected = erlide_np_util:get_function_comments(Collected, Comments),
    ?X(666),
    ?D(CommentedCollected),
    Model = #model{forms=CommentedCollected, comments=Comments},
    %%erlide_noparse_server:create(ScannerName, Model),
%%     ?D({"Model", length(Model#model.forms), erts_debug:flat_size(Model)}),
    FixedModel = fixup_model(Model),
    ?X(777),
%%     ?D(erts_debug:flat_size(FixedModel)),
    %% 	?D([erts_debug:flat_size(F) || F <- element(2, FixedModel)]),
    ?D(StateDir),
    case StateDir of
        "" -> ok;
        _ ->
            ?D({RefsFileName, length(Refs)}),
            file:write_file(RefsFileName, term_to_binary(Refs, [compressed]))
    end,
    update_search_server(UpdateSearchServer, ScannerName, Refs),
%%     ?D(FixedModel),
    ?X(888),
    FixedModel.

update_search_server(true, ScannerName, Refs) ->
    erlide_search_server:add_module_refs(ScannerName, Refs);
update_search_server(_, _, _) ->
    ok.

fixup_model(#model{forms=Forms, comments=Comments}) ->
    FixedComments = fixup_tokens(Comments),
    FixedForms = fixup_forms(Forms),
    #model{forms=FixedForms, comments=FixedComments}.

fixup_forms(Forms) ->
    [fixup_form(Form) || Form <- Forms].

fixup_tokens(Tokens) ->
    [fixup_token(Token) || Token <- Tokens].

fixup_token(#token{value=Value} = Token) when is_list(Value) ->
    Token#token{value=to_binary(Value)};
fixup_token(#token{text=Text} = Token) when is_list(Text) ->
    Token#token{value=to_binary(Text)};
fixup_token(Token) ->
    Token.

binary_args(Args) when is_list(Args) ->
    [iolist_to_binary(A) || A <- Args];
binary_args(_) ->
    [].

fixup_form(#function{comment=Comment, clauses=Clauses, args=Args} = Function) ->
    Function#function{comment= to_binary(Comment), clauses=fixup_forms(Clauses), args=binary_args(Args)};
fixup_form(#clause{head=Head, args=Args} = Clause) ->
    Clause#clause{head=to_binary(Head), args=binary_args(Args)};
fixup_form(Other) ->
    Other.

to_binary(Comment) when is_list(Comment) ->
    try 
        iolist_to_binary(Comment) 
    catch 
        _:_ -> 
            unicode:characters_to_binary(Comment) 
    end;
to_binary(Other) ->
    Other.

%% parse_test(ScannerName, File) ->
%%     erlide_scanner_server:scan_uncached(ScannerName, File, ""),
%%     Toks = erlide_scanner_server:getTokens(ScannerName),
%%     {UncommentToks, Comments} = extract_comments(Toks),
%%     Functions = split_after_dots(UncommentToks, [], []),
%%     {Collected, Refs} = classify_and_collect(Functions, [], []),
%%     _Model = #model{forms=Collected, comments=Comments},
%%     %%erlide_noparse_server:create(ScannerName, Model, ""),
%%     ok.

classify_and_collect([], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([eof], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([C | Rest], Acc, RefsAcc, Exports, Imports) ->
    {R, Refs, MoreExports, MoreImports} = cac(check_class(C), C, Exports, Imports),
    classify_and_collect(Rest, [R | Acc], Refs++RefsAcc, MoreExports++Exports, MoreImports++Imports).

cac(function, Tokens, Exports, Imports) ->
    ClauseList = split_clauses(Tokens),
    %?D(ClauseList),
    ClausesAndRefs = fix_clauses(ClauseList),
    %?D(length(Clauses)),
    [{#clause{pos=P, name=N, args=A, head=H, name_pos=NP}, _Refs} | _] = ClausesAndRefs,
    ?D(A),
    Arity = length(A),
    Exported = get_exported({N, Arity}, Exports),
    Function = case ClausesAndRefs of  	% only show subclauses when more than one
                   [_] ->
                       #function{pos=P, name=N, arity=Arity, args=A, head=H, 
                                 clauses=[], name_pos=NP, exported=Exported};
                   _ ->
                       Clauses = [C || {C, _} <- ClausesAndRefs],
                       #function{pos=P, name=N, arity=Arity, clauses=Clauses, 
                                 name_pos=NP, exported=Exported}
               end,
    {Function, fix_refs(ClausesAndRefs, Function, Imports), [], []};
cac(attribute, Attribute, _Exports, _Imports) ->
    ?D(Attribute),
    case Attribute of
        %% -spec, -type or -opaque
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=Kind, line=_Line, offset=_Offset, value=Name} | Args]
          when (Kind=:='spec') or ((Kind=:=atom) and (Name=:='type'))
                   or ((Kind=:=atom) and (Name=:='opaque'))->
            get_type_attribute(Kind, Name, Offset, Line, Attribute, Args);
        %% other attributes
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=atom, value=Name, line=_Line, offset=_Offset},
         _, #token{value=Args} | _] = Attribute ->
            get_other_attribute(Name, Offset, Line, Attribute, Args);
        [_, #token{kind=atom, value=Name, line=Line, offset=Offset} | _] ->
            #token{line=LastLine, offset=LastOffset, 
                   length=LastLength} = last_not_eof(Attribute),
            PosLength = LastOffset - Offset + LastLength,
            {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                       name=Name, args=[]}, [], [], []}
    end;
cac(other, [#token{value=Name, line=Line, offset=Offset, length=Length} | _], 
    _Exports, _Imports) ->
    {#other{pos={{Line, Line, Offset}, Length}, name=Name}, [], [], []};
cac(_, _D, _E, _I) ->
    {eof, [], [], []}.

get_type_attribute(Kind, Name0, Offset, Line, Attribute, Args) ->
    ?D(Name0),
    Name = case Kind of 'spec' -> Kind; _ -> Name0 end,
    #token{line=LastLine, offset=LastOffset,
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    ?D(Args),
    Extra = to_string(Args),
    ?D(Extra),
    {AttrArgs, _, _} = get_attribute_args(Kind, Args, Args),
    ?D({AttrArgs, Extra}),
    ExternalRefs = get_refs(tl(AttrArgs), Extra, ?ARI_TYPESPEC),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     [#ref{data=#type_def{type=Name}, offset=Offset, length=PosLength, function=Name, 
           arity=?ARI_TYPESPEC, clause="", sub_clause=false} | ExternalRefs], [], []}.

get_other_attribute(Name, Offset, Line, Attribute, Args) ->
    #token{line=LastLine, offset=LastOffset, 
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    Between = erlide_np_util:get_between_outer_pars(Attribute, '(', ')'),
    Extra = to_string(Between),
    {AttrArgs, Exports, Imports} = get_attribute_args(Name, Between, Args),
    ?D({AttrArgs, Between}),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     make_attribute_ref(Name, AttrArgs, Extra, Offset, PosLength)++
         make_attribute_arg_refs(Name, AttrArgs, Between),
     Exports, Imports}.

get_exported(F_A, Exports) ->
    lists:member(F_A, Exports).

get_attribute_args(import, Between, _Args) ->
    From = get_first_of_kind(atom, Between),
    Tokens = get_between(Between, '[', ']'),
    Import = {From, fun_arity_from_tokens(Tokens)},
    {Import, [], [Import]};
get_attribute_args(export, Between, _Args) ->
    Tokens = get_between(Between, '[', ']'),
    Exports = fun_arity_from_tokens(Tokens),
    {Exports, Exports, []};
get_attribute_args(record, Between, _Args) ->
    [RecordToken | _] = Between,
    RecordName = RecordToken#token.value,
    Tokens = erlide_np_util:get_between_outer_pars(Between, '{', '}'),
    R = {RecordName, field_list_from_tokens(Tokens)},
    {R, [], []};
get_attribute_args(_, _Between, Args) ->
    {Args, [], []}.

check_class([#token{kind = AtomOrMacro}, #token{kind = '('} | _])
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    function;
check_class([#token{kind = '-'}, #token{kind = atom} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'spec'} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'type'} | _]) ->
    attribute;
check_class(_X) ->
    %?D(lists:sublist(_X, 5)),
    other.

get_first_of_kind(Kind, Tokens) ->
    case erlide_np_util:skip_to(Tokens, Kind) of
        [#token{kind=Kind, value=Value} | _] ->
            Value;
        _ ->
            undefined
    end.
        
fun_arity_from_tokens([#token{kind=atom, value=Fun}, #token{kind='/'}, 
                       #token{kind=integer, value=Arity} | Rest]) ->
    [{Fun, Arity} | fun_arity_from_tokens(Rest)];
fun_arity_from_tokens([_ | Rest]) ->
    fun_arity_from_tokens(Rest);
fun_arity_from_tokens(_) ->
    [].

field_list_from_tokens([#token{kind=atom, value=Field, line=Line, 
                               offset=Offset, length=Length} | Rest]) ->
    {Extra, NewRest} = get_upto2(Rest, ','),
    [{Field, {{Line, Line, Offset}, Length}, to_string(Extra)}
         | field_list_from_tokens(NewRest)];
field_list_from_tokens([_ | Rest]) ->
    field_list_from_tokens(Rest);
field_list_from_tokens(_) ->
    [].

to_string(Tokens) ->
    S = erlide_scanner:tokens_to_string(Tokens),
    erlide_text:strip(S).
%%     unspacify(S).

get_head(T) ->
    A = get_args(T),
    case get_guards(T) of
        "" ->
            A;
        G ->
            A ++ " when " ++ G
    end.

get_args(T) ->
    case erlide_np_util:get_between_outer_pars(T, '(', ')') of
        [] ->
            "";
        P ->
            "("++to_string(P)++")"
    end.

get_guards([]) ->
    "";
get_guards(T) ->
    to_string(get_between(T, 'when', '->')). 

last_not_eof(L) -> 
    case lists:reverse(L) of
        [eof, Last | _] ->
            Last;
        [Last | _] ->
            Last;
        _ ->
            error
    end.    


get_between(L, A, B) ->
    R = get_upto(L, B),
    lists:reverse(get_upto(lists:reverse(R), A)).

get_upto(L, Delim) ->
        get_upto(L, Delim, []).

get_upto([], _Delim, _Acc) ->
    [];
get_upto([#token{kind=Delim} | _], Delim, Acc) ->
        lists:reverse(Acc);
get_upto([T | Rest], Delim, Acc) ->
    get_upto(Rest, Delim, [T | Acc]).

get_upto2(L, Delim) ->
    get_upto2(L, Delim, []).

get_upto2([], _Delim, Acc) ->
    {lists:reverse(Acc), []};
get_upto2([#token{kind=Delim} | Rest], Delim, Acc) ->
    {lists:reverse(Acc), Rest};
get_upto2([T | Rest], Delim, Acc) ->
    get_upto2(Rest, Delim, [T | Acc]).

check_clause([#token{kind = ';'} | Rest], false) ->
    check_class(Rest) == function;
check_clause(_, _) ->
    false.

split_clauses(F) ->
    split_clauses(F, false, [], []).

split_clauses([], _HaveWhen, Acc, []) ->
    erlide_util:reverse2(Acc);
split_clauses([], HaveWhen, Acc, ClAcc) ->
    split_clauses([], HaveWhen, [ClAcc | Acc], []);
split_clauses([#token{kind=Kind}=T | Rest], _HaveWhen, Acc, ClAcc) 
  when Kind=:='end'; Kind=:=dot; Kind=:='->'; Kind=:='when' ->
    split_clauses(Rest, Kind=:='when', Acc, [T | ClAcc]);
split_clauses([T | Rest] = Tokens, HaveWhen, Acc, ClAcc) ->
    case check_clause(Tokens, HaveWhen) of
        false ->
            split_clauses(Rest, HaveWhen, Acc, [T | ClAcc]);
        true ->
            split_clauses(Rest, HaveWhen, [[T | ClAcc] | Acc], [])
    end.

%% fix_clause([#token{kind=atom, value=Name, line=Line, offset=Offset, length=Length} | Rest] = Code) ->
fix_clauses(Clauses) ->
    fix_clauses(Clauses, []).

fix_clauses([], Acc) ->
    lists:reverse(Acc);
fix_clauses([C | Rest], Acc) ->
    {Clause, Refs} = fix_clause(C),
    fix_clauses(Rest, [{Clause, Refs} | Acc]).

fix_clause([#token{kind=AtomOrMacro, value=Name, line=Line, offset=Offset, length=Length} | Rest]) 
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    #token{line=LastLine, offset=LastOffset, length=LastLength} = last_not_eof(Rest),
    PosLength = LastOffset - Offset + LastLength+1,
    ExternalRefs = get_refs_in_code(Rest),
    {#clause{pos={{Line, LastLine, Offset}, PosLength}, name_pos={{Line, Offset}, Length},
             name=Name, args=get_function_args(Rest), head=get_head(Rest)},
     ExternalRefs}.

get_function_args(Tokens) ->
    P = erlide_np_util:get_between_outer_pars(Tokens, '(', ')'),
    L = erlide_text:split_comma_list(P),
    [to_string(A) || A <- L].

fix_refs(ClausesAndRefs, Function, Imports) ->
    fix_refs(ClausesAndRefs, Function, Imports, []).

fix_refs([], #function{name=Name, arity=Arity, name_pos={{_, Offset}, Length}}, _Imports, Acc) ->
    Ref = #ref{data=#function_def{function=Name, arity=Arity}, offset=Offset, 
               length=Length, function=Name, arity=Arity, clause="", sub_clause=false},
    [Ref | Acc];
fix_refs([{Clause, Refs} | Rest], Function, Imports, Acc0) ->
    Acc1 = fix_refs(Refs, Clause, Function, Imports, Acc0),
    fix_refs(Rest, Function, Imports, Acc1).

fix_refs([], _Clause, _Function, _Imports, Acc) ->
    Acc;
fix_refs([{Offset, Length, RefData} | Rest], #clause{head=Head}=Clause, 
         #function{name=Name, arity=Arity, clauses=Clauses}=Function, 
         Imports, Acc) ->
    SubClause = case Clauses of
                    [] -> false;
                    [_] -> false;
                    _ -> true
                end,
    NewRefData = fix_imported_ref(RefData, Imports),
    Ref = #ref{data=NewRefData, offset=Offset, length=Length, function=Name, 
               arity=Arity, clause=Head, sub_clause=SubClause},
    fix_refs(Rest, Clause, Function, Imports, [Ref | Acc]).

fix_imported_ref(#local_call{function=F, arity=A}=RefData, [{Module, Imports} | Rest]) ->
    case lists:member({F, A}, Imports) of
        true -> #external_call{module=Module, function=F, arity=A};
        false -> fix_imported_ref(RefData, Rest)
    end;
fix_imported_ref(RefData, _Imports) ->
    RefData.

%% find references in code
%% VERY simple, M:F(_ ...), F(_ ...), #R, ?M, and that's it
%% returns {Offset, Length, Ref}, which is made into proper refs
%% by fix_refs
get_refs_in_code(L) ->
    get_refs_in_code(L, []).

get_refs_in_code([], Acc) ->
    lists:reverse(Acc);
get_refs_in_code([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
                  #token{kind=atom, value=F, offset=Offset2, length=Length2},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length2+Offset2-Offset, 
         #external_call{module = M, function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=atom, value=F, offset=Offset, length=Length},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length, #local_call{function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest], 
                 Acc) ->
    R = {Offset, Length, #macro_ref{macro=M}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind='#', offset=Offset}, 
                  #token{kind=atom, value=Record, offset=Offset2, length=Length2} | Rest],
                 Acc) ->
    R = {Offset, Length2+Offset2-Offset, #record_ref{record=Record}},
    {NewRest, Fields, RightSides} = erlide_np_records:check_fields(Rest, Record),
    NewAcc = get_refs_in_code(RightSides, Acc),
    get_refs_in_code(NewRest, Fields ++ [R | NewAcc]);
get_refs_in_code([#token{kind=var, value=V, offset=Offset, length=Length} | Rest],
                 Acc) ->
    R = {Offset, Length, make_var_def_ref(Acc, V)},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([_ | Rest], Acc) ->
    get_refs_in_code(Rest, Acc).

make_var_def_ref([], Var) ->
	#var_def{variable=Var};
make_var_def_ref([{_, _, #var_def{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([{_, _, #var_ref{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([_ | Rest], Var) ->
	make_var_def_ref(Rest, Var).

%% find type references in spec, type and opaque
%% also VERY simple, type(), ?M, #R, only
%% returns proper refs
get_refs(Tokens, Name, Arity) ->
	get_refs(Tokens, Name, Arity, []).

get_refs([], _, _, Acc) ->
    lists:reverse(Acc);
get_refs([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #macro_ref{macro=M}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind='#', offset=Offset},
          #token{kind=atom, value=R, offset=Offset2, length=Length2} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset, #record_ref{record=R}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
		  #token{kind=atom, value=T, offset=Offset2, length=Length2},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset,
                   #type_ref{module=M, type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=T, offset=Offset, length=Length},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #type_ref{module='_', type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=variable, value=V, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #var_ref{variable=V}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([_ | Rest], Name, Arity, Acc) ->
    get_refs(Rest, Name, Arity, Acc).

get_record_field_defs([], _) ->
    [];
get_record_field_defs([{FieldName, {{_, _, Offset}, Length}, _Extra} | Rest],
                      RecordName) ->
    [make_ref(Offset, Length, #record_field_def{record=RecordName, field=FieldName}, 
              FieldName, ?ARI_RECORD_FIELD_DEF)
         | get_record_field_defs(Rest, RecordName)].

make_ref(Offset, Length, Data, Name, Arity) ->
    #ref{data=Data, offset=Offset, length=Length, function=Name, arity=Arity, 
         clause="", sub_clause=false}.

make_attribute_arg_refs(define, Name, [#token{}, #token{kind='('} | Rest]) ->
    get_refs(skip_to_rparen(Rest), Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(define, Name, [#token{}, #token{kind=','} | Rest]) ->
    get_refs(Rest, Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(record, {Name, Fields}, [#token{}, #token{kind=','} | Rest]) ->
    get_record_field_defs(Fields, Name) ++ get_refs(Rest, Name, ?ARI_RECORD_DEF);
make_attribute_arg_refs(_, _, _) ->
    [].

skip_to_rparen([]) ->
    [];
skip_to_rparen([#token{kind=')'} | Rest]) ->
    Rest;
skip_to_rparen([_ | Rest]) ->
    skip_to_rparen(Rest).

make_attribute_ref(Name, Between, Extra, Offset, Length) ->
    ?D({Name, Between, Offset, Length}),
    case make_attribute_ref(Name, Between, Extra) of
        [] ->
            [];
        {Arity, AName, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=AName, 
                  arity=Arity, clause="", sub_clause=false}];
        {Arity, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=Name, 
                  arity=Arity, clause="", sub_clause=false}]
    end.

make_attribute_ref(module, _, Extra) ->
    {?ARI_ATTRIBUTE, #module_def{module=Extra}};
make_attribute_ref(record, Between, _E) ->
    ?D({Between, _E}),
    Name = case Between of
               {N, _} -> N;
               N -> N
           end,
    R= #record_def{record=Name},
    ?D(R),
    {?ARI_RECORD_DEF, Name, R};
make_attribute_ref(define, [Name | _], _) when is_list(Name) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(define, Name, _) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(include, _, Extra) ->
    {?ARI_INCLUDE, #include_ref{filename=unquote(Extra)}};
make_attribute_ref(_, _, _) ->
    [].

unquote(L) ->
    lists:reverse(unquote_first(lists:reverse(unquote_first(L)))).

unquote_first([$" | Rest]) ->
    Rest;
unquote_first(L) ->
    L.

remove_rest([], _) -> [];
remove_rest(Rest, Rest) -> [];
remove_rest([Hd | Tl], Rest) -> [Hd | remove_rest(Tl, Rest)].
%% Description: Split source files into functions and clauses and collects references
%% Author: jakob
%% Created: Mar 23, 2006

-module(erlide_noparse).

%%
%% Exported Functions
%%

%% called from Java
-export([initial_parse/5, reparse/1, remove_cache_files/2]).

%% called from Erlang
-export([read_module_refs/3]).

%%
%% Include files
%%

%% -define(DEBUG, 1).
%% -define(IO_FORMAT_DEBUG, 1).

-define(CACHE_VERSION, 25).
-define(SERVER, erlide_noparse).

-include("erlide.hrl").
-include("erlide_noparse.hrl").
-include("erlide_scanner.hrl").
-include("erlide_search_server.hrl").

%% special arity flags
-define(ARI_TYPESPEC, -2).
-define(ARI_ATTRIBUTE, -3).
-define(ARI_RECORD_DEF, -4).
-define(ARI_MACRO_DEF, -5).
-define(ARI_INCLUDE, -6).
-define(ARI_RECORD_FIELD_DEF, -7).

-define(X(X), erlide_log:log({X, erlang:now()})).


%%
%% API Functions
%%

initial_parse(ScannerName, ModuleFileName, StateDir, UseCache,
              UpdateSearchServer) ->
    try
        %%         ?D({StateDir, ModuleFileName}),
        BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
        RefsFileName = BaseName ++ ".refs",
        RenewFun = fun(_F) ->
                           do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer)
                   end,
        CacheFileName = BaseName ++ ".noparse",
        ?D(CacheFileName),
        {Cached, Res} = erlide_util:check_and_renew_cached(
                          ModuleFileName, CacheFileName, ?CACHE_VERSION, 
                          RenewFun, UseCache),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        ?D(updated),
        {ok, Res, Cached}
    catch
        error:Reason ->
            {error, Reason}
    end.

reparse(ScannerName) ->
    try
        Res = do_parse(ScannerName, "", "", true),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        {ok, Res, unused}
    catch
        error:Reason ->
            {error, Reason}
    end.

read_module_refs(ScannerName, ModulePath, StateDir) ->
    ?D(ScannerName),
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    RefsFileName = BaseName ++ ".refs",
    ?D(RefsFileName),
    case file:read_file(RefsFileName) of
        {ok, Binary} ->
            ?D(byte_size(Binary)),
            binary_to_term(Binary);
        _ ->
            ?D(ModulePath),
            {ok, InitialTextBin} = file:read_file(ModulePath),
            ?D(byte_size(InitialTextBin)),
            InitialText = binary_to_list(InitialTextBin),
            _D = erlide_scanner_server:initialScan(
                   ScannerName, ModulePath, InitialText, StateDir, true),
            ?D(_D),
            initial_parse(ScannerName, ModulePath, StateDir, true, false),
            ?D(RefsFileName),
            {ok, Binary} = file:read_file(RefsFileName),
            ?D(byte_size(Binary)),
            binary_to_term(Binary)
    end.

remove_cache_files(ScannerName, StateDir) ->
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    ScannerCacheFileName = BaseName ++ ".scan",
    file:delete(ScannerCacheFileName),
    RefsFileName = BaseName ++ ".refs",
    file:delete(RefsFileName),
    CacheFileName = BaseName ++ ".noparse",
    file:delete(CacheFileName).

%%
%% Internal functions
%%

do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer) ->
    Toks = erlide_scanner_server:getTokens(ScannerName),
    do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer).

do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer) ->
    ?D({do_parse, ScannerName, length(Toks)}),
    ?X(111),
    {UncommentToks, Comments} = erlide_np_util:extract_comments(Toks),
    ?X(222),
    ?D({length(UncommentToks), length(Comments)}),
    ?D({UncommentToks}),
    Functions = erlide_np_util:split_after_dots(UncommentToks),
    ?X(333),
    ?D(length(Functions)),
    AutoImports = erlide_util:add_auto_imported([]),
    ?X(444),
%%     ?D(AutoImports),
    {Collected, Refs} = classify_and_collect(Functions, [], [], [], AutoImports),
    ?X(555),
    ?D({'>>',length(Collected)}),
    CommentedCollected = erlide_np_util:get_function_comments(Collected, Comments),
    ?X(666),
    ?D(CommentedCollected),
    Model = #model{forms=CommentedCollected, comments=Comments},
    %%erlide_noparse_server:create(ScannerName, Model),
%%     ?D({"Model", length(Model#model.forms), erts_debug:flat_size(Model)}),
    FixedModel = fixup_model(Model),
    ?X(777),
%%     ?D(erts_debug:flat_size(FixedModel)),
    %% 	?D([erts_debug:flat_size(F) || F <- element(2, FixedModel)]),
    ?D(StateDir),
    case StateDir of
        "" -> ok;
        _ ->
            ?D({RefsFileName, length(Refs)}),
            file:write_file(RefsFileName, term_to_binary(Refs, [compressed]))
    end,
    update_search_server(UpdateSearchServer, ScannerName, Refs),
%%     ?D(FixedModel),
    ?X(888),
    FixedModel.

update_search_server(true, ScannerName, Refs) ->
    erlide_search_server:add_module_refs(ScannerName, Refs);
update_search_server(_, _, _) ->
    ok.

fixup_model(#model{forms=Forms, comments=Comments}) ->
    FixedComments = fixup_tokens(Comments),
    FixedForms = fixup_forms(Forms),
    #model{forms=FixedForms, comments=FixedComments}.

fixup_forms(Forms) ->
    [fixup_form(Form) || Form <- Forms].

fixup_tokens(Tokens) ->
    [fixup_token(Token) || Token <- Tokens].

fixup_token(#token{value=Value} = Token) when is_list(Value) ->
    Token#token{value=to_binary(Value)};
fixup_token(#token{text=Text} = Token) when is_list(Text) ->
    Token#token{value=to_binary(Text)};
fixup_token(Token) ->
    Token.

binary_args(Args) when is_list(Args) ->
    [iolist_to_binary(A) || A <- Args];
binary_args(_) ->
    [].

fixup_form(#function{comment=Comment, clauses=Clauses, args=Args} = Function) ->
    Function#function{comment= to_binary(Comment), clauses=fixup_forms(Clauses), args=binary_args(Args)};
fixup_form(#clause{head=Head, args=Args} = Clause) ->
    Clause#clause{head=to_binary(Head), args=binary_args(Args)};
fixup_form(Other) ->
    Other.

to_binary(Comment) when is_list(Comment) ->
    try 
        iolist_to_binary(Comment) 
    catch 
        _:_ -> 
            unicode:characters_to_binary(Comment) 
    end;
to_binary(Other) ->
    Other.

%% parse_test(ScannerName, File) ->
%%     erlide_scanner_server:scan_uncached(ScannerName, File, ""),
%%     Toks = erlide_scanner_server:getTokens(ScannerName),
%%     {UncommentToks, Comments} = extract_comments(Toks),
%%     Functions = split_after_dots(UncommentToks, [], []),
%%     {Collected, Refs} = classify_and_collect(Functions, [], []),
%%     _Model = #model{forms=Collected, comments=Comments},
%%     %%erlide_noparse_server:create(ScannerName, Model, ""),
%%     ok.

classify_and_collect([], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([eof], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([C | Rest], Acc, RefsAcc, Exports, Imports) ->
    {R, Refs, MoreExports, MoreImports} = cac(check_class(C), C, Exports, Imports),
    classify_and_collect(Rest, [R | Acc], Refs++RefsAcc, MoreExports++Exports, MoreImports++Imports).

cac(function, Tokens, Exports, Imports) ->
    ClauseList = split_clauses(Tokens),
    %?D(ClauseList),
    ClausesAndRefs = fix_clauses(ClauseList),
    %?D(length(Clauses)),
    [{#clause{pos=P, name=N, args=A, head=H, name_pos=NP}, _Refs} | _] = ClausesAndRefs,
    ?D(A),
    Arity = length(A),
    Exported = get_exported({N, Arity}, Exports),
    Function = case ClausesAndRefs of  	% only show subclauses when more than one
                   [_] ->
                       #function{pos=P, name=N, arity=Arity, args=A, head=H, 
                                 clauses=[], name_pos=NP, exported=Exported};
                   _ ->
                       Clauses = [C || {C, _} <- ClausesAndRefs],
                       #function{pos=P, name=N, arity=Arity, clauses=Clauses, 
                                 name_pos=NP, exported=Exported}
               end,
    {Function, fix_refs(ClausesAndRefs, Function, Imports), [], []};
cac(attribute, Attribute, _Exports, _Imports) ->
    ?D(Attribute),
    case Attribute of
        %% -spec, -type or -opaque
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=Kind, line=_Line, offset=_Offset, value=Name} | Args]
          when (Kind=:='spec') or ((Kind=:=atom) and (Name=:='type'))
                   or ((Kind=:=atom) and (Name=:='opaque'))->
            get_type_attribute(Kind, Name, Offset, Line, Attribute, Args);
        %% other attributes
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=atom, value=Name, line=_Line, offset=_Offset},
         _, #token{value=Args} | _] = Attribute ->
            get_other_attribute(Name, Offset, Line, Attribute, Args);
        [_, #token{kind=atom, value=Name, line=Line, offset=Offset} | _] ->
            #token{line=LastLine, offset=LastOffset, 
                   length=LastLength} = last_not_eof(Attribute),
            PosLength = LastOffset - Offset + LastLength,
            {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                       name=Name, args=[]}, [], [], []}
    end;
cac(other, [#token{value=Name, line=Line, offset=Offset, length=Length} | _], 
    _Exports, _Imports) ->
    {#other{pos={{Line, Line, Offset}, Length}, name=Name}, [], [], []};
cac(_, _D, _E, _I) ->
    {eof, [], [], []}.

get_type_attribute(Kind, Name0, Offset, Line, Attribute, Args) ->
    ?D(Name0),
    Name = case Kind of 'spec' -> Kind; _ -> Name0 end,
    #token{line=LastLine, offset=LastOffset,
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    ?D(Args),
    Extra = to_string(Args),
    ?D(Extra),
    {AttrArgs, _, _} = get_attribute_args(Kind, Args, Args),
    ?D({AttrArgs, Extra}),
    ExternalRefs = get_refs(tl(AttrArgs), Extra, ?ARI_TYPESPEC),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     [#ref{data=#type_def{type=Name}, offset=Offset, length=PosLength, function=Name, 
           arity=?ARI_TYPESPEC, clause="", sub_clause=false} | ExternalRefs], [], []}.

get_other_attribute(Name, Offset, Line, Attribute, Args) ->
    #token{line=LastLine, offset=LastOffset, 
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    Between = erlide_np_util:get_between_outer_pars(Attribute, '(', ')'),
    Extra = to_string(Between),
    {AttrArgs, Exports, Imports} = get_attribute_args(Name, Between, Args),
    ?D({AttrArgs, Between}),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     make_attribute_ref(Name, AttrArgs, Extra, Offset, PosLength)++
         make_attribute_arg_refs(Name, AttrArgs, Between),
     Exports, Imports}.

get_exported(F_A, Exports) ->
    lists:member(F_A, Exports).

get_attribute_args(import, Between, _Args) ->
    From = get_first_of_kind(atom, Between),
    Tokens = get_between(Between, '[', ']'),
    Import = {From, fun_arity_from_tokens(Tokens)},
    {Import, [], [Import]};
get_attribute_args(export, Between, _Args) ->
    Tokens = get_between(Between, '[', ']'),
    Exports = fun_arity_from_tokens(Tokens),
    {Exports, Exports, []};
get_attribute_args(record, Between, _Args) ->
    [RecordToken | _] = Between,
    RecordName = RecordToken#token.value,
    Tokens = erlide_np_util:get_between_outer_pars(Between, '{', '}'),
    R = {RecordName, field_list_from_tokens(Tokens)},
    {R, [], []};
get_attribute_args(_, _Between, Args) ->
    {Args, [], []}.

check_class([#token{kind = AtomOrMacro}, #token{kind = '('} | _])
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    function;
check_class([#token{kind = '-'}, #token{kind = atom} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'spec'} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'type'} | _]) ->
    attribute;
check_class(_X) ->
    %?D(lists:sublist(_X, 5)),
    other.

get_first_of_kind(Kind, Tokens) ->
    case erlide_np_util:skip_to(Tokens, Kind) of
        [#token{kind=Kind, value=Value} | _] ->
            Value;
        _ ->
            undefined
    end.
        
fun_arity_from_tokens([#token{kind=atom, value=Fun}, #token{kind='/'}, 
                       #token{kind=integer, value=Arity} | Rest]) ->
    [{Fun, Arity} | fun_arity_from_tokens(Rest)];
fun_arity_from_tokens([_ | Rest]) ->
    fun_arity_from_tokens(Rest);
fun_arity_from_tokens(_) ->
    [].

field_list_from_tokens([#token{kind=atom, value=Field, line=Line, 
                               offset=Offset, length=Length} | Rest]) ->
    {Extra, NewRest} = get_upto2(Rest, ','),
    [{Field, {{Line, Line, Offset}, Length}, to_string(Extra)}
         | field_list_from_tokens(NewRest)];
field_list_from_tokens([_ | Rest]) ->
    field_list_from_tokens(Rest);
field_list_from_tokens(_) ->
    [].

to_string(Tokens) ->
    S = erlide_scanner:tokens_to_string(Tokens),
    erlide_text:strip(S).
%%     unspacify(S).

get_head(T) ->
    A = get_args(T),
    case get_guards(T) of
        "" ->
            A;
        G ->
            A ++ " when " ++ G
    end.

get_args(T) ->
    case erlide_np_util:get_between_outer_pars(T, '(', ')') of
        [] ->
            "";
        P ->
            "("++to_string(P)++")"
    end.

get_guards([]) ->
    "";
get_guards(T) ->
    to_string(get_between(T, 'when', '->')). 

last_not_eof(L) -> 
    case lists:reverse(L) of
        [eof, Last | _] ->
            Last;
        [Last | _] ->
            Last;
        _ ->
            error
    end.    


get_between(L, A, B) ->
    R = get_upto(L, B),
    lists:reverse(get_upto(lists:reverse(R), A)).

get_upto(L, Delim) ->
        get_upto(L, Delim, []).

get_upto([], _Delim, _Acc) ->
    [];
get_upto([#token{kind=Delim} | _], Delim, Acc) ->
        lists:reverse(Acc);
get_upto([T | Rest], Delim, Acc) ->
    get_upto(Rest, Delim, [T | Acc]).

get_upto2(L, Delim) ->
    get_upto2(L, Delim, []).

get_upto2([], _Delim, Acc) ->
    {lists:reverse(Acc), []};
get_upto2([#token{kind=Delim} | Rest], Delim, Acc) ->
    {lists:reverse(Acc), Rest};
get_upto2([T | Rest], Delim, Acc) ->
    get_upto2(Rest, Delim, [T | Acc]).

check_clause([#token{kind = ';'} | Rest], false) ->
    check_class(Rest) == function;
check_clause(_, _) ->
    false.

split_clauses(F) ->
    split_clauses(F, false, [], []).

split_clauses([], _HaveWhen, Acc, []) ->
    erlide_util:reverse2(Acc);
split_clauses([], HaveWhen, Acc, ClAcc) ->
    split_clauses([], HaveWhen, [ClAcc | Acc], []);
split_clauses([#token{kind=Kind}=T | Rest], _HaveWhen, Acc, ClAcc) 
  when Kind=:='end'; Kind=:=dot; Kind=:='->'; Kind=:='when' ->
    split_clauses(Rest, Kind=:='when', Acc, [T | ClAcc]);
split_clauses([T | Rest] = Tokens, HaveWhen, Acc, ClAcc) ->
    case check_clause(Tokens, HaveWhen) of
        false ->
            split_clauses(Rest, HaveWhen, Acc, [T | ClAcc]);
        true ->
            split_clauses(Rest, HaveWhen, [[T | ClAcc] | Acc], [])
    end.

%% fix_clause([#token{kind=atom, value=Name, line=Line, offset=Offset, length=Length} | Rest] = Code) ->
fix_clauses(Clauses) ->
    fix_clauses(Clauses, []).

fix_clauses([], Acc) ->
    lists:reverse(Acc);
fix_clauses([C | Rest], Acc) ->
    {Clause, Refs} = fix_clause(C),
    fix_clauses(Rest, [{Clause, Refs} | Acc]).

fix_clause([#token{kind=AtomOrMacro, value=Name, line=Line, offset=Offset, length=Length} | Rest]) 
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    #token{line=LastLine, offset=LastOffset, length=LastLength} = last_not_eof(Rest),
    PosLength = LastOffset - Offset + LastLength+1,
    ExternalRefs = get_refs_in_code(Rest),
    {#clause{pos={{Line, LastLine, Offset}, PosLength}, name_pos={{Line, Offset}, Length},
             name=Name, args=get_function_args(Rest), head=get_head(Rest)},
     ExternalRefs}.

get_function_args(Tokens) ->
    P = erlide_np_util:get_between_outer_pars(Tokens, '(', ')'),
    L = erlide_text:split_comma_list(P),
    [to_string(A) || A <- L].

fix_refs(ClausesAndRefs, Function, Imports) ->
    fix_refs(ClausesAndRefs, Function, Imports, []).

fix_refs([], #function{name=Name, arity=Arity, name_pos={{_, Offset}, Length}}, _Imports, Acc) ->
    Ref = #ref{data=#function_def{function=Name, arity=Arity}, offset=Offset, 
               length=Length, function=Name, arity=Arity, clause="", sub_clause=false},
    [Ref | Acc];
fix_refs([{Clause, Refs} | Rest], Function, Imports, Acc0) ->
    Acc1 = fix_refs(Refs, Clause, Function, Imports, Acc0),
    fix_refs(Rest, Function, Imports, Acc1).

fix_refs([], _Clause, _Function, _Imports, Acc) ->
    Acc;
fix_refs([{Offset, Length, RefData} | Rest], #clause{head=Head}=Clause, 
         #function{name=Name, arity=Arity, clauses=Clauses}=Function, 
         Imports, Acc) ->
    SubClause = case Clauses of
                    [] -> false;
                    [_] -> false;
                    _ -> true
                end,
    NewRefData = fix_imported_ref(RefData, Imports),
    Ref = #ref{data=NewRefData, offset=Offset, length=Length, function=Name, 
               arity=Arity, clause=Head, sub_clause=SubClause},
    fix_refs(Rest, Clause, Function, Imports, [Ref | Acc]).

fix_imported_ref(#local_call{function=F, arity=A}=RefData, [{Module, Imports} | Rest]) ->
    case lists:member({F, A}, Imports) of
        true -> #external_call{module=Module, function=F, arity=A};
        false -> fix_imported_ref(RefData, Rest)
    end;
fix_imported_ref(RefData, _Imports) ->
    RefData.

%% find references in code
%% VERY simple, M:F(_ ...), F(_ ...), #R, ?M, and that's it
%% returns {Offset, Length, Ref}, which is made into proper refs
%% by fix_refs
get_refs_in_code(L) ->
    get_refs_in_code(L, []).

get_refs_in_code([], Acc) ->
    lists:reverse(Acc);
get_refs_in_code([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
                  #token{kind=atom, value=F, offset=Offset2, length=Length2},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length2+Offset2-Offset, 
         #external_call{module = M, function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=atom, value=F, offset=Offset, length=Length},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length, #local_call{function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest], 
                 Acc) ->
    R = {Offset, Length, #macro_ref{macro=M}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind='#', offset=Offset}, 
                  #token{kind=atom, value=Record, offset=Offset2, length=Length2} | Rest],
                 Acc) ->
    R = {Offset, Length2+Offset2-Offset, #record_ref{record=Record}},
    {NewRest, Fields, RightSides} = erlide_np_records:check_fields(Rest, Record),
    NewAcc = get_refs_in_code(RightSides, Acc),
    get_refs_in_code(NewRest, Fields ++ [R | NewAcc]);
get_refs_in_code([#token{kind=var, value=V, offset=Offset, length=Length} | Rest],
                 Acc) ->
    R = {Offset, Length, make_var_def_ref(Acc, V)},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([_ | Rest], Acc) ->
    get_refs_in_code(Rest, Acc).

make_var_def_ref([], Var) ->
	#var_def{variable=Var};
make_var_def_ref([{_, _, #var_def{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([{_, _, #var_ref{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([_ | Rest], Var) ->
	make_var_def_ref(Rest, Var).

%% find type references in spec, type and opaque
%% also VERY simple, type(), ?M, #R, only
%% returns proper refs
get_refs(Tokens, Name, Arity) ->
	get_refs(Tokens, Name, Arity, []).

get_refs([], _, _, Acc) ->
    lists:reverse(Acc);
get_refs([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #macro_ref{macro=M}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind='#', offset=Offset},
          #token{kind=atom, value=R, offset=Offset2, length=Length2} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset, #record_ref{record=R}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
		  #token{kind=atom, value=T, offset=Offset2, length=Length2},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset,
                   #type_ref{module=M, type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=T, offset=Offset, length=Length},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #type_ref{module='_', type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=variable, value=V, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #var_ref{variable=V}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([_ | Rest], Name, Arity, Acc) ->
    get_refs(Rest, Name, Arity, Acc).

get_record_field_defs([], _) ->
    [];
get_record_field_defs([{FieldName, {{_, _, Offset}, Length}, _Extra} | Rest],
                      RecordName) ->
    [make_ref(Offset, Length, #record_field_def{record=RecordName, field=FieldName}, 
              FieldName, ?ARI_RECORD_FIELD_DEF)
         | get_record_field_defs(Rest, RecordName)].

make_ref(Offset, Length, Data, Name, Arity) ->
    #ref{data=Data, offset=Offset, length=Length, function=Name, arity=Arity, 
         clause="", sub_clause=false}.

make_attribute_arg_refs(define, Name, [#token{}, #token{kind='('} | Rest]) ->
    get_refs(skip_to_rparen(Rest), Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(define, Name, [#token{}, #token{kind=','} | Rest]) ->
    get_refs(Rest, Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(record, {Name, Fields}, [#token{}, #token{kind=','} | Rest]) ->
    get_record_field_defs(Fields, Name) ++ get_refs(Rest, Name, ?ARI_RECORD_DEF);
make_attribute_arg_refs(_, _, _) ->
    [].

skip_to_rparen([]) ->
    [];
skip_to_rparen([#token{kind=')'} | Rest]) ->
    Rest;
skip_to_rparen([_ | Rest]) ->
    skip_to_rparen(Rest).

make_attribute_ref(Name, Between, Extra, Offset, Length) ->
    ?D({Name, Between, Offset, Length}),
    case make_attribute_ref(Name, Between, Extra) of
        [] ->
            [];
        {Arity, AName, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=AName, 
                  arity=Arity, clause="", sub_clause=false}];
        {Arity, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=Name, 
                  arity=Arity, clause="", sub_clause=false}]
    end.

make_attribute_ref(module, _, Extra) ->
    {?ARI_ATTRIBUTE, #module_def{module=Extra}};
make_attribute_ref(record, Between, _E) ->
    ?D({Between, _E}),
    Name = case Between of
               {N, _} -> N;
               N -> N
           end,
    R= #record_def{record=Name},
    ?D(R),
    {?ARI_RECORD_DEF, Name, R};
make_attribute_ref(define, [Name | _], _) when is_list(Name) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(define, Name, _) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(include, _, Extra) ->
    {?ARI_INCLUDE, #include_ref{filename=unquote(Extra)}};
make_attribute_ref(_, _, _) ->
    [].

unquote(L) ->
    lists:reverse(unquote_first(lists:reverse(unquote_first(L)))).

unquote_first([$" | Rest]) ->
    Rest;
unquote_first(L) ->
    L.

remove_rest([], _) -> [];
remove_rest(Rest, Rest) -> [];
remove_rest([Hd | Tl], Rest) -> [Hd | remove_rest(Tl, Rest)].
%% Description: Split source files into functions and clauses and collects references
%% Author: jakob
%% Created: Mar 23, 2006

-module(erlide_noparse).

%%
%% Exported Functions
%%

%% called from Java
-export([initial_parse/5, reparse/1, remove_cache_files/2]).

%% called from Erlang
-export([read_module_refs/3]).

%%
%% Include files
%%

%% -define(DEBUG, 1).
%% -define(IO_FORMAT_DEBUG, 1).

-define(CACHE_VERSION, 25).
-define(SERVER, erlide_noparse).

-include("erlide.hrl").
-include("erlide_noparse.hrl").
-include("erlide_scanner.hrl").
-include("erlide_search_server.hrl").

%% special arity flags
-define(ARI_TYPESPEC, -2).
-define(ARI_ATTRIBUTE, -3).
-define(ARI_RECORD_DEF, -4).
-define(ARI_MACRO_DEF, -5).
-define(ARI_INCLUDE, -6).
-define(ARI_RECORD_FIELD_DEF, -7).

-define(X(X), erlide_log:log({X, erlang:now()})).


%%
%% API Functions
%%

initial_parse(ScannerName, ModuleFileName, StateDir, UseCache,
              UpdateSearchServer) ->
    try
        %%         ?D({StateDir, ModuleFileName}),
        BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
        RefsFileName = BaseName ++ ".refs",
        RenewFun = fun(_F) ->
                           do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer)
                   end,
        CacheFileName = BaseName ++ ".noparse",
        ?D(CacheFileName),
        {Cached, Res} = erlide_util:check_and_renew_cached(
                          ModuleFileName, CacheFileName, ?CACHE_VERSION, 
                          RenewFun, UseCache),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        ?D(updated),
        {ok, Res, Cached}
    catch
        error:Reason ->
            {error, Reason}
    end.

reparse(ScannerName) ->
    try
        Res = do_parse(ScannerName, "", "", true),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        {ok, Res, unused}
    catch
        error:Reason ->
            {error, Reason}
    end.

read_module_refs(ScannerName, ModulePath, StateDir) ->
    ?D(ScannerName),
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    RefsFileName = BaseName ++ ".refs",
    ?D(RefsFileName),
    case file:read_file(RefsFileName) of
        {ok, Binary} ->
            ?D(byte_size(Binary)),
            binary_to_term(Binary);
        _ ->
            ?D(ModulePath),
            {ok, InitialTextBin} = file:read_file(ModulePath),
            ?D(byte_size(InitialTextBin)),
            InitialText = binary_to_list(InitialTextBin),
            _D = erlide_scanner_server:initialScan(
                   ScannerName, ModulePath, InitialText, StateDir, true),
            ?D(_D),
            initial_parse(ScannerName, ModulePath, StateDir, true, false),
            ?D(RefsFileName),
            {ok, Binary} = file:read_file(RefsFileName),
            ?D(byte_size(Binary)),
            binary_to_term(Binary)
    end.

remove_cache_files(ScannerName, StateDir) ->
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    ScannerCacheFileName = BaseName ++ ".scan",
    file:delete(ScannerCacheFileName),
    RefsFileName = BaseName ++ ".refs",
    file:delete(RefsFileName),
    CacheFileName = BaseName ++ ".noparse",
    file:delete(CacheFileName).

%%
%% Internal functions
%%

do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer) ->
    Toks = erlide_scanner_server:getTokens(ScannerName),
    do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer).

do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer) ->
    ?D({do_parse, ScannerName, length(Toks)}),
    ?X(111),
    {UncommentToks, Comments} = erlide_np_util:extract_comments(Toks),
    ?X(222),
    ?D({length(UncommentToks), length(Comments)}),
    ?D({UncommentToks}),
    Functions = erlide_np_util:split_after_dots(UncommentToks),
    ?X(333),
    ?D(length(Functions)),
    AutoImports = erlide_util:add_auto_imported([]),
    ?X(444),
%%     ?D(AutoImports),
    {Collected, Refs} = classify_and_collect(Functions, [], [], [], AutoImports),
    ?X(555),
    ?D({'>>',length(Collected)}),
    CommentedCollected = erlide_np_util:get_function_comments(Collected, Comments),
    ?X(666),
    ?D(CommentedCollected),
    Model = #model{forms=CommentedCollected, comments=Comments},
    %%erlide_noparse_server:create(ScannerName, Model),
%%     ?D({"Model", length(Model#model.forms), erts_debug:flat_size(Model)}),
    FixedModel = fixup_model(Model),
    ?X(777),
%%     ?D(erts_debug:flat_size(FixedModel)),
    %% 	?D([erts_debug:flat_size(F) || F <- element(2, FixedModel)]),
    ?D(StateDir),
    case StateDir of
        "" -> ok;
        _ ->
            ?D({RefsFileName, length(Refs)}),
            file:write_file(RefsFileName, term_to_binary(Refs, [compressed]))
    end,
    update_search_server(UpdateSearchServer, ScannerName, Refs),
%%     ?D(FixedModel),
    ?X(888),
    FixedModel.

update_search_server(true, ScannerName, Refs) ->
    erlide_search_server:add_module_refs(ScannerName, Refs);
update_search_server(_, _, _) ->
    ok.

fixup_model(#model{forms=Forms, comments=Comments}) ->
    FixedComments = fixup_tokens(Comments),
    FixedForms = fixup_forms(Forms),
    #model{forms=FixedForms, comments=FixedComments}.

fixup_forms(Forms) ->
    [fixup_form(Form) || Form <- Forms].

fixup_tokens(Tokens) ->
    [fixup_token(Token) || Token <- Tokens].

fixup_token(#token{value=Value} = Token) when is_list(Value) ->
    Token#token{value=to_binary(Value)};
fixup_token(#token{text=Text} = Token) when is_list(Text) ->
    Token#token{value=to_binary(Text)};
fixup_token(Token) ->
    Token.

binary_args(Args) when is_list(Args) ->
    [iolist_to_binary(A) || A <- Args];
binary_args(_) ->
    [].

fixup_form(#function{comment=Comment, clauses=Clauses, args=Args} = Function) ->
    Function#function{comment= to_binary(Comment), clauses=fixup_forms(Clauses), args=binary_args(Args)};
fixup_form(#clause{head=Head, args=Args} = Clause) ->
    Clause#clause{head=to_binary(Head), args=binary_args(Args)};
fixup_form(Other) ->
    Other.

to_binary(Comment) when is_list(Comment) ->
    try 
        iolist_to_binary(Comment) 
    catch 
        _:_ -> 
            unicode:characters_to_binary(Comment) 
    end;
to_binary(Other) ->
    Other.

%% parse_test(ScannerName, File) ->
%%     erlide_scanner_server:scan_uncached(ScannerName, File, ""),
%%     Toks = erlide_scanner_server:getTokens(ScannerName),
%%     {UncommentToks, Comments} = extract_comments(Toks),
%%     Functions = split_after_dots(UncommentToks, [], []),
%%     {Collected, Refs} = classify_and_collect(Functions, [], []),
%%     _Model = #model{forms=Collected, comments=Comments},
%%     %%erlide_noparse_server:create(ScannerName, Model, ""),
%%     ok.

classify_and_collect([], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([eof], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([C | Rest], Acc, RefsAcc, Exports, Imports) ->
    {R, Refs, MoreExports, MoreImports} = cac(check_class(C), C, Exports, Imports),
    classify_and_collect(Rest, [R | Acc], Refs++RefsAcc, MoreExports++Exports, MoreImports++Imports).

cac(function, Tokens, Exports, Imports) ->
    ClauseList = split_clauses(Tokens),
    %?D(ClauseList),
    ClausesAndRefs = fix_clauses(ClauseList),
    %?D(length(Clauses)),
    [{#clause{pos=P, name=N, args=A, head=H, name_pos=NP}, _Refs} | _] = ClausesAndRefs,
    ?D(A),
    Arity = length(A),
    Exported = get_exported({N, Arity}, Exports),
    Function = case ClausesAndRefs of  	% only show subclauses when more than one
                   [_] ->
                       #function{pos=P, name=N, arity=Arity, args=A, head=H, 
                                 clauses=[], name_pos=NP, exported=Exported};
                   _ ->
                       Clauses = [C || {C, _} <- ClausesAndRefs],
                       #function{pos=P, name=N, arity=Arity, clauses=Clauses, 
                                 name_pos=NP, exported=Exported}
               end,
    {Function, fix_refs(ClausesAndRefs, Function, Imports), [], []};
cac(attribute, Attribute, _Exports, _Imports) ->
    ?D(Attribute),
    case Attribute of
        %% -spec, -type or -opaque
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=Kind, line=_Line, offset=_Offset, value=Name} | Args]
          when (Kind=:='spec') or ((Kind=:=atom) and (Name=:='type'))
                   or ((Kind=:=atom) and (Name=:='opaque'))->
            get_type_attribute(Kind, Name, Offset, Line, Attribute, Args);
        %% other attributes
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=atom, value=Name, line=_Line, offset=_Offset},
         _, #token{value=Args} | _] = Attribute ->
            get_other_attribute(Name, Offset, Line, Attribute, Args);
        [_, #token{kind=atom, value=Name, line=Line, offset=Offset} | _] ->
            #token{line=LastLine, offset=LastOffset, 
                   length=LastLength} = last_not_eof(Attribute),
            PosLength = LastOffset - Offset + LastLength,
            {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                       name=Name, args=[]}, [], [], []}
    end;
cac(other, [#token{value=Name, line=Line, offset=Offset, length=Length} | _], 
    _Exports, _Imports) ->
    {#other{pos={{Line, Line, Offset}, Length}, name=Name}, [], [], []};
cac(_, _D, _E, _I) ->
    {eof, [], [], []}.

get_type_attribute(Kind, Name0, Offset, Line, Attribute, Args) ->
    ?D(Name0),
    Name = case Kind of 'spec' -> Kind; _ -> Name0 end,
    #token{line=LastLine, offset=LastOffset,
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    ?D(Args),
    Extra = to_string(Args),
    ?D(Extra),
    {AttrArgs, _, _} = get_attribute_args(Kind, Args, Args),
    ?D({AttrArgs, Extra}),
    ExternalRefs = get_refs(tl(AttrArgs), Extra, ?ARI_TYPESPEC),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     [#ref{data=#type_def{type=Name}, offset=Offset, length=PosLength, function=Name, 
           arity=?ARI_TYPESPEC, clause="", sub_clause=false} | ExternalRefs], [], []}.

get_other_attribute(Name, Offset, Line, Attribute, Args) ->
    #token{line=LastLine, offset=LastOffset, 
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    Between = erlide_np_util:get_between_outer_pars(Attribute, '(', ')'),
    Extra = to_string(Between),
    {AttrArgs, Exports, Imports} = get_attribute_args(Name, Between, Args),
    ?D({AttrArgs, Between}),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     make_attribute_ref(Name, AttrArgs, Extra, Offset, PosLength)++
         make_attribute_arg_refs(Name, AttrArgs, Between),
     Exports, Imports}.

get_exported(F_A, Exports) ->
    lists:member(F_A, Exports).

get_attribute_args(import, Between, _Args) ->
    From = get_first_of_kind(atom, Between),
    Tokens = get_between(Between, '[', ']'),
    Import = {From, fun_arity_from_tokens(Tokens)},
    {Import, [], [Import]};
get_attribute_args(export, Between, _Args) ->
    Tokens = get_between(Between, '[', ']'),
    Exports = fun_arity_from_tokens(Tokens),
    {Exports, Exports, []};
get_attribute_args(record, Between, _Args) ->
    [RecordToken | _] = Between,
    RecordName = RecordToken#token.value,
    Tokens = erlide_np_util:get_between_outer_pars(Between, '{', '}'),
    R = {RecordName, field_list_from_tokens(Tokens)},
    {R, [], []};
get_attribute_args(_, _Between, Args) ->
    {Args, [], []}.

check_class([#token{kind = AtomOrMacro}, #token{kind = '('} | _])
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    function;
check_class([#token{kind = '-'}, #token{kind = atom} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'spec'} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'type'} | _]) ->
    attribute;
check_class(_X) ->
    %?D(lists:sublist(_X, 5)),
    other.

get_first_of_kind(Kind, Tokens) ->
    case erlide_np_util:skip_to(Tokens, Kind) of
        [#token{kind=Kind, value=Value} | _] ->
            Value;
        _ ->
            undefined
    end.
        
fun_arity_from_tokens([#token{kind=atom, value=Fun}, #token{kind='/'}, 
                       #token{kind=integer, value=Arity} | Rest]) ->
    [{Fun, Arity} | fun_arity_from_tokens(Rest)];
fun_arity_from_tokens([_ | Rest]) ->
    fun_arity_from_tokens(Rest);
fun_arity_from_tokens(_) ->
    [].

field_list_from_tokens([#token{kind=atom, value=Field, line=Line, 
                               offset=Offset, length=Length} | Rest]) ->
    {Extra, NewRest} = get_upto2(Rest, ','),
    [{Field, {{Line, Line, Offset}, Length}, to_string(Extra)}
         | field_list_from_tokens(NewRest)];
field_list_from_tokens([_ | Rest]) ->
    field_list_from_tokens(Rest);
field_list_from_tokens(_) ->
    [].

to_string(Tokens) ->
    S = erlide_scanner:tokens_to_string(Tokens),
    erlide_text:strip(S).
%%     unspacify(S).

get_head(T) ->
    A = get_args(T),
    case get_guards(T) of
        "" ->
            A;
        G ->
            A ++ " when " ++ G
    end.

get_args(T) ->
    case erlide_np_util:get_between_outer_pars(T, '(', ')') of
        [] ->
            "";
        P ->
            "("++to_string(P)++")"
    end.

get_guards([]) ->
    "";
get_guards(T) ->
    to_string(get_between(T, 'when', '->')). 

last_not_eof(L) -> 
    case lists:reverse(L) of
        [eof, Last | _] ->
            Last;
        [Last | _] ->
            Last;
        _ ->
            error
    end.    


get_between(L, A, B) ->
    R = get_upto(L, B),
    lists:reverse(get_upto(lists:reverse(R), A)).

get_upto(L, Delim) ->
        get_upto(L, Delim, []).

get_upto([], _Delim, _Acc) ->
    [];
get_upto([#token{kind=Delim} | _], Delim, Acc) ->
        lists:reverse(Acc);
get_upto([T | Rest], Delim, Acc) ->
    get_upto(Rest, Delim, [T | Acc]).

get_upto2(L, Delim) ->
    get_upto2(L, Delim, []).

get_upto2([], _Delim, Acc) ->
    {lists:reverse(Acc), []};
get_upto2([#token{kind=Delim} | Rest], Delim, Acc) ->
    {lists:reverse(Acc), Rest};
get_upto2([T | Rest], Delim, Acc) ->
    get_upto2(Rest, Delim, [T | Acc]).

check_clause([#token{kind = ';'} | Rest], false) ->
    check_class(Rest) == function;
check_clause(_, _) ->
    false.

split_clauses(F) ->
    split_clauses(F, false, [], []).

split_clauses([], _HaveWhen, Acc, []) ->
    erlide_util:reverse2(Acc);
split_clauses([], HaveWhen, Acc, ClAcc) ->
    split_clauses([], HaveWhen, [ClAcc | Acc], []);
split_clauses([#token{kind=Kind}=T | Rest], _HaveWhen, Acc, ClAcc) 
  when Kind=:='end'; Kind=:=dot; Kind=:='->'; Kind=:='when' ->
    split_clauses(Rest, Kind=:='when', Acc, [T | ClAcc]);
split_clauses([T | Rest] = Tokens, HaveWhen, Acc, ClAcc) ->
    case check_clause(Tokens, HaveWhen) of
        false ->
            split_clauses(Rest, HaveWhen, Acc, [T | ClAcc]);
        true ->
            split_clauses(Rest, HaveWhen, [[T | ClAcc] | Acc], [])
    end.

%% fix_clause([#token{kind=atom, value=Name, line=Line, offset=Offset, length=Length} | Rest] = Code) ->
fix_clauses(Clauses) ->
    fix_clauses(Clauses, []).

fix_clauses([], Acc) ->
    lists:reverse(Acc);
fix_clauses([C | Rest], Acc) ->
    {Clause, Refs} = fix_clause(C),
    fix_clauses(Rest, [{Clause, Refs} | Acc]).

fix_clause([#token{kind=AtomOrMacro, value=Name, line=Line, offset=Offset, length=Length} | Rest]) 
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    #token{line=LastLine, offset=LastOffset, length=LastLength} = last_not_eof(Rest),
    PosLength = LastOffset - Offset + LastLength+1,
    ExternalRefs = get_refs_in_code(Rest),
    {#clause{pos={{Line, LastLine, Offset}, PosLength}, name_pos={{Line, Offset}, Length},
             name=Name, args=get_function_args(Rest), head=get_head(Rest)},
     ExternalRefs}.

get_function_args(Tokens) ->
    P = erlide_np_util:get_between_outer_pars(Tokens, '(', ')'),
    L = erlide_text:split_comma_list(P),
    [to_string(A) || A <- L].

fix_refs(ClausesAndRefs, Function, Imports) ->
    fix_refs(ClausesAndRefs, Function, Imports, []).

fix_refs([], #function{name=Name, arity=Arity, name_pos={{_, Offset}, Length}}, _Imports, Acc) ->
    Ref = #ref{data=#function_def{function=Name, arity=Arity}, offset=Offset, 
               length=Length, function=Name, arity=Arity, clause="", sub_clause=false},
    [Ref | Acc];
fix_refs([{Clause, Refs} | Rest], Function, Imports, Acc0) ->
    Acc1 = fix_refs(Refs, Clause, Function, Imports, Acc0),
    fix_refs(Rest, Function, Imports, Acc1).

fix_refs([], _Clause, _Function, _Imports, Acc) ->
    Acc;
fix_refs([{Offset, Length, RefData} | Rest], #clause{head=Head}=Clause, 
         #function{name=Name, arity=Arity, clauses=Clauses}=Function, 
         Imports, Acc) ->
    SubClause = case Clauses of
                    [] -> false;
                    [_] -> false;
                    _ -> true
                end,
    NewRefData = fix_imported_ref(RefData, Imports),
    Ref = #ref{data=NewRefData, offset=Offset, length=Length, function=Name, 
               arity=Arity, clause=Head, sub_clause=SubClause},
    fix_refs(Rest, Clause, Function, Imports, [Ref | Acc]).

fix_imported_ref(#local_call{function=F, arity=A}=RefData, [{Module, Imports} | Rest]) ->
    case lists:member({F, A}, Imports) of
        true -> #external_call{module=Module, function=F, arity=A};
        false -> fix_imported_ref(RefData, Rest)
    end;
fix_imported_ref(RefData, _Imports) ->
    RefData.

%% find references in code
%% VERY simple, M:F(_ ...), F(_ ...), #R, ?M, and that's it
%% returns {Offset, Length, Ref}, which is made into proper refs
%% by fix_refs
get_refs_in_code(L) ->
    get_refs_in_code(L, []).

get_refs_in_code([], Acc) ->
    lists:reverse(Acc);
get_refs_in_code([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
                  #token{kind=atom, value=F, offset=Offset2, length=Length2},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length2+Offset2-Offset, 
         #external_call{module = M, function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=atom, value=F, offset=Offset, length=Length},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length, #local_call{function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest], 
                 Acc) ->
    R = {Offset, Length, #macro_ref{macro=M}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind='#', offset=Offset}, 
                  #token{kind=atom, value=Record, offset=Offset2, length=Length2} | Rest],
                 Acc) ->
    R = {Offset, Length2+Offset2-Offset, #record_ref{record=Record}},
    {NewRest, Fields, RightSides} = erlide_np_records:check_fields(Rest, Record),
    NewAcc = get_refs_in_code(RightSides, Acc),
    get_refs_in_code(NewRest, Fields ++ [R | NewAcc]);
get_refs_in_code([#token{kind=var, value=V, offset=Offset, length=Length} | Rest],
                 Acc) ->
    R = {Offset, Length, make_var_def_ref(Acc, V)},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([_ | Rest], Acc) ->
    get_refs_in_code(Rest, Acc).

make_var_def_ref([], Var) ->
	#var_def{variable=Var};
make_var_def_ref([{_, _, #var_def{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([{_, _, #var_ref{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([_ | Rest], Var) ->
	make_var_def_ref(Rest, Var).

%% find type references in spec, type and opaque
%% also VERY simple, type(), ?M, #R, only
%% returns proper refs
get_refs(Tokens, Name, Arity) ->
	get_refs(Tokens, Name, Arity, []).

get_refs([], _, _, Acc) ->
    lists:reverse(Acc);
get_refs([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #macro_ref{macro=M}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind='#', offset=Offset},
          #token{kind=atom, value=R, offset=Offset2, length=Length2} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset, #record_ref{record=R}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
		  #token{kind=atom, value=T, offset=Offset2, length=Length2},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset,
                   #type_ref{module=M, type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=T, offset=Offset, length=Length},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #type_ref{module='_', type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=variable, value=V, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #var_ref{variable=V}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([_ | Rest], Name, Arity, Acc) ->
    get_refs(Rest, Name, Arity, Acc).

get_record_field_defs([], _) ->
    [];
get_record_field_defs([{FieldName, {{_, _, Offset}, Length}, _Extra} | Rest],
                      RecordName) ->
    [make_ref(Offset, Length, #record_field_def{record=RecordName, field=FieldName}, 
              FieldName, ?ARI_RECORD_FIELD_DEF)
         | get_record_field_defs(Rest, RecordName)].

make_ref(Offset, Length, Data, Name, Arity) ->
    #ref{data=Data, offset=Offset, length=Length, function=Name, arity=Arity, 
         clause="", sub_clause=false}.

make_attribute_arg_refs(define, Name, [#token{}, #token{kind='('} | Rest]) ->
    get_refs(skip_to_rparen(Rest), Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(define, Name, [#token{}, #token{kind=','} | Rest]) ->
    get_refs(Rest, Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(record, {Name, Fields}, [#token{}, #token{kind=','} | Rest]) ->
    get_record_field_defs(Fields, Name) ++ get_refs(Rest, Name, ?ARI_RECORD_DEF);
make_attribute_arg_refs(_, _, _) ->
    [].

skip_to_rparen([]) ->
    [];
skip_to_rparen([#token{kind=')'} | Rest]) ->
    Rest;
skip_to_rparen([_ | Rest]) ->
    skip_to_rparen(Rest).

make_attribute_ref(Name, Between, Extra, Offset, Length) ->
    ?D({Name, Between, Offset, Length}),
    case make_attribute_ref(Name, Between, Extra) of
        [] ->
            [];
        {Arity, AName, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=AName, 
                  arity=Arity, clause="", sub_clause=false}];
        {Arity, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=Name, 
                  arity=Arity, clause="", sub_clause=false}]
    end.

make_attribute_ref(module, _, Extra) ->
    {?ARI_ATTRIBUTE, #module_def{module=Extra}};
make_attribute_ref(record, Between, _E) ->
    ?D({Between, _E}),
    Name = case Between of
               {N, _} -> N;
               N -> N
           end,
    R= #record_def{record=Name},
    ?D(R),
    {?ARI_RECORD_DEF, Name, R};
make_attribute_ref(define, [Name | _], _) when is_list(Name) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(define, Name, _) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(include, _, Extra) ->
    {?ARI_INCLUDE, #include_ref{filename=unquote(Extra)}};
make_attribute_ref(_, _, _) ->
    [].

unquote(L) ->
    lists:reverse(unquote_first(lists:reverse(unquote_first(L)))).

unquote_first([$" | Rest]) ->
    Rest;
unquote_first(L) ->
    L.

remove_rest([], _) -> [];
remove_rest(Rest, Rest) -> [];
remove_rest([Hd | Tl], Rest) -> [Hd | remove_rest(Tl, Rest)].
%% Description: Split source files into functions and clauses and collects references
%% Author: jakob
%% Created: Mar 23, 2006

-module(erlide_noparse).

%%
%% Exported Functions
%%

%% called from Java
-export([initial_parse/5, reparse/1, remove_cache_files/2]).

%% called from Erlang
-export([read_module_refs/3]).

%%
%% Include files
%%

%% -define(DEBUG, 1).
%% -define(IO_FORMAT_DEBUG, 1).

-define(CACHE_VERSION, 25).
-define(SERVER, erlide_noparse).

-include("erlide.hrl").
-include("erlide_noparse.hrl").
-include("erlide_scanner.hrl").
-include("erlide_search_server.hrl").

%% special arity flags
-define(ARI_TYPESPEC, -2).
-define(ARI_ATTRIBUTE, -3).
-define(ARI_RECORD_DEF, -4).
-define(ARI_MACRO_DEF, -5).
-define(ARI_INCLUDE, -6).
-define(ARI_RECORD_FIELD_DEF, -7).

-define(X(X), erlide_log:log({X, erlang:now()})).


%%
%% API Functions
%%

initial_parse(ScannerName, ModuleFileName, StateDir, UseCache,
              UpdateSearchServer) ->
    try
        %%         ?D({StateDir, ModuleFileName}),
        BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
        RefsFileName = BaseName ++ ".refs",
        RenewFun = fun(_F) ->
                           do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer)
                   end,
        CacheFileName = BaseName ++ ".noparse",
        ?D(CacheFileName),
        {Cached, Res} = erlide_util:check_and_renew_cached(
                          ModuleFileName, CacheFileName, ?CACHE_VERSION, 
                          RenewFun, UseCache),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        ?D(updated),
        {ok, Res, Cached}
    catch
        error:Reason ->
            {error, Reason}
    end.

reparse(ScannerName) ->
    try
        Res = do_parse(ScannerName, "", "", true),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        {ok, Res, unused}
    catch
        error:Reason ->
            {error, Reason}
    end.

read_module_refs(ScannerName, ModulePath, StateDir) ->
    ?D(ScannerName),
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    RefsFileName = BaseName ++ ".refs",
    ?D(RefsFileName),
    case file:read_file(RefsFileName) of
        {ok, Binary} ->
            ?D(byte_size(Binary)),
            binary_to_term(Binary);
        _ ->
            ?D(ModulePath),
            {ok, InitialTextBin} = file:read_file(ModulePath),
            ?D(byte_size(InitialTextBin)),
            InitialText = binary_to_list(InitialTextBin),
            _D = erlide_scanner_server:initialScan(
                   ScannerName, ModulePath, InitialText, StateDir, true),
            ?D(_D),
            initial_parse(ScannerName, ModulePath, StateDir, true, false),
            ?D(RefsFileName),
            {ok, Binary} = file:read_file(RefsFileName),
            ?D(byte_size(Binary)),
            binary_to_term(Binary)
    end.

remove_cache_files(ScannerName, StateDir) ->
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    ScannerCacheFileName = BaseName ++ ".scan",
    file:delete(ScannerCacheFileName),
    RefsFileName = BaseName ++ ".refs",
    file:delete(RefsFileName),
    CacheFileName = BaseName ++ ".noparse",
    file:delete(CacheFileName).

%%
%% Internal functions
%%

do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer) ->
    Toks = erlide_scanner_server:getTokens(ScannerName),
    do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer).

do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer) ->
    ?D({do_parse, ScannerName, length(Toks)}),
    ?X(111),
    {UncommentToks, Comments} = erlide_np_util:extract_comments(Toks),
    ?X(222),
    ?D({length(UncommentToks), length(Comments)}),
    ?D({UncommentToks}),
    Functions = erlide_np_util:split_after_dots(UncommentToks),
    ?X(333),
    ?D(length(Functions)),
    AutoImports = erlide_util:add_auto_imported([]),
    ?X(444),
%%     ?D(AutoImports),
    {Collected, Refs} = classify_and_collect(Functions, [], [], [], AutoImports),
    ?X(555),
    ?D({'>>',length(Collected)}),
    CommentedCollected = erlide_np_util:get_function_comments(Collected, Comments),
    ?X(666),
    ?D(CommentedCollected),
    Model = #model{forms=CommentedCollected, comments=Comments},
    %%erlide_noparse_server:create(ScannerName, Model),
%%     ?D({"Model", length(Model#model.forms), erts_debug:flat_size(Model)}),
    FixedModel = fixup_model(Model),
    ?X(777),
%%     ?D(erts_debug:flat_size(FixedModel)),
    %% 	?D([erts_debug:flat_size(F) || F <- element(2, FixedModel)]),
    ?D(StateDir),
    case StateDir of
        "" -> ok;
        _ ->
            ?D({RefsFileName, length(Refs)}),
            file:write_file(RefsFileName, term_to_binary(Refs, [compressed]))
    end,
    update_search_server(UpdateSearchServer, ScannerName, Refs),
%%     ?D(FixedModel),
    ?X(888),
    FixedModel.

update_search_server(true, ScannerName, Refs) ->
    erlide_search_server:add_module_refs(ScannerName, Refs);
update_search_server(_, _, _) ->
    ok.

fixup_model(#model{forms=Forms, comments=Comments}) ->
    FixedComments = fixup_tokens(Comments),
    FixedForms = fixup_forms(Forms),
    #model{forms=FixedForms, comments=FixedComments}.

fixup_forms(Forms) ->
    [fixup_form(Form) || Form <- Forms].

fixup_tokens(Tokens) ->
    [fixup_token(Token) || Token <- Tokens].

fixup_token(#token{value=Value} = Token) when is_list(Value) ->
    Token#token{value=to_binary(Value)};
fixup_token(#token{text=Text} = Token) when is_list(Text) ->
    Token#token{value=to_binary(Text)};
fixup_token(Token) ->
    Token.

binary_args(Args) when is_list(Args) ->
    [iolist_to_binary(A) || A <- Args];
binary_args(_) ->
    [].

fixup_form(#function{comment=Comment, clauses=Clauses, args=Args} = Function) ->
    Function#function{comment= to_binary(Comment), clauses=fixup_forms(Clauses), args=binary_args(Args)};
fixup_form(#clause{head=Head, args=Args} = Clause) ->
    Clause#clause{head=to_binary(Head), args=binary_args(Args)};
fixup_form(Other) ->
    Other.

to_binary(Comment) when is_list(Comment) ->
    try 
        iolist_to_binary(Comment) 
    catch 
        _:_ -> 
            unicode:characters_to_binary(Comment) 
    end;
to_binary(Other) ->
    Other.

%% parse_test(ScannerName, File) ->
%%     erlide_scanner_server:scan_uncached(ScannerName, File, ""),
%%     Toks = erlide_scanner_server:getTokens(ScannerName),
%%     {UncommentToks, Comments} = extract_comments(Toks),
%%     Functions = split_after_dots(UncommentToks, [], []),
%%     {Collected, Refs} = classify_and_collect(Functions, [], []),
%%     _Model = #model{forms=Collected, comments=Comments},
%%     %%erlide_noparse_server:create(ScannerName, Model, ""),
%%     ok.

classify_and_collect([], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([eof], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([C | Rest], Acc, RefsAcc, Exports, Imports) ->
    {R, Refs, MoreExports, MoreImports} = cac(check_class(C), C, Exports, Imports),
    classify_and_collect(Rest, [R | Acc], Refs++RefsAcc, MoreExports++Exports, MoreImports++Imports).

cac(function, Tokens, Exports, Imports) ->
    ClauseList = split_clauses(Tokens),
    %?D(ClauseList),
    ClausesAndRefs = fix_clauses(ClauseList),
    %?D(length(Clauses)),
    [{#clause{pos=P, name=N, args=A, head=H, name_pos=NP}, _Refs} | _] = ClausesAndRefs,
    ?D(A),
    Arity = length(A),
    Exported = get_exported({N, Arity}, Exports),
    Function = case ClausesAndRefs of  	% only show subclauses when more than one
                   [_] ->
                       #function{pos=P, name=N, arity=Arity, args=A, head=H, 
                                 clauses=[], name_pos=NP, exported=Exported};
                   _ ->
                       Clauses = [C || {C, _} <- ClausesAndRefs],
                       #function{pos=P, name=N, arity=Arity, clauses=Clauses, 
                                 name_pos=NP, exported=Exported}
               end,
    {Function, fix_refs(ClausesAndRefs, Function, Imports), [], []};
cac(attribute, Attribute, _Exports, _Imports) ->
    ?D(Attribute),
    case Attribute of
        %% -spec, -type or -opaque
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=Kind, line=_Line, offset=_Offset, value=Name} | Args]
          when (Kind=:='spec') or ((Kind=:=atom) and (Name=:='type'))
                   or ((Kind=:=atom) and (Name=:='opaque'))->
            get_type_attribute(Kind, Name, Offset, Line, Attribute, Args);
        %% other attributes
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=atom, value=Name, line=_Line, offset=_Offset},
         _, #token{value=Args} | _] = Attribute ->
            get_other_attribute(Name, Offset, Line, Attribute, Args);
        [_, #token{kind=atom, value=Name, line=Line, offset=Offset} | _] ->
            #token{line=LastLine, offset=LastOffset, 
                   length=LastLength} = last_not_eof(Attribute),
            PosLength = LastOffset - Offset + LastLength,
            {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                       name=Name, args=[]}, [], [], []}
    end;
cac(other, [#token{value=Name, line=Line, offset=Offset, length=Length} | _], 
    _Exports, _Imports) ->
    {#other{pos={{Line, Line, Offset}, Length}, name=Name}, [], [], []};
cac(_, _D, _E, _I) ->
    {eof, [], [], []}.

get_type_attribute(Kind, Name0, Offset, Line, Attribute, Args) ->
    ?D(Name0),
    Name = case Kind of 'spec' -> Kind; _ -> Name0 end,
    #token{line=LastLine, offset=LastOffset,
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    ?D(Args),
    Extra = to_string(Args),
    ?D(Extra),
    {AttrArgs, _, _} = get_attribute_args(Kind, Args, Args),
    ?D({AttrArgs, Extra}),
    ExternalRefs = get_refs(tl(AttrArgs), Extra, ?ARI_TYPESPEC),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     [#ref{data=#type_def{type=Name}, offset=Offset, length=PosLength, function=Name, 
           arity=?ARI_TYPESPEC, clause="", sub_clause=false} | ExternalRefs], [], []}.

get_other_attribute(Name, Offset, Line, Attribute, Args) ->
    #token{line=LastLine, offset=LastOffset, 
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    Between = erlide_np_util:get_between_outer_pars(Attribute, '(', ')'),
    Extra = to_string(Between),
    {AttrArgs, Exports, Imports} = get_attribute_args(Name, Between, Args),
    ?D({AttrArgs, Between}),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     make_attribute_ref(Name, AttrArgs, Extra, Offset, PosLength)++
         make_attribute_arg_refs(Name, AttrArgs, Between),
     Exports, Imports}.

get_exported(F_A, Exports) ->
    lists:member(F_A, Exports).

get_attribute_args(import, Between, _Args) ->
    From = get_first_of_kind(atom, Between),
    Tokens = get_between(Between, '[', ']'),
    Import = {From, fun_arity_from_tokens(Tokens)},
    {Import, [], [Import]};
get_attribute_args(export, Between, _Args) ->
    Tokens = get_between(Between, '[', ']'),
    Exports = fun_arity_from_tokens(Tokens),
    {Exports, Exports, []};
get_attribute_args(record, Between, _Args) ->
    [RecordToken | _] = Between,
    RecordName = RecordToken#token.value,
    Tokens = erlide_np_util:get_between_outer_pars(Between, '{', '}'),
    R = {RecordName, field_list_from_tokens(Tokens)},
    {R, [], []};
get_attribute_args(_, _Between, Args) ->
    {Args, [], []}.

check_class([#token{kind = AtomOrMacro}, #token{kind = '('} | _])
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    function;
check_class([#token{kind = '-'}, #token{kind = atom} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'spec'} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'type'} | _]) ->
    attribute;
check_class(_X) ->
    %?D(lists:sublist(_X, 5)),
    other.

get_first_of_kind(Kind, Tokens) ->
    case erlide_np_util:skip_to(Tokens, Kind) of
        [#token{kind=Kind, value=Value} | _] ->
            Value;
        _ ->
            undefined
    end.
        
fun_arity_from_tokens([#token{kind=atom, value=Fun}, #token{kind='/'}, 
                       #token{kind=integer, value=Arity} | Rest]) ->
    [{Fun, Arity} | fun_arity_from_tokens(Rest)];
fun_arity_from_tokens([_ | Rest]) ->
    fun_arity_from_tokens(Rest);
fun_arity_from_tokens(_) ->
    [].

field_list_from_tokens([#token{kind=atom, value=Field, line=Line, 
                               offset=Offset, length=Length} | Rest]) ->
    {Extra, NewRest} = get_upto2(Rest, ','),
    [{Field, {{Line, Line, Offset}, Length}, to_string(Extra)}
         | field_list_from_tokens(NewRest)];
field_list_from_tokens([_ | Rest]) ->
    field_list_from_tokens(Rest);
field_list_from_tokens(_) ->
    [].

to_string(Tokens) ->
    S = erlide_scanner:tokens_to_string(Tokens),
    erlide_text:strip(S).
%%     unspacify(S).

get_head(T) ->
    A = get_args(T),
    case get_guards(T) of
        "" ->
            A;
        G ->
            A ++ " when " ++ G
    end.

get_args(T) ->
    case erlide_np_util:get_between_outer_pars(T, '(', ')') of
        [] ->
            "";
        P ->
            "("++to_string(P)++")"
    end.

get_guards([]) ->
    "";
get_guards(T) ->
    to_string(get_between(T, 'when', '->')). 

last_not_eof(L) -> 
    case lists:reverse(L) of
        [eof, Last | _] ->
            Last;
        [Last | _] ->
            Last;
        _ ->
            error
    end.    


get_between(L, A, B) ->
    R = get_upto(L, B),
    lists:reverse(get_upto(lists:reverse(R), A)).

get_upto(L, Delim) ->
        get_upto(L, Delim, []).

get_upto([], _Delim, _Acc) ->
    [];
get_upto([#token{kind=Delim} | _], Delim, Acc) ->
        lists:reverse(Acc);
get_upto([T | Rest], Delim, Acc) ->
    get_upto(Rest, Delim, [T | Acc]).

get_upto2(L, Delim) ->
    get_upto2(L, Delim, []).

get_upto2([], _Delim, Acc) ->
    {lists:reverse(Acc), []};
get_upto2([#token{kind=Delim} | Rest], Delim, Acc) ->
    {lists:reverse(Acc), Rest};
get_upto2([T | Rest], Delim, Acc) ->
    get_upto2(Rest, Delim, [T | Acc]).

check_clause([#token{kind = ';'} | Rest], false) ->
    check_class(Rest) == function;
check_clause(_, _) ->
    false.

split_clauses(F) ->
    split_clauses(F, false, [], []).

split_clauses([], _HaveWhen, Acc, []) ->
    erlide_util:reverse2(Acc);
split_clauses([], HaveWhen, Acc, ClAcc) ->
    split_clauses([], HaveWhen, [ClAcc | Acc], []);
split_clauses([#token{kind=Kind}=T | Rest], _HaveWhen, Acc, ClAcc) 
  when Kind=:='end'; Kind=:=dot; Kind=:='->'; Kind=:='when' ->
    split_clauses(Rest, Kind=:='when', Acc, [T | ClAcc]);
split_clauses([T | Rest] = Tokens, HaveWhen, Acc, ClAcc) ->
    case check_clause(Tokens, HaveWhen) of
        false ->
            split_clauses(Rest, HaveWhen, Acc, [T | ClAcc]);
        true ->
            split_clauses(Rest, HaveWhen, [[T | ClAcc] | Acc], [])
    end.

%% fix_clause([#token{kind=atom, value=Name, line=Line, offset=Offset, length=Length} | Rest] = Code) ->
fix_clauses(Clauses) ->
    fix_clauses(Clauses, []).

fix_clauses([], Acc) ->
    lists:reverse(Acc);
fix_clauses([C | Rest], Acc) ->
    {Clause, Refs} = fix_clause(C),
    fix_clauses(Rest, [{Clause, Refs} | Acc]).

fix_clause([#token{kind=AtomOrMacro, value=Name, line=Line, offset=Offset, length=Length} | Rest]) 
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    #token{line=LastLine, offset=LastOffset, length=LastLength} = last_not_eof(Rest),
    PosLength = LastOffset - Offset + LastLength+1,
    ExternalRefs = get_refs_in_code(Rest),
    {#clause{pos={{Line, LastLine, Offset}, PosLength}, name_pos={{Line, Offset}, Length},
             name=Name, args=get_function_args(Rest), head=get_head(Rest)},
     ExternalRefs}.

get_function_args(Tokens) ->
    P = erlide_np_util:get_between_outer_pars(Tokens, '(', ')'),
    L = erlide_text:split_comma_list(P),
    [to_string(A) || A <- L].

fix_refs(ClausesAndRefs, Function, Imports) ->
    fix_refs(ClausesAndRefs, Function, Imports, []).

fix_refs([], #function{name=Name, arity=Arity, name_pos={{_, Offset}, Length}}, _Imports, Acc) ->
    Ref = #ref{data=#function_def{function=Name, arity=Arity}, offset=Offset, 
               length=Length, function=Name, arity=Arity, clause="", sub_clause=false},
    [Ref | Acc];
fix_refs([{Clause, Refs} | Rest], Function, Imports, Acc0) ->
    Acc1 = fix_refs(Refs, Clause, Function, Imports, Acc0),
    fix_refs(Rest, Function, Imports, Acc1).

fix_refs([], _Clause, _Function, _Imports, Acc) ->
    Acc;
fix_refs([{Offset, Length, RefData} | Rest], #clause{head=Head}=Clause, 
         #function{name=Name, arity=Arity, clauses=Clauses}=Function, 
         Imports, Acc) ->
    SubClause = case Clauses of
                    [] -> false;
                    [_] -> false;
                    _ -> true
                end,
    NewRefData = fix_imported_ref(RefData, Imports),
    Ref = #ref{data=NewRefData, offset=Offset, length=Length, function=Name, 
               arity=Arity, clause=Head, sub_clause=SubClause},
    fix_refs(Rest, Clause, Function, Imports, [Ref | Acc]).

fix_imported_ref(#local_call{function=F, arity=A}=RefData, [{Module, Imports} | Rest]) ->
    case lists:member({F, A}, Imports) of
        true -> #external_call{module=Module, function=F, arity=A};
        false -> fix_imported_ref(RefData, Rest)
    end;
fix_imported_ref(RefData, _Imports) ->
    RefData.

%% find references in code
%% VERY simple, M:F(_ ...), F(_ ...), #R, ?M, and that's it
%% returns {Offset, Length, Ref}, which is made into proper refs
%% by fix_refs
get_refs_in_code(L) ->
    get_refs_in_code(L, []).

get_refs_in_code([], Acc) ->
    lists:reverse(Acc);
get_refs_in_code([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
                  #token{kind=atom, value=F, offset=Offset2, length=Length2},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length2+Offset2-Offset, 
         #external_call{module = M, function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=atom, value=F, offset=Offset, length=Length},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length, #local_call{function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest], 
                 Acc) ->
    R = {Offset, Length, #macro_ref{macro=M}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind='#', offset=Offset}, 
                  #token{kind=atom, value=Record, offset=Offset2, length=Length2} | Rest],
                 Acc) ->
    R = {Offset, Length2+Offset2-Offset, #record_ref{record=Record}},
    {NewRest, Fields, RightSides} = erlide_np_records:check_fields(Rest, Record),
    NewAcc = get_refs_in_code(RightSides, Acc),
    get_refs_in_code(NewRest, Fields ++ [R | NewAcc]);
get_refs_in_code([#token{kind=var, value=V, offset=Offset, length=Length} | Rest],
                 Acc) ->
    R = {Offset, Length, make_var_def_ref(Acc, V)},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([_ | Rest], Acc) ->
    get_refs_in_code(Rest, Acc).

make_var_def_ref([], Var) ->
	#var_def{variable=Var};
make_var_def_ref([{_, _, #var_def{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([{_, _, #var_ref{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([_ | Rest], Var) ->
	make_var_def_ref(Rest, Var).

%% find type references in spec, type and opaque
%% also VERY simple, type(), ?M, #R, only
%% returns proper refs
get_refs(Tokens, Name, Arity) ->
	get_refs(Tokens, Name, Arity, []).

get_refs([], _, _, Acc) ->
    lists:reverse(Acc);
get_refs([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #macro_ref{macro=M}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind='#', offset=Offset},
          #token{kind=atom, value=R, offset=Offset2, length=Length2} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset, #record_ref{record=R}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
		  #token{kind=atom, value=T, offset=Offset2, length=Length2},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset,
                   #type_ref{module=M, type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=T, offset=Offset, length=Length},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #type_ref{module='_', type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=variable, value=V, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #var_ref{variable=V}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([_ | Rest], Name, Arity, Acc) ->
    get_refs(Rest, Name, Arity, Acc).

get_record_field_defs([], _) ->
    [];
get_record_field_defs([{FieldName, {{_, _, Offset}, Length}, _Extra} | Rest],
                      RecordName) ->
    [make_ref(Offset, Length, #record_field_def{record=RecordName, field=FieldName}, 
              FieldName, ?ARI_RECORD_FIELD_DEF)
         | get_record_field_defs(Rest, RecordName)].

make_ref(Offset, Length, Data, Name, Arity) ->
    #ref{data=Data, offset=Offset, length=Length, function=Name, arity=Arity, 
         clause="", sub_clause=false}.

make_attribute_arg_refs(define, Name, [#token{}, #token{kind='('} | Rest]) ->
    get_refs(skip_to_rparen(Rest), Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(define, Name, [#token{}, #token{kind=','} | Rest]) ->
    get_refs(Rest, Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(record, {Name, Fields}, [#token{}, #token{kind=','} | Rest]) ->
    get_record_field_defs(Fields, Name) ++ get_refs(Rest, Name, ?ARI_RECORD_DEF);
make_attribute_arg_refs(_, _, _) ->
    [].

skip_to_rparen([]) ->
    [];
skip_to_rparen([#token{kind=')'} | Rest]) ->
    Rest;
skip_to_rparen([_ | Rest]) ->
    skip_to_rparen(Rest).

make_attribute_ref(Name, Between, Extra, Offset, Length) ->
    ?D({Name, Between, Offset, Length}),
    case make_attribute_ref(Name, Between, Extra) of
        [] ->
            [];
        {Arity, AName, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=AName, 
                  arity=Arity, clause="", sub_clause=false}];
        {Arity, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=Name, 
                  arity=Arity, clause="", sub_clause=false}]
    end.

make_attribute_ref(module, _, Extra) ->
    {?ARI_ATTRIBUTE, #module_def{module=Extra}};
make_attribute_ref(record, Between, _E) ->
    ?D({Between, _E}),
    Name = case Between of
               {N, _} -> N;
               N -> N
           end,
    R= #record_def{record=Name},
    ?D(R),
    {?ARI_RECORD_DEF, Name, R};
make_attribute_ref(define, [Name | _], _) when is_list(Name) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(define, Name, _) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(include, _, Extra) ->
    {?ARI_INCLUDE, #include_ref{filename=unquote(Extra)}};
make_attribute_ref(_, _, _) ->
    [].

unquote(L) ->
    lists:reverse(unquote_first(lists:reverse(unquote_first(L)))).

unquote_first([$" | Rest]) ->
    Rest;
unquote_first(L) ->
    L.

remove_rest([], _) -> [];
remove_rest(Rest, Rest) -> [];
remove_rest([Hd | Tl], Rest) -> [Hd | remove_rest(Tl, Rest)].
%% Description: Split source files into functions and clauses and collects references
%% Author: jakob
%% Created: Mar 23, 2006

-module(erlide_noparse).

%%
%% Exported Functions
%%

%% called from Java
-export([initial_parse/5, reparse/1, remove_cache_files/2]).

%% called from Erlang
-export([read_module_refs/3]).

%%
%% Include files
%%

%% -define(DEBUG, 1).
%% -define(IO_FORMAT_DEBUG, 1).

-define(CACHE_VERSION, 25).
-define(SERVER, erlide_noparse).

-include("erlide.hrl").
-include("erlide_noparse.hrl").
-include("erlide_scanner.hrl").
-include("erlide_search_server.hrl").

%% special arity flags
-define(ARI_TYPESPEC, -2).
-define(ARI_ATTRIBUTE, -3).
-define(ARI_RECORD_DEF, -4).
-define(ARI_MACRO_DEF, -5).
-define(ARI_INCLUDE, -6).
-define(ARI_RECORD_FIELD_DEF, -7).

-define(X(X), erlide_log:log({X, erlang:now()})).


%%
%% API Functions
%%

initial_parse(ScannerName, ModuleFileName, StateDir, UseCache,
              UpdateSearchServer) ->
    try
        %%         ?D({StateDir, ModuleFileName}),
        BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
        RefsFileName = BaseName ++ ".refs",
        RenewFun = fun(_F) ->
                           do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer)
                   end,
        CacheFileName = BaseName ++ ".noparse",
        ?D(CacheFileName),
        {Cached, Res} = erlide_util:check_and_renew_cached(
                          ModuleFileName, CacheFileName, ?CACHE_VERSION, 
                          RenewFun, UseCache),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        ?D(updated),
        {ok, Res, Cached}
    catch
        error:Reason ->
            {error, Reason}
    end.

reparse(ScannerName) ->
    try
        Res = do_parse(ScannerName, "", "", true),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        {ok, Res, unused}
    catch
        error:Reason ->
            {error, Reason}
    end.

read_module_refs(ScannerName, ModulePath, StateDir) ->
    ?D(ScannerName),
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    RefsFileName = BaseName ++ ".refs",
    ?D(RefsFileName),
    case file:read_file(RefsFileName) of
        {ok, Binary} ->
            ?D(byte_size(Binary)),
            binary_to_term(Binary);
        _ ->
            ?D(ModulePath),
            {ok, InitialTextBin} = file:read_file(ModulePath),
            ?D(byte_size(InitialTextBin)),
            InitialText = binary_to_list(InitialTextBin),
            _D = erlide_scanner_server:initialScan(
                   ScannerName, ModulePath, InitialText, StateDir, true),
            ?D(_D),
            initial_parse(ScannerName, ModulePath, StateDir, true, false),
            ?D(RefsFileName),
            {ok, Binary} = file:read_file(RefsFileName),
            ?D(byte_size(Binary)),
            binary_to_term(Binary)
    end.

remove_cache_files(ScannerName, StateDir) ->
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    ScannerCacheFileName = BaseName ++ ".scan",
    file:delete(ScannerCacheFileName),
    RefsFileName = BaseName ++ ".refs",
    file:delete(RefsFileName),
    CacheFileName = BaseName ++ ".noparse",
    file:delete(CacheFileName).

%%
%% Internal functions
%%

do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer) ->
    Toks = erlide_scanner_server:getTokens(ScannerName),
    do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer).

do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer) ->
    ?D({do_parse, ScannerName, length(Toks)}),
    ?X(111),
    {UncommentToks, Comments} = erlide_np_util:extract_comments(Toks),
    ?X(222),
    ?D({length(UncommentToks), length(Comments)}),
    ?D({UncommentToks}),
    Functions = erlide_np_util:split_after_dots(UncommentToks),
    ?X(333),
    ?D(length(Functions)),
    AutoImports = erlide_util:add_auto_imported([]),
    ?X(444),
%%     ?D(AutoImports),
    {Collected, Refs} = classify_and_collect(Functions, [], [], [], AutoImports),
    ?X(555),
    ?D({'>>',length(Collected)}),
    CommentedCollected = erlide_np_util:get_function_comments(Collected, Comments),
    ?X(666),
    ?D(CommentedCollected),
    Model = #model{forms=CommentedCollected, comments=Comments},
    %%erlide_noparse_server:create(ScannerName, Model),
%%     ?D({"Model", length(Model#model.forms), erts_debug:flat_size(Model)}),
    FixedModel = fixup_model(Model),
    ?X(777),
%%     ?D(erts_debug:flat_size(FixedModel)),
    %% 	?D([erts_debug:flat_size(F) || F <- element(2, FixedModel)]),
    ?D(StateDir),
    case StateDir of
        "" -> ok;
        _ ->
            ?D({RefsFileName, length(Refs)}),
            file:write_file(RefsFileName, term_to_binary(Refs, [compressed]))
    end,
    update_search_server(UpdateSearchServer, ScannerName, Refs),
%%     ?D(FixedModel),
    ?X(888),
    FixedModel.

update_search_server(true, ScannerName, Refs) ->
    erlide_search_server:add_module_refs(ScannerName, Refs);
update_search_server(_, _, _) ->
    ok.

fixup_model(#model{forms=Forms, comments=Comments}) ->
    FixedComments = fixup_tokens(Comments),
    FixedForms = fixup_forms(Forms),
    #model{forms=FixedForms, comments=FixedComments}.

fixup_forms(Forms) ->
    [fixup_form(Form) || Form <- Forms].

fixup_tokens(Tokens) ->
    [fixup_token(Token) || Token <- Tokens].

fixup_token(#token{value=Value} = Token) when is_list(Value) ->
    Token#token{value=to_binary(Value)};
fixup_token(#token{text=Text} = Token) when is_list(Text) ->
    Token#token{value=to_binary(Text)};
fixup_token(Token) ->
    Token.

binary_args(Args) when is_list(Args) ->
    [iolist_to_binary(A) || A <- Args];
binary_args(_) ->
    [].

fixup_form(#function{comment=Comment, clauses=Clauses, args=Args} = Function) ->
    Function#function{comment= to_binary(Comment), clauses=fixup_forms(Clauses), args=binary_args(Args)};
fixup_form(#clause{head=Head, args=Args} = Clause) ->
    Clause#clause{head=to_binary(Head), args=binary_args(Args)};
fixup_form(Other) ->
    Other.

to_binary(Comment) when is_list(Comment) ->
    try 
        iolist_to_binary(Comment) 
    catch 
        _:_ -> 
            unicode:characters_to_binary(Comment) 
    end;
to_binary(Other) ->
    Other.

%% parse_test(ScannerName, File) ->
%%     erlide_scanner_server:scan_uncached(ScannerName, File, ""),
%%     Toks = erlide_scanner_server:getTokens(ScannerName),
%%     {UncommentToks, Comments} = extract_comments(Toks),
%%     Functions = split_after_dots(UncommentToks, [], []),
%%     {Collected, Refs} = classify_and_collect(Functions, [], []),
%%     _Model = #model{forms=Collected, comments=Comments},
%%     %%erlide_noparse_server:create(ScannerName, Model, ""),
%%     ok.

classify_and_collect([], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([eof], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([C | Rest], Acc, RefsAcc, Exports, Imports) ->
    {R, Refs, MoreExports, MoreImports} = cac(check_class(C), C, Exports, Imports),
    classify_and_collect(Rest, [R | Acc], Refs++RefsAcc, MoreExports++Exports, MoreImports++Imports).

cac(function, Tokens, Exports, Imports) ->
    ClauseList = split_clauses(Tokens),
    %?D(ClauseList),
    ClausesAndRefs = fix_clauses(ClauseList),
    %?D(length(Clauses)),
    [{#clause{pos=P, name=N, args=A, head=H, name_pos=NP}, _Refs} | _] = ClausesAndRefs,
    ?D(A),
    Arity = length(A),
    Exported = get_exported({N, Arity}, Exports),
    Function = case ClausesAndRefs of  	% only show subclauses when more than one
                   [_] ->
                       #function{pos=P, name=N, arity=Arity, args=A, head=H, 
                                 clauses=[], name_pos=NP, exported=Exported};
                   _ ->
                       Clauses = [C || {C, _} <- ClausesAndRefs],
                       #function{pos=P, name=N, arity=Arity, clauses=Clauses, 
                                 name_pos=NP, exported=Exported}
               end,
    {Function, fix_refs(ClausesAndRefs, Function, Imports), [], []};
cac(attribute, Attribute, _Exports, _Imports) ->
    ?D(Attribute),
    case Attribute of
        %% -spec, -type or -opaque
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=Kind, line=_Line, offset=_Offset, value=Name} | Args]
          when (Kind=:='spec') or ((Kind=:=atom) and (Name=:='type'))
                   or ((Kind=:=atom) and (Name=:='opaque'))->
            get_type_attribute(Kind, Name, Offset, Line, Attribute, Args);
        %% other attributes
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=atom, value=Name, line=_Line, offset=_Offset},
         _, #token{value=Args} | _] = Attribute ->
            get_other_attribute(Name, Offset, Line, Attribute, Args);
        [_, #token{kind=atom, value=Name, line=Line, offset=Offset} | _] ->
            #token{line=LastLine, offset=LastOffset, 
                   length=LastLength} = last_not_eof(Attribute),
            PosLength = LastOffset - Offset + LastLength,
            {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                       name=Name, args=[]}, [], [], []}
    end;
cac(other, [#token{value=Name, line=Line, offset=Offset, length=Length} | _], 
    _Exports, _Imports) ->
    {#other{pos={{Line, Line, Offset}, Length}, name=Name}, [], [], []};
cac(_, _D, _E, _I) ->
    {eof, [], [], []}.

get_type_attribute(Kind, Name0, Offset, Line, Attribute, Args) ->
    ?D(Name0),
    Name = case Kind of 'spec' -> Kind; _ -> Name0 end,
    #token{line=LastLine, offset=LastOffset,
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    ?D(Args),
    Extra = to_string(Args),
    ?D(Extra),
    {AttrArgs, _, _} = get_attribute_args(Kind, Args, Args),
    ?D({AttrArgs, Extra}),
    ExternalRefs = get_refs(tl(AttrArgs), Extra, ?ARI_TYPESPEC),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     [#ref{data=#type_def{type=Name}, offset=Offset, length=PosLength, function=Name, 
           arity=?ARI_TYPESPEC, clause="", sub_clause=false} | ExternalRefs], [], []}.

get_other_attribute(Name, Offset, Line, Attribute, Args) ->
    #token{line=LastLine, offset=LastOffset, 
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    Between = erlide_np_util:get_between_outer_pars(Attribute, '(', ')'),
    Extra = to_string(Between),
    {AttrArgs, Exports, Imports} = get_attribute_args(Name, Between, Args),
    ?D({AttrArgs, Between}),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     make_attribute_ref(Name, AttrArgs, Extra, Offset, PosLength)++
         make_attribute_arg_refs(Name, AttrArgs, Between),
     Exports, Imports}.

get_exported(F_A, Exports) ->
    lists:member(F_A, Exports).

get_attribute_args(import, Between, _Args) ->
    From = get_first_of_kind(atom, Between),
    Tokens = get_between(Between, '[', ']'),
    Import = {From, fun_arity_from_tokens(Tokens)},
    {Import, [], [Import]};
get_attribute_args(export, Between, _Args) ->
    Tokens = get_between(Between, '[', ']'),
    Exports = fun_arity_from_tokens(Tokens),
    {Exports, Exports, []};
get_attribute_args(record, Between, _Args) ->
    [RecordToken | _] = Between,
    RecordName = RecordToken#token.value,
    Tokens = erlide_np_util:get_between_outer_pars(Between, '{', '}'),
    R = {RecordName, field_list_from_tokens(Tokens)},
    {R, [], []};
get_attribute_args(_, _Between, Args) ->
    {Args, [], []}.

check_class([#token{kind = AtomOrMacro}, #token{kind = '('} | _])
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    function;
check_class([#token{kind = '-'}, #token{kind = atom} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'spec'} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'type'} | _]) ->
    attribute;
check_class(_X) ->
    %?D(lists:sublist(_X, 5)),
    other.

get_first_of_kind(Kind, Tokens) ->
    case erlide_np_util:skip_to(Tokens, Kind) of
        [#token{kind=Kind, value=Value} | _] ->
            Value;
        _ ->
            undefined
    end.
        
fun_arity_from_tokens([#token{kind=atom, value=Fun}, #token{kind='/'}, 
                       #token{kind=integer, value=Arity} | Rest]) ->
    [{Fun, Arity} | fun_arity_from_tokens(Rest)];
fun_arity_from_tokens([_ | Rest]) ->
    fun_arity_from_tokens(Rest);
fun_arity_from_tokens(_) ->
    [].

field_list_from_tokens([#token{kind=atom, value=Field, line=Line, 
                               offset=Offset, length=Length} | Rest]) ->
    {Extra, NewRest} = get_upto2(Rest, ','),
    [{Field, {{Line, Line, Offset}, Length}, to_string(Extra)}
         | field_list_from_tokens(NewRest)];
field_list_from_tokens([_ | Rest]) ->
    field_list_from_tokens(Rest);
field_list_from_tokens(_) ->
    [].

to_string(Tokens) ->
    S = erlide_scanner:tokens_to_string(Tokens),
    erlide_text:strip(S).
%%     unspacify(S).

get_head(T) ->
    A = get_args(T),
    case get_guards(T) of
        "" ->
            A;
        G ->
            A ++ " when " ++ G
    end.

get_args(T) ->
    case erlide_np_util:get_between_outer_pars(T, '(', ')') of
        [] ->
            "";
        P ->
            "("++to_string(P)++")"
    end.

get_guards([]) ->
    "";
get_guards(T) ->
    to_string(get_between(T, 'when', '->')). 

last_not_eof(L) -> 
    case lists:reverse(L) of
        [eof, Last | _] ->
            Last;
        [Last | _] ->
            Last;
        _ ->
            error
    end.    


get_between(L, A, B) ->
    R = get_upto(L, B),
    lists:reverse(get_upto(lists:reverse(R), A)).

get_upto(L, Delim) ->
        get_upto(L, Delim, []).

get_upto([], _Delim, _Acc) ->
    [];
get_upto([#token{kind=Delim} | _], Delim, Acc) ->
        lists:reverse(Acc);
get_upto([T | Rest], Delim, Acc) ->
    get_upto(Rest, Delim, [T | Acc]).

get_upto2(L, Delim) ->
    get_upto2(L, Delim, []).

get_upto2([], _Delim, Acc) ->
    {lists:reverse(Acc), []};
get_upto2([#token{kind=Delim} | Rest], Delim, Acc) ->
    {lists:reverse(Acc), Rest};
get_upto2([T | Rest], Delim, Acc) ->
    get_upto2(Rest, Delim, [T | Acc]).

check_clause([#token{kind = ';'} | Rest], false) ->
    check_class(Rest) == function;
check_clause(_, _) ->
    false.

split_clauses(F) ->
    split_clauses(F, false, [], []).

split_clauses([], _HaveWhen, Acc, []) ->
    erlide_util:reverse2(Acc);
split_clauses([], HaveWhen, Acc, ClAcc) ->
    split_clauses([], HaveWhen, [ClAcc | Acc], []);
split_clauses([#token{kind=Kind}=T | Rest], _HaveWhen, Acc, ClAcc) 
  when Kind=:='end'; Kind=:=dot; Kind=:='->'; Kind=:='when' ->
    split_clauses(Rest, Kind=:='when', Acc, [T | ClAcc]);
split_clauses([T | Rest] = Tokens, HaveWhen, Acc, ClAcc) ->
    case check_clause(Tokens, HaveWhen) of
        false ->
            split_clauses(Rest, HaveWhen, Acc, [T | ClAcc]);
        true ->
            split_clauses(Rest, HaveWhen, [[T | ClAcc] | Acc], [])
    end.

%% fix_clause([#token{kind=atom, value=Name, line=Line, offset=Offset, length=Length} | Rest] = Code) ->
fix_clauses(Clauses) ->
    fix_clauses(Clauses, []).

fix_clauses([], Acc) ->
    lists:reverse(Acc);
fix_clauses([C | Rest], Acc) ->
    {Clause, Refs} = fix_clause(C),
    fix_clauses(Rest, [{Clause, Refs} | Acc]).

fix_clause([#token{kind=AtomOrMacro, value=Name, line=Line, offset=Offset, length=Length} | Rest]) 
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    #token{line=LastLine, offset=LastOffset, length=LastLength} = last_not_eof(Rest),
    PosLength = LastOffset - Offset + LastLength+1,
    ExternalRefs = get_refs_in_code(Rest),
    {#clause{pos={{Line, LastLine, Offset}, PosLength}, name_pos={{Line, Offset}, Length},
             name=Name, args=get_function_args(Rest), head=get_head(Rest)},
     ExternalRefs}.

get_function_args(Tokens) ->
    P = erlide_np_util:get_between_outer_pars(Tokens, '(', ')'),
    L = erlide_text:split_comma_list(P),
    [to_string(A) || A <- L].

fix_refs(ClausesAndRefs, Function, Imports) ->
    fix_refs(ClausesAndRefs, Function, Imports, []).

fix_refs([], #function{name=Name, arity=Arity, name_pos={{_, Offset}, Length}}, _Imports, Acc) ->
    Ref = #ref{data=#function_def{function=Name, arity=Arity}, offset=Offset, 
               length=Length, function=Name, arity=Arity, clause="", sub_clause=false},
    [Ref | Acc];
fix_refs([{Clause, Refs} | Rest], Function, Imports, Acc0) ->
    Acc1 = fix_refs(Refs, Clause, Function, Imports, Acc0),
    fix_refs(Rest, Function, Imports, Acc1).

fix_refs([], _Clause, _Function, _Imports, Acc) ->
    Acc;
fix_refs([{Offset, Length, RefData} | Rest], #clause{head=Head}=Clause, 
         #function{name=Name, arity=Arity, clauses=Clauses}=Function, 
         Imports, Acc) ->
    SubClause = case Clauses of
                    [] -> false;
                    [_] -> false;
                    _ -> true
                end,
    NewRefData = fix_imported_ref(RefData, Imports),
    Ref = #ref{data=NewRefData, offset=Offset, length=Length, function=Name, 
               arity=Arity, clause=Head, sub_clause=SubClause},
    fix_refs(Rest, Clause, Function, Imports, [Ref | Acc]).

fix_imported_ref(#local_call{function=F, arity=A}=RefData, [{Module, Imports} | Rest]) ->
    case lists:member({F, A}, Imports) of
        true -> #external_call{module=Module, function=F, arity=A};
        false -> fix_imported_ref(RefData, Rest)
    end;
fix_imported_ref(RefData, _Imports) ->
    RefData.

%% find references in code
%% VERY simple, M:F(_ ...), F(_ ...), #R, ?M, and that's it
%% returns {Offset, Length, Ref}, which is made into proper refs
%% by fix_refs
get_refs_in_code(L) ->
    get_refs_in_code(L, []).

get_refs_in_code([], Acc) ->
    lists:reverse(Acc);
get_refs_in_code([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
                  #token{kind=atom, value=F, offset=Offset2, length=Length2},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length2+Offset2-Offset, 
         #external_call{module = M, function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=atom, value=F, offset=Offset, length=Length},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length, #local_call{function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest], 
                 Acc) ->
    R = {Offset, Length, #macro_ref{macro=M}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind='#', offset=Offset}, 
                  #token{kind=atom, value=Record, offset=Offset2, length=Length2} | Rest],
                 Acc) ->
    R = {Offset, Length2+Offset2-Offset, #record_ref{record=Record}},
    {NewRest, Fields, RightSides} = erlide_np_records:check_fields(Rest, Record),
    NewAcc = get_refs_in_code(RightSides, Acc),
    get_refs_in_code(NewRest, Fields ++ [R | NewAcc]);
get_refs_in_code([#token{kind=var, value=V, offset=Offset, length=Length} | Rest],
                 Acc) ->
    R = {Offset, Length, make_var_def_ref(Acc, V)},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([_ | Rest], Acc) ->
    get_refs_in_code(Rest, Acc).

make_var_def_ref([], Var) ->
	#var_def{variable=Var};
make_var_def_ref([{_, _, #var_def{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([{_, _, #var_ref{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([_ | Rest], Var) ->
	make_var_def_ref(Rest, Var).

%% find type references in spec, type and opaque
%% also VERY simple, type(), ?M, #R, only
%% returns proper refs
get_refs(Tokens, Name, Arity) ->
	get_refs(Tokens, Name, Arity, []).

get_refs([], _, _, Acc) ->
    lists:reverse(Acc);
get_refs([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #macro_ref{macro=M}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind='#', offset=Offset},
          #token{kind=atom, value=R, offset=Offset2, length=Length2} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset, #record_ref{record=R}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
		  #token{kind=atom, value=T, offset=Offset2, length=Length2},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset,
                   #type_ref{module=M, type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=T, offset=Offset, length=Length},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #type_ref{module='_', type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=variable, value=V, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #var_ref{variable=V}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([_ | Rest], Name, Arity, Acc) ->
    get_refs(Rest, Name, Arity, Acc).

get_record_field_defs([], _) ->
    [];
get_record_field_defs([{FieldName, {{_, _, Offset}, Length}, _Extra} | Rest],
                      RecordName) ->
    [make_ref(Offset, Length, #record_field_def{record=RecordName, field=FieldName}, 
              FieldName, ?ARI_RECORD_FIELD_DEF)
         | get_record_field_defs(Rest, RecordName)].

make_ref(Offset, Length, Data, Name, Arity) ->
    #ref{data=Data, offset=Offset, length=Length, function=Name, arity=Arity, 
         clause="", sub_clause=false}.

make_attribute_arg_refs(define, Name, [#token{}, #token{kind='('} | Rest]) ->
    get_refs(skip_to_rparen(Rest), Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(define, Name, [#token{}, #token{kind=','} | Rest]) ->
    get_refs(Rest, Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(record, {Name, Fields}, [#token{}, #token{kind=','} | Rest]) ->
    get_record_field_defs(Fields, Name) ++ get_refs(Rest, Name, ?ARI_RECORD_DEF);
make_attribute_arg_refs(_, _, _) ->
    [].

skip_to_rparen([]) ->
    [];
skip_to_rparen([#token{kind=')'} | Rest]) ->
    Rest;
skip_to_rparen([_ | Rest]) ->
    skip_to_rparen(Rest).

make_attribute_ref(Name, Between, Extra, Offset, Length) ->
    ?D({Name, Between, Offset, Length}),
    case make_attribute_ref(Name, Between, Extra) of
        [] ->
            [];
        {Arity, AName, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=AName, 
                  arity=Arity, clause="", sub_clause=false}];
        {Arity, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=Name, 
                  arity=Arity, clause="", sub_clause=false}]
    end.

make_attribute_ref(module, _, Extra) ->
    {?ARI_ATTRIBUTE, #module_def{module=Extra}};
make_attribute_ref(record, Between, _E) ->
    ?D({Between, _E}),
    Name = case Between of
               {N, _} -> N;
               N -> N
           end,
    R= #record_def{record=Name},
    ?D(R),
    {?ARI_RECORD_DEF, Name, R};
make_attribute_ref(define, [Name | _], _) when is_list(Name) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(define, Name, _) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(include, _, Extra) ->
    {?ARI_INCLUDE, #include_ref{filename=unquote(Extra)}};
make_attribute_ref(_, _, _) ->
    [].

unquote(L) ->
    lists:reverse(unquote_first(lists:reverse(unquote_first(L)))).

unquote_first([$" | Rest]) ->
    Rest;
unquote_first(L) ->
    L.

remove_rest([], _) -> [];
remove_rest(Rest, Rest) -> [];
remove_rest([Hd | Tl], Rest) -> [Hd | remove_rest(Tl, Rest)].
%% Description: Split source files into functions and clauses and collects references
%% Author: jakob
%% Created: Mar 23, 2006

-module(erlide_noparse).

%%
%% Exported Functions
%%

%% called from Java
-export([initial_parse/5, reparse/1, remove_cache_files/2]).

%% called from Erlang
-export([read_module_refs/3]).

%%
%% Include files
%%

%% -define(DEBUG, 1).
%% -define(IO_FORMAT_DEBUG, 1).

-define(CACHE_VERSION, 25).
-define(SERVER, erlide_noparse).

-include("erlide.hrl").
-include("erlide_noparse.hrl").
-include("erlide_scanner.hrl").
-include("erlide_search_server.hrl").

%% special arity flags
-define(ARI_TYPESPEC, -2).
-define(ARI_ATTRIBUTE, -3).
-define(ARI_RECORD_DEF, -4).
-define(ARI_MACRO_DEF, -5).
-define(ARI_INCLUDE, -6).
-define(ARI_RECORD_FIELD_DEF, -7).

-define(X(X), erlide_log:log({X, erlang:now()})).


%%
%% API Functions
%%

initial_parse(ScannerName, ModuleFileName, StateDir, UseCache,
              UpdateSearchServer) ->
    try
        %%         ?D({StateDir, ModuleFileName}),
        BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
        RefsFileName = BaseName ++ ".refs",
        RenewFun = fun(_F) ->
                           do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer)
                   end,
        CacheFileName = BaseName ++ ".noparse",
        ?D(CacheFileName),
        {Cached, Res} = erlide_util:check_and_renew_cached(
                          ModuleFileName, CacheFileName, ?CACHE_VERSION, 
                          RenewFun, UseCache),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        ?D(updated),
        {ok, Res, Cached}
    catch
        error:Reason ->
            {error, Reason}
    end.

reparse(ScannerName) ->
    try
        Res = do_parse(ScannerName, "", "", true),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        {ok, Res, unused}
    catch
        error:Reason ->
            {error, Reason}
    end.

read_module_refs(ScannerName, ModulePath, StateDir) ->
    ?D(ScannerName),
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    RefsFileName = BaseName ++ ".refs",
    ?D(RefsFileName),
    case file:read_file(RefsFileName) of
        {ok, Binary} ->
            ?D(byte_size(Binary)),
            binary_to_term(Binary);
        _ ->
            ?D(ModulePath),
            {ok, InitialTextBin} = file:read_file(ModulePath),
            ?D(byte_size(InitialTextBin)),
            InitialText = binary_to_list(InitialTextBin),
            _D = erlide_scanner_server:initialScan(
                   ScannerName, ModulePath, InitialText, StateDir, true),
            ?D(_D),
            initial_parse(ScannerName, ModulePath, StateDir, true, false),
            ?D(RefsFileName),
            {ok, Binary} = file:read_file(RefsFileName),
            ?D(byte_size(Binary)),
            binary_to_term(Binary)
    end.

remove_cache_files(ScannerName, StateDir) ->
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    ScannerCacheFileName = BaseName ++ ".scan",
    file:delete(ScannerCacheFileName),
    RefsFileName = BaseName ++ ".refs",
    file:delete(RefsFileName),
    CacheFileName = BaseName ++ ".noparse",
    file:delete(CacheFileName).

%%
%% Internal functions
%%

do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer) ->
    Toks = erlide_scanner_server:getTokens(ScannerName),
    do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer).

do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer) ->
    ?D({do_parse, ScannerName, length(Toks)}),
    ?X(111),
    {UncommentToks, Comments} = erlide_np_util:extract_comments(Toks),
    ?X(222),
    ?D({length(UncommentToks), length(Comments)}),
    ?D({UncommentToks}),
    Functions = erlide_np_util:split_after_dots(UncommentToks),
    ?X(333),
    ?D(length(Functions)),
    AutoImports = erlide_util:add_auto_imported([]),
    ?X(444),
%%     ?D(AutoImports),
    {Collected, Refs} = classify_and_collect(Functions, [], [], [], AutoImports),
    ?X(555),
    ?D({'>>',length(Collected)}),
    CommentedCollected = erlide_np_util:get_function_comments(Collected, Comments),
    ?X(666),
    ?D(CommentedCollected),
    Model = #model{forms=CommentedCollected, comments=Comments},
    %%erlide_noparse_server:create(ScannerName, Model),
%%     ?D({"Model", length(Model#model.forms), erts_debug:flat_size(Model)}),
    FixedModel = fixup_model(Model),
    ?X(777),
%%     ?D(erts_debug:flat_size(FixedModel)),
    %% 	?D([erts_debug:flat_size(F) || F <- element(2, FixedModel)]),
    ?D(StateDir),
    case StateDir of
        "" -> ok;
        _ ->
            ?D({RefsFileName, length(Refs)}),
            file:write_file(RefsFileName, term_to_binary(Refs, [compressed]))
    end,
    update_search_server(UpdateSearchServer, ScannerName, Refs),
%%     ?D(FixedModel),
    ?X(888),
    FixedModel.

update_search_server(true, ScannerName, Refs) ->
    erlide_search_server:add_module_refs(ScannerName, Refs);
update_search_server(_, _, _) ->
    ok.

fixup_model(#model{forms=Forms, comments=Comments}) ->
    FixedComments = fixup_tokens(Comments),
    FixedForms = fixup_forms(Forms),
    #model{forms=FixedForms, comments=FixedComments}.

fixup_forms(Forms) ->
    [fixup_form(Form) || Form <- Forms].

fixup_tokens(Tokens) ->
    [fixup_token(Token) || Token <- Tokens].

fixup_token(#token{value=Value} = Token) when is_list(Value) ->
    Token#token{value=to_binary(Value)};
fixup_token(#token{text=Text} = Token) when is_list(Text) ->
    Token#token{value=to_binary(Text)};
fixup_token(Token) ->
    Token.

binary_args(Args) when is_list(Args) ->
    [iolist_to_binary(A) || A <- Args];
binary_args(_) ->
    [].

fixup_form(#function{comment=Comment, clauses=Clauses, args=Args} = Function) ->
    Function#function{comment= to_binary(Comment), clauses=fixup_forms(Clauses), args=binary_args(Args)};
fixup_form(#clause{head=Head, args=Args} = Clause) ->
    Clause#clause{head=to_binary(Head), args=binary_args(Args)};
fixup_form(Other) ->
    Other.

to_binary(Comment) when is_list(Comment) ->
    try 
        iolist_to_binary(Comment) 
    catch 
        _:_ -> 
            unicode:characters_to_binary(Comment) 
    end;
to_binary(Other) ->
    Other.

%% parse_test(ScannerName, File) ->
%%     erlide_scanner_server:scan_uncached(ScannerName, File, ""),
%%     Toks = erlide_scanner_server:getTokens(ScannerName),
%%     {UncommentToks, Comments} = extract_comments(Toks),
%%     Functions = split_after_dots(UncommentToks, [], []),
%%     {Collected, Refs} = classify_and_collect(Functions, [], []),
%%     _Model = #model{forms=Collected, comments=Comments},
%%     %%erlide_noparse_server:create(ScannerName, Model, ""),
%%     ok.

classify_and_collect([], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([eof], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([C | Rest], Acc, RefsAcc, Exports, Imports) ->
    {R, Refs, MoreExports, MoreImports} = cac(check_class(C), C, Exports, Imports),
    classify_and_collect(Rest, [R | Acc], Refs++RefsAcc, MoreExports++Exports, MoreImports++Imports).

cac(function, Tokens, Exports, Imports) ->
    ClauseList = split_clauses(Tokens),
    %?D(ClauseList),
    ClausesAndRefs = fix_clauses(ClauseList),
    %?D(length(Clauses)),
    [{#clause{pos=P, name=N, args=A, head=H, name_pos=NP}, _Refs} | _] = ClausesAndRefs,
    ?D(A),
    Arity = length(A),
    Exported = get_exported({N, Arity}, Exports),
    Function = case ClausesAndRefs of  	% only show subclauses when more than one
                   [_] ->
                       #function{pos=P, name=N, arity=Arity, args=A, head=H, 
                                 clauses=[], name_pos=NP, exported=Exported};
                   _ ->
                       Clauses = [C || {C, _} <- ClausesAndRefs],
                       #function{pos=P, name=N, arity=Arity, clauses=Clauses, 
                                 name_pos=NP, exported=Exported}
               end,
    {Function, fix_refs(ClausesAndRefs, Function, Imports), [], []};
cac(attribute, Attribute, _Exports, _Imports) ->
    ?D(Attribute),
    case Attribute of
        %% -spec, -type or -opaque
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=Kind, line=_Line, offset=_Offset, value=Name} | Args]
          when (Kind=:='spec') or ((Kind=:=atom) and (Name=:='type'))
                   or ((Kind=:=atom) and (Name=:='opaque'))->
            get_type_attribute(Kind, Name, Offset, Line, Attribute, Args);
        %% other attributes
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=atom, value=Name, line=_Line, offset=_Offset},
         _, #token{value=Args} | _] = Attribute ->
            get_other_attribute(Name, Offset, Line, Attribute, Args);
        [_, #token{kind=atom, value=Name, line=Line, offset=Offset} | _] ->
            #token{line=LastLine, offset=LastOffset, 
                   length=LastLength} = last_not_eof(Attribute),
            PosLength = LastOffset - Offset + LastLength,
            {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                       name=Name, args=[]}, [], [], []}
    end;
cac(other, [#token{value=Name, line=Line, offset=Offset, length=Length} | _], 
    _Exports, _Imports) ->
    {#other{pos={{Line, Line, Offset}, Length}, name=Name}, [], [], []};
cac(_, _D, _E, _I) ->
    {eof, [], [], []}.

get_type_attribute(Kind, Name0, Offset, Line, Attribute, Args) ->
    ?D(Name0),
    Name = case Kind of 'spec' -> Kind; _ -> Name0 end,
    #token{line=LastLine, offset=LastOffset,
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    ?D(Args),
    Extra = to_string(Args),
    ?D(Extra),
    {AttrArgs, _, _} = get_attribute_args(Kind, Args, Args),
    ?D({AttrArgs, Extra}),
    ExternalRefs = get_refs(tl(AttrArgs), Extra, ?ARI_TYPESPEC),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     [#ref{data=#type_def{type=Name}, offset=Offset, length=PosLength, function=Name, 
           arity=?ARI_TYPESPEC, clause="", sub_clause=false} | ExternalRefs], [], []}.

get_other_attribute(Name, Offset, Line, Attribute, Args) ->
    #token{line=LastLine, offset=LastOffset, 
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    Between = erlide_np_util:get_between_outer_pars(Attribute, '(', ')'),
    Extra = to_string(Between),
    {AttrArgs, Exports, Imports} = get_attribute_args(Name, Between, Args),
    ?D({AttrArgs, Between}),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     make_attribute_ref(Name, AttrArgs, Extra, Offset, PosLength)++
         make_attribute_arg_refs(Name, AttrArgs, Between),
     Exports, Imports}.

get_exported(F_A, Exports) ->
    lists:member(F_A, Exports).

get_attribute_args(import, Between, _Args) ->
    From = get_first_of_kind(atom, Between),
    Tokens = get_between(Between, '[', ']'),
    Import = {From, fun_arity_from_tokens(Tokens)},
    {Import, [], [Import]};
get_attribute_args(export, Between, _Args) ->
    Tokens = get_between(Between, '[', ']'),
    Exports = fun_arity_from_tokens(Tokens),
    {Exports, Exports, []};
get_attribute_args(record, Between, _Args) ->
    [RecordToken | _] = Between,
    RecordName = RecordToken#token.value,
    Tokens = erlide_np_util:get_between_outer_pars(Between, '{', '}'),
    R = {RecordName, field_list_from_tokens(Tokens)},
    {R, [], []};
get_attribute_args(_, _Between, Args) ->
    {Args, [], []}.

check_class([#token{kind = AtomOrMacro}, #token{kind = '('} | _])
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    function;
check_class([#token{kind = '-'}, #token{kind = atom} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'spec'} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'type'} | _]) ->
    attribute;
check_class(_X) ->
    %?D(lists:sublist(_X, 5)),
    other.

get_first_of_kind(Kind, Tokens) ->
    case erlide_np_util:skip_to(Tokens, Kind) of
        [#token{kind=Kind, value=Value} | _] ->
            Value;
        _ ->
            undefined
    end.
        
fun_arity_from_tokens([#token{kind=atom, value=Fun}, #token{kind='/'}, 
                       #token{kind=integer, value=Arity} | Rest]) ->
    [{Fun, Arity} | fun_arity_from_tokens(Rest)];
fun_arity_from_tokens([_ | Rest]) ->
    fun_arity_from_tokens(Rest);
fun_arity_from_tokens(_) ->
    [].

field_list_from_tokens([#token{kind=atom, value=Field, line=Line, 
                               offset=Offset, length=Length} | Rest]) ->
    {Extra, NewRest} = get_upto2(Rest, ','),
    [{Field, {{Line, Line, Offset}, Length}, to_string(Extra)}
         | field_list_from_tokens(NewRest)];
field_list_from_tokens([_ | Rest]) ->
    field_list_from_tokens(Rest);
field_list_from_tokens(_) ->
    [].

to_string(Tokens) ->
    S = erlide_scanner:tokens_to_string(Tokens),
    erlide_text:strip(S).
%%     unspacify(S).

get_head(T) ->
    A = get_args(T),
    case get_guards(T) of
        "" ->
            A;
        G ->
            A ++ " when " ++ G
    end.

get_args(T) ->
    case erlide_np_util:get_between_outer_pars(T, '(', ')') of
        [] ->
            "";
        P ->
            "("++to_string(P)++")"
    end.

get_guards([]) ->
    "";
get_guards(T) ->
    to_string(get_between(T, 'when', '->')). 

last_not_eof(L) -> 
    case lists:reverse(L) of
        [eof, Last | _] ->
            Last;
        [Last | _] ->
            Last;
        _ ->
            error
    end.    


get_between(L, A, B) ->
    R = get_upto(L, B),
    lists:reverse(get_upto(lists:reverse(R), A)).

get_upto(L, Delim) ->
        get_upto(L, Delim, []).

get_upto([], _Delim, _Acc) ->
    [];
get_upto([#token{kind=Delim} | _], Delim, Acc) ->
        lists:reverse(Acc);
get_upto([T | Rest], Delim, Acc) ->
    get_upto(Rest, Delim, [T | Acc]).

get_upto2(L, Delim) ->
    get_upto2(L, Delim, []).

get_upto2([], _Delim, Acc) ->
    {lists:reverse(Acc), []};
get_upto2([#token{kind=Delim} | Rest], Delim, Acc) ->
    {lists:reverse(Acc), Rest};
get_upto2([T | Rest], Delim, Acc) ->
    get_upto2(Rest, Delim, [T | Acc]).

check_clause([#token{kind = ';'} | Rest], false) ->
    check_class(Rest) == function;
check_clause(_, _) ->
    false.

split_clauses(F) ->
    split_clauses(F, false, [], []).

split_clauses([], _HaveWhen, Acc, []) ->
    erlide_util:reverse2(Acc);
split_clauses([], HaveWhen, Acc, ClAcc) ->
    split_clauses([], HaveWhen, [ClAcc | Acc], []);
split_clauses([#token{kind=Kind}=T | Rest], _HaveWhen, Acc, ClAcc) 
  when Kind=:='end'; Kind=:=dot; Kind=:='->'; Kind=:='when' ->
    split_clauses(Rest, Kind=:='when', Acc, [T | ClAcc]);
split_clauses([T | Rest] = Tokens, HaveWhen, Acc, ClAcc) ->
    case check_clause(Tokens, HaveWhen) of
        false ->
            split_clauses(Rest, HaveWhen, Acc, [T | ClAcc]);
        true ->
            split_clauses(Rest, HaveWhen, [[T | ClAcc] | Acc], [])
    end.

%% fix_clause([#token{kind=atom, value=Name, line=Line, offset=Offset, length=Length} | Rest] = Code) ->
fix_clauses(Clauses) ->
    fix_clauses(Clauses, []).

fix_clauses([], Acc) ->
    lists:reverse(Acc);
fix_clauses([C | Rest], Acc) ->
    {Clause, Refs} = fix_clause(C),
    fix_clauses(Rest, [{Clause, Refs} | Acc]).

fix_clause([#token{kind=AtomOrMacro, value=Name, line=Line, offset=Offset, length=Length} | Rest]) 
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    #token{line=LastLine, offset=LastOffset, length=LastLength} = last_not_eof(Rest),
    PosLength = LastOffset - Offset + LastLength+1,
    ExternalRefs = get_refs_in_code(Rest),
    {#clause{pos={{Line, LastLine, Offset}, PosLength}, name_pos={{Line, Offset}, Length},
             name=Name, args=get_function_args(Rest), head=get_head(Rest)},
     ExternalRefs}.

get_function_args(Tokens) ->
    P = erlide_np_util:get_between_outer_pars(Tokens, '(', ')'),
    L = erlide_text:split_comma_list(P),
    [to_string(A) || A <- L].

fix_refs(ClausesAndRefs, Function, Imports) ->
    fix_refs(ClausesAndRefs, Function, Imports, []).

fix_refs([], #function{name=Name, arity=Arity, name_pos={{_, Offset}, Length}}, _Imports, Acc) ->
    Ref = #ref{data=#function_def{function=Name, arity=Arity}, offset=Offset, 
               length=Length, function=Name, arity=Arity, clause="", sub_clause=false},
    [Ref | Acc];
fix_refs([{Clause, Refs} | Rest], Function, Imports, Acc0) ->
    Acc1 = fix_refs(Refs, Clause, Function, Imports, Acc0),
    fix_refs(Rest, Function, Imports, Acc1).

fix_refs([], _Clause, _Function, _Imports, Acc) ->
    Acc;
fix_refs([{Offset, Length, RefData} | Rest], #clause{head=Head}=Clause, 
         #function{name=Name, arity=Arity, clauses=Clauses}=Function, 
         Imports, Acc) ->
    SubClause = case Clauses of
                    [] -> false;
                    [_] -> false;
                    _ -> true
                end,
    NewRefData = fix_imported_ref(RefData, Imports),
    Ref = #ref{data=NewRefData, offset=Offset, length=Length, function=Name, 
               arity=Arity, clause=Head, sub_clause=SubClause},
    fix_refs(Rest, Clause, Function, Imports, [Ref | Acc]).

fix_imported_ref(#local_call{function=F, arity=A}=RefData, [{Module, Imports} | Rest]) ->
    case lists:member({F, A}, Imports) of
        true -> #external_call{module=Module, function=F, arity=A};
        false -> fix_imported_ref(RefData, Rest)
    end;
fix_imported_ref(RefData, _Imports) ->
    RefData.

%% find references in code
%% VERY simple, M:F(_ ...), F(_ ...), #R, ?M, and that's it
%% returns {Offset, Length, Ref}, which is made into proper refs
%% by fix_refs
get_refs_in_code(L) ->
    get_refs_in_code(L, []).

get_refs_in_code([], Acc) ->
    lists:reverse(Acc);
get_refs_in_code([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
                  #token{kind=atom, value=F, offset=Offset2, length=Length2},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length2+Offset2-Offset, 
         #external_call{module = M, function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=atom, value=F, offset=Offset, length=Length},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length, #local_call{function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest], 
                 Acc) ->
    R = {Offset, Length, #macro_ref{macro=M}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind='#', offset=Offset}, 
                  #token{kind=atom, value=Record, offset=Offset2, length=Length2} | Rest],
                 Acc) ->
    R = {Offset, Length2+Offset2-Offset, #record_ref{record=Record}},
    {NewRest, Fields, RightSides} = erlide_np_records:check_fields(Rest, Record),
    NewAcc = get_refs_in_code(RightSides, Acc),
    get_refs_in_code(NewRest, Fields ++ [R | NewAcc]);
get_refs_in_code([#token{kind=var, value=V, offset=Offset, length=Length} | Rest],
                 Acc) ->
    R = {Offset, Length, make_var_def_ref(Acc, V)},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([_ | Rest], Acc) ->
    get_refs_in_code(Rest, Acc).

make_var_def_ref([], Var) ->
	#var_def{variable=Var};
make_var_def_ref([{_, _, #var_def{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([{_, _, #var_ref{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([_ | Rest], Var) ->
	make_var_def_ref(Rest, Var).

%% find type references in spec, type and opaque
%% also VERY simple, type(), ?M, #R, only
%% returns proper refs
get_refs(Tokens, Name, Arity) ->
	get_refs(Tokens, Name, Arity, []).

get_refs([], _, _, Acc) ->
    lists:reverse(Acc);
get_refs([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #macro_ref{macro=M}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind='#', offset=Offset},
          #token{kind=atom, value=R, offset=Offset2, length=Length2} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset, #record_ref{record=R}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
		  #token{kind=atom, value=T, offset=Offset2, length=Length2},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset,
                   #type_ref{module=M, type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=T, offset=Offset, length=Length},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #type_ref{module='_', type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=variable, value=V, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #var_ref{variable=V}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([_ | Rest], Name, Arity, Acc) ->
    get_refs(Rest, Name, Arity, Acc).

get_record_field_defs([], _) ->
    [];
get_record_field_defs([{FieldName, {{_, _, Offset}, Length}, _Extra} | Rest],
                      RecordName) ->
    [make_ref(Offset, Length, #record_field_def{record=RecordName, field=FieldName}, 
              FieldName, ?ARI_RECORD_FIELD_DEF)
         | get_record_field_defs(Rest, RecordName)].

make_ref(Offset, Length, Data, Name, Arity) ->
    #ref{data=Data, offset=Offset, length=Length, function=Name, arity=Arity, 
         clause="", sub_clause=false}.

make_attribute_arg_refs(define, Name, [#token{}, #token{kind='('} | Rest]) ->
    get_refs(skip_to_rparen(Rest), Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(define, Name, [#token{}, #token{kind=','} | Rest]) ->
    get_refs(Rest, Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(record, {Name, Fields}, [#token{}, #token{kind=','} | Rest]) ->
    get_record_field_defs(Fields, Name) ++ get_refs(Rest, Name, ?ARI_RECORD_DEF);
make_attribute_arg_refs(_, _, _) ->
    [].

skip_to_rparen([]) ->
    [];
skip_to_rparen([#token{kind=')'} | Rest]) ->
    Rest;
skip_to_rparen([_ | Rest]) ->
    skip_to_rparen(Rest).

make_attribute_ref(Name, Between, Extra, Offset, Length) ->
    ?D({Name, Between, Offset, Length}),
    case make_attribute_ref(Name, Between, Extra) of
        [] ->
            [];
        {Arity, AName, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=AName, 
                  arity=Arity, clause="", sub_clause=false}];
        {Arity, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=Name, 
                  arity=Arity, clause="", sub_clause=false}]
    end.

make_attribute_ref(module, _, Extra) ->
    {?ARI_ATTRIBUTE, #module_def{module=Extra}};
make_attribute_ref(record, Between, _E) ->
    ?D({Between, _E}),
    Name = case Between of
               {N, _} -> N;
               N -> N
           end,
    R= #record_def{record=Name},
    ?D(R),
    {?ARI_RECORD_DEF, Name, R};
make_attribute_ref(define, [Name | _], _) when is_list(Name) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(define, Name, _) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(include, _, Extra) ->
    {?ARI_INCLUDE, #include_ref{filename=unquote(Extra)}};
make_attribute_ref(_, _, _) ->
    [].

unquote(L) ->
    lists:reverse(unquote_first(lists:reverse(unquote_first(L)))).

unquote_first([$" | Rest]) ->
    Rest;
unquote_first(L) ->
    L.

remove_rest([], _) -> [];
remove_rest(Rest, Rest) -> [];
remove_rest([Hd | Tl], Rest) -> [Hd | remove_rest(Tl, Rest)].
%% Description: Split source files into functions and clauses and collects references
%% Author: jakob
%% Created: Mar 23, 2006

-module(erlide_noparse).

%%
%% Exported Functions
%%

%% called from Java
-export([initial_parse/5, reparse/1, remove_cache_files/2]).

%% called from Erlang
-export([read_module_refs/3]).

%%
%% Include files
%%

%% -define(DEBUG, 1).
%% -define(IO_FORMAT_DEBUG, 1).

-define(CACHE_VERSION, 25).
-define(SERVER, erlide_noparse).

-include("erlide.hrl").
-include("erlide_noparse.hrl").
-include("erlide_scanner.hrl").
-include("erlide_search_server.hrl").

%% special arity flags
-define(ARI_TYPESPEC, -2).
-define(ARI_ATTRIBUTE, -3).
-define(ARI_RECORD_DEF, -4).
-define(ARI_MACRO_DEF, -5).
-define(ARI_INCLUDE, -6).
-define(ARI_RECORD_FIELD_DEF, -7).

-define(X(X), erlide_log:log({X, erlang:now()})).


%%
%% API Functions
%%

initial_parse(ScannerName, ModuleFileName, StateDir, UseCache,
              UpdateSearchServer) ->
    try
        %%         ?D({StateDir, ModuleFileName}),
        BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
        RefsFileName = BaseName ++ ".refs",
        RenewFun = fun(_F) ->
                           do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer)
                   end,
        CacheFileName = BaseName ++ ".noparse",
        ?D(CacheFileName),
        {Cached, Res} = erlide_util:check_and_renew_cached(
                          ModuleFileName, CacheFileName, ?CACHE_VERSION, 
                          RenewFun, UseCache),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        ?D(updated),
        {ok, Res, Cached}
    catch
        error:Reason ->
            {error, Reason}
    end.

reparse(ScannerName) ->
    try
        Res = do_parse(ScannerName, "", "", true),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        {ok, Res, unused}
    catch
        error:Reason ->
            {error, Reason}
    end.

read_module_refs(ScannerName, ModulePath, StateDir) ->
    ?D(ScannerName),
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    RefsFileName = BaseName ++ ".refs",
    ?D(RefsFileName),
    case file:read_file(RefsFileName) of
        {ok, Binary} ->
            ?D(byte_size(Binary)),
            binary_to_term(Binary);
        _ ->
            ?D(ModulePath),
            {ok, InitialTextBin} = file:read_file(ModulePath),
            ?D(byte_size(InitialTextBin)),
            InitialText = binary_to_list(InitialTextBin),
            _D = erlide_scanner_server:initialScan(
                   ScannerName, ModulePath, InitialText, StateDir, true),
            ?D(_D),
            initial_parse(ScannerName, ModulePath, StateDir, true, false),
            ?D(RefsFileName),
            {ok, Binary} = file:read_file(RefsFileName),
            ?D(byte_size(Binary)),
            binary_to_term(Binary)
    end.

remove_cache_files(ScannerName, StateDir) ->
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    ScannerCacheFileName = BaseName ++ ".scan",
    file:delete(ScannerCacheFileName),
    RefsFileName = BaseName ++ ".refs",
    file:delete(RefsFileName),
    CacheFileName = BaseName ++ ".noparse",
    file:delete(CacheFileName).

%%
%% Internal functions
%%

do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer) ->
    Toks = erlide_scanner_server:getTokens(ScannerName),
    do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer).

do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer) ->
    ?D({do_parse, ScannerName, length(Toks)}),
    ?X(111),
    {UncommentToks, Comments} = erlide_np_util:extract_comments(Toks),
    ?X(222),
    ?D({length(UncommentToks), length(Comments)}),
    ?D({UncommentToks}),
    Functions = erlide_np_util:split_after_dots(UncommentToks),
    ?X(333),
    ?D(length(Functions)),
    AutoImports = erlide_util:add_auto_imported([]),
    ?X(444),
%%     ?D(AutoImports),
    {Collected, Refs} = classify_and_collect(Functions, [], [], [], AutoImports),
    ?X(555),
    ?D({'>>',length(Collected)}),
    CommentedCollected = erlide_np_util:get_function_comments(Collected, Comments),
    ?X(666),
    ?D(CommentedCollected),
    Model = #model{forms=CommentedCollected, comments=Comments},
    %%erlide_noparse_server:create(ScannerName, Model),
%%     ?D({"Model", length(Model#model.forms), erts_debug:flat_size(Model)}),
    FixedModel = fixup_model(Model),
    ?X(777),
%%     ?D(erts_debug:flat_size(FixedModel)),
    %% 	?D([erts_debug:flat_size(F) || F <- element(2, FixedModel)]),
    ?D(StateDir),
    case StateDir of
        "" -> ok;
        _ ->
            ?D({RefsFileName, length(Refs)}),
            file:write_file(RefsFileName, term_to_binary(Refs, [compressed]))
    end,
    update_search_server(UpdateSearchServer, ScannerName, Refs),
%%     ?D(FixedModel),
    ?X(888),
    FixedModel.

update_search_server(true, ScannerName, Refs) ->
    erlide_search_server:add_module_refs(ScannerName, Refs);
update_search_server(_, _, _) ->
    ok.

fixup_model(#model{forms=Forms, comments=Comments}) ->
    FixedComments = fixup_tokens(Comments),
    FixedForms = fixup_forms(Forms),
    #model{forms=FixedForms, comments=FixedComments}.

fixup_forms(Forms) ->
    [fixup_form(Form) || Form <- Forms].

fixup_tokens(Tokens) ->
    [fixup_token(Token) || Token <- Tokens].

fixup_token(#token{value=Value} = Token) when is_list(Value) ->
    Token#token{value=to_binary(Value)};
fixup_token(#token{text=Text} = Token) when is_list(Text) ->
    Token#token{value=to_binary(Text)};
fixup_token(Token) ->
    Token.

binary_args(Args) when is_list(Args) ->
    [iolist_to_binary(A) || A <- Args];
binary_args(_) ->
    [].

fixup_form(#function{comment=Comment, clauses=Clauses, args=Args} = Function) ->
    Function#function{comment= to_binary(Comment), clauses=fixup_forms(Clauses), args=binary_args(Args)};
fixup_form(#clause{head=Head, args=Args} = Clause) ->
    Clause#clause{head=to_binary(Head), args=binary_args(Args)};
fixup_form(Other) ->
    Other.

to_binary(Comment) when is_list(Comment) ->
    try 
        iolist_to_binary(Comment) 
    catch 
        _:_ -> 
            unicode:characters_to_binary(Comment) 
    end;
to_binary(Other) ->
    Other.

%% parse_test(ScannerName, File) ->
%%     erlide_scanner_server:scan_uncached(ScannerName, File, ""),
%%     Toks = erlide_scanner_server:getTokens(ScannerName),
%%     {UncommentToks, Comments} = extract_comments(Toks),
%%     Functions = split_after_dots(UncommentToks, [], []),
%%     {Collected, Refs} = classify_and_collect(Functions, [], []),
%%     _Model = #model{forms=Collected, comments=Comments},
%%     %%erlide_noparse_server:create(ScannerName, Model, ""),
%%     ok.

classify_and_collect([], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([eof], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([C | Rest], Acc, RefsAcc, Exports, Imports) ->
    {R, Refs, MoreExports, MoreImports} = cac(check_class(C), C, Exports, Imports),
    classify_and_collect(Rest, [R | Acc], Refs++RefsAcc, MoreExports++Exports, MoreImports++Imports).

cac(function, Tokens, Exports, Imports) ->
    ClauseList = split_clauses(Tokens),
    %?D(ClauseList),
    ClausesAndRefs = fix_clauses(ClauseList),
    %?D(length(Clauses)),
    [{#clause{pos=P, name=N, args=A, head=H, name_pos=NP}, _Refs} | _] = ClausesAndRefs,
    ?D(A),
    Arity = length(A),
    Exported = get_exported({N, Arity}, Exports),
    Function = case ClausesAndRefs of  	% only show subclauses when more than one
                   [_] ->
                       #function{pos=P, name=N, arity=Arity, args=A, head=H, 
                                 clauses=[], name_pos=NP, exported=Exported};
                   _ ->
                       Clauses = [C || {C, _} <- ClausesAndRefs],
                       #function{pos=P, name=N, arity=Arity, clauses=Clauses, 
                                 name_pos=NP, exported=Exported}
               end,
    {Function, fix_refs(ClausesAndRefs, Function, Imports), [], []};
cac(attribute, Attribute, _Exports, _Imports) ->
    ?D(Attribute),
    case Attribute of
        %% -spec, -type or -opaque
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=Kind, line=_Line, offset=_Offset, value=Name} | Args]
          when (Kind=:='spec') or ((Kind=:=atom) and (Name=:='type'))
                   or ((Kind=:=atom) and (Name=:='opaque'))->
            get_type_attribute(Kind, Name, Offset, Line, Attribute, Args);
        %% other attributes
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=atom, value=Name, line=_Line, offset=_Offset},
         _, #token{value=Args} | _] = Attribute ->
            get_other_attribute(Name, Offset, Line, Attribute, Args);
        [_, #token{kind=atom, value=Name, line=Line, offset=Offset} | _] ->
            #token{line=LastLine, offset=LastOffset, 
                   length=LastLength} = last_not_eof(Attribute),
            PosLength = LastOffset - Offset + LastLength,
            {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                       name=Name, args=[]}, [], [], []}
    end;
cac(other, [#token{value=Name, line=Line, offset=Offset, length=Length} | _], 
    _Exports, _Imports) ->
    {#other{pos={{Line, Line, Offset}, Length}, name=Name}, [], [], []};
cac(_, _D, _E, _I) ->
    {eof, [], [], []}.

get_type_attribute(Kind, Name0, Offset, Line, Attribute, Args) ->
    ?D(Name0),
    Name = case Kind of 'spec' -> Kind; _ -> Name0 end,
    #token{line=LastLine, offset=LastOffset,
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    ?D(Args),
    Extra = to_string(Args),
    ?D(Extra),
    {AttrArgs, _, _} = get_attribute_args(Kind, Args, Args),
    ?D({AttrArgs, Extra}),
    ExternalRefs = get_refs(tl(AttrArgs), Extra, ?ARI_TYPESPEC),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     [#ref{data=#type_def{type=Name}, offset=Offset, length=PosLength, function=Name, 
           arity=?ARI_TYPESPEC, clause="", sub_clause=false} | ExternalRefs], [], []}.

get_other_attribute(Name, Offset, Line, Attribute, Args) ->
    #token{line=LastLine, offset=LastOffset, 
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    Between = erlide_np_util:get_between_outer_pars(Attribute, '(', ')'),
    Extra = to_string(Between),
    {AttrArgs, Exports, Imports} = get_attribute_args(Name, Between, Args),
    ?D({AttrArgs, Between}),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     make_attribute_ref(Name, AttrArgs, Extra, Offset, PosLength)++
         make_attribute_arg_refs(Name, AttrArgs, Between),
     Exports, Imports}.

get_exported(F_A, Exports) ->
    lists:member(F_A, Exports).

get_attribute_args(import, Between, _Args) ->
    From = get_first_of_kind(atom, Between),
    Tokens = get_between(Between, '[', ']'),
    Import = {From, fun_arity_from_tokens(Tokens)},
    {Import, [], [Import]};
get_attribute_args(export, Between, _Args) ->
    Tokens = get_between(Between, '[', ']'),
    Exports = fun_arity_from_tokens(Tokens),
    {Exports, Exports, []};
get_attribute_args(record, Between, _Args) ->
    [RecordToken | _] = Between,
    RecordName = RecordToken#token.value,
    Tokens = erlide_np_util:get_between_outer_pars(Between, '{', '}'),
    R = {RecordName, field_list_from_tokens(Tokens)},
    {R, [], []};
get_attribute_args(_, _Between, Args) ->
    {Args, [], []}.

check_class([#token{kind = AtomOrMacro}, #token{kind = '('} | _])
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    function;
check_class([#token{kind = '-'}, #token{kind = atom} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'spec'} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'type'} | _]) ->
    attribute;
check_class(_X) ->
    %?D(lists:sublist(_X, 5)),
    other.

get_first_of_kind(Kind, Tokens) ->
    case erlide_np_util:skip_to(Tokens, Kind) of
        [#token{kind=Kind, value=Value} | _] ->
            Value;
        _ ->
            undefined
    end.
        
fun_arity_from_tokens([#token{kind=atom, value=Fun}, #token{kind='/'}, 
                       #token{kind=integer, value=Arity} | Rest]) ->
    [{Fun, Arity} | fun_arity_from_tokens(Rest)];
fun_arity_from_tokens([_ | Rest]) ->
    fun_arity_from_tokens(Rest);
fun_arity_from_tokens(_) ->
    [].

field_list_from_tokens([#token{kind=atom, value=Field, line=Line, 
                               offset=Offset, length=Length} | Rest]) ->
    {Extra, NewRest} = get_upto2(Rest, ','),
    [{Field, {{Line, Line, Offset}, Length}, to_string(Extra)}
         | field_list_from_tokens(NewRest)];
field_list_from_tokens([_ | Rest]) ->
    field_list_from_tokens(Rest);
field_list_from_tokens(_) ->
    [].

to_string(Tokens) ->
    S = erlide_scanner:tokens_to_string(Tokens),
    erlide_text:strip(S).
%%     unspacify(S).

get_head(T) ->
    A = get_args(T),
    case get_guards(T) of
        "" ->
            A;
        G ->
            A ++ " when " ++ G
    end.

get_args(T) ->
    case erlide_np_util:get_between_outer_pars(T, '(', ')') of
        [] ->
            "";
        P ->
            "("++to_string(P)++")"
    end.

get_guards([]) ->
    "";
get_guards(T) ->
    to_string(get_between(T, 'when', '->')). 

last_not_eof(L) -> 
    case lists:reverse(L) of
        [eof, Last | _] ->
            Last;
        [Last | _] ->
            Last;
        _ ->
            error
    end.    


get_between(L, A, B) ->
    R = get_upto(L, B),
    lists:reverse(get_upto(lists:reverse(R), A)).

get_upto(L, Delim) ->
        get_upto(L, Delim, []).

get_upto([], _Delim, _Acc) ->
    [];
get_upto([#token{kind=Delim} | _], Delim, Acc) ->
        lists:reverse(Acc);
get_upto([T | Rest], Delim, Acc) ->
    get_upto(Rest, Delim, [T | Acc]).

get_upto2(L, Delim) ->
    get_upto2(L, Delim, []).

get_upto2([], _Delim, Acc) ->
    {lists:reverse(Acc), []};
get_upto2([#token{kind=Delim} | Rest], Delim, Acc) ->
    {lists:reverse(Acc), Rest};
get_upto2([T | Rest], Delim, Acc) ->
    get_upto2(Rest, Delim, [T | Acc]).

check_clause([#token{kind = ';'} | Rest], false) ->
    check_class(Rest) == function;
check_clause(_, _) ->
    false.

split_clauses(F) ->
    split_clauses(F, false, [], []).

split_clauses([], _HaveWhen, Acc, []) ->
    erlide_util:reverse2(Acc);
split_clauses([], HaveWhen, Acc, ClAcc) ->
    split_clauses([], HaveWhen, [ClAcc | Acc], []);
split_clauses([#token{kind=Kind}=T | Rest], _HaveWhen, Acc, ClAcc) 
  when Kind=:='end'; Kind=:=dot; Kind=:='->'; Kind=:='when' ->
    split_clauses(Rest, Kind=:='when', Acc, [T | ClAcc]);
split_clauses([T | Rest] = Tokens, HaveWhen, Acc, ClAcc) ->
    case check_clause(Tokens, HaveWhen) of
        false ->
            split_clauses(Rest, HaveWhen, Acc, [T | ClAcc]);
        true ->
            split_clauses(Rest, HaveWhen, [[T | ClAcc] | Acc], [])
    end.

%% fix_clause([#token{kind=atom, value=Name, line=Line, offset=Offset, length=Length} | Rest] = Code) ->
fix_clauses(Clauses) ->
    fix_clauses(Clauses, []).

fix_clauses([], Acc) ->
    lists:reverse(Acc);
fix_clauses([C | Rest], Acc) ->
    {Clause, Refs} = fix_clause(C),
    fix_clauses(Rest, [{Clause, Refs} | Acc]).

fix_clause([#token{kind=AtomOrMacro, value=Name, line=Line, offset=Offset, length=Length} | Rest]) 
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    #token{line=LastLine, offset=LastOffset, length=LastLength} = last_not_eof(Rest),
    PosLength = LastOffset - Offset + LastLength+1,
    ExternalRefs = get_refs_in_code(Rest),
    {#clause{pos={{Line, LastLine, Offset}, PosLength}, name_pos={{Line, Offset}, Length},
             name=Name, args=get_function_args(Rest), head=get_head(Rest)},
     ExternalRefs}.

get_function_args(Tokens) ->
    P = erlide_np_util:get_between_outer_pars(Tokens, '(', ')'),
    L = erlide_text:split_comma_list(P),
    [to_string(A) || A <- L].

fix_refs(ClausesAndRefs, Function, Imports) ->
    fix_refs(ClausesAndRefs, Function, Imports, []).

fix_refs([], #function{name=Name, arity=Arity, name_pos={{_, Offset}, Length}}, _Imports, Acc) ->
    Ref = #ref{data=#function_def{function=Name, arity=Arity}, offset=Offset, 
               length=Length, function=Name, arity=Arity, clause="", sub_clause=false},
    [Ref | Acc];
fix_refs([{Clause, Refs} | Rest], Function, Imports, Acc0) ->
    Acc1 = fix_refs(Refs, Clause, Function, Imports, Acc0),
    fix_refs(Rest, Function, Imports, Acc1).

fix_refs([], _Clause, _Function, _Imports, Acc) ->
    Acc;
fix_refs([{Offset, Length, RefData} | Rest], #clause{head=Head}=Clause, 
         #function{name=Name, arity=Arity, clauses=Clauses}=Function, 
         Imports, Acc) ->
    SubClause = case Clauses of
                    [] -> false;
                    [_] -> false;
                    _ -> true
                end,
    NewRefData = fix_imported_ref(RefData, Imports),
    Ref = #ref{data=NewRefData, offset=Offset, length=Length, function=Name, 
               arity=Arity, clause=Head, sub_clause=SubClause},
    fix_refs(Rest, Clause, Function, Imports, [Ref | Acc]).

fix_imported_ref(#local_call{function=F, arity=A}=RefData, [{Module, Imports} | Rest]) ->
    case lists:member({F, A}, Imports) of
        true -> #external_call{module=Module, function=F, arity=A};
        false -> fix_imported_ref(RefData, Rest)
    end;
fix_imported_ref(RefData, _Imports) ->
    RefData.

%% find references in code
%% VERY simple, M:F(_ ...), F(_ ...), #R, ?M, and that's it
%% returns {Offset, Length, Ref}, which is made into proper refs
%% by fix_refs
get_refs_in_code(L) ->
    get_refs_in_code(L, []).

get_refs_in_code([], Acc) ->
    lists:reverse(Acc);
get_refs_in_code([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
                  #token{kind=atom, value=F, offset=Offset2, length=Length2},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length2+Offset2-Offset, 
         #external_call{module = M, function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=atom, value=F, offset=Offset, length=Length},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length, #local_call{function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest], 
                 Acc) ->
    R = {Offset, Length, #macro_ref{macro=M}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind='#', offset=Offset}, 
                  #token{kind=atom, value=Record, offset=Offset2, length=Length2} | Rest],
                 Acc) ->
    R = {Offset, Length2+Offset2-Offset, #record_ref{record=Record}},
    {NewRest, Fields, RightSides} = erlide_np_records:check_fields(Rest, Record),
    NewAcc = get_refs_in_code(RightSides, Acc),
    get_refs_in_code(NewRest, Fields ++ [R | NewAcc]);
get_refs_in_code([#token{kind=var, value=V, offset=Offset, length=Length} | Rest],
                 Acc) ->
    R = {Offset, Length, make_var_def_ref(Acc, V)},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([_ | Rest], Acc) ->
    get_refs_in_code(Rest, Acc).

make_var_def_ref([], Var) ->
	#var_def{variable=Var};
make_var_def_ref([{_, _, #var_def{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([{_, _, #var_ref{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([_ | Rest], Var) ->
	make_var_def_ref(Rest, Var).

%% find type references in spec, type and opaque
%% also VERY simple, type(), ?M, #R, only
%% returns proper refs
get_refs(Tokens, Name, Arity) ->
	get_refs(Tokens, Name, Arity, []).

get_refs([], _, _, Acc) ->
    lists:reverse(Acc);
get_refs([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #macro_ref{macro=M}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind='#', offset=Offset},
          #token{kind=atom, value=R, offset=Offset2, length=Length2} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset, #record_ref{record=R}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
		  #token{kind=atom, value=T, offset=Offset2, length=Length2},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset,
                   #type_ref{module=M, type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=T, offset=Offset, length=Length},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #type_ref{module='_', type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=variable, value=V, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #var_ref{variable=V}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([_ | Rest], Name, Arity, Acc) ->
    get_refs(Rest, Name, Arity, Acc).

get_record_field_defs([], _) ->
    [];
get_record_field_defs([{FieldName, {{_, _, Offset}, Length}, _Extra} | Rest],
                      RecordName) ->
    [make_ref(Offset, Length, #record_field_def{record=RecordName, field=FieldName}, 
              FieldName, ?ARI_RECORD_FIELD_DEF)
         | get_record_field_defs(Rest, RecordName)].

make_ref(Offset, Length, Data, Name, Arity) ->
    #ref{data=Data, offset=Offset, length=Length, function=Name, arity=Arity, 
         clause="", sub_clause=false}.

make_attribute_arg_refs(define, Name, [#token{}, #token{kind='('} | Rest]) ->
    get_refs(skip_to_rparen(Rest), Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(define, Name, [#token{}, #token{kind=','} | Rest]) ->
    get_refs(Rest, Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(record, {Name, Fields}, [#token{}, #token{kind=','} | Rest]) ->
    get_record_field_defs(Fields, Name) ++ get_refs(Rest, Name, ?ARI_RECORD_DEF);
make_attribute_arg_refs(_, _, _) ->
    [].

skip_to_rparen([]) ->
    [];
skip_to_rparen([#token{kind=')'} | Rest]) ->
    Rest;
skip_to_rparen([_ | Rest]) ->
    skip_to_rparen(Rest).

make_attribute_ref(Name, Between, Extra, Offset, Length) ->
    ?D({Name, Between, Offset, Length}),
    case make_attribute_ref(Name, Between, Extra) of
        [] ->
            [];
        {Arity, AName, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=AName, 
                  arity=Arity, clause="", sub_clause=false}];
        {Arity, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=Name, 
                  arity=Arity, clause="", sub_clause=false}]
    end.

make_attribute_ref(module, _, Extra) ->
    {?ARI_ATTRIBUTE, #module_def{module=Extra}};
make_attribute_ref(record, Between, _E) ->
    ?D({Between, _E}),
    Name = case Between of
               {N, _} -> N;
               N -> N
           end,
    R= #record_def{record=Name},
    ?D(R),
    {?ARI_RECORD_DEF, Name, R};
make_attribute_ref(define, [Name | _], _) when is_list(Name) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(define, Name, _) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(include, _, Extra) ->
    {?ARI_INCLUDE, #include_ref{filename=unquote(Extra)}};
make_attribute_ref(_, _, _) ->
    [].

unquote(L) ->
    lists:reverse(unquote_first(lists:reverse(unquote_first(L)))).

unquote_first([$" | Rest]) ->
    Rest;
unquote_first(L) ->
    L.

remove_rest([], _) -> [];
remove_rest(Rest, Rest) -> [];
remove_rest([Hd | Tl], Rest) -> [Hd | remove_rest(Tl, Rest)].
%% Description: Split source files into functions and clauses and collects references
%% Author: jakob
%% Created: Mar 23, 2006

-module(erlide_noparse).

%%
%% Exported Functions
%%

%% called from Java
-export([initial_parse/5, reparse/1, remove_cache_files/2]).

%% called from Erlang
-export([read_module_refs/3]).

%%
%% Include files
%%

%% -define(DEBUG, 1).
%% -define(IO_FORMAT_DEBUG, 1).

-define(CACHE_VERSION, 25).
-define(SERVER, erlide_noparse).

-include("erlide.hrl").
-include("erlide_noparse.hrl").
-include("erlide_scanner.hrl").
-include("erlide_search_server.hrl").

%% special arity flags
-define(ARI_TYPESPEC, -2).
-define(ARI_ATTRIBUTE, -3).
-define(ARI_RECORD_DEF, -4).
-define(ARI_MACRO_DEF, -5).
-define(ARI_INCLUDE, -6).
-define(ARI_RECORD_FIELD_DEF, -7).

-define(X(X), erlide_log:log({X, erlang:now()})).


%%
%% API Functions
%%

initial_parse(ScannerName, ModuleFileName, StateDir, UseCache,
              UpdateSearchServer) ->
    try
        %%         ?D({StateDir, ModuleFileName}),
        BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
        RefsFileName = BaseName ++ ".refs",
        RenewFun = fun(_F) ->
                           do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer)
                   end,
        CacheFileName = BaseName ++ ".noparse",
        ?D(CacheFileName),
        {Cached, Res} = erlide_util:check_and_renew_cached(
                          ModuleFileName, CacheFileName, ?CACHE_VERSION, 
                          RenewFun, UseCache),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        ?D(updated),
        {ok, Res, Cached}
    catch
        error:Reason ->
            {error, Reason}
    end.

reparse(ScannerName) ->
    try
        Res = do_parse(ScannerName, "", "", true),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        {ok, Res, unused}
    catch
        error:Reason ->
            {error, Reason}
    end.

read_module_refs(ScannerName, ModulePath, StateDir) ->
    ?D(ScannerName),
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    RefsFileName = BaseName ++ ".refs",
    ?D(RefsFileName),
    case file:read_file(RefsFileName) of
        {ok, Binary} ->
            ?D(byte_size(Binary)),
            binary_to_term(Binary);
        _ ->
            ?D(ModulePath),
            {ok, InitialTextBin} = file:read_file(ModulePath),
            ?D(byte_size(InitialTextBin)),
            InitialText = binary_to_list(InitialTextBin),
            _D = erlide_scanner_server:initialScan(
                   ScannerName, ModulePath, InitialText, StateDir, true),
            ?D(_D),
            initial_parse(ScannerName, ModulePath, StateDir, true, false),
            ?D(RefsFileName),
            {ok, Binary} = file:read_file(RefsFileName),
            ?D(byte_size(Binary)),
            binary_to_term(Binary)
    end.

remove_cache_files(ScannerName, StateDir) ->
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    ScannerCacheFileName = BaseName ++ ".scan",
    file:delete(ScannerCacheFileName),
    RefsFileName = BaseName ++ ".refs",
    file:delete(RefsFileName),
    CacheFileName = BaseName ++ ".noparse",
    file:delete(CacheFileName).

%%
%% Internal functions
%%

do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer) ->
    Toks = erlide_scanner_server:getTokens(ScannerName),
    do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer).

do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer) ->
    ?D({do_parse, ScannerName, length(Toks)}),
    ?X(111),
    {UncommentToks, Comments} = erlide_np_util:extract_comments(Toks),
    ?X(222),
    ?D({length(UncommentToks), length(Comments)}),
    ?D({UncommentToks}),
    Functions = erlide_np_util:split_after_dots(UncommentToks),
    ?X(333),
    ?D(length(Functions)),
    AutoImports = erlide_util:add_auto_imported([]),
    ?X(444),
%%     ?D(AutoImports),
    {Collected, Refs} = classify_and_collect(Functions, [], [], [], AutoImports),
    ?X(555),
    ?D({'>>',length(Collected)}),
    CommentedCollected = erlide_np_util:get_function_comments(Collected, Comments),
    ?X(666),
    ?D(CommentedCollected),
    Model = #model{forms=CommentedCollected, comments=Comments},
    %%erlide_noparse_server:create(ScannerName, Model),
%%     ?D({"Model", length(Model#model.forms), erts_debug:flat_size(Model)}),
    FixedModel = fixup_model(Model),
    ?X(777),
%%     ?D(erts_debug:flat_size(FixedModel)),
    %% 	?D([erts_debug:flat_size(F) || F <- element(2, FixedModel)]),
    ?D(StateDir),
    case StateDir of
        "" -> ok;
        _ ->
            ?D({RefsFileName, length(Refs)}),
            file:write_file(RefsFileName, term_to_binary(Refs, [compressed]))
    end,
    update_search_server(UpdateSearchServer, ScannerName, Refs),
%%     ?D(FixedModel),
    ?X(888),
    FixedModel.

update_search_server(true, ScannerName, Refs) ->
    erlide_search_server:add_module_refs(ScannerName, Refs);
update_search_server(_, _, _) ->
    ok.

fixup_model(#model{forms=Forms, comments=Comments}) ->
    FixedComments = fixup_tokens(Comments),
    FixedForms = fixup_forms(Forms),
    #model{forms=FixedForms, comments=FixedComments}.

fixup_forms(Forms) ->
    [fixup_form(Form) || Form <- Forms].

fixup_tokens(Tokens) ->
    [fixup_token(Token) || Token <- Tokens].

fixup_token(#token{value=Value} = Token) when is_list(Value) ->
    Token#token{value=to_binary(Value)};
fixup_token(#token{text=Text} = Token) when is_list(Text) ->
    Token#token{value=to_binary(Text)};
fixup_token(Token) ->
    Token.

binary_args(Args) when is_list(Args) ->
    [iolist_to_binary(A) || A <- Args];
binary_args(_) ->
    [].

fixup_form(#function{comment=Comment, clauses=Clauses, args=Args} = Function) ->
    Function#function{comment= to_binary(Comment), clauses=fixup_forms(Clauses), args=binary_args(Args)};
fixup_form(#clause{head=Head, args=Args} = Clause) ->
    Clause#clause{head=to_binary(Head), args=binary_args(Args)};
fixup_form(Other) ->
    Other.

to_binary(Comment) when is_list(Comment) ->
    try 
        iolist_to_binary(Comment) 
    catch 
        _:_ -> 
            unicode:characters_to_binary(Comment) 
    end;
to_binary(Other) ->
    Other.

%% parse_test(ScannerName, File) ->
%%     erlide_scanner_server:scan_uncached(ScannerName, File, ""),
%%     Toks = erlide_scanner_server:getTokens(ScannerName),
%%     {UncommentToks, Comments} = extract_comments(Toks),
%%     Functions = split_after_dots(UncommentToks, [], []),
%%     {Collected, Refs} = classify_and_collect(Functions, [], []),
%%     _Model = #model{forms=Collected, comments=Comments},
%%     %%erlide_noparse_server:create(ScannerName, Model, ""),
%%     ok.

classify_and_collect([], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([eof], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([C | Rest], Acc, RefsAcc, Exports, Imports) ->
    {R, Refs, MoreExports, MoreImports} = cac(check_class(C), C, Exports, Imports),
    classify_and_collect(Rest, [R | Acc], Refs++RefsAcc, MoreExports++Exports, MoreImports++Imports).

cac(function, Tokens, Exports, Imports) ->
    ClauseList = split_clauses(Tokens),
    %?D(ClauseList),
    ClausesAndRefs = fix_clauses(ClauseList),
    %?D(length(Clauses)),
    [{#clause{pos=P, name=N, args=A, head=H, name_pos=NP}, _Refs} | _] = ClausesAndRefs,
    ?D(A),
    Arity = length(A),
    Exported = get_exported({N, Arity}, Exports),
    Function = case ClausesAndRefs of  	% only show subclauses when more than one
                   [_] ->
                       #function{pos=P, name=N, arity=Arity, args=A, head=H, 
                                 clauses=[], name_pos=NP, exported=Exported};
                   _ ->
                       Clauses = [C || {C, _} <- ClausesAndRefs],
                       #function{pos=P, name=N, arity=Arity, clauses=Clauses, 
                                 name_pos=NP, exported=Exported}
               end,
    {Function, fix_refs(ClausesAndRefs, Function, Imports), [], []};
cac(attribute, Attribute, _Exports, _Imports) ->
    ?D(Attribute),
    case Attribute of
        %% -spec, -type or -opaque
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=Kind, line=_Line, offset=_Offset, value=Name} | Args]
          when (Kind=:='spec') or ((Kind=:=atom) and (Name=:='type'))
                   or ((Kind=:=atom) and (Name=:='opaque'))->
            get_type_attribute(Kind, Name, Offset, Line, Attribute, Args);
        %% other attributes
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=atom, value=Name, line=_Line, offset=_Offset},
         _, #token{value=Args} | _] = Attribute ->
            get_other_attribute(Name, Offset, Line, Attribute, Args);
        [_, #token{kind=atom, value=Name, line=Line, offset=Offset} | _] ->
            #token{line=LastLine, offset=LastOffset, 
                   length=LastLength} = last_not_eof(Attribute),
            PosLength = LastOffset - Offset + LastLength,
            {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                       name=Name, args=[]}, [], [], []}
    end;
cac(other, [#token{value=Name, line=Line, offset=Offset, length=Length} | _], 
    _Exports, _Imports) ->
    {#other{pos={{Line, Line, Offset}, Length}, name=Name}, [], [], []};
cac(_, _D, _E, _I) ->
    {eof, [], [], []}.

get_type_attribute(Kind, Name0, Offset, Line, Attribute, Args) ->
    ?D(Name0),
    Name = case Kind of 'spec' -> Kind; _ -> Name0 end,
    #token{line=LastLine, offset=LastOffset,
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    ?D(Args),
    Extra = to_string(Args),
    ?D(Extra),
    {AttrArgs, _, _} = get_attribute_args(Kind, Args, Args),
    ?D({AttrArgs, Extra}),
    ExternalRefs = get_refs(tl(AttrArgs), Extra, ?ARI_TYPESPEC),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     [#ref{data=#type_def{type=Name}, offset=Offset, length=PosLength, function=Name, 
           arity=?ARI_TYPESPEC, clause="", sub_clause=false} | ExternalRefs], [], []}.

get_other_attribute(Name, Offset, Line, Attribute, Args) ->
    #token{line=LastLine, offset=LastOffset, 
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    Between = erlide_np_util:get_between_outer_pars(Attribute, '(', ')'),
    Extra = to_string(Between),
    {AttrArgs, Exports, Imports} = get_attribute_args(Name, Between, Args),
    ?D({AttrArgs, Between}),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     make_attribute_ref(Name, AttrArgs, Extra, Offset, PosLength)++
         make_attribute_arg_refs(Name, AttrArgs, Between),
     Exports, Imports}.

get_exported(F_A, Exports) ->
    lists:member(F_A, Exports).

get_attribute_args(import, Between, _Args) ->
    From = get_first_of_kind(atom, Between),
    Tokens = get_between(Between, '[', ']'),
    Import = {From, fun_arity_from_tokens(Tokens)},
    {Import, [], [Import]};
get_attribute_args(export, Between, _Args) ->
    Tokens = get_between(Between, '[', ']'),
    Exports = fun_arity_from_tokens(Tokens),
    {Exports, Exports, []};
get_attribute_args(record, Between, _Args) ->
    [RecordToken | _] = Between,
    RecordName = RecordToken#token.value,
    Tokens = erlide_np_util:get_between_outer_pars(Between, '{', '}'),
    R = {RecordName, field_list_from_tokens(Tokens)},
    {R, [], []};
get_attribute_args(_, _Between, Args) ->
    {Args, [], []}.

check_class([#token{kind = AtomOrMacro}, #token{kind = '('} | _])
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    function;
check_class([#token{kind = '-'}, #token{kind = atom} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'spec'} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'type'} | _]) ->
    attribute;
check_class(_X) ->
    %?D(lists:sublist(_X, 5)),
    other.

get_first_of_kind(Kind, Tokens) ->
    case erlide_np_util:skip_to(Tokens, Kind) of
        [#token{kind=Kind, value=Value} | _] ->
            Value;
        _ ->
            undefined
    end.
        
fun_arity_from_tokens([#token{kind=atom, value=Fun}, #token{kind='/'}, 
                       #token{kind=integer, value=Arity} | Rest]) ->
    [{Fun, Arity} | fun_arity_from_tokens(Rest)];
fun_arity_from_tokens([_ | Rest]) ->
    fun_arity_from_tokens(Rest);
fun_arity_from_tokens(_) ->
    [].

field_list_from_tokens([#token{kind=atom, value=Field, line=Line, 
                               offset=Offset, length=Length} | Rest]) ->
    {Extra, NewRest} = get_upto2(Rest, ','),
    [{Field, {{Line, Line, Offset}, Length}, to_string(Extra)}
         | field_list_from_tokens(NewRest)];
field_list_from_tokens([_ | Rest]) ->
    field_list_from_tokens(Rest);
field_list_from_tokens(_) ->
    [].

to_string(Tokens) ->
    S = erlide_scanner:tokens_to_string(Tokens),
    erlide_text:strip(S).
%%     unspacify(S).

get_head(T) ->
    A = get_args(T),
    case get_guards(T) of
        "" ->
            A;
        G ->
            A ++ " when " ++ G
    end.

get_args(T) ->
    case erlide_np_util:get_between_outer_pars(T, '(', ')') of
        [] ->
            "";
        P ->
            "("++to_string(P)++")"
    end.

get_guards([]) ->
    "";
get_guards(T) ->
    to_string(get_between(T, 'when', '->')). 

last_not_eof(L) -> 
    case lists:reverse(L) of
        [eof, Last | _] ->
            Last;
        [Last | _] ->
            Last;
        _ ->
            error
    end.    


get_between(L, A, B) ->
    R = get_upto(L, B),
    lists:reverse(get_upto(lists:reverse(R), A)).

get_upto(L, Delim) ->
        get_upto(L, Delim, []).

get_upto([], _Delim, _Acc) ->
    [];
get_upto([#token{kind=Delim} | _], Delim, Acc) ->
        lists:reverse(Acc);
get_upto([T | Rest], Delim, Acc) ->
    get_upto(Rest, Delim, [T | Acc]).

get_upto2(L, Delim) ->
    get_upto2(L, Delim, []).

get_upto2([], _Delim, Acc) ->
    {lists:reverse(Acc), []};
get_upto2([#token{kind=Delim} | Rest], Delim, Acc) ->
    {lists:reverse(Acc), Rest};
get_upto2([T | Rest], Delim, Acc) ->
    get_upto2(Rest, Delim, [T | Acc]).

check_clause([#token{kind = ';'} | Rest], false) ->
    check_class(Rest) == function;
check_clause(_, _) ->
    false.

split_clauses(F) ->
    split_clauses(F, false, [], []).

split_clauses([], _HaveWhen, Acc, []) ->
    erlide_util:reverse2(Acc);
split_clauses([], HaveWhen, Acc, ClAcc) ->
    split_clauses([], HaveWhen, [ClAcc | Acc], []);
split_clauses([#token{kind=Kind}=T | Rest], _HaveWhen, Acc, ClAcc) 
  when Kind=:='end'; Kind=:=dot; Kind=:='->'; Kind=:='when' ->
    split_clauses(Rest, Kind=:='when', Acc, [T | ClAcc]);
split_clauses([T | Rest] = Tokens, HaveWhen, Acc, ClAcc) ->
    case check_clause(Tokens, HaveWhen) of
        false ->
            split_clauses(Rest, HaveWhen, Acc, [T | ClAcc]);
        true ->
            split_clauses(Rest, HaveWhen, [[T | ClAcc] | Acc], [])
    end.

%% fix_clause([#token{kind=atom, value=Name, line=Line, offset=Offset, length=Length} | Rest] = Code) ->
fix_clauses(Clauses) ->
    fix_clauses(Clauses, []).

fix_clauses([], Acc) ->
    lists:reverse(Acc);
fix_clauses([C | Rest], Acc) ->
    {Clause, Refs} = fix_clause(C),
    fix_clauses(Rest, [{Clause, Refs} | Acc]).

fix_clause([#token{kind=AtomOrMacro, value=Name, line=Line, offset=Offset, length=Length} | Rest]) 
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    #token{line=LastLine, offset=LastOffset, length=LastLength} = last_not_eof(Rest),
    PosLength = LastOffset - Offset + LastLength+1,
    ExternalRefs = get_refs_in_code(Rest),
    {#clause{pos={{Line, LastLine, Offset}, PosLength}, name_pos={{Line, Offset}, Length},
             name=Name, args=get_function_args(Rest), head=get_head(Rest)},
     ExternalRefs}.

get_function_args(Tokens) ->
    P = erlide_np_util:get_between_outer_pars(Tokens, '(', ')'),
    L = erlide_text:split_comma_list(P),
    [to_string(A) || A <- L].

fix_refs(ClausesAndRefs, Function, Imports) ->
    fix_refs(ClausesAndRefs, Function, Imports, []).

fix_refs([], #function{name=Name, arity=Arity, name_pos={{_, Offset}, Length}}, _Imports, Acc) ->
    Ref = #ref{data=#function_def{function=Name, arity=Arity}, offset=Offset, 
               length=Length, function=Name, arity=Arity, clause="", sub_clause=false},
    [Ref | Acc];
fix_refs([{Clause, Refs} | Rest], Function, Imports, Acc0) ->
    Acc1 = fix_refs(Refs, Clause, Function, Imports, Acc0),
    fix_refs(Rest, Function, Imports, Acc1).

fix_refs([], _Clause, _Function, _Imports, Acc) ->
    Acc;
fix_refs([{Offset, Length, RefData} | Rest], #clause{head=Head}=Clause, 
         #function{name=Name, arity=Arity, clauses=Clauses}=Function, 
         Imports, Acc) ->
    SubClause = case Clauses of
                    [] -> false;
                    [_] -> false;
                    _ -> true
                end,
    NewRefData = fix_imported_ref(RefData, Imports),
    Ref = #ref{data=NewRefData, offset=Offset, length=Length, function=Name, 
               arity=Arity, clause=Head, sub_clause=SubClause},
    fix_refs(Rest, Clause, Function, Imports, [Ref | Acc]).

fix_imported_ref(#local_call{function=F, arity=A}=RefData, [{Module, Imports} | Rest]) ->
    case lists:member({F, A}, Imports) of
        true -> #external_call{module=Module, function=F, arity=A};
        false -> fix_imported_ref(RefData, Rest)
    end;
fix_imported_ref(RefData, _Imports) ->
    RefData.

%% find references in code
%% VERY simple, M:F(_ ...), F(_ ...), #R, ?M, and that's it
%% returns {Offset, Length, Ref}, which is made into proper refs
%% by fix_refs
get_refs_in_code(L) ->
    get_refs_in_code(L, []).

get_refs_in_code([], Acc) ->
    lists:reverse(Acc);
get_refs_in_code([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
                  #token{kind=atom, value=F, offset=Offset2, length=Length2},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length2+Offset2-Offset, 
         #external_call{module = M, function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=atom, value=F, offset=Offset, length=Length},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length, #local_call{function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest], 
                 Acc) ->
    R = {Offset, Length, #macro_ref{macro=M}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind='#', offset=Offset}, 
                  #token{kind=atom, value=Record, offset=Offset2, length=Length2} | Rest],
                 Acc) ->
    R = {Offset, Length2+Offset2-Offset, #record_ref{record=Record}},
    {NewRest, Fields, RightSides} = erlide_np_records:check_fields(Rest, Record),
    NewAcc = get_refs_in_code(RightSides, Acc),
    get_refs_in_code(NewRest, Fields ++ [R | NewAcc]);
get_refs_in_code([#token{kind=var, value=V, offset=Offset, length=Length} | Rest],
                 Acc) ->
    R = {Offset, Length, make_var_def_ref(Acc, V)},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([_ | Rest], Acc) ->
    get_refs_in_code(Rest, Acc).

make_var_def_ref([], Var) ->
	#var_def{variable=Var};
make_var_def_ref([{_, _, #var_def{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([{_, _, #var_ref{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([_ | Rest], Var) ->
	make_var_def_ref(Rest, Var).

%% find type references in spec, type and opaque
%% also VERY simple, type(), ?M, #R, only
%% returns proper refs
get_refs(Tokens, Name, Arity) ->
	get_refs(Tokens, Name, Arity, []).

get_refs([], _, _, Acc) ->
    lists:reverse(Acc);
get_refs([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #macro_ref{macro=M}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind='#', offset=Offset},
          #token{kind=atom, value=R, offset=Offset2, length=Length2} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset, #record_ref{record=R}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
		  #token{kind=atom, value=T, offset=Offset2, length=Length2},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset,
                   #type_ref{module=M, type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=T, offset=Offset, length=Length},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #type_ref{module='_', type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=variable, value=V, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #var_ref{variable=V}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([_ | Rest], Name, Arity, Acc) ->
    get_refs(Rest, Name, Arity, Acc).

get_record_field_defs([], _) ->
    [];
get_record_field_defs([{FieldName, {{_, _, Offset}, Length}, _Extra} | Rest],
                      RecordName) ->
    [make_ref(Offset, Length, #record_field_def{record=RecordName, field=FieldName}, 
              FieldName, ?ARI_RECORD_FIELD_DEF)
         | get_record_field_defs(Rest, RecordName)].

make_ref(Offset, Length, Data, Name, Arity) ->
    #ref{data=Data, offset=Offset, length=Length, function=Name, arity=Arity, 
         clause="", sub_clause=false}.

make_attribute_arg_refs(define, Name, [#token{}, #token{kind='('} | Rest]) ->
    get_refs(skip_to_rparen(Rest), Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(define, Name, [#token{}, #token{kind=','} | Rest]) ->
    get_refs(Rest, Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(record, {Name, Fields}, [#token{}, #token{kind=','} | Rest]) ->
    get_record_field_defs(Fields, Name) ++ get_refs(Rest, Name, ?ARI_RECORD_DEF);
make_attribute_arg_refs(_, _, _) ->
    [].

skip_to_rparen([]) ->
    [];
skip_to_rparen([#token{kind=')'} | Rest]) ->
    Rest;
skip_to_rparen([_ | Rest]) ->
    skip_to_rparen(Rest).

make_attribute_ref(Name, Between, Extra, Offset, Length) ->
    ?D({Name, Between, Offset, Length}),
    case make_attribute_ref(Name, Between, Extra) of
        [] ->
            [];
        {Arity, AName, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=AName, 
                  arity=Arity, clause="", sub_clause=false}];
        {Arity, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=Name, 
                  arity=Arity, clause="", sub_clause=false}]
    end.

make_attribute_ref(module, _, Extra) ->
    {?ARI_ATTRIBUTE, #module_def{module=Extra}};
make_attribute_ref(record, Between, _E) ->
    ?D({Between, _E}),
    Name = case Between of
               {N, _} -> N;
               N -> N
           end,
    R= #record_def{record=Name},
    ?D(R),
    {?ARI_RECORD_DEF, Name, R};
make_attribute_ref(define, [Name | _], _) when is_list(Name) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(define, Name, _) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(include, _, Extra) ->
    {?ARI_INCLUDE, #include_ref{filename=unquote(Extra)}};
make_attribute_ref(_, _, _) ->
    [].

unquote(L) ->
    lists:reverse(unquote_first(lists:reverse(unquote_first(L)))).

unquote_first([$" | Rest]) ->
    Rest;
unquote_first(L) ->
    L.

remove_rest([], _) -> [];
remove_rest(Rest, Rest) -> [];
remove_rest([Hd | Tl], Rest) -> [Hd | remove_rest(Tl, Rest)].
%% Description: Split source files into functions and clauses and collects references
%% Author: jakob
%% Created: Mar 23, 2006

-module(erlide_noparse).

%%
%% Exported Functions
%%

%% called from Java
-export([initial_parse/5, reparse/1, remove_cache_files/2]).

%% called from Erlang
-export([read_module_refs/3]).

%%
%% Include files
%%

%% -define(DEBUG, 1).
%% -define(IO_FORMAT_DEBUG, 1).

-define(CACHE_VERSION, 25).
-define(SERVER, erlide_noparse).

-include("erlide.hrl").
-include("erlide_noparse.hrl").
-include("erlide_scanner.hrl").
-include("erlide_search_server.hrl").

%% special arity flags
-define(ARI_TYPESPEC, -2).
-define(ARI_ATTRIBUTE, -3).
-define(ARI_RECORD_DEF, -4).
-define(ARI_MACRO_DEF, -5).
-define(ARI_INCLUDE, -6).
-define(ARI_RECORD_FIELD_DEF, -7).

-define(X(X), erlide_log:log({X, erlang:now()})).


%%
%% API Functions
%%

initial_parse(ScannerName, ModuleFileName, StateDir, UseCache,
              UpdateSearchServer) ->
    try
        %%         ?D({StateDir, ModuleFileName}),
        BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
        RefsFileName = BaseName ++ ".refs",
        RenewFun = fun(_F) ->
                           do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer)
                   end,
        CacheFileName = BaseName ++ ".noparse",
        ?D(CacheFileName),
        {Cached, Res} = erlide_util:check_and_renew_cached(
                          ModuleFileName, CacheFileName, ?CACHE_VERSION, 
                          RenewFun, UseCache),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        ?D(updated),
        {ok, Res, Cached}
    catch
        error:Reason ->
            {error, Reason}
    end.

reparse(ScannerName) ->
    try
        Res = do_parse(ScannerName, "", "", true),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        {ok, Res, unused}
    catch
        error:Reason ->
            {error, Reason}
    end.

read_module_refs(ScannerName, ModulePath, StateDir) ->
    ?D(ScannerName),
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    RefsFileName = BaseName ++ ".refs",
    ?D(RefsFileName),
    case file:read_file(RefsFileName) of
        {ok, Binary} ->
            ?D(byte_size(Binary)),
            binary_to_term(Binary);
        _ ->
            ?D(ModulePath),
            {ok, InitialTextBin} = file:read_file(ModulePath),
            ?D(byte_size(InitialTextBin)),
            InitialText = binary_to_list(InitialTextBin),
            _D = erlide_scanner_server:initialScan(
                   ScannerName, ModulePath, InitialText, StateDir, true),
            ?D(_D),
            initial_parse(ScannerName, ModulePath, StateDir, true, false),
            ?D(RefsFileName),
            {ok, Binary} = file:read_file(RefsFileName),
            ?D(byte_size(Binary)),
            binary_to_term(Binary)
    end.

remove_cache_files(ScannerName, StateDir) ->
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    ScannerCacheFileName = BaseName ++ ".scan",
    file:delete(ScannerCacheFileName),
    RefsFileName = BaseName ++ ".refs",
    file:delete(RefsFileName),
    CacheFileName = BaseName ++ ".noparse",
    file:delete(CacheFileName).

%%
%% Internal functions
%%

do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer) ->
    Toks = erlide_scanner_server:getTokens(ScannerName),
    do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer).

do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer) ->
    ?D({do_parse, ScannerName, length(Toks)}),
    ?X(111),
    {UncommentToks, Comments} = erlide_np_util:extract_comments(Toks),
    ?X(222),
    ?D({length(UncommentToks), length(Comments)}),
    ?D({UncommentToks}),
    Functions = erlide_np_util:split_after_dots(UncommentToks),
    ?X(333),
    ?D(length(Functions)),
    AutoImports = erlide_util:add_auto_imported([]),
    ?X(444),
%%     ?D(AutoImports),
    {Collected, Refs} = classify_and_collect(Functions, [], [], [], AutoImports),
    ?X(555),
    ?D({'>>',length(Collected)}),
    CommentedCollected = erlide_np_util:get_function_comments(Collected, Comments),
    ?X(666),
    ?D(CommentedCollected),
    Model = #model{forms=CommentedCollected, comments=Comments},
    %%erlide_noparse_server:create(ScannerName, Model),
%%     ?D({"Model", length(Model#model.forms), erts_debug:flat_size(Model)}),
    FixedModel = fixup_model(Model),
    ?X(777),
%%     ?D(erts_debug:flat_size(FixedModel)),
    %% 	?D([erts_debug:flat_size(F) || F <- element(2, FixedModel)]),
    ?D(StateDir),
    case StateDir of
        "" -> ok;
        _ ->
            ?D({RefsFileName, length(Refs)}),
            file:write_file(RefsFileName, term_to_binary(Refs, [compressed]))
    end,
    update_search_server(UpdateSearchServer, ScannerName, Refs),
%%     ?D(FixedModel),
    ?X(888),
    FixedModel.

update_search_server(true, ScannerName, Refs) ->
    erlide_search_server:add_module_refs(ScannerName, Refs);
update_search_server(_, _, _) ->
    ok.

fixup_model(#model{forms=Forms, comments=Comments}) ->
    FixedComments = fixup_tokens(Comments),
    FixedForms = fixup_forms(Forms),
    #model{forms=FixedForms, comments=FixedComments}.

fixup_forms(Forms) ->
    [fixup_form(Form) || Form <- Forms].

fixup_tokens(Tokens) ->
    [fixup_token(Token) || Token <- Tokens].

fixup_token(#token{value=Value} = Token) when is_list(Value) ->
    Token#token{value=to_binary(Value)};
fixup_token(#token{text=Text} = Token) when is_list(Text) ->
    Token#token{value=to_binary(Text)};
fixup_token(Token) ->
    Token.

binary_args(Args) when is_list(Args) ->
    [iolist_to_binary(A) || A <- Args];
binary_args(_) ->
    [].

fixup_form(#function{comment=Comment, clauses=Clauses, args=Args} = Function) ->
    Function#function{comment= to_binary(Comment), clauses=fixup_forms(Clauses), args=binary_args(Args)};
fixup_form(#clause{head=Head, args=Args} = Clause) ->
    Clause#clause{head=to_binary(Head), args=binary_args(Args)};
fixup_form(Other) ->
    Other.

to_binary(Comment) when is_list(Comment) ->
    try 
        iolist_to_binary(Comment) 
    catch 
        _:_ -> 
            unicode:characters_to_binary(Comment) 
    end;
to_binary(Other) ->
    Other.

%% parse_test(ScannerName, File) ->
%%     erlide_scanner_server:scan_uncached(ScannerName, File, ""),
%%     Toks = erlide_scanner_server:getTokens(ScannerName),
%%     {UncommentToks, Comments} = extract_comments(Toks),
%%     Functions = split_after_dots(UncommentToks, [], []),
%%     {Collected, Refs} = classify_and_collect(Functions, [], []),
%%     _Model = #model{forms=Collected, comments=Comments},
%%     %%erlide_noparse_server:create(ScannerName, Model, ""),
%%     ok.

classify_and_collect([], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([eof], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([C | Rest], Acc, RefsAcc, Exports, Imports) ->
    {R, Refs, MoreExports, MoreImports} = cac(check_class(C), C, Exports, Imports),
    classify_and_collect(Rest, [R | Acc], Refs++RefsAcc, MoreExports++Exports, MoreImports++Imports).

cac(function, Tokens, Exports, Imports) ->
    ClauseList = split_clauses(Tokens),
    %?D(ClauseList),
    ClausesAndRefs = fix_clauses(ClauseList),
    %?D(length(Clauses)),
    [{#clause{pos=P, name=N, args=A, head=H, name_pos=NP}, _Refs} | _] = ClausesAndRefs,
    ?D(A),
    Arity = length(A),
    Exported = get_exported({N, Arity}, Exports),
    Function = case ClausesAndRefs of  	% only show subclauses when more than one
                   [_] ->
                       #function{pos=P, name=N, arity=Arity, args=A, head=H, 
                                 clauses=[], name_pos=NP, exported=Exported};
                   _ ->
                       Clauses = [C || {C, _} <- ClausesAndRefs],
                       #function{pos=P, name=N, arity=Arity, clauses=Clauses, 
                                 name_pos=NP, exported=Exported}
               end,
    {Function, fix_refs(ClausesAndRefs, Function, Imports), [], []};
cac(attribute, Attribute, _Exports, _Imports) ->
    ?D(Attribute),
    case Attribute of
        %% -spec, -type or -opaque
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=Kind, line=_Line, offset=_Offset, value=Name} | Args]
          when (Kind=:='spec') or ((Kind=:=atom) and (Name=:='type'))
                   or ((Kind=:=atom) and (Name=:='opaque'))->
            get_type_attribute(Kind, Name, Offset, Line, Attribute, Args);
        %% other attributes
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=atom, value=Name, line=_Line, offset=_Offset},
         _, #token{value=Args} | _] = Attribute ->
            get_other_attribute(Name, Offset, Line, Attribute, Args);
        [_, #token{kind=atom, value=Name, line=Line, offset=Offset} | _] ->
            #token{line=LastLine, offset=LastOffset, 
                   length=LastLength} = last_not_eof(Attribute),
            PosLength = LastOffset - Offset + LastLength,
            {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                       name=Name, args=[]}, [], [], []}
    end;
cac(other, [#token{value=Name, line=Line, offset=Offset, length=Length} | _], 
    _Exports, _Imports) ->
    {#other{pos={{Line, Line, Offset}, Length}, name=Name}, [], [], []};
cac(_, _D, _E, _I) ->
    {eof, [], [], []}.

get_type_attribute(Kind, Name0, Offset, Line, Attribute, Args) ->
    ?D(Name0),
    Name = case Kind of 'spec' -> Kind; _ -> Name0 end,
    #token{line=LastLine, offset=LastOffset,
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    ?D(Args),
    Extra = to_string(Args),
    ?D(Extra),
    {AttrArgs, _, _} = get_attribute_args(Kind, Args, Args),
    ?D({AttrArgs, Extra}),
    ExternalRefs = get_refs(tl(AttrArgs), Extra, ?ARI_TYPESPEC),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     [#ref{data=#type_def{type=Name}, offset=Offset, length=PosLength, function=Name, 
           arity=?ARI_TYPESPEC, clause="", sub_clause=false} | ExternalRefs], [], []}.

get_other_attribute(Name, Offset, Line, Attribute, Args) ->
    #token{line=LastLine, offset=LastOffset, 
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    Between = erlide_np_util:get_between_outer_pars(Attribute, '(', ')'),
    Extra = to_string(Between),
    {AttrArgs, Exports, Imports} = get_attribute_args(Name, Between, Args),
    ?D({AttrArgs, Between}),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     make_attribute_ref(Name, AttrArgs, Extra, Offset, PosLength)++
         make_attribute_arg_refs(Name, AttrArgs, Between),
     Exports, Imports}.

get_exported(F_A, Exports) ->
    lists:member(F_A, Exports).

get_attribute_args(import, Between, _Args) ->
    From = get_first_of_kind(atom, Between),
    Tokens = get_between(Between, '[', ']'),
    Import = {From, fun_arity_from_tokens(Tokens)},
    {Import, [], [Import]};
get_attribute_args(export, Between, _Args) ->
    Tokens = get_between(Between, '[', ']'),
    Exports = fun_arity_from_tokens(Tokens),
    {Exports, Exports, []};
get_attribute_args(record, Between, _Args) ->
    [RecordToken | _] = Between,
    RecordName = RecordToken#token.value,
    Tokens = erlide_np_util:get_between_outer_pars(Between, '{', '}'),
    R = {RecordName, field_list_from_tokens(Tokens)},
    {R, [], []};
get_attribute_args(_, _Between, Args) ->
    {Args, [], []}.

check_class([#token{kind = AtomOrMacro}, #token{kind = '('} | _])
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    function;
check_class([#token{kind = '-'}, #token{kind = atom} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'spec'} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'type'} | _]) ->
    attribute;
check_class(_X) ->
    %?D(lists:sublist(_X, 5)),
    other.

get_first_of_kind(Kind, Tokens) ->
    case erlide_np_util:skip_to(Tokens, Kind) of
        [#token{kind=Kind, value=Value} | _] ->
            Value;
        _ ->
            undefined
    end.
        
fun_arity_from_tokens([#token{kind=atom, value=Fun}, #token{kind='/'}, 
                       #token{kind=integer, value=Arity} | Rest]) ->
    [{Fun, Arity} | fun_arity_from_tokens(Rest)];
fun_arity_from_tokens([_ | Rest]) ->
    fun_arity_from_tokens(Rest);
fun_arity_from_tokens(_) ->
    [].

field_list_from_tokens([#token{kind=atom, value=Field, line=Line, 
                               offset=Offset, length=Length} | Rest]) ->
    {Extra, NewRest} = get_upto2(Rest, ','),
    [{Field, {{Line, Line, Offset}, Length}, to_string(Extra)}
         | field_list_from_tokens(NewRest)];
field_list_from_tokens([_ | Rest]) ->
    field_list_from_tokens(Rest);
field_list_from_tokens(_) ->
    [].

to_string(Tokens) ->
    S = erlide_scanner:tokens_to_string(Tokens),
    erlide_text:strip(S).
%%     unspacify(S).

get_head(T) ->
    A = get_args(T),
    case get_guards(T) of
        "" ->
            A;
        G ->
            A ++ " when " ++ G
    end.

get_args(T) ->
    case erlide_np_util:get_between_outer_pars(T, '(', ')') of
        [] ->
            "";
        P ->
            "("++to_string(P)++")"
    end.

get_guards([]) ->
    "";
get_guards(T) ->
    to_string(get_between(T, 'when', '->')). 

last_not_eof(L) -> 
    case lists:reverse(L) of
        [eof, Last | _] ->
            Last;
        [Last | _] ->
            Last;
        _ ->
            error
    end.    


get_between(L, A, B) ->
    R = get_upto(L, B),
    lists:reverse(get_upto(lists:reverse(R), A)).

get_upto(L, Delim) ->
        get_upto(L, Delim, []).

get_upto([], _Delim, _Acc) ->
    [];
get_upto([#token{kind=Delim} | _], Delim, Acc) ->
        lists:reverse(Acc);
get_upto([T | Rest], Delim, Acc) ->
    get_upto(Rest, Delim, [T | Acc]).

get_upto2(L, Delim) ->
    get_upto2(L, Delim, []).

get_upto2([], _Delim, Acc) ->
    {lists:reverse(Acc), []};
get_upto2([#token{kind=Delim} | Rest], Delim, Acc) ->
    {lists:reverse(Acc), Rest};
get_upto2([T | Rest], Delim, Acc) ->
    get_upto2(Rest, Delim, [T | Acc]).

check_clause([#token{kind = ';'} | Rest], false) ->
    check_class(Rest) == function;
check_clause(_, _) ->
    false.

split_clauses(F) ->
    split_clauses(F, false, [], []).

split_clauses([], _HaveWhen, Acc, []) ->
    erlide_util:reverse2(Acc);
split_clauses([], HaveWhen, Acc, ClAcc) ->
    split_clauses([], HaveWhen, [ClAcc | Acc], []);
split_clauses([#token{kind=Kind}=T | Rest], _HaveWhen, Acc, ClAcc) 
  when Kind=:='end'; Kind=:=dot; Kind=:='->'; Kind=:='when' ->
    split_clauses(Rest, Kind=:='when', Acc, [T | ClAcc]);
split_clauses([T | Rest] = Tokens, HaveWhen, Acc, ClAcc) ->
    case check_clause(Tokens, HaveWhen) of
        false ->
            split_clauses(Rest, HaveWhen, Acc, [T | ClAcc]);
        true ->
            split_clauses(Rest, HaveWhen, [[T | ClAcc] | Acc], [])
    end.

%% fix_clause([#token{kind=atom, value=Name, line=Line, offset=Offset, length=Length} | Rest] = Code) ->
fix_clauses(Clauses) ->
    fix_clauses(Clauses, []).

fix_clauses([], Acc) ->
    lists:reverse(Acc);
fix_clauses([C | Rest], Acc) ->
    {Clause, Refs} = fix_clause(C),
    fix_clauses(Rest, [{Clause, Refs} | Acc]).

fix_clause([#token{kind=AtomOrMacro, value=Name, line=Line, offset=Offset, length=Length} | Rest]) 
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    #token{line=LastLine, offset=LastOffset, length=LastLength} = last_not_eof(Rest),
    PosLength = LastOffset - Offset + LastLength+1,
    ExternalRefs = get_refs_in_code(Rest),
    {#clause{pos={{Line, LastLine, Offset}, PosLength}, name_pos={{Line, Offset}, Length},
             name=Name, args=get_function_args(Rest), head=get_head(Rest)},
     ExternalRefs}.

get_function_args(Tokens) ->
    P = erlide_np_util:get_between_outer_pars(Tokens, '(', ')'),
    L = erlide_text:split_comma_list(P),
    [to_string(A) || A <- L].

fix_refs(ClausesAndRefs, Function, Imports) ->
    fix_refs(ClausesAndRefs, Function, Imports, []).

fix_refs([], #function{name=Name, arity=Arity, name_pos={{_, Offset}, Length}}, _Imports, Acc) ->
    Ref = #ref{data=#function_def{function=Name, arity=Arity}, offset=Offset, 
               length=Length, function=Name, arity=Arity, clause="", sub_clause=false},
    [Ref | Acc];
fix_refs([{Clause, Refs} | Rest], Function, Imports, Acc0) ->
    Acc1 = fix_refs(Refs, Clause, Function, Imports, Acc0),
    fix_refs(Rest, Function, Imports, Acc1).

fix_refs([], _Clause, _Function, _Imports, Acc) ->
    Acc;
fix_refs([{Offset, Length, RefData} | Rest], #clause{head=Head}=Clause, 
         #function{name=Name, arity=Arity, clauses=Clauses}=Function, 
         Imports, Acc) ->
    SubClause = case Clauses of
                    [] -> false;
                    [_] -> false;
                    _ -> true
                end,
    NewRefData = fix_imported_ref(RefData, Imports),
    Ref = #ref{data=NewRefData, offset=Offset, length=Length, function=Name, 
               arity=Arity, clause=Head, sub_clause=SubClause},
    fix_refs(Rest, Clause, Function, Imports, [Ref | Acc]).

fix_imported_ref(#local_call{function=F, arity=A}=RefData, [{Module, Imports} | Rest]) ->
    case lists:member({F, A}, Imports) of
        true -> #external_call{module=Module, function=F, arity=A};
        false -> fix_imported_ref(RefData, Rest)
    end;
fix_imported_ref(RefData, _Imports) ->
    RefData.

%% find references in code
%% VERY simple, M:F(_ ...), F(_ ...), #R, ?M, and that's it
%% returns {Offset, Length, Ref}, which is made into proper refs
%% by fix_refs
get_refs_in_code(L) ->
    get_refs_in_code(L, []).

get_refs_in_code([], Acc) ->
    lists:reverse(Acc);
get_refs_in_code([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
                  #token{kind=atom, value=F, offset=Offset2, length=Length2},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length2+Offset2-Offset, 
         #external_call{module = M, function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=atom, value=F, offset=Offset, length=Length},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length, #local_call{function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest], 
                 Acc) ->
    R = {Offset, Length, #macro_ref{macro=M}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind='#', offset=Offset}, 
                  #token{kind=atom, value=Record, offset=Offset2, length=Length2} | Rest],
                 Acc) ->
    R = {Offset, Length2+Offset2-Offset, #record_ref{record=Record}},
    {NewRest, Fields, RightSides} = erlide_np_records:check_fields(Rest, Record),
    NewAcc = get_refs_in_code(RightSides, Acc),
    get_refs_in_code(NewRest, Fields ++ [R | NewAcc]);
get_refs_in_code([#token{kind=var, value=V, offset=Offset, length=Length} | Rest],
                 Acc) ->
    R = {Offset, Length, make_var_def_ref(Acc, V)},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([_ | Rest], Acc) ->
    get_refs_in_code(Rest, Acc).

make_var_def_ref([], Var) ->
	#var_def{variable=Var};
make_var_def_ref([{_, _, #var_def{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([{_, _, #var_ref{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([_ | Rest], Var) ->
	make_var_def_ref(Rest, Var).

%% find type references in spec, type and opaque
%% also VERY simple, type(), ?M, #R, only
%% returns proper refs
get_refs(Tokens, Name, Arity) ->
	get_refs(Tokens, Name, Arity, []).

get_refs([], _, _, Acc) ->
    lists:reverse(Acc);
get_refs([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #macro_ref{macro=M}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind='#', offset=Offset},
          #token{kind=atom, value=R, offset=Offset2, length=Length2} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset, #record_ref{record=R}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
		  #token{kind=atom, value=T, offset=Offset2, length=Length2},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset,
                   #type_ref{module=M, type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=T, offset=Offset, length=Length},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #type_ref{module='_', type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=variable, value=V, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #var_ref{variable=V}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([_ | Rest], Name, Arity, Acc) ->
    get_refs(Rest, Name, Arity, Acc).

get_record_field_defs([], _) ->
    [];
get_record_field_defs([{FieldName, {{_, _, Offset}, Length}, _Extra} | Rest],
                      RecordName) ->
    [make_ref(Offset, Length, #record_field_def{record=RecordName, field=FieldName}, 
              FieldName, ?ARI_RECORD_FIELD_DEF)
         | get_record_field_defs(Rest, RecordName)].

make_ref(Offset, Length, Data, Name, Arity) ->
    #ref{data=Data, offset=Offset, length=Length, function=Name, arity=Arity, 
         clause="", sub_clause=false}.

make_attribute_arg_refs(define, Name, [#token{}, #token{kind='('} | Rest]) ->
    get_refs(skip_to_rparen(Rest), Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(define, Name, [#token{}, #token{kind=','} | Rest]) ->
    get_refs(Rest, Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(record, {Name, Fields}, [#token{}, #token{kind=','} | Rest]) ->
    get_record_field_defs(Fields, Name) ++ get_refs(Rest, Name, ?ARI_RECORD_DEF);
make_attribute_arg_refs(_, _, _) ->
    [].

skip_to_rparen([]) ->
    [];
skip_to_rparen([#token{kind=')'} | Rest]) ->
    Rest;
skip_to_rparen([_ | Rest]) ->
    skip_to_rparen(Rest).

make_attribute_ref(Name, Between, Extra, Offset, Length) ->
    ?D({Name, Between, Offset, Length}),
    case make_attribute_ref(Name, Between, Extra) of
        [] ->
            [];
        {Arity, AName, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=AName, 
                  arity=Arity, clause="", sub_clause=false}];
        {Arity, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=Name, 
                  arity=Arity, clause="", sub_clause=false}]
    end.

make_attribute_ref(module, _, Extra) ->
    {?ARI_ATTRIBUTE, #module_def{module=Extra}};
make_attribute_ref(record, Between, _E) ->
    ?D({Between, _E}),
    Name = case Between of
               {N, _} -> N;
               N -> N
           end,
    R= #record_def{record=Name},
    ?D(R),
    {?ARI_RECORD_DEF, Name, R};
make_attribute_ref(define, [Name | _], _) when is_list(Name) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(define, Name, _) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(include, _, Extra) ->
    {?ARI_INCLUDE, #include_ref{filename=unquote(Extra)}};
make_attribute_ref(_, _, _) ->
    [].

unquote(L) ->
    lists:reverse(unquote_first(lists:reverse(unquote_first(L)))).

unquote_first([$" | Rest]) ->
    Rest;
unquote_first(L) ->
    L.

remove_rest([], _) -> [];
remove_rest(Rest, Rest) -> [];
remove_rest([Hd | Tl], Rest) -> [Hd | remove_rest(Tl, Rest)].
%% Description: Split source files into functions and clauses and collects references
%% Author: jakob
%% Created: Mar 23, 2006

-module(erlide_noparse).

%%
%% Exported Functions
%%

%% called from Java
-export([initial_parse/5, reparse/1, remove_cache_files/2]).

%% called from Erlang
-export([read_module_refs/3]).

%%
%% Include files
%%

%% -define(DEBUG, 1).
%% -define(IO_FORMAT_DEBUG, 1).

-define(CACHE_VERSION, 25).
-define(SERVER, erlide_noparse).

-include("erlide.hrl").
-include("erlide_noparse.hrl").
-include("erlide_scanner.hrl").
-include("erlide_search_server.hrl").

%% special arity flags
-define(ARI_TYPESPEC, -2).
-define(ARI_ATTRIBUTE, -3).
-define(ARI_RECORD_DEF, -4).
-define(ARI_MACRO_DEF, -5).
-define(ARI_INCLUDE, -6).
-define(ARI_RECORD_FIELD_DEF, -7).

-define(X(X), erlide_log:log({X, erlang:now()})).


%%
%% API Functions
%%

initial_parse(ScannerName, ModuleFileName, StateDir, UseCache,
              UpdateSearchServer) ->
    try
        %%         ?D({StateDir, ModuleFileName}),
        BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
        RefsFileName = BaseName ++ ".refs",
        RenewFun = fun(_F) ->
                           do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer)
                   end,
        CacheFileName = BaseName ++ ".noparse",
        ?D(CacheFileName),
        {Cached, Res} = erlide_util:check_and_renew_cached(
                          ModuleFileName, CacheFileName, ?CACHE_VERSION, 
                          RenewFun, UseCache),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        ?D(updated),
        {ok, Res, Cached}
    catch
        error:Reason ->
            {error, Reason}
    end.

reparse(ScannerName) ->
    try
        Res = do_parse(ScannerName, "", "", true),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        {ok, Res, unused}
    catch
        error:Reason ->
            {error, Reason}
    end.

read_module_refs(ScannerName, ModulePath, StateDir) ->
    ?D(ScannerName),
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    RefsFileName = BaseName ++ ".refs",
    ?D(RefsFileName),
    case file:read_file(RefsFileName) of
        {ok, Binary} ->
            ?D(byte_size(Binary)),
            binary_to_term(Binary);
        _ ->
            ?D(ModulePath),
            {ok, InitialTextBin} = file:read_file(ModulePath),
            ?D(byte_size(InitialTextBin)),
            InitialText = binary_to_list(InitialTextBin),
            _D = erlide_scanner_server:initialScan(
                   ScannerName, ModulePath, InitialText, StateDir, true),
            ?D(_D),
            initial_parse(ScannerName, ModulePath, StateDir, true, false),
            ?D(RefsFileName),
            {ok, Binary} = file:read_file(RefsFileName),
            ?D(byte_size(Binary)),
            binary_to_term(Binary)
    end.

remove_cache_files(ScannerName, StateDir) ->
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    ScannerCacheFileName = BaseName ++ ".scan",
    file:delete(ScannerCacheFileName),
    RefsFileName = BaseName ++ ".refs",
    file:delete(RefsFileName),
    CacheFileName = BaseName ++ ".noparse",
    file:delete(CacheFileName).

%%
%% Internal functions
%%

do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer) ->
    Toks = erlide_scanner_server:getTokens(ScannerName),
    do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer).

do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer) ->
    ?D({do_parse, ScannerName, length(Toks)}),
    ?X(111),
    {UncommentToks, Comments} = erlide_np_util:extract_comments(Toks),
    ?X(222),
    ?D({length(UncommentToks), length(Comments)}),
    ?D({UncommentToks}),
    Functions = erlide_np_util:split_after_dots(UncommentToks),
    ?X(333),
    ?D(length(Functions)),
    AutoImports = erlide_util:add_auto_imported([]),
    ?X(444),
%%     ?D(AutoImports),
    {Collected, Refs} = classify_and_collect(Functions, [], [], [], AutoImports),
    ?X(555),
    ?D({'>>',length(Collected)}),
    CommentedCollected = erlide_np_util:get_function_comments(Collected, Comments),
    ?X(666),
    ?D(CommentedCollected),
    Model = #model{forms=CommentedCollected, comments=Comments},
    %%erlide_noparse_server:create(ScannerName, Model),
%%     ?D({"Model", length(Model#model.forms), erts_debug:flat_size(Model)}),
    FixedModel = fixup_model(Model),
    ?X(777),
%%     ?D(erts_debug:flat_size(FixedModel)),
    %% 	?D([erts_debug:flat_size(F) || F <- element(2, FixedModel)]),
    ?D(StateDir),
    case StateDir of
        "" -> ok;
        _ ->
            ?D({RefsFileName, length(Refs)}),
            file:write_file(RefsFileName, term_to_binary(Refs, [compressed]))
    end,
    update_search_server(UpdateSearchServer, ScannerName, Refs),
%%     ?D(FixedModel),
    ?X(888),
    FixedModel.

update_search_server(true, ScannerName, Refs) ->
    erlide_search_server:add_module_refs(ScannerName, Refs);
update_search_server(_, _, _) ->
    ok.

fixup_model(#model{forms=Forms, comments=Comments}) ->
    FixedComments = fixup_tokens(Comments),
    FixedForms = fixup_forms(Forms),
    #model{forms=FixedForms, comments=FixedComments}.

fixup_forms(Forms) ->
    [fixup_form(Form) || Form <- Forms].

fixup_tokens(Tokens) ->
    [fixup_token(Token) || Token <- Tokens].

fixup_token(#token{value=Value} = Token) when is_list(Value) ->
    Token#token{value=to_binary(Value)};
fixup_token(#token{text=Text} = Token) when is_list(Text) ->
    Token#token{value=to_binary(Text)};
fixup_token(Token) ->
    Token.

binary_args(Args) when is_list(Args) ->
    [iolist_to_binary(A) || A <- Args];
binary_args(_) ->
    [].

fixup_form(#function{comment=Comment, clauses=Clauses, args=Args} = Function) ->
    Function#function{comment= to_binary(Comment), clauses=fixup_forms(Clauses), args=binary_args(Args)};
fixup_form(#clause{head=Head, args=Args} = Clause) ->
    Clause#clause{head=to_binary(Head), args=binary_args(Args)};
fixup_form(Other) ->
    Other.

to_binary(Comment) when is_list(Comment) ->
    try 
        iolist_to_binary(Comment) 
    catch 
        _:_ -> 
            unicode:characters_to_binary(Comment) 
    end;
to_binary(Other) ->
    Other.

%% parse_test(ScannerName, File) ->
%%     erlide_scanner_server:scan_uncached(ScannerName, File, ""),
%%     Toks = erlide_scanner_server:getTokens(ScannerName),
%%     {UncommentToks, Comments} = extract_comments(Toks),
%%     Functions = split_after_dots(UncommentToks, [], []),
%%     {Collected, Refs} = classify_and_collect(Functions, [], []),
%%     _Model = #model{forms=Collected, comments=Comments},
%%     %%erlide_noparse_server:create(ScannerName, Model, ""),
%%     ok.

classify_and_collect([], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([eof], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([C | Rest], Acc, RefsAcc, Exports, Imports) ->
    {R, Refs, MoreExports, MoreImports} = cac(check_class(C), C, Exports, Imports),
    classify_and_collect(Rest, [R | Acc], Refs++RefsAcc, MoreExports++Exports, MoreImports++Imports).

cac(function, Tokens, Exports, Imports) ->
    ClauseList = split_clauses(Tokens),
    %?D(ClauseList),
    ClausesAndRefs = fix_clauses(ClauseList),
    %?D(length(Clauses)),
    [{#clause{pos=P, name=N, args=A, head=H, name_pos=NP}, _Refs} | _] = ClausesAndRefs,
    ?D(A),
    Arity = length(A),
    Exported = get_exported({N, Arity}, Exports),
    Function = case ClausesAndRefs of  	% only show subclauses when more than one
                   [_] ->
                       #function{pos=P, name=N, arity=Arity, args=A, head=H, 
                                 clauses=[], name_pos=NP, exported=Exported};
                   _ ->
                       Clauses = [C || {C, _} <- ClausesAndRefs],
                       #function{pos=P, name=N, arity=Arity, clauses=Clauses, 
                                 name_pos=NP, exported=Exported}
               end,
    {Function, fix_refs(ClausesAndRefs, Function, Imports), [], []};
cac(attribute, Attribute, _Exports, _Imports) ->
    ?D(Attribute),
    case Attribute of
        %% -spec, -type or -opaque
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=Kind, line=_Line, offset=_Offset, value=Name} | Args]
          when (Kind=:='spec') or ((Kind=:=atom) and (Name=:='type'))
                   or ((Kind=:=atom) and (Name=:='opaque'))->
            get_type_attribute(Kind, Name, Offset, Line, Attribute, Args);
        %% other attributes
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=atom, value=Name, line=_Line, offset=_Offset},
         _, #token{value=Args} | _] = Attribute ->
            get_other_attribute(Name, Offset, Line, Attribute, Args);
        [_, #token{kind=atom, value=Name, line=Line, offset=Offset} | _] ->
            #token{line=LastLine, offset=LastOffset, 
                   length=LastLength} = last_not_eof(Attribute),
            PosLength = LastOffset - Offset + LastLength,
            {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                       name=Name, args=[]}, [], [], []}
    end;
cac(other, [#token{value=Name, line=Line, offset=Offset, length=Length} | _], 
    _Exports, _Imports) ->
    {#other{pos={{Line, Line, Offset}, Length}, name=Name}, [], [], []};
cac(_, _D, _E, _I) ->
    {eof, [], [], []}.

get_type_attribute(Kind, Name0, Offset, Line, Attribute, Args) ->
    ?D(Name0),
    Name = case Kind of 'spec' -> Kind; _ -> Name0 end,
    #token{line=LastLine, offset=LastOffset,
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    ?D(Args),
    Extra = to_string(Args),
    ?D(Extra),
    {AttrArgs, _, _} = get_attribute_args(Kind, Args, Args),
    ?D({AttrArgs, Extra}),
    ExternalRefs = get_refs(tl(AttrArgs), Extra, ?ARI_TYPESPEC),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     [#ref{data=#type_def{type=Name}, offset=Offset, length=PosLength, function=Name, 
           arity=?ARI_TYPESPEC, clause="", sub_clause=false} | ExternalRefs], [], []}.

get_other_attribute(Name, Offset, Line, Attribute, Args) ->
    #token{line=LastLine, offset=LastOffset, 
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    Between = erlide_np_util:get_between_outer_pars(Attribute, '(', ')'),
    Extra = to_string(Between),
    {AttrArgs, Exports, Imports} = get_attribute_args(Name, Between, Args),
    ?D({AttrArgs, Between}),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     make_attribute_ref(Name, AttrArgs, Extra, Offset, PosLength)++
         make_attribute_arg_refs(Name, AttrArgs, Between),
     Exports, Imports}.

get_exported(F_A, Exports) ->
    lists:member(F_A, Exports).

get_attribute_args(import, Between, _Args) ->
    From = get_first_of_kind(atom, Between),
    Tokens = get_between(Between, '[', ']'),
    Import = {From, fun_arity_from_tokens(Tokens)},
    {Import, [], [Import]};
get_attribute_args(export, Between, _Args) ->
    Tokens = get_between(Between, '[', ']'),
    Exports = fun_arity_from_tokens(Tokens),
    {Exports, Exports, []};
get_attribute_args(record, Between, _Args) ->
    [RecordToken | _] = Between,
    RecordName = RecordToken#token.value,
    Tokens = erlide_np_util:get_between_outer_pars(Between, '{', '}'),
    R = {RecordName, field_list_from_tokens(Tokens)},
    {R, [], []};
get_attribute_args(_, _Between, Args) ->
    {Args, [], []}.

check_class([#token{kind = AtomOrMacro}, #token{kind = '('} | _])
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    function;
check_class([#token{kind = '-'}, #token{kind = atom} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'spec'} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'type'} | _]) ->
    attribute;
check_class(_X) ->
    %?D(lists:sublist(_X, 5)),
    other.

get_first_of_kind(Kind, Tokens) ->
    case erlide_np_util:skip_to(Tokens, Kind) of
        [#token{kind=Kind, value=Value} | _] ->
            Value;
        _ ->
            undefined
    end.
        
fun_arity_from_tokens([#token{kind=atom, value=Fun}, #token{kind='/'}, 
                       #token{kind=integer, value=Arity} | Rest]) ->
    [{Fun, Arity} | fun_arity_from_tokens(Rest)];
fun_arity_from_tokens([_ | Rest]) ->
    fun_arity_from_tokens(Rest);
fun_arity_from_tokens(_) ->
    [].

field_list_from_tokens([#token{kind=atom, value=Field, line=Line, 
                               offset=Offset, length=Length} | Rest]) ->
    {Extra, NewRest} = get_upto2(Rest, ','),
    [{Field, {{Line, Line, Offset}, Length}, to_string(Extra)}
         | field_list_from_tokens(NewRest)];
field_list_from_tokens([_ | Rest]) ->
    field_list_from_tokens(Rest);
field_list_from_tokens(_) ->
    [].

to_string(Tokens) ->
    S = erlide_scanner:tokens_to_string(Tokens),
    erlide_text:strip(S).
%%     unspacify(S).

get_head(T) ->
    A = get_args(T),
    case get_guards(T) of
        "" ->
            A;
        G ->
            A ++ " when " ++ G
    end.

get_args(T) ->
    case erlide_np_util:get_between_outer_pars(T, '(', ')') of
        [] ->
            "";
        P ->
            "("++to_string(P)++")"
    end.

get_guards([]) ->
    "";
get_guards(T) ->
    to_string(get_between(T, 'when', '->')). 

last_not_eof(L) -> 
    case lists:reverse(L) of
        [eof, Last | _] ->
            Last;
        [Last | _] ->
            Last;
        _ ->
            error
    end.    


get_between(L, A, B) ->
    R = get_upto(L, B),
    lists:reverse(get_upto(lists:reverse(R), A)).

get_upto(L, Delim) ->
        get_upto(L, Delim, []).

get_upto([], _Delim, _Acc) ->
    [];
get_upto([#token{kind=Delim} | _], Delim, Acc) ->
        lists:reverse(Acc);
get_upto([T | Rest], Delim, Acc) ->
    get_upto(Rest, Delim, [T | Acc]).

get_upto2(L, Delim) ->
    get_upto2(L, Delim, []).

get_upto2([], _Delim, Acc) ->
    {lists:reverse(Acc), []};
get_upto2([#token{kind=Delim} | Rest], Delim, Acc) ->
    {lists:reverse(Acc), Rest};
get_upto2([T | Rest], Delim, Acc) ->
    get_upto2(Rest, Delim, [T | Acc]).

check_clause([#token{kind = ';'} | Rest], false) ->
    check_class(Rest) == function;
check_clause(_, _) ->
    false.

split_clauses(F) ->
    split_clauses(F, false, [], []).

split_clauses([], _HaveWhen, Acc, []) ->
    erlide_util:reverse2(Acc);
split_clauses([], HaveWhen, Acc, ClAcc) ->
    split_clauses([], HaveWhen, [ClAcc | Acc], []);
split_clauses([#token{kind=Kind}=T | Rest], _HaveWhen, Acc, ClAcc) 
  when Kind=:='end'; Kind=:=dot; Kind=:='->'; Kind=:='when' ->
    split_clauses(Rest, Kind=:='when', Acc, [T | ClAcc]);
split_clauses([T | Rest] = Tokens, HaveWhen, Acc, ClAcc) ->
    case check_clause(Tokens, HaveWhen) of
        false ->
            split_clauses(Rest, HaveWhen, Acc, [T | ClAcc]);
        true ->
            split_clauses(Rest, HaveWhen, [[T | ClAcc] | Acc], [])
    end.

%% fix_clause([#token{kind=atom, value=Name, line=Line, offset=Offset, length=Length} | Rest] = Code) ->
fix_clauses(Clauses) ->
    fix_clauses(Clauses, []).

fix_clauses([], Acc) ->
    lists:reverse(Acc);
fix_clauses([C | Rest], Acc) ->
    {Clause, Refs} = fix_clause(C),
    fix_clauses(Rest, [{Clause, Refs} | Acc]).

fix_clause([#token{kind=AtomOrMacro, value=Name, line=Line, offset=Offset, length=Length} | Rest]) 
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    #token{line=LastLine, offset=LastOffset, length=LastLength} = last_not_eof(Rest),
    PosLength = LastOffset - Offset + LastLength+1,
    ExternalRefs = get_refs_in_code(Rest),
    {#clause{pos={{Line, LastLine, Offset}, PosLength}, name_pos={{Line, Offset}, Length},
             name=Name, args=get_function_args(Rest), head=get_head(Rest)},
     ExternalRefs}.

get_function_args(Tokens) ->
    P = erlide_np_util:get_between_outer_pars(Tokens, '(', ')'),
    L = erlide_text:split_comma_list(P),
    [to_string(A) || A <- L].

fix_refs(ClausesAndRefs, Function, Imports) ->
    fix_refs(ClausesAndRefs, Function, Imports, []).

fix_refs([], #function{name=Name, arity=Arity, name_pos={{_, Offset}, Length}}, _Imports, Acc) ->
    Ref = #ref{data=#function_def{function=Name, arity=Arity}, offset=Offset, 
               length=Length, function=Name, arity=Arity, clause="", sub_clause=false},
    [Ref | Acc];
fix_refs([{Clause, Refs} | Rest], Function, Imports, Acc0) ->
    Acc1 = fix_refs(Refs, Clause, Function, Imports, Acc0),
    fix_refs(Rest, Function, Imports, Acc1).

fix_refs([], _Clause, _Function, _Imports, Acc) ->
    Acc;
fix_refs([{Offset, Length, RefData} | Rest], #clause{head=Head}=Clause, 
         #function{name=Name, arity=Arity, clauses=Clauses}=Function, 
         Imports, Acc) ->
    SubClause = case Clauses of
                    [] -> false;
                    [_] -> false;
                    _ -> true
                end,
    NewRefData = fix_imported_ref(RefData, Imports),
    Ref = #ref{data=NewRefData, offset=Offset, length=Length, function=Name, 
               arity=Arity, clause=Head, sub_clause=SubClause},
    fix_refs(Rest, Clause, Function, Imports, [Ref | Acc]).

fix_imported_ref(#local_call{function=F, arity=A}=RefData, [{Module, Imports} | Rest]) ->
    case lists:member({F, A}, Imports) of
        true -> #external_call{module=Module, function=F, arity=A};
        false -> fix_imported_ref(RefData, Rest)
    end;
fix_imported_ref(RefData, _Imports) ->
    RefData.

%% find references in code
%% VERY simple, M:F(_ ...), F(_ ...), #R, ?M, and that's it
%% returns {Offset, Length, Ref}, which is made into proper refs
%% by fix_refs
get_refs_in_code(L) ->
    get_refs_in_code(L, []).

get_refs_in_code([], Acc) ->
    lists:reverse(Acc);
get_refs_in_code([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
                  #token{kind=atom, value=F, offset=Offset2, length=Length2},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length2+Offset2-Offset, 
         #external_call{module = M, function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=atom, value=F, offset=Offset, length=Length},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length, #local_call{function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest], 
                 Acc) ->
    R = {Offset, Length, #macro_ref{macro=M}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind='#', offset=Offset}, 
                  #token{kind=atom, value=Record, offset=Offset2, length=Length2} | Rest],
                 Acc) ->
    R = {Offset, Length2+Offset2-Offset, #record_ref{record=Record}},
    {NewRest, Fields, RightSides} = erlide_np_records:check_fields(Rest, Record),
    NewAcc = get_refs_in_code(RightSides, Acc),
    get_refs_in_code(NewRest, Fields ++ [R | NewAcc]);
get_refs_in_code([#token{kind=var, value=V, offset=Offset, length=Length} | Rest],
                 Acc) ->
    R = {Offset, Length, make_var_def_ref(Acc, V)},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([_ | Rest], Acc) ->
    get_refs_in_code(Rest, Acc).

make_var_def_ref([], Var) ->
	#var_def{variable=Var};
make_var_def_ref([{_, _, #var_def{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([{_, _, #var_ref{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([_ | Rest], Var) ->
	make_var_def_ref(Rest, Var).

%% find type references in spec, type and opaque
%% also VERY simple, type(), ?M, #R, only
%% returns proper refs
get_refs(Tokens, Name, Arity) ->
	get_refs(Tokens, Name, Arity, []).

get_refs([], _, _, Acc) ->
    lists:reverse(Acc);
get_refs([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #macro_ref{macro=M}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind='#', offset=Offset},
          #token{kind=atom, value=R, offset=Offset2, length=Length2} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset, #record_ref{record=R}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
		  #token{kind=atom, value=T, offset=Offset2, length=Length2},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset,
                   #type_ref{module=M, type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=T, offset=Offset, length=Length},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #type_ref{module='_', type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=variable, value=V, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #var_ref{variable=V}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([_ | Rest], Name, Arity, Acc) ->
    get_refs(Rest, Name, Arity, Acc).

get_record_field_defs([], _) ->
    [];
get_record_field_defs([{FieldName, {{_, _, Offset}, Length}, _Extra} | Rest],
                      RecordName) ->
    [make_ref(Offset, Length, #record_field_def{record=RecordName, field=FieldName}, 
              FieldName, ?ARI_RECORD_FIELD_DEF)
         | get_record_field_defs(Rest, RecordName)].

make_ref(Offset, Length, Data, Name, Arity) ->
    #ref{data=Data, offset=Offset, length=Length, function=Name, arity=Arity, 
         clause="", sub_clause=false}.

make_attribute_arg_refs(define, Name, [#token{}, #token{kind='('} | Rest]) ->
    get_refs(skip_to_rparen(Rest), Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(define, Name, [#token{}, #token{kind=','} | Rest]) ->
    get_refs(Rest, Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(record, {Name, Fields}, [#token{}, #token{kind=','} | Rest]) ->
    get_record_field_defs(Fields, Name) ++ get_refs(Rest, Name, ?ARI_RECORD_DEF);
make_attribute_arg_refs(_, _, _) ->
    [].

skip_to_rparen([]) ->
    [];
skip_to_rparen([#token{kind=')'} | Rest]) ->
    Rest;
skip_to_rparen([_ | Rest]) ->
    skip_to_rparen(Rest).

make_attribute_ref(Name, Between, Extra, Offset, Length) ->
    ?D({Name, Between, Offset, Length}),
    case make_attribute_ref(Name, Between, Extra) of
        [] ->
            [];
        {Arity, AName, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=AName, 
                  arity=Arity, clause="", sub_clause=false}];
        {Arity, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=Name, 
                  arity=Arity, clause="", sub_clause=false}]
    end.

make_attribute_ref(module, _, Extra) ->
    {?ARI_ATTRIBUTE, #module_def{module=Extra}};
make_attribute_ref(record, Between, _E) ->
    ?D({Between, _E}),
    Name = case Between of
               {N, _} -> N;
               N -> N
           end,
    R= #record_def{record=Name},
    ?D(R),
    {?ARI_RECORD_DEF, Name, R};
make_attribute_ref(define, [Name | _], _) when is_list(Name) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(define, Name, _) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(include, _, Extra) ->
    {?ARI_INCLUDE, #include_ref{filename=unquote(Extra)}};
make_attribute_ref(_, _, _) ->
    [].

unquote(L) ->
    lists:reverse(unquote_first(lists:reverse(unquote_first(L)))).

unquote_first([$" | Rest]) ->
    Rest;
unquote_first(L) ->
    L.

remove_rest([], _) -> [];
remove_rest(Rest, Rest) -> [];
remove_rest([Hd | Tl], Rest) -> [Hd | remove_rest(Tl, Rest)].
%% Description: Split source files into functions and clauses and collects references
%% Author: jakob
%% Created: Mar 23, 2006

-module(erlide_noparse).

%%
%% Exported Functions
%%

%% called from Java
-export([initial_parse/5, reparse/1, remove_cache_files/2]).

%% called from Erlang
-export([read_module_refs/3]).

%%
%% Include files
%%

%% -define(DEBUG, 1).
%% -define(IO_FORMAT_DEBUG, 1).

-define(CACHE_VERSION, 25).
-define(SERVER, erlide_noparse).

-include("erlide.hrl").
-include("erlide_noparse.hrl").
-include("erlide_scanner.hrl").
-include("erlide_search_server.hrl").

%% special arity flags
-define(ARI_TYPESPEC, -2).
-define(ARI_ATTRIBUTE, -3).
-define(ARI_RECORD_DEF, -4).
-define(ARI_MACRO_DEF, -5).
-define(ARI_INCLUDE, -6).
-define(ARI_RECORD_FIELD_DEF, -7).

-define(X(X), erlide_log:log({X, erlang:now()})).


%%
%% API Functions
%%

initial_parse(ScannerName, ModuleFileName, StateDir, UseCache,
              UpdateSearchServer) ->
    try
        %%         ?D({StateDir, ModuleFileName}),
        BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
        RefsFileName = BaseName ++ ".refs",
        RenewFun = fun(_F) ->
                           do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer)
                   end,
        CacheFileName = BaseName ++ ".noparse",
        ?D(CacheFileName),
        {Cached, Res} = erlide_util:check_and_renew_cached(
                          ModuleFileName, CacheFileName, ?CACHE_VERSION, 
                          RenewFun, UseCache),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        ?D(updated),
        {ok, Res, Cached}
    catch
        error:Reason ->
            {error, Reason}
    end.

reparse(ScannerName) ->
    try
        Res = do_parse(ScannerName, "", "", true),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        {ok, Res, unused}
    catch
        error:Reason ->
            {error, Reason}
    end.

read_module_refs(ScannerName, ModulePath, StateDir) ->
    ?D(ScannerName),
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    RefsFileName = BaseName ++ ".refs",
    ?D(RefsFileName),
    case file:read_file(RefsFileName) of
        {ok, Binary} ->
            ?D(byte_size(Binary)),
            binary_to_term(Binary);
        _ ->
            ?D(ModulePath),
            {ok, InitialTextBin} = file:read_file(ModulePath),
            ?D(byte_size(InitialTextBin)),
            InitialText = binary_to_list(InitialTextBin),
            _D = erlide_scanner_server:initialScan(
                   ScannerName, ModulePath, InitialText, StateDir, true),
            ?D(_D),
            initial_parse(ScannerName, ModulePath, StateDir, true, false),
            ?D(RefsFileName),
            {ok, Binary} = file:read_file(RefsFileName),
            ?D(byte_size(Binary)),
            binary_to_term(Binary)
    end.

remove_cache_files(ScannerName, StateDir) ->
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    ScannerCacheFileName = BaseName ++ ".scan",
    file:delete(ScannerCacheFileName),
    RefsFileName = BaseName ++ ".refs",
    file:delete(RefsFileName),
    CacheFileName = BaseName ++ ".noparse",
    file:delete(CacheFileName).

%%
%% Internal functions
%%

do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer) ->
    Toks = erlide_scanner_server:getTokens(ScannerName),
    do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer).

do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer) ->
    ?D({do_parse, ScannerName, length(Toks)}),
    ?X(111),
    {UncommentToks, Comments} = erlide_np_util:extract_comments(Toks),
    ?X(222),
    ?D({length(UncommentToks), length(Comments)}),
    ?D({UncommentToks}),
    Functions = erlide_np_util:split_after_dots(UncommentToks),
    ?X(333),
    ?D(length(Functions)),
    AutoImports = erlide_util:add_auto_imported([]),
    ?X(444),
%%     ?D(AutoImports),
    {Collected, Refs} = classify_and_collect(Functions, [], [], [], AutoImports),
    ?X(555),
    ?D({'>>',length(Collected)}),
    CommentedCollected = erlide_np_util:get_function_comments(Collected, Comments),
    ?X(666),
    ?D(CommentedCollected),
    Model = #model{forms=CommentedCollected, comments=Comments},
    %%erlide_noparse_server:create(ScannerName, Model),
%%     ?D({"Model", length(Model#model.forms), erts_debug:flat_size(Model)}),
    FixedModel = fixup_model(Model),
    ?X(777),
%%     ?D(erts_debug:flat_size(FixedModel)),
    %% 	?D([erts_debug:flat_size(F) || F <- element(2, FixedModel)]),
    ?D(StateDir),
    case StateDir of
        "" -> ok;
        _ ->
            ?D({RefsFileName, length(Refs)}),
            file:write_file(RefsFileName, term_to_binary(Refs, [compressed]))
    end,
    update_search_server(UpdateSearchServer, ScannerName, Refs),
%%     ?D(FixedModel),
    ?X(888),
    FixedModel.

update_search_server(true, ScannerName, Refs) ->
    erlide_search_server:add_module_refs(ScannerName, Refs);
update_search_server(_, _, _) ->
    ok.

fixup_model(#model{forms=Forms, comments=Comments}) ->
    FixedComments = fixup_tokens(Comments),
    FixedForms = fixup_forms(Forms),
    #model{forms=FixedForms, comments=FixedComments}.

fixup_forms(Forms) ->
    [fixup_form(Form) || Form <- Forms].

fixup_tokens(Tokens) ->
    [fixup_token(Token) || Token <- Tokens].

fixup_token(#token{value=Value} = Token) when is_list(Value) ->
    Token#token{value=to_binary(Value)};
fixup_token(#token{text=Text} = Token) when is_list(Text) ->
    Token#token{value=to_binary(Text)};
fixup_token(Token) ->
    Token.

binary_args(Args) when is_list(Args) ->
    [iolist_to_binary(A) || A <- Args];
binary_args(_) ->
    [].

fixup_form(#function{comment=Comment, clauses=Clauses, args=Args} = Function) ->
    Function#function{comment= to_binary(Comment), clauses=fixup_forms(Clauses), args=binary_args(Args)};
fixup_form(#clause{head=Head, args=Args} = Clause) ->
    Clause#clause{head=to_binary(Head), args=binary_args(Args)};
fixup_form(Other) ->
    Other.

to_binary(Comment) when is_list(Comment) ->
    try 
        iolist_to_binary(Comment) 
    catch 
        _:_ -> 
            unicode:characters_to_binary(Comment) 
    end;
to_binary(Other) ->
    Other.

%% parse_test(ScannerName, File) ->
%%     erlide_scanner_server:scan_uncached(ScannerName, File, ""),
%%     Toks = erlide_scanner_server:getTokens(ScannerName),
%%     {UncommentToks, Comments} = extract_comments(Toks),
%%     Functions = split_after_dots(UncommentToks, [], []),
%%     {Collected, Refs} = classify_and_collect(Functions, [], []),
%%     _Model = #model{forms=Collected, comments=Comments},
%%     %%erlide_noparse_server:create(ScannerName, Model, ""),
%%     ok.

classify_and_collect([], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([eof], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([C | Rest], Acc, RefsAcc, Exports, Imports) ->
    {R, Refs, MoreExports, MoreImports} = cac(check_class(C), C, Exports, Imports),
    classify_and_collect(Rest, [R | Acc], Refs++RefsAcc, MoreExports++Exports, MoreImports++Imports).

cac(function, Tokens, Exports, Imports) ->
    ClauseList = split_clauses(Tokens),
    %?D(ClauseList),
    ClausesAndRefs = fix_clauses(ClauseList),
    %?D(length(Clauses)),
    [{#clause{pos=P, name=N, args=A, head=H, name_pos=NP}, _Refs} | _] = ClausesAndRefs,
    ?D(A),
    Arity = length(A),
    Exported = get_exported({N, Arity}, Exports),
    Function = case ClausesAndRefs of  	% only show subclauses when more than one
                   [_] ->
                       #function{pos=P, name=N, arity=Arity, args=A, head=H, 
                                 clauses=[], name_pos=NP, exported=Exported};
                   _ ->
                       Clauses = [C || {C, _} <- ClausesAndRefs],
                       #function{pos=P, name=N, arity=Arity, clauses=Clauses, 
                                 name_pos=NP, exported=Exported}
               end,
    {Function, fix_refs(ClausesAndRefs, Function, Imports), [], []};
cac(attribute, Attribute, _Exports, _Imports) ->
    ?D(Attribute),
    case Attribute of
        %% -spec, -type or -opaque
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=Kind, line=_Line, offset=_Offset, value=Name} | Args]
          when (Kind=:='spec') or ((Kind=:=atom) and (Name=:='type'))
                   or ((Kind=:=atom) and (Name=:='opaque'))->
            get_type_attribute(Kind, Name, Offset, Line, Attribute, Args);
        %% other attributes
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=atom, value=Name, line=_Line, offset=_Offset},
         _, #token{value=Args} | _] = Attribute ->
            get_other_attribute(Name, Offset, Line, Attribute, Args);
        [_, #token{kind=atom, value=Name, line=Line, offset=Offset} | _] ->
            #token{line=LastLine, offset=LastOffset, 
                   length=LastLength} = last_not_eof(Attribute),
            PosLength = LastOffset - Offset + LastLength,
            {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                       name=Name, args=[]}, [], [], []}
    end;
cac(other, [#token{value=Name, line=Line, offset=Offset, length=Length} | _], 
    _Exports, _Imports) ->
    {#other{pos={{Line, Line, Offset}, Length}, name=Name}, [], [], []};
cac(_, _D, _E, _I) ->
    {eof, [], [], []}.

get_type_attribute(Kind, Name0, Offset, Line, Attribute, Args) ->
    ?D(Name0),
    Name = case Kind of 'spec' -> Kind; _ -> Name0 end,
    #token{line=LastLine, offset=LastOffset,
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    ?D(Args),
    Extra = to_string(Args),
    ?D(Extra),
    {AttrArgs, _, _} = get_attribute_args(Kind, Args, Args),
    ?D({AttrArgs, Extra}),
    ExternalRefs = get_refs(tl(AttrArgs), Extra, ?ARI_TYPESPEC),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     [#ref{data=#type_def{type=Name}, offset=Offset, length=PosLength, function=Name, 
           arity=?ARI_TYPESPEC, clause="", sub_clause=false} | ExternalRefs], [], []}.

get_other_attribute(Name, Offset, Line, Attribute, Args) ->
    #token{line=LastLine, offset=LastOffset, 
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    Between = erlide_np_util:get_between_outer_pars(Attribute, '(', ')'),
    Extra = to_string(Between),
    {AttrArgs, Exports, Imports} = get_attribute_args(Name, Between, Args),
    ?D({AttrArgs, Between}),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     make_attribute_ref(Name, AttrArgs, Extra, Offset, PosLength)++
         make_attribute_arg_refs(Name, AttrArgs, Between),
     Exports, Imports}.

get_exported(F_A, Exports) ->
    lists:member(F_A, Exports).

get_attribute_args(import, Between, _Args) ->
    From = get_first_of_kind(atom, Between),
    Tokens = get_between(Between, '[', ']'),
    Import = {From, fun_arity_from_tokens(Tokens)},
    {Import, [], [Import]};
get_attribute_args(export, Between, _Args) ->
    Tokens = get_between(Between, '[', ']'),
    Exports = fun_arity_from_tokens(Tokens),
    {Exports, Exports, []};
get_attribute_args(record, Between, _Args) ->
    [RecordToken | _] = Between,
    RecordName = RecordToken#token.value,
    Tokens = erlide_np_util:get_between_outer_pars(Between, '{', '}'),
    R = {RecordName, field_list_from_tokens(Tokens)},
    {R, [], []};
get_attribute_args(_, _Between, Args) ->
    {Args, [], []}.

check_class([#token{kind = AtomOrMacro}, #token{kind = '('} | _])
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    function;
check_class([#token{kind = '-'}, #token{kind = atom} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'spec'} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'type'} | _]) ->
    attribute;
check_class(_X) ->
    %?D(lists:sublist(_X, 5)),
    other.

get_first_of_kind(Kind, Tokens) ->
    case erlide_np_util:skip_to(Tokens, Kind) of
        [#token{kind=Kind, value=Value} | _] ->
            Value;
        _ ->
            undefined
    end.
        
fun_arity_from_tokens([#token{kind=atom, value=Fun}, #token{kind='/'}, 
                       #token{kind=integer, value=Arity} | Rest]) ->
    [{Fun, Arity} | fun_arity_from_tokens(Rest)];
fun_arity_from_tokens([_ | Rest]) ->
    fun_arity_from_tokens(Rest);
fun_arity_from_tokens(_) ->
    [].

field_list_from_tokens([#token{kind=atom, value=Field, line=Line, 
                               offset=Offset, length=Length} | Rest]) ->
    {Extra, NewRest} = get_upto2(Rest, ','),
    [{Field, {{Line, Line, Offset}, Length}, to_string(Extra)}
         | field_list_from_tokens(NewRest)];
field_list_from_tokens([_ | Rest]) ->
    field_list_from_tokens(Rest);
field_list_from_tokens(_) ->
    [].

to_string(Tokens) ->
    S = erlide_scanner:tokens_to_string(Tokens),
    erlide_text:strip(S).
%%     unspacify(S).

get_head(T) ->
    A = get_args(T),
    case get_guards(T) of
        "" ->
            A;
        G ->
            A ++ " when " ++ G
    end.

get_args(T) ->
    case erlide_np_util:get_between_outer_pars(T, '(', ')') of
        [] ->
            "";
        P ->
            "("++to_string(P)++")"
    end.

get_guards([]) ->
    "";
get_guards(T) ->
    to_string(get_between(T, 'when', '->')). 

last_not_eof(L) -> 
    case lists:reverse(L) of
        [eof, Last | _] ->
            Last;
        [Last | _] ->
            Last;
        _ ->
            error
    end.    


get_between(L, A, B) ->
    R = get_upto(L, B),
    lists:reverse(get_upto(lists:reverse(R), A)).

get_upto(L, Delim) ->
        get_upto(L, Delim, []).

get_upto([], _Delim, _Acc) ->
    [];
get_upto([#token{kind=Delim} | _], Delim, Acc) ->
        lists:reverse(Acc);
get_upto([T | Rest], Delim, Acc) ->
    get_upto(Rest, Delim, [T | Acc]).

get_upto2(L, Delim) ->
    get_upto2(L, Delim, []).

get_upto2([], _Delim, Acc) ->
    {lists:reverse(Acc), []};
get_upto2([#token{kind=Delim} | Rest], Delim, Acc) ->
    {lists:reverse(Acc), Rest};
get_upto2([T | Rest], Delim, Acc) ->
    get_upto2(Rest, Delim, [T | Acc]).

check_clause([#token{kind = ';'} | Rest], false) ->
    check_class(Rest) == function;
check_clause(_, _) ->
    false.

split_clauses(F) ->
    split_clauses(F, false, [], []).

split_clauses([], _HaveWhen, Acc, []) ->
    erlide_util:reverse2(Acc);
split_clauses([], HaveWhen, Acc, ClAcc) ->
    split_clauses([], HaveWhen, [ClAcc | Acc], []);
split_clauses([#token{kind=Kind}=T | Rest], _HaveWhen, Acc, ClAcc) 
  when Kind=:='end'; Kind=:=dot; Kind=:='->'; Kind=:='when' ->
    split_clauses(Rest, Kind=:='when', Acc, [T | ClAcc]);
split_clauses([T | Rest] = Tokens, HaveWhen, Acc, ClAcc) ->
    case check_clause(Tokens, HaveWhen) of
        false ->
            split_clauses(Rest, HaveWhen, Acc, [T | ClAcc]);
        true ->
            split_clauses(Rest, HaveWhen, [[T | ClAcc] | Acc], [])
    end.

%% fix_clause([#token{kind=atom, value=Name, line=Line, offset=Offset, length=Length} | Rest] = Code) ->
fix_clauses(Clauses) ->
    fix_clauses(Clauses, []).

fix_clauses([], Acc) ->
    lists:reverse(Acc);
fix_clauses([C | Rest], Acc) ->
    {Clause, Refs} = fix_clause(C),
    fix_clauses(Rest, [{Clause, Refs} | Acc]).

fix_clause([#token{kind=AtomOrMacro, value=Name, line=Line, offset=Offset, length=Length} | Rest]) 
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    #token{line=LastLine, offset=LastOffset, length=LastLength} = last_not_eof(Rest),
    PosLength = LastOffset - Offset + LastLength+1,
    ExternalRefs = get_refs_in_code(Rest),
    {#clause{pos={{Line, LastLine, Offset}, PosLength}, name_pos={{Line, Offset}, Length},
             name=Name, args=get_function_args(Rest), head=get_head(Rest)},
     ExternalRefs}.

get_function_args(Tokens) ->
    P = erlide_np_util:get_between_outer_pars(Tokens, '(', ')'),
    L = erlide_text:split_comma_list(P),
    [to_string(A) || A <- L].

fix_refs(ClausesAndRefs, Function, Imports) ->
    fix_refs(ClausesAndRefs, Function, Imports, []).

fix_refs([], #function{name=Name, arity=Arity, name_pos={{_, Offset}, Length}}, _Imports, Acc) ->
    Ref = #ref{data=#function_def{function=Name, arity=Arity}, offset=Offset, 
               length=Length, function=Name, arity=Arity, clause="", sub_clause=false},
    [Ref | Acc];
fix_refs([{Clause, Refs} | Rest], Function, Imports, Acc0) ->
    Acc1 = fix_refs(Refs, Clause, Function, Imports, Acc0),
    fix_refs(Rest, Function, Imports, Acc1).

fix_refs([], _Clause, _Function, _Imports, Acc) ->
    Acc;
fix_refs([{Offset, Length, RefData} | Rest], #clause{head=Head}=Clause, 
         #function{name=Name, arity=Arity, clauses=Clauses}=Function, 
         Imports, Acc) ->
    SubClause = case Clauses of
                    [] -> false;
                    [_] -> false;
                    _ -> true
                end,
    NewRefData = fix_imported_ref(RefData, Imports),
    Ref = #ref{data=NewRefData, offset=Offset, length=Length, function=Name, 
               arity=Arity, clause=Head, sub_clause=SubClause},
    fix_refs(Rest, Clause, Function, Imports, [Ref | Acc]).

fix_imported_ref(#local_call{function=F, arity=A}=RefData, [{Module, Imports} | Rest]) ->
    case lists:member({F, A}, Imports) of
        true -> #external_call{module=Module, function=F, arity=A};
        false -> fix_imported_ref(RefData, Rest)
    end;
fix_imported_ref(RefData, _Imports) ->
    RefData.

%% find references in code
%% VERY simple, M:F(_ ...), F(_ ...), #R, ?M, and that's it
%% returns {Offset, Length, Ref}, which is made into proper refs
%% by fix_refs
get_refs_in_code(L) ->
    get_refs_in_code(L, []).

get_refs_in_code([], Acc) ->
    lists:reverse(Acc);
get_refs_in_code([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
                  #token{kind=atom, value=F, offset=Offset2, length=Length2},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length2+Offset2-Offset, 
         #external_call{module = M, function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=atom, value=F, offset=Offset, length=Length},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length, #local_call{function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest], 
                 Acc) ->
    R = {Offset, Length, #macro_ref{macro=M}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind='#', offset=Offset}, 
                  #token{kind=atom, value=Record, offset=Offset2, length=Length2} | Rest],
                 Acc) ->
    R = {Offset, Length2+Offset2-Offset, #record_ref{record=Record}},
    {NewRest, Fields, RightSides} = erlide_np_records:check_fields(Rest, Record),
    NewAcc = get_refs_in_code(RightSides, Acc),
    get_refs_in_code(NewRest, Fields ++ [R | NewAcc]);
get_refs_in_code([#token{kind=var, value=V, offset=Offset, length=Length} | Rest],
                 Acc) ->
    R = {Offset, Length, make_var_def_ref(Acc, V)},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([_ | Rest], Acc) ->
    get_refs_in_code(Rest, Acc).

make_var_def_ref([], Var) ->
	#var_def{variable=Var};
make_var_def_ref([{_, _, #var_def{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([{_, _, #var_ref{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([_ | Rest], Var) ->
	make_var_def_ref(Rest, Var).

%% find type references in spec, type and opaque
%% also VERY simple, type(), ?M, #R, only
%% returns proper refs
get_refs(Tokens, Name, Arity) ->
	get_refs(Tokens, Name, Arity, []).

get_refs([], _, _, Acc) ->
    lists:reverse(Acc);
get_refs([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #macro_ref{macro=M}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind='#', offset=Offset},
          #token{kind=atom, value=R, offset=Offset2, length=Length2} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset, #record_ref{record=R}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
		  #token{kind=atom, value=T, offset=Offset2, length=Length2},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset,
                   #type_ref{module=M, type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=T, offset=Offset, length=Length},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #type_ref{module='_', type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=variable, value=V, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #var_ref{variable=V}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([_ | Rest], Name, Arity, Acc) ->
    get_refs(Rest, Name, Arity, Acc).

get_record_field_defs([], _) ->
    [];
get_record_field_defs([{FieldName, {{_, _, Offset}, Length}, _Extra} | Rest],
                      RecordName) ->
    [make_ref(Offset, Length, #record_field_def{record=RecordName, field=FieldName}, 
              FieldName, ?ARI_RECORD_FIELD_DEF)
         | get_record_field_defs(Rest, RecordName)].

make_ref(Offset, Length, Data, Name, Arity) ->
    #ref{data=Data, offset=Offset, length=Length, function=Name, arity=Arity, 
         clause="", sub_clause=false}.

make_attribute_arg_refs(define, Name, [#token{}, #token{kind='('} | Rest]) ->
    get_refs(skip_to_rparen(Rest), Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(define, Name, [#token{}, #token{kind=','} | Rest]) ->
    get_refs(Rest, Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(record, {Name, Fields}, [#token{}, #token{kind=','} | Rest]) ->
    get_record_field_defs(Fields, Name) ++ get_refs(Rest, Name, ?ARI_RECORD_DEF);
make_attribute_arg_refs(_, _, _) ->
    [].

skip_to_rparen([]) ->
    [];
skip_to_rparen([#token{kind=')'} | Rest]) ->
    Rest;
skip_to_rparen([_ | Rest]) ->
    skip_to_rparen(Rest).

make_attribute_ref(Name, Between, Extra, Offset, Length) ->
    ?D({Name, Between, Offset, Length}),
    case make_attribute_ref(Name, Between, Extra) of
        [] ->
            [];
        {Arity, AName, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=AName, 
                  arity=Arity, clause="", sub_clause=false}];
        {Arity, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=Name, 
                  arity=Arity, clause="", sub_clause=false}]
    end.

make_attribute_ref(module, _, Extra) ->
    {?ARI_ATTRIBUTE, #module_def{module=Extra}};
make_attribute_ref(record, Between, _E) ->
    ?D({Between, _E}),
    Name = case Between of
               {N, _} -> N;
               N -> N
           end,
    R= #record_def{record=Name},
    ?D(R),
    {?ARI_RECORD_DEF, Name, R};
make_attribute_ref(define, [Name | _], _) when is_list(Name) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(define, Name, _) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(include, _, Extra) ->
    {?ARI_INCLUDE, #include_ref{filename=unquote(Extra)}};
make_attribute_ref(_, _, _) ->
    [].

unquote(L) ->
    lists:reverse(unquote_first(lists:reverse(unquote_first(L)))).

unquote_first([$" | Rest]) ->
    Rest;
unquote_first(L) ->
    L.

remove_rest([], _) -> [];
remove_rest(Rest, Rest) -> [];
remove_rest([Hd | Tl], Rest) -> [Hd | remove_rest(Tl, Rest)].
%% Description: Split source files into functions and clauses and collects references
%% Author: jakob
%% Created: Mar 23, 2006

-module(erlide_noparse).

%%
%% Exported Functions
%%

%% called from Java
-export([initial_parse/5, reparse/1, remove_cache_files/2]).

%% called from Erlang
-export([read_module_refs/3]).

%%
%% Include files
%%

%% -define(DEBUG, 1).
%% -define(IO_FORMAT_DEBUG, 1).

-define(CACHE_VERSION, 25).
-define(SERVER, erlide_noparse).

-include("erlide.hrl").
-include("erlide_noparse.hrl").
-include("erlide_scanner.hrl").
-include("erlide_search_server.hrl").

%% special arity flags
-define(ARI_TYPESPEC, -2).
-define(ARI_ATTRIBUTE, -3).
-define(ARI_RECORD_DEF, -4).
-define(ARI_MACRO_DEF, -5).
-define(ARI_INCLUDE, -6).
-define(ARI_RECORD_FIELD_DEF, -7).

-define(X(X), erlide_log:log({X, erlang:now()})).


%%
%% API Functions
%%

initial_parse(ScannerName, ModuleFileName, StateDir, UseCache,
              UpdateSearchServer) ->
    try
        %%         ?D({StateDir, ModuleFileName}),
        BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
        RefsFileName = BaseName ++ ".refs",
        RenewFun = fun(_F) ->
                           do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer)
                   end,
        CacheFileName = BaseName ++ ".noparse",
        ?D(CacheFileName),
        {Cached, Res} = erlide_util:check_and_renew_cached(
                          ModuleFileName, CacheFileName, ?CACHE_VERSION, 
                          RenewFun, UseCache),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        ?D(updated),
        {ok, Res, Cached}
    catch
        error:Reason ->
            {error, Reason}
    end.

reparse(ScannerName) ->
    try
        Res = do_parse(ScannerName, "", "", true),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        {ok, Res, unused}
    catch
        error:Reason ->
            {error, Reason}
    end.

read_module_refs(ScannerName, ModulePath, StateDir) ->
    ?D(ScannerName),
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    RefsFileName = BaseName ++ ".refs",
    ?D(RefsFileName),
    case file:read_file(RefsFileName) of
        {ok, Binary} ->
            ?D(byte_size(Binary)),
            binary_to_term(Binary);
        _ ->
            ?D(ModulePath),
            {ok, InitialTextBin} = file:read_file(ModulePath),
            ?D(byte_size(InitialTextBin)),
            InitialText = binary_to_list(InitialTextBin),
            _D = erlide_scanner_server:initialScan(
                   ScannerName, ModulePath, InitialText, StateDir, true),
            ?D(_D),
            initial_parse(ScannerName, ModulePath, StateDir, true, false),
            ?D(RefsFileName),
            {ok, Binary} = file:read_file(RefsFileName),
            ?D(byte_size(Binary)),
            binary_to_term(Binary)
    end.

remove_cache_files(ScannerName, StateDir) ->
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    ScannerCacheFileName = BaseName ++ ".scan",
    file:delete(ScannerCacheFileName),
    RefsFileName = BaseName ++ ".refs",
    file:delete(RefsFileName),
    CacheFileName = BaseName ++ ".noparse",
    file:delete(CacheFileName).

%%
%% Internal functions
%%

do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer) ->
    Toks = erlide_scanner_server:getTokens(ScannerName),
    do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer).

do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer) ->
    ?D({do_parse, ScannerName, length(Toks)}),
    ?X(111),
    {UncommentToks, Comments} = erlide_np_util:extract_comments(Toks),
    ?X(222),
    ?D({length(UncommentToks), length(Comments)}),
    ?D({UncommentToks}),
    Functions = erlide_np_util:split_after_dots(UncommentToks),
    ?X(333),
    ?D(length(Functions)),
    AutoImports = erlide_util:add_auto_imported([]),
    ?X(444),
%%     ?D(AutoImports),
    {Collected, Refs} = classify_and_collect(Functions, [], [], [], AutoImports),
    ?X(555),
    ?D({'>>',length(Collected)}),
    CommentedCollected = erlide_np_util:get_function_comments(Collected, Comments),
    ?X(666),
    ?D(CommentedCollected),
    Model = #model{forms=CommentedCollected, comments=Comments},
    %%erlide_noparse_server:create(ScannerName, Model),
%%     ?D({"Model", length(Model#model.forms), erts_debug:flat_size(Model)}),
    FixedModel = fixup_model(Model),
    ?X(777),
%%     ?D(erts_debug:flat_size(FixedModel)),
    %% 	?D([erts_debug:flat_size(F) || F <- element(2, FixedModel)]),
    ?D(StateDir),
    case StateDir of
        "" -> ok;
        _ ->
            ?D({RefsFileName, length(Refs)}),
            file:write_file(RefsFileName, term_to_binary(Refs, [compressed]))
    end,
    update_search_server(UpdateSearchServer, ScannerName, Refs),
%%     ?D(FixedModel),
    ?X(888),
    FixedModel.

update_search_server(true, ScannerName, Refs) ->
    erlide_search_server:add_module_refs(ScannerName, Refs);
update_search_server(_, _, _) ->
    ok.

fixup_model(#model{forms=Forms, comments=Comments}) ->
    FixedComments = fixup_tokens(Comments),
    FixedForms = fixup_forms(Forms),
    #model{forms=FixedForms, comments=FixedComments}.

fixup_forms(Forms) ->
    [fixup_form(Form) || Form <- Forms].

fixup_tokens(Tokens) ->
    [fixup_token(Token) || Token <- Tokens].

fixup_token(#token{value=Value} = Token) when is_list(Value) ->
    Token#token{value=to_binary(Value)};
fixup_token(#token{text=Text} = Token) when is_list(Text) ->
    Token#token{value=to_binary(Text)};
fixup_token(Token) ->
    Token.

binary_args(Args) when is_list(Args) ->
    [iolist_to_binary(A) || A <- Args];
binary_args(_) ->
    [].

fixup_form(#function{comment=Comment, clauses=Clauses, args=Args} = Function) ->
    Function#function{comment= to_binary(Comment), clauses=fixup_forms(Clauses), args=binary_args(Args)};
fixup_form(#clause{head=Head, args=Args} = Clause) ->
    Clause#clause{head=to_binary(Head), args=binary_args(Args)};
fixup_form(Other) ->
    Other.

to_binary(Comment) when is_list(Comment) ->
    try 
        iolist_to_binary(Comment) 
    catch 
        _:_ -> 
            unicode:characters_to_binary(Comment) 
    end;
to_binary(Other) ->
    Other.

%% parse_test(ScannerName, File) ->
%%     erlide_scanner_server:scan_uncached(ScannerName, File, ""),
%%     Toks = erlide_scanner_server:getTokens(ScannerName),
%%     {UncommentToks, Comments} = extract_comments(Toks),
%%     Functions = split_after_dots(UncommentToks, [], []),
%%     {Collected, Refs} = classify_and_collect(Functions, [], []),
%%     _Model = #model{forms=Collected, comments=Comments},
%%     %%erlide_noparse_server:create(ScannerName, Model, ""),
%%     ok.

classify_and_collect([], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([eof], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([C | Rest], Acc, RefsAcc, Exports, Imports) ->
    {R, Refs, MoreExports, MoreImports} = cac(check_class(C), C, Exports, Imports),
    classify_and_collect(Rest, [R | Acc], Refs++RefsAcc, MoreExports++Exports, MoreImports++Imports).

cac(function, Tokens, Exports, Imports) ->
    ClauseList = split_clauses(Tokens),
    %?D(ClauseList),
    ClausesAndRefs = fix_clauses(ClauseList),
    %?D(length(Clauses)),
    [{#clause{pos=P, name=N, args=A, head=H, name_pos=NP}, _Refs} | _] = ClausesAndRefs,
    ?D(A),
    Arity = length(A),
    Exported = get_exported({N, Arity}, Exports),
    Function = case ClausesAndRefs of  	% only show subclauses when more than one
                   [_] ->
                       #function{pos=P, name=N, arity=Arity, args=A, head=H, 
                                 clauses=[], name_pos=NP, exported=Exported};
                   _ ->
                       Clauses = [C || {C, _} <- ClausesAndRefs],
                       #function{pos=P, name=N, arity=Arity, clauses=Clauses, 
                                 name_pos=NP, exported=Exported}
               end,
    {Function, fix_refs(ClausesAndRefs, Function, Imports), [], []};
cac(attribute, Attribute, _Exports, _Imports) ->
    ?D(Attribute),
    case Attribute of
        %% -spec, -type or -opaque
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=Kind, line=_Line, offset=_Offset, value=Name} | Args]
          when (Kind=:='spec') or ((Kind=:=atom) and (Name=:='type'))
                   or ((Kind=:=atom) and (Name=:='opaque'))->
            get_type_attribute(Kind, Name, Offset, Line, Attribute, Args);
        %% other attributes
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=atom, value=Name, line=_Line, offset=_Offset},
         _, #token{value=Args} | _] = Attribute ->
            get_other_attribute(Name, Offset, Line, Attribute, Args);
        [_, #token{kind=atom, value=Name, line=Line, offset=Offset} | _] ->
            #token{line=LastLine, offset=LastOffset, 
                   length=LastLength} = last_not_eof(Attribute),
            PosLength = LastOffset - Offset + LastLength,
            {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                       name=Name, args=[]}, [], [], []}
    end;
cac(other, [#token{value=Name, line=Line, offset=Offset, length=Length} | _], 
    _Exports, _Imports) ->
    {#other{pos={{Line, Line, Offset}, Length}, name=Name}, [], [], []};
cac(_, _D, _E, _I) ->
    {eof, [], [], []}.

get_type_attribute(Kind, Name0, Offset, Line, Attribute, Args) ->
    ?D(Name0),
    Name = case Kind of 'spec' -> Kind; _ -> Name0 end,
    #token{line=LastLine, offset=LastOffset,
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    ?D(Args),
    Extra = to_string(Args),
    ?D(Extra),
    {AttrArgs, _, _} = get_attribute_args(Kind, Args, Args),
    ?D({AttrArgs, Extra}),
    ExternalRefs = get_refs(tl(AttrArgs), Extra, ?ARI_TYPESPEC),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     [#ref{data=#type_def{type=Name}, offset=Offset, length=PosLength, function=Name, 
           arity=?ARI_TYPESPEC, clause="", sub_clause=false} | ExternalRefs], [], []}.

get_other_attribute(Name, Offset, Line, Attribute, Args) ->
    #token{line=LastLine, offset=LastOffset, 
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    Between = erlide_np_util:get_between_outer_pars(Attribute, '(', ')'),
    Extra = to_string(Between),
    {AttrArgs, Exports, Imports} = get_attribute_args(Name, Between, Args),
    ?D({AttrArgs, Between}),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     make_attribute_ref(Name, AttrArgs, Extra, Offset, PosLength)++
         make_attribute_arg_refs(Name, AttrArgs, Between),
     Exports, Imports}.

get_exported(F_A, Exports) ->
    lists:member(F_A, Exports).

get_attribute_args(import, Between, _Args) ->
    From = get_first_of_kind(atom, Between),
    Tokens = get_between(Between, '[', ']'),
    Import = {From, fun_arity_from_tokens(Tokens)},
    {Import, [], [Import]};
get_attribute_args(export, Between, _Args) ->
    Tokens = get_between(Between, '[', ']'),
    Exports = fun_arity_from_tokens(Tokens),
    {Exports, Exports, []};
get_attribute_args(record, Between, _Args) ->
    [RecordToken | _] = Between,
    RecordName = RecordToken#token.value,
    Tokens = erlide_np_util:get_between_outer_pars(Between, '{', '}'),
    R = {RecordName, field_list_from_tokens(Tokens)},
    {R, [], []};
get_attribute_args(_, _Between, Args) ->
    {Args, [], []}.

check_class([#token{kind = AtomOrMacro}, #token{kind = '('} | _])
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    function;
check_class([#token{kind = '-'}, #token{kind = atom} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'spec'} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'type'} | _]) ->
    attribute;
check_class(_X) ->
    %?D(lists:sublist(_X, 5)),
    other.

get_first_of_kind(Kind, Tokens) ->
    case erlide_np_util:skip_to(Tokens, Kind) of
        [#token{kind=Kind, value=Value} | _] ->
            Value;
        _ ->
            undefined
    end.
        
fun_arity_from_tokens([#token{kind=atom, value=Fun}, #token{kind='/'}, 
                       #token{kind=integer, value=Arity} | Rest]) ->
    [{Fun, Arity} | fun_arity_from_tokens(Rest)];
fun_arity_from_tokens([_ | Rest]) ->
    fun_arity_from_tokens(Rest);
fun_arity_from_tokens(_) ->
    [].

field_list_from_tokens([#token{kind=atom, value=Field, line=Line, 
                               offset=Offset, length=Length} | Rest]) ->
    {Extra, NewRest} = get_upto2(Rest, ','),
    [{Field, {{Line, Line, Offset}, Length}, to_string(Extra)}
         | field_list_from_tokens(NewRest)];
field_list_from_tokens([_ | Rest]) ->
    field_list_from_tokens(Rest);
field_list_from_tokens(_) ->
    [].

to_string(Tokens) ->
    S = erlide_scanner:tokens_to_string(Tokens),
    erlide_text:strip(S).
%%     unspacify(S).

get_head(T) ->
    A = get_args(T),
    case get_guards(T) of
        "" ->
            A;
        G ->
            A ++ " when " ++ G
    end.

get_args(T) ->
    case erlide_np_util:get_between_outer_pars(T, '(', ')') of
        [] ->
            "";
        P ->
            "("++to_string(P)++")"
    end.

get_guards([]) ->
    "";
get_guards(T) ->
    to_string(get_between(T, 'when', '->')). 

last_not_eof(L) -> 
    case lists:reverse(L) of
        [eof, Last | _] ->
            Last;
        [Last | _] ->
            Last;
        _ ->
            error
    end.    


get_between(L, A, B) ->
    R = get_upto(L, B),
    lists:reverse(get_upto(lists:reverse(R), A)).

get_upto(L, Delim) ->
        get_upto(L, Delim, []).

get_upto([], _Delim, _Acc) ->
    [];
get_upto([#token{kind=Delim} | _], Delim, Acc) ->
        lists:reverse(Acc);
get_upto([T | Rest], Delim, Acc) ->
    get_upto(Rest, Delim, [T | Acc]).

get_upto2(L, Delim) ->
    get_upto2(L, Delim, []).

get_upto2([], _Delim, Acc) ->
    {lists:reverse(Acc), []};
get_upto2([#token{kind=Delim} | Rest], Delim, Acc) ->
    {lists:reverse(Acc), Rest};
get_upto2([T | Rest], Delim, Acc) ->
    get_upto2(Rest, Delim, [T | Acc]).

check_clause([#token{kind = ';'} | Rest], false) ->
    check_class(Rest) == function;
check_clause(_, _) ->
    false.

split_clauses(F) ->
    split_clauses(F, false, [], []).

split_clauses([], _HaveWhen, Acc, []) ->
    erlide_util:reverse2(Acc);
split_clauses([], HaveWhen, Acc, ClAcc) ->
    split_clauses([], HaveWhen, [ClAcc | Acc], []);
split_clauses([#token{kind=Kind}=T | Rest], _HaveWhen, Acc, ClAcc) 
  when Kind=:='end'; Kind=:=dot; Kind=:='->'; Kind=:='when' ->
    split_clauses(Rest, Kind=:='when', Acc, [T | ClAcc]);
split_clauses([T | Rest] = Tokens, HaveWhen, Acc, ClAcc) ->
    case check_clause(Tokens, HaveWhen) of
        false ->
            split_clauses(Rest, HaveWhen, Acc, [T | ClAcc]);
        true ->
            split_clauses(Rest, HaveWhen, [[T | ClAcc] | Acc], [])
    end.

%% fix_clause([#token{kind=atom, value=Name, line=Line, offset=Offset, length=Length} | Rest] = Code) ->
fix_clauses(Clauses) ->
    fix_clauses(Clauses, []).

fix_clauses([], Acc) ->
    lists:reverse(Acc);
fix_clauses([C | Rest], Acc) ->
    {Clause, Refs} = fix_clause(C),
    fix_clauses(Rest, [{Clause, Refs} | Acc]).

fix_clause([#token{kind=AtomOrMacro, value=Name, line=Line, offset=Offset, length=Length} | Rest]) 
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    #token{line=LastLine, offset=LastOffset, length=LastLength} = last_not_eof(Rest),
    PosLength = LastOffset - Offset + LastLength+1,
    ExternalRefs = get_refs_in_code(Rest),
    {#clause{pos={{Line, LastLine, Offset}, PosLength}, name_pos={{Line, Offset}, Length},
             name=Name, args=get_function_args(Rest), head=get_head(Rest)},
     ExternalRefs}.

get_function_args(Tokens) ->
    P = erlide_np_util:get_between_outer_pars(Tokens, '(', ')'),
    L = erlide_text:split_comma_list(P),
    [to_string(A) || A <- L].

fix_refs(ClausesAndRefs, Function, Imports) ->
    fix_refs(ClausesAndRefs, Function, Imports, []).

fix_refs([], #function{name=Name, arity=Arity, name_pos={{_, Offset}, Length}}, _Imports, Acc) ->
    Ref = #ref{data=#function_def{function=Name, arity=Arity}, offset=Offset, 
               length=Length, function=Name, arity=Arity, clause="", sub_clause=false},
    [Ref | Acc];
fix_refs([{Clause, Refs} | Rest], Function, Imports, Acc0) ->
    Acc1 = fix_refs(Refs, Clause, Function, Imports, Acc0),
    fix_refs(Rest, Function, Imports, Acc1).

fix_refs([], _Clause, _Function, _Imports, Acc) ->
    Acc;
fix_refs([{Offset, Length, RefData} | Rest], #clause{head=Head}=Clause, 
         #function{name=Name, arity=Arity, clauses=Clauses}=Function, 
         Imports, Acc) ->
    SubClause = case Clauses of
                    [] -> false;
                    [_] -> false;
                    _ -> true
                end,
    NewRefData = fix_imported_ref(RefData, Imports),
    Ref = #ref{data=NewRefData, offset=Offset, length=Length, function=Name, 
               arity=Arity, clause=Head, sub_clause=SubClause},
    fix_refs(Rest, Clause, Function, Imports, [Ref | Acc]).

fix_imported_ref(#local_call{function=F, arity=A}=RefData, [{Module, Imports} | Rest]) ->
    case lists:member({F, A}, Imports) of
        true -> #external_call{module=Module, function=F, arity=A};
        false -> fix_imported_ref(RefData, Rest)
    end;
fix_imported_ref(RefData, _Imports) ->
    RefData.

%% find references in code
%% VERY simple, M:F(_ ...), F(_ ...), #R, ?M, and that's it
%% returns {Offset, Length, Ref}, which is made into proper refs
%% by fix_refs
get_refs_in_code(L) ->
    get_refs_in_code(L, []).

get_refs_in_code([], Acc) ->
    lists:reverse(Acc);
get_refs_in_code([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
                  #token{kind=atom, value=F, offset=Offset2, length=Length2},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length2+Offset2-Offset, 
         #external_call{module = M, function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=atom, value=F, offset=Offset, length=Length},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length, #local_call{function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest], 
                 Acc) ->
    R = {Offset, Length, #macro_ref{macro=M}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind='#', offset=Offset}, 
                  #token{kind=atom, value=Record, offset=Offset2, length=Length2} | Rest],
                 Acc) ->
    R = {Offset, Length2+Offset2-Offset, #record_ref{record=Record}},
    {NewRest, Fields, RightSides} = erlide_np_records:check_fields(Rest, Record),
    NewAcc = get_refs_in_code(RightSides, Acc),
    get_refs_in_code(NewRest, Fields ++ [R | NewAcc]);
get_refs_in_code([#token{kind=var, value=V, offset=Offset, length=Length} | Rest],
                 Acc) ->
    R = {Offset, Length, make_var_def_ref(Acc, V)},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([_ | Rest], Acc) ->
    get_refs_in_code(Rest, Acc).

make_var_def_ref([], Var) ->
	#var_def{variable=Var};
make_var_def_ref([{_, _, #var_def{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([{_, _, #var_ref{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([_ | Rest], Var) ->
	make_var_def_ref(Rest, Var).

%% find type references in spec, type and opaque
%% also VERY simple, type(), ?M, #R, only
%% returns proper refs
get_refs(Tokens, Name, Arity) ->
	get_refs(Tokens, Name, Arity, []).

get_refs([], _, _, Acc) ->
    lists:reverse(Acc);
get_refs([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #macro_ref{macro=M}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind='#', offset=Offset},
          #token{kind=atom, value=R, offset=Offset2, length=Length2} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset, #record_ref{record=R}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
		  #token{kind=atom, value=T, offset=Offset2, length=Length2},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset,
                   #type_ref{module=M, type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=T, offset=Offset, length=Length},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #type_ref{module='_', type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=variable, value=V, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #var_ref{variable=V}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([_ | Rest], Name, Arity, Acc) ->
    get_refs(Rest, Name, Arity, Acc).

get_record_field_defs([], _) ->
    [];
get_record_field_defs([{FieldName, {{_, _, Offset}, Length}, _Extra} | Rest],
                      RecordName) ->
    [make_ref(Offset, Length, #record_field_def{record=RecordName, field=FieldName}, 
              FieldName, ?ARI_RECORD_FIELD_DEF)
         | get_record_field_defs(Rest, RecordName)].

make_ref(Offset, Length, Data, Name, Arity) ->
    #ref{data=Data, offset=Offset, length=Length, function=Name, arity=Arity, 
         clause="", sub_clause=false}.

make_attribute_arg_refs(define, Name, [#token{}, #token{kind='('} | Rest]) ->
    get_refs(skip_to_rparen(Rest), Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(define, Name, [#token{}, #token{kind=','} | Rest]) ->
    get_refs(Rest, Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(record, {Name, Fields}, [#token{}, #token{kind=','} | Rest]) ->
    get_record_field_defs(Fields, Name) ++ get_refs(Rest, Name, ?ARI_RECORD_DEF);
make_attribute_arg_refs(_, _, _) ->
    [].

skip_to_rparen([]) ->
    [];
skip_to_rparen([#token{kind=')'} | Rest]) ->
    Rest;
skip_to_rparen([_ | Rest]) ->
    skip_to_rparen(Rest).

make_attribute_ref(Name, Between, Extra, Offset, Length) ->
    ?D({Name, Between, Offset, Length}),
    case make_attribute_ref(Name, Between, Extra) of
        [] ->
            [];
        {Arity, AName, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=AName, 
                  arity=Arity, clause="", sub_clause=false}];
        {Arity, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=Name, 
                  arity=Arity, clause="", sub_clause=false}]
    end.

make_attribute_ref(module, _, Extra) ->
    {?ARI_ATTRIBUTE, #module_def{module=Extra}};
make_attribute_ref(record, Between, _E) ->
    ?D({Between, _E}),
    Name = case Between of
               {N, _} -> N;
               N -> N
           end,
    R= #record_def{record=Name},
    ?D(R),
    {?ARI_RECORD_DEF, Name, R};
make_attribute_ref(define, [Name | _], _) when is_list(Name) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(define, Name, _) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(include, _, Extra) ->
    {?ARI_INCLUDE, #include_ref{filename=unquote(Extra)}};
make_attribute_ref(_, _, _) ->
    [].

unquote(L) ->
    lists:reverse(unquote_first(lists:reverse(unquote_first(L)))).

unquote_first([$" | Rest]) ->
    Rest;
unquote_first(L) ->
    L.

remove_rest([], _) -> [];
remove_rest(Rest, Rest) -> [];
remove_rest([Hd | Tl], Rest) -> [Hd | remove_rest(Tl, Rest)].
%% Description: Split source files into functions and clauses and collects references
%% Author: jakob
%% Created: Mar 23, 2006

-module(erlide_noparse).

%%
%% Exported Functions
%%

%% called from Java
-export([initial_parse/5, reparse/1, remove_cache_files/2]).

%% called from Erlang
-export([read_module_refs/3]).

%%
%% Include files
%%

%% -define(DEBUG, 1).
%% -define(IO_FORMAT_DEBUG, 1).

-define(CACHE_VERSION, 25).
-define(SERVER, erlide_noparse).

-include("erlide.hrl").
-include("erlide_noparse.hrl").
-include("erlide_scanner.hrl").
-include("erlide_search_server.hrl").

%% special arity flags
-define(ARI_TYPESPEC, -2).
-define(ARI_ATTRIBUTE, -3).
-define(ARI_RECORD_DEF, -4).
-define(ARI_MACRO_DEF, -5).
-define(ARI_INCLUDE, -6).
-define(ARI_RECORD_FIELD_DEF, -7).

-define(X(X), erlide_log:log({X, erlang:now()})).


%%
%% API Functions
%%

initial_parse(ScannerName, ModuleFileName, StateDir, UseCache,
              UpdateSearchServer) ->
    try
        %%         ?D({StateDir, ModuleFileName}),
        BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
        RefsFileName = BaseName ++ ".refs",
        RenewFun = fun(_F) ->
                           do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer)
                   end,
        CacheFileName = BaseName ++ ".noparse",
        ?D(CacheFileName),
        {Cached, Res} = erlide_util:check_and_renew_cached(
                          ModuleFileName, CacheFileName, ?CACHE_VERSION, 
                          RenewFun, UseCache),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        ?D(updated),
        {ok, Res, Cached}
    catch
        error:Reason ->
            {error, Reason}
    end.

reparse(ScannerName) ->
    try
        Res = do_parse(ScannerName, "", "", true),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        {ok, Res, unused}
    catch
        error:Reason ->
            {error, Reason}
    end.

read_module_refs(ScannerName, ModulePath, StateDir) ->
    ?D(ScannerName),
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    RefsFileName = BaseName ++ ".refs",
    ?D(RefsFileName),
    case file:read_file(RefsFileName) of
        {ok, Binary} ->
            ?D(byte_size(Binary)),
            binary_to_term(Binary);
        _ ->
            ?D(ModulePath),
            {ok, InitialTextBin} = file:read_file(ModulePath),
            ?D(byte_size(InitialTextBin)),
            InitialText = binary_to_list(InitialTextBin),
            _D = erlide_scanner_server:initialScan(
                   ScannerName, ModulePath, InitialText, StateDir, true),
            ?D(_D),
            initial_parse(ScannerName, ModulePath, StateDir, true, false),
            ?D(RefsFileName),
            {ok, Binary} = file:read_file(RefsFileName),
            ?D(byte_size(Binary)),
            binary_to_term(Binary)
    end.

remove_cache_files(ScannerName, StateDir) ->
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    ScannerCacheFileName = BaseName ++ ".scan",
    file:delete(ScannerCacheFileName),
    RefsFileName = BaseName ++ ".refs",
    file:delete(RefsFileName),
    CacheFileName = BaseName ++ ".noparse",
    file:delete(CacheFileName).

%%
%% Internal functions
%%

do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer) ->
    Toks = erlide_scanner_server:getTokens(ScannerName),
    do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer).

do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer) ->
    ?D({do_parse, ScannerName, length(Toks)}),
    ?X(111),
    {UncommentToks, Comments} = erlide_np_util:extract_comments(Toks),
    ?X(222),
    ?D({length(UncommentToks), length(Comments)}),
    ?D({UncommentToks}),
    Functions = erlide_np_util:split_after_dots(UncommentToks),
    ?X(333),
    ?D(length(Functions)),
    AutoImports = erlide_util:add_auto_imported([]),
    ?X(444),
%%     ?D(AutoImports),
    {Collected, Refs} = classify_and_collect(Functions, [], [], [], AutoImports),
    ?X(555),
    ?D({'>>',length(Collected)}),
    CommentedCollected = erlide_np_util:get_function_comments(Collected, Comments),
    ?X(666),
    ?D(CommentedCollected),
    Model = #model{forms=CommentedCollected, comments=Comments},
    %%erlide_noparse_server:create(ScannerName, Model),
%%     ?D({"Model", length(Model#model.forms), erts_debug:flat_size(Model)}),
    FixedModel = fixup_model(Model),
    ?X(777),
%%     ?D(erts_debug:flat_size(FixedModel)),
    %% 	?D([erts_debug:flat_size(F) || F <- element(2, FixedModel)]),
    ?D(StateDir),
    case StateDir of
        "" -> ok;
        _ ->
            ?D({RefsFileName, length(Refs)}),
            file:write_file(RefsFileName, term_to_binary(Refs, [compressed]))
    end,
    update_search_server(UpdateSearchServer, ScannerName, Refs),
%%     ?D(FixedModel),
    ?X(888),
    FixedModel.

update_search_server(true, ScannerName, Refs) ->
    erlide_search_server:add_module_refs(ScannerName, Refs);
update_search_server(_, _, _) ->
    ok.

fixup_model(#model{forms=Forms, comments=Comments}) ->
    FixedComments = fixup_tokens(Comments),
    FixedForms = fixup_forms(Forms),
    #model{forms=FixedForms, comments=FixedComments}.

fixup_forms(Forms) ->
    [fixup_form(Form) || Form <- Forms].

fixup_tokens(Tokens) ->
    [fixup_token(Token) || Token <- Tokens].

fixup_token(#token{value=Value} = Token) when is_list(Value) ->
    Token#token{value=to_binary(Value)};
fixup_token(#token{text=Text} = Token) when is_list(Text) ->
    Token#token{value=to_binary(Text)};
fixup_token(Token) ->
    Token.

binary_args(Args) when is_list(Args) ->
    [iolist_to_binary(A) || A <- Args];
binary_args(_) ->
    [].

fixup_form(#function{comment=Comment, clauses=Clauses, args=Args} = Function) ->
    Function#function{comment= to_binary(Comment), clauses=fixup_forms(Clauses), args=binary_args(Args)};
fixup_form(#clause{head=Head, args=Args} = Clause) ->
    Clause#clause{head=to_binary(Head), args=binary_args(Args)};
fixup_form(Other) ->
    Other.

to_binary(Comment) when is_list(Comment) ->
    try 
        iolist_to_binary(Comment) 
    catch 
        _:_ -> 
            unicode:characters_to_binary(Comment) 
    end;
to_binary(Other) ->
    Other.

%% parse_test(ScannerName, File) ->
%%     erlide_scanner_server:scan_uncached(ScannerName, File, ""),
%%     Toks = erlide_scanner_server:getTokens(ScannerName),
%%     {UncommentToks, Comments} = extract_comments(Toks),
%%     Functions = split_after_dots(UncommentToks, [], []),
%%     {Collected, Refs} = classify_and_collect(Functions, [], []),
%%     _Model = #model{forms=Collected, comments=Comments},
%%     %%erlide_noparse_server:create(ScannerName, Model, ""),
%%     ok.

classify_and_collect([], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([eof], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([C | Rest], Acc, RefsAcc, Exports, Imports) ->
    {R, Refs, MoreExports, MoreImports} = cac(check_class(C), C, Exports, Imports),
    classify_and_collect(Rest, [R | Acc], Refs++RefsAcc, MoreExports++Exports, MoreImports++Imports).

cac(function, Tokens, Exports, Imports) ->
    ClauseList = split_clauses(Tokens),
    %?D(ClauseList),
    ClausesAndRefs = fix_clauses(ClauseList),
    %?D(length(Clauses)),
    [{#clause{pos=P, name=N, args=A, head=H, name_pos=NP}, _Refs} | _] = ClausesAndRefs,
    ?D(A),
    Arity = length(A),
    Exported = get_exported({N, Arity}, Exports),
    Function = case ClausesAndRefs of  	% only show subclauses when more than one
                   [_] ->
                       #function{pos=P, name=N, arity=Arity, args=A, head=H, 
                                 clauses=[], name_pos=NP, exported=Exported};
                   _ ->
                       Clauses = [C || {C, _} <- ClausesAndRefs],
                       #function{pos=P, name=N, arity=Arity, clauses=Clauses, 
                                 name_pos=NP, exported=Exported}
               end,
    {Function, fix_refs(ClausesAndRefs, Function, Imports), [], []};
cac(attribute, Attribute, _Exports, _Imports) ->
    ?D(Attribute),
    case Attribute of
        %% -spec, -type or -opaque
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=Kind, line=_Line, offset=_Offset, value=Name} | Args]
          when (Kind=:='spec') or ((Kind=:=atom) and (Name=:='type'))
                   or ((Kind=:=atom) and (Name=:='opaque'))->
            get_type_attribute(Kind, Name, Offset, Line, Attribute, Args);
        %% other attributes
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=atom, value=Name, line=_Line, offset=_Offset},
         _, #token{value=Args} | _] = Attribute ->
            get_other_attribute(Name, Offset, Line, Attribute, Args);
        [_, #token{kind=atom, value=Name, line=Line, offset=Offset} | _] ->
            #token{line=LastLine, offset=LastOffset, 
                   length=LastLength} = last_not_eof(Attribute),
            PosLength = LastOffset - Offset + LastLength,
            {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                       name=Name, args=[]}, [], [], []}
    end;
cac(other, [#token{value=Name, line=Line, offset=Offset, length=Length} | _], 
    _Exports, _Imports) ->
    {#other{pos={{Line, Line, Offset}, Length}, name=Name}, [], [], []};
cac(_, _D, _E, _I) ->
    {eof, [], [], []}.

get_type_attribute(Kind, Name0, Offset, Line, Attribute, Args) ->
    ?D(Name0),
    Name = case Kind of 'spec' -> Kind; _ -> Name0 end,
    #token{line=LastLine, offset=LastOffset,
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    ?D(Args),
    Extra = to_string(Args),
    ?D(Extra),
    {AttrArgs, _, _} = get_attribute_args(Kind, Args, Args),
    ?D({AttrArgs, Extra}),
    ExternalRefs = get_refs(tl(AttrArgs), Extra, ?ARI_TYPESPEC),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     [#ref{data=#type_def{type=Name}, offset=Offset, length=PosLength, function=Name, 
           arity=?ARI_TYPESPEC, clause="", sub_clause=false} | ExternalRefs], [], []}.

get_other_attribute(Name, Offset, Line, Attribute, Args) ->
    #token{line=LastLine, offset=LastOffset, 
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    Between = erlide_np_util:get_between_outer_pars(Attribute, '(', ')'),
    Extra = to_string(Between),
    {AttrArgs, Exports, Imports} = get_attribute_args(Name, Between, Args),
    ?D({AttrArgs, Between}),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     make_attribute_ref(Name, AttrArgs, Extra, Offset, PosLength)++
         make_attribute_arg_refs(Name, AttrArgs, Between),
     Exports, Imports}.

get_exported(F_A, Exports) ->
    lists:member(F_A, Exports).

get_attribute_args(import, Between, _Args) ->
    From = get_first_of_kind(atom, Between),
    Tokens = get_between(Between, '[', ']'),
    Import = {From, fun_arity_from_tokens(Tokens)},
    {Import, [], [Import]};
get_attribute_args(export, Between, _Args) ->
    Tokens = get_between(Between, '[', ']'),
    Exports = fun_arity_from_tokens(Tokens),
    {Exports, Exports, []};
get_attribute_args(record, Between, _Args) ->
    [RecordToken | _] = Between,
    RecordName = RecordToken#token.value,
    Tokens = erlide_np_util:get_between_outer_pars(Between, '{', '}'),
    R = {RecordName, field_list_from_tokens(Tokens)},
    {R, [], []};
get_attribute_args(_, _Between, Args) ->
    {Args, [], []}.

check_class([#token{kind = AtomOrMacro}, #token{kind = '('} | _])
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    function;
check_class([#token{kind = '-'}, #token{kind = atom} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'spec'} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'type'} | _]) ->
    attribute;
check_class(_X) ->
    %?D(lists:sublist(_X, 5)),
    other.

get_first_of_kind(Kind, Tokens) ->
    case erlide_np_util:skip_to(Tokens, Kind) of
        [#token{kind=Kind, value=Value} | _] ->
            Value;
        _ ->
            undefined
    end.
        
fun_arity_from_tokens([#token{kind=atom, value=Fun}, #token{kind='/'}, 
                       #token{kind=integer, value=Arity} | Rest]) ->
    [{Fun, Arity} | fun_arity_from_tokens(Rest)];
fun_arity_from_tokens([_ | Rest]) ->
    fun_arity_from_tokens(Rest);
fun_arity_from_tokens(_) ->
    [].

field_list_from_tokens([#token{kind=atom, value=Field, line=Line, 
                               offset=Offset, length=Length} | Rest]) ->
    {Extra, NewRest} = get_upto2(Rest, ','),
    [{Field, {{Line, Line, Offset}, Length}, to_string(Extra)}
         | field_list_from_tokens(NewRest)];
field_list_from_tokens([_ | Rest]) ->
    field_list_from_tokens(Rest);
field_list_from_tokens(_) ->
    [].

to_string(Tokens) ->
    S = erlide_scanner:tokens_to_string(Tokens),
    erlide_text:strip(S).
%%     unspacify(S).

get_head(T) ->
    A = get_args(T),
    case get_guards(T) of
        "" ->
            A;
        G ->
            A ++ " when " ++ G
    end.

get_args(T) ->
    case erlide_np_util:get_between_outer_pars(T, '(', ')') of
        [] ->
            "";
        P ->
            "("++to_string(P)++")"
    end.

get_guards([]) ->
    "";
get_guards(T) ->
    to_string(get_between(T, 'when', '->')). 

last_not_eof(L) -> 
    case lists:reverse(L) of
        [eof, Last | _] ->
            Last;
        [Last | _] ->
            Last;
        _ ->
            error
    end.    


get_between(L, A, B) ->
    R = get_upto(L, B),
    lists:reverse(get_upto(lists:reverse(R), A)).

get_upto(L, Delim) ->
        get_upto(L, Delim, []).

get_upto([], _Delim, _Acc) ->
    [];
get_upto([#token{kind=Delim} | _], Delim, Acc) ->
        lists:reverse(Acc);
get_upto([T | Rest], Delim, Acc) ->
    get_upto(Rest, Delim, [T | Acc]).

get_upto2(L, Delim) ->
    get_upto2(L, Delim, []).

get_upto2([], _Delim, Acc) ->
    {lists:reverse(Acc), []};
get_upto2([#token{kind=Delim} | Rest], Delim, Acc) ->
    {lists:reverse(Acc), Rest};
get_upto2([T | Rest], Delim, Acc) ->
    get_upto2(Rest, Delim, [T | Acc]).

check_clause([#token{kind = ';'} | Rest], false) ->
    check_class(Rest) == function;
check_clause(_, _) ->
    false.

split_clauses(F) ->
    split_clauses(F, false, [], []).

split_clauses([], _HaveWhen, Acc, []) ->
    erlide_util:reverse2(Acc);
split_clauses([], HaveWhen, Acc, ClAcc) ->
    split_clauses([], HaveWhen, [ClAcc | Acc], []);
split_clauses([#token{kind=Kind}=T | Rest], _HaveWhen, Acc, ClAcc) 
  when Kind=:='end'; Kind=:=dot; Kind=:='->'; Kind=:='when' ->
    split_clauses(Rest, Kind=:='when', Acc, [T | ClAcc]);
split_clauses([T | Rest] = Tokens, HaveWhen, Acc, ClAcc) ->
    case check_clause(Tokens, HaveWhen) of
        false ->
            split_clauses(Rest, HaveWhen, Acc, [T | ClAcc]);
        true ->
            split_clauses(Rest, HaveWhen, [[T | ClAcc] | Acc], [])
    end.

%% fix_clause([#token{kind=atom, value=Name, line=Line, offset=Offset, length=Length} | Rest] = Code) ->
fix_clauses(Clauses) ->
    fix_clauses(Clauses, []).

fix_clauses([], Acc) ->
    lists:reverse(Acc);
fix_clauses([C | Rest], Acc) ->
    {Clause, Refs} = fix_clause(C),
    fix_clauses(Rest, [{Clause, Refs} | Acc]).

fix_clause([#token{kind=AtomOrMacro, value=Name, line=Line, offset=Offset, length=Length} | Rest]) 
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    #token{line=LastLine, offset=LastOffset, length=LastLength} = last_not_eof(Rest),
    PosLength = LastOffset - Offset + LastLength+1,
    ExternalRefs = get_refs_in_code(Rest),
    {#clause{pos={{Line, LastLine, Offset}, PosLength}, name_pos={{Line, Offset}, Length},
             name=Name, args=get_function_args(Rest), head=get_head(Rest)},
     ExternalRefs}.

get_function_args(Tokens) ->
    P = erlide_np_util:get_between_outer_pars(Tokens, '(', ')'),
    L = erlide_text:split_comma_list(P),
    [to_string(A) || A <- L].

fix_refs(ClausesAndRefs, Function, Imports) ->
    fix_refs(ClausesAndRefs, Function, Imports, []).

fix_refs([], #function{name=Name, arity=Arity, name_pos={{_, Offset}, Length}}, _Imports, Acc) ->
    Ref = #ref{data=#function_def{function=Name, arity=Arity}, offset=Offset, 
               length=Length, function=Name, arity=Arity, clause="", sub_clause=false},
    [Ref | Acc];
fix_refs([{Clause, Refs} | Rest], Function, Imports, Acc0) ->
    Acc1 = fix_refs(Refs, Clause, Function, Imports, Acc0),
    fix_refs(Rest, Function, Imports, Acc1).

fix_refs([], _Clause, _Function, _Imports, Acc) ->
    Acc;
fix_refs([{Offset, Length, RefData} | Rest], #clause{head=Head}=Clause, 
         #function{name=Name, arity=Arity, clauses=Clauses}=Function, 
         Imports, Acc) ->
    SubClause = case Clauses of
                    [] -> false;
                    [_] -> false;
                    _ -> true
                end,
    NewRefData = fix_imported_ref(RefData, Imports),
    Ref = #ref{data=NewRefData, offset=Offset, length=Length, function=Name, 
               arity=Arity, clause=Head, sub_clause=SubClause},
    fix_refs(Rest, Clause, Function, Imports, [Ref | Acc]).

fix_imported_ref(#local_call{function=F, arity=A}=RefData, [{Module, Imports} | Rest]) ->
    case lists:member({F, A}, Imports) of
        true -> #external_call{module=Module, function=F, arity=A};
        false -> fix_imported_ref(RefData, Rest)
    end;
fix_imported_ref(RefData, _Imports) ->
    RefData.

%% find references in code
%% VERY simple, M:F(_ ...), F(_ ...), #R, ?M, and that's it
%% returns {Offset, Length, Ref}, which is made into proper refs
%% by fix_refs
get_refs_in_code(L) ->
    get_refs_in_code(L, []).

get_refs_in_code([], Acc) ->
    lists:reverse(Acc);
get_refs_in_code([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
                  #token{kind=atom, value=F, offset=Offset2, length=Length2},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length2+Offset2-Offset, 
         #external_call{module = M, function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=atom, value=F, offset=Offset, length=Length},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length, #local_call{function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest], 
                 Acc) ->
    R = {Offset, Length, #macro_ref{macro=M}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind='#', offset=Offset}, 
                  #token{kind=atom, value=Record, offset=Offset2, length=Length2} | Rest],
                 Acc) ->
    R = {Offset, Length2+Offset2-Offset, #record_ref{record=Record}},
    {NewRest, Fields, RightSides} = erlide_np_records:check_fields(Rest, Record),
    NewAcc = get_refs_in_code(RightSides, Acc),
    get_refs_in_code(NewRest, Fields ++ [R | NewAcc]);
get_refs_in_code([#token{kind=var, value=V, offset=Offset, length=Length} | Rest],
                 Acc) ->
    R = {Offset, Length, make_var_def_ref(Acc, V)},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([_ | Rest], Acc) ->
    get_refs_in_code(Rest, Acc).

make_var_def_ref([], Var) ->
	#var_def{variable=Var};
make_var_def_ref([{_, _, #var_def{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([{_, _, #var_ref{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([_ | Rest], Var) ->
	make_var_def_ref(Rest, Var).

%% find type references in spec, type and opaque
%% also VERY simple, type(), ?M, #R, only
%% returns proper refs
get_refs(Tokens, Name, Arity) ->
	get_refs(Tokens, Name, Arity, []).

get_refs([], _, _, Acc) ->
    lists:reverse(Acc);
get_refs([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #macro_ref{macro=M}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind='#', offset=Offset},
          #token{kind=atom, value=R, offset=Offset2, length=Length2} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset, #record_ref{record=R}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
		  #token{kind=atom, value=T, offset=Offset2, length=Length2},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset,
                   #type_ref{module=M, type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=T, offset=Offset, length=Length},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #type_ref{module='_', type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=variable, value=V, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #var_ref{variable=V}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([_ | Rest], Name, Arity, Acc) ->
    get_refs(Rest, Name, Arity, Acc).

get_record_field_defs([], _) ->
    [];
get_record_field_defs([{FieldName, {{_, _, Offset}, Length}, _Extra} | Rest],
                      RecordName) ->
    [make_ref(Offset, Length, #record_field_def{record=RecordName, field=FieldName}, 
              FieldName, ?ARI_RECORD_FIELD_DEF)
         | get_record_field_defs(Rest, RecordName)].

make_ref(Offset, Length, Data, Name, Arity) ->
    #ref{data=Data, offset=Offset, length=Length, function=Name, arity=Arity, 
         clause="", sub_clause=false}.

make_attribute_arg_refs(define, Name, [#token{}, #token{kind='('} | Rest]) ->
    get_refs(skip_to_rparen(Rest), Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(define, Name, [#token{}, #token{kind=','} | Rest]) ->
    get_refs(Rest, Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(record, {Name, Fields}, [#token{}, #token{kind=','} | Rest]) ->
    get_record_field_defs(Fields, Name) ++ get_refs(Rest, Name, ?ARI_RECORD_DEF);
make_attribute_arg_refs(_, _, _) ->
    [].

skip_to_rparen([]) ->
    [];
skip_to_rparen([#token{kind=')'} | Rest]) ->
    Rest;
skip_to_rparen([_ | Rest]) ->
    skip_to_rparen(Rest).

make_attribute_ref(Name, Between, Extra, Offset, Length) ->
    ?D({Name, Between, Offset, Length}),
    case make_attribute_ref(Name, Between, Extra) of
        [] ->
            [];
        {Arity, AName, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=AName, 
                  arity=Arity, clause="", sub_clause=false}];
        {Arity, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=Name, 
                  arity=Arity, clause="", sub_clause=false}]
    end.

make_attribute_ref(module, _, Extra) ->
    {?ARI_ATTRIBUTE, #module_def{module=Extra}};
make_attribute_ref(record, Between, _E) ->
    ?D({Between, _E}),
    Name = case Between of
               {N, _} -> N;
               N -> N
           end,
    R= #record_def{record=Name},
    ?D(R),
    {?ARI_RECORD_DEF, Name, R};
make_attribute_ref(define, [Name | _], _) when is_list(Name) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(define, Name, _) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(include, _, Extra) ->
    {?ARI_INCLUDE, #include_ref{filename=unquote(Extra)}};
make_attribute_ref(_, _, _) ->
    [].

unquote(L) ->
    lists:reverse(unquote_first(lists:reverse(unquote_first(L)))).

unquote_first([$" | Rest]) ->
    Rest;
unquote_first(L) ->
    L.

remove_rest([], _) -> [];
remove_rest(Rest, Rest) -> [];
remove_rest([Hd | Tl], Rest) -> [Hd | remove_rest(Tl, Rest)].
%% Description: Split source files into functions and clauses and collects references
%% Author: jakob
%% Created: Mar 23, 2006

-module(erlide_noparse).

%%
%% Exported Functions
%%

%% called from Java
-export([initial_parse/5, reparse/1, remove_cache_files/2]).

%% called from Erlang
-export([read_module_refs/3]).

%%
%% Include files
%%

%% -define(DEBUG, 1).
%% -define(IO_FORMAT_DEBUG, 1).

-define(CACHE_VERSION, 25).
-define(SERVER, erlide_noparse).

-include("erlide.hrl").
-include("erlide_noparse.hrl").
-include("erlide_scanner.hrl").
-include("erlide_search_server.hrl").

%% special arity flags
-define(ARI_TYPESPEC, -2).
-define(ARI_ATTRIBUTE, -3).
-define(ARI_RECORD_DEF, -4).
-define(ARI_MACRO_DEF, -5).
-define(ARI_INCLUDE, -6).
-define(ARI_RECORD_FIELD_DEF, -7).

-define(X(X), erlide_log:log({X, erlang:now()})).


%%
%% API Functions
%%

initial_parse(ScannerName, ModuleFileName, StateDir, UseCache,
              UpdateSearchServer) ->
    try
        %%         ?D({StateDir, ModuleFileName}),
        BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
        RefsFileName = BaseName ++ ".refs",
        RenewFun = fun(_F) ->
                           do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer)
                   end,
        CacheFileName = BaseName ++ ".noparse",
        ?D(CacheFileName),
        {Cached, Res} = erlide_util:check_and_renew_cached(
                          ModuleFileName, CacheFileName, ?CACHE_VERSION, 
                          RenewFun, UseCache),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        ?D(updated),
        {ok, Res, Cached}
    catch
        error:Reason ->
            {error, Reason}
    end.

reparse(ScannerName) ->
    try
        Res = do_parse(ScannerName, "", "", true),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        {ok, Res, unused}
    catch
        error:Reason ->
            {error, Reason}
    end.

read_module_refs(ScannerName, ModulePath, StateDir) ->
    ?D(ScannerName),
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    RefsFileName = BaseName ++ ".refs",
    ?D(RefsFileName),
    case file:read_file(RefsFileName) of
        {ok, Binary} ->
            ?D(byte_size(Binary)),
            binary_to_term(Binary);
        _ ->
            ?D(ModulePath),
            {ok, InitialTextBin} = file:read_file(ModulePath),
            ?D(byte_size(InitialTextBin)),
            InitialText = binary_to_list(InitialTextBin),
            _D = erlide_scanner_server:initialScan(
                   ScannerName, ModulePath, InitialText, StateDir, true),
            ?D(_D),
            initial_parse(ScannerName, ModulePath, StateDir, true, false),
            ?D(RefsFileName),
            {ok, Binary} = file:read_file(RefsFileName),
            ?D(byte_size(Binary)),
            binary_to_term(Binary)
    end.

remove_cache_files(ScannerName, StateDir) ->
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    ScannerCacheFileName = BaseName ++ ".scan",
    file:delete(ScannerCacheFileName),
    RefsFileName = BaseName ++ ".refs",
    file:delete(RefsFileName),
    CacheFileName = BaseName ++ ".noparse",
    file:delete(CacheFileName).

%%
%% Internal functions
%%

do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer) ->
    Toks = erlide_scanner_server:getTokens(ScannerName),
    do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer).

do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer) ->
    ?D({do_parse, ScannerName, length(Toks)}),
    ?X(111),
    {UncommentToks, Comments} = erlide_np_util:extract_comments(Toks),
    ?X(222),
    ?D({length(UncommentToks), length(Comments)}),
    ?D({UncommentToks}),
    Functions = erlide_np_util:split_after_dots(UncommentToks),
    ?X(333),
    ?D(length(Functions)),
    AutoImports = erlide_util:add_auto_imported([]),
    ?X(444),
%%     ?D(AutoImports),
    {Collected, Refs} = classify_and_collect(Functions, [], [], [], AutoImports),
    ?X(555),
    ?D({'>>',length(Collected)}),
    CommentedCollected = erlide_np_util:get_function_comments(Collected, Comments),
    ?X(666),
    ?D(CommentedCollected),
    Model = #model{forms=CommentedCollected, comments=Comments},
    %%erlide_noparse_server:create(ScannerName, Model),
%%     ?D({"Model", length(Model#model.forms), erts_debug:flat_size(Model)}),
    FixedModel = fixup_model(Model),
    ?X(777),
%%     ?D(erts_debug:flat_size(FixedModel)),
    %% 	?D([erts_debug:flat_size(F) || F <- element(2, FixedModel)]),
    ?D(StateDir),
    case StateDir of
        "" -> ok;
        _ ->
            ?D({RefsFileName, length(Refs)}),
            file:write_file(RefsFileName, term_to_binary(Refs, [compressed]))
    end,
    update_search_server(UpdateSearchServer, ScannerName, Refs),
%%     ?D(FixedModel),
    ?X(888),
    FixedModel.

update_search_server(true, ScannerName, Refs) ->
    erlide_search_server:add_module_refs(ScannerName, Refs);
update_search_server(_, _, _) ->
    ok.

fixup_model(#model{forms=Forms, comments=Comments}) ->
    FixedComments = fixup_tokens(Comments),
    FixedForms = fixup_forms(Forms),
    #model{forms=FixedForms, comments=FixedComments}.

fixup_forms(Forms) ->
    [fixup_form(Form) || Form <- Forms].

fixup_tokens(Tokens) ->
    [fixup_token(Token) || Token <- Tokens].

fixup_token(#token{value=Value} = Token) when is_list(Value) ->
    Token#token{value=to_binary(Value)};
fixup_token(#token{text=Text} = Token) when is_list(Text) ->
    Token#token{value=to_binary(Text)};
fixup_token(Token) ->
    Token.

binary_args(Args) when is_list(Args) ->
    [iolist_to_binary(A) || A <- Args];
binary_args(_) ->
    [].

fixup_form(#function{comment=Comment, clauses=Clauses, args=Args} = Function) ->
    Function#function{comment= to_binary(Comment), clauses=fixup_forms(Clauses), args=binary_args(Args)};
fixup_form(#clause{head=Head, args=Args} = Clause) ->
    Clause#clause{head=to_binary(Head), args=binary_args(Args)};
fixup_form(Other) ->
    Other.

to_binary(Comment) when is_list(Comment) ->
    try 
        iolist_to_binary(Comment) 
    catch 
        _:_ -> 
            unicode:characters_to_binary(Comment) 
    end;
to_binary(Other) ->
    Other.

%% parse_test(ScannerName, File) ->
%%     erlide_scanner_server:scan_uncached(ScannerName, File, ""),
%%     Toks = erlide_scanner_server:getTokens(ScannerName),
%%     {UncommentToks, Comments} = extract_comments(Toks),
%%     Functions = split_after_dots(UncommentToks, [], []),
%%     {Collected, Refs} = classify_and_collect(Functions, [], []),
%%     _Model = #model{forms=Collected, comments=Comments},
%%     %%erlide_noparse_server:create(ScannerName, Model, ""),
%%     ok.

classify_and_collect([], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([eof], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([C | Rest], Acc, RefsAcc, Exports, Imports) ->
    {R, Refs, MoreExports, MoreImports} = cac(check_class(C), C, Exports, Imports),
    classify_and_collect(Rest, [R | Acc], Refs++RefsAcc, MoreExports++Exports, MoreImports++Imports).

cac(function, Tokens, Exports, Imports) ->
    ClauseList = split_clauses(Tokens),
    %?D(ClauseList),
    ClausesAndRefs = fix_clauses(ClauseList),
    %?D(length(Clauses)),
    [{#clause{pos=P, name=N, args=A, head=H, name_pos=NP}, _Refs} | _] = ClausesAndRefs,
    ?D(A),
    Arity = length(A),
    Exported = get_exported({N, Arity}, Exports),
    Function = case ClausesAndRefs of  	% only show subclauses when more than one
                   [_] ->
                       #function{pos=P, name=N, arity=Arity, args=A, head=H, 
                                 clauses=[], name_pos=NP, exported=Exported};
                   _ ->
                       Clauses = [C || {C, _} <- ClausesAndRefs],
                       #function{pos=P, name=N, arity=Arity, clauses=Clauses, 
                                 name_pos=NP, exported=Exported}
               end,
    {Function, fix_refs(ClausesAndRefs, Function, Imports), [], []};
cac(attribute, Attribute, _Exports, _Imports) ->
    ?D(Attribute),
    case Attribute of
        %% -spec, -type or -opaque
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=Kind, line=_Line, offset=_Offset, value=Name} | Args]
          when (Kind=:='spec') or ((Kind=:=atom) and (Name=:='type'))
                   or ((Kind=:=atom) and (Name=:='opaque'))->
            get_type_attribute(Kind, Name, Offset, Line, Attribute, Args);
        %% other attributes
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=atom, value=Name, line=_Line, offset=_Offset},
         _, #token{value=Args} | _] = Attribute ->
            get_other_attribute(Name, Offset, Line, Attribute, Args);
        [_, #token{kind=atom, value=Name, line=Line, offset=Offset} | _] ->
            #token{line=LastLine, offset=LastOffset, 
                   length=LastLength} = last_not_eof(Attribute),
            PosLength = LastOffset - Offset + LastLength,
            {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                       name=Name, args=[]}, [], [], []}
    end;
cac(other, [#token{value=Name, line=Line, offset=Offset, length=Length} | _], 
    _Exports, _Imports) ->
    {#other{pos={{Line, Line, Offset}, Length}, name=Name}, [], [], []};
cac(_, _D, _E, _I) ->
    {eof, [], [], []}.

get_type_attribute(Kind, Name0, Offset, Line, Attribute, Args) ->
    ?D(Name0),
    Name = case Kind of 'spec' -> Kind; _ -> Name0 end,
    #token{line=LastLine, offset=LastOffset,
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    ?D(Args),
    Extra = to_string(Args),
    ?D(Extra),
    {AttrArgs, _, _} = get_attribute_args(Kind, Args, Args),
    ?D({AttrArgs, Extra}),
    ExternalRefs = get_refs(tl(AttrArgs), Extra, ?ARI_TYPESPEC),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     [#ref{data=#type_def{type=Name}, offset=Offset, length=PosLength, function=Name, 
           arity=?ARI_TYPESPEC, clause="", sub_clause=false} | ExternalRefs], [], []}.

get_other_attribute(Name, Offset, Line, Attribute, Args) ->
    #token{line=LastLine, offset=LastOffset, 
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    Between = erlide_np_util:get_between_outer_pars(Attribute, '(', ')'),
    Extra = to_string(Between),
    {AttrArgs, Exports, Imports} = get_attribute_args(Name, Between, Args),
    ?D({AttrArgs, Between}),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     make_attribute_ref(Name, AttrArgs, Extra, Offset, PosLength)++
         make_attribute_arg_refs(Name, AttrArgs, Between),
     Exports, Imports}.

get_exported(F_A, Exports) ->
    lists:member(F_A, Exports).

get_attribute_args(import, Between, _Args) ->
    From = get_first_of_kind(atom, Between),
    Tokens = get_between(Between, '[', ']'),
    Import = {From, fun_arity_from_tokens(Tokens)},
    {Import, [], [Import]};
get_attribute_args(export, Between, _Args) ->
    Tokens = get_between(Between, '[', ']'),
    Exports = fun_arity_from_tokens(Tokens),
    {Exports, Exports, []};
get_attribute_args(record, Between, _Args) ->
    [RecordToken | _] = Between,
    RecordName = RecordToken#token.value,
    Tokens = erlide_np_util:get_between_outer_pars(Between, '{', '}'),
    R = {RecordName, field_list_from_tokens(Tokens)},
    {R, [], []};
get_attribute_args(_, _Between, Args) ->
    {Args, [], []}.

check_class([#token{kind = AtomOrMacro}, #token{kind = '('} | _])
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    function;
check_class([#token{kind = '-'}, #token{kind = atom} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'spec'} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'type'} | _]) ->
    attribute;
check_class(_X) ->
    %?D(lists:sublist(_X, 5)),
    other.

get_first_of_kind(Kind, Tokens) ->
    case erlide_np_util:skip_to(Tokens, Kind) of
        [#token{kind=Kind, value=Value} | _] ->
            Value;
        _ ->
            undefined
    end.
        
fun_arity_from_tokens([#token{kind=atom, value=Fun}, #token{kind='/'}, 
                       #token{kind=integer, value=Arity} | Rest]) ->
    [{Fun, Arity} | fun_arity_from_tokens(Rest)];
fun_arity_from_tokens([_ | Rest]) ->
    fun_arity_from_tokens(Rest);
fun_arity_from_tokens(_) ->
    [].

field_list_from_tokens([#token{kind=atom, value=Field, line=Line, 
                               offset=Offset, length=Length} | Rest]) ->
    {Extra, NewRest} = get_upto2(Rest, ','),
    [{Field, {{Line, Line, Offset}, Length}, to_string(Extra)}
         | field_list_from_tokens(NewRest)];
field_list_from_tokens([_ | Rest]) ->
    field_list_from_tokens(Rest);
field_list_from_tokens(_) ->
    [].

to_string(Tokens) ->
    S = erlide_scanner:tokens_to_string(Tokens),
    erlide_text:strip(S).
%%     unspacify(S).

get_head(T) ->
    A = get_args(T),
    case get_guards(T) of
        "" ->
            A;
        G ->
            A ++ " when " ++ G
    end.

get_args(T) ->
    case erlide_np_util:get_between_outer_pars(T, '(', ')') of
        [] ->
            "";
        P ->
            "("++to_string(P)++")"
    end.

get_guards([]) ->
    "";
get_guards(T) ->
    to_string(get_between(T, 'when', '->')). 

last_not_eof(L) -> 
    case lists:reverse(L) of
        [eof, Last | _] ->
            Last;
        [Last | _] ->
            Last;
        _ ->
            error
    end.    


get_between(L, A, B) ->
    R = get_upto(L, B),
    lists:reverse(get_upto(lists:reverse(R), A)).

get_upto(L, Delim) ->
        get_upto(L, Delim, []).

get_upto([], _Delim, _Acc) ->
    [];
get_upto([#token{kind=Delim} | _], Delim, Acc) ->
        lists:reverse(Acc);
get_upto([T | Rest], Delim, Acc) ->
    get_upto(Rest, Delim, [T | Acc]).

get_upto2(L, Delim) ->
    get_upto2(L, Delim, []).

get_upto2([], _Delim, Acc) ->
    {lists:reverse(Acc), []};
get_upto2([#token{kind=Delim} | Rest], Delim, Acc) ->
    {lists:reverse(Acc), Rest};
get_upto2([T | Rest], Delim, Acc) ->
    get_upto2(Rest, Delim, [T | Acc]).

check_clause([#token{kind = ';'} | Rest], false) ->
    check_class(Rest) == function;
check_clause(_, _) ->
    false.

split_clauses(F) ->
    split_clauses(F, false, [], []).

split_clauses([], _HaveWhen, Acc, []) ->
    erlide_util:reverse2(Acc);
split_clauses([], HaveWhen, Acc, ClAcc) ->
    split_clauses([], HaveWhen, [ClAcc | Acc], []);
split_clauses([#token{kind=Kind}=T | Rest], _HaveWhen, Acc, ClAcc) 
  when Kind=:='end'; Kind=:=dot; Kind=:='->'; Kind=:='when' ->
    split_clauses(Rest, Kind=:='when', Acc, [T | ClAcc]);
split_clauses([T | Rest] = Tokens, HaveWhen, Acc, ClAcc) ->
    case check_clause(Tokens, HaveWhen) of
        false ->
            split_clauses(Rest, HaveWhen, Acc, [T | ClAcc]);
        true ->
            split_clauses(Rest, HaveWhen, [[T | ClAcc] | Acc], [])
    end.

%% fix_clause([#token{kind=atom, value=Name, line=Line, offset=Offset, length=Length} | Rest] = Code) ->
fix_clauses(Clauses) ->
    fix_clauses(Clauses, []).

fix_clauses([], Acc) ->
    lists:reverse(Acc);
fix_clauses([C | Rest], Acc) ->
    {Clause, Refs} = fix_clause(C),
    fix_clauses(Rest, [{Clause, Refs} | Acc]).

fix_clause([#token{kind=AtomOrMacro, value=Name, line=Line, offset=Offset, length=Length} | Rest]) 
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    #token{line=LastLine, offset=LastOffset, length=LastLength} = last_not_eof(Rest),
    PosLength = LastOffset - Offset + LastLength+1,
    ExternalRefs = get_refs_in_code(Rest),
    {#clause{pos={{Line, LastLine, Offset}, PosLength}, name_pos={{Line, Offset}, Length},
             name=Name, args=get_function_args(Rest), head=get_head(Rest)},
     ExternalRefs}.

get_function_args(Tokens) ->
    P = erlide_np_util:get_between_outer_pars(Tokens, '(', ')'),
    L = erlide_text:split_comma_list(P),
    [to_string(A) || A <- L].

fix_refs(ClausesAndRefs, Function, Imports) ->
    fix_refs(ClausesAndRefs, Function, Imports, []).

fix_refs([], #function{name=Name, arity=Arity, name_pos={{_, Offset}, Length}}, _Imports, Acc) ->
    Ref = #ref{data=#function_def{function=Name, arity=Arity}, offset=Offset, 
               length=Length, function=Name, arity=Arity, clause="", sub_clause=false},
    [Ref | Acc];
fix_refs([{Clause, Refs} | Rest], Function, Imports, Acc0) ->
    Acc1 = fix_refs(Refs, Clause, Function, Imports, Acc0),
    fix_refs(Rest, Function, Imports, Acc1).

fix_refs([], _Clause, _Function, _Imports, Acc) ->
    Acc;
fix_refs([{Offset, Length, RefData} | Rest], #clause{head=Head}=Clause, 
         #function{name=Name, arity=Arity, clauses=Clauses}=Function, 
         Imports, Acc) ->
    SubClause = case Clauses of
                    [] -> false;
                    [_] -> false;
                    _ -> true
                end,
    NewRefData = fix_imported_ref(RefData, Imports),
    Ref = #ref{data=NewRefData, offset=Offset, length=Length, function=Name, 
               arity=Arity, clause=Head, sub_clause=SubClause},
    fix_refs(Rest, Clause, Function, Imports, [Ref | Acc]).

fix_imported_ref(#local_call{function=F, arity=A}=RefData, [{Module, Imports} | Rest]) ->
    case lists:member({F, A}, Imports) of
        true -> #external_call{module=Module, function=F, arity=A};
        false -> fix_imported_ref(RefData, Rest)
    end;
fix_imported_ref(RefData, _Imports) ->
    RefData.

%% find references in code
%% VERY simple, M:F(_ ...), F(_ ...), #R, ?M, and that's it
%% returns {Offset, Length, Ref}, which is made into proper refs
%% by fix_refs
get_refs_in_code(L) ->
    get_refs_in_code(L, []).

get_refs_in_code([], Acc) ->
    lists:reverse(Acc);
get_refs_in_code([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
                  #token{kind=atom, value=F, offset=Offset2, length=Length2},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length2+Offset2-Offset, 
         #external_call{module = M, function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=atom, value=F, offset=Offset, length=Length},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length, #local_call{function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest], 
                 Acc) ->
    R = {Offset, Length, #macro_ref{macro=M}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind='#', offset=Offset}, 
                  #token{kind=atom, value=Record, offset=Offset2, length=Length2} | Rest],
                 Acc) ->
    R = {Offset, Length2+Offset2-Offset, #record_ref{record=Record}},
    {NewRest, Fields, RightSides} = erlide_np_records:check_fields(Rest, Record),
    NewAcc = get_refs_in_code(RightSides, Acc),
    get_refs_in_code(NewRest, Fields ++ [R | NewAcc]);
get_refs_in_code([#token{kind=var, value=V, offset=Offset, length=Length} | Rest],
                 Acc) ->
    R = {Offset, Length, make_var_def_ref(Acc, V)},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([_ | Rest], Acc) ->
    get_refs_in_code(Rest, Acc).

make_var_def_ref([], Var) ->
	#var_def{variable=Var};
make_var_def_ref([{_, _, #var_def{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([{_, _, #var_ref{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([_ | Rest], Var) ->
	make_var_def_ref(Rest, Var).

%% find type references in spec, type and opaque
%% also VERY simple, type(), ?M, #R, only
%% returns proper refs
get_refs(Tokens, Name, Arity) ->
	get_refs(Tokens, Name, Arity, []).

get_refs([], _, _, Acc) ->
    lists:reverse(Acc);
get_refs([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #macro_ref{macro=M}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind='#', offset=Offset},
          #token{kind=atom, value=R, offset=Offset2, length=Length2} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset, #record_ref{record=R}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
		  #token{kind=atom, value=T, offset=Offset2, length=Length2},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset,
                   #type_ref{module=M, type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=T, offset=Offset, length=Length},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #type_ref{module='_', type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=variable, value=V, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #var_ref{variable=V}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([_ | Rest], Name, Arity, Acc) ->
    get_refs(Rest, Name, Arity, Acc).

get_record_field_defs([], _) ->
    [];
get_record_field_defs([{FieldName, {{_, _, Offset}, Length}, _Extra} | Rest],
                      RecordName) ->
    [make_ref(Offset, Length, #record_field_def{record=RecordName, field=FieldName}, 
              FieldName, ?ARI_RECORD_FIELD_DEF)
         | get_record_field_defs(Rest, RecordName)].

make_ref(Offset, Length, Data, Name, Arity) ->
    #ref{data=Data, offset=Offset, length=Length, function=Name, arity=Arity, 
         clause="", sub_clause=false}.

make_attribute_arg_refs(define, Name, [#token{}, #token{kind='('} | Rest]) ->
    get_refs(skip_to_rparen(Rest), Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(define, Name, [#token{}, #token{kind=','} | Rest]) ->
    get_refs(Rest, Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(record, {Name, Fields}, [#token{}, #token{kind=','} | Rest]) ->
    get_record_field_defs(Fields, Name) ++ get_refs(Rest, Name, ?ARI_RECORD_DEF);
make_attribute_arg_refs(_, _, _) ->
    [].

skip_to_rparen([]) ->
    [];
skip_to_rparen([#token{kind=')'} | Rest]) ->
    Rest;
skip_to_rparen([_ | Rest]) ->
    skip_to_rparen(Rest).

make_attribute_ref(Name, Between, Extra, Offset, Length) ->
    ?D({Name, Between, Offset, Length}),
    case make_attribute_ref(Name, Between, Extra) of
        [] ->
            [];
        {Arity, AName, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=AName, 
                  arity=Arity, clause="", sub_clause=false}];
        {Arity, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=Name, 
                  arity=Arity, clause="", sub_clause=false}]
    end.

make_attribute_ref(module, _, Extra) ->
    {?ARI_ATTRIBUTE, #module_def{module=Extra}};
make_attribute_ref(record, Between, _E) ->
    ?D({Between, _E}),
    Name = case Between of
               {N, _} -> N;
               N -> N
           end,
    R= #record_def{record=Name},
    ?D(R),
    {?ARI_RECORD_DEF, Name, R};
make_attribute_ref(define, [Name | _], _) when is_list(Name) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(define, Name, _) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(include, _, Extra) ->
    {?ARI_INCLUDE, #include_ref{filename=unquote(Extra)}};
make_attribute_ref(_, _, _) ->
    [].

unquote(L) ->
    lists:reverse(unquote_first(lists:reverse(unquote_first(L)))).

unquote_first([$" | Rest]) ->
    Rest;
unquote_first(L) ->
    L.

remove_rest([], _) -> [];
remove_rest(Rest, Rest) -> [];
remove_rest([Hd | Tl], Rest) -> [Hd | remove_rest(Tl, Rest)].
%% Description: Split source files into functions and clauses and collects references
%% Author: jakob
%% Created: Mar 23, 2006

-module(erlide_noparse).

%%
%% Exported Functions
%%

%% called from Java
-export([initial_parse/5, reparse/1, remove_cache_files/2]).

%% called from Erlang
-export([read_module_refs/3]).

%%
%% Include files
%%

%% -define(DEBUG, 1).
%% -define(IO_FORMAT_DEBUG, 1).

-define(CACHE_VERSION, 25).
-define(SERVER, erlide_noparse).

-include("erlide.hrl").
-include("erlide_noparse.hrl").
-include("erlide_scanner.hrl").
-include("erlide_search_server.hrl").

%% special arity flags
-define(ARI_TYPESPEC, -2).
-define(ARI_ATTRIBUTE, -3).
-define(ARI_RECORD_DEF, -4).
-define(ARI_MACRO_DEF, -5).
-define(ARI_INCLUDE, -6).
-define(ARI_RECORD_FIELD_DEF, -7).

-define(X(X), erlide_log:log({X, erlang:now()})).


%%
%% API Functions
%%

initial_parse(ScannerName, ModuleFileName, StateDir, UseCache,
              UpdateSearchServer) ->
    try
        %%         ?D({StateDir, ModuleFileName}),
        BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
        RefsFileName = BaseName ++ ".refs",
        RenewFun = fun(_F) ->
                           do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer)
                   end,
        CacheFileName = BaseName ++ ".noparse",
        ?D(CacheFileName),
        {Cached, Res} = erlide_util:check_and_renew_cached(
                          ModuleFileName, CacheFileName, ?CACHE_VERSION, 
                          RenewFun, UseCache),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        ?D(updated),
        {ok, Res, Cached}
    catch
        error:Reason ->
            {error, Reason}
    end.

reparse(ScannerName) ->
    try
        Res = do_parse(ScannerName, "", "", true),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        {ok, Res, unused}
    catch
        error:Reason ->
            {error, Reason}
    end.

read_module_refs(ScannerName, ModulePath, StateDir) ->
    ?D(ScannerName),
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    RefsFileName = BaseName ++ ".refs",
    ?D(RefsFileName),
    case file:read_file(RefsFileName) of
        {ok, Binary} ->
            ?D(byte_size(Binary)),
            binary_to_term(Binary);
        _ ->
            ?D(ModulePath),
            {ok, InitialTextBin} = file:read_file(ModulePath),
            ?D(byte_size(InitialTextBin)),
            InitialText = binary_to_list(InitialTextBin),
            _D = erlide_scanner_server:initialScan(
                   ScannerName, ModulePath, InitialText, StateDir, true),
            ?D(_D),
            initial_parse(ScannerName, ModulePath, StateDir, true, false),
            ?D(RefsFileName),
            {ok, Binary} = file:read_file(RefsFileName),
            ?D(byte_size(Binary)),
            binary_to_term(Binary)
    end.

remove_cache_files(ScannerName, StateDir) ->
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    ScannerCacheFileName = BaseName ++ ".scan",
    file:delete(ScannerCacheFileName),
    RefsFileName = BaseName ++ ".refs",
    file:delete(RefsFileName),
    CacheFileName = BaseName ++ ".noparse",
    file:delete(CacheFileName).

%%
%% Internal functions
%%

do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer) ->
    Toks = erlide_scanner_server:getTokens(ScannerName),
    do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer).

do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer) ->
    ?D({do_parse, ScannerName, length(Toks)}),
    ?X(111),
    {UncommentToks, Comments} = erlide_np_util:extract_comments(Toks),
    ?X(222),
    ?D({length(UncommentToks), length(Comments)}),
    ?D({UncommentToks}),
    Functions = erlide_np_util:split_after_dots(UncommentToks),
    ?X(333),
    ?D(length(Functions)),
    AutoImports = erlide_util:add_auto_imported([]),
    ?X(444),
%%     ?D(AutoImports),
    {Collected, Refs} = classify_and_collect(Functions, [], [], [], AutoImports),
    ?X(555),
    ?D({'>>',length(Collected)}),
    CommentedCollected = erlide_np_util:get_function_comments(Collected, Comments),
    ?X(666),
    ?D(CommentedCollected),
    Model = #model{forms=CommentedCollected, comments=Comments},
    %%erlide_noparse_server:create(ScannerName, Model),
%%     ?D({"Model", length(Model#model.forms), erts_debug:flat_size(Model)}),
    FixedModel = fixup_model(Model),
    ?X(777),
%%     ?D(erts_debug:flat_size(FixedModel)),
    %% 	?D([erts_debug:flat_size(F) || F <- element(2, FixedModel)]),
    ?D(StateDir),
    case StateDir of
        "" -> ok;
        _ ->
            ?D({RefsFileName, length(Refs)}),
            file:write_file(RefsFileName, term_to_binary(Refs, [compressed]))
    end,
    update_search_server(UpdateSearchServer, ScannerName, Refs),
%%     ?D(FixedModel),
    ?X(888),
    FixedModel.

update_search_server(true, ScannerName, Refs) ->
    erlide_search_server:add_module_refs(ScannerName, Refs);
update_search_server(_, _, _) ->
    ok.

fixup_model(#model{forms=Forms, comments=Comments}) ->
    FixedComments = fixup_tokens(Comments),
    FixedForms = fixup_forms(Forms),
    #model{forms=FixedForms, comments=FixedComments}.

fixup_forms(Forms) ->
    [fixup_form(Form) || Form <- Forms].

fixup_tokens(Tokens) ->
    [fixup_token(Token) || Token <- Tokens].

fixup_token(#token{value=Value} = Token) when is_list(Value) ->
    Token#token{value=to_binary(Value)};
fixup_token(#token{text=Text} = Token) when is_list(Text) ->
    Token#token{value=to_binary(Text)};
fixup_token(Token) ->
    Token.

binary_args(Args) when is_list(Args) ->
    [iolist_to_binary(A) || A <- Args];
binary_args(_) ->
    [].

fixup_form(#function{comment=Comment, clauses=Clauses, args=Args} = Function) ->
    Function#function{comment= to_binary(Comment), clauses=fixup_forms(Clauses), args=binary_args(Args)};
fixup_form(#clause{head=Head, args=Args} = Clause) ->
    Clause#clause{head=to_binary(Head), args=binary_args(Args)};
fixup_form(Other) ->
    Other.

to_binary(Comment) when is_list(Comment) ->
    try 
        iolist_to_binary(Comment) 
    catch 
        _:_ -> 
            unicode:characters_to_binary(Comment) 
    end;
to_binary(Other) ->
    Other.

%% parse_test(ScannerName, File) ->
%%     erlide_scanner_server:scan_uncached(ScannerName, File, ""),
%%     Toks = erlide_scanner_server:getTokens(ScannerName),
%%     {UncommentToks, Comments} = extract_comments(Toks),
%%     Functions = split_after_dots(UncommentToks, [], []),
%%     {Collected, Refs} = classify_and_collect(Functions, [], []),
%%     _Model = #model{forms=Collected, comments=Comments},
%%     %%erlide_noparse_server:create(ScannerName, Model, ""),
%%     ok.

classify_and_collect([], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([eof], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([C | Rest], Acc, RefsAcc, Exports, Imports) ->
    {R, Refs, MoreExports, MoreImports} = cac(check_class(C), C, Exports, Imports),
    classify_and_collect(Rest, [R | Acc], Refs++RefsAcc, MoreExports++Exports, MoreImports++Imports).

cac(function, Tokens, Exports, Imports) ->
    ClauseList = split_clauses(Tokens),
    %?D(ClauseList),
    ClausesAndRefs = fix_clauses(ClauseList),
    %?D(length(Clauses)),
    [{#clause{pos=P, name=N, args=A, head=H, name_pos=NP}, _Refs} | _] = ClausesAndRefs,
    ?D(A),
    Arity = length(A),
    Exported = get_exported({N, Arity}, Exports),
    Function = case ClausesAndRefs of  	% only show subclauses when more than one
                   [_] ->
                       #function{pos=P, name=N, arity=Arity, args=A, head=H, 
                                 clauses=[], name_pos=NP, exported=Exported};
                   _ ->
                       Clauses = [C || {C, _} <- ClausesAndRefs],
                       #function{pos=P, name=N, arity=Arity, clauses=Clauses, 
                                 name_pos=NP, exported=Exported}
               end,
    {Function, fix_refs(ClausesAndRefs, Function, Imports), [], []};
cac(attribute, Attribute, _Exports, _Imports) ->
    ?D(Attribute),
    case Attribute of
        %% -spec, -type or -opaque
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=Kind, line=_Line, offset=_Offset, value=Name} | Args]
          when (Kind=:='spec') or ((Kind=:=atom) and (Name=:='type'))
                   or ((Kind=:=atom) and (Name=:='opaque'))->
            get_type_attribute(Kind, Name, Offset, Line, Attribute, Args);
        %% other attributes
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=atom, value=Name, line=_Line, offset=_Offset},
         _, #token{value=Args} | _] = Attribute ->
            get_other_attribute(Name, Offset, Line, Attribute, Args);
        [_, #token{kind=atom, value=Name, line=Line, offset=Offset} | _] ->
            #token{line=LastLine, offset=LastOffset, 
                   length=LastLength} = last_not_eof(Attribute),
            PosLength = LastOffset - Offset + LastLength,
            {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                       name=Name, args=[]}, [], [], []}
    end;
cac(other, [#token{value=Name, line=Line, offset=Offset, length=Length} | _], 
    _Exports, _Imports) ->
    {#other{pos={{Line, Line, Offset}, Length}, name=Name}, [], [], []};
cac(_, _D, _E, _I) ->
    {eof, [], [], []}.

get_type_attribute(Kind, Name0, Offset, Line, Attribute, Args) ->
    ?D(Name0),
    Name = case Kind of 'spec' -> Kind; _ -> Name0 end,
    #token{line=LastLine, offset=LastOffset,
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    ?D(Args),
    Extra = to_string(Args),
    ?D(Extra),
    {AttrArgs, _, _} = get_attribute_args(Kind, Args, Args),
    ?D({AttrArgs, Extra}),
    ExternalRefs = get_refs(tl(AttrArgs), Extra, ?ARI_TYPESPEC),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     [#ref{data=#type_def{type=Name}, offset=Offset, length=PosLength, function=Name, 
           arity=?ARI_TYPESPEC, clause="", sub_clause=false} | ExternalRefs], [], []}.

get_other_attribute(Name, Offset, Line, Attribute, Args) ->
    #token{line=LastLine, offset=LastOffset, 
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    Between = erlide_np_util:get_between_outer_pars(Attribute, '(', ')'),
    Extra = to_string(Between),
    {AttrArgs, Exports, Imports} = get_attribute_args(Name, Between, Args),
    ?D({AttrArgs, Between}),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     make_attribute_ref(Name, AttrArgs, Extra, Offset, PosLength)++
         make_attribute_arg_refs(Name, AttrArgs, Between),
     Exports, Imports}.

get_exported(F_A, Exports) ->
    lists:member(F_A, Exports).

get_attribute_args(import, Between, _Args) ->
    From = get_first_of_kind(atom, Between),
    Tokens = get_between(Between, '[', ']'),
    Import = {From, fun_arity_from_tokens(Tokens)},
    {Import, [], [Import]};
get_attribute_args(export, Between, _Args) ->
    Tokens = get_between(Between, '[', ']'),
    Exports = fun_arity_from_tokens(Tokens),
    {Exports, Exports, []};
get_attribute_args(record, Between, _Args) ->
    [RecordToken | _] = Between,
    RecordName = RecordToken#token.value,
    Tokens = erlide_np_util:get_between_outer_pars(Between, '{', '}'),
    R = {RecordName, field_list_from_tokens(Tokens)},
    {R, [], []};
get_attribute_args(_, _Between, Args) ->
    {Args, [], []}.

check_class([#token{kind = AtomOrMacro}, #token{kind = '('} | _])
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    function;
check_class([#token{kind = '-'}, #token{kind = atom} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'spec'} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'type'} | _]) ->
    attribute;
check_class(_X) ->
    %?D(lists:sublist(_X, 5)),
    other.

get_first_of_kind(Kind, Tokens) ->
    case erlide_np_util:skip_to(Tokens, Kind) of
        [#token{kind=Kind, value=Value} | _] ->
            Value;
        _ ->
            undefined
    end.
        
fun_arity_from_tokens([#token{kind=atom, value=Fun}, #token{kind='/'}, 
                       #token{kind=integer, value=Arity} | Rest]) ->
    [{Fun, Arity} | fun_arity_from_tokens(Rest)];
fun_arity_from_tokens([_ | Rest]) ->
    fun_arity_from_tokens(Rest);
fun_arity_from_tokens(_) ->
    [].

field_list_from_tokens([#token{kind=atom, value=Field, line=Line, 
                               offset=Offset, length=Length} | Rest]) ->
    {Extra, NewRest} = get_upto2(Rest, ','),
    [{Field, {{Line, Line, Offset}, Length}, to_string(Extra)}
         | field_list_from_tokens(NewRest)];
field_list_from_tokens([_ | Rest]) ->
    field_list_from_tokens(Rest);
field_list_from_tokens(_) ->
    [].

to_string(Tokens) ->
    S = erlide_scanner:tokens_to_string(Tokens),
    erlide_text:strip(S).
%%     unspacify(S).

get_head(T) ->
    A = get_args(T),
    case get_guards(T) of
        "" ->
            A;
        G ->
            A ++ " when " ++ G
    end.

get_args(T) ->
    case erlide_np_util:get_between_outer_pars(T, '(', ')') of
        [] ->
            "";
        P ->
            "("++to_string(P)++")"
    end.

get_guards([]) ->
    "";
get_guards(T) ->
    to_string(get_between(T, 'when', '->')). 

last_not_eof(L) -> 
    case lists:reverse(L) of
        [eof, Last | _] ->
            Last;
        [Last | _] ->
            Last;
        _ ->
            error
    end.    


get_between(L, A, B) ->
    R = get_upto(L, B),
    lists:reverse(get_upto(lists:reverse(R), A)).

get_upto(L, Delim) ->
        get_upto(L, Delim, []).

get_upto([], _Delim, _Acc) ->
    [];
get_upto([#token{kind=Delim} | _], Delim, Acc) ->
        lists:reverse(Acc);
get_upto([T | Rest], Delim, Acc) ->
    get_upto(Rest, Delim, [T | Acc]).

get_upto2(L, Delim) ->
    get_upto2(L, Delim, []).

get_upto2([], _Delim, Acc) ->
    {lists:reverse(Acc), []};
get_upto2([#token{kind=Delim} | Rest], Delim, Acc) ->
    {lists:reverse(Acc), Rest};
get_upto2([T | Rest], Delim, Acc) ->
    get_upto2(Rest, Delim, [T | Acc]).

check_clause([#token{kind = ';'} | Rest], false) ->
    check_class(Rest) == function;
check_clause(_, _) ->
    false.

split_clauses(F) ->
    split_clauses(F, false, [], []).

split_clauses([], _HaveWhen, Acc, []) ->
    erlide_util:reverse2(Acc);
split_clauses([], HaveWhen, Acc, ClAcc) ->
    split_clauses([], HaveWhen, [ClAcc | Acc], []);
split_clauses([#token{kind=Kind}=T | Rest], _HaveWhen, Acc, ClAcc) 
  when Kind=:='end'; Kind=:=dot; Kind=:='->'; Kind=:='when' ->
    split_clauses(Rest, Kind=:='when', Acc, [T | ClAcc]);
split_clauses([T | Rest] = Tokens, HaveWhen, Acc, ClAcc) ->
    case check_clause(Tokens, HaveWhen) of
        false ->
            split_clauses(Rest, HaveWhen, Acc, [T | ClAcc]);
        true ->
            split_clauses(Rest, HaveWhen, [[T | ClAcc] | Acc], [])
    end.

%% fix_clause([#token{kind=atom, value=Name, line=Line, offset=Offset, length=Length} | Rest] = Code) ->
fix_clauses(Clauses) ->
    fix_clauses(Clauses, []).

fix_clauses([], Acc) ->
    lists:reverse(Acc);
fix_clauses([C | Rest], Acc) ->
    {Clause, Refs} = fix_clause(C),
    fix_clauses(Rest, [{Clause, Refs} | Acc]).

fix_clause([#token{kind=AtomOrMacro, value=Name, line=Line, offset=Offset, length=Length} | Rest]) 
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    #token{line=LastLine, offset=LastOffset, length=LastLength} = last_not_eof(Rest),
    PosLength = LastOffset - Offset + LastLength+1,
    ExternalRefs = get_refs_in_code(Rest),
    {#clause{pos={{Line, LastLine, Offset}, PosLength}, name_pos={{Line, Offset}, Length},
             name=Name, args=get_function_args(Rest), head=get_head(Rest)},
     ExternalRefs}.

get_function_args(Tokens) ->
    P = erlide_np_util:get_between_outer_pars(Tokens, '(', ')'),
    L = erlide_text:split_comma_list(P),
    [to_string(A) || A <- L].

fix_refs(ClausesAndRefs, Function, Imports) ->
    fix_refs(ClausesAndRefs, Function, Imports, []).

fix_refs([], #function{name=Name, arity=Arity, name_pos={{_, Offset}, Length}}, _Imports, Acc) ->
    Ref = #ref{data=#function_def{function=Name, arity=Arity}, offset=Offset, 
               length=Length, function=Name, arity=Arity, clause="", sub_clause=false},
    [Ref | Acc];
fix_refs([{Clause, Refs} | Rest], Function, Imports, Acc0) ->
    Acc1 = fix_refs(Refs, Clause, Function, Imports, Acc0),
    fix_refs(Rest, Function, Imports, Acc1).

fix_refs([], _Clause, _Function, _Imports, Acc) ->
    Acc;
fix_refs([{Offset, Length, RefData} | Rest], #clause{head=Head}=Clause, 
         #function{name=Name, arity=Arity, clauses=Clauses}=Function, 
         Imports, Acc) ->
    SubClause = case Clauses of
                    [] -> false;
                    [_] -> false;
                    _ -> true
                end,
    NewRefData = fix_imported_ref(RefData, Imports),
    Ref = #ref{data=NewRefData, offset=Offset, length=Length, function=Name, 
               arity=Arity, clause=Head, sub_clause=SubClause},
    fix_refs(Rest, Clause, Function, Imports, [Ref | Acc]).

fix_imported_ref(#local_call{function=F, arity=A}=RefData, [{Module, Imports} | Rest]) ->
    case lists:member({F, A}, Imports) of
        true -> #external_call{module=Module, function=F, arity=A};
        false -> fix_imported_ref(RefData, Rest)
    end;
fix_imported_ref(RefData, _Imports) ->
    RefData.

%% find references in code
%% VERY simple, M:F(_ ...), F(_ ...), #R, ?M, and that's it
%% returns {Offset, Length, Ref}, which is made into proper refs
%% by fix_refs
get_refs_in_code(L) ->
    get_refs_in_code(L, []).

get_refs_in_code([], Acc) ->
    lists:reverse(Acc);
get_refs_in_code([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
                  #token{kind=atom, value=F, offset=Offset2, length=Length2},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length2+Offset2-Offset, 
         #external_call{module = M, function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=atom, value=F, offset=Offset, length=Length},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length, #local_call{function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest], 
                 Acc) ->
    R = {Offset, Length, #macro_ref{macro=M}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind='#', offset=Offset}, 
                  #token{kind=atom, value=Record, offset=Offset2, length=Length2} | Rest],
                 Acc) ->
    R = {Offset, Length2+Offset2-Offset, #record_ref{record=Record}},
    {NewRest, Fields, RightSides} = erlide_np_records:check_fields(Rest, Record),
    NewAcc = get_refs_in_code(RightSides, Acc),
    get_refs_in_code(NewRest, Fields ++ [R | NewAcc]);
get_refs_in_code([#token{kind=var, value=V, offset=Offset, length=Length} | Rest],
                 Acc) ->
    R = {Offset, Length, make_var_def_ref(Acc, V)},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([_ | Rest], Acc) ->
    get_refs_in_code(Rest, Acc).

make_var_def_ref([], Var) ->
	#var_def{variable=Var};
make_var_def_ref([{_, _, #var_def{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([{_, _, #var_ref{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([_ | Rest], Var) ->
	make_var_def_ref(Rest, Var).

%% find type references in spec, type and opaque
%% also VERY simple, type(), ?M, #R, only
%% returns proper refs
get_refs(Tokens, Name, Arity) ->
	get_refs(Tokens, Name, Arity, []).

get_refs([], _, _, Acc) ->
    lists:reverse(Acc);
get_refs([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #macro_ref{macro=M}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind='#', offset=Offset},
          #token{kind=atom, value=R, offset=Offset2, length=Length2} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset, #record_ref{record=R}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
		  #token{kind=atom, value=T, offset=Offset2, length=Length2},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset,
                   #type_ref{module=M, type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=T, offset=Offset, length=Length},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #type_ref{module='_', type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=variable, value=V, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #var_ref{variable=V}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([_ | Rest], Name, Arity, Acc) ->
    get_refs(Rest, Name, Arity, Acc).

get_record_field_defs([], _) ->
    [];
get_record_field_defs([{FieldName, {{_, _, Offset}, Length}, _Extra} | Rest],
                      RecordName) ->
    [make_ref(Offset, Length, #record_field_def{record=RecordName, field=FieldName}, 
              FieldName, ?ARI_RECORD_FIELD_DEF)
         | get_record_field_defs(Rest, RecordName)].

make_ref(Offset, Length, Data, Name, Arity) ->
    #ref{data=Data, offset=Offset, length=Length, function=Name, arity=Arity, 
         clause="", sub_clause=false}.

make_attribute_arg_refs(define, Name, [#token{}, #token{kind='('} | Rest]) ->
    get_refs(skip_to_rparen(Rest), Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(define, Name, [#token{}, #token{kind=','} | Rest]) ->
    get_refs(Rest, Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(record, {Name, Fields}, [#token{}, #token{kind=','} | Rest]) ->
    get_record_field_defs(Fields, Name) ++ get_refs(Rest, Name, ?ARI_RECORD_DEF);
make_attribute_arg_refs(_, _, _) ->
    [].

skip_to_rparen([]) ->
    [];
skip_to_rparen([#token{kind=')'} | Rest]) ->
    Rest;
skip_to_rparen([_ | Rest]) ->
    skip_to_rparen(Rest).

make_attribute_ref(Name, Between, Extra, Offset, Length) ->
    ?D({Name, Between, Offset, Length}),
    case make_attribute_ref(Name, Between, Extra) of
        [] ->
            [];
        {Arity, AName, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=AName, 
                  arity=Arity, clause="", sub_clause=false}];
        {Arity, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=Name, 
                  arity=Arity, clause="", sub_clause=false}]
    end.

make_attribute_ref(module, _, Extra) ->
    {?ARI_ATTRIBUTE, #module_def{module=Extra}};
make_attribute_ref(record, Between, _E) ->
    ?D({Between, _E}),
    Name = case Between of
               {N, _} -> N;
               N -> N
           end,
    R= #record_def{record=Name},
    ?D(R),
    {?ARI_RECORD_DEF, Name, R};
make_attribute_ref(define, [Name | _], _) when is_list(Name) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(define, Name, _) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(include, _, Extra) ->
    {?ARI_INCLUDE, #include_ref{filename=unquote(Extra)}};
make_attribute_ref(_, _, _) ->
    [].

unquote(L) ->
    lists:reverse(unquote_first(lists:reverse(unquote_first(L)))).

unquote_first([$" | Rest]) ->
    Rest;
unquote_first(L) ->
    L.

remove_rest([], _) -> [];
remove_rest(Rest, Rest) -> [];
remove_rest([Hd | Tl], Rest) -> [Hd | remove_rest(Tl, Rest)].
%% Description: Split source files into functions and clauses and collects references
%% Author: jakob
%% Created: Mar 23, 2006

-module(erlide_noparse).

%%
%% Exported Functions
%%

%% called from Java
-export([initial_parse/5, reparse/1, remove_cache_files/2]).

%% called from Erlang
-export([read_module_refs/3]).

%%
%% Include files
%%

%% -define(DEBUG, 1).
%% -define(IO_FORMAT_DEBUG, 1).

-define(CACHE_VERSION, 25).
-define(SERVER, erlide_noparse).

-include("erlide.hrl").
-include("erlide_noparse.hrl").
-include("erlide_scanner.hrl").
-include("erlide_search_server.hrl").

%% special arity flags
-define(ARI_TYPESPEC, -2).
-define(ARI_ATTRIBUTE, -3).
-define(ARI_RECORD_DEF, -4).
-define(ARI_MACRO_DEF, -5).
-define(ARI_INCLUDE, -6).
-define(ARI_RECORD_FIELD_DEF, -7).

-define(X(X), erlide_log:log({X, erlang:now()})).


%%
%% API Functions
%%

initial_parse(ScannerName, ModuleFileName, StateDir, UseCache,
              UpdateSearchServer) ->
    try
        %%         ?D({StateDir, ModuleFileName}),
        BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
        RefsFileName = BaseName ++ ".refs",
        RenewFun = fun(_F) ->
                           do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer)
                   end,
        CacheFileName = BaseName ++ ".noparse",
        ?D(CacheFileName),
        {Cached, Res} = erlide_util:check_and_renew_cached(
                          ModuleFileName, CacheFileName, ?CACHE_VERSION, 
                          RenewFun, UseCache),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        ?D(updated),
        {ok, Res, Cached}
    catch
        error:Reason ->
            {error, Reason}
    end.

reparse(ScannerName) ->
    try
        Res = do_parse(ScannerName, "", "", true),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        {ok, Res, unused}
    catch
        error:Reason ->
            {error, Reason}
    end.

read_module_refs(ScannerName, ModulePath, StateDir) ->
    ?D(ScannerName),
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    RefsFileName = BaseName ++ ".refs",
    ?D(RefsFileName),
    case file:read_file(RefsFileName) of
        {ok, Binary} ->
            ?D(byte_size(Binary)),
            binary_to_term(Binary);
        _ ->
            ?D(ModulePath),
            {ok, InitialTextBin} = file:read_file(ModulePath),
            ?D(byte_size(InitialTextBin)),
            InitialText = binary_to_list(InitialTextBin),
            _D = erlide_scanner_server:initialScan(
                   ScannerName, ModulePath, InitialText, StateDir, true),
            ?D(_D),
            initial_parse(ScannerName, ModulePath, StateDir, true, false),
            ?D(RefsFileName),
            {ok, Binary} = file:read_file(RefsFileName),
            ?D(byte_size(Binary)),
            binary_to_term(Binary)
    end.

remove_cache_files(ScannerName, StateDir) ->
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    ScannerCacheFileName = BaseName ++ ".scan",
    file:delete(ScannerCacheFileName),
    RefsFileName = BaseName ++ ".refs",
    file:delete(RefsFileName),
    CacheFileName = BaseName ++ ".noparse",
    file:delete(CacheFileName).

%%
%% Internal functions
%%

do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer) ->
    Toks = erlide_scanner_server:getTokens(ScannerName),
    do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer).

do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer) ->
    ?D({do_parse, ScannerName, length(Toks)}),
    ?X(111),
    {UncommentToks, Comments} = erlide_np_util:extract_comments(Toks),
    ?X(222),
    ?D({length(UncommentToks), length(Comments)}),
    ?D({UncommentToks}),
    Functions = erlide_np_util:split_after_dots(UncommentToks),
    ?X(333),
    ?D(length(Functions)),
    AutoImports = erlide_util:add_auto_imported([]),
    ?X(444),
%%     ?D(AutoImports),
    {Collected, Refs} = classify_and_collect(Functions, [], [], [], AutoImports),
    ?X(555),
    ?D({'>>',length(Collected)}),
    CommentedCollected = erlide_np_util:get_function_comments(Collected, Comments),
    ?X(666),
    ?D(CommentedCollected),
    Model = #model{forms=CommentedCollected, comments=Comments},
    %%erlide_noparse_server:create(ScannerName, Model),
%%     ?D({"Model", length(Model#model.forms), erts_debug:flat_size(Model)}),
    FixedModel = fixup_model(Model),
    ?X(777),
%%     ?D(erts_debug:flat_size(FixedModel)),
    %% 	?D([erts_debug:flat_size(F) || F <- element(2, FixedModel)]),
    ?D(StateDir),
    case StateDir of
        "" -> ok;
        _ ->
            ?D({RefsFileName, length(Refs)}),
            file:write_file(RefsFileName, term_to_binary(Refs, [compressed]))
    end,
    update_search_server(UpdateSearchServer, ScannerName, Refs),
%%     ?D(FixedModel),
    ?X(888),
    FixedModel.

update_search_server(true, ScannerName, Refs) ->
    erlide_search_server:add_module_refs(ScannerName, Refs);
update_search_server(_, _, _) ->
    ok.

fixup_model(#model{forms=Forms, comments=Comments}) ->
    FixedComments = fixup_tokens(Comments),
    FixedForms = fixup_forms(Forms),
    #model{forms=FixedForms, comments=FixedComments}.

fixup_forms(Forms) ->
    [fixup_form(Form) || Form <- Forms].

fixup_tokens(Tokens) ->
    [fixup_token(Token) || Token <- Tokens].

fixup_token(#token{value=Value} = Token) when is_list(Value) ->
    Token#token{value=to_binary(Value)};
fixup_token(#token{text=Text} = Token) when is_list(Text) ->
    Token#token{value=to_binary(Text)};
fixup_token(Token) ->
    Token.

binary_args(Args) when is_list(Args) ->
    [iolist_to_binary(A) || A <- Args];
binary_args(_) ->
    [].

fixup_form(#function{comment=Comment, clauses=Clauses, args=Args} = Function) ->
    Function#function{comment= to_binary(Comment), clauses=fixup_forms(Clauses), args=binary_args(Args)};
fixup_form(#clause{head=Head, args=Args} = Clause) ->
    Clause#clause{head=to_binary(Head), args=binary_args(Args)};
fixup_form(Other) ->
    Other.

to_binary(Comment) when is_list(Comment) ->
    try 
        iolist_to_binary(Comment) 
    catch 
        _:_ -> 
            unicode:characters_to_binary(Comment) 
    end;
to_binary(Other) ->
    Other.

%% parse_test(ScannerName, File) ->
%%     erlide_scanner_server:scan_uncached(ScannerName, File, ""),
%%     Toks = erlide_scanner_server:getTokens(ScannerName),
%%     {UncommentToks, Comments} = extract_comments(Toks),
%%     Functions = split_after_dots(UncommentToks, [], []),
%%     {Collected, Refs} = classify_and_collect(Functions, [], []),
%%     _Model = #model{forms=Collected, comments=Comments},
%%     %%erlide_noparse_server:create(ScannerName, Model, ""),
%%     ok.

classify_and_collect([], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([eof], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([C | Rest], Acc, RefsAcc, Exports, Imports) ->
    {R, Refs, MoreExports, MoreImports} = cac(check_class(C), C, Exports, Imports),
    classify_and_collect(Rest, [R | Acc], Refs++RefsAcc, MoreExports++Exports, MoreImports++Imports).

cac(function, Tokens, Exports, Imports) ->
    ClauseList = split_clauses(Tokens),
    %?D(ClauseList),
    ClausesAndRefs = fix_clauses(ClauseList),
    %?D(length(Clauses)),
    [{#clause{pos=P, name=N, args=A, head=H, name_pos=NP}, _Refs} | _] = ClausesAndRefs,
    ?D(A),
    Arity = length(A),
    Exported = get_exported({N, Arity}, Exports),
    Function = case ClausesAndRefs of  	% only show subclauses when more than one
                   [_] ->
                       #function{pos=P, name=N, arity=Arity, args=A, head=H, 
                                 clauses=[], name_pos=NP, exported=Exported};
                   _ ->
                       Clauses = [C || {C, _} <- ClausesAndRefs],
                       #function{pos=P, name=N, arity=Arity, clauses=Clauses, 
                                 name_pos=NP, exported=Exported}
               end,
    {Function, fix_refs(ClausesAndRefs, Function, Imports), [], []};
cac(attribute, Attribute, _Exports, _Imports) ->
    ?D(Attribute),
    case Attribute of
        %% -spec, -type or -opaque
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=Kind, line=_Line, offset=_Offset, value=Name} | Args]
          when (Kind=:='spec') or ((Kind=:=atom) and (Name=:='type'))
                   or ((Kind=:=atom) and (Name=:='opaque'))->
            get_type_attribute(Kind, Name, Offset, Line, Attribute, Args);
        %% other attributes
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=atom, value=Name, line=_Line, offset=_Offset},
         _, #token{value=Args} | _] = Attribute ->
            get_other_attribute(Name, Offset, Line, Attribute, Args);
        [_, #token{kind=atom, value=Name, line=Line, offset=Offset} | _] ->
            #token{line=LastLine, offset=LastOffset, 
                   length=LastLength} = last_not_eof(Attribute),
            PosLength = LastOffset - Offset + LastLength,
            {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                       name=Name, args=[]}, [], [], []}
    end;
cac(other, [#token{value=Name, line=Line, offset=Offset, length=Length} | _], 
    _Exports, _Imports) ->
    {#other{pos={{Line, Line, Offset}, Length}, name=Name}, [], [], []};
cac(_, _D, _E, _I) ->
    {eof, [], [], []}.

get_type_attribute(Kind, Name0, Offset, Line, Attribute, Args) ->
    ?D(Name0),
    Name = case Kind of 'spec' -> Kind; _ -> Name0 end,
    #token{line=LastLine, offset=LastOffset,
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    ?D(Args),
    Extra = to_string(Args),
    ?D(Extra),
    {AttrArgs, _, _} = get_attribute_args(Kind, Args, Args),
    ?D({AttrArgs, Extra}),
    ExternalRefs = get_refs(tl(AttrArgs), Extra, ?ARI_TYPESPEC),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     [#ref{data=#type_def{type=Name}, offset=Offset, length=PosLength, function=Name, 
           arity=?ARI_TYPESPEC, clause="", sub_clause=false} | ExternalRefs], [], []}.

get_other_attribute(Name, Offset, Line, Attribute, Args) ->
    #token{line=LastLine, offset=LastOffset, 
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    Between = erlide_np_util:get_between_outer_pars(Attribute, '(', ')'),
    Extra = to_string(Between),
    {AttrArgs, Exports, Imports} = get_attribute_args(Name, Between, Args),
    ?D({AttrArgs, Between}),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     make_attribute_ref(Name, AttrArgs, Extra, Offset, PosLength)++
         make_attribute_arg_refs(Name, AttrArgs, Between),
     Exports, Imports}.

get_exported(F_A, Exports) ->
    lists:member(F_A, Exports).

get_attribute_args(import, Between, _Args) ->
    From = get_first_of_kind(atom, Between),
    Tokens = get_between(Between, '[', ']'),
    Import = {From, fun_arity_from_tokens(Tokens)},
    {Import, [], [Import]};
get_attribute_args(export, Between, _Args) ->
    Tokens = get_between(Between, '[', ']'),
    Exports = fun_arity_from_tokens(Tokens),
    {Exports, Exports, []};
get_attribute_args(record, Between, _Args) ->
    [RecordToken | _] = Between,
    RecordName = RecordToken#token.value,
    Tokens = erlide_np_util:get_between_outer_pars(Between, '{', '}'),
    R = {RecordName, field_list_from_tokens(Tokens)},
    {R, [], []};
get_attribute_args(_, _Between, Args) ->
    {Args, [], []}.

check_class([#token{kind = AtomOrMacro}, #token{kind = '('} | _])
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    function;
check_class([#token{kind = '-'}, #token{kind = atom} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'spec'} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'type'} | _]) ->
    attribute;
check_class(_X) ->
    %?D(lists:sublist(_X, 5)),
    other.

get_first_of_kind(Kind, Tokens) ->
    case erlide_np_util:skip_to(Tokens, Kind) of
        [#token{kind=Kind, value=Value} | _] ->
            Value;
        _ ->
            undefined
    end.
        
fun_arity_from_tokens([#token{kind=atom, value=Fun}, #token{kind='/'}, 
                       #token{kind=integer, value=Arity} | Rest]) ->
    [{Fun, Arity} | fun_arity_from_tokens(Rest)];
fun_arity_from_tokens([_ | Rest]) ->
    fun_arity_from_tokens(Rest);
fun_arity_from_tokens(_) ->
    [].

field_list_from_tokens([#token{kind=atom, value=Field, line=Line, 
                               offset=Offset, length=Length} | Rest]) ->
    {Extra, NewRest} = get_upto2(Rest, ','),
    [{Field, {{Line, Line, Offset}, Length}, to_string(Extra)}
         | field_list_from_tokens(NewRest)];
field_list_from_tokens([_ | Rest]) ->
    field_list_from_tokens(Rest);
field_list_from_tokens(_) ->
    [].

to_string(Tokens) ->
    S = erlide_scanner:tokens_to_string(Tokens),
    erlide_text:strip(S).
%%     unspacify(S).

get_head(T) ->
    A = get_args(T),
    case get_guards(T) of
        "" ->
            A;
        G ->
            A ++ " when " ++ G
    end.

get_args(T) ->
    case erlide_np_util:get_between_outer_pars(T, '(', ')') of
        [] ->
            "";
        P ->
            "("++to_string(P)++")"
    end.

get_guards([]) ->
    "";
get_guards(T) ->
    to_string(get_between(T, 'when', '->')). 

last_not_eof(L) -> 
    case lists:reverse(L) of
        [eof, Last | _] ->
            Last;
        [Last | _] ->
            Last;
        _ ->
            error
    end.    


get_between(L, A, B) ->
    R = get_upto(L, B),
    lists:reverse(get_upto(lists:reverse(R), A)).

get_upto(L, Delim) ->
        get_upto(L, Delim, []).

get_upto([], _Delim, _Acc) ->
    [];
get_upto([#token{kind=Delim} | _], Delim, Acc) ->
        lists:reverse(Acc);
get_upto([T | Rest], Delim, Acc) ->
    get_upto(Rest, Delim, [T | Acc]).

get_upto2(L, Delim) ->
    get_upto2(L, Delim, []).

get_upto2([], _Delim, Acc) ->
    {lists:reverse(Acc), []};
get_upto2([#token{kind=Delim} | Rest], Delim, Acc) ->
    {lists:reverse(Acc), Rest};
get_upto2([T | Rest], Delim, Acc) ->
    get_upto2(Rest, Delim, [T | Acc]).

check_clause([#token{kind = ';'} | Rest], false) ->
    check_class(Rest) == function;
check_clause(_, _) ->
    false.

split_clauses(F) ->
    split_clauses(F, false, [], []).

split_clauses([], _HaveWhen, Acc, []) ->
    erlide_util:reverse2(Acc);
split_clauses([], HaveWhen, Acc, ClAcc) ->
    split_clauses([], HaveWhen, [ClAcc | Acc], []);
split_clauses([#token{kind=Kind}=T | Rest], _HaveWhen, Acc, ClAcc) 
  when Kind=:='end'; Kind=:=dot; Kind=:='->'; Kind=:='when' ->
    split_clauses(Rest, Kind=:='when', Acc, [T | ClAcc]);
split_clauses([T | Rest] = Tokens, HaveWhen, Acc, ClAcc) ->
    case check_clause(Tokens, HaveWhen) of
        false ->
            split_clauses(Rest, HaveWhen, Acc, [T | ClAcc]);
        true ->
            split_clauses(Rest, HaveWhen, [[T | ClAcc] | Acc], [])
    end.

%% fix_clause([#token{kind=atom, value=Name, line=Line, offset=Offset, length=Length} | Rest] = Code) ->
fix_clauses(Clauses) ->
    fix_clauses(Clauses, []).

fix_clauses([], Acc) ->
    lists:reverse(Acc);
fix_clauses([C | Rest], Acc) ->
    {Clause, Refs} = fix_clause(C),
    fix_clauses(Rest, [{Clause, Refs} | Acc]).

fix_clause([#token{kind=AtomOrMacro, value=Name, line=Line, offset=Offset, length=Length} | Rest]) 
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    #token{line=LastLine, offset=LastOffset, length=LastLength} = last_not_eof(Rest),
    PosLength = LastOffset - Offset + LastLength+1,
    ExternalRefs = get_refs_in_code(Rest),
    {#clause{pos={{Line, LastLine, Offset}, PosLength}, name_pos={{Line, Offset}, Length},
             name=Name, args=get_function_args(Rest), head=get_head(Rest)},
     ExternalRefs}.

get_function_args(Tokens) ->
    P = erlide_np_util:get_between_outer_pars(Tokens, '(', ')'),
    L = erlide_text:split_comma_list(P),
    [to_string(A) || A <- L].

fix_refs(ClausesAndRefs, Function, Imports) ->
    fix_refs(ClausesAndRefs, Function, Imports, []).

fix_refs([], #function{name=Name, arity=Arity, name_pos={{_, Offset}, Length}}, _Imports, Acc) ->
    Ref = #ref{data=#function_def{function=Name, arity=Arity}, offset=Offset, 
               length=Length, function=Name, arity=Arity, clause="", sub_clause=false},
    [Ref | Acc];
fix_refs([{Clause, Refs} | Rest], Function, Imports, Acc0) ->
    Acc1 = fix_refs(Refs, Clause, Function, Imports, Acc0),
    fix_refs(Rest, Function, Imports, Acc1).

fix_refs([], _Clause, _Function, _Imports, Acc) ->
    Acc;
fix_refs([{Offset, Length, RefData} | Rest], #clause{head=Head}=Clause, 
         #function{name=Name, arity=Arity, clauses=Clauses}=Function, 
         Imports, Acc) ->
    SubClause = case Clauses of
                    [] -> false;
                    [_] -> false;
                    _ -> true
                end,
    NewRefData = fix_imported_ref(RefData, Imports),
    Ref = #ref{data=NewRefData, offset=Offset, length=Length, function=Name, 
               arity=Arity, clause=Head, sub_clause=SubClause},
    fix_refs(Rest, Clause, Function, Imports, [Ref | Acc]).

fix_imported_ref(#local_call{function=F, arity=A}=RefData, [{Module, Imports} | Rest]) ->
    case lists:member({F, A}, Imports) of
        true -> #external_call{module=Module, function=F, arity=A};
        false -> fix_imported_ref(RefData, Rest)
    end;
fix_imported_ref(RefData, _Imports) ->
    RefData.

%% find references in code
%% VERY simple, M:F(_ ...), F(_ ...), #R, ?M, and that's it
%% returns {Offset, Length, Ref}, which is made into proper refs
%% by fix_refs
get_refs_in_code(L) ->
    get_refs_in_code(L, []).

get_refs_in_code([], Acc) ->
    lists:reverse(Acc);
get_refs_in_code([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
                  #token{kind=atom, value=F, offset=Offset2, length=Length2},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length2+Offset2-Offset, 
         #external_call{module = M, function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=atom, value=F, offset=Offset, length=Length},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length, #local_call{function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest], 
                 Acc) ->
    R = {Offset, Length, #macro_ref{macro=M}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind='#', offset=Offset}, 
                  #token{kind=atom, value=Record, offset=Offset2, length=Length2} | Rest],
                 Acc) ->
    R = {Offset, Length2+Offset2-Offset, #record_ref{record=Record}},
    {NewRest, Fields, RightSides} = erlide_np_records:check_fields(Rest, Record),
    NewAcc = get_refs_in_code(RightSides, Acc),
    get_refs_in_code(NewRest, Fields ++ [R | NewAcc]);
get_refs_in_code([#token{kind=var, value=V, offset=Offset, length=Length} | Rest],
                 Acc) ->
    R = {Offset, Length, make_var_def_ref(Acc, V)},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([_ | Rest], Acc) ->
    get_refs_in_code(Rest, Acc).

make_var_def_ref([], Var) ->
	#var_def{variable=Var};
make_var_def_ref([{_, _, #var_def{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([{_, _, #var_ref{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([_ | Rest], Var) ->
	make_var_def_ref(Rest, Var).

%% find type references in spec, type and opaque
%% also VERY simple, type(), ?M, #R, only
%% returns proper refs
get_refs(Tokens, Name, Arity) ->
	get_refs(Tokens, Name, Arity, []).

get_refs([], _, _, Acc) ->
    lists:reverse(Acc);
get_refs([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #macro_ref{macro=M}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind='#', offset=Offset},
          #token{kind=atom, value=R, offset=Offset2, length=Length2} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset, #record_ref{record=R}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
		  #token{kind=atom, value=T, offset=Offset2, length=Length2},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset,
                   #type_ref{module=M, type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=T, offset=Offset, length=Length},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #type_ref{module='_', type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=variable, value=V, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #var_ref{variable=V}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([_ | Rest], Name, Arity, Acc) ->
    get_refs(Rest, Name, Arity, Acc).

get_record_field_defs([], _) ->
    [];
get_record_field_defs([{FieldName, {{_, _, Offset}, Length}, _Extra} | Rest],
                      RecordName) ->
    [make_ref(Offset, Length, #record_field_def{record=RecordName, field=FieldName}, 
              FieldName, ?ARI_RECORD_FIELD_DEF)
         | get_record_field_defs(Rest, RecordName)].

make_ref(Offset, Length, Data, Name, Arity) ->
    #ref{data=Data, offset=Offset, length=Length, function=Name, arity=Arity, 
         clause="", sub_clause=false}.

make_attribute_arg_refs(define, Name, [#token{}, #token{kind='('} | Rest]) ->
    get_refs(skip_to_rparen(Rest), Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(define, Name, [#token{}, #token{kind=','} | Rest]) ->
    get_refs(Rest, Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(record, {Name, Fields}, [#token{}, #token{kind=','} | Rest]) ->
    get_record_field_defs(Fields, Name) ++ get_refs(Rest, Name, ?ARI_RECORD_DEF);
make_attribute_arg_refs(_, _, _) ->
    [].

skip_to_rparen([]) ->
    [];
skip_to_rparen([#token{kind=')'} | Rest]) ->
    Rest;
skip_to_rparen([_ | Rest]) ->
    skip_to_rparen(Rest).

make_attribute_ref(Name, Between, Extra, Offset, Length) ->
    ?D({Name, Between, Offset, Length}),
    case make_attribute_ref(Name, Between, Extra) of
        [] ->
            [];
        {Arity, AName, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=AName, 
                  arity=Arity, clause="", sub_clause=false}];
        {Arity, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=Name, 
                  arity=Arity, clause="", sub_clause=false}]
    end.

make_attribute_ref(module, _, Extra) ->
    {?ARI_ATTRIBUTE, #module_def{module=Extra}};
make_attribute_ref(record, Between, _E) ->
    ?D({Between, _E}),
    Name = case Between of
               {N, _} -> N;
               N -> N
           end,
    R= #record_def{record=Name},
    ?D(R),
    {?ARI_RECORD_DEF, Name, R};
make_attribute_ref(define, [Name | _], _) when is_list(Name) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(define, Name, _) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(include, _, Extra) ->
    {?ARI_INCLUDE, #include_ref{filename=unquote(Extra)}};
make_attribute_ref(_, _, _) ->
    [].

unquote(L) ->
    lists:reverse(unquote_first(lists:reverse(unquote_first(L)))).

unquote_first([$" | Rest]) ->
    Rest;
unquote_first(L) ->
    L.

remove_rest([], _) -> [];
remove_rest(Rest, Rest) -> [];
remove_rest([Hd | Tl], Rest) -> [Hd | remove_rest(Tl, Rest)].
%% Description: Split source files into functions and clauses and collects references
%% Author: jakob
%% Created: Mar 23, 2006

-module(erlide_noparse).

%%
%% Exported Functions
%%

%% called from Java
-export([initial_parse/5, reparse/1, remove_cache_files/2]).

%% called from Erlang
-export([read_module_refs/3]).

%%
%% Include files
%%

%% -define(DEBUG, 1).
%% -define(IO_FORMAT_DEBUG, 1).

-define(CACHE_VERSION, 25).
-define(SERVER, erlide_noparse).

-include("erlide.hrl").
-include("erlide_noparse.hrl").
-include("erlide_scanner.hrl").
-include("erlide_search_server.hrl").

%% special arity flags
-define(ARI_TYPESPEC, -2).
-define(ARI_ATTRIBUTE, -3).
-define(ARI_RECORD_DEF, -4).
-define(ARI_MACRO_DEF, -5).
-define(ARI_INCLUDE, -6).
-define(ARI_RECORD_FIELD_DEF, -7).

-define(X(X), erlide_log:log({X, erlang:now()})).


%%
%% API Functions
%%

initial_parse(ScannerName, ModuleFileName, StateDir, UseCache,
              UpdateSearchServer) ->
    try
        %%         ?D({StateDir, ModuleFileName}),
        BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
        RefsFileName = BaseName ++ ".refs",
        RenewFun = fun(_F) ->
                           do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer)
                   end,
        CacheFileName = BaseName ++ ".noparse",
        ?D(CacheFileName),
        {Cached, Res} = erlide_util:check_and_renew_cached(
                          ModuleFileName, CacheFileName, ?CACHE_VERSION, 
                          RenewFun, UseCache),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        ?D(updated),
        {ok, Res, Cached}
    catch
        error:Reason ->
            {error, Reason}
    end.

reparse(ScannerName) ->
    try
        Res = do_parse(ScannerName, "", "", true),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        {ok, Res, unused}
    catch
        error:Reason ->
            {error, Reason}
    end.

read_module_refs(ScannerName, ModulePath, StateDir) ->
    ?D(ScannerName),
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    RefsFileName = BaseName ++ ".refs",
    ?D(RefsFileName),
    case file:read_file(RefsFileName) of
        {ok, Binary} ->
            ?D(byte_size(Binary)),
            binary_to_term(Binary);
        _ ->
            ?D(ModulePath),
            {ok, InitialTextBin} = file:read_file(ModulePath),
            ?D(byte_size(InitialTextBin)),
            InitialText = binary_to_list(InitialTextBin),
            _D = erlide_scanner_server:initialScan(
                   ScannerName, ModulePath, InitialText, StateDir, true),
            ?D(_D),
            initial_parse(ScannerName, ModulePath, StateDir, true, false),
            ?D(RefsFileName),
            {ok, Binary} = file:read_file(RefsFileName),
            ?D(byte_size(Binary)),
            binary_to_term(Binary)
    end.

remove_cache_files(ScannerName, StateDir) ->
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    ScannerCacheFileName = BaseName ++ ".scan",
    file:delete(ScannerCacheFileName),
    RefsFileName = BaseName ++ ".refs",
    file:delete(RefsFileName),
    CacheFileName = BaseName ++ ".noparse",
    file:delete(CacheFileName).

%%
%% Internal functions
%%

do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer) ->
    Toks = erlide_scanner_server:getTokens(ScannerName),
    do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer).

do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer) ->
    ?D({do_parse, ScannerName, length(Toks)}),
    ?X(111),
    {UncommentToks, Comments} = erlide_np_util:extract_comments(Toks),
    ?X(222),
    ?D({length(UncommentToks), length(Comments)}),
    ?D({UncommentToks}),
    Functions = erlide_np_util:split_after_dots(UncommentToks),
    ?X(333),
    ?D(length(Functions)),
    AutoImports = erlide_util:add_auto_imported([]),
    ?X(444),
%%     ?D(AutoImports),
    {Collected, Refs} = classify_and_collect(Functions, [], [], [], AutoImports),
    ?X(555),
    ?D({'>>',length(Collected)}),
    CommentedCollected = erlide_np_util:get_function_comments(Collected, Comments),
    ?X(666),
    ?D(CommentedCollected),
    Model = #model{forms=CommentedCollected, comments=Comments},
    %%erlide_noparse_server:create(ScannerName, Model),
%%     ?D({"Model", length(Model#model.forms), erts_debug:flat_size(Model)}),
    FixedModel = fixup_model(Model),
    ?X(777),
%%     ?D(erts_debug:flat_size(FixedModel)),
    %% 	?D([erts_debug:flat_size(F) || F <- element(2, FixedModel)]),
    ?D(StateDir),
    case StateDir of
        "" -> ok;
        _ ->
            ?D({RefsFileName, length(Refs)}),
            file:write_file(RefsFileName, term_to_binary(Refs, [compressed]))
    end,
    update_search_server(UpdateSearchServer, ScannerName, Refs),
%%     ?D(FixedModel),
    ?X(888),
    FixedModel.

update_search_server(true, ScannerName, Refs) ->
    erlide_search_server:add_module_refs(ScannerName, Refs);
update_search_server(_, _, _) ->
    ok.

fixup_model(#model{forms=Forms, comments=Comments}) ->
    FixedComments = fixup_tokens(Comments),
    FixedForms = fixup_forms(Forms),
    #model{forms=FixedForms, comments=FixedComments}.

fixup_forms(Forms) ->
    [fixup_form(Form) || Form <- Forms].

fixup_tokens(Tokens) ->
    [fixup_token(Token) || Token <- Tokens].

fixup_token(#token{value=Value} = Token) when is_list(Value) ->
    Token#token{value=to_binary(Value)};
fixup_token(#token{text=Text} = Token) when is_list(Text) ->
    Token#token{value=to_binary(Text)};
fixup_token(Token) ->
    Token.

binary_args(Args) when is_list(Args) ->
    [iolist_to_binary(A) || A <- Args];
binary_args(_) ->
    [].

fixup_form(#function{comment=Comment, clauses=Clauses, args=Args} = Function) ->
    Function#function{comment= to_binary(Comment), clauses=fixup_forms(Clauses), args=binary_args(Args)};
fixup_form(#clause{head=Head, args=Args} = Clause) ->
    Clause#clause{head=to_binary(Head), args=binary_args(Args)};
fixup_form(Other) ->
    Other.

to_binary(Comment) when is_list(Comment) ->
    try 
        iolist_to_binary(Comment) 
    catch 
        _:_ -> 
            unicode:characters_to_binary(Comment) 
    end;
to_binary(Other) ->
    Other.

%% parse_test(ScannerName, File) ->
%%     erlide_scanner_server:scan_uncached(ScannerName, File, ""),
%%     Toks = erlide_scanner_server:getTokens(ScannerName),
%%     {UncommentToks, Comments} = extract_comments(Toks),
%%     Functions = split_after_dots(UncommentToks, [], []),
%%     {Collected, Refs} = classify_and_collect(Functions, [], []),
%%     _Model = #model{forms=Collected, comments=Comments},
%%     %%erlide_noparse_server:create(ScannerName, Model, ""),
%%     ok.

classify_and_collect([], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([eof], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([C | Rest], Acc, RefsAcc, Exports, Imports) ->
    {R, Refs, MoreExports, MoreImports} = cac(check_class(C), C, Exports, Imports),
    classify_and_collect(Rest, [R | Acc], Refs++RefsAcc, MoreExports++Exports, MoreImports++Imports).

cac(function, Tokens, Exports, Imports) ->
    ClauseList = split_clauses(Tokens),
    %?D(ClauseList),
    ClausesAndRefs = fix_clauses(ClauseList),
    %?D(length(Clauses)),
    [{#clause{pos=P, name=N, args=A, head=H, name_pos=NP}, _Refs} | _] = ClausesAndRefs,
    ?D(A),
    Arity = length(A),
    Exported = get_exported({N, Arity}, Exports),
    Function = case ClausesAndRefs of  	% only show subclauses when more than one
                   [_] ->
                       #function{pos=P, name=N, arity=Arity, args=A, head=H, 
                                 clauses=[], name_pos=NP, exported=Exported};
                   _ ->
                       Clauses = [C || {C, _} <- ClausesAndRefs],
                       #function{pos=P, name=N, arity=Arity, clauses=Clauses, 
                                 name_pos=NP, exported=Exported}
               end,
    {Function, fix_refs(ClausesAndRefs, Function, Imports), [], []};
cac(attribute, Attribute, _Exports, _Imports) ->
    ?D(Attribute),
    case Attribute of
        %% -spec, -type or -opaque
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=Kind, line=_Line, offset=_Offset, value=Name} | Args]
          when (Kind=:='spec') or ((Kind=:=atom) and (Name=:='type'))
                   or ((Kind=:=atom) and (Name=:='opaque'))->
            get_type_attribute(Kind, Name, Offset, Line, Attribute, Args);
        %% other attributes
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=atom, value=Name, line=_Line, offset=_Offset},
         _, #token{value=Args} | _] = Attribute ->
            get_other_attribute(Name, Offset, Line, Attribute, Args);
        [_, #token{kind=atom, value=Name, line=Line, offset=Offset} | _] ->
            #token{line=LastLine, offset=LastOffset, 
                   length=LastLength} = last_not_eof(Attribute),
            PosLength = LastOffset - Offset + LastLength,
            {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                       name=Name, args=[]}, [], [], []}
    end;
cac(other, [#token{value=Name, line=Line, offset=Offset, length=Length} | _], 
    _Exports, _Imports) ->
    {#other{pos={{Line, Line, Offset}, Length}, name=Name}, [], [], []};
cac(_, _D, _E, _I) ->
    {eof, [], [], []}.

get_type_attribute(Kind, Name0, Offset, Line, Attribute, Args) ->
    ?D(Name0),
    Name = case Kind of 'spec' -> Kind; _ -> Name0 end,
    #token{line=LastLine, offset=LastOffset,
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    ?D(Args),
    Extra = to_string(Args),
    ?D(Extra),
    {AttrArgs, _, _} = get_attribute_args(Kind, Args, Args),
    ?D({AttrArgs, Extra}),
    ExternalRefs = get_refs(tl(AttrArgs), Extra, ?ARI_TYPESPEC),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     [#ref{data=#type_def{type=Name}, offset=Offset, length=PosLength, function=Name, 
           arity=?ARI_TYPESPEC, clause="", sub_clause=false} | ExternalRefs], [], []}.

get_other_attribute(Name, Offset, Line, Attribute, Args) ->
    #token{line=LastLine, offset=LastOffset, 
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    Between = erlide_np_util:get_between_outer_pars(Attribute, '(', ')'),
    Extra = to_string(Between),
    {AttrArgs, Exports, Imports} = get_attribute_args(Name, Between, Args),
    ?D({AttrArgs, Between}),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     make_attribute_ref(Name, AttrArgs, Extra, Offset, PosLength)++
         make_attribute_arg_refs(Name, AttrArgs, Between),
     Exports, Imports}.

get_exported(F_A, Exports) ->
    lists:member(F_A, Exports).

get_attribute_args(import, Between, _Args) ->
    From = get_first_of_kind(atom, Between),
    Tokens = get_between(Between, '[', ']'),
    Import = {From, fun_arity_from_tokens(Tokens)},
    {Import, [], [Import]};
get_attribute_args(export, Between, _Args) ->
    Tokens = get_between(Between, '[', ']'),
    Exports = fun_arity_from_tokens(Tokens),
    {Exports, Exports, []};
get_attribute_args(record, Between, _Args) ->
    [RecordToken | _] = Between,
    RecordName = RecordToken#token.value,
    Tokens = erlide_np_util:get_between_outer_pars(Between, '{', '}'),
    R = {RecordName, field_list_from_tokens(Tokens)},
    {R, [], []};
get_attribute_args(_, _Between, Args) ->
    {Args, [], []}.

check_class([#token{kind = AtomOrMacro}, #token{kind = '('} | _])
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    function;
check_class([#token{kind = '-'}, #token{kind = atom} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'spec'} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'type'} | _]) ->
    attribute;
check_class(_X) ->
    %?D(lists:sublist(_X, 5)),
    other.

get_first_of_kind(Kind, Tokens) ->
    case erlide_np_util:skip_to(Tokens, Kind) of
        [#token{kind=Kind, value=Value} | _] ->
            Value;
        _ ->
            undefined
    end.
        
fun_arity_from_tokens([#token{kind=atom, value=Fun}, #token{kind='/'}, 
                       #token{kind=integer, value=Arity} | Rest]) ->
    [{Fun, Arity} | fun_arity_from_tokens(Rest)];
fun_arity_from_tokens([_ | Rest]) ->
    fun_arity_from_tokens(Rest);
fun_arity_from_tokens(_) ->
    [].

field_list_from_tokens([#token{kind=atom, value=Field, line=Line, 
                               offset=Offset, length=Length} | Rest]) ->
    {Extra, NewRest} = get_upto2(Rest, ','),
    [{Field, {{Line, Line, Offset}, Length}, to_string(Extra)}
         | field_list_from_tokens(NewRest)];
field_list_from_tokens([_ | Rest]) ->
    field_list_from_tokens(Rest);
field_list_from_tokens(_) ->
    [].

to_string(Tokens) ->
    S = erlide_scanner:tokens_to_string(Tokens),
    erlide_text:strip(S).
%%     unspacify(S).

get_head(T) ->
    A = get_args(T),
    case get_guards(T) of
        "" ->
            A;
        G ->
            A ++ " when " ++ G
    end.

get_args(T) ->
    case erlide_np_util:get_between_outer_pars(T, '(', ')') of
        [] ->
            "";
        P ->
            "("++to_string(P)++")"
    end.

get_guards([]) ->
    "";
get_guards(T) ->
    to_string(get_between(T, 'when', '->')). 

last_not_eof(L) -> 
    case lists:reverse(L) of
        [eof, Last | _] ->
            Last;
        [Last | _] ->
            Last;
        _ ->
            error
    end.    


get_between(L, A, B) ->
    R = get_upto(L, B),
    lists:reverse(get_upto(lists:reverse(R), A)).

get_upto(L, Delim) ->
        get_upto(L, Delim, []).

get_upto([], _Delim, _Acc) ->
    [];
get_upto([#token{kind=Delim} | _], Delim, Acc) ->
        lists:reverse(Acc);
get_upto([T | Rest], Delim, Acc) ->
    get_upto(Rest, Delim, [T | Acc]).

get_upto2(L, Delim) ->
    get_upto2(L, Delim, []).

get_upto2([], _Delim, Acc) ->
    {lists:reverse(Acc), []};
get_upto2([#token{kind=Delim} | Rest], Delim, Acc) ->
    {lists:reverse(Acc), Rest};
get_upto2([T | Rest], Delim, Acc) ->
    get_upto2(Rest, Delim, [T | Acc]).

check_clause([#token{kind = ';'} | Rest], false) ->
    check_class(Rest) == function;
check_clause(_, _) ->
    false.

split_clauses(F) ->
    split_clauses(F, false, [], []).

split_clauses([], _HaveWhen, Acc, []) ->
    erlide_util:reverse2(Acc);
split_clauses([], HaveWhen, Acc, ClAcc) ->
    split_clauses([], HaveWhen, [ClAcc | Acc], []);
split_clauses([#token{kind=Kind}=T | Rest], _HaveWhen, Acc, ClAcc) 
  when Kind=:='end'; Kind=:=dot; Kind=:='->'; Kind=:='when' ->
    split_clauses(Rest, Kind=:='when', Acc, [T | ClAcc]);
split_clauses([T | Rest] = Tokens, HaveWhen, Acc, ClAcc) ->
    case check_clause(Tokens, HaveWhen) of
        false ->
            split_clauses(Rest, HaveWhen, Acc, [T | ClAcc]);
        true ->
            split_clauses(Rest, HaveWhen, [[T | ClAcc] | Acc], [])
    end.

%% fix_clause([#token{kind=atom, value=Name, line=Line, offset=Offset, length=Length} | Rest] = Code) ->
fix_clauses(Clauses) ->
    fix_clauses(Clauses, []).

fix_clauses([], Acc) ->
    lists:reverse(Acc);
fix_clauses([C | Rest], Acc) ->
    {Clause, Refs} = fix_clause(C),
    fix_clauses(Rest, [{Clause, Refs} | Acc]).

fix_clause([#token{kind=AtomOrMacro, value=Name, line=Line, offset=Offset, length=Length} | Rest]) 
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    #token{line=LastLine, offset=LastOffset, length=LastLength} = last_not_eof(Rest),
    PosLength = LastOffset - Offset + LastLength+1,
    ExternalRefs = get_refs_in_code(Rest),
    {#clause{pos={{Line, LastLine, Offset}, PosLength}, name_pos={{Line, Offset}, Length},
             name=Name, args=get_function_args(Rest), head=get_head(Rest)},
     ExternalRefs}.

get_function_args(Tokens) ->
    P = erlide_np_util:get_between_outer_pars(Tokens, '(', ')'),
    L = erlide_text:split_comma_list(P),
    [to_string(A) || A <- L].

fix_refs(ClausesAndRefs, Function, Imports) ->
    fix_refs(ClausesAndRefs, Function, Imports, []).

fix_refs([], #function{name=Name, arity=Arity, name_pos={{_, Offset}, Length}}, _Imports, Acc) ->
    Ref = #ref{data=#function_def{function=Name, arity=Arity}, offset=Offset, 
               length=Length, function=Name, arity=Arity, clause="", sub_clause=false},
    [Ref | Acc];
fix_refs([{Clause, Refs} | Rest], Function, Imports, Acc0) ->
    Acc1 = fix_refs(Refs, Clause, Function, Imports, Acc0),
    fix_refs(Rest, Function, Imports, Acc1).

fix_refs([], _Clause, _Function, _Imports, Acc) ->
    Acc;
fix_refs([{Offset, Length, RefData} | Rest], #clause{head=Head}=Clause, 
         #function{name=Name, arity=Arity, clauses=Clauses}=Function, 
         Imports, Acc) ->
    SubClause = case Clauses of
                    [] -> false;
                    [_] -> false;
                    _ -> true
                end,
    NewRefData = fix_imported_ref(RefData, Imports),
    Ref = #ref{data=NewRefData, offset=Offset, length=Length, function=Name, 
               arity=Arity, clause=Head, sub_clause=SubClause},
    fix_refs(Rest, Clause, Function, Imports, [Ref | Acc]).

fix_imported_ref(#local_call{function=F, arity=A}=RefData, [{Module, Imports} | Rest]) ->
    case lists:member({F, A}, Imports) of
        true -> #external_call{module=Module, function=F, arity=A};
        false -> fix_imported_ref(RefData, Rest)
    end;
fix_imported_ref(RefData, _Imports) ->
    RefData.

%% find references in code
%% VERY simple, M:F(_ ...), F(_ ...), #R, ?M, and that's it
%% returns {Offset, Length, Ref}, which is made into proper refs
%% by fix_refs
get_refs_in_code(L) ->
    get_refs_in_code(L, []).

get_refs_in_code([], Acc) ->
    lists:reverse(Acc);
get_refs_in_code([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
                  #token{kind=atom, value=F, offset=Offset2, length=Length2},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length2+Offset2-Offset, 
         #external_call{module = M, function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=atom, value=F, offset=Offset, length=Length},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length, #local_call{function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest], 
                 Acc) ->
    R = {Offset, Length, #macro_ref{macro=M}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind='#', offset=Offset}, 
                  #token{kind=atom, value=Record, offset=Offset2, length=Length2} | Rest],
                 Acc) ->
    R = {Offset, Length2+Offset2-Offset, #record_ref{record=Record}},
    {NewRest, Fields, RightSides} = erlide_np_records:check_fields(Rest, Record),
    NewAcc = get_refs_in_code(RightSides, Acc),
    get_refs_in_code(NewRest, Fields ++ [R | NewAcc]);
get_refs_in_code([#token{kind=var, value=V, offset=Offset, length=Length} | Rest],
                 Acc) ->
    R = {Offset, Length, make_var_def_ref(Acc, V)},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([_ | Rest], Acc) ->
    get_refs_in_code(Rest, Acc).

make_var_def_ref([], Var) ->
	#var_def{variable=Var};
make_var_def_ref([{_, _, #var_def{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([{_, _, #var_ref{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([_ | Rest], Var) ->
	make_var_def_ref(Rest, Var).

%% find type references in spec, type and opaque
%% also VERY simple, type(), ?M, #R, only
%% returns proper refs
get_refs(Tokens, Name, Arity) ->
	get_refs(Tokens, Name, Arity, []).

get_refs([], _, _, Acc) ->
    lists:reverse(Acc);
get_refs([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #macro_ref{macro=M}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind='#', offset=Offset},
          #token{kind=atom, value=R, offset=Offset2, length=Length2} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset, #record_ref{record=R}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
		  #token{kind=atom, value=T, offset=Offset2, length=Length2},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset,
                   #type_ref{module=M, type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=T, offset=Offset, length=Length},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #type_ref{module='_', type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=variable, value=V, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #var_ref{variable=V}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([_ | Rest], Name, Arity, Acc) ->
    get_refs(Rest, Name, Arity, Acc).

get_record_field_defs([], _) ->
    [];
get_record_field_defs([{FieldName, {{_, _, Offset}, Length}, _Extra} | Rest],
                      RecordName) ->
    [make_ref(Offset, Length, #record_field_def{record=RecordName, field=FieldName}, 
              FieldName, ?ARI_RECORD_FIELD_DEF)
         | get_record_field_defs(Rest, RecordName)].

make_ref(Offset, Length, Data, Name, Arity) ->
    #ref{data=Data, offset=Offset, length=Length, function=Name, arity=Arity, 
         clause="", sub_clause=false}.

make_attribute_arg_refs(define, Name, [#token{}, #token{kind='('} | Rest]) ->
    get_refs(skip_to_rparen(Rest), Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(define, Name, [#token{}, #token{kind=','} | Rest]) ->
    get_refs(Rest, Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(record, {Name, Fields}, [#token{}, #token{kind=','} | Rest]) ->
    get_record_field_defs(Fields, Name) ++ get_refs(Rest, Name, ?ARI_RECORD_DEF);
make_attribute_arg_refs(_, _, _) ->
    [].

skip_to_rparen([]) ->
    [];
skip_to_rparen([#token{kind=')'} | Rest]) ->
    Rest;
skip_to_rparen([_ | Rest]) ->
    skip_to_rparen(Rest).

make_attribute_ref(Name, Between, Extra, Offset, Length) ->
    ?D({Name, Between, Offset, Length}),
    case make_attribute_ref(Name, Between, Extra) of
        [] ->
            [];
        {Arity, AName, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=AName, 
                  arity=Arity, clause="", sub_clause=false}];
        {Arity, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=Name, 
                  arity=Arity, clause="", sub_clause=false}]
    end.

make_attribute_ref(module, _, Extra) ->
    {?ARI_ATTRIBUTE, #module_def{module=Extra}};
make_attribute_ref(record, Between, _E) ->
    ?D({Between, _E}),
    Name = case Between of
               {N, _} -> N;
               N -> N
           end,
    R= #record_def{record=Name},
    ?D(R),
    {?ARI_RECORD_DEF, Name, R};
make_attribute_ref(define, [Name | _], _) when is_list(Name) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(define, Name, _) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(include, _, Extra) ->
    {?ARI_INCLUDE, #include_ref{filename=unquote(Extra)}};
make_attribute_ref(_, _, _) ->
    [].

unquote(L) ->
    lists:reverse(unquote_first(lists:reverse(unquote_first(L)))).

unquote_first([$" | Rest]) ->
    Rest;
unquote_first(L) ->
    L.

remove_rest([], _) -> [];
remove_rest(Rest, Rest) -> [];
remove_rest([Hd | Tl], Rest) -> [Hd | remove_rest(Tl, Rest)].
%% Description: Split source files into functions and clauses and collects references
%% Author: jakob
%% Created: Mar 23, 2006

-module(erlide_noparse).

%%
%% Exported Functions
%%

%% called from Java
-export([initial_parse/5, reparse/1, remove_cache_files/2]).

%% called from Erlang
-export([read_module_refs/3]).

%%
%% Include files
%%

%% -define(DEBUG, 1).
%% -define(IO_FORMAT_DEBUG, 1).

-define(CACHE_VERSION, 25).
-define(SERVER, erlide_noparse).

-include("erlide.hrl").
-include("erlide_noparse.hrl").
-include("erlide_scanner.hrl").
-include("erlide_search_server.hrl").

%% special arity flags
-define(ARI_TYPESPEC, -2).
-define(ARI_ATTRIBUTE, -3).
-define(ARI_RECORD_DEF, -4).
-define(ARI_MACRO_DEF, -5).
-define(ARI_INCLUDE, -6).
-define(ARI_RECORD_FIELD_DEF, -7).

-define(X(X), erlide_log:log({X, erlang:now()})).


%%
%% API Functions
%%

initial_parse(ScannerName, ModuleFileName, StateDir, UseCache,
              UpdateSearchServer) ->
    try
        %%         ?D({StateDir, ModuleFileName}),
        BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
        RefsFileName = BaseName ++ ".refs",
        RenewFun = fun(_F) ->
                           do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer)
                   end,
        CacheFileName = BaseName ++ ".noparse",
        ?D(CacheFileName),
        {Cached, Res} = erlide_util:check_and_renew_cached(
                          ModuleFileName, CacheFileName, ?CACHE_VERSION, 
                          RenewFun, UseCache),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        ?D(updated),
        {ok, Res, Cached}
    catch
        error:Reason ->
            {error, Reason}
    end.

reparse(ScannerName) ->
    try
        Res = do_parse(ScannerName, "", "", true),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        {ok, Res, unused}
    catch
        error:Reason ->
            {error, Reason}
    end.

read_module_refs(ScannerName, ModulePath, StateDir) ->
    ?D(ScannerName),
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    RefsFileName = BaseName ++ ".refs",
    ?D(RefsFileName),
    case file:read_file(RefsFileName) of
        {ok, Binary} ->
            ?D(byte_size(Binary)),
            binary_to_term(Binary);
        _ ->
            ?D(ModulePath),
            {ok, InitialTextBin} = file:read_file(ModulePath),
            ?D(byte_size(InitialTextBin)),
            InitialText = binary_to_list(InitialTextBin),
            _D = erlide_scanner_server:initialScan(
                   ScannerName, ModulePath, InitialText, StateDir, true),
            ?D(_D),
            initial_parse(ScannerName, ModulePath, StateDir, true, false),
            ?D(RefsFileName),
            {ok, Binary} = file:read_file(RefsFileName),
            ?D(byte_size(Binary)),
            binary_to_term(Binary)
    end.

remove_cache_files(ScannerName, StateDir) ->
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    ScannerCacheFileName = BaseName ++ ".scan",
    file:delete(ScannerCacheFileName),
    RefsFileName = BaseName ++ ".refs",
    file:delete(RefsFileName),
    CacheFileName = BaseName ++ ".noparse",
    file:delete(CacheFileName).

%%
%% Internal functions
%%

do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer) ->
    Toks = erlide_scanner_server:getTokens(ScannerName),
    do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer).

do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer) ->
    ?D({do_parse, ScannerName, length(Toks)}),
    ?X(111),
    {UncommentToks, Comments} = erlide_np_util:extract_comments(Toks),
    ?X(222),
    ?D({length(UncommentToks), length(Comments)}),
    ?D({UncommentToks}),
    Functions = erlide_np_util:split_after_dots(UncommentToks),
    ?X(333),
    ?D(length(Functions)),
    AutoImports = erlide_util:add_auto_imported([]),
    ?X(444),
%%     ?D(AutoImports),
    {Collected, Refs} = classify_and_collect(Functions, [], [], [], AutoImports),
    ?X(555),
    ?D({'>>',length(Collected)}),
    CommentedCollected = erlide_np_util:get_function_comments(Collected, Comments),
    ?X(666),
    ?D(CommentedCollected),
    Model = #model{forms=CommentedCollected, comments=Comments},
    %%erlide_noparse_server:create(ScannerName, Model),
%%     ?D({"Model", length(Model#model.forms), erts_debug:flat_size(Model)}),
    FixedModel = fixup_model(Model),
    ?X(777),
%%     ?D(erts_debug:flat_size(FixedModel)),
    %% 	?D([erts_debug:flat_size(F) || F <- element(2, FixedModel)]),
    ?D(StateDir),
    case StateDir of
        "" -> ok;
        _ ->
            ?D({RefsFileName, length(Refs)}),
            file:write_file(RefsFileName, term_to_binary(Refs, [compressed]))
    end,
    update_search_server(UpdateSearchServer, ScannerName, Refs),
%%     ?D(FixedModel),
    ?X(888),
    FixedModel.

update_search_server(true, ScannerName, Refs) ->
    erlide_search_server:add_module_refs(ScannerName, Refs);
update_search_server(_, _, _) ->
    ok.

fixup_model(#model{forms=Forms, comments=Comments}) ->
    FixedComments = fixup_tokens(Comments),
    FixedForms = fixup_forms(Forms),
    #model{forms=FixedForms, comments=FixedComments}.

fixup_forms(Forms) ->
    [fixup_form(Form) || Form <- Forms].

fixup_tokens(Tokens) ->
    [fixup_token(Token) || Token <- Tokens].

fixup_token(#token{value=Value} = Token) when is_list(Value) ->
    Token#token{value=to_binary(Value)};
fixup_token(#token{text=Text} = Token) when is_list(Text) ->
    Token#token{value=to_binary(Text)};
fixup_token(Token) ->
    Token.

binary_args(Args) when is_list(Args) ->
    [iolist_to_binary(A) || A <- Args];
binary_args(_) ->
    [].

fixup_form(#function{comment=Comment, clauses=Clauses, args=Args} = Function) ->
    Function#function{comment= to_binary(Comment), clauses=fixup_forms(Clauses), args=binary_args(Args)};
fixup_form(#clause{head=Head, args=Args} = Clause) ->
    Clause#clause{head=to_binary(Head), args=binary_args(Args)};
fixup_form(Other) ->
    Other.

to_binary(Comment) when is_list(Comment) ->
    try 
        iolist_to_binary(Comment) 
    catch 
        _:_ -> 
            unicode:characters_to_binary(Comment) 
    end;
to_binary(Other) ->
    Other.

%% parse_test(ScannerName, File) ->
%%     erlide_scanner_server:scan_uncached(ScannerName, File, ""),
%%     Toks = erlide_scanner_server:getTokens(ScannerName),
%%     {UncommentToks, Comments} = extract_comments(Toks),
%%     Functions = split_after_dots(UncommentToks, [], []),
%%     {Collected, Refs} = classify_and_collect(Functions, [], []),
%%     _Model = #model{forms=Collected, comments=Comments},
%%     %%erlide_noparse_server:create(ScannerName, Model, ""),
%%     ok.

classify_and_collect([], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([eof], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([C | Rest], Acc, RefsAcc, Exports, Imports) ->
    {R, Refs, MoreExports, MoreImports} = cac(check_class(C), C, Exports, Imports),
    classify_and_collect(Rest, [R | Acc], Refs++RefsAcc, MoreExports++Exports, MoreImports++Imports).

cac(function, Tokens, Exports, Imports) ->
    ClauseList = split_clauses(Tokens),
    %?D(ClauseList),
    ClausesAndRefs = fix_clauses(ClauseList),
    %?D(length(Clauses)),
    [{#clause{pos=P, name=N, args=A, head=H, name_pos=NP}, _Refs} | _] = ClausesAndRefs,
    ?D(A),
    Arity = length(A),
    Exported = get_exported({N, Arity}, Exports),
    Function = case ClausesAndRefs of  	% only show subclauses when more than one
                   [_] ->
                       #function{pos=P, name=N, arity=Arity, args=A, head=H, 
                                 clauses=[], name_pos=NP, exported=Exported};
                   _ ->
                       Clauses = [C || {C, _} <- ClausesAndRefs],
                       #function{pos=P, name=N, arity=Arity, clauses=Clauses, 
                                 name_pos=NP, exported=Exported}
               end,
    {Function, fix_refs(ClausesAndRefs, Function, Imports), [], []};
cac(attribute, Attribute, _Exports, _Imports) ->
    ?D(Attribute),
    case Attribute of
        %% -spec, -type or -opaque
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=Kind, line=_Line, offset=_Offset, value=Name} | Args]
          when (Kind=:='spec') or ((Kind=:=atom) and (Name=:='type'))
                   or ((Kind=:=atom) and (Name=:='opaque'))->
            get_type_attribute(Kind, Name, Offset, Line, Attribute, Args);
        %% other attributes
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=atom, value=Name, line=_Line, offset=_Offset},
         _, #token{value=Args} | _] = Attribute ->
            get_other_attribute(Name, Offset, Line, Attribute, Args);
        [_, #token{kind=atom, value=Name, line=Line, offset=Offset} | _] ->
            #token{line=LastLine, offset=LastOffset, 
                   length=LastLength} = last_not_eof(Attribute),
            PosLength = LastOffset - Offset + LastLength,
            {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                       name=Name, args=[]}, [], [], []}
    end;
cac(other, [#token{value=Name, line=Line, offset=Offset, length=Length} | _], 
    _Exports, _Imports) ->
    {#other{pos={{Line, Line, Offset}, Length}, name=Name}, [], [], []};
cac(_, _D, _E, _I) ->
    {eof, [], [], []}.

get_type_attribute(Kind, Name0, Offset, Line, Attribute, Args) ->
    ?D(Name0),
    Name = case Kind of 'spec' -> Kind; _ -> Name0 end,
    #token{line=LastLine, offset=LastOffset,
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    ?D(Args),
    Extra = to_string(Args),
    ?D(Extra),
    {AttrArgs, _, _} = get_attribute_args(Kind, Args, Args),
    ?D({AttrArgs, Extra}),
    ExternalRefs = get_refs(tl(AttrArgs), Extra, ?ARI_TYPESPEC),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     [#ref{data=#type_def{type=Name}, offset=Offset, length=PosLength, function=Name, 
           arity=?ARI_TYPESPEC, clause="", sub_clause=false} | ExternalRefs], [], []}.

get_other_attribute(Name, Offset, Line, Attribute, Args) ->
    #token{line=LastLine, offset=LastOffset, 
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    Between = erlide_np_util:get_between_outer_pars(Attribute, '(', ')'),
    Extra = to_string(Between),
    {AttrArgs, Exports, Imports} = get_attribute_args(Name, Between, Args),
    ?D({AttrArgs, Between}),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     make_attribute_ref(Name, AttrArgs, Extra, Offset, PosLength)++
         make_attribute_arg_refs(Name, AttrArgs, Between),
     Exports, Imports}.

get_exported(F_A, Exports) ->
    lists:member(F_A, Exports).

get_attribute_args(import, Between, _Args) ->
    From = get_first_of_kind(atom, Between),
    Tokens = get_between(Between, '[', ']'),
    Import = {From, fun_arity_from_tokens(Tokens)},
    {Import, [], [Import]};
get_attribute_args(export, Between, _Args) ->
    Tokens = get_between(Between, '[', ']'),
    Exports = fun_arity_from_tokens(Tokens),
    {Exports, Exports, []};
get_attribute_args(record, Between, _Args) ->
    [RecordToken | _] = Between,
    RecordName = RecordToken#token.value,
    Tokens = erlide_np_util:get_between_outer_pars(Between, '{', '}'),
    R = {RecordName, field_list_from_tokens(Tokens)},
    {R, [], []};
get_attribute_args(_, _Between, Args) ->
    {Args, [], []}.

check_class([#token{kind = AtomOrMacro}, #token{kind = '('} | _])
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    function;
check_class([#token{kind = '-'}, #token{kind = atom} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'spec'} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'type'} | _]) ->
    attribute;
check_class(_X) ->
    %?D(lists:sublist(_X, 5)),
    other.

get_first_of_kind(Kind, Tokens) ->
    case erlide_np_util:skip_to(Tokens, Kind) of
        [#token{kind=Kind, value=Value} | _] ->
            Value;
        _ ->
            undefined
    end.
        
fun_arity_from_tokens([#token{kind=atom, value=Fun}, #token{kind='/'}, 
                       #token{kind=integer, value=Arity} | Rest]) ->
    [{Fun, Arity} | fun_arity_from_tokens(Rest)];
fun_arity_from_tokens([_ | Rest]) ->
    fun_arity_from_tokens(Rest);
fun_arity_from_tokens(_) ->
    [].

field_list_from_tokens([#token{kind=atom, value=Field, line=Line, 
                               offset=Offset, length=Length} | Rest]) ->
    {Extra, NewRest} = get_upto2(Rest, ','),
    [{Field, {{Line, Line, Offset}, Length}, to_string(Extra)}
         | field_list_from_tokens(NewRest)];
field_list_from_tokens([_ | Rest]) ->
    field_list_from_tokens(Rest);
field_list_from_tokens(_) ->
    [].

to_string(Tokens) ->
    S = erlide_scanner:tokens_to_string(Tokens),
    erlide_text:strip(S).
%%     unspacify(S).

get_head(T) ->
    A = get_args(T),
    case get_guards(T) of
        "" ->
            A;
        G ->
            A ++ " when " ++ G
    end.

get_args(T) ->
    case erlide_np_util:get_between_outer_pars(T, '(', ')') of
        [] ->
            "";
        P ->
            "("++to_string(P)++")"
    end.

get_guards([]) ->
    "";
get_guards(T) ->
    to_string(get_between(T, 'when', '->')). 

last_not_eof(L) -> 
    case lists:reverse(L) of
        [eof, Last | _] ->
            Last;
        [Last | _] ->
            Last;
        _ ->
            error
    end.    


get_between(L, A, B) ->
    R = get_upto(L, B),
    lists:reverse(get_upto(lists:reverse(R), A)).

get_upto(L, Delim) ->
        get_upto(L, Delim, []).

get_upto([], _Delim, _Acc) ->
    [];
get_upto([#token{kind=Delim} | _], Delim, Acc) ->
        lists:reverse(Acc);
get_upto([T | Rest], Delim, Acc) ->
    get_upto(Rest, Delim, [T | Acc]).

get_upto2(L, Delim) ->
    get_upto2(L, Delim, []).

get_upto2([], _Delim, Acc) ->
    {lists:reverse(Acc), []};
get_upto2([#token{kind=Delim} | Rest], Delim, Acc) ->
    {lists:reverse(Acc), Rest};
get_upto2([T | Rest], Delim, Acc) ->
    get_upto2(Rest, Delim, [T | Acc]).

check_clause([#token{kind = ';'} | Rest], false) ->
    check_class(Rest) == function;
check_clause(_, _) ->
    false.

split_clauses(F) ->
    split_clauses(F, false, [], []).

split_clauses([], _HaveWhen, Acc, []) ->
    erlide_util:reverse2(Acc);
split_clauses([], HaveWhen, Acc, ClAcc) ->
    split_clauses([], HaveWhen, [ClAcc | Acc], []);
split_clauses([#token{kind=Kind}=T | Rest], _HaveWhen, Acc, ClAcc) 
  when Kind=:='end'; Kind=:=dot; Kind=:='->'; Kind=:='when' ->
    split_clauses(Rest, Kind=:='when', Acc, [T | ClAcc]);
split_clauses([T | Rest] = Tokens, HaveWhen, Acc, ClAcc) ->
    case check_clause(Tokens, HaveWhen) of
        false ->
            split_clauses(Rest, HaveWhen, Acc, [T | ClAcc]);
        true ->
            split_clauses(Rest, HaveWhen, [[T | ClAcc] | Acc], [])
    end.

%% fix_clause([#token{kind=atom, value=Name, line=Line, offset=Offset, length=Length} | Rest] = Code) ->
fix_clauses(Clauses) ->
    fix_clauses(Clauses, []).

fix_clauses([], Acc) ->
    lists:reverse(Acc);
fix_clauses([C | Rest], Acc) ->
    {Clause, Refs} = fix_clause(C),
    fix_clauses(Rest, [{Clause, Refs} | Acc]).

fix_clause([#token{kind=AtomOrMacro, value=Name, line=Line, offset=Offset, length=Length} | Rest]) 
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    #token{line=LastLine, offset=LastOffset, length=LastLength} = last_not_eof(Rest),
    PosLength = LastOffset - Offset + LastLength+1,
    ExternalRefs = get_refs_in_code(Rest),
    {#clause{pos={{Line, LastLine, Offset}, PosLength}, name_pos={{Line, Offset}, Length},
             name=Name, args=get_function_args(Rest), head=get_head(Rest)},
     ExternalRefs}.

get_function_args(Tokens) ->
    P = erlide_np_util:get_between_outer_pars(Tokens, '(', ')'),
    L = erlide_text:split_comma_list(P),
    [to_string(A) || A <- L].

fix_refs(ClausesAndRefs, Function, Imports) ->
    fix_refs(ClausesAndRefs, Function, Imports, []).

fix_refs([], #function{name=Name, arity=Arity, name_pos={{_, Offset}, Length}}, _Imports, Acc) ->
    Ref = #ref{data=#function_def{function=Name, arity=Arity}, offset=Offset, 
               length=Length, function=Name, arity=Arity, clause="", sub_clause=false},
    [Ref | Acc];
fix_refs([{Clause, Refs} | Rest], Function, Imports, Acc0) ->
    Acc1 = fix_refs(Refs, Clause, Function, Imports, Acc0),
    fix_refs(Rest, Function, Imports, Acc1).

fix_refs([], _Clause, _Function, _Imports, Acc) ->
    Acc;
fix_refs([{Offset, Length, RefData} | Rest], #clause{head=Head}=Clause, 
         #function{name=Name, arity=Arity, clauses=Clauses}=Function, 
         Imports, Acc) ->
    SubClause = case Clauses of
                    [] -> false;
                    [_] -> false;
                    _ -> true
                end,
    NewRefData = fix_imported_ref(RefData, Imports),
    Ref = #ref{data=NewRefData, offset=Offset, length=Length, function=Name, 
               arity=Arity, clause=Head, sub_clause=SubClause},
    fix_refs(Rest, Clause, Function, Imports, [Ref | Acc]).

fix_imported_ref(#local_call{function=F, arity=A}=RefData, [{Module, Imports} | Rest]) ->
    case lists:member({F, A}, Imports) of
        true -> #external_call{module=Module, function=F, arity=A};
        false -> fix_imported_ref(RefData, Rest)
    end;
fix_imported_ref(RefData, _Imports) ->
    RefData.

%% find references in code
%% VERY simple, M:F(_ ...), F(_ ...), #R, ?M, and that's it
%% returns {Offset, Length, Ref}, which is made into proper refs
%% by fix_refs
get_refs_in_code(L) ->
    get_refs_in_code(L, []).

get_refs_in_code([], Acc) ->
    lists:reverse(Acc);
get_refs_in_code([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
                  #token{kind=atom, value=F, offset=Offset2, length=Length2},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length2+Offset2-Offset, 
         #external_call{module = M, function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=atom, value=F, offset=Offset, length=Length},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length, #local_call{function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest], 
                 Acc) ->
    R = {Offset, Length, #macro_ref{macro=M}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind='#', offset=Offset}, 
                  #token{kind=atom, value=Record, offset=Offset2, length=Length2} | Rest],
                 Acc) ->
    R = {Offset, Length2+Offset2-Offset, #record_ref{record=Record}},
    {NewRest, Fields, RightSides} = erlide_np_records:check_fields(Rest, Record),
    NewAcc = get_refs_in_code(RightSides, Acc),
    get_refs_in_code(NewRest, Fields ++ [R | NewAcc]);
get_refs_in_code([#token{kind=var, value=V, offset=Offset, length=Length} | Rest],
                 Acc) ->
    R = {Offset, Length, make_var_def_ref(Acc, V)},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([_ | Rest], Acc) ->
    get_refs_in_code(Rest, Acc).

make_var_def_ref([], Var) ->
	#var_def{variable=Var};
make_var_def_ref([{_, _, #var_def{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([{_, _, #var_ref{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([_ | Rest], Var) ->
	make_var_def_ref(Rest, Var).

%% find type references in spec, type and opaque
%% also VERY simple, type(), ?M, #R, only
%% returns proper refs
get_refs(Tokens, Name, Arity) ->
	get_refs(Tokens, Name, Arity, []).

get_refs([], _, _, Acc) ->
    lists:reverse(Acc);
get_refs([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #macro_ref{macro=M}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind='#', offset=Offset},
          #token{kind=atom, value=R, offset=Offset2, length=Length2} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset, #record_ref{record=R}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
		  #token{kind=atom, value=T, offset=Offset2, length=Length2},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset,
                   #type_ref{module=M, type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=T, offset=Offset, length=Length},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #type_ref{module='_', type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=variable, value=V, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #var_ref{variable=V}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([_ | Rest], Name, Arity, Acc) ->
    get_refs(Rest, Name, Arity, Acc).

get_record_field_defs([], _) ->
    [];
get_record_field_defs([{FieldName, {{_, _, Offset}, Length}, _Extra} | Rest],
                      RecordName) ->
    [make_ref(Offset, Length, #record_field_def{record=RecordName, field=FieldName}, 
              FieldName, ?ARI_RECORD_FIELD_DEF)
         | get_record_field_defs(Rest, RecordName)].

make_ref(Offset, Length, Data, Name, Arity) ->
    #ref{data=Data, offset=Offset, length=Length, function=Name, arity=Arity, 
         clause="", sub_clause=false}.

make_attribute_arg_refs(define, Name, [#token{}, #token{kind='('} | Rest]) ->
    get_refs(skip_to_rparen(Rest), Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(define, Name, [#token{}, #token{kind=','} | Rest]) ->
    get_refs(Rest, Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(record, {Name, Fields}, [#token{}, #token{kind=','} | Rest]) ->
    get_record_field_defs(Fields, Name) ++ get_refs(Rest, Name, ?ARI_RECORD_DEF);
make_attribute_arg_refs(_, _, _) ->
    [].

skip_to_rparen([]) ->
    [];
skip_to_rparen([#token{kind=')'} | Rest]) ->
    Rest;
skip_to_rparen([_ | Rest]) ->
    skip_to_rparen(Rest).

make_attribute_ref(Name, Between, Extra, Offset, Length) ->
    ?D({Name, Between, Offset, Length}),
    case make_attribute_ref(Name, Between, Extra) of
        [] ->
            [];
        {Arity, AName, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=AName, 
                  arity=Arity, clause="", sub_clause=false}];
        {Arity, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=Name, 
                  arity=Arity, clause="", sub_clause=false}]
    end.

make_attribute_ref(module, _, Extra) ->
    {?ARI_ATTRIBUTE, #module_def{module=Extra}};
make_attribute_ref(record, Between, _E) ->
    ?D({Between, _E}),
    Name = case Between of
               {N, _} -> N;
               N -> N
           end,
    R= #record_def{record=Name},
    ?D(R),
    {?ARI_RECORD_DEF, Name, R};
make_attribute_ref(define, [Name | _], _) when is_list(Name) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(define, Name, _) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(include, _, Extra) ->
    {?ARI_INCLUDE, #include_ref{filename=unquote(Extra)}};
make_attribute_ref(_, _, _) ->
    [].

unquote(L) ->
    lists:reverse(unquote_first(lists:reverse(unquote_first(L)))).

unquote_first([$" | Rest]) ->
    Rest;
unquote_first(L) ->
    L.

remove_rest([], _) -> [];
remove_rest(Rest, Rest) -> [];
remove_rest([Hd | Tl], Rest) -> [Hd | remove_rest(Tl, Rest)].
%% Description: Split source files into functions and clauses and collects references
%% Author: jakob
%% Created: Mar 23, 2006

-module(erlide_noparse).

%%
%% Exported Functions
%%

%% called from Java
-export([initial_parse/5, reparse/1, remove_cache_files/2]).

%% called from Erlang
-export([read_module_refs/3]).

%%
%% Include files
%%

%% -define(DEBUG, 1).
%% -define(IO_FORMAT_DEBUG, 1).

-define(CACHE_VERSION, 25).
-define(SERVER, erlide_noparse).

-include("erlide.hrl").
-include("erlide_noparse.hrl").
-include("erlide_scanner.hrl").
-include("erlide_search_server.hrl").

%% special arity flags
-define(ARI_TYPESPEC, -2).
-define(ARI_ATTRIBUTE, -3).
-define(ARI_RECORD_DEF, -4).
-define(ARI_MACRO_DEF, -5).
-define(ARI_INCLUDE, -6).
-define(ARI_RECORD_FIELD_DEF, -7).

-define(X(X), erlide_log:log({X, erlang:now()})).


%%
%% API Functions
%%

initial_parse(ScannerName, ModuleFileName, StateDir, UseCache,
              UpdateSearchServer) ->
    try
        %%         ?D({StateDir, ModuleFileName}),
        BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
        RefsFileName = BaseName ++ ".refs",
        RenewFun = fun(_F) ->
                           do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer)
                   end,
        CacheFileName = BaseName ++ ".noparse",
        ?D(CacheFileName),
        {Cached, Res} = erlide_util:check_and_renew_cached(
                          ModuleFileName, CacheFileName, ?CACHE_VERSION, 
                          RenewFun, UseCache),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        ?D(updated),
        {ok, Res, Cached}
    catch
        error:Reason ->
            {error, Reason}
    end.

reparse(ScannerName) ->
    try
        Res = do_parse(ScannerName, "", "", true),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        {ok, Res, unused}
    catch
        error:Reason ->
            {error, Reason}
    end.

read_module_refs(ScannerName, ModulePath, StateDir) ->
    ?D(ScannerName),
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    RefsFileName = BaseName ++ ".refs",
    ?D(RefsFileName),
    case file:read_file(RefsFileName) of
        {ok, Binary} ->
            ?D(byte_size(Binary)),
            binary_to_term(Binary);
        _ ->
            ?D(ModulePath),
            {ok, InitialTextBin} = file:read_file(ModulePath),
            ?D(byte_size(InitialTextBin)),
            InitialText = binary_to_list(InitialTextBin),
            _D = erlide_scanner_server:initialScan(
                   ScannerName, ModulePath, InitialText, StateDir, true),
            ?D(_D),
            initial_parse(ScannerName, ModulePath, StateDir, true, false),
            ?D(RefsFileName),
            {ok, Binary} = file:read_file(RefsFileName),
            ?D(byte_size(Binary)),
            binary_to_term(Binary)
    end.

remove_cache_files(ScannerName, StateDir) ->
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    ScannerCacheFileName = BaseName ++ ".scan",
    file:delete(ScannerCacheFileName),
    RefsFileName = BaseName ++ ".refs",
    file:delete(RefsFileName),
    CacheFileName = BaseName ++ ".noparse",
    file:delete(CacheFileName).

%%
%% Internal functions
%%

do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer) ->
    Toks = erlide_scanner_server:getTokens(ScannerName),
    do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer).

do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer) ->
    ?D({do_parse, ScannerName, length(Toks)}),
    ?X(111),
    {UncommentToks, Comments} = erlide_np_util:extract_comments(Toks),
    ?X(222),
    ?D({length(UncommentToks), length(Comments)}),
    ?D({UncommentToks}),
    Functions = erlide_np_util:split_after_dots(UncommentToks),
    ?X(333),
    ?D(length(Functions)),
    AutoImports = erlide_util:add_auto_imported([]),
    ?X(444),
%%     ?D(AutoImports),
    {Collected, Refs} = classify_and_collect(Functions, [], [], [], AutoImports),
    ?X(555),
    ?D({'>>',length(Collected)}),
    CommentedCollected = erlide_np_util:get_function_comments(Collected, Comments),
    ?X(666),
    ?D(CommentedCollected),
    Model = #model{forms=CommentedCollected, comments=Comments},
    %%erlide_noparse_server:create(ScannerName, Model),
%%     ?D({"Model", length(Model#model.forms), erts_debug:flat_size(Model)}),
    FixedModel = fixup_model(Model),
    ?X(777),
%%     ?D(erts_debug:flat_size(FixedModel)),
    %% 	?D([erts_debug:flat_size(F) || F <- element(2, FixedModel)]),
    ?D(StateDir),
    case StateDir of
        "" -> ok;
        _ ->
            ?D({RefsFileName, length(Refs)}),
            file:write_file(RefsFileName, term_to_binary(Refs, [compressed]))
    end,
    update_search_server(UpdateSearchServer, ScannerName, Refs),
%%     ?D(FixedModel),
    ?X(888),
    FixedModel.

update_search_server(true, ScannerName, Refs) ->
    erlide_search_server:add_module_refs(ScannerName, Refs);
update_search_server(_, _, _) ->
    ok.

fixup_model(#model{forms=Forms, comments=Comments}) ->
    FixedComments = fixup_tokens(Comments),
    FixedForms = fixup_forms(Forms),
    #model{forms=FixedForms, comments=FixedComments}.

fixup_forms(Forms) ->
    [fixup_form(Form) || Form <- Forms].

fixup_tokens(Tokens) ->
    [fixup_token(Token) || Token <- Tokens].

fixup_token(#token{value=Value} = Token) when is_list(Value) ->
    Token#token{value=to_binary(Value)};
fixup_token(#token{text=Text} = Token) when is_list(Text) ->
    Token#token{value=to_binary(Text)};
fixup_token(Token) ->
    Token.

binary_args(Args) when is_list(Args) ->
    [iolist_to_binary(A) || A <- Args];
binary_args(_) ->
    [].

fixup_form(#function{comment=Comment, clauses=Clauses, args=Args} = Function) ->
    Function#function{comment= to_binary(Comment), clauses=fixup_forms(Clauses), args=binary_args(Args)};
fixup_form(#clause{head=Head, args=Args} = Clause) ->
    Clause#clause{head=to_binary(Head), args=binary_args(Args)};
fixup_form(Other) ->
    Other.

to_binary(Comment) when is_list(Comment) ->
    try 
        iolist_to_binary(Comment) 
    catch 
        _:_ -> 
            unicode:characters_to_binary(Comment) 
    end;
to_binary(Other) ->
    Other.

%% parse_test(ScannerName, File) ->
%%     erlide_scanner_server:scan_uncached(ScannerName, File, ""),
%%     Toks = erlide_scanner_server:getTokens(ScannerName),
%%     {UncommentToks, Comments} = extract_comments(Toks),
%%     Functions = split_after_dots(UncommentToks, [], []),
%%     {Collected, Refs} = classify_and_collect(Functions, [], []),
%%     _Model = #model{forms=Collected, comments=Comments},
%%     %%erlide_noparse_server:create(ScannerName, Model, ""),
%%     ok.

classify_and_collect([], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([eof], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([C | Rest], Acc, RefsAcc, Exports, Imports) ->
    {R, Refs, MoreExports, MoreImports} = cac(check_class(C), C, Exports, Imports),
    classify_and_collect(Rest, [R | Acc], Refs++RefsAcc, MoreExports++Exports, MoreImports++Imports).

cac(function, Tokens, Exports, Imports) ->
    ClauseList = split_clauses(Tokens),
    %?D(ClauseList),
    ClausesAndRefs = fix_clauses(ClauseList),
    %?D(length(Clauses)),
    [{#clause{pos=P, name=N, args=A, head=H, name_pos=NP}, _Refs} | _] = ClausesAndRefs,
    ?D(A),
    Arity = length(A),
    Exported = get_exported({N, Arity}, Exports),
    Function = case ClausesAndRefs of  	% only show subclauses when more than one
                   [_] ->
                       #function{pos=P, name=N, arity=Arity, args=A, head=H, 
                                 clauses=[], name_pos=NP, exported=Exported};
                   _ ->
                       Clauses = [C || {C, _} <- ClausesAndRefs],
                       #function{pos=P, name=N, arity=Arity, clauses=Clauses, 
                                 name_pos=NP, exported=Exported}
               end,
    {Function, fix_refs(ClausesAndRefs, Function, Imports), [], []};
cac(attribute, Attribute, _Exports, _Imports) ->
    ?D(Attribute),
    case Attribute of
        %% -spec, -type or -opaque
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=Kind, line=_Line, offset=_Offset, value=Name} | Args]
          when (Kind=:='spec') or ((Kind=:=atom) and (Name=:='type'))
                   or ((Kind=:=atom) and (Name=:='opaque'))->
            get_type_attribute(Kind, Name, Offset, Line, Attribute, Args);
        %% other attributes
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=atom, value=Name, line=_Line, offset=_Offset},
         _, #token{value=Args} | _] = Attribute ->
            get_other_attribute(Name, Offset, Line, Attribute, Args);
        [_, #token{kind=atom, value=Name, line=Line, offset=Offset} | _] ->
            #token{line=LastLine, offset=LastOffset, 
                   length=LastLength} = last_not_eof(Attribute),
            PosLength = LastOffset - Offset + LastLength,
            {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                       name=Name, args=[]}, [], [], []}
    end;
cac(other, [#token{value=Name, line=Line, offset=Offset, length=Length} | _], 
    _Exports, _Imports) ->
    {#other{pos={{Line, Line, Offset}, Length}, name=Name}, [], [], []};
cac(_, _D, _E, _I) ->
    {eof, [], [], []}.

get_type_attribute(Kind, Name0, Offset, Line, Attribute, Args) ->
    ?D(Name0),
    Name = case Kind of 'spec' -> Kind; _ -> Name0 end,
    #token{line=LastLine, offset=LastOffset,
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    ?D(Args),
    Extra = to_string(Args),
    ?D(Extra),
    {AttrArgs, _, _} = get_attribute_args(Kind, Args, Args),
    ?D({AttrArgs, Extra}),
    ExternalRefs = get_refs(tl(AttrArgs), Extra, ?ARI_TYPESPEC),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     [#ref{data=#type_def{type=Name}, offset=Offset, length=PosLength, function=Name, 
           arity=?ARI_TYPESPEC, clause="", sub_clause=false} | ExternalRefs], [], []}.

get_other_attribute(Name, Offset, Line, Attribute, Args) ->
    #token{line=LastLine, offset=LastOffset, 
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    Between = erlide_np_util:get_between_outer_pars(Attribute, '(', ')'),
    Extra = to_string(Between),
    {AttrArgs, Exports, Imports} = get_attribute_args(Name, Between, Args),
    ?D({AttrArgs, Between}),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     make_attribute_ref(Name, AttrArgs, Extra, Offset, PosLength)++
         make_attribute_arg_refs(Name, AttrArgs, Between),
     Exports, Imports}.

get_exported(F_A, Exports) ->
    lists:member(F_A, Exports).

get_attribute_args(import, Between, _Args) ->
    From = get_first_of_kind(atom, Between),
    Tokens = get_between(Between, '[', ']'),
    Import = {From, fun_arity_from_tokens(Tokens)},
    {Import, [], [Import]};
get_attribute_args(export, Between, _Args) ->
    Tokens = get_between(Between, '[', ']'),
    Exports = fun_arity_from_tokens(Tokens),
    {Exports, Exports, []};
get_attribute_args(record, Between, _Args) ->
    [RecordToken | _] = Between,
    RecordName = RecordToken#token.value,
    Tokens = erlide_np_util:get_between_outer_pars(Between, '{', '}'),
    R = {RecordName, field_list_from_tokens(Tokens)},
    {R, [], []};
get_attribute_args(_, _Between, Args) ->
    {Args, [], []}.

check_class([#token{kind = AtomOrMacro}, #token{kind = '('} | _])
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    function;
check_class([#token{kind = '-'}, #token{kind = atom} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'spec'} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'type'} | _]) ->
    attribute;
check_class(_X) ->
    %?D(lists:sublist(_X, 5)),
    other.

get_first_of_kind(Kind, Tokens) ->
    case erlide_np_util:skip_to(Tokens, Kind) of
        [#token{kind=Kind, value=Value} | _] ->
            Value;
        _ ->
            undefined
    end.
        
fun_arity_from_tokens([#token{kind=atom, value=Fun}, #token{kind='/'}, 
                       #token{kind=integer, value=Arity} | Rest]) ->
    [{Fun, Arity} | fun_arity_from_tokens(Rest)];
fun_arity_from_tokens([_ | Rest]) ->
    fun_arity_from_tokens(Rest);
fun_arity_from_tokens(_) ->
    [].

field_list_from_tokens([#token{kind=atom, value=Field, line=Line, 
                               offset=Offset, length=Length} | Rest]) ->
    {Extra, NewRest} = get_upto2(Rest, ','),
    [{Field, {{Line, Line, Offset}, Length}, to_string(Extra)}
         | field_list_from_tokens(NewRest)];
field_list_from_tokens([_ | Rest]) ->
    field_list_from_tokens(Rest);
field_list_from_tokens(_) ->
    [].

to_string(Tokens) ->
    S = erlide_scanner:tokens_to_string(Tokens),
    erlide_text:strip(S).
%%     unspacify(S).

get_head(T) ->
    A = get_args(T),
    case get_guards(T) of
        "" ->
            A;
        G ->
            A ++ " when " ++ G
    end.

get_args(T) ->
    case erlide_np_util:get_between_outer_pars(T, '(', ')') of
        [] ->
            "";
        P ->
            "("++to_string(P)++")"
    end.

get_guards([]) ->
    "";
get_guards(T) ->
    to_string(get_between(T, 'when', '->')). 

last_not_eof(L) -> 
    case lists:reverse(L) of
        [eof, Last | _] ->
            Last;
        [Last | _] ->
            Last;
        _ ->
            error
    end.    


get_between(L, A, B) ->
    R = get_upto(L, B),
    lists:reverse(get_upto(lists:reverse(R), A)).

get_upto(L, Delim) ->
        get_upto(L, Delim, []).

get_upto([], _Delim, _Acc) ->
    [];
get_upto([#token{kind=Delim} | _], Delim, Acc) ->
        lists:reverse(Acc);
get_upto([T | Rest], Delim, Acc) ->
    get_upto(Rest, Delim, [T | Acc]).

get_upto2(L, Delim) ->
    get_upto2(L, Delim, []).

get_upto2([], _Delim, Acc) ->
    {lists:reverse(Acc), []};
get_upto2([#token{kind=Delim} | Rest], Delim, Acc) ->
    {lists:reverse(Acc), Rest};
get_upto2([T | Rest], Delim, Acc) ->
    get_upto2(Rest, Delim, [T | Acc]).

check_clause([#token{kind = ';'} | Rest], false) ->
    check_class(Rest) == function;
check_clause(_, _) ->
    false.

split_clauses(F) ->
    split_clauses(F, false, [], []).

split_clauses([], _HaveWhen, Acc, []) ->
    erlide_util:reverse2(Acc);
split_clauses([], HaveWhen, Acc, ClAcc) ->
    split_clauses([], HaveWhen, [ClAcc | Acc], []);
split_clauses([#token{kind=Kind}=T | Rest], _HaveWhen, Acc, ClAcc) 
  when Kind=:='end'; Kind=:=dot; Kind=:='->'; Kind=:='when' ->
    split_clauses(Rest, Kind=:='when', Acc, [T | ClAcc]);
split_clauses([T | Rest] = Tokens, HaveWhen, Acc, ClAcc) ->
    case check_clause(Tokens, HaveWhen) of
        false ->
            split_clauses(Rest, HaveWhen, Acc, [T | ClAcc]);
        true ->
            split_clauses(Rest, HaveWhen, [[T | ClAcc] | Acc], [])
    end.

%% fix_clause([#token{kind=atom, value=Name, line=Line, offset=Offset, length=Length} | Rest] = Code) ->
fix_clauses(Clauses) ->
    fix_clauses(Clauses, []).

fix_clauses([], Acc) ->
    lists:reverse(Acc);
fix_clauses([C | Rest], Acc) ->
    {Clause, Refs} = fix_clause(C),
    fix_clauses(Rest, [{Clause, Refs} | Acc]).

fix_clause([#token{kind=AtomOrMacro, value=Name, line=Line, offset=Offset, length=Length} | Rest]) 
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    #token{line=LastLine, offset=LastOffset, length=LastLength} = last_not_eof(Rest),
    PosLength = LastOffset - Offset + LastLength+1,
    ExternalRefs = get_refs_in_code(Rest),
    {#clause{pos={{Line, LastLine, Offset}, PosLength}, name_pos={{Line, Offset}, Length},
             name=Name, args=get_function_args(Rest), head=get_head(Rest)},
     ExternalRefs}.

get_function_args(Tokens) ->
    P = erlide_np_util:get_between_outer_pars(Tokens, '(', ')'),
    L = erlide_text:split_comma_list(P),
    [to_string(A) || A <- L].

fix_refs(ClausesAndRefs, Function, Imports) ->
    fix_refs(ClausesAndRefs, Function, Imports, []).

fix_refs([], #function{name=Name, arity=Arity, name_pos={{_, Offset}, Length}}, _Imports, Acc) ->
    Ref = #ref{data=#function_def{function=Name, arity=Arity}, offset=Offset, 
               length=Length, function=Name, arity=Arity, clause="", sub_clause=false},
    [Ref | Acc];
fix_refs([{Clause, Refs} | Rest], Function, Imports, Acc0) ->
    Acc1 = fix_refs(Refs, Clause, Function, Imports, Acc0),
    fix_refs(Rest, Function, Imports, Acc1).

fix_refs([], _Clause, _Function, _Imports, Acc) ->
    Acc;
fix_refs([{Offset, Length, RefData} | Rest], #clause{head=Head}=Clause, 
         #function{name=Name, arity=Arity, clauses=Clauses}=Function, 
         Imports, Acc) ->
    SubClause = case Clauses of
                    [] -> false;
                    [_] -> false;
                    _ -> true
                end,
    NewRefData = fix_imported_ref(RefData, Imports),
    Ref = #ref{data=NewRefData, offset=Offset, length=Length, function=Name, 
               arity=Arity, clause=Head, sub_clause=SubClause},
    fix_refs(Rest, Clause, Function, Imports, [Ref | Acc]).

fix_imported_ref(#local_call{function=F, arity=A}=RefData, [{Module, Imports} | Rest]) ->
    case lists:member({F, A}, Imports) of
        true -> #external_call{module=Module, function=F, arity=A};
        false -> fix_imported_ref(RefData, Rest)
    end;
fix_imported_ref(RefData, _Imports) ->
    RefData.

%% find references in code
%% VERY simple, M:F(_ ...), F(_ ...), #R, ?M, and that's it
%% returns {Offset, Length, Ref}, which is made into proper refs
%% by fix_refs
get_refs_in_code(L) ->
    get_refs_in_code(L, []).

get_refs_in_code([], Acc) ->
    lists:reverse(Acc);
get_refs_in_code([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
                  #token{kind=atom, value=F, offset=Offset2, length=Length2},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length2+Offset2-Offset, 
         #external_call{module = M, function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=atom, value=F, offset=Offset, length=Length},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length, #local_call{function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest], 
                 Acc) ->
    R = {Offset, Length, #macro_ref{macro=M}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind='#', offset=Offset}, 
                  #token{kind=atom, value=Record, offset=Offset2, length=Length2} | Rest],
                 Acc) ->
    R = {Offset, Length2+Offset2-Offset, #record_ref{record=Record}},
    {NewRest, Fields, RightSides} = erlide_np_records:check_fields(Rest, Record),
    NewAcc = get_refs_in_code(RightSides, Acc),
    get_refs_in_code(NewRest, Fields ++ [R | NewAcc]);
get_refs_in_code([#token{kind=var, value=V, offset=Offset, length=Length} | Rest],
                 Acc) ->
    R = {Offset, Length, make_var_def_ref(Acc, V)},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([_ | Rest], Acc) ->
    get_refs_in_code(Rest, Acc).

make_var_def_ref([], Var) ->
	#var_def{variable=Var};
make_var_def_ref([{_, _, #var_def{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([{_, _, #var_ref{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([_ | Rest], Var) ->
	make_var_def_ref(Rest, Var).

%% find type references in spec, type and opaque
%% also VERY simple, type(), ?M, #R, only
%% returns proper refs
get_refs(Tokens, Name, Arity) ->
	get_refs(Tokens, Name, Arity, []).

get_refs([], _, _, Acc) ->
    lists:reverse(Acc);
get_refs([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #macro_ref{macro=M}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind='#', offset=Offset},
          #token{kind=atom, value=R, offset=Offset2, length=Length2} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset, #record_ref{record=R}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
		  #token{kind=atom, value=T, offset=Offset2, length=Length2},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset,
                   #type_ref{module=M, type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=T, offset=Offset, length=Length},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #type_ref{module='_', type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=variable, value=V, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #var_ref{variable=V}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([_ | Rest], Name, Arity, Acc) ->
    get_refs(Rest, Name, Arity, Acc).

get_record_field_defs([], _) ->
    [];
get_record_field_defs([{FieldName, {{_, _, Offset}, Length}, _Extra} | Rest],
                      RecordName) ->
    [make_ref(Offset, Length, #record_field_def{record=RecordName, field=FieldName}, 
              FieldName, ?ARI_RECORD_FIELD_DEF)
         | get_record_field_defs(Rest, RecordName)].

make_ref(Offset, Length, Data, Name, Arity) ->
    #ref{data=Data, offset=Offset, length=Length, function=Name, arity=Arity, 
         clause="", sub_clause=false}.

make_attribute_arg_refs(define, Name, [#token{}, #token{kind='('} | Rest]) ->
    get_refs(skip_to_rparen(Rest), Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(define, Name, [#token{}, #token{kind=','} | Rest]) ->
    get_refs(Rest, Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(record, {Name, Fields}, [#token{}, #token{kind=','} | Rest]) ->
    get_record_field_defs(Fields, Name) ++ get_refs(Rest, Name, ?ARI_RECORD_DEF);
make_attribute_arg_refs(_, _, _) ->
    [].

skip_to_rparen([]) ->
    [];
skip_to_rparen([#token{kind=')'} | Rest]) ->
    Rest;
skip_to_rparen([_ | Rest]) ->
    skip_to_rparen(Rest).

make_attribute_ref(Name, Between, Extra, Offset, Length) ->
    ?D({Name, Between, Offset, Length}),
    case make_attribute_ref(Name, Between, Extra) of
        [] ->
            [];
        {Arity, AName, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=AName, 
                  arity=Arity, clause="", sub_clause=false}];
        {Arity, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=Name, 
                  arity=Arity, clause="", sub_clause=false}]
    end.

make_attribute_ref(module, _, Extra) ->
    {?ARI_ATTRIBUTE, #module_def{module=Extra}};
make_attribute_ref(record, Between, _E) ->
    ?D({Between, _E}),
    Name = case Between of
               {N, _} -> N;
               N -> N
           end,
    R= #record_def{record=Name},
    ?D(R),
    {?ARI_RECORD_DEF, Name, R};
make_attribute_ref(define, [Name | _], _) when is_list(Name) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(define, Name, _) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(include, _, Extra) ->
    {?ARI_INCLUDE, #include_ref{filename=unquote(Extra)}};
make_attribute_ref(_, _, _) ->
    [].

unquote(L) ->
    lists:reverse(unquote_first(lists:reverse(unquote_first(L)))).

unquote_first([$" | Rest]) ->
    Rest;
unquote_first(L) ->
    L.

remove_rest([], _) -> [];
remove_rest(Rest, Rest) -> [];
remove_rest([Hd | Tl], Rest) -> [Hd | remove_rest(Tl, Rest)].
%% Description: Split source files into functions and clauses and collects references
%% Author: jakob
%% Created: Mar 23, 2006

-module(erlide_noparse).

%%
%% Exported Functions
%%

%% called from Java
-export([initial_parse/5, reparse/1, remove_cache_files/2]).

%% called from Erlang
-export([read_module_refs/3]).

%%
%% Include files
%%

%% -define(DEBUG, 1).
%% -define(IO_FORMAT_DEBUG, 1).

-define(CACHE_VERSION, 25).
-define(SERVER, erlide_noparse).

-include("erlide.hrl").
-include("erlide_noparse.hrl").
-include("erlide_scanner.hrl").
-include("erlide_search_server.hrl").

%% special arity flags
-define(ARI_TYPESPEC, -2).
-define(ARI_ATTRIBUTE, -3).
-define(ARI_RECORD_DEF, -4).
-define(ARI_MACRO_DEF, -5).
-define(ARI_INCLUDE, -6).
-define(ARI_RECORD_FIELD_DEF, -7).

-define(X(X), erlide_log:log({X, erlang:now()})).


%%
%% API Functions
%%

initial_parse(ScannerName, ModuleFileName, StateDir, UseCache,
              UpdateSearchServer) ->
    try
        %%         ?D({StateDir, ModuleFileName}),
        BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
        RefsFileName = BaseName ++ ".refs",
        RenewFun = fun(_F) ->
                           do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer)
                   end,
        CacheFileName = BaseName ++ ".noparse",
        ?D(CacheFileName),
        {Cached, Res} = erlide_util:check_and_renew_cached(
                          ModuleFileName, CacheFileName, ?CACHE_VERSION, 
                          RenewFun, UseCache),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        ?D(updated),
        {ok, Res, Cached}
    catch
        error:Reason ->
            {error, Reason}
    end.

reparse(ScannerName) ->
    try
        Res = do_parse(ScannerName, "", "", true),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        {ok, Res, unused}
    catch
        error:Reason ->
            {error, Reason}
    end.

read_module_refs(ScannerName, ModulePath, StateDir) ->
    ?D(ScannerName),
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    RefsFileName = BaseName ++ ".refs",
    ?D(RefsFileName),
    case file:read_file(RefsFileName) of
        {ok, Binary} ->
            ?D(byte_size(Binary)),
            binary_to_term(Binary);
        _ ->
            ?D(ModulePath),
            {ok, InitialTextBin} = file:read_file(ModulePath),
            ?D(byte_size(InitialTextBin)),
            InitialText = binary_to_list(InitialTextBin),
            _D = erlide_scanner_server:initialScan(
                   ScannerName, ModulePath, InitialText, StateDir, true),
            ?D(_D),
            initial_parse(ScannerName, ModulePath, StateDir, true, false),
            ?D(RefsFileName),
            {ok, Binary} = file:read_file(RefsFileName),
            ?D(byte_size(Binary)),
            binary_to_term(Binary)
    end.

remove_cache_files(ScannerName, StateDir) ->
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    ScannerCacheFileName = BaseName ++ ".scan",
    file:delete(ScannerCacheFileName),
    RefsFileName = BaseName ++ ".refs",
    file:delete(RefsFileName),
    CacheFileName = BaseName ++ ".noparse",
    file:delete(CacheFileName).

%%
%% Internal functions
%%

do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer) ->
    Toks = erlide_scanner_server:getTokens(ScannerName),
    do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer).

do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer) ->
    ?D({do_parse, ScannerName, length(Toks)}),
    ?X(111),
    {UncommentToks, Comments} = erlide_np_util:extract_comments(Toks),
    ?X(222),
    ?D({length(UncommentToks), length(Comments)}),
    ?D({UncommentToks}),
    Functions = erlide_np_util:split_after_dots(UncommentToks),
    ?X(333),
    ?D(length(Functions)),
    AutoImports = erlide_util:add_auto_imported([]),
    ?X(444),
%%     ?D(AutoImports),
    {Collected, Refs} = classify_and_collect(Functions, [], [], [], AutoImports),
    ?X(555),
    ?D({'>>',length(Collected)}),
    CommentedCollected = erlide_np_util:get_function_comments(Collected, Comments),
    ?X(666),
    ?D(CommentedCollected),
    Model = #model{forms=CommentedCollected, comments=Comments},
    %%erlide_noparse_server:create(ScannerName, Model),
%%     ?D({"Model", length(Model#model.forms), erts_debug:flat_size(Model)}),
    FixedModel = fixup_model(Model),
    ?X(777),
%%     ?D(erts_debug:flat_size(FixedModel)),
    %% 	?D([erts_debug:flat_size(F) || F <- element(2, FixedModel)]),
    ?D(StateDir),
    case StateDir of
        "" -> ok;
        _ ->
            ?D({RefsFileName, length(Refs)}),
            file:write_file(RefsFileName, term_to_binary(Refs, [compressed]))
    end,
    update_search_server(UpdateSearchServer, ScannerName, Refs),
%%     ?D(FixedModel),
    ?X(888),
    FixedModel.

update_search_server(true, ScannerName, Refs) ->
    erlide_search_server:add_module_refs(ScannerName, Refs);
update_search_server(_, _, _) ->
    ok.

fixup_model(#model{forms=Forms, comments=Comments}) ->
    FixedComments = fixup_tokens(Comments),
    FixedForms = fixup_forms(Forms),
    #model{forms=FixedForms, comments=FixedComments}.

fixup_forms(Forms) ->
    [fixup_form(Form) || Form <- Forms].

fixup_tokens(Tokens) ->
    [fixup_token(Token) || Token <- Tokens].

fixup_token(#token{value=Value} = Token) when is_list(Value) ->
    Token#token{value=to_binary(Value)};
fixup_token(#token{text=Text} = Token) when is_list(Text) ->
    Token#token{value=to_binary(Text)};
fixup_token(Token) ->
    Token.

binary_args(Args) when is_list(Args) ->
    [iolist_to_binary(A) || A <- Args];
binary_args(_) ->
    [].

fixup_form(#function{comment=Comment, clauses=Clauses, args=Args} = Function) ->
    Function#function{comment= to_binary(Comment), clauses=fixup_forms(Clauses), args=binary_args(Args)};
fixup_form(#clause{head=Head, args=Args} = Clause) ->
    Clause#clause{head=to_binary(Head), args=binary_args(Args)};
fixup_form(Other) ->
    Other.

to_binary(Comment) when is_list(Comment) ->
    try 
        iolist_to_binary(Comment) 
    catch 
        _:_ -> 
            unicode:characters_to_binary(Comment) 
    end;
to_binary(Other) ->
    Other.

%% parse_test(ScannerName, File) ->
%%     erlide_scanner_server:scan_uncached(ScannerName, File, ""),
%%     Toks = erlide_scanner_server:getTokens(ScannerName),
%%     {UncommentToks, Comments} = extract_comments(Toks),
%%     Functions = split_after_dots(UncommentToks, [], []),
%%     {Collected, Refs} = classify_and_collect(Functions, [], []),
%%     _Model = #model{forms=Collected, comments=Comments},
%%     %%erlide_noparse_server:create(ScannerName, Model, ""),
%%     ok.

classify_and_collect([], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([eof], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([C | Rest], Acc, RefsAcc, Exports, Imports) ->
    {R, Refs, MoreExports, MoreImports} = cac(check_class(C), C, Exports, Imports),
    classify_and_collect(Rest, [R | Acc], Refs++RefsAcc, MoreExports++Exports, MoreImports++Imports).

cac(function, Tokens, Exports, Imports) ->
    ClauseList = split_clauses(Tokens),
    %?D(ClauseList),
    ClausesAndRefs = fix_clauses(ClauseList),
    %?D(length(Clauses)),
    [{#clause{pos=P, name=N, args=A, head=H, name_pos=NP}, _Refs} | _] = ClausesAndRefs,
    ?D(A),
    Arity = length(A),
    Exported = get_exported({N, Arity}, Exports),
    Function = case ClausesAndRefs of  	% only show subclauses when more than one
                   [_] ->
                       #function{pos=P, name=N, arity=Arity, args=A, head=H, 
                                 clauses=[], name_pos=NP, exported=Exported};
                   _ ->
                       Clauses = [C || {C, _} <- ClausesAndRefs],
                       #function{pos=P, name=N, arity=Arity, clauses=Clauses, 
                                 name_pos=NP, exported=Exported}
               end,
    {Function, fix_refs(ClausesAndRefs, Function, Imports), [], []};
cac(attribute, Attribute, _Exports, _Imports) ->
    ?D(Attribute),
    case Attribute of
        %% -spec, -type or -opaque
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=Kind, line=_Line, offset=_Offset, value=Name} | Args]
          when (Kind=:='spec') or ((Kind=:=atom) and (Name=:='type'))
                   or ((Kind=:=atom) and (Name=:='opaque'))->
            get_type_attribute(Kind, Name, Offset, Line, Attribute, Args);
        %% other attributes
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=atom, value=Name, line=_Line, offset=_Offset},
         _, #token{value=Args} | _] = Attribute ->
            get_other_attribute(Name, Offset, Line, Attribute, Args);
        [_, #token{kind=atom, value=Name, line=Line, offset=Offset} | _] ->
            #token{line=LastLine, offset=LastOffset, 
                   length=LastLength} = last_not_eof(Attribute),
            PosLength = LastOffset - Offset + LastLength,
            {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                       name=Name, args=[]}, [], [], []}
    end;
cac(other, [#token{value=Name, line=Line, offset=Offset, length=Length} | _], 
    _Exports, _Imports) ->
    {#other{pos={{Line, Line, Offset}, Length}, name=Name}, [], [], []};
cac(_, _D, _E, _I) ->
    {eof, [], [], []}.

get_type_attribute(Kind, Name0, Offset, Line, Attribute, Args) ->
    ?D(Name0),
    Name = case Kind of 'spec' -> Kind; _ -> Name0 end,
    #token{line=LastLine, offset=LastOffset,
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    ?D(Args),
    Extra = to_string(Args),
    ?D(Extra),
    {AttrArgs, _, _} = get_attribute_args(Kind, Args, Args),
    ?D({AttrArgs, Extra}),
    ExternalRefs = get_refs(tl(AttrArgs), Extra, ?ARI_TYPESPEC),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     [#ref{data=#type_def{type=Name}, offset=Offset, length=PosLength, function=Name, 
           arity=?ARI_TYPESPEC, clause="", sub_clause=false} | ExternalRefs], [], []}.

get_other_attribute(Name, Offset, Line, Attribute, Args) ->
    #token{line=LastLine, offset=LastOffset, 
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    Between = erlide_np_util:get_between_outer_pars(Attribute, '(', ')'),
    Extra = to_string(Between),
    {AttrArgs, Exports, Imports} = get_attribute_args(Name, Between, Args),
    ?D({AttrArgs, Between}),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     make_attribute_ref(Name, AttrArgs, Extra, Offset, PosLength)++
         make_attribute_arg_refs(Name, AttrArgs, Between),
     Exports, Imports}.

get_exported(F_A, Exports) ->
    lists:member(F_A, Exports).

get_attribute_args(import, Between, _Args) ->
    From = get_first_of_kind(atom, Between),
    Tokens = get_between(Between, '[', ']'),
    Import = {From, fun_arity_from_tokens(Tokens)},
    {Import, [], [Import]};
get_attribute_args(export, Between, _Args) ->
    Tokens = get_between(Between, '[', ']'),
    Exports = fun_arity_from_tokens(Tokens),
    {Exports, Exports, []};
get_attribute_args(record, Between, _Args) ->
    [RecordToken | _] = Between,
    RecordName = RecordToken#token.value,
    Tokens = erlide_np_util:get_between_outer_pars(Between, '{', '}'),
    R = {RecordName, field_list_from_tokens(Tokens)},
    {R, [], []};
get_attribute_args(_, _Between, Args) ->
    {Args, [], []}.

check_class([#token{kind = AtomOrMacro}, #token{kind = '('} | _])
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    function;
check_class([#token{kind = '-'}, #token{kind = atom} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'spec'} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'type'} | _]) ->
    attribute;
check_class(_X) ->
    %?D(lists:sublist(_X, 5)),
    other.

get_first_of_kind(Kind, Tokens) ->
    case erlide_np_util:skip_to(Tokens, Kind) of
        [#token{kind=Kind, value=Value} | _] ->
            Value;
        _ ->
            undefined
    end.
        
fun_arity_from_tokens([#token{kind=atom, value=Fun}, #token{kind='/'}, 
                       #token{kind=integer, value=Arity} | Rest]) ->
    [{Fun, Arity} | fun_arity_from_tokens(Rest)];
fun_arity_from_tokens([_ | Rest]) ->
    fun_arity_from_tokens(Rest);
fun_arity_from_tokens(_) ->
    [].

field_list_from_tokens([#token{kind=atom, value=Field, line=Line, 
                               offset=Offset, length=Length} | Rest]) ->
    {Extra, NewRest} = get_upto2(Rest, ','),
    [{Field, {{Line, Line, Offset}, Length}, to_string(Extra)}
         | field_list_from_tokens(NewRest)];
field_list_from_tokens([_ | Rest]) ->
    field_list_from_tokens(Rest);
field_list_from_tokens(_) ->
    [].

to_string(Tokens) ->
    S = erlide_scanner:tokens_to_string(Tokens),
    erlide_text:strip(S).
%%     unspacify(S).

get_head(T) ->
    A = get_args(T),
    case get_guards(T) of
        "" ->
            A;
        G ->
            A ++ " when " ++ G
    end.

get_args(T) ->
    case erlide_np_util:get_between_outer_pars(T, '(', ')') of
        [] ->
            "";
        P ->
            "("++to_string(P)++")"
    end.

get_guards([]) ->
    "";
get_guards(T) ->
    to_string(get_between(T, 'when', '->')). 

last_not_eof(L) -> 
    case lists:reverse(L) of
        [eof, Last | _] ->
            Last;
        [Last | _] ->
            Last;
        _ ->
            error
    end.    


get_between(L, A, B) ->
    R = get_upto(L, B),
    lists:reverse(get_upto(lists:reverse(R), A)).

get_upto(L, Delim) ->
        get_upto(L, Delim, []).

get_upto([], _Delim, _Acc) ->
    [];
get_upto([#token{kind=Delim} | _], Delim, Acc) ->
        lists:reverse(Acc);
get_upto([T | Rest], Delim, Acc) ->
    get_upto(Rest, Delim, [T | Acc]).

get_upto2(L, Delim) ->
    get_upto2(L, Delim, []).

get_upto2([], _Delim, Acc) ->
    {lists:reverse(Acc), []};
get_upto2([#token{kind=Delim} | Rest], Delim, Acc) ->
    {lists:reverse(Acc), Rest};
get_upto2([T | Rest], Delim, Acc) ->
    get_upto2(Rest, Delim, [T | Acc]).

check_clause([#token{kind = ';'} | Rest], false) ->
    check_class(Rest) == function;
check_clause(_, _) ->
    false.

split_clauses(F) ->
    split_clauses(F, false, [], []).

split_clauses([], _HaveWhen, Acc, []) ->
    erlide_util:reverse2(Acc);
split_clauses([], HaveWhen, Acc, ClAcc) ->
    split_clauses([], HaveWhen, [ClAcc | Acc], []);
split_clauses([#token{kind=Kind}=T | Rest], _HaveWhen, Acc, ClAcc) 
  when Kind=:='end'; Kind=:=dot; Kind=:='->'; Kind=:='when' ->
    split_clauses(Rest, Kind=:='when', Acc, [T | ClAcc]);
split_clauses([T | Rest] = Tokens, HaveWhen, Acc, ClAcc) ->
    case check_clause(Tokens, HaveWhen) of
        false ->
            split_clauses(Rest, HaveWhen, Acc, [T | ClAcc]);
        true ->
            split_clauses(Rest, HaveWhen, [[T | ClAcc] | Acc], [])
    end.

%% fix_clause([#token{kind=atom, value=Name, line=Line, offset=Offset, length=Length} | Rest] = Code) ->
fix_clauses(Clauses) ->
    fix_clauses(Clauses, []).

fix_clauses([], Acc) ->
    lists:reverse(Acc);
fix_clauses([C | Rest], Acc) ->
    {Clause, Refs} = fix_clause(C),
    fix_clauses(Rest, [{Clause, Refs} | Acc]).

fix_clause([#token{kind=AtomOrMacro, value=Name, line=Line, offset=Offset, length=Length} | Rest]) 
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    #token{line=LastLine, offset=LastOffset, length=LastLength} = last_not_eof(Rest),
    PosLength = LastOffset - Offset + LastLength+1,
    ExternalRefs = get_refs_in_code(Rest),
    {#clause{pos={{Line, LastLine, Offset}, PosLength}, name_pos={{Line, Offset}, Length},
             name=Name, args=get_function_args(Rest), head=get_head(Rest)},
     ExternalRefs}.

get_function_args(Tokens) ->
    P = erlide_np_util:get_between_outer_pars(Tokens, '(', ')'),
    L = erlide_text:split_comma_list(P),
    [to_string(A) || A <- L].

fix_refs(ClausesAndRefs, Function, Imports) ->
    fix_refs(ClausesAndRefs, Function, Imports, []).

fix_refs([], #function{name=Name, arity=Arity, name_pos={{_, Offset}, Length}}, _Imports, Acc) ->
    Ref = #ref{data=#function_def{function=Name, arity=Arity}, offset=Offset, 
               length=Length, function=Name, arity=Arity, clause="", sub_clause=false},
    [Ref | Acc];
fix_refs([{Clause, Refs} | Rest], Function, Imports, Acc0) ->
    Acc1 = fix_refs(Refs, Clause, Function, Imports, Acc0),
    fix_refs(Rest, Function, Imports, Acc1).

fix_refs([], _Clause, _Function, _Imports, Acc) ->
    Acc;
fix_refs([{Offset, Length, RefData} | Rest], #clause{head=Head}=Clause, 
         #function{name=Name, arity=Arity, clauses=Clauses}=Function, 
         Imports, Acc) ->
    SubClause = case Clauses of
                    [] -> false;
                    [_] -> false;
                    _ -> true
                end,
    NewRefData = fix_imported_ref(RefData, Imports),
    Ref = #ref{data=NewRefData, offset=Offset, length=Length, function=Name, 
               arity=Arity, clause=Head, sub_clause=SubClause},
    fix_refs(Rest, Clause, Function, Imports, [Ref | Acc]).

fix_imported_ref(#local_call{function=F, arity=A}=RefData, [{Module, Imports} | Rest]) ->
    case lists:member({F, A}, Imports) of
        true -> #external_call{module=Module, function=F, arity=A};
        false -> fix_imported_ref(RefData, Rest)
    end;
fix_imported_ref(RefData, _Imports) ->
    RefData.

%% find references in code
%% VERY simple, M:F(_ ...), F(_ ...), #R, ?M, and that's it
%% returns {Offset, Length, Ref}, which is made into proper refs
%% by fix_refs
get_refs_in_code(L) ->
    get_refs_in_code(L, []).

get_refs_in_code([], Acc) ->
    lists:reverse(Acc);
get_refs_in_code([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
                  #token{kind=atom, value=F, offset=Offset2, length=Length2},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length2+Offset2-Offset, 
         #external_call{module = M, function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=atom, value=F, offset=Offset, length=Length},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length, #local_call{function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest], 
                 Acc) ->
    R = {Offset, Length, #macro_ref{macro=M}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind='#', offset=Offset}, 
                  #token{kind=atom, value=Record, offset=Offset2, length=Length2} | Rest],
                 Acc) ->
    R = {Offset, Length2+Offset2-Offset, #record_ref{record=Record}},
    {NewRest, Fields, RightSides} = erlide_np_records:check_fields(Rest, Record),
    NewAcc = get_refs_in_code(RightSides, Acc),
    get_refs_in_code(NewRest, Fields ++ [R | NewAcc]);
get_refs_in_code([#token{kind=var, value=V, offset=Offset, length=Length} | Rest],
                 Acc) ->
    R = {Offset, Length, make_var_def_ref(Acc, V)},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([_ | Rest], Acc) ->
    get_refs_in_code(Rest, Acc).

make_var_def_ref([], Var) ->
	#var_def{variable=Var};
make_var_def_ref([{_, _, #var_def{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([{_, _, #var_ref{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([_ | Rest], Var) ->
	make_var_def_ref(Rest, Var).

%% find type references in spec, type and opaque
%% also VERY simple, type(), ?M, #R, only
%% returns proper refs
get_refs(Tokens, Name, Arity) ->
	get_refs(Tokens, Name, Arity, []).

get_refs([], _, _, Acc) ->
    lists:reverse(Acc);
get_refs([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #macro_ref{macro=M}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind='#', offset=Offset},
          #token{kind=atom, value=R, offset=Offset2, length=Length2} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset, #record_ref{record=R}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
		  #token{kind=atom, value=T, offset=Offset2, length=Length2},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset,
                   #type_ref{module=M, type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=T, offset=Offset, length=Length},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #type_ref{module='_', type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=variable, value=V, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #var_ref{variable=V}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([_ | Rest], Name, Arity, Acc) ->
    get_refs(Rest, Name, Arity, Acc).

get_record_field_defs([], _) ->
    [];
get_record_field_defs([{FieldName, {{_, _, Offset}, Length}, _Extra} | Rest],
                      RecordName) ->
    [make_ref(Offset, Length, #record_field_def{record=RecordName, field=FieldName}, 
              FieldName, ?ARI_RECORD_FIELD_DEF)
         | get_record_field_defs(Rest, RecordName)].

make_ref(Offset, Length, Data, Name, Arity) ->
    #ref{data=Data, offset=Offset, length=Length, function=Name, arity=Arity, 
         clause="", sub_clause=false}.

make_attribute_arg_refs(define, Name, [#token{}, #token{kind='('} | Rest]) ->
    get_refs(skip_to_rparen(Rest), Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(define, Name, [#token{}, #token{kind=','} | Rest]) ->
    get_refs(Rest, Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(record, {Name, Fields}, [#token{}, #token{kind=','} | Rest]) ->
    get_record_field_defs(Fields, Name) ++ get_refs(Rest, Name, ?ARI_RECORD_DEF);
make_attribute_arg_refs(_, _, _) ->
    [].

skip_to_rparen([]) ->
    [];
skip_to_rparen([#token{kind=')'} | Rest]) ->
    Rest;
skip_to_rparen([_ | Rest]) ->
    skip_to_rparen(Rest).

make_attribute_ref(Name, Between, Extra, Offset, Length) ->
    ?D({Name, Between, Offset, Length}),
    case make_attribute_ref(Name, Between, Extra) of
        [] ->
            [];
        {Arity, AName, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=AName, 
                  arity=Arity, clause="", sub_clause=false}];
        {Arity, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=Name, 
                  arity=Arity, clause="", sub_clause=false}]
    end.

make_attribute_ref(module, _, Extra) ->
    {?ARI_ATTRIBUTE, #module_def{module=Extra}};
make_attribute_ref(record, Between, _E) ->
    ?D({Between, _E}),
    Name = case Between of
               {N, _} -> N;
               N -> N
           end,
    R= #record_def{record=Name},
    ?D(R),
    {?ARI_RECORD_DEF, Name, R};
make_attribute_ref(define, [Name | _], _) when is_list(Name) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(define, Name, _) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(include, _, Extra) ->
    {?ARI_INCLUDE, #include_ref{filename=unquote(Extra)}};
make_attribute_ref(_, _, _) ->
    [].

unquote(L) ->
    lists:reverse(unquote_first(lists:reverse(unquote_first(L)))).

unquote_first([$" | Rest]) ->
    Rest;
unquote_first(L) ->
    L.

remove_rest([], _) -> [];
remove_rest(Rest, Rest) -> [];
remove_rest([Hd | Tl], Rest) -> [Hd | remove_rest(Tl, Rest)].
%% Description: Split source files into functions and clauses and collects references
%% Author: jakob
%% Created: Mar 23, 2006

-module(erlide_noparse).

%%
%% Exported Functions
%%

%% called from Java
-export([initial_parse/5, reparse/1, remove_cache_files/2]).

%% called from Erlang
-export([read_module_refs/3]).

%%
%% Include files
%%

%% -define(DEBUG, 1).
%% -define(IO_FORMAT_DEBUG, 1).

-define(CACHE_VERSION, 25).
-define(SERVER, erlide_noparse).

-include("erlide.hrl").
-include("erlide_noparse.hrl").
-include("erlide_scanner.hrl").
-include("erlide_search_server.hrl").

%% special arity flags
-define(ARI_TYPESPEC, -2).
-define(ARI_ATTRIBUTE, -3).
-define(ARI_RECORD_DEF, -4).
-define(ARI_MACRO_DEF, -5).
-define(ARI_INCLUDE, -6).
-define(ARI_RECORD_FIELD_DEF, -7).

-define(X(X), erlide_log:log({X, erlang:now()})).


%%
%% API Functions
%%

initial_parse(ScannerName, ModuleFileName, StateDir, UseCache,
              UpdateSearchServer) ->
    try
        %%         ?D({StateDir, ModuleFileName}),
        BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
        RefsFileName = BaseName ++ ".refs",
        RenewFun = fun(_F) ->
                           do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer)
                   end,
        CacheFileName = BaseName ++ ".noparse",
        ?D(CacheFileName),
        {Cached, Res} = erlide_util:check_and_renew_cached(
                          ModuleFileName, CacheFileName, ?CACHE_VERSION, 
                          RenewFun, UseCache),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        ?D(updated),
        {ok, Res, Cached}
    catch
        error:Reason ->
            {error, Reason}
    end.

reparse(ScannerName) ->
    try
        Res = do_parse(ScannerName, "", "", true),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        {ok, Res, unused}
    catch
        error:Reason ->
            {error, Reason}
    end.

read_module_refs(ScannerName, ModulePath, StateDir) ->
    ?D(ScannerName),
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    RefsFileName = BaseName ++ ".refs",
    ?D(RefsFileName),
    case file:read_file(RefsFileName) of
        {ok, Binary} ->
            ?D(byte_size(Binary)),
            binary_to_term(Binary);
        _ ->
            ?D(ModulePath),
            {ok, InitialTextBin} = file:read_file(ModulePath),
            ?D(byte_size(InitialTextBin)),
            InitialText = binary_to_list(InitialTextBin),
            _D = erlide_scanner_server:initialScan(
                   ScannerName, ModulePath, InitialText, StateDir, true),
            ?D(_D),
            initial_parse(ScannerName, ModulePath, StateDir, true, false),
            ?D(RefsFileName),
            {ok, Binary} = file:read_file(RefsFileName),
            ?D(byte_size(Binary)),
            binary_to_term(Binary)
    end.

remove_cache_files(ScannerName, StateDir) ->
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    ScannerCacheFileName = BaseName ++ ".scan",
    file:delete(ScannerCacheFileName),
    RefsFileName = BaseName ++ ".refs",
    file:delete(RefsFileName),
    CacheFileName = BaseName ++ ".noparse",
    file:delete(CacheFileName).

%%
%% Internal functions
%%

do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer) ->
    Toks = erlide_scanner_server:getTokens(ScannerName),
    do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer).

do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer) ->
    ?D({do_parse, ScannerName, length(Toks)}),
    ?X(111),
    {UncommentToks, Comments} = erlide_np_util:extract_comments(Toks),
    ?X(222),
    ?D({length(UncommentToks), length(Comments)}),
    ?D({UncommentToks}),
    Functions = erlide_np_util:split_after_dots(UncommentToks),
    ?X(333),
    ?D(length(Functions)),
    AutoImports = erlide_util:add_auto_imported([]),
    ?X(444),
%%     ?D(AutoImports),
    {Collected, Refs} = classify_and_collect(Functions, [], [], [], AutoImports),
    ?X(555),
    ?D({'>>',length(Collected)}),
    CommentedCollected = erlide_np_util:get_function_comments(Collected, Comments),
    ?X(666),
    ?D(CommentedCollected),
    Model = #model{forms=CommentedCollected, comments=Comments},
    %%erlide_noparse_server:create(ScannerName, Model),
%%     ?D({"Model", length(Model#model.forms), erts_debug:flat_size(Model)}),
    FixedModel = fixup_model(Model),
    ?X(777),
%%     ?D(erts_debug:flat_size(FixedModel)),
    %% 	?D([erts_debug:flat_size(F) || F <- element(2, FixedModel)]),
    ?D(StateDir),
    case StateDir of
        "" -> ok;
        _ ->
            ?D({RefsFileName, length(Refs)}),
            file:write_file(RefsFileName, term_to_binary(Refs, [compressed]))
    end,
    update_search_server(UpdateSearchServer, ScannerName, Refs),
%%     ?D(FixedModel),
    ?X(888),
    FixedModel.

update_search_server(true, ScannerName, Refs) ->
    erlide_search_server:add_module_refs(ScannerName, Refs);
update_search_server(_, _, _) ->
    ok.

fixup_model(#model{forms=Forms, comments=Comments}) ->
    FixedComments = fixup_tokens(Comments),
    FixedForms = fixup_forms(Forms),
    #model{forms=FixedForms, comments=FixedComments}.

fixup_forms(Forms) ->
    [fixup_form(Form) || Form <- Forms].

fixup_tokens(Tokens) ->
    [fixup_token(Token) || Token <- Tokens].

fixup_token(#token{value=Value} = Token) when is_list(Value) ->
    Token#token{value=to_binary(Value)};
fixup_token(#token{text=Text} = Token) when is_list(Text) ->
    Token#token{value=to_binary(Text)};
fixup_token(Token) ->
    Token.

binary_args(Args) when is_list(Args) ->
    [iolist_to_binary(A) || A <- Args];
binary_args(_) ->
    [].

fixup_form(#function{comment=Comment, clauses=Clauses, args=Args} = Function) ->
    Function#function{comment= to_binary(Comment), clauses=fixup_forms(Clauses), args=binary_args(Args)};
fixup_form(#clause{head=Head, args=Args} = Clause) ->
    Clause#clause{head=to_binary(Head), args=binary_args(Args)};
fixup_form(Other) ->
    Other.

to_binary(Comment) when is_list(Comment) ->
    try 
        iolist_to_binary(Comment) 
    catch 
        _:_ -> 
            unicode:characters_to_binary(Comment) 
    end;
to_binary(Other) ->
    Other.

%% parse_test(ScannerName, File) ->
%%     erlide_scanner_server:scan_uncached(ScannerName, File, ""),
%%     Toks = erlide_scanner_server:getTokens(ScannerName),
%%     {UncommentToks, Comments} = extract_comments(Toks),
%%     Functions = split_after_dots(UncommentToks, [], []),
%%     {Collected, Refs} = classify_and_collect(Functions, [], []),
%%     _Model = #model{forms=Collected, comments=Comments},
%%     %%erlide_noparse_server:create(ScannerName, Model, ""),
%%     ok.

classify_and_collect([], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([eof], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([C | Rest], Acc, RefsAcc, Exports, Imports) ->
    {R, Refs, MoreExports, MoreImports} = cac(check_class(C), C, Exports, Imports),
    classify_and_collect(Rest, [R | Acc], Refs++RefsAcc, MoreExports++Exports, MoreImports++Imports).

cac(function, Tokens, Exports, Imports) ->
    ClauseList = split_clauses(Tokens),
    %?D(ClauseList),
    ClausesAndRefs = fix_clauses(ClauseList),
    %?D(length(Clauses)),
    [{#clause{pos=P, name=N, args=A, head=H, name_pos=NP}, _Refs} | _] = ClausesAndRefs,
    ?D(A),
    Arity = length(A),
    Exported = get_exported({N, Arity}, Exports),
    Function = case ClausesAndRefs of  	% only show subclauses when more than one
                   [_] ->
                       #function{pos=P, name=N, arity=Arity, args=A, head=H, 
                                 clauses=[], name_pos=NP, exported=Exported};
                   _ ->
                       Clauses = [C || {C, _} <- ClausesAndRefs],
                       #function{pos=P, name=N, arity=Arity, clauses=Clauses, 
                                 name_pos=NP, exported=Exported}
               end,
    {Function, fix_refs(ClausesAndRefs, Function, Imports), [], []};
cac(attribute, Attribute, _Exports, _Imports) ->
    ?D(Attribute),
    case Attribute of
        %% -spec, -type or -opaque
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=Kind, line=_Line, offset=_Offset, value=Name} | Args]
          when (Kind=:='spec') or ((Kind=:=atom) and (Name=:='type'))
                   or ((Kind=:=atom) and (Name=:='opaque'))->
            get_type_attribute(Kind, Name, Offset, Line, Attribute, Args);
        %% other attributes
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=atom, value=Name, line=_Line, offset=_Offset},
         _, #token{value=Args} | _] = Attribute ->
            get_other_attribute(Name, Offset, Line, Attribute, Args);
        [_, #token{kind=atom, value=Name, line=Line, offset=Offset} | _] ->
            #token{line=LastLine, offset=LastOffset, 
                   length=LastLength} = last_not_eof(Attribute),
            PosLength = LastOffset - Offset + LastLength,
            {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                       name=Name, args=[]}, [], [], []}
    end;
cac(other, [#token{value=Name, line=Line, offset=Offset, length=Length} | _], 
    _Exports, _Imports) ->
    {#other{pos={{Line, Line, Offset}, Length}, name=Name}, [], [], []};
cac(_, _D, _E, _I) ->
    {eof, [], [], []}.

get_type_attribute(Kind, Name0, Offset, Line, Attribute, Args) ->
    ?D(Name0),
    Name = case Kind of 'spec' -> Kind; _ -> Name0 end,
    #token{line=LastLine, offset=LastOffset,
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    ?D(Args),
    Extra = to_string(Args),
    ?D(Extra),
    {AttrArgs, _, _} = get_attribute_args(Kind, Args, Args),
    ?D({AttrArgs, Extra}),
    ExternalRefs = get_refs(tl(AttrArgs), Extra, ?ARI_TYPESPEC),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     [#ref{data=#type_def{type=Name}, offset=Offset, length=PosLength, function=Name, 
           arity=?ARI_TYPESPEC, clause="", sub_clause=false} | ExternalRefs], [], []}.

get_other_attribute(Name, Offset, Line, Attribute, Args) ->
    #token{line=LastLine, offset=LastOffset, 
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    Between = erlide_np_util:get_between_outer_pars(Attribute, '(', ')'),
    Extra = to_string(Between),
    {AttrArgs, Exports, Imports} = get_attribute_args(Name, Between, Args),
    ?D({AttrArgs, Between}),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     make_attribute_ref(Name, AttrArgs, Extra, Offset, PosLength)++
         make_attribute_arg_refs(Name, AttrArgs, Between),
     Exports, Imports}.

get_exported(F_A, Exports) ->
    lists:member(F_A, Exports).

get_attribute_args(import, Between, _Args) ->
    From = get_first_of_kind(atom, Between),
    Tokens = get_between(Between, '[', ']'),
    Import = {From, fun_arity_from_tokens(Tokens)},
    {Import, [], [Import]};
get_attribute_args(export, Between, _Args) ->
    Tokens = get_between(Between, '[', ']'),
    Exports = fun_arity_from_tokens(Tokens),
    {Exports, Exports, []};
get_attribute_args(record, Between, _Args) ->
    [RecordToken | _] = Between,
    RecordName = RecordToken#token.value,
    Tokens = erlide_np_util:get_between_outer_pars(Between, '{', '}'),
    R = {RecordName, field_list_from_tokens(Tokens)},
    {R, [], []};
get_attribute_args(_, _Between, Args) ->
    {Args, [], []}.

check_class([#token{kind = AtomOrMacro}, #token{kind = '('} | _])
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    function;
check_class([#token{kind = '-'}, #token{kind = atom} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'spec'} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'type'} | _]) ->
    attribute;
check_class(_X) ->
    %?D(lists:sublist(_X, 5)),
    other.

get_first_of_kind(Kind, Tokens) ->
    case erlide_np_util:skip_to(Tokens, Kind) of
        [#token{kind=Kind, value=Value} | _] ->
            Value;
        _ ->
            undefined
    end.
        
fun_arity_from_tokens([#token{kind=atom, value=Fun}, #token{kind='/'}, 
                       #token{kind=integer, value=Arity} | Rest]) ->
    [{Fun, Arity} | fun_arity_from_tokens(Rest)];
fun_arity_from_tokens([_ | Rest]) ->
    fun_arity_from_tokens(Rest);
fun_arity_from_tokens(_) ->
    [].

field_list_from_tokens([#token{kind=atom, value=Field, line=Line, 
                               offset=Offset, length=Length} | Rest]) ->
    {Extra, NewRest} = get_upto2(Rest, ','),
    [{Field, {{Line, Line, Offset}, Length}, to_string(Extra)}
         | field_list_from_tokens(NewRest)];
field_list_from_tokens([_ | Rest]) ->
    field_list_from_tokens(Rest);
field_list_from_tokens(_) ->
    [].

to_string(Tokens) ->
    S = erlide_scanner:tokens_to_string(Tokens),
    erlide_text:strip(S).
%%     unspacify(S).

get_head(T) ->
    A = get_args(T),
    case get_guards(T) of
        "" ->
            A;
        G ->
            A ++ " when " ++ G
    end.

get_args(T) ->
    case erlide_np_util:get_between_outer_pars(T, '(', ')') of
        [] ->
            "";
        P ->
            "("++to_string(P)++")"
    end.

get_guards([]) ->
    "";
get_guards(T) ->
    to_string(get_between(T, 'when', '->')). 

last_not_eof(L) -> 
    case lists:reverse(L) of
        [eof, Last | _] ->
            Last;
        [Last | _] ->
            Last;
        _ ->
            error
    end.    


get_between(L, A, B) ->
    R = get_upto(L, B),
    lists:reverse(get_upto(lists:reverse(R), A)).

get_upto(L, Delim) ->
        get_upto(L, Delim, []).

get_upto([], _Delim, _Acc) ->
    [];
get_upto([#token{kind=Delim} | _], Delim, Acc) ->
        lists:reverse(Acc);
get_upto([T | Rest], Delim, Acc) ->
    get_upto(Rest, Delim, [T | Acc]).

get_upto2(L, Delim) ->
    get_upto2(L, Delim, []).

get_upto2([], _Delim, Acc) ->
    {lists:reverse(Acc), []};
get_upto2([#token{kind=Delim} | Rest], Delim, Acc) ->
    {lists:reverse(Acc), Rest};
get_upto2([T | Rest], Delim, Acc) ->
    get_upto2(Rest, Delim, [T | Acc]).

check_clause([#token{kind = ';'} | Rest], false) ->
    check_class(Rest) == function;
check_clause(_, _) ->
    false.

split_clauses(F) ->
    split_clauses(F, false, [], []).

split_clauses([], _HaveWhen, Acc, []) ->
    erlide_util:reverse2(Acc);
split_clauses([], HaveWhen, Acc, ClAcc) ->
    split_clauses([], HaveWhen, [ClAcc | Acc], []);
split_clauses([#token{kind=Kind}=T | Rest], _HaveWhen, Acc, ClAcc) 
  when Kind=:='end'; Kind=:=dot; Kind=:='->'; Kind=:='when' ->
    split_clauses(Rest, Kind=:='when', Acc, [T | ClAcc]);
split_clauses([T | Rest] = Tokens, HaveWhen, Acc, ClAcc) ->
    case check_clause(Tokens, HaveWhen) of
        false ->
            split_clauses(Rest, HaveWhen, Acc, [T | ClAcc]);
        true ->
            split_clauses(Rest, HaveWhen, [[T | ClAcc] | Acc], [])
    end.

%% fix_clause([#token{kind=atom, value=Name, line=Line, offset=Offset, length=Length} | Rest] = Code) ->
fix_clauses(Clauses) ->
    fix_clauses(Clauses, []).

fix_clauses([], Acc) ->
    lists:reverse(Acc);
fix_clauses([C | Rest], Acc) ->
    {Clause, Refs} = fix_clause(C),
    fix_clauses(Rest, [{Clause, Refs} | Acc]).

fix_clause([#token{kind=AtomOrMacro, value=Name, line=Line, offset=Offset, length=Length} | Rest]) 
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    #token{line=LastLine, offset=LastOffset, length=LastLength} = last_not_eof(Rest),
    PosLength = LastOffset - Offset + LastLength+1,
    ExternalRefs = get_refs_in_code(Rest),
    {#clause{pos={{Line, LastLine, Offset}, PosLength}, name_pos={{Line, Offset}, Length},
             name=Name, args=get_function_args(Rest), head=get_head(Rest)},
     ExternalRefs}.

get_function_args(Tokens) ->
    P = erlide_np_util:get_between_outer_pars(Tokens, '(', ')'),
    L = erlide_text:split_comma_list(P),
    [to_string(A) || A <- L].

fix_refs(ClausesAndRefs, Function, Imports) ->
    fix_refs(ClausesAndRefs, Function, Imports, []).

fix_refs([], #function{name=Name, arity=Arity, name_pos={{_, Offset}, Length}}, _Imports, Acc) ->
    Ref = #ref{data=#function_def{function=Name, arity=Arity}, offset=Offset, 
               length=Length, function=Name, arity=Arity, clause="", sub_clause=false},
    [Ref | Acc];
fix_refs([{Clause, Refs} | Rest], Function, Imports, Acc0) ->
    Acc1 = fix_refs(Refs, Clause, Function, Imports, Acc0),
    fix_refs(Rest, Function, Imports, Acc1).

fix_refs([], _Clause, _Function, _Imports, Acc) ->
    Acc;
fix_refs([{Offset, Length, RefData} | Rest], #clause{head=Head}=Clause, 
         #function{name=Name, arity=Arity, clauses=Clauses}=Function, 
         Imports, Acc) ->
    SubClause = case Clauses of
                    [] -> false;
                    [_] -> false;
                    _ -> true
                end,
    NewRefData = fix_imported_ref(RefData, Imports),
    Ref = #ref{data=NewRefData, offset=Offset, length=Length, function=Name, 
               arity=Arity, clause=Head, sub_clause=SubClause},
    fix_refs(Rest, Clause, Function, Imports, [Ref | Acc]).

fix_imported_ref(#local_call{function=F, arity=A}=RefData, [{Module, Imports} | Rest]) ->
    case lists:member({F, A}, Imports) of
        true -> #external_call{module=Module, function=F, arity=A};
        false -> fix_imported_ref(RefData, Rest)
    end;
fix_imported_ref(RefData, _Imports) ->
    RefData.

%% find references in code
%% VERY simple, M:F(_ ...), F(_ ...), #R, ?M, and that's it
%% returns {Offset, Length, Ref}, which is made into proper refs
%% by fix_refs
get_refs_in_code(L) ->
    get_refs_in_code(L, []).

get_refs_in_code([], Acc) ->
    lists:reverse(Acc);
get_refs_in_code([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
                  #token{kind=atom, value=F, offset=Offset2, length=Length2},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length2+Offset2-Offset, 
         #external_call{module = M, function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=atom, value=F, offset=Offset, length=Length},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length, #local_call{function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest], 
                 Acc) ->
    R = {Offset, Length, #macro_ref{macro=M}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind='#', offset=Offset}, 
                  #token{kind=atom, value=Record, offset=Offset2, length=Length2} | Rest],
                 Acc) ->
    R = {Offset, Length2+Offset2-Offset, #record_ref{record=Record}},
    {NewRest, Fields, RightSides} = erlide_np_records:check_fields(Rest, Record),
    NewAcc = get_refs_in_code(RightSides, Acc),
    get_refs_in_code(NewRest, Fields ++ [R | NewAcc]);
get_refs_in_code([#token{kind=var, value=V, offset=Offset, length=Length} | Rest],
                 Acc) ->
    R = {Offset, Length, make_var_def_ref(Acc, V)},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([_ | Rest], Acc) ->
    get_refs_in_code(Rest, Acc).

make_var_def_ref([], Var) ->
	#var_def{variable=Var};
make_var_def_ref([{_, _, #var_def{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([{_, _, #var_ref{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([_ | Rest], Var) ->
	make_var_def_ref(Rest, Var).

%% find type references in spec, type and opaque
%% also VERY simple, type(), ?M, #R, only
%% returns proper refs
get_refs(Tokens, Name, Arity) ->
	get_refs(Tokens, Name, Arity, []).

get_refs([], _, _, Acc) ->
    lists:reverse(Acc);
get_refs([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #macro_ref{macro=M}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind='#', offset=Offset},
          #token{kind=atom, value=R, offset=Offset2, length=Length2} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset, #record_ref{record=R}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
		  #token{kind=atom, value=T, offset=Offset2, length=Length2},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset,
                   #type_ref{module=M, type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=T, offset=Offset, length=Length},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #type_ref{module='_', type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=variable, value=V, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #var_ref{variable=V}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([_ | Rest], Name, Arity, Acc) ->
    get_refs(Rest, Name, Arity, Acc).

get_record_field_defs([], _) ->
    [];
get_record_field_defs([{FieldName, {{_, _, Offset}, Length}, _Extra} | Rest],
                      RecordName) ->
    [make_ref(Offset, Length, #record_field_def{record=RecordName, field=FieldName}, 
              FieldName, ?ARI_RECORD_FIELD_DEF)
         | get_record_field_defs(Rest, RecordName)].

make_ref(Offset, Length, Data, Name, Arity) ->
    #ref{data=Data, offset=Offset, length=Length, function=Name, arity=Arity, 
         clause="", sub_clause=false}.

make_attribute_arg_refs(define, Name, [#token{}, #token{kind='('} | Rest]) ->
    get_refs(skip_to_rparen(Rest), Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(define, Name, [#token{}, #token{kind=','} | Rest]) ->
    get_refs(Rest, Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(record, {Name, Fields}, [#token{}, #token{kind=','} | Rest]) ->
    get_record_field_defs(Fields, Name) ++ get_refs(Rest, Name, ?ARI_RECORD_DEF);
make_attribute_arg_refs(_, _, _) ->
    [].

skip_to_rparen([]) ->
    [];
skip_to_rparen([#token{kind=')'} | Rest]) ->
    Rest;
skip_to_rparen([_ | Rest]) ->
    skip_to_rparen(Rest).

make_attribute_ref(Name, Between, Extra, Offset, Length) ->
    ?D({Name, Between, Offset, Length}),
    case make_attribute_ref(Name, Between, Extra) of
        [] ->
            [];
        {Arity, AName, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=AName, 
                  arity=Arity, clause="", sub_clause=false}];
        {Arity, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=Name, 
                  arity=Arity, clause="", sub_clause=false}]
    end.

make_attribute_ref(module, _, Extra) ->
    {?ARI_ATTRIBUTE, #module_def{module=Extra}};
make_attribute_ref(record, Between, _E) ->
    ?D({Between, _E}),
    Name = case Between of
               {N, _} -> N;
               N -> N
           end,
    R= #record_def{record=Name},
    ?D(R),
    {?ARI_RECORD_DEF, Name, R};
make_attribute_ref(define, [Name | _], _) when is_list(Name) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(define, Name, _) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(include, _, Extra) ->
    {?ARI_INCLUDE, #include_ref{filename=unquote(Extra)}};
make_attribute_ref(_, _, _) ->
    [].

unquote(L) ->
    lists:reverse(unquote_first(lists:reverse(unquote_first(L)))).

unquote_first([$" | Rest]) ->
    Rest;
unquote_first(L) ->
    L.

remove_rest([], _) -> [];
remove_rest(Rest, Rest) -> [];
remove_rest([Hd | Tl], Rest) -> [Hd | remove_rest(Tl, Rest)].
%% Description: Split source files into functions and clauses and collects references
%% Author: jakob
%% Created: Mar 23, 2006

-module(erlide_noparse).

%%
%% Exported Functions
%%

%% called from Java
-export([initial_parse/5, reparse/1, remove_cache_files/2]).

%% called from Erlang
-export([read_module_refs/3]).

%%
%% Include files
%%

%% -define(DEBUG, 1).
%% -define(IO_FORMAT_DEBUG, 1).

-define(CACHE_VERSION, 25).
-define(SERVER, erlide_noparse).

-include("erlide.hrl").
-include("erlide_noparse.hrl").
-include("erlide_scanner.hrl").
-include("erlide_search_server.hrl").

%% special arity flags
-define(ARI_TYPESPEC, -2).
-define(ARI_ATTRIBUTE, -3).
-define(ARI_RECORD_DEF, -4).
-define(ARI_MACRO_DEF, -5).
-define(ARI_INCLUDE, -6).
-define(ARI_RECORD_FIELD_DEF, -7).

-define(X(X), erlide_log:log({X, erlang:now()})).


%%
%% API Functions
%%

initial_parse(ScannerName, ModuleFileName, StateDir, UseCache,
              UpdateSearchServer) ->
    try
        %%         ?D({StateDir, ModuleFileName}),
        BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
        RefsFileName = BaseName ++ ".refs",
        RenewFun = fun(_F) ->
                           do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer)
                   end,
        CacheFileName = BaseName ++ ".noparse",
        ?D(CacheFileName),
        {Cached, Res} = erlide_util:check_and_renew_cached(
                          ModuleFileName, CacheFileName, ?CACHE_VERSION, 
                          RenewFun, UseCache),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        ?D(updated),
        {ok, Res, Cached}
    catch
        error:Reason ->
            {error, Reason}
    end.

reparse(ScannerName) ->
    try
        Res = do_parse(ScannerName, "", "", true),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        {ok, Res, unused}
    catch
        error:Reason ->
            {error, Reason}
    end.

read_module_refs(ScannerName, ModulePath, StateDir) ->
    ?D(ScannerName),
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    RefsFileName = BaseName ++ ".refs",
    ?D(RefsFileName),
    case file:read_file(RefsFileName) of
        {ok, Binary} ->
            ?D(byte_size(Binary)),
            binary_to_term(Binary);
        _ ->
            ?D(ModulePath),
            {ok, InitialTextBin} = file:read_file(ModulePath),
            ?D(byte_size(InitialTextBin)),
            InitialText = binary_to_list(InitialTextBin),
            _D = erlide_scanner_server:initialScan(
                   ScannerName, ModulePath, InitialText, StateDir, true),
            ?D(_D),
            initial_parse(ScannerName, ModulePath, StateDir, true, false),
            ?D(RefsFileName),
            {ok, Binary} = file:read_file(RefsFileName),
            ?D(byte_size(Binary)),
            binary_to_term(Binary)
    end.

remove_cache_files(ScannerName, StateDir) ->
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    ScannerCacheFileName = BaseName ++ ".scan",
    file:delete(ScannerCacheFileName),
    RefsFileName = BaseName ++ ".refs",
    file:delete(RefsFileName),
    CacheFileName = BaseName ++ ".noparse",
    file:delete(CacheFileName).

%%
%% Internal functions
%%

do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer) ->
    Toks = erlide_scanner_server:getTokens(ScannerName),
    do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer).

do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer) ->
    ?D({do_parse, ScannerName, length(Toks)}),
    ?X(111),
    {UncommentToks, Comments} = erlide_np_util:extract_comments(Toks),
    ?X(222),
    ?D({length(UncommentToks), length(Comments)}),
    ?D({UncommentToks}),
    Functions = erlide_np_util:split_after_dots(UncommentToks),
    ?X(333),
    ?D(length(Functions)),
    AutoImports = erlide_util:add_auto_imported([]),
    ?X(444),
%%     ?D(AutoImports),
    {Collected, Refs} = classify_and_collect(Functions, [], [], [], AutoImports),
    ?X(555),
    ?D({'>>',length(Collected)}),
    CommentedCollected = erlide_np_util:get_function_comments(Collected, Comments),
    ?X(666),
    ?D(CommentedCollected),
    Model = #model{forms=CommentedCollected, comments=Comments},
    %%erlide_noparse_server:create(ScannerName, Model),
%%     ?D({"Model", length(Model#model.forms), erts_debug:flat_size(Model)}),
    FixedModel = fixup_model(Model),
    ?X(777),
%%     ?D(erts_debug:flat_size(FixedModel)),
    %% 	?D([erts_debug:flat_size(F) || F <- element(2, FixedModel)]),
    ?D(StateDir),
    case StateDir of
        "" -> ok;
        _ ->
            ?D({RefsFileName, length(Refs)}),
            file:write_file(RefsFileName, term_to_binary(Refs, [compressed]))
    end,
    update_search_server(UpdateSearchServer, ScannerName, Refs),
%%     ?D(FixedModel),
    ?X(888),
    FixedModel.

update_search_server(true, ScannerName, Refs) ->
    erlide_search_server:add_module_refs(ScannerName, Refs);
update_search_server(_, _, _) ->
    ok.

fixup_model(#model{forms=Forms, comments=Comments}) ->
    FixedComments = fixup_tokens(Comments),
    FixedForms = fixup_forms(Forms),
    #model{forms=FixedForms, comments=FixedComments}.

fixup_forms(Forms) ->
    [fixup_form(Form) || Form <- Forms].

fixup_tokens(Tokens) ->
    [fixup_token(Token) || Token <- Tokens].

fixup_token(#token{value=Value} = Token) when is_list(Value) ->
    Token#token{value=to_binary(Value)};
fixup_token(#token{text=Text} = Token) when is_list(Text) ->
    Token#token{value=to_binary(Text)};
fixup_token(Token) ->
    Token.

binary_args(Args) when is_list(Args) ->
    [iolist_to_binary(A) || A <- Args];
binary_args(_) ->
    [].

fixup_form(#function{comment=Comment, clauses=Clauses, args=Args} = Function) ->
    Function#function{comment= to_binary(Comment), clauses=fixup_forms(Clauses), args=binary_args(Args)};
fixup_form(#clause{head=Head, args=Args} = Clause) ->
    Clause#clause{head=to_binary(Head), args=binary_args(Args)};
fixup_form(Other) ->
    Other.

to_binary(Comment) when is_list(Comment) ->
    try 
        iolist_to_binary(Comment) 
    catch 
        _:_ -> 
            unicode:characters_to_binary(Comment) 
    end;
to_binary(Other) ->
    Other.

%% parse_test(ScannerName, File) ->
%%     erlide_scanner_server:scan_uncached(ScannerName, File, ""),
%%     Toks = erlide_scanner_server:getTokens(ScannerName),
%%     {UncommentToks, Comments} = extract_comments(Toks),
%%     Functions = split_after_dots(UncommentToks, [], []),
%%     {Collected, Refs} = classify_and_collect(Functions, [], []),
%%     _Model = #model{forms=Collected, comments=Comments},
%%     %%erlide_noparse_server:create(ScannerName, Model, ""),
%%     ok.

classify_and_collect([], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([eof], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([C | Rest], Acc, RefsAcc, Exports, Imports) ->
    {R, Refs, MoreExports, MoreImports} = cac(check_class(C), C, Exports, Imports),
    classify_and_collect(Rest, [R | Acc], Refs++RefsAcc, MoreExports++Exports, MoreImports++Imports).

cac(function, Tokens, Exports, Imports) ->
    ClauseList = split_clauses(Tokens),
    %?D(ClauseList),
    ClausesAndRefs = fix_clauses(ClauseList),
    %?D(length(Clauses)),
    [{#clause{pos=P, name=N, args=A, head=H, name_pos=NP}, _Refs} | _] = ClausesAndRefs,
    ?D(A),
    Arity = length(A),
    Exported = get_exported({N, Arity}, Exports),
    Function = case ClausesAndRefs of  	% only show subclauses when more than one
                   [_] ->
                       #function{pos=P, name=N, arity=Arity, args=A, head=H, 
                                 clauses=[], name_pos=NP, exported=Exported};
                   _ ->
                       Clauses = [C || {C, _} <- ClausesAndRefs],
                       #function{pos=P, name=N, arity=Arity, clauses=Clauses, 
                                 name_pos=NP, exported=Exported}
               end,
    {Function, fix_refs(ClausesAndRefs, Function, Imports), [], []};
cac(attribute, Attribute, _Exports, _Imports) ->
    ?D(Attribute),
    case Attribute of
        %% -spec, -type or -opaque
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=Kind, line=_Line, offset=_Offset, value=Name} | Args]
          when (Kind=:='spec') or ((Kind=:=atom) and (Name=:='type'))
                   or ((Kind=:=atom) and (Name=:='opaque'))->
            get_type_attribute(Kind, Name, Offset, Line, Attribute, Args);
        %% other attributes
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=atom, value=Name, line=_Line, offset=_Offset},
         _, #token{value=Args} | _] = Attribute ->
            get_other_attribute(Name, Offset, Line, Attribute, Args);
        [_, #token{kind=atom, value=Name, line=Line, offset=Offset} | _] ->
            #token{line=LastLine, offset=LastOffset, 
                   length=LastLength} = last_not_eof(Attribute),
            PosLength = LastOffset - Offset + LastLength,
            {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                       name=Name, args=[]}, [], [], []}
    end;
cac(other, [#token{value=Name, line=Line, offset=Offset, length=Length} | _], 
    _Exports, _Imports) ->
    {#other{pos={{Line, Line, Offset}, Length}, name=Name}, [], [], []};
cac(_, _D, _E, _I) ->
    {eof, [], [], []}.

get_type_attribute(Kind, Name0, Offset, Line, Attribute, Args) ->
    ?D(Name0),
    Name = case Kind of 'spec' -> Kind; _ -> Name0 end,
    #token{line=LastLine, offset=LastOffset,
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    ?D(Args),
    Extra = to_string(Args),
    ?D(Extra),
    {AttrArgs, _, _} = get_attribute_args(Kind, Args, Args),
    ?D({AttrArgs, Extra}),
    ExternalRefs = get_refs(tl(AttrArgs), Extra, ?ARI_TYPESPEC),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     [#ref{data=#type_def{type=Name}, offset=Offset, length=PosLength, function=Name, 
           arity=?ARI_TYPESPEC, clause="", sub_clause=false} | ExternalRefs], [], []}.

get_other_attribute(Name, Offset, Line, Attribute, Args) ->
    #token{line=LastLine, offset=LastOffset, 
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    Between = erlide_np_util:get_between_outer_pars(Attribute, '(', ')'),
    Extra = to_string(Between),
    {AttrArgs, Exports, Imports} = get_attribute_args(Name, Between, Args),
    ?D({AttrArgs, Between}),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     make_attribute_ref(Name, AttrArgs, Extra, Offset, PosLength)++
         make_attribute_arg_refs(Name, AttrArgs, Between),
     Exports, Imports}.

get_exported(F_A, Exports) ->
    lists:member(F_A, Exports).

get_attribute_args(import, Between, _Args) ->
    From = get_first_of_kind(atom, Between),
    Tokens = get_between(Between, '[', ']'),
    Import = {From, fun_arity_from_tokens(Tokens)},
    {Import, [], [Import]};
get_attribute_args(export, Between, _Args) ->
    Tokens = get_between(Between, '[', ']'),
    Exports = fun_arity_from_tokens(Tokens),
    {Exports, Exports, []};
get_attribute_args(record, Between, _Args) ->
    [RecordToken | _] = Between,
    RecordName = RecordToken#token.value,
    Tokens = erlide_np_util:get_between_outer_pars(Between, '{', '}'),
    R = {RecordName, field_list_from_tokens(Tokens)},
    {R, [], []};
get_attribute_args(_, _Between, Args) ->
    {Args, [], []}.

check_class([#token{kind = AtomOrMacro}, #token{kind = '('} | _])
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    function;
check_class([#token{kind = '-'}, #token{kind = atom} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'spec'} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'type'} | _]) ->
    attribute;
check_class(_X) ->
    %?D(lists:sublist(_X, 5)),
    other.

get_first_of_kind(Kind, Tokens) ->
    case erlide_np_util:skip_to(Tokens, Kind) of
        [#token{kind=Kind, value=Value} | _] ->
            Value;
        _ ->
            undefined
    end.
        
fun_arity_from_tokens([#token{kind=atom, value=Fun}, #token{kind='/'}, 
                       #token{kind=integer, value=Arity} | Rest]) ->
    [{Fun, Arity} | fun_arity_from_tokens(Rest)];
fun_arity_from_tokens([_ | Rest]) ->
    fun_arity_from_tokens(Rest);
fun_arity_from_tokens(_) ->
    [].

field_list_from_tokens([#token{kind=atom, value=Field, line=Line, 
                               offset=Offset, length=Length} | Rest]) ->
    {Extra, NewRest} = get_upto2(Rest, ','),
    [{Field, {{Line, Line, Offset}, Length}, to_string(Extra)}
         | field_list_from_tokens(NewRest)];
field_list_from_tokens([_ | Rest]) ->
    field_list_from_tokens(Rest);
field_list_from_tokens(_) ->
    [].

to_string(Tokens) ->
    S = erlide_scanner:tokens_to_string(Tokens),
    erlide_text:strip(S).
%%     unspacify(S).

get_head(T) ->
    A = get_args(T),
    case get_guards(T) of
        "" ->
            A;
        G ->
            A ++ " when " ++ G
    end.

get_args(T) ->
    case erlide_np_util:get_between_outer_pars(T, '(', ')') of
        [] ->
            "";
        P ->
            "("++to_string(P)++")"
    end.

get_guards([]) ->
    "";
get_guards(T) ->
    to_string(get_between(T, 'when', '->')). 

last_not_eof(L) -> 
    case lists:reverse(L) of
        [eof, Last | _] ->
            Last;
        [Last | _] ->
            Last;
        _ ->
            error
    end.    


get_between(L, A, B) ->
    R = get_upto(L, B),
    lists:reverse(get_upto(lists:reverse(R), A)).

get_upto(L, Delim) ->
        get_upto(L, Delim, []).

get_upto([], _Delim, _Acc) ->
    [];
get_upto([#token{kind=Delim} | _], Delim, Acc) ->
        lists:reverse(Acc);
get_upto([T | Rest], Delim, Acc) ->
    get_upto(Rest, Delim, [T | Acc]).

get_upto2(L, Delim) ->
    get_upto2(L, Delim, []).

get_upto2([], _Delim, Acc) ->
    {lists:reverse(Acc), []};
get_upto2([#token{kind=Delim} | Rest], Delim, Acc) ->
    {lists:reverse(Acc), Rest};
get_upto2([T | Rest], Delim, Acc) ->
    get_upto2(Rest, Delim, [T | Acc]).

check_clause([#token{kind = ';'} | Rest], false) ->
    check_class(Rest) == function;
check_clause(_, _) ->
    false.

split_clauses(F) ->
    split_clauses(F, false, [], []).

split_clauses([], _HaveWhen, Acc, []) ->
    erlide_util:reverse2(Acc);
split_clauses([], HaveWhen, Acc, ClAcc) ->
    split_clauses([], HaveWhen, [ClAcc | Acc], []);
split_clauses([#token{kind=Kind}=T | Rest], _HaveWhen, Acc, ClAcc) 
  when Kind=:='end'; Kind=:=dot; Kind=:='->'; Kind=:='when' ->
    split_clauses(Rest, Kind=:='when', Acc, [T | ClAcc]);
split_clauses([T | Rest] = Tokens, HaveWhen, Acc, ClAcc) ->
    case check_clause(Tokens, HaveWhen) of
        false ->
            split_clauses(Rest, HaveWhen, Acc, [T | ClAcc]);
        true ->
            split_clauses(Rest, HaveWhen, [[T | ClAcc] | Acc], [])
    end.

%% fix_clause([#token{kind=atom, value=Name, line=Line, offset=Offset, length=Length} | Rest] = Code) ->
fix_clauses(Clauses) ->
    fix_clauses(Clauses, []).

fix_clauses([], Acc) ->
    lists:reverse(Acc);
fix_clauses([C | Rest], Acc) ->
    {Clause, Refs} = fix_clause(C),
    fix_clauses(Rest, [{Clause, Refs} | Acc]).

fix_clause([#token{kind=AtomOrMacro, value=Name, line=Line, offset=Offset, length=Length} | Rest]) 
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    #token{line=LastLine, offset=LastOffset, length=LastLength} = last_not_eof(Rest),
    PosLength = LastOffset - Offset + LastLength+1,
    ExternalRefs = get_refs_in_code(Rest),
    {#clause{pos={{Line, LastLine, Offset}, PosLength}, name_pos={{Line, Offset}, Length},
             name=Name, args=get_function_args(Rest), head=get_head(Rest)},
     ExternalRefs}.

get_function_args(Tokens) ->
    P = erlide_np_util:get_between_outer_pars(Tokens, '(', ')'),
    L = erlide_text:split_comma_list(P),
    [to_string(A) || A <- L].

fix_refs(ClausesAndRefs, Function, Imports) ->
    fix_refs(ClausesAndRefs, Function, Imports, []).

fix_refs([], #function{name=Name, arity=Arity, name_pos={{_, Offset}, Length}}, _Imports, Acc) ->
    Ref = #ref{data=#function_def{function=Name, arity=Arity}, offset=Offset, 
               length=Length, function=Name, arity=Arity, clause="", sub_clause=false},
    [Ref | Acc];
fix_refs([{Clause, Refs} | Rest], Function, Imports, Acc0) ->
    Acc1 = fix_refs(Refs, Clause, Function, Imports, Acc0),
    fix_refs(Rest, Function, Imports, Acc1).

fix_refs([], _Clause, _Function, _Imports, Acc) ->
    Acc;
fix_refs([{Offset, Length, RefData} | Rest], #clause{head=Head}=Clause, 
         #function{name=Name, arity=Arity, clauses=Clauses}=Function, 
         Imports, Acc) ->
    SubClause = case Clauses of
                    [] -> false;
                    [_] -> false;
                    _ -> true
                end,
    NewRefData = fix_imported_ref(RefData, Imports),
    Ref = #ref{data=NewRefData, offset=Offset, length=Length, function=Name, 
               arity=Arity, clause=Head, sub_clause=SubClause},
    fix_refs(Rest, Clause, Function, Imports, [Ref | Acc]).

fix_imported_ref(#local_call{function=F, arity=A}=RefData, [{Module, Imports} | Rest]) ->
    case lists:member({F, A}, Imports) of
        true -> #external_call{module=Module, function=F, arity=A};
        false -> fix_imported_ref(RefData, Rest)
    end;
fix_imported_ref(RefData, _Imports) ->
    RefData.

%% find references in code
%% VERY simple, M:F(_ ...), F(_ ...), #R, ?M, and that's it
%% returns {Offset, Length, Ref}, which is made into proper refs
%% by fix_refs
get_refs_in_code(L) ->
    get_refs_in_code(L, []).

get_refs_in_code([], Acc) ->
    lists:reverse(Acc);
get_refs_in_code([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
                  #token{kind=atom, value=F, offset=Offset2, length=Length2},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length2+Offset2-Offset, 
         #external_call{module = M, function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=atom, value=F, offset=Offset, length=Length},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length, #local_call{function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest], 
                 Acc) ->
    R = {Offset, Length, #macro_ref{macro=M}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind='#', offset=Offset}, 
                  #token{kind=atom, value=Record, offset=Offset2, length=Length2} | Rest],
                 Acc) ->
    R = {Offset, Length2+Offset2-Offset, #record_ref{record=Record}},
    {NewRest, Fields, RightSides} = erlide_np_records:check_fields(Rest, Record),
    NewAcc = get_refs_in_code(RightSides, Acc),
    get_refs_in_code(NewRest, Fields ++ [R | NewAcc]);
get_refs_in_code([#token{kind=var, value=V, offset=Offset, length=Length} | Rest],
                 Acc) ->
    R = {Offset, Length, make_var_def_ref(Acc, V)},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([_ | Rest], Acc) ->
    get_refs_in_code(Rest, Acc).

make_var_def_ref([], Var) ->
	#var_def{variable=Var};
make_var_def_ref([{_, _, #var_def{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([{_, _, #var_ref{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([_ | Rest], Var) ->
	make_var_def_ref(Rest, Var).

%% find type references in spec, type and opaque
%% also VERY simple, type(), ?M, #R, only
%% returns proper refs
get_refs(Tokens, Name, Arity) ->
	get_refs(Tokens, Name, Arity, []).

get_refs([], _, _, Acc) ->
    lists:reverse(Acc);
get_refs([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #macro_ref{macro=M}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind='#', offset=Offset},
          #token{kind=atom, value=R, offset=Offset2, length=Length2} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset, #record_ref{record=R}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
		  #token{kind=atom, value=T, offset=Offset2, length=Length2},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset,
                   #type_ref{module=M, type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=T, offset=Offset, length=Length},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #type_ref{module='_', type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=variable, value=V, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #var_ref{variable=V}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([_ | Rest], Name, Arity, Acc) ->
    get_refs(Rest, Name, Arity, Acc).

get_record_field_defs([], _) ->
    [];
get_record_field_defs([{FieldName, {{_, _, Offset}, Length}, _Extra} | Rest],
                      RecordName) ->
    [make_ref(Offset, Length, #record_field_def{record=RecordName, field=FieldName}, 
              FieldName, ?ARI_RECORD_FIELD_DEF)
         | get_record_field_defs(Rest, RecordName)].

make_ref(Offset, Length, Data, Name, Arity) ->
    #ref{data=Data, offset=Offset, length=Length, function=Name, arity=Arity, 
         clause="", sub_clause=false}.

make_attribute_arg_refs(define, Name, [#token{}, #token{kind='('} | Rest]) ->
    get_refs(skip_to_rparen(Rest), Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(define, Name, [#token{}, #token{kind=','} | Rest]) ->
    get_refs(Rest, Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(record, {Name, Fields}, [#token{}, #token{kind=','} | Rest]) ->
    get_record_field_defs(Fields, Name) ++ get_refs(Rest, Name, ?ARI_RECORD_DEF);
make_attribute_arg_refs(_, _, _) ->
    [].

skip_to_rparen([]) ->
    [];
skip_to_rparen([#token{kind=')'} | Rest]) ->
    Rest;
skip_to_rparen([_ | Rest]) ->
    skip_to_rparen(Rest).

make_attribute_ref(Name, Between, Extra, Offset, Length) ->
    ?D({Name, Between, Offset, Length}),
    case make_attribute_ref(Name, Between, Extra) of
        [] ->
            [];
        {Arity, AName, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=AName, 
                  arity=Arity, clause="", sub_clause=false}];
        {Arity, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=Name, 
                  arity=Arity, clause="", sub_clause=false}]
    end.

make_attribute_ref(module, _, Extra) ->
    {?ARI_ATTRIBUTE, #module_def{module=Extra}};
make_attribute_ref(record, Between, _E) ->
    ?D({Between, _E}),
    Name = case Between of
               {N, _} -> N;
               N -> N
           end,
    R= #record_def{record=Name},
    ?D(R),
    {?ARI_RECORD_DEF, Name, R};
make_attribute_ref(define, [Name | _], _) when is_list(Name) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(define, Name, _) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(include, _, Extra) ->
    {?ARI_INCLUDE, #include_ref{filename=unquote(Extra)}};
make_attribute_ref(_, _, _) ->
    [].

unquote(L) ->
    lists:reverse(unquote_first(lists:reverse(unquote_first(L)))).

unquote_first([$" | Rest]) ->
    Rest;
unquote_first(L) ->
    L.

remove_rest([], _) -> [];
remove_rest(Rest, Rest) -> [];
remove_rest([Hd | Tl], Rest) -> [Hd | remove_rest(Tl, Rest)].
%% Description: Split source files into functions and clauses and collects references
%% Author: jakob
%% Created: Mar 23, 2006

-module(erlide_noparse).

%%
%% Exported Functions
%%

%% called from Java
-export([initial_parse/5, reparse/1, remove_cache_files/2]).

%% called from Erlang
-export([read_module_refs/3]).

%%
%% Include files
%%

%% -define(DEBUG, 1).
%% -define(IO_FORMAT_DEBUG, 1).

-define(CACHE_VERSION, 25).
-define(SERVER, erlide_noparse).

-include("erlide.hrl").
-include("erlide_noparse.hrl").
-include("erlide_scanner.hrl").
-include("erlide_search_server.hrl").

%% special arity flags
-define(ARI_TYPESPEC, -2).
-define(ARI_ATTRIBUTE, -3).
-define(ARI_RECORD_DEF, -4).
-define(ARI_MACRO_DEF, -5).
-define(ARI_INCLUDE, -6).
-define(ARI_RECORD_FIELD_DEF, -7).

-define(X(X), erlide_log:log({X, erlang:now()})).


%%
%% API Functions
%%

initial_parse(ScannerName, ModuleFileName, StateDir, UseCache,
              UpdateSearchServer) ->
    try
        %%         ?D({StateDir, ModuleFileName}),
        BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
        RefsFileName = BaseName ++ ".refs",
        RenewFun = fun(_F) ->
                           do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer)
                   end,
        CacheFileName = BaseName ++ ".noparse",
        ?D(CacheFileName),
        {Cached, Res} = erlide_util:check_and_renew_cached(
                          ModuleFileName, CacheFileName, ?CACHE_VERSION, 
                          RenewFun, UseCache),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        ?D(updated),
        {ok, Res, Cached}
    catch
        error:Reason ->
            {error, Reason}
    end.

reparse(ScannerName) ->
    try
        Res = do_parse(ScannerName, "", "", true),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        {ok, Res, unused}
    catch
        error:Reason ->
            {error, Reason}
    end.

read_module_refs(ScannerName, ModulePath, StateDir) ->
    ?D(ScannerName),
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    RefsFileName = BaseName ++ ".refs",
    ?D(RefsFileName),
    case file:read_file(RefsFileName) of
        {ok, Binary} ->
            ?D(byte_size(Binary)),
            binary_to_term(Binary);
        _ ->
            ?D(ModulePath),
            {ok, InitialTextBin} = file:read_file(ModulePath),
            ?D(byte_size(InitialTextBin)),
            InitialText = binary_to_list(InitialTextBin),
            _D = erlide_scanner_server:initialScan(
                   ScannerName, ModulePath, InitialText, StateDir, true),
            ?D(_D),
            initial_parse(ScannerName, ModulePath, StateDir, true, false),
            ?D(RefsFileName),
            {ok, Binary} = file:read_file(RefsFileName),
            ?D(byte_size(Binary)),
            binary_to_term(Binary)
    end.

remove_cache_files(ScannerName, StateDir) ->
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    ScannerCacheFileName = BaseName ++ ".scan",
    file:delete(ScannerCacheFileName),
    RefsFileName = BaseName ++ ".refs",
    file:delete(RefsFileName),
    CacheFileName = BaseName ++ ".noparse",
    file:delete(CacheFileName).

%%
%% Internal functions
%%

do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer) ->
    Toks = erlide_scanner_server:getTokens(ScannerName),
    do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer).

do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer) ->
    ?D({do_parse, ScannerName, length(Toks)}),
    ?X(111),
    {UncommentToks, Comments} = erlide_np_util:extract_comments(Toks),
    ?X(222),
    ?D({length(UncommentToks), length(Comments)}),
    ?D({UncommentToks}),
    Functions = erlide_np_util:split_after_dots(UncommentToks),
    ?X(333),
    ?D(length(Functions)),
    AutoImports = erlide_util:add_auto_imported([]),
    ?X(444),
%%     ?D(AutoImports),
    {Collected, Refs} = classify_and_collect(Functions, [], [], [], AutoImports),
    ?X(555),
    ?D({'>>',length(Collected)}),
    CommentedCollected = erlide_np_util:get_function_comments(Collected, Comments),
    ?X(666),
    ?D(CommentedCollected),
    Model = #model{forms=CommentedCollected, comments=Comments},
    %%erlide_noparse_server:create(ScannerName, Model),
%%     ?D({"Model", length(Model#model.forms), erts_debug:flat_size(Model)}),
    FixedModel = fixup_model(Model),
    ?X(777),
%%     ?D(erts_debug:flat_size(FixedModel)),
    %% 	?D([erts_debug:flat_size(F) || F <- element(2, FixedModel)]),
    ?D(StateDir),
    case StateDir of
        "" -> ok;
        _ ->
            ?D({RefsFileName, length(Refs)}),
            file:write_file(RefsFileName, term_to_binary(Refs, [compressed]))
    end,
    update_search_server(UpdateSearchServer, ScannerName, Refs),
%%     ?D(FixedModel),
    ?X(888),
    FixedModel.

update_search_server(true, ScannerName, Refs) ->
    erlide_search_server:add_module_refs(ScannerName, Refs);
update_search_server(_, _, _) ->
    ok.

fixup_model(#model{forms=Forms, comments=Comments}) ->
    FixedComments = fixup_tokens(Comments),
    FixedForms = fixup_forms(Forms),
    #model{forms=FixedForms, comments=FixedComments}.

fixup_forms(Forms) ->
    [fixup_form(Form) || Form <- Forms].

fixup_tokens(Tokens) ->
    [fixup_token(Token) || Token <- Tokens].

fixup_token(#token{value=Value} = Token) when is_list(Value) ->
    Token#token{value=to_binary(Value)};
fixup_token(#token{text=Text} = Token) when is_list(Text) ->
    Token#token{value=to_binary(Text)};
fixup_token(Token) ->
    Token.

binary_args(Args) when is_list(Args) ->
    [iolist_to_binary(A) || A <- Args];
binary_args(_) ->
    [].

fixup_form(#function{comment=Comment, clauses=Clauses, args=Args} = Function) ->
    Function#function{comment= to_binary(Comment), clauses=fixup_forms(Clauses), args=binary_args(Args)};
fixup_form(#clause{head=Head, args=Args} = Clause) ->
    Clause#clause{head=to_binary(Head), args=binary_args(Args)};
fixup_form(Other) ->
    Other.

to_binary(Comment) when is_list(Comment) ->
    try 
        iolist_to_binary(Comment) 
    catch 
        _:_ -> 
            unicode:characters_to_binary(Comment) 
    end;
to_binary(Other) ->
    Other.

%% parse_test(ScannerName, File) ->
%%     erlide_scanner_server:scan_uncached(ScannerName, File, ""),
%%     Toks = erlide_scanner_server:getTokens(ScannerName),
%%     {UncommentToks, Comments} = extract_comments(Toks),
%%     Functions = split_after_dots(UncommentToks, [], []),
%%     {Collected, Refs} = classify_and_collect(Functions, [], []),
%%     _Model = #model{forms=Collected, comments=Comments},
%%     %%erlide_noparse_server:create(ScannerName, Model, ""),
%%     ok.

classify_and_collect([], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([eof], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([C | Rest], Acc, RefsAcc, Exports, Imports) ->
    {R, Refs, MoreExports, MoreImports} = cac(check_class(C), C, Exports, Imports),
    classify_and_collect(Rest, [R | Acc], Refs++RefsAcc, MoreExports++Exports, MoreImports++Imports).

cac(function, Tokens, Exports, Imports) ->
    ClauseList = split_clauses(Tokens),
    %?D(ClauseList),
    ClausesAndRefs = fix_clauses(ClauseList),
    %?D(length(Clauses)),
    [{#clause{pos=P, name=N, args=A, head=H, name_pos=NP}, _Refs} | _] = ClausesAndRefs,
    ?D(A),
    Arity = length(A),
    Exported = get_exported({N, Arity}, Exports),
    Function = case ClausesAndRefs of  	% only show subclauses when more than one
                   [_] ->
                       #function{pos=P, name=N, arity=Arity, args=A, head=H, 
                                 clauses=[], name_pos=NP, exported=Exported};
                   _ ->
                       Clauses = [C || {C, _} <- ClausesAndRefs],
                       #function{pos=P, name=N, arity=Arity, clauses=Clauses, 
                                 name_pos=NP, exported=Exported}
               end,
    {Function, fix_refs(ClausesAndRefs, Function, Imports), [], []};
cac(attribute, Attribute, _Exports, _Imports) ->
    ?D(Attribute),
    case Attribute of
        %% -spec, -type or -opaque
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=Kind, line=_Line, offset=_Offset, value=Name} | Args]
          when (Kind=:='spec') or ((Kind=:=atom) and (Name=:='type'))
                   or ((Kind=:=atom) and (Name=:='opaque'))->
            get_type_attribute(Kind, Name, Offset, Line, Attribute, Args);
        %% other attributes
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=atom, value=Name, line=_Line, offset=_Offset},
         _, #token{value=Args} | _] = Attribute ->
            get_other_attribute(Name, Offset, Line, Attribute, Args);
        [_, #token{kind=atom, value=Name, line=Line, offset=Offset} | _] ->
            #token{line=LastLine, offset=LastOffset, 
                   length=LastLength} = last_not_eof(Attribute),
            PosLength = LastOffset - Offset + LastLength,
            {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                       name=Name, args=[]}, [], [], []}
    end;
cac(other, [#token{value=Name, line=Line, offset=Offset, length=Length} | _], 
    _Exports, _Imports) ->
    {#other{pos={{Line, Line, Offset}, Length}, name=Name}, [], [], []};
cac(_, _D, _E, _I) ->
    {eof, [], [], []}.

get_type_attribute(Kind, Name0, Offset, Line, Attribute, Args) ->
    ?D(Name0),
    Name = case Kind of 'spec' -> Kind; _ -> Name0 end,
    #token{line=LastLine, offset=LastOffset,
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    ?D(Args),
    Extra = to_string(Args),
    ?D(Extra),
    {AttrArgs, _, _} = get_attribute_args(Kind, Args, Args),
    ?D({AttrArgs, Extra}),
    ExternalRefs = get_refs(tl(AttrArgs), Extra, ?ARI_TYPESPEC),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     [#ref{data=#type_def{type=Name}, offset=Offset, length=PosLength, function=Name, 
           arity=?ARI_TYPESPEC, clause="", sub_clause=false} | ExternalRefs], [], []}.

get_other_attribute(Name, Offset, Line, Attribute, Args) ->
    #token{line=LastLine, offset=LastOffset, 
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    Between = erlide_np_util:get_between_outer_pars(Attribute, '(', ')'),
    Extra = to_string(Between),
    {AttrArgs, Exports, Imports} = get_attribute_args(Name, Between, Args),
    ?D({AttrArgs, Between}),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     make_attribute_ref(Name, AttrArgs, Extra, Offset, PosLength)++
         make_attribute_arg_refs(Name, AttrArgs, Between),
     Exports, Imports}.

get_exported(F_A, Exports) ->
    lists:member(F_A, Exports).

get_attribute_args(import, Between, _Args) ->
    From = get_first_of_kind(atom, Between),
    Tokens = get_between(Between, '[', ']'),
    Import = {From, fun_arity_from_tokens(Tokens)},
    {Import, [], [Import]};
get_attribute_args(export, Between, _Args) ->
    Tokens = get_between(Between, '[', ']'),
    Exports = fun_arity_from_tokens(Tokens),
    {Exports, Exports, []};
get_attribute_args(record, Between, _Args) ->
    [RecordToken | _] = Between,
    RecordName = RecordToken#token.value,
    Tokens = erlide_np_util:get_between_outer_pars(Between, '{', '}'),
    R = {RecordName, field_list_from_tokens(Tokens)},
    {R, [], []};
get_attribute_args(_, _Between, Args) ->
    {Args, [], []}.

check_class([#token{kind = AtomOrMacro}, #token{kind = '('} | _])
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    function;
check_class([#token{kind = '-'}, #token{kind = atom} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'spec'} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'type'} | _]) ->
    attribute;
check_class(_X) ->
    %?D(lists:sublist(_X, 5)),
    other.

get_first_of_kind(Kind, Tokens) ->
    case erlide_np_util:skip_to(Tokens, Kind) of
        [#token{kind=Kind, value=Value} | _] ->
            Value;
        _ ->
            undefined
    end.
        
fun_arity_from_tokens([#token{kind=atom, value=Fun}, #token{kind='/'}, 
                       #token{kind=integer, value=Arity} | Rest]) ->
    [{Fun, Arity} | fun_arity_from_tokens(Rest)];
fun_arity_from_tokens([_ | Rest]) ->
    fun_arity_from_tokens(Rest);
fun_arity_from_tokens(_) ->
    [].

field_list_from_tokens([#token{kind=atom, value=Field, line=Line, 
                               offset=Offset, length=Length} | Rest]) ->
    {Extra, NewRest} = get_upto2(Rest, ','),
    [{Field, {{Line, Line, Offset}, Length}, to_string(Extra)}
         | field_list_from_tokens(NewRest)];
field_list_from_tokens([_ | Rest]) ->
    field_list_from_tokens(Rest);
field_list_from_tokens(_) ->
    [].

to_string(Tokens) ->
    S = erlide_scanner:tokens_to_string(Tokens),
    erlide_text:strip(S).
%%     unspacify(S).

get_head(T) ->
    A = get_args(T),
    case get_guards(T) of
        "" ->
            A;
        G ->
            A ++ " when " ++ G
    end.

get_args(T) ->
    case erlide_np_util:get_between_outer_pars(T, '(', ')') of
        [] ->
            "";
        P ->
            "("++to_string(P)++")"
    end.

get_guards([]) ->
    "";
get_guards(T) ->
    to_string(get_between(T, 'when', '->')). 

last_not_eof(L) -> 
    case lists:reverse(L) of
        [eof, Last | _] ->
            Last;
        [Last | _] ->
            Last;
        _ ->
            error
    end.    


get_between(L, A, B) ->
    R = get_upto(L, B),
    lists:reverse(get_upto(lists:reverse(R), A)).

get_upto(L, Delim) ->
        get_upto(L, Delim, []).

get_upto([], _Delim, _Acc) ->
    [];
get_upto([#token{kind=Delim} | _], Delim, Acc) ->
        lists:reverse(Acc);
get_upto([T | Rest], Delim, Acc) ->
    get_upto(Rest, Delim, [T | Acc]).

get_upto2(L, Delim) ->
    get_upto2(L, Delim, []).

get_upto2([], _Delim, Acc) ->
    {lists:reverse(Acc), []};
get_upto2([#token{kind=Delim} | Rest], Delim, Acc) ->
    {lists:reverse(Acc), Rest};
get_upto2([T | Rest], Delim, Acc) ->
    get_upto2(Rest, Delim, [T | Acc]).

check_clause([#token{kind = ';'} | Rest], false) ->
    check_class(Rest) == function;
check_clause(_, _) ->
    false.

split_clauses(F) ->
    split_clauses(F, false, [], []).

split_clauses([], _HaveWhen, Acc, []) ->
    erlide_util:reverse2(Acc);
split_clauses([], HaveWhen, Acc, ClAcc) ->
    split_clauses([], HaveWhen, [ClAcc | Acc], []);
split_clauses([#token{kind=Kind}=T | Rest], _HaveWhen, Acc, ClAcc) 
  when Kind=:='end'; Kind=:=dot; Kind=:='->'; Kind=:='when' ->
    split_clauses(Rest, Kind=:='when', Acc, [T | ClAcc]);
split_clauses([T | Rest] = Tokens, HaveWhen, Acc, ClAcc) ->
    case check_clause(Tokens, HaveWhen) of
        false ->
            split_clauses(Rest, HaveWhen, Acc, [T | ClAcc]);
        true ->
            split_clauses(Rest, HaveWhen, [[T | ClAcc] | Acc], [])
    end.

%% fix_clause([#token{kind=atom, value=Name, line=Line, offset=Offset, length=Length} | Rest] = Code) ->
fix_clauses(Clauses) ->
    fix_clauses(Clauses, []).

fix_clauses([], Acc) ->
    lists:reverse(Acc);
fix_clauses([C | Rest], Acc) ->
    {Clause, Refs} = fix_clause(C),
    fix_clauses(Rest, [{Clause, Refs} | Acc]).

fix_clause([#token{kind=AtomOrMacro, value=Name, line=Line, offset=Offset, length=Length} | Rest]) 
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    #token{line=LastLine, offset=LastOffset, length=LastLength} = last_not_eof(Rest),
    PosLength = LastOffset - Offset + LastLength+1,
    ExternalRefs = get_refs_in_code(Rest),
    {#clause{pos={{Line, LastLine, Offset}, PosLength}, name_pos={{Line, Offset}, Length},
             name=Name, args=get_function_args(Rest), head=get_head(Rest)},
     ExternalRefs}.

get_function_args(Tokens) ->
    P = erlide_np_util:get_between_outer_pars(Tokens, '(', ')'),
    L = erlide_text:split_comma_list(P),
    [to_string(A) || A <- L].

fix_refs(ClausesAndRefs, Function, Imports) ->
    fix_refs(ClausesAndRefs, Function, Imports, []).

fix_refs([], #function{name=Name, arity=Arity, name_pos={{_, Offset}, Length}}, _Imports, Acc) ->
    Ref = #ref{data=#function_def{function=Name, arity=Arity}, offset=Offset, 
               length=Length, function=Name, arity=Arity, clause="", sub_clause=false},
    [Ref | Acc];
fix_refs([{Clause, Refs} | Rest], Function, Imports, Acc0) ->
    Acc1 = fix_refs(Refs, Clause, Function, Imports, Acc0),
    fix_refs(Rest, Function, Imports, Acc1).

fix_refs([], _Clause, _Function, _Imports, Acc) ->
    Acc;
fix_refs([{Offset, Length, RefData} | Rest], #clause{head=Head}=Clause, 
         #function{name=Name, arity=Arity, clauses=Clauses}=Function, 
         Imports, Acc) ->
    SubClause = case Clauses of
                    [] -> false;
                    [_] -> false;
                    _ -> true
                end,
    NewRefData = fix_imported_ref(RefData, Imports),
    Ref = #ref{data=NewRefData, offset=Offset, length=Length, function=Name, 
               arity=Arity, clause=Head, sub_clause=SubClause},
    fix_refs(Rest, Clause, Function, Imports, [Ref | Acc]).

fix_imported_ref(#local_call{function=F, arity=A}=RefData, [{Module, Imports} | Rest]) ->
    case lists:member({F, A}, Imports) of
        true -> #external_call{module=Module, function=F, arity=A};
        false -> fix_imported_ref(RefData, Rest)
    end;
fix_imported_ref(RefData, _Imports) ->
    RefData.

%% find references in code
%% VERY simple, M:F(_ ...), F(_ ...), #R, ?M, and that's it
%% returns {Offset, Length, Ref}, which is made into proper refs
%% by fix_refs
get_refs_in_code(L) ->
    get_refs_in_code(L, []).

get_refs_in_code([], Acc) ->
    lists:reverse(Acc);
get_refs_in_code([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
                  #token{kind=atom, value=F, offset=Offset2, length=Length2},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length2+Offset2-Offset, 
         #external_call{module = M, function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=atom, value=F, offset=Offset, length=Length},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length, #local_call{function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest], 
                 Acc) ->
    R = {Offset, Length, #macro_ref{macro=M}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind='#', offset=Offset}, 
                  #token{kind=atom, value=Record, offset=Offset2, length=Length2} | Rest],
                 Acc) ->
    R = {Offset, Length2+Offset2-Offset, #record_ref{record=Record}},
    {NewRest, Fields, RightSides} = erlide_np_records:check_fields(Rest, Record),
    NewAcc = get_refs_in_code(RightSides, Acc),
    get_refs_in_code(NewRest, Fields ++ [R | NewAcc]);
get_refs_in_code([#token{kind=var, value=V, offset=Offset, length=Length} | Rest],
                 Acc) ->
    R = {Offset, Length, make_var_def_ref(Acc, V)},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([_ | Rest], Acc) ->
    get_refs_in_code(Rest, Acc).

make_var_def_ref([], Var) ->
	#var_def{variable=Var};
make_var_def_ref([{_, _, #var_def{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([{_, _, #var_ref{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([_ | Rest], Var) ->
	make_var_def_ref(Rest, Var).

%% find type references in spec, type and opaque
%% also VERY simple, type(), ?M, #R, only
%% returns proper refs
get_refs(Tokens, Name, Arity) ->
	get_refs(Tokens, Name, Arity, []).

get_refs([], _, _, Acc) ->
    lists:reverse(Acc);
get_refs([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #macro_ref{macro=M}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind='#', offset=Offset},
          #token{kind=atom, value=R, offset=Offset2, length=Length2} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset, #record_ref{record=R}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
		  #token{kind=atom, value=T, offset=Offset2, length=Length2},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset,
                   #type_ref{module=M, type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=T, offset=Offset, length=Length},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #type_ref{module='_', type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=variable, value=V, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #var_ref{variable=V}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([_ | Rest], Name, Arity, Acc) ->
    get_refs(Rest, Name, Arity, Acc).

get_record_field_defs([], _) ->
    [];
get_record_field_defs([{FieldName, {{_, _, Offset}, Length}, _Extra} | Rest],
                      RecordName) ->
    [make_ref(Offset, Length, #record_field_def{record=RecordName, field=FieldName}, 
              FieldName, ?ARI_RECORD_FIELD_DEF)
         | get_record_field_defs(Rest, RecordName)].

make_ref(Offset, Length, Data, Name, Arity) ->
    #ref{data=Data, offset=Offset, length=Length, function=Name, arity=Arity, 
         clause="", sub_clause=false}.

make_attribute_arg_refs(define, Name, [#token{}, #token{kind='('} | Rest]) ->
    get_refs(skip_to_rparen(Rest), Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(define, Name, [#token{}, #token{kind=','} | Rest]) ->
    get_refs(Rest, Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(record, {Name, Fields}, [#token{}, #token{kind=','} | Rest]) ->
    get_record_field_defs(Fields, Name) ++ get_refs(Rest, Name, ?ARI_RECORD_DEF);
make_attribute_arg_refs(_, _, _) ->
    [].

skip_to_rparen([]) ->
    [];
skip_to_rparen([#token{kind=')'} | Rest]) ->
    Rest;
skip_to_rparen([_ | Rest]) ->
    skip_to_rparen(Rest).

make_attribute_ref(Name, Between, Extra, Offset, Length) ->
    ?D({Name, Between, Offset, Length}),
    case make_attribute_ref(Name, Between, Extra) of
        [] ->
            [];
        {Arity, AName, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=AName, 
                  arity=Arity, clause="", sub_clause=false}];
        {Arity, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=Name, 
                  arity=Arity, clause="", sub_clause=false}]
    end.

make_attribute_ref(module, _, Extra) ->
    {?ARI_ATTRIBUTE, #module_def{module=Extra}};
make_attribute_ref(record, Between, _E) ->
    ?D({Between, _E}),
    Name = case Between of
               {N, _} -> N;
               N -> N
           end,
    R= #record_def{record=Name},
    ?D(R),
    {?ARI_RECORD_DEF, Name, R};
make_attribute_ref(define, [Name | _], _) when is_list(Name) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(define, Name, _) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(include, _, Extra) ->
    {?ARI_INCLUDE, #include_ref{filename=unquote(Extra)}};
make_attribute_ref(_, _, _) ->
    [].

unquote(L) ->
    lists:reverse(unquote_first(lists:reverse(unquote_first(L)))).

unquote_first([$" | Rest]) ->
    Rest;
unquote_first(L) ->
    L.

remove_rest([], _) -> [];
remove_rest(Rest, Rest) -> [];
remove_rest([Hd | Tl], Rest) -> [Hd | remove_rest(Tl, Rest)].
%% Description: Split source files into functions and clauses and collects references
%% Author: jakob
%% Created: Mar 23, 2006

-module(erlide_noparse).

%%
%% Exported Functions
%%

%% called from Java
-export([initial_parse/5, reparse/1, remove_cache_files/2]).

%% called from Erlang
-export([read_module_refs/3]).

%%
%% Include files
%%

%% -define(DEBUG, 1).
%% -define(IO_FORMAT_DEBUG, 1).

-define(CACHE_VERSION, 25).
-define(SERVER, erlide_noparse).

-include("erlide.hrl").
-include("erlide_noparse.hrl").
-include("erlide_scanner.hrl").
-include("erlide_search_server.hrl").

%% special arity flags
-define(ARI_TYPESPEC, -2).
-define(ARI_ATTRIBUTE, -3).
-define(ARI_RECORD_DEF, -4).
-define(ARI_MACRO_DEF, -5).
-define(ARI_INCLUDE, -6).
-define(ARI_RECORD_FIELD_DEF, -7).

-define(X(X), erlide_log:log({X, erlang:now()})).


%%
%% API Functions
%%

initial_parse(ScannerName, ModuleFileName, StateDir, UseCache,
              UpdateSearchServer) ->
    try
        %%         ?D({StateDir, ModuleFileName}),
        BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
        RefsFileName = BaseName ++ ".refs",
        RenewFun = fun(_F) ->
                           do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer)
                   end,
        CacheFileName = BaseName ++ ".noparse",
        ?D(CacheFileName),
        {Cached, Res} = erlide_util:check_and_renew_cached(
                          ModuleFileName, CacheFileName, ?CACHE_VERSION, 
                          RenewFun, UseCache),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        ?D(updated),
        {ok, Res, Cached}
    catch
        error:Reason ->
            {error, Reason}
    end.

reparse(ScannerName) ->
    try
        Res = do_parse(ScannerName, "", "", true),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        {ok, Res, unused}
    catch
        error:Reason ->
            {error, Reason}
    end.

read_module_refs(ScannerName, ModulePath, StateDir) ->
    ?D(ScannerName),
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    RefsFileName = BaseName ++ ".refs",
    ?D(RefsFileName),
    case file:read_file(RefsFileName) of
        {ok, Binary} ->
            ?D(byte_size(Binary)),
            binary_to_term(Binary);
        _ ->
            ?D(ModulePath),
            {ok, InitialTextBin} = file:read_file(ModulePath),
            ?D(byte_size(InitialTextBin)),
            InitialText = binary_to_list(InitialTextBin),
            _D = erlide_scanner_server:initialScan(
                   ScannerName, ModulePath, InitialText, StateDir, true),
            ?D(_D),
            initial_parse(ScannerName, ModulePath, StateDir, true, false),
            ?D(RefsFileName),
            {ok, Binary} = file:read_file(RefsFileName),
            ?D(byte_size(Binary)),
            binary_to_term(Binary)
    end.

remove_cache_files(ScannerName, StateDir) ->
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    ScannerCacheFileName = BaseName ++ ".scan",
    file:delete(ScannerCacheFileName),
    RefsFileName = BaseName ++ ".refs",
    file:delete(RefsFileName),
    CacheFileName = BaseName ++ ".noparse",
    file:delete(CacheFileName).

%%
%% Internal functions
%%

do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer) ->
    Toks = erlide_scanner_server:getTokens(ScannerName),
    do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer).

do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer) ->
    ?D({do_parse, ScannerName, length(Toks)}),
    ?X(111),
    {UncommentToks, Comments} = erlide_np_util:extract_comments(Toks),
    ?X(222),
    ?D({length(UncommentToks), length(Comments)}),
    ?D({UncommentToks}),
    Functions = erlide_np_util:split_after_dots(UncommentToks),
    ?X(333),
    ?D(length(Functions)),
    AutoImports = erlide_util:add_auto_imported([]),
    ?X(444),
%%     ?D(AutoImports),
    {Collected, Refs} = classify_and_collect(Functions, [], [], [], AutoImports),
    ?X(555),
    ?D({'>>',length(Collected)}),
    CommentedCollected = erlide_np_util:get_function_comments(Collected, Comments),
    ?X(666),
    ?D(CommentedCollected),
    Model = #model{forms=CommentedCollected, comments=Comments},
    %%erlide_noparse_server:create(ScannerName, Model),
%%     ?D({"Model", length(Model#model.forms), erts_debug:flat_size(Model)}),
    FixedModel = fixup_model(Model),
    ?X(777),
%%     ?D(erts_debug:flat_size(FixedModel)),
    %% 	?D([erts_debug:flat_size(F) || F <- element(2, FixedModel)]),
    ?D(StateDir),
    case StateDir of
        "" -> ok;
        _ ->
            ?D({RefsFileName, length(Refs)}),
            file:write_file(RefsFileName, term_to_binary(Refs, [compressed]))
    end,
    update_search_server(UpdateSearchServer, ScannerName, Refs),
%%     ?D(FixedModel),
    ?X(888),
    FixedModel.

update_search_server(true, ScannerName, Refs) ->
    erlide_search_server:add_module_refs(ScannerName, Refs);
update_search_server(_, _, _) ->
    ok.

fixup_model(#model{forms=Forms, comments=Comments}) ->
    FixedComments = fixup_tokens(Comments),
    FixedForms = fixup_forms(Forms),
    #model{forms=FixedForms, comments=FixedComments}.

fixup_forms(Forms) ->
    [fixup_form(Form) || Form <- Forms].

fixup_tokens(Tokens) ->
    [fixup_token(Token) || Token <- Tokens].

fixup_token(#token{value=Value} = Token) when is_list(Value) ->
    Token#token{value=to_binary(Value)};
fixup_token(#token{text=Text} = Token) when is_list(Text) ->
    Token#token{value=to_binary(Text)};
fixup_token(Token) ->
    Token.

binary_args(Args) when is_list(Args) ->
    [iolist_to_binary(A) || A <- Args];
binary_args(_) ->
    [].

fixup_form(#function{comment=Comment, clauses=Clauses, args=Args} = Function) ->
    Function#function{comment= to_binary(Comment), clauses=fixup_forms(Clauses), args=binary_args(Args)};
fixup_form(#clause{head=Head, args=Args} = Clause) ->
    Clause#clause{head=to_binary(Head), args=binary_args(Args)};
fixup_form(Other) ->
    Other.

to_binary(Comment) when is_list(Comment) ->
    try 
        iolist_to_binary(Comment) 
    catch 
        _:_ -> 
            unicode:characters_to_binary(Comment) 
    end;
to_binary(Other) ->
    Other.

%% parse_test(ScannerName, File) ->
%%     erlide_scanner_server:scan_uncached(ScannerName, File, ""),
%%     Toks = erlide_scanner_server:getTokens(ScannerName),
%%     {UncommentToks, Comments} = extract_comments(Toks),
%%     Functions = split_after_dots(UncommentToks, [], []),
%%     {Collected, Refs} = classify_and_collect(Functions, [], []),
%%     _Model = #model{forms=Collected, comments=Comments},
%%     %%erlide_noparse_server:create(ScannerName, Model, ""),
%%     ok.

classify_and_collect([], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([eof], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([C | Rest], Acc, RefsAcc, Exports, Imports) ->
    {R, Refs, MoreExports, MoreImports} = cac(check_class(C), C, Exports, Imports),
    classify_and_collect(Rest, [R | Acc], Refs++RefsAcc, MoreExports++Exports, MoreImports++Imports).

cac(function, Tokens, Exports, Imports) ->
    ClauseList = split_clauses(Tokens),
    %?D(ClauseList),
    ClausesAndRefs = fix_clauses(ClauseList),
    %?D(length(Clauses)),
    [{#clause{pos=P, name=N, args=A, head=H, name_pos=NP}, _Refs} | _] = ClausesAndRefs,
    ?D(A),
    Arity = length(A),
    Exported = get_exported({N, Arity}, Exports),
    Function = case ClausesAndRefs of  	% only show subclauses when more than one
                   [_] ->
                       #function{pos=P, name=N, arity=Arity, args=A, head=H, 
                                 clauses=[], name_pos=NP, exported=Exported};
                   _ ->
                       Clauses = [C || {C, _} <- ClausesAndRefs],
                       #function{pos=P, name=N, arity=Arity, clauses=Clauses, 
                                 name_pos=NP, exported=Exported}
               end,
    {Function, fix_refs(ClausesAndRefs, Function, Imports), [], []};
cac(attribute, Attribute, _Exports, _Imports) ->
    ?D(Attribute),
    case Attribute of
        %% -spec, -type or -opaque
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=Kind, line=_Line, offset=_Offset, value=Name} | Args]
          when (Kind=:='spec') or ((Kind=:=atom) and (Name=:='type'))
                   or ((Kind=:=atom) and (Name=:='opaque'))->
            get_type_attribute(Kind, Name, Offset, Line, Attribute, Args);
        %% other attributes
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=atom, value=Name, line=_Line, offset=_Offset},
         _, #token{value=Args} | _] = Attribute ->
            get_other_attribute(Name, Offset, Line, Attribute, Args);
        [_, #token{kind=atom, value=Name, line=Line, offset=Offset} | _] ->
            #token{line=LastLine, offset=LastOffset, 
                   length=LastLength} = last_not_eof(Attribute),
            PosLength = LastOffset - Offset + LastLength,
            {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                       name=Name, args=[]}, [], [], []}
    end;
cac(other, [#token{value=Name, line=Line, offset=Offset, length=Length} | _], 
    _Exports, _Imports) ->
    {#other{pos={{Line, Line, Offset}, Length}, name=Name}, [], [], []};
cac(_, _D, _E, _I) ->
    {eof, [], [], []}.

get_type_attribute(Kind, Name0, Offset, Line, Attribute, Args) ->
    ?D(Name0),
    Name = case Kind of 'spec' -> Kind; _ -> Name0 end,
    #token{line=LastLine, offset=LastOffset,
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    ?D(Args),
    Extra = to_string(Args),
    ?D(Extra),
    {AttrArgs, _, _} = get_attribute_args(Kind, Args, Args),
    ?D({AttrArgs, Extra}),
    ExternalRefs = get_refs(tl(AttrArgs), Extra, ?ARI_TYPESPEC),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     [#ref{data=#type_def{type=Name}, offset=Offset, length=PosLength, function=Name, 
           arity=?ARI_TYPESPEC, clause="", sub_clause=false} | ExternalRefs], [], []}.

get_other_attribute(Name, Offset, Line, Attribute, Args) ->
    #token{line=LastLine, offset=LastOffset, 
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    Between = erlide_np_util:get_between_outer_pars(Attribute, '(', ')'),
    Extra = to_string(Between),
    {AttrArgs, Exports, Imports} = get_attribute_args(Name, Between, Args),
    ?D({AttrArgs, Between}),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     make_attribute_ref(Name, AttrArgs, Extra, Offset, PosLength)++
         make_attribute_arg_refs(Name, AttrArgs, Between),
     Exports, Imports}.

get_exported(F_A, Exports) ->
    lists:member(F_A, Exports).

get_attribute_args(import, Between, _Args) ->
    From = get_first_of_kind(atom, Between),
    Tokens = get_between(Between, '[', ']'),
    Import = {From, fun_arity_from_tokens(Tokens)},
    {Import, [], [Import]};
get_attribute_args(export, Between, _Args) ->
    Tokens = get_between(Between, '[', ']'),
    Exports = fun_arity_from_tokens(Tokens),
    {Exports, Exports, []};
get_attribute_args(record, Between, _Args) ->
    [RecordToken | _] = Between,
    RecordName = RecordToken#token.value,
    Tokens = erlide_np_util:get_between_outer_pars(Between, '{', '}'),
    R = {RecordName, field_list_from_tokens(Tokens)},
    {R, [], []};
get_attribute_args(_, _Between, Args) ->
    {Args, [], []}.

check_class([#token{kind = AtomOrMacro}, #token{kind = '('} | _])
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    function;
check_class([#token{kind = '-'}, #token{kind = atom} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'spec'} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'type'} | _]) ->
    attribute;
check_class(_X) ->
    %?D(lists:sublist(_X, 5)),
    other.

get_first_of_kind(Kind, Tokens) ->
    case erlide_np_util:skip_to(Tokens, Kind) of
        [#token{kind=Kind, value=Value} | _] ->
            Value;
        _ ->
            undefined
    end.
        
fun_arity_from_tokens([#token{kind=atom, value=Fun}, #token{kind='/'}, 
                       #token{kind=integer, value=Arity} | Rest]) ->
    [{Fun, Arity} | fun_arity_from_tokens(Rest)];
fun_arity_from_tokens([_ | Rest]) ->
    fun_arity_from_tokens(Rest);
fun_arity_from_tokens(_) ->
    [].

field_list_from_tokens([#token{kind=atom, value=Field, line=Line, 
                               offset=Offset, length=Length} | Rest]) ->
    {Extra, NewRest} = get_upto2(Rest, ','),
    [{Field, {{Line, Line, Offset}, Length}, to_string(Extra)}
         | field_list_from_tokens(NewRest)];
field_list_from_tokens([_ | Rest]) ->
    field_list_from_tokens(Rest);
field_list_from_tokens(_) ->
    [].

to_string(Tokens) ->
    S = erlide_scanner:tokens_to_string(Tokens),
    erlide_text:strip(S).
%%     unspacify(S).

get_head(T) ->
    A = get_args(T),
    case get_guards(T) of
        "" ->
            A;
        G ->
            A ++ " when " ++ G
    end.

get_args(T) ->
    case erlide_np_util:get_between_outer_pars(T, '(', ')') of
        [] ->
            "";
        P ->
            "("++to_string(P)++")"
    end.

get_guards([]) ->
    "";
get_guards(T) ->
    to_string(get_between(T, 'when', '->')). 

last_not_eof(L) -> 
    case lists:reverse(L) of
        [eof, Last | _] ->
            Last;
        [Last | _] ->
            Last;
        _ ->
            error
    end.    


get_between(L, A, B) ->
    R = get_upto(L, B),
    lists:reverse(get_upto(lists:reverse(R), A)).

get_upto(L, Delim) ->
        get_upto(L, Delim, []).

get_upto([], _Delim, _Acc) ->
    [];
get_upto([#token{kind=Delim} | _], Delim, Acc) ->
        lists:reverse(Acc);
get_upto([T | Rest], Delim, Acc) ->
    get_upto(Rest, Delim, [T | Acc]).

get_upto2(L, Delim) ->
    get_upto2(L, Delim, []).

get_upto2([], _Delim, Acc) ->
    {lists:reverse(Acc), []};
get_upto2([#token{kind=Delim} | Rest], Delim, Acc) ->
    {lists:reverse(Acc), Rest};
get_upto2([T | Rest], Delim, Acc) ->
    get_upto2(Rest, Delim, [T | Acc]).

check_clause([#token{kind = ';'} | Rest], false) ->
    check_class(Rest) == function;
check_clause(_, _) ->
    false.

split_clauses(F) ->
    split_clauses(F, false, [], []).

split_clauses([], _HaveWhen, Acc, []) ->
    erlide_util:reverse2(Acc);
split_clauses([], HaveWhen, Acc, ClAcc) ->
    split_clauses([], HaveWhen, [ClAcc | Acc], []);
split_clauses([#token{kind=Kind}=T | Rest], _HaveWhen, Acc, ClAcc) 
  when Kind=:='end'; Kind=:=dot; Kind=:='->'; Kind=:='when' ->
    split_clauses(Rest, Kind=:='when', Acc, [T | ClAcc]);
split_clauses([T | Rest] = Tokens, HaveWhen, Acc, ClAcc) ->
    case check_clause(Tokens, HaveWhen) of
        false ->
            split_clauses(Rest, HaveWhen, Acc, [T | ClAcc]);
        true ->
            split_clauses(Rest, HaveWhen, [[T | ClAcc] | Acc], [])
    end.

%% fix_clause([#token{kind=atom, value=Name, line=Line, offset=Offset, length=Length} | Rest] = Code) ->
fix_clauses(Clauses) ->
    fix_clauses(Clauses, []).

fix_clauses([], Acc) ->
    lists:reverse(Acc);
fix_clauses([C | Rest], Acc) ->
    {Clause, Refs} = fix_clause(C),
    fix_clauses(Rest, [{Clause, Refs} | Acc]).

fix_clause([#token{kind=AtomOrMacro, value=Name, line=Line, offset=Offset, length=Length} | Rest]) 
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    #token{line=LastLine, offset=LastOffset, length=LastLength} = last_not_eof(Rest),
    PosLength = LastOffset - Offset + LastLength+1,
    ExternalRefs = get_refs_in_code(Rest),
    {#clause{pos={{Line, LastLine, Offset}, PosLength}, name_pos={{Line, Offset}, Length},
             name=Name, args=get_function_args(Rest), head=get_head(Rest)},
     ExternalRefs}.

get_function_args(Tokens) ->
    P = erlide_np_util:get_between_outer_pars(Tokens, '(', ')'),
    L = erlide_text:split_comma_list(P),
    [to_string(A) || A <- L].

fix_refs(ClausesAndRefs, Function, Imports) ->
    fix_refs(ClausesAndRefs, Function, Imports, []).

fix_refs([], #function{name=Name, arity=Arity, name_pos={{_, Offset}, Length}}, _Imports, Acc) ->
    Ref = #ref{data=#function_def{function=Name, arity=Arity}, offset=Offset, 
               length=Length, function=Name, arity=Arity, clause="", sub_clause=false},
    [Ref | Acc];
fix_refs([{Clause, Refs} | Rest], Function, Imports, Acc0) ->
    Acc1 = fix_refs(Refs, Clause, Function, Imports, Acc0),
    fix_refs(Rest, Function, Imports, Acc1).

fix_refs([], _Clause, _Function, _Imports, Acc) ->
    Acc;
fix_refs([{Offset, Length, RefData} | Rest], #clause{head=Head}=Clause, 
         #function{name=Name, arity=Arity, clauses=Clauses}=Function, 
         Imports, Acc) ->
    SubClause = case Clauses of
                    [] -> false;
                    [_] -> false;
                    _ -> true
                end,
    NewRefData = fix_imported_ref(RefData, Imports),
    Ref = #ref{data=NewRefData, offset=Offset, length=Length, function=Name, 
               arity=Arity, clause=Head, sub_clause=SubClause},
    fix_refs(Rest, Clause, Function, Imports, [Ref | Acc]).

fix_imported_ref(#local_call{function=F, arity=A}=RefData, [{Module, Imports} | Rest]) ->
    case lists:member({F, A}, Imports) of
        true -> #external_call{module=Module, function=F, arity=A};
        false -> fix_imported_ref(RefData, Rest)
    end;
fix_imported_ref(RefData, _Imports) ->
    RefData.

%% find references in code
%% VERY simple, M:F(_ ...), F(_ ...), #R, ?M, and that's it
%% returns {Offset, Length, Ref}, which is made into proper refs
%% by fix_refs
get_refs_in_code(L) ->
    get_refs_in_code(L, []).

get_refs_in_code([], Acc) ->
    lists:reverse(Acc);
get_refs_in_code([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
                  #token{kind=atom, value=F, offset=Offset2, length=Length2},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length2+Offset2-Offset, 
         #external_call{module = M, function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=atom, value=F, offset=Offset, length=Length},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length, #local_call{function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest], 
                 Acc) ->
    R = {Offset, Length, #macro_ref{macro=M}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind='#', offset=Offset}, 
                  #token{kind=atom, value=Record, offset=Offset2, length=Length2} | Rest],
                 Acc) ->
    R = {Offset, Length2+Offset2-Offset, #record_ref{record=Record}},
    {NewRest, Fields, RightSides} = erlide_np_records:check_fields(Rest, Record),
    NewAcc = get_refs_in_code(RightSides, Acc),
    get_refs_in_code(NewRest, Fields ++ [R | NewAcc]);
get_refs_in_code([#token{kind=var, value=V, offset=Offset, length=Length} | Rest],
                 Acc) ->
    R = {Offset, Length, make_var_def_ref(Acc, V)},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([_ | Rest], Acc) ->
    get_refs_in_code(Rest, Acc).

make_var_def_ref([], Var) ->
	#var_def{variable=Var};
make_var_def_ref([{_, _, #var_def{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([{_, _, #var_ref{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([_ | Rest], Var) ->
	make_var_def_ref(Rest, Var).

%% find type references in spec, type and opaque
%% also VERY simple, type(), ?M, #R, only
%% returns proper refs
get_refs(Tokens, Name, Arity) ->
	get_refs(Tokens, Name, Arity, []).

get_refs([], _, _, Acc) ->
    lists:reverse(Acc);
get_refs([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #macro_ref{macro=M}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind='#', offset=Offset},
          #token{kind=atom, value=R, offset=Offset2, length=Length2} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset, #record_ref{record=R}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
		  #token{kind=atom, value=T, offset=Offset2, length=Length2},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset,
                   #type_ref{module=M, type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=T, offset=Offset, length=Length},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #type_ref{module='_', type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=variable, value=V, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #var_ref{variable=V}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([_ | Rest], Name, Arity, Acc) ->
    get_refs(Rest, Name, Arity, Acc).

get_record_field_defs([], _) ->
    [];
get_record_field_defs([{FieldName, {{_, _, Offset}, Length}, _Extra} | Rest],
                      RecordName) ->
    [make_ref(Offset, Length, #record_field_def{record=RecordName, field=FieldName}, 
              FieldName, ?ARI_RECORD_FIELD_DEF)
         | get_record_field_defs(Rest, RecordName)].

make_ref(Offset, Length, Data, Name, Arity) ->
    #ref{data=Data, offset=Offset, length=Length, function=Name, arity=Arity, 
         clause="", sub_clause=false}.

make_attribute_arg_refs(define, Name, [#token{}, #token{kind='('} | Rest]) ->
    get_refs(skip_to_rparen(Rest), Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(define, Name, [#token{}, #token{kind=','} | Rest]) ->
    get_refs(Rest, Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(record, {Name, Fields}, [#token{}, #token{kind=','} | Rest]) ->
    get_record_field_defs(Fields, Name) ++ get_refs(Rest, Name, ?ARI_RECORD_DEF);
make_attribute_arg_refs(_, _, _) ->
    [].

skip_to_rparen([]) ->
    [];
skip_to_rparen([#token{kind=')'} | Rest]) ->
    Rest;
skip_to_rparen([_ | Rest]) ->
    skip_to_rparen(Rest).

make_attribute_ref(Name, Between, Extra, Offset, Length) ->
    ?D({Name, Between, Offset, Length}),
    case make_attribute_ref(Name, Between, Extra) of
        [] ->
            [];
        {Arity, AName, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=AName, 
                  arity=Arity, clause="", sub_clause=false}];
        {Arity, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=Name, 
                  arity=Arity, clause="", sub_clause=false}]
    end.

make_attribute_ref(module, _, Extra) ->
    {?ARI_ATTRIBUTE, #module_def{module=Extra}};
make_attribute_ref(record, Between, _E) ->
    ?D({Between, _E}),
    Name = case Between of
               {N, _} -> N;
               N -> N
           end,
    R= #record_def{record=Name},
    ?D(R),
    {?ARI_RECORD_DEF, Name, R};
make_attribute_ref(define, [Name | _], _) when is_list(Name) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(define, Name, _) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(include, _, Extra) ->
    {?ARI_INCLUDE, #include_ref{filename=unquote(Extra)}};
make_attribute_ref(_, _, _) ->
    [].

unquote(L) ->
    lists:reverse(unquote_first(lists:reverse(unquote_first(L)))).

unquote_first([$" | Rest]) ->
    Rest;
unquote_first(L) ->
    L.

remove_rest([], _) -> [];
remove_rest(Rest, Rest) -> [];
remove_rest([Hd | Tl], Rest) -> [Hd | remove_rest(Tl, Rest)].
%% Description: Split source files into functions and clauses and collects references
%% Author: jakob
%% Created: Mar 23, 2006

-module(erlide_noparse).

%%
%% Exported Functions
%%

%% called from Java
-export([initial_parse/5, reparse/1, remove_cache_files/2]).

%% called from Erlang
-export([read_module_refs/3]).

%%
%% Include files
%%

%% -define(DEBUG, 1).
%% -define(IO_FORMAT_DEBUG, 1).

-define(CACHE_VERSION, 25).
-define(SERVER, erlide_noparse).

-include("erlide.hrl").
-include("erlide_noparse.hrl").
-include("erlide_scanner.hrl").
-include("erlide_search_server.hrl").

%% special arity flags
-define(ARI_TYPESPEC, -2).
-define(ARI_ATTRIBUTE, -3).
-define(ARI_RECORD_DEF, -4).
-define(ARI_MACRO_DEF, -5).
-define(ARI_INCLUDE, -6).
-define(ARI_RECORD_FIELD_DEF, -7).

-define(X(X), erlide_log:log({X, erlang:now()})).


%%
%% API Functions
%%

initial_parse(ScannerName, ModuleFileName, StateDir, UseCache,
              UpdateSearchServer) ->
    try
        %%         ?D({StateDir, ModuleFileName}),
        BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
        RefsFileName = BaseName ++ ".refs",
        RenewFun = fun(_F) ->
                           do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer)
                   end,
        CacheFileName = BaseName ++ ".noparse",
        ?D(CacheFileName),
        {Cached, Res} = erlide_util:check_and_renew_cached(
                          ModuleFileName, CacheFileName, ?CACHE_VERSION, 
                          RenewFun, UseCache),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        ?D(updated),
        {ok, Res, Cached}
    catch
        error:Reason ->
            {error, Reason}
    end.

reparse(ScannerName) ->
    try
        Res = do_parse(ScannerName, "", "", true),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        {ok, Res, unused}
    catch
        error:Reason ->
            {error, Reason}
    end.

read_module_refs(ScannerName, ModulePath, StateDir) ->
    ?D(ScannerName),
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    RefsFileName = BaseName ++ ".refs",
    ?D(RefsFileName),
    case file:read_file(RefsFileName) of
        {ok, Binary} ->
            ?D(byte_size(Binary)),
            binary_to_term(Binary);
        _ ->
            ?D(ModulePath),
            {ok, InitialTextBin} = file:read_file(ModulePath),
            ?D(byte_size(InitialTextBin)),
            InitialText = binary_to_list(InitialTextBin),
            _D = erlide_scanner_server:initialScan(
                   ScannerName, ModulePath, InitialText, StateDir, true),
            ?D(_D),
            initial_parse(ScannerName, ModulePath, StateDir, true, false),
            ?D(RefsFileName),
            {ok, Binary} = file:read_file(RefsFileName),
            ?D(byte_size(Binary)),
            binary_to_term(Binary)
    end.

remove_cache_files(ScannerName, StateDir) ->
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    ScannerCacheFileName = BaseName ++ ".scan",
    file:delete(ScannerCacheFileName),
    RefsFileName = BaseName ++ ".refs",
    file:delete(RefsFileName),
    CacheFileName = BaseName ++ ".noparse",
    file:delete(CacheFileName).

%%
%% Internal functions
%%

do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer) ->
    Toks = erlide_scanner_server:getTokens(ScannerName),
    do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer).

do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer) ->
    ?D({do_parse, ScannerName, length(Toks)}),
    ?X(111),
    {UncommentToks, Comments} = erlide_np_util:extract_comments(Toks),
    ?X(222),
    ?D({length(UncommentToks), length(Comments)}),
    ?D({UncommentToks}),
    Functions = erlide_np_util:split_after_dots(UncommentToks),
    ?X(333),
    ?D(length(Functions)),
    AutoImports = erlide_util:add_auto_imported([]),
    ?X(444),
%%     ?D(AutoImports),
    {Collected, Refs} = classify_and_collect(Functions, [], [], [], AutoImports),
    ?X(555),
    ?D({'>>',length(Collected)}),
    CommentedCollected = erlide_np_util:get_function_comments(Collected, Comments),
    ?X(666),
    ?D(CommentedCollected),
    Model = #model{forms=CommentedCollected, comments=Comments},
    %%erlide_noparse_server:create(ScannerName, Model),
%%     ?D({"Model", length(Model#model.forms), erts_debug:flat_size(Model)}),
    FixedModel = fixup_model(Model),
    ?X(777),
%%     ?D(erts_debug:flat_size(FixedModel)),
    %% 	?D([erts_debug:flat_size(F) || F <- element(2, FixedModel)]),
    ?D(StateDir),
    case StateDir of
        "" -> ok;
        _ ->
            ?D({RefsFileName, length(Refs)}),
            file:write_file(RefsFileName, term_to_binary(Refs, [compressed]))
    end,
    update_search_server(UpdateSearchServer, ScannerName, Refs),
%%     ?D(FixedModel),
    ?X(888),
    FixedModel.

update_search_server(true, ScannerName, Refs) ->
    erlide_search_server:add_module_refs(ScannerName, Refs);
update_search_server(_, _, _) ->
    ok.

fixup_model(#model{forms=Forms, comments=Comments}) ->
    FixedComments = fixup_tokens(Comments),
    FixedForms = fixup_forms(Forms),
    #model{forms=FixedForms, comments=FixedComments}.

fixup_forms(Forms) ->
    [fixup_form(Form) || Form <- Forms].

fixup_tokens(Tokens) ->
    [fixup_token(Token) || Token <- Tokens].

fixup_token(#token{value=Value} = Token) when is_list(Value) ->
    Token#token{value=to_binary(Value)};
fixup_token(#token{text=Text} = Token) when is_list(Text) ->
    Token#token{value=to_binary(Text)};
fixup_token(Token) ->
    Token.

binary_args(Args) when is_list(Args) ->
    [iolist_to_binary(A) || A <- Args];
binary_args(_) ->
    [].

fixup_form(#function{comment=Comment, clauses=Clauses, args=Args} = Function) ->
    Function#function{comment= to_binary(Comment), clauses=fixup_forms(Clauses), args=binary_args(Args)};
fixup_form(#clause{head=Head, args=Args} = Clause) ->
    Clause#clause{head=to_binary(Head), args=binary_args(Args)};
fixup_form(Other) ->
    Other.

to_binary(Comment) when is_list(Comment) ->
    try 
        iolist_to_binary(Comment) 
    catch 
        _:_ -> 
            unicode:characters_to_binary(Comment) 
    end;
to_binary(Other) ->
    Other.

%% parse_test(ScannerName, File) ->
%%     erlide_scanner_server:scan_uncached(ScannerName, File, ""),
%%     Toks = erlide_scanner_server:getTokens(ScannerName),
%%     {UncommentToks, Comments} = extract_comments(Toks),
%%     Functions = split_after_dots(UncommentToks, [], []),
%%     {Collected, Refs} = classify_and_collect(Functions, [], []),
%%     _Model = #model{forms=Collected, comments=Comments},
%%     %%erlide_noparse_server:create(ScannerName, Model, ""),
%%     ok.

classify_and_collect([], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([eof], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([C | Rest], Acc, RefsAcc, Exports, Imports) ->
    {R, Refs, MoreExports, MoreImports} = cac(check_class(C), C, Exports, Imports),
    classify_and_collect(Rest, [R | Acc], Refs++RefsAcc, MoreExports++Exports, MoreImports++Imports).

cac(function, Tokens, Exports, Imports) ->
    ClauseList = split_clauses(Tokens),
    %?D(ClauseList),
    ClausesAndRefs = fix_clauses(ClauseList),
    %?D(length(Clauses)),
    [{#clause{pos=P, name=N, args=A, head=H, name_pos=NP}, _Refs} | _] = ClausesAndRefs,
    ?D(A),
    Arity = length(A),
    Exported = get_exported({N, Arity}, Exports),
    Function = case ClausesAndRefs of  	% only show subclauses when more than one
                   [_] ->
                       #function{pos=P, name=N, arity=Arity, args=A, head=H, 
                                 clauses=[], name_pos=NP, exported=Exported};
                   _ ->
                       Clauses = [C || {C, _} <- ClausesAndRefs],
                       #function{pos=P, name=N, arity=Arity, clauses=Clauses, 
                                 name_pos=NP, exported=Exported}
               end,
    {Function, fix_refs(ClausesAndRefs, Function, Imports), [], []};
cac(attribute, Attribute, _Exports, _Imports) ->
    ?D(Attribute),
    case Attribute of
        %% -spec, -type or -opaque
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=Kind, line=_Line, offset=_Offset, value=Name} | Args]
          when (Kind=:='spec') or ((Kind=:=atom) and (Name=:='type'))
                   or ((Kind=:=atom) and (Name=:='opaque'))->
            get_type_attribute(Kind, Name, Offset, Line, Attribute, Args);
        %% other attributes
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=atom, value=Name, line=_Line, offset=_Offset},
         _, #token{value=Args} | _] = Attribute ->
            get_other_attribute(Name, Offset, Line, Attribute, Args);
        [_, #token{kind=atom, value=Name, line=Line, offset=Offset} | _] ->
            #token{line=LastLine, offset=LastOffset, 
                   length=LastLength} = last_not_eof(Attribute),
            PosLength = LastOffset - Offset + LastLength,
            {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                       name=Name, args=[]}, [], [], []}
    end;
cac(other, [#token{value=Name, line=Line, offset=Offset, length=Length} | _], 
    _Exports, _Imports) ->
    {#other{pos={{Line, Line, Offset}, Length}, name=Name}, [], [], []};
cac(_, _D, _E, _I) ->
    {eof, [], [], []}.

get_type_attribute(Kind, Name0, Offset, Line, Attribute, Args) ->
    ?D(Name0),
    Name = case Kind of 'spec' -> Kind; _ -> Name0 end,
    #token{line=LastLine, offset=LastOffset,
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    ?D(Args),
    Extra = to_string(Args),
    ?D(Extra),
    {AttrArgs, _, _} = get_attribute_args(Kind, Args, Args),
    ?D({AttrArgs, Extra}),
    ExternalRefs = get_refs(tl(AttrArgs), Extra, ?ARI_TYPESPEC),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     [#ref{data=#type_def{type=Name}, offset=Offset, length=PosLength, function=Name, 
           arity=?ARI_TYPESPEC, clause="", sub_clause=false} | ExternalRefs], [], []}.

get_other_attribute(Name, Offset, Line, Attribute, Args) ->
    #token{line=LastLine, offset=LastOffset, 
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    Between = erlide_np_util:get_between_outer_pars(Attribute, '(', ')'),
    Extra = to_string(Between),
    {AttrArgs, Exports, Imports} = get_attribute_args(Name, Between, Args),
    ?D({AttrArgs, Between}),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     make_attribute_ref(Name, AttrArgs, Extra, Offset, PosLength)++
         make_attribute_arg_refs(Name, AttrArgs, Between),
     Exports, Imports}.

get_exported(F_A, Exports) ->
    lists:member(F_A, Exports).

get_attribute_args(import, Between, _Args) ->
    From = get_first_of_kind(atom, Between),
    Tokens = get_between(Between, '[', ']'),
    Import = {From, fun_arity_from_tokens(Tokens)},
    {Import, [], [Import]};
get_attribute_args(export, Between, _Args) ->
    Tokens = get_between(Between, '[', ']'),
    Exports = fun_arity_from_tokens(Tokens),
    {Exports, Exports, []};
get_attribute_args(record, Between, _Args) ->
    [RecordToken | _] = Between,
    RecordName = RecordToken#token.value,
    Tokens = erlide_np_util:get_between_outer_pars(Between, '{', '}'),
    R = {RecordName, field_list_from_tokens(Tokens)},
    {R, [], []};
get_attribute_args(_, _Between, Args) ->
    {Args, [], []}.

check_class([#token{kind = AtomOrMacro}, #token{kind = '('} | _])
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    function;
check_class([#token{kind = '-'}, #token{kind = atom} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'spec'} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'type'} | _]) ->
    attribute;
check_class(_X) ->
    %?D(lists:sublist(_X, 5)),
    other.

get_first_of_kind(Kind, Tokens) ->
    case erlide_np_util:skip_to(Tokens, Kind) of
        [#token{kind=Kind, value=Value} | _] ->
            Value;
        _ ->
            undefined
    end.
        
fun_arity_from_tokens([#token{kind=atom, value=Fun}, #token{kind='/'}, 
                       #token{kind=integer, value=Arity} | Rest]) ->
    [{Fun, Arity} | fun_arity_from_tokens(Rest)];
fun_arity_from_tokens([_ | Rest]) ->
    fun_arity_from_tokens(Rest);
fun_arity_from_tokens(_) ->
    [].

field_list_from_tokens([#token{kind=atom, value=Field, line=Line, 
                               offset=Offset, length=Length} | Rest]) ->
    {Extra, NewRest} = get_upto2(Rest, ','),
    [{Field, {{Line, Line, Offset}, Length}, to_string(Extra)}
         | field_list_from_tokens(NewRest)];
field_list_from_tokens([_ | Rest]) ->
    field_list_from_tokens(Rest);
field_list_from_tokens(_) ->
    [].

to_string(Tokens) ->
    S = erlide_scanner:tokens_to_string(Tokens),
    erlide_text:strip(S).
%%     unspacify(S).

get_head(T) ->
    A = get_args(T),
    case get_guards(T) of
        "" ->
            A;
        G ->
            A ++ " when " ++ G
    end.

get_args(T) ->
    case erlide_np_util:get_between_outer_pars(T, '(', ')') of
        [] ->
            "";
        P ->
            "("++to_string(P)++")"
    end.

get_guards([]) ->
    "";
get_guards(T) ->
    to_string(get_between(T, 'when', '->')). 

last_not_eof(L) -> 
    case lists:reverse(L) of
        [eof, Last | _] ->
            Last;
        [Last | _] ->
            Last;
        _ ->
            error
    end.    


get_between(L, A, B) ->
    R = get_upto(L, B),
    lists:reverse(get_upto(lists:reverse(R), A)).

get_upto(L, Delim) ->
        get_upto(L, Delim, []).

get_upto([], _Delim, _Acc) ->
    [];
get_upto([#token{kind=Delim} | _], Delim, Acc) ->
        lists:reverse(Acc);
get_upto([T | Rest], Delim, Acc) ->
    get_upto(Rest, Delim, [T | Acc]).

get_upto2(L, Delim) ->
    get_upto2(L, Delim, []).

get_upto2([], _Delim, Acc) ->
    {lists:reverse(Acc), []};
get_upto2([#token{kind=Delim} | Rest], Delim, Acc) ->
    {lists:reverse(Acc), Rest};
get_upto2([T | Rest], Delim, Acc) ->
    get_upto2(Rest, Delim, [T | Acc]).

check_clause([#token{kind = ';'} | Rest], false) ->
    check_class(Rest) == function;
check_clause(_, _) ->
    false.

split_clauses(F) ->
    split_clauses(F, false, [], []).

split_clauses([], _HaveWhen, Acc, []) ->
    erlide_util:reverse2(Acc);
split_clauses([], HaveWhen, Acc, ClAcc) ->
    split_clauses([], HaveWhen, [ClAcc | Acc], []);
split_clauses([#token{kind=Kind}=T | Rest], _HaveWhen, Acc, ClAcc) 
  when Kind=:='end'; Kind=:=dot; Kind=:='->'; Kind=:='when' ->
    split_clauses(Rest, Kind=:='when', Acc, [T | ClAcc]);
split_clauses([T | Rest] = Tokens, HaveWhen, Acc, ClAcc) ->
    case check_clause(Tokens, HaveWhen) of
        false ->
            split_clauses(Rest, HaveWhen, Acc, [T | ClAcc]);
        true ->
            split_clauses(Rest, HaveWhen, [[T | ClAcc] | Acc], [])
    end.

%% fix_clause([#token{kind=atom, value=Name, line=Line, offset=Offset, length=Length} | Rest] = Code) ->
fix_clauses(Clauses) ->
    fix_clauses(Clauses, []).

fix_clauses([], Acc) ->
    lists:reverse(Acc);
fix_clauses([C | Rest], Acc) ->
    {Clause, Refs} = fix_clause(C),
    fix_clauses(Rest, [{Clause, Refs} | Acc]).

fix_clause([#token{kind=AtomOrMacro, value=Name, line=Line, offset=Offset, length=Length} | Rest]) 
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    #token{line=LastLine, offset=LastOffset, length=LastLength} = last_not_eof(Rest),
    PosLength = LastOffset - Offset + LastLength+1,
    ExternalRefs = get_refs_in_code(Rest),
    {#clause{pos={{Line, LastLine, Offset}, PosLength}, name_pos={{Line, Offset}, Length},
             name=Name, args=get_function_args(Rest), head=get_head(Rest)},
     ExternalRefs}.

get_function_args(Tokens) ->
    P = erlide_np_util:get_between_outer_pars(Tokens, '(', ')'),
    L = erlide_text:split_comma_list(P),
    [to_string(A) || A <- L].

fix_refs(ClausesAndRefs, Function, Imports) ->
    fix_refs(ClausesAndRefs, Function, Imports, []).

fix_refs([], #function{name=Name, arity=Arity, name_pos={{_, Offset}, Length}}, _Imports, Acc) ->
    Ref = #ref{data=#function_def{function=Name, arity=Arity}, offset=Offset, 
               length=Length, function=Name, arity=Arity, clause="", sub_clause=false},
    [Ref | Acc];
fix_refs([{Clause, Refs} | Rest], Function, Imports, Acc0) ->
    Acc1 = fix_refs(Refs, Clause, Function, Imports, Acc0),
    fix_refs(Rest, Function, Imports, Acc1).

fix_refs([], _Clause, _Function, _Imports, Acc) ->
    Acc;
fix_refs([{Offset, Length, RefData} | Rest], #clause{head=Head}=Clause, 
         #function{name=Name, arity=Arity, clauses=Clauses}=Function, 
         Imports, Acc) ->
    SubClause = case Clauses of
                    [] -> false;
                    [_] -> false;
                    _ -> true
                end,
    NewRefData = fix_imported_ref(RefData, Imports),
    Ref = #ref{data=NewRefData, offset=Offset, length=Length, function=Name, 
               arity=Arity, clause=Head, sub_clause=SubClause},
    fix_refs(Rest, Clause, Function, Imports, [Ref | Acc]).

fix_imported_ref(#local_call{function=F, arity=A}=RefData, [{Module, Imports} | Rest]) ->
    case lists:member({F, A}, Imports) of
        true -> #external_call{module=Module, function=F, arity=A};
        false -> fix_imported_ref(RefData, Rest)
    end;
fix_imported_ref(RefData, _Imports) ->
    RefData.

%% find references in code
%% VERY simple, M:F(_ ...), F(_ ...), #R, ?M, and that's it
%% returns {Offset, Length, Ref}, which is made into proper refs
%% by fix_refs
get_refs_in_code(L) ->
    get_refs_in_code(L, []).

get_refs_in_code([], Acc) ->
    lists:reverse(Acc);
get_refs_in_code([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
                  #token{kind=atom, value=F, offset=Offset2, length=Length2},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length2+Offset2-Offset, 
         #external_call{module = M, function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=atom, value=F, offset=Offset, length=Length},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length, #local_call{function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest], 
                 Acc) ->
    R = {Offset, Length, #macro_ref{macro=M}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind='#', offset=Offset}, 
                  #token{kind=atom, value=Record, offset=Offset2, length=Length2} | Rest],
                 Acc) ->
    R = {Offset, Length2+Offset2-Offset, #record_ref{record=Record}},
    {NewRest, Fields, RightSides} = erlide_np_records:check_fields(Rest, Record),
    NewAcc = get_refs_in_code(RightSides, Acc),
    get_refs_in_code(NewRest, Fields ++ [R | NewAcc]);
get_refs_in_code([#token{kind=var, value=V, offset=Offset, length=Length} | Rest],
                 Acc) ->
    R = {Offset, Length, make_var_def_ref(Acc, V)},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([_ | Rest], Acc) ->
    get_refs_in_code(Rest, Acc).

make_var_def_ref([], Var) ->
	#var_def{variable=Var};
make_var_def_ref([{_, _, #var_def{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([{_, _, #var_ref{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([_ | Rest], Var) ->
	make_var_def_ref(Rest, Var).

%% find type references in spec, type and opaque
%% also VERY simple, type(), ?M, #R, only
%% returns proper refs
get_refs(Tokens, Name, Arity) ->
	get_refs(Tokens, Name, Arity, []).

get_refs([], _, _, Acc) ->
    lists:reverse(Acc);
get_refs([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #macro_ref{macro=M}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind='#', offset=Offset},
          #token{kind=atom, value=R, offset=Offset2, length=Length2} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset, #record_ref{record=R}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
		  #token{kind=atom, value=T, offset=Offset2, length=Length2},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset,
                   #type_ref{module=M, type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=T, offset=Offset, length=Length},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #type_ref{module='_', type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=variable, value=V, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #var_ref{variable=V}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([_ | Rest], Name, Arity, Acc) ->
    get_refs(Rest, Name, Arity, Acc).

get_record_field_defs([], _) ->
    [];
get_record_field_defs([{FieldName, {{_, _, Offset}, Length}, _Extra} | Rest],
                      RecordName) ->
    [make_ref(Offset, Length, #record_field_def{record=RecordName, field=FieldName}, 
              FieldName, ?ARI_RECORD_FIELD_DEF)
         | get_record_field_defs(Rest, RecordName)].

make_ref(Offset, Length, Data, Name, Arity) ->
    #ref{data=Data, offset=Offset, length=Length, function=Name, arity=Arity, 
         clause="", sub_clause=false}.

make_attribute_arg_refs(define, Name, [#token{}, #token{kind='('} | Rest]) ->
    get_refs(skip_to_rparen(Rest), Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(define, Name, [#token{}, #token{kind=','} | Rest]) ->
    get_refs(Rest, Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(record, {Name, Fields}, [#token{}, #token{kind=','} | Rest]) ->
    get_record_field_defs(Fields, Name) ++ get_refs(Rest, Name, ?ARI_RECORD_DEF);
make_attribute_arg_refs(_, _, _) ->
    [].

skip_to_rparen([]) ->
    [];
skip_to_rparen([#token{kind=')'} | Rest]) ->
    Rest;
skip_to_rparen([_ | Rest]) ->
    skip_to_rparen(Rest).

make_attribute_ref(Name, Between, Extra, Offset, Length) ->
    ?D({Name, Between, Offset, Length}),
    case make_attribute_ref(Name, Between, Extra) of
        [] ->
            [];
        {Arity, AName, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=AName, 
                  arity=Arity, clause="", sub_clause=false}];
        {Arity, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=Name, 
                  arity=Arity, clause="", sub_clause=false}]
    end.

make_attribute_ref(module, _, Extra) ->
    {?ARI_ATTRIBUTE, #module_def{module=Extra}};
make_attribute_ref(record, Between, _E) ->
    ?D({Between, _E}),
    Name = case Between of
               {N, _} -> N;
               N -> N
           end,
    R= #record_def{record=Name},
    ?D(R),
    {?ARI_RECORD_DEF, Name, R};
make_attribute_ref(define, [Name | _], _) when is_list(Name) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(define, Name, _) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(include, _, Extra) ->
    {?ARI_INCLUDE, #include_ref{filename=unquote(Extra)}};
make_attribute_ref(_, _, _) ->
    [].

unquote(L) ->
    lists:reverse(unquote_first(lists:reverse(unquote_first(L)))).

unquote_first([$" | Rest]) ->
    Rest;
unquote_first(L) ->
    L.

remove_rest([], _) -> [];
remove_rest(Rest, Rest) -> [];
remove_rest([Hd | Tl], Rest) -> [Hd | remove_rest(Tl, Rest)].
%% Description: Split source files into functions and clauses and collects references
%% Author: jakob
%% Created: Mar 23, 2006

-module(erlide_noparse).

%%
%% Exported Functions
%%

%% called from Java
-export([initial_parse/5, reparse/1, remove_cache_files/2]).

%% called from Erlang
-export([read_module_refs/3]).

%%
%% Include files
%%

%% -define(DEBUG, 1).
%% -define(IO_FORMAT_DEBUG, 1).

-define(CACHE_VERSION, 25).
-define(SERVER, erlide_noparse).

-include("erlide.hrl").
-include("erlide_noparse.hrl").
-include("erlide_scanner.hrl").
-include("erlide_search_server.hrl").

%% special arity flags
-define(ARI_TYPESPEC, -2).
-define(ARI_ATTRIBUTE, -3).
-define(ARI_RECORD_DEF, -4).
-define(ARI_MACRO_DEF, -5).
-define(ARI_INCLUDE, -6).
-define(ARI_RECORD_FIELD_DEF, -7).

-define(X(X), erlide_log:log({X, erlang:now()})).


%%
%% API Functions
%%

initial_parse(ScannerName, ModuleFileName, StateDir, UseCache,
              UpdateSearchServer) ->
    try
        %%         ?D({StateDir, ModuleFileName}),
        BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
        RefsFileName = BaseName ++ ".refs",
        RenewFun = fun(_F) ->
                           do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer)
                   end,
        CacheFileName = BaseName ++ ".noparse",
        ?D(CacheFileName),
        {Cached, Res} = erlide_util:check_and_renew_cached(
                          ModuleFileName, CacheFileName, ?CACHE_VERSION, 
                          RenewFun, UseCache),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        ?D(updated),
        {ok, Res, Cached}
    catch
        error:Reason ->
            {error, Reason}
    end.

reparse(ScannerName) ->
    try
        Res = do_parse(ScannerName, "", "", true),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        {ok, Res, unused}
    catch
        error:Reason ->
            {error, Reason}
    end.

read_module_refs(ScannerName, ModulePath, StateDir) ->
    ?D(ScannerName),
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    RefsFileName = BaseName ++ ".refs",
    ?D(RefsFileName),
    case file:read_file(RefsFileName) of
        {ok, Binary} ->
            ?D(byte_size(Binary)),
            binary_to_term(Binary);
        _ ->
            ?D(ModulePath),
            {ok, InitialTextBin} = file:read_file(ModulePath),
            ?D(byte_size(InitialTextBin)),
            InitialText = binary_to_list(InitialTextBin),
            _D = erlide_scanner_server:initialScan(
                   ScannerName, ModulePath, InitialText, StateDir, true),
            ?D(_D),
            initial_parse(ScannerName, ModulePath, StateDir, true, false),
            ?D(RefsFileName),
            {ok, Binary} = file:read_file(RefsFileName),
            ?D(byte_size(Binary)),
            binary_to_term(Binary)
    end.

remove_cache_files(ScannerName, StateDir) ->
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    ScannerCacheFileName = BaseName ++ ".scan",
    file:delete(ScannerCacheFileName),
    RefsFileName = BaseName ++ ".refs",
    file:delete(RefsFileName),
    CacheFileName = BaseName ++ ".noparse",
    file:delete(CacheFileName).

%%
%% Internal functions
%%

do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer) ->
    Toks = erlide_scanner_server:getTokens(ScannerName),
    do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer).

do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer) ->
    ?D({do_parse, ScannerName, length(Toks)}),
    ?X(111),
    {UncommentToks, Comments} = erlide_np_util:extract_comments(Toks),
    ?X(222),
    ?D({length(UncommentToks), length(Comments)}),
    ?D({UncommentToks}),
    Functions = erlide_np_util:split_after_dots(UncommentToks),
    ?X(333),
    ?D(length(Functions)),
    AutoImports = erlide_util:add_auto_imported([]),
    ?X(444),
%%     ?D(AutoImports),
    {Collected, Refs} = classify_and_collect(Functions, [], [], [], AutoImports),
    ?X(555),
    ?D({'>>',length(Collected)}),
    CommentedCollected = erlide_np_util:get_function_comments(Collected, Comments),
    ?X(666),
    ?D(CommentedCollected),
    Model = #model{forms=CommentedCollected, comments=Comments},
    %%erlide_noparse_server:create(ScannerName, Model),
%%     ?D({"Model", length(Model#model.forms), erts_debug:flat_size(Model)}),
    FixedModel = fixup_model(Model),
    ?X(777),
%%     ?D(erts_debug:flat_size(FixedModel)),
    %% 	?D([erts_debug:flat_size(F) || F <- element(2, FixedModel)]),
    ?D(StateDir),
    case StateDir of
        "" -> ok;
        _ ->
            ?D({RefsFileName, length(Refs)}),
            file:write_file(RefsFileName, term_to_binary(Refs, [compressed]))
    end,
    update_search_server(UpdateSearchServer, ScannerName, Refs),
%%     ?D(FixedModel),
    ?X(888),
    FixedModel.

update_search_server(true, ScannerName, Refs) ->
    erlide_search_server:add_module_refs(ScannerName, Refs);
update_search_server(_, _, _) ->
    ok.

fixup_model(#model{forms=Forms, comments=Comments}) ->
    FixedComments = fixup_tokens(Comments),
    FixedForms = fixup_forms(Forms),
    #model{forms=FixedForms, comments=FixedComments}.

fixup_forms(Forms) ->
    [fixup_form(Form) || Form <- Forms].

fixup_tokens(Tokens) ->
    [fixup_token(Token) || Token <- Tokens].

fixup_token(#token{value=Value} = Token) when is_list(Value) ->
    Token#token{value=to_binary(Value)};
fixup_token(#token{text=Text} = Token) when is_list(Text) ->
    Token#token{value=to_binary(Text)};
fixup_token(Token) ->
    Token.

binary_args(Args) when is_list(Args) ->
    [iolist_to_binary(A) || A <- Args];
binary_args(_) ->
    [].

fixup_form(#function{comment=Comment, clauses=Clauses, args=Args} = Function) ->
    Function#function{comment= to_binary(Comment), clauses=fixup_forms(Clauses), args=binary_args(Args)};
fixup_form(#clause{head=Head, args=Args} = Clause) ->
    Clause#clause{head=to_binary(Head), args=binary_args(Args)};
fixup_form(Other) ->
    Other.

to_binary(Comment) when is_list(Comment) ->
    try 
        iolist_to_binary(Comment) 
    catch 
        _:_ -> 
            unicode:characters_to_binary(Comment) 
    end;
to_binary(Other) ->
    Other.

%% parse_test(ScannerName, File) ->
%%     erlide_scanner_server:scan_uncached(ScannerName, File, ""),
%%     Toks = erlide_scanner_server:getTokens(ScannerName),
%%     {UncommentToks, Comments} = extract_comments(Toks),
%%     Functions = split_after_dots(UncommentToks, [], []),
%%     {Collected, Refs} = classify_and_collect(Functions, [], []),
%%     _Model = #model{forms=Collected, comments=Comments},
%%     %%erlide_noparse_server:create(ScannerName, Model, ""),
%%     ok.

classify_and_collect([], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([eof], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([C | Rest], Acc, RefsAcc, Exports, Imports) ->
    {R, Refs, MoreExports, MoreImports} = cac(check_class(C), C, Exports, Imports),
    classify_and_collect(Rest, [R | Acc], Refs++RefsAcc, MoreExports++Exports, MoreImports++Imports).

cac(function, Tokens, Exports, Imports) ->
    ClauseList = split_clauses(Tokens),
    %?D(ClauseList),
    ClausesAndRefs = fix_clauses(ClauseList),
    %?D(length(Clauses)),
    [{#clause{pos=P, name=N, args=A, head=H, name_pos=NP}, _Refs} | _] = ClausesAndRefs,
    ?D(A),
    Arity = length(A),
    Exported = get_exported({N, Arity}, Exports),
    Function = case ClausesAndRefs of  	% only show subclauses when more than one
                   [_] ->
                       #function{pos=P, name=N, arity=Arity, args=A, head=H, 
                                 clauses=[], name_pos=NP, exported=Exported};
                   _ ->
                       Clauses = [C || {C, _} <- ClausesAndRefs],
                       #function{pos=P, name=N, arity=Arity, clauses=Clauses, 
                                 name_pos=NP, exported=Exported}
               end,
    {Function, fix_refs(ClausesAndRefs, Function, Imports), [], []};
cac(attribute, Attribute, _Exports, _Imports) ->
    ?D(Attribute),
    case Attribute of
        %% -spec, -type or -opaque
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=Kind, line=_Line, offset=_Offset, value=Name} | Args]
          when (Kind=:='spec') or ((Kind=:=atom) and (Name=:='type'))
                   or ((Kind=:=atom) and (Name=:='opaque'))->
            get_type_attribute(Kind, Name, Offset, Line, Attribute, Args);
        %% other attributes
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=atom, value=Name, line=_Line, offset=_Offset},
         _, #token{value=Args} | _] = Attribute ->
            get_other_attribute(Name, Offset, Line, Attribute, Args);
        [_, #token{kind=atom, value=Name, line=Line, offset=Offset} | _] ->
            #token{line=LastLine, offset=LastOffset, 
                   length=LastLength} = last_not_eof(Attribute),
            PosLength = LastOffset - Offset + LastLength,
            {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                       name=Name, args=[]}, [], [], []}
    end;
cac(other, [#token{value=Name, line=Line, offset=Offset, length=Length} | _], 
    _Exports, _Imports) ->
    {#other{pos={{Line, Line, Offset}, Length}, name=Name}, [], [], []};
cac(_, _D, _E, _I) ->
    {eof, [], [], []}.

get_type_attribute(Kind, Name0, Offset, Line, Attribute, Args) ->
    ?D(Name0),
    Name = case Kind of 'spec' -> Kind; _ -> Name0 end,
    #token{line=LastLine, offset=LastOffset,
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    ?D(Args),
    Extra = to_string(Args),
    ?D(Extra),
    {AttrArgs, _, _} = get_attribute_args(Kind, Args, Args),
    ?D({AttrArgs, Extra}),
    ExternalRefs = get_refs(tl(AttrArgs), Extra, ?ARI_TYPESPEC),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     [#ref{data=#type_def{type=Name}, offset=Offset, length=PosLength, function=Name, 
           arity=?ARI_TYPESPEC, clause="", sub_clause=false} | ExternalRefs], [], []}.

get_other_attribute(Name, Offset, Line, Attribute, Args) ->
    #token{line=LastLine, offset=LastOffset, 
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    Between = erlide_np_util:get_between_outer_pars(Attribute, '(', ')'),
    Extra = to_string(Between),
    {AttrArgs, Exports, Imports} = get_attribute_args(Name, Between, Args),
    ?D({AttrArgs, Between}),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     make_attribute_ref(Name, AttrArgs, Extra, Offset, PosLength)++
         make_attribute_arg_refs(Name, AttrArgs, Between),
     Exports, Imports}.

get_exported(F_A, Exports) ->
    lists:member(F_A, Exports).

get_attribute_args(import, Between, _Args) ->
    From = get_first_of_kind(atom, Between),
    Tokens = get_between(Between, '[', ']'),
    Import = {From, fun_arity_from_tokens(Tokens)},
    {Import, [], [Import]};
get_attribute_args(export, Between, _Args) ->
    Tokens = get_between(Between, '[', ']'),
    Exports = fun_arity_from_tokens(Tokens),
    {Exports, Exports, []};
get_attribute_args(record, Between, _Args) ->
    [RecordToken | _] = Between,
    RecordName = RecordToken#token.value,
    Tokens = erlide_np_util:get_between_outer_pars(Between, '{', '}'),
    R = {RecordName, field_list_from_tokens(Tokens)},
    {R, [], []};
get_attribute_args(_, _Between, Args) ->
    {Args, [], []}.

check_class([#token{kind = AtomOrMacro}, #token{kind = '('} | _])
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    function;
check_class([#token{kind = '-'}, #token{kind = atom} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'spec'} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'type'} | _]) ->
    attribute;
check_class(_X) ->
    %?D(lists:sublist(_X, 5)),
    other.

get_first_of_kind(Kind, Tokens) ->
    case erlide_np_util:skip_to(Tokens, Kind) of
        [#token{kind=Kind, value=Value} | _] ->
            Value;
        _ ->
            undefined
    end.
        
fun_arity_from_tokens([#token{kind=atom, value=Fun}, #token{kind='/'}, 
                       #token{kind=integer, value=Arity} | Rest]) ->
    [{Fun, Arity} | fun_arity_from_tokens(Rest)];
fun_arity_from_tokens([_ | Rest]) ->
    fun_arity_from_tokens(Rest);
fun_arity_from_tokens(_) ->
    [].

field_list_from_tokens([#token{kind=atom, value=Field, line=Line, 
                               offset=Offset, length=Length} | Rest]) ->
    {Extra, NewRest} = get_upto2(Rest, ','),
    [{Field, {{Line, Line, Offset}, Length}, to_string(Extra)}
         | field_list_from_tokens(NewRest)];
field_list_from_tokens([_ | Rest]) ->
    field_list_from_tokens(Rest);
field_list_from_tokens(_) ->
    [].

to_string(Tokens) ->
    S = erlide_scanner:tokens_to_string(Tokens),
    erlide_text:strip(S).
%%     unspacify(S).

get_head(T) ->
    A = get_args(T),
    case get_guards(T) of
        "" ->
            A;
        G ->
            A ++ " when " ++ G
    end.

get_args(T) ->
    case erlide_np_util:get_between_outer_pars(T, '(', ')') of
        [] ->
            "";
        P ->
            "("++to_string(P)++")"
    end.

get_guards([]) ->
    "";
get_guards(T) ->
    to_string(get_between(T, 'when', '->')). 

last_not_eof(L) -> 
    case lists:reverse(L) of
        [eof, Last | _] ->
            Last;
        [Last | _] ->
            Last;
        _ ->
            error
    end.    


get_between(L, A, B) ->
    R = get_upto(L, B),
    lists:reverse(get_upto(lists:reverse(R), A)).

get_upto(L, Delim) ->
        get_upto(L, Delim, []).

get_upto([], _Delim, _Acc) ->
    [];
get_upto([#token{kind=Delim} | _], Delim, Acc) ->
        lists:reverse(Acc);
get_upto([T | Rest], Delim, Acc) ->
    get_upto(Rest, Delim, [T | Acc]).

get_upto2(L, Delim) ->
    get_upto2(L, Delim, []).

get_upto2([], _Delim, Acc) ->
    {lists:reverse(Acc), []};
get_upto2([#token{kind=Delim} | Rest], Delim, Acc) ->
    {lists:reverse(Acc), Rest};
get_upto2([T | Rest], Delim, Acc) ->
    get_upto2(Rest, Delim, [T | Acc]).

check_clause([#token{kind = ';'} | Rest], false) ->
    check_class(Rest) == function;
check_clause(_, _) ->
    false.

split_clauses(F) ->
    split_clauses(F, false, [], []).

split_clauses([], _HaveWhen, Acc, []) ->
    erlide_util:reverse2(Acc);
split_clauses([], HaveWhen, Acc, ClAcc) ->
    split_clauses([], HaveWhen, [ClAcc | Acc], []);
split_clauses([#token{kind=Kind}=T | Rest], _HaveWhen, Acc, ClAcc) 
  when Kind=:='end'; Kind=:=dot; Kind=:='->'; Kind=:='when' ->
    split_clauses(Rest, Kind=:='when', Acc, [T | ClAcc]);
split_clauses([T | Rest] = Tokens, HaveWhen, Acc, ClAcc) ->
    case check_clause(Tokens, HaveWhen) of
        false ->
            split_clauses(Rest, HaveWhen, Acc, [T | ClAcc]);
        true ->
            split_clauses(Rest, HaveWhen, [[T | ClAcc] | Acc], [])
    end.

%% fix_clause([#token{kind=atom, value=Name, line=Line, offset=Offset, length=Length} | Rest] = Code) ->
fix_clauses(Clauses) ->
    fix_clauses(Clauses, []).

fix_clauses([], Acc) ->
    lists:reverse(Acc);
fix_clauses([C | Rest], Acc) ->
    {Clause, Refs} = fix_clause(C),
    fix_clauses(Rest, [{Clause, Refs} | Acc]).

fix_clause([#token{kind=AtomOrMacro, value=Name, line=Line, offset=Offset, length=Length} | Rest]) 
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    #token{line=LastLine, offset=LastOffset, length=LastLength} = last_not_eof(Rest),
    PosLength = LastOffset - Offset + LastLength+1,
    ExternalRefs = get_refs_in_code(Rest),
    {#clause{pos={{Line, LastLine, Offset}, PosLength}, name_pos={{Line, Offset}, Length},
             name=Name, args=get_function_args(Rest), head=get_head(Rest)},
     ExternalRefs}.

get_function_args(Tokens) ->
    P = erlide_np_util:get_between_outer_pars(Tokens, '(', ')'),
    L = erlide_text:split_comma_list(P),
    [to_string(A) || A <- L].

fix_refs(ClausesAndRefs, Function, Imports) ->
    fix_refs(ClausesAndRefs, Function, Imports, []).

fix_refs([], #function{name=Name, arity=Arity, name_pos={{_, Offset}, Length}}, _Imports, Acc) ->
    Ref = #ref{data=#function_def{function=Name, arity=Arity}, offset=Offset, 
               length=Length, function=Name, arity=Arity, clause="", sub_clause=false},
    [Ref | Acc];
fix_refs([{Clause, Refs} | Rest], Function, Imports, Acc0) ->
    Acc1 = fix_refs(Refs, Clause, Function, Imports, Acc0),
    fix_refs(Rest, Function, Imports, Acc1).

fix_refs([], _Clause, _Function, _Imports, Acc) ->
    Acc;
fix_refs([{Offset, Length, RefData} | Rest], #clause{head=Head}=Clause, 
         #function{name=Name, arity=Arity, clauses=Clauses}=Function, 
         Imports, Acc) ->
    SubClause = case Clauses of
                    [] -> false;
                    [_] -> false;
                    _ -> true
                end,
    NewRefData = fix_imported_ref(RefData, Imports),
    Ref = #ref{data=NewRefData, offset=Offset, length=Length, function=Name, 
               arity=Arity, clause=Head, sub_clause=SubClause},
    fix_refs(Rest, Clause, Function, Imports, [Ref | Acc]).

fix_imported_ref(#local_call{function=F, arity=A}=RefData, [{Module, Imports} | Rest]) ->
    case lists:member({F, A}, Imports) of
        true -> #external_call{module=Module, function=F, arity=A};
        false -> fix_imported_ref(RefData, Rest)
    end;
fix_imported_ref(RefData, _Imports) ->
    RefData.

%% find references in code
%% VERY simple, M:F(_ ...), F(_ ...), #R, ?M, and that's it
%% returns {Offset, Length, Ref}, which is made into proper refs
%% by fix_refs
get_refs_in_code(L) ->
    get_refs_in_code(L, []).

get_refs_in_code([], Acc) ->
    lists:reverse(Acc);
get_refs_in_code([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
                  #token{kind=atom, value=F, offset=Offset2, length=Length2},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length2+Offset2-Offset, 
         #external_call{module = M, function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=atom, value=F, offset=Offset, length=Length},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length, #local_call{function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest], 
                 Acc) ->
    R = {Offset, Length, #macro_ref{macro=M}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind='#', offset=Offset}, 
                  #token{kind=atom, value=Record, offset=Offset2, length=Length2} | Rest],
                 Acc) ->
    R = {Offset, Length2+Offset2-Offset, #record_ref{record=Record}},
    {NewRest, Fields, RightSides} = erlide_np_records:check_fields(Rest, Record),
    NewAcc = get_refs_in_code(RightSides, Acc),
    get_refs_in_code(NewRest, Fields ++ [R | NewAcc]);
get_refs_in_code([#token{kind=var, value=V, offset=Offset, length=Length} | Rest],
                 Acc) ->
    R = {Offset, Length, make_var_def_ref(Acc, V)},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([_ | Rest], Acc) ->
    get_refs_in_code(Rest, Acc).

make_var_def_ref([], Var) ->
	#var_def{variable=Var};
make_var_def_ref([{_, _, #var_def{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([{_, _, #var_ref{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([_ | Rest], Var) ->
	make_var_def_ref(Rest, Var).

%% find type references in spec, type and opaque
%% also VERY simple, type(), ?M, #R, only
%% returns proper refs
get_refs(Tokens, Name, Arity) ->
	get_refs(Tokens, Name, Arity, []).

get_refs([], _, _, Acc) ->
    lists:reverse(Acc);
get_refs([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #macro_ref{macro=M}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind='#', offset=Offset},
          #token{kind=atom, value=R, offset=Offset2, length=Length2} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset, #record_ref{record=R}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
		  #token{kind=atom, value=T, offset=Offset2, length=Length2},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset,
                   #type_ref{module=M, type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=T, offset=Offset, length=Length},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #type_ref{module='_', type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=variable, value=V, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #var_ref{variable=V}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([_ | Rest], Name, Arity, Acc) ->
    get_refs(Rest, Name, Arity, Acc).

get_record_field_defs([], _) ->
    [];
get_record_field_defs([{FieldName, {{_, _, Offset}, Length}, _Extra} | Rest],
                      RecordName) ->
    [make_ref(Offset, Length, #record_field_def{record=RecordName, field=FieldName}, 
              FieldName, ?ARI_RECORD_FIELD_DEF)
         | get_record_field_defs(Rest, RecordName)].

make_ref(Offset, Length, Data, Name, Arity) ->
    #ref{data=Data, offset=Offset, length=Length, function=Name, arity=Arity, 
         clause="", sub_clause=false}.

make_attribute_arg_refs(define, Name, [#token{}, #token{kind='('} | Rest]) ->
    get_refs(skip_to_rparen(Rest), Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(define, Name, [#token{}, #token{kind=','} | Rest]) ->
    get_refs(Rest, Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(record, {Name, Fields}, [#token{}, #token{kind=','} | Rest]) ->
    get_record_field_defs(Fields, Name) ++ get_refs(Rest, Name, ?ARI_RECORD_DEF);
make_attribute_arg_refs(_, _, _) ->
    [].

skip_to_rparen([]) ->
    [];
skip_to_rparen([#token{kind=')'} | Rest]) ->
    Rest;
skip_to_rparen([_ | Rest]) ->
    skip_to_rparen(Rest).

make_attribute_ref(Name, Between, Extra, Offset, Length) ->
    ?D({Name, Between, Offset, Length}),
    case make_attribute_ref(Name, Between, Extra) of
        [] ->
            [];
        {Arity, AName, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=AName, 
                  arity=Arity, clause="", sub_clause=false}];
        {Arity, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=Name, 
                  arity=Arity, clause="", sub_clause=false}]
    end.

make_attribute_ref(module, _, Extra) ->
    {?ARI_ATTRIBUTE, #module_def{module=Extra}};
make_attribute_ref(record, Between, _E) ->
    ?D({Between, _E}),
    Name = case Between of
               {N, _} -> N;
               N -> N
           end,
    R= #record_def{record=Name},
    ?D(R),
    {?ARI_RECORD_DEF, Name, R};
make_attribute_ref(define, [Name | _], _) when is_list(Name) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(define, Name, _) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(include, _, Extra) ->
    {?ARI_INCLUDE, #include_ref{filename=unquote(Extra)}};
make_attribute_ref(_, _, _) ->
    [].

unquote(L) ->
    lists:reverse(unquote_first(lists:reverse(unquote_first(L)))).

unquote_first([$" | Rest]) ->
    Rest;
unquote_first(L) ->
    L.

remove_rest([], _) -> [];
remove_rest(Rest, Rest) -> [];
remove_rest([Hd | Tl], Rest) -> [Hd | remove_rest(Tl, Rest)].
%% Description: Split source files into functions and clauses and collects references
%% Author: jakob
%% Created: Mar 23, 2006

-module(erlide_noparse).

%%
%% Exported Functions
%%

%% called from Java
-export([initial_parse/5, reparse/1, remove_cache_files/2]).

%% called from Erlang
-export([read_module_refs/3]).

%%
%% Include files
%%

%% -define(DEBUG, 1).
%% -define(IO_FORMAT_DEBUG, 1).

-define(CACHE_VERSION, 25).
-define(SERVER, erlide_noparse).

-include("erlide.hrl").
-include("erlide_noparse.hrl").
-include("erlide_scanner.hrl").
-include("erlide_search_server.hrl").

%% special arity flags
-define(ARI_TYPESPEC, -2).
-define(ARI_ATTRIBUTE, -3).
-define(ARI_RECORD_DEF, -4).
-define(ARI_MACRO_DEF, -5).
-define(ARI_INCLUDE, -6).
-define(ARI_RECORD_FIELD_DEF, -7).

-define(X(X), erlide_log:log({X, erlang:now()})).


%%
%% API Functions
%%

initial_parse(ScannerName, ModuleFileName, StateDir, UseCache,
              UpdateSearchServer) ->
    try
        %%         ?D({StateDir, ModuleFileName}),
        BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
        RefsFileName = BaseName ++ ".refs",
        RenewFun = fun(_F) ->
                           do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer)
                   end,
        CacheFileName = BaseName ++ ".noparse",
        ?D(CacheFileName),
        {Cached, Res} = erlide_util:check_and_renew_cached(
                          ModuleFileName, CacheFileName, ?CACHE_VERSION, 
                          RenewFun, UseCache),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        ?D(updated),
        {ok, Res, Cached}
    catch
        error:Reason ->
            {error, Reason}
    end.

reparse(ScannerName) ->
    try
        Res = do_parse(ScannerName, "", "", true),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        {ok, Res, unused}
    catch
        error:Reason ->
            {error, Reason}
    end.

read_module_refs(ScannerName, ModulePath, StateDir) ->
    ?D(ScannerName),
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    RefsFileName = BaseName ++ ".refs",
    ?D(RefsFileName),
    case file:read_file(RefsFileName) of
        {ok, Binary} ->
            ?D(byte_size(Binary)),
            binary_to_term(Binary);
        _ ->
            ?D(ModulePath),
            {ok, InitialTextBin} = file:read_file(ModulePath),
            ?D(byte_size(InitialTextBin)),
            InitialText = binary_to_list(InitialTextBin),
            _D = erlide_scanner_server:initialScan(
                   ScannerName, ModulePath, InitialText, StateDir, true),
            ?D(_D),
            initial_parse(ScannerName, ModulePath, StateDir, true, false),
            ?D(RefsFileName),
            {ok, Binary} = file:read_file(RefsFileName),
            ?D(byte_size(Binary)),
            binary_to_term(Binary)
    end.

remove_cache_files(ScannerName, StateDir) ->
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    ScannerCacheFileName = BaseName ++ ".scan",
    file:delete(ScannerCacheFileName),
    RefsFileName = BaseName ++ ".refs",
    file:delete(RefsFileName),
    CacheFileName = BaseName ++ ".noparse",
    file:delete(CacheFileName).

%%
%% Internal functions
%%

do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer) ->
    Toks = erlide_scanner_server:getTokens(ScannerName),
    do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer).

do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer) ->
    ?D({do_parse, ScannerName, length(Toks)}),
    ?X(111),
    {UncommentToks, Comments} = erlide_np_util:extract_comments(Toks),
    ?X(222),
    ?D({length(UncommentToks), length(Comments)}),
    ?D({UncommentToks}),
    Functions = erlide_np_util:split_after_dots(UncommentToks),
    ?X(333),
    ?D(length(Functions)),
    AutoImports = erlide_util:add_auto_imported([]),
    ?X(444),
%%     ?D(AutoImports),
    {Collected, Refs} = classify_and_collect(Functions, [], [], [], AutoImports),
    ?X(555),
    ?D({'>>',length(Collected)}),
    CommentedCollected = erlide_np_util:get_function_comments(Collected, Comments),
    ?X(666),
    ?D(CommentedCollected),
    Model = #model{forms=CommentedCollected, comments=Comments},
    %%erlide_noparse_server:create(ScannerName, Model),
%%     ?D({"Model", length(Model#model.forms), erts_debug:flat_size(Model)}),
    FixedModel = fixup_model(Model),
    ?X(777),
%%     ?D(erts_debug:flat_size(FixedModel)),
    %% 	?D([erts_debug:flat_size(F) || F <- element(2, FixedModel)]),
    ?D(StateDir),
    case StateDir of
        "" -> ok;
        _ ->
            ?D({RefsFileName, length(Refs)}),
            file:write_file(RefsFileName, term_to_binary(Refs, [compressed]))
    end,
    update_search_server(UpdateSearchServer, ScannerName, Refs),
%%     ?D(FixedModel),
    ?X(888),
    FixedModel.

update_search_server(true, ScannerName, Refs) ->
    erlide_search_server:add_module_refs(ScannerName, Refs);
update_search_server(_, _, _) ->
    ok.

fixup_model(#model{forms=Forms, comments=Comments}) ->
    FixedComments = fixup_tokens(Comments),
    FixedForms = fixup_forms(Forms),
    #model{forms=FixedForms, comments=FixedComments}.

fixup_forms(Forms) ->
    [fixup_form(Form) || Form <- Forms].

fixup_tokens(Tokens) ->
    [fixup_token(Token) || Token <- Tokens].

fixup_token(#token{value=Value} = Token) when is_list(Value) ->
    Token#token{value=to_binary(Value)};
fixup_token(#token{text=Text} = Token) when is_list(Text) ->
    Token#token{value=to_binary(Text)};
fixup_token(Token) ->
    Token.

binary_args(Args) when is_list(Args) ->
    [iolist_to_binary(A) || A <- Args];
binary_args(_) ->
    [].

fixup_form(#function{comment=Comment, clauses=Clauses, args=Args} = Function) ->
    Function#function{comment= to_binary(Comment), clauses=fixup_forms(Clauses), args=binary_args(Args)};
fixup_form(#clause{head=Head, args=Args} = Clause) ->
    Clause#clause{head=to_binary(Head), args=binary_args(Args)};
fixup_form(Other) ->
    Other.

to_binary(Comment) when is_list(Comment) ->
    try 
        iolist_to_binary(Comment) 
    catch 
        _:_ -> 
            unicode:characters_to_binary(Comment) 
    end;
to_binary(Other) ->
    Other.

%% parse_test(ScannerName, File) ->
%%     erlide_scanner_server:scan_uncached(ScannerName, File, ""),
%%     Toks = erlide_scanner_server:getTokens(ScannerName),
%%     {UncommentToks, Comments} = extract_comments(Toks),
%%     Functions = split_after_dots(UncommentToks, [], []),
%%     {Collected, Refs} = classify_and_collect(Functions, [], []),
%%     _Model = #model{forms=Collected, comments=Comments},
%%     %%erlide_noparse_server:create(ScannerName, Model, ""),
%%     ok.

classify_and_collect([], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([eof], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([C | Rest], Acc, RefsAcc, Exports, Imports) ->
    {R, Refs, MoreExports, MoreImports} = cac(check_class(C), C, Exports, Imports),
    classify_and_collect(Rest, [R | Acc], Refs++RefsAcc, MoreExports++Exports, MoreImports++Imports).

cac(function, Tokens, Exports, Imports) ->
    ClauseList = split_clauses(Tokens),
    %?D(ClauseList),
    ClausesAndRefs = fix_clauses(ClauseList),
    %?D(length(Clauses)),
    [{#clause{pos=P, name=N, args=A, head=H, name_pos=NP}, _Refs} | _] = ClausesAndRefs,
    ?D(A),
    Arity = length(A),
    Exported = get_exported({N, Arity}, Exports),
    Function = case ClausesAndRefs of  	% only show subclauses when more than one
                   [_] ->
                       #function{pos=P, name=N, arity=Arity, args=A, head=H, 
                                 clauses=[], name_pos=NP, exported=Exported};
                   _ ->
                       Clauses = [C || {C, _} <- ClausesAndRefs],
                       #function{pos=P, name=N, arity=Arity, clauses=Clauses, 
                                 name_pos=NP, exported=Exported}
               end,
    {Function, fix_refs(ClausesAndRefs, Function, Imports), [], []};
cac(attribute, Attribute, _Exports, _Imports) ->
    ?D(Attribute),
    case Attribute of
        %% -spec, -type or -opaque
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=Kind, line=_Line, offset=_Offset, value=Name} | Args]
          when (Kind=:='spec') or ((Kind=:=atom) and (Name=:='type'))
                   or ((Kind=:=atom) and (Name=:='opaque'))->
            get_type_attribute(Kind, Name, Offset, Line, Attribute, Args);
        %% other attributes
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=atom, value=Name, line=_Line, offset=_Offset},
         _, #token{value=Args} | _] = Attribute ->
            get_other_attribute(Name, Offset, Line, Attribute, Args);
        [_, #token{kind=atom, value=Name, line=Line, offset=Offset} | _] ->
            #token{line=LastLine, offset=LastOffset, 
                   length=LastLength} = last_not_eof(Attribute),
            PosLength = LastOffset - Offset + LastLength,
            {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                       name=Name, args=[]}, [], [], []}
    end;
cac(other, [#token{value=Name, line=Line, offset=Offset, length=Length} | _], 
    _Exports, _Imports) ->
    {#other{pos={{Line, Line, Offset}, Length}, name=Name}, [], [], []};
cac(_, _D, _E, _I) ->
    {eof, [], [], []}.

get_type_attribute(Kind, Name0, Offset, Line, Attribute, Args) ->
    ?D(Name0),
    Name = case Kind of 'spec' -> Kind; _ -> Name0 end,
    #token{line=LastLine, offset=LastOffset,
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    ?D(Args),
    Extra = to_string(Args),
    ?D(Extra),
    {AttrArgs, _, _} = get_attribute_args(Kind, Args, Args),
    ?D({AttrArgs, Extra}),
    ExternalRefs = get_refs(tl(AttrArgs), Extra, ?ARI_TYPESPEC),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     [#ref{data=#type_def{type=Name}, offset=Offset, length=PosLength, function=Name, 
           arity=?ARI_TYPESPEC, clause="", sub_clause=false} | ExternalRefs], [], []}.

get_other_attribute(Name, Offset, Line, Attribute, Args) ->
    #token{line=LastLine, offset=LastOffset, 
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    Between = erlide_np_util:get_between_outer_pars(Attribute, '(', ')'),
    Extra = to_string(Between),
    {AttrArgs, Exports, Imports} = get_attribute_args(Name, Between, Args),
    ?D({AttrArgs, Between}),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     make_attribute_ref(Name, AttrArgs, Extra, Offset, PosLength)++
         make_attribute_arg_refs(Name, AttrArgs, Between),
     Exports, Imports}.

get_exported(F_A, Exports) ->
    lists:member(F_A, Exports).

get_attribute_args(import, Between, _Args) ->
    From = get_first_of_kind(atom, Between),
    Tokens = get_between(Between, '[', ']'),
    Import = {From, fun_arity_from_tokens(Tokens)},
    {Import, [], [Import]};
get_attribute_args(export, Between, _Args) ->
    Tokens = get_between(Between, '[', ']'),
    Exports = fun_arity_from_tokens(Tokens),
    {Exports, Exports, []};
get_attribute_args(record, Between, _Args) ->
    [RecordToken | _] = Between,
    RecordName = RecordToken#token.value,
    Tokens = erlide_np_util:get_between_outer_pars(Between, '{', '}'),
    R = {RecordName, field_list_from_tokens(Tokens)},
    {R, [], []};
get_attribute_args(_, _Between, Args) ->
    {Args, [], []}.

check_class([#token{kind = AtomOrMacro}, #token{kind = '('} | _])
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    function;
check_class([#token{kind = '-'}, #token{kind = atom} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'spec'} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'type'} | _]) ->
    attribute;
check_class(_X) ->
    %?D(lists:sublist(_X, 5)),
    other.

get_first_of_kind(Kind, Tokens) ->
    case erlide_np_util:skip_to(Tokens, Kind) of
        [#token{kind=Kind, value=Value} | _] ->
            Value;
        _ ->
            undefined
    end.
        
fun_arity_from_tokens([#token{kind=atom, value=Fun}, #token{kind='/'}, 
                       #token{kind=integer, value=Arity} | Rest]) ->
    [{Fun, Arity} | fun_arity_from_tokens(Rest)];
fun_arity_from_tokens([_ | Rest]) ->
    fun_arity_from_tokens(Rest);
fun_arity_from_tokens(_) ->
    [].

field_list_from_tokens([#token{kind=atom, value=Field, line=Line, 
                               offset=Offset, length=Length} | Rest]) ->
    {Extra, NewRest} = get_upto2(Rest, ','),
    [{Field, {{Line, Line, Offset}, Length}, to_string(Extra)}
         | field_list_from_tokens(NewRest)];
field_list_from_tokens([_ | Rest]) ->
    field_list_from_tokens(Rest);
field_list_from_tokens(_) ->
    [].

to_string(Tokens) ->
    S = erlide_scanner:tokens_to_string(Tokens),
    erlide_text:strip(S).
%%     unspacify(S).

get_head(T) ->
    A = get_args(T),
    case get_guards(T) of
        "" ->
            A;
        G ->
            A ++ " when " ++ G
    end.

get_args(T) ->
    case erlide_np_util:get_between_outer_pars(T, '(', ')') of
        [] ->
            "";
        P ->
            "("++to_string(P)++")"
    end.

get_guards([]) ->
    "";
get_guards(T) ->
    to_string(get_between(T, 'when', '->')). 

last_not_eof(L) -> 
    case lists:reverse(L) of
        [eof, Last | _] ->
            Last;
        [Last | _] ->
            Last;
        _ ->
            error
    end.    


get_between(L, A, B) ->
    R = get_upto(L, B),
    lists:reverse(get_upto(lists:reverse(R), A)).

get_upto(L, Delim) ->
        get_upto(L, Delim, []).

get_upto([], _Delim, _Acc) ->
    [];
get_upto([#token{kind=Delim} | _], Delim, Acc) ->
        lists:reverse(Acc);
get_upto([T | Rest], Delim, Acc) ->
    get_upto(Rest, Delim, [T | Acc]).

get_upto2(L, Delim) ->
    get_upto2(L, Delim, []).

get_upto2([], _Delim, Acc) ->
    {lists:reverse(Acc), []};
get_upto2([#token{kind=Delim} | Rest], Delim, Acc) ->
    {lists:reverse(Acc), Rest};
get_upto2([T | Rest], Delim, Acc) ->
    get_upto2(Rest, Delim, [T | Acc]).

check_clause([#token{kind = ';'} | Rest], false) ->
    check_class(Rest) == function;
check_clause(_, _) ->
    false.

split_clauses(F) ->
    split_clauses(F, false, [], []).

split_clauses([], _HaveWhen, Acc, []) ->
    erlide_util:reverse2(Acc);
split_clauses([], HaveWhen, Acc, ClAcc) ->
    split_clauses([], HaveWhen, [ClAcc | Acc], []);
split_clauses([#token{kind=Kind}=T | Rest], _HaveWhen, Acc, ClAcc) 
  when Kind=:='end'; Kind=:=dot; Kind=:='->'; Kind=:='when' ->
    split_clauses(Rest, Kind=:='when', Acc, [T | ClAcc]);
split_clauses([T | Rest] = Tokens, HaveWhen, Acc, ClAcc) ->
    case check_clause(Tokens, HaveWhen) of
        false ->
            split_clauses(Rest, HaveWhen, Acc, [T | ClAcc]);
        true ->
            split_clauses(Rest, HaveWhen, [[T | ClAcc] | Acc], [])
    end.

%% fix_clause([#token{kind=atom, value=Name, line=Line, offset=Offset, length=Length} | Rest] = Code) ->
fix_clauses(Clauses) ->
    fix_clauses(Clauses, []).

fix_clauses([], Acc) ->
    lists:reverse(Acc);
fix_clauses([C | Rest], Acc) ->
    {Clause, Refs} = fix_clause(C),
    fix_clauses(Rest, [{Clause, Refs} | Acc]).

fix_clause([#token{kind=AtomOrMacro, value=Name, line=Line, offset=Offset, length=Length} | Rest]) 
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    #token{line=LastLine, offset=LastOffset, length=LastLength} = last_not_eof(Rest),
    PosLength = LastOffset - Offset + LastLength+1,
    ExternalRefs = get_refs_in_code(Rest),
    {#clause{pos={{Line, LastLine, Offset}, PosLength}, name_pos={{Line, Offset}, Length},
             name=Name, args=get_function_args(Rest), head=get_head(Rest)},
     ExternalRefs}.

get_function_args(Tokens) ->
    P = erlide_np_util:get_between_outer_pars(Tokens, '(', ')'),
    L = erlide_text:split_comma_list(P),
    [to_string(A) || A <- L].

fix_refs(ClausesAndRefs, Function, Imports) ->
    fix_refs(ClausesAndRefs, Function, Imports, []).

fix_refs([], #function{name=Name, arity=Arity, name_pos={{_, Offset}, Length}}, _Imports, Acc) ->
    Ref = #ref{data=#function_def{function=Name, arity=Arity}, offset=Offset, 
               length=Length, function=Name, arity=Arity, clause="", sub_clause=false},
    [Ref | Acc];
fix_refs([{Clause, Refs} | Rest], Function, Imports, Acc0) ->
    Acc1 = fix_refs(Refs, Clause, Function, Imports, Acc0),
    fix_refs(Rest, Function, Imports, Acc1).

fix_refs([], _Clause, _Function, _Imports, Acc) ->
    Acc;
fix_refs([{Offset, Length, RefData} | Rest], #clause{head=Head}=Clause, 
         #function{name=Name, arity=Arity, clauses=Clauses}=Function, 
         Imports, Acc) ->
    SubClause = case Clauses of
                    [] -> false;
                    [_] -> false;
                    _ -> true
                end,
    NewRefData = fix_imported_ref(RefData, Imports),
    Ref = #ref{data=NewRefData, offset=Offset, length=Length, function=Name, 
               arity=Arity, clause=Head, sub_clause=SubClause},
    fix_refs(Rest, Clause, Function, Imports, [Ref | Acc]).

fix_imported_ref(#local_call{function=F, arity=A}=RefData, [{Module, Imports} | Rest]) ->
    case lists:member({F, A}, Imports) of
        true -> #external_call{module=Module, function=F, arity=A};
        false -> fix_imported_ref(RefData, Rest)
    end;
fix_imported_ref(RefData, _Imports) ->
    RefData.

%% find references in code
%% VERY simple, M:F(_ ...), F(_ ...), #R, ?M, and that's it
%% returns {Offset, Length, Ref}, which is made into proper refs
%% by fix_refs
get_refs_in_code(L) ->
    get_refs_in_code(L, []).

get_refs_in_code([], Acc) ->
    lists:reverse(Acc);
get_refs_in_code([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
                  #token{kind=atom, value=F, offset=Offset2, length=Length2},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length2+Offset2-Offset, 
         #external_call{module = M, function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=atom, value=F, offset=Offset, length=Length},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length, #local_call{function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest], 
                 Acc) ->
    R = {Offset, Length, #macro_ref{macro=M}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind='#', offset=Offset}, 
                  #token{kind=atom, value=Record, offset=Offset2, length=Length2} | Rest],
                 Acc) ->
    R = {Offset, Length2+Offset2-Offset, #record_ref{record=Record}},
    {NewRest, Fields, RightSides} = erlide_np_records:check_fields(Rest, Record),
    NewAcc = get_refs_in_code(RightSides, Acc),
    get_refs_in_code(NewRest, Fields ++ [R | NewAcc]);
get_refs_in_code([#token{kind=var, value=V, offset=Offset, length=Length} | Rest],
                 Acc) ->
    R = {Offset, Length, make_var_def_ref(Acc, V)},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([_ | Rest], Acc) ->
    get_refs_in_code(Rest, Acc).

make_var_def_ref([], Var) ->
	#var_def{variable=Var};
make_var_def_ref([{_, _, #var_def{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([{_, _, #var_ref{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([_ | Rest], Var) ->
	make_var_def_ref(Rest, Var).

%% find type references in spec, type and opaque
%% also VERY simple, type(), ?M, #R, only
%% returns proper refs
get_refs(Tokens, Name, Arity) ->
	get_refs(Tokens, Name, Arity, []).

get_refs([], _, _, Acc) ->
    lists:reverse(Acc);
get_refs([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #macro_ref{macro=M}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind='#', offset=Offset},
          #token{kind=atom, value=R, offset=Offset2, length=Length2} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset, #record_ref{record=R}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
		  #token{kind=atom, value=T, offset=Offset2, length=Length2},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset,
                   #type_ref{module=M, type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=T, offset=Offset, length=Length},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #type_ref{module='_', type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=variable, value=V, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #var_ref{variable=V}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([_ | Rest], Name, Arity, Acc) ->
    get_refs(Rest, Name, Arity, Acc).

get_record_field_defs([], _) ->
    [];
get_record_field_defs([{FieldName, {{_, _, Offset}, Length}, _Extra} | Rest],
                      RecordName) ->
    [make_ref(Offset, Length, #record_field_def{record=RecordName, field=FieldName}, 
              FieldName, ?ARI_RECORD_FIELD_DEF)
         | get_record_field_defs(Rest, RecordName)].

make_ref(Offset, Length, Data, Name, Arity) ->
    #ref{data=Data, offset=Offset, length=Length, function=Name, arity=Arity, 
         clause="", sub_clause=false}.

make_attribute_arg_refs(define, Name, [#token{}, #token{kind='('} | Rest]) ->
    get_refs(skip_to_rparen(Rest), Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(define, Name, [#token{}, #token{kind=','} | Rest]) ->
    get_refs(Rest, Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(record, {Name, Fields}, [#token{}, #token{kind=','} | Rest]) ->
    get_record_field_defs(Fields, Name) ++ get_refs(Rest, Name, ?ARI_RECORD_DEF);
make_attribute_arg_refs(_, _, _) ->
    [].

skip_to_rparen([]) ->
    [];
skip_to_rparen([#token{kind=')'} | Rest]) ->
    Rest;
skip_to_rparen([_ | Rest]) ->
    skip_to_rparen(Rest).

make_attribute_ref(Name, Between, Extra, Offset, Length) ->
    ?D({Name, Between, Offset, Length}),
    case make_attribute_ref(Name, Between, Extra) of
        [] ->
            [];
        {Arity, AName, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=AName, 
                  arity=Arity, clause="", sub_clause=false}];
        {Arity, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=Name, 
                  arity=Arity, clause="", sub_clause=false}]
    end.

make_attribute_ref(module, _, Extra) ->
    {?ARI_ATTRIBUTE, #module_def{module=Extra}};
make_attribute_ref(record, Between, _E) ->
    ?D({Between, _E}),
    Name = case Between of
               {N, _} -> N;
               N -> N
           end,
    R= #record_def{record=Name},
    ?D(R),
    {?ARI_RECORD_DEF, Name, R};
make_attribute_ref(define, [Name | _], _) when is_list(Name) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(define, Name, _) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(include, _, Extra) ->
    {?ARI_INCLUDE, #include_ref{filename=unquote(Extra)}};
make_attribute_ref(_, _, _) ->
    [].

unquote(L) ->
    lists:reverse(unquote_first(lists:reverse(unquote_first(L)))).

unquote_first([$" | Rest]) ->
    Rest;
unquote_first(L) ->
    L.

remove_rest([], _) -> [];
remove_rest(Rest, Rest) -> [];
remove_rest([Hd | Tl], Rest) -> [Hd | remove_rest(Tl, Rest)].
%% Description: Split source files into functions and clauses and collects references
%% Author: jakob
%% Created: Mar 23, 2006

-module(erlide_noparse).

%%
%% Exported Functions
%%

%% called from Java
-export([initial_parse/5, reparse/1, remove_cache_files/2]).

%% called from Erlang
-export([read_module_refs/3]).

%%
%% Include files
%%

%% -define(DEBUG, 1).
%% -define(IO_FORMAT_DEBUG, 1).

-define(CACHE_VERSION, 25).
-define(SERVER, erlide_noparse).

-include("erlide.hrl").
-include("erlide_noparse.hrl").
-include("erlide_scanner.hrl").
-include("erlide_search_server.hrl").

%% special arity flags
-define(ARI_TYPESPEC, -2).
-define(ARI_ATTRIBUTE, -3).
-define(ARI_RECORD_DEF, -4).
-define(ARI_MACRO_DEF, -5).
-define(ARI_INCLUDE, -6).
-define(ARI_RECORD_FIELD_DEF, -7).

-define(X(X), erlide_log:log({X, erlang:now()})).


%%
%% API Functions
%%

initial_parse(ScannerName, ModuleFileName, StateDir, UseCache,
              UpdateSearchServer) ->
    try
        %%         ?D({StateDir, ModuleFileName}),
        BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
        RefsFileName = BaseName ++ ".refs",
        RenewFun = fun(_F) ->
                           do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer)
                   end,
        CacheFileName = BaseName ++ ".noparse",
        ?D(CacheFileName),
        {Cached, Res} = erlide_util:check_and_renew_cached(
                          ModuleFileName, CacheFileName, ?CACHE_VERSION, 
                          RenewFun, UseCache),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        ?D(updated),
        {ok, Res, Cached}
    catch
        error:Reason ->
            {error, Reason}
    end.

reparse(ScannerName) ->
    try
        Res = do_parse(ScannerName, "", "", true),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        {ok, Res, unused}
    catch
        error:Reason ->
            {error, Reason}
    end.

read_module_refs(ScannerName, ModulePath, StateDir) ->
    ?D(ScannerName),
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    RefsFileName = BaseName ++ ".refs",
    ?D(RefsFileName),
    case file:read_file(RefsFileName) of
        {ok, Binary} ->
            ?D(byte_size(Binary)),
            binary_to_term(Binary);
        _ ->
            ?D(ModulePath),
            {ok, InitialTextBin} = file:read_file(ModulePath),
            ?D(byte_size(InitialTextBin)),
            InitialText = binary_to_list(InitialTextBin),
            _D = erlide_scanner_server:initialScan(
                   ScannerName, ModulePath, InitialText, StateDir, true),
            ?D(_D),
            initial_parse(ScannerName, ModulePath, StateDir, true, false),
            ?D(RefsFileName),
            {ok, Binary} = file:read_file(RefsFileName),
            ?D(byte_size(Binary)),
            binary_to_term(Binary)
    end.

remove_cache_files(ScannerName, StateDir) ->
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    ScannerCacheFileName = BaseName ++ ".scan",
    file:delete(ScannerCacheFileName),
    RefsFileName = BaseName ++ ".refs",
    file:delete(RefsFileName),
    CacheFileName = BaseName ++ ".noparse",
    file:delete(CacheFileName).

%%
%% Internal functions
%%

do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer) ->
    Toks = erlide_scanner_server:getTokens(ScannerName),
    do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer).

do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer) ->
    ?D({do_parse, ScannerName, length(Toks)}),
    ?X(111),
    {UncommentToks, Comments} = erlide_np_util:extract_comments(Toks),
    ?X(222),
    ?D({length(UncommentToks), length(Comments)}),
    ?D({UncommentToks}),
    Functions = erlide_np_util:split_after_dots(UncommentToks),
    ?X(333),
    ?D(length(Functions)),
    AutoImports = erlide_util:add_auto_imported([]),
    ?X(444),
%%     ?D(AutoImports),
    {Collected, Refs} = classify_and_collect(Functions, [], [], [], AutoImports),
    ?X(555),
    ?D({'>>',length(Collected)}),
    CommentedCollected = erlide_np_util:get_function_comments(Collected, Comments),
    ?X(666),
    ?D(CommentedCollected),
    Model = #model{forms=CommentedCollected, comments=Comments},
    %%erlide_noparse_server:create(ScannerName, Model),
%%     ?D({"Model", length(Model#model.forms), erts_debug:flat_size(Model)}),
    FixedModel = fixup_model(Model),
    ?X(777),
%%     ?D(erts_debug:flat_size(FixedModel)),
    %% 	?D([erts_debug:flat_size(F) || F <- element(2, FixedModel)]),
    ?D(StateDir),
    case StateDir of
        "" -> ok;
        _ ->
            ?D({RefsFileName, length(Refs)}),
            file:write_file(RefsFileName, term_to_binary(Refs, [compressed]))
    end,
    update_search_server(UpdateSearchServer, ScannerName, Refs),
%%     ?D(FixedModel),
    ?X(888),
    FixedModel.

update_search_server(true, ScannerName, Refs) ->
    erlide_search_server:add_module_refs(ScannerName, Refs);
update_search_server(_, _, _) ->
    ok.

fixup_model(#model{forms=Forms, comments=Comments}) ->
    FixedComments = fixup_tokens(Comments),
    FixedForms = fixup_forms(Forms),
    #model{forms=FixedForms, comments=FixedComments}.

fixup_forms(Forms) ->
    [fixup_form(Form) || Form <- Forms].

fixup_tokens(Tokens) ->
    [fixup_token(Token) || Token <- Tokens].

fixup_token(#token{value=Value} = Token) when is_list(Value) ->
    Token#token{value=to_binary(Value)};
fixup_token(#token{text=Text} = Token) when is_list(Text) ->
    Token#token{value=to_binary(Text)};
fixup_token(Token) ->
    Token.

binary_args(Args) when is_list(Args) ->
    [iolist_to_binary(A) || A <- Args];
binary_args(_) ->
    [].

fixup_form(#function{comment=Comment, clauses=Clauses, args=Args} = Function) ->
    Function#function{comment= to_binary(Comment), clauses=fixup_forms(Clauses), args=binary_args(Args)};
fixup_form(#clause{head=Head, args=Args} = Clause) ->
    Clause#clause{head=to_binary(Head), args=binary_args(Args)};
fixup_form(Other) ->
    Other.

to_binary(Comment) when is_list(Comment) ->
    try 
        iolist_to_binary(Comment) 
    catch 
        _:_ -> 
            unicode:characters_to_binary(Comment) 
    end;
to_binary(Other) ->
    Other.

%% parse_test(ScannerName, File) ->
%%     erlide_scanner_server:scan_uncached(ScannerName, File, ""),
%%     Toks = erlide_scanner_server:getTokens(ScannerName),
%%     {UncommentToks, Comments} = extract_comments(Toks),
%%     Functions = split_after_dots(UncommentToks, [], []),
%%     {Collected, Refs} = classify_and_collect(Functions, [], []),
%%     _Model = #model{forms=Collected, comments=Comments},
%%     %%erlide_noparse_server:create(ScannerName, Model, ""),
%%     ok.

classify_and_collect([], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([eof], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([C | Rest], Acc, RefsAcc, Exports, Imports) ->
    {R, Refs, MoreExports, MoreImports} = cac(check_class(C), C, Exports, Imports),
    classify_and_collect(Rest, [R | Acc], Refs++RefsAcc, MoreExports++Exports, MoreImports++Imports).

cac(function, Tokens, Exports, Imports) ->
    ClauseList = split_clauses(Tokens),
    %?D(ClauseList),
    ClausesAndRefs = fix_clauses(ClauseList),
    %?D(length(Clauses)),
    [{#clause{pos=P, name=N, args=A, head=H, name_pos=NP}, _Refs} | _] = ClausesAndRefs,
    ?D(A),
    Arity = length(A),
    Exported = get_exported({N, Arity}, Exports),
    Function = case ClausesAndRefs of  	% only show subclauses when more than one
                   [_] ->
                       #function{pos=P, name=N, arity=Arity, args=A, head=H, 
                                 clauses=[], name_pos=NP, exported=Exported};
                   _ ->
                       Clauses = [C || {C, _} <- ClausesAndRefs],
                       #function{pos=P, name=N, arity=Arity, clauses=Clauses, 
                                 name_pos=NP, exported=Exported}
               end,
    {Function, fix_refs(ClausesAndRefs, Function, Imports), [], []};
cac(attribute, Attribute, _Exports, _Imports) ->
    ?D(Attribute),
    case Attribute of
        %% -spec, -type or -opaque
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=Kind, line=_Line, offset=_Offset, value=Name} | Args]
          when (Kind=:='spec') or ((Kind=:=atom) and (Name=:='type'))
                   or ((Kind=:=atom) and (Name=:='opaque'))->
            get_type_attribute(Kind, Name, Offset, Line, Attribute, Args);
        %% other attributes
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=atom, value=Name, line=_Line, offset=_Offset},
         _, #token{value=Args} | _] = Attribute ->
            get_other_attribute(Name, Offset, Line, Attribute, Args);
        [_, #token{kind=atom, value=Name, line=Line, offset=Offset} | _] ->
            #token{line=LastLine, offset=LastOffset, 
                   length=LastLength} = last_not_eof(Attribute),
            PosLength = LastOffset - Offset + LastLength,
            {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                       name=Name, args=[]}, [], [], []}
    end;
cac(other, [#token{value=Name, line=Line, offset=Offset, length=Length} | _], 
    _Exports, _Imports) ->
    {#other{pos={{Line, Line, Offset}, Length}, name=Name}, [], [], []};
cac(_, _D, _E, _I) ->
    {eof, [], [], []}.

get_type_attribute(Kind, Name0, Offset, Line, Attribute, Args) ->
    ?D(Name0),
    Name = case Kind of 'spec' -> Kind; _ -> Name0 end,
    #token{line=LastLine, offset=LastOffset,
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    ?D(Args),
    Extra = to_string(Args),
    ?D(Extra),
    {AttrArgs, _, _} = get_attribute_args(Kind, Args, Args),
    ?D({AttrArgs, Extra}),
    ExternalRefs = get_refs(tl(AttrArgs), Extra, ?ARI_TYPESPEC),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     [#ref{data=#type_def{type=Name}, offset=Offset, length=PosLength, function=Name, 
           arity=?ARI_TYPESPEC, clause="", sub_clause=false} | ExternalRefs], [], []}.

get_other_attribute(Name, Offset, Line, Attribute, Args) ->
    #token{line=LastLine, offset=LastOffset, 
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    Between = erlide_np_util:get_between_outer_pars(Attribute, '(', ')'),
    Extra = to_string(Between),
    {AttrArgs, Exports, Imports} = get_attribute_args(Name, Between, Args),
    ?D({AttrArgs, Between}),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     make_attribute_ref(Name, AttrArgs, Extra, Offset, PosLength)++
         make_attribute_arg_refs(Name, AttrArgs, Between),
     Exports, Imports}.

get_exported(F_A, Exports) ->
    lists:member(F_A, Exports).

get_attribute_args(import, Between, _Args) ->
    From = get_first_of_kind(atom, Between),
    Tokens = get_between(Between, '[', ']'),
    Import = {From, fun_arity_from_tokens(Tokens)},
    {Import, [], [Import]};
get_attribute_args(export, Between, _Args) ->
    Tokens = get_between(Between, '[', ']'),
    Exports = fun_arity_from_tokens(Tokens),
    {Exports, Exports, []};
get_attribute_args(record, Between, _Args) ->
    [RecordToken | _] = Between,
    RecordName = RecordToken#token.value,
    Tokens = erlide_np_util:get_between_outer_pars(Between, '{', '}'),
    R = {RecordName, field_list_from_tokens(Tokens)},
    {R, [], []};
get_attribute_args(_, _Between, Args) ->
    {Args, [], []}.

check_class([#token{kind = AtomOrMacro}, #token{kind = '('} | _])
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    function;
check_class([#token{kind = '-'}, #token{kind = atom} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'spec'} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'type'} | _]) ->
    attribute;
check_class(_X) ->
    %?D(lists:sublist(_X, 5)),
    other.

get_first_of_kind(Kind, Tokens) ->
    case erlide_np_util:skip_to(Tokens, Kind) of
        [#token{kind=Kind, value=Value} | _] ->
            Value;
        _ ->
            undefined
    end.
        
fun_arity_from_tokens([#token{kind=atom, value=Fun}, #token{kind='/'}, 
                       #token{kind=integer, value=Arity} | Rest]) ->
    [{Fun, Arity} | fun_arity_from_tokens(Rest)];
fun_arity_from_tokens([_ | Rest]) ->
    fun_arity_from_tokens(Rest);
fun_arity_from_tokens(_) ->
    [].

field_list_from_tokens([#token{kind=atom, value=Field, line=Line, 
                               offset=Offset, length=Length} | Rest]) ->
    {Extra, NewRest} = get_upto2(Rest, ','),
    [{Field, {{Line, Line, Offset}, Length}, to_string(Extra)}
         | field_list_from_tokens(NewRest)];
field_list_from_tokens([_ | Rest]) ->
    field_list_from_tokens(Rest);
field_list_from_tokens(_) ->
    [].

to_string(Tokens) ->
    S = erlide_scanner:tokens_to_string(Tokens),
    erlide_text:strip(S).
%%     unspacify(S).

get_head(T) ->
    A = get_args(T),
    case get_guards(T) of
        "" ->
            A;
        G ->
            A ++ " when " ++ G
    end.

get_args(T) ->
    case erlide_np_util:get_between_outer_pars(T, '(', ')') of
        [] ->
            "";
        P ->
            "("++to_string(P)++")"
    end.

get_guards([]) ->
    "";
get_guards(T) ->
    to_string(get_between(T, 'when', '->')). 

last_not_eof(L) -> 
    case lists:reverse(L) of
        [eof, Last | _] ->
            Last;
        [Last | _] ->
            Last;
        _ ->
            error
    end.    


get_between(L, A, B) ->
    R = get_upto(L, B),
    lists:reverse(get_upto(lists:reverse(R), A)).

get_upto(L, Delim) ->
        get_upto(L, Delim, []).

get_upto([], _Delim, _Acc) ->
    [];
get_upto([#token{kind=Delim} | _], Delim, Acc) ->
        lists:reverse(Acc);
get_upto([T | Rest], Delim, Acc) ->
    get_upto(Rest, Delim, [T | Acc]).

get_upto2(L, Delim) ->
    get_upto2(L, Delim, []).

get_upto2([], _Delim, Acc) ->
    {lists:reverse(Acc), []};
get_upto2([#token{kind=Delim} | Rest], Delim, Acc) ->
    {lists:reverse(Acc), Rest};
get_upto2([T | Rest], Delim, Acc) ->
    get_upto2(Rest, Delim, [T | Acc]).

check_clause([#token{kind = ';'} | Rest], false) ->
    check_class(Rest) == function;
check_clause(_, _) ->
    false.

split_clauses(F) ->
    split_clauses(F, false, [], []).

split_clauses([], _HaveWhen, Acc, []) ->
    erlide_util:reverse2(Acc);
split_clauses([], HaveWhen, Acc, ClAcc) ->
    split_clauses([], HaveWhen, [ClAcc | Acc], []);
split_clauses([#token{kind=Kind}=T | Rest], _HaveWhen, Acc, ClAcc) 
  when Kind=:='end'; Kind=:=dot; Kind=:='->'; Kind=:='when' ->
    split_clauses(Rest, Kind=:='when', Acc, [T | ClAcc]);
split_clauses([T | Rest] = Tokens, HaveWhen, Acc, ClAcc) ->
    case check_clause(Tokens, HaveWhen) of
        false ->
            split_clauses(Rest, HaveWhen, Acc, [T | ClAcc]);
        true ->
            split_clauses(Rest, HaveWhen, [[T | ClAcc] | Acc], [])
    end.

%% fix_clause([#token{kind=atom, value=Name, line=Line, offset=Offset, length=Length} | Rest] = Code) ->
fix_clauses(Clauses) ->
    fix_clauses(Clauses, []).

fix_clauses([], Acc) ->
    lists:reverse(Acc);
fix_clauses([C | Rest], Acc) ->
    {Clause, Refs} = fix_clause(C),
    fix_clauses(Rest, [{Clause, Refs} | Acc]).

fix_clause([#token{kind=AtomOrMacro, value=Name, line=Line, offset=Offset, length=Length} | Rest]) 
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    #token{line=LastLine, offset=LastOffset, length=LastLength} = last_not_eof(Rest),
    PosLength = LastOffset - Offset + LastLength+1,
    ExternalRefs = get_refs_in_code(Rest),
    {#clause{pos={{Line, LastLine, Offset}, PosLength}, name_pos={{Line, Offset}, Length},
             name=Name, args=get_function_args(Rest), head=get_head(Rest)},
     ExternalRefs}.

get_function_args(Tokens) ->
    P = erlide_np_util:get_between_outer_pars(Tokens, '(', ')'),
    L = erlide_text:split_comma_list(P),
    [to_string(A) || A <- L].

fix_refs(ClausesAndRefs, Function, Imports) ->
    fix_refs(ClausesAndRefs, Function, Imports, []).

fix_refs([], #function{name=Name, arity=Arity, name_pos={{_, Offset}, Length}}, _Imports, Acc) ->
    Ref = #ref{data=#function_def{function=Name, arity=Arity}, offset=Offset, 
               length=Length, function=Name, arity=Arity, clause="", sub_clause=false},
    [Ref | Acc];
fix_refs([{Clause, Refs} | Rest], Function, Imports, Acc0) ->
    Acc1 = fix_refs(Refs, Clause, Function, Imports, Acc0),
    fix_refs(Rest, Function, Imports, Acc1).

fix_refs([], _Clause, _Function, _Imports, Acc) ->
    Acc;
fix_refs([{Offset, Length, RefData} | Rest], #clause{head=Head}=Clause, 
         #function{name=Name, arity=Arity, clauses=Clauses}=Function, 
         Imports, Acc) ->
    SubClause = case Clauses of
                    [] -> false;
                    [_] -> false;
                    _ -> true
                end,
    NewRefData = fix_imported_ref(RefData, Imports),
    Ref = #ref{data=NewRefData, offset=Offset, length=Length, function=Name, 
               arity=Arity, clause=Head, sub_clause=SubClause},
    fix_refs(Rest, Clause, Function, Imports, [Ref | Acc]).

fix_imported_ref(#local_call{function=F, arity=A}=RefData, [{Module, Imports} | Rest]) ->
    case lists:member({F, A}, Imports) of
        true -> #external_call{module=Module, function=F, arity=A};
        false -> fix_imported_ref(RefData, Rest)
    end;
fix_imported_ref(RefData, _Imports) ->
    RefData.

%% find references in code
%% VERY simple, M:F(_ ...), F(_ ...), #R, ?M, and that's it
%% returns {Offset, Length, Ref}, which is made into proper refs
%% by fix_refs
get_refs_in_code(L) ->
    get_refs_in_code(L, []).

get_refs_in_code([], Acc) ->
    lists:reverse(Acc);
get_refs_in_code([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
                  #token{kind=atom, value=F, offset=Offset2, length=Length2},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length2+Offset2-Offset, 
         #external_call{module = M, function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=atom, value=F, offset=Offset, length=Length},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length, #local_call{function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest], 
                 Acc) ->
    R = {Offset, Length, #macro_ref{macro=M}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind='#', offset=Offset}, 
                  #token{kind=atom, value=Record, offset=Offset2, length=Length2} | Rest],
                 Acc) ->
    R = {Offset, Length2+Offset2-Offset, #record_ref{record=Record}},
    {NewRest, Fields, RightSides} = erlide_np_records:check_fields(Rest, Record),
    NewAcc = get_refs_in_code(RightSides, Acc),
    get_refs_in_code(NewRest, Fields ++ [R | NewAcc]);
get_refs_in_code([#token{kind=var, value=V, offset=Offset, length=Length} | Rest],
                 Acc) ->
    R = {Offset, Length, make_var_def_ref(Acc, V)},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([_ | Rest], Acc) ->
    get_refs_in_code(Rest, Acc).

make_var_def_ref([], Var) ->
	#var_def{variable=Var};
make_var_def_ref([{_, _, #var_def{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([{_, _, #var_ref{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([_ | Rest], Var) ->
	make_var_def_ref(Rest, Var).

%% find type references in spec, type and opaque
%% also VERY simple, type(), ?M, #R, only
%% returns proper refs
get_refs(Tokens, Name, Arity) ->
	get_refs(Tokens, Name, Arity, []).

get_refs([], _, _, Acc) ->
    lists:reverse(Acc);
get_refs([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #macro_ref{macro=M}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind='#', offset=Offset},
          #token{kind=atom, value=R, offset=Offset2, length=Length2} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset, #record_ref{record=R}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
		  #token{kind=atom, value=T, offset=Offset2, length=Length2},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset,
                   #type_ref{module=M, type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=T, offset=Offset, length=Length},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #type_ref{module='_', type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=variable, value=V, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #var_ref{variable=V}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([_ | Rest], Name, Arity, Acc) ->
    get_refs(Rest, Name, Arity, Acc).

get_record_field_defs([], _) ->
    [];
get_record_field_defs([{FieldName, {{_, _, Offset}, Length}, _Extra} | Rest],
                      RecordName) ->
    [make_ref(Offset, Length, #record_field_def{record=RecordName, field=FieldName}, 
              FieldName, ?ARI_RECORD_FIELD_DEF)
         | get_record_field_defs(Rest, RecordName)].

make_ref(Offset, Length, Data, Name, Arity) ->
    #ref{data=Data, offset=Offset, length=Length, function=Name, arity=Arity, 
         clause="", sub_clause=false}.

make_attribute_arg_refs(define, Name, [#token{}, #token{kind='('} | Rest]) ->
    get_refs(skip_to_rparen(Rest), Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(define, Name, [#token{}, #token{kind=','} | Rest]) ->
    get_refs(Rest, Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(record, {Name, Fields}, [#token{}, #token{kind=','} | Rest]) ->
    get_record_field_defs(Fields, Name) ++ get_refs(Rest, Name, ?ARI_RECORD_DEF);
make_attribute_arg_refs(_, _, _) ->
    [].

skip_to_rparen([]) ->
    [];
skip_to_rparen([#token{kind=')'} | Rest]) ->
    Rest;
skip_to_rparen([_ | Rest]) ->
    skip_to_rparen(Rest).

make_attribute_ref(Name, Between, Extra, Offset, Length) ->
    ?D({Name, Between, Offset, Length}),
    case make_attribute_ref(Name, Between, Extra) of
        [] ->
            [];
        {Arity, AName, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=AName, 
                  arity=Arity, clause="", sub_clause=false}];
        {Arity, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=Name, 
                  arity=Arity, clause="", sub_clause=false}]
    end.

make_attribute_ref(module, _, Extra) ->
    {?ARI_ATTRIBUTE, #module_def{module=Extra}};
make_attribute_ref(record, Between, _E) ->
    ?D({Between, _E}),
    Name = case Between of
               {N, _} -> N;
               N -> N
           end,
    R= #record_def{record=Name},
    ?D(R),
    {?ARI_RECORD_DEF, Name, R};
make_attribute_ref(define, [Name | _], _) when is_list(Name) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(define, Name, _) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(include, _, Extra) ->
    {?ARI_INCLUDE, #include_ref{filename=unquote(Extra)}};
make_attribute_ref(_, _, _) ->
    [].

unquote(L) ->
    lists:reverse(unquote_first(lists:reverse(unquote_first(L)))).

unquote_first([$" | Rest]) ->
    Rest;
unquote_first(L) ->
    L.

remove_rest([], _) -> [];
remove_rest(Rest, Rest) -> [];
remove_rest([Hd | Tl], Rest) -> [Hd | remove_rest(Tl, Rest)].
%% Description: Split source files into functions and clauses and collects references
%% Author: jakob
%% Created: Mar 23, 2006

-module(erlide_noparse).

%%
%% Exported Functions
%%

%% called from Java
-export([initial_parse/5, reparse/1, remove_cache_files/2]).

%% called from Erlang
-export([read_module_refs/3]).

%%
%% Include files
%%

%% -define(DEBUG, 1).
%% -define(IO_FORMAT_DEBUG, 1).

-define(CACHE_VERSION, 25).
-define(SERVER, erlide_noparse).

-include("erlide.hrl").
-include("erlide_noparse.hrl").
-include("erlide_scanner.hrl").
-include("erlide_search_server.hrl").

%% special arity flags
-define(ARI_TYPESPEC, -2).
-define(ARI_ATTRIBUTE, -3).
-define(ARI_RECORD_DEF, -4).
-define(ARI_MACRO_DEF, -5).
-define(ARI_INCLUDE, -6).
-define(ARI_RECORD_FIELD_DEF, -7).

-define(X(X), erlide_log:log({X, erlang:now()})).


%%
%% API Functions
%%

initial_parse(ScannerName, ModuleFileName, StateDir, UseCache,
              UpdateSearchServer) ->
    try
        %%         ?D({StateDir, ModuleFileName}),
        BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
        RefsFileName = BaseName ++ ".refs",
        RenewFun = fun(_F) ->
                           do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer)
                   end,
        CacheFileName = BaseName ++ ".noparse",
        ?D(CacheFileName),
        {Cached, Res} = erlide_util:check_and_renew_cached(
                          ModuleFileName, CacheFileName, ?CACHE_VERSION, 
                          RenewFun, UseCache),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        ?D(updated),
        {ok, Res, Cached}
    catch
        error:Reason ->
            {error, Reason}
    end.

reparse(ScannerName) ->
    try
        Res = do_parse(ScannerName, "", "", true),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        {ok, Res, unused}
    catch
        error:Reason ->
            {error, Reason}
    end.

read_module_refs(ScannerName, ModulePath, StateDir) ->
    ?D(ScannerName),
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    RefsFileName = BaseName ++ ".refs",
    ?D(RefsFileName),
    case file:read_file(RefsFileName) of
        {ok, Binary} ->
            ?D(byte_size(Binary)),
            binary_to_term(Binary);
        _ ->
            ?D(ModulePath),
            {ok, InitialTextBin} = file:read_file(ModulePath),
            ?D(byte_size(InitialTextBin)),
            InitialText = binary_to_list(InitialTextBin),
            _D = erlide_scanner_server:initialScan(
                   ScannerName, ModulePath, InitialText, StateDir, true),
            ?D(_D),
            initial_parse(ScannerName, ModulePath, StateDir, true, false),
            ?D(RefsFileName),
            {ok, Binary} = file:read_file(RefsFileName),
            ?D(byte_size(Binary)),
            binary_to_term(Binary)
    end.

remove_cache_files(ScannerName, StateDir) ->
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    ScannerCacheFileName = BaseName ++ ".scan",
    file:delete(ScannerCacheFileName),
    RefsFileName = BaseName ++ ".refs",
    file:delete(RefsFileName),
    CacheFileName = BaseName ++ ".noparse",
    file:delete(CacheFileName).

%%
%% Internal functions
%%

do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer) ->
    Toks = erlide_scanner_server:getTokens(ScannerName),
    do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer).

do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer) ->
    ?D({do_parse, ScannerName, length(Toks)}),
    ?X(111),
    {UncommentToks, Comments} = erlide_np_util:extract_comments(Toks),
    ?X(222),
    ?D({length(UncommentToks), length(Comments)}),
    ?D({UncommentToks}),
    Functions = erlide_np_util:split_after_dots(UncommentToks),
    ?X(333),
    ?D(length(Functions)),
    AutoImports = erlide_util:add_auto_imported([]),
    ?X(444),
%%     ?D(AutoImports),
    {Collected, Refs} = classify_and_collect(Functions, [], [], [], AutoImports),
    ?X(555),
    ?D({'>>',length(Collected)}),
    CommentedCollected = erlide_np_util:get_function_comments(Collected, Comments),
    ?X(666),
    ?D(CommentedCollected),
    Model = #model{forms=CommentedCollected, comments=Comments},
    %%erlide_noparse_server:create(ScannerName, Model),
%%     ?D({"Model", length(Model#model.forms), erts_debug:flat_size(Model)}),
    FixedModel = fixup_model(Model),
    ?X(777),
%%     ?D(erts_debug:flat_size(FixedModel)),
    %% 	?D([erts_debug:flat_size(F) || F <- element(2, FixedModel)]),
    ?D(StateDir),
    case StateDir of
        "" -> ok;
        _ ->
            ?D({RefsFileName, length(Refs)}),
            file:write_file(RefsFileName, term_to_binary(Refs, [compressed]))
    end,
    update_search_server(UpdateSearchServer, ScannerName, Refs),
%%     ?D(FixedModel),
    ?X(888),
    FixedModel.

update_search_server(true, ScannerName, Refs) ->
    erlide_search_server:add_module_refs(ScannerName, Refs);
update_search_server(_, _, _) ->
    ok.

fixup_model(#model{forms=Forms, comments=Comments}) ->
    FixedComments = fixup_tokens(Comments),
    FixedForms = fixup_forms(Forms),
    #model{forms=FixedForms, comments=FixedComments}.

fixup_forms(Forms) ->
    [fixup_form(Form) || Form <- Forms].

fixup_tokens(Tokens) ->
    [fixup_token(Token) || Token <- Tokens].

fixup_token(#token{value=Value} = Token) when is_list(Value) ->
    Token#token{value=to_binary(Value)};
fixup_token(#token{text=Text} = Token) when is_list(Text) ->
    Token#token{value=to_binary(Text)};
fixup_token(Token) ->
    Token.

binary_args(Args) when is_list(Args) ->
    [iolist_to_binary(A) || A <- Args];
binary_args(_) ->
    [].

fixup_form(#function{comment=Comment, clauses=Clauses, args=Args} = Function) ->
    Function#function{comment= to_binary(Comment), clauses=fixup_forms(Clauses), args=binary_args(Args)};
fixup_form(#clause{head=Head, args=Args} = Clause) ->
    Clause#clause{head=to_binary(Head), args=binary_args(Args)};
fixup_form(Other) ->
    Other.

to_binary(Comment) when is_list(Comment) ->
    try 
        iolist_to_binary(Comment) 
    catch 
        _:_ -> 
            unicode:characters_to_binary(Comment) 
    end;
to_binary(Other) ->
    Other.

%% parse_test(ScannerName, File) ->
%%     erlide_scanner_server:scan_uncached(ScannerName, File, ""),
%%     Toks = erlide_scanner_server:getTokens(ScannerName),
%%     {UncommentToks, Comments} = extract_comments(Toks),
%%     Functions = split_after_dots(UncommentToks, [], []),
%%     {Collected, Refs} = classify_and_collect(Functions, [], []),
%%     _Model = #model{forms=Collected, comments=Comments},
%%     %%erlide_noparse_server:create(ScannerName, Model, ""),
%%     ok.

classify_and_collect([], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([eof], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([C | Rest], Acc, RefsAcc, Exports, Imports) ->
    {R, Refs, MoreExports, MoreImports} = cac(check_class(C), C, Exports, Imports),
    classify_and_collect(Rest, [R | Acc], Refs++RefsAcc, MoreExports++Exports, MoreImports++Imports).

cac(function, Tokens, Exports, Imports) ->
    ClauseList = split_clauses(Tokens),
    %?D(ClauseList),
    ClausesAndRefs = fix_clauses(ClauseList),
    %?D(length(Clauses)),
    [{#clause{pos=P, name=N, args=A, head=H, name_pos=NP}, _Refs} | _] = ClausesAndRefs,
    ?D(A),
    Arity = length(A),
    Exported = get_exported({N, Arity}, Exports),
    Function = case ClausesAndRefs of  	% only show subclauses when more than one
                   [_] ->
                       #function{pos=P, name=N, arity=Arity, args=A, head=H, 
                                 clauses=[], name_pos=NP, exported=Exported};
                   _ ->
                       Clauses = [C || {C, _} <- ClausesAndRefs],
                       #function{pos=P, name=N, arity=Arity, clauses=Clauses, 
                                 name_pos=NP, exported=Exported}
               end,
    {Function, fix_refs(ClausesAndRefs, Function, Imports), [], []};
cac(attribute, Attribute, _Exports, _Imports) ->
    ?D(Attribute),
    case Attribute of
        %% -spec, -type or -opaque
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=Kind, line=_Line, offset=_Offset, value=Name} | Args]
          when (Kind=:='spec') or ((Kind=:=atom) and (Name=:='type'))
                   or ((Kind=:=atom) and (Name=:='opaque'))->
            get_type_attribute(Kind, Name, Offset, Line, Attribute, Args);
        %% other attributes
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=atom, value=Name, line=_Line, offset=_Offset},
         _, #token{value=Args} | _] = Attribute ->
            get_other_attribute(Name, Offset, Line, Attribute, Args);
        [_, #token{kind=atom, value=Name, line=Line, offset=Offset} | _] ->
            #token{line=LastLine, offset=LastOffset, 
                   length=LastLength} = last_not_eof(Attribute),
            PosLength = LastOffset - Offset + LastLength,
            {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                       name=Name, args=[]}, [], [], []}
    end;
cac(other, [#token{value=Name, line=Line, offset=Offset, length=Length} | _], 
    _Exports, _Imports) ->
    {#other{pos={{Line, Line, Offset}, Length}, name=Name}, [], [], []};
cac(_, _D, _E, _I) ->
    {eof, [], [], []}.

get_type_attribute(Kind, Name0, Offset, Line, Attribute, Args) ->
    ?D(Name0),
    Name = case Kind of 'spec' -> Kind; _ -> Name0 end,
    #token{line=LastLine, offset=LastOffset,
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    ?D(Args),
    Extra = to_string(Args),
    ?D(Extra),
    {AttrArgs, _, _} = get_attribute_args(Kind, Args, Args),
    ?D({AttrArgs, Extra}),
    ExternalRefs = get_refs(tl(AttrArgs), Extra, ?ARI_TYPESPEC),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     [#ref{data=#type_def{type=Name}, offset=Offset, length=PosLength, function=Name, 
           arity=?ARI_TYPESPEC, clause="", sub_clause=false} | ExternalRefs], [], []}.

get_other_attribute(Name, Offset, Line, Attribute, Args) ->
    #token{line=LastLine, offset=LastOffset, 
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    Between = erlide_np_util:get_between_outer_pars(Attribute, '(', ')'),
    Extra = to_string(Between),
    {AttrArgs, Exports, Imports} = get_attribute_args(Name, Between, Args),
    ?D({AttrArgs, Between}),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     make_attribute_ref(Name, AttrArgs, Extra, Offset, PosLength)++
         make_attribute_arg_refs(Name, AttrArgs, Between),
     Exports, Imports}.

get_exported(F_A, Exports) ->
    lists:member(F_A, Exports).

get_attribute_args(import, Between, _Args) ->
    From = get_first_of_kind(atom, Between),
    Tokens = get_between(Between, '[', ']'),
    Import = {From, fun_arity_from_tokens(Tokens)},
    {Import, [], [Import]};
get_attribute_args(export, Between, _Args) ->
    Tokens = get_between(Between, '[', ']'),
    Exports = fun_arity_from_tokens(Tokens),
    {Exports, Exports, []};
get_attribute_args(record, Between, _Args) ->
    [RecordToken | _] = Between,
    RecordName = RecordToken#token.value,
    Tokens = erlide_np_util:get_between_outer_pars(Between, '{', '}'),
    R = {RecordName, field_list_from_tokens(Tokens)},
    {R, [], []};
get_attribute_args(_, _Between, Args) ->
    {Args, [], []}.

check_class([#token{kind = AtomOrMacro}, #token{kind = '('} | _])
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    function;
check_class([#token{kind = '-'}, #token{kind = atom} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'spec'} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'type'} | _]) ->
    attribute;
check_class(_X) ->
    %?D(lists:sublist(_X, 5)),
    other.

get_first_of_kind(Kind, Tokens) ->
    case erlide_np_util:skip_to(Tokens, Kind) of
        [#token{kind=Kind, value=Value} | _] ->
            Value;
        _ ->
            undefined
    end.
        
fun_arity_from_tokens([#token{kind=atom, value=Fun}, #token{kind='/'}, 
                       #token{kind=integer, value=Arity} | Rest]) ->
    [{Fun, Arity} | fun_arity_from_tokens(Rest)];
fun_arity_from_tokens([_ | Rest]) ->
    fun_arity_from_tokens(Rest);
fun_arity_from_tokens(_) ->
    [].

field_list_from_tokens([#token{kind=atom, value=Field, line=Line, 
                               offset=Offset, length=Length} | Rest]) ->
    {Extra, NewRest} = get_upto2(Rest, ','),
    [{Field, {{Line, Line, Offset}, Length}, to_string(Extra)}
         | field_list_from_tokens(NewRest)];
field_list_from_tokens([_ | Rest]) ->
    field_list_from_tokens(Rest);
field_list_from_tokens(_) ->
    [].

to_string(Tokens) ->
    S = erlide_scanner:tokens_to_string(Tokens),
    erlide_text:strip(S).
%%     unspacify(S).

get_head(T) ->
    A = get_args(T),
    case get_guards(T) of
        "" ->
            A;
        G ->
            A ++ " when " ++ G
    end.

get_args(T) ->
    case erlide_np_util:get_between_outer_pars(T, '(', ')') of
        [] ->
            "";
        P ->
            "("++to_string(P)++")"
    end.

get_guards([]) ->
    "";
get_guards(T) ->
    to_string(get_between(T, 'when', '->')). 

last_not_eof(L) -> 
    case lists:reverse(L) of
        [eof, Last | _] ->
            Last;
        [Last | _] ->
            Last;
        _ ->
            error
    end.    


get_between(L, A, B) ->
    R = get_upto(L, B),
    lists:reverse(get_upto(lists:reverse(R), A)).

get_upto(L, Delim) ->
        get_upto(L, Delim, []).

get_upto([], _Delim, _Acc) ->
    [];
get_upto([#token{kind=Delim} | _], Delim, Acc) ->
        lists:reverse(Acc);
get_upto([T | Rest], Delim, Acc) ->
    get_upto(Rest, Delim, [T | Acc]).

get_upto2(L, Delim) ->
    get_upto2(L, Delim, []).

get_upto2([], _Delim, Acc) ->
    {lists:reverse(Acc), []};
get_upto2([#token{kind=Delim} | Rest], Delim, Acc) ->
    {lists:reverse(Acc), Rest};
get_upto2([T | Rest], Delim, Acc) ->
    get_upto2(Rest, Delim, [T | Acc]).

check_clause([#token{kind = ';'} | Rest], false) ->
    check_class(Rest) == function;
check_clause(_, _) ->
    false.

split_clauses(F) ->
    split_clauses(F, false, [], []).

split_clauses([], _HaveWhen, Acc, []) ->
    erlide_util:reverse2(Acc);
split_clauses([], HaveWhen, Acc, ClAcc) ->
    split_clauses([], HaveWhen, [ClAcc | Acc], []);
split_clauses([#token{kind=Kind}=T | Rest], _HaveWhen, Acc, ClAcc) 
  when Kind=:='end'; Kind=:=dot; Kind=:='->'; Kind=:='when' ->
    split_clauses(Rest, Kind=:='when', Acc, [T | ClAcc]);
split_clauses([T | Rest] = Tokens, HaveWhen, Acc, ClAcc) ->
    case check_clause(Tokens, HaveWhen) of
        false ->
            split_clauses(Rest, HaveWhen, Acc, [T | ClAcc]);
        true ->
            split_clauses(Rest, HaveWhen, [[T | ClAcc] | Acc], [])
    end.

%% fix_clause([#token{kind=atom, value=Name, line=Line, offset=Offset, length=Length} | Rest] = Code) ->
fix_clauses(Clauses) ->
    fix_clauses(Clauses, []).

fix_clauses([], Acc) ->
    lists:reverse(Acc);
fix_clauses([C | Rest], Acc) ->
    {Clause, Refs} = fix_clause(C),
    fix_clauses(Rest, [{Clause, Refs} | Acc]).

fix_clause([#token{kind=AtomOrMacro, value=Name, line=Line, offset=Offset, length=Length} | Rest]) 
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    #token{line=LastLine, offset=LastOffset, length=LastLength} = last_not_eof(Rest),
    PosLength = LastOffset - Offset + LastLength+1,
    ExternalRefs = get_refs_in_code(Rest),
    {#clause{pos={{Line, LastLine, Offset}, PosLength}, name_pos={{Line, Offset}, Length},
             name=Name, args=get_function_args(Rest), head=get_head(Rest)},
     ExternalRefs}.

get_function_args(Tokens) ->
    P = erlide_np_util:get_between_outer_pars(Tokens, '(', ')'),
    L = erlide_text:split_comma_list(P),
    [to_string(A) || A <- L].

fix_refs(ClausesAndRefs, Function, Imports) ->
    fix_refs(ClausesAndRefs, Function, Imports, []).

fix_refs([], #function{name=Name, arity=Arity, name_pos={{_, Offset}, Length}}, _Imports, Acc) ->
    Ref = #ref{data=#function_def{function=Name, arity=Arity}, offset=Offset, 
               length=Length, function=Name, arity=Arity, clause="", sub_clause=false},
    [Ref | Acc];
fix_refs([{Clause, Refs} | Rest], Function, Imports, Acc0) ->
    Acc1 = fix_refs(Refs, Clause, Function, Imports, Acc0),
    fix_refs(Rest, Function, Imports, Acc1).

fix_refs([], _Clause, _Function, _Imports, Acc) ->
    Acc;
fix_refs([{Offset, Length, RefData} | Rest], #clause{head=Head}=Clause, 
         #function{name=Name, arity=Arity, clauses=Clauses}=Function, 
         Imports, Acc) ->
    SubClause = case Clauses of
                    [] -> false;
                    [_] -> false;
                    _ -> true
                end,
    NewRefData = fix_imported_ref(RefData, Imports),
    Ref = #ref{data=NewRefData, offset=Offset, length=Length, function=Name, 
               arity=Arity, clause=Head, sub_clause=SubClause},
    fix_refs(Rest, Clause, Function, Imports, [Ref | Acc]).

fix_imported_ref(#local_call{function=F, arity=A}=RefData, [{Module, Imports} | Rest]) ->
    case lists:member({F, A}, Imports) of
        true -> #external_call{module=Module, function=F, arity=A};
        false -> fix_imported_ref(RefData, Rest)
    end;
fix_imported_ref(RefData, _Imports) ->
    RefData.

%% find references in code
%% VERY simple, M:F(_ ...), F(_ ...), #R, ?M, and that's it
%% returns {Offset, Length, Ref}, which is made into proper refs
%% by fix_refs
get_refs_in_code(L) ->
    get_refs_in_code(L, []).

get_refs_in_code([], Acc) ->
    lists:reverse(Acc);
get_refs_in_code([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
                  #token{kind=atom, value=F, offset=Offset2, length=Length2},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length2+Offset2-Offset, 
         #external_call{module = M, function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=atom, value=F, offset=Offset, length=Length},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length, #local_call{function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest], 
                 Acc) ->
    R = {Offset, Length, #macro_ref{macro=M}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind='#', offset=Offset}, 
                  #token{kind=atom, value=Record, offset=Offset2, length=Length2} | Rest],
                 Acc) ->
    R = {Offset, Length2+Offset2-Offset, #record_ref{record=Record}},
    {NewRest, Fields, RightSides} = erlide_np_records:check_fields(Rest, Record),
    NewAcc = get_refs_in_code(RightSides, Acc),
    get_refs_in_code(NewRest, Fields ++ [R | NewAcc]);
get_refs_in_code([#token{kind=var, value=V, offset=Offset, length=Length} | Rest],
                 Acc) ->
    R = {Offset, Length, make_var_def_ref(Acc, V)},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([_ | Rest], Acc) ->
    get_refs_in_code(Rest, Acc).

make_var_def_ref([], Var) ->
	#var_def{variable=Var};
make_var_def_ref([{_, _, #var_def{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([{_, _, #var_ref{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([_ | Rest], Var) ->
	make_var_def_ref(Rest, Var).

%% find type references in spec, type and opaque
%% also VERY simple, type(), ?M, #R, only
%% returns proper refs
get_refs(Tokens, Name, Arity) ->
	get_refs(Tokens, Name, Arity, []).

get_refs([], _, _, Acc) ->
    lists:reverse(Acc);
get_refs([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #macro_ref{macro=M}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind='#', offset=Offset},
          #token{kind=atom, value=R, offset=Offset2, length=Length2} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset, #record_ref{record=R}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
		  #token{kind=atom, value=T, offset=Offset2, length=Length2},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset,
                   #type_ref{module=M, type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=T, offset=Offset, length=Length},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #type_ref{module='_', type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=variable, value=V, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #var_ref{variable=V}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([_ | Rest], Name, Arity, Acc) ->
    get_refs(Rest, Name, Arity, Acc).

get_record_field_defs([], _) ->
    [];
get_record_field_defs([{FieldName, {{_, _, Offset}, Length}, _Extra} | Rest],
                      RecordName) ->
    [make_ref(Offset, Length, #record_field_def{record=RecordName, field=FieldName}, 
              FieldName, ?ARI_RECORD_FIELD_DEF)
         | get_record_field_defs(Rest, RecordName)].

make_ref(Offset, Length, Data, Name, Arity) ->
    #ref{data=Data, offset=Offset, length=Length, function=Name, arity=Arity, 
         clause="", sub_clause=false}.

make_attribute_arg_refs(define, Name, [#token{}, #token{kind='('} | Rest]) ->
    get_refs(skip_to_rparen(Rest), Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(define, Name, [#token{}, #token{kind=','} | Rest]) ->
    get_refs(Rest, Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(record, {Name, Fields}, [#token{}, #token{kind=','} | Rest]) ->
    get_record_field_defs(Fields, Name) ++ get_refs(Rest, Name, ?ARI_RECORD_DEF);
make_attribute_arg_refs(_, _, _) ->
    [].

skip_to_rparen([]) ->
    [];
skip_to_rparen([#token{kind=')'} | Rest]) ->
    Rest;
skip_to_rparen([_ | Rest]) ->
    skip_to_rparen(Rest).

make_attribute_ref(Name, Between, Extra, Offset, Length) ->
    ?D({Name, Between, Offset, Length}),
    case make_attribute_ref(Name, Between, Extra) of
        [] ->
            [];
        {Arity, AName, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=AName, 
                  arity=Arity, clause="", sub_clause=false}];
        {Arity, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=Name, 
                  arity=Arity, clause="", sub_clause=false}]
    end.

make_attribute_ref(module, _, Extra) ->
    {?ARI_ATTRIBUTE, #module_def{module=Extra}};
make_attribute_ref(record, Between, _E) ->
    ?D({Between, _E}),
    Name = case Between of
               {N, _} -> N;
               N -> N
           end,
    R= #record_def{record=Name},
    ?D(R),
    {?ARI_RECORD_DEF, Name, R};
make_attribute_ref(define, [Name | _], _) when is_list(Name) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(define, Name, _) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(include, _, Extra) ->
    {?ARI_INCLUDE, #include_ref{filename=unquote(Extra)}};
make_attribute_ref(_, _, _) ->
    [].

unquote(L) ->
    lists:reverse(unquote_first(lists:reverse(unquote_first(L)))).

unquote_first([$" | Rest]) ->
    Rest;
unquote_first(L) ->
    L.

remove_rest([], _) -> [];
remove_rest(Rest, Rest) -> [];
remove_rest([Hd | Tl], Rest) -> [Hd | remove_rest(Tl, Rest)].
%% Description: Split source files into functions and clauses and collects references
%% Author: jakob
%% Created: Mar 23, 2006

-module(erlide_noparse).

%%
%% Exported Functions
%%

%% called from Java
-export([initial_parse/5, reparse/1, remove_cache_files/2]).

%% called from Erlang
-export([read_module_refs/3]).

%%
%% Include files
%%

%% -define(DEBUG, 1).
%% -define(IO_FORMAT_DEBUG, 1).

-define(CACHE_VERSION, 25).
-define(SERVER, erlide_noparse).

-include("erlide.hrl").
-include("erlide_noparse.hrl").
-include("erlide_scanner.hrl").
-include("erlide_search_server.hrl").

%% special arity flags
-define(ARI_TYPESPEC, -2).
-define(ARI_ATTRIBUTE, -3).
-define(ARI_RECORD_DEF, -4).
-define(ARI_MACRO_DEF, -5).
-define(ARI_INCLUDE, -6).
-define(ARI_RECORD_FIELD_DEF, -7).

-define(X(X), erlide_log:log({X, erlang:now()})).


%%
%% API Functions
%%

initial_parse(ScannerName, ModuleFileName, StateDir, UseCache,
              UpdateSearchServer) ->
    try
        %%         ?D({StateDir, ModuleFileName}),
        BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
        RefsFileName = BaseName ++ ".refs",
        RenewFun = fun(_F) ->
                           do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer)
                   end,
        CacheFileName = BaseName ++ ".noparse",
        ?D(CacheFileName),
        {Cached, Res} = erlide_util:check_and_renew_cached(
                          ModuleFileName, CacheFileName, ?CACHE_VERSION, 
                          RenewFun, UseCache),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        ?D(updated),
        {ok, Res, Cached}
    catch
        error:Reason ->
            {error, Reason}
    end.

reparse(ScannerName) ->
    try
        Res = do_parse(ScannerName, "", "", true),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        {ok, Res, unused}
    catch
        error:Reason ->
            {error, Reason}
    end.

read_module_refs(ScannerName, ModulePath, StateDir) ->
    ?D(ScannerName),
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    RefsFileName = BaseName ++ ".refs",
    ?D(RefsFileName),
    case file:read_file(RefsFileName) of
        {ok, Binary} ->
            ?D(byte_size(Binary)),
            binary_to_term(Binary);
        _ ->
            ?D(ModulePath),
            {ok, InitialTextBin} = file:read_file(ModulePath),
            ?D(byte_size(InitialTextBin)),
            InitialText = binary_to_list(InitialTextBin),
            _D = erlide_scanner_server:initialScan(
                   ScannerName, ModulePath, InitialText, StateDir, true),
            ?D(_D),
            initial_parse(ScannerName, ModulePath, StateDir, true, false),
            ?D(RefsFileName),
            {ok, Binary} = file:read_file(RefsFileName),
            ?D(byte_size(Binary)),
            binary_to_term(Binary)
    end.

remove_cache_files(ScannerName, StateDir) ->
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    ScannerCacheFileName = BaseName ++ ".scan",
    file:delete(ScannerCacheFileName),
    RefsFileName = BaseName ++ ".refs",
    file:delete(RefsFileName),
    CacheFileName = BaseName ++ ".noparse",
    file:delete(CacheFileName).

%%
%% Internal functions
%%

do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer) ->
    Toks = erlide_scanner_server:getTokens(ScannerName),
    do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer).

do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer) ->
    ?D({do_parse, ScannerName, length(Toks)}),
    ?X(111),
    {UncommentToks, Comments} = erlide_np_util:extract_comments(Toks),
    ?X(222),
    ?D({length(UncommentToks), length(Comments)}),
    ?D({UncommentToks}),
    Functions = erlide_np_util:split_after_dots(UncommentToks),
    ?X(333),
    ?D(length(Functions)),
    AutoImports = erlide_util:add_auto_imported([]),
    ?X(444),
%%     ?D(AutoImports),
    {Collected, Refs} = classify_and_collect(Functions, [], [], [], AutoImports),
    ?X(555),
    ?D({'>>',length(Collected)}),
    CommentedCollected = erlide_np_util:get_function_comments(Collected, Comments),
    ?X(666),
    ?D(CommentedCollected),
    Model = #model{forms=CommentedCollected, comments=Comments},
    %%erlide_noparse_server:create(ScannerName, Model),
%%     ?D({"Model", length(Model#model.forms), erts_debug:flat_size(Model)}),
    FixedModel = fixup_model(Model),
    ?X(777),
%%     ?D(erts_debug:flat_size(FixedModel)),
    %% 	?D([erts_debug:flat_size(F) || F <- element(2, FixedModel)]),
    ?D(StateDir),
    case StateDir of
        "" -> ok;
        _ ->
            ?D({RefsFileName, length(Refs)}),
            file:write_file(RefsFileName, term_to_binary(Refs, [compressed]))
    end,
    update_search_server(UpdateSearchServer, ScannerName, Refs),
%%     ?D(FixedModel),
    ?X(888),
    FixedModel.

update_search_server(true, ScannerName, Refs) ->
    erlide_search_server:add_module_refs(ScannerName, Refs);
update_search_server(_, _, _) ->
    ok.

fixup_model(#model{forms=Forms, comments=Comments}) ->
    FixedComments = fixup_tokens(Comments),
    FixedForms = fixup_forms(Forms),
    #model{forms=FixedForms, comments=FixedComments}.

fixup_forms(Forms) ->
    [fixup_form(Form) || Form <- Forms].

fixup_tokens(Tokens) ->
    [fixup_token(Token) || Token <- Tokens].

fixup_token(#token{value=Value} = Token) when is_list(Value) ->
    Token#token{value=to_binary(Value)};
fixup_token(#token{text=Text} = Token) when is_list(Text) ->
    Token#token{value=to_binary(Text)};
fixup_token(Token) ->
    Token.

binary_args(Args) when is_list(Args) ->
    [iolist_to_binary(A) || A <- Args];
binary_args(_) ->
    [].

fixup_form(#function{comment=Comment, clauses=Clauses, args=Args} = Function) ->
    Function#function{comment= to_binary(Comment), clauses=fixup_forms(Clauses), args=binary_args(Args)};
fixup_form(#clause{head=Head, args=Args} = Clause) ->
    Clause#clause{head=to_binary(Head), args=binary_args(Args)};
fixup_form(Other) ->
    Other.

to_binary(Comment) when is_list(Comment) ->
    try 
        iolist_to_binary(Comment) 
    catch 
        _:_ -> 
            unicode:characters_to_binary(Comment) 
    end;
to_binary(Other) ->
    Other.

%% parse_test(ScannerName, File) ->
%%     erlide_scanner_server:scan_uncached(ScannerName, File, ""),
%%     Toks = erlide_scanner_server:getTokens(ScannerName),
%%     {UncommentToks, Comments} = extract_comments(Toks),
%%     Functions = split_after_dots(UncommentToks, [], []),
%%     {Collected, Refs} = classify_and_collect(Functions, [], []),
%%     _Model = #model{forms=Collected, comments=Comments},
%%     %%erlide_noparse_server:create(ScannerName, Model, ""),
%%     ok.

classify_and_collect([], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([eof], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([C | Rest], Acc, RefsAcc, Exports, Imports) ->
    {R, Refs, MoreExports, MoreImports} = cac(check_class(C), C, Exports, Imports),
    classify_and_collect(Rest, [R | Acc], Refs++RefsAcc, MoreExports++Exports, MoreImports++Imports).

cac(function, Tokens, Exports, Imports) ->
    ClauseList = split_clauses(Tokens),
    %?D(ClauseList),
    ClausesAndRefs = fix_clauses(ClauseList),
    %?D(length(Clauses)),
    [{#clause{pos=P, name=N, args=A, head=H, name_pos=NP}, _Refs} | _] = ClausesAndRefs,
    ?D(A),
    Arity = length(A),
    Exported = get_exported({N, Arity}, Exports),
    Function = case ClausesAndRefs of  	% only show subclauses when more than one
                   [_] ->
                       #function{pos=P, name=N, arity=Arity, args=A, head=H, 
                                 clauses=[], name_pos=NP, exported=Exported};
                   _ ->
                       Clauses = [C || {C, _} <- ClausesAndRefs],
                       #function{pos=P, name=N, arity=Arity, clauses=Clauses, 
                                 name_pos=NP, exported=Exported}
               end,
    {Function, fix_refs(ClausesAndRefs, Function, Imports), [], []};
cac(attribute, Attribute, _Exports, _Imports) ->
    ?D(Attribute),
    case Attribute of
        %% -spec, -type or -opaque
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=Kind, line=_Line, offset=_Offset, value=Name} | Args]
          when (Kind=:='spec') or ((Kind=:=atom) and (Name=:='type'))
                   or ((Kind=:=atom) and (Name=:='opaque'))->
            get_type_attribute(Kind, Name, Offset, Line, Attribute, Args);
        %% other attributes
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=atom, value=Name, line=_Line, offset=_Offset},
         _, #token{value=Args} | _] = Attribute ->
            get_other_attribute(Name, Offset, Line, Attribute, Args);
        [_, #token{kind=atom, value=Name, line=Line, offset=Offset} | _] ->
            #token{line=LastLine, offset=LastOffset, 
                   length=LastLength} = last_not_eof(Attribute),
            PosLength = LastOffset - Offset + LastLength,
            {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                       name=Name, args=[]}, [], [], []}
    end;
cac(other, [#token{value=Name, line=Line, offset=Offset, length=Length} | _], 
    _Exports, _Imports) ->
    {#other{pos={{Line, Line, Offset}, Length}, name=Name}, [], [], []};
cac(_, _D, _E, _I) ->
    {eof, [], [], []}.

get_type_attribute(Kind, Name0, Offset, Line, Attribute, Args) ->
    ?D(Name0),
    Name = case Kind of 'spec' -> Kind; _ -> Name0 end,
    #token{line=LastLine, offset=LastOffset,
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    ?D(Args),
    Extra = to_string(Args),
    ?D(Extra),
    {AttrArgs, _, _} = get_attribute_args(Kind, Args, Args),
    ?D({AttrArgs, Extra}),
    ExternalRefs = get_refs(tl(AttrArgs), Extra, ?ARI_TYPESPEC),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     [#ref{data=#type_def{type=Name}, offset=Offset, length=PosLength, function=Name, 
           arity=?ARI_TYPESPEC, clause="", sub_clause=false} | ExternalRefs], [], []}.

get_other_attribute(Name, Offset, Line, Attribute, Args) ->
    #token{line=LastLine, offset=LastOffset, 
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    Between = erlide_np_util:get_between_outer_pars(Attribute, '(', ')'),
    Extra = to_string(Between),
    {AttrArgs, Exports, Imports} = get_attribute_args(Name, Between, Args),
    ?D({AttrArgs, Between}),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     make_attribute_ref(Name, AttrArgs, Extra, Offset, PosLength)++
         make_attribute_arg_refs(Name, AttrArgs, Between),
     Exports, Imports}.

get_exported(F_A, Exports) ->
    lists:member(F_A, Exports).

get_attribute_args(import, Between, _Args) ->
    From = get_first_of_kind(atom, Between),
    Tokens = get_between(Between, '[', ']'),
    Import = {From, fun_arity_from_tokens(Tokens)},
    {Import, [], [Import]};
get_attribute_args(export, Between, _Args) ->
    Tokens = get_between(Between, '[', ']'),
    Exports = fun_arity_from_tokens(Tokens),
    {Exports, Exports, []};
get_attribute_args(record, Between, _Args) ->
    [RecordToken | _] = Between,
    RecordName = RecordToken#token.value,
    Tokens = erlide_np_util:get_between_outer_pars(Between, '{', '}'),
    R = {RecordName, field_list_from_tokens(Tokens)},
    {R, [], []};
get_attribute_args(_, _Between, Args) ->
    {Args, [], []}.

check_class([#token{kind = AtomOrMacro}, #token{kind = '('} | _])
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    function;
check_class([#token{kind = '-'}, #token{kind = atom} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'spec'} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'type'} | _]) ->
    attribute;
check_class(_X) ->
    %?D(lists:sublist(_X, 5)),
    other.

get_first_of_kind(Kind, Tokens) ->
    case erlide_np_util:skip_to(Tokens, Kind) of
        [#token{kind=Kind, value=Value} | _] ->
            Value;
        _ ->
            undefined
    end.
        
fun_arity_from_tokens([#token{kind=atom, value=Fun}, #token{kind='/'}, 
                       #token{kind=integer, value=Arity} | Rest]) ->
    [{Fun, Arity} | fun_arity_from_tokens(Rest)];
fun_arity_from_tokens([_ | Rest]) ->
    fun_arity_from_tokens(Rest);
fun_arity_from_tokens(_) ->
    [].

field_list_from_tokens([#token{kind=atom, value=Field, line=Line, 
                               offset=Offset, length=Length} | Rest]) ->
    {Extra, NewRest} = get_upto2(Rest, ','),
    [{Field, {{Line, Line, Offset}, Length}, to_string(Extra)}
         | field_list_from_tokens(NewRest)];
field_list_from_tokens([_ | Rest]) ->
    field_list_from_tokens(Rest);
field_list_from_tokens(_) ->
    [].

to_string(Tokens) ->
    S = erlide_scanner:tokens_to_string(Tokens),
    erlide_text:strip(S).
%%     unspacify(S).

get_head(T) ->
    A = get_args(T),
    case get_guards(T) of
        "" ->
            A;
        G ->
            A ++ " when " ++ G
    end.

get_args(T) ->
    case erlide_np_util:get_between_outer_pars(T, '(', ')') of
        [] ->
            "";
        P ->
            "("++to_string(P)++")"
    end.

get_guards([]) ->
    "";
get_guards(T) ->
    to_string(get_between(T, 'when', '->')). 

last_not_eof(L) -> 
    case lists:reverse(L) of
        [eof, Last | _] ->
            Last;
        [Last | _] ->
            Last;
        _ ->
            error
    end.    


get_between(L, A, B) ->
    R = get_upto(L, B),
    lists:reverse(get_upto(lists:reverse(R), A)).

get_upto(L, Delim) ->
        get_upto(L, Delim, []).

get_upto([], _Delim, _Acc) ->
    [];
get_upto([#token{kind=Delim} | _], Delim, Acc) ->
        lists:reverse(Acc);
get_upto([T | Rest], Delim, Acc) ->
    get_upto(Rest, Delim, [T | Acc]).

get_upto2(L, Delim) ->
    get_upto2(L, Delim, []).

get_upto2([], _Delim, Acc) ->
    {lists:reverse(Acc), []};
get_upto2([#token{kind=Delim} | Rest], Delim, Acc) ->
    {lists:reverse(Acc), Rest};
get_upto2([T | Rest], Delim, Acc) ->
    get_upto2(Rest, Delim, [T | Acc]).

check_clause([#token{kind = ';'} | Rest], false) ->
    check_class(Rest) == function;
check_clause(_, _) ->
    false.

split_clauses(F) ->
    split_clauses(F, false, [], []).

split_clauses([], _HaveWhen, Acc, []) ->
    erlide_util:reverse2(Acc);
split_clauses([], HaveWhen, Acc, ClAcc) ->
    split_clauses([], HaveWhen, [ClAcc | Acc], []);
split_clauses([#token{kind=Kind}=T | Rest], _HaveWhen, Acc, ClAcc) 
  when Kind=:='end'; Kind=:=dot; Kind=:='->'; Kind=:='when' ->
    split_clauses(Rest, Kind=:='when', Acc, [T | ClAcc]);
split_clauses([T | Rest] = Tokens, HaveWhen, Acc, ClAcc) ->
    case check_clause(Tokens, HaveWhen) of
        false ->
            split_clauses(Rest, HaveWhen, Acc, [T | ClAcc]);
        true ->
            split_clauses(Rest, HaveWhen, [[T | ClAcc] | Acc], [])
    end.

%% fix_clause([#token{kind=atom, value=Name, line=Line, offset=Offset, length=Length} | Rest] = Code) ->
fix_clauses(Clauses) ->
    fix_clauses(Clauses, []).

fix_clauses([], Acc) ->
    lists:reverse(Acc);
fix_clauses([C | Rest], Acc) ->
    {Clause, Refs} = fix_clause(C),
    fix_clauses(Rest, [{Clause, Refs} | Acc]).

fix_clause([#token{kind=AtomOrMacro, value=Name, line=Line, offset=Offset, length=Length} | Rest]) 
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    #token{line=LastLine, offset=LastOffset, length=LastLength} = last_not_eof(Rest),
    PosLength = LastOffset - Offset + LastLength+1,
    ExternalRefs = get_refs_in_code(Rest),
    {#clause{pos={{Line, LastLine, Offset}, PosLength}, name_pos={{Line, Offset}, Length},
             name=Name, args=get_function_args(Rest), head=get_head(Rest)},
     ExternalRefs}.

get_function_args(Tokens) ->
    P = erlide_np_util:get_between_outer_pars(Tokens, '(', ')'),
    L = erlide_text:split_comma_list(P),
    [to_string(A) || A <- L].

fix_refs(ClausesAndRefs, Function, Imports) ->
    fix_refs(ClausesAndRefs, Function, Imports, []).

fix_refs([], #function{name=Name, arity=Arity, name_pos={{_, Offset}, Length}}, _Imports, Acc) ->
    Ref = #ref{data=#function_def{function=Name, arity=Arity}, offset=Offset, 
               length=Length, function=Name, arity=Arity, clause="", sub_clause=false},
    [Ref | Acc];
fix_refs([{Clause, Refs} | Rest], Function, Imports, Acc0) ->
    Acc1 = fix_refs(Refs, Clause, Function, Imports, Acc0),
    fix_refs(Rest, Function, Imports, Acc1).

fix_refs([], _Clause, _Function, _Imports, Acc) ->
    Acc;
fix_refs([{Offset, Length, RefData} | Rest], #clause{head=Head}=Clause, 
         #function{name=Name, arity=Arity, clauses=Clauses}=Function, 
         Imports, Acc) ->
    SubClause = case Clauses of
                    [] -> false;
                    [_] -> false;
                    _ -> true
                end,
    NewRefData = fix_imported_ref(RefData, Imports),
    Ref = #ref{data=NewRefData, offset=Offset, length=Length, function=Name, 
               arity=Arity, clause=Head, sub_clause=SubClause},
    fix_refs(Rest, Clause, Function, Imports, [Ref | Acc]).

fix_imported_ref(#local_call{function=F, arity=A}=RefData, [{Module, Imports} | Rest]) ->
    case lists:member({F, A}, Imports) of
        true -> #external_call{module=Module, function=F, arity=A};
        false -> fix_imported_ref(RefData, Rest)
    end;
fix_imported_ref(RefData, _Imports) ->
    RefData.

%% find references in code
%% VERY simple, M:F(_ ...), F(_ ...), #R, ?M, and that's it
%% returns {Offset, Length, Ref}, which is made into proper refs
%% by fix_refs
get_refs_in_code(L) ->
    get_refs_in_code(L, []).

get_refs_in_code([], Acc) ->
    lists:reverse(Acc);
get_refs_in_code([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
                  #token{kind=atom, value=F, offset=Offset2, length=Length2},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length2+Offset2-Offset, 
         #external_call{module = M, function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=atom, value=F, offset=Offset, length=Length},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length, #local_call{function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest], 
                 Acc) ->
    R = {Offset, Length, #macro_ref{macro=M}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind='#', offset=Offset}, 
                  #token{kind=atom, value=Record, offset=Offset2, length=Length2} | Rest],
                 Acc) ->
    R = {Offset, Length2+Offset2-Offset, #record_ref{record=Record}},
    {NewRest, Fields, RightSides} = erlide_np_records:check_fields(Rest, Record),
    NewAcc = get_refs_in_code(RightSides, Acc),
    get_refs_in_code(NewRest, Fields ++ [R | NewAcc]);
get_refs_in_code([#token{kind=var, value=V, offset=Offset, length=Length} | Rest],
                 Acc) ->
    R = {Offset, Length, make_var_def_ref(Acc, V)},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([_ | Rest], Acc) ->
    get_refs_in_code(Rest, Acc).

make_var_def_ref([], Var) ->
	#var_def{variable=Var};
make_var_def_ref([{_, _, #var_def{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([{_, _, #var_ref{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([_ | Rest], Var) ->
	make_var_def_ref(Rest, Var).

%% find type references in spec, type and opaque
%% also VERY simple, type(), ?M, #R, only
%% returns proper refs
get_refs(Tokens, Name, Arity) ->
	get_refs(Tokens, Name, Arity, []).

get_refs([], _, _, Acc) ->
    lists:reverse(Acc);
get_refs([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #macro_ref{macro=M}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind='#', offset=Offset},
          #token{kind=atom, value=R, offset=Offset2, length=Length2} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset, #record_ref{record=R}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
		  #token{kind=atom, value=T, offset=Offset2, length=Length2},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset,
                   #type_ref{module=M, type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=T, offset=Offset, length=Length},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #type_ref{module='_', type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=variable, value=V, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #var_ref{variable=V}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([_ | Rest], Name, Arity, Acc) ->
    get_refs(Rest, Name, Arity, Acc).

get_record_field_defs([], _) ->
    [];
get_record_field_defs([{FieldName, {{_, _, Offset}, Length}, _Extra} | Rest],
                      RecordName) ->
    [make_ref(Offset, Length, #record_field_def{record=RecordName, field=FieldName}, 
              FieldName, ?ARI_RECORD_FIELD_DEF)
         | get_record_field_defs(Rest, RecordName)].

make_ref(Offset, Length, Data, Name, Arity) ->
    #ref{data=Data, offset=Offset, length=Length, function=Name, arity=Arity, 
         clause="", sub_clause=false}.

make_attribute_arg_refs(define, Name, [#token{}, #token{kind='('} | Rest]) ->
    get_refs(skip_to_rparen(Rest), Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(define, Name, [#token{}, #token{kind=','} | Rest]) ->
    get_refs(Rest, Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(record, {Name, Fields}, [#token{}, #token{kind=','} | Rest]) ->
    get_record_field_defs(Fields, Name) ++ get_refs(Rest, Name, ?ARI_RECORD_DEF);
make_attribute_arg_refs(_, _, _) ->
    [].

skip_to_rparen([]) ->
    [];
skip_to_rparen([#token{kind=')'} | Rest]) ->
    Rest;
skip_to_rparen([_ | Rest]) ->
    skip_to_rparen(Rest).

make_attribute_ref(Name, Between, Extra, Offset, Length) ->
    ?D({Name, Between, Offset, Length}),
    case make_attribute_ref(Name, Between, Extra) of
        [] ->
            [];
        {Arity, AName, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=AName, 
                  arity=Arity, clause="", sub_clause=false}];
        {Arity, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=Name, 
                  arity=Arity, clause="", sub_clause=false}]
    end.

make_attribute_ref(module, _, Extra) ->
    {?ARI_ATTRIBUTE, #module_def{module=Extra}};
make_attribute_ref(record, Between, _E) ->
    ?D({Between, _E}),
    Name = case Between of
               {N, _} -> N;
               N -> N
           end,
    R= #record_def{record=Name},
    ?D(R),
    {?ARI_RECORD_DEF, Name, R};
make_attribute_ref(define, [Name | _], _) when is_list(Name) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(define, Name, _) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(include, _, Extra) ->
    {?ARI_INCLUDE, #include_ref{filename=unquote(Extra)}};
make_attribute_ref(_, _, _) ->
    [].

unquote(L) ->
    lists:reverse(unquote_first(lists:reverse(unquote_first(L)))).

unquote_first([$" | Rest]) ->
    Rest;
unquote_first(L) ->
    L.

remove_rest([], _) -> [];
remove_rest(Rest, Rest) -> [];
remove_rest([Hd | Tl], Rest) -> [Hd | remove_rest(Tl, Rest)].
%% Description: Split source files into functions and clauses and collects references
%% Author: jakob
%% Created: Mar 23, 2006

-module(erlide_noparse).

%%
%% Exported Functions
%%

%% called from Java
-export([initial_parse/5, reparse/1, remove_cache_files/2]).

%% called from Erlang
-export([read_module_refs/3]).

%%
%% Include files
%%

%% -define(DEBUG, 1).
%% -define(IO_FORMAT_DEBUG, 1).

-define(CACHE_VERSION, 25).
-define(SERVER, erlide_noparse).

-include("erlide.hrl").
-include("erlide_noparse.hrl").
-include("erlide_scanner.hrl").
-include("erlide_search_server.hrl").

%% special arity flags
-define(ARI_TYPESPEC, -2).
-define(ARI_ATTRIBUTE, -3).
-define(ARI_RECORD_DEF, -4).
-define(ARI_MACRO_DEF, -5).
-define(ARI_INCLUDE, -6).
-define(ARI_RECORD_FIELD_DEF, -7).

-define(X(X), erlide_log:log({X, erlang:now()})).


%%
%% API Functions
%%

initial_parse(ScannerName, ModuleFileName, StateDir, UseCache,
              UpdateSearchServer) ->
    try
        %%         ?D({StateDir, ModuleFileName}),
        BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
        RefsFileName = BaseName ++ ".refs",
        RenewFun = fun(_F) ->
                           do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer)
                   end,
        CacheFileName = BaseName ++ ".noparse",
        ?D(CacheFileName),
        {Cached, Res} = erlide_util:check_and_renew_cached(
                          ModuleFileName, CacheFileName, ?CACHE_VERSION, 
                          RenewFun, UseCache),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        ?D(updated),
        {ok, Res, Cached}
    catch
        error:Reason ->
            {error, Reason}
    end.

reparse(ScannerName) ->
    try
        Res = do_parse(ScannerName, "", "", true),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        {ok, Res, unused}
    catch
        error:Reason ->
            {error, Reason}
    end.

read_module_refs(ScannerName, ModulePath, StateDir) ->
    ?D(ScannerName),
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    RefsFileName = BaseName ++ ".refs",
    ?D(RefsFileName),
    case file:read_file(RefsFileName) of
        {ok, Binary} ->
            ?D(byte_size(Binary)),
            binary_to_term(Binary);
        _ ->
            ?D(ModulePath),
            {ok, InitialTextBin} = file:read_file(ModulePath),
            ?D(byte_size(InitialTextBin)),
            InitialText = binary_to_list(InitialTextBin),
            _D = erlide_scanner_server:initialScan(
                   ScannerName, ModulePath, InitialText, StateDir, true),
            ?D(_D),
            initial_parse(ScannerName, ModulePath, StateDir, true, false),
            ?D(RefsFileName),
            {ok, Binary} = file:read_file(RefsFileName),
            ?D(byte_size(Binary)),
            binary_to_term(Binary)
    end.

remove_cache_files(ScannerName, StateDir) ->
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    ScannerCacheFileName = BaseName ++ ".scan",
    file:delete(ScannerCacheFileName),
    RefsFileName = BaseName ++ ".refs",
    file:delete(RefsFileName),
    CacheFileName = BaseName ++ ".noparse",
    file:delete(CacheFileName).

%%
%% Internal functions
%%

do_parse(ScannerName, RefsFileName, StateDir, UpdateSearchServer) ->
    Toks = erlide_scanner_server:getTokens(ScannerName),
    do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer).

do_parse2(ScannerName, RefsFileName, Toks, StateDir, UpdateSearchServer) ->
    ?D({do_parse, ScannerName, length(Toks)}),
    ?X(111),
    {UncommentToks, Comments} = erlide_np_util:extract_comments(Toks),
    ?X(222),
    ?D({length(UncommentToks), length(Comments)}),
    ?D({UncommentToks}),
    Functions = erlide_np_util:split_after_dots(UncommentToks),
    ?X(333),
    ?D(length(Functions)),
    AutoImports = erlide_util:add_auto_imported([]),
    ?X(444),
%%     ?D(AutoImports),
    {Collected, Refs} = classify_and_collect(Functions, [], [], [], AutoImports),
    ?X(555),
    ?D({'>>',length(Collected)}),
    CommentedCollected = erlide_np_util:get_function_comments(Collected, Comments),
    ?X(666),
    ?D(CommentedCollected),
    Model = #model{forms=CommentedCollected, comments=Comments},
    %%erlide_noparse_server:create(ScannerName, Model),
%%     ?D({"Model", length(Model#model.forms), erts_debug:flat_size(Model)}),
    FixedModel = fixup_model(Model),
    ?X(777),
%%     ?D(erts_debug:flat_size(FixedModel)),
    %% 	?D([erts_debug:flat_size(F) || F <- element(2, FixedModel)]),
    ?D(StateDir),
    case StateDir of
        "" -> ok;
        _ ->
            ?D({RefsFileName, length(Refs)}),
            file:write_file(RefsFileName, term_to_binary(Refs, [compressed]))
    end,
    update_search_server(UpdateSearchServer, ScannerName, Refs),
%%     ?D(FixedModel),
    ?X(888),
    FixedModel.

update_search_server(true, ScannerName, Refs) ->
    erlide_search_server:add_module_refs(ScannerName, Refs);
update_search_server(_, _, _) ->
    ok.

fixup_model(#model{forms=Forms, comments=Comments}) ->
    FixedComments = fixup_tokens(Comments),
    FixedForms = fixup_forms(Forms),
    #model{forms=FixedForms, comments=FixedComments}.

fixup_forms(Forms) ->
    [fixup_form(Form) || Form <- Forms].

fixup_tokens(Tokens) ->
    [fixup_token(Token) || Token <- Tokens].

fixup_token(#token{value=Value} = Token) when is_list(Value) ->
    Token#token{value=to_binary(Value)};
fixup_token(#token{text=Text} = Token) when is_list(Text) ->
    Token#token{value=to_binary(Text)};
fixup_token(Token) ->
    Token.

binary_args(Args) when is_list(Args) ->
    [iolist_to_binary(A) || A <- Args];
binary_args(_) ->
    [].

fixup_form(#function{comment=Comment, clauses=Clauses, args=Args} = Function) ->
    Function#function{comment= to_binary(Comment), clauses=fixup_forms(Clauses), args=binary_args(Args)};
fixup_form(#clause{head=Head, args=Args} = Clause) ->
    Clause#clause{head=to_binary(Head), args=binary_args(Args)};
fixup_form(Other) ->
    Other.

to_binary(Comment) when is_list(Comment) ->
    try 
        iolist_to_binary(Comment) 
    catch 
        _:_ -> 
            unicode:characters_to_binary(Comment) 
    end;
to_binary(Other) ->
    Other.

%% parse_test(ScannerName, File) ->
%%     erlide_scanner_server:scan_uncached(ScannerName, File, ""),
%%     Toks = erlide_scanner_server:getTokens(ScannerName),
%%     {UncommentToks, Comments} = extract_comments(Toks),
%%     Functions = split_after_dots(UncommentToks, [], []),
%%     {Collected, Refs} = classify_and_collect(Functions, [], []),
%%     _Model = #model{forms=Collected, comments=Comments},
%%     %%erlide_noparse_server:create(ScannerName, Model, ""),
%%     ok.

classify_and_collect([], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([eof], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect([C | Rest], Acc, RefsAcc, Exports, Imports) ->
    {R, Refs, MoreExports, MoreImports} = cac(check_class(C), C, Exports, Imports),
    classify_and_collect(Rest, [R | Acc], Refs++RefsAcc, MoreExports++Exports, MoreImports++Imports).

cac(function, Tokens, Exports, Imports) ->
    ClauseList = split_clauses(Tokens),
    %?D(ClauseList),
    ClausesAndRefs = fix_clauses(ClauseList),
    %?D(length(Clauses)),
    [{#clause{pos=P, name=N, args=A, head=H, name_pos=NP}, _Refs} | _] = ClausesAndRefs,
    ?D(A),
    Arity = length(A),
    Exported = get_exported({N, Arity}, Exports),
    Function = case ClausesAndRefs of  	% only show subclauses when more than one
                   [_] ->
                       #function{pos=P, name=N, arity=Arity, args=A, head=H, 
                                 clauses=[], name_pos=NP, exported=Exported};
                   _ ->
                       Clauses = [C || {C, _} <- ClausesAndRefs],
                       #function{pos=P, name=N, arity=Arity, clauses=Clauses, 
                                 name_pos=NP, exported=Exported}
               end,
    {Function, fix_refs(ClausesAndRefs, Function, Imports), [], []};
cac(attribute, Attribute, _Exports, _Imports) ->
    ?D(Attribute),
    case Attribute of
        %% -spec, -type or -opaque
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=Kind, line=_Line, offset=_Offset, value=Name} | Args]
          when (Kind=:='spec') or ((Kind=:=atom) and (Name=:='type'))
                   or ((Kind=:=atom) and (Name=:='opaque'))->
            get_type_attribute(Kind, Name, Offset, Line, Attribute, Args);
        %% other attributes
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=atom, value=Name, line=_Line, offset=_Offset},
         _, #token{value=Args} | _] = Attribute ->
            get_other_attribute(Name, Offset, Line, Attribute, Args);
        [_, #token{kind=atom, value=Name, line=Line, offset=Offset} | _] ->
            #token{line=LastLine, offset=LastOffset, 
                   length=LastLength} = last_not_eof(Attribute),
            PosLength = LastOffset - Offset + LastLength,
            {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                       name=Name, args=[]}, [], [], []}
    end;
cac(other, [#token{value=Name, line=Line, offset=Offset, length=Length} | _], 
    _Exports, _Imports) ->
    {#other{pos={{Line, Line, Offset}, Length}, name=Name}, [], [], []};
cac(_, _D, _E, _I) ->
    {eof, [], [], []}.

get_type_attribute(Kind, Name0, Offset, Line, Attribute, Args) ->
    ?D(Name0),
    Name = case Kind of 'spec' -> Kind; _ -> Name0 end,
    #token{line=LastLine, offset=LastOffset,
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    ?D(Args),
    Extra = to_string(Args),
    ?D(Extra),
    {AttrArgs, _, _} = get_attribute_args(Kind, Args, Args),
    ?D({AttrArgs, Extra}),
    ExternalRefs = get_refs(tl(AttrArgs), Extra, ?ARI_TYPESPEC),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     [#ref{data=#type_def{type=Name}, offset=Offset, length=PosLength, function=Name, 
           arity=?ARI_TYPESPEC, clause="", sub_clause=false} | ExternalRefs], [], []}.

get_other_attribute(Name, Offset, Line, Attribute, Args) ->
    #token{line=LastLine, offset=LastOffset, 
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    Between = erlide_np_util:get_between_outer_pars(Attribute, '(', ')'),
    Extra = to_string(Between),
    {AttrArgs, Exports, Imports} = get_attribute_args(Name, Between, Args),
    ?D({AttrArgs, Between}),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     make_attribute_ref(Name, AttrArgs, Extra, Offset, PosLength)++
         make_attribute_arg_refs(Name, AttrArgs, Between),
     Exports, Imports}.

get_exported(F_A, Exports) ->
    lists:member(F_A, Exports).

get_attribute_args(import, Between, _Args) ->
    From = get_first_of_kind(atom, Between),
    Tokens = get_between(Between, '[', ']'),
    Import = {From, fun_arity_from_tokens(Tokens)},
    {Import, [], [Import]};
get_attribute_args(export, Between, _Args) ->
    Tokens = get_between(Between, '[', ']'),
    Exports = fun_arity_from_tokens(Tokens),
    {Exports, Exports, []};
get_attribute_args(record, Between, _Args) ->
    [RecordToken | _] = Between,
    RecordName = RecordToken#token.value,
    Tokens = erlide_np_util:get_between_outer_pars(Between, '{', '}'),
    R = {RecordName, field_list_from_tokens(Tokens)},
    {R, [], []};
get_attribute_args(_, _Between, Args) ->
    {Args, [], []}.

check_class([#token{kind = AtomOrMacro}, #token{kind = '('} | _])
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    function;
check_class([#token{kind = '-'}, #token{kind = atom} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'spec'} | _]) ->
    attribute;
check_class([#token{kind = '-'}, #token{kind = 'type'} | _]) ->
    attribute;
check_class(_X) ->
    %?D(lists:sublist(_X, 5)),
    other.

get_first_of_kind(Kind, Tokens) ->
    case erlide_np_util:skip_to(Tokens, Kind) of
        [#token{kind=Kind, value=Value} | _] ->
            Value;
        _ ->
            undefined
    end.
        
fun_arity_from_tokens([#token{kind=atom, value=Fun}, #token{kind='/'}, 
                       #token{kind=integer, value=Arity} | Rest]) ->
    [{Fun, Arity} | fun_arity_from_tokens(Rest)];
fun_arity_from_tokens([_ | Rest]) ->
    fun_arity_from_tokens(Rest);
fun_arity_from_tokens(_) ->
    [].

field_list_from_tokens([#token{kind=atom, value=Field, line=Line, 
                               offset=Offset, length=Length} | Rest]) ->
    {Extra, NewRest} = get_upto2(Rest, ','),
    [{Field, {{Line, Line, Offset}, Length}, to_string(Extra)}
         | field_list_from_tokens(NewRest)];
field_list_from_tokens([_ | Rest]) ->
    field_list_from_tokens(Rest);
field_list_from_tokens(_) ->
    [].

to_string(Tokens) ->
    S = erlide_scanner:tokens_to_string(Tokens),
    erlide_text:strip(S).
%%     unspacify(S).

get_head(T) ->
    A = get_args(T),
    case get_guards(T) of
        "" ->
            A;
        G ->
            A ++ " when " ++ G
    end.

get_args(T) ->
    case erlide_np_util:get_between_outer_pars(T, '(', ')') of
        [] ->
            "";
        P ->
            "("++to_string(P)++")"
    end.

get_guards([]) ->
    "";
get_guards(T) ->
    to_string(get_between(T, 'when', '->')). 

last_not_eof(L) -> 
    case lists:reverse(L) of
        [eof, Last | _] ->
            Last;
        [Last | _] ->
            Last;
        _ ->
            error
    end.    


get_between(L, A, B) ->
    R = get_upto(L, B),
    lists:reverse(get_upto(lists:reverse(R), A)).

get_upto(L, Delim) ->
        get_upto(L, Delim, []).

get_upto([], _Delim, _Acc) ->
    [];
get_upto([#token{kind=Delim} | _], Delim, Acc) ->
        lists:reverse(Acc);
get_upto([T | Rest], Delim, Acc) ->
    get_upto(Rest, Delim, [T | Acc]).

get_upto2(L, Delim) ->
    get_upto2(L, Delim, []).

get_upto2([], _Delim, Acc) ->
    {lists:reverse(Acc), []};
get_upto2([#token{kind=Delim} | Rest], Delim, Acc) ->
    {lists:reverse(Acc), Rest};
get_upto2([T | Rest], Delim, Acc) ->
    get_upto2(Rest, Delim, [T | Acc]).

check_clause([#token{kind = ';'} | Rest], false) ->
    check_class(Rest) == function;
check_clause(_, _) ->
    false.

split_clauses(F) ->
    split_clauses(F, false, [], []).

split_clauses([], _HaveWhen, Acc, []) ->
    erlide_util:reverse2(Acc);
split_clauses([], HaveWhen, Acc, ClAcc) ->
    split_clauses([], HaveWhen, [ClAcc | Acc], []);
split_clauses([#token{kind=Kind}=T | Rest], _HaveWhen, Acc, ClAcc) 
  when Kind=:='end'; Kind=:=dot; Kind=:='->'; Kind=:='when' ->
    split_clauses(Rest, Kind=:='when', Acc, [T | ClAcc]);
split_clauses([T | Rest] = Tokens, HaveWhen, Acc, ClAcc) ->
    case check_clause(Tokens, HaveWhen) of
        false ->
            split_clauses(Rest, HaveWhen, Acc, [T | ClAcc]);
        true ->
            split_clauses(Rest, HaveWhen, [[T | ClAcc] | Acc], [])
    end.

%% fix_clause([#token{kind=atom, value=Name, line=Line, offset=Offset, length=Length} | Rest] = Code) ->
fix_clauses(Clauses) ->
    fix_clauses(Clauses, []).

fix_clauses([], Acc) ->
    lists:reverse(Acc);
fix_clauses([C | Rest], Acc) ->
    {Clause, Refs} = fix_clause(C),
    fix_clauses(Rest, [{Clause, Refs} | Acc]).

fix_clause([#token{kind=AtomOrMacro, value=Name, line=Line, offset=Offset, length=Length} | Rest]) 
  when AtomOrMacro=:=atom; AtomOrMacro=:=macro ->
    #token{line=LastLine, offset=LastOffset, length=LastLength} = last_not_eof(Rest),
    PosLength = LastOffset - Offset + LastLength+1,
    ExternalRefs = get_refs_in_code(Rest),
    {#clause{pos={{Line, LastLine, Offset}, PosLength}, name_pos={{Line, Offset}, Length},
             name=Name, args=get_function_args(Rest), head=get_head(Rest)},
     ExternalRefs}.

get_function_args(Tokens) ->
    P = erlide_np_util:get_between_outer_pars(Tokens, '(', ')'),
    L = erlide_text:split_comma_list(P),
    [to_string(A) || A <- L].

fix_refs(ClausesAndRefs, Function, Imports) ->
    fix_refs(ClausesAndRefs, Function, Imports, []).

fix_refs([], #function{name=Name, arity=Arity, name_pos={{_, Offset}, Length}}, _Imports, Acc) ->
    Ref = #ref{data=#function_def{function=Name, arity=Arity}, offset=Offset, 
               length=Length, function=Name, arity=Arity, clause="", sub_clause=false},
    [Ref | Acc];
fix_refs([{Clause, Refs} | Rest], Function, Imports, Acc0) ->
    Acc1 = fix_refs(Refs, Clause, Function, Imports, Acc0),
    fix_refs(Rest, Function, Imports, Acc1).

fix_refs([], _Clause, _Function, _Imports, Acc) ->
    Acc;
fix_refs([{Offset, Length, RefData} | Rest], #clause{head=Head}=Clause, 
         #function{name=Name, arity=Arity, clauses=Clauses}=Function, 
         Imports, Acc) ->
    SubClause = case Clauses of
                    [] -> false;
                    [_] -> false;
                    _ -> true
                end,
    NewRefData = fix_imported_ref(RefData, Imports),
    Ref = #ref{data=NewRefData, offset=Offset, length=Length, function=Name, 
               arity=Arity, clause=Head, sub_clause=SubClause},
    fix_refs(Rest, Clause, Function, Imports, [Ref | Acc]).

fix_imported_ref(#local_call{function=F, arity=A}=RefData, [{Module, Imports} | Rest]) ->
    case lists:member({F, A}, Imports) of
        true -> #external_call{module=Module, function=F, arity=A};
        false -> fix_imported_ref(RefData, Rest)
    end;
fix_imported_ref(RefData, _Imports) ->
    RefData.

%% find references in code
%% VERY simple, M:F(_ ...), F(_ ...), #R, ?M, and that's it
%% returns {Offset, Length, Ref}, which is made into proper refs
%% by fix_refs
get_refs_in_code(L) ->
    get_refs_in_code(L, []).

get_refs_in_code([], Acc) ->
    lists:reverse(Acc);
get_refs_in_code([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
                  #token{kind=atom, value=F, offset=Offset2, length=Length2},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length2+Offset2-Offset, 
         #external_call{module = M, function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=atom, value=F, offset=Offset, length=Length},
                  #token{kind='('} | Rest], Acc) ->
    Arity = erlide_text:guess_arity(Rest),
    R = {Offset, Length, #local_call{function=F, arity=Arity}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest], 
                 Acc) ->
    R = {Offset, Length, #macro_ref{macro=M}},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([#token{kind='#', offset=Offset}, 
                  #token{kind=atom, value=Record, offset=Offset2, length=Length2} | Rest],
                 Acc) ->
    R = {Offset, Length2+Offset2-Offset, #record_ref{record=Record}},
    {NewRest, Fields, RightSides} = erlide_np_records:check_fields(Rest, Record),
    NewAcc = get_refs_in_code(RightSides, Acc),
    get_refs_in_code(NewRest, Fields ++ [R | NewAcc]);
get_refs_in_code([#token{kind=var, value=V, offset=Offset, length=Length} | Rest],
                 Acc) ->
    R = {Offset, Length, make_var_def_ref(Acc, V)},
    get_refs_in_code(Rest, [R | Acc]);
get_refs_in_code([_ | Rest], Acc) ->
    get_refs_in_code(Rest, Acc).

make_var_def_ref([], Var) ->
	#var_def{variable=Var};
make_var_def_ref([{_, _, #var_def{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([{_, _, #var_ref{variable=Var}} | _], Var) ->
	#var_ref{variable=Var};
make_var_def_ref([_ | Rest], Var) ->
	make_var_def_ref(Rest, Var).

%% find type references in spec, type and opaque
%% also VERY simple, type(), ?M, #R, only
%% returns proper refs
get_refs(Tokens, Name, Arity) ->
	get_refs(Tokens, Name, Arity, []).

get_refs([], _, _, Acc) ->
    lists:reverse(Acc);
get_refs([#token{kind=macro, value=M, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #macro_ref{macro=M}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind='#', offset=Offset},
          #token{kind=atom, value=R, offset=Offset2, length=Length2} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset, #record_ref{record=R}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
		  #token{kind=atom, value=T, offset=Offset2, length=Length2},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length2+Offset2-Offset,
                   #type_ref{module=M, type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=atom, value=T, offset=Offset, length=Length},
		  #token{kind='('}, #token{kind=')'} | Rest], Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #type_ref{module='_', type=T}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([#token{kind=variable, value=V, offset=Offset, length=Length} | Rest],
		 Name, Arity, Acc) ->
    Ref = make_ref(Offset, Length, #var_ref{variable=V}, Name, Arity),
    get_refs(Rest, Name, Arity, [Ref | Acc]);
get_refs([_ | Rest], Name, Arity, Acc) ->
    get_refs(Rest, Name, Arity, Acc).

get_record_field_defs([], _) ->
    [];
get_record_field_defs([{FieldName, {{_, _, Offset}, Length}, _Extra} | Rest],
                      RecordName) ->
    [make_ref(Offset, Length, #record_field_def{record=RecordName, field=FieldName}, 
              FieldName, ?ARI_RECORD_FIELD_DEF)
         | get_record_field_defs(Rest, RecordName)].

make_ref(Offset, Length, Data, Name, Arity) ->
    #ref{data=Data, offset=Offset, length=Length, function=Name, arity=Arity, 
         clause="", sub_clause=false}.

make_attribute_arg_refs(define, Name, [#token{}, #token{kind='('} | Rest]) ->
    get_refs(skip_to_rparen(Rest), Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(define, Name, [#token{}, #token{kind=','} | Rest]) ->
    get_refs(Rest, Name, ?ARI_MACRO_DEF);
make_attribute_arg_refs(record, {Name, Fields}, [#token{}, #token{kind=','} | Rest]) ->
    get_record_field_defs(Fields, Name) ++ get_refs(Rest, Name, ?ARI_RECORD_DEF);
make_attribute_arg_refs(_, _, _) ->
    [].

skip_to_rparen([]) ->
    [];
skip_to_rparen([#token{kind=')'} | Rest]) ->
    Rest;
skip_to_rparen([_ | Rest]) ->
    skip_to_rparen(Rest).

make_attribute_ref(Name, Between, Extra, Offset, Length) ->
    ?D({Name, Between, Offset, Length}),
    case make_attribute_ref(Name, Between, Extra) of
        [] ->
            [];
        {Arity, AName, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=AName, 
                  arity=Arity, clause="", sub_clause=false}];
        {Arity, Data} ->
            [#ref{data=Data, offset=Offset, length=Length, function=Name, 
                  arity=Arity, clause="", sub_clause=false}]
    end.

make_attribute_ref(module, _, Extra) ->
    {?ARI_ATTRIBUTE, #module_def{module=Extra}};
make_attribute_ref(record, Between, _E) ->
    ?D({Between, _E}),
    Name = case Between of
               {N, _} -> N;
               N -> N
           end,
    R= #record_def{record=Name},
    ?D(R),
    {?ARI_RECORD_DEF, Name, R};
make_attribute_ref(define, [Name | _], _) when is_list(Name) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(define, Name, _) ->
    {?ARI_MACRO_DEF, Name, #macro_def{macro=Name}};
make_attribute_ref(include, _, Extra) ->
    {?ARI_INCLUDE, #include_ref{filename=unquote(Extra)}};
make_attribute_ref(_, _, _) ->
    [].

unquote(L) ->
    lists:reverse(unquote_first(lists:reverse(unquote_first(L)))).

unquote_first([$" | Rest]) ->
    Rest;
unquote_first(L) ->
    L.

remove_rest([], _) -> [];
remove_rest(Rest, Rest) -> [];
remove_rest([Hd | Tl], Rest) -> [Hd | remove_rest(Tl, Rest)].
