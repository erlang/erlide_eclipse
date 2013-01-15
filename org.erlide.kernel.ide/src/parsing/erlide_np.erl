%% @author jakob
%% the functional interface for the "parser"
%% returns forms, comments and referencs
 

-module(erlide_np).

-export([parse/1]).

-include("erlide.hrl").
-include("erlide_scanner.hrl").
-include("erlide_noparse.hrl").
-include("erlide_search.hrl").

parse(Tokens) ->
    {TokensWoComments, Comments} = erlide_np_util:extract_comments(Tokens),
    Functions = erlide_np_util:split_after_dots(TokensWoComments),
    AutoImports = erlide_util:add_auto_imported([]),
    {Forms, References} = classify_and_collect_forms(Functions, [], [], [], AutoImports),
    {Forms, Comments, References}.

classify_and_collect_forms([], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect_forms([eof], Acc, RefsAcc, _Exports, _Imports) ->
    {lists:reverse(Acc), lists:reverse(RefsAcc)};
classify_and_collect_forms([C | Rest], Acc, RefsAcc, Exports, Imports) ->
    {R, Refs, MoreExports, MoreImports} = cac_form(check_class(C), C, Exports, Imports),
    classify_and_collect_forms(Rest, [R | Acc], Refs++RefsAcc, MoreExports++Exports, MoreImports++Imports).

cac_form(function, Tokens, Exports, Imports) ->
    get_function(Tokens, Exports, Imports);
cac_form(attribute, Attribute, _Exports, _Imports) ->
    get_attribute(Attribute);
cac_form(other, [#token{value=Name, line=Line, offset=Offset, length=Length} | _], 
    _Exports, _Imports) ->
    {#other{pos={{Line, Line, Offset}, Length}, name=Name}, [], [], []};
cac_form(_, _D, _E, _I) ->
    {eof, [], [], []}.

%% special arity flags
-define(ARI_TYPESPEC, -2).
-define(ARI_ATTRIBUTE, -3).
-define(ARI_RECORD_DEF, -4).
-define(ARI_MACRO_DEF, -5).
-define(ARI_INCLUDE, -6).
-define(ARI_RECORD_FIELD_DEF, -7).

get_type_attribute(Kind, Name0, Offset, Line, Attribute, Args) ->
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
    Arity = case Kind of
                'spec' ->
                    erlide_text:guess_arity(Args);
                _ ->
                    -1
            end,
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra, arity=Arity},
     [#ref{data=#type_def{type=Name}, offset=Offset, length=PosLength, function=Name, 
           arity=?ARI_TYPESPEC, clause="", sub_clause=false} | ExternalRefs], [], []}.

get_function(Tokens, Exports, Imports) ->
    ClauseList = split_clauses(Tokens),
    ClausesAndRefs = get_clauses(ClauseList),
    [{#clause{pos=P, name=Name, args=Arguments, head=H, name_pos=NP}, _Refs} | _] = ClausesAndRefs,
    Arity = length(Arguments),
    Exported = lists:member({Name, Arity}, Exports), 
    Function = case ClausesAndRefs of   % only show subclauses when more than one
                   [_] ->
                       #function{pos=P, name=Name, arity=Arity, args=Arguments, head=H, 
                                 clauses=[], name_pos=NP, exported=Exported};
                   _ ->
                       Clauses = [C || {C, _} <- ClausesAndRefs],
                       #function{pos=P, name=Name, arity=Arity, clauses=Clauses, 
                                 name_pos=NP, exported=Exported}
               end,
    {Function, fix_code_refs(ClausesAndRefs, Function, Imports), [], []}.

get_attribute([#token{kind='-', offset=Offset, line=Line},
               #token{kind=Kind, line=_Line, offset=_Offset, value=Name} | Args] = Attribute)
  when (Kind=:='spec') or ((Kind=:=atom) and (Name=:='type'))
           or ((Kind=:=atom) and (Name=:='opaque'))->
    %% -spec, -type or -opaque
    get_type_attribute(Kind, Name, Offset, Line, Attribute, Args);
get_attribute([#token{kind='-', offset=Offset, line=Line},
               #token{kind=atom, value=Name, line=_Line, offset=_Offset},
               _, #token{value=Args} | _] = Attribute) ->
    %% other attributes
    get_other_attribute(Name, Offset, Line, Attribute, Args);
get_attribute([_, #token{kind=atom, value=Name, 
                         line=Line, offset=Offset}
               | _] = Attribute) ->
    #token{line=LastLine, offset=LastOffset, 
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=[]}, [], [], []}.

get_other_attribute(Name, Offset, Line, Attribute, Args) ->
    #token{line=LastLine, offset=LastOffset, 
           length=LastLength} = last_not_eof(Attribute),
    PosLength = LastOffset - Offset + LastLength,
    Between = erlide_np_util:get_between_outer_pars(Attribute, '(', ')'),
    Extra = to_string(Between),
    {AttrArgs, Exports, Imports} = get_attribute_args(Name, Between, Args),
    {#attribute{pos={{Line, LastLine, Offset}, PosLength},
                name=Name, args=AttrArgs, extra=Extra},
     make_attribute_ref(Name, AttrArgs, Extra, Offset, PosLength)++
         make_attribute_arg_refs(Name, AttrArgs, Between),
     Exports, Imports}.

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

get_clauses(Clauses) ->
    get_clauses(Clauses, []).

get_clauses([], Acc) ->
    lists:reverse(Acc);
get_clauses([C | Rest], Acc) ->
    {Clause, Refs} = get_clause(C),
    get_clauses(Rest, [{Clause, Refs} | Acc]).

get_clause([#token{kind=AtomOrMacro, value=Name, line=Line, offset=Offset, length=Length} | Rest]) 
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

fix_code_refs(ClausesAndRefs, Function, Imports) ->
    fix_code_refs(ClausesAndRefs, Function, Imports, []).

fix_code_refs([], #function{name=Name, arity=Arity, name_pos={{_, Offset}, Length}}, _Imports, Acc) ->
    Ref = #ref{data=#function_def{function=Name, arity=Arity}, offset=Offset, 
               length=Length, function=Name, arity=Arity, clause="", sub_clause=false},
    [Ref | Acc];
fix_code_refs([{Clause, Refs} | Rest], Function, Imports, Acc0) ->
    Acc1 = fix_code_refs(Refs, Clause, Function, Imports, Acc0),
    fix_code_refs(Rest, Function, Imports, Acc1).

fix_code_refs([], _Clause, _Function, _Imports, Acc) ->
    Acc;
fix_code_refs([{Offset, Length, RefData} | Rest], #clause{head=Head}=Clause, 
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
    fix_code_refs(Rest, Clause, Function, Imports, [Ref | Acc]).

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
get_record_field_defs([{FieldName, {{_, _, Offset}, Length}, _Extra} |�Rest],
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
    Name = case Between of
               {N, _} -> N;
               N -> N
           end,
    R= #record_def{record=Name},
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
