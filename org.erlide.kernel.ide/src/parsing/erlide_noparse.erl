%% Author: jakob
%% Created: Mar 23, 2006
%% Description: TODO: Add description to erlide_noparse

-module(erlide_noparse).

%%
%% Exported Functions
%%

-export([initial_parse/6, reparse/1]).

-compile(export_all).

%%
%% Include files
%%

-define(DEBUG, 1).
%% -define(IO_FORMAT_DEBUG, 1).

-define(CACHE_VERSION, 19).
-define(SERVER, erlide_noparse).

-include("erlide.hrl").
-include("erlide_scanner.hrl").

%%
%% API Functions
%%

initial_parse(ScannerName, ModuleFileName, InitialTextBin, 
              StateDir, ErlidePath, UpdateCaches) ->
    InitialText = binary_to_list(InitialTextBin),
    try
        ?D({StateDir, ModuleFileName, ErlidePath}),
        RenewFun = fun(_F) ->
                           do_parse(ScannerName, ModuleFileName, 
                                    InitialText, StateDir, ErlidePath,
                                    UpdateCaches) 
                   end,
        CacheFun = fun(D) ->
                           erlide_scanner_server:initialScan(
                             ScannerName, ModuleFileName, InitialText,
                             StateDir, ErlidePath, UpdateCaches),
                           D
                   end,
        CacheFileName = filename:join(StateDir, atom_to_list(ScannerName) ++ ".noparse"),
        ?D(CacheFileName),
        {Cached, Res} = erlide_util:check_and_renew_cached(ModuleFileName, CacheFileName,
                                                           ?CACHE_VERSION, RenewFun, CacheFun,
                                                           UpdateCaches),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        ?D(updated),
        {ok, Res, Cached}
    catch
        error:Reason ->
            {error, Reason}
    end.

reparse(ScannerName) ->
    try
        Res = do_parse(ScannerName, "", "", "", "", false),
        %%erlide_noparse_server:update_state(ScannerName, Res),
        {ok, Res, unused}
    catch
        error:Reason ->
            {error, Reason}
    end.

%%
%% Internal functions
%%

-record(model, {forms, comments}).

-record(function, {pos, name, arity, args, head, clauses, name_pos, code_dont_use, external_refs_dont_use, comment}).
-record(clause, {pos, name, args, head, code_dont_use, name_pos, external_refs_dont_use}).
-record(attribute, {pos, name, args, extra}).
-record(other, {pos, name, tokens}).
-record(module, {name, erlide_path, model}).
-record(position, {offset, length}).
-record(external_call, {module, function, arity, position}).
-record(macro_ref, {macro, position}).
-record(record_ref, {record, position}).

do_parse(ScannerName, ModuleFileName, InitalText, StateDir, ErlidePath, UpdateCaches) ->
    Toks = scan(ScannerName, ModuleFileName, InitalText, StateDir, ErlidePath, UpdateCaches),
	do_parse2(ScannerName, Toks, ErlidePath).

do_parse2(_ScannerName, Toks, _ErlidePath) ->
    ?D({do_parse, _ScannerName, length(Toks)}),
    {UncommentToks, Comments} = extract_comments(Toks),
    %?D({length(UncommentToks), length(Comments)}),
    %?D({UncommentToks}),
    Functions = split_after_dots(UncommentToks, [], []),
    ?D(length(Functions)),
    Collected = [classify_and_collect(I) || I <- Functions, I =/= [eof]],
    ?D(length(Collected)),
    CommentedCollected = get_function_comments(Collected, Comments),
    Model = #model{forms=CommentedCollected, comments=Comments},
    %%erlide_noparse_server:create(ScannerName, Model, ErlidePath),
    ?D({"Model", length(Model#model.forms), erts_debug:flat_size(Model)}),
    FixedModel = fixup_model(Model),
    ?D(erts_debug:flat_size(FixedModel)),
    %% 	?D([erts_debug:flat_size(F) || F <- element(2, FixedModel)]),
    FixedModel.

fixup_model(#model{forms=Forms, comments=Comments}) ->
    ?D(a),
    FixedComments = fixup_tokens(Comments),
    ?D(a),
    FixedForms = fixup_forms(Forms),
    ?D(a),
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

fixup_form(#function{comment=Comment, clauses=Clauses} = Function) ->
	Function#function{comment= to_binary(Comment), clauses=fixup_forms(Clauses), args=[]};
fixup_form(#clause{head=Head} = Clause) ->
	Clause#clause{head=to_binary(Head), args=[]};
fixup_form(Other) ->
	Other.

to_binary(Comment) when is_list(Comment) ->
	iolist_to_binary(Comment);
to_binary(Other) ->
	Other.

parse_test(ScannerName, File) ->
    erlide_scanner_server:scan_uncached(ScannerName, File, ""),
    Toks = erlide_scanner_server:getTokens(ScannerName),
    {UncommentToks, Comments} = extract_comments(Toks),
    Functions = split_after_dots(UncommentToks, [], []),
    Collected = [classify_and_collect(I) || I <- Functions, I =/= [eof]],
    _Model = #model{forms=Collected, comments=Comments},
    %%erlide_noparse_server:create(ScannerName, Model, ""),
    ok.

classify_and_collect(C) ->
    %?D(C),
    R = cac(check_class(C), C),
    %?D(R),
    R.

cac(function, Tokens) ->
    ClauseList = split_clauses(Tokens),
    %?D(ClauseList),
    Clauses = [fix_clause(C) || C <- ClauseList],
    %?D(length(Clauses)),
    [#clause{pos=P, name=N, args=A, head=H, name_pos=NP} | _] = Clauses,
    Arity = erlide_text:guess_arity(A),
    case Clauses of					% only show subclauses when more than one
        [_] ->
            #function{pos=P, name=N, arity=Arity, args=A, head=H, clauses=[], name_pos=NP};
        _ ->
            #function{pos=P, name=N, arity=Arity, clauses=Clauses, name_pos=NP}
    end;
cac(attribute, Attribute) ->
    ?D(Attribute),
    case Attribute of
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=Kind, line=_Line, offset=_Offset, value=Value} | Args]
          when (Kind=:='spec') or ((Kind=:=atom) and (Value=:='type'))
                   or ((Kind=:=atom) and (Value=:='opaque'))->
            ?D(Value),
            Name = case Kind of 'spec' -> Kind; _ -> Value end,
            #token{line=LastLine, offset=LastOffset,
                   length=LastLength} = last_not_eof(Attribute),
            PosLength = LastOffset - Offset + LastLength,
            ?D(Args),
            Extra = to_string(Args),
            ?D(Extra),
            #attribute{pos={{Line, LastLine, Offset}, PosLength},
                       name=Name, args=get_attribute_args(Kind, Args, Args), extra=Extra};
        [#token{kind='-', offset=Offset, line=Line},
         #token{kind=atom, value=Name, line=_Line, offset=_Offset},
         _, #token{value=Args} | _] = Attribute ->
            #token{line=LastLine, offset=LastOffset, 
                   length=LastLength} = last_not_eof(Attribute),
            PosLength = LastOffset - Offset + LastLength,
            Between = get_between_outer_pars(Attribute),
            Extra = to_string(Between),
            #attribute{pos={{Line, LastLine, Offset}, PosLength},
                       name=Name, args=get_attribute_args(Name, Between, Args), extra=Extra};
        [_, #token{kind=atom, value=Name, line=Line, offset=Offset} | _] ->
            #token{line=LastLine, offset=LastOffset, 
                   length=LastLength} = last_not_eof(Attribute),
            PosLength = LastOffset - Offset + LastLength,
            #attribute{pos={{Line, LastLine, Offset}, PosLength},
                       name=Name, args=[]}
    end;
cac(other, [#token{value=Name, line=Line,
                   offset=Offset, length=Length} | _]) ->
    #other{pos={{Line, Line, Offset}, Length}, name=Name};
cac(_, _D) ->
    %?D(_D),
    eof.

get_attribute_args(import, Between, _Args) ->
    From = get_first_of_kind(atom, Between),
    Tokens = get_between(Between, '[', ']'),
    {From, fun_arity_from_tokens(Tokens)};
get_attribute_args(export, Between, _Args) ->
    Tokens = get_between(Between, '[', ']'),
    fun_arity_from_tokens(Tokens);
get_attribute_args(record, Between, _Args) ->
	%?D({Between, _Args}),
    RecordToken = hd(Between),
	RecordName = RecordToken#token.value,
    Tokens = get_between(Between, '{', '}'),
    {RecordName, field_list_from_tokens(Tokens)};
get_attribute_args(_, _Between, Args) ->
    Args.

check_class([#token{kind = atom}, #token{kind = '('} | _]) ->
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
    case skip_to(Tokens, Kind) of
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

field_list_from_tokens([#token{kind=atom, value=Field} | Rest]) ->
    [Field | field_list_from_tokens(skip_to(Rest, ','))];
field_list_from_tokens([_ | Rest]) ->
    field_list_from_tokens(Rest);
field_list_from_tokens(_) ->
    [].

to_string(Tokens) ->
    S = erlide_scanner:tokens_to_string(Tokens),
    erlide_text:strip(S).
%%     unspacify(S).

unspacify(S) ->
    unspacify(S, false, "").

is_space($\s) -> true;
is_space($\t) -> true;
is_space(_) -> false.

unspacify([], _PrevSpace, Acc) ->
    lists:reverse(Acc);
unspacify([C | Rest], true, Acc) ->
    case is_space(C) of
       true -> unspacify(Rest, true, Acc);
       false -> unspacify(Rest, false, [C | Acc])
    end;
unspacify([C | Rest], false, Acc) ->
    case is_space(C) of
       true -> unspacify(Rest, true, Acc);
       false -> unspacify(Rest, is_space(C), [C | Acc])
    end.

get_head(T) ->
    case get_args(T) of
        "" ->
            "";
        A ->
            case get_guards(T) of
                "" ->
                    A;
                G ->
                    A ++ " when " ++ G
            end
    end.

get_args(T) ->
    case get_between_outer_pars(T) of
        "" ->
            "";
        P ->
            "("++to_string(P)++")"
    end.

get_between_outer_pars(T) ->
    case skip_to(T, '(') of
        [] ->
            [];
        [_ | S] ->
            {R, _Rest} = gbop(S),
            lists:reverse(tl(lists:reverse(R)))
    end.

gbop([]) ->
    {[], []};
gbop([eof | _]) ->
    {[], []};
gbop([#token{kind=')'}=T | Rest]) ->
    {[T], Rest};
gbop([#token{kind='('}=T | Rest]) ->
    {R, Rest1} = gbop(Rest),
    {R2, Rest2} = gbop(Rest1),
    {[T] ++ R ++ R2, Rest2};
gbop([T | Rest]) ->
    {R, Rest1} = gbop(Rest),
    {[T] ++ R, Rest1}.

get_guards(T) ->
    to_string(get_between(T, 'when', '->')). 

get_between_pars(T) ->
    get_between(T, '(', ')').

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

skip_to([], _Delim) ->
    [];
skip_to([#token{kind=Delim} | _] = L, Delim) ->
    L;
skip_to([_ | Rest], Delim) ->
    skip_to(Rest, Delim).

%% get_between([], _A, _B) ->
%%     [];
%% get_between([#token{kind = A} | Rest], A, B) ->
%%     get_between2(Rest, B, []);
%% get_between([_ | Rest], A, B) ->
%%     get_between(Rest, A, B).
%% 
%% get_between2([], _B, Acc) ->
%%     lists:reverse(Acc);
%% get_between2([#token{kind = B} | _], B, Acc) ->
%%     get_between2([], B, Acc);
%% get_between2([T | Rest], B, Acc) ->
%%     get_between2(Rest, B, [T | Acc]).

reverse2(L) ->
    lists:reverse([lists:reverse(A) || A <- L]).

split_after_dots([], Acc, []) ->
    reverse2(Acc);
split_after_dots([], Acc, FunAcc) ->
    split_after_dots([], [FunAcc | Acc], []);
split_after_dots([#token{kind = eof} | _], Acc, FunAcc) ->
    split_after_dots([], Acc, FunAcc);
split_after_dots([T = #token{kind = dot} | TRest], Acc, FunAcc) ->
    split_after_dots(TRest, [[T | FunAcc] | Acc], []);
split_after_dots([T | TRest], Acc, FunAcc) ->
    split_after_dots(TRest, Acc, [T | FunAcc]).

check_clause([#token{kind = ';'} | Rest]) ->
    check_class(Rest) == function;
check_clause(_) ->
    false.

split_clauses(F) ->
    split_clauses(F, [], []).

split_clauses([], Acc, []) ->
    reverse2(Acc);
split_clauses([], Acc, ClAcc) ->
    split_clauses([], [ClAcc | Acc], []);
split_clauses([T | TRest] = Tokens, Acc, ClAcc) ->
    case check_clause(Tokens) of
        false ->
            split_clauses(TRest, Acc, [T | ClAcc]);
        true ->
            split_clauses(TRest, [[T | ClAcc] | Acc], [])
    end.

%% fix_clause([#token{kind=atom, value=Name, line=Line, offset=Offset, length=Length} | Rest] = Code) ->
fix_clause([#token{kind=atom, value=Name, line=Line, offset=Offset, length=Length} | Rest]) ->
    #token{line=LastLine, offset=LastOffset, length=LastLength} = last_not_eof(Rest),
    PosLength = LastOffset - Offset + LastLength,
    ExternalRefs = get_refs(Rest),
    ?D([Rest, ExternalRefs]),
    #clause{pos={{Line, LastLine, Offset}, PosLength}, name_pos={{Line, Offset}, Length},
%%             name=Name, args=get_between_pars(Rest), head=get_head(Rest), code=Code,
            name=Name, args=get_between_pars(Rest), head=get_head(Rest) 
            }.

scan(ScannerName, "", _, _, _, _) -> % reparse, just get the tokens, they are updated by reconciler 
    erlide_scanner_server:getTokens(ScannerName);    
scan(ScannerName, ModuleFileName, InitialText, StateDir, ErlidePath, UpdateCaches) ->
    _D = erlide_scanner_server:initialScan(ScannerName, ModuleFileName, InitialText, StateDir, ErlidePath, UpdateCaches),
	?D(_D),
    S = erlide_scanner_server:getTokens(ScannerName),
    S.

%% ex(Module) ->
%%     Model = Module#module.model,
%%     Forms = Model#model.forms,
%%     external_calls(Forms).
%% 
%% external_calls(Functions) ->
%%     lists:foldl(fun(F, A) ->
%%                         external_calls_f(F)++A
%%                 end, [], Functions).
%% 
%% external_calls_f(#function{clauses=[], code=Tokens}) ->
%%     case external_calls_code(Tokens) of
%%         [] -> [];
%%         L -> L
%%     end;
%% external_calls_f(#function{clauses=Clauses}) ->
%%     lists:foldl(fun(C, A) ->
%%                         case external_calls_cl(C) of
%%                             [] -> A;
%%                             L -> L++A
%%                         end
%%                 end, [], Clauses);
%% external_calls_f(_) ->
%%     [].
%% 
%% external_calls_cl(#clause{code=Tokens}) ->
%%     external_calls_code(Tokens).


%% find references in code
%% VERY simple, M:F(_ ...), #R, ?M, and that's it 
get_refs([]) ->
    [];
get_refs([#token{kind=atom, value=M, offset=Offset}, #token{kind=':'},
                     #token{kind=atom, value=F, offset=Offset2, length=Length2},
                     #token{kind='('} | Rest]) ->
    Arity = erlide_text:guess_arity(Rest),
    Position = #position{offset=Offset, length=Length2+Offset2-Offset},
    XC = #external_call{module = M, function=F, arity=Arity, position=Position},
    [XC | get_refs(Rest)];
get_refs([#token{kind='?', offset=Offset}, 
          #token{kind=atom, value=_M, text=M, offset=Offset2, length=Length2} | Rest]) ->
    Position = #position{offset=Offset, length=Length2+Offset2-Offset},
    MR = #macro_ref{macro=M, position=Position},
    [MR | get_refs(Rest)];
get_refs([#token{kind='#', offset=Offset}, 
          #token{kind=atom, value=_R, offset=Offset2, length=Length2, text=R} | Rest]) ->
    Position = #position{offset=Offset, length=Length2+Offset2-Offset},
    RR = #record_ref{record=R, position=Position},
    [RR | get_refs(Rest)];
get_refs([_|Rest]) ->
    get_refs(Rest).

%% @spec (Tokens::tokens()) -> {tokens(), tokens()}
%% @type tokens() = [#token]
%%
%% @doc extract comments from tokens, and concatenate multiline comments to one token

extract_comments(Tokens) ->
    extract_comments(Tokens, -1, [], []).

extract_comments([], _, TAcc, CAcc) ->
    {lists:reverse(TAcc), lists:reverse(CAcc)};
extract_comments([#token{kind=comment, offset=ONext, length=LNext, line=NNext,
                         value=VNext}
                  | Rest], NNext, TAcc,
                 [#token{kind=comment, offset=O, value=V}=C | CAcc]) ->
    NewComment = C#token{offset=O, length=ONext-O+LNext, value=V++"\n"++VNext,
                         last_line=NNext},
    extract_comments(Rest, NNext+1, TAcc, [NewComment | CAcc]);
extract_comments([C = #token{kind=comment, line=N} | Rest], _, TAcc, CAcc) ->
    extract_comments(Rest, N+1, TAcc, [C | CAcc]);
extract_comments([T | Rest], _, TAcc, CAcc) ->
    extract_comments(Rest, -1, [T | TAcc], CAcc).


is_var(#token{kind=var}) -> true;
is_var(_) -> false.

vars(Tokens) when is_list(Tokens) ->
    [Token || Token <- Tokens, is_var(Token)];
vars(_) ->
    [].

%% do_vdump1(#module{model=M}) ->
%%     Forms = M#model.forms,
%%     [[{F#function.name, vars(F#function.code), [{C#clause.head, vars(C#clause.code)} || C <- F#function.clauses]}]
%%     || F <- Forms, is_record(F, function)].
    
%% do_get_vars(FOrC, Module) ->
%%     case find_function_or_clause(FOrC, Module) of
%%         #function{code=Code} ->
%%             {ok, vars(Code)};
%%         #clause{code=Code} ->
%%             {ok, vars(Code)};
%%         _ ->
%%             not_found
%%     end.

find_function_or_clause({F, A, Head}, Module) ->
    ffoc((Module#module.model)#model.forms, F, A, Head);
find_function_or_clause({F, A}, Module) ->
    ffoc((Module#module.model)#model.forms, F, A).

ffoc([], _F, _A) ->
    not_found;
ffoc([#function{name=F, arity=A} = Function | _], F, A) ->
    Function;
ffoc([_ | R], F, A) ->
    ffoc(R, F, A).

ffoc(L, F, A, Head) ->
    case ffoc(L, F, A) of
        #function{clauses=C} ->
            ffoc(C, Head);
        not_found ->
            not_found
    end.

ffoc([], _Head) ->
    not_found;
ffoc([#clause{head=Head} = Clause | _], Head) ->
    Clause;
ffoc([_ | R], Head) ->
    ffoc(R, Head).

       
get_function_comments(Forms, Comments) ->
    lists:map(fun(#function{} = F) ->
		      get_function_comment(F, Comments);
		 (Other) ->
		      Other
	      end, Forms).

get_function_comment(F, []) ->
    F;
get_function_comment(#function{name_pos={{Line, _}, _}}=F, [#token{last_line=LastLine, value=Value} | _])
  when LastLine+1=:=Line; LastLine+2=:=Line->
    F#function{comment=erlide_text:strip_percents_and_spaces(Value)};
get_function_comment(#function{name_pos={{Line, _}, _}}=F, [#token{line=LastLine, last_line=undefined, value=Value} | _])
  when LastLine+1=:=Line; LastLine+2=:=Line->
    F#function{comment=erlide_text:strip_percents_and_spaces(Value)};
get_function_comment(F, [_ | Rest]) ->
    get_function_comment(F, Rest).


