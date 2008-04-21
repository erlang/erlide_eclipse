%% Author: jakob
%% Created: 2006-jan-28
%% Description: 
%% TODO: Add description to erlide_indent
-module(erlide_indent).

%%
%% Include files
%%

%%
%% Exported Functions
%%

-export([indent_line/5,
         indent_lines/4]).

%-define(IO_FORMAT_DEBUG, 1).
-define(DEBUG, 1).

-include("erlide.hrl").
-include("erlide_scanner.hrl").

default_indent_prefs() ->
    [{before_binary_op, 4},
     {after_binary_op, 4},
     {before_arrow, 2},
     {after_arrow, 4},
     {after_unary_op, 4},
     {clause, 4},
     {'case', 4},
     {'try', 4},
     {'catch', 4},
     {function_parameters, 2},
     {'fun', 3},
     {fun_body, 5}].

%%
%% API Functions
%%
indent_line(St, OldLine, CommandText, Tablength, Prefs) ->
    indent_line(St, OldLine, CommandText, -1, Tablength, Prefs).

indent_line(St, OldLine, CommandText, N, Tablength, Prefs) ->
    S = erlide_text:detab(St, Tablength),
    ?D(OldLine),
    case erlide_scan:string(S ++ string:strip(CommandText, left)) of
        {ok, T, _} ->
            LineOffsets = erlide_text:get_line_offsets(S),
            Tr = fix_tokens(T, size(LineOffsets)),
            LineN = case N of
                        -1 ->
                            size(LineOffsets)+1;
                        _ ->
                            N
                    end,
            I = indent(Tr, LineOffsets, LineN, Prefs),
            {I, initial_whitespace(OldLine)};
        _  ->
            error
    end.

fix_tokens(Tokens, NL) ->
    [erlide_scanner:mktoken(T, 0, 0) || T <- Tokens] ++ [#token{kind=eof, line=NL+1}].

-record(i, {anchor, indent_line, current, prefs}).

get_prefs([], OldP, Acc) ->
    Acc ++ OldP;
get_prefs([{Key, Value} | Rest], OldP, Acc) ->
    P = lists:keydelete(Key, 1, OldP),
    get_prefs(Rest, P, [{Key, Value} | Acc]).

get_prefs(Prefs) ->
    get_prefs(Prefs, default_indent_prefs(), []).

indent(Tokens, LineOffsets, LineN, Prefs) ->
    try
        ?D(Prefs),
        P = get_prefs(Prefs),
		?D(P),
        I = #i{anchor=hd(Tokens), indent_line=LineN, current=0, prefs=P},
        ?D({I, LineOffsets}),
        i_form_list(Tokens, I),
        ?D(no_catch)
    catch
        throw:{indent, A, C} ->
            ?D({indent, A, C}),
            get_indent_of(A, C, LineOffsets);
        throw:{indent, N} ->
            ?D(N),
            N
    end.

get_indent_of(_A = #token{kind=eof}, C, _LineOffsets) ->
    C;
get_indent_of(_A = #token{line=N, offset=O}, C, LineOffsets) ->
    LO = element(N, LineOffsets),
    TI = O - LO,
    ?D({O, LO, C, _A}),
    TI+C.

indent_lines(S, From, Tablength, Prefs) ->
    ?D(S),
    {First, FirstLineNum, Lines} = erlide_text:get_text_and_lines(S, From),
    do_indent_lines(Lines, Tablength, First, Prefs, FirstLineNum, "").

%%
%% Local Functions
%%

%% TODO: Add description of asd/function_arity
%%
do_indent_lines([], _, _, _, _, A) ->
    A;
do_indent_lines([Line | Rest], Tablength, Text, Prefs, N, Acc) ->
    ?D({Text++Acc, Line}),
    {NewI, _OldI} = indent_line(Text ++ Acc, Line, N, Tablength, Prefs),
    NewLine = reindent_line(Line, NewI),
    ?D({NewI, _OldI, Line, NewLine}),
    do_indent_lines(Rest, Tablength, Text, Prefs, N+1, Acc ++ NewLine).

%% TODO: Add description of asd/function_arity
%%
reindent_line(" " ++ S, I) ->
    reindent_line(S, I);
reindent_line("\t" ++ S, I) ->
    reindent_line(S, I);
reindent_line(S, I) ->
    lists:duplicate(I, $ )++S.

%% indent_of(S, Tablength) ->
%%     indent_of(S, Tablength, 0).
%% 
%% indent_of("\t"++S, Tablength, I) ->
%%     indent_of(S, Tablength, I + Tablength - I rem Tablength);
%% indent_of(" "++S, Tablength, I) ->
%%    	indent_of(S, Tablength, I + 1);
%% indent_of(_, _, I) ->
%% 	I.
 
initial_whitespace(" " ++ S) ->
    1 + initial_whitespace(S);
initial_whitespace("\t" ++ S) ->
    1 + initial_whitespace(S);
initial_whitespace(_) ->
    0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

i_check_aux(#token{line=K}, #i{indent_line=L, anchor=A, current=C}) when K >= L ->
    throw({indent, A, C});
i_check_aux(eof, #i{anchor=A, current=C}) ->
    throw({indent_eof, A, C});
i_check_aux([], I) ->
    i_check_aux(eof, I);
i_check_aux([T | _], I) ->
    i_check_aux(T, I);
i_check_aux(_, _) ->
    not_yet.

i_check(T, I) ->
    case i_check_aux(T, I) of
        not_yet ->
            not_yet;
        Throw ->
            throw(Throw)
    end.

indent_by(Key, Prefs) ->
    proplists:get_value(Key, Prefs, 0).

%% indent_by(before_binary_op, Prefs) -> Prefs#indent_prefs.before_binary_op;
%% indent_by(after_binary_op, Prefs) -> Prefs#indent_prefs.after_binary_op;
%% indent_by(before_arrow, Prefs) -> Prefs#indent_prefs.before_arrow;
%% indent_by(after_arrow, Prefs) -> Prefs#indent_prefs.after_arrow;
%% indent_by(after_unary_op, Prefs) -> Prefs#indent_prefs.after_unary_op;
%% indent_by(clause, Prefs) -> Prefs#indent_prefs.clause;
%% indent_by('case', Prefs) -> Prefs#indent_prefs.'case';
%% indent_by('try', Prefs) -> Prefs#indent_prefs.'try';
%% indent_by('catch', Prefs) -> Prefs#indent_prefs.'catch';
%% indent_by(function_parameters, Prefs) -> Prefs#indent_prefs.function_parameters;
%% indent_by('fun', Prefs) -> Prefs#indent_prefs.'fun'.

i_with(W, I) ->
    I#i{current=indent_by(W, I#i.prefs)}.

i_with(W, A, I) ->
    I#i{current=indent_by(W, I#i.prefs), anchor=A}.

i_par_list(R0, I0) ->
    R1 = i_paren('(', R0, I0),
    R2 = i_parameters(R1, I0),
    i_end_paren(R2, I0).

i_expr([], _I) ->
    {[], eof};
i_expr(R, I) ->
    i_check(R, I),
    R0 = i_comments(R, I),
    [A | _] = R, % R0
    I0 = I#i{anchor=A, current=0},
    R1 = i_1_expr(R0, I0),
    i_expr_rest(R1, I0, A).

i_expr_rest(R0, I, A) ->
    case i_sniff(R0) of
        #token{kind='('} -> % function call
		    I1 = i_with(function_parameters, A, I),
            R1 = i_par_list(R0, I1),
            i_expr_rest(R1, I1, A);
        eof ->
            {R0, A};
        #token{kind='#'} -> % record something
            i_record_something(R0, I);
        #token{kind=':'} -> % external function call
            R1 = i_kind(':', R0, I),
            R2 = i_1_expr(R1, I),
            {R3, A1} = i_expr_rest(R2, I, A),
            {R3, A1};
        #token{kind='||'} -> % list comprehension
            R1 = i_kind('||', R0, I),
            R2 = i_expr_list(R1, I),
            {R2, A};
        O ->
            case is_binary_op(O) of
                true ->
                    R1 = i_binary_op(R0, i_with(before_binary_op, I)),
                    i_expr(R1, i_with(after_binary_op, I));
                false ->
                    {R0, A}
            end
    end.

i_expr_list(R, I) ->
    i_check(R, I),
    R0 = i_comments(R, I),
    ?D(R0),
    {R1, _A} = i_expr(R0, I),
    R2 = i_comments(R1, I),
    case i_sniff(R2) of
        #token{kind=','} ->
            R3 = i_comma(R2, I),
            i_expr_list(R3, I);
        _ ->
            R2
    end.

i_binary_op(R0, I) ->
    i_check(hd(R0), I),
    [_ | R1] = i_comments(R0, I),
    R1.

i_end_paren_or_expr_list(R, I) ->
	i_check(R, I),
    case i_sniff(R) of
        #token{kind=Kind} when Kind=='}'; Kind==']'; Kind==')' ->
            i_kind(Kind, R, I);
        _ ->
            I1 = I#i{anchor=hd(R), current=0},
            i_expr_list(R, I1)
    end.

i_1_expr([#token{kind=comment}=T | Rest], I) ->
    i_check(T, I),
    Rest;
i_1_expr([#token{kind=Kind}=T | Rest], I) when Kind=='{'; Kind=='[' ->
    i_check(T, I),
    I1 = I#i{anchor=T, current=1},
    ?D(Rest),
    Rest1 = i_end_paren_or_expr_list(Rest, I1),
    ?D(Rest1),
    i_end_paren(Rest1, I1);
i_1_expr([#token{kind='('}=T | Rest], I) ->
    ?D(Rest),
    i_check(T, I),
    I1 = I#i{anchor=T, current=1},
    Rest1 = i_1_expr(Rest, I1),
    i_end_paren(Rest1, I1);
i_1_expr([#token{kind=atom}=T | Rest], I) ->
    i_check(T, I),
    Rest;
i_1_expr([#token{kind=integer}=T | Rest], I) ->
    i_check(T, I),
    Rest;
i_1_expr([#token{kind=string}=T | Rest], I) ->
    i_check(T, I),
    Rest;
i_1_expr([#token{kind=float}=T | Rest], I) ->
    i_check(T, I),
    Rest;
i_1_expr([#token{kind=var}=T | Rest], I) ->
    i_check(T, I),
    Rest;
i_1_expr([#token{kind='#'} | _] = L, I) ->
    {R, _A} = i_record_something(L, I),
    R;
i_1_expr([#token{kind='case'}=T | R0], I) ->
    i_check(T, I),
    I1 = i_with('case', I#i{anchor=T}),
    {R1, _A} = i_expr(R0, I1),
    R2 = i_of(R1, I1),
    R3 = i_clause_list(R2, I1),
    i_block_end(T#token.kind, R3, I);
i_1_expr([#token{kind='fun'}=T | R0], I) ->
    I1 = i_with('fun', T, I),
    R1 = case i_sniff(R0) of
             #token{kind='('} ->
                 i_fun_clause_list(R0, I1);
             _ ->
                 {R01, _a} = i_expr(R0, I1),
                 R01
         end,
    i_kind('end', R1, I);
i_1_expr([#token{kind='try'}=T | R0], I) ->
    i_check(T, I),
    I1 = i_with('try', T, I),
    R1 = i_kind('try', R0, I1),
    R2 = i_expr_list(R1, I1),
    R3 = i_kind('catch', R2, I1),
    I2 = i_with('catch', hd(R2), I),
    R3 = i_clause_list(R3, I2),
    i_kind('end', R3, I2);
i_1_expr([T | Rest], I) ->
    i_check(T, I),
    case is_unary_op(T) of
        true ->
            i_1_expr(Rest, i_with(after_unary_op, T, I));
        false ->
            Rest
    end.

i_of([#token{kind='of'}=T | Rest], I) ->
    i_check(T, I),
    Rest.

is_binary_op(#token{kind=Kind}) ->
    erlide_text:is_op2(Kind).

is_unary_op(#token{kind=Kind}) ->
    erlide_text:is_op1(Kind).

%% is_block_begin(#token{kind=Kind}) ->
%%     erlide_text:is_block_start_token(Kind).

%% i_block_begin(L, I) ->
%%     i_one(L, I).

i_block_end(_Begin, L, I) ->
    i_one(L, I).

i_one(L, I) ->
    i_check(L, I),
    [_ | Rest] = i_comments(L, I),
    Rest.

i_parameters(R, I) ->
    i_check(R, I),
    case i_sniff(R) of
        #token{kind=')'} ->
            R;
        _ ->
            i_expr_list(R, I)
    end.

i_predicate_list(R, I) ->
    i_check(R, I),
    {R0, _A} = i_expr(R, I),
    case i_sniff(R0) of
        #token{kind=','} ->
            R1 = i_comma(R0, I),
            i_predicate_list(R1, I);
        #token{kind=';'} ->
            R1 = i_semicolon(R0, I),
            i_predicate_list(R1, I);
        _ ->
            R0
    end.

i_record_something([#token{kind='#'} | R0], I) ->
	R1 = i_comments(R0, I),
	A = hd(R1),
	R2 = i_kind(atom, R1, I),
    ?D(R2),
	case i_sniff(R2) of
		#token{kind='.'} ->
            R3 = i_kind('.', R2, I),
            {R4, _A} = i_expr(R3, I),
			?D(R4),
            {R4, A};
        #token{kind='{'} ->
			i_expr(R2, I);
		_ ->
            {R2, A}
    end.

comment_kind("%%%" ++ _) ->
    comment_3;
comment_kind("%%" ++ _) ->
    comment_2;
comment_kind("%" ++ _) ->
    comment_1;
comment_kind(_) ->
    comment_0.

%% i_comments([], _I) ->
%%     [];

i_comments([#token{kind=comment, value=V} = C | Rest], I) ->
    case comment_kind(V) of
        comment_3 ->
            case i_check_aux(C, I) of
                not_yet ->
                    not_yet;
                _ ->
                    throw({indent, 0})
            end;
        _ ->
            i_check(C, I)
    end,
    i_comments(Rest, I);
i_comments(Rest, _I) ->
    Rest.

skip_comments([]) ->
    [];
skip_comments([#token{kind=comment} | Rest]) ->
    skip_comments(Rest);
skip_comments(Rest) ->
    Rest.

i_comma(T, I) ->
    i_kind(',', T, I).

i_semicolon(T, I) ->
    i_kind(';', T, I).

i_dot(T, I) ->
    i_kind(dot, T, I).

i_kind(Kind, T, I) ->
    %%?D(Kind),
    i_check(T, I),
    CT = i_comments(T, I),
    [#token{kind=Kind} | Rest] = CT,
    Rest.

i_end_paren([#token{kind=Kind} | Rest] = T, I) when Kind==')'; Kind=='}'; Kind==']'; Kind=='>>' ->
    ?D(Rest),
    i_check(T, I),
    Rest;
i_end_paren(R, I) ->
    i_check(R, I),
    R.

i_paren(Kind, L, I) ->
    i_kind(Kind, L, I).

i_form_list(R0, I) ->
    R = i_form(R0, I),
    i_form_list(R, I).

i_form(R0, I) ->
    %%?D(R0),
	R1 = i_comments(R0, I),
    case i_sniff(R1) of
        #token{kind='-'} ->
            i_declaration(R1, I);
        _ ->
            R2 = i_clause(R1, I),
            case i_sniff(R2) of
                #token{kind=dot} ->
                    i_dot(R2, I);
                #token{kind=';'} ->
                    i_semicolon(R2, I);
                _ ->
                    R2
            end
    end.

i_declaration(R0, I) ->
    i_check(R0, I),
    R1 = i_kind('-', R0, I),
    {R, _A} = i_expr(R1, I),
    i_dot(R, I).

i_fun_clause(R0, I) ->
    ?D(R0),
    R1 = i_comments(R0, I),
    [A | _] = R1,
    R2 = i_par_list(R1, I),
    R3 = i_kind('->', R2, I),
	I1 = i_with(fun_body, A, I),
    i_expr_list(R3, I1).

i_fun_clause_list(R, I) ->
	?D(R),
    R0 = i_fun_clause(R, I),
    case i_sniff(R0) of
        #token{kind=';'} ->
            R1 = i_semicolon(R0, I),
            i_fun_clause_list(R1, I);
        _ ->
            R0
    end.

i_clause(R0, I) ->
    {R1, A} = i_expr(R0, I),
    %%?D(R1),
    I1 = i_with(before_arrow, A, I),
    R2 = case i_sniff(R1) of
             #token{kind='when'} ->
                 R11 = i_kind('when', R1, I1),
                 i_predicate_list(R11, I1);
             _ ->
                 R1
         end,
    R3 = i_kind('->', R2, I1),
    I2 = i_with(after_arrow, I1),
    i_expr_list(R3, I2).

i_clause_list(R, I) ->
    %%?D(R),
    R0 = i_clause(R, I),
    case i_sniff(R0) of
        #token{kind=';'} ->
            R1 = i_semicolon(R0, I),
            i_clause_list(R1, I);
        _ ->
            R0
    end.

i_sniff(L) ->
    case skip_comments(L) of
        [] ->
            eof;
        [T | _] ->
            T
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% get_indents/2
%%
%% get_indents("", _) ->
%%     {0};
%% get_indents(S, Tablength) ->
%%     get_indents(S, 0, true, Tablength, []).

%% get_indents("", _, _, _, Acc) ->
%%     list_to_tuple(lists:reverse(Acc));
%% get_indents(" " ++ Rest, I, B, T, Acc) ->
%%     get_indents(Rest, I+1, B, T, Acc);
%% get_indents("\t" ++ Rest, I, B, Tablength, Acc) ->
%%     J = I + Tablength - (I rem Tablength),
%%     get_indents(Rest, J, B, Tablength, Acc);
%% get_indents("\r\n" ++ Rest, _I, true, T, Acc) ->
%%     get_indents(Rest, 0, true, T, [0 | Acc]);
%% get_indents([EOL|Rest], _I, true, T, Acc) when EOL =:= $\n; EOL =:= $\r ->
%%     get_indents(Rest, 0, true, T, [0 | Acc]);
%% get_indents("\r\n"++Rest, _I, false, T, Acc) ->
%%     get_indents(Rest, 0, true, T, Acc);
%% get_indents([EOL|Rest], _I, false, T, Acc) when EOL =:= $\n; EOL =:= $\r ->
%%     get_indents(Rest, 0, true, T, Acc);
%% get_indents([_|Rest], I, true, T, Acc) ->
%%     get_indents(Rest, 0, false, T, [I | Acc]);
%% get_indents([_|Rest], _I, false, T, Acc) ->
%%     get_indents(Rest, 0, false, T, Acc).

%% TODO: Add description of asd/function_arity
%%
%% check_partial_expr(L) ->
%%     case erlide_text:is_op2(L) of
%%         true ->
%%             A = skip_to_expr_start(tail_if(L)),
%%             {partial, A};
%%         false ->
%%             check_paren_list(L)
%%     end.

%% check_paren_list([{T, _, _} | _] = L) when T =:= '('; T =:= '{'; T =:= '(' ->
%%     {paren, L};
%% check_paren_list([{',',_ ,_}|Before]) ->
%%      Start = skip_to_expr_start(Before),
%%      check_paren_list(tail_if(Start));
%% check_paren_list(_) ->
%%      false.

%% TODO: Add description of asd/function_arity
%%
%% skip_to_block_start([], _Semi) ->
%%     [];
%% skip_to_block_start([{';', _, _} | Before], Semi) ->
%%      Start = Before, %Start = skip_to_block_start(Before),
%%      skip_to_block_start(Start, Semi);
%% skip_to_block_start([{'end', _, _} | Before], Semi) ->
%%     Start0 = skip_to_block_start(Before, false),
%%     Start1 = tail_if(Start0),
%%     skip_to_block_start(Start1, Semi);
%% skip_to_block_start([{'->', _, _} | Before], Semi) ->
%%     case Semi of
%%         true ->
%%         %%?D(Before),
%%             A = skip_guard(Before),
%%         %%?D(A),
%%         A;
%%         false ->
%%             Before
%%     end;
%%     case is_function_declaration(Before) of
%%         true ->
%%             skip_to_fn_start(Before);
%%         false ->
%%             case Semi of
%%                 true ->
%%                     skip_guard(Before);
%%                 false ->
%%                     A = skip_to_expr_start(Before),
%%                     B = tail_if(A),
%%                     skip_to_block_start(B, Semi)
%%             end
%%     end;
%% skip_to_block_start([{'when', _, _} | Before], Semi) ->
%%     case Semi orelse is_function_declaration(Before) of
%%         true ->
%%             skip_to_fn_start_without_guards(Before);
%%         false ->
%%             A = skip_to_expr_start(Before),
%%             B = tail_if(A),
%%             skip_to_block_start(B, Semi)
%%     end;
%% skip_to_block_start([{',', _, _} | Before], Semi) ->
%%     Start = skip_to_expr_start(Before),
%%     B = tail_if(Start),
%%     skip_to_block_start(B, Semi);
%% skip_to_block_start([{'of', _, _} | Before], _Semi) ->
%%     Start0 = skip_to_expr_start(Before),
%%     _Start1 = tail_if(Start0);
%% skip_to_block_start([{T, _, _V} | _Before] = L, Semi) ->
%%     case erlide_text:is_block_start_token(T) of
%%         true ->
%%             L;
%%         false ->
%%             A = skip_to_expr_start(L),
%%             B = skip_to_block_start(tail_if(A), Semi),
%%             B
%%     end.

%% skip_to_fn_start_without_guards([{')', _, _} | _] = L) ->
%%     A = skip_to_expr_start(L),
%%     A; %% tail_if(A);
%% skip_to_fn_start_without_guards(L) ->
%%     tail_if(L).

%% skip to beginning of clause (containing ->)
%% to_clause_start([{'->', _, _V} | Before]) ->
%%     B = skip_guard(Before),
%%     to_expr_start(B);
%% to_clause_start([{T, _, _V} | _Before] = L) ->
%%     case erlide_text:is_block_start_token(T) of
%%         true ->
%%             L;
%%         false ->
%%             A = to_expr_start(L),
%%             ?D(A),
%%             to_clause_start(tail_if(A))
%%         end.

%% to_block_start([_, {'of', _, _} | Rest]) ->
%%     A = to_expr_start(Rest),
%%     tail_if(A);

%% to_block_start(L) ->    
%%     ?D(L),
%%     A = tail_if(L),
%%     [{T, _, _V} | _] = A,
%%     case erlide_text:is_block_start_token(T) of
%%     true ->
%%         A;
%%     false ->
%%         B = to_clause_start(A),
%%         to_block_start(B)
%%     end.

%% to_expr_start([{_T, _, _V} | _Before] = L) ->
%%        skip_to_expr_start(L).

%% TODO: Add description of asd/function_arity
%%
%% skip_to_fn_start([{')', _, _} | _] = L) ->
%%     skip_guard(L);
%% %%    skip_guard(L, L);
%% skip_to_fn_start(L) ->
%%     L.

%% skip past guards
%% skip_guard(L) ->
%%     A = skip_to_expr_start(L),
%%     skip_guard(A, L).

%% skip past guards
%% skip_guard([_, {',', _, _} | A], Old) ->
%%     B = skip_to_expr_start(A),
%%     skip_guard(B, Old);
%% skip_guard([_, {';', _, _} | A], Old) ->
%%     B = skip_to_expr_start(A),
%%     skip_guard(B, Old);
%% skip_guard([_, {'when', _, _} | A], _Old) ->
%%     skip_to_expr_start(A);
%% skip_guard(_A, Old) ->
%%     Old.

%% TODO: Add description of asd/function_arity
%%
%% is_function_declaration([{')', _, _} | _]) ->
%%     true;
%% is_function_declaration(_) ->
%%     false.

%% TODO: Add description of asd/function_arity
%%
%% skip_to_token(_T, []) ->
%%     [];
%% skip_to_token(T, [{T, _, _} | _] = L) ->
%%     L;
%% skip_to_token(T, [_ | R]) ->
%%     skip_to_token(T, R).

%% TODO: Add description of asd/function_arity
%%
%% get_token_indent([{T, _, _} | _]) ->
%%     get_token_indent(T);
%% get_token_indent('begin') ->
%%     4;
%% get_token_indent('of') ->
%%     4;
%% get_token_indent(op2) ->
%%     8;
%% get_token_indent('->') ->
%%     4;
%% get_token_indent(dot) ->
%%     0;
%% get_token_indent(';') ->
%%     0;
%% get_token_indent('(') ->
%%     1;
%% get_token_indent(_) ->
%%     0.

%% TODO: Add description of asd/function_arity
%%
%% indent_with(I, D) ->
%%     I+D.

%% indent_next_line(S, Tablength) ->
%%     case erlide_scan:string(S) of
%%         {ok, T, _} ->
%%             ?D(T),
%%             LineIndents = get_indents(S, Tablength),
%%             Tr = fix_scan_tuples(T),
%%             indent_after(Tr, LineIndents);
%%         _  ->
%%             0
%%     end.

%% TODO: Add description of asd/function_arity
%%
%% indent_after([{'->', _, _} | Before], Indents) ->
%%     Start = skip_guard(Before),
%%     indent_with(get_indent(Start, Indents), get_token_indent('->'));
%% indent_after([{dot, _, _} | _Before], _Indents) ->
%%     0; %    skip_and_indent('->', dot, Before, Indents);
%% indent_after([{';', _, _} | Before], Indents) ->
%%     Start = to_clause_start(Before),
%%     get_column(Start, Indents);
%%%%     Start = skip_to_block_start(Before, true),
%%%%     get_column(Start, Indents); %% skip_and_indent('->', ';', Before, Indents);
%% indent_after([{'of', _, _} | Before], Indents) ->
%%     Start0 = skip_to_expr_start(Before),
%%     Start1 = tail_if(Start0),
%%     Start2 = skip_to_token('case', Start1),
%%     indent_with(get_column(Start2, Indents), get_token_indent('of'));
%% indent_after([{',', _, _} | Before], Indents) ->
%%     case skip_to_expr_start(Before) of
%%         [_ | [{',', _, _} | _] = Rest] ->
%%             indent_after(Rest, Indents);
%%         Start ->
%%             get_column(Start, Indents)
%%     end;
%% indent_after([{T, _, _} | _] = L, Indents) ->
%%     case erlide_text:is_block_start_token(T) of
%%         true ->
%%             indent_with(get_column(L, Indents),
%%                         get_token_indent(L));
%%         false ->
%%             other_indent(L, Indents)
%%     end;
%% indent_after(L, Indents) ->
%%     other_indent(L, Indents).

%% other_indent(L, Indents) ->
%%     case check_partial_expr(L) of
%%         {partial, A} ->
%%             indent_with(get_column(A, Indents), get_token_indent(op2));
%%         {paren, B} -> % Can it really get here?
%%             indent_with(get_column(B, Indents), get_token_indent('('));
%%         _ ->
%%             Start = skip_to_block_start(L, false),
%%             get_column(Start, Indents)
%%     end.


%% TODO: Add description of asd/function_arity
%%
%% get_indent(Line, Indents) when is_integer(Line) ->
%%     element(Line, Indents);
%% get_indent([{_, {{Line, _}, _}, _} | _], Indents) ->
%%     element(Line, Indents).

%% skip_fn_call([_, {T, _, _} | _] = L)
%%   when T == ','; T == 'of'; T == '.'; T == 'after'; T == 'receive' ->
%%     L;
%% skip_fn_call([_|L]) ->
%%     skip_to_expr_start(L).

%% skip to the start of an expression, checking parens, operators etc
%%
%% skip_to_expr_start([{'end', _, _} | Before]) ->
%%     to_block_start(Before);
%% skip_to_expr_start([{')', _, _} | Before]) ->
%%     A = skip_paren(Before, '('),
%%     L = tail_if(A),
%%     case erlide_text:is_op2(L) of
%%         true ->
%%             skip_to_expr_start(A);
%%         false ->
%%             skip_fn_call(A)
%%     end;
%% skip_to_expr_start([{'}', _, _} | Before]) ->
%%     A = skip_paren(Before, '{'),
%%     case tail_if(A) of
%%         [{atom, _, _}, {'#', _, _} | Rest] ->  % handle record definitions
%%             skip_to_expr_start(Rest);
%%         L ->
%%             case erlide_text:is_op2(L) of
%%                 true ->
%%                     skip_to_expr_start(A);
%%                 false ->
%%                     A
%%             end
%%     end;
%% skip_to_expr_start([{']', _, _} | Before]) ->
%%     A = skip_paren(Before, '['),
%%     L = tail_if(A),
%%     ?D(L),
%%     case erlide_text:is_op2(L) of
%%         true ->
%%             skip_to_expr_start(A);
%%         false ->
%%             A
%%     end;
%% skip_to_expr_start([_Tup, {'->',_, _} | _] = L) ->
%%     L;
%% skip_to_expr_start([_Tup, {';',_, _} | _] = L) ->
%%     L;
%% skip_to_expr_start([_Tup, {',',_, _} | _] = L) ->
%%     L;
%% skip_to_expr_start([_Tup, {Op,_, _} | Before] = L) ->
%%     case erlide_text:is_op2(Op) of
%%         true ->
%%             skip_to_expr_start(Before);
%%         false ->
%%             L
%%     end;
%% skip_to_expr_start(L) ->
%%     L.

%% TODO: Add description of asd/function_arity
%%
%% skip_paren([], _) ->
%%     [];
%% skip_paren([{P, _, _} | _] = L, P) ->
%%     L;
%% skip_paren([{T, _, _} | Before], P) when T =:= '}'; T =:= ')'; T =:= ']' ->
%%     skip_paren(tail_if(skip_paren(Before, erlide_text:matching_paren(T))), P);
%% skip_paren([_ | Before], P) ->
%%     skip_paren(Before, P).

%% get_column for an element given as list
%%
%% get_column([], Indents) ->
%%     element(1, Indents);
%% get_column([{_, {{Line, Offs}, _}, _} = _T | _] = L, Indents) ->
%%     get_column(L, Line, Offs, Offs, Indents).

%% get_column([], Line, FirstOffs, PrevOffs, Indents) ->
%%     R = get_indent(Line, Indents) + FirstOffs - PrevOffs,
%%     R; % nore more lines, done
%% get_column([{_, {{Line, Offs}, _}, _} | Rest], Line, FirstOffs, _PrevOffs, Indents) ->
%%     get_column(Rest, Line, FirstOffs, Offs, Indents); % same line, iter
%% get_column(_, Line, FirstOffs, PrevOffs, Indents) ->
%%     R = get_indent(Line, Indents) + FirstOffs - PrevOffs,
%%     R. % new line, done


%% TODO: Add description of asd/function_arity
%%
%% fix_scan_tuples(L) ->
%%     fix_scan_tuples(L, []).

%% fix_scan_tuples([], Acc) ->
%%     Acc;
%% fix_scan_tuples([{T, P} | Rest], Acc) ->
%%     fix_scan_tuples(Rest, [{T, P, T} | Acc]);
%% fix_scan_tuples([{T, P, I, _S} | Rest], Acc) ->
%%     fix_scan_tuples(Rest, [{T, P, I} | Acc]);
%% fix_scan_tuples([{comment, _, _} | Rest], Acc) ->
%%     fix_scan_tuples(Rest, Acc);
%% fix_scan_tuples([{T, P, V} | Rest], Acc) ->
%%     fix_scan_tuples(Rest, [{T, P, V} | Acc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% tail_if([_ | Tail]) -> Tail;
%% tail_if(L) -> L.


