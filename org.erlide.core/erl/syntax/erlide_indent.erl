%% Author: jakob
%% Created: 2006-jan-28
%% Description: TODO: Add description to erlide_indent
-module(erlide_indent).

%%
%% Include files
%%

%%
%% Exported Functions
%%

-export([indent_next_line/1,
         indent_next_line/2,
         indent_lines/2,
         indent_lines/3]).

%%-define(DEBUG, 1).

-include("erlide.hrl").

%%
%% API Functions
%%
indent_next_line(S) ->
    indent_next_line(S, 8).

indent_next_line(S, Tablength) ->
    case erlide_scan:string(S) of
        {ok, T, _} ->
            LineIndents = get_indents(S, Tablength),
            Tr = fix_scan_tuples(T),
            ?D(Tr),
            indent_after(Tr, LineIndents);
        _  ->
            0
    end.

indent_lines(S, From) ->
    indent_lines(S, From, 8).

indent_lines(S, From, Tablength) ->
    {First, Lines} = erlide_text:get_text_and_lines(S, From),
    do_indent_lines(Lines, Tablength, First, "").

%%
%% Local Functions
%%

%% TODO: Add description of asd/function_arity
%%
do_indent_lines([], _, _, A) ->
    A;
do_indent_lines([Line | Rest], Tablength, Text, Acc) ->
    N = indent_next_line(Text ++ Acc, Tablength),
    NewLine = reindent_line(Line, N),
    do_indent_lines(Rest, Tablength, Text, Acc ++ NewLine).

%% TODO: Add description of asd/function_arity
%%
reindent_line(" " ++ S, I) ->
    reindent_line(S, I);
reindent_line("\t" ++ S, I) ->
    reindent_line(S, I);
reindent_line(S, I) ->
    lists:duplicate(I, $ )++S.


%% TODO: Add description of asd/function_arity
%%
fix_scan_tuples(L) ->
    fix_scan_tuples(L, []).

fix_scan_tuples([], Acc) ->
    Acc;
fix_scan_tuples([{T, P} | Rest], Acc) ->
    fix_scan_tuples(Rest, [{T, P, T} | Acc]);
fix_scan_tuples([{T, P, I, _S} | Rest], Acc) ->
    fix_scan_tuples(Rest, [{T, P, I} | Acc]);
fix_scan_tuples([{comment, _, _} | Rest], Acc) ->
    fix_scan_tuples(Rest, Acc);
fix_scan_tuples([{T, P, V} | Rest], Acc) ->
    fix_scan_tuples(Rest, [{T, P, V} | Acc]).

%% TODO: Add description of asd/function_arity
%%
indent_after([{'->', _, _} | Before], Indents) ->
    Start = skip_guard(Before),
    ?D(Start),
    indent_with(get_indent(Start, Indents), get_token_indent('->'));
indent_after([{dot, _, _} | _Before], _Indents) ->
    0; %    skip_and_indent('->', dot, Before, Indents);
indent_after([{';', _, _} | Before], Indents) ->
    Start = skip_to_block_start(Before, true),
    get_column(Start, Indents); %% skip_and_indent('->', ';', Before, Indents);
indent_after([{'of', _, _} | Before], Indents) ->
    Start0 = skip_to_expr_start(Before),
    Start1 = tail_if(Start0),
    Start2 = skip_to_token('case', Start1),
    indent_with(get_column(Start2, Indents), get_token_indent('of'));
indent_after([{',', _, _} | Before], Indents) ->
    case skip_to_expr_start(Before) of
        [_ | [{',', _, _} | _] = Rest] ->
            indent_after(Rest, Indents);
        Start ->
            ?D(Start),
            get_column(Start, Indents)
    end;
indent_after([{T, _, _} | _] = L, Indents) ->
    ?D({indent_after, L}),
    case erlide_text:is_block_start_token(T) of
        true ->
            indent_with(get_column(L, Indents),
                        get_token_indent(L));
        false ->
            other_indent(L, Indents)
    end;
indent_after(L, Indents) ->
    other_indent(L, Indents).

other_indent(L, Indents) ->
    case check_partial_expr(L) of
        {partial, A} ->
            indent_with(get_column(A, Indents), get_token_indent(op2));
        {paren, B} -> % Can it really get here?
            indent_with(get_column(B, Indents), get_token_indent('('));
        _ ->
            Start = skip_to_block_start(L, false),
            get_column(Start, Indents)
    end.

%% TODO: Add description of asd/function_arity
%%
check_partial_expr(L) ->
    case erlide_text:is_op2(L) of
        true ->
            A = skip_to_expr_start(tail_if(L)),
            {partial, A};
        false ->
            check_paren_list(L)
    end.

check_paren_list([{T, _, _} | _] = L) when T =:= '('; T =:= '{'; T =:= '(' ->
    {paren, L};
check_paren_list([{',',_ ,_}|Before]) ->
     Start = skip_to_expr_start(Before),
     check_paren_list(tail_if(Start));
check_paren_list(_) ->
     false.

%% TODO: Add description of asd/function_arity
%%
skip_to_block_start([], _Semi) ->
    [];
skip_to_block_start([{';', _, _} | Before], Semi) ->
    Start = Before, %Start = skip_to_block_start(Before),
    skip_to_block_start(Start, Semi);
skip_to_block_start([{'end', _, _} | Before], Semi) ->
    ?D(Before),
    Start0 = skip_to_block_start(Before, false),
    Start1 = tail_if(Start0),
    ?D(Start1),
    skip_to_block_start(Start1, Semi);
skip_to_block_start([{'->', _, _} | Before], Semi) ->
    ?D({arrow, Semi, Before}),
        case Semi of
        true ->
            skip_guard(Before);
        false ->
            Before
    end;
%%     ?D({arrow, Semi}),
%%     case is_function_declaration(Before) of
%%         true ->
%%             ?D({fn, Before}),
%%             skip_to_fn_start(Before);
%%         false ->
%%             case Semi of
%%                 true ->
%%                     ?D({no_fn_C, Before}),
%%                     skip_guard(Before);
%%                 false ->
%%                     A = skip_to_expr_start(Before),
%%                     ?D({no_fn_A, A}),
%%                     B = tail_if(A),
%%                     ?D({no_fn_B, B}),
%%                     skip_to_block_start(B, Semi)
%%             end
%%     end;
skip_to_block_start([{'when', _, _} | Before], Semi) ->
    ?D({arrow, Semi}),
    case Semi orelse is_function_declaration(Before) of
        true ->
            ?D({fn, Before}),
            skip_to_fn_start_without_guards(Before);
        false ->
            A = skip_to_expr_start(Before),
            ?D({no_fn_A, A}),
            B = tail_if(A),
            ?D({no_fn_B, B}),
            skip_to_block_start(B, Semi)
    end;
skip_to_block_start([{',', _, _} | Before], Semi) ->
    ?D(Before),
    Start = skip_to_expr_start(Before),
    B = tail_if(Start),
    skip_to_block_start(B, Semi);
skip_to_block_start([{'of', _, _} | Before], _Semi) ->
    ?D('of'),
    Start0 = skip_to_expr_start(Before),
    ?D(Start0),
    _Start1 = tail_if(Start0);
skip_to_block_start([{T, _, _V} | _Before] = L, Semi) ->
    ?D(L),
    case erlide_text:is_block_start_token(T) of
        true ->
            L;
        false ->
            A = skip_to_expr_start(L),
            ?D(A),
            skip_to_block_start(tail_if(A), Semi)
    end.

skip_to_fn_start_without_guards([{')', _, _} | _] = L) ->
    A = skip_to_expr_start(L),
    ?D(A),
    A; %% tail_if(A);
skip_to_fn_start_without_guards(L) ->
    tail_if(L).

%% TODO: Add description of asd/function_arity
%%
skip_to_fn_start([{')', _, _} | _] = L) ->
    skip_guard(L);
%%    skip_guard(L, L);
skip_to_fn_start(L) ->
    L.

%% skip past guards
skip_guard(L) ->
    A = skip_to_expr_start(L),
    skip_guard(A, L).

%% skip past guards
skip_guard([_, {',', _, _} | A], Old) ->
    B = skip_to_expr_start(A),
    ?D(B),
    skip_guard(B, Old);
skip_guard([_, {';', _, _} | A], Old) ->
    B = skip_to_expr_start(A),
    ?D(B),
    skip_guard(B, Old);
skip_guard([_, {'when', _, _} | A], _Old) ->
    ?D(A),
    skip_to_expr_start(A);
skip_guard(_A, Old) ->
    Old.

%% TODO: Add description of asd/function_arity
%%
is_function_declaration([{')', _, _} | _]) ->
    true;
is_function_declaration(_) ->
    false.

%% TODO: Add description of asd/function_arity
%%
skip_to_token(_T, []) ->
    [];
skip_to_token(T, [{T, _, _} | _] = L) ->
    L;
skip_to_token(T, [_ | R]) ->
    skip_to_token(T, R).

%% TODO: Add description of asd/function_arity
%%
get_token_indent([{T, _, _} | _]) ->
    get_token_indent(T);
get_token_indent('begin') ->
    4;
get_token_indent('of') ->
    4;
get_token_indent(op2) ->
    8;
get_token_indent('->') ->
    4;
get_token_indent(dot) ->
    0;
get_token_indent(';') ->
    0;
get_token_indent('(') ->
    1;
get_token_indent(_) ->
    0.

%% TODO: Add description of asd/function_arity
%%
indent_with(I, D) ->
    I+D.

%% TODO: Add description of asd/function_arity
%%
get_indent(Line, Indents) when is_integer(Line) ->
    element(Line, Indents);
get_indent([{_, {{Line, _}, _}, _} | _], Indents) ->
    element(Line, Indents).

skip_fn_call([_, {T, _, _} | _] = L)
  when T == ','; T == 'of'; T == '.'; T == 'after'; T == 'receive' ->
    L;
skip_fn_call([_|L]) ->
    skip_to_expr_start(L).

%% skip to the start of an expression, checking parens, operators etc
%%
skip_to_expr_start([{'end', _, _} | Before]) ->
    skip_to_block_start(Before, false);
skip_to_expr_start([{')', _, _} | Before]) ->
    A = skip_paren(Before, '('),
    ?D(A),
    L = tail_if(A),
    case erlide_text:is_op2(L) of
        true ->
            skip_to_expr_start(A);
        false ->
            ?D(A),
            skip_fn_call(A)
    end;
skip_to_expr_start([{'}', _, _} | Before]) ->
    A = skip_paren(Before, '{'),
    case tail_if(A) of
        [{atom, _, _}, {'#', _, _} | Rest] ->  % handle record definitions
            skip_to_expr_start(Rest);
        L ->
            case erlide_text:is_op2(L) of
                true ->
                    skip_to_expr_start(A);
                false ->
                    A
            end
    end;
skip_to_expr_start([{']', _, _} | Before]) ->
    A = skip_paren(Before, '['),
    L = tail_if(A),
    case erlide_text:is_op2(L) of
        true ->
            skip_to_expr_start(A);
        false ->
            A
    end;
skip_to_expr_start([_Tup, {'->',_, _} | _] = L) ->
    L;
skip_to_expr_start([_Tup, {';',_, _} | _] = L) ->
    L;
skip_to_expr_start([_Tup, {',',_, _} | _] = L) ->
    L;
skip_to_expr_start([_Tup, {Op,_, _} | Before] = L) ->
    case erlide_text:is_op2(Op) of
        true ->
            skip_to_expr_start(Before);
        false ->
            L
    end;
skip_to_expr_start(L) ->
    ?D(d),
    L.

%% TODO: Add description of asd/function_arity
%%
skip_paren([], _) ->
    [];
skip_paren([{P, _, _} | _] = L, P) ->
    ?D(L),
    L;
skip_paren([{T, _, _} | Before], P) when T =:= '}'; T =:= ')'; T =:= ']' ->
    ?D(Before),
    skip_paren(tail_if(skip_paren(Before, erlide_text:matching_paren(T))), P);
skip_paren([_ | Before], P) ->
    skip_paren(Before, P).

%% get_column for an element given as list
%%
get_column([], Indents) ->
    element(1, Indents);
get_column([{_, {{Line, Offs}, _}, _} = _T | _] = L, Indents) ->
    ?D({get_column, _T}),
    get_column(L, Line, Offs, Offs, Indents).

get_column([], Line, FirstOffs, PrevOffs, Indents) ->
    R = get_indent(Line, Indents) + FirstOffs - PrevOffs,
    ?D(R),
    R; % nore more lines, done
get_column([{_, {{Line, Offs}, _}, _} | Rest], Line, FirstOffs, _PrevOffs, Indents) ->
    get_column(Rest, Line, FirstOffs, Offs, Indents); % same line, iter
get_column(_, Line, FirstOffs, PrevOffs, Indents) ->
    R = get_indent(Line, Indents) + FirstOffs - PrevOffs,
    ?D(R),
    R. % new line, done

%% get_indents/2
%%
get_indents("", _) ->
    {0};
get_indents(S, Tablength) ->
    get_indents(S, 0, true, Tablength, []).

get_indents("", _, _, _, Acc) ->
    list_to_tuple(lists:reverse(Acc));
get_indents(" " ++ Rest, I, B, T, Acc) ->
    get_indents(Rest, I+1, B, T, Acc);
get_indents("\t" ++ Rest, I, B, Tablength, Acc) ->
    J = I + Tablength - (I rem Tablength),
    get_indents(Rest, J, B, Tablength, Acc);
get_indents("\r\n" ++ Rest, _I, true, T, Acc) ->
    get_indents(Rest, 0, true, T, [0 | Acc]);
get_indents([EOL|Rest], _I, true, T, Acc) when EOL =:= $\n; EOL =:= $\r ->
    get_indents(Rest, 0, true, T, [0 | Acc]);
get_indents("\r\n"++Rest, _I, false, T, Acc) ->
    get_indents(Rest, 0, true, T, Acc);
get_indents([EOL|Rest], _I, false, T, Acc) when EOL =:= $\n; EOL =:= $\r ->
    get_indents(Rest, 0, true, T, Acc);
get_indents([_|Rest], I, true, T, Acc) ->
    get_indents(Rest, 0, false, T, [I | Acc]);
get_indents([_|Rest], _I, false, T, Acc) ->
    get_indents(Rest, 0, false, T, Acc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tail_if([_ | Tail]) -> Tail;
tail_if(L) -> L.
