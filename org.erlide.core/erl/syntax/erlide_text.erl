%% Author: jakob
%% Created: Mar 23, 2006
%% @doc Some text-handling utilities

-module(erlide_text).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([get_text_and_lines/2,
         split_lines/1,
         check_function_call/2,
         check_variable_macro_or_record/2,
         matching_paren/1,
         is_paren/1,
         is_op1/1,
         is_op2/1,
         is_block_start_token/1,
         guess_arity/1,
         clean_tokens/2,
         get_line_offsets/1,
         detab/2]).

%% -define(DEBUG, 1).

-include("erlide.hrl").

-include("erlide_scanner.hrl").

%%
%% API Functions
%%

%% get_text_and_lines(Text, From) -> {First, Lines}
%%     Text = string(), From = integer(), First = string(), Lines = [string()]
%%
%% @doc Given text and a From position, return a tuple with
%% the text upto From and a list of the rest of the lines
%% @spec get_text_and_lines(S::string(), From::integer()) -> [{string()}]
get_text_and_lines(S, From) ->
    L = split_lines(S),
    get_text_and_lines(L, From, []).

%% @doc Get function call from token list
%% @spec check_function_call(Tokens::list(), Index::integer()) -> 
%%           {ok, Module::atom(), Function::atom(), Rest::term()} 
check_function_call([_|Rest], Index) when Index > 2 ->
    ?D({Index, Rest}),
    check_function_call(Rest, Index-1);
check_function_call([#token{kind=atom, value=M}, #token{kind=':'},
                     #token{kind=atom, value=F}, #token{kind='('} | Rest], Index) when Index > -1 ->
    ?D({Index, Rest}),
    {ok, M, F, Rest};
check_function_call([#token{kind=atom, value=F}, #token{kind='('} | Rest], Index) when Index > -1 ->
    ?D({Index, F}),
    {ok, F, Rest};
check_function_call([#token{kind=atom, value=F}, #token{kind='/'}, #token{kind=integer, value=Arity} | _],
                    Index) when Index > -1 ->
    ?D({Index, {F, Arity}}),
    {ok, F, Arity};
check_function_call([_|Rest]=_L, Index) when Index > -1 ->
    ?D({nomatch, Index, _L}),
    check_function_call(Rest, Index-1);
check_function_call(_L, _Index) ->
    ?D({ _Index, _L}),
    false.

%% this doesn't work with the new scanner!

%% @doc get variable, macro or record from token list
check_variable_macro_or_record([_|Rest] = _L, Index) when Index > 0 ->
    ?D({macro_or_record, Index, _L}),
    check_variable_macro_or_record(Rest, Index-1);
check_variable_macro_or_record([#token{kind=macro, value=Var} | _], Index) when Index > -1 ->
    {ok, macro, Var};
check_variable_macro_or_record([#token{kind=record, value=Var} | _], Index) when Index > -1 ->
    {ok, record, Var};
check_variable_macro_or_record([#token{kind=var, value=Var} | _], Index) when Index > -1 ->
    {ok, var, Var};
check_variable_macro_or_record(_, _) ->
    false.


%%
%% Local Functions
%%

get_text_and_lines([], _From, Acc) ->
    {"", Acc};
get_text_and_lines([{Pos, Line} | Rest], From, Acc) when Pos =< From ->
    {text_from_r_lines(Rest), [Line | Acc]};
get_text_and_lines([{_, Line} | Rest], From, Acc) ->
    get_text_and_lines(Rest, From, [Line | Acc]).

text_from_r_lines(Lines) ->
    lists:append(lists:reverse([L || {_, L} <- Lines])).

%% split lines of string and return reverse list
split_lines(S) ->
    split_lines(S, 0, 0, "", []).

split_lines("", _Pos, _LinePos, "", Acc) ->
    Acc;
split_lines("", _Pos, LinePos, LineAcc, Acc) ->
    [{LinePos, lists:reverse(LineAcc)} | Acc];
split_lines("\r\n" ++ Rest, Pos, LinePos, LineAcc, Acc) ->
    NewPos = Pos + 2,
    split_lines(Rest, NewPos, NewPos, "",
                [{LinePos, lists:reverse(LineAcc, "\r\n")} | Acc]);
split_lines("\n" ++ Rest, Pos, LinePos, LineAcc, Acc) ->
    NewPos = Pos + 1,
    split_lines(Rest, NewPos, NewPos, "",
                [{LinePos, lists:reverse(LineAcc, "\n")} | Acc]);
split_lines("\r" ++ Rest, Pos, LinePos, LineAcc, Acc) ->
    NewPos = Pos + 1,
    split_lines(Rest, NewPos, NewPos, "",
                [{LinePos, lists:reverse(LineAcc, "\r")} | Acc]);
split_lines([C | Rest], Pos, LinePos, LineAcc, Acc) ->
    split_lines(Rest, Pos+1, LinePos, [C | LineAcc], Acc).


%% matching_paren(char()) -> char()
%% get matching paren
%%
matching_paren('<<') -> '>>';
matching_paren('>>') -> '<<';
matching_paren('(') -> ')';
matching_paren('{') -> '}';
matching_paren('[') -> ']';
matching_paren(')') -> '(';
matching_paren('}') -> '{';
matching_paren(']') -> '['.

is_paren('<<') -> opening;
is_paren('>>') -> closing;
is_paren('(') -> opening;
is_paren('{') -> opening;
is_paren('[') -> opening;
is_paren(')') -> closing;
is_paren('}') -> closing;
is_paren(']') -> closing;
is_paren(_) -> false.

%% return true if Op is a binary operator
%%
is_op2([{Op, _, _} | _]) ->
    is_op2(Op);
is_op2(Op) ->
    lists:member(Op, ['andalso', 'orelse', 'div', 'rem',
                      'band', 'and', 'bor', 'bxor', 'bsl',
                      'bsr', 'or', 'xor', '<-', '=', '==', '/=',
                      '=/=', '=:=', ':', '+', '-', '*', '/', '!',
                      '++', '--', '.', '#']).

%% is_op1(atom()) -> boolean()
%%
is_op1(Op) ->
    lists:member(Op, ['not', '-', '?', 'catch']).

%% is_block_start_token(atom()) -> boolean()
%%
is_block_start_token(T) ->
    lists:member(T, ['fun', 'case', 'begin', 'if', 'try']).

%%
%% guess_arity: guess function arity from a tokenized parameter list
%%

guess_arity(N) when is_integer(N) ->
    N;
guess_arity(Tokens) ->
    ?D(Tokens),
    R = guess_arity(Tokens, 0),
    ?D(R),
    R.

guess_arity([], N) ->
    N;
guess_arity([#token{kind=')'} | _], N) ->
    N;
guess_arity([#token{kind=','} | Rest], N) ->
    guess_arity(Rest, N+1);
guess_arity(Tokens, 0) ->
    guess_arity(skip_expr(Tokens), 1);
guess_arity(Tokens, N) ->
    guess_arity(skip_expr(Tokens), N).

%% skip over an expression, checking parens, operators etc
%%
skip_expr([#token{kind=T} | Before]) when T =:= '{'; T =:= '['; T =:= '(' ->
    A = skip_paren(Before, erlide_text:matching_paren(T)),
    ?D(A),
    case is_op2(A) of
        true ->
            skip_expr(A);
        false ->
            A
    end;
skip_expr([Tup | Rest]) ->
    case is_block_start_token(Tup) of
        true ->
            skip_block_end(Rest);
        false ->
            case Rest of
                [{Op,_, _} | R] ->
                    case is_op2(Op) of
                        true ->
                            skip_expr(R);
                        false ->
                            Rest
                    end;
                _ ->
                    Rest
            end
    end;
skip_expr(L) ->
    L.


%% TODO: Add description of asd/function_arity
%%
skip_paren([], _) ->
    [];
skip_paren([#token{kind=P} | Rest], P) ->
    Rest;
skip_paren([#token{kind=T} | Before], P) when T =:= '{'; T =:= '('; T =:= '[' ->
    skip_paren(skip_paren(Before, erlide_text:matching_paren(T)), P);
skip_paren([_ | Before], P) ->
    skip_paren(Before, P).

skip_block_end([]) ->
    [];
skip_block_end([#token{kind='end'}|Rest]) ->
    Rest;
skip_block_end([#token{kind=Tup} | Rest]) ->
    R = case erlide_text:is_block_start_token(Tup) of
            true ->
                skip_block_end(Rest);
            false ->
                Rest
        end,
    skip_block_end(R);
skip_block_end([_ | Rest]) ->
    skip_block_end(Rest).

clean_tokens(L, W) ->
    clean_tokens(L, W, 0, []).

clean_tokens([], W, _, Acc) ->
    R = lists:reverse(Acc),
    ?D(R),
    {R, W};
%% clean_tokens([#token{kind=ws} | Rest], Acc) ->
%%     clean_tokens(Rest, Acc);
clean_tokens([#token{kind='?'} = Q, #token{kind=Kind, value=Value} | Rest], W, I, Acc) when Kind==atom; Kind==var->
    Macro = Q#token{kind=macro, value=Value, text=undefined},
    NewW = case W > I of
               true -> W-1;
               false -> W
           end,
    clean_tokens(Rest, NewW, I+1, [Macro | Acc]);
clean_tokens([#token{kind='#'} = Q, #token{kind=Kind, value=Value} | Rest], W, I, Acc) when Kind==atom; Kind==var->
    Macro = Q#token{kind=record, value=Value, text=undefined},
    NewW = case W > I of
               true -> W-1;
               false -> W
           end,
    clean_tokens(Rest, NewW, I+1, [Macro | Acc]);
clean_tokens([T | Rest], W, I, Acc) ->
    clean_tokens(Rest, W, I+1, [T | Acc]).

%% detab(string(), integer()) -> string()
%% replace tabs (\t) with spaces
detab(S, Tablength) ->
	detab(S, Tablength, 0, "").

detab("", _, _, Acc) ->
	lists:reverse(Acc);
detab("\t"++Rest, Tablength, I, Acc) ->
    S = get_tab_spaces(I, Tablength),
	detab(Rest, Tablength, I+length(S), S ++ Acc);
detab([EOL | Rest], Tablength, _I, Acc) when EOL =:= $\n; EOL =:= $\r ->
	detab(Rest, Tablength, 0, [EOL | Acc]);
detab([C | Rest], Tablength, I, Acc) ->
	detab(Rest, Tablength, I+1, [C | Acc]).

get_tab_spaces(I, Tablength) ->
	Rest = Tablength - I rem Tablength,
	string:chars($ , Rest).

%% get_line_offsets/1
%%
get_line_offsets("") ->
    {0};
get_line_offsets(S) ->
    get_line_offsets(S, 1, [1]).

get_line_offsets("", _, Acc) ->
    list_to_tuple(lists:reverse(Acc));
get_line_offsets("\r\n" ++ Rest, O, Acc) ->
	O2 = O+2,
    get_line_offsets(Rest, O2, [O2 | Acc]);
get_line_offsets([EOL|Rest], O, Acc) when EOL =:= $\n; EOL =:= $\r ->
    O1 = O+1,
    get_line_offsets(Rest, O1, [O1 | Acc]);
get_line_offsets([_|Rest], O, Acc) ->
    get_line_offsets(Rest, O+1, Acc).
