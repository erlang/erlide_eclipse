-module(erlide_boxp).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("/home/mee/workspaces/erlidegit/erlide/org.erlide.core/erl/pprint/erlide_boxp.yrl", 63).

%%% ******************************************************************************
%%%  Copyright (c) 2004 Vlad Dumitrescu and others.
%%%  All rights reserved. This program and the accompanying materials
%%%  are made available under the terms of the Eclipse Public License v1.0
%%%  which accompanies this distribution, and is available at
%%%  http://www.eclipse.org/legal/epl-v10.html
%%%
%%%  Contributors:
%%%      Vlad Dumitrescu
%%% ******************************************************************************/
%%

-export([string/1]).

-export([c/0, test/0]).

c() ->
    yecc:yecc("erlide_box.yrl", "erlide_boxp.erl", true),
    c:c(erlide_boxp),
    erlide_boxp:test().

test() ->
%%     {ok, S} = file:read_file("test.box"),
%%     Text = binary_to_list(S),
%%     {ok, L, _} = erl_scan:string(Text),
%%     %%io:format("~p~n===============---============~n", [L]),
%%     {ok, R} = boxp:parse(L),
%%     io:format("~p~n", [R]),


    M =string("h[ \">\" H #H  \"...\" {h0[\"a\" H] \",\"} \"<\" ]"),
    io:format("====~n~p~n", [M]),

    ok.

string(Str) ->
    {ok, T, _} = erl_scan:string(Str),
    parse(T).

%%%%%

mkbox(L) when is_list(L) ->
    lists:flatten(lists:map(fun mkbox/1, L));
mkbox({{string, Str}=X, _}) when is_list(Str) ->
    X;
mkbox({{boxvar, {Str, R}}=X, _}) when is_atom(Str), is_atom(R) ->
    X;
mkbox({{boxvar, {Str, R}, quoted}=X, _}) when is_atom(Str), is_atom(R) ->
    X;
mkbox({{splicevar, {Str, R}}=X, _}) when is_atom(Str), is_atom(R) ->
    X;
mkbox({{list, Tpl, Sep}, _}) ->
	TplBox = mkbox(Tpl),
	L = extr_var(TplBox),
    {list, L, TplBox, mkbox(Sep)};
mkbox({{h, SOpts, Boxes}, _}) ->
    {h, mksopt(h, SOpts), mkbox(Boxes)};
mkbox({{v, SOpts, Boxes}, _}) ->
    {v, mksopt(v, SOpts), mkbox(Boxes)};
mkbox({{hv, SOpts, Boxes}, _}) ->
    {hv, mksopt(SOpts), mkbox(Boxes)};
mkbox({{hov, SOpts, Boxes}, _}) ->
    {hov, mksopt(SOpts), mkbox(Boxes)};
mkbox({{i, SOpts, [Box]}, _}) ->
    {i, mksopt(i, SOpts), mkbox(Box)};
mkbox({{wd, [], [Box]}, _}) ->
    {wd, mkbox(Box)};
mkbox({{a, AOpts, SOpts, Boxes}, {_, L, _}}) ->
    A = mkaopt(AOpts),
    B = mkbox(Boxes),
    case lists:filter(fun({row, _}) -> false; 
			 ({splicevar, _}) -> false; 
			 ({boxvar, _}) -> false; 
			 (_)->true 
		      end, B) of
	[] ->
	    {table, A, mksopt(SOpts), B};
	Z ->
	    return_error(L, msg(["'A' box can only contain 'R' boxes, found ", 
			     io_lib:format("~p", [Z])]))
    end;
mkbox({{r, [], Boxes}, _}) ->
    {row, mkbox(Boxes)};
mkbox({{cmt, [], Boxes}, _}) ->
    {comment, mkbox(Boxes)};
mkbox({{f, SOpts, [Box]}, _}) ->
    {font, mkfopt(SOpts), mkbox(Box)};
mkbox({{kw, [], [Box]}, _}) ->
    {keyword, mkbox(Box)};
mkbox({{var, [], [Box]}, _}) ->
    {variable, mkbox(Box)};
mkbox({{num, [], [Box]}, _}) ->
    {number, mkbox(Box)};
mkbox({{brk, [], [Box]}, _}) ->
    {bracket, mkbox(Box)};
mkbox({{lbl, [], Str, Box}, _}) when is_list(Str) ->
    {label, Str, mkbox(Box)};
mkbox({{ref, [], Str, Box}, _}) when is_list(Str) ->
    {reference, Str, mkbox(Box)};
mkbox({X, {_, L, _}=R}) ->
    case split(X) of
	no ->
	    return_error(L, ["bad box:", msg(io_lib:format("~p", [X]))]);
	New ->
	    mkbox({New, R})
    end.

mkaopt(L) when is_list(L) ->
    [mkaopt(X) || X <- L];
mkaopt({{l, S}, _}) ->
    {left, mksopt(S)};
mkaopt({{c, S}, _}) ->
    {center, mksopt(S)};
mkaopt({{r, S}, _}) ->
    {right, mksopt(S)};
mkaopt({X, {_, L, _}}) ->
    return_error(L, {"bad aopt:", X}).

mksopt(E, L) when is_list(L) ->
    [mksopt(E, X) || X <- L];
mksopt(h, {{default, E},_}) ->
    {hs, E};
mksopt(v, {{default, E},_}) ->
    {vs, E};
mksopt(i, {{default, E},_}) ->
    {is, E};
mksopt(_, E) ->
    mksopt(E).


mksopt(L) when is_list(L) ->
    [mksopt(X) || X <- L];
mksopt({{hs, _}=X, _}) ->
    X;
mksopt({{vs, _}=X, _}) ->
    X;
mksopt({{is, _}=X, _}) ->
    X;
mksopt({{ts, _}=X, _}) ->
    X;
mksopt({X, {_, L, _}}) ->
    return_error(L, {"bad sopt:", X}).
 
mkfopt(L) when is_list(L) ->
    [mkfopt(X) || X <- L];
mkfopt({{fn, N}, _}) when is_atom(N) ->
    {name, N};
mkfopt({{fm, N}, _}) when is_atom(N) ->
    {family, N};
mkfopt({{se, N}, _}) when is_atom(N) ->
    {series, N};
mkfopt({{sh, N}, _}) when is_atom(N) ->
    {shape, N};
mkfopt({{sz, N}, _}) when is_integer(N) ->
    {size, N};
mkfopt({{cl, N}, _}) when is_atom(N); is_integer(N) ->
    {color, N};
mkfopt({X, {_, L, _}}) ->
    return_error(L, {"bad fopt:", X}).

val({string, _, S}) ->
    S;
val({atom, _, S}) ->
    S;
val({var, _, S}) ->
    S;
val({integer, _, S}) ->
    S;
val(X) ->
    X.

msg(L) ->
    lists:flatten(L).

extr_var(L) when is_list(L) ->
	case lists:filter(fun(X) -> is_atom(X) end, 
			  [extr_var(X) || X<-L]) of
		[] ->
			[];
		R ->
			hd(R)
	end;
extr_var({boxvar, {N, _}}) ->
	N;
extr_var({_, L}) when is_list(L) ->
	extr_var(L);
extr_var({_, _, L}) when is_list(L) ->
	extr_var(L);
extr_var(_) ->
	0.
	
split({N, Opts, Data}) ->
    S = atom_to_list(N),
    {C,A} = lists:splitwith(fun(X) -> X >$@ end, S),
    case A of
	[] ->
	    no;
	_ ->
	    {list_to_atom(C), [{{default, list_to_integer(A)},{0,0,0}}|Opts], Data}
    end;
split(_) ->
    no.


-file("/usr/lib/erlang/lib/parsetools-2.0/include/yeccpre.hrl", 0).
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

-type(yecc_ret() :: {'error', _} | {'ok', _}).

-spec parse(Tokens :: list()) -> yecc_ret().
parse(Tokens) ->
    yeccpars0(Tokens, {no_func, no_line}, 0, [], []).

-spec(parse_and_scan/1 ::
      ({function() | {atom(), atom()}, [_]} | {atom(), atom(), [_]}) ->
            yecc_ret()).
parse_and_scan({F, A}) -> % Fun or {M, F}
    yeccpars0([], {{F, A}, no_line}, 0, [], []);
parse_and_scan({M, F, A}) ->
    yeccpars0([], {{{M, F}, A}, no_line}, 0, [], []).

-spec(format_error/1 :: (any()) -> [char() | list()]).
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
	true ->
	    Message;
	_ ->
	    io_lib:write(Message)
    end.

% To be used in grammar files to throw an error message to the parser
% toplevel. Doesn't have to be exported!
-compile({nowarn_unused_function, return_error/2}).
-spec(return_error/2 :: (integer(), any()) -> no_return()).
return_error(Line, Message) ->
    throw({error, {Line, ?MODULE, Message}}).

-define(CODE_VERSION, "1.4").

yeccpars0(Tokens, Tzr, State, States, Vstack) ->
    try yeccpars1(Tokens, Tzr, State, States, Vstack)
    catch 
        error: Error ->
            Stacktrace = erlang:get_stacktrace(),
            try yecc_error_type(Error, Stacktrace) of
                {syntax_error, Token} ->
                    yeccerror(Token);
                {missing_in_goto_table=Tag, Symbol, State} ->
                    Desc = {Symbol, State, Tag},
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                 Stacktrace)
            catch _:_ -> erlang:raise(error, Error, Stacktrace)
            end;
        %% Probably thrown from return_error/2:
        throw: {error, {_Line, ?MODULE, _M}} = Error ->
            Error
    end.

yecc_error_type(function_clause, [{?MODULE,F,[State,_,_,_,Token,_,_]} | _]) ->
    case atom_to_list(F) of
        "yeccpars2" ++ _ ->
            {syntax_error, Token};
        "yeccgoto_" ++ SymbolL ->
            {ok,[{atom,_,Symbol}],_} = erl_scan:string(SymbolL),
            {missing_in_goto_table, Symbol, State}
    end.

yeccpars1([Token | Tokens], Tzr, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens, Tzr);
yeccpars1([], {{F, A},_Line}, State, States, Vstack) ->
    case apply(F, A) of
        {ok, Tokens, Endline} ->
	    yeccpars1(Tokens, {{F, A}, Endline}, State, States, Vstack);
        {eof, Endline} ->
            yeccpars1([], {no_func, Endline}, State, States, Vstack);
        {error, Descriptor, _Endline} ->
            {error, Descriptor}
    end;
yeccpars1([], {no_func, no_line}, State, States, Vstack) ->
    Line = 999999,
    yeccpars2(State, '$end', States, Vstack, yecc_end(Line), [],
              {no_func, Line});
yeccpars1([], {no_func, Endline}, State, States, Vstack) ->
    yeccpars2(State, '$end', States, Vstack, yecc_end(Endline), [],
              {no_func, Endline}).

%% yeccpars1/7 is called from generated code.
%%
%% When using the {includefile, Includefile} option, make sure that
%% yeccpars1/7 can be found by parsing the file without following
%% include directives. yecc will otherwise assume that an old
%% yeccpre.hrl is included (one which defines yeccpars1/5).
yeccpars1(State1, State, States, Vstack, Token0, [Token | Tokens], Tzr) ->
    yeccpars2(State, element(1, Token), [State1 | States],
              [Token0 | Vstack], Token, Tokens, Tzr);
yeccpars1(State1, State, States, Vstack, Token0, [], {{_F,_A}, _Line}=Tzr) ->
    yeccpars1([], Tzr, State, [State1 | States], [Token0 | Vstack]);
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, no_line}) ->
    Line = yecctoken_end_location(Token0),
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line});
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, Line}) ->
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line}).

% For internal use only.
yecc_end({Line,_Column}) ->
    {'$end', Line};
yecc_end(Line) ->
    {'$end', Line}.

yecctoken_end_location(Token) ->
    try
        {text, Str} = erl_scan:token_info(Token, text),
        {line, Line} = erl_scan:token_info(Token, line),
        Parts = re:split(Str, "\n"),
        Dline = length(Parts) - 1,
        Yline = Line + Dline,
        case erl_scan:token_info(Token, column) of
            {column, Column} ->
                Col = byte_size(lists:last(Parts)),
                {Yline, Col + if Dline =:= 0 -> Column; true -> 1 end};
            undefined ->
                Yline
        end
    catch _:_ ->
        yecctoken_location(Token)
    end.

yeccerror(Token) ->
    Text = yecctoken_to_string(Token),
    Location = yecctoken_location(Token),
    {error, {Location, ?MODULE, ["syntax error before: ", Text]}}.

yecctoken_to_string(Token) ->
    case catch erl_scan:token_info(Token, text) of
        {text, Txt} -> Txt;
        _ -> yecctoken2string(Token)
    end.

yecctoken_location(Token) ->
    case catch erl_scan:token_info(Token, location) of
        {location, Loc} -> Loc;
        _ -> element(2, Token)
    end.

yecctoken2string({atom, _, A}) -> io_lib:write(A);
yecctoken2string({integer,_,N}) -> io_lib:write(N);
yecctoken2string({float,_,F}) -> io_lib:write(F);
yecctoken2string({char,_,C}) -> io_lib:write_char(C);
yecctoken2string({var,_,V}) -> io_lib:format("~s", [V]);
yecctoken2string({string,_,S}) -> io_lib:write_unicode_string(S);
yecctoken2string({reserved_symbol, _, A}) -> io_lib:write(A);
yecctoken2string({_Cat, _, Val}) -> io_lib:write(Val);
yecctoken2string({dot, _}) -> "'.'";
yecctoken2string({'$end', _}) ->
    [];
yecctoken2string({Other, _}) when is_atom(Other) ->
    io_lib:write(Other);
yecctoken2string(Other) ->
    io_lib:write(Other).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



-file("/home/mee/workspaces/erlidegit/erlide/org.erlide.core/erl/pprint/erlide_boxp.erl", 391).

yeccpars2(0=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(1=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_1(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(2=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(3=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_3(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(4=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(5=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(6=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(7=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(8=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(9=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(10=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_10(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(11=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(12=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(13=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(14=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(15=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(16=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(17=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(18=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(19=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(20=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(21=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(22=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(23=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(24=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(25=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(26=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(27=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(28=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(29=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(30=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(31=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(32=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(33=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_33(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(34=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(35=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(36=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(37=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_37(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(38=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(39=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(40=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(41=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_41(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(42=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(43=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(44=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(45=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(46=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_46(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(47=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(48=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(49=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.3",{missing_state_in_action_table, Other}}).

yeccpars2_0(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 4, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 5, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 6, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 7, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr).

yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_1_(Stack),
 yeccgoto_box(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_2(_S, '$end', _Ss, Stack,  _T, _Ts, _Tzr) ->
 {ok, hd(Stack)}.

yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_3_(Stack),
 yeccgoto_pprule(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_4(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr).

yeccpars2_5(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_5(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_5(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_5_(Stack),
 yeccpars2_14(14, Cat, [5 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_6_(Stack),
 yeccgoto_box(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_7(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_7_(Stack),
 yeccgoto_xvar(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_8: see yeccpars2_0

yeccpars2_9(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_10(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr).

yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_11_(Stack),
 yeccgoto_box(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_12_(Stack),
 yeccgoto_box(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_13_(Stack),
 yeccgoto_xvar(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_14(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr).

yeccpars2_15(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_15_(Stack),
 yeccpars2_41(_S, Cat, [15 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_16(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_16_(Stack),
 yeccpars2_35(35, Cat, [16 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_17(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr).

yeccpars2_18(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr).

yeccpars2_19(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_19_(Stack),
 yeccgoto_sfopt(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_20(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr).

yeccpars2_21(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_21_(Stack),
 yeccgoto_sfopt(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_22_(Stack),
 yeccgoto_sfopt(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_23_(Stack),
 yeccgoto_sfopt(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_24(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr).

yeccpars2_25(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_25_(Stack),
 yeccgoto_aoptlist1(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_26_(Stack),
 yeccgoto_aoptlist(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_27(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_27(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_27_(Stack),
 yeccpars2_31(_S, Cat, [27 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_28(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr).

yeccpars2_29(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_29_(Stack),
 yeccgoto_xvar(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_30_(Stack),
 yeccgoto_xvar(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_31_(Stack),
 yeccgoto_aopt(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_32(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr).

yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_33_(Stack),
 yeccgoto_aoptlist1(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_34_(Stack),
 yeccgoto_aoptlist(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_35(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr).

yeccpars2_36(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 4, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 5, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 6, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 7, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_36_(Stack),
 yeccpars2_37(37, Cat, [36 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_37(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr).

yeccpars2_38(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 4, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 5, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 6, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 7, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_38_(Stack),
 yeccpars2_39(_S, Cat, [38 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_39_(Stack),
 yeccgoto_boxlist(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_40_(Stack),
 yeccgoto_box(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_41_(Stack),
 yeccgoto_sfoptlist(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_42(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 4, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 5, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 7, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_42_(Stack),
 yeccpars2_43(43, Cat, [42 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_43(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr).

yeccpars2_44(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_44_(Stack),
 yeccgoto_box(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_45: see yeccpars2_0

yeccpars2_46(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr).

yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_47_(Stack),
 yeccgoto_box(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_48_(Stack),
 yeccgoto_box(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_49_(Stack),
 yeccgoto_xvar(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccgoto_aopt(17, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(25, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_aopt(32, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(25, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_aoptlist(5, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(16, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_aoptlist1(17, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(24, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_aoptlist1(32=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_box(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_box(8, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_box(9, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(10, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_box(36, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(38, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_box(38, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(38, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_box(42, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(38, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_box(45, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_boxlist(36, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(37, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_boxlist(38=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_boxlist(42, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(43, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_pprule(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_sfopt(5, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(15, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sfopt(15, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(15, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sfopt(16, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(15, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sfopt(27, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(15, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_sfoptlist(5, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(14, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sfoptlist(15=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sfoptlist(16, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(35, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sfoptlist(27=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_xvar(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_xvar(8=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_xvar(9=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_xvar(36=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_xvar(38=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_xvar(42=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_xvar(45=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr).

-compile({inline,yeccpars2_1_/1}).
-file("/home/mee/workspaces/erlidegit/erlide/org.erlide.core/erl/pprint/erlide_boxp.yrl", 32).
yeccpars2_1_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { __1 , __1 }
  end | __Stack].

-compile({inline,yeccpars2_3_/1}).
-file("/home/mee/workspaces/erlidegit/erlide/org.erlide.core/erl/pprint/erlide_boxp.yrl", 24).
yeccpars2_3_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   mkbox ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_5_/1}).
-file("/home/mee/workspaces/erlidegit/erlide/org.erlide.core/erl/pprint/erlide_boxp.yrl", 40).
yeccpars2_5_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_6_/1}).
-file("/home/mee/workspaces/erlidegit/erlide/org.erlide.core/erl/pprint/erlide_boxp.yrl", 33).
yeccpars2_6_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { { string , val ( __1 ) } , __1 }
  end | __Stack].

-compile({inline,yeccpars2_7_/1}).
-file("/home/mee/workspaces/erlidegit/erlide/org.erlide.core/erl/pprint/erlide_boxp.yrl", 26).
yeccpars2_7_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { boxvar , { val ( __1 ) , '_' } }
  end | __Stack].

-compile({inline,yeccpars2_11_/1}).
-file("/home/mee/workspaces/erlidegit/erlide/org.erlide.core/erl/pprint/erlide_boxp.yrl", 37).
yeccpars2_11_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { { list , val ( __2 ) , { { string , [] } , __3 } } , __1 }
  end | __Stack].

-compile({inline,yeccpars2_12_/1}).
-file("/home/mee/workspaces/erlidegit/erlide/org.erlide.core/erl/pprint/erlide_boxp.yrl", 38).
yeccpars2_12_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { { list , val ( __2 ) , val ( __3 ) } , __1 }
  end | __Stack].

-compile({inline,yeccpars2_13_/1}).
-file("/home/mee/workspaces/erlidegit/erlide/org.erlide.core/erl/pprint/erlide_boxp.yrl", 29).
yeccpars2_13_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { splicevar , { val ( __1 ) , '_' } }
  end | __Stack].

-compile({inline,yeccpars2_15_/1}).
-file("/home/mee/workspaces/erlidegit/erlide/org.erlide.core/erl/pprint/erlide_boxp.yrl", 40).
yeccpars2_15_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_16_/1}).
-file("/home/mee/workspaces/erlidegit/erlide/org.erlide.core/erl/pprint/erlide_boxp.yrl", 40).
yeccpars2_16_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_19_/1}).
-file("/home/mee/workspaces/erlidegit/erlide/org.erlide.core/erl/pprint/erlide_boxp.yrl", 44).
yeccpars2_19_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { { default , val ( __1 ) } , __1 }
  end | __Stack].

-compile({inline,yeccpars2_21_/1}).
-file("/home/mee/workspaces/erlidegit/erlide/org.erlide.core/erl/pprint/erlide_boxp.yrl", 45).
yeccpars2_21_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { { val ( __1 ) , val ( __3 ) } , __1 }
  end | __Stack].

-compile({inline,yeccpars2_22_/1}).
-file("/home/mee/workspaces/erlidegit/erlide/org.erlide.core/erl/pprint/erlide_boxp.yrl", 43).
yeccpars2_22_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { { val ( __1 ) , val ( __3 ) } , __1 }
  end | __Stack].

-compile({inline,yeccpars2_23_/1}).
-file("/home/mee/workspaces/erlidegit/erlide/org.erlide.core/erl/pprint/erlide_boxp.yrl", 46).
yeccpars2_23_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { { val ( __1 ) , val ( __3 ) } , __1 }
  end | __Stack].

-compile({inline,yeccpars2_25_/1}).
-file("/home/mee/workspaces/erlidegit/erlide/org.erlide.core/erl/pprint/erlide_boxp.yrl", 53).
yeccpars2_25_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_26_/1}).
-file("/home/mee/workspaces/erlidegit/erlide/org.erlide.core/erl/pprint/erlide_boxp.yrl", 50).
yeccpars2_26_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ ]
  end | __Stack].

-compile({inline,yeccpars2_27_/1}).
-file("/home/mee/workspaces/erlidegit/erlide/org.erlide.core/erl/pprint/erlide_boxp.yrl", 40).
yeccpars2_27_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_29_/1}).
-file("/home/mee/workspaces/erlidegit/erlide/org.erlide.core/erl/pprint/erlide_boxp.yrl", 28).
yeccpars2_29_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { boxvar , { val ( __3 ) , val ( __1 ) } }
  end | __Stack].

-compile({inline,yeccpars2_30_/1}).
-file("/home/mee/workspaces/erlidegit/erlide/org.erlide.core/erl/pprint/erlide_boxp.yrl", 30).
yeccpars2_30_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { splicevar , { val ( __3 ) , val ( __1 ) } }
  end | __Stack].

-compile({inline,yeccpars2_31_/1}).
-file("/home/mee/workspaces/erlidegit/erlide/org.erlide.core/erl/pprint/erlide_boxp.yrl", 48).
yeccpars2_31_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { { val ( __1 ) , __2 } , __1 }
  end | __Stack].

-compile({inline,yeccpars2_33_/1}).
-file("/home/mee/workspaces/erlidegit/erlide/org.erlide.core/erl/pprint/erlide_boxp.yrl", 54).
yeccpars2_33_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_34_/1}).
-file("/home/mee/workspaces/erlidegit/erlide/org.erlide.core/erl/pprint/erlide_boxp.yrl", 51).
yeccpars2_34_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_36_/1}).
-file("/home/mee/workspaces/erlidegit/erlide/org.erlide.core/erl/pprint/erlide_boxp.yrl", 56).
yeccpars2_36_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_38_/1}).
-file("/home/mee/workspaces/erlidegit/erlide/org.erlide.core/erl/pprint/erlide_boxp.yrl", 56).
yeccpars2_38_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_39_/1}).
-file("/home/mee/workspaces/erlidegit/erlide/org.erlide.core/erl/pprint/erlide_boxp.yrl", 57).
yeccpars2_39_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_40_/1}).
-file("/home/mee/workspaces/erlidegit/erlide/org.erlide.core/erl/pprint/erlide_boxp.yrl", 35).
yeccpars2_40_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { { val ( __1 ) , __2 , __3 , __5 } , __1 }
  end | __Stack].

-compile({inline,yeccpars2_41_/1}).
-file("/home/mee/workspaces/erlidegit/erlide/org.erlide.core/erl/pprint/erlide_boxp.yrl", 41).
yeccpars2_41_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_42_/1}).
-file("/home/mee/workspaces/erlidegit/erlide/org.erlide.core/erl/pprint/erlide_boxp.yrl", 56).
yeccpars2_42_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_44_/1}).
-file("/home/mee/workspaces/erlidegit/erlide/org.erlide.core/erl/pprint/erlide_boxp.yrl", 33).
yeccpars2_44_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { { string , val ( __1 ) } , __1 }
  end | __Stack].

-compile({inline,yeccpars2_47_/1}).
-file("/home/mee/workspaces/erlidegit/erlide/org.erlide.core/erl/pprint/erlide_boxp.yrl", 36).
yeccpars2_47_(__Stack0) ->
 [__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { { val ( __1 ) , __2 , val ( __4 ) , __6 } , __1 }
  end | __Stack].

-compile({inline,yeccpars2_48_/1}).
-file("/home/mee/workspaces/erlidegit/erlide/org.erlide.core/erl/pprint/erlide_boxp.yrl", 34).
yeccpars2_48_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { { val ( __1 ) , __2 , __4 } , __1 }
  end | __Stack].

-compile({inline,yeccpars2_49_/1}).
-file("/home/mee/workspaces/erlidegit/erlide/org.erlide.core/erl/pprint/erlide_boxp.yrl", 27).
yeccpars2_49_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { boxvar , { val ( __2 ) , '_' } , quoted }
  end | __Stack].


-file("/home/mee/workspaces/erlidegit/erlide/org.erlide.core/erl/pprint/erlide_boxp.yrl", 268).
