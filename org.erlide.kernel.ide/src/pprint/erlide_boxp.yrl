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
%% Definition of the Box grammar.

Nonterminals
pprule
sfopt sfoptlist 
aopt aoptlist aoptlist1
xvar 
box boxlist.


Terminals
integer atom string var
'[' ']' '=' '(' ')' ',' '*' '{' '}' '#'
.

Rootsymbol pprule.

pprule -> box : mkbox('$1').

xvar -> var : {boxvar, {val('$1'), '_'}}.
xvar -> '#' var : {boxvar, {val('$2'), '_'}, quoted}.
xvar -> atom '(' var ')' : {boxvar, {val('$3'), val('$1')}}.
xvar -> var '*' : {splicevar, {val('$1'), '_'}}.
xvar -> atom '(' var ')' '*' : {splicevar, {val('$3'), val('$1')}}.

box -> xvar: {'$1', '$1'}.
box -> string: {{string, val('$1')}, '$1'}.
box -> atom sfoptlist '[' boxlist ']' : {{val('$1'), '$2', '$4'}, '$1'}.
box -> atom aoptlist sfoptlist '[' boxlist ']' : {{val('$1'), '$2', '$3', '$5'}, '$1'}.
box -> atom sfoptlist '[' string ',' box ']' : {{val('$1'), '$2', val('$4'), '$6'}, '$1'}.
box -> '{' box '}' : {{list, val('$2'), {{string, ""}, '$3'}}, '$1'}.
box -> '{' box box '}' : {{list, val('$2'), val('$3')}, '$1'}.

sfoptlist -> '$empty' : [].
sfoptlist -> sfopt sfoptlist : ['$1' | '$2'].

sfopt -> atom '=' integer : {{val('$1'), val('$3')}, '$1'}.
sfopt -> integer : {{default, val('$1')}, '$1'}.
sfopt -> atom '=' atom : {{val('$1'), val('$3')}, '$1'}.
sfopt -> atom '=' var : {{val('$1'), val('$3')}, '$1'}.

aopt -> atom sfoptlist : {{val('$1'), '$2'}, '$1'}.

aoptlist -> '(' ')' : [].
aoptlist -> '(' aoptlist1 ')' : '$2'.

aoptlist1 -> aopt : ['$1'].
aoptlist1 -> aopt ',' aoptlist1 : ['$1' | '$3'].

boxlist -> '$empty' : [].
boxlist -> box boxlist : ['$1' | '$2'].

Erlang code.

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

