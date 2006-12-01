%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%%     $Id$
%%
-module(erlide_pp).

%% A rather simplistic pretty printer for Erlang code in the same
%% format as returned from the parser. It does not try and work out
%% when it should break a line, but breaks or does break according to
%% some predefined rules whatever the consequences. It should really
%% be smarter.
%% This has now been improved upon: line breaks are sometimes inserted.
%% The cost is however quite high since the same code may be traversed
%% many times.

%% The after hook has been extended to work at each level, attribute,
%% function, clause, and expression. Lists of things, like guards, still
%% have to be lists of things.

-export([form/1,form/2,
   attribute/1,attribute/2,function/1,function/2,rule/1,rule/2,
   guard/1,guard/2,exprs/1,exprs/2,exprs/3,expr/1,expr/2,expr/3,expr/4]).

%% The following exports are here for backwards compatibility.
-export([seq/1,seq/2]).
-deprecated([{seq,1},{seq,2}]).

-import(lists, [flatten/1, map/2, max/1]).
-import(io_lib, [write/1,format/2,write_char/1,write_string/1]).
-import(erl_parse, [inop_prec/1,preop_prec/1,func_prec/0]).

-define(MAXLINE, 78).
-define(LEFT, 8). % Minimum number of columns to be gained by a line break.

%% seq(Expressions)
%% seq(Expressions, Hook)
%%  These calls are here for backwards compatibility (BC sucks!).

seq(Es) -> exprs(Es).

seq(Es, Hook) -> exprs(Es, Hook).

%% After each "thing" has been printed the cursor is left in place. It is
%% up to what comes after, or the caller to decide how to terminate the
%% previous "thing".

form(Thing) ->
     form(Thing, none).

form({attribute,Line,Name,Arg}, Hook) ->
    attribute({attribute,Line,Name,Arg}, Hook);
form({function,Line,Name,Arity,Clauses}, Hook) ->
    function({function,Line,Name,Arity,Clauses}, Hook);
form({rule,Line,Name,Arity,Clauses}, Hook) ->
    rule({rule,Line,Name,Arity,Clauses}, Hook);
%% These are specials to make it easier for the compiler.
form({error,E}, _Hook) ->
    format("~p~n", [{error,E}]);
form({warning,W}, _Hook) ->
    format("~p~n", [{warning,W}]);
form({eof,_Line}, _Hook) -> "\n".

attribute(Thing) ->
    attribute(Thing, none).

attribute({attribute,_Line,Name,Arg}, Hook) ->
    attribute(Name, Arg, Hook).

attribute(module, {M,Vs}, _Hook) ->
    format("-module(~s, ~s).\n", [pname(M), vlist(Vs)]);
attribute(module, M, _Hook) ->
    format("-module(~s).\n", [pname(M)]);
attribute(export, Falist, _Hook) ->
    format("-export(~s).\n", [falist(Falist)]);
attribute(import, Name, _Hook) when is_list(Name) ->
    format("-import(~s).\n", [pname(Name)]);
attribute(import, {From,Falist}, _Hook) when is_list(From) ->
    format("-import(~s, ~s).\n", [pname(From),falist(Falist)]);
attribute(import, {From,Falist}, _Hook) ->
    format("-import(~w, ~s).\n", [From,falist(Falist)]);
attribute(file, {Name,Line}, _Hook) ->
    format("-file(~p, ~w).\n", [Name,Line]);
attribute(record, {Name,Is}, Hook) ->
    Nl = flatten(write(Name)),
    format("-record(~w, ~s).\n",
     [Name,record_fields(Is, 10+length(Nl), Hook)]);
attribute(Name, Arg, _Hook) ->
    format("-~w(~w).\n", [Name,Arg]).

vlist([]) -> "[]";
vlist(Vs) -> vlist(Vs, $[).

vlist([V | Vs], P) ->
    [P, format("~s", [V]) | vlist(Vs, $,)];
vlist([], _) -> [$]].

pname(['' | As]) ->
    [$. | pname(As)];
pname([A]) ->
    write(A);
pname([A | As]) ->
    [write(A),$.|pname(As)];
pname(A) when is_atom(A) ->
    write(A).

falist([]) -> "[]";
falist(Fas) -> falist(Fas, $[).

falist([{Name,Arity}|Falist], P) ->
    [P,write(Name),$/,write(Arity)|falist(Falist, $,)];
falist([], _) -> [$]].

function(F) -> function(F, none).

function({function,_Line,Name,_Arity,Cs}, Hook) ->
    [clauses(fun (C, _, H) -> func_clause(Name, C, H) end, 0, Hook, Cs),".\n"].

func_clause(Name, {clause,Line,Head,Guard,Body}, Hook) ->
    Hl = expr({call,Line,{atom,Line,Name},Head}, 0, Hook),
    [guard_when(Hl, Guard, 0, Hook),
     body(Body, 4, Hook)].

rule(R) -> rule(R, none).

rule({rule,_Line,Name,_Arity,Cs}, Hook) ->
    [clauses(fun (C, _, H) -> rule_clause(Name, C, H) end, 0, Hook, Cs),".\n"].

rule_clause(Name, {clause,Line,Head,Guard,Body}, Hook) ->
    Hl = expr({call,Line,{atom,Line,Name},Head}, 0, Hook),
    [guard_when(Hl, Guard, 0, Hook),
     rule_body(Body, 4, Hook)].

rule_body(Es, I, Hook) ->
     [" :-",nl_indent(I)|lc_quals(Es, I, Hook)].

%% guard(GuardExpressions)
%% guard(GuardExpressions, Hook)
%% guard(GuardExpressions, Indentation, Hook)

guard(Gs) -> guard(Gs, none).

guard(Gs, Hook) -> guard(Gs, 0, Hook).

guard_no_when([E|Es], I, Hook) when is_list(E) ->
    separated_list_nl(fun guard0/3, "; ", I, Hook, [E|Es]);
guard_no_when([E|Es], I, Hook) ->
    guard_no_when([[E|Es]], I, Hook);
guard_no_when([], _, _) -> [].

guard_when(BeforeL, Guard, I, Hook) ->
    Szl = map( fun( E ) -> indentation(expr(E, Hook), 0)
               end, flatten(Guard)),
    Wsz = 6, % " when "
    Bsz = 3, % " ->"
    I1 = I + 4,
    Ia = indentation(BeforeL, I),
    [BeforeL|case (Szl =:= []) orelse (max(Szl) + Ia + Wsz =< ?MAXLINE-Bsz) of
                 false when I1+?LEFT =< Ia ->
                     [nl_indent(I1),guard(Guard, Wsz+I1, Hook)];
                 _ ->
                     guard(Guard, Wsz+Ia, Hook)
             end].

guard([E|Es], I, Hook) when is_list(E) ->
    " when " ++ separated_list_nl(fun guard0/3, "; ", I, Hook, [E|Es]);
guard([E|Es], I, Hook) ->
    guard([[E|Es]], I, Hook);
guard([], _, _) -> [].

guard0([E|Es], I, Hook) ->
    separated_list_nl(fun expr/3, ", ", I, Hook, [E|Es]);
guard0([], _, _) -> [].

%% body(Es, Indentation, Hook) -> [Char].

body(Es, I, Hook) ->
    [" ->",nl_indent(I)|exprs(Es, I, Hook)].

%% exprs(Expressions)
%% exprs(Expressions, Hook)
%% exprs(Expressions, Indentation, Punctuation, Hook)
%%  Prettyprint expressions.

-define(NONL, -10000). %A hack to prohibit line breaks

exprs(Es) ->
    exprs(Es, none).

exprs(Es, Hook) ->
    exprs(Es, ?NONL, Hook).

exprs(Es, I, Hook) ->
    separated_list(fun expr/3, "," ++ nl_indent(I), I, Hook, Es).

%% expr(Expression)
%% expr(Expression, Hook)
%% expr(Expression, Indentation, Hook)
%% expr(Expression, Indentation, Precedence, Hook)
%%  Prettyprint one expr. Seeing everything is a "expr" we have to handle
%%  operator precedences for arithmetic expressions as well.
%%  N.B. We use a simple length/1 call to calculate indent

expr(E) -> expr(E, ?NONL, 0, none).

expr(E, Hook) -> expr(E, ?NONL, 0, Hook).

expr(E, I, Hook) -> expr(E, I, 0, Hook).

expr({var,_,V}, _, _, _) when is_integer(V) ->	%Special hack for Robert
    format("_~w", [V]);
expr({var,_,V}, _, _, _) -> format("~s", [V]);
expr({char,_,C}, _, _, _) -> write_char(C);
expr({integer,_,N}, _, _, _) -> write(N);
expr({float,_,F}, _, _, _) -> write(F);
expr({atom,_,A}, _, _, _) -> write(A);
expr({string,_,S}, _, _, _) -> write_string(S);
expr({nil,_}, _, _, _) -> "[]";
expr({macro,_,V}, _, _, _) -> format("~s", [V]);
expr({cons,_,H,T}, I, _, Hook) ->
    Hl = expr(H, ?NONL, 0, Hook),
    Tl = tail(T, "", ?NONL, Hook),
    Cl = [$[,Hl,Tl],
    case too_wide(Cl, I) of
        true ->
            I1 = I + 1,
            [$[,expr(H, I1, 0, Hook)|tail(T, nl_indent(I1), I1, Hook)];
        false ->
            Cl
    end;
expr({lc,_,E,Qs}, I, _Prec, Hook) ->
    ["[ ",
     expr(E, I+2, 0, Hook),
     " ||", nl_indent(I+4),
     lc_quals(Qs, I+4, Hook),
     nl_indent(I),"]"];
expr({tuple,_,Elts}, I, _, Hook) ->
    expr_list(Elts, "{", "}", I, Hook);
%%expr({struct,_,Tag,Elts}, I, _, Hook) ->
%%    Tl = flatten(write(Tag)),
%%    [Tl|expr_list(Elts, "{", "}", I+length(Tl), Hook)];
expr({record_index, _, Name, F}, I, Prec, Hook) ->
    {P,R} = preop_prec('#'),
    Nl = flatten(write(Name)),
    El = ["#",Nl,".",expr(F, I+length(Nl)+2, R, Hook)],
    maybe_paren(P, Prec, El);
expr({record, _, Name, Fs}, I, Prec, Hook) ->
    {P,_R} = preop_prec('#'),
    Nl = flatten(write(Name)),
    El = ["#",Nl,record_fields(Fs, I+length(Nl)+1, Hook)],
    maybe_paren(P, Prec, El);
expr({record_field, _, Rec, Name, F}, I, Prec, Hook) ->
    {L,P,R} = inop_prec('#'),
    Rl = expr(Rec, I, L, Hook),
    Nl = flatten(write(Name)),
    El = [Rl,"#",Nl,".",expr(F, indentation(Rl, I)+length(Nl)+2, R, Hook)],
    maybe_paren(P, Prec, El);
expr({record, _, Rec, Name, Fs}, I, Prec, Hook) ->
    {L,P,_R} = inop_prec('#'),
    Rl = expr(Rec, I, L, Hook),
    Nl = flatten(write(Name)),
    El = [Rl,"#",Nl,record_fields(Fs, indentation(Rl, I)+length(Nl)+1, Hook)],
    maybe_paren(P, Prec, El);
expr({record_field, _, {atom,_,''}, F}, I, Prec, Hook) ->
    {_,P,R} = inop_prec('.'),
    El = [".",expr(F, I + 1, R, Hook)],
    maybe_paren(P, Prec, El);
expr({record_field, _, Rec, F}, I, Prec, Hook) ->
    {L,P,R} = inop_prec('.'),
    Rl = expr(Rec, I, L, Hook),
    El = [Rl,".",expr(F, indentation(Rl, I)+1, R, Hook)],
    maybe_paren(P, Prec, El);
expr({block,_,Es}, I, _, Hook) ->
    ["begin",nl_indent(I+4),
     exprs(Es, I+4, Hook),
     nl_indent(I), "end"];
expr({'if',_,Cs}, I, _, Hook) ->
    I1 = I + 4,
    ["if",nl_indent(I1),
     if_clauses(Cs, I1, Hook),
     nl_indent(I),"end"];
expr({'case',_,Expr,Cs}, I, _, Hook) ->
    I1 = I + 4,
    ["case ",
     expr(Expr, I+5, 0, Hook),
     " of",nl_indent(I1),
     cr_clauses(Cs, I1, Hook),
     nl_indent(I),"end"];
expr({'cond',_,Cs}, I, _, Hook) ->
    I1 = I + 4,
    ["cond",nl_indent(I1),
     cond_clauses(Cs, I1, Hook),
     nl_indent(I),"end"];
expr({'receive',_,Cs}, I, _, Hook) ->
    I1 = I + 4,
    ["receive",nl_indent(I1),
     cr_clauses(Cs, I1, Hook),
     nl_indent(I),"end"];
expr({'receive',_,Cs,To,ToOpt}, I, _, Hook) ->
    I1 = I + 4,
    ["receive",
     if						%Must special case no clauses
   Cs =/= [] -> [nl_indent(I1),cr_clauses(Cs, I1, Hook)];
   true -> ""
     end,
     %% Now for the timeout bit.
     nl_indent(I), "after",
     nl_indent(I1),
     expr(To, I1, 0, Hook),
     body(ToOpt, I1+4, Hook),
     nl_indent(I), "end"];
expr({'fun',_,{function,F,A}}, _I, _Prec, _Hook) ->
    ["fun ",write(F),$/,write(A)];
expr({'fun',_,{function,F,A},Extra}, I, _Prec, _Hook) ->
    [fun_info(Extra, I),
     "fun ",write(F),$/,write(A)];
expr({'fun',_,{function,M,F,A}}, _I, _Prec, _Hook) ->
    ["fun ",write(M),$:,write(F),$/,write(A)];
expr({'fun',_,{clauses,Cs}}, I, _Prec, Hook) ->
    ["fun ",
     fun_clauses(Cs, I+4, Hook),
     " end"];
expr({'fun',_,{clauses,Cs},Extra}, I, _Prec, Hook) ->
    [fun_info(Extra, I),
     "fun ",
     fun_clauses(Cs, I+4, Hook),
     " end"];
expr({'query',_,Lc}, I, _Prec, Hook) ->
    ["query ",
     expr(Lc, I+6, 0, Hook),
     " end"];
expr({call,L,{remote,_,{atom,_,M},{atom,_,F}=N}=Name,Args}, I, Prec, Hook) ->
    case erl_internal:bif(M, F, length(Args)) of
        true ->
            expr({call,L,N,Args}, I, Prec, Hook);
        false ->
            call(Name, Args, I, Prec, Hook)
    end;
expr({call,_,Name,Args}, I, Prec, Hook) ->
    call(Name, Args, I, Prec, Hook);
expr({'try',_,Es,Scs,Ccs,As}, I, _, Hook) ->
    I1 = I + 4,
    ["try",nl_indent(I1),
     exprs(Es, I1, Hook),nl_indent(I),
     if Scs =:= [] ->
       [];
  true ->
       ["of",nl_indent(I1),
        cr_clauses(Scs, I1, Hook),nl_indent(I)]
     end,
     if Ccs =:= [] ->
             [];
        true ->
             ["catch",nl_indent(I1),
              try_clauses(Ccs, I1, Hook),nl_indent(I)]
     end,
     if As =:= [] ->
       [];
  true ->
       ["after",nl_indent(I1),
        exprs(As, I1, Hook),nl_indent(I)]
     end,
     "end"];
expr({'catch',_,Expr}, I, Prec, Hook) ->
    {P,R} = preop_prec('catch'),
    El = ["catch ",expr(Expr, I+6, R, Hook)],
    maybe_paren(P, Prec, El);
expr({match,_,Lhs,Rhs}, I, Prec, Hook) ->
    {L,P,R} = inop_prec('='),
    Pl = expr(Lhs, I, L, Hook),
    Ip = indentation(Pl, I)+3,
    Rl1 = expr(Rhs, ?NONL, R, Hook),
    El = [Pl," = ",
          case too_wide(Rl1, Ip) of
              true when Ip - I >= ?LEFT ->
                  [nl_indent(I+4), expr(Rhs, I+4, R, Hook)];
              _ ->
                  expr(Rhs, Ip, R, Hook)
          end],
    maybe_paren(P, Prec, El);
expr({op,_,Op,Arg}, I, Prec, Hook) ->
    {P,R} = preop_prec(Op),
    Ol = flatten(format("~s ", [Op])),
    El = [Ol,expr(Arg, I+length(Ol), R, Hook)],
    maybe_paren(P, Prec, El);
expr({op,_,Op,Larg,Rarg}, I, Prec, Hook) ->
    {L,P,R} = inop_prec(Op),
    Ll1 = expr(Larg, ?NONL, L, Hook),
    Ol1 = flatten(format(" ~s ", [Op])),
    Lr1 = expr(Rarg, ?NONL, R, Hook),
    El1 = maybe_paren(P, Prec, [Ll1,Ol1,Lr1]),
    case too_wide(El1, I) of
        true ->
            X = n_paren(P, Prec),
            Ll = expr(Larg, I+X, L, Hook),
            Ol2 = flatten(format(" ~s", [Op])),
            Ol = case too_wide(Ol2, indentation(Ll, I+X))  of
         true ->
       [nl_indent(I+X), Ol2];
         false ->
                         Ol2
                 end,
            Lr1 = expr(Rarg, ?NONL, R, Hook),
            Lr = case too_wide(Lr1, indentation([Ll,Ol," "], I+X)) of
          true ->
        [nl_indent(I+X), expr(Rarg, I+X, R, Hook)];
                false ->
                    [" ",Lr1]
            end,
            El = [Ll,Ol,Lr],
            maybe_paren(P, Prec, El);
        false ->
            El1
    end;
%% Special expressions which are not really legal everywhere.
expr({remote,_,M,F}, I, Prec, Hook) ->
    {L,P,R} = inop_prec(':'),
    Ml = expr(M, I, L, Hook),
    El = [Ml,":",expr(F, indentation(Ml, I)+1, R, Hook)],
    maybe_paren(P, Prec, El);
%% BIT SYNTAX:
expr({bin,_,Fs}, I, _, Hook) ->
    bit_grp(Fs,I,Hook);
%% Special case for straight values.
expr({value,_,Val}, _, _,_) ->
    write(Val);
%% Now do the hook.
expr(Other, _Indentation, _Precedence, none) ->
    format("INVALID-FORM:~w:",[Other]);
expr(Expr, Indentation, Precedence, {Mod,Func,Eas}) when Mod =/= 'fun' ->
    apply(Mod, Func, [Expr,Indentation,Precedence,{Mod,Func,Eas}|Eas]);
expr(Expr, Indentation, Precedence, Func) ->
    Func(Expr, Indentation, Precedence, Func).

call(Name, Args, I, Prec, Hook) ->
    {F,P} = func_prec(),
    Nl = expr(Name, I, F, Hook),
    In = indentation(Nl, I),
    Al = expr_list(Args, "(", ")", ?NONL, Hook),
    case too_wide(maybe_paren(P, Prec, Al), In) of
        true when In-I-4 >= ?LEFT ->
            X = n_paren(P, Prec),
            Inl = I + X+4,
            El = [Nl,nl_indent(Inl),expr_list(Args, "(", ")", Inl, Hook)],
            maybe_paren(P, Prec, El);
        _ ->
            El = [Nl|expr_list(Args, "(", ")", In, Hook)],
            maybe_paren(P, Prec, El)
    end.

fun_info(Extra, I) when I >= 0 ->
    [io_lib:format("% fun-info: ~p", [Extra]),
     nl_indent(I)];
fun_info(_Extra, _I) ->
    %% Force line breaks just to get fun-info right.
    not_a_valid_iolist.

%% BITS:
bit_grp(Fs,I,Hook) ->
    try
        S = bin_string(Fs),
        true = io_lib:printable_list(S),
        [{bin_element,L,_,_,_}|_] = Fs,
        ["<<",expr({string,L,S}, I+2, 0, Hook),">>"]
    catch _:_ ->
        ["<<", bit_elems(Fs,I+2,Hook),">>"]
    end.

bin_string([]) ->
    [];
bin_string([{bin_element,_,{char,_,C},_,_}|Bin]) ->
    [C | bin_string(Bin)].

bit_elems([E], I, Hook) ->
    [ bit_elem(E, I, Hook) ];
bit_elems([E|Es], I, Hook) ->
    [ bit_elem(E, I, Hook), ",", nl_indent(I),
      bit_elems(Es, I, Hook) ];
bit_elems([],_I,_Hook) ->
    [].

bit_elem({bin_element,_,Expr,Sz,Types}, I, Hook) ->
    Expr1 =
  if Sz =/= default ->
    {remote, 0, Expr, Sz};
     true ->
    Expr
  end,
    if Types =/= default ->
      [expr(Expr1,I,0,Hook), $/, bit_elem_types(Types)];
       true ->
      expr(Expr1,I,0,Hook)
    end.

bit_elem_types([T]) ->
    bit_elem_type(T);
bit_elem_types([T | Rest]) ->
    [bit_elem_type(T), $-, bit_elem_types(Rest)].

bit_elem_type({A, B}) ->
    expr({remote, 0, erl_parse:abstract(A), erl_parse:abstract(B)});
bit_elem_type(T) ->
    expr(erl_parse:abstract(T)).

%%% end of BITS

record_fields(Fs, I, Hook) ->
    I1 = I + 1,
    ["{",
     separated_list(fun record_field/3, "," ++ nl_indent(I1), I1, Hook, Fs),
     "}"].

record_field({record_field,_,F,Val}, I, Hook) ->
    {L,_P,R} = inop_prec('='),
    Fl = expr(F, I, L, Hook),
    [Fl," = ",expr(Val, indentation(Fl, I)+3, R, Hook)];
record_field({record_field,_,F}, I, Hook) ->
    expr(F, I, Hook).

tail({cons,_,H,T}, S, I, Hook) ->
    [$,, S, expr(H, I, 0, Hook)|tail(T, S, I, Hook)];
tail({nil,_}, _, _, _) -> "]";
tail(Other, S, I, Hook) ->
    [$|,S,expr(Other, I, 0, Hook),$]].

expr_list(Es, First, Last, I, Hook) ->
    [First,
     separated_list_nl(fun expr/3, ",", I+length(First), Last, Hook, Es),
     Last].

%% if_clauses(Clauses, Indentation, Hook) -> [Char].
%%  Print 'if' clauses.

if_clauses(Cs, I, Hook) -> clauses(fun if_clause/3, I, Hook, Cs).

if_clause({clause,_,[],G,B}, I, Hook) ->
    [guard_no_when(G, I+2, Hook),
     body(B, I+4, Hook)].

%% cr_clauses(Clauses, Indentation, Hook) -> [Char].
%%  Print 'case'/'receive' clauses.

cr_clauses(Cs, I, Hook) -> clauses(fun cr_clause/3, I, Hook, Cs).

cr_clause({clause,_,[T],G,B}, I, Hook) ->
    El = expr(T, I, 0, Hook),
    [guard_when(El, G, I, Hook),
     body(B, I+4, Hook)].

%% try_clauses(Clauses, Indentation, Hook) -> [Char].
%%  Print 'try' clauses.

try_clauses(Cs, I, Hook) -> clauses(fun try_clause/3, I, Hook, Cs).

try_clause({clause,_,[{tuple,_,[{atom,_,throw},V,S]}],G,B}, I, Hook) ->
    El = expr(V, I, 0, Hook),
    Sl = stack_backtrace(S, El, I, Hook),
    [guard_when([El,Sl], G, I, Hook),
     body(B, I+4, Hook)];
try_clause({clause,_,[{tuple,_,[C,V,S]}],G,B}, I, Hook) ->
    Cs = expr(C, I, 0, Hook),
    El = expr(V, indentation(Cs, I)+1, 0, Hook),
    CsEl = [Cs, ":", El],
    Sl = stack_backtrace(S, indentation(CsEl, I), I, Hook),
    [guard_when([CsEl,Sl], G, I, Hook),
     body(B, I+4, Hook)].

stack_backtrace({var,_,'_'}, _Es, _I, _Hook) ->
    [];
stack_backtrace(S, Es, I, Hook) ->
    [":", expr(S, indentation(Es, I)+1, 0, Hook)].

%% fun_clauses(Clauses, Indentation, Hook) -> [Char].
%%  Print 'fun' clauses.

fun_clauses(Cs, I, Hook) -> clauses(fun fun_clause/3, I, Hook, Cs).

fun_clause({clause,_,A,G,B}, I, Hook) ->
    El = expr_list(A, "(", ")", I, Hook),
    [guard_when(El, G, I, Hook),
     body(B, I+4, Hook)].

%% cond_clauses(Clauses, Indentation, Hook) -> [Char].
%%  Print 'cond' clauses.

cond_clauses(Cs, I, Hook) -> clauses(fun cond_clause/3, I, Hook, Cs).

cond_clause({clause,_,[],[[E]],B}, I, Hook) ->
    [expr(E, I, 0, Hook),
     body(B, I+4, Hook)].

%% clauses(Type, Identation, Hook) -> [Char].
%%  Generic clause printing function.

clauses(Type, I, Hook, Cs) ->
    separated_list(Type, ";" ++ nl_indent(I), I, Hook, Cs).

separated_list_nl(Fun, S, I, Hook, Es) ->
    separated_list_nl(Fun, S, I, [], Hook, Es).

separated_list_nl(Fun, S, I, After, Hook, Es) ->
    L = separated_list(Fun, S, ?NONL, Hook, Es),
    case too_wide([L,After], I) of
        true ->
            separated_list(Fun, S ++ nl_indent(I), I, Hook, Es);
        false ->
            L
    end.

%% separated_list(Fun, Sep, Indentation, Hook, Es) -> [Char].
%%  Generic function for printing a list of things with separators
%%  between them. We can handle the empty case.

separated_list(Fun, S, I, Hook, [E1|Es]) ->
    [Fun(E1, I, Hook)|map( fun (E) -> [S,Fun(E, I, Hook)] end, Es)];
separated_list(_Fun, _S, _I, _Hook, []) -> "".

%% lc_quals(Qualifiers, Indentation, Hook)
%% List comprehension qualifiers

lc_quals(Qs, I, Hook) ->
    separated_list(fun lc_qual/3, "," ++ nl_indent(I), I, Hook, Qs).

lc_qual({generate,_,Pat,E}, I, Hook) ->
    Pl = expr(Pat, I, 0, Hook),
    [Pl," <- ",expr(E, indentation(Pl, I)+4, 0, Hook)];
lc_qual(Q, I, Hook) ->
    expr(Q, I, 0, Hook).

%%
%% Utilities
%%

indentation(E, I) when I >= 0 ->
    io_lib_format:indentation(E, I);
indentation(_E, I) ->
    I.

maybe_paren(P, Prec, Expr) when P < Prec -> [$(,Expr,$)];
maybe_paren(_P, _Prec, Expr) -> Expr.

n_paren(P, Prec) when P < Prec ->
    1;
n_paren(_P, _Prec) ->
    0.

nl_indent(I) when I >= 0 -> [$\n|string:chars($\s, I)];
nl_indent(_) -> " ".

%% fun-info will always break the line
too_wide(L, I) ->
    try iolist_size(L),
        (I >= 0) andalso (indentation(L, I) > ?MAXLINE)
    catch _:_ -> true
    end.
