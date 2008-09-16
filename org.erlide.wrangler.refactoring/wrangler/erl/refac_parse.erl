-module(refac_parse).
-export([parse/1, parse_and_scan/1, format_error/1]).

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
%%     $Id: refac_parse.erl,v 1.1 2008/05/20 14:49:41 go30 Exp $
%%
%% Modified: 17 Jan 2007 by  Huiqing Li <hl@kent.ac.uk>
-export([parse_form/1,parse_exprs/1,parse_term/1]).
-export([normalise/1,abstract/1,tokens/1,tokens/2]).
-export([abstract/2, package_segments/1]).
-export([inop_prec/1,preop_prec/1,func_prec/0,max_prec/0]).

%% The following directive is needed for (significantly) faster compilation
%% of the generated .erl file by the HiPE compiler.  Please do not remove.
-compile([inline,{hipe,[{regalloc,linear_scan}]}]).


%% mkop(Op, Arg) -> {op,Line,Op,Arg}.
%% mkop(Left, Op, Right) -> {op,Line,Op,Left,Right}.

mkop(L, {Op,Pos}, R) -> {op,Pos,Op,L,R}.

mkop({Op,Pos}, A) -> {op,Pos,Op,A}.

%% keep track of line info in tokens
line(Tup) -> element(2, Tup).

%% Entry points compatible to old erl_parse.
%% These really suck and are only here until Calle gets multiple
%% entry points working.

parse_form(Tokens) ->
    parse(Tokens).

parse_exprs(Tokens) ->
    case parse([{atom,0,f},{'(',0},{')',0},{'->',0}|Tokens]) of
	{ok,{function,_Lf,f,0,[{clause,_Lc,[],[],Exprs}]}} ->
	    {ok,Exprs};
	{error,E} -> {error,E}
    end.

parse_term(Tokens) ->
    case parse([{atom,0,f},{'(',0},{')',0},{'->',0}|Tokens]) of
	{ok,{function,_Lf,f,0,[{clause,_Lc,[],[],[Expr]}]}} ->
	    case catch normalise(Expr) of
		{'EXIT',_R} ->
		    {error,{line(Expr),?MODULE,"bad term"}};
		Term -> {ok,Term}
	    end;
	{ok,{function,_Lf,f,0,[{clause,_Lc,[],[],[_E1,E2|_Es]}]}} ->
	    {error,{line(E2),?MODULE,"bad term"}};
	{error,E} -> {error,E}
    end.

%% build_attribute(AttrName, AttrValue) ->
%%	{attribute,Line,module,Module}
%%	{attribute,Line,export,Exports}
%%	{attribute,Line,import,Imports}
%%	{attribute,Line,record,{Name,Inits}}
%%	{attribute,Line,file,{Name,Line}}
%%	{attribute,Line,Name,Val}

build_attribute({atom,La,module}, Val) ->
    case Val of
	[{atom,_Lm,Module}] ->
	    {attribute,La,module,Module};
	[{atom,_Lm,Module},ExpList] ->
	    {attribute,La,module,{Module,var_list(ExpList)}};
	[Name] ->
	    case package_segments(Name) of
		error ->
		    error_bad_decl(La, module);
		Module ->
		    {attribute,La,module,Module}
	    end;
	[Name,ExpList] ->
	    case package_segments(Name) of
		error ->
		    error_bad_decl(La, module);
		Module ->
		    {attribute,La,module,{Module,var_list(ExpList)}}
	    end;
	_Other ->
	    error_bad_decl(La, module)
    end;
build_attribute({atom,La,export}, Val) ->
    case Val of
	[ExpList] ->
	    {attribute,La,export,farity_list(ExpList)};
	_Other -> error_bad_decl(La, export)
    end;
build_attribute({atom,La,import}, Val) ->
    case Val of
	[Name] ->
	    case package_segments(Name) of
		error ->
		    error_bad_decl(La, import);
		Module ->
		    {attribute,La,import,Module}
	    end;
	[{atom,_Lm,Mod},ImpList] ->
	    {attribute,La,import,{Mod,farity_list(ImpList)}};
	[Name, ImpList] ->
	    case package_segments(Name) of
		error ->
		    error_bad_decl(La, import);
		Module ->
		    {attribute,La,import,{Module,farity_list(ImpList)}}
	    end;
	_Other -> error_bad_decl(La, import)
    end;
build_attribute({atom,La,record}, Val) ->
    case Val of
	[{atom,_Ln,Record},RecTuple] ->
	    {attribute,La,record,{Record,record_tuple(RecTuple)}};
	_Other -> error_bad_decl(La, record)
    end;
build_attribute({atom,La,file}, Val) ->
    case Val of
	[{string,_Ln,Name},{integer,_Ll,Line}] ->
	    {attribute,La,file,{Name,Line}};
	_Other -> error_bad_decl(La, file)
    end;
build_attribute({atom,La,Attr}, Val) ->
    case Val of
	[Expr] ->
	    {attribute,La,Attr,term(Expr)};
	_Other -> return_error(La, "bad attribute")
    end.

var_list({cons,_Lc,{var,L,V},Tail}) ->
    [{var, L, V}|var_list(Tail)];            % Modified by Huiqing Li
var_list({nil,_Ln}) -> [];
var_list(Other) ->
    return_error(line(Other), "bad variable list").

error_bad_decl(L, S) ->
    return_error(L, io_lib:format("bad ~w declaration", [S])).

farity_list({cons,_Lc,{op,_Lo,'/',{atom,La,A},{integer,Li,I}},Tail}) ->
    [{{atom, La,A},{integer, Li, I}}|farity_list(Tail)];     %% Modified by Huiqing Li.
farity_list({nil,_Ln}) -> [];
farity_list(Other) ->
    return_error(line(Other), "bad function arity").

record_tuple({tuple,_Lt,Fields}) ->
    record_fields(Fields);
record_tuple(Other) ->
    return_error(line(Other), "bad record declaration").

record_fields([{atom,La,A}|Fields]) ->
    [{record_field,La,{atom,La,A}}|record_fields(Fields)];
record_fields([{match,_Lm,{atom,La,A},Expr}|Fields]) ->
    [{record_field,La,{atom,La,A},Expr}|record_fields(Fields)];
record_fields([Other|_Fields]) ->
    return_error(line(Other), "bad record field");
record_fields([]) -> [].

term(Expr) ->
    case catch normalise(Expr) of
	{'EXIT',_R} -> return_error(line(Expr), "bad attribute");
	Term -> Term
    end.

package_segments(Name) ->
    package_segments(Name, [], []).

package_segments({record_field, _, F1, F2}, Fs, As) ->
    package_segments(F1, [F2 | Fs], As);
package_segments({atom, _, A}, [F | Fs], As) ->
    package_segments(F, Fs, [A | As]);
package_segments({atom, _, A}, [], As) ->
    lists:reverse([A | As]);
package_segments(_, _, _) ->
    error.

%% build_function([Clause]) -> {function,Line,Name,Arity,[Clause]}

build_function(Cs) ->
    Name = element(3, hd(Cs)),
    Arity = length(element(4, hd(Cs))),
    {function,line(hd(Cs)),Name,Arity,check_clauses(Cs, Name, Arity)}.

%% build_rule([Clause]) -> {rule,Line,Name,Arity,[Clause]'}

build_rule(Cs) ->
    Name = element(3, hd(Cs)),
    Arity = length(element(4, hd(Cs))),
    {rule,line(hd(Cs)),Name,Arity,check_clauses(Cs, Name, Arity)}.

%% build_fun(Line, [Clause]) -> {'fun',Line,{clauses,[Clause]}}.

build_fun(Line, Cs) ->
    Arity = length(element(4, hd(Cs))),
    {'fun',Line,{clauses,check_clauses(Cs, 'fun', Arity)}}.

check_clauses(Cs, Name, Arity) ->
     mapl(fun ({clause,L,N,As,G,B}) when N == Name, length(As) == Arity ->
		 {clause,L,As,G,B};
	     ({clause,L,_N,_As,_G,_B}) ->
		 return_error(L, "head mismatch") end, Cs).

build_try(L,Es,Scs,{Ccs,As}) ->
    {'try',L,Es,Scs,Ccs,As}.

%% mapl(F,List)
%% an alternative map which always maps from left to right
%% and makes it possible to interrupt the mapping with throw on
%% the first occurence from left as expected.
%% can be removed when the jam machine (and all other machines)
%% uses the standardized (Erlang 5.0) evaluation order (from left to right)
mapl(F, [H|T]) ->
	V = F(H),
	[V | mapl(F,T)];
mapl(_, []) ->
	[].

%% normalise(AbsTerm)
%% abstract(Term)
%%  Convert between the abstract form of a term and a term.

normalise({char,_,C}) -> C;
normalise({integer,_,I}) -> I;
normalise({float,_,F}) -> F;
normalise({atom,_,A}) -> A;
normalise({string,_,S}) -> S;
normalise({nil,_}) -> [];
normalise({bin,_,Fs}) ->
    {value, B, _} =
	eval_bits:expr_grp(Fs, [],
			   fun(E, _) ->
				   {value, normalise(E), []}
			   end, [], true),
    B;
normalise({cons,_,Head,Tail}) ->
    [normalise(Head)|normalise(Tail)];
normalise({tuple,_,Args}) ->
    list_to_tuple(normalise_list(Args));
%% Atom dot-notation, as in 'foo.bar.baz'
normalise({record_field,_,_,_}=A) ->
    case package_segments(A) of
	error -> erlang:fault({badarg, A});
	As -> list_to_atom(packages:concat(As))
    end;
%% Special case for unary +/-.
normalise({op,_,'+',{char,_,I}}) -> I;
normalise({op,_,'+',{integer,_,I}}) -> I;
normalise({op,_,'+',{float,_,F}}) -> F;
normalise({op,_,'-',{char,_,I}}) -> -I;		%Weird, but compatible!
normalise({op,_,'-',{integer,_,I}}) -> -I;
normalise({op,_,'-',{float,_,F}}) -> -F;
normalise(X) -> erlang:fault({badarg, X}).

normalise_list([H|T]) ->
    [normalise(H)|normalise_list(T)];
normalise_list([]) ->
    [].

abstract(T) when integer(T) -> {integer,0,T};
abstract(T) when float(T) -> {float,0,T};
abstract(T) when atom(T) -> {atom,0,T};
abstract([]) -> {nil,0};
abstract(B) when binary(B) ->
    {bin, 0, lists:map(fun(Byte) ->
			       {bin_element, 0,
				{integer, 0, Byte}, default, default}
		       end,
		       binary_to_list(B))};
abstract([C|T]) when integer(C), 0 =< C, C < 256 ->
    abstract_string(T, [C]);
abstract([H|T]) ->
    {cons,0,abstract(H),abstract(T)};
abstract(Tuple) when tuple(Tuple) ->
    {tuple,0,abstract_list(tuple_to_list(Tuple))}.

abstract_string([C|T], String) when integer(C), 0 =< C, C < 256 ->
    abstract_string(T, [C|String]);
abstract_string([], String) ->
    {string, 0, lists:reverse(String)};
abstract_string(T, String) ->
    not_string(String, abstract(T)).

not_string([C|T], Result) ->
    not_string(T, {cons, 0, {integer, 0, C}, Result});
not_string([], Result) ->
    Result.

abstract_list([H|T]) ->
    [abstract(H)|abstract_list(T)];
abstract_list([]) ->
    [].

%%% abstract/2 keeps the line number
abstract(T, Line) when integer(T) -> {integer,Line,T};
abstract(T, Line) when float(T) -> {float,Line,T};
abstract(T, Line) when atom(T) -> {atom,Line,T};
abstract([], Line) -> {nil,Line};
abstract(B, Line) when binary(B) ->
    {bin, Line, lists:map(fun(Byte) ->
			       {bin_element, Line,
				{integer, Line, Byte}, default, default}
		       end,
		       binary_to_list(B))};
abstract([C|T], Line) when integer(C), 0 =< C, C < 256 ->
    abstract_string(T, [C], Line);
abstract([H|T], Line) ->
    {cons,Line,abstract(H, Line),abstract(T, Line)};
abstract(Tuple, Line) when tuple(Tuple) ->
    {tuple,Line,abstract_list(tuple_to_list(Tuple), Line)}.

abstract_string([C|T], String, Line) when integer(C), 0 =< C, C < 256 ->
    abstract_string(T, [C|String], Line);
abstract_string([], String, Line) ->
    {string, Line, lists:reverse(String)};
abstract_string(T, String, Line) ->
    not_string(String, abstract(T, Line), Line).

not_string([C|T], Result, Line) ->
    not_string(T, {cons, Line, {integer, Line, C}, Result}, Line);
not_string([], Result, _Line) ->
    Result.

abstract_list([H|T], Line) ->
    [abstract(H, Line)|abstract_list(T, Line)];
abstract_list([], _Line) ->
    [].

%% tokens(AbsTerm) -> [Token]
%% tokens(AbsTerm, More) -> [Token]
%%  Generate a list of tokens representing the abstract term.

tokens(Abs) ->
    tokens(Abs, []).

tokens({char,L,C}, More) -> [{char,L,C}|More];
tokens({integer,L,N}, More) -> [{integer,L,N}|More];
tokens({float,L,F}, More) -> [{float,L,F}|More];
tokens({atom,L,A}, More) -> [{atom,L,A}|More];
tokens({var,L,V}, More) -> [{var,L,V}|More];
tokens({string,L,S}, More) -> [{string,L,S}|More];
tokens({nil,L}, More) -> [{'[',L},{']',L}|More];
tokens({cons,L,Head,Tail}, More) ->
    [{'[',L}|tokens(Head, tokens_tail(Tail, More))];
tokens({tuple,L,[]}, More) ->
    [{'{',L},{'}',L}|More];
tokens({tuple,L,[E|Es]}, More) ->
    [{'{',L}|tokens(E, tokens_tuple(Es, line(E), More))].

tokens_tail({cons,L,Head,Tail}, More) ->
    [{',',L}|tokens(Head, tokens_tail(Tail, More))];
tokens_tail({nil,L}, More) ->
    [{']',L}|More];
tokens_tail(Other, More) ->
    L = line(Other),
    [{'|',L}|tokens(Other, [{']',L}|More])].

tokens_tuple([E|Es], Line, More) ->
    [{',',Line}|tokens(E, tokens_tuple(Es, line(E), More))];
tokens_tuple([], Line, More) ->
    [{'}',Line}|More].

%% Give the relative precedences of operators.

inop_prec('=') -> {150,100,100};
inop_prec('!') -> {150,100,100};
inop_prec('orelse') -> {160,150,150};
inop_prec('andalso') -> {200,160,160};
inop_prec('==') -> {300,200,300};
inop_prec('/=') -> {300,200,300};
inop_prec('=<') -> {300,200,300};
inop_prec('<') -> {300,200,300};
inop_prec('>=') -> {300,200,300};
inop_prec('>') -> {300,200,300};
inop_prec('=:=') -> {300,200,300};
inop_prec('=/=') -> {300,200,300};
inop_prec('++') -> {300,300,400};
inop_prec('--') -> {300,300,400};
inop_prec('+') -> {400,400,500};
inop_prec('-') -> {400,400,500};
inop_prec('bor') -> {400,400,500};
inop_prec('bxor') -> {400,400,500};
inop_prec('bsl') -> {400,400,500};
inop_prec('bsr') -> {400,400,500};
inop_prec('or') -> {400,400,500};
inop_prec('xor') -> {400,400,500};
inop_prec('*') -> {500,500,600};
inop_prec('/') -> {500,500,600};
inop_prec('div') -> {500,500,600};
inop_prec('rem') -> {500,500,600};
inop_prec('band') -> {500,500,600};
inop_prec('and') -> {500,500,600};
inop_prec('#') -> {800,700,800};
inop_prec(':') -> {900,800,900};
inop_prec('.') -> {900,900,1000}.

preop_prec('catch') -> {0,100};
preop_prec('+') -> {600,700};
preop_prec('-') -> {600,700};
preop_prec('bnot') -> {600,700};
preop_prec('not') -> {600,700};
preop_prec('#') -> {700,800}.

func_prec() -> {800,700}.

max_prec() -> 1000.

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
%%     $Id: refac_parse.erl,v 1.1 2008/05/20 14:49:41 go30 Exp $
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

parse(Tokens) ->
    case catch yeccpars1(Tokens, false, 0, [], []) of
	error ->
	    Errorline =
		if Tokens == [] -> 0; true -> element(2, hd(Tokens)) end,
	    {error,
	     {Errorline, ?MODULE, "syntax error at or after this line."}};
	Other ->
	    Other
    end.

parse_and_scan({Mod, Fun, Args}) ->
    case apply(Mod, Fun, Args) of
	{eof, _} ->
	    {ok, eof};
	{error, Descriptor, _} ->
	    {error, Descriptor};
	{ok, Tokens, _} ->
	    yeccpars1(Tokens, {Mod, Fun, Args}, 0, [], [])
    end.

format_error(Message) ->
    case io_lib:deep_char_list(Message) of
	true ->
	    Message;
	_ ->
	    io_lib:write(Message)
    end.

% To be used in grammar files to throw an error message to the parser toplevel.
% Doesn't have to be exported!
return_error(Line, Message) ->
    throw({error, {Line, ?MODULE, Message}}).


% Don't change yeccpars1/6 too much, it is called recursively by yeccpars2/8!
yeccpars1([Token | Tokens], Tokenizer, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens,
	      Tokenizer);
yeccpars1([], {M, F, A}, State, States, Vstack) ->
    case catch apply(M, F, A) of
        {eof, _Endline} ->
            yeccpars1([], false, State, States, Vstack);
        {error, Descriptor, _Endline} ->
            {error, Descriptor};
        {'EXIT', Reason} ->
            {error, {0, ?MODULE, Reason}};
        {ok, Tokens, _Endline} ->
	    case catch yeccpars1(Tokens, {M, F, A}, State, States, Vstack) of
		error ->
		    Errorline = element(2, hd(Tokens)),
		    {error, {Errorline, ?MODULE,
			     "syntax error at or after this line."}};
		Other ->
		    Other
	    end
    end;
yeccpars1([], false, State, States, Vstack) ->
    yeccpars2(State, '$end', States, Vstack, {'$end', 999999}, [], false).

% For internal use only.
yeccerror(Token) ->
    {error,
     {element(2, Token), ?MODULE,
      ["syntax error before: ", yecctoken2string(Token)]}}.

yecctoken2string({atom, _, A}) -> io_lib:write(A);
yecctoken2string({integer,_,N}) -> io_lib:write(N);
yecctoken2string({float,_,F}) -> io_lib:write(F);
yecctoken2string({char,_,C}) -> io_lib:write_char(C);
yecctoken2string({var,_,V}) -> io_lib:format('~s', [V]);
yecctoken2string({string,_,S}) -> io_lib:write_string(S);
yecctoken2string({reserved_symbol, _, A}) -> io_lib:format('~w', [A]);
yecctoken2string({_Cat, _, Val}) -> io_lib:format('~w', [Val]);

yecctoken2string({'dot', _}) -> io_lib:format('~w', ['.']);
yecctoken2string({'$end', _}) ->
    [];
yecctoken2string({Other, _}) when atom(Other) ->
    io_lib:format('~w', [Other]);
yecctoken2string(Other) ->
    io_lib:write(Other).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


yeccpars2(0, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 2, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 1, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(1, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 292, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(2, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 14, [2 | __Ss], [__T | __Stack]);
yeccpars2(2, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(3, 'dot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 289, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(4, '$end', _, __Stack, _, _, _) ->
 {ok, hd(__Stack)};
yeccpars2(4, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(5, 'dot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 288, [5 | __Ss], [__T | __Stack]);
yeccpars2(5, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(6, ';', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 282, [6 | __Ss], [__T | __Stack]);
yeccpars2(6, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1],
 yeccpars2(yeccgoto(function_clauses, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(7, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  build_function(__1),
 yeccpars2(yeccgoto(function, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(8, 'dot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 281, [8 | __Ss], [__T | __Stack]);
yeccpars2(8, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(9, ';', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 11, [9 | __Ss], [__T | __Stack]);
yeccpars2(9, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1],
 yeccpars2(yeccgoto(rule_clauses, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(10, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  build_rule(__1),
 yeccpars2(yeccgoto(rule, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(11, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 12, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(12, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 14, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(13, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1|__3],
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(rule_clauses, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(14, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 278, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(15, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  element(1,__1),
 yeccpars2(yeccgoto(clause_args, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(16, 'when', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [16 | __Ss], [__T | __Stack]);
yeccpars2(16, __Cat, __Ss,  __Stack, __T, __Ts, __Tzr) ->
 __Val =  [],
 yeccpars2(17, __Cat, [16 | __Ss], [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(17, ':-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 275, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(18, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(19, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 271, [19 | __Ss], [__T | __Stack]);
yeccpars2(19, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(20, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(21, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(prefix_op, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(22, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(prefix_op, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(23, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 268, [23 | __Ss], [__T | __Stack]);
yeccpars2(23, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(24, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, '>>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 245, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(25, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 235, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(26, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(atomic, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(27, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(expr_max, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(28, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(29, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(expr_max, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(30, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(prefix_op, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(31, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [31 | __Ss], [__T | __Stack]);
yeccpars2(31, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [31 | __Ss], [__T | __Stack]);
yeccpars2(31, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [31 | __Ss], [__T | __Stack]);
yeccpars2(31, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [31 | __Ss], [__T | __Stack]);
yeccpars2(31, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [31 | __Ss], [__T | __Stack]);
yeccpars2(31, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [31 | __Ss], [__T | __Stack]);
yeccpars2(31, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [31 | __Ss], [__T | __Stack]);
yeccpars2(31, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [31 | __Ss], [__T | __Stack]);
yeccpars2(31, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [31 | __Ss], [__T | __Stack]);
yeccpars2(31, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [31 | __Ss], [__T | __Stack]);
yeccpars2(31, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [31 | __Ss], [__T | __Stack]);
yeccpars2(31, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [31 | __Ss], [__T | __Stack]);
yeccpars2(31, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [31 | __Ss], [__T | __Stack]);
yeccpars2(31, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [31 | __Ss], [__T | __Stack]);
yeccpars2(31, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [31 | __Ss], [__T | __Stack]);
yeccpars2(31, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [31 | __Ss], [__T | __Stack]);
yeccpars2(31, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [31 | __Ss], [__T | __Stack]);
yeccpars2(31, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [31 | __Ss], [__T | __Stack]);
yeccpars2(31, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [31 | __Ss], [__T | __Stack]);
yeccpars2(31, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [31 | __Ss], [__T | __Stack]);
yeccpars2(31, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [31 | __Ss], [__T | __Stack]);
yeccpars2(31, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [31 | __Ss], [__T | __Stack]);
yeccpars2(31, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [31 | __Ss], [__T | __Stack]);
yeccpars2(31, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [31 | __Ss], [__T | __Stack]);
yeccpars2(31, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(32, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(expr_max, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(33, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(34, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(atomic, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(35, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 226, [35 | __Ss], [__T | __Stack]);
yeccpars2(35, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1],
 yeccpars2(yeccgoto(exprs, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(36, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(expr, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(37, '!', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 222, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 223, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(expr_100, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(38, 'orelse', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 220, [38 | __Ss], [__T | __Stack]);
yeccpars2(38, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(expr_150, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(39, 'andalso', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 218, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(expr_160, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(40, '=/=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 210, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, '=:=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 211, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 214, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, '>=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 215, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 209, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, '=<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 212, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, '/=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 208, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, '==', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 213, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(expr_200, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(41, '--', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 197, [41 | __Ss], [__T | __Stack]);
yeccpars2(41, '++', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 195, [41 | __Ss], [__T | __Stack]);
yeccpars2(41, 'xor', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 205, [41 | __Ss], [__T | __Stack]);
yeccpars2(41, 'or', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 204, [41 | __Ss], [__T | __Stack]);
yeccpars2(41, 'bsr', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 201, [41 | __Ss], [__T | __Stack]);
yeccpars2(41, 'bsl', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 200, [41 | __Ss], [__T | __Stack]);
yeccpars2(41, 'bxor', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 202, [41 | __Ss], [__T | __Stack]);
yeccpars2(41, 'bor', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 199, [41 | __Ss], [__T | __Stack]);
yeccpars2(41, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 196, [41 | __Ss], [__T | __Stack]);
yeccpars2(41, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 194, [41 | __Ss], [__T | __Stack]);
yeccpars2(41, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(expr_300, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(42, 'and', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 188, [42 | __Ss], [__T | __Stack]);
yeccpars2(42, 'band', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 189, [42 | __Ss], [__T | __Stack]);
yeccpars2(42, 'rem', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 192, [42 | __Ss], [__T | __Stack]);
yeccpars2(42, 'div', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 190, [42 | __Ss], [__T | __Stack]);
yeccpars2(42, '*', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 186, [42 | __Ss], [__T | __Stack]);
yeccpars2(42, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 187, [42 | __Ss], [__T | __Stack]);
yeccpars2(42, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(expr_400, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(43, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(expr_500, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(44, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(expr_600, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(45, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 14, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(expr_700, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(46, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 181, [46 | __Ss], [__T | __Stack]);
yeccpars2(46, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 182, [46 | __Ss], [__T | __Stack]);
yeccpars2(46, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(expr_800, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(47, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 163, [47 | __Ss], [__T | __Stack]);
yeccpars2(47, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(expr_900, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(48, ';', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 161, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1],
 yeccpars2(yeccgoto(guard, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(49, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(atomic, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(50, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 14, [50 | __Ss], [__T | __Stack]);
yeccpars2(50, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 147, [50 | __Ss], [__T | __Stack]);
yeccpars2(50, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(51, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(expr_max, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(52, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(expr_700, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(53, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __2,
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(clause_guard, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(54, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(55, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(expr_max, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(56, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(atomic, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(57, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(expr_max, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(58, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(expr_max, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(59, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(prefix_op, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(60, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(61, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 125, [61 | __Ss], [__T | __Stack]);
yeccpars2(61, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(62, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(expr_max, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(63, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [63 | __Ss], [__T | __Stack]);
yeccpars2(63, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [63 | __Ss], [__T | __Stack]);
yeccpars2(63, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [63 | __Ss], [__T | __Stack]);
yeccpars2(63, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [63 | __Ss], [__T | __Stack]);
yeccpars2(63, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [63 | __Ss], [__T | __Stack]);
yeccpars2(63, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [63 | __Ss], [__T | __Stack]);
yeccpars2(63, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [63 | __Ss], [__T | __Stack]);
yeccpars2(63, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [63 | __Ss], [__T | __Stack]);
yeccpars2(63, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [63 | __Ss], [__T | __Stack]);
yeccpars2(63, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [63 | __Ss], [__T | __Stack]);
yeccpars2(63, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [63 | __Ss], [__T | __Stack]);
yeccpars2(63, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [63 | __Ss], [__T | __Stack]);
yeccpars2(63, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [63 | __Ss], [__T | __Stack]);
yeccpars2(63, 'after', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 115, [63 | __Ss], [__T | __Stack]);
yeccpars2(63, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [63 | __Ss], [__T | __Stack]);
yeccpars2(63, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [63 | __Ss], [__T | __Stack]);
yeccpars2(63, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [63 | __Ss], [__T | __Stack]);
yeccpars2(63, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [63 | __Ss], [__T | __Stack]);
yeccpars2(63, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [63 | __Ss], [__T | __Stack]);
yeccpars2(63, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [63 | __Ss], [__T | __Stack]);
yeccpars2(63, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [63 | __Ss], [__T | __Stack]);
yeccpars2(63, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [63 | __Ss], [__T | __Stack]);
yeccpars2(63, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [63 | __Ss], [__T | __Stack]);
yeccpars2(63, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [63 | __Ss], [__T | __Stack]);
yeccpars2(63, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [63 | __Ss], [__T | __Stack]);
yeccpars2(63, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(64, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(expr_max, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(65, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(expr_700, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(66, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [66 | __Ss], [__T | __Stack]);
yeccpars2(66, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(strings, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(67, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(atomic, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(68, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(69, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(expr_max, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(70, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(expr_max, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(71, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(expr_max, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(72, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(73, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 75, [73 | __Ss], [__T | __Stack]);
yeccpars2(73, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(74, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {tuple,line(__1),[]},
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(tuple, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(75, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {tuple,line(__1),__2},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(tuple, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(76, 'after', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 77, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 78, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, 'of', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 79, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(77, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(78, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 91, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 95, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(79, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(80, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  build_try(line(__1),__2,[],__3),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(try_expr, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(81, ';', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 89, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1],
 yeccpars2(yeccgoto(cr_clauses, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(82, 'after', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 77, [82 | __Ss], [__T | __Stack]);
yeccpars2(82, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 78, [82 | __Ss], [__T | __Stack]);
yeccpars2(82, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(83, 'when', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [83 | __Ss], [__T | __Stack]);
yeccpars2(83, __Cat, __Ss,  __Stack, __T, __Ts, __Tzr) ->
 __Val =  [],
 yeccpars2(84, __Cat, [83 | __Ss], [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(84, '->', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 85, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(85, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(86, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {clause,line(__1),[__1],__2,__3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(cr_clause, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(87, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __2,
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(clause_body, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(88, __Cat, __Ss,  [__5,__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  build_try(line(__1),__2,__4,__5),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(try_expr, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(89, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(90, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1|__3],
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(cr_clauses, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(91, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 108, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(atomic, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(92, 'when', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [92 | __Ss], [__T | __Stack]);
yeccpars2(92, __Cat, __Ss,  __Stack, __T, __Ts, __Tzr) ->
 __Val =  [],
 yeccpars2(106, __Cat, [92 | __Ss], [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(93, ';', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 104, [93 | __Ss], [__T | __Stack]);
yeccpars2(93, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1],
 yeccpars2(yeccgoto(try_clauses, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(94, 'after', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, 'end', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 101, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(95, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 96, [95 | __Ss], [__T | __Stack]);
yeccpars2(95, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(expr_max, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(96, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(97, 'when', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [97 | __Ss], [__T | __Stack]);
yeccpars2(97, __Cat, __Ss,  __Stack, __T, __Ts, __Tzr) ->
 __Val =  [],
 yeccpars2(98, __Cat, [97 | __Ss], [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(98, '->', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 85, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(99, __Cat, __Ss,  [__5,__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val = begin
   L = line(__1), {clause,L,[{tuple,L,[__1,__3,{var,L,'_'}]}],__4,__5}
  end,
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(try_clause, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(100, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [100 | __Ss], [__T | __Stack]);
yeccpars2(100, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [100 | __Ss], [__T | __Stack]);
yeccpars2(100, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [100 | __Ss], [__T | __Stack]);
yeccpars2(100, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [100 | __Ss], [__T | __Stack]);
yeccpars2(100, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [100 | __Ss], [__T | __Stack]);
yeccpars2(100, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [100 | __Ss], [__T | __Stack]);
yeccpars2(100, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [100 | __Ss], [__T | __Stack]);
yeccpars2(100, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [100 | __Ss], [__T | __Stack]);
yeccpars2(100, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [100 | __Ss], [__T | __Stack]);
yeccpars2(100, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [100 | __Ss], [__T | __Stack]);
yeccpars2(100, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [100 | __Ss], [__T | __Stack]);
yeccpars2(100, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [100 | __Ss], [__T | __Stack]);
yeccpars2(100, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [100 | __Ss], [__T | __Stack]);
yeccpars2(100, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [100 | __Ss], [__T | __Stack]);
yeccpars2(100, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [100 | __Ss], [__T | __Stack]);
yeccpars2(100, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [100 | __Ss], [__T | __Stack]);
yeccpars2(100, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [100 | __Ss], [__T | __Stack]);
yeccpars2(100, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [100 | __Ss], [__T | __Stack]);
yeccpars2(100, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [100 | __Ss], [__T | __Stack]);
yeccpars2(100, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [100 | __Ss], [__T | __Stack]);
yeccpars2(100, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [100 | __Ss], [__T | __Stack]);
yeccpars2(100, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [100 | __Ss], [__T | __Stack]);
yeccpars2(100, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [100 | __Ss], [__T | __Stack]);
yeccpars2(100, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [100 | __Ss], [__T | __Stack]);
yeccpars2(100, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(101, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {__2,[]},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(try_catch, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(102, 'end', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 103, [102 | __Ss], [__T | __Stack]);
yeccpars2(102, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(103, __Cat, __Ss,  [__5,__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {__2,__4},
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(try_catch, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(104, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [104 | __Ss], [__T | __Stack]);
yeccpars2(104, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [104 | __Ss], [__T | __Stack]);
yeccpars2(104, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [104 | __Ss], [__T | __Stack]);
yeccpars2(104, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [104 | __Ss], [__T | __Stack]);
yeccpars2(104, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [104 | __Ss], [__T | __Stack]);
yeccpars2(104, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 91, [104 | __Ss], [__T | __Stack]);
yeccpars2(104, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [104 | __Ss], [__T | __Stack]);
yeccpars2(104, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [104 | __Ss], [__T | __Stack]);
yeccpars2(104, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [104 | __Ss], [__T | __Stack]);
yeccpars2(104, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [104 | __Ss], [__T | __Stack]);
yeccpars2(104, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 95, [104 | __Ss], [__T | __Stack]);
yeccpars2(104, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [104 | __Ss], [__T | __Stack]);
yeccpars2(104, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [104 | __Ss], [__T | __Stack]);
yeccpars2(104, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [104 | __Ss], [__T | __Stack]);
yeccpars2(104, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [104 | __Ss], [__T | __Stack]);
yeccpars2(104, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [104 | __Ss], [__T | __Stack]);
yeccpars2(104, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [104 | __Ss], [__T | __Stack]);
yeccpars2(104, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [104 | __Ss], [__T | __Stack]);
yeccpars2(104, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [104 | __Ss], [__T | __Stack]);
yeccpars2(104, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [104 | __Ss], [__T | __Stack]);
yeccpars2(104, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [104 | __Ss], [__T | __Stack]);
yeccpars2(104, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [104 | __Ss], [__T | __Stack]);
yeccpars2(104, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [104 | __Ss], [__T | __Stack]);
yeccpars2(104, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [104 | __Ss], [__T | __Stack]);
yeccpars2(104, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(105, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1|__3],
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(try_clauses, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(106, '->', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 85, [106 | __Ss], [__T | __Stack]);
yeccpars2(106, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(107, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val = begin
   L = line(__1), {clause,L,[{tuple,L,[{atom,L,throw},__1,{var,L,'_'}]}],__2,__3}
  end,
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(try_clause, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(108, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(109, 'when', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [109 | __Ss], [__T | __Stack]);
yeccpars2(109, __Cat, __Ss,  __Stack, __T, __Ts, __Tzr) ->
 __Val =  [],
 yeccpars2(110, __Cat, [109 | __Ss], [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(110, '->', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 85, [110 | __Ss], [__T | __Stack]);
yeccpars2(110, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(111, __Cat, __Ss,  [__5,__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val = begin
   L = line(__1), {clause,L,[{tuple,L,[__1,__3,{var,L,'_'}]}],__4,__5}
  end,
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(try_clause, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(112, 'end', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 113, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(113, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {[],__2},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(try_catch, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(114, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {string,line(__1),element(3,__1) ++ element(3,__2)},
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(strings, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(115, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [115 | __Ss], [__T | __Stack]);
yeccpars2(115, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [115 | __Ss], [__T | __Stack]);
yeccpars2(115, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [115 | __Ss], [__T | __Stack]);
yeccpars2(115, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [115 | __Ss], [__T | __Stack]);
yeccpars2(115, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [115 | __Ss], [__T | __Stack]);
yeccpars2(115, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [115 | __Ss], [__T | __Stack]);
yeccpars2(115, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [115 | __Ss], [__T | __Stack]);
yeccpars2(115, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [115 | __Ss], [__T | __Stack]);
yeccpars2(115, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [115 | __Ss], [__T | __Stack]);
yeccpars2(115, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [115 | __Ss], [__T | __Stack]);
yeccpars2(115, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [115 | __Ss], [__T | __Stack]);
yeccpars2(115, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [115 | __Ss], [__T | __Stack]);
yeccpars2(115, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [115 | __Ss], [__T | __Stack]);
yeccpars2(115, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [115 | __Ss], [__T | __Stack]);
yeccpars2(115, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [115 | __Ss], [__T | __Stack]);
yeccpars2(115, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [115 | __Ss], [__T | __Stack]);
yeccpars2(115, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [115 | __Ss], [__T | __Stack]);
yeccpars2(115, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [115 | __Ss], [__T | __Stack]);
yeccpars2(115, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [115 | __Ss], [__T | __Stack]);
yeccpars2(115, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [115 | __Ss], [__T | __Stack]);
yeccpars2(115, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [115 | __Ss], [__T | __Stack]);
yeccpars2(115, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [115 | __Ss], [__T | __Stack]);
yeccpars2(115, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [115 | __Ss], [__T | __Stack]);
yeccpars2(115, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [115 | __Ss], [__T | __Stack]);
yeccpars2(115, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(116, 'after', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 117, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, 'end', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 118, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(117, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(118, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {'receive',line(__1),__2},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(receive_expr, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(119, '->', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 85, [119 | __Ss], [__T | __Stack]);
yeccpars2(119, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(120, 'end', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 121, [120 | __Ss], [__T | __Stack]);
yeccpars2(120, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(121, __Cat, __Ss,  [__6,__5,__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {'receive',line(__1),__2,__4,__5},
 __Nss = lists:nthtail(5, __Ss),
 yeccpars2(yeccgoto(receive_expr, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(122, '->', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 85, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(123, 'end', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 124, [123 | __Ss], [__T | __Stack]);
yeccpars2(123, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(124, __Cat, __Ss,  [__5,__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {'receive',line(__1),[],__3,__4},
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(receive_expr, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(125, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(126, 'end', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 127, [126 | __Ss], [__T | __Stack]);
yeccpars2(126, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(127, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {'query',line(__1),__2},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(query_expr, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(128, '||', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [128 | __Ss], [__T | __Stack]);
yeccpars2(128, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(129, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(130, '<-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [130 | __Ss], [__T | __Stack]);
yeccpars2(130, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(lc_expr, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(131, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [131 | __Ss], [__T | __Stack]);
yeccpars2(131, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1],
 yeccpars2(yeccgoto(lc_exprs, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(132, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [132 | __Ss], [__T | __Stack]);
yeccpars2(132, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(133, __Cat, __Ss,  [__5,__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {lc,line(__1),__2,__4},
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(list_comprehension, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(134, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [134 | __Ss], [__T | __Stack]);
yeccpars2(134, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [134 | __Ss], [__T | __Stack]);
yeccpars2(134, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [134 | __Ss], [__T | __Stack]);
yeccpars2(134, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [134 | __Ss], [__T | __Stack]);
yeccpars2(134, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [134 | __Ss], [__T | __Stack]);
yeccpars2(134, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [134 | __Ss], [__T | __Stack]);
yeccpars2(134, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [134 | __Ss], [__T | __Stack]);
yeccpars2(134, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [134 | __Ss], [__T | __Stack]);
yeccpars2(134, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [134 | __Ss], [__T | __Stack]);
yeccpars2(134, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [134 | __Ss], [__T | __Stack]);
yeccpars2(134, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [134 | __Ss], [__T | __Stack]);
yeccpars2(134, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [134 | __Ss], [__T | __Stack]);
yeccpars2(134, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [134 | __Ss], [__T | __Stack]);
yeccpars2(134, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [134 | __Ss], [__T | __Stack]);
yeccpars2(134, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [134 | __Ss], [__T | __Stack]);
yeccpars2(134, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [134 | __Ss], [__T | __Stack]);
yeccpars2(134, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [134 | __Ss], [__T | __Stack]);
yeccpars2(134, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [134 | __Ss], [__T | __Stack]);
yeccpars2(134, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [134 | __Ss], [__T | __Stack]);
yeccpars2(134, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [134 | __Ss], [__T | __Stack]);
yeccpars2(134, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [134 | __Ss], [__T | __Stack]);
yeccpars2(134, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [134 | __Ss], [__T | __Stack]);
yeccpars2(134, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [134 | __Ss], [__T | __Stack]);
yeccpars2(134, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [134 | __Ss], [__T | __Stack]);
yeccpars2(134, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(135, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1|__3],
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(lc_exprs, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(136, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(137, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {generate,line(__2),__1,__3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(lc_expr, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(138, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  mkop(__1,__2),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(expr_600, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(139, '->', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 85, [139 | __Ss], [__T | __Stack]);
yeccpars2(139, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(140, ';', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 143, [140 | __Ss], [__T | __Stack]);
yeccpars2(140, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1],
 yeccpars2(yeccgoto(if_clauses, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(141, 'end', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 142, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(142, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {'if',line(__1),__2},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(if_expr, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(143, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(144, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1|__3],
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(if_clauses, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(145, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {clause,line(hd(hd(__1))),[],__1,__2},
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(if_clause, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(146, 'when', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, __Cat, __Ss,  __Stack, __T, __Ts, __Tzr) ->
 __Val =  [],
 yeccpars2(159, __Cat, [146 | __Ss], [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(147, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 154, [147 | __Ss], [__T | __Stack]);
yeccpars2(147, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 153, [147 | __Ss], [__T | __Stack]);
yeccpars2(147, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(148, ';', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 151, [148 | __Ss], [__T | __Stack]);
yeccpars2(148, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1],
 yeccpars2(yeccgoto(fun_clauses, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(149, 'end', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 150, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(150, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  build_fun(line(__1),__2),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(fun_expr, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(151, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 14, [151 | __Ss], [__T | __Stack]);
yeccpars2(151, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(152, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1|__3],
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(fun_clauses, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(153, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 158, [153 | __Ss], [__T | __Stack]);
yeccpars2(153, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(154, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 155, [154 | __Ss], [__T | __Stack]);
yeccpars2(154, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(155, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 156, [155 | __Ss], [__T | __Stack]);
yeccpars2(155, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(156, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 157, [156 | __Ss], [__T | __Stack]);
yeccpars2(156, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(157, __Cat, __Ss,  [__6,__5,__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {'fun',line(__1),{function,element(3,__2),element(3,__4),element(3,__6)}},
 __Nss = lists:nthtail(5, __Ss),
 yeccpars2(yeccgoto(fun_expr, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(158, __Cat, __Ss,  [__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {'fun',line(__1),{function,element(3,__2),element(3,__4)}},
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(fun_expr, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(159, '->', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 85, [159 | __Ss], [__T | __Stack]);
yeccpars2(159, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(160, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val = begin
   {Args,Pos} = __1, {clause,Pos,'fun',Args,__2,__3}
  end,
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(fun_clause, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(161, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [161 | __Ss], [__T | __Stack]);
yeccpars2(161, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [161 | __Ss], [__T | __Stack]);
yeccpars2(161, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [161 | __Ss], [__T | __Stack]);
yeccpars2(161, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [161 | __Ss], [__T | __Stack]);
yeccpars2(161, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [161 | __Ss], [__T | __Stack]);
yeccpars2(161, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [161 | __Ss], [__T | __Stack]);
yeccpars2(161, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [161 | __Ss], [__T | __Stack]);
yeccpars2(161, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [161 | __Ss], [__T | __Stack]);
yeccpars2(161, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [161 | __Ss], [__T | __Stack]);
yeccpars2(161, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [161 | __Ss], [__T | __Stack]);
yeccpars2(161, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [161 | __Ss], [__T | __Stack]);
yeccpars2(161, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [161 | __Ss], [__T | __Stack]);
yeccpars2(161, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [161 | __Ss], [__T | __Stack]);
yeccpars2(161, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [161 | __Ss], [__T | __Stack]);
yeccpars2(161, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [161 | __Ss], [__T | __Stack]);
yeccpars2(161, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [161 | __Ss], [__T | __Stack]);
yeccpars2(161, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [161 | __Ss], [__T | __Stack]);
yeccpars2(161, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [161 | __Ss], [__T | __Stack]);
yeccpars2(161, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [161 | __Ss], [__T | __Stack]);
yeccpars2(161, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [161 | __Ss], [__T | __Stack]);
yeccpars2(161, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [161 | __Ss], [__T | __Stack]);
yeccpars2(161, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [161 | __Ss], [__T | __Stack]);
yeccpars2(161, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [161 | __Ss], [__T | __Stack]);
yeccpars2(161, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [161 | __Ss], [__T | __Stack]);
yeccpars2(161, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(162, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1|__3],
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(guard, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(163, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 164, [163 | __Ss], [__T | __Stack]);
yeccpars2(163, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(164, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 167, [164 | __Ss], [__T | __Stack]);
yeccpars2(164, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 165, [164 | __Ss], [__T | __Stack]);
yeccpars2(164, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(165, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 180, [165 | __Ss], [__T | __Stack]);
yeccpars2(165, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(166, __Cat, __Ss,  [__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {record,line(__2),__1,element(3,__3),__4},
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(record_expr, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(167, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 168, [167 | __Ss], [__T | __Stack]);
yeccpars2(167, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 171, [167 | __Ss], [__T | __Stack]);
yeccpars2(167, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 172, [167 | __Ss], [__T | __Stack]);
yeccpars2(167, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(168, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 178, [168 | __Ss], [__T | __Stack]);
yeccpars2(168, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(169, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 176, [169 | __Ss], [__T | __Stack]);
yeccpars2(169, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1],
 yeccpars2(yeccgoto(record_fields, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(170, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 175, [170 | __Ss], [__T | __Stack]);
yeccpars2(170, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(171, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 173, [171 | __Ss], [__T | __Stack]);
yeccpars2(171, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(172, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [],
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(record_tuple, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(173, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [173 | __Ss], [__T | __Stack]);
yeccpars2(173, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [173 | __Ss], [__T | __Stack]);
yeccpars2(173, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [173 | __Ss], [__T | __Stack]);
yeccpars2(173, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [173 | __Ss], [__T | __Stack]);
yeccpars2(173, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [173 | __Ss], [__T | __Stack]);
yeccpars2(173, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [173 | __Ss], [__T | __Stack]);
yeccpars2(173, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [173 | __Ss], [__T | __Stack]);
yeccpars2(173, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [173 | __Ss], [__T | __Stack]);
yeccpars2(173, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [173 | __Ss], [__T | __Stack]);
yeccpars2(173, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [173 | __Ss], [__T | __Stack]);
yeccpars2(173, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [173 | __Ss], [__T | __Stack]);
yeccpars2(173, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [173 | __Ss], [__T | __Stack]);
yeccpars2(173, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [173 | __Ss], [__T | __Stack]);
yeccpars2(173, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [173 | __Ss], [__T | __Stack]);
yeccpars2(173, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [173 | __Ss], [__T | __Stack]);
yeccpars2(173, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [173 | __Ss], [__T | __Stack]);
yeccpars2(173, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [173 | __Ss], [__T | __Stack]);
yeccpars2(173, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [173 | __Ss], [__T | __Stack]);
yeccpars2(173, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [173 | __Ss], [__T | __Stack]);
yeccpars2(173, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [173 | __Ss], [__T | __Stack]);
yeccpars2(173, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [173 | __Ss], [__T | __Stack]);
yeccpars2(173, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [173 | __Ss], [__T | __Stack]);
yeccpars2(173, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [173 | __Ss], [__T | __Stack]);
yeccpars2(173, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [173 | __Ss], [__T | __Stack]);
yeccpars2(173, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(174, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {record_field,line(__1),__1,__3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(record_field, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(175, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __2,
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(record_tuple, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(176, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 168, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 171, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(177, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1|__3],
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(record_fields, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(178, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [178 | __Ss], [__T | __Stack]);
yeccpars2(178, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [178 | __Ss], [__T | __Stack]);
yeccpars2(178, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [178 | __Ss], [__T | __Stack]);
yeccpars2(178, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [178 | __Ss], [__T | __Stack]);
yeccpars2(178, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [178 | __Ss], [__T | __Stack]);
yeccpars2(178, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [178 | __Ss], [__T | __Stack]);
yeccpars2(178, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [178 | __Ss], [__T | __Stack]);
yeccpars2(178, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [178 | __Ss], [__T | __Stack]);
yeccpars2(178, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [178 | __Ss], [__T | __Stack]);
yeccpars2(178, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [178 | __Ss], [__T | __Stack]);
yeccpars2(178, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [178 | __Ss], [__T | __Stack]);
yeccpars2(178, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [178 | __Ss], [__T | __Stack]);
yeccpars2(178, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [178 | __Ss], [__T | __Stack]);
yeccpars2(178, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [178 | __Ss], [__T | __Stack]);
yeccpars2(178, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [178 | __Ss], [__T | __Stack]);
yeccpars2(178, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [178 | __Ss], [__T | __Stack]);
yeccpars2(178, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [178 | __Ss], [__T | __Stack]);
yeccpars2(178, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [178 | __Ss], [__T | __Stack]);
yeccpars2(178, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [178 | __Ss], [__T | __Stack]);
yeccpars2(178, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [178 | __Ss], [__T | __Stack]);
yeccpars2(178, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [178 | __Ss], [__T | __Stack]);
yeccpars2(178, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [178 | __Ss], [__T | __Stack]);
yeccpars2(178, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [178 | __Ss], [__T | __Stack]);
yeccpars2(178, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [178 | __Ss], [__T | __Stack]);
yeccpars2(178, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(179, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {record_field,line(__1),__1,__3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(record_field, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(180, __Cat, __Ss,  [__5,__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {record_field,line(__2),__1,element(3,__3),__5},
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(record_expr, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(181, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 184, [181 | __Ss], [__T | __Stack]);
yeccpars2(181, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(182, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [182 | __Ss], [__T | __Stack]);
yeccpars2(182, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [182 | __Ss], [__T | __Stack]);
yeccpars2(182, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [182 | __Ss], [__T | __Stack]);
yeccpars2(182, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [182 | __Ss], [__T | __Stack]);
yeccpars2(182, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [182 | __Ss], [__T | __Stack]);
yeccpars2(182, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [182 | __Ss], [__T | __Stack]);
yeccpars2(182, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [182 | __Ss], [__T | __Stack]);
yeccpars2(182, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [182 | __Ss], [__T | __Stack]);
yeccpars2(182, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [182 | __Ss], [__T | __Stack]);
yeccpars2(182, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [182 | __Ss], [__T | __Stack]);
yeccpars2(182, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [182 | __Ss], [__T | __Stack]);
yeccpars2(182, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [182 | __Ss], [__T | __Stack]);
yeccpars2(182, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [182 | __Ss], [__T | __Stack]);
yeccpars2(182, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [182 | __Ss], [__T | __Stack]);
yeccpars2(182, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [182 | __Ss], [__T | __Stack]);
yeccpars2(182, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [182 | __Ss], [__T | __Stack]);
yeccpars2(182, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [182 | __Ss], [__T | __Stack]);
yeccpars2(182, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(183, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {remote,line(__2),__1,__3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(expr_800, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(184, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {record_field,line(__2),__1,__3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(expr_900, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(185, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {call,line(__1),__1,element(1,__2)},
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(function_call, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(186, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(mult_op, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(187, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(mult_op, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(188, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(mult_op, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(189, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(mult_op, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(190, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(mult_op, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(191, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(192, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(mult_op, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(193, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  mkop(__1,__2,__3),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(expr_500, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(194, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(add_op, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(195, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(list_op, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(196, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(add_op, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(197, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(list_op, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(198, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [198 | __Ss], [__T | __Stack]);
yeccpars2(198, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [198 | __Ss], [__T | __Stack]);
yeccpars2(198, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [198 | __Ss], [__T | __Stack]);
yeccpars2(198, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [198 | __Ss], [__T | __Stack]);
yeccpars2(198, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [198 | __Ss], [__T | __Stack]);
yeccpars2(198, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [198 | __Ss], [__T | __Stack]);
yeccpars2(198, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [198 | __Ss], [__T | __Stack]);
yeccpars2(198, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [198 | __Ss], [__T | __Stack]);
yeccpars2(198, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [198 | __Ss], [__T | __Stack]);
yeccpars2(198, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [198 | __Ss], [__T | __Stack]);
yeccpars2(198, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [198 | __Ss], [__T | __Stack]);
yeccpars2(198, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [198 | __Ss], [__T | __Stack]);
yeccpars2(198, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [198 | __Ss], [__T | __Stack]);
yeccpars2(198, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [198 | __Ss], [__T | __Stack]);
yeccpars2(198, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [198 | __Ss], [__T | __Stack]);
yeccpars2(198, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [198 | __Ss], [__T | __Stack]);
yeccpars2(198, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [198 | __Ss], [__T | __Stack]);
yeccpars2(198, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [198 | __Ss], [__T | __Stack]);
yeccpars2(198, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [198 | __Ss], [__T | __Stack]);
yeccpars2(198, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [198 | __Ss], [__T | __Stack]);
yeccpars2(198, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [198 | __Ss], [__T | __Stack]);
yeccpars2(198, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [198 | __Ss], [__T | __Stack]);
yeccpars2(198, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [198 | __Ss], [__T | __Stack]);
yeccpars2(198, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(199, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(add_op, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(200, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(add_op, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(201, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(add_op, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(202, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(add_op, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(203, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(204, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(add_op, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(205, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(add_op, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(206, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  mkop(__1,__2,__3),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(expr_300, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(207, 'and', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 188, [207 | __Ss], [__T | __Stack]);
yeccpars2(207, 'band', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 189, [207 | __Ss], [__T | __Stack]);
yeccpars2(207, 'rem', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 192, [207 | __Ss], [__T | __Stack]);
yeccpars2(207, 'div', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 190, [207 | __Ss], [__T | __Stack]);
yeccpars2(207, '*', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 186, [207 | __Ss], [__T | __Stack]);
yeccpars2(207, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 187, [207 | __Ss], [__T | __Stack]);
yeccpars2(207, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  mkop(__1,__2,__3),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(expr_400, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(208, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(comp_op, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(209, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(comp_op, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(210, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(comp_op, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(211, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(comp_op, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(212, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(comp_op, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(213, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(comp_op, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(214, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(comp_op, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(215, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(comp_op, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(216, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(217, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  mkop(__1,__2,__3),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(expr_200, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(218, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(219, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  mkop(__1,__2,__3),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(expr_160, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(220, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(221, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  mkop(__1,__2,__3),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(expr_150, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(222, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(223, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [223 | __Ss], [__T | __Stack]);
yeccpars2(223, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [223 | __Ss], [__T | __Stack]);
yeccpars2(223, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [223 | __Ss], [__T | __Stack]);
yeccpars2(223, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [223 | __Ss], [__T | __Stack]);
yeccpars2(223, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [223 | __Ss], [__T | __Stack]);
yeccpars2(223, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [223 | __Ss], [__T | __Stack]);
yeccpars2(223, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [223 | __Ss], [__T | __Stack]);
yeccpars2(223, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [223 | __Ss], [__T | __Stack]);
yeccpars2(223, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [223 | __Ss], [__T | __Stack]);
yeccpars2(223, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [223 | __Ss], [__T | __Stack]);
yeccpars2(223, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [223 | __Ss], [__T | __Stack]);
yeccpars2(223, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [223 | __Ss], [__T | __Stack]);
yeccpars2(223, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [223 | __Ss], [__T | __Stack]);
yeccpars2(223, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [223 | __Ss], [__T | __Stack]);
yeccpars2(223, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [223 | __Ss], [__T | __Stack]);
yeccpars2(223, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [223 | __Ss], [__T | __Stack]);
yeccpars2(223, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [223 | __Ss], [__T | __Stack]);
yeccpars2(223, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [223 | __Ss], [__T | __Stack]);
yeccpars2(223, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [223 | __Ss], [__T | __Stack]);
yeccpars2(223, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [223 | __Ss], [__T | __Stack]);
yeccpars2(223, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [223 | __Ss], [__T | __Stack]);
yeccpars2(223, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [223 | __Ss], [__T | __Stack]);
yeccpars2(223, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [223 | __Ss], [__T | __Stack]);
yeccpars2(223, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(224, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {match,line(__2),__1,__3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(expr_100, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(225, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  mkop(__1,__2,__3),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(expr_100, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(226, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(227, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1|__3],
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(exprs, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(228, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {'catch',line(__1),__2},
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(expr, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(229, 'of', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 230, [229 | __Ss], [__T | __Stack]);
yeccpars2(229, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(230, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(231, 'end', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 232, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(232, __Cat, __Ss,  [__5,__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {'case',line(__1),__2,__4},
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(case_expr, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(233, 'end', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 234, [233 | __Ss], [__T | __Stack]);
yeccpars2(233, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(234, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {block,line(__1),__2},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(expr_max, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(235, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {nil,line(__1)},
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(list, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(236, '||', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [236 | __Ss], [__T | __Stack]);
yeccpars2(236, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 237, [236 | __Ss], [__T | __Stack]);
yeccpars2(236, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 240, [236 | __Ss], [__T | __Stack]);
yeccpars2(236, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 238, [236 | __Ss], [__T | __Stack]);
yeccpars2(236, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(237, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [237 | __Ss], [__T | __Stack]);
yeccpars2(237, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [237 | __Ss], [__T | __Stack]);
yeccpars2(237, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [237 | __Ss], [__T | __Stack]);
yeccpars2(237, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [237 | __Ss], [__T | __Stack]);
yeccpars2(237, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [237 | __Ss], [__T | __Stack]);
yeccpars2(237, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [237 | __Ss], [__T | __Stack]);
yeccpars2(237, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [237 | __Ss], [__T | __Stack]);
yeccpars2(237, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [237 | __Ss], [__T | __Stack]);
yeccpars2(237, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [237 | __Ss], [__T | __Stack]);
yeccpars2(237, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [237 | __Ss], [__T | __Stack]);
yeccpars2(237, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [237 | __Ss], [__T | __Stack]);
yeccpars2(237, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [237 | __Ss], [__T | __Stack]);
yeccpars2(237, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [237 | __Ss], [__T | __Stack]);
yeccpars2(237, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [237 | __Ss], [__T | __Stack]);
yeccpars2(237, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [237 | __Ss], [__T | __Stack]);
yeccpars2(237, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [237 | __Ss], [__T | __Stack]);
yeccpars2(237, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [237 | __Ss], [__T | __Stack]);
yeccpars2(237, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [237 | __Ss], [__T | __Stack]);
yeccpars2(237, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [237 | __Ss], [__T | __Stack]);
yeccpars2(237, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [237 | __Ss], [__T | __Stack]);
yeccpars2(237, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [237 | __Ss], [__T | __Stack]);
yeccpars2(237, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [237 | __Ss], [__T | __Stack]);
yeccpars2(237, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [237 | __Ss], [__T | __Stack]);
yeccpars2(237, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [237 | __Ss], [__T | __Stack]);
yeccpars2(237, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(238, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {nil,line(__1)},
 yeccpars2(yeccgoto(tail, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(239, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {cons,line(__1),__2,__3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(list, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(240, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(241, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 242, [241 | __Ss], [__T | __Stack]);
yeccpars2(241, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(242, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __2,
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(tail, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(243, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 237, [243 | __Ss], [__T | __Stack]);
yeccpars2(243, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 240, [243 | __Ss], [__T | __Stack]);
yeccpars2(243, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 238, [243 | __Ss], [__T | __Stack]);
yeccpars2(243, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(244, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {cons,line(__2),__2,__3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(tail, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(245, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {bin,line(__1),[]},
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(binary, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(246, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 266, [246 | __Ss], [__T | __Stack]);
yeccpars2(246, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1],
 yeccpars2(yeccgoto(bin_elements, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(247, '>>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 265, [247 | __Ss], [__T | __Stack]);
yeccpars2(247, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(248, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 252, [248 | __Ss], [__T | __Stack]);
yeccpars2(248, __Cat, __Ss,  __Stack, __T, __Ts, __Tzr) ->
 __Val =  default,
 yeccpars2(253, __Cat, [248 | __Ss], [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(249, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(bit_expr, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(250, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [250 | __Ss], [__T | __Stack]);
yeccpars2(250, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [250 | __Ss], [__T | __Stack]);
yeccpars2(250, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [250 | __Ss], [__T | __Stack]);
yeccpars2(250, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [250 | __Ss], [__T | __Stack]);
yeccpars2(250, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [250 | __Ss], [__T | __Stack]);
yeccpars2(250, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [250 | __Ss], [__T | __Stack]);
yeccpars2(250, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [250 | __Ss], [__T | __Stack]);
yeccpars2(250, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [250 | __Ss], [__T | __Stack]);
yeccpars2(250, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [250 | __Ss], [__T | __Stack]);
yeccpars2(250, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [250 | __Ss], [__T | __Stack]);
yeccpars2(250, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [250 | __Ss], [__T | __Stack]);
yeccpars2(250, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [250 | __Ss], [__T | __Stack]);
yeccpars2(250, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [250 | __Ss], [__T | __Stack]);
yeccpars2(250, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [250 | __Ss], [__T | __Stack]);
yeccpars2(250, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [250 | __Ss], [__T | __Stack]);
yeccpars2(250, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [250 | __Ss], [__T | __Stack]);
yeccpars2(250, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [250 | __Ss], [__T | __Stack]);
yeccpars2(250, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(251, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  mkop(__1,__2),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(bit_expr, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(252, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [252 | __Ss], [__T | __Stack]);
yeccpars2(252, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [252 | __Ss], [__T | __Stack]);
yeccpars2(252, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [252 | __Ss], [__T | __Stack]);
yeccpars2(252, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [252 | __Ss], [__T | __Stack]);
yeccpars2(252, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [252 | __Ss], [__T | __Stack]);
yeccpars2(252, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [252 | __Ss], [__T | __Stack]);
yeccpars2(252, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [252 | __Ss], [__T | __Stack]);
yeccpars2(252, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [252 | __Ss], [__T | __Stack]);
yeccpars2(252, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [252 | __Ss], [__T | __Stack]);
yeccpars2(252, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [252 | __Ss], [__T | __Stack]);
yeccpars2(252, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [252 | __Ss], [__T | __Stack]);
yeccpars2(252, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [252 | __Ss], [__T | __Stack]);
yeccpars2(252, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [252 | __Ss], [__T | __Stack]);
yeccpars2(252, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [252 | __Ss], [__T | __Stack]);
yeccpars2(252, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [252 | __Ss], [__T | __Stack]);
yeccpars2(252, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [252 | __Ss], [__T | __Stack]);
yeccpars2(252, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [252 | __Ss], [__T | __Stack]);
yeccpars2(252, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(253, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 254, [253 | __Ss], [__T | __Stack]);
yeccpars2(253, __Cat, __Ss,  __Stack, __T, __Ts, __Tzr) ->
 __Val =  default,
 yeccpars2(255, __Cat, [253 | __Ss], [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(254, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 256, [254 | __Ss], [__T | __Stack]);
yeccpars2(254, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(255, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {bin_element,line(__1),__1,__2,__3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(bin_element, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(256, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 261, [256 | __Ss], [__T | __Stack]);
yeccpars2(256, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  element(3,__1),
 yeccpars2(yeccgoto(bit_type, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(257, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 259, [257 | __Ss], [__T | __Stack]);
yeccpars2(257, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1],
 yeccpars2(yeccgoto(bit_type_list, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(258, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __2,
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(opt_bit_type_list, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(259, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 256, [259 | __Ss], [__T | __Stack]);
yeccpars2(259, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(260, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1|__3],
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(bit_type_list, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(261, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 262, [261 | __Ss], [__T | __Stack]);
yeccpars2(261, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(262, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {element(3,__1),element(3,__3)},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(bit_type, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(263, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __2,
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(opt_bit_size_expr, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(264, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(bit_size_expr, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(265, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {bin,line(__1),__2},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(binary, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(266, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [266 | __Ss], [__T | __Stack]);
yeccpars2(266, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [266 | __Ss], [__T | __Stack]);
yeccpars2(266, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [266 | __Ss], [__T | __Stack]);
yeccpars2(266, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [266 | __Ss], [__T | __Stack]);
yeccpars2(266, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [266 | __Ss], [__T | __Stack]);
yeccpars2(266, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [266 | __Ss], [__T | __Stack]);
yeccpars2(266, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [266 | __Ss], [__T | __Stack]);
yeccpars2(266, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [266 | __Ss], [__T | __Stack]);
yeccpars2(266, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [266 | __Ss], [__T | __Stack]);
yeccpars2(266, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [266 | __Ss], [__T | __Stack]);
yeccpars2(266, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [266 | __Ss], [__T | __Stack]);
yeccpars2(266, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [266 | __Ss], [__T | __Stack]);
yeccpars2(266, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [266 | __Ss], [__T | __Stack]);
yeccpars2(266, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [266 | __Ss], [__T | __Stack]);
yeccpars2(266, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [266 | __Ss], [__T | __Stack]);
yeccpars2(266, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [266 | __Ss], [__T | __Stack]);
yeccpars2(266, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [266 | __Ss], [__T | __Stack]);
yeccpars2(266, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [266 | __Ss], [__T | __Stack]);
yeccpars2(266, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [266 | __Ss], [__T | __Stack]);
yeccpars2(266, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [266 | __Ss], [__T | __Stack]);
yeccpars2(266, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [266 | __Ss], [__T | __Stack]);
yeccpars2(266, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(267, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1|__3],
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(bin_elements, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(268, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {record_field,line(__1),{atom,line(__1),''},__2},
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(expr_900, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(269, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 270, [269 | __Ss], [__T | __Stack]);
yeccpars2(269, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(270, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __2,
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(expr_max, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(271, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 167, [271 | __Ss], [__T | __Stack]);
yeccpars2(271, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 272, [271 | __Ss], [__T | __Stack]);
yeccpars2(271, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(272, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 274, [272 | __Ss], [__T | __Stack]);
yeccpars2(272, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(273, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {record,line(__1),element(3,__2),__3},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(record_expr, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(274, __Cat, __Ss,  [__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {record_index,line(__1),element(3,__2),__4},
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(record_expr, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(275, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(276, __Cat, __Ss,  [__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {clause,line(__1),element(3,__1),__2,__3,__4},
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(rule_clause, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(277, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __2,
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(rule_body, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(278, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {[],line(__1)},
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(argument_list, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(279, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 280, [279 | __Ss], [__T | __Stack]);
yeccpars2(279, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(280, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {__2,line(__1)},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(argument_list, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(281, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(form, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(282, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 283, [282 | __Ss], [__T | __Stack]);
yeccpars2(282, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(283, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 14, [283 | __Ss], [__T | __Stack]);
yeccpars2(283, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(284, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1|__3],
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(function_clauses, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(285, 'when', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [285 | __Ss], [__T | __Stack]);
yeccpars2(285, __Cat, __Ss,  __Stack, __T, __Ts, __Tzr) ->
 __Val =  [],
 yeccpars2(286, __Cat, [285 | __Ss], [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(286, '->', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 85, [286 | __Ss], [__T | __Stack]);
yeccpars2(286, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(287, __Cat, __Ss,  [__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {clause,line(__1),element(3,__1),__2,__3,__4},
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(function_clause, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(288, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(form, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(289, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(form, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(290, 'when', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [290 | __Ss], [__T | __Stack]);
yeccpars2(290, __Cat, __Ss,  __Stack, __T, __Ts, __Tzr) ->
 __Val =  [],
 yeccpars2(291, __Cat, [290 | __Ss], [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(291, ':-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 275, [291 | __Ss], [__T | __Stack]);
yeccpars2(291, '->', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 85, [291 | __Ss], [__T | __Stack]);
yeccpars2(291, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(292, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 293, [292 | __Ss], [__T | __Stack]);
yeccpars2(292, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(293, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, 'string', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, 'atom', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, 'float', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, 'integer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, 'char', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, 'var', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(294, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 296, [294 | __Ss], [__T | __Stack]);
yeccpars2(294, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(295, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(attr_val, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(296, __Cat, __Ss,  [__5,__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  build_attribute(__2,__4),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(attribute, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(__Other, _, _, _, _, _, _) ->
 exit({parser, __Other, missing_state_in_action_table}).

yeccgoto(add_op, 41) ->
 198;
yeccgoto(argument_list, 2) ->
 15;
yeccgoto(argument_list, 12) ->
 15;
yeccgoto(argument_list, 45) ->
 185;
yeccgoto(argument_list, 50) ->
 146;
yeccgoto(argument_list, 151) ->
 146;
yeccgoto(argument_list, 283) ->
 15;
yeccgoto(atomic, 14) ->
 27;
yeccgoto(atomic, 18) ->
 27;
yeccgoto(atomic, 20) ->
 27;
yeccgoto(atomic, 24) ->
 27;
yeccgoto(atomic, 25) ->
 27;
yeccgoto(atomic, 28) ->
 27;
yeccgoto(atomic, 31) ->
 27;
yeccgoto(atomic, 33) ->
 27;
yeccgoto(atomic, 54) ->
 27;
yeccgoto(atomic, 60) ->
 27;
yeccgoto(atomic, 63) ->
 27;
yeccgoto(atomic, 68) ->
 27;
yeccgoto(atomic, 72) ->
 27;
yeccgoto(atomic, 77) ->
 27;
yeccgoto(atomic, 78) ->
 27;
yeccgoto(atomic, 79) ->
 27;
yeccgoto(atomic, 85) ->
 27;
yeccgoto(atomic, 89) ->
 27;
yeccgoto(atomic, 96) ->
 27;
yeccgoto(atomic, 100) ->
 27;
yeccgoto(atomic, 104) ->
 27;
yeccgoto(atomic, 108) ->
 27;
yeccgoto(atomic, 115) ->
 27;
yeccgoto(atomic, 117) ->
 27;
yeccgoto(atomic, 125) ->
 27;
yeccgoto(atomic, 129) ->
 27;
yeccgoto(atomic, 134) ->
 27;
yeccgoto(atomic, 136) ->
 27;
yeccgoto(atomic, 143) ->
 27;
yeccgoto(atomic, 161) ->
 27;
yeccgoto(atomic, 173) ->
 27;
yeccgoto(atomic, 178) ->
 27;
yeccgoto(atomic, 182) ->
 27;
yeccgoto(atomic, 191) ->
 27;
yeccgoto(atomic, 198) ->
 27;
yeccgoto(atomic, 203) ->
 27;
yeccgoto(atomic, 216) ->
 27;
yeccgoto(atomic, 218) ->
 27;
yeccgoto(atomic, 220) ->
 27;
yeccgoto(atomic, 222) ->
 27;
yeccgoto(atomic, 223) ->
 27;
yeccgoto(atomic, 226) ->
 27;
yeccgoto(atomic, 230) ->
 27;
yeccgoto(atomic, 237) ->
 27;
yeccgoto(atomic, 240) ->
 27;
yeccgoto(atomic, 250) ->
 27;
yeccgoto(atomic, 252) ->
 27;
yeccgoto(atomic, 266) ->
 27;
yeccgoto(atomic, 275) ->
 27;
yeccgoto(atomic, 293) ->
 27;
yeccgoto(attr_val, 293) ->
 294;
yeccgoto(attribute, 0) ->
 3;
yeccgoto(bin_element, 24) ->
 246;
yeccgoto(bin_element, 266) ->
 246;
yeccgoto(bin_elements, 24) ->
 247;
yeccgoto(bin_elements, 266) ->
 267;
yeccgoto(binary, 14) ->
 29;
yeccgoto(binary, 18) ->
 29;
yeccgoto(binary, 20) ->
 29;
yeccgoto(binary, 24) ->
 29;
yeccgoto(binary, 25) ->
 29;
yeccgoto(binary, 28) ->
 29;
yeccgoto(binary, 31) ->
 29;
yeccgoto(binary, 33) ->
 29;
yeccgoto(binary, 54) ->
 29;
yeccgoto(binary, 60) ->
 29;
yeccgoto(binary, 63) ->
 29;
yeccgoto(binary, 68) ->
 29;
yeccgoto(binary, 72) ->
 29;
yeccgoto(binary, 77) ->
 29;
yeccgoto(binary, 78) ->
 29;
yeccgoto(binary, 79) ->
 29;
yeccgoto(binary, 85) ->
 29;
yeccgoto(binary, 89) ->
 29;
yeccgoto(binary, 96) ->
 29;
yeccgoto(binary, 100) ->
 29;
yeccgoto(binary, 104) ->
 29;
yeccgoto(binary, 108) ->
 29;
yeccgoto(binary, 115) ->
 29;
yeccgoto(binary, 117) ->
 29;
yeccgoto(binary, 125) ->
 29;
yeccgoto(binary, 129) ->
 29;
yeccgoto(binary, 134) ->
 29;
yeccgoto(binary, 136) ->
 29;
yeccgoto(binary, 143) ->
 29;
yeccgoto(binary, 161) ->
 29;
yeccgoto(binary, 173) ->
 29;
yeccgoto(binary, 178) ->
 29;
yeccgoto(binary, 182) ->
 29;
yeccgoto(binary, 191) ->
 29;
yeccgoto(binary, 198) ->
 29;
yeccgoto(binary, 203) ->
 29;
yeccgoto(binary, 216) ->
 29;
yeccgoto(binary, 218) ->
 29;
yeccgoto(binary, 220) ->
 29;
yeccgoto(binary, 222) ->
 29;
yeccgoto(binary, 223) ->
 29;
yeccgoto(binary, 226) ->
 29;
yeccgoto(binary, 230) ->
 29;
yeccgoto(binary, 237) ->
 29;
yeccgoto(binary, 240) ->
 29;
yeccgoto(binary, 250) ->
 29;
yeccgoto(binary, 252) ->
 29;
yeccgoto(binary, 266) ->
 29;
yeccgoto(binary, 275) ->
 29;
yeccgoto(binary, 293) ->
 29;
yeccgoto(bit_expr, 24) ->
 248;
yeccgoto(bit_expr, 266) ->
 248;
yeccgoto(bit_size_expr, 252) ->
 263;
yeccgoto(bit_type, 254) ->
 257;
yeccgoto(bit_type, 259) ->
 257;
yeccgoto(bit_type_list, 254) ->
 258;
yeccgoto(bit_type_list, 259) ->
 260;
yeccgoto(case_expr, 14) ->
 32;
yeccgoto(case_expr, 18) ->
 32;
yeccgoto(case_expr, 20) ->
 32;
yeccgoto(case_expr, 24) ->
 32;
yeccgoto(case_expr, 25) ->
 32;
yeccgoto(case_expr, 28) ->
 32;
yeccgoto(case_expr, 31) ->
 32;
yeccgoto(case_expr, 33) ->
 32;
yeccgoto(case_expr, 54) ->
 32;
yeccgoto(case_expr, 60) ->
 32;
yeccgoto(case_expr, 63) ->
 32;
yeccgoto(case_expr, 68) ->
 32;
yeccgoto(case_expr, 72) ->
 32;
yeccgoto(case_expr, 77) ->
 32;
yeccgoto(case_expr, 78) ->
 32;
yeccgoto(case_expr, 79) ->
 32;
yeccgoto(case_expr, 85) ->
 32;
yeccgoto(case_expr, 89) ->
 32;
yeccgoto(case_expr, 96) ->
 32;
yeccgoto(case_expr, 100) ->
 32;
yeccgoto(case_expr, 104) ->
 32;
yeccgoto(case_expr, 108) ->
 32;
yeccgoto(case_expr, 115) ->
 32;
yeccgoto(case_expr, 117) ->
 32;
yeccgoto(case_expr, 125) ->
 32;
yeccgoto(case_expr, 129) ->
 32;
yeccgoto(case_expr, 134) ->
 32;
yeccgoto(case_expr, 136) ->
 32;
yeccgoto(case_expr, 143) ->
 32;
yeccgoto(case_expr, 161) ->
 32;
yeccgoto(case_expr, 173) ->
 32;
yeccgoto(case_expr, 178) ->
 32;
yeccgoto(case_expr, 182) ->
 32;
yeccgoto(case_expr, 191) ->
 32;
yeccgoto(case_expr, 198) ->
 32;
yeccgoto(case_expr, 203) ->
 32;
yeccgoto(case_expr, 216) ->
 32;
yeccgoto(case_expr, 218) ->
 32;
yeccgoto(case_expr, 220) ->
 32;
yeccgoto(case_expr, 222) ->
 32;
yeccgoto(case_expr, 223) ->
 32;
yeccgoto(case_expr, 226) ->
 32;
yeccgoto(case_expr, 230) ->
 32;
yeccgoto(case_expr, 237) ->
 32;
yeccgoto(case_expr, 240) ->
 32;
yeccgoto(case_expr, 250) ->
 32;
yeccgoto(case_expr, 252) ->
 32;
yeccgoto(case_expr, 266) ->
 32;
yeccgoto(case_expr, 275) ->
 32;
yeccgoto(case_expr, 293) ->
 32;
yeccgoto(clause_args, 2) ->
 290;
yeccgoto(clause_args, 12) ->
 16;
yeccgoto(clause_args, 283) ->
 285;
yeccgoto(clause_body, 84) ->
 86;
yeccgoto(clause_body, 98) ->
 99;
yeccgoto(clause_body, 106) ->
 107;
yeccgoto(clause_body, 110) ->
 111;
yeccgoto(clause_body, 119) ->
 120;
yeccgoto(clause_body, 122) ->
 123;
yeccgoto(clause_body, 139) ->
 145;
yeccgoto(clause_body, 159) ->
 160;
yeccgoto(clause_body, 286) ->
 287;
yeccgoto(clause_body, 291) ->
 287;
yeccgoto(clause_guard, 16) ->
 17;
yeccgoto(clause_guard, 83) ->
 84;
yeccgoto(clause_guard, 92) ->
 106;
yeccgoto(clause_guard, 97) ->
 98;
yeccgoto(clause_guard, 109) ->
 110;
yeccgoto(clause_guard, 146) ->
 159;
yeccgoto(clause_guard, 285) ->
 286;
yeccgoto(clause_guard, 290) ->
 291;
yeccgoto(comp_op, 40) ->
 216;
yeccgoto(cr_clause, 63) ->
 81;
yeccgoto(cr_clause, 79) ->
 81;
yeccgoto(cr_clause, 89) ->
 81;
yeccgoto(cr_clause, 230) ->
 81;
yeccgoto(cr_clauses, 63) ->
 116;
yeccgoto(cr_clauses, 79) ->
 82;
yeccgoto(cr_clauses, 89) ->
 90;
yeccgoto(cr_clauses, 230) ->
 231;
yeccgoto(expr, 14) ->
 35;
yeccgoto(expr, 18) ->
 35;
yeccgoto(expr, 20) ->
 269;
yeccgoto(expr, 25) ->
 236;
yeccgoto(expr, 28) ->
 35;
yeccgoto(expr, 31) ->
 229;
yeccgoto(expr, 33) ->
 228;
yeccgoto(expr, 54) ->
 35;
yeccgoto(expr, 63) ->
 83;
yeccgoto(expr, 68) ->
 35;
yeccgoto(expr, 72) ->
 35;
yeccgoto(expr, 77) ->
 35;
yeccgoto(expr, 78) ->
 92;
yeccgoto(expr, 79) ->
 83;
yeccgoto(expr, 85) ->
 35;
yeccgoto(expr, 89) ->
 83;
yeccgoto(expr, 96) ->
 97;
yeccgoto(expr, 100) ->
 35;
yeccgoto(expr, 104) ->
 92;
yeccgoto(expr, 108) ->
 109;
yeccgoto(expr, 115) ->
 122;
yeccgoto(expr, 117) ->
 119;
yeccgoto(expr, 125) ->
 128;
yeccgoto(expr, 129) ->
 130;
yeccgoto(expr, 134) ->
 130;
yeccgoto(expr, 136) ->
 137;
yeccgoto(expr, 143) ->
 35;
yeccgoto(expr, 161) ->
 35;
yeccgoto(expr, 173) ->
 174;
yeccgoto(expr, 178) ->
 179;
yeccgoto(expr, 226) ->
 35;
yeccgoto(expr, 230) ->
 83;
yeccgoto(expr, 237) ->
 243;
yeccgoto(expr, 240) ->
 241;
yeccgoto(expr, 275) ->
 130;
yeccgoto(expr, 293) ->
 35;
yeccgoto(expr_100, 14) ->
 36;
yeccgoto(expr_100, 18) ->
 36;
yeccgoto(expr_100, 20) ->
 36;
yeccgoto(expr_100, 25) ->
 36;
yeccgoto(expr_100, 28) ->
 36;
yeccgoto(expr_100, 31) ->
 36;
yeccgoto(expr_100, 33) ->
 36;
yeccgoto(expr_100, 54) ->
 36;
yeccgoto(expr_100, 63) ->
 36;
yeccgoto(expr_100, 68) ->
 36;
yeccgoto(expr_100, 72) ->
 36;
yeccgoto(expr_100, 77) ->
 36;
yeccgoto(expr_100, 78) ->
 36;
yeccgoto(expr_100, 79) ->
 36;
yeccgoto(expr_100, 85) ->
 36;
yeccgoto(expr_100, 89) ->
 36;
yeccgoto(expr_100, 96) ->
 36;
yeccgoto(expr_100, 100) ->
 36;
yeccgoto(expr_100, 104) ->
 36;
yeccgoto(expr_100, 108) ->
 36;
yeccgoto(expr_100, 115) ->
 36;
yeccgoto(expr_100, 117) ->
 36;
yeccgoto(expr_100, 125) ->
 36;
yeccgoto(expr_100, 129) ->
 36;
yeccgoto(expr_100, 134) ->
 36;
yeccgoto(expr_100, 136) ->
 36;
yeccgoto(expr_100, 143) ->
 36;
yeccgoto(expr_100, 161) ->
 36;
yeccgoto(expr_100, 173) ->
 36;
yeccgoto(expr_100, 178) ->
 36;
yeccgoto(expr_100, 222) ->
 225;
yeccgoto(expr_100, 223) ->
 224;
yeccgoto(expr_100, 226) ->
 36;
yeccgoto(expr_100, 230) ->
 36;
yeccgoto(expr_100, 237) ->
 36;
yeccgoto(expr_100, 240) ->
 36;
yeccgoto(expr_100, 275) ->
 36;
yeccgoto(expr_100, 293) ->
 36;
yeccgoto(expr_150, 14) ->
 37;
yeccgoto(expr_150, 18) ->
 37;
yeccgoto(expr_150, 20) ->
 37;
yeccgoto(expr_150, 25) ->
 37;
yeccgoto(expr_150, 28) ->
 37;
yeccgoto(expr_150, 31) ->
 37;
yeccgoto(expr_150, 33) ->
 37;
yeccgoto(expr_150, 54) ->
 37;
yeccgoto(expr_150, 63) ->
 37;
yeccgoto(expr_150, 68) ->
 37;
yeccgoto(expr_150, 72) ->
 37;
yeccgoto(expr_150, 77) ->
 37;
yeccgoto(expr_150, 78) ->
 37;
yeccgoto(expr_150, 79) ->
 37;
yeccgoto(expr_150, 85) ->
 37;
yeccgoto(expr_150, 89) ->
 37;
yeccgoto(expr_150, 96) ->
 37;
yeccgoto(expr_150, 100) ->
 37;
yeccgoto(expr_150, 104) ->
 37;
yeccgoto(expr_150, 108) ->
 37;
yeccgoto(expr_150, 115) ->
 37;
yeccgoto(expr_150, 117) ->
 37;
yeccgoto(expr_150, 125) ->
 37;
yeccgoto(expr_150, 129) ->
 37;
yeccgoto(expr_150, 134) ->
 37;
yeccgoto(expr_150, 136) ->
 37;
yeccgoto(expr_150, 143) ->
 37;
yeccgoto(expr_150, 161) ->
 37;
yeccgoto(expr_150, 173) ->
 37;
yeccgoto(expr_150, 178) ->
 37;
yeccgoto(expr_150, 220) ->
 221;
yeccgoto(expr_150, 222) ->
 37;
yeccgoto(expr_150, 223) ->
 37;
yeccgoto(expr_150, 226) ->
 37;
yeccgoto(expr_150, 230) ->
 37;
yeccgoto(expr_150, 237) ->
 37;
yeccgoto(expr_150, 240) ->
 37;
yeccgoto(expr_150, 275) ->
 37;
yeccgoto(expr_150, 293) ->
 37;
yeccgoto(expr_160, 14) ->
 38;
yeccgoto(expr_160, 18) ->
 38;
yeccgoto(expr_160, 20) ->
 38;
yeccgoto(expr_160, 25) ->
 38;
yeccgoto(expr_160, 28) ->
 38;
yeccgoto(expr_160, 31) ->
 38;
yeccgoto(expr_160, 33) ->
 38;
yeccgoto(expr_160, 54) ->
 38;
yeccgoto(expr_160, 63) ->
 38;
yeccgoto(expr_160, 68) ->
 38;
yeccgoto(expr_160, 72) ->
 38;
yeccgoto(expr_160, 77) ->
 38;
yeccgoto(expr_160, 78) ->
 38;
yeccgoto(expr_160, 79) ->
 38;
yeccgoto(expr_160, 85) ->
 38;
yeccgoto(expr_160, 89) ->
 38;
yeccgoto(expr_160, 96) ->
 38;
yeccgoto(expr_160, 100) ->
 38;
yeccgoto(expr_160, 104) ->
 38;
yeccgoto(expr_160, 108) ->
 38;
yeccgoto(expr_160, 115) ->
 38;
yeccgoto(expr_160, 117) ->
 38;
yeccgoto(expr_160, 125) ->
 38;
yeccgoto(expr_160, 129) ->
 38;
yeccgoto(expr_160, 134) ->
 38;
yeccgoto(expr_160, 136) ->
 38;
yeccgoto(expr_160, 143) ->
 38;
yeccgoto(expr_160, 161) ->
 38;
yeccgoto(expr_160, 173) ->
 38;
yeccgoto(expr_160, 178) ->
 38;
yeccgoto(expr_160, 218) ->
 219;
yeccgoto(expr_160, 220) ->
 38;
yeccgoto(expr_160, 222) ->
 38;
yeccgoto(expr_160, 223) ->
 38;
yeccgoto(expr_160, 226) ->
 38;
yeccgoto(expr_160, 230) ->
 38;
yeccgoto(expr_160, 237) ->
 38;
yeccgoto(expr_160, 240) ->
 38;
yeccgoto(expr_160, 275) ->
 38;
yeccgoto(expr_160, 293) ->
 38;
yeccgoto(expr_200, 14) ->
 39;
yeccgoto(expr_200, 18) ->
 39;
yeccgoto(expr_200, 20) ->
 39;
yeccgoto(expr_200, 25) ->
 39;
yeccgoto(expr_200, 28) ->
 39;
yeccgoto(expr_200, 31) ->
 39;
yeccgoto(expr_200, 33) ->
 39;
yeccgoto(expr_200, 54) ->
 39;
yeccgoto(expr_200, 63) ->
 39;
yeccgoto(expr_200, 68) ->
 39;
yeccgoto(expr_200, 72) ->
 39;
yeccgoto(expr_200, 77) ->
 39;
yeccgoto(expr_200, 78) ->
 39;
yeccgoto(expr_200, 79) ->
 39;
yeccgoto(expr_200, 85) ->
 39;
yeccgoto(expr_200, 89) ->
 39;
yeccgoto(expr_200, 96) ->
 39;
yeccgoto(expr_200, 100) ->
 39;
yeccgoto(expr_200, 104) ->
 39;
yeccgoto(expr_200, 108) ->
 39;
yeccgoto(expr_200, 115) ->
 39;
yeccgoto(expr_200, 117) ->
 39;
yeccgoto(expr_200, 125) ->
 39;
yeccgoto(expr_200, 129) ->
 39;
yeccgoto(expr_200, 134) ->
 39;
yeccgoto(expr_200, 136) ->
 39;
yeccgoto(expr_200, 143) ->
 39;
yeccgoto(expr_200, 161) ->
 39;
yeccgoto(expr_200, 173) ->
 39;
yeccgoto(expr_200, 178) ->
 39;
yeccgoto(expr_200, 218) ->
 39;
yeccgoto(expr_200, 220) ->
 39;
yeccgoto(expr_200, 222) ->
 39;
yeccgoto(expr_200, 223) ->
 39;
yeccgoto(expr_200, 226) ->
 39;
yeccgoto(expr_200, 230) ->
 39;
yeccgoto(expr_200, 237) ->
 39;
yeccgoto(expr_200, 240) ->
 39;
yeccgoto(expr_200, 275) ->
 39;
yeccgoto(expr_200, 293) ->
 39;
yeccgoto(expr_300, 14) ->
 40;
yeccgoto(expr_300, 18) ->
 40;
yeccgoto(expr_300, 20) ->
 40;
yeccgoto(expr_300, 25) ->
 40;
yeccgoto(expr_300, 28) ->
 40;
yeccgoto(expr_300, 31) ->
 40;
yeccgoto(expr_300, 33) ->
 40;
yeccgoto(expr_300, 54) ->
 40;
yeccgoto(expr_300, 63) ->
 40;
yeccgoto(expr_300, 68) ->
 40;
yeccgoto(expr_300, 72) ->
 40;
yeccgoto(expr_300, 77) ->
 40;
yeccgoto(expr_300, 78) ->
 40;
yeccgoto(expr_300, 79) ->
 40;
yeccgoto(expr_300, 85) ->
 40;
yeccgoto(expr_300, 89) ->
 40;
yeccgoto(expr_300, 96) ->
 40;
yeccgoto(expr_300, 100) ->
 40;
yeccgoto(expr_300, 104) ->
 40;
yeccgoto(expr_300, 108) ->
 40;
yeccgoto(expr_300, 115) ->
 40;
yeccgoto(expr_300, 117) ->
 40;
yeccgoto(expr_300, 125) ->
 40;
yeccgoto(expr_300, 129) ->
 40;
yeccgoto(expr_300, 134) ->
 40;
yeccgoto(expr_300, 136) ->
 40;
yeccgoto(expr_300, 143) ->
 40;
yeccgoto(expr_300, 161) ->
 40;
yeccgoto(expr_300, 173) ->
 40;
yeccgoto(expr_300, 178) ->
 40;
yeccgoto(expr_300, 203) ->
 206;
yeccgoto(expr_300, 216) ->
 217;
yeccgoto(expr_300, 218) ->
 40;
yeccgoto(expr_300, 220) ->
 40;
yeccgoto(expr_300, 222) ->
 40;
yeccgoto(expr_300, 223) ->
 40;
yeccgoto(expr_300, 226) ->
 40;
yeccgoto(expr_300, 230) ->
 40;
yeccgoto(expr_300, 237) ->
 40;
yeccgoto(expr_300, 240) ->
 40;
yeccgoto(expr_300, 275) ->
 40;
yeccgoto(expr_300, 293) ->
 40;
yeccgoto(expr_400, 14) ->
 41;
yeccgoto(expr_400, 18) ->
 41;
yeccgoto(expr_400, 20) ->
 41;
yeccgoto(expr_400, 25) ->
 41;
yeccgoto(expr_400, 28) ->
 41;
yeccgoto(expr_400, 31) ->
 41;
yeccgoto(expr_400, 33) ->
 41;
yeccgoto(expr_400, 54) ->
 41;
yeccgoto(expr_400, 63) ->
 41;
yeccgoto(expr_400, 68) ->
 41;
yeccgoto(expr_400, 72) ->
 41;
yeccgoto(expr_400, 77) ->
 41;
yeccgoto(expr_400, 78) ->
 41;
yeccgoto(expr_400, 79) ->
 41;
yeccgoto(expr_400, 85) ->
 41;
yeccgoto(expr_400, 89) ->
 41;
yeccgoto(expr_400, 96) ->
 41;
yeccgoto(expr_400, 100) ->
 41;
yeccgoto(expr_400, 104) ->
 41;
yeccgoto(expr_400, 108) ->
 41;
yeccgoto(expr_400, 115) ->
 41;
yeccgoto(expr_400, 117) ->
 41;
yeccgoto(expr_400, 125) ->
 41;
yeccgoto(expr_400, 129) ->
 41;
yeccgoto(expr_400, 134) ->
 41;
yeccgoto(expr_400, 136) ->
 41;
yeccgoto(expr_400, 143) ->
 41;
yeccgoto(expr_400, 161) ->
 41;
yeccgoto(expr_400, 173) ->
 41;
yeccgoto(expr_400, 178) ->
 41;
yeccgoto(expr_400, 203) ->
 41;
yeccgoto(expr_400, 216) ->
 41;
yeccgoto(expr_400, 218) ->
 41;
yeccgoto(expr_400, 220) ->
 41;
yeccgoto(expr_400, 222) ->
 41;
yeccgoto(expr_400, 223) ->
 41;
yeccgoto(expr_400, 226) ->
 41;
yeccgoto(expr_400, 230) ->
 41;
yeccgoto(expr_400, 237) ->
 41;
yeccgoto(expr_400, 240) ->
 41;
yeccgoto(expr_400, 275) ->
 41;
yeccgoto(expr_400, 293) ->
 41;
yeccgoto(expr_500, 14) ->
 42;
yeccgoto(expr_500, 18) ->
 42;
yeccgoto(expr_500, 20) ->
 42;
yeccgoto(expr_500, 25) ->
 42;
yeccgoto(expr_500, 28) ->
 42;
yeccgoto(expr_500, 31) ->
 42;
yeccgoto(expr_500, 33) ->
 42;
yeccgoto(expr_500, 54) ->
 42;
yeccgoto(expr_500, 63) ->
 42;
yeccgoto(expr_500, 68) ->
 42;
yeccgoto(expr_500, 72) ->
 42;
yeccgoto(expr_500, 77) ->
 42;
yeccgoto(expr_500, 78) ->
 42;
yeccgoto(expr_500, 79) ->
 42;
yeccgoto(expr_500, 85) ->
 42;
yeccgoto(expr_500, 89) ->
 42;
yeccgoto(expr_500, 96) ->
 42;
yeccgoto(expr_500, 100) ->
 42;
yeccgoto(expr_500, 104) ->
 42;
yeccgoto(expr_500, 108) ->
 42;
yeccgoto(expr_500, 115) ->
 42;
yeccgoto(expr_500, 117) ->
 42;
yeccgoto(expr_500, 125) ->
 42;
yeccgoto(expr_500, 129) ->
 42;
yeccgoto(expr_500, 134) ->
 42;
yeccgoto(expr_500, 136) ->
 42;
yeccgoto(expr_500, 143) ->
 42;
yeccgoto(expr_500, 161) ->
 42;
yeccgoto(expr_500, 173) ->
 42;
yeccgoto(expr_500, 178) ->
 42;
yeccgoto(expr_500, 198) ->
 207;
yeccgoto(expr_500, 203) ->
 42;
yeccgoto(expr_500, 216) ->
 42;
yeccgoto(expr_500, 218) ->
 42;
yeccgoto(expr_500, 220) ->
 42;
yeccgoto(expr_500, 222) ->
 42;
yeccgoto(expr_500, 223) ->
 42;
yeccgoto(expr_500, 226) ->
 42;
yeccgoto(expr_500, 230) ->
 42;
yeccgoto(expr_500, 237) ->
 42;
yeccgoto(expr_500, 240) ->
 42;
yeccgoto(expr_500, 275) ->
 42;
yeccgoto(expr_500, 293) ->
 42;
yeccgoto(expr_600, 14) ->
 43;
yeccgoto(expr_600, 18) ->
 43;
yeccgoto(expr_600, 20) ->
 43;
yeccgoto(expr_600, 25) ->
 43;
yeccgoto(expr_600, 28) ->
 43;
yeccgoto(expr_600, 31) ->
 43;
yeccgoto(expr_600, 33) ->
 43;
yeccgoto(expr_600, 54) ->
 43;
yeccgoto(expr_600, 63) ->
 43;
yeccgoto(expr_600, 68) ->
 43;
yeccgoto(expr_600, 72) ->
 43;
yeccgoto(expr_600, 77) ->
 43;
yeccgoto(expr_600, 78) ->
 43;
yeccgoto(expr_600, 79) ->
 43;
yeccgoto(expr_600, 85) ->
 43;
yeccgoto(expr_600, 89) ->
 43;
yeccgoto(expr_600, 96) ->
 43;
yeccgoto(expr_600, 100) ->
 43;
yeccgoto(expr_600, 104) ->
 43;
yeccgoto(expr_600, 108) ->
 43;
yeccgoto(expr_600, 115) ->
 43;
yeccgoto(expr_600, 117) ->
 43;
yeccgoto(expr_600, 125) ->
 43;
yeccgoto(expr_600, 129) ->
 43;
yeccgoto(expr_600, 134) ->
 43;
yeccgoto(expr_600, 136) ->
 43;
yeccgoto(expr_600, 143) ->
 43;
yeccgoto(expr_600, 161) ->
 43;
yeccgoto(expr_600, 173) ->
 43;
yeccgoto(expr_600, 178) ->
 43;
yeccgoto(expr_600, 191) ->
 193;
yeccgoto(expr_600, 198) ->
 43;
yeccgoto(expr_600, 203) ->
 43;
yeccgoto(expr_600, 216) ->
 43;
yeccgoto(expr_600, 218) ->
 43;
yeccgoto(expr_600, 220) ->
 43;
yeccgoto(expr_600, 222) ->
 43;
yeccgoto(expr_600, 223) ->
 43;
yeccgoto(expr_600, 226) ->
 43;
yeccgoto(expr_600, 230) ->
 43;
yeccgoto(expr_600, 237) ->
 43;
yeccgoto(expr_600, 240) ->
 43;
yeccgoto(expr_600, 275) ->
 43;
yeccgoto(expr_600, 293) ->
 43;
yeccgoto(expr_700, 14) ->
 44;
yeccgoto(expr_700, 18) ->
 44;
yeccgoto(expr_700, 20) ->
 44;
yeccgoto(expr_700, 25) ->
 44;
yeccgoto(expr_700, 28) ->
 44;
yeccgoto(expr_700, 31) ->
 44;
yeccgoto(expr_700, 33) ->
 44;
yeccgoto(expr_700, 54) ->
 44;
yeccgoto(expr_700, 60) ->
 138;
yeccgoto(expr_700, 63) ->
 44;
yeccgoto(expr_700, 68) ->
 44;
yeccgoto(expr_700, 72) ->
 44;
yeccgoto(expr_700, 77) ->
 44;
yeccgoto(expr_700, 78) ->
 44;
yeccgoto(expr_700, 79) ->
 44;
yeccgoto(expr_700, 85) ->
 44;
yeccgoto(expr_700, 89) ->
 44;
yeccgoto(expr_700, 96) ->
 44;
yeccgoto(expr_700, 100) ->
 44;
yeccgoto(expr_700, 104) ->
 44;
yeccgoto(expr_700, 108) ->
 44;
yeccgoto(expr_700, 115) ->
 44;
yeccgoto(expr_700, 117) ->
 44;
yeccgoto(expr_700, 125) ->
 44;
yeccgoto(expr_700, 129) ->
 44;
yeccgoto(expr_700, 134) ->
 44;
yeccgoto(expr_700, 136) ->
 44;
yeccgoto(expr_700, 143) ->
 44;
yeccgoto(expr_700, 161) ->
 44;
yeccgoto(expr_700, 173) ->
 44;
yeccgoto(expr_700, 178) ->
 44;
yeccgoto(expr_700, 191) ->
 44;
yeccgoto(expr_700, 198) ->
 44;
yeccgoto(expr_700, 203) ->
 44;
yeccgoto(expr_700, 216) ->
 44;
yeccgoto(expr_700, 218) ->
 44;
yeccgoto(expr_700, 220) ->
 44;
yeccgoto(expr_700, 222) ->
 44;
yeccgoto(expr_700, 223) ->
 44;
yeccgoto(expr_700, 226) ->
 44;
yeccgoto(expr_700, 230) ->
 44;
yeccgoto(expr_700, 237) ->
 44;
yeccgoto(expr_700, 240) ->
 44;
yeccgoto(expr_700, 275) ->
 44;
yeccgoto(expr_700, 293) ->
 44;
yeccgoto(expr_800, 14) ->
 45;
yeccgoto(expr_800, 18) ->
 45;
yeccgoto(expr_800, 20) ->
 45;
yeccgoto(expr_800, 25) ->
 45;
yeccgoto(expr_800, 28) ->
 45;
yeccgoto(expr_800, 31) ->
 45;
yeccgoto(expr_800, 33) ->
 45;
yeccgoto(expr_800, 54) ->
 45;
yeccgoto(expr_800, 60) ->
 45;
yeccgoto(expr_800, 63) ->
 45;
yeccgoto(expr_800, 68) ->
 45;
yeccgoto(expr_800, 72) ->
 45;
yeccgoto(expr_800, 77) ->
 45;
yeccgoto(expr_800, 78) ->
 45;
yeccgoto(expr_800, 79) ->
 45;
yeccgoto(expr_800, 85) ->
 45;
yeccgoto(expr_800, 89) ->
 45;
yeccgoto(expr_800, 96) ->
 45;
yeccgoto(expr_800, 100) ->
 45;
yeccgoto(expr_800, 104) ->
 45;
yeccgoto(expr_800, 108) ->
 45;
yeccgoto(expr_800, 115) ->
 45;
yeccgoto(expr_800, 117) ->
 45;
yeccgoto(expr_800, 125) ->
 45;
yeccgoto(expr_800, 129) ->
 45;
yeccgoto(expr_800, 134) ->
 45;
yeccgoto(expr_800, 136) ->
 45;
yeccgoto(expr_800, 143) ->
 45;
yeccgoto(expr_800, 161) ->
 45;
yeccgoto(expr_800, 173) ->
 45;
yeccgoto(expr_800, 178) ->
 45;
yeccgoto(expr_800, 191) ->
 45;
yeccgoto(expr_800, 198) ->
 45;
yeccgoto(expr_800, 203) ->
 45;
yeccgoto(expr_800, 216) ->
 45;
yeccgoto(expr_800, 218) ->
 45;
yeccgoto(expr_800, 220) ->
 45;
yeccgoto(expr_800, 222) ->
 45;
yeccgoto(expr_800, 223) ->
 45;
yeccgoto(expr_800, 226) ->
 45;
yeccgoto(expr_800, 230) ->
 45;
yeccgoto(expr_800, 237) ->
 45;
yeccgoto(expr_800, 240) ->
 45;
yeccgoto(expr_800, 275) ->
 45;
yeccgoto(expr_800, 293) ->
 45;
yeccgoto(expr_900, 14) ->
 46;
yeccgoto(expr_900, 18) ->
 46;
yeccgoto(expr_900, 20) ->
 46;
yeccgoto(expr_900, 25) ->
 46;
yeccgoto(expr_900, 28) ->
 46;
yeccgoto(expr_900, 31) ->
 46;
yeccgoto(expr_900, 33) ->
 46;
yeccgoto(expr_900, 54) ->
 46;
yeccgoto(expr_900, 60) ->
 46;
yeccgoto(expr_900, 63) ->
 46;
yeccgoto(expr_900, 68) ->
 46;
yeccgoto(expr_900, 72) ->
 46;
yeccgoto(expr_900, 77) ->
 46;
yeccgoto(expr_900, 78) ->
 46;
yeccgoto(expr_900, 79) ->
 46;
yeccgoto(expr_900, 85) ->
 46;
yeccgoto(expr_900, 89) ->
 46;
yeccgoto(expr_900, 96) ->
 46;
yeccgoto(expr_900, 100) ->
 46;
yeccgoto(expr_900, 104) ->
 46;
yeccgoto(expr_900, 108) ->
 46;
yeccgoto(expr_900, 115) ->
 46;
yeccgoto(expr_900, 117) ->
 46;
yeccgoto(expr_900, 125) ->
 46;
yeccgoto(expr_900, 129) ->
 46;
yeccgoto(expr_900, 134) ->
 46;
yeccgoto(expr_900, 136) ->
 46;
yeccgoto(expr_900, 143) ->
 46;
yeccgoto(expr_900, 161) ->
 46;
yeccgoto(expr_900, 173) ->
 46;
yeccgoto(expr_900, 178) ->
 46;
yeccgoto(expr_900, 191) ->
 46;
yeccgoto(expr_900, 198) ->
 46;
yeccgoto(expr_900, 203) ->
 46;
yeccgoto(expr_900, 216) ->
 46;
yeccgoto(expr_900, 218) ->
 46;
yeccgoto(expr_900, 220) ->
 46;
yeccgoto(expr_900, 222) ->
 46;
yeccgoto(expr_900, 223) ->
 46;
yeccgoto(expr_900, 226) ->
 46;
yeccgoto(expr_900, 230) ->
 46;
yeccgoto(expr_900, 237) ->
 46;
yeccgoto(expr_900, 240) ->
 46;
yeccgoto(expr_900, 275) ->
 46;
yeccgoto(expr_900, 293) ->
 46;
yeccgoto(expr_max, 14) ->
 47;
yeccgoto(expr_max, 18) ->
 47;
yeccgoto(expr_max, 20) ->
 47;
yeccgoto(expr_max, 24) ->
 249;
yeccgoto(expr_max, 25) ->
 47;
yeccgoto(expr_max, 28) ->
 47;
yeccgoto(expr_max, 31) ->
 47;
yeccgoto(expr_max, 33) ->
 47;
yeccgoto(expr_max, 54) ->
 47;
yeccgoto(expr_max, 60) ->
 47;
yeccgoto(expr_max, 63) ->
 47;
yeccgoto(expr_max, 68) ->
 47;
yeccgoto(expr_max, 72) ->
 47;
yeccgoto(expr_max, 77) ->
 47;
yeccgoto(expr_max, 78) ->
 47;
yeccgoto(expr_max, 79) ->
 47;
yeccgoto(expr_max, 85) ->
 47;
yeccgoto(expr_max, 89) ->
 47;
yeccgoto(expr_max, 96) ->
 47;
yeccgoto(expr_max, 100) ->
 47;
yeccgoto(expr_max, 104) ->
 47;
yeccgoto(expr_max, 108) ->
 47;
yeccgoto(expr_max, 115) ->
 47;
yeccgoto(expr_max, 117) ->
 47;
yeccgoto(expr_max, 125) ->
 47;
yeccgoto(expr_max, 129) ->
 47;
yeccgoto(expr_max, 134) ->
 47;
yeccgoto(expr_max, 136) ->
 47;
yeccgoto(expr_max, 143) ->
 47;
yeccgoto(expr_max, 161) ->
 47;
yeccgoto(expr_max, 173) ->
 47;
yeccgoto(expr_max, 178) ->
 47;
yeccgoto(expr_max, 182) ->
 183;
yeccgoto(expr_max, 191) ->
 47;
yeccgoto(expr_max, 198) ->
 47;
yeccgoto(expr_max, 203) ->
 47;
yeccgoto(expr_max, 216) ->
 47;
yeccgoto(expr_max, 218) ->
 47;
yeccgoto(expr_max, 220) ->
 47;
yeccgoto(expr_max, 222) ->
 47;
yeccgoto(expr_max, 223) ->
 47;
yeccgoto(expr_max, 226) ->
 47;
yeccgoto(expr_max, 230) ->
 47;
yeccgoto(expr_max, 237) ->
 47;
yeccgoto(expr_max, 240) ->
 47;
yeccgoto(expr_max, 250) ->
 251;
yeccgoto(expr_max, 252) ->
 264;
yeccgoto(expr_max, 266) ->
 249;
yeccgoto(expr_max, 275) ->
 47;
yeccgoto(expr_max, 293) ->
 47;
yeccgoto(exprs, 14) ->
 279;
yeccgoto(exprs, 18) ->
 48;
yeccgoto(exprs, 28) ->
 233;
yeccgoto(exprs, 54) ->
 48;
yeccgoto(exprs, 68) ->
 76;
yeccgoto(exprs, 72) ->
 73;
yeccgoto(exprs, 77) ->
 112;
yeccgoto(exprs, 85) ->
 87;
yeccgoto(exprs, 100) ->
 102;
yeccgoto(exprs, 143) ->
 48;
yeccgoto(exprs, 161) ->
 48;
yeccgoto(exprs, 226) ->
 227;
yeccgoto(exprs, 293) ->
 295;
yeccgoto(form, 0) ->
 4;
yeccgoto(fun_clause, 50) ->
 148;
yeccgoto(fun_clause, 151) ->
 148;
yeccgoto(fun_clauses, 50) ->
 149;
yeccgoto(fun_clauses, 151) ->
 152;
yeccgoto(fun_expr, 14) ->
 51;
yeccgoto(fun_expr, 18) ->
 51;
yeccgoto(fun_expr, 20) ->
 51;
yeccgoto(fun_expr, 24) ->
 51;
yeccgoto(fun_expr, 25) ->
 51;
yeccgoto(fun_expr, 28) ->
 51;
yeccgoto(fun_expr, 31) ->
 51;
yeccgoto(fun_expr, 33) ->
 51;
yeccgoto(fun_expr, 54) ->
 51;
yeccgoto(fun_expr, 60) ->
 51;
yeccgoto(fun_expr, 63) ->
 51;
yeccgoto(fun_expr, 68) ->
 51;
yeccgoto(fun_expr, 72) ->
 51;
yeccgoto(fun_expr, 77) ->
 51;
yeccgoto(fun_expr, 78) ->
 51;
yeccgoto(fun_expr, 79) ->
 51;
yeccgoto(fun_expr, 85) ->
 51;
yeccgoto(fun_expr, 89) ->
 51;
yeccgoto(fun_expr, 96) ->
 51;
yeccgoto(fun_expr, 100) ->
 51;
yeccgoto(fun_expr, 104) ->
 51;
yeccgoto(fun_expr, 108) ->
 51;
yeccgoto(fun_expr, 115) ->
 51;
yeccgoto(fun_expr, 117) ->
 51;
yeccgoto(fun_expr, 125) ->
 51;
yeccgoto(fun_expr, 129) ->
 51;
yeccgoto(fun_expr, 134) ->
 51;
yeccgoto(fun_expr, 136) ->
 51;
yeccgoto(fun_expr, 143) ->
 51;
yeccgoto(fun_expr, 161) ->
 51;
yeccgoto(fun_expr, 173) ->
 51;
yeccgoto(fun_expr, 178) ->
 51;
yeccgoto(fun_expr, 182) ->
 51;
yeccgoto(fun_expr, 191) ->
 51;
yeccgoto(fun_expr, 198) ->
 51;
yeccgoto(fun_expr, 203) ->
 51;
yeccgoto(fun_expr, 216) ->
 51;
yeccgoto(fun_expr, 218) ->
 51;
yeccgoto(fun_expr, 220) ->
 51;
yeccgoto(fun_expr, 222) ->
 51;
yeccgoto(fun_expr, 223) ->
 51;
yeccgoto(fun_expr, 226) ->
 51;
yeccgoto(fun_expr, 230) ->
 51;
yeccgoto(fun_expr, 237) ->
 51;
yeccgoto(fun_expr, 240) ->
 51;
yeccgoto(fun_expr, 250) ->
 51;
yeccgoto(fun_expr, 252) ->
 51;
yeccgoto(fun_expr, 266) ->
 51;
yeccgoto(fun_expr, 275) ->
 51;
yeccgoto(fun_expr, 293) ->
 51;
yeccgoto(function, 0) ->
 5;
yeccgoto(function_call, 14) ->
 52;
yeccgoto(function_call, 18) ->
 52;
yeccgoto(function_call, 20) ->
 52;
yeccgoto(function_call, 25) ->
 52;
yeccgoto(function_call, 28) ->
 52;
yeccgoto(function_call, 31) ->
 52;
yeccgoto(function_call, 33) ->
 52;
yeccgoto(function_call, 54) ->
 52;
yeccgoto(function_call, 60) ->
 52;
yeccgoto(function_call, 63) ->
 52;
yeccgoto(function_call, 68) ->
 52;
yeccgoto(function_call, 72) ->
 52;
yeccgoto(function_call, 77) ->
 52;
yeccgoto(function_call, 78) ->
 52;
yeccgoto(function_call, 79) ->
 52;
yeccgoto(function_call, 85) ->
 52;
yeccgoto(function_call, 89) ->
 52;
yeccgoto(function_call, 96) ->
 52;
yeccgoto(function_call, 100) ->
 52;
yeccgoto(function_call, 104) ->
 52;
yeccgoto(function_call, 108) ->
 52;
yeccgoto(function_call, 115) ->
 52;
yeccgoto(function_call, 117) ->
 52;
yeccgoto(function_call, 125) ->
 52;
yeccgoto(function_call, 129) ->
 52;
yeccgoto(function_call, 134) ->
 52;
yeccgoto(function_call, 136) ->
 52;
yeccgoto(function_call, 143) ->
 52;
yeccgoto(function_call, 161) ->
 52;
yeccgoto(function_call, 173) ->
 52;
yeccgoto(function_call, 178) ->
 52;
yeccgoto(function_call, 191) ->
 52;
yeccgoto(function_call, 198) ->
 52;
yeccgoto(function_call, 203) ->
 52;
yeccgoto(function_call, 216) ->
 52;
yeccgoto(function_call, 218) ->
 52;
yeccgoto(function_call, 220) ->
 52;
yeccgoto(function_call, 222) ->
 52;
yeccgoto(function_call, 223) ->
 52;
yeccgoto(function_call, 226) ->
 52;
yeccgoto(function_call, 230) ->
 52;
yeccgoto(function_call, 237) ->
 52;
yeccgoto(function_call, 240) ->
 52;
yeccgoto(function_call, 275) ->
 52;
yeccgoto(function_call, 293) ->
 52;
yeccgoto(function_clause, 0) ->
 6;
yeccgoto(function_clause, 282) ->
 6;
yeccgoto(function_clauses, 0) ->
 7;
yeccgoto(function_clauses, 282) ->
 284;
yeccgoto(guard, 18) ->
 53;
yeccgoto(guard, 54) ->
 139;
yeccgoto(guard, 143) ->
 139;
yeccgoto(guard, 161) ->
 162;
yeccgoto(if_clause, 54) ->
 140;
yeccgoto(if_clause, 143) ->
 140;
yeccgoto(if_clauses, 54) ->
 141;
yeccgoto(if_clauses, 143) ->
 144;
yeccgoto(if_expr, 14) ->
 55;
yeccgoto(if_expr, 18) ->
 55;
yeccgoto(if_expr, 20) ->
 55;
yeccgoto(if_expr, 24) ->
 55;
yeccgoto(if_expr, 25) ->
 55;
yeccgoto(if_expr, 28) ->
 55;
yeccgoto(if_expr, 31) ->
 55;
yeccgoto(if_expr, 33) ->
 55;
yeccgoto(if_expr, 54) ->
 55;
yeccgoto(if_expr, 60) ->
 55;
yeccgoto(if_expr, 63) ->
 55;
yeccgoto(if_expr, 68) ->
 55;
yeccgoto(if_expr, 72) ->
 55;
yeccgoto(if_expr, 77) ->
 55;
yeccgoto(if_expr, 78) ->
 55;
yeccgoto(if_expr, 79) ->
 55;
yeccgoto(if_expr, 85) ->
 55;
yeccgoto(if_expr, 89) ->
 55;
yeccgoto(if_expr, 96) ->
 55;
yeccgoto(if_expr, 100) ->
 55;
yeccgoto(if_expr, 104) ->
 55;
yeccgoto(if_expr, 108) ->
 55;
yeccgoto(if_expr, 115) ->
 55;
yeccgoto(if_expr, 117) ->
 55;
yeccgoto(if_expr, 125) ->
 55;
yeccgoto(if_expr, 129) ->
 55;
yeccgoto(if_expr, 134) ->
 55;
yeccgoto(if_expr, 136) ->
 55;
yeccgoto(if_expr, 143) ->
 55;
yeccgoto(if_expr, 161) ->
 55;
yeccgoto(if_expr, 173) ->
 55;
yeccgoto(if_expr, 178) ->
 55;
yeccgoto(if_expr, 182) ->
 55;
yeccgoto(if_expr, 191) ->
 55;
yeccgoto(if_expr, 198) ->
 55;
yeccgoto(if_expr, 203) ->
 55;
yeccgoto(if_expr, 216) ->
 55;
yeccgoto(if_expr, 218) ->
 55;
yeccgoto(if_expr, 220) ->
 55;
yeccgoto(if_expr, 222) ->
 55;
yeccgoto(if_expr, 223) ->
 55;
yeccgoto(if_expr, 226) ->
 55;
yeccgoto(if_expr, 230) ->
 55;
yeccgoto(if_expr, 237) ->
 55;
yeccgoto(if_expr, 240) ->
 55;
yeccgoto(if_expr, 250) ->
 55;
yeccgoto(if_expr, 252) ->
 55;
yeccgoto(if_expr, 266) ->
 55;
yeccgoto(if_expr, 275) ->
 55;
yeccgoto(if_expr, 293) ->
 55;
yeccgoto(lc_expr, 129) ->
 131;
yeccgoto(lc_expr, 134) ->
 131;
yeccgoto(lc_expr, 275) ->
 131;
yeccgoto(lc_exprs, 129) ->
 132;
yeccgoto(lc_exprs, 134) ->
 135;
yeccgoto(lc_exprs, 275) ->
 277;
yeccgoto(list, 14) ->
 57;
yeccgoto(list, 18) ->
 57;
yeccgoto(list, 20) ->
 57;
yeccgoto(list, 24) ->
 57;
yeccgoto(list, 25) ->
 57;
yeccgoto(list, 28) ->
 57;
yeccgoto(list, 31) ->
 57;
yeccgoto(list, 33) ->
 57;
yeccgoto(list, 54) ->
 57;
yeccgoto(list, 60) ->
 57;
yeccgoto(list, 63) ->
 57;
yeccgoto(list, 68) ->
 57;
yeccgoto(list, 72) ->
 57;
yeccgoto(list, 77) ->
 57;
yeccgoto(list, 78) ->
 57;
yeccgoto(list, 79) ->
 57;
yeccgoto(list, 85) ->
 57;
yeccgoto(list, 89) ->
 57;
yeccgoto(list, 96) ->
 57;
yeccgoto(list, 100) ->
 57;
yeccgoto(list, 104) ->
 57;
yeccgoto(list, 108) ->
 57;
yeccgoto(list, 115) ->
 57;
yeccgoto(list, 117) ->
 57;
yeccgoto(list, 125) ->
 57;
yeccgoto(list, 129) ->
 57;
yeccgoto(list, 134) ->
 57;
yeccgoto(list, 136) ->
 57;
yeccgoto(list, 143) ->
 57;
yeccgoto(list, 161) ->
 57;
yeccgoto(list, 173) ->
 57;
yeccgoto(list, 178) ->
 57;
yeccgoto(list, 182) ->
 57;
yeccgoto(list, 191) ->
 57;
yeccgoto(list, 198) ->
 57;
yeccgoto(list, 203) ->
 57;
yeccgoto(list, 216) ->
 57;
yeccgoto(list, 218) ->
 57;
yeccgoto(list, 220) ->
 57;
yeccgoto(list, 222) ->
 57;
yeccgoto(list, 223) ->
 57;
yeccgoto(list, 226) ->
 57;
yeccgoto(list, 230) ->
 57;
yeccgoto(list, 237) ->
 57;
yeccgoto(list, 240) ->
 57;
yeccgoto(list, 250) ->
 57;
yeccgoto(list, 252) ->
 57;
yeccgoto(list, 266) ->
 57;
yeccgoto(list, 275) ->
 57;
yeccgoto(list, 293) ->
 57;
yeccgoto(list_comprehension, 14) ->
 58;
yeccgoto(list_comprehension, 18) ->
 58;
yeccgoto(list_comprehension, 20) ->
 58;
yeccgoto(list_comprehension, 24) ->
 58;
yeccgoto(list_comprehension, 25) ->
 58;
yeccgoto(list_comprehension, 28) ->
 58;
yeccgoto(list_comprehension, 31) ->
 58;
yeccgoto(list_comprehension, 33) ->
 58;
yeccgoto(list_comprehension, 54) ->
 58;
yeccgoto(list_comprehension, 60) ->
 58;
yeccgoto(list_comprehension, 61) ->
 126;
yeccgoto(list_comprehension, 63) ->
 58;
yeccgoto(list_comprehension, 68) ->
 58;
yeccgoto(list_comprehension, 72) ->
 58;
yeccgoto(list_comprehension, 77) ->
 58;
yeccgoto(list_comprehension, 78) ->
 58;
yeccgoto(list_comprehension, 79) ->
 58;
yeccgoto(list_comprehension, 85) ->
 58;
yeccgoto(list_comprehension, 89) ->
 58;
yeccgoto(list_comprehension, 96) ->
 58;
yeccgoto(list_comprehension, 100) ->
 58;
yeccgoto(list_comprehension, 104) ->
 58;
yeccgoto(list_comprehension, 108) ->
 58;
yeccgoto(list_comprehension, 115) ->
 58;
yeccgoto(list_comprehension, 117) ->
 58;
yeccgoto(list_comprehension, 125) ->
 58;
yeccgoto(list_comprehension, 129) ->
 58;
yeccgoto(list_comprehension, 134) ->
 58;
yeccgoto(list_comprehension, 136) ->
 58;
yeccgoto(list_comprehension, 143) ->
 58;
yeccgoto(list_comprehension, 161) ->
 58;
yeccgoto(list_comprehension, 173) ->
 58;
yeccgoto(list_comprehension, 178) ->
 58;
yeccgoto(list_comprehension, 182) ->
 58;
yeccgoto(list_comprehension, 191) ->
 58;
yeccgoto(list_comprehension, 198) ->
 58;
yeccgoto(list_comprehension, 203) ->
 58;
yeccgoto(list_comprehension, 216) ->
 58;
yeccgoto(list_comprehension, 218) ->
 58;
yeccgoto(list_comprehension, 220) ->
 58;
yeccgoto(list_comprehension, 222) ->
 58;
yeccgoto(list_comprehension, 223) ->
 58;
yeccgoto(list_comprehension, 226) ->
 58;
yeccgoto(list_comprehension, 230) ->
 58;
yeccgoto(list_comprehension, 237) ->
 58;
yeccgoto(list_comprehension, 240) ->
 58;
yeccgoto(list_comprehension, 250) ->
 58;
yeccgoto(list_comprehension, 252) ->
 58;
yeccgoto(list_comprehension, 266) ->
 58;
yeccgoto(list_comprehension, 275) ->
 58;
yeccgoto(list_comprehension, 293) ->
 58;
yeccgoto(list_op, 41) ->
 203;
yeccgoto(mult_op, 42) ->
 191;
yeccgoto(mult_op, 207) ->
 191;
yeccgoto(opt_bit_size_expr, 248) ->
 253;
yeccgoto(opt_bit_type_list, 253) ->
 255;
yeccgoto(prefix_op, 14) ->
 60;
yeccgoto(prefix_op, 18) ->
 60;
yeccgoto(prefix_op, 20) ->
 60;
yeccgoto(prefix_op, 24) ->
 250;
yeccgoto(prefix_op, 25) ->
 60;
yeccgoto(prefix_op, 28) ->
 60;
yeccgoto(prefix_op, 31) ->
 60;
yeccgoto(prefix_op, 33) ->
 60;
yeccgoto(prefix_op, 54) ->
 60;
yeccgoto(prefix_op, 63) ->
 60;
yeccgoto(prefix_op, 68) ->
 60;
yeccgoto(prefix_op, 72) ->
 60;
yeccgoto(prefix_op, 77) ->
 60;
yeccgoto(prefix_op, 78) ->
 60;
yeccgoto(prefix_op, 79) ->
 60;
yeccgoto(prefix_op, 85) ->
 60;
yeccgoto(prefix_op, 89) ->
 60;
yeccgoto(prefix_op, 96) ->
 60;
yeccgoto(prefix_op, 100) ->
 60;
yeccgoto(prefix_op, 104) ->
 60;
yeccgoto(prefix_op, 108) ->
 60;
yeccgoto(prefix_op, 115) ->
 60;
yeccgoto(prefix_op, 117) ->
 60;
yeccgoto(prefix_op, 125) ->
 60;
yeccgoto(prefix_op, 129) ->
 60;
yeccgoto(prefix_op, 134) ->
 60;
yeccgoto(prefix_op, 136) ->
 60;
yeccgoto(prefix_op, 143) ->
 60;
yeccgoto(prefix_op, 161) ->
 60;
yeccgoto(prefix_op, 173) ->
 60;
yeccgoto(prefix_op, 178) ->
 60;
yeccgoto(prefix_op, 191) ->
 60;
yeccgoto(prefix_op, 198) ->
 60;
yeccgoto(prefix_op, 203) ->
 60;
yeccgoto(prefix_op, 216) ->
 60;
yeccgoto(prefix_op, 218) ->
 60;
yeccgoto(prefix_op, 220) ->
 60;
yeccgoto(prefix_op, 222) ->
 60;
yeccgoto(prefix_op, 223) ->
 60;
yeccgoto(prefix_op, 226) ->
 60;
yeccgoto(prefix_op, 230) ->
 60;
yeccgoto(prefix_op, 237) ->
 60;
yeccgoto(prefix_op, 240) ->
 60;
yeccgoto(prefix_op, 266) ->
 250;
yeccgoto(prefix_op, 275) ->
 60;
yeccgoto(prefix_op, 293) ->
 60;
yeccgoto(query_expr, 14) ->
 62;
yeccgoto(query_expr, 18) ->
 62;
yeccgoto(query_expr, 20) ->
 62;
yeccgoto(query_expr, 24) ->
 62;
yeccgoto(query_expr, 25) ->
 62;
yeccgoto(query_expr, 28) ->
 62;
yeccgoto(query_expr, 31) ->
 62;
yeccgoto(query_expr, 33) ->
 62;
yeccgoto(query_expr, 54) ->
 62;
yeccgoto(query_expr, 60) ->
 62;
yeccgoto(query_expr, 63) ->
 62;
yeccgoto(query_expr, 68) ->
 62;
yeccgoto(query_expr, 72) ->
 62;
yeccgoto(query_expr, 77) ->
 62;
yeccgoto(query_expr, 78) ->
 62;
yeccgoto(query_expr, 79) ->
 62;
yeccgoto(query_expr, 85) ->
 62;
yeccgoto(query_expr, 89) ->
 62;
yeccgoto(query_expr, 96) ->
 62;
yeccgoto(query_expr, 100) ->
 62;
yeccgoto(query_expr, 104) ->
 62;
yeccgoto(query_expr, 108) ->
 62;
yeccgoto(query_expr, 115) ->
 62;
yeccgoto(query_expr, 117) ->
 62;
yeccgoto(query_expr, 125) ->
 62;
yeccgoto(query_expr, 129) ->
 62;
yeccgoto(query_expr, 134) ->
 62;
yeccgoto(query_expr, 136) ->
 62;
yeccgoto(query_expr, 143) ->
 62;
yeccgoto(query_expr, 161) ->
 62;
yeccgoto(query_expr, 173) ->
 62;
yeccgoto(query_expr, 178) ->
 62;
yeccgoto(query_expr, 182) ->
 62;
yeccgoto(query_expr, 191) ->
 62;
yeccgoto(query_expr, 198) ->
 62;
yeccgoto(query_expr, 203) ->
 62;
yeccgoto(query_expr, 216) ->
 62;
yeccgoto(query_expr, 218) ->
 62;
yeccgoto(query_expr, 220) ->
 62;
yeccgoto(query_expr, 222) ->
 62;
yeccgoto(query_expr, 223) ->
 62;
yeccgoto(query_expr, 226) ->
 62;
yeccgoto(query_expr, 230) ->
 62;
yeccgoto(query_expr, 237) ->
 62;
yeccgoto(query_expr, 240) ->
 62;
yeccgoto(query_expr, 250) ->
 62;
yeccgoto(query_expr, 252) ->
 62;
yeccgoto(query_expr, 266) ->
 62;
yeccgoto(query_expr, 275) ->
 62;
yeccgoto(query_expr, 293) ->
 62;
yeccgoto(receive_expr, 14) ->
 64;
yeccgoto(receive_expr, 18) ->
 64;
yeccgoto(receive_expr, 20) ->
 64;
yeccgoto(receive_expr, 24) ->
 64;
yeccgoto(receive_expr, 25) ->
 64;
yeccgoto(receive_expr, 28) ->
 64;
yeccgoto(receive_expr, 31) ->
 64;
yeccgoto(receive_expr, 33) ->
 64;
yeccgoto(receive_expr, 54) ->
 64;
yeccgoto(receive_expr, 60) ->
 64;
yeccgoto(receive_expr, 63) ->
 64;
yeccgoto(receive_expr, 68) ->
 64;
yeccgoto(receive_expr, 72) ->
 64;
yeccgoto(receive_expr, 77) ->
 64;
yeccgoto(receive_expr, 78) ->
 64;
yeccgoto(receive_expr, 79) ->
 64;
yeccgoto(receive_expr, 85) ->
 64;
yeccgoto(receive_expr, 89) ->
 64;
yeccgoto(receive_expr, 96) ->
 64;
yeccgoto(receive_expr, 100) ->
 64;
yeccgoto(receive_expr, 104) ->
 64;
yeccgoto(receive_expr, 108) ->
 64;
yeccgoto(receive_expr, 115) ->
 64;
yeccgoto(receive_expr, 117) ->
 64;
yeccgoto(receive_expr, 125) ->
 64;
yeccgoto(receive_expr, 129) ->
 64;
yeccgoto(receive_expr, 134) ->
 64;
yeccgoto(receive_expr, 136) ->
 64;
yeccgoto(receive_expr, 143) ->
 64;
yeccgoto(receive_expr, 161) ->
 64;
yeccgoto(receive_expr, 173) ->
 64;
yeccgoto(receive_expr, 178) ->
 64;
yeccgoto(receive_expr, 182) ->
 64;
yeccgoto(receive_expr, 191) ->
 64;
yeccgoto(receive_expr, 198) ->
 64;
yeccgoto(receive_expr, 203) ->
 64;
yeccgoto(receive_expr, 216) ->
 64;
yeccgoto(receive_expr, 218) ->
 64;
yeccgoto(receive_expr, 220) ->
 64;
yeccgoto(receive_expr, 222) ->
 64;
yeccgoto(receive_expr, 223) ->
 64;
yeccgoto(receive_expr, 226) ->
 64;
yeccgoto(receive_expr, 230) ->
 64;
yeccgoto(receive_expr, 237) ->
 64;
yeccgoto(receive_expr, 240) ->
 64;
yeccgoto(receive_expr, 250) ->
 64;
yeccgoto(receive_expr, 252) ->
 64;
yeccgoto(receive_expr, 266) ->
 64;
yeccgoto(receive_expr, 275) ->
 64;
yeccgoto(receive_expr, 293) ->
 64;
yeccgoto(record_expr, 14) ->
 65;
yeccgoto(record_expr, 18) ->
 65;
yeccgoto(record_expr, 20) ->
 65;
yeccgoto(record_expr, 25) ->
 65;
yeccgoto(record_expr, 28) ->
 65;
yeccgoto(record_expr, 31) ->
 65;
yeccgoto(record_expr, 33) ->
 65;
yeccgoto(record_expr, 54) ->
 65;
yeccgoto(record_expr, 60) ->
 65;
yeccgoto(record_expr, 63) ->
 65;
yeccgoto(record_expr, 68) ->
 65;
yeccgoto(record_expr, 72) ->
 65;
yeccgoto(record_expr, 77) ->
 65;
yeccgoto(record_expr, 78) ->
 65;
yeccgoto(record_expr, 79) ->
 65;
yeccgoto(record_expr, 85) ->
 65;
yeccgoto(record_expr, 89) ->
 65;
yeccgoto(record_expr, 96) ->
 65;
yeccgoto(record_expr, 100) ->
 65;
yeccgoto(record_expr, 104) ->
 65;
yeccgoto(record_expr, 108) ->
 65;
yeccgoto(record_expr, 115) ->
 65;
yeccgoto(record_expr, 117) ->
 65;
yeccgoto(record_expr, 125) ->
 65;
yeccgoto(record_expr, 129) ->
 65;
yeccgoto(record_expr, 134) ->
 65;
yeccgoto(record_expr, 136) ->
 65;
yeccgoto(record_expr, 143) ->
 65;
yeccgoto(record_expr, 161) ->
 65;
yeccgoto(record_expr, 173) ->
 65;
yeccgoto(record_expr, 178) ->
 65;
yeccgoto(record_expr, 191) ->
 65;
yeccgoto(record_expr, 198) ->
 65;
yeccgoto(record_expr, 203) ->
 65;
yeccgoto(record_expr, 216) ->
 65;
yeccgoto(record_expr, 218) ->
 65;
yeccgoto(record_expr, 220) ->
 65;
yeccgoto(record_expr, 222) ->
 65;
yeccgoto(record_expr, 223) ->
 65;
yeccgoto(record_expr, 226) ->
 65;
yeccgoto(record_expr, 230) ->
 65;
yeccgoto(record_expr, 237) ->
 65;
yeccgoto(record_expr, 240) ->
 65;
yeccgoto(record_expr, 275) ->
 65;
yeccgoto(record_expr, 293) ->
 65;
yeccgoto(record_field, 167) ->
 169;
yeccgoto(record_field, 176) ->
 169;
yeccgoto(record_fields, 167) ->
 170;
yeccgoto(record_fields, 176) ->
 177;
yeccgoto(record_tuple, 164) ->
 166;
yeccgoto(record_tuple, 271) ->
 273;
yeccgoto(rule, 0) ->
 8;
yeccgoto(rule_body, 17) ->
 276;
yeccgoto(rule_body, 291) ->
 276;
yeccgoto(rule_clause, 0) ->
 9;
yeccgoto(rule_clause, 11) ->
 9;
yeccgoto(rule_clauses, 0) ->
 10;
yeccgoto(rule_clauses, 11) ->
 13;
yeccgoto(strings, 14) ->
 67;
yeccgoto(strings, 18) ->
 67;
yeccgoto(strings, 20) ->
 67;
yeccgoto(strings, 24) ->
 67;
yeccgoto(strings, 25) ->
 67;
yeccgoto(strings, 28) ->
 67;
yeccgoto(strings, 31) ->
 67;
yeccgoto(strings, 33) ->
 67;
yeccgoto(strings, 54) ->
 67;
yeccgoto(strings, 60) ->
 67;
yeccgoto(strings, 63) ->
 67;
yeccgoto(strings, 66) ->
 114;
yeccgoto(strings, 68) ->
 67;
yeccgoto(strings, 72) ->
 67;
yeccgoto(strings, 77) ->
 67;
yeccgoto(strings, 78) ->
 67;
yeccgoto(strings, 79) ->
 67;
yeccgoto(strings, 85) ->
 67;
yeccgoto(strings, 89) ->
 67;
yeccgoto(strings, 96) ->
 67;
yeccgoto(strings, 100) ->
 67;
yeccgoto(strings, 104) ->
 67;
yeccgoto(strings, 108) ->
 67;
yeccgoto(strings, 115) ->
 67;
yeccgoto(strings, 117) ->
 67;
yeccgoto(strings, 125) ->
 67;
yeccgoto(strings, 129) ->
 67;
yeccgoto(strings, 134) ->
 67;
yeccgoto(strings, 136) ->
 67;
yeccgoto(strings, 143) ->
 67;
yeccgoto(strings, 161) ->
 67;
yeccgoto(strings, 173) ->
 67;
yeccgoto(strings, 178) ->
 67;
yeccgoto(strings, 182) ->
 67;
yeccgoto(strings, 191) ->
 67;
yeccgoto(strings, 198) ->
 67;
yeccgoto(strings, 203) ->
 67;
yeccgoto(strings, 216) ->
 67;
yeccgoto(strings, 218) ->
 67;
yeccgoto(strings, 220) ->
 67;
yeccgoto(strings, 222) ->
 67;
yeccgoto(strings, 223) ->
 67;
yeccgoto(strings, 226) ->
 67;
yeccgoto(strings, 230) ->
 67;
yeccgoto(strings, 237) ->
 67;
yeccgoto(strings, 240) ->
 67;
yeccgoto(strings, 250) ->
 67;
yeccgoto(strings, 252) ->
 67;
yeccgoto(strings, 266) ->
 67;
yeccgoto(strings, 275) ->
 67;
yeccgoto(strings, 293) ->
 67;
yeccgoto(tail, 236) ->
 239;
yeccgoto(tail, 243) ->
 244;
yeccgoto(try_catch, 76) ->
 80;
yeccgoto(try_catch, 82) ->
 88;
yeccgoto(try_clause, 78) ->
 93;
yeccgoto(try_clause, 104) ->
 93;
yeccgoto(try_clauses, 78) ->
 94;
yeccgoto(try_clauses, 104) ->
 105;
yeccgoto(try_expr, 14) ->
 69;
yeccgoto(try_expr, 18) ->
 69;
yeccgoto(try_expr, 20) ->
 69;
yeccgoto(try_expr, 24) ->
 69;
yeccgoto(try_expr, 25) ->
 69;
yeccgoto(try_expr, 28) ->
 69;
yeccgoto(try_expr, 31) ->
 69;
yeccgoto(try_expr, 33) ->
 69;
yeccgoto(try_expr, 54) ->
 69;
yeccgoto(try_expr, 60) ->
 69;
yeccgoto(try_expr, 63) ->
 69;
yeccgoto(try_expr, 68) ->
 69;
yeccgoto(try_expr, 72) ->
 69;
yeccgoto(try_expr, 77) ->
 69;
yeccgoto(try_expr, 78) ->
 69;
yeccgoto(try_expr, 79) ->
 69;
yeccgoto(try_expr, 85) ->
 69;
yeccgoto(try_expr, 89) ->
 69;
yeccgoto(try_expr, 96) ->
 69;
yeccgoto(try_expr, 100) ->
 69;
yeccgoto(try_expr, 104) ->
 69;
yeccgoto(try_expr, 108) ->
 69;
yeccgoto(try_expr, 115) ->
 69;
yeccgoto(try_expr, 117) ->
 69;
yeccgoto(try_expr, 125) ->
 69;
yeccgoto(try_expr, 129) ->
 69;
yeccgoto(try_expr, 134) ->
 69;
yeccgoto(try_expr, 136) ->
 69;
yeccgoto(try_expr, 143) ->
 69;
yeccgoto(try_expr, 161) ->
 69;
yeccgoto(try_expr, 173) ->
 69;
yeccgoto(try_expr, 178) ->
 69;
yeccgoto(try_expr, 182) ->
 69;
yeccgoto(try_expr, 191) ->
 69;
yeccgoto(try_expr, 198) ->
 69;
yeccgoto(try_expr, 203) ->
 69;
yeccgoto(try_expr, 216) ->
 69;
yeccgoto(try_expr, 218) ->
 69;
yeccgoto(try_expr, 220) ->
 69;
yeccgoto(try_expr, 222) ->
 69;
yeccgoto(try_expr, 223) ->
 69;
yeccgoto(try_expr, 226) ->
 69;
yeccgoto(try_expr, 230) ->
 69;
yeccgoto(try_expr, 237) ->
 69;
yeccgoto(try_expr, 240) ->
 69;
yeccgoto(try_expr, 250) ->
 69;
yeccgoto(try_expr, 252) ->
 69;
yeccgoto(try_expr, 266) ->
 69;
yeccgoto(try_expr, 275) ->
 69;
yeccgoto(try_expr, 293) ->
 69;
yeccgoto(tuple, 14) ->
 70;
yeccgoto(tuple, 18) ->
 70;
yeccgoto(tuple, 20) ->
 70;
yeccgoto(tuple, 24) ->
 70;
yeccgoto(tuple, 25) ->
 70;
yeccgoto(tuple, 28) ->
 70;
yeccgoto(tuple, 31) ->
 70;
yeccgoto(tuple, 33) ->
 70;
yeccgoto(tuple, 54) ->
 70;
yeccgoto(tuple, 60) ->
 70;
yeccgoto(tuple, 63) ->
 70;
yeccgoto(tuple, 68) ->
 70;
yeccgoto(tuple, 72) ->
 70;
yeccgoto(tuple, 77) ->
 70;
yeccgoto(tuple, 78) ->
 70;
yeccgoto(tuple, 79) ->
 70;
yeccgoto(tuple, 85) ->
 70;
yeccgoto(tuple, 89) ->
 70;
yeccgoto(tuple, 96) ->
 70;
yeccgoto(tuple, 100) ->
 70;
yeccgoto(tuple, 104) ->
 70;
yeccgoto(tuple, 108) ->
 70;
yeccgoto(tuple, 115) ->
 70;
yeccgoto(tuple, 117) ->
 70;
yeccgoto(tuple, 125) ->
 70;
yeccgoto(tuple, 129) ->
 70;
yeccgoto(tuple, 134) ->
 70;
yeccgoto(tuple, 136) ->
 70;
yeccgoto(tuple, 143) ->
 70;
yeccgoto(tuple, 161) ->
 70;
yeccgoto(tuple, 173) ->
 70;
yeccgoto(tuple, 178) ->
 70;
yeccgoto(tuple, 182) ->
 70;
yeccgoto(tuple, 191) ->
 70;
yeccgoto(tuple, 198) ->
 70;
yeccgoto(tuple, 203) ->
 70;
yeccgoto(tuple, 216) ->
 70;
yeccgoto(tuple, 218) ->
 70;
yeccgoto(tuple, 220) ->
 70;
yeccgoto(tuple, 222) ->
 70;
yeccgoto(tuple, 223) ->
 70;
yeccgoto(tuple, 226) ->
 70;
yeccgoto(tuple, 230) ->
 70;
yeccgoto(tuple, 237) ->
 70;
yeccgoto(tuple, 240) ->
 70;
yeccgoto(tuple, 250) ->
 70;
yeccgoto(tuple, 252) ->
 70;
yeccgoto(tuple, 266) ->
 70;
yeccgoto(tuple, 275) ->
 70;
yeccgoto(tuple, 293) ->
 70;
yeccgoto(__Symbol, __State) ->
 exit({__Symbol, __State, missing_in_goto_table}).


