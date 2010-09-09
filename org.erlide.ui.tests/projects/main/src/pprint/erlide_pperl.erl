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
-module(erlide_pperl).

-compile(export_all).

-define(P(C,X), io:format("~s ~p~n", 
			  [C, X])).

-define(PS(C,X), io:format("~s |~n~s~n|~n", 
			   [C, X])).

load(File, Module) ->
    {ok, S} = file:read_file(File),
    Text = binary_to_list(S),
    {ok, L, _} = erl_scan:string(Text),
    Rules = split(L),
    Res = [parse(X) || X<-Rules],
    Clauses = sort([format(X) || X<-Res]),
    FmtList = fmt_list_code(),
    Code = [{attribute,0,module,Module},
	    {attribute,0,export,[{fmt,1},{fmt,2}]},
	    {function,0,
	     fmt,
	     1,
	     [{clause,0,
	       [{var,0,'X'}],
	       [],
	       [{call,
		 0,
		 {atom,0,fmt},
		 [{atom,0,'$none'}, {var,0,'X'}]
		}
	       ]
	      }]
	    },
	    {function,0,
	     fmt,2,
	     Clauses ++
	     [{clause,0,
	       [{var,0,'_'},{var,0,'X'}],
	       [],
	       [{tuple,0,
		 [{atom,0,literal},
		  {var,0,'X'}]
		}
	       ]
	      }
	     ]
	    }
	    |
	    FmtList
	   ],
    TxtCode = [erl_pp:form(X)||X<-Code],
    F = atom_to_list(Module)++".erl",
    file:write_file(F, list_to_binary(TxtCode)),
    {ok, Module, Bin} = compile:forms(Code),
    code:load_binary(Module, F, Bin).

pp(M, E) ->
    {T, _} = box2text:process(M:fmt(E), [{width, 20}]),
    ?P("!! ", T),
    box2text:format(T).

t() ->
    load("test.box", test),
    tt(),
    ok.

tt() ->
    %%?P(" >", test:fmt([alfa])),
    %%?PS("+>", pp(test:fmt([alfa]))),
    %%?P(" >", test:fmt([[alfa, beta], gamma])),
    %%?PS("+>", pp(test:fmt([[alfa, beta,pi,ro], gamma,delta,fi]))),
    %%?P(" >", test:fmt(q, [[alfa, beta,ro], [gamma, delta, mu], alef, zeta])),
%%    {ok, [V]} = g("[a, {b, \"ca\", [82,83]}]."),
%%    ?P("--",V),
%%    ?P(" >", test:fmt(V)),
%%    ?PS("+>", pp(test, V)),

    W = {block, [a,"",'Db',"Ccar","dd", 3], [a,"",'Db',"Ccar","dd", 3]},
    %%{table, [{row, hello, world, hi}, {row, hello, world, hi}, {row, hello, world, hi}]},%
    %%{demo, "q", "q", 'Q', 'Q'},
    ?P("--",W),
    ?P(" >", test:fmt(W)),
    ?PS("+>", pp(test, W)),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%

split(L) ->
    split(L, [], []).

split([], _, Rs) ->
    lists:reverse(Rs);
split([{dot, _}|T], R, Rs)->
    split(T, [], [lists:reverse(R)|Rs]);
split([H|T], R, Rs) ->
    split(T, [H|R], Rs).


split2(L) ->
    Res = split2(L, [], []),
    case Res of
	[P, B] ->
	    {{var,0,'_'}, P, B};
	[[N], P, B] ->
	    {N, P, B}
    end.

split2([], R, Rs) ->
    lists:reverse([lists:reverse(R)|Rs]);
split2([{':',_}, {':',_}|T], R, Rs)->
    split2(T, [], [lists:reverse(R)|Rs]);
split2([H|T], R, Rs) ->
    split2(T, [H|R], Rs).


parse(L) ->
    {Name, Pat, Box} = split2(L),
    {ok, [Pat1]} = erl_parse:parse_exprs(Pat++[{dot,0}]),
    {ok, Box1} = boxp:parse(Box),
    {Name, Pat1, Box1}.


format({Name, Pat, Box}) ->
    {clause, 0, [Name,Pat], [], [prepare(erl_parse:abstract(Box))]}.

prepare({tuple,_,[{atom,_,list},{atom,0,L},Box,Sep]}) ->
    {call,0,{atom,0,fmt_list},[{var,0,L},{atom,0,L},Box,Sep]};
prepare({tuple,_,[{atom,_,boxvar},{tuple,_,[{atom,_,V}, {atom,_,R}]}, {atom,_,quoted}]}) ->
    {tuple,0,[{atom,0,quoted},{call,0,{atom,0,fmt},[{atom,0,R},{var,0,V}]}]};
prepare({tuple,_,[{atom,_,boxvar},{tuple,_,[{atom,_,V}, {atom,_,R}]}]}) ->
    {call,0,{atom,0,fmt},[{atom,0,R},{var,0,V}]};
prepare({tuple,_,[{atom,_,splicevar},{tuple,_,[{atom,_,V}, {atom,_,R}]}]}) ->
    {xcall,0,{atom,0,fmt},[{atom,0,R},{var,0,V}]};
prepare({cons, _, _, _}=L) ->
    Lx = tolist(L),
    Rx = prepare(Lx),
    case splice(Rx) of
	no ->
	    mklist(Rx);
	{yes, Res} ->
	    Resx = mksum(Res),
	    Resx
    end;
prepare({N, _, R}) ->
    {N, 0, prepare(R)};
prepare({N, _, R1, R2}) ->
    {N, 0, prepare(R1), prepare(R2)};
prepare(L) when is_list(L) ->
    case io_lib:deep_char_list(L) of
	true ->
	    L;
	false ->
	    [prepare(X) || X <-L]
    end;
prepare(X) ->
    X.

splice(L) ->
    Res = splice(L, [], []),
    case (length(Res)==1) and (is_list(hd(Res))) of
	true ->
	    no;
	_ ->
	    {yes, Res}
    end.

tolist({nil,0}) ->
    [];
tolist({cons,0,H,T}) ->
    [H|tolist(T)];
tolist(X) ->
    X.

splice([], [], Res) ->
    lists:reverse(Res);
splice([], R, Res) ->
    lists:reverse([lists:reverse(R)|Res]);
splice([{xcall,A,B,C}|T], [], Res) ->
    Call = {call,0,{remote,0,{atom,0,?MODULE},{atom,0,extract}}, [{call,A,B,C}]},
    splice(T, [], [Call|Res]);
splice([{xcall,A,B,C}|T], R, Res) ->
    Call = {call,0,{remote,0,{atom,0,?MODULE},{atom,0,extract}}, [{call,A,B,C}]},
    splice(T, [], [Call,lists:reverse(R)|Res]);
splice([H|T], R, Res) ->
    splice(T, [H|R], Res).

mksum(L) ->
    Lx = [mklist(X) || X<-L],
    {call,0,{remote,0,{atom,0,lists},{atom,0,append}}, [mklist(Lx)]}.

mklist([]) ->
    {nil,0};
mklist([H|T]) ->
    {cons, 0, H, mklist(T)};
mklist(X) ->
    X.

g(S) ->
    {ok, T, _} = erl_scan:string(S),
    AT = erl_parse:parse_exprs(T),
    AT.

%% group by namespace (in alphabetical order), otherwise keep original order (important!)
sort(L) ->
    Fun = fun({clause,_,[{atom,_,_},_],_,_}, {clause,_,[{var,_,'_'},_],_,_}) ->
		  true;
	     ({clause,_,[{atom,_,A},_],_,_}, {clause,_,[{atom,_,B},_],_,_}) ->
		  A =< B;
	     ({clause,_,[{var,_,_},_],_,_}, {clause,_,[{var,_,_},_],_,_}) ->
		  true;
	     (_,_) ->
		  false
	  end,
    lists:sort(Fun, L).

extract(L) when is_list(L) ->
    L;
extract({_,_,L}) when is_list(L) ->
    L;
extract(X) ->
    X.


parse_code(Code) ->
    {ok, T, _} = erl_scan:string(Code),
    {ok, F} = erl_parse:parse_form(T),
    F.

fmt_list_code() ->
    A = parse_code(
	  "fmt_list([],_,_Tpl,_Sep) ->"
	  "    [];"
	  "fmt_list([H],N,Tpl,_Sep) ->"
	  "    [repl(H, N, Tpl)];"
	  "fmt_list([H|T],N,Tpl,Sep) ->"
	  "    [{h,[{hs,0}],[repl(H,N,Tpl),Sep]} | fmt_list(T,N,Tpl,Sep)]."
	 ),
    B = parse_code(
	  "repl(Var, Name, {boxvar, {Name, NS}}) ->"
	  "    fmt(NS, Var);"
	  "repl(Var, Name, {boxvar, {Name, NS}, quoted}) ->"
	  "    {quoted, fmt(NS, Var)};"
	  "repl(Var, Name, {A,B,C}) when is_list(C) ->"
	  "    {A, B, [repl(Var, Name, X) || X<-C]};"
	  "repl(Var, Name, {A,C}) when is_list(C) ->"
	  "    {A, [repl(Var, Name, X) || X<-C]};"
	  "repl(_Var, _Name, Tpl) ->"
	  "    Tpl."
	 ),
    [A,B].

