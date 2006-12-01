-module(erlide_pprint).

-compile(export_all).

h(Els) ->
    h([], Els).

h(Opts, Els) ->
    {h, Opts, Els}.

v(Els) ->
    v([], Els).

v(Opts, Els) ->
    {v, Opts, Els}.

i(Els) ->
    {i, [], Els}.

i(Opts, Els) ->
    {i, Opts, Els}.

hv(Els) ->
    hv([], Els).

hv(Opts, Els) ->
    {hv, Opts, Els}.

a(Els) ->
    a([], Els).

a(Opts, Els) ->
    {a, Opts, Els}.

r(Els) ->
    {r, [], Els}.

r(Opts, Els) ->
    {r, Opts, Els}.

alt(Opts, Els) ->
    {alt, Opts, Els}.

kw(X) ->
    {kw, X}.

id(X) ->
    {id, X}.

str(X) ->
    {str, X}.

nr(X) ->
    {nr, integer_to_list(X)}.

var(X) ->
    {var, X}.

f(X) ->
    {f, X}.

lbl(X) ->
    {lbl, X}.

ref(X) ->
    {ref, X}.

c(X) ->
    {c, X}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

opt(X) ->
    X.

iter(X) ->
    X.

iter_star(X) ->
    X.

iter_sep(X) ->
    X.

iter_star_sep(X) ->
    X.

alt(X) ->
    X.

seq(X) ->
    X.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process(X) ->
    process(X, {1, 1}).

process(Data, {Col, Row}) when is_list(Data) ->
    {{Data, {Col, Row}}, {Col+length(Data), Row}};
process({str, Data}, {Col, Row}) when is_list(Data) ->
    S = "\""++Data++"\"",
    {{S, {Col, Row}}, {Col+length(S), Row}};
process({_, Data}, {Col, Row}) when is_list(Data) ->
    {{Data, {Col, Row}}, {Col+length(Data), Row}};
process({h, Opts, Data}, {Col, Row}) ->
    HS = hs(Opts),
    {Res, {NCol, NRow}} = lists:mapfoldl(fun(E, {C, R}) -> 
						 {Res1, {C1, R1}} = process(E, {C, R}),
						 {Res1, {C1+HS, R1}}
					 end, 
					 {Col, Row}, 
					 Data),
    {lists:flatten(Res), {NCol, NRow}};
process({v, _Opts, []}, {Col, Row}) ->
    {"", {Col, Row-1}};
process({v, Opts, Data}, {Col, Row}) ->
    VS = vs(Opts),
    IS = is(Opts),
    {Res0, {C0, R0}} = process(hd(Data), {Col, Row}),
    case tl(Data) of
	[] ->
	    {Res0, {C0, R0}};
	_ ->
	    [Last | Mid] = lists:reverse(tl(Data)),
	    {Res, {NCol, NRow}} = lists:mapfoldl(fun(E, {C, R}) -> 
							 {Res1, {_C1, R1}} = process(E, {C, R}),
							 {Res1, {C, R1+VS}}
						 end, 
						 {Col+IS, R0+VS}, 
						 lists:reverse(Mid)),
	    {ResN, {CN, RN}} = process(Last, {NCol, NRow}),
	    {lists:flatten([Res0|Res]++[ResN]), {CN, RN}}
    end;
process({a, _Opts, Data}, {Col, Row}) ->
    {{Data, {Col, Row}}, {Col, Row}};
process(X, {Col, Row}) ->
    XX = io_lib:format("[?~w?]", [X]),
    {{XX, {Col, Row}}, {Col+length(XX), Row}}.

diff_fmt({S1, {C1, R1}}, {_S2, {C2, R2}}) ->
    E1 = C1 + length(S1),
    case R2 == R1 of
	true ->
	    spc(C2-E1);
	false ->
	    nl(R2-R1)++spc(C2-1)
    end.

format(L) ->    
    format(L, []).

format([], R) ->    
    lists:flatten(lists:reverse(R));
format([{H,_}], R) ->
    lists:reverse([H|R]);
format([{S,_}=H1,H2|T], R) ->
    format([H2|T], [diff_fmt(H1, H2),S|R]).


getopt(Key, L, Def) ->
    case lists:keysearch(Key, 1, L) of
	{value, {Key, V}} ->
	    V;
	_ ->
	    Def
    end.

spc(N) ->
    lists:duplicate(N, $\s).

nl(N) ->    
    lists:duplicate(N, $\n).

hs(Opts) ->
    getopt(hs, Opts, 1).

vs(Opts) ->
    getopt(vs, Opts, 1).

is(Opts) ->
    getopt(is, Opts, 0).


