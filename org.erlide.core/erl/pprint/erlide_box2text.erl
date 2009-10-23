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
-module(erlide_box2text).

-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Popts = [POpt]
%% POpt = {width, N} % page width

process(X, POpts) ->
    process(X, {1, 1}, POpts).

process({string, Data}, {Col, Row}, _POpts) when is_list(Data) ->
    {{Data, {Col, Row}}, {Col+length(Data), Row}};
process({h, Opts, Data0}, {Col, Row}, POpts) ->
    HS = hs(Opts),
    Data = lists:flatten(Data0),
    {Res, {NCol, NRow}} = lists:mapfoldl(fun(E, {C, R}) -> 
						 {Res1, {C1, R1}} = process(E, {C, R}, POpts),
						 {Res1, {C1+HS, R1}}
					 end, 
					 {Col, Row}, 
					 Data),
    {lists:flatten(Res), {NCol-HS, NRow}};
process({v, _Opts, []}, {Col, Row}, _POpts) ->
    {"", {Col, Row-1}};
process({v, Opts, Data0}, {Col, Row}, POpts) ->
    VS = vs(Opts),
    IS = is(Opts),
    Data = lists:flatten(Data0),
    case Data of
	[] ->
	    {"", {Col, Row-1}};
	_ ->
	    {Res0, {C0, R0}} = process(hd(Data), {Col, Row}, POpts),
	    case tl(Data) of
		[] ->
		    {[Res0], {C0, R0}};
		_ ->
		    [Last | Mid] = lists:reverse(tl(Data)),
		    {Res, {NCol, NRow}} = lists:mapfoldl(fun(E, {C, R}) -> 
								 {Res1, {_C1, R1}} = process(E, {C, R}, POpts),
								 {Res1, {C, R1+VS}}
							 end, 
							 {Col+IS, R0+VS}, 
							 lists:reverse(Mid)),
		    {ResN, {CN, RN}} = process(Last, {NCol, NRow}, POpts),
		    {lists:flatten([Res0|Res]++[ResN]), {CN, RN}}
	    end
    end;
process({hv, Opts, Data0}, {Col, Row}, POpts) ->
    Data = lists:flatten(Data0),
    layout_hv(Opts, Data, {Col, Row}, POpts);
process({hov, Opts, Data}, {Col, Row}, POpts) ->
    R1 = process({h, Opts, Data}, {Col, Row}, POpts),
    {_, {NCol, _}} = R1,
    case NCol < getopt(POpts, width) of
	true ->
	    R1;
	_ ->
	    process({v, Opts, Data}, {Col, Row}, POpts)
    end;
process({i, Opts, Data}, {Col, Row}, POpts) ->
    IS = is(Opts),
    process(Data, {Col+IS, Row}, POpts);
process({wd, Data}, {Col, Row}, POpts) ->
    {Content, _} = process(Data, {Col, Row}, POpts),
    LastCol = getWidth(lists:flatten(Content)),
    {"", {LastCol, Row}};
process({table, AOpts, Opts, Data}, {Col, Row}, POpts) ->
    process_table(AOpts, Opts, Data, {Col, Row}, POpts);
process({keyword, Data}, {Col, Row}, POpts) ->
    process(Data, {Col, Row}, POpts);
process({variable, Data}, {Col, Row}, POpts) ->
    process(Data, {Col, Row}, POpts);
process({number, Data}, {Col, Row}, POpts) ->
    process(Data, {Col, Row}, POpts);
process({bracket, Data}, {Col, Row}, POpts) ->
    process(Data, {Col, Row}, POpts);
process({comment, Data}, {Col, Row}, POpts) ->
    process(Data, {Col, Row}, POpts);
process({literal, ""}, {Col, Row}, _POpts)  ->
    {{"", {Col, Row}}, {Col, Row}};
process({literal, X}, {Col, Row}, _POpts) when is_list(X); is_atom(X) ->
    XX = lists:flatten(io_lib:format("~s", [X])),
    {{XX, {Col, Row}}, {Col+length(XX), Row}};
process({literal, X}, {Col, Row}, _POpts) when is_integer(X) ->
    XX = lists:flatten(io_lib:format("~p", [X])),
    {{XX, {Col, Row}}, {Col+length(XX), Row}};
process({qliteral, ""}, {Col, Row}, _POpts)  ->
    {{"\"\"", {Col, Row}}, {Col+2, Row}};
process({qliteral, X}, {Col, Row}, _POpts) when is_integer(X); is_list(X); is_atom(X) ->
    XX = lists:flatten(io_lib:format("~p", [X])),
    {{XX, {Col, Row}}, {Col+length(XX), Row}};
process({quoted, X}, {Col, Row}, POpts) ->
    process(quote(X), {Col, Row}, POpts);
process(X, {Col, Row}, _POpts) ->
    XX = io_lib:format("!@?~w?@!", [X]),
    {{XX, {1, Row+1}}, {Col, Row+2}}.

%% this isn't working as advertised, yet!
process_table(AOpts, Opts, Data, {Col, Row}, POpts) when is_list(Data) ->
    R = [process_table(AOpts, Opts, X, {Col, Row}, POpts) || X<-Data],
    R1 = [D || {D, _}<-R],
    Rx = fix_pos(R1, Row, []),
    {lists:flatten(Rx), {Col, Row+length(Rx)}};
process_table(_AOpts, Opts, {row, Data}, {Col, Row}, POpts) ->
    %% simulate a row
    process({h, Opts, Data}, {Col, Row}, POpts).

fix_pos([], _, Res) ->
    lists:reverse(Res);
fix_pos([H|T], R, Res) ->
    fix_pos(T, R+1, [fix_pos1(H, R, [])|Res]).
    
fix_pos1({D,{C, _}}, R, _Res) ->
    {D,{C,R}};
fix_pos1([], _, Res) ->
    lists:reverse(Res);
fix_pos1([{D,{C, _}}|T], R, Res) ->
    fix_pos1(T, R, [{D,{C,R}}|Res]).
    
diff_fmt({S1, {C1, R1}}, {_S2, {C2, R2}}) ->
    E1 = C1 + length(S1),
    case R2 == R1 of
	true ->
	    spc(C2-E1);
	false ->
	    nl(R2-R1)++spc(C2-1)
    end.

format(L) when is_list(L) ->    
    format(L, []);
format(E) ->
    format([E]).

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


test() ->
    Res = boxp:string("v[\"see\" "
		      "h[\"too\" h[\"in\" h[kw[\"the\"] \"deep\"] \"space\"] \"...\" ]"
		      "h[\"too\" h[\"in\" \"the\" \"deep\" \"space\"] \"...\" ]"
		      " \"fly\"]"),
    {ok, Z} = Res,
    try
	io:format("~p~n----------------~n", [Z]),

	%%dbg:tracer(),    dbg:p(self(), [c]),    
	%%dbg:tpl({?MODULE, format, '_'}, [{'_',[],[{return_trace}]}]),
	%%dbg:tpl({?MODULE, myhv, '_'}, [{'_',[],[{return_trace}]}]),

	{D1, _} = process(Z, [{width, 10}]),
	io:format("~p~n================~n", [D1]),
	io:format("~s~n123456789012345678901234567890~n", [format(D1)]),

	{D2, _} = process(Z, [{width, 20}]),
	%%io:format("~p~n================~n", [D2]),
	io:format("~s~n123456789012345678901234567890~n", [format(D2)]),

	{D3, _} = process(Z, [{width, 30}]),
	%%io:format("~p~n================~n", [D3]),
	io:format("~s~n123456789012345678901234567890~n", [format(D3)]),

	ok

    catch
	throw:X -> {error, X}
    end.

getopt(L, width) ->
    case lists:keysearch(width, 1, L) of
	{value, {width, N}} ->
	    N;
	_ ->
	    80
    end;
getopt(L, N) ->
    case lists:keysearch(N, 1, L) of
	{value, {N, N}} ->
	    N;
	_ ->
	    0
    end.

layout_hv(Opts, Data, {Col, Row}, POpts) ->
    Res0 = lists:mapfoldl(fun myhv/2, 
			  {Col, Row, {Opts, POpts, Col}}, 
			  Data),
    {Res, {NCol, NRow, _}} = Res0,
    
    {lists:flatten(Res), {NCol, NRow}}.

myhv(E, {C, R, {Opts, POpts, Col}}) -> 
    HS = hs(Opts),
    W = getopt(POpts, width),
    {Res1, {C1, R1}} = process(E, {C, R}, POpts),
    case C1 < W of
	true ->
	    {Res1, {C1+HS, R1, {Opts, POpts, Col}}};
	false ->
	    VS = vs(Opts),
	    {Res2, {C2, R2}} = process(E, {Col, R+VS}, POpts),
	    {Res2, {C2+HS, R2, {Opts, POpts, Col}}}
    end.

%% TODO	
getWidth(_Content) ->
    10.

quote({literal, X}) ->
    {qliteral, X};
quote({A,B,C}) ->
    {A,B,[quote(X)||X<-C]};
quote({A,C}) ->
    {A,[quote(X)||X<-C]};
quote(X) ->
   X. 
