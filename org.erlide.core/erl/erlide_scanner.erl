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
-module(erlide_scanner).

-compile(export_all).

-include_lib("stdlib/include/ms_transform.hrl").

-export([initialScan/4, isScanned/1]).
 
%%-include_lib("eunit/include/eunit.hrl").
%%-include_lib("eunit/include/eunit_test.hrl").

%%-define(DEBUG, 1).

-include("erlide.hrl").
-include("erlide_scanner.hrl").

-define(SERVER, erlide_scanner).

initialScan(ScannerName, ModuleFileName, InitialText, StateDir) ->
    ?D({ScannerName, ModuleFileName, StateDir}),
    try
    	case isScanned(ScannerName) of
            true ->
                ok;
            false ->
                CacheFileName = filename:join(StateDir, atom_to_list(ScannerName) ++ ".scan"),
                ?D(CacheFileName),
                RenewFun = fun(_F) -> do_scan(ScannerName, InitialText) end,
        		CacheFun = fun(B) -> do_add_cached(ScannerName, B) end,
				erlide_util:check_cached(ModuleFileName, CacheFileName, RenewFun, CacheFun)
%%                 do_scan(ScannerName, InitialText)
        end,
        do_getTokens(ScannerName)
    catch
        error:Reason ->
            {error, Reason}
    end.

do_scan(ScannerName, InitialText) ->
    ?D(ScannerName),
    create(ScannerName),
    ?D(b),
    insertText(ScannerName, 1, InitialText),
    ?D(i),
    ets:tab2list(ScannerName).

do_add_cached(ScannerName, List) ->
    create(ScannerName),
    ets:insert(ScannerName, List).

spawn_owner() ->
    case whereis(?SERVER) of
      undefined ->
            Pid = spawn(fun loop/0),
            erlang:register(?SERVER, Pid);
        _ ->
            ok
    end.

loop() ->
    receive
    	stop ->
            ok;
        {new, Module} ->
            _T = ets:new(Module, [ordered_set, public, named_table, {keypos, #token.offset}]),
            ets:insert(Module, mktoken({eof, {{1, 1}, 0}}, 0, 0)),
            loop();
        _ ->
            loop()
    end.


create(Module) ->
    spawn_owner(),
    ?SERVER ! {new, Module},
    receive after 1 -> ok end,
  ok.

destroy(Module) ->
    ?D(Module),
    catch ets:delete(Module),
    ok.

do_getWsTokens(Module) ->
    {ok, getWsTokens(Module)}.

getWsTokens(Module) ->
        ets:tab2list(Module).

do_getTokens(Module) ->
  {ok, getTokens(Module)}.

getTokens(Module) ->
    filter_ws(getWsTokens(Module)).

do_getTokenWindow(Module, Offset, Window) ->
  {ok, getTokenWindow(Module, Offset, Window)}.

getTokenWindow(Module, Offset, Window) ->
    T = getTokenAt(Module, Offset),
    Tp = getPrevTokenWs(Module, T, Window),
    Tn = getNextTokenWs(Module, T, Window),
    ?Debug({tp, Tp}),
    ?Debug({t, T}),
        ?Debug({tn, Tn}),
        clean(lists:reverse(Tp)++[T|Tn]).

getPrevTokenWs(Module, Token) ->
    Ofs = Token#token.offset-1,
    R=getTokenAt(Module, Ofs),
    R.

getNextTokenWs(Module, Token) ->
    Ofs = Token#token.offset+Token#token.length,
    getTokenAt(Module, Ofs).

getPrevTokenWs(_Module, _Token, 0) ->
    [];
getPrevTokenWs(Module, Token, N) ->
    T = getPrevTokenWs(Module, Token),
    [T | getPrevTokenWs(Module, T, N-1)].

getPrevToken(Module, Token) ->
    case getPrevTokenWs(Module, Token) of
        #token{kind=ws}=T ->
            getPrevToken(Module, T);
        T ->
            T
    end.

getNextTokenWs(_Module, _Token, 0) ->
    [];
getNextTokenWs(Module, Token, N) ->
    T = getNextTokenWs(Module, Token),
    [T | getNextTokenWs(Module, T, N-1)].

getNextToken(Module, Token) ->
    case getNextTokenWs(Module, Token) of
        #token{kind=ws}=T ->
            getNextToken(Module, T);
        T ->
            T
    end.

do_getTokenAt(Module, Offset) ->
  {ok, getTokenAt(Module, Offset)}.

getTokenAt(Module, Offset) when Offset =< 0 ->
    %% ets:lookup returns [] sometimes....
    case ets:lookup(Module, 1) of
        [Token | _] ->
            Token;
        _ ->
            K = ets:first(Module),
            [A] = ets:lookup(Module, K),
            A
    end;
getTokenAt(Module, Offset) ->
     MS = ets:fun2ms(fun(#token{offset=Ofs, length=Len}=T)
           when Ofs =< Offset, Ofs+Len>Offset ->
          T
        end),
    case ets:select(Module, MS) of
        [X] -> 
            X;
        [] -> 
            hd(ets:lookup(Module, ets:last(Module)))
    end.

do_getTokensAround(Module, Offset) ->
  {ok, getTokensAround(Module, Offset)}.

getTokensAround(Module, Offset) ->
        T1 = getTokenAt(Module, Offset),
    T2 = findTokenLeft(Module, Offset),
    case T1==T2 of
        true -> T1;
        false -> [T2, T1]
    end.

isScanned(Module) ->
    lists:member(Module, ets:all()).


insertText(Module, Offset, Text) ->
     MS = ets:fun2ms(fun(#token{kind=eof}=T) ->
          T#token.offset
        end),
    [Eof] = ets:select(Module, MS), 
    if Offset < 1 ->
        {error, bad_offset, Offset};
       Offset > Eof ->
        {error, bad_offset, Offset, Eof};
       true ->
         do_insertText(Module, Offset, Text)
    end.           

do_insertText(Module, Offset, Text) ->
    Z = getTokensAround(Module, Offset),
    ?D({"*> insert at ~p: ~p~n", [Offset, Z]}),
    {Ofs, L, XL, Text2} = case Z of
                              #token{kind=eof}=T ->
                                  ?D(T),
                                  {T#token.offset, T#token.line, 0, Text};
                              [bof, #token{kind=eof}=T] ->
                                  ?D(T),
                                  {T#token.offset, T#token.line, 0, Text};
                              [T1, #token{kind=eof}=_T2] ->
                                  ?D(T1),
                                  Ofs1 = T1#token.offset + T1#token.length,
                                  ?D(a),
                                  case Offset of
                                      Ofs1 ->
                                          ?D(b),
                                          ets:delete(Module, T1#token.offset),
                                          {T1#token.offset, T1#token.line, nl(T1), get_text(T1)++Text};
                                      _ ->
                                          ?D(c),
                                          {Offset, T1#token.line, 0, Text}
                                      end;
                              [T1, T2] ->
                                  ?D(T1),
                                  ets:delete(Module, T1#token.offset),
                                  ets:delete(Module, T2#token.offset),
                                  {T1#token.offset, T1#token.line, nl(T1)+nl(T2), get_text(T1)++Text++get_text(T2)};
                              T -> %% delete T, split the text and paste it to Text
                                  ?D(T),
                                  ets:delete(Module, T#token.offset),
                                  {A, B} = split(T, Offset),
                                  {T#token.offset, T#token.line, nl(T), A++Text++B}
                          end,
    ?D(d),
    %% update offsets of tokens following the insertion point
    {ok, Tks, {LL, _LO}} = erlide_scan:string_ws(Text2),
    %%?D({sCAN, Text2, Tks}),
    %%io:format(">>> ~p ~p ~p/~p   ~n", [Ofs, Text2, L, LL-XL-1]),
    %%io:format("1 &&& ~p~n", [getWsTokens(Module)]),
    update_after(Module, Ofs, length(Text), LL-XL-1),
    %%io:format("2 &&& ~p~n", [getWsTokens(Module)]),
    %%io:format("      ~p~n", [Tks]),
    lists:foreach(fun(X)-> ets:insert(Module, mktoken(X, Ofs-1, L-1)) end, Tks),
    ?D({"3 &&& ~p~n", [getWsTokens(Module)]}),
    ok.

removeText(Module, Offset, Length) ->
     MS = ets:fun2ms(fun(#token{kind=eof}=T) ->
          T#token.offset
        end),
    [Eof] = ets:select(Module, MS), 
    if Offset < 1 ->
        {error, bad_offset, Offset};
       Offset+Length > Eof-1 ->
        {error, bad_offset, Offset, Length, Eof};
       true ->
        do_removeText(Module, Offset, Length)
     end.

do_removeText(Module, Offset, Length) ->
        T1 = findTokenLeft(Module, Offset),
    T2 = getTokenAt(Module, Offset+Length),
    %%io:format("R> ~p ~p::~n >  ~p~n >  ~p~n", [Offset, Length, T1, T2]),
    %% split T1 and T2 texts, remove all between T1 and T2, rescan and insert T1'+T2'
    {_Ofs, DeletedLines, NewText} = case {T1, T2} of
               {#token{kind=eof}, #token{kind=eof}} ->
                   {0, 0, ""};
               {#token{kind=eof}, _} ->
                                {T1#token.offset, 0, ""};
               {T1, T1} ->
             ets:delete_object(Module, T1),
             {T1#token.offset, 0, cut(get_text(T1), Offset-T1#token.offset+1, Length)};
               {_, #token{kind=eof, offset=Ofs}} ->
             delete_between(Module, Offset, Ofs-Offset-1),
             LX = Offset-T1#token.offset,
             Txt1 = cut(get_text(T1), LX+1, Length),
             {T1#token.offset, T2#token.line-T1#token.line, Txt1};
               {T1, T2} ->
             delete_between(Module, T1#token.offset, Length+Offset-T1#token.offset),
             Txt1 = cut(get_text(T1), Offset-T1#token.offset+1, Length),
             Txt2 = cut(get_text(T2), 1, Offset+Length-T2#token.offset),
             NL = case T2 of
                #token{kind=ws, text="\n"} ->1;
                _ -> 0
            end,
             {T1#token.offset, T2#token.line-T1#token.line+NL, Txt1++Txt2}
           end,
    {ok, Tks, {NewLines, _LO}} = erlide_scan:string_ws(NewText),
    %%io:format("x> ~p ~p:~p~n", [NewText, NewLines-1, DeletedLines]),
    update_after(Module, Offset+Length, -Length, NewLines-1-DeletedLines),
    lists:foreach(fun(X)-> ets:insert(Module, mktoken(X, T1#token.offset-1, T1#token.line-1)) end, Tks),
    ok.

clean([]) ->
    [];
clean([T]) ->
    [T];
clean([#token{kind=eof}|Rest]) ->
    clean(Rest);
clean(L) ->
    lists:reverse(clean_2(lists:reverse(L))).

clean_2([]) ->
    [];
clean_2([T]) ->
    [T];
clean_2([#token{kind=eof}, #token{kind=eof}|Rest]) ->
    clean([#token{kind=eof}|Rest]);
clean_2(L) ->
    L.

nl(#token{kind=ws, text="\n"}) ->
    1;
nl(_) ->
    0.

update_after(Module, Offset, DOfs, DL) ->
    MS = ets:fun2ms(fun(#token{offset=Ofs}=T)
           when Ofs >= Offset ->
          T
        end),
    Z = ets:select(Module, MS),
    case Z of
        [] ->
            ok;
        _ ->
            [fix(Module, T, DOfs, DL) || T<-Z]
    end.

fix(Module, Tok, DOfs, DL) ->
    Ofs = Tok#token.offset,
    LL = Tok#token.line,
    %%io:format("fix-- ~p = ~p ~p -> ~p ~p~n", [Tok, Ofs, LL, Ofs+DOfs, LL+DL]),
    ets:delete_object(Module, Tok),
    ets:insert(Module, Tok#token{offset = Ofs + DOfs, line=LL + DL}).

mktoken({K, {{L, O}, G}}, Ofs, NL) ->
    #token{kind=K, line=L+NL, offset=O+Ofs, length=G};
mktoken({ws, {{L, O}, G}, T}, Ofs, NL) ->
    #token{kind=ws, line=L+NL, offset=O+Ofs, length=G, text=T};
mktoken({K, {{L, O}, G}, V}, Ofs, NL) ->
    #token{kind=K, line=L+NL, offset=O+Ofs, length=G, value=V};
mktoken({K, {{L, O}, G}, V, T}, Ofs, NL) ->
    #token{kind=K, line=L+NL, offset=O+Ofs, length=G, value=V, text=T}.

revert_token(#token{text=undefined, value=undefined}=T) ->
    {T#token.kind, {{T#token.line, T#token.offset}, T#token.length}};
revert_token(#token{text=undefined}=T) ->
    {T#token.kind, {{T#token.line, T#token.offset}, T#token.length}, T#token.value};
revert_token(#token{value=undefined}=T) ->
    {T#token.kind, {{T#token.line, T#token.offset}, T#token.length}, T#token.text};
revert_token(#token{}=T) ->
    {T#token.kind, {{T#token.line, T#token.offset}, T#token.length}, T#token.value}.



findTokenLeft(_Module, Offset) when Offset =< 0 ->
    bof;
findTokenLeft(Module, Offset) ->
    getTokenAt(Module, Offset-1).

findTokenRight(Module, Offset) ->
    getTokenAt(Module, Offset+1).

split(Tok, Ofs) ->
    Txt = get_text(Tok),
    lists:split(Ofs-Tok#token.offset, Txt).

listify(L) when is_list(L) ->
    L;
listify(A) when is_atom(A) ->
    atom_to_list(A).

get_text(#token{text=V}) when V =/= undefined ->
    V;
get_text(#token{value=V}) when V =/= undefined ->
    listify(V);
get_text(#token{kind=V}) ->
    listify(V).


cut(Src, Ofs, Len) ->
    %%io:format("cut% ~p ~p ~p~n", [Src, Ofs, Len]),
    case Len=<0 of
        true ->
            Src;
        false ->
            case Ofs > length(Src) of
                true ->
                    Src;
                false ->
                    case Ofs+Len > length(Src) of
                        false ->
                            string:sub_string(Src, 1, Ofs-1) ++ string:sub_string(Src, Ofs+Len);
                        true ->
                            string:sub_string(Src, 1, Ofs-1)
                    end
            end
    end.

delete_between(Module, Offset, Length) ->
    MS = ets:fun2ms(fun(#token{offset=Ofs, length=Len}=T)
           when Ofs >= Offset, Ofs =< Offset+Length ->
          Ofs
        end),
    Z = ets:select(Module, MS),
    %%io:format("del between=~p~n :~p~n", [Z, ets:tab2list(Module)]),
    [ets:delete(Module, X) || X<-Z],
    ok.

filter_ws(L) ->
    Fun = fun(#token{kind=ws}) -> false;
             (_) -> true
          end,
    lists:filter(Fun, L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_test({Before, {F, Arg1, Arg2}, After}=Cmd) ->
    erlide_log:log("--------------------"),
    destroy(xx_before),
    create(xx_before),
    insertText(xx_before, 1, Before),
    Fun = case F of
        insert -> fun insertText/3;
        remove -> fun removeText/3
          end,
    X = Fun(xx_before, Arg1, Arg2),
    erlide_log:log(X),
    
    case X of
        ok ->
    L1 = ets:tab2list(xx_before),
    destroy(xx_after),
    create(xx_after),
    insertText(xx_after, 1, After),
    L2 = ets:tab2list(xx_after),
    %%erlide_log:log({L1, L2}),
    if L1==L2 ->
      [];
       true ->
      [{error, L1, L2}]
    end;
        Err ->
            [Err]
        end.
    
test() ->
    Tests = [
     {"", {insert, 1, "hej"}, "hej"},
     {"hej", {insert, 4, "ha"}, "hejha"},
     {"hej", {insert, 3, "ha"}, "hehaj"},
               
          
     
     {"", {insert, 1, "hej"}, "hej"}
     ],
    lists:flatten([do_test(X) || X<-Tests]).


%% create_test_() ->
%%     destroy(xx),
%%     create(xx),
%%
%%     ResT = ets:tab2list(xx),
%%     RefT = ref_ins("", "", 0),
%%
%%     ?_assert(ResT == RefT).
%%
%% %%%%
%%
%% ins_1_test_() ->
%%     %%io:format("~n~n********************~n********************~n~n"),
%%     Src = " abc d   efg ",
%%     Ins = "xz",
%%     [ ins_testz("ins1_"++integer_to_list(X), Src, Ins, X) || X<-lists:seq(1, length(Src)+1) ].
%%
%% ins_2_test_() ->
%%     %%io:format("~n~n********************~n********************~n~n"),
%%     Src = "abc",
%%     Ins = " x z",
%%     [ ins_testz("ins2_"++integer_to_list(X), Src, Ins, X) || X<-lists:seq(1, length(Src)+1) ].
%%
%% ins_3_test_() ->
%%     %%io:format("~n~n********************~n********************~n~n"),
%%     Src = "a\nb  \n\n qwe \n",
%%     Ins = "z\nx\n",
%%     [ ins_testz("ins3_"++integer_to_list(X), Src, Ins, X) || X<-lists:seq(1, length(Src)+1) ].
%%
%% ins_testz(Name, Src, Ins, Ofs) ->
%%     %%    io:format("***  ~p ~p ~p~n", [Src, Ins, Ofs]),
%%     RefT = ref_ins(Src, Ins, Ofs),
%%
%%     destroy(xx),
%%     create(xx),
%%     insertText(xx, 1, Src),
%%     insertText(xx, Ofs, Ins),
%%
%%     ResT = ets:tab2list(xx),
%%
%%     %%    io:format("*  ~p~n++~p~n++~p~n==~p~n", [Ofs, ResT, RefT, ResT==RefT]),
%%     ?_assert_(Name, ResT == RefT).
%%
%% ref_ins(Src, Ins, Ofs) ->
%%     %%io:format("#### ~p ~p ~p ~n", [Src, Ins, Ofs]),
%%     {A, B} = if Ofs<1 -> {[], Src};
%%                 true ->lists:split(Ofs-1, Src) end,
%%     Txt = A++Ins++B,
%%     %%io:format("#### ~p = ~p~n", [{A, B}, Txt]),
%%     {ok, Toks, {LL, LO}} = erlide_scan:string_ws(Txt),
%%     [mktoken(X, 0, 0) || X<-Toks]++[mktoken({eof, {{LL, LO}, 0}}, 0, 0)].
%%
%% %%%%%
%%
%% rmv_1_test_() ->
%%     %%io:format("~n~n********************~n********************~n~n"),
%%     Src = "asdfbc=/=mogz",
%%     N = 3,
%%
%%     [rmv_testz("rmv1_"++integer_to_list(X), Src, X, N) || X<-lists:seq(1, length(Src)-N+1)].
%%
%% rmv_1a_test_() ->
%%     %%io:format("~n~n********************~n********************~n~n"),
%%     Src = "asdfbc=/=mogz",
%%     N = 1,
%%
%%     [rmv_testz("rmv1_"++integer_to_list(X), Src, X, N) || X<-lists:seq(1, length(Src)-N+1)].
%%
%% rmv_2_test_() ->
%%     %%io:format("~n~n********************~n********************~n~n"),
%%     Src = "abc d    fdgdfg dfgd gdfg  d fgdfg   dfg dfg",
%%     N = 6,
%%
%%     [rmv_testz("rmv1_"++integer_to_list(X), Src, X, N) || X<-lists:seq(1, length(Src)-N+1)].
%%
%% rmv_3_test_() ->
%%     %%io:format("~n~n********************~n********************~n~n"),
%%     Src = "ab\ncd",
%%     N = 1,
%%
%%     [rmv_testz("rmv1_"++integer_to_list(X), Src, X, N) || X<-lists:seq(1, length(Src)-N+1)].
%%
%% rmv_4_test_() ->
%%     %%io:format("~n~n********************~n********************~n~n"),
%%     Src = "ab\ncd",
%%     N = 2,
%%
%%     [rmv_testz("rmv1_"++integer_to_list(X), Src, X, N) || X<-lists:seq(1, length(Src)-N+1)].
%%
%% rmv_5_test_() ->
%%     %%io:format("~n~n********************~n********************~n~n"),
%%     Src = "z\nab\n\ncdefg\n\n\nghjk\n\n\nwerty",
%%     N = 2,
%%
%%     [rmv_testz("rmv1_"++integer_to_list(X), Src, X, N) || X<-lists:seq(1, length(Src)-N+1)].
%%
%% rmv_testz(Name, Src, Ofs, Len) ->
%%     %%io:format("*** ~p ~p ~p ~n", [Src, Ofs, Len]),
%%     Txt = cut(Src, Ofs, Len),
%%     %%io:format("========= ~p ~p~n", [Ofs, Txt]),
%%
%%     RefT = ref_rmv(Src, Ofs, Len),
%%
%%     destroy(xx),
%%     create(xx),
%%     insertText(xx, 1, Src),
%%     removeText(xx, Ofs, Len),
%%
%%     ResT = ets:tab2list(xx),
%%
%%     %%io:format("*  ~p~n++ ~p~n++ ~p~n== ~p~n~n~n", [Ofs, ResT, RefT, ResT==RefT]),
%%     ?_assert_(Name, ResT == RefT).
%%
%% ref_rmv(Src, Ofs, Len) ->
%%     Txt = case Len of 0 -> Src; _ -> cut(Src, Ofs, Len) end,
%%     %%io:format("### ~p ~w ~w:: ~p~n", [Src, Ofs, Len, Txt]),
%%     {ok, Tok, {LL, LO}} = erlide_scan:string_ws(Txt),
%%     [mktoken(X, 0, 0) || X<-Tok]++[mktoken({eof, {{LL, LO}, 0}}, 0, 0)].
%%
%%
%% prev_test() ->
%%     Src = "abc def   mor",
%%     io:format("*** ~p ~n", [Src]),
%%
%%     destroy(xx),
%%     create(xx),
%%     insertText(xx, 1, Src),
%%
%%     ResT = ets:tab2list(xx),
%%
%%     RefT = lists:reverse(tl(lists:foldl(fun(X, Acc)-> [getNextToken(xx, X) | Acc] end, [], ResT))),
%%
%%     io:format("++ ~p~n++ ~p~n== ~p~n~n~n", [ResT, RefT, ResT==RefT]),
%%     ?_assert_("prev", ResT == RefT).
%%

