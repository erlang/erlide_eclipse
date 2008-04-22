%%% ******************************************************************************
%%%  Copyright (c) 2004 Vlad Dumitrescu and others.
%%%  All rights reserved. This program and the accompanying materials
%%%  are made available under the terms of the Eclipse Public License v1.0
%%%  which accompanies this distribution, and is available at
%%%  http://www.eclipse.org/legal/epl-v10.html
%%%
%%%  Contributors:
%%%      Vlad Dumitrescu
%%% ******************************************************************************
%%%

-module(erlide_scanner).

-include_lib("stdlib/include/ms_transform.hrl").

-compile(export_all).

-export([initialScan/4, insertText/3, removeText/3, destroy/1, mktoken/3, create/1, 
         getTokens/1, revert_token/1, getTokenWindow/3, getTokenAt/2,
         getNextToken/2, getPrevToken/2]).
-export([test/0]).
 
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

%% the window is asymmetric, to allow finding number of arguments
do_getTokenWindow(Module, Offset, Window) ->
  {ok, getTokenWindow(Module, Offset, Window)}.

%% the window is asymmetric, to allow finding number of arguments
getTokenWindow(Module, Offset, Window) ->
    T = getTokenAt(Module, Offset),
    Tp = getPrevToken(Module, T, Window),
    Tn = getNextToken(Module, T, Window*10),
    %%?Debug({tp, Tp}),
    %%?Debug({t, T}),
    %%    ?Debug({tn, Tn}),
    clean(lists:reverse(Tp)++[T|Tn]).

getPrevTokenWs(Module, Token) ->
    Ofs = Token#token.offset-1,
    R=getTokenAt(Module, Ofs),
    R.

getNextTokenWs(Module, Token) ->
    Ofs = Token#token.offset+Token#token.length,
    getTokenAt(Module, Ofs).

getPrevToken(_Module, _Token, 0) ->
    [];
getPrevToken(Module, Token, N) ->
    T = getPrevToken(Module, Token),
    [T | getPrevToken(Module, T, N-1)].

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

getNextToken(_Module, _Token, 0) ->
    [];
getNextToken(Module, Token, N) ->
    T = getNextToken(Module, Token),
    [T | getNextToken(Module, T, N-1)].

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
    T1 = findTokenLeft(Module, Offset),
    T2 = getTokenAt(Module, Offset),
        {Ofs, L, XL, Text2} = case {T1, T2} of
                              {#token{kind=eof}, #token{kind=eof}} ->
                                  {T1#token.offset, T1#token.line, 0, Text};
                              {T1, #token{kind=eof}} ->
                                  Ofs1 = T1#token.offset + T1#token.length,
                                  case Offset of
                                      Ofs1 ->
                                          ets:delete(Module, T1#token.offset),
                                          {T1#token.offset, T1#token.line, nl(T1), get_text(T1)++Text};
                                      _ ->
                                          {Offset, T1#token.line, 0, Text}
                                      end;
                              {T1, T1} -> %% delete T, split the text and paste it to Text
                                  ets:delete(Module, T1#token.offset),
                                  {A, B} = split(T1, Offset),
                                  {T1#token.offset, T1#token.line, nl(T1), A++Text++B};
                              {T1, T2} ->
                                  ets:delete(Module, T1#token.offset),
                                  ets:delete(Module, T2#token.offset),
                                  {T1#token.offset, T1#token.line, nl(T1)+nl(T2), get_text(T1)++Text++get_text(T2)}
                          end,
    %% update offsets of tokens following the insertion point
    {ok, Tks0, {LL, _LO}} = erlide_scan:string_ws(Text2++"\n"),
    Tks = lists:reverse(tl(lists:reverse(Tks0))),
    update_after(Module, Ofs, length(Text), LL-XL-2),
    lists:foreach(fun(X)-> ets:insert(Module, mktoken(X, Ofs-1, L-1)) end, Tks),
    ok.

removeText(Module, Offset, Length) ->
     MS = ets:fun2ms(fun(#token{kind=eof}=T) ->
          T#token.offset
        end),
    [Eof] = ets:select(Module, MS), 
    if Offset < 1 ->
        {error, bad_offset, Offset};
       Offset+Length > Eof ->
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
    {ok, Tks0, {NewLines, _LO}} = erlide_scan:string_ws(NewText++"\n"),
    Tks = lists:reverse(tl(lists:reverse(Tks0))),
    update_after(Module, Offset+Length, -Length, NewLines-2-DeletedLines),
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
    end,
    ok.

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



findTokenLeft(Module, Offset) when Offset =< 0 ->
    findTokenLeft(Module, 1);
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

do_test({Before, {F, Arg1, Arg2}, Expect}=Cmd) ->
    %erlide_log:log({"----- test ", Cmd}),
    destroy(xx_before),
    create(xx_before),
    insertText(xx_before, 1, Before),
    Fun = case F of
              insert -> fun insertText/3;
              remove -> fun removeText/3
          end,
    X = (catch Fun(xx_before, Arg1, Arg2)),
          
    case X of
          ok ->
		    Fun2 = case F of
        		       	insert -> fun ins/3;
               			remove -> fun del/3
           			end,
    		After = (catch Fun2(Before, Arg1, Arg2)),      
    		%%erlide_log:log({"    - ", Cmd, After}),
                     
              destroy(xx_after),
              create(xx_after),
              insertText(xx_after, 1, After),
                   
              L1 = ets:tab2list(xx_before),
              L2 = ets:tab2list(xx_after),
              if L1==L2 ->
                   [];
                 true ->
                   [{mismatch, Cmd, After, L1, L2}]
            end;
          Other ->
              case Expect of
                  Other ->
                      [];
                  _ ->
                      [{unexpected, Cmd, Other}]
              end
    end.
   
ins(Src, Ofs, Ins) ->
     {A, B} = if Ofs<1 -> {[], Src};
                 true ->lists:split(Ofs-1, Src) end,
     A++Ins++B.

del(Src, Ofs, Len) ->
     case Len of 
         0 -> Src; 
         _ -> cut(Src, Ofs, Len) 
     end.

test() ->
    Tests = [ 
     {"", {insert, 1, "hej"}, ok},
     {"hej", {insert, 0, "ha"}, {error, bad_offset, 0}},
     {"hej", {insert, 1, "ha"}, ok},
     {"hej", {insert, 3, "ha"}, ok},
     {"hej", {insert, 5, "ha"}, {error, bad_offset, 5, 4}},
     
     {"\"hej\"", {insert, 3, "ha"}, ok},
     {"hej", {insert, 3, "\"ha\""}, ok},
     
     {"hej\n", {insert, 1, "%"}, ok},
     {"hej\n", {insert, 2, "%"}, ok},
     
     {"ba\nx%hej\nba", {insert, 2, "ha"}, ok},
     {"ba\nx%hej\nba", {insert, 4, "ha"}, ok},
     {"ba\nx%hej\nba", {insert, 5, "ha"}, ok},
     {"ba\nx%hej\nba", {insert, 6, "ha"}, ok},
     {"ba\nx%hej\nba", {insert, 10, "ha"}, ok},
     
     {"12", {insert, 2, "+"}, ok},
     {"aabb", {insert, 3, " "}, ok},
     
     {"a\"a b\"b", {insert, 1, "x\"y\""}, ok},
     {"a\"a b\"b", {insert, 2, "x\"y\""}, ok},
     {"a\"a b\"b", {insert, 3, "x\"y\""}, ok},
     {"a\"a b\"b", {insert, 4, "x\"y\""}, ok},
     {"a\"a b\"b", {insert, 5, "x\"y\""}, ok},
     
     {"a 'a b' b", {insert, 1, "x \"y\""}, ok},
     {"a 'a b' b", {insert, 2, "x \"y\""}, ok},
     {"a 'a b' b", {insert, 3, "x \"y\""}, ok},
     {"a 'a b' b", {insert, 4, "x \"y\""}, ok},
     {"a 'a b' b", {insert, 5, "x \"y\""}, ok},
     {"a 'a b' b", {insert, 6, "x \"y\""}, ok},
     {"a 'a b' b", {insert, 7, "x \"y\""}, ok},
     {"a 'a b' b", {insert, 8, "x \"y\""}, ok},
     
     {"hej", {remove, 2, 1}, ok},                
     {"hej", {remove, 1, 3}, ok},                
     {"hej", {remove, 2, 3}, {error, bad_offset, 2, 3, 4}},                
     {"hej", {remove, 0, 1}, {error, bad_offset, 0}},                
     
     {"ba\n%hej\nba", {remove, 6, 1}, ok},                
     {"ba\n%hej\nba", {remove, 2, 6}, ok},                
     
     {"aa bb", {remove, 3, 1}, ok},
     
     {"", {insert, 1, ""}, ok}
     ],
    lists:flatten([do_test(X) || X<-Tests]).

