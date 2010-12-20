%%% ******************************************************************************
%%%  Copyright (c) 2009 Vlad Dumitrescu and others.
%%%  All rights reserved. This program and the accompanying materials
%%%  are made available under the terms of the Eclipse Public License v1.0
%%%  which accompanies this distribution, and is available at
%%%  http://www.eclipse.org/legal/epl-v10.html
%%%
%%%  Contributors:
%%%      Vlad Dumitrescu
%%% ******************************************************************************/
-module(erlide_scanner_listener).

-export([
         start/0
        ]).

-include("erlide.hrl"). 

start() ->
    Pid = spawn(fun()->
                        ?SAVE_CALLS, 
                        loop([]) 
                end),
    register(?MODULE, Pid),
    ok.

loop(L) ->
    receive
        stop ->
            ok;
        {change, _Module, _Offset, _Length, _Text} = Msg ->
            %%erlide_log:logp({scanner_listener, Msg}),
            loop(aggregate(Msg, L));
        {new, _Module} = _Msg ->
            %%erlide_log:logp({scanner_listener, _Msg}),
            loop(L);
        _Msg ->
            %%erlide_log:logp({scanner_listener, unknown, _Msg}),
            loop(L)
    
        after 600 ->
            case L of 
                [] -> 
                    ok;
                _ ->
                    [handle(X) || X <- lists:reverse(L)]
            end,
            loop([])
    end.

aggregate(Msg={change, Module, Offset, 0, Text}, L=[{change, Module, Offset2, 0, Text2}|T]) ->
    case Offset2+length(Text2) of
        Offset ->
            [{change, Module, Offset2, 0, Text2++Text}|T];
        _ ->
            [Msg|L]
    end;
aggregate(Msg, L) ->
    [Msg|L].

handle(_X={change, _Module, _Offset, _Length, _Text}) ->
    %%erlide_log:logp({reconcile, _X}),
    %%erlide_scanner_server:replaceText(Module, Offset, Length, Text),
    ok.
