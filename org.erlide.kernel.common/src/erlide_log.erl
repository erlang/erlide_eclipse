%%% ******************************************************************************
%%%  Copyright (c) 2008 Vlad Dumitrescu and others.
%%%  All rights reserved. This program and the accompanying materials
%%%  are made available under the terms of the Eclipse Public License v1.0
%%%  which accompanies this distribution, and is available at
%%%  http://www.eclipse.org/legal/epl-v10.html
%%%
%%%  Contributors:
%%%      Vlad Dumitrescu
%%% ******************************************************************************/
%%% File    : erlide_log.erl
%%% Author  :  Vlad Dumitrescu
%%% Description :

-module(erlide_log).

-export([log/1, logp/1, logp/2, log/2, erlangLog/4, erlangLogStack/4]).

-define(DEFAULT_LEVEL, info).

log(Msg) ->
    log(?DEFAULT_LEVEL, Msg).

logp(Msg) ->
    logp("~p", [Msg]).

logp(Fmt, Msgs) when is_list(Fmt), is_list(Msgs) ->
    log(?DEFAULT_LEVEL, lists:flatten(io_lib:format(Fmt, Msgs))).

log(Level, Msg) when is_atom(Level) ->
    erlide_jrpc:event(log, {Level, Msg}).

erlangLog(Module, Line, Level, Msg) when is_atom(Level) ->
    erlide_jrpc:event(erlang_log, {Module, Line, Level, Msg}).

erlangLogStack(Module, Line, Level, Msg) when is_atom(Level) ->
    erlide_jrpc:event(erlang_log, {Module, Line, Level, Msg, erlang:process_info(self(), backtrace)}).

