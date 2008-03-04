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

-export([log/1, log/2, erlangLog/4]).

-define(DEFAULT_LEVEL, info).

log(Msg) ->
    log(?DEFAULT_LEVEL, Msg).

log(Level, Msg) ->
    jrpc:event(log, {Level, Msg}).

erlangLog(Module, Line, Level, Msg) ->
    jrpc:event(erlang_log, {Module, Line, Level, Msg}).
