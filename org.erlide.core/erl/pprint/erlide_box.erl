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

-module(erlide_box).

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
    i([], Els).

i(Opts, Els) ->
    {i, Opts, Els}.

wd(Els) ->
    wd([], Els).

wd(Opts, Els) ->
    {wd, Opts, Els}.

hv(Els) ->
    hv([], Els).

hv(Opts, Els) ->
    {hv, Opts, Els}.

hov(Els) ->
    hv([], Els).

hov(Opts, Els) ->
    {hov, Opts, Els}.

a(AOpts, Els) ->
    a(AOpts, [], Els).

a(AOpts, Opts, Els) ->
    {table, AOpts, Opts, Els}.

r(Els) ->
    {row, [], Els}.

r(Opts, Els) ->
    {r, Opts, Els}.

kw(X) ->
    {keyword, X}.

var(X) ->
    {variable, X}.

str(X) ->
    {string, X}.

num(X) ->
    {number, integer_to_list(X)}.

f(Opts, X) ->
    {font, Opts, X}.

lbl(X) ->
    {label, X}.

ref(X) ->
    {ref, X}.

cmt(X) ->
    {comment, X}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
