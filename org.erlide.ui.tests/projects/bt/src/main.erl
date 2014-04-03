%% coding: utf-8

-module(main).

-export([foo/0, bar/0]).

%% Здравствуйте
%% 汉语/漢語 中文 ff
foo() ->
	io:format("kakak~n"),
	F = fun() -> ok end,
	F.

bar() ->
	fo,
	supervisor:start_child(1, 2),
	lists:reverse([]).

