-module(main).

-export([foo/0]).

%% Здравствуйте
%% 汉语/漢語 中文 ff
foo() ->
	io:format("kakak~n"),
	oki, "ok",
	ok, 
	fun() -> ok end,
	ok.

bar() ->
	fo,
	supervisor:start_child(1, 2),
	lists:reverse([]).

