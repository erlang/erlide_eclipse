-module(x).
-compile(export_all).
-record('REC', {a,b,c}).

-define(myrec, #'REC'{}).

f() ->
	X=[1,4,6],
	X=1.

g() ->
	dict:new(),
	f().

