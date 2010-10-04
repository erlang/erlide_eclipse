-module(x).

-record('REC', {a,b,c}).

-define(myrec, #'REC'{}).

f() ->
	X=0,
	X=1.

