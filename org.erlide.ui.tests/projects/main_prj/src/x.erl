-module(x).

-record('REC', {a,b,c}).

-define(myrec, #'REC'{}).

f() ->
	#'R
