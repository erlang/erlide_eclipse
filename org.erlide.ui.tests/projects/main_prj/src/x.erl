-module(x).
-compile(export_all).
-record('REC', {a,b,c}).

-define(myrec, #'REC'{}).
-define(amacro, "").
-define(Amacro, "").
-define(aMacro, "").

%%    hello
%% doc for f
-spec f() ->
		  any().
f() -> 
	X=[1,4,6,8], 
	Z = lists:reverse(X),
	io:format(""),
	Z.

g() -> 
	'a\1b',
	new_file:ok(),
	dict:new(),
	hello,
	f(), 
	"he  ~n ha \b \0123 \xcafa \x{cafe}s he\"j '",
	[$", $', $\567, $\x{cafe}]. 

