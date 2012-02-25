-module(x).
-compile(export_all).
-foo x.
-record('REC', {a,b,c}).

-define(myrec, #'REC'{}).
    
f() ->
	X=[1,4,6],
	X.
 
g() -> 
	'a\1b',
	new_file:ok(),
	   
	dict:new(),
	f(), 
	"he  ~n ha \b \0123 \xcafa \x{cafe}s he\"j '",
	[$", $', $\567, $\x{cafe}]. 

  
  