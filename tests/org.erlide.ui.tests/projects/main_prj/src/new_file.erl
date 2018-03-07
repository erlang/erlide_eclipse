-module(new_file).

-export([ok/0]).
-compile(export_all).

ok() ->   
 	ok.

foo() ->
	Z=[],
	[X || X<-Z, 
		  begin true 
		  end],
	"ok".

