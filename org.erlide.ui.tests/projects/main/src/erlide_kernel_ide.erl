-module(erlide_kernel_ide).

-export([
		 init/0
		]).

init() ->	
	spawn(fun()->
				  erlide_scanner_listener:start(),
				  ok
		  end),
	ok.

init1(L) when is_list(L) ->
	[init1(X) || X<-L];
init1(execute) ->
	ok;
init1(ide) ->
	ok;
init1(build) ->
	erlide_xref:start(),
	ok;
init1(monitor) ->
	%watch_eclipse(node(JRex)),
	ok.	

