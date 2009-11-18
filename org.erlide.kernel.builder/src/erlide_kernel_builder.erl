-module(erlide_kernel_builder).

-export([
		 init/0
		]).

init() ->
	spawn(fun()->
				  erlide_batch:start(erlide_builder),
				  ok
		  end),
	ok.
