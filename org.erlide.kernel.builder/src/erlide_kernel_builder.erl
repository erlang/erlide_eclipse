-module(erlide_kernel_builder).

-export([
		 init/0
		]).

init() ->
	spawn(fun()->
				  %% is started by erlide_kernel_common
				  %% erlide_batch:start(erlide_builder),
				  ok
		  end),
	ok.
