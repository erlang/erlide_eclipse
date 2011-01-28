%% mockup for bt_run
-module(bt_run).

-compile(export_all).

-include("erlide.hrl"). 

run(A, B, C, D) ->
	?Info({A, B, C, D}),
	ok.

notify(Cb, Event) ->
	ok.
