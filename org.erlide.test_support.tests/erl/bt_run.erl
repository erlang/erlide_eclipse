%% mockup for bt_run
-module(bt_run).

-compile(export_all).

-include("erlide.hrl"). 

run(Dir, Suite, Arg, Cb) ->
	?Info({Dir, Suite, Arg, Cb}),
	spawn(fun()->run(Cb) end),
	ok.

run(Cb) ->
	timer:sleep(100),
	notify(Cb, init, init),
	do_tests(Cb, 5).

do_tests(Cb, 0) ->
	stop(Cb);
do_tests(Cb, N) ->
	timer:sleep(200),
	notify(Cb, tc_result, ok),
	do_tests(Cb, N-1).

stop(Cb) ->
	notify(Cb, done, ok),
	ok.
	

notify(Cb, Event, Args) ->
	?Info({Cb, Event, Args}),
	Cb:Event(Args),
	ok.
