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
	notify(Cb, init, {"",s,c}),
	do_tests(Cb, 5).

do_tests(Cb, 0) ->
	stop(Cb);
do_tests(Cb, N=2) ->
	F=list_to_atom("f"++[N+$0]),
	notify(Cb, tc_start, {m,F}),
	timer:sleep(800),
	notify(Cb, tc_fail, {{m,F}, [], reason}),
	do_tests(Cb, N-1);
do_tests(Cb, N) ->
	F=list_to_atom("f"++[N+$0]),
	notify(Cb, tc_start, {m,F}),
	timer:sleep(800),
	notify(Cb, tc_result, {m, F, ok}),
	do_tests(Cb, N-1).

stop(Cb) ->
	notify(Cb, done, {m, "", {3,4,5}, []}),
	ok.
	

notify(Cb, Event, Args) ->
	?Info({Cb, Event, Args}),
	Cb:Event(Args),
	ok.
