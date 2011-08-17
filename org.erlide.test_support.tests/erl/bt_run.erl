%% mockup for bt_run
-module(bt_run).

-compile(export_all).

-include("erlide.hrl").

-define(NUM_TESTS, 7).

run(Dir, Suite, Arg, Cb) ->
	?Info({"BT_RUN RUN", Dir, Suite, Arg, Cb}),
	spawn(fun()->arun(Dir, Suite, Arg, Cb) end),
	ok.

arun(Dir, Suite, Arg, Cb) ->
	timer:sleep(100),
	notify(Cb, init, {"",s,c}),
	Z=(catch blabla_SUITE:f()),
	?Info(Z),
	do_tests(Dir, Suite, Arg, Cb, ?NUM_TESTS).

do_tests(Dir, Suite, Arg, Cb, 0) ->
	stop(Dir, Suite, Arg, Cb);
do_tests(Dir, Suite, Arg, Cb, N) ->
	F=list_to_atom("f"++[?NUM_TESTS-N+$1]),
	notify(Cb, tc_start, {m,F}),
	apply(Dir, Suite, Arg),
	case N of
		2 ->	
			notify(Cb, tc_fail, {{m,F}, [], reason});
		4 ->	
			notify(Cb, tc_fail, {{m,F}, [], reason});
		%% 		3 ->	
		%% 			notify(Cb, tc_skip, {{m,F}, [], reason});
		_ ->
			notify(Cb, tc_result, {m, F, ok})
	end,
	do_tests(Dir, Suite, Arg, Cb, N-1).

stop(Dir, Suite, _Arg, Cb) ->
	notify(Cb, done, {Dir, Suite, {3,4,5}, []}),
	ok.
	

notify(Cb, Event, Args) ->
	?Info({Cb, Event, Args}),
	Cb:Event(Args),
	ok.
