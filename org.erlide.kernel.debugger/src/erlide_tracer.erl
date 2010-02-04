
-module(erlide_tracer).

-export([start/1]).

start(Pid) when is_pid(Pid) ->
	dbg:start(),
	dbg:tracer(process, {fun handler/2, Pid}),
	ok.

handler(Message, Pid) ->
	Pid ! Message,
	Pid.

