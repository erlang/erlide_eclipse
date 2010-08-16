%% Author: Piotr Dorobisz
%% Created: Jul 29, 2010
-module(ttb_integration).

%%
%% Exported Functions
%%
-export([start/0, stop/0, stop_tracing/0]).


start()->
	ttb:tracer(all, [{handler, {create_handler(), initial_state}}]).

stop() ->
	spawn(?MODULE, stop_tracing, []).

stop_tracing()->
	ttb:stop([format]),
	erlide_jrpc:event(trace_event, stop_tracing).
	%% 	HandlerPid ! stop_tracing.

create_handler() ->
	fun(Fd, Trace, _TraceInfo, State) ->
			case Trace of
				{X, Pid, call, {Mod, Fun, Arg}} ->
%% 					HandlerPid ! {X, Pid, call, {Mod, Fun,[avoid_interpreting_as_string] ++ Arg}},
					erlide_jrpc:event(trace_event, {X, Pid, call, {Mod, Fun,[avoid_interpreting_as_string] ++ Arg}});
				{X, Pid, spawn, Pid2, {M, F, Args}} ->
%% 					HandlerPid ! {X, Pid, spawn, Pid2, {M, F, [avoid_interpreting_as_string] ++ Args}},
					erlide_jrpc:event(trace_event, {X, Pid, spawn, Pid2, {M, F, [avoid_interpreting_as_string] ++ Args}});
				_ ->
%% 					HandlerPid ! Trace,
					erlide_jrpc:event(trace_event, Trace)
			end,
			State
	end.