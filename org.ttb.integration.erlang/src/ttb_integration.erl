%% Author: Piotr Dorobisz
%% Created: Jul 29, 2010
-module(ttb_integration).

%%
%% Exported Functions
%%
-export([start/1, stop/1]).


start(HandlerPid)->
	ttb:tracer(all, [{handler, {create_handler(HandlerPid), initial_state}}]).

stop(HandlerPid)->
	ttb:stop([format]),
	HandlerPid ! stop_tracing.

create_handler(HandlerPid) ->
	fun(Fd, Trace, _TraceInfo, State) ->
			case Trace of
				{X, Pid, call, {Mod, Fun, Arg}} ->
					HandlerPid ! {X, Pid, call, {Mod, Fun,[avoid_interpreting_as_string] ++ Arg}};
				{X, Pid, spawn, Pid2, {M, F, Args}} ->
					HandlerPid ! {X, Pid, spawn, Pid2, {M, F, [avoid_interpreting_as_string] ++ Args}};
				_ ->
					HandlerPid ! Trace
			end,
			State
	end.