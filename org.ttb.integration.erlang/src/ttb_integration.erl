%% Author: Piotr Dorobisz
%% Created: Jul 29, 2010
-module(ttb_integration).

%%
%% Exported Functions
%%
-export([start/1, stop/1]).


start(Pid)->
	ttb:tracer(all, [{handler, {create_handler(Pid), initial_state}}]).

stop(Pid)->
	ttb:stop([format]),
	Pid ! stop.

create_handler(Pid) ->
	fun(Fd, Trace, _TraceInfo, State) ->
			case Trace of
				{trace, {TracePid, _, Node}, call, {Mod, Fun, Arg}} ->
					Pid ! {Node, Mod, Fun, [avoid_interpreting_as_string] ++ Arg};
				_ ->
					Pid ! trace_end
			end,
			State
	end.