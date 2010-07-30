%% Author: Piotr Dorobisz
%% Created: Jul 29, 2010
-module(ttb_integration).

%%
%% Exported Functions
%%
-export([start/1, stop/1]).


start(Pid)->
	Fun = fun(Fd, Trace, _TraceInfo, State) ->
%% 				  Pid ! {Fd, Trace, _TraceInfo},
				  Pid ! Trace,
				  State
		  end, 
	ttb:tracer(all, [{handler, {Fun, initial_state}}]),
	ttb:p(all, call).

stop(Pid)->
	ttb:stop([format]),
	Pid ! stop.
