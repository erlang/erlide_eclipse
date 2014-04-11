-module(erlide_tracer).

-export([
         start/0,
         start/1,
         trace/1
        ]).

%% send to this pid
start(Pid) when is_pid(Pid) ->
    dbg:start(),
    dbg:tracer(process, {fun send_to_pid/2, Pid}),
    ok.

%% output to log
start() ->
    dbg:start(),
    dbg:tracer(process, {fun send_to_log/2, ok}),
    ok.

send_to_pid(Message, Pid) ->
    Pid ! Message,
    Pid.

send_to_log(Message, _) ->
    erlide_log:log({tracing, Message}),
    ok.

trace(Module) ->
    dbg:p(all, [c]),
    dbg:tp(Module, cx),
    ok.
