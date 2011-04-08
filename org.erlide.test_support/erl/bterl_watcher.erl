-module(bterl_watcher).

-export([start/0, 
         start_bterl/1,
         init_debugger/0
        ]).

-export([init/1, 
         start_failed/1,
         log_started/1,
         done/1,
         
         tc_start/1,
         tc_result/1,
         tc_fail/1,
         tc_skip/1
        ]).

-include("erlide.hrl").

start() ->
	spawn(fun() ->
                  ?Info({bterl_watcher, self()}),
				  wait_start()
		  end).

%% shut down everything when there are no more jobs to do
stop_when_done() ->
	timer:sleep(1000),
	Jobs = test_server_ctrl:jobs(),
	case Jobs of
		[] ->
            ?Info({"!!stop!! ", stop_bterl}),
			init:stop();
		_ ->
			stop_when_done()
	end.

wait_start() ->
	timer:sleep(1000),
	Jobs = (catch test_server_ctrl:jobs()),
	case Jobs of
		[] ->
			wait_start();
		{'EXIT', _R} ->
			wait_start();
		_ ->
			stop_when_done()
	end.

start_bterl(Str) ->
    start(),
    {ok, {Flags, Cmd, _Trace, Cb, _Dir}=X} = erlide_backend:parse_term(Str++"."),
    ?Info({start_bterl, X}),
%%     Pids = case bt_run:has_flag($t, Flags) of 
%%         true ->
%%             Spec = [],
%%             TracePid = bt_run:start_trace(Dir, Spec),
%%             [TracePid];
%%         false ->
%%             []
%%     end,
    Init = case Cb of
               undefined ->
                   undefined;
               {M, F} ->
                   catch M:F()
           end,
    ?Info({">> init cb: ", Init}),
    
    Pids = [],
	case catch bt_run:run(Flags, Cmd, Pids, ?MODULE) of
		{'EXIT', Reason} ->
			?Info({"Could not start bterl", Reason});
		_ ->
			ok
	end,

    %% sent 'finished' event to caller
    ok.

init_debugger() ->
    ?Info("initializing Debugger..."),
    %% FIXME: DON'T HARDCODE THIS! wait until builder is done...
    timer:sleep(1000),
    erlide_jrpc:event(bterl_debugger, self()),
    receive
        ok ->
            ?Info("Debugger initialized!"),
            ok
    after 30000 ->
            ?Info("Debugger initialization failed..."),
            nok
    end.


%% Event callbacks

init(Args) ->
    %os:cmd("touch /home/qvladum/zzz"),
    notify({init, Args}),
    ok.

start_failed(Reason) ->
    notify({start_failed, Reason}),
    ok.

tc_start(Result) ->
    notify({start, Result}),
    ok.

tc_result(Result) ->
    notify({result, Result}),
    ok.

tc_fail(Result) ->
    notify({fail, Result}),
    ok.

tc_skip(Result) ->
    notify({skip, Result}),
    ok.

log_started(Arg) ->
    notify({log_started, Arg}),
    ok.

done(Arg) ->
    notify({done, Arg}),
    ok.

%%

notify(Event) ->
    erlide_jrpc:event(bterl, Event).

