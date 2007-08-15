-module(jrpc).

-export([
		 call/3,
         call/4,
         uicall/3,
         uicall/4,
         cast/3,
         event/2,
          
         rpc_loop/1 
]).

-export([test/0]).

call(Rcvr, Msg, Args) ->
    call0(call, Rcvr, Msg, Args, infinity).

call(Rcvr, Msg, Args, Timeout) ->
    call0(call, Rcvr, Msg, Args, Timeout).

uicall(Rcvr, Msg, Args) ->
    call0(uicall, Rcvr, Msg, Args, infinity).

uicall(Rcvr, Msg, Args, Timeout) ->
    call0(uicall, Rcvr, Msg, Args, Timeout).


call0(Kind, Rcvr, Msg, Args, Timeout) ->
        erlide_rex ! {Kind, Rcvr, Msg, Args, self()},
    receive
              {reply, Resp} ->
                  {ok, Resp};
                       Err ->
                           {error, Err}
             after Timeout ->
                 timeout
                   end.

cast(Rcvr, Msg, Args) ->
    erlide_rex ! {cast, Rcvr, Msg, Args},
    ok.

event(Id, Msg) ->
    erlide_rex ! {event, Id, Msg},
    ok.

rpc_loop(JavaNode) ->
    receive
       Msg ->
           {rex, JavaNode} ! Msg,
           rpc_loop(JavaNode)
       end,
    ok.

test() ->
       
        {ok, R0}=call('org.erlide.jinterface.RpcUtil', testing, [], 2000),
        io:format(">>>> ~p~n", [R0]),
        Rx=call(R0, get, ["alfa"], 2000),
        io:format(">>>> ~p~n", [Rx]),

        %%java_util_HashMap:put(R0, "gamma", "delta"),
        %%Rx1=java_util_HashMap:get(R0, "gamma"),
        %%io:format(">>>> ~p~n", [Rx1]),
                
        R1=call('org.erlide.jinterface.RpcUtil', testing, [1], 2000),
        io:format(">>>> ~p~n", [R1]),
        R1a=call(<<"org.erlide.jinterface.RpcUtil">>, "testing", ["A"], 2000),
        io:format(">>>> ~p~n", [R1a]),
        R2=call('org.erlide.jinterface.RpcUtil', testing, [2, 'end'], 2000),
        io:format(">>>> ~p~n", [R2]),
        R=call('org.erlide.jinterface.RpcUtil', testing, [3.14, 'end', {999, self()}], 2000),
        io:format(">>>> ~p~n", [R]),
        
        P = 'org.eclipse.core.runtime.Platform',
        io:format("---- ~p~n", [call(P, getApplicationArgs, [], 2000)]),
        io:format("---- ~p~n", [call(P, knownOSValues, [], 2000)]),
                
        U = 'org.eclipse.ui.PlatformUI',
        io:format("++++ ~p~n", [call(U, getWorkbench, [], 2000)]),
                        
    ok.