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

