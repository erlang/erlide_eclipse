-module(jrpc).

%% The processes spawned from this module are not meant to support code reload,
%% because they are crucial to the functioning of the backends and 
%% careless update of code could make nothing work anymore.

-export([
		 init/1,
		 add_service/2,
		 get_service_listeners/1,
		 
		 call/3,
		 call/4,
		 uicall/3,
		 uicall/4,
		 cast/3,
		 event/2 
		]).

init(JPid) ->
	spawn(fun() -> manager([]) end),
	
	case whereis(erlide_rex) of
		undefined ->
			ok;
		Pid ->
			exit(Pid, kill)
	end,
	RpcPid = spawn(fun() -> rpc_loop(JPid) end),
	register(erlide_rex, RpcPid),
	RpcPid.


call(Rcvr, Msg, Args) ->
	call0(call, Rcvr, Msg, Args, 5000).

call(Rcvr, Msg, Args, Timeout) ->
	call0(call, Rcvr, Msg, Args, Timeout).

uicall(Rcvr, Msg, Args) ->
	call0(uicall, Rcvr, Msg, Args, 5000).

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
	erlide_rex ! {cast, Rcvr, Msg, Args, self()},
	ok.

event(Id, Msg) ->
	erlide_rex ! {event, Id, Msg, self()},
	ok.

rpc_loop(JRex) when is_pid(JRex) ->
	receive
		Msg ->
			%%io:format("... msg=~p~n", [Msg]),
			JRex ! Msg,
			rpc_loop(JRex)
	end.

manager(State) ->
	receive
		{add, Service, Pid} ->
			Old = lists:keytake(Service, 1, State),
			State2 = case Old of 
						 false ->
							 [{Service, [Pid]}];
						 {value, {Service, Values}, State1} ->
							 case lists:member(Pid, Values) of
								 true ->
									 State;
								 false ->
									 [{Service, [Pid|Values]} | State1]
							 end
					 end,
			manager(State2);
		{get, Service, From} ->
			Value = case lists:keysearch(Service, 1, State) of
						false ->
							[];
						{value, Service, Pids} ->
							Pids
					end,
			From ! Value,
			manager(State);
		stop ->
			ok;
		_ ->
			manager(State)
	end.

add_service(Service, Pid) ->
	erlide_rex_manager ! {add, Service, Pid}.

get_service_listeners(Service) ->
	erlide_rex_manager ! {get, Service},
	receive X -> X end.

notify(Service, Message) ->
	L = get_service_listeners(Service),
	[Pid ! Message || Pid <-L].
