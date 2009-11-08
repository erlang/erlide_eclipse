-module(erlide_jrpc).

%% The processes spawned from this module are not meant to support code reload,
%% because they are crucial to the functioning of the backends and 
%% careless update of code could make nothing work anymore.

-export([
		 init/1,
		 add_service/2,
		 get_service_listeners/1,
		 notify/2,
		 
		 event/2 
		]).

-include("erlide.hrl"). 

-define(MANAGER, erlide_rex_manager).

init(JPid) ->
	case whereis(?MANAGER) of
		undefined ->
			Pid = spawn(fun() -> 
								?SAVE_CALLS,
								manager([]) 
						end),
			register(?MANAGER, Pid);
		_ ->
			ok
	end,
	
	add_service(log, JPid),
	add_service(erlang_log, JPid),
	add_service(io_server, JPid),
	
    %% catch_all handler
	add_service(generic_catchall, JPid),
	
	ok.	


event(Id, Msg) ->
	Self = self(),
	spawn(fun() -> notify(Id, {event, Id, Msg, Self}) end).

manager(State) ->
	receive
		{add, Service, Pid} ->
			Old = keytake(Service, 1, State),
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
						{value, {Service, Pids}} ->
							Pids
					end,
			From ! Value,
			manager(State);
		stop ->
			ok;
		_Msg -> 
			manager(State)
	end.

add_service(Service, Pid) when is_atom(Service), is_pid(Pid) ->
	?MANAGER ! {add, Service, Pid}.

get_service_listeners(Service) when is_atom(Service) ->
	?MANAGER ! {get, Service, self()},
	receive X -> X end.

notify(Service, Message) when is_atom(Service) ->
	L = case get_service_listeners(Service) of 
			[] -> 
				get_service_listeners(generic_catchall);
			L0 -> 
				L0
		end,
	[Pid ! Message || Pid <-L],
	ok.

keytake(Key, N, L) when is_integer(N), N > 0 ->
    keytake(Key, N, L, []).

keytake(Key, N, [H|T], L) when element(N, H) == Key ->
    {value, H, lists:reverse(L, T)};
keytake(Key, N, [H|T], L) ->
    keytake(Key, N, T, [H|L]);
keytake(_K, _N, [], _L) -> false.
