%%% -------------------------------------------------------------------
%%% Author  : qvladum
%%% Description :
%%%
%%% Created : 9 sep 2009
%%% -------------------------------------------------------------------
-module(erlide_monitor).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([
		 start/0, 
		 stop/0,
		 configure/1,
		 configure/2,
		 subscribe/1,
		 unsubscribe/1,
		 get_state/0,
		 get_previous_state/0,
		 get_all_diffs/0,
		 get_diff/0
		]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
				poll_interval= 300000,
				subscribers=[],
				ignored_processes=[],
				ignored_ets=[],
				old_snapshot,
				new_snapshot,
				diffs=[]
			   }).

-record(snapshot, {
				   time,
				   processes=[], 
				   ets=[], 
				   memory=[],
				   stats=[]
				  }).

%% ====================================================================
%% External functions
%% ====================================================================

start() ->
	gen_server:start({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:cast(?MODULE, stop).

subscribe(Pid) ->
	gen_server:cast(?MODULE, {subscribe, Pid}).

unsubscribe(Pid) ->
	gen_server:cast(?MODULE, {unsubscribe, Pid}).

configure(Options) when is_list(Options) ->
	[gen_server:cast(?MODULE, {configure, K, V}) || {K, V}<-Options].

configure(Key, Val) ->
	gen_server:cast(?MODULE, {configure, Key, Val}).

get_state() ->
	gen_server:call(?MODULE, get_state).

get_previous_state() ->
	gen_server:call(?MODULE, get_previous_state).

get_all_diffs() ->
	gen_server:call(?MODULE, get_all_diffs).

get_diff() ->
	gen_server:call(?MODULE, get_diff).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
	erlang:send_after(1000, ?MODULE, take_snapshot),
	{ok, #state{
				ignored_processes=processes(), 
				ignored_ets=ets:all()
			   }}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(get_state, _From, #state{old_snapshot=Snap}=State) ->
	Reply = Snap,
	{reply, Reply, State};
handle_call(get_previous_state, _From, #state{new_snapshot=Snap}=State) ->
	Reply = Snap,
	{reply, Reply, State};
handle_call(get_all_diffs, _From, #state{diffs=Diffs}=State) ->
	Reply = Diffs,
	{reply, Reply, State};
handle_call(get_diff, _From, #state{diffs=Diffs}=State) ->
	Reply = case Diffs of 
				[] -> [];
				[H|_] -> H
			end,
	{reply, Reply, State};
handle_call(Request, From, State) ->
	erlide_log:logp("monitor:: unrecognized call: ~p from ~p", [Request, From]),	
	Reply = ok,
	{reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(stop, State) ->
	{stop, normal, State};
handle_cast({subscribe, Pid}, #state{subscribers=Subs}=State) ->
	Subs1 = case lists:member(Pid, Subs) of
				true -> Subs;
				false -> [Pid | Subs]
			end,
	{noreply, State#state{subscribers=Subs1}};
handle_cast({unsubscribe, Pid}, #state{subscribers=Subs}=State) ->
	Subs1 = lists:delete(Pid, Subs),
	{noreply, State#state{subscribers=Subs1}};
handle_cast({configure, poll_interval, Val}, State) when is_integer(Val) ->
	{noreply, State#state{poll_interval=Val}};
handle_cast({configure, Key, Val}, State) ->
	erlide_log:logp("monitor:: unrecognized configure option: ~p", [{Key, Val}]),	
	{noreply, State};
handle_cast(Msg, State) ->
	erlide_log:logp("monitor:: unrecognized cast: ~p", [Msg]),	
	{noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(take_snapshot, #state{subscribers=[]}=State) ->
    {noreply, State};
handle_info(take_snapshot, #state{new_snapshot=Snap, diffs=Diffs}=State) ->
	NewSnap = take_snapshot(State#state.ignored_processes, State#state.ignored_ets),
	Diff = diff_snapshot(Snap, NewSnap),
	State1 = case is_empty_diff(Diff) of
				 true ->
					 State;
				 false ->
					 lists:foreach(fun(Pid) -> Pid ! {?MODULE, node(), Diff} end, 
								   State#state.subscribers),
					 State#state{old_snapshot=Snap, new_snapshot=NewSnap, diffs=[Diff|Diffs]}
			 end,
	
	Time = State#state.poll_interval,
	erlang:send_after(Time, ?MODULE, take_snapshot),
	{noreply, State1};
handle_info(Info, State) ->
	erlide_log:logp("monitor:: unrecognized message: ~p", [Info]),	
	{noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, _State) ->
	erlide_log:logp("Monitor: terminated!!"),
	erlide_log:logp("Reason ~p", [Reason]),
	ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

take_snapshot(IgnoredProcesses, IgnoredEts) ->
	Now = calendar:local_time(),
	%%erlide_log:logp("Taking system snapshot @ ~p", [Now]),
	Procs = lists:sort([lists:sort([{'Pid', X} | pinfo(X)]) || X<-processes()--IgnoredProcesses]),
	Ets = lists:sort([lists:sort([{'Id', X} | einfo(X)]) || X<-ets:all()--IgnoredEts]),
	Mem = lists:sort(erlang:system_info(allocated_areas)),
	Stats = [{X, erlang:statistics(X)} || X<-[context_switches, io, reductions, run_queue, runtime, wall_clock]],
	#snapshot{time=Now, processes=clean(Procs), ets=clean(Ets), memory=Mem, stats=Stats}.

einfo(X) ->
	case ets:info(X) of 
		undefined -> []; 
		Info-> Info 
	end.

diff_snapshot(undefined, #snapshot{time=T, processes=P, ets=E, memory=M, stats=S}) ->
	[{time, T}, 
	 {processes, [{added, P}]}, 
	 {ets, [{added, E}]}, 
	 {memory, [{added, M}]},
	 {stats, [{added, S}]}];
diff_snapshot(#snapshot{processes=P1, ets=E1, memory=M1, stats=S1}, 
			  #snapshot{processes=P2, ets=E2, memory=M2, stats=S2, time=T}) ->
	Pdiff = diff_list_id(P1, P2, ['Pid', registered_name]),
	Ediff = diff_list_id(E1, E2, ['Id', name]),
	Mdiff = diff_list(M1, M2),
	Sdiff = diff_list(S1, S2),
	[{time, T}, {processes, Pdiff}, {ets, Ediff}, {memory, Mdiff}, {stats, Sdiff}].

diff_list(Old, New) ->
	diff_list_1(Old, New, []).

diff_list_1([], [], Result) ->
	lists:reverse(Result);
diff_list_1([{K, V}|T1], [{K, V}|T2], Result) ->
	diff_list_1(T1, T2, Result);
diff_list_1([{K, V1, V2}|T1], [{K, V1, V2}|T2], Result) ->
	diff_list_1(T1, T2, Result);
diff_list_1([{K, _}|T1], [{K, V2}|T2], Result) ->
	diff_list_1(T1, T2, [{K, V2}|Result]);
diff_list_1([{K, _, _}|T1], [{K, V2a, V2b}|T2], Result) ->
	diff_list_1(T1, T2, [{K, V2a, V2b}|Result]);
diff_list_1(L1, [H2|T2], Result) ->
	diff_list_1(L1, T2, [H2|Result]).


diff_list_id(Old, New, Ids) ->
	Id = hd(Ids),
	
	OldIds = [get_id(X, Id) || X<-Old],
	NewIds = [get_id(X, Id) || X<-New],
	AddedIds = NewIds -- OldIds,
	DeletedIds = OldIds -- NewIds,
	ModifiedIds = ((OldIds ++ NewIds) -- AddedIds) -- DeletedIds,
	
	Added = filter(Id, AddedIds, New), 
	ModifiedOld = filter(Id, ModifiedIds, Old),
	ModifiedNew = filter(Id, ModifiedIds, New),
	
	R0=[],
	R1=case trim_same_values(ModifiedOld, ModifiedNew, Ids) of 
		   [] -> R0;
		   Modded -> [{modified, Modded} | R0]
	   end,
	R2=case DeletedIds of 
		   [] -> R1;
		   _ -> [{deleted, [{Id, X} || X<-DeletedIds]} | R1]
	   end,
	R3=case Added of 
		   [] -> R2;
		   _ -> [{added, Added} | R2]
	   end,
	R3. 

filter(Id, L1, L2) ->
	lists:filter(fun(X) -> lists:member(get_id(X, Id), L1) end, L2).

trim_same_values(Old, New, Ids) ->
	trim_same_values(Old, New, Ids, []).

trim_same_values([], [], _, Result) ->
	lists:reverse(Result);
trim_same_values([H1|Old], [H2|New], Ids, Result) ->
	Result1 = trim_values(H1, H2, Ids, Result),
	trim_same_values(Old, New, Ids, Result1).

trim_values(H1, H2, Ids, Result) ->
	Fun = fun({X, _}=Y) ->
				  case lists:member(X, Ids) of
					  true -> true;
					  false -> not lists:member(Y, H1)
				  end
		  end,
	L = lists:filter(Fun, H2),
	Result1 = case length(L) > length(Ids) of
				  true ->
					  [L | Result];
				  false ->
					  Result
			  end,
	Result1.

get_id(Info, Key) ->
	{value, {Key, Id}} = lists:keysearch(Key, 1, Info),
	Id.

is_empty_diff([{time, _}, 
			   {processes, []}, 
			   {ets, []}, 
			   {memory, []},
			   {stats, _}]) ->
	true;
is_empty_diff(_) ->
	false.

pinfo(Pid) when is_pid(Pid) ->
	R0 = erlang:process_info(Pid),
	%% 	[{backtrace, BT}] = info(Pid, backtrace),
	%% 	BT1 = case is_binary(BT) of
	%% 			  true -> string:tokens(binary_to_list(BT), "\n");
	%% 			  false -> BT
	%% 		  end,
	LC = info(Pid, last_calls),
	M = info(Pid, memory),
	lists:append([R0, 
				  %%[{backtrace, BT1}], 
				  LC, M]).

info(Pid, Key) ->
	try 
		[erlang:process_info(Pid, Key)] 
	catch 
		_:_ -> [] 
	end.

clean(Pid) when is_pid(Pid) -> 
	{'$pid', pid_to_list(Pid)};
clean(L) when is_list(L) ->
	[clean(X) || X<-L];
clean(T) when is_tuple(T) ->
	list_to_tuple([clean(X) || X<-tuple_to_list(T)]);
clean(X) ->
	X.




