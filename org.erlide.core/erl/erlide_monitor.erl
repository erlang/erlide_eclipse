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
		 get_status/0,
		 get_diff/0
		]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
				poll_interval= 30000,
				subscribers=[],
				ignored_processes=[],
				ignored_ets=[],
				old_snapshot,
				new_snapshot,
				diff
			   }).

-record(snapshot, {
				   time,
				   processes=[], 
				   ets=[], 
				   memory=[]
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

get_status() ->
	gen_server:call(?MODULE, get_status).

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
	erlang:send_after(500, ?MODULE, take_snapshot),
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
handle_call(get_status, _From, #state{new_snapshot=Snap}=State) ->
	Reply = Snap,
	{reply, Reply, State};
handle_call(get_diff, _From, #state{diff=Diff}=State) ->
	Reply = Diff,
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
handle_cast({configure, poll_interval, Val}, State) ->
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
handle_info(take_snapshot, #state{new_snapshot=Snap}=State) ->
	NewSnap = take_snapshot(State#state.ignored_processes, State#state.ignored_ets),
	Diff = diff_snapshot(Snap, NewSnap),
	
	lists:foreach(fun(Pid) -> Pid ! {?MODULE, node(), Diff} end, 
				  State#state.subscribers),
	
	Time = State#state.poll_interval,
	erlang:send_after(Time, ?MODULE, take_snapshot),
	{noreply, State#state{old_snapshot=Snap, new_snapshot=NewSnap, diff=Diff}};
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
	%% erlide_log:logp("Taking system snapshot @ ~p", [Now]),
	Procs = lists:sort([lists:sort([{'Pid', X} | erlang:process_info(X)]) || X<-processes()--IgnoredProcesses]),
	Ets = lists:sort([lists:sort([{'Id', X} | case ets:info(X) of undefined -> []; Info-> Info end]) || X<-ets:all()--IgnoredEts]),
	Mem = lists:sort(erlang:system_info(allocated_areas)),
	#snapshot{time=Now, processes=Procs, ets=Ets, memory=Mem}.


diff_snapshot(undefined, Snap) ->
	Snap;
diff_snapshot(#snapshot{time=T1, processes=P1, ets=E1, memory=M1}, 
			  #snapshot{time=T2, processes=P2, ets=E2, memory=M2}) ->
	Pdiff = diff_list_id(P1, P2, 'Pid'),
	Ediff = diff_list_id(E1, E2, 'Id'),
	Mdiff = diff_list(M1, M2),
	[{time, T1, T2}, {processes, Pdiff}, {ets, Ediff}, {memory, Mdiff}].

diff_list(Old, New) ->
	diff_list_1(Old, New, []).

diff_list_1([], [], Result) ->
	lists:reverse(Result);
diff_list_1([{K, V}|T1]=_L1, [{K, V}|T2]=_L2, Result) ->
	diff_list_1(T1, T2, Result);
diff_list_1([{K, Va, Vb}|T1]=_L1, [{K, Va, Vb}|T2]=_L2, Result) ->
	diff_list_1(T1, T2, Result);
diff_list_1([{K, V1}|T1]=_L1, [{K, V2}|T2]=_L2, Result) ->
	diff_list_1(T1, T2, [{K, {V1, V2}}|Result]);
diff_list_1([{K, V1a, V1b}|T1]=_L1, [{K, V2a, V2b}|T2]=_L2, Result) ->
	diff_list_1(T1, T2, [{K, {V1a, V2a}, {V1b, V2b}}|Result]);
diff_list_1([{_K1, _V1}|_]=L1, [{K2, V2}|T2]=_L2, Result) ->
	diff_list_1(L1, T2, [{K2, {undefined, V2}}|Result]);
diff_list_1([{_K1, _V1a, _V1b}|_]=L1, [{K2, V2a, V2b}|T2]=_L2, Result) ->
	diff_list_1(L1, T2, [{K2, {undefined, V2a}, {undefined, V2b}}|Result]).


diff_list_id(Old, New, Id) ->
	OldIds = [get_id(X, Id) || X<-Old],
	NewIds = [get_id(X, Id) || X<-New],
	AddedIds = NewIds -- OldIds,
	DeletedIds = OldIds -- NewIds,
	ModifiedIds = ((OldIds ++ NewIds) -- AddedIds) -- DeletedIds,
	Added = lists:filter(fun(X) -> lists:member(get_id(X, Id), AddedIds) end, New),
	Deleted = lists:filter(fun(X) -> lists:member(get_id(X, Id), DeletedIds) end, Old),
	ModifiedOld = lists:filter(fun(X) -> lists:member(get_id(X, Id), ModifiedIds) end, Old),
	ModifiedNew = lists:filter(fun(X) -> lists:member(get_id(X, Id), ModifiedIds) end, New),
	[{added, Added}, {deleted, Deleted}, {modified, trim_same_values(ModifiedOld, ModifiedNew, Id)}].

trim_same_values(Old, New, Id) ->
	trim_same_values(Old, New, Id, []).

trim_same_values([], [], _, Result) ->
	lists:reverse(Result);
trim_same_values([H1|Old], [H2|New], Id, Result) ->
	Fun = fun({X, _}) when X==Id -> true;
			 ({registered_name, _})-> true;
			 ({name, _})-> true;
			 (X) -> not lists:member(X, H1)
		  end,
	L = lists:filter(Fun, H2),
	LL = lists:keydelete(name, 1, lists:keydelete('Pid', 1, lists:keydelete(registered_name, 1, L))),
	Result1 = case length(LL)>0 of
				  false ->
					  Result;
				  true ->
					  [L | Result]
			  end,
	trim_same_values(Old, New, Result1).

get_id(Info, Key) ->
	{value, {Key, Id}} = lists:keysearch(Key, 1, Info),
	Id.



