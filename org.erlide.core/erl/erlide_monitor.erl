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
		 get_status/0,
		 get_statistics/0,
		 get_last_diff/0
		]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
				poll_interval= 5000,
				subscribers=[],
				snapshots=[],
				diffs=[]
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

configure(Options) when is_list(Options) ->
	gen_server:call(?MODULE, {configure, Options}).

configure(Key, Val) ->
	gen_server:call(?MODULE, {configure, Key, Val}).

get_status() ->
	gen_server:call(?MODULE, get_status).

get_statistics() ->
	gen_server:call(?MODULE, get_statistics).

get_last_diff() ->
	gen_server:call(?MODULE, get_last_diff).

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
	{ok, #state{}}.

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
handle_call({configure, Options}, From, State) ->
	Reply = ok,
	{reply, Reply, State};
handle_call({configure, Key, Val}, From, State) ->
	Reply = ok,
	{reply, Reply, State};
handle_call(get_status, From, #state{snapshots=Snaps}=State) ->
	Reply = case Snaps of
				[] ->
					none;
				[H | _] ->
					H
			end,
	{reply, Reply, State};
handle_call(get_statistics, From, State) ->
	Reply = State,
	{reply, Reply, State};
handle_call(get_last_diff, From, #state{diffs=Diffs}=State) ->
	Reply = case Diffs of
				[] ->
					none;
				[H | _] ->
					H
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
handle_info(take_snapshot, #state{snapshots=Snaps, diffs=Diffs}=State) ->
	Snap = take_snapshot(),
	Snaps1 = [Snap | Snaps],
	
	Diffs1 = case Snaps of
				 [H | _] ->
					 Diff = diff_snapshot(H, Snap),
					 erlide_log:logp("DIFF: ~p", [Diff]),
					 [Diff | Diffs];
				 _ ->
					 Diffs
			 end,
	
	Time = State#state.poll_interval,
	erlang:send_after(Time, ?MODULE, take_snapshot),
	erlide_log:logp(">> snapshot done!"),
	{noreply, State#state{snapshots=Snaps1, diffs=Diffs1}};
handle_info(Info, State) ->
	erlide_log:logp("monitor:: unrecognized info: ~p~n", [Info]),	
	{noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
	erlide_log:logp("Monitor: terminated!"),
	%%erlide_log:logp("Reason ~p", [Reason]),
	ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
	{ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

take_snapshot() ->
	Now = calendar:local_time(),
	erlide_log:logp("Taking system snapshot @ ~p", [Now]),
	Procs = lists:sort([lists:sort(erlang:process_info(X)) || X<-processes()]),
	Ets = lists:sort([lists:sort(ets:info(X)) || X<-ets:all()]),
	Mem = lists:sort(erlang:system_info(allocated_areas)),
	#snapshot{time=Now, processes=Procs, ets=Ets, memory=Mem}.


diff_list(Old, New) ->
	diff_list_1(Old, New, []).

diff_list_1([], [], Result) ->
	lists:reverse(Result);
diff_list_1([{K, V}|T1]=_L1, [{K, V}|T2]=_L2, Result) ->
	diff_list_1(T1, T2, Result);
diff_list_1([{K, Va, Vb}|T1]=_L1, [{K, Va, Vb}|T2]=_L2, Result) ->
	diff_list_1(T1, T2, Result);
diff_list_1([{K, V1}|T1]=_L1, [{K, V2}|T2]=_L2, Result) ->
	diff_list_1(T1, T2, [{K, V1, V2}|Result]);
diff_list_1([{K, V1a, V1b}|T1]=_L1, [{K, V2a, V2b}|T2]=_L2, Result) ->
	diff_list_1(T1, T2, [{K, V1a, V1b, V2a, V2b}|Result]);
diff_list_1([{_K1, _V1}|_]=L1, [{K2, V2}|T2]=_L2, Result) ->
	diff_list_1(L1, T2, [{K2, undefined, V2}|Result]);
diff_list_1([{_K1, _V1a, _V1b}|_]=L1, [{K2, V2a, V2b}|T2]=_L2, Result) ->
	diff_list_1(L1, T2, [{K2, undefined, undefined, V2a, V2b}|Result]).

x_diff_list(L1, L2) ->
	{A, D, M} = split_diff(L1, L2),
	{A, D, M}.

x_diff_list_1([], [], New, Dead, Modified) ->
	{New, Dead, Modified};
x_diff_list_1([], X, New, Dead, Modified) ->
	{New++X, Dead, Modified};
x_diff_list_1(X, [], New, Dead, Modified) ->
	{New, Dead++X, Modified};
x_diff_list_1([H1|T1], [H2|T2], New, Dead, Modified) ->
	%% NOT FINISHED
	x_diff_list_1(T1, T2, New, Dead, Modified).

split_diff(Old, New) ->
	{added, deleted, trim_same_values(modified_old, modified_new)}.

trim_same_values(Old, New) ->
	New.

diff_snapshot(#snapshot{processes=P1, ets=E1, memory=M1}, #snapshot{time=T, processes=P2, ets=E2, memory=M2}) ->
	Pdiff = [], %x_diff_list(P1, P2),
	Ediff = x_diff_list(E1, E2),
	Mdiff = diff_list(M1, M2),
	#snapshot{time=T, processes=Pdiff, ets=Ediff, memory=Mdiff}.



