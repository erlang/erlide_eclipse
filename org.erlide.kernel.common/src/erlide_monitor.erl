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
     start/2,
     stop/0,
     send_info/0
    ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
        warnLimit = 10*1000000,
        killLimit = 30*1000000
         }).

-define(GC_TIME_KILL_LIMIT, 5000).
-define(INTERVAL, 20000).

%% ====================================================================
%% External functions
%% ====================================================================

start(HeapWarnLimit, HeapKillLimit) ->
  gen_server:start({local, ?MODULE}, ?MODULE, [HeapWarnLimit, HeapKillLimit], []).

stop() ->
  gen_server:cast(?MODULE, stop).

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
init([HeapWarnLimit, HeapKillLimit]) ->
  State = #state{
        warnLimit = HeapWarnLimit*1000000,
        killLimit = HeapKillLimit*1000000
         },
  erlide_log:log({"Start monitor process: ", State#state.warnLimit, State#state.killLimit}),
  erlang:send_after(?INTERVAL, self(), notify),
  {ok, State}.

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
handle_call(_Request, _From, State) ->
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
handle_cast(_Msg, State) ->
  {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({monitor, GcPid, long_gc, Info}, State) ->
  case lists:keyfind(timeout, 1, Info) of
    {timeout, Time} when Time > ?GC_TIME_KILL_LIMIT ->
      erlide_log:log({gc_killing, GcPid, process_info(GcPid, registered_name), process_info(GcPid, heap_size)}),
%%      erlang:kill(GcPid),
      ok;
    _ ->
      ok
  end,
  {noreply, State};
handle_info({monitor, GcPid, large_heap, Info}, #state{warnLimit=WarnLimit, killLimit=KillLimit}=State) ->
  case lists:keyfind(heap_size, 1, Info) of
    {heap_size, Size} when Size > KillLimit ->
      erlide_log:log(warn, {heap_killing, GcPid, process_info(GcPid, registered_name), process_info(GcPid, heap_size)}),
%%       erlang:kill(GcPid),
      send_info(),
      ok;
    {heap_size, Size} when Size > WarnLimit ->
      erlide_log:log(warn, {heap_warning, GcPid, process_info(GcPid, registered_name), process_info(GcPid, heap_size)}),
      send_info(),
      ok;
    _ ->
      ok
  end,
  {noreply, State};
handle_info(notify, State) ->
    send_info(),
    erlang:send_after(?INTERVAL, self(), notify),
    {noreply, State};
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

all_processes_info() ->
    L = [{
          erlang:process_info(P, memory),
          erlang:process_info(P, heap_size),
          erlang:process_info(P, stack_size),
          erlang:process_info(P, total_heap_size),
          erlang:process_info(P, binary),
          {name, erlang:process_info(P, registered_name)},
          catch erlang:process_info(P, current_stacktrace),
          {pid, P}
         }
         || P <- processes()
        ],
    lists:sublist(lists:reverse(lists:sort(L)),
                  20).

send_info() ->
    PInfo =  all_processes_info(),
    MInfo = erlang:memory(),
    erlide_jrpc:event(system_status, {PInfo, MInfo, erlang:registered()}),
    %% erlide_log:logp({"PROCESSES---------------", PInfo}),
    %% erlide_log:logp({"SYSTEM------------------", MInfo}),
    ok.
