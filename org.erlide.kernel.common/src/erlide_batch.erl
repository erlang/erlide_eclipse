%%% ******************************************************************************
%%%  Copyright (c) 2009 Vlad Dumitrescu and others.
%%%  All rights reserved. This program and the accompanying materials
%%%  are made available under the terms of the Eclipse Public License v1.0
%%%  which accompanies this distribution, and is available at
%%%  http://www.eclipse.org/legal/epl-v10.html
%%%
%%%  Contributors:
%%%      Vlad Dumitrescu
%%% ******************************************************************************/
%% Author: Vlad
%% Created: 7 okt 2009
%% Description: A server that processes requests in a pool of worker processes.
%%   It's a simple implementation, do not use for large pools.

-module(erlide_batch).

%%
%% Include files
%%

%%-define(DEBUG, 1).

-include("erlide.hrl").

%%
%% Exported Functions
%%
-export([
     start/1,
     start/2,
     call/3
    ]).

-record(state, {queue = queue:new(), max=1, crt=0}).

start(Name) when is_atom(Name) ->
  Max = erlang:system_info(schedulers),
  start(Name, Max).

start(Name, {multiplier, Multi}) when is_atom(Name), is_integer(Multi), Multi>0 ->
  Max = erlang:system_info(schedulers)*Multi,
  start(Name, Max);
start(Name, Max) when is_atom(Name), is_integer(Max) ->
  Pid = spawn(fun() ->
            loop(#state{max=Max})
        end),
  register(Name, Pid),
  ok.

call(Name, Fun, Args) when is_atom(Name) ->
  Name ! {call, self(), Fun, Args},
  receive
    Result ->
      Result
  end.


%%
%% Local Functions
%%

loop(#state{}=State) ->
  receive
    done ->
      do_work(State);
    {call, _From, _Fun, _Args} = Msg ->
      queue_work(State, Msg);
    _Other ->
      loop(State)
  end.

do_work(#state{queue=Queue, crt=N}=State) ->
  case queue:out(Queue) of
    {{value, {call, From, Fun, Args}}, Queue2} ->
      spawn_worker(From, Fun, Args),
      loop(State#state{queue=Queue2});
    _ ->
      loop(State#state{crt=N-1})
  end.

queue_work(#state{queue=Queue, crt=N, max=Max}=State, {call, From, Fun, Args}=Msg) ->
  case N < Max of
    true ->
      spawn_worker(From, Fun, Args),
      loop(State#state{crt=N+1});
    false ->
      loop(State#state{queue=queue:in(Msg, Queue)})
  end.


spawn_worker(From, Fun, Args) ->
  Server = self(),
  Pid = spawn(fun()-> worker(Server, From, Fun, Args) end),
  Pid.

worker(Server, From, Fun, Args) ->
  %%erlide_log:logp("--- %%$$ CALL  ~p", [hd(Args)]),
  Result = (catch apply(Fun, Args)),
  From ! Result,
  Server ! done,
  %%erlide_log:logp("--- %%$$ OK  ~p", [hd(Args)]),
  ok.
