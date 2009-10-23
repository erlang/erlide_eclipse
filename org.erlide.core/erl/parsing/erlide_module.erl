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
-module(erlide_module).

-export([
		 start/1,
		 contentChange/4		 
		]).

-include("erlide.hrl"). 

%% For now we have a simple content model: a string.

-record(state, {name, content=""}).

start(Name) ->
	spawn(fun() -> 
				  ?SAVE_CALLS,
				  loop(#state{name=Name}) 
		  end).

contentChange(Pid, Offset, Length, Text) ->
	Pid ! {change, Offset, Length, Text}.


loop(State) ->
	Name = State#state.name,
	receive
		stop ->
			ok;
		{get_string_content, From} ->
			From ! {module_content, State#state.content},
			?MODULE:loop(State);
		{get_binary_content, From} ->
			From ! {module_content, list_to_binary(State#state.content)},
			?MODULE:loop(State);
		{change, Offset, Length, Text}=Msg ->
			erlide_log:logp("Module ~s:: ~p", [Name, Msg]),
			Content1 = replace_text(State#state.content, Offset, Length, Text),
			?MODULE:loop(State#state{content=Content1});
		Msg ->
			erlide_log:logp("Unknown message in module ~s: ~p", [Name, Msg]),
			?MODULE:loop(State)
	end.

replace_text(Initial, Offset, Length, Text) ->
    {A, B} = lists:split(Offset, Initial),
    {_, C} = lists:split(Length, B),
    A++Text++C.
