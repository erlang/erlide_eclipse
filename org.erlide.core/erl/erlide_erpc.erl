%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%%     $Id$
%%

%%%----------------------------------------------------------------------
%%% File    : erlide_erpc.erl
%%% Author  : ottuser local account <otpuser@tiger>
%%% Purpose : Accept a tcp/ip connection and then handle in and ivr protocols
%%% Created : 26 May 1999 by ottuser local account <otpuser@tiger>
%%%----------------------------------------------------------------------
%%% This gen_server exists for the life of a socket connection.
%%% It is spawned by tcp_listen, which sends us it's own PID
%%% so we can ask it to set up a new one of these if/when this one accepts
%%% a socket connection.
%%%----------------------------------------------------------------------
%%% Modified by Vlad Dumitrescu for erlide
%%%----------------------------------------------------------------------

-module(erlide_erpc).

-author('shinde@one2one.co.uk').

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start/2, start/4, worker/6]).

-behaviour(gen_server).

-include_lib("kernel/include/inet.hrl").

% Internal state for this socket process
-record(state,{
	       socket = undefined,	% Socket ref
	       allow = all,		% Allowed modules/functions
	       from = all,		% List of allowed hostnames/addresses
	       allowed_host = true,	% Whether this socket is from an allowed host
	       secret = null}).		% Shared Secret

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
%% Secret is either 'false' or a binary.
%% Allow is either 'all' or a list of allowed modules: [Mod|{Mod,Func}]
start(Port, Secret) ->
    start(Port, Secret, all, all).

start(Port, Secret, Allow, From) ->
    gen_server:start_link(?MODULE, {Port, Secret, Allow, From},[]).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init({Port, Secret, Allow, From}) ->
	{ok, S} = gen_tcp:connect("localhost", Port,
		[binary, {packet,4}, {active,true}]),
    {ok, #state{
    	socket = S,
		allow = Allow,
		from = From,
		secret = format_secret(Secret)}}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call(_Request,_From,State) ->
    {reply,ok,State}.


%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(_Reply ,State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info({tcp, _Socket, _Packet}, State) when State#state.allowed_host == false ->
    {noreply, State};				% Just ignore all commands.
handle_info({tcp, Socket, Packet}, State) ->
    case catch binary_to_term(Packet) of
	{heartbeat, Ref, Checksum} ->
	    case check(Ref, State#state.secret, Checksum) of
		true ->
		    gen_tcp:send(Socket, term_to_binary({heart_reply, Ref})),
		    {noreply, State};
		false ->
		    {noreply, State}
	    end;

	%%% TODO remove this clause...
	{apply, M, F, A, Ref, Checksum} ->
	    case check({M, F, A, Ref}, State#state.secret, Checksum) of
		true ->
		    case check_mod(M, F, State#state.allow) of
			allow ->
			    _Pid = spawn_link(?MODULE, worker, [M, F, A, Ref, infinite, Socket]),
			    {noreply, State};
			forbid ->
			    {noreply, State}
		    end;
		false ->
		    {noreply, State}
	    end;
	%%%

	{apply, M, F, A, Timeout, Ref, Checksum} ->
	    case check({M, F, A, Ref}, State#state.secret, Checksum) of
		true ->
		    case check_mod(M, F, State#state.allow) of
			allow ->
			    _Pid = spawn_link(?MODULE, worker, [M, F, A, Ref, Timeout, Socket]),
			    {noreply, State};
			forbid ->
			    {noreply, State}
		    end;
		false ->
		    {noreply, State}
	    end;
	Else ->
	    io:format("Socket Received Else: ~p~n",[Else]),
	    {noreply, State}
    end;

handle_info({tcp_closed, _Socket}, State) ->
    {stop, erlide_erpc_skt_closed, State};

handle_info({tcp_error, _Socket, _Reason}, State) ->
    gen_tcp:close(State#state.socket),
    {stop, erlide_erpc_skt_error, State};


handle_info(_Anymessage, State) ->
    {noreply, State}.


%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(_Reason, #state{socket = undefined}) ->
    ok;
terminate(_Reason, #state{socket = Socket}) ->
    gen_tcp:close(Socket),
    ok.

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%% Spawned to do the work being asked for. Should normally exit with
%% reason normal so we don't upset the main gen_server.
worker(M, F, A, Ref, _Timeout, Socket) ->
	%% TODO handle timeout
    Reply = (catch apply(M, F, A)),
    %%io:format("erpc Reply: ~p~n", [Reply]),
    ok = gen_tcp:send(Socket, term_to_binary({reply, Ref, Reply})).


check(_Ref, false, _Checksum) ->
    true;
check(Ref, Secret, Checksum) ->
    Checksum == erlang:md5(concat_binary([term_to_binary(Ref), Secret])).

%% List is a list containing either {M,F} or just M (or all to allow any)
%% If there is a M which matches, it overides any {M, F}
check_mod(_M, _F, all) ->
    allow;
check_mod(M, F, [{M, F}|_T]) ->
    allow;
check_mod(M, _F, [M|_T]) ->
    allow;
check_mod(M, F, [_H|T]) ->
    check_mod(M, F, T);
check_mod(_M, _F, []) ->
    forbid.

format_secret(Secret) when list(Secret) ->
    list_to_binary(Secret);
format_secret(false) ->
    false.
