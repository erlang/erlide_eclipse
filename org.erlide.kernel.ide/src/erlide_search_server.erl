%% Author: jakob (jakobce at g mail dot com)
%% Created: 10 mar 2010
%% Description: 

-module(erlide_search_server).

%%
%% Include files
%%

%% -define(DEBUG, 1).

-include("erlide.hrl").
-include("erlide_scanner.hrl").

%%
%% Exported Functions
%%

-export([start/0, 
         stop/0,
         add_modules/1]).

%%
%% Internal Exports
%%

-export([loop/1]).

%%
%% Macros and Records
%%

-define(SERVER, erlide_search_server).
-record(state, {modules=[], refs = []}). %% FIXME overly simple data model

%%
%% API Functions
%%

start() ->
    start(whereis(?SERVER)).

stop() ->
    server_cmd(stop).
    
%%
%% Local Functions
%%

start(undefined) ->
    Self = self(),
    spawn(fun() ->
                  ?SAVE_CALLS,
                  erlang:yield(),
                  Self ! started,
                  loop(#state{})
          end),
    receive
        started ->
            ok
    after 10000 ->
            {error, timeout_waiting_for_search_server}
    end;
start(_) ->
    ok.

add_modules(Modules) ->
    server_cmd(add_modules, Modules).

server_cmd(Command) ->
    server_cmd(Command, []).

server_cmd(Command, Args) ->
    try
        ?SERVER ! {Command, self(), Args},
        receive
            {Command, _Pid, Result} ->
                Result
        end
    catch _:Exception ->
              {error, Exception}
    end.


loop(State) ->
    receive
        {stop, From, []} ->
            reply(stop, From, stopped);
        {Cmd, From, Args} ->
            NewState = cmd(Cmd, From, Args, State),
            ?MODULE:loop(NewState)
    end.

cmd(Cmd, From, Args, State) ->
    try
        case get(logging) of
            on ->
                put(log, get(log)++[{Cmd, Args}]);
            _ ->
                ok
        end,
        case do_cmd(Cmd, Args, State) of
            {R, NewState} ->
                reply(Cmd, From, R),
                NewState;
            ok ->
                reply(Cmd, From, ok),
                State;
            NewState ->
                reply(Cmd, From, ok),
                NewState
        end
    catch
        exit:Error ->
            reply(Cmd, From, {exit, Error}),
            State;
        error:Error ->
            reply(Cmd, From, {error, Error}),
            State
    end.

reply(Cmd, From, R) ->
    From ! {Cmd, self(), R}.

do_cmd(add_modules, Modules, State) ->
    ?D({add_modules, Modules})
    do_add_modules(Modules, State).

do_add_modules([], State) ->
    State;
do_add_modules([Module | Rest], State = #state{refs = Refs, modules = Modules}) ->
    MoreRefs = scan_for_refs(Module),
    State#state{refs = MoreRefs ++ Refs, modules = [Module | Modules]}.

scan_for_refs(Module) when is_list(Module) ->
    Tree = erlide_epp_dodger:parse_file(Module),
        ok.



