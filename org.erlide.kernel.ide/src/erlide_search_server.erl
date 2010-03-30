%% Description: Search server
%% Author: jakob (jakobce at g mail dot com)
%% Created: 10 mar 2010

-module(erlide_search_server).

%%
%% Include files
%%

%% -define(DEBUG, 1).

-include("erlide.hrl").
-include("erlide_scanner.hrl").

-include_lib("kernel/include/file.hrl").

-include("erlide_search_server.hrl").

%%
%% Exported Functions
%%


%% called from Java
-export([start/0, 
         stop/0,
         %% add_modules/1,
         find_refs/3]).

%% called from Erlang
-export([remove_module/1,
         add_module_refs/2]).

%% for testing

-export([state/0, modules/0]).

%%
%% Internal Exports
%%

-export([loop/1]).

%%
%% Macros and Records
%%

-define(SERVER, erlide_search_server).

-record(state, {modules=[], dummy}). %% FIXME still too simple data model
-record(module, {scanner_name, refs}).

%%
%% API Functions
%%

start() ->
    start(whereis(?SERVER)).

stop() ->
    server_cmd(stop).

%% add_modules(Modules) ->
%%     R = server_cmd(add_modules, Modules),
%%     ?D(state()),
%%     R.

state() ->
    server_cmd(state).

modules() ->
    server_cmd(modules).

%% modules is {ScannerName, ModulePath}
find_refs(Pattern, Modules, StateDir) 
  when is_tuple(Pattern), is_list(Modules), is_list(StateDir) ->
    find_refs([Pattern], Modules, StateDir);
find_refs(Pattern, Modules, StateDir) 
  when is_list(Pattern), is_list(Modules), is_list(StateDir) ->
    ?D(Pattern),
    R = server_cmd(find_refs, {Pattern, Modules, StateDir}),
    ?D(R),
    R.

remove_module(ScannerName) ->
    server_cmd(remove_module, ScannerName).

add_module_refs(ScannerName, Refs) ->
    server_cmd(add_module_refs, {ScannerName, Refs}).

%%
%% Local Functions
%%

start(undefined) ->
    Self = self(),
    spawn(fun() ->
                  ?SAVE_CALLS,
                  erlang:yield(),
                  erlang:register(?SERVER, self()),
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

server_cmd(Command) ->
    server_cmd(Command, []).

server_cmd(Command, Args) ->
    start(),
    try
        ?SERVER ! {Command, self(), Args},
        receive
            {Command, _Pid, Result} ->
                Result
        end
    catch
        _:Exception ->
              {error, Exception}
    end.


loop(State) ->
    ?D(State),
    receive
        {stop, From, []} ->
            reply(stop, From, stopped);
        {Cmd, From, Args} ->
            ?D(Cmd),
            NewState = cmd(Cmd, From, Args, State),
            ?D(NewState),
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

do_cmd(add_module_refs, {Module, Refs}, State) ->
    do_add_module_refs(Module, Refs, State);
do_cmd(find_refs, {Ref, Modules, StateDir}, State) ->
    ?D(Ref),
    ?D(Modules),
    ?D(State),
    R = do_find_refs(Modules, Ref, StateDir, State, []),
    ?D(R),
    R;
do_cmd(remove_module, Module, #state{modules=Modules0} = State) ->
    Modules1 = lists:keydelete(Module, #module.scanner_name, Modules0),
    State#state{modules=Modules1};
do_cmd(state, _, State) ->
    {State, State};
do_cmd(modules, _, #state{modules=Modules} = State) ->
    Names = [M || #module{scanner_name=M} <- Modules],
    {Names, State}.

do_find_refs([], _, _, State, Acc) ->
    {{ok, Acc}, State};
do_find_refs([{ScannerName, ModulePath} | Rest], Pattern, StateDir, 
             #state{modules=Modules} = State, Acc0) ->
    ?D(ScannerName),
    Refs = get_module_refs(ScannerName, ModulePath, StateDir, Modules),
    Mod = get_module_name(ModulePath),
    Acc1 = find_data(Refs, Pattern, Mod, ModulePath, Acc0),
    ?D(Acc1),
    do_find_refs(Rest, Pattern, StateDir, State, Acc1).

get_module_name(ModulePath) ->
    L = filename:rootname(filename:basename(ModulePath)),
    list_to_atom(L).

find_data([], _, _, _, Acc) ->
    Acc;
find_data([#ref{function=F, arity=A, clause=C, data=D, offset=O, length=L, sub_clause=S} | Rest],
          Pattern, Mod, M, Acc) ->
    NewAcc = case check_pattern(Pattern, Mod, D) of
                 true ->
                     [{M, F, A, C, S, O, L} | Acc];
                 false ->
                     Acc
             end,
    find_data(Rest, Pattern, Mod, M, NewAcc).

check_pattern(Pattern, Mod, #local_call{function=F, arity=A})->
    lists:member(#external_call{module=Mod, function=F, arity=A}, Pattern);
check_pattern(Pattern, _Mod, D) ->
    lists:member(D, Pattern).

get_module_refs(ScannerName, ModulePath, StateDir, Modules) ->
    ?D(Modules),
    case lists:keysearch(ScannerName, #module.scanner_name, Modules) of
        {value, #module{refs=Refs}} ->
            ?D(ye),
            Refs;
        false ->
            ?D(ok),
            read_module_refs(ScannerName, ModulePath, StateDir)
    end.

read_module_refs(ScannerName, ModulePath, StateDir) ->
    erlide_noparse:read_module_refs(ScannerName, ModulePath, StateDir).

do_add_module_refs(Module, Refs, #state{modules=Modules0} = State) ->
    ?D(Module),
    Modules1 = lists:keydelete(Module, #module.scanner_name, Modules0),
    Modules2 = [#module{scanner_name=Module, refs=Refs} | Modules1],
    ?D(Modules1),
    ?D(Modules2),
    State#state{modules=Modules2}.
