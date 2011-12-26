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
         find_refs/3,
         start_find_refs/4,
         cancel_find_refs/1]).

%% called from Erlang
-export([remove_module/1,
         add_module_refs/2,
         check_pattern/6]).


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

-record(state, {modules=[], dummy}). %% FIXME still too simple data mode
-record(module, {scanner_name, module_name, refs}).

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
    R = server_cmd(find_refs, {Pattern, Modules, StateDir}),
    R.

%% 
start_find_refs(JPid, Pattern, Modules, StateDir)
  when is_tuple(Pattern), is_list(Modules), is_list(StateDir) ->
    start_find_refs(JPid, [Pattern], Modules, StateDir); 
start_find_refs(JPid, Pattern, Modules, StateDir)
  when is_list(Pattern), is_list(Modules), is_list(StateDir) ->
    ?D({JPid, Pattern}),
    R = server_cmd(start_find_refs, {Pattern, Modules, JPid, StateDir}),
    R.

remove_module(ScannerName) ->
    server_cmd(remove_module, ScannerName).

add_module_refs(ScannerName, Refs) ->
    server_cmd(add_module_refs, {ScannerName, Refs}).

cancel_find_refs(Pid) ->
    server_cmd(cancel_find_refs, Pid).

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
              {error, Exception, erlang:get_stacktrace()}
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
            reply(Cmd, From, {exit, Error, erlang:get_stacktrace()}),
            State;
        error:Error ->
            reply(Cmd, From, {error, Error, erlang:get_stacktrace()}),
            State
    end.

reply(Cmd, From, R) ->
    From ! {Cmd, self(), R}.

do_cmd(add_module_refs, {ScannerName, Refs}, State) ->
    do_add_module_refs(ScannerName, Refs, State);
do_cmd(find_refs, {Ref, Modules, StateDir}, State) ->
    ?D(Ref),
    ?D(Modules),
    ?D(State),
    R = do_find_refs(Modules, Ref, StateDir, State, []),
    ?D(R),
    R;
do_cmd(start_find_refs, {Pattern, Modules, JPid, StateDir}, State) ->
    ?D(start_find_refs),
    R = do_start_find_refs(Pattern, Modules, JPid, StateDir, State),
    R;
do_cmd(cancel_find_refs, Pid, State) ->
    Pid ! cancel,
    {stopped, State};
do_cmd(remove_module, Module, #state{modules=Modules0} = State) ->
    Modules1 = lists:keydelete(Module, #module.scanner_name, Modules0),
    State#state{modules=Modules1};
do_cmd(state, _, State) ->
    {State, State};
do_cmd(modules, _, #state{modules=Modules} = State) ->
    Names = [M || #module{scanner_name=M} <- Modules],
    {Names, State}.

do_start_find_refs(Pattern, Modules, JPid, StateDir, State) ->
    ?D({do_start_find_refs, Pattern, JPid}),
    Pid = spawn_link(fun() ->
                             ModuleChunks = chunkify(Modules, 10),
                             ?D({JPid, length(ModuleChunks)}),
                             JPid ! {start, length(ModuleChunks)},
                             ?D({JPid, length(ModuleChunks)}),
                             R = do_background_find_refs(ModuleChunks, Pattern, JPid, StateDir, State),
                             ?D({stop, R}),
                             JPid ! {stop, R}
                     end),
    {Pid, State}.

do_background_find_refs([], _Pattern, _JPid, _StateDir, _State) ->
    ok;
do_background_find_refs([Chunk | Rest], Pattern, JPid, StateDir, State) ->
    {R, _State} = do_find_refs(Chunk, Pattern, StateDir, State, []),
    ?D({1, R}),
    JPid ! {progress, {self(), 1, R}},
    receive
        cancel -> 
            ok
    after 0 ->
            do_background_find_refs(Rest, Pattern, JPid, StateDir, State)
    end.

chunkify(List, N) ->
    chunkify(List, N, []).

chunkify([], _, Acc) ->
    lists:reverse(Acc);
chunkify(List, N, Acc) ->
    {Chunk, Rest} = chunkify_aux(List, N, []),
    chunkify(Rest, N, [Chunk | Acc]).

chunkify_aux([], _, Acc) ->
    {lists:reverse(Acc), []};
chunkify_aux(List, 0, Acc) ->
    {lists:reverse(Acc), List};
chunkify_aux([H | T], N, Acc) ->
    chunkify_aux(T, N-1, [H | Acc]).

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
    NewAcc = case check_pattern(Pattern, Mod, D, F, A, C) of
                 true ->
                     [{M, F, A, C, S, O, L, is_def(D)} | Acc];
                 false ->
                     Acc
             end,
    find_data(Rest, Pattern, Mod, M, NewAcc).

is_def(#function_def{}) -> true;
is_def(#macro_def{}) -> true;
is_def(#type_def{}) -> true;
is_def(#module_def{}) -> true;
is_def(#var_def{}) -> true;
is_def(#record_field_def{}) -> true;
is_def(_) -> false.

check_pattern(Pattern, Mod, #local_call{function=F, arity=A}, _, _, _)->
    check_function_ref(#external_call{module=Mod, function=F, arity=A}, Pattern);
check_pattern(Pattern, Mod, #function_def{function=F, arity=A} = FD, _, _, _)->
    check_function_ref(FD, Pattern) orelse
        check_function_ref(#function_def_mod{module=Mod, function=F, arity=A}, Pattern);
check_pattern(Pattern, Mod, #type_ref{module='_', type=T}, _, _, _) ->
    lists:member(#type_ref{module=Mod, type=T}, Pattern);
check_pattern(Pattern, _Mod, #var_ref{}=VR, F, A, C) ->
	check_var_pattern(Pattern, VR, F, A, C);
check_pattern(Pattern, _Mod, #var_def{}=VD, F, A, C) ->
	check_var_pattern(Pattern, VD, F, A, C);
check_pattern(Pattern, _Mod, D, _, _, _) ->
%%     ?D({check_pattern, Pattern, D}),
    lists:member(D, Pattern).

check_function_ref(_, []) ->
    false;
check_function_ref(#external_call{module=Mod, function=F, arity=A1}, [#external_call{module=Mod, function=F, arity=A2}|_]) ->
    A1==A2 orelse A2==undefined;
check_function_ref(#function_def{function=F, arity=A1}, [#function_def{function=F, arity=A2}|_]) ->
    A1==A2 orelse A2==undefined;
check_function_ref(#function_def_mod{module=Mod, function=F, arity=A1}, [#function_def_mod{module=Mod, function=F, arity=A2}|_]) ->
    A1==A2 orelse A2==undefined;
check_function_ref(X, [_|Tail]) ->
    check_function_ref(X, Tail).

    

check_var_pattern([], _, _, _, _) ->
	false;
check_var_pattern([#var_pattern{vardefref=VL, function=F, arity=A, clause=C} | Rest], V, F, A, C) ->
	case lists:member(V, VL) of
		true -> true;
		false -> check_var_pattern(Rest, V, F, A, C)
	end;
check_var_pattern([_ | Rest], V, F, A, C) ->
	check_var_pattern(Rest, V, F, A, C).

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
    ?D({read_module_refs, ScannerName, ModulePath}),
    R = erlide_noparse:read_module_refs(ScannerName, ModulePath, StateDir),
    ?D(R),
    R.

do_add_module_refs(ScannerName, Refs, #state{modules=Modules0} = State) ->
    Modules1 = lists:keydelete(ScannerName, #module.scanner_name, Modules0),
    Modules2 = [#module{scanner_name=ScannerName, refs=Refs} | Modules1],
    State#state{modules=Modules2}.
