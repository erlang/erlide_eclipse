%% Description: Search server
%% Author: jakob (jakobce at g mail dot com)
%% Created: 10 mar 2010

-module(erlide_search_server).

%%
%% Include files
%%

%% -define(DEBUG, 1).

-include("erlide.hrl").

-include_lib("kernel/include/file.hrl").

%%
%% Exported Functions
%%


%% called from Java
-export([start/0,
         stop/0,
         find_refs/4,
         start_find_refs/5,
         cancel_find_refs/1]).

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
-define(N_MODULES_KEPT, 5).

-record(module, {scanner_name :: atom(),
                 sequence_number :: integer(),
                 module_name :: string(),
                 refs :: list()}).
-record(state, {modules=[] :: list(#module{}),
                sequence_number :: integer()}).

%%
%% API Functions
%%

start() ->
    start(whereis(?SERVER)).

stop() ->
    server_cmd(stop).

state() ->
    server_cmd(state).

modules() ->
    server_cmd(modules).

%% modules is {ScannerName, ModulePath}
find_refs(Pattern, Modules, StateDir, UpdateSearchServer)
  when is_tuple(Pattern), is_list(Modules), is_list(StateDir) ->
    find_refs([Pattern], Modules, StateDir, UpdateSearchServer);
find_refs(Pattern, Modules, StateDir, UpdateSearchServer)
  when is_list(Pattern), is_list(Modules), is_list(StateDir) ->
    R = server_cmd(find_refs, {Pattern, Modules, StateDir, UpdateSearchServer}),
    R.

%%
start_find_refs(JPid, Pattern, Modules, StateDir, UpdateSearchServer)
  when is_tuple(Pattern), is_list(Modules), is_list(StateDir) ->
    start_find_refs(JPid, [Pattern], Modules, StateDir, UpdateSearchServer);
start_find_refs(JPid, Pattern, Modules, StateDir, UpdateSearchServer)
  when is_list(Pattern), is_list(Modules), is_list(StateDir) ->
    ?D({JPid, Pattern}),
    R = server_cmd(start_find_refs, {Pattern, Modules, JPid, StateDir, UpdateSearchServer}),
    R.

remove_module(ScannerName) ->
    server_cmd(remove_module, ScannerName).

add_module_refs(ScannerName, Refs) ->
    ?D({add_module_refs, ScannerName}),
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
                  erlang:process_flag(min_heap_size, 64*1024),
                  erlang:yield(),
                  erlang:register(?SERVER, self()),
                  Self ! started,
                  loop(#state{sequence_number=0})
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
    receive
        {stop, From, []} ->
            reply(stop, From, stopped);
        {Cmd, From, Args} ->
            NewState = cmd(Cmd, From, Args, State),
            ?MODULE:loop(NewState);
        _ ->
            ?MODULE:loop(State)
    end.

cmd(Cmd, From, Args, State) ->
    try
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
do_cmd(find_refs, {Ref, Modules, StateDir, UpdateSearchServer}, State) ->
    R = do_find_refs(Modules, Ref, StateDir, State, UpdateSearchServer, []),
    R;
do_cmd(start_find_refs, {Pattern, Modules, JPid, StateDir, UpdateSearchServer}, State) ->
    ?D(start_find_refs),
    R = do_start_find_refs(Pattern, Modules, JPid, StateDir, UpdateSearchServer, State),
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

do_start_find_refs(Pattern, Modules, JPid, StateDir, UpdateSearchServer, State) ->
    ?D({do_start_find_refs, Pattern, JPid}),
    Pid = spawn(fun() ->
                        ModuleChunks = chunkify(Modules, 3),
                        JPid ! {start, length(ModuleChunks)},
                        R = try 
                                do_background_find_refs(ModuleChunks, Pattern, JPid, StateDir, UpdateSearchServer, State)
                            catch
                                _:_ ->
                                    crashed
                            end,
                        JPid ! {stop, R}
                end),
    {Pid, State}.

do_background_find_refs([], _Pattern, _JPid, _StateDir, _UpdateSearchServer, _State) ->
    ok;
do_background_find_refs([Chunk | Rest], Pattern, JPid, StateDir,
                        UpdateSearchServer, State) ->
    {R, _State} = do_find_refs(Chunk, Pattern, StateDir, State,
                               UpdateSearchServer, []),
    ?D({1, R}),
    JPid ! {progress, {self(), 1, R}},
    receive
        cancel ->
            ok
    after 0 ->
            do_background_find_refs(Rest, Pattern, JPid, StateDir,
                                    UpdateSearchServer, State)
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

do_find_refs([], _, _, State, _, Acc) ->
    {{ok, Acc}, State};
do_find_refs([{ScannerName, ModulePath} | Rest], Pattern, StateDir,
             #state{modules=Modules} = State, UpdateSearchServer, Acc0) ->
    Refs = get_module_refs(ScannerName, ModulePath, StateDir, Modules,
                           UpdateSearchServer),
    Mod = get_module_name(ModulePath),
    Acc1 = Acc0 ++ erlide_search:find_data(Refs, Pattern, Mod, ModulePath),
    do_find_refs(Rest, Pattern, StateDir, State, UpdateSearchServer, Acc1).

get_module_name(ModulePath) ->
    L = filename:rootname(filename:basename(ModulePath)),
    list_to_atom(L).


get_module_refs(ScannerName, ModulePath, StateDir, Modules, UpdateSearchServer) ->
    case lists:keysearch(ScannerName, #module.scanner_name, Modules) of
        {value, #module{refs=Refs}} ->
            Refs;
        false ->
            get_module_refs(ScannerName, ModulePath, StateDir, UpdateSearchServer)
    end.

get_module_refs(ScannerName, ModulePath, StateDir, UpdateSearchServer) ->
    ?D({get_module_refs, ScannerName, ModulePath}),
    R = erlide_noparse:get_module_refs(ScannerName, ModulePath, StateDir,
                                       UpdateSearchServer),
    ?D(R),
    R.

do_add_module_refs(ScannerName, Refs, #state{modules=Modules0, sequence_number=SequenceNumber} = State) ->
    SequenceNumber1 = SequenceNumber+1,
    Modules1 = [M || M <- Modules0, M#module.sequence_number + ?N_MODULES_KEPT > SequenceNumber1, M#module.scanner_name /= ScannerName],
    Modules2 = [#module{scanner_name=ScannerName, refs=Refs, sequence_number=SequenceNumber1} | Modules1],
    State#state{modules=Modules2, sequence_number=SequenceNumber1}.
