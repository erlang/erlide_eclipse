%% Author: jakob
%% Description: TODO: Add description to erlide_noparse_server

-module(erlide_noparse_server).

%%
%% Exported Functions
%%

-export([modules/0, dump_module/1, dump_log/0, create/3, destroy/1, logging/1,
         all/0, stop/0, find/3, xdump/0]).

-compile(export_all).

%% internal exports 
-export([loop/1]).
-export([main_loop/1]).

%%
%% Include files
%%

%% -define(DEBUG, 1).

-define(CACHE_VERSION, 18).
-define(SERVER, erlide_noparse).

-include("erlide.hrl").
-include("erlide_scanner.hrl").

-record(module, {name, erlide_path, model}).

%%
%% API Functions
%%

modules() ->
    server_cmd(modules, []).

create(Module, Model, ErlidePath) when is_atom(Module), is_list(ErlidePath) ->
    server_cmd(create, {Module, Model, ErlidePath}).

destroy(Module) when is_atom(Module) ->
    server_cmd(destroy, Module).

dump_log() ->
    server_cmd(dump_log, []).

logging(OnOff) ->
    server_cmd(logging, OnOff).

all() ->
    server_cmd(all, []).

xdump() ->
    server_cmd(xdump, []).

%% cdump() ->
%%     server_cmd(cdump, []).

%% vdump() ->
%%     server_cmd(vdump, []).

dump_module(Module) when is_atom(Module) ->
    server_cmd(dump_module, Module).

stop() ->
    server_cmd(stop, []).

find(M, F, A) ->
    server_cmd(find, {external_call, {M, F, A}}).


%%
%% Internal functions
%%


main_loop(Modules) ->
    receive
        {stop, From, []} ->
            reply(stop, From, stopped);
        {Cmd, From, Args} ->
            NewMods = cmd(Cmd, From, Args, Modules),
            ?MODULE:main_loop(NewMods)
    end.

update_state(ScannerName, Model) ->
    server_cmd(update_state, {ScannerName, Model}).

server_cmd(Command, Args) ->
    spawn_server(),
    ?SERVER ! {Command, self(), Args},
    receive
        {Command, _Pid, Result} ->
            Result
    end.

spawn_server() ->
    case whereis(?SERVER) of
        undefined ->
            Pid = spawn(fun() ->
                                ?SAVE_CALLS,
                                main_loop([]) 
                        end),
            erlang:register(?SERVER, Pid);
        _ ->
            ok
    end.

cmd(Cmd, From, Args, Modules) ->
    try
        case get(logging) of
            on ->
                put(log, get(log)++[{Cmd, Args}]);
            _ ->
                ok
        end,
        case do_cmd(Cmd, Args, Modules) of
            {R, NewMods} ->
                reply(Cmd, From, R),
                NewMods;
            NewMods ->
                reply(Cmd, From, ok),
                NewMods
        end
    catch
        exit:Error ->
            reply(Cmd, From, {exit, Error}),
            Modules;
        error:Error ->
            reply(Cmd, From, {error, Error}),
            Modules
    end.

reply(Cmd, From, R) ->
    From ! {Cmd, self(), R}.

do_cmd(create, {Mod, Model, ErlidePath}, Modules) ->
    [#module{name=Mod, model=Model, erlide_path=ErlidePath} | lists:keydelete(Mod, #module.name, Modules)];
do_cmd(destroy, Mod, Modules) ->
    lists:keydelete(Mod, #module.name, Modules);
do_cmd(update_state, {Mod, Model}, Modules) ->
    NewMod = case lists:keysearch(Mod, #module.name, Modules) of
                 {value, OldMod} ->
                     OldMod#module{model=Model};
                 false ->
                     #module{name=Mod, model=Model}
             end,
    [NewMod | lists:keydelete(Mod, #module.name, Modules)];
do_cmd(all, [], Modules) ->
    {Modules, Modules};
do_cmd(modules, [], Modules) ->
    Mods = [{Name, Path} || #module{name=Name, erlide_path=Path} <- Modules],
    {Mods, Modules};
do_cmd(dump_module, Mod, Modules) ->
    {value, Module} = lists:keysearch(Mod, #module.name, Modules),
    {Module, Modules};
do_cmd(logging, OnOff, Modules) ->
    put(log, []),
    {put(logging, OnOff), Modules};
do_cmd(find, {external_call, {M, F, A}}, Modules) ->
    {erlide_noparse:find_external_call({M, F, A}, Modules), Modules};
do_cmd(dump_log, [], Modules) ->
    {get(log), Modules};
%% do_cmd(cdump, [], Modules) ->
%%     {do_cdump(Modules), Modules};
%% do_cmd(vdump, [], Modules) ->
%%     {do_vdump(Modules), Modules};
do_cmd(xdump, [], Modules) ->
    {erlide_noparse:do_xdump(Modules), Modules}.


%%% Worker process

loop(Modules) ->
    receive
        stop ->
            ok;
        {Cmd, From, Args} ->
            NewMods = cmd(Cmd, From, Args, Modules),
            ?MODULE:loop(NewMods)
    end.

