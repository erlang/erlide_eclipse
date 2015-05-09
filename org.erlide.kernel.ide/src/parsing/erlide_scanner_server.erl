%% Author: jakob
%% Created: 24 apr 2008
%% Description:
-module(erlide_scanner_server).

%%
%% Include files
%%

%% -define(DEBUG, 1).

-include("erlide.hrl").
-include("erlide_scanner_server.hrl").

%%
%% Exported Functions
%%

-export([server_cmd/2, server_cmd/3,
         spawn_server/1]).

%% stop/0

%% internal exports
-export([loop/2]).

%%
%% API Functions
%%

server_cmd(ScannerName, Command) ->
    server_cmd(ScannerName, Command, []).

server_cmd(ScannerName, Command, Args) ->
    try
        ScannerName ! {Command, self(), Args},
        receive
            {Command, _Pid, Result} ->
                Result
        end
    catch _:Exception ->
              {error, Exception, erlang:get_stacktrace()}
    end.

spawn_server(ScannerName) ->
    case whereis(ScannerName) of
        undefined ->
            Pid = spawn(fun() ->
                                ?SAVE_CALLS,
                                erlang:process_flag(min_heap_size, 64*1024),
                                loop(#module{name=ScannerName}, 0)
                        end),
            erlang:register(ScannerName, Pid);
        _ ->
            ok
    end,
    server_cmd(ScannerName, addref, []),
    ok.

%%
%% Local Functions
%%

loop(Module, Refs) ->
    receive
        {addref, From, []} ->
            ?D({addref, Module#module.name}),
            reply(addref, From, ok),
            ?MODULE:loop(Module, Refs+1);
        {dispose, From, []} ->
            ?D({dispose, Module#module.name}),
            reply(dispose, From, ok),
            case Refs=<1 of
                true ->
                    ok;
                _ ->
                    ?MODULE:loop(Module, Refs-1)
            end;
        {Cmd, From, Args} ->
            NewModule = cmd(Cmd, From, Args, Module),
            ?MODULE:loop(NewModule, Refs);
        Msg ->
            erlide_log:log({scanner, Module#module.name, unexpected_message, Msg}),
            ?MODULE:loop(Module, Refs)
    end.

cmd(Cmd, From, Args, Module) ->
    try
        case do_cmd(Cmd, Args, Module) of
            {R, NewModule} ->
                reply(Cmd, From, R),
                NewModule;
            NewModule ->
                reply(Cmd, From, ok),
                NewModule
        end
    catch
        exit:Error ->
            reply(Cmd, From, {exit, Error}),
            Module;
        error:Error ->
            reply(Cmd, From, {error, Error, erlang:get_stacktrace()}),
            Module
    end.

reply(Cmd, From, R) ->
    From ! {Cmd, self(), R}.

do_cmd(initial_scan, {ScannerName, ModuleFileName, InitialText, StateDir, UseCache}, _Module) ->
    ?D({initial_scan, ScannerName, length(InitialText)}),
    {Cached, Module1} = erlide_scanner:initial_scan_0(ScannerName, ModuleFileName, InitialText, StateDir, UseCache),
    {{ok, Cached}, Module1};
do_cmd(dump_module, [], Module) ->
    {Module, Module};
do_cmd(get_token_at, Offset, Module) ->
    {erlide_scan_model:get_token_at(Module, Offset), Module};
do_cmd(replace_text, {Offset, RemoveLength, NewText}, Module) ->
    ?D({replace_text, Offset, RemoveLength, length(NewText)}),
    erlide_scan_model:replace_text(Module, Offset, RemoveLength, NewText);
do_cmd(get_text, [], Module) ->
    {erlide_scan_model:get_text(Module), Module};
do_cmd(get_tokens, [], Module) ->
    {erlide_scan_model:get_all_tokens(Module), Module};
do_cmd(get_token_window, {Offset, Before, After}, Module) ->
    {erlide_scan_model:get_token_window(Module, Offset, Before, After), Module}.
