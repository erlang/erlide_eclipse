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
         spawn_server/1, scan_test/2, match_test/2]).

%% stop/0

%% internal exports
-export([loop/2]).

%%
%% API Functions
%%

match_test(Module, Text) ->
    case erlide_scanner:get_text(Module) of
        Text ->
            "match\n";
        ModText ->
            "text mismatch!"++
                "\n(Scanner text)----------------\n\"" ++ ModText ++
                "\"\n(Eclipse text)----------------\n\""++Text++"\"\n"
    end.

scan_test(Module, GetTokens) ->
    ModText = erlide_scanner:get_text(Module),
    M = erlide_scan_model:do_scan(dont_care, ModText),
    T = erlide_scan_model:get_all_tokens(M),
    Tokens = erlide_scanner:get_tokens(Module),
    R = case Tokens of
            T ->
                "scan match\n";
            _ ->
                "scan mismatch!\n"
        end,
    case GetTokens of
        true ->
            {R, Tokens, T};
        false ->
            R
    end.

%%
%% Local Functions
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

log(#module{log=none}=Module, _Event) ->
    Module;
log(#module{log=Log}=Module, Event) ->
    NewLog = [Event | Log],
    Module#module{log=NewLog}.

logging(Module, on) ->
    Module#module{log=[]};
logging(Module, off) ->
    Module#module{log=none}.

do_cmd(initial_scan, {ScannerName, ModuleFileName, InitialText, StateDir, UseCache, Logging}, _Module) ->
    ?D({initial_scan, ScannerName, length(InitialText)}),
    {{Cached, Module1}, Text} = erlide_scanner:initial_scan(ScannerName, ModuleFileName, InitialText, StateDir, UseCache),
    Module2 = logging(Module1, Logging),
    Module3 = log(Module2, {initial_scan, ScannerName, ModuleFileName, InitialText, Text}),
    {{ok, Cached}, Module3};
do_cmd(dump_module, [], Module) ->
    {Module, Module};
do_cmd(dump_log, Filename, Module) ->
    Log = lists:reverse(Module#module.log),
    {ok, File} = file:open(Filename, [write]),
    [io:format(File, "~p.\n", [L]) || L <- Log],
    file:close(File),
    {{ok, Filename}, Module};
do_cmd(get_token_at, Offset, Module) ->
    {erlide_scan_model:get_token_at(Module, Offset), Module};
do_cmd(replace_text, {Offset, RemoveLength, NewText}, Module) ->
    ?D({replace_text, Offset, RemoveLength, length(NewText)}),
    NewModule = log(Module, {replace_text, Offset, RemoveLength, NewText}),
    erlide_scan_model:replace_text(NewModule, Offset, RemoveLength, NewText);
do_cmd(get_text, [], Module) ->
    {erlide_scan_model:get_text(Module), Module};
do_cmd(get_text_line, Line, Module) ->
    L = lists:nth(Line+1, Module#module.lines),
    {L, Module};
do_cmd(get_tokens, [], Module) ->
    {erlide_scan_model:get_all_tokens(Module), Module};
do_cmd(get_token_window, {Offset, Before, After}, Module) ->
    {erlide_scan_model:get_token_window(Module, Offset, Before, After), Module}.


