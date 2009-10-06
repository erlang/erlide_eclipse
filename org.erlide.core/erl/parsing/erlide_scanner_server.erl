%% Author: jakob
%% Created: 24 apr 2008
%% Description: 
-module(erlide_scanner_server).

%%
%% Include files
%%

%% -define(DEBUG, 1).

-include("erlide.hrl").
-include("erlide_scanner.hrl").

%%
%% Exported Functions
%%

-export([create/2, destroy/1, initialScan/6, getTokenAt/2, getTokenWindow/4, 
         getTokens/1, replaceText/4, stop/0]).

%% just for testing
-export([all/0, modules/0, getTextLine/2, getText/1, check_all/2,
         dump_module/1, logging/1, dump_log/0, scan_uncached/3]).

%% internal exports 
-export([loop/1]).

%%
%% API Functions
%%

-define(SERVER, erlide_scanner).

create(Module, ErlidePath) when is_atom(Module) ->
    server_cmd(create, {Module, ErlidePath}).

destroy(Module) when is_atom(Module) ->
    server_cmd(destroy, Module).

getText(Module) when is_atom(Module) ->
    server_cmd(get_text, Module).

getTextLine(Module, Line) when is_atom(Module), is_integer(Line) ->
    server_cmd(get_text_line, {Module, Line}).

getTokens(Module) when is_atom(Module) ->
    server_cmd(get_tokens, Module).

getTokenWindow(Module, Offset, Before, After) 
  when is_atom(Module), is_integer(Offset), is_integer(Before), is_integer(After) ->
    server_cmd(get_token_window, {Module, Offset, Before, After}).

getTokenAt(Module, Offset) when is_atom(Module), is_integer(Offset) ->
    server_cmd(get_token_at, {Module, Offset}).

initialScan(ScannerName, ModuleFileName, InitialText, StateDir, ErlidePath, UpdateCache) 
  when is_atom(ScannerName), is_list(ModuleFileName), is_list(InitialText), is_list(StateDir) ->
    server_cmd(initial_scan, {ScannerName, ModuleFileName, InitialText, StateDir, ErlidePath, UpdateCache}).

scan_uncached(ScannerName, ModuleFileName, ErlidePath) ->
    server_cmd(scan_uncached, {ScannerName, ModuleFileName, ErlidePath}).

modules() ->
    server_cmd(modules, []).

dump_log() ->
    server_cmd(dump_log, []).

all() ->
    server_cmd(all, []).

dump_module(Module) when is_atom(Module) ->
    server_cmd(dump_module, Module).

stop() ->
    server_cmd(stop, []).

replaceText(Module, Offset, RemoveLength, NewText)
  when is_atom(Module), is_integer(Offset), is_integer(RemoveLength), is_list(NewText) ->
    server_cmd(replace_text, {Module, Offset, RemoveLength, NewText}).

check_all(Module, Text) when is_atom(Module), is_list(Text) ->
    MatchTest = match_test(Module, Text),
    ScanTest = scan_test(Module),
    MatchTest ++ ScanTest.
            
logging(OnOff) ->
    server_cmd(logging, OnOff).

match_test(Module, Text) ->
    case getText(Module) of
        Text ->
            "match\n";
        ModText -> 
            "text mismatch!"++
		  "\n(Scanner text)----------------\n\"" ++ ModText ++
		"\"\n(Eclipse text)----------------\n\""++Text++"\"\n"
    end.

scan_test(Module) ->
    ModText = getText(Module),
    M = erlide_scanner:do_scan(dont_care, ModText, "don't care"),
    T = erlide_scanner:get_all_tokens(M),
    case getTokens(Module) of
        T ->
            "scan match\n";
        _ ->
            "scan mismatch!\n"
    end.

%%
%% Local Functions
%%

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
								loop([]) 
						end),
			erlang:register(?SERVER, Pid);
		_ ->
			ok
	end.

loop(Modules) ->
	receive
		{stop, From, []} ->
			reply(stop, From, stopped);
		{Cmd, From, Args} ->
			NewMods = cmd(Cmd, From, Args, Modules),
			?MODULE:loop(NewMods)
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

-record(module, {name,
                 lines = [], % [{Length, String}]
                 tokens = [], % [{Length, [Token]}]
                 cachedTokens = [],
                 erlide_path="",
                 log = []}).

do_cmd(create, {Mod, ErlidePath}, Modules) ->
    [#module{name=Mod, erlide_path=ErlidePath} 
    | lists:keydelete(Mod, #module.name, Modules)];
do_cmd(destroy, Mod, Modules) ->
    lists:keydelete(Mod, #module.name, Modules);
do_cmd(scan_uncached, {Mod, ModuleFileName, ErlidePath}, Modules) ->
    NewMod = erlide_scanner:do_scan_uncached(Mod, ModuleFileName, ErlidePath),
    [NewMod | lists:keydelete(Mod, #module.name, Modules)];
do_cmd(initial_scan, {_Mod, _ModuleFileName, "", _StateDir, _}, Modules) ->   % rescan, ignore
    ?D({rescan, _Mod}),
    Modules;
do_cmd(initial_scan, {Mod, ModuleFileName, InitialText, StateDir, ErlidePath, UpdateCache}, Modules) ->
    ?D({initial_scan, Mod}),
    {Cached, NewMod} = erlide_scanner:initial_scan(Mod, ModuleFileName, InitialText, StateDir, ErlidePath, UpdateCache),
    ?D({done, Mod}),
    {{ok, Cached}, [NewMod | lists:keydelete(Mod, #module.name, Modules)]};
do_cmd(all, [], Modules) ->
    {Modules, Modules};
do_cmd(modules, [], Modules) ->
    Mods = [M#module.name || M <- Modules],
    {Mods, Modules};
do_cmd(dump_module, Mod, Modules) ->
    {value, Module} = lists:keysearch(Mod, #module.name, Modules),
    {Module, Modules};
do_cmd(get_token_at, {Mod, Offset}, Modules) ->
    {value, Module} = lists:keysearch(Mod, #module.name, Modules),
    {erlide_scanner:get_token_at(Module, Offset), Modules};
do_cmd(replace_text, {Mod, Offset, RemoveLength, NewText}, Modules) ->
    ?D({replace_text, Mod, Offset, RemoveLength, length(NewText)}),
    {value, Module} = lists:keysearch(Mod, #module.name, Modules),
    NewMod = erlide_scanner:replace_text(Module, Offset, RemoveLength, NewText),
    [NewMod | lists:keydelete(Mod, #module.name, Modules)];
do_cmd(get_text, Mod, Modules) ->
    {value, Module} = lists:keysearch(Mod, #module.name, Modules),
    {erlide_scanner:lines_to_text(Module#module.lines), Modules};
do_cmd(get_text_line, {Mod, Line}, Modules) ->
    {value, Module} = lists:keysearch(Mod, #module.name, Modules),
    L = lists:nth(Line+1, Module#module.lines),
    {L, Modules};
do_cmd(get_tokens, Mod, Modules) ->
	X = lists:keysearch(Mod, #module.name, Modules),
    {value, Module} = X,
    {erlide_scanner:get_all_tokens(Module), Modules};
do_cmd(logging, OnOff, Modules) ->
    put(log, []),
    {put(logging, OnOff), Modules};
do_cmd(dump_log, [], Modules) ->
    {get(log), Modules};
do_cmd(get_token_window, {Mod, Offset, Before, After}, Modules) ->
    {value, Module} = lists:keysearch(Mod, #module.name, Modules),
    {erlide_scanner:get_token_window(Module, Offset, Before, After), Modules}.

