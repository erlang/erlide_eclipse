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
-include("erlide_scanner_server.hrl").
-include("erlide_search_server.hrl").

%%
%% Exported Functions
%%

-export([create/1, destroy/1, initialScan/6, getTokenAt/2, getTokenWindow/4, 
         getTokens/1, replaceText/4, check_all/2]).

%% stop/0

%% just for testing
-export([getTextLine/2, getText/1, dump_module/1, dump_log/2]).

%% internal exports 
-export([loop/1]).

%%
%% API Functions
%%

create(ScannerName) when is_atom(ScannerName) ->
	spawn_server(ScannerName).

destroy(ScannerName) when is_atom(ScannerName) ->
    erlide_search_server:remove_module(ScannerName),
    server_cmd(ScannerName, stop).

getText(ScannerName) when is_atom(ScannerName) ->
    server_cmd(ScannerName, get_text).

getTextLine(ScannerName, Line) when is_atom(ScannerName), is_integer(Line) ->
    server_cmd(ScannerName, get_text_line, Line).

getTokens(ScannerName) when is_atom(ScannerName) ->
    server_cmd(ScannerName, get_tokens).

getTokenWindow(ScannerName, Offset, Before, After) 
  when is_atom(ScannerName), is_integer(Offset), is_integer(Before), is_integer(After) ->
    server_cmd(ScannerName, get_token_window, {Offset, Before, After}).

getTokenAt(ScannerName, Offset) when is_atom(ScannerName), is_integer(Offset) ->
    server_cmd(ScannerName, get_token_at, Offset).

initialScan(ScannerName, ModuleFileName, InitialText, StateDir, UseCache, Logging) 
  when is_atom(ScannerName), is_list(ModuleFileName), is_list(InitialText), is_list(StateDir) ->
	spawn_server(ScannerName),
    server_cmd(ScannerName, initial_scan,
               {ScannerName, ModuleFileName, InitialText, StateDir, UseCache, Logging}).

dump_module(ScannerName) when is_atom(ScannerName) ->
    server_cmd(ScannerName, dump_module).

dump_log(ScannerName, Filename) when is_atom(ScannerName) ->
    server_cmd(ScannerName, dump_log, Filename).

replaceText(ScannerName, Offset, RemoveLength, NewText)
  when is_atom(ScannerName), is_integer(Offset), is_integer(RemoveLength), is_list(NewText) ->
    server_cmd(ScannerName, replace_text, {Offset, RemoveLength, NewText}).

check_all(ScannerName, Text) when is_atom(ScannerName), is_list(Text) ->
    MatchTest = match_test(ScannerName, Text),
    ScanTest = scan_test(ScannerName),
    MatchTest ++ ScanTest.
            
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
    M = erlide_scanner:do_scan(dont_care, ModText),
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
								loop(#module{name=ScannerName}) 
						end),
			erlang:register(ScannerName, Pid);
		_ ->
			ok
	end.

loop(Module) ->
    receive
	{stop, From, []} ->
            ?D({stop, erlang:process_info(self(), registered_name)}),
	    reply(stop, From, stopped);
	{Cmd, From, Args} ->
	    NewModule = cmd(Cmd, From, Args, Module),
	    ?MODULE:loop(NewModule)
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
    {erlide_scanner:get_token_at(Module, Offset), Module};
do_cmd(replace_text, {Offset, RemoveLength, NewText}, Module) ->
    ?D({replace_text, Offset, RemoveLength, length(NewText)}),
    NewModule = log(Module, {replace_text, Offset, RemoveLength, NewText}),
    erlide_scanner:replace_text(NewModule, Offset, RemoveLength, NewText);
do_cmd(get_text, [], Module) ->
    {erlide_scanner:lines_to_text(Module#module.lines), Module};
do_cmd(get_text_line, Line, Module) ->
    L = lists:nth(Line+1, Module#module.lines),
    {L, Module};
do_cmd(get_tokens, [], Module) ->
    {erlide_scanner:get_all_tokens(Module), Module};
do_cmd(get_token_window, {Offset, Before, After}, Module) ->
    {erlide_scanner:get_token_window(Module, Offset, Before, After), Module}.


