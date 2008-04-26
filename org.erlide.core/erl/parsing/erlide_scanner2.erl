%% Author: jakob
%% Created: 24 apr 2008
%% Description: TODO: Add description to erlide_scan2.erl
-module(erlide_scanner2).

%%
%% Include files
%%

%-define(DEBUG, 1).

-include("erlide.hrl").

%%
%% Exported Functions
%%
-export([create/1, destroy/1, getText/1, initialScan/4, getTokenAt/2, getTokenWindow/4]).
-compile(export_all).

%%
%% API Functions
%%

-define(CACHE_VERSION, 2). %% odd numbers for scanner, even numbers for scanner2

-define(SERVER, ?MODULE).

create(Module) ->
    spawn_server(),
    ?SERVER ! {create, Module},
    ok.

destroy(Module) ->
    spawn_server(),
    ?SERVER ! {destroy, Module},
    ok.

getText(Module) ->
    spawn_server(),
    ?SERVER ! {get_text, self(), Module},
    receive
        {get_text, _Pid, Result} ->
            Result
    end.

getTokens(Module) ->
    spawn_server(),
    ?SERVER ! {get_tokens, self(), Module},
    receive
        {get_tokens, _Pid, Result} ->
            Result
    end.

%% Currently, only after is used, and only upto a full line......
getTokenWindow(Module, Offset, Before, After) ->
    spawn_server(),
    ?SERVER ! {get_token_window, self(), Module, Offset, Before, After},
    receive
        {get_token_window, _Pid, Result} ->
            Result
    end.

getTokenAt(Module, Offset) ->
	spawn_server(),
    ?SERVER ! {get_token_at, self(), Module, Offset},
    receive
        {get_token_at, _Pid, Result} ->
            Result
    end.

initialScan(ScannerName, ModuleFileName, InitialText, StateDir) ->
    spawn_server(),
    ?SERVER ! {initial_scan, ScannerName, ModuleFileName, InitialText, 
               StateDir},
    ok.

modules() ->
    spawn_server(),
    ?SERVER ! {modules, self()},
    receive
        {modules, _Pid, Result} ->
            Result
    end.

dump_module(Module) ->
    spawn_server(),
    ?SERVER ! {dump_module, self(), Module},
    receive
        {dump_module, _Pid, Result} ->
            Result
    end.

stop() ->
    ?SERVER ! stop.

replaceText(Module, Offset, RemoveLength, NewText) ->
    spawn_server(),
	?SERVER ! {replace_text, Module, Offset, RemoveLength, NewText},
	ok.

%%
%% Local Functions
%%

replace_between2(From, Length, With, In) ->
    string:substr(In, 1, From)++With++string:substr(In, From+Length+1).

replace_between(From, Length, With, In) ->
    {A, B} = lists:split(From, In),
    {_, C} = lists:split(Length, B),
    A++With++C.

%% [{Offset, Text}, ...]
split_lines_w_offsets(Text) ->
    split_lines_w_offsets(Text, 0, 0, [], []).


split_lines_w_offsets("", _Offset, _LineOffset, [], Acc) ->
    lists:reverse(Acc);
split_lines_w_offsets("", _Offset, LineOffset, LineAcc, Acc) ->
    lists:reverse(Acc, [{LineOffset, lists:reverse(LineAcc)}]);
split_lines_w_offsets("\n\r" ++ Text, Offset, LineOffset, LineAcc, Acc) ->
    split_lines_w_offsets(Text, Offset+2, Offset+2, [], 
                          [{LineOffset, lists:reverse(LineAcc, "\n\r")} | Acc]);
split_lines_w_offsets("\n" ++ Text, Offset, LineOffset, LineAcc, Acc) ->
    split_lines_w_offsets(Text, Offset+1, Offset+1, [], 
                          [{LineOffset, lists:reverse(LineAcc, "\n")} | Acc]);
split_lines_w_offsets("\r" ++ Text, Offset, LineOffset, LineAcc, Acc) ->
    split_lines_w_offsets(Text, Offset+1, Offset+1, [], 
                          [{LineOffset, lists:reverse(LineAcc, "\r")} | Acc]);
split_lines_w_offsets([C | Text], Offset, LineOffset, LineAcc, Acc) ->
    split_lines_w_offsets(Text, Offset+1, LineOffset, [C | LineAcc], Acc).


split_lines_w_lengths(Text) ->
    split_lines_w_lengths(Text, 0, [], []).

split_lines_w_lengths("", _Length, [], Acc) ->
    lists:reverse(Acc);
split_lines_w_lengths("", Length, LineAcc, Acc) ->
    lists:reverse(Acc, [{Length, lists:reverse(LineAcc)}]);
split_lines_w_lengths("\n\r" ++ Text, Length, LineAcc, Acc) ->
    split_lines_w_lengths(Text, 0, [], 
                          [{Length+2, lists:reverse(LineAcc, "\n\r")} | Acc]);
split_lines_w_lengths("\n" ++ Text, Length, LineAcc, Acc) ->
    split_lines_w_lengths(Text, 0, [], 
                          [{Length+1, lists:reverse(LineAcc, "\n")} | Acc]);
split_lines_w_lengths("\r" ++ Text, Length, LineAcc, Acc) ->
    split_lines_w_lengths(Text, 0, [], 
                          [{Length+1, lists:reverse(LineAcc, "\r")} | Acc]);
split_lines_w_lengths([C | Text], Length, LineAcc, Acc) ->
    split_lines_w_lengths(Text, Length+1, [C | LineAcc], Acc).

find_line_w_offset(Offset, Lines) ->
    find_line_w_offset(Offset, 0, 0, Lines).

find_line_w_offset(_Offset, _Pos, _N, []) ->
    not_found;
find_line_w_offset(Offset, Pos, N, [{Length, _Line} | Lines]) when Offset >= Pos+Length ->
	find_line_w_offset(Offset, Pos+Length, N+1, Lines);
find_line_w_offset(Offset, Pos, N, [{Length, Line} |_]) when Pos =< Offset, Offset < Pos + Length ->
	{N, Pos, Length, Line}.

replace_between_lines(From, Length, With, Lines) ->
    {LineNo1, Pos1, _Length1, Line1} = find_line_w_offset(From, Lines),
    {LineNo2, Pos2, _Length2, Line2} = find_line_w_offset(From+Length, Lines),
    FirstPiece = string:substr(Line1, 1, From-Pos1),
    LastPiece = string:substr(Line2, From+Length-Pos2+1),
	WLines = split_lines_w_lengths(FirstPiece++With++LastPiece),
    NOldLines = LineNo2-LineNo1+1,
	{LineNo1, NOldLines, WLines,
     replace_between(LineNo1, NOldLines, WLines, Lines)}.

lines_to_text(Lines) ->
	lists:append([L || {_, L} <- Lines]).

replace_start_ends(New1, New2, []) ->
    [New1, New2];
replace_start_ends(New1, New2, [_]) ->
    [New1, New2];
replace_start_ends(New1, New2, List) ->
	[New1] ++ string:substr(List, 2, length(List)-2) ++ [New2].

t() ->
	{ok, B} = file:read_file("/Users/jakob/Utveckling/erl/indent/in.erl"),
	T = binary_to_list(B),
	L = split_lines_w_lengths(T),
	{T, L}.

u() ->
	u("apapapa", 50, 50).

u(S, From, Length) ->
	{_T, L} = t(),
	user_default:da(?MODULE),
	R = replace_between_lines(From, Length, S, L),
	user_default:ds(),
    R.


a() ->
	ok.

-record(module, {name,
                 lines = [], % [{Length, String}]
                 tokens = [],
                 cachedTokens = []}). % [{Length, [Token]}]

spawn_server() ->
    case whereis(?SERVER) of
        undefined ->
            Pid = spawn(fun() -> loop([]) end),
            erlang:register(?SERVER, Pid);
        _ ->
            ok
    end.

loop(Modules) ->
    receive
        {create, Mod} ->
            NewMods = [#module{name=Mod} | lists:keydelete(Mod, #module.name, Modules)],
            ?MODULE:loop(NewMods);
        {destroy, Mod} ->
            NewMods = lists:keydelete(Mod, #module.name, Modules),
            ?MODULE:loop(NewMods);
        {initial_scan, _Mod, _ModuleFileName, "", _StateDir} ->   % rescan, ignore
            ?MODULE:loop(Modules);
        {initial_scan, Mod, ModuleFileName, InitialText, StateDir} ->
            NewMod = initial_scan(Mod, ModuleFileName, InitialText, StateDir),
            NewMods = [NewMod | lists:keydelete(Mod, #module.name, Modules)],
            ?MODULE:loop(NewMods);
        {modules, From} ->
            From ! {modules, self(), Modules},
            ?MODULE:loop(Modules);
		{dump_module, From, Mod} ->
            case lists:keysearch(Mod, #module.name, Modules) of
                {value, Module} ->
                    From ! {dump_module, self(), Module};
                false ->
                    From ! {dump_module, self(), module_not_found}
            end,
            ?MODULE:loop(Modules);
        {get_token_at, From, Mod, Offset} ->
            case lists:keysearch(Mod, #module.name, Modules) of
                {value, Module} ->
                    From ! {get_token_at, self(), get_token_at(Module, Offset)};
                false ->
                    From ! {get_token_at, self(), module_not_found}
            end,
            ?MODULE:loop(Modules);
		{replace_text, Mod, Offset, RemoveLength, NewText} ->
			case lists:keysearch(Mod, #module.name, Modules) of
                {value, Module} ->
                    NewMod = replace_text(Module, Offset, RemoveLength, NewText),
                    NewMods = [NewMod | lists:keydelete(Mod, #module.name, Modules)],
                    ?MODULE:loop(NewMods);
                false ->
                    ?MODULE:loop(Modules)
            end;
        stop ->
			stop;
        {get_text, From, Mod} ->
            case lists:keysearch(Mod, #module.name, Modules) of
                {value, Module} ->
                    From ! {get_text, self(), lines_to_text(Module#module.lines)};
                false ->
                    From ! {get_text, self(), module_not_found}
            end,
            ?MODULE:loop(Modules);
        {get_tokens, From, Mod} ->
            case lists:keysearch(Mod, #module.name, Modules) of
                {value, Module} ->
                    From ! {get_tokens, self(), get_all_tokens(Module)};
                false ->
                    From ! {get_tokens, self(), module_not_found}
            end,
            ?MODULE:loop(Modules);
        {get_token_window, From, Mod, Offset, Before, After} ->
            case lists:keysearch(Mod, #module.name, Modules) of
                {value, Module} ->
                    From ! {get_token_window, self(), 
                            get_token_window(Module, Offset, Before, After)};
                false ->
                    From ! {get_token_window, self(), module_not_found}
            end,
            ?MODULE:loop(Modules)
    end.

initial_scan(ScannerName, ModuleFileName, InitialText, StateDir) ->
    CacheFileName = filename:join(StateDir, atom_to_list(ScannerName) ++ ".scan"),
    RenewFun = fun(_F) -> do_scan(ScannerName, InitialText) end,
    erlide_util:check_cached(ModuleFileName, CacheFileName, ?CACHE_VERSION, RenewFun).

do_scan(ScannerName, InitialText) ->
    Lines = split_lines_w_lengths(InitialText),
    LineTokens = [scan_line(L) || L <- Lines],
    #module{name=ScannerName, lines=Lines, tokens=LineTokens}.

scan_line({Length, S}) ->
    {ok, T, _} = erlide_scan:string(S),
    {Length, T}.


replace_text(Module, Offset, RemoveLength, NewText) ->
    ?D({Module, Offset, RemoveLength, NewText}),
    {Line, NOldLines, AffectedLines, NewLines} = 
        replace_between_lines(Offset, RemoveLength, NewText, Module#module.lines),
    LineTokens = [scan_line(L) || L <- AffectedLines],
    NewTokens = replace_between(Line, NOldLines, LineTokens, Module#module.tokens),
    Module#module{lines=NewLines, tokens=NewTokens}.

get_token_at(Module, Offset) ->
    case find_line_w_offset(Offset, Module#module.tokens) of
        {N, Pos, _Length, Tokens} ->
            T = get_token_at_aux(Tokens, Offset - Pos),
            M = erlide_scanner:mktoken(T, Pos, N),
            {ok, M};
        not_found ->
            line_not_found
    end.

fix_tokens(Tokens, Offset, Line) ->
    [erlide_scanner:mktoken(T, Offset, Line) || T <- Tokens].

token_pos({_, Pos}) -> Pos;
token_pos({_, Pos, _}) -> Pos;
token_pos({_, Pos, _, _}) ->Pos.

get_token_at_aux([], _) ->
    token_not_found;
get_token_at_aux([T | Rest], Offset) ->
  	case token_pos(T) of
        {{_, Ofs}, Len} 
          when Offset >= Ofs, Offset < Ofs+Len ->
            T;
        _ ->
            get_token_at_aux(Rest, Offset)
    end.

get_all_tokens(#module{tokens=Tokens}) ->
	get_all_tokens(Tokens, 0, 0, []).

get_all_tokens([], _, _, Acc) ->
    lists:flatten(Acc); % instead of append(reverse())
get_all_tokens([{Length, Tokens} | Rest], Line, Pos, Acc) ->
	T = fix_tokens(Tokens, Pos, Line),
	get_all_tokens(Rest, Line+1, Pos+Length, [Acc, T]).

get_token_window(Module, Offset, _Before, _After) ->
    {get_token_at(Module, Offset), 0}.




