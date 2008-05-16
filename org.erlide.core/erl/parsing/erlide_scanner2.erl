%% Author: jakob
%% Created: 24 apr 2008
%% Description: 
-module(erlide_scanner2).

%%
%% Include files
%%

%%-define(DEBUG, 1).

-include("erlide.hrl").
-include("erlide_scanner.hrl").

%%
%% Exported Functions
%%

-export([create/1, destroy/1, initialScan/4, getTokenAt/2, getTokenWindow/4, 
         getTokens/1, replaceText/4, stop/0]).

%% just for testing
-export([all/0, modules/0, getTextLine/2, getText/1, check_all/2,
 		 dump_module/1]).

%% internal exports 
-export([loop/1]).

%%
%% API Functions
%%

-define(CACHE_VERSION, 10). %% odd numbers for scanner, even numbers for scanner2

-define(SERVER, ?MODULE).

create(Module) when is_atom(Module) ->
    server_cmd(create, Module).

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

initialScan(ScannerName, ModuleFileName, InitialText, StateDir) 
  when is_atom(ScannerName), is_list(ModuleFileName), is_list(InitialText), is_list(StateDir) ->
	server_cmd(initial_scan, {ScannerName, ModuleFileName, InitialText, StateDir}).

modules() ->
	server_cmd(modules, []).

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
    case getText(Module) of
        Text ->
            "match";
        ModText -> 
            "text mismatch!\n-----------------\""++ModText++"\"\n----------------\n\""++Text++"\""
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

replace_between(From, Length, With, In) ->
    {A, B} = lists:split(From, In),
    {_, C} = lists:split(Length, B),
    A++With++C.

%% [{Length, TextIncNL}...]
split_lines_w_lengths(Text) ->
    split_lines_w_lengths(Text, 0, [], []).

split_lines_w_lengths("", _Length, [], Acc) ->
    lists:reverse(Acc);
split_lines_w_lengths("", Length, LineAcc, Acc) ->
    lists:reverse(Acc, [{Length, lists:reverse(LineAcc)}]);
split_lines_w_lengths("\r\n" ++ Text, Length, LineAcc, Acc) ->
    split_lines_w_lengths(Text, 0, [], 
                          [{Length+2, lists:reverse(LineAcc, "\r\n")} | Acc]);
split_lines_w_lengths("\n" ++ Text, Length, LineAcc, Acc) ->
    split_lines_w_lengths(Text, 0, [], 
                          [{Length+1, lists:reverse(LineAcc, "\n")} | Acc]);
split_lines_w_lengths("\r" ++ Text, Length, LineAcc, Acc) ->
    split_lines_w_lengths(Text, 0, [], 
                          [{Length+1, lists:reverse(LineAcc, "\r")} | Acc]);
split_lines_w_lengths([C | Text], Length, LineAcc, Acc) ->
    split_lines_w_lengths(Text, Length+1, [C | LineAcc], Acc).

%% Find a line from [{Length, Line

find_line_w_offset(Offset, Lines) ->
    find_line_w_offset(Offset, 0, 0, Lines).

find_line_w_offset(0, _Pos, _N, []) ->
    {0, 0, 0, "", on_eof};
find_line_w_offset(_Offset, _Pos, _N, []) ->
    not_found;
find_line_w_offset(Offset, Pos, N, [{Length, _Line} | Lines]) when Offset >= Pos+Length, Lines =/= [] ->
	find_line_w_offset(Offset, Pos+Length, N+1, Lines);
find_line_w_offset(Offset, Pos, N, [{Length, Line} |_]) when Pos =< Offset, Offset < Pos + Length ->
	{N, Pos, Length, Line, false};
find_line_w_offset(_Offset, Pos, N, [{Length, Line}]) ->
    case ends_with_newline(Line) of
        true ->
            {N+1, Pos+Length, 0, "", beyond_eof};
        false ->
            {N, Pos+Length, Length, Line, on_eof}
    end.

ends_with_newline("") -> false;
ends_with_newline("\n") -> true;
ends_with_newline("\r") -> true;
ends_with_newline("\r\n") -> true;
ends_with_newline([_C | R]) -> ends_with_newline(R).

replace_between_lines(From, Length, With, Lines) ->
    ?D([From, Length, With, Lines]),
    {LineNo1, Pos1, _Length1, Line1, Beyond1} = find_line_w_offset(From, Lines),
    FirstPiece = string:substr(Line1, 1, From-Pos1),
    {LineNo2, Pos2, _Length2, Line2, Beyond2} = find_line_w_offset(From+Length, Lines),
	LastPiece = string:substr(Line2, From+Length-Pos2+1),
    ?D([LineNo1, Pos1, Line1, LineNo2, Pos2, Line2, FirstPiece, LastPiece, Beyond1, Beyond2]),
    WLines = split_lines_w_lengths(FirstPiece++With++LastPiece),
    NOldLines = case {Beyond1, Beyond2} of
                    {on_eof, on_eof} -> 0;
                    {beyond_eof, _} -> 0;
                    {_, beyond_eof} -> LineNo2-LineNo1;
                    _ -> LineNo2-LineNo1+1
                end,
    ?D([LineNo1, NOldLines, WLines, Lines]),
    {LineNo1, NOldLines, WLines,
     replace_between(LineNo1, NOldLines, WLines, Lines)}.

lines_to_text(Lines) ->
    lists:append([L || {_, L} <- Lines]).

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
        {stop, From, []} ->
            reply(stop, From, stopped);
        {Cmd, From, Args} ->
            NewMods = cmd(Cmd, From, Args, Modules),
            ?MODULE:loop(NewMods)
    end.

initial_scan(ScannerName, ModuleFileName, InitialText, StateDir) ->
    CacheFileName = filename:join(StateDir, atom_to_list(ScannerName) ++ ".scan"),
    RenewFun = fun(_F) -> do_scan(ScannerName, InitialText) end,
    erlide_util:check_cached(ModuleFileName, CacheFileName, ?CACHE_VERSION, RenewFun).

do_scan(ScannerName, InitialText) ->
    Lines = split_lines_w_lengths(InitialText),
    LineTokens = [scan_line(L) || L <- Lines],
    ?D([ScannerName, InitialText, LineTokens]),
    #module{name=ScannerName, lines=Lines, tokens=LineTokens}.

scan_line({Length, S}) ->
    case erlide_scan:string(S, {0, 0}) of
	    {ok, T, _} ->
            {Length, erlide_scan:filter_ws(T)};
        {error, _, _} ->
            {Length, [{string, {{0, 0}, length(S)}, S, "\"" ++ S ++ "\""}]}
    end.

replace_text(Module, Offset, RemoveLength, NewText) ->
    ?D({text_length, length(lines_to_text(Module#module.lines))}),
    ?D({Module#module.name, Offset, RemoveLength, length(NewText)}),
    {Line, NOldLines, AffectedLines, NewLines} = 
        replace_between_lines(Offset, RemoveLength, NewText, Module#module.lines),
    ?D(AffectedLines),
    LineTokens = [scan_line(L) || L <- AffectedLines],
    NewTokens = replace_between(Line, NOldLines, LineTokens, Module#module.tokens),
    Module#module{lines=NewLines, tokens=NewTokens}.

get_token_at(Module, Offset) ->
    case find_line_w_offset(Offset, Module#module.tokens) of
        {N, Pos, _Length, Tokens, false} ->
            case get_token_at_aux(Tokens, Offset - Pos) of
                token_not_found ->
                    token_not_found;
                T ->
                    M = mktoken(T, Pos, N),
                    {ok, M}
            end;
        _ ->
            line_not_found
    end.

get_tokens_at(Module, Offset, N) -> 
    get_tokens_at(Module, Offset, N, []).

get_tokens_at(_Module, _Offset, 0, Acc) ->
    lists:reverse(Acc);
get_tokens_at(Module, Offset, N, Acc0) ->
    case find_line_w_offset(Offset, Module#module.tokens) of
        {LineNo, Pos, Length, Tokens, false} ->
            {M, Ts} = get_tokens_at_aux(Tokens, Offset - Pos, N),
            Acc1 = 
                lists:foldl(fun(T, LAcc) ->
                                    [mktoken(T, Pos, LineNo) | LAcc]
                            end, Acc0, Ts),
            get_tokens_at(Module, Pos+Length, N-M, Acc1);
        _ ->
            lists:reverse(Acc0)
    end.

%% get tokens _before_ an offset, quite complicated...
get_tokens_before(Module, Offset, N) ->
    Lines = get_lines_before_and_upto(Module#module.tokens, Offset),
    R = get_tokens_before_aux(Lines, Offset, N, []),
    lists:reverse(R).

get_lines_before_and_upto(Lines, Offset) -> 
    get_lines_before_and_upto(Lines, 0, 0, Offset, []).

get_lines_before_and_upto(L, CurOfs, _, Offset, Acc) when L == []; CurOfs >= Offset ->
    Acc;
get_lines_before_and_upto([{Length, L} | Rest], CurOfs, N, Offset, Acc) ->
	get_lines_before_and_upto(Rest, CurOfs+Length, N+1, Offset, [{CurOfs, N, L} | Acc]).

get_tokens_before_aux(L, _, N, Acc) when L == []; N == 0 ->
	Acc;
get_tokens_before_aux([{LineOfs, LineNo, Tokens} | Rest], Offset, N0, Acc0) ->
	{N1, Acc1} = get_tokens_before_aux2(lists:reverse(Tokens), Offset, LineOfs, LineNo, N0, Acc0),
	get_tokens_before_aux(Rest, Offset, N1, Acc1).

get_tokens_before_aux2(L, _, _, _, N, Acc) when L == []; N == 0 ->
	{N, Acc};
get_tokens_before_aux2([T | Rest], Offset, LineOfs, LineNo, N, Acc) ->
    case token_pos(T) of
        {{_, Ofs}, Len}
          when Offset >= LineOfs+Ofs+Len ->
            get_tokens_before_aux2(Rest, Offset, LineOfs, LineNo, N-1, [mktoken(T, LineOfs, LineNo) | Acc]);
        _ ->
            get_tokens_before_aux2(Rest, Offset, LineOfs, LineNo, N, Acc)
    end.

mktokens(Tokens, Offset, Line) ->
    [mktoken(T, Offset, Line) || T <- Tokens].


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

get_tokens_at_aux(Tokens, Offset, N) -> 
    get_tokens_at_aux(Tokens, Offset, 0, N, []).

get_tokens_at_aux([], _, M, _, Acc) ->
	{M, lists:reverse(Acc)};
get_tokens_at_aux(_, _, M, N, Acc) when M == N ->
    {M, lists:reverse(Acc)};
get_tokens_at_aux([T | Rest], Offset, M, N, Acc) ->
	case token_pos(T) of
        {{_, Ofs}, Len}
          when Offset < Ofs+Len ->
            get_tokens_at_aux(Rest, Offset, M+1, N, [T | Acc]);
		_ ->
            get_tokens_at_aux(Rest, Offset, M, N, Acc)
	end.


get_all_tokens(#module{tokens=Tokens}) ->
	get_all_tokens(Tokens, 0, 0, []).

get_all_tokens([], _, _, Acc) ->
    lists:flatten(Acc); % instead of append(reverse())
get_all_tokens([{Length, Tokens} | Rest], Line, Pos, Acc) ->
	T = mktokens(Tokens, Pos, Line),
	get_all_tokens(Rest, Line+1, Pos+Length, [Acc, T]).

get_token_window(Module, Offset, Before, After) ->
    ?D({Module, Offset, Before, After}),
    A = get_tokens_at(Module, Offset, After),
    ?D(A),
    B = get_tokens_before(Module, Offset, Before),
    ?D(B),
    {A, B}.

token_pos({_, Pos}) -> Pos;
token_pos({_, Pos, _}) -> Pos;
token_pos({_, Pos, _, _}) ->Pos.

mktoken({dot, {{L, O}, G}}, Ofs, NL) ->
    #token{kind=dot, line=L+NL, offset=O+Ofs, length=G, text="."};
mktoken({ws, {{L, O}, G}, T}, Ofs, NL) ->
    #token{kind=ws, line=L+NL, offset=O+Ofs, length=G, text=T};
mktoken({K, {{L, O}, G}}, Ofs, NL) ->
    #token{kind=K, line=L+NL, offset=O+Ofs, length=G};
mktoken({K, {{L, O}, G}, V}, Ofs, NL) ->
    #token{kind=K, line=L+NL, offset=O+Ofs, length=G, value=V};
mktoken({K, {{L, O}, G}, V, T}, Ofs, NL) ->
    #token{kind=K, line=L+NL, offset=O+Ofs, length=G, value=V, text=T}.

cmd(Cmd, From, Args, Modules) ->
    try
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

do_cmd(create, Mod, Modules) ->
	[#module{name=Mod} | lists:keydelete(Mod, #module.name, Modules)];
do_cmd(destroy, Mod, Modules) ->
    lists:keydelete(Mod, #module.name, Modules);
do_cmd(initial_scan, {_Mod, _ModuleFileName, "", _StateDir}, Modules) ->   % rescan, ignore
	Modules;
do_cmd(initial_scan, {Mod, ModuleFileName, InitialText, StateDir}, Modules) ->
	NewMod = initial_scan(Mod, ModuleFileName, InitialText, StateDir),
    [NewMod | lists:keydelete(Mod, #module.name, Modules)];
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
    {get_token_at(Module, Offset), Modules};
do_cmd(replace_text, {Mod, Offset, RemoveLength, NewText}, Modules) ->
	{value, Module} = lists:keysearch(Mod, #module.name, Modules),
	NewMod = replace_text(Module, Offset, RemoveLength, NewText),
	[NewMod | lists:keydelete(Mod, #module.name, Modules)];
do_cmd(get_text, Mod, Modules) ->
	{value, Module} = lists:keysearch(Mod, #module.name, Modules),
	{lines_to_text(Module#module.lines), Modules};
do_cmd(get_text_line, {Mod, Line}, Modules) ->
	{value, Module} = lists:keysearch(Mod, #module.name, Modules),
    L = lists:nth(Line+1, Module#module.lines),
	{L, Modules};
do_cmd(get_tokens, Mod, Modules) ->
	{value, Module} = lists:keysearch(Mod, #module.name, Modules),
    {get_all_tokens(Module), Modules};
do_cmd(get_token_window, {Mod, Offset, Before, After}, Modules) ->
	{value, Module} = lists:keysearch(Mod, #module.name, Modules),
	{get_token_window(Module, Offset, Before, After), Modules}.

