%% Author: jakob
%% Created: 24 apr 2008
%% Description: 
-module(erlide_scanner).

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
         getTokens/1, replaceText/4, stop/0, tokens_to_string/1, light_scan_string/1]).

%% called from erlide_indent
-export([convert_tokens/1]).
 
%% just for testing
-export([all/0, modules/0, getTextLine/2, getText/1, check_all/2,
         dump_module/1, logging/1, dump_log/0, scan_uncached/3]).
%% -compile(export_all).

%% internal exports 
-export([loop/1]).

%%
%% API Functions
%%

-define(CACHE_VERSION, 16).

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
    M = do_scan(dont_care, ModText, "don't care"),
    T = get_all_tokens(M),
    case getTokens(Module) of
        T ->
            "scan match\n";
        _ ->
            "scan mismatch!\n"
    end.

light_scan_string(S) ->
    case erlide_scan:string(S, {0, 0}) of
	{ok, T, _} ->
	    {ok, convert_tokens(T)};	    
	{error, _, _} ->
	    error
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
find_line_w_offset(Offset, Pos, N, [{Length, Line}]) ->
    case ends_with_newline(Line) orelse Offset >= Pos + Length of
        true ->
            {N+1, Pos+Length, 0, "", beyond_eof};
        false ->
            {N, Pos+Length, Length, Line, on_eof}
    end.

ends_with_newline("") -> false;
ends_with_newline("\n") -> true;
ends_with_newline("\r") -> true;
ends_with_newline("\r\n") -> true;
ends_with_newline([_C | R]) ->
    ends_with_newline(R).

%%
%% Nicer version of string:substring/2 accepting out-of-bounds parameters
%% (should be removed eventually)
%%
substr(Text, Start) when Start>length(Text) ->
    "";
substr(Text, Start) when Start < 1 ->
    Text;
substr(Text, Start) ->
    string:substr(Text, Start).    

%%
%% Nicer version of string:substring/3 accepting out-of-bounds parameters
%% (should be removed eventually)
%%
substr(Text, Start, Length) when Start>length(Text); Length=<0 ->
    "";
substr(Text, Start, Length) when Start < 1 ->
    substr(Text, 1, Length+Start-1);
substr(Text, Start, Length) when Start+Length>length(Text) ->
    substr(Text, Start, length(Text)-(Start+Length));
substr(Text, Start, Length) ->
    string:substr(Text, Start, Length).    

replace_between_lines(From, Length, With, Lines) ->
    {LineNo1, Pos1, _Length1, Line1, Beyond1} = find_line_w_offset(From, Lines),
    FirstPiece = substr(Line1, 1, From-Pos1),
    {LineNo2, Pos2, _Length2, Line2, Beyond2} = find_line_w_offset(From+Length, Lines),
    LastPiece = substr(Line2, From+Length-Pos2+1),
    WLines = split_lines_w_lengths(FirstPiece++With++LastPiece),
    NOldLines = case {Beyond1, Beyond2} of
                    {on_eof, on_eof} -> 0;
                    {beyond_eof, _} -> 0;
                    {_, beyond_eof} -> LineNo2-LineNo1;
                    _ -> LineNo2-LineNo1+1
                end,
    {LineNo1, NOldLines, WLines,
     replace_between(LineNo1, NOldLines, WLines, Lines)}.

lines_to_text(Lines) ->
    lists:append([L || {_, L} <- Lines]).

-record(module, {name,
                 lines = [], % [{Length, String}]
                 tokens = [], % [{Length, [Token]}]
                 cachedTokens = [],
                 erlide_path="",
                 log = []}).

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

initial_scan(ScannerName, ModuleFileName, InitialText, StateDir, ErlidePath, UpdateCache) ->
    CacheFileName = filename:join(StateDir, atom_to_list(ScannerName) ++ ".scan"),
    RenewFun = fun(_F) -> do_scan(ScannerName, InitialText, ErlidePath) end,
    erlide_util:check_and_renew_cached(ModuleFileName, CacheFileName, ?CACHE_VERSION, RenewFun, UpdateCache).

do_scan_uncached(ScannerName, ModuleFileName, ErlidePath) ->
    {ok, B} = file:read_file(ModuleFileName),
    InitialText = binary_to_list(B),
    do_scan(ScannerName, InitialText, ErlidePath).

do_scan(ScannerName, InitialText, ErlidePath) ->
    ?D(do_scan),
    Lines = split_lines_w_lengths(InitialText),
    LineTokens = [scan_line(L) || L <- Lines],
    ?D([ScannerName]), % , InitialText, LineTokens]),
    #module{name=ScannerName, lines=Lines, tokens=LineTokens,
            erlide_path=ErlidePath}.

scan_line({Length, S}) ->
    case erlide_scan:string(S, {0, 0}) of
        {ok, T0, _} ->
	    T = erlide_scan:filter_ws(T0),
            {Length, convert_tokens(T)};
        {error, _, _} ->
            {Length, [#token{kind=string, line=0, offset=0, length=length(S),
			     value=S, text="\""++S++"\"", last_line=0}]}
    end.

replace_text(Module, Offset, RemoveLength, NewText) ->
    {Line, NOldLines, AffectedLines, NewLines} = 
        replace_between_lines(Offset, RemoveLength, NewText, Module#module.lines),
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
                    M = fix_token(T, Pos, N),
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
                                    [fix_token(T, Pos, LineNo) | LAcc]
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
    case T of
        #token{offset=Ofs, length=Len} when Offset >= LineOfs+Ofs+Len ->
            get_tokens_before_aux2(Rest, Offset, LineOfs, LineNo, N-1, [fix_token(T, LineOfs, LineNo) | Acc]);
        _ ->
            get_tokens_before_aux2(Rest, Offset, LineOfs, LineNo, N, Acc)
    end.

get_token_at_aux([], _) ->
    token_not_found;
get_token_at_aux([T | Rest], Offset) ->
    case T of
	#token{offset=Ofs, length=Len} when Offset >= Ofs, Offset < Ofs+Len ->
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
    case T of
        #token{offset=Ofs, length=Len} when Offset < Ofs+Len ->
            get_tokens_at_aux(Rest, Offset, M+1, N, [T | Acc]);
        _ ->
            get_tokens_at_aux(Rest, Offset, M, N, Acc)
    end.


get_all_tokens(#module{tokens=Tokens}) ->
    get_all_tokens(Tokens, 0, 0, []).

get_all_tokens([], _, _, Acc) ->
    lists:flatten(Acc); % instead of append(reverse())
get_all_tokens([{Length, Tokens} | Rest], Line, Pos, Acc) ->
    T = fix_tokens(Tokens, Pos, Line),
    get_all_tokens(Rest, Line+1, Pos+Length, [Acc, T]).

get_token_window(Module, Offset, Before, After) ->
    ?D({Module, Offset, Before, After}),
    A = get_tokens_at(Module, Offset, After),
    ?D(A),
    B = get_tokens_before(Module, Offset, Before),
    ?D(B),
    {A, B}.

fix_token(T = #token{offset=O, line=L, last_line=undefined}, Offset, Line) ->
    T#token{offset=Offset+O, line=Line+L};
fix_token(T = #token{offset=O, line=L, last_line=LL}, Offset, Line) ->
    T#token{offset=Offset+O, line=Line+L, last_line=Line+LL}.

fix_tokens(Tokens, Offset, Line) ->
    [fix_token(T, Offset, Line) || T <- Tokens].

convert_tokens(Tokens) ->
    convert_tokens(Tokens, 0).

convert_tokens(Tokens, NL) ->
    convert_tokens(Tokens, 0, NL).

convert_tokens(Tokens, Offset, NL) ->
    convert_tokens(Tokens, Offset, NL, []).

convert_tokens([], _Ofs, _NL, Acc) ->
    lists:reverse(Acc);
convert_tokens([{dot, {{L, O}, G}} | Rest], Ofs, NL, Acc) ->
    T = #token{kind=dot, line=L+NL, offset=O+Ofs, length=G, text="."},
    convert_tokens(Rest, Ofs, NL, [T | Acc]);
convert_tokens([{ws, {{L, O}, G}, Txt} | Rest], Ofs, NL, Acc) ->
    T = #token{kind=ws, line=L+NL, offset=O+Ofs, length=G, text=Txt},
    convert_tokens(Rest, Ofs, NL, [T | Acc]);
convert_tokens([{'?', {{L, O}, 1}}, {atom, {{L, O1}, G}, V} | Rest],
	   Ofs, NL, Acc) when O1=:=O+1->
    T = make_macro(L, NL, O, G, V),
    convert_tokens(Rest, Ofs, NL, [T | Acc]);
convert_tokens([{'?', {{L, O}, 1}}, {var, {{L, O1}, G}, V} | Rest],
	   Ofs, NL, Acc) when O1=:=O+1->
    T = make_macro(L, NL, O, G, V),
    convert_tokens(Rest, Ofs, NL, [T | Acc]);
convert_tokens([{'?', {{L, O}, 1}}, {atom, {{L, O1}, G}, V, _Txt} | Rest],
	   Ofs, NL, Acc) when O1=:=O+1->
    T = make_macro(L, NL, O, G, V),
    convert_tokens(Rest, Ofs, NL, [T | Acc]);
convert_tokens([{K, {{L, O}, G}} | Rest], Ofs, NL, Acc) ->
    T = #token{kind=K, line=L+NL, offset=O+Ofs, length=G},
    convert_tokens(Rest, Ofs, NL, [T | Acc]);
convert_tokens([{K, {{L, O}, G}, V} | Rest], Ofs, NL, Acc) ->
    T = #token{kind=K, line=L+NL, offset=O+Ofs, length=G, value=V},
    convert_tokens(Rest, Ofs, NL, [T | Acc]);
convert_tokens([{K, {{L, O}, G}, V, Txt} | Rest], Ofs, NL, Acc) ->
    T = #token{kind=K, line=L+NL, offset=O+Ofs, length=G, value=V, text=Txt},
    convert_tokens(Rest, Ofs, NL, [T | Acc]).

make_macro(L, NL, O, G, V0) ->
    V = list_to_atom([$? | atom_to_list(V0)]),
    #token{kind=macro, line=L+NL, offset=O, length=G+1, value=V}.

%% mktoken({dot, {{L, O}, G}}, Ofs, NL) ->
%%     #token{kind=dot, line=L+NL, offset=O+Ofs, length=G, text="."};
%% mktoken({ws, {{L, O}, G}, T}, Ofs, NL) ->
%%     #token{kind=ws, line=L+NL, offset=O+Ofs, length=G, text=T};
%% mktoken({K, {{L, O}, G}}, Ofs, NL) ->
%%     #token{kind=K, line=L+NL, offset=O+Ofs, length=G};
%% mktoken({K, {{L, O}, G}, V}, Ofs, NL) ->
%%     #token{kind=K, line=L+NL, offset=O+Ofs, length=G, value=V};
%% mktoken({K, {{L, O}, G}, V, T}, Ofs, NL) ->
%%     #token{kind=K, line=L+NL, offset=O+Ofs, length=G, value=V, text=T}.

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

do_cmd(create, {Mod, ErlidePath}, Modules) ->
    [#module{name=Mod, erlide_path=ErlidePath} 
    | lists:keydelete(Mod, #module.name, Modules)];
do_cmd(destroy, Mod, Modules) ->
    lists:keydelete(Mod, #module.name, Modules);
do_cmd(scan_uncached, {Mod, ModuleFileName, ErlidePath}, Modules) ->
    NewMod = do_scan_uncached(Mod, ModuleFileName, ErlidePath),
    [NewMod | lists:keydelete(Mod, #module.name, Modules)];
do_cmd(initial_scan, {_Mod, _ModuleFileName, "", _StateDir, _}, Modules) ->   % rescan, ignore
    ?D({rescan, _Mod}),
    Modules;
do_cmd(initial_scan, {Mod, ModuleFileName, InitialText, StateDir, ErlidePath, UpdateCache}, Modules) ->
    ?D({initial_scan, Mod}),
    {Cached, NewMod} = initial_scan(Mod, ModuleFileName, InitialText, StateDir, ErlidePath, UpdateCache),
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
    {get_token_at(Module, Offset), Modules};
do_cmd(replace_text, {Mod, Offset, RemoveLength, NewText}, Modules) ->
    ?D({replace_text, Mod, Offset, RemoveLength, length(NewText)}),
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
do_cmd(logging, OnOff, Modules) ->
    put(log, []),
    {put(logging, OnOff), Modules};
do_cmd(dump_log, [], Modules) ->
    {get(log), Modules};
do_cmd(get_token_window, {Mod, Offset, Before, After}, Modules) ->
    {value, Module} = lists:keysearch(Mod, #module.name, Modules),
    {get_token_window(Module, Offset, Before, After), Modules}.

tokens_to_string(T) ->
    S = tokens_to_string(T, []),
    S.

token_to_string(#token{text=Text}) when is_list(Text) ->
	Text;
token_to_string(#token{value=Value}) when is_list(Value) ->
	Value;
token_to_string(#token{kind=atom, value=Value}) ->
    atom_to_list(Value);
token_to_string(#token{value=Value}) when Value =/= undefined ->
    atom_to_list(Value);
token_to_string(#token{kind=Kind}) ->
    atom_to_list(Kind).

tokens_to_string([], Acc) ->
    lists:flatten(Acc);
tokens_to_string([T], Acc) ->
    S = token_to_string(T),
    tokens_to_string([], [Acc, S]);
tokens_to_string([T1 | [T2 | _] = Rest], Acc) ->
    S = token_to_string(T1),
    Sb = space_between(T1, T2),
    tokens_to_string(Rest, [Acc, S, Sb]).

space_between(#token{offset=O1, length=Len1}, #token{offset=O2}) ->
    lists:duplicate(O2-O1-Len1, $ ).














