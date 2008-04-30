%% Author: jakob
%% Created: 24 apr 2008
%% Description: TODO: Add description to erlide_scan2.erl
-module(erlide_scanner2).

%%
%% Include files
%%

-define(DEBUG, 1).

-include("erlide.hrl").
-include("erlide_scanner.hrl").

%%
%% Exported Functions
%%
-export([create/1, destroy/1, getTextLine/2, initialScan/4, getTokenAt/2, getTokenWindow/4]).

%% just for testing
-export([all/0, modules/0, getText/1]).

-compile(export_all).

%%
%% API Functions
%%

-define(CACHE_VERSION, 4). %% odd numbers for scanner, even numbers for scanner2

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

getTextLine(Module, Line) ->
    spawn_server(),
    ?SERVER ! {get_text_line, self(), Module, Line},
    receive
        {get_text_line, _Pid, Result} ->
            Result
    end.

getTokens(Module) ->
    spawn_server(),
    ?SERVER ! {get_tokens, self(), Module},
    receive
        {get_tokens, _Pid, Result} ->
            Result
    end.

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

all() ->
    spawn_server(),
    ?SERVER ! {all, self()},
    receive
        {all, _Pid, Result} ->
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
    FirstPiece = string:substr(Line1, 1, From-Pos1),
    {LineNo2, Pos2, _Length2, Line2} = find_line_w_offset(From+Length, Lines),
	LastPiece = string:substr(Line2, From+Length-Pos2+1),
%%     io:format("LineNo1, Pos1, Line1, LineNo2, Pos2, Line2, FirstPiece, LastPiece ~p \n",
              ?D([LineNo1, Pos1, Line1, LineNo2, Pos2, Line2, FirstPiece, LastPiece]),
    WLines = split_lines_w_lengths(FirstPiece++With++LastPiece),
    NOldLines = LineNo2-LineNo1+1,
%%     io:format("LineNo1, NOldLines, length(Lines) WLines ~p \n",
              ?D([LineNo1, NOldLines, length(Lines), WLines]),
    {LineNo1, NOldLines, WLines,
     replace_between(LineNo1, NOldLines, WLines, Lines)}.

replace_between2(From, Length, With, In) ->
    string:substr(In, 1, From)++With++string:substr(In, From+Length+1).

lines_to_text(Lines) ->
	lists:append([L || {_, L} <- Lines, [L] =/= [eof]]).

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
        {all, From} ->
            From ! {all, self(), Modules},
            ?MODULE:loop(Modules);
		{modules, From} ->
			Mods = [M#module.name || M <- Modules],
			From ! {modules, self(), Mods},
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
                    ?D(a),
                    From ! {get_text, self(), lines_to_text(Module#module.lines)},
                    ?D(b);
                false ->
                    From ! {get_text, self(), module_not_found}
            end,
            ?MODULE:loop(Modules);
        {get_text_line, From, Mod, Line} ->
            case lists:keysearch(Mod, #module.name, Modules) of
                {value, Module} ->
                    L = (catch lists:nth(Line+1, Module#module.lines)),
                    From ! {get_text_line, self(), L};
                false ->
                    From ! {get_text_line, self(), module_not_found}
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
    Lines = split_lines_w_lengths(InitialText) ++ [{1, [eof]}],
    LineTokens = [scan_line(L) || L <- Lines],
    #module{name=ScannerName, lines=Lines, tokens=LineTokens}.

scan_line({1, [eof]} = T) ->
	T;
scan_line({Length, S}) ->
    {ok, T, _} = erlide_scan:string(S, {0, 0}),
    {Length, erlide_scan:filter_ws(T)}.

replace_text(Module, Offset, RemoveLength, NewText) ->
    ?D({text_length, length(lines_to_text(Module#module.lines))}),
    ?D({Module#module.name, Offset, RemoveLength, length(NewText)}),
    {Line, NOldLines, AffectedLines, NewLines} = 
        replace_between_lines(Offset, RemoveLength, NewText, Module#module.lines),
    LineTokens = [scan_line(L) || L <- AffectedLines],
    NewTokens = replace_between(Line, NOldLines, LineTokens, Module#module.tokens),
    Module#module{lines=NewLines, tokens=NewTokens}.

get_token_at(Module, Offset) ->
    case find_line_w_offset(Offset, Module#module.tokens) of
        {N, Pos, _Length, Tokens} ->
            case get_token_at_aux(Tokens, Offset - Pos) of
                token_not_found ->
                    token_not_found;
                T ->
                    M = mktoken(T, Pos, N),
                    {ok, M}
            end;
        not_found ->
            line_not_found
    end.

get_tokens_at(Module, Offset, N) -> 
    get_tokens_at(Module, Offset, N, []).

get_tokens_at(_Module, _Offset, 0, Acc) ->
    lists:reverse(Acc);
get_tokens_at(Module, Offset, N, Acc0) ->
    case find_line_w_offset(Offset, Module#module.tokens) of
        {LineNo, Pos, Length, Tokens} ->
            {M, Ts} = get_tokens_at_aux(Tokens, Offset - Pos, N),
            Acc1 = 
                lists:foldl(fun(T, LAcc) ->
                                    [mktoken(T, Pos, LineNo) | LAcc]
                            end, Acc0, Ts),
            get_tokens_at(Module, Pos+Length, N-M, Acc1);
        not_found ->
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
          when Offset > LineOfs+Ofs+Len ->
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
    {get_tokens_at(Module, Offset, After), get_tokens_before(Module, Offset, Before)}.

token_pos({_, Pos}) -> Pos;
token_pos({_, Pos, _}) -> Pos;
token_pos({_, Pos, _, _}) ->Pos;
token_pos(eof) -> 1000000000.

mktoken({dot, {{L, O}, G}}, Ofs, NL) ->
    #token{kind=dot, line=L+NL, offset=O+Ofs, length=G, text="."};
mktoken({ws, {{L, O}, G}, T}, Ofs, NL) ->
    #token{kind=ws, line=L+NL, offset=O+Ofs, length=G, text=T};
mktoken({K, {{L, O}, G}}, Ofs, NL) ->
    #token{kind=K, line=L+NL, offset=O+Ofs, length=G};
mktoken({K, {{L, O}, G}, V}, Ofs, NL) ->
    #token{kind=K, line=L+NL, offset=O+Ofs, length=G, value=V};
mktoken({K, {{L, O}, G}, V, T}, Ofs, NL) ->
    #token{kind=K, line=L+NL, offset=O+Ofs, length=G, value=V, text=T};
mktoken(eof, _, _) ->
    eof.

