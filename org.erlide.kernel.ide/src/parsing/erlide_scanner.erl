%% Author: jakob
%% Created: 24 apr 2008
%% Description:
-module(erlide_scanner).

%%
%% Include files
%%

%% -define(DEBUG, 1).

-include("erlide.hrl").
-include("erlide_token.hrl").
-include("erlide_scanner_server.hrl").

%%
%% Exported Functions
%%

-export([light_scan_string/2, scan_string/1, initial_scan/5, get_token_at/2,
         initial_scan/6, create/1, addref/1, dispose/1, get_text/1,
         get_text_line/2, get_tokens/1, get_token_window/4, dump_log/2,
         dump_module/1, replace_text/4, check_all/3]).

%%
%% API Functions
%%

-define(CACHE_VERSION, 24).

light_scan_string(B, latin1) ->
    S = binary_to_list(B),
    do_light_scan(S);
light_scan_string(B, utf8) ->
    S = unicode:characters_to_list(B),
    do_light_scan(S).

scan_string(B) when is_binary(B) ->
    scan_string(binary_to_list(B));
scan_string(L) when is_list(L) ->
    M = erlide_scan_model:do_scan('', L),
    erlide_scan_model:get_all_tokens(M).

initial_scan(ScannerName, ModuleFileName, InitialText, StateDir, UseCache) ->
    %%     Text = case InitialText of
    %%                "" ->
    %%                    {ok, B} = file:read_file(ModuleFileName),
    %%                    binary_to_list(B);
    %%                _ ->
    %%                    InitialText
    %%            end,
    Text = InitialText,
    CacheFileName = filename:join(StateDir, atom_to_list(ScannerName) ++ ".scan"),
    RenewFun = fun(_F) -> erlide_scan_model:do_scan(ScannerName, Text) end,
    Result = erlide_util:check_and_renew_cached(ModuleFileName, CacheFileName, ?CACHE_VERSION, RenewFun, UseCache),
    {Result, Text}.

get_token_at(ScannerName, Offset) when is_atom(ScannerName), is_integer(Offset) ->
    erlide_scanner_server:server_cmd(ScannerName, get_token_at, Offset).

initial_scan(ScannerName, ModuleFileName, InitialText, StateDir, UseCache, Logging)
  when is_atom(ScannerName), is_list(ModuleFileName), is_list(InitialText), is_list(StateDir) ->
    erlide_scanner_server:server_cmd(ScannerName, initial_scan,
                                     {ScannerName, ModuleFileName, InitialText, StateDir, UseCache, Logging}).

create(ScannerName) when is_atom(ScannerName) ->
    erlide_scanner_server:spawn_server(ScannerName).

addref(ScannerName) when is_atom(ScannerName) ->
    erlide_scanner_server:server_cmd(ScannerName, addref).

dispose(ScannerName) when is_atom(ScannerName) ->
    %% TODO move this from here?
    erlide_search_server:remove_module(ScannerName),
    erlide_scanner_server:server_cmd(ScannerName, dispose).

get_text(ScannerName) when is_atom(ScannerName) ->
    erlide_scanner_server:server_cmd(ScannerName, get_text).

get_text_line(ScannerName, Line) when is_atom(ScannerName), is_integer(Line) ->
    erlide_scanner_server:server_cmd(ScannerName, get_text_line, Line).

get_tokens(ScannerName) when is_atom(ScannerName) ->
    erlide_scanner_server:server_cmd(ScannerName, get_tokens).

get_token_window(ScannerName, Offset, Before, After)
  when is_atom(ScannerName), is_integer(Offset), is_integer(Before), is_integer(After) ->
    erlide_scanner_server:server_cmd(ScannerName, get_token_window, {Offset, Before, After}).

dump_module(ScannerName) when is_atom(ScannerName) ->
    erlide_scanner_server:server_cmd(ScannerName, dump_module).

dump_log(ScannerName, Filename) when is_atom(ScannerName) ->
    erlide_scanner_server:server_cmd(ScannerName, dump_log, Filename).

replace_text(ScannerName, Offset, RemoveLength, NewText)
  when is_atom(ScannerName), is_integer(Offset), is_integer(RemoveLength), is_list(NewText) ->
    erlide_scanner_server:server_cmd(ScannerName, replace_text, {Offset, RemoveLength, NewText}).

check_all(ScannerName, Text, GetTokens)
  when is_atom(ScannerName), is_list(Text), is_boolean(GetTokens) ->
    MatchTest = erlide_scanner_server:match_test(ScannerName, Text),
    ScanTest = erlide_scanner_server:scan_test(ScannerName, GetTokens),
    case ScanTest of
        {R, Tokens, T} ->
            {MatchTest ++ R, Tokens, T};
        _ ->
            MatchTest ++ ScanTest
    end.



%%
%% Local Functions
%%

do_light_scan(S) ->
    case erlide_scan:string(S, {0, 0}) of
        {ok, T, _} ->
            {ok, fixup_tokens(T, [])};
        {error, _, _} ->
            error
    end.

-define(TOK_OTHER, 0).
-define(TOK_WS, 1).
-define(TOK_STR, 2).
-define(TOK_ATOM, 3).
-define(TOK_VAR, 4).
-define(TOK_CHAR, 5).
-define(TOK_MACRO, 6).
-define(TOK_ARROW, 7).
-define(TOK_INTEGER,8).
-define(TOK_FLOAT, 9).
-define(TOK_COMMENT, 10).
-define(TOK_KEYWORD, 11).

kind_small(ws) -> ?TOK_WS;
kind_small(string) -> ?TOK_STR;
kind_small(atom) -> ?TOK_ATOM;
kind_small(var) -> ?TOK_VAR;
kind_small(macro) -> ?TOK_MACRO;
kind_small(dot) -> ?TOK_OTHER;
kind_small(float) -> ?TOK_FLOAT;
kind_small(integer) -> ?TOK_INTEGER;
kind_small(char) -> ?TOK_CHAR;
kind_small('->') -> ?TOK_ARROW;
kind_small(comment) -> ?TOK_COMMENT;
kind_small(Kind) when is_atom(Kind) ->
    case erlide_scan:reserved_word(Kind) of
        true ->
            ?TOK_KEYWORD;
        false ->
            case atom_to_list(Kind) of
                [I] when I > ?TOK_KEYWORD -> I;
                _ -> ?TOK_OTHER
            end
    end.

fixup_macro(L, O, G) ->
    ?D({macro, L, O, G}),
    <<?TOK_MACRO, L:24, O:24, (G+1):24>>.

fixup_tokens([], Acc) ->
    erlang:iolist_to_binary(Acc);
fixup_tokens([{'?', {{L, _}, _}}=T1, {'?', {{L, _}, _}}=T2, T3 | Rest], Acc) ->
    fixup_tokens(Rest, [Acc | [fixup_tokens([T1], []), fixup_tokens([T2], []),
                               fixup_tokens([T3], [])]]);
fixup_tokens([{'?', {{L, O}, _}}, {var, {{L, O1}, G}, _V} | Rest], Acc) when O1=:=O+1->
    T = fixup_macro(L, O, G),
    fixup_tokens(Rest, [Acc | T]);
fixup_tokens([{'?', {{L, O}, _}}, {atom, {{L, O1}, G}, _V, _Txt} | Rest], Acc) when O1=:=O+1->
    T = fixup_macro(L, O, G),
    fixup_tokens(Rest, [Acc | T]);
fixup_tokens([{'?', {{L, O}, _}}, {atom, {{L, O1}, G}, _V} | Rest], Acc) when O1=:=O+1->
    T = fixup_macro(L, O, G),
    fixup_tokens(Rest, [Acc | T]);
fixup_tokens([{Kind, {{L, O}, G}} | Rest], Acc) ->
    fixup_tokens(Rest, [Acc | <<(kind_small(Kind)), L:24, O:24, G:24>>]);
fixup_tokens([{Kind, {{L, O}, G}, _A} | Rest], Acc) ->
    fixup_tokens(Rest, [Acc | <<(kind_small(Kind)), L:24, O:24, G:24>>]);
fixup_tokens([{Kind, {{L, O}, G}, _A, _B} | Rest], Acc) ->
    fixup_tokens(Rest, [Acc | <<(kind_small(Kind)), L:24, O:24, G:24>>]).


