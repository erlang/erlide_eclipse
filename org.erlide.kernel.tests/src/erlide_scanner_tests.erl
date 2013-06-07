%% Author: jakob
%% Created: 12 okt 2009
%% Description: TODO: Add description to erlide_scanner_tests
-module(erlide_scanner_tests).

%%
%% Include files
%%

-include_lib("eunit/include/eunit.hrl").
-include("erlide_token.hrl").

%%
%% Exported Functions
%%

%%
%% API Functions
%%

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

scanner_light_scan_string_test_() ->
    [
     ?_assertEqual({ok,
                    <<?TOK_ATOM, 0:24, 0:24, 1:24>>},
                   erlide_scanner:light_scan_string(<<"a">>, latin1)),
     ?_assertEqual({ok,
                    <<?TOK_ATOM, 0:24, 0:24, 3:24>>},
                   erlide_scanner:light_scan_string(<<"'a'">>, latin1)),
     ?_assertEqual({ok,
                    <<$(, 0:24, 0:24, 1:24>>},
                   erlide_scanner:light_scan_string(<<"(">>, latin1)),
     ?_assertEqual({ok,
                    <<?TOK_STR, 0:24, 0:24, 3:24>>},
                   erlide_scanner:light_scan_string(<<"\"a\"">>, latin1)),
     ?_assertEqual({ok,
                    <<?TOK_VAR, 0:24, 0:24, 1:24>>},
                   erlide_scanner:light_scan_string(<<"A">>, latin1)),
     ?_assertEqual({ok,
                    <<?TOK_CHAR, 0:24, 0:24, 2:24>>},
                   erlide_scanner:light_scan_string(<<"$a">>, latin1)),
     ?_assertEqual({ok,
                    <<?TOK_CHAR, 0:24, 0:24, 3:24>>},
                   erlide_scanner:light_scan_string(<<"$\\b">>, latin1)),
     ?_assertEqual({ok,
                    <<?TOK_CHAR, 0:24, 0:24, 3:24>>},
                   erlide_scanner:light_scan_string(<<"$\\n">>, latin1)),
     ?_assertEqual({ok,
                    <<?TOK_CHAR, 0:24, 0:24, 5:24>>},
                   erlide_scanner:light_scan_string(<<"$\\122">>, latin1)),
%%      ?_assertEqual({ok,
%%                     <<?TOK_CHAR, 0:24, 0:24, 5:24>>},
%%                    erlide_scanner:light_scan_string(<<"$\\x{faca}">>, latin1)),
     ?_assertEqual({ok,
                    <<?TOK_INTEGER, 0:24, 0:24, 1:24>>},
                   erlide_scanner:light_scan_string(<<"1">>, latin1)),
     ?_assertEqual({ok,
                    <<?TOK_FLOAT, 0:24, 0:24, 3:24>>},
                   erlide_scanner:light_scan_string(<<"1.1">>, latin1)),
     ?_assertEqual({ok,
                    <<?TOK_FLOAT, 0:24, 0:24, 5:24>>},
                   erlide_scanner:light_scan_string(<<"1.1e3">>, latin1)),
     ?_assertEqual({ok,
                    <<?TOK_KEYWORD, 0:24, 0:24, 3:24>>},
                   erlide_scanner:light_scan_string(<<"end">>, latin1)),
     ?_assertEqual({ok,
                    <<?TOK_ATOM, 0:24, 0:24, 5:24>>},
                   erlide_scanner:light_scan_string(<<"'end'">>, latin1)),
     ?_assertEqual({ok,
                    <<?TOK_ATOM, 0:24, 0:24, 1:24,
                      ?TOK_OTHER, 0:24, 1:24, 1:24>>},
                   erlide_scanner:light_scan_string(<<"a.">>, latin1)),
     ?_assertEqual({ok,
                    <<?TOK_WS, 0:24, 0:24, 3:24>>},
                   erlide_scanner:light_scan_string(<<" \t ">>, latin1)),
     ?_assertEqual({ok,
                    <<?TOK_ARROW, 0:24, 0:24, 2:24>>},
                   erlide_scanner:light_scan_string(<<"->">>, latin1)),
     ?_assertEqual({ok,
                    <<?TOK_MACRO, 0:24, 0:24, 3:24>>},
                   erlide_scanner:light_scan_string(<<"?hi">>, latin1)),
     ?_assertEqual({ok,
                    <<?TOK_MACRO, 0:24, 0:24, 3:24>>},
                   erlide_scanner:light_scan_string(<<"?HI">>, latin1)),
     ?_assertEqual({ok,
                    <<$?, 0:24, 0:24, 1:24,
                      $?, 0:24, 1:24, 1:24,
                      ?TOK_ATOM, 0:24, 2:24, 2:24>>},
                   erlide_scanner:light_scan_string(<<"??hi">>, latin1)),
     ?_assertEqual({ok,
                    <<?TOK_COMMENT, 0:24, 0:24, 2:24>>},
                   erlide_scanner:light_scan_string(<<"%b">>, latin1)),
     ?_assertEqual({ok,
                    <<?TOK_COMMENT, 0:24, 0:24, 2:24,
                      ?TOK_WS, 0:24, 2:24, 1:24>>},
                   erlide_scanner:light_scan_string(<<"%b\n">>, latin1)),
     ?_assertEqual({ok,
                    <<?TOK_WS, 0:24, 0:24, 1:24>>},
                   erlide_scanner:light_scan_string(<<"\n">>, latin1)),
     ?_assertEqual({ok,
                    <<?TOK_WS, 0:24, 0:24, 1:24,
                      ?TOK_WS, 1:24, 1:24, 1:24>>},
                   erlide_scanner:light_scan_string(<<"\n ">>, latin1)),
     ?_assertEqual({ok,
                    <<?TOK_WS, 0:24, 0:24, 1:24,
                      ?TOK_WS, 1:24, 1:24, 1:24,
                      ?TOK_WS, 1:24, 2:24, 1:24>>},
                   erlide_scanner:light_scan_string(<<"\n \n">>, latin1)),
     ?_assertEqual({ok,
                    <<?TOK_WS, 0:24, 0:24, 1:24,
                      ?TOK_ATOM, 1:24, 1:24, 1:24>>},
                   erlide_scanner:light_scan_string(<<"\nc">>, latin1)),
     ?_assertEqual({ok,
                    <<?TOK_ATOM, 0:24, 0:24, 1:24,
                      $(, 0:24, 1:24, 1:24,
                      $), 0:24, 2:24, 1:24,
                      ?TOK_WS, 0:24, 3:24, 1:24,
                      ?TOK_ARROW, 0:24, 4:24, 2:24,
                      ?TOK_WS, 0:24, 6:24, 1:24,
                      ?TOK_ATOM, 0:24, 7:24, 1:24,
                      ?TOK_OTHER, 0:24, 8:24, 1:24>>},
                   erlide_scanner:light_scan_string(<<"a() -> b.">>, latin1))
     ].

scanner_test_() ->
    [?_assertEqual([#token{kind = atom, line = 0, offset = 0,length = 1, value = a},
                    #token{kind = '(', line = 0, offset = 1, length = 1},
                    #token{kind = ')', line = 0, offset = 2, length = 1},
                    #token{kind = '->', line = 0, offset = 4, length = 2},
                    #token{kind = atom, line = 0, offset = 7, length = 1, value = b},
                    #token{kind = dot, line = 0, offset = 8, length = 1, text = "."}],
                   test_scan("a() -> b.")),
     ?_assertEqual({[#token{kind = atom, line = 0, offset = 0,length = 1, value = a},
                     #token{kind = '(', line = 0, offset = 1, length = 1},
                     #token{kind = ')', line = 0, offset = 2, length = 1},
                     #token{kind = '->', line = 0, offset = 4, length = 2},
                     #token{kind = atom, line = 0, offset = 7, length = 1, value = b},
                     #token{kind = dot, line = 0, offset = 8, length = 1, text = "."}],
                    [#token{kind = atom, line = 0, offset = 0,length = 4, value = test},
                     #token{kind = '(', line = 0, offset = 4, length = 1},
                     #token{kind = ')', line = 0, offset = 5, length = 1},
                     #token{kind = '->', line = 0, offset = 7, length = 2},
                     #token{kind = atom, line = 0, offset = 10, length = 1, value = b},
                     #token{kind = dot, line = 0, offset = 11, length = 1, text = "."}]},
                   test_replace("a() -> b.", 0, 1, "test"))
    ].

replace_at_eof_test_() ->
    [?_assertEqual({[#token{kind = atom, line = 0, offset = 0, length = 2, value = ab}],
                    [#token{kind = atom, line = 0, offset = 0, length = 3, value = abc}]},
                   test_replace("ab", 2, 0, "c"))
     ].

newline_char_test_() ->
    [?_assertEqual([#token{kind = '[', line = 0, offset = 0, length = 1},
                    #token{kind = char, line = 0, offset = 1, length = 3, value=$\n, text = "$\\n"},
                    #token{kind = ']', line = 0, offset = 4, length = 1}],
                   test_scan("[$\\n]"))
     ].

newline_char_simple_test_() ->
    [?_assertEqual({ok, [{'[', {{1,1},1}},
                         {char, {{1,2},3}, 10, "$\\n"},
                         {']',{{1,5}, 1}}
                         ], {1,6}},
                   erlide_scan:string("[$\\n]"))
     ].

%%
%% Local Functions
%%

test_scan(S) ->
    erlide_scanner:create(testing),
    erlide_scanner:initial_scan(testing, "", S, "/tmp", false, off),
    R = erlide_scanner:get_tokens(testing),
    erlide_scanner:dispose(testing),
    R.

test_replace(S, Pos, RemoveLength, NewText) ->
    erlide_scanner:create(testing),
    erlide_scanner:initial_scan(testing, "", S, "/tmp", false, off),
    R1 = erlide_scanner:get_tokens(testing),
    erlide_scanner:replace_text(testing, Pos, RemoveLength, NewText),
    R2 = erlide_scanner:get_tokens(testing),
    erlide_scanner:dispose(testing),
    {R1, R2}.
