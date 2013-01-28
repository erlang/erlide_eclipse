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
    [?_assertEqual({ok,
                    <<?TOK_ATOM, 0:24, 0:24, 1:24,
                      $(, 0:24, 1:24, 1:24,
                      $), 0:24, 2:24, 1:24,
                      ?TOK_WS, 0:24, 3:24, 1:24,
                      ?TOK_ARROW, 0:24, 4:24, 2:24,
                      ?TOK_WS, 0:24, 6:24, 1:24,
                      ?TOK_ATOM, 0:24, 7:24, 1:24,
                      ?TOK_OTHER, 0:24, 8:24, 1:24>>},
                   erlide_scanner:light_scan_string(<<"a() -> b.">>, latin1))].

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
