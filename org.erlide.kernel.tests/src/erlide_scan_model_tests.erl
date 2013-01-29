%% @author jakob
%% @doc @todo Add description to erlide_scan_model_tests.


-module(erlide_scan_model_tests).


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

insert_at_eof_test_() ->
    [?_assertEqual({[#token{kind = atom, line = 0, offset = 0, length = 2, value = ab}],
                    [#token{kind = atom, line = 0, offset = 0, length = 3, value = abc}]},
                   test_replace("ab", 2, 0, "c"))
     ].

insert_at_eol_test_() ->
    [?_assertEqual({[#token{kind = atom, line = 0, offset = 0, length = 1, value = a},
                     #token{kind = atom, line = 1, offset = 2, length = 1, value = b}],
                    [#token{kind = atom, line = 0, offset = 0, length = 2, value = ac},
                     #token{kind = atom, line = 1, offset = 3, length = 1, value = b}]},
                   test_replace("a\nb", 1, 0, "c"))
     ].

%%
%% Local Functions
%%

test_scan(S) ->
    M = erlide_scan_model:do_scan(testing, S),
    erlide_scan_model:get_all_tokens(M).

test_replace(S, Pos, RemoveLength, NewText) ->
    M = erlide_scan_model:do_scan(testing, S),
    NewM = erlide_scan_model:replace_text(M, Pos, RemoveLength, NewText),
    R1 = erlide_scan_model:get_all_tokens(M),
    R2 = erlide_scan_model:get_all_tokens(NewM),
    {R1, R2}.
