%% Description: TODO: Add description to erlide_scanner_tests
-module(erlide_scan_tests).

%%
%% Include files
%%

-include_lib("eunit/include/eunit.hrl").
-include("erlide_token.hrl").

%%
%% Exported Functions
%%

scanner_test_() ->
    [?_assertEqual([#token{kind = atom, line = 0, offset = 0, length = 1, value = a, text="a"}],
                   test_scan("a")),
     ?_assertEqual([#token{kind = atom, line = 0, offset = 0, length = 1, value = a, text="a"},
                    #token{kind = '(', line = 0, offset = 1, length = 1, text="("}],
                   test_scan("a(")),
     ?_assertEqual([#token{kind = atom, line = 0, offset = 0, length = 1, value = a, text="a"},
                    #token{kind = white_space, line = 0, offset = 1, length = 1, text=" "},
                    #token{kind = '(', line = 0, offset = 2, length = 1, text="("}],
                   test_scan("a (")),
     ?_assertEqual([#token{kind = atom, line = 0, offset = 0, length = 1, value = a, text="a"},
                    #token{kind = white_space, line = 0, offset = 1, length = 1, text="\n"},
                    #token{kind = '(', line = 1, offset = 2, length = 1, text="("}],
                   test_scan("a\n(")),
     ?_assertEqual([#token{kind = atom, line = 0, offset = 0, length = 1, value = a, text="a"},
                    #token{kind = white_space, line = 0, offset = 1, length = 2, text="\n "},
                    #token{kind = '(', line = 1, offset = 3, length = 1, text="("}],
                   test_scan("a\n (")),
     ?_assertEqual([#token{kind = atom, line = 0, offset = 0, length = 1, value = a, text="a"},
                    #token{kind = white_space, line = 0, offset = 1, length = 1, text=" "},
                    #token{kind = macro, line = 0, offset = 2, length = 2, text="?a", value='?a'},
                    #token{kind = white_space, line = 0, offset = 4, length = 1, text=" "},
                    #token{kind = atom, line = 0, offset = 5, length = 1, value = a, text="a"}],
                   test_scan("a ?a a")),
     ?_assertEqual([#token{kind = atom, line = 0, offset = 0, length = 1, value = a, text="a"},
                    #token{kind = white_space, line = 0, offset = 1, length = 1, text=" "},
                    #token{kind = macro, line = 0, offset = 2, length = 2, text="?A", value='?A'},
                    #token{kind = white_space, line = 0, offset = 4, length = 1, text=" "},
                    #token{kind = atom, line = 0, offset = 5, length = 1, value = a, text="a"}],
                   test_scan("a ?A a")),
     ?_assertEqual([#token{kind = atom, line = 0, offset = 0, length = 1, value = a, text="a"},
                    #token{kind = white_space, line = 0, offset = 1, length = 1, text=" "},
                    #token{kind = '?', line = 0, offset = 2, length = 1, text="?"},
                    #token{kind = '?', line = 0, offset = 3, length = 1, text="?"},
                    #token{kind = atom, line = 0, offset = 4, length = 1, text="a", value=a},
                    #token{kind = white_space, line = 0, offset = 5, length = 1, text=" "},
                    #token{kind = atom, line = 0, offset = 6, length = 1, value = a, text="a"}],
                   test_scan("a ??a a")),
     ?_assertEqual([#token{kind = atom, line = 0, offset = 0, length = 1, value = a, text="a"},
                    #token{kind = '(', line = 0, offset = 1, length = 1, text="("},
                    #token{kind = ')', line = 0, offset = 2, length = 1, text=")"},
                    #token{kind = white_space, line = 0, offset = 3, length = 1, text=" "},
                    #token{kind = '->', line = 0, offset = 4, length = 2, text="->"},
                    #token{kind = white_space, line = 0, offset = 6, length = 1, text=" "},
                    #token{kind = atom, line = 0, offset = 7, length = 1, value = b, text="b"},
                    #token{kind = dot, line = 0, offset = 8, length = 1, text = "."}],
                   test_scan("a() -> b.")),
     ?_assertEqual([#token{kind = atom, line = 0, offset = 0, length = 1, value = a, text="a"},
                    #token{kind = '(', line = 0, offset = 1, length = 1, text="("},
                    #token{kind = ')', line = 0, offset = 2, length = 1, text=")"},
                    #token{kind = white_space, line = 0, offset = 3, length = 1, text=" "},
                    #token{kind = '->', line = 0, offset = 4, length = 2, text="->"},
                    #token{kind = white_space, line = 0, offset = 6, length = 2, text="\n "},
                    #token{kind = atom, line = 1, offset = 8, length = 1, value = b, text="b"},
                    #token{kind = dot, line = 1, offset = 9, length = 1, text = "."},
                    #token{kind = white_space, line = 1, offset = 10, length = 1, text="\n"},
                    #token{kind = white_space, line = 2, offset = 11, length = 1, text=" "},
                    #token{kind = atom, line = 2, offset = 12, length = 1, value = x, text="x"}],
                   test_scan("a() ->\n b.\n x"))
     ].

filter_test_() ->
    [
     ].

test_scan(S) ->
    {ok, Tokens, _EndPos} = erlide_scan:string(S, {0, 1}, [return]),
    Tokens.
