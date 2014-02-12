%% Description: TODO: Add description to erlide_scanner_tests
-module(erlide_scan_tests).

%%
%% Include files
%%

-include_lib("eunit/include/eunit.hrl").
-include("erlide_token.hrl").

-define(D(X), begin Y=X, io:format("~p~n", [Y]), Y end).

-define(CHAR, (case erlang:system_info(otp_release) of "R14"++_ -> integer; "R15"++_ -> integer; _-> char end)).

%%
%% Exported Functions
%%

tokens_test_() ->
    [?_assertEqual({ok, [#token{kind = atom, line = 0, offset = 0, length = 1, value = a, text="a"}],
                    {0,2,1}},
                   test_scan("a")),
     ?_assertEqual({ok, [#token{kind = var, line = 0, offset = 0, length = 1, value = 'A', text="A"}],
                    {0,2,1}},
                   test_scan("A")),
     ?_assertEqual({ok, [#token{kind = macro, line = 0, offset = 0, length = 2, value = '?a', text="?a"}],
                    {0,3,2}},
                   test_scan("?a")),
     ?_assertEqual({ok, [#token{kind = macro, line = 0, offset = 0, length = 2, value = '?A', text="?A"}],
                    {0,3,2}},
                   test_scan("?A")),
     ?_assertEqual({ok, [#token{kind = macro, line = 0, offset = 0, length = 3, value = '??a', text="??a"}],
                    {0,4,3}},
                   test_scan("??a")),
     ?_assertEqual({ok, [#token{kind = macro, line = 0, offset = 0, length = 3, value = '??A', text="??A"}],
                    {0,4,3}},
                   test_scan("??A")),
     ?_assertEqual({ok, [#token{kind = white_space, line = 0, offset = 0, length = 1, text= <<" ">>}],
                    {0,2,1}},
                   test_scan(" ")),
     ?_assertEqual({ok, [#token{kind = string, line = 0, offset = 0, length = 4, text= "\" a\"", value=" a"}],
                    {0,5,4}},
                   test_scan("\" a\"")),
     ?_assertEqual({ok, [#token{kind = string, line = 0, offset = 0, length = 5, text= "\"a\nb\"", value="a\nb"}],
                    {1,3,5}},
                   test_scan("\"a\nb\"")),
     %% this test depends on which version of erlang is running! R15- gives list of integers, R16+ gives string
     %%      ?_assertEqual({ok, [#token{kind = string, line = 0, offset = 0, length = 12, text= "\"z\\x{faca}z\"", value="z\x{faca}z"}],
     %%                     {0,13,12}},
     %%                    test_scan("\"z\\x{faca}z\"")),
     ?_assertEqual({ok, [#token{kind = string, line = 0, offset = 0, length = 4, text= "\"\\s\"", value="\s"}],
                    {0,5,4}},
                   test_scan("\"\\s\"")),
     ?_assertEqual({ok, [#token{kind = char, line = 0, offset = 0, length = 3, text= "$\\s", value=$\s}],
                    {0,4,3}},
                   test_scan("$\\s")),
     ?_assertEqual({ok, [#token{kind = ?CHAR, line = 0, offset = 0, length = 9, text= "$\\x{faca}", value=16#faca}],
                    {0,10,9}},
                   test_scan("$\\x{faca}")),
     ?_assertEqual({ok, [#token{kind = integer, line = 0, offset = 0, length = 1, text= "3", value=3}],
                    {0,2,1}},
                   test_scan("3")),
     ?_assertEqual({ok, [#token{kind = integer, line = 0, offset = 0, length = 3, text= "4#3", value=3}],
                    {0,4,3}},
                   test_scan("4#3")),
     ?_assertEqual({ok, [#token{kind = integer, line = 0, offset = 0, length = 4, text= "16#f", value=15}],
                    {0,5,4}},
                   test_scan("16#f")),
     ?_assertEqual({ok, [#token{kind = atom, line = 0, offset = 0, length = 5, text= "'try'", value='try'}],
                    {0,6,5}},
                   test_scan("'try'")),
     ?_assertEqual({ok, [#token{kind = comment, line = 0, offset = 0, length = 3, text= <<"% h">>}],
                    {0,4,3}},
                   test_scan("% h")),
     ?_assertEqual({ok, [#token{kind = float, line = 0, offset = 0, length = 3, text= "2.5", value=2.5}],
                    {0,4,3}},
                   test_scan("2.5")),
     ?_assertEqual({ok, [#token{kind = float, line = 0, offset = 0, length = 5, text= "2.5e3", value=2.5e3}],
                    {0,6,5}},
                   test_scan("2.5e3")),
     ?_assertEqual({ok, [#token{kind = float, line = 0, offset = 0, length = 6, text= "2.5e-3", value=2.5e-3}],
                    {0,7,6}},
                   test_scan("2.5e-3")),
     ?_assertEqual({ok, [#token{kind = dot, line = 0, offset = 0, length = 1, text= "."}],
                    {0,2,1}},
                   test_scan(".")),
     ?_assertEqual({ok, [#token{kind = 'try', line = 0, offset = 0, length = 3, text= "try"}],
                    {0,4,3}},
                   test_scan("try"))
    ].

scanner_test_() ->
    [?_assertEqual({ok, [#token{kind = atom, line = 0, offset = 0, length = 1, value = a, text="a"},
                         #token{kind = '(', line = 0, offset = 1, length = 1, text="("}],
                    {0,3,2}},
                   test_scan("a(")),
     ?_assertEqual({ok, [#token{kind = float, line = 0, offset = 0, length = 3, text= "2.5", value=2.5},
                         #token{kind = dot, line = 0, offset = 3, length = 1, text= "."}],
                    {0,5,4}},
                   test_scan("2.5.")),
     ?_assertEqual({ok, [#token{kind = atom, line = 0, offset = 0, length = 1, value = a, text="a"},
                         #token{kind = white_space, line = 0, offset = 1, length = 1, text= <<" ">>},
                         #token{kind = '(', line = 0, offset = 2, length = 1, text="("}],
                    {0,4,3}},
                   test_scan("a (")),
     ?_assertEqual({ok, [#token{kind = atom, line = 0, offset = 0, length = 1, value = a, text="a"},
                         #token{kind = white_space, line = 0, offset = 1, length = 1, text= <<"\n">>},
                         #token{kind = '(', line = 1, offset = 2, length = 1, text="("}],
                    {1,2,3}},
                   test_scan("a\n(")),
     ?_assertEqual({ok, [#token{kind = atom, line = 0, offset = 0, length = 1, value = a, text="a"},
                         #token{kind = white_space, line = 0, offset = 1, length = 2, text= <<"\n ">>},
                         #token{kind = '(', line = 1, offset = 3, length = 1, text="("}],
                    {1,3,4}},
                   test_scan("a\n (")),
     ?_assertEqual({ok, [#token{kind = atom, line = 0, offset = 0, length = 1, value = a, text="a"},
                         #token{kind = white_space, line = 0, offset = 1, length = 1, text= <<" ">>},
                         #token{kind = macro, line = 0, offset = 2, length = 2, text="?a", value='?a'},
                         #token{kind = white_space, line = 0, offset = 4, length = 1, text= <<" ">>},
                         #token{kind = atom, line = 0, offset = 5, length = 1, value = a, text="a"}],
                    {0,7,6}},
                   test_scan("a ?a a")),
     ?_assertEqual({ok, [#token{kind = atom, line = 0, offset = 0, length = 1, value = a, text="a"},
                         #token{kind = '(', line = 0, offset = 1, length = 1, text="("},
                         #token{kind = ')', line = 0, offset = 2, length = 1, text=")"},
                         #token{kind = white_space, line = 0, offset = 3, length = 1, text= <<" ">>},
                         #token{kind = '->', line = 0, offset = 4, length = 2, text="->"},
                         #token{kind = white_space, line = 0, offset = 6, length = 1, text= <<" ">>},
                         #token{kind = atom, line = 0, offset = 7, length = 1, value = b, text="b"},
                         #token{kind = dot, line = 0, offset = 8, length = 1, text = "."}],
                    {0,10,9}},
                   test_scan("a() -> b.")),
     ?_assertEqual({ok, [#token{kind = atom, line = 0, offset = 0, length = 1, value = a, text="a"},
                         #token{kind = '(', line = 0, offset = 1, length = 1, text="("},
                         #token{kind = ')', line = 0, offset = 2, length = 1, text=")"},
                         #token{kind = white_space, line = 0, offset = 3, length = 1, text= <<" ">>},
                         #token{kind = '->', line = 0, offset = 4, length = 2, text="->"},
                         #token{kind = white_space, line = 0, offset = 6, length = 2, text= <<"\n ">>},
                         #token{kind = atom, line = 1, offset = 8, length = 1, value = b, text="b"},
                         #token{kind = dot, line = 1, offset = 9, length = 1, text = "."},
                         #token{kind = white_space, line = 1, offset = 10, length = 1, text= <<"\n">>},
                         #token{kind = white_space, line = 2, offset = 11, length = 1, text= <<" ">>},
                         #token{kind = atom, line = 2, offset = 12, length = 1, value = x, text="x"}],
                    {2,3,13}},
                   test_scan("a() ->\n b.\n x")),
     ?_assertEqual({ok, [#token{kind = atom, line = 3, offset = 5, length = 1, value = a, text="a"}],
                    {3,5,6}},
                   test_scan("a", 3, 4, 5))
    ].

offset1_test() ->
    Str1 = "a ",
    Str2 = "b",
    {ok, T, {L,C,O}} = test_scan(Str1++Str2),
    {ok, T1, {L1,C1,O1}} = test_scan(Str1),
    {ok, T2, {L2,C2,O2}} = test_scan(Str2, L1, C1, O1),
    [?_assertEqual(T, T1++T2),
     ?_assertEqual({L,C,O}, {L2,C2,O2})].

offset2_test() ->
    Str1 = "a\n",
    Str2 = "b",
    {ok, T, {L,C,O}} = test_scan(Str1++Str2),
    {ok, T1, {L1,C1,O1}} = test_scan(Str1),
    {ok, T2, {L2,C2,O2}} = test_scan(Str2, L1, C1, O1),
    [?_assertEqual(T, T1++T2),
     ?_assertEqual({L,C,O}, {L2,C2,O2})].

offset_test_() ->
    [
     offset1_test(),
     offset2_test()
    ].

filter_test_() ->
    [
     ].

test_scan(S) ->
    test_scan(S, 0, 1, 0).

test_scan(S, L, C, O) ->
    erlide_scan:string(S, {L, C, O}, [return]).
