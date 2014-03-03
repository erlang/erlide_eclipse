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

scanner_test_() ->
    [?_assertEqual({[#token{kind = atom, line = 0, offset = 0,length = 1, value = a, text="a"},
                     #token{kind = '(', line = 0, offset = 1, length = 1, text="("},
                     #token{kind = ')', line = 0, offset = 2, length = 1, text=")"},
                     #token{kind = '->', line = 0, offset = 4, length = 2, text="->"},
                     #token{kind = atom, line = 0, offset = 7, length = 1, value = b, text="b"},
                     #token{kind = dot, line = 0, offset = 8, length = 1, text = "."}],
                    [#token{kind = atom, line = 0, offset = 0,length = 4, value = test, text="test"},
                     #token{kind = '(', line = 0, offset = 4, length = 1, text="("},
                     #token{kind = ')', line = 0, offset = 5, length = 1, text=")"},
                     #token{kind = '->', line = 0, offset = 7, length = 2, text="->"},
                     #token{kind = atom, line = 0, offset = 10, length = 1, value = b, text="b"},
                     #token{kind = dot, line = 0, offset = 11, length = 1, text = "."}]},
                   test_replace("a() -> b.", 0, 1, "test"))
    ].

replace_at_eof_test_() ->
    [?_assertEqual({[#token{kind = atom, line = 0, offset = 0, length = 2, value = ab, text="ab"}],
                    [#token{kind = atom, line = 0, offset = 0, length = 3, value = abc, text="abc"}]},
                   test_replace("ab", 2, 0, "c"))
     ].

replace_at_eol_test_() ->
    [?_assertEqual({[#token{kind = atom, line = 0, offset = 0, length = 1, value = a, text="a"},
                     #token{kind = atom, line = 1, offset = 2, length = 1, value = b, text="b"}],
                    [#token{kind = atom, line = 0, offset = 0, length = 2, value = ac, text="ac"},
                     #token{kind = atom, line = 1, offset = 3, length = 1, value = b, text="b"}]},
                   test_replace("a\nb", 1, 0, "c"))
     ].

%%
%% Local Functions
%%

test_replace(S, Pos, RemoveLength, NewText) ->
    M = erlide_scan_model:do_scan(testing, S),
    NewM = erlide_scan_model:replace_text(M, Pos, RemoveLength, NewText),
    R1 = erlide_scan_model:get_all_tokens(M),
    R2 = erlide_scan_model:get_all_tokens(NewM),
    {R1, R2}.
