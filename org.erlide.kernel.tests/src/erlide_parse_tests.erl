-module(erlide_parse_tests).

%%
%% Include files
%%

-include_lib("eunit/include/eunit.hrl").

split_test_() ->
    [
     ?_assertEqual([], erlide_parse:split([])),
     ?_assertEqual([[a,b,c]], erlide_parse:split([a,b,c])),
     ?_assertEqual([[a,{'.', 1}],[d,e]], erlide_parse:split([a,{'.', 1},d,e])),
     ?_assertEqual([[a,{'.', 1}],[d,e,{'.', 1}]], erlide_parse:split([a,{'.', 1},d,e,{'.', 1}]))
    ].
