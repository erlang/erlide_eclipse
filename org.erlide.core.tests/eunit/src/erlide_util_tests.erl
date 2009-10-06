-module(erlide_util_tests).

-include_lib("eunit/include/eunit.hrl").


reverse_nil_test() -> [] = lists:reverse([]).
reverse_one_test() -> [1] = lists:reverse([1]).
reverse_two_test() -> [2,1] = lists:reverse([1,2]).

reverse_fail_test() -> [2,1] = lists:reverse([1,2,3]).