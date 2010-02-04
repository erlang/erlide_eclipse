-module(erlide_util_tests).

-include_lib("eunit/include/eunit.hrl").

reverse_test_() ->
	[?_assertEqual([], lists:reverse([])),
	 ?_assertEqual([1], lists:reverse([1])),
	 ?_assertEqual([2,1], lists:reverse([1,2])),
	 ?_assertEqual([3,2,1], lists:reverse([1,2,3]))
	 ].
