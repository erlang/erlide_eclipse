-module(core_tests).

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
	[
	 erlide_util_tests,
	 erlide_scanner_tests
	].