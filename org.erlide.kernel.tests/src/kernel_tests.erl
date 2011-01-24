-module(kernel_tests).

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
	[
	 erlide_util_tests,
	 erlide_scanner_tests,
         erlide_parsing_tests,
         erlide_indent_tests,
         erlide_search_tests
	].