-module(erlide_util_tests).

-include_lib("eunit/include/eunit.hrl").

reverse_test_() ->
	[
	 ?_assertEqual([[3,2,1],[a,b,c]], erlide_util:reverse2([[c,b,a],[1,2,3]]))
	].

pack_test_() ->
	[
	 ?_assertEqual(["aa","bb","cc"],
				   erlide_util:unpack("aa;bb;cc")),
	 ?_assertEqual("aa;bb;cc",
				   erlide_util:pack(["aa","bb","cc"]))
	].

string_test_() ->
	[
	 ?_assertEqual("ty",
				   erlide_util:get_from_str("Texty", "ex")),
	 ?_assertEqual(" Super ",
				   erlide_util:get_between_strs("More Super Help", "re", "He")),
	 ?_assertEqual([" Super ", "x mon", "s"],
				   erlide_util:get_all_between_strs("More Super Helprex monHes", "re", "He"))
	].

split_lines_test_() ->
	[
	 ?_assertEqual(["ab"],
				   erlide_util:split_lines("ab")),
	 ?_assertEqual(["ab"],
				   erlide_util:split_lines("ab\n")),
	 ?_assertEqual(["ab", "cd"],
				   erlide_util:split_lines("ab\ncd")),
	 ?_assertEqual(["ab", "cd"],
				   erlide_util:split_lines("ab\r\ncd")),
	 ?_assertEqual(["ab", "cd","ef","gh", "", "hi"],
				   erlide_util:split_lines("ab\ncd\ref\r\ngh\n\nhi")),
	 ?_assertEqual([],
				   erlide_util:split_lines(""))
	 ].