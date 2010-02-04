%% Author: jakob
%% Created: 12 okt 2009
%% Description: TODO: Add description to erlide_scanner_tests
-module(erlide_scanner_tests).

%%
%% Include files
%%

-include_lib("eunit/include/eunit.hrl").
-include("erlide_scanner.hrl").

%%
%% Exported Functions
%%

%%
%% API Functions
%%

scanner_light_scan_string_test_() ->
	[?_assertEqual({ok, [#token{kind = atom, line = 0, offset = 0,length = 1, value = a},
						 #token{kind = '(', line = 0, offset = 1, length = 1},
						 #token{kind = ')', line = 0, offset = 2, length = 1},
						 #token{kind = ws, line = 0, offset = 3, length = 1, text = " "},
						 #token{kind = '->', line = 0, offset = 4, length = 2},
						 #token{kind = ws, line = 0, offset = 6, length = 1, text = " "},
						 #token{kind = atom, line = 0, offset = 7, length = 1, value = b},
						 #token{kind = dot, line = 0, offset = 8, length = 1, text = "."}]},
				   erlide_scanner:light_scan_string("a() -> b."))].

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

%%
%% Local Functions
%%

test_scan(S) ->
	erlide_scanner_server:initialScan(testing, "", S, "/tmp", "", false), 
	R = erlide_scanner_server:getTokens(testing),
	erlide_scanner_server:destroy(testing),
	R.

test_replace(S, Pos, RemoveLength, NewText) ->
	erlide_scanner_server:initialScan(testing, "", S, "/tmp", "", false), 
	R1 = erlide_scanner_server:getTokens(testing),
	erlide_scanner_server:replaceText(testing, Pos, RemoveLength, NewText),
	R2 = erlide_scanner_server:getTokens(testing),
	erlide_scanner_server:destroy(testing),
	{R1, R2}.
