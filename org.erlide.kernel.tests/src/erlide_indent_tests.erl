%% Author: jakob
%% Created: 24 nov 2009
%% Description: TODO: Add description to erlide_indent_tests
-module(erlide_indent_tests).

%%
%% Include files
%%

-include_lib("eunit/include/eunit.hrl").
-include("erlide_scanner.hrl").

%%
%% test Functions
%%

simple_function_test_() ->
    ?_assertEqual("a() ->\n    b.\n", 
                  erlide_indent:indent_lines("a() ->\nb.\n", 0, 10, 8, true, [])).

expressions_test_() ->
    S = "#r{a=a,\nb=b, [a,\nb],\n{a, b,\nc, fn(a, \nb)}},",
    ?_assertEqual("#r{a=a,\n   b=b, [a,\n\t b],\n   {a, b,\n    c, fn(a, \n\t  b)}},",
                  erlide_indent:indent_lines(S, 0, length(S), 8, true, [])).

%%
%% Local Functions
%%
