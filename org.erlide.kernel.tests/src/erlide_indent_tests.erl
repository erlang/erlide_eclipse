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
    S = "a() ->\n    b.\n",
    SIndent = "a() ->\nb.\n",
    test_indent(SIndent, S).

expressions_test_() ->
    S = "#r{a=a,\nb=b, [a,\nb],\n{a, b,\nc, fn(a, \nb)}},",
    SIndent = "#r{a=a,\n   b=b, [a,\n         b],\n   {a, b,\n    c, fn(a, \n          b)}},",
    test_indent(SIndent, S).

try_catch_test_() ->
    S = "cmd(Cmd, From, Args, Modules) ->\ntry\ncase get(logging) of\non ->\n"++
            "put(log, get(log)++[{Cmd, Args}]);\n_ ->\nok\nend,\n"++
            "case do_cmd(Cmd, Args, Modules) of\n{R, NewMods} ->\nreply(Cmd, From, R),\n"++
            "NewMods;\nNewMods ->\nreply(Cmd, From, ok),\nNewMods\nend\ncatch\n"++
            "exit:Error ->\nreply(Cmd, From, {exit, Error}),\nModules;\n"++
            "error:Error ->\nreply(Cmd, From, {error, Error}),\nModules\nend.",
    SIndent = "cmd(Cmd, From, Args, Modules) ->\n    try\n"++
                  "        case get(logging) of\n            on ->\n"++
                  "                put(log, get(log)++[{Cmd, Args}]);\n"++
                  "            _ ->\n                ok\n        end,\n"++
                  "        case do_cmd(Cmd, Args, Modules) of\n"++
                  "            {R, NewMods} ->\n                reply(Cmd, From, R),\n"++
                  "                NewMods;\n            NewMods ->\n"++
                  "                reply(Cmd, From, ok),\n"++
                  "                NewMods\n        end\n"++
                  "    catch\n        exit:Error ->\n"++
                  "            reply(Cmd, From, {exit, Error}),\n"++
                  "            Modules;\n        error:Error ->\n"++
                  "            reply(Cmd, From, {error, Error}),\n"++
                  "            Modules\n    end.\n",
    test_indent(SIndent, S).

%%
%% Local Functions
%%

test_indent(SIndent, S) ->
    ?_assertEqual(SIndent, erlide_indent:indent_lines(S, 0, length(S), 8, false, [])).