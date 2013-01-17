%% Author: jakob
%% Created: 30 aug 2010
%% Description: TODO: Add description to erlide_parsing_test
-module(erlide_parsing_tests).

%%
%% Include files
%%

-include_lib("eunit/include/eunit.hrl").
-include("erlide_scanner.hrl").
-include("erlide_noparse.hrl").

%%
%% Exported Functions
%%

-export([test_parse/1]).

%%
%% API Functions
%%

parse_directive_test_() ->
    [?_assertEqual({[#attribute{pos = {{0,0,0},51},
                                name = compile,
                                args = u,
                                extra = "[inline,{hipe,[{regalloc,linear_scan}]}]"}],
                    []},
                   test_parse("-compile([inline,{hipe,[{regalloc,linear_scan}]}])."))].

parse_small_functions_test_() ->
    [?_assertEqual({[#function{pos = {{0,1,0},14},
                               name = f, arity = 0,
                               args = [], head = "", clauses = [],
                               name_pos = {{0, 0}, 1},
                               exported = false}],
                    []},
                   test_parse("f() ->\n    a."))].

parsing_define_with_record_ref_test_() ->
    %% http://www.assembla.com/spaces/erlide/tickets/602-parser-can-t-handle-record-definitions-in-defines
    [?_assertEqual({[#attribute{pos={{0,0,0},18},
                                name=define,
                                args=xx,
                                extra="xx, #x{}"}],
                    []},
                   test_parse("-define(xx, #x{})."))].

parsing_record_def_test_() ->
    Expected = {[#attribute{pos = {{0, 0, 0}, 32},
                            name = record,
                            args = {a, [{b, {{0, 0, 12}, 1}, ""},
                                        {c, {{0, 0, 15}, 1}, ":: integer()"}]},
                            extra = "a, {b, c :: integer()}"}],
                []},
    Value = test_parse("-record(a, {b, c :: integer()})."),
    [?_assertEqual(Expected, Value)].

parsing_function_with_macro_test_() ->
    %% http://www.assembla.com/spaces/erlide/tickets/571-functions-defined-with-macros-confuses-the-model
    [?_assertEqual({[#function{pos = {{0, 0, 0},12},
                               name = '?f', arity = 0,
                               args = [], head = "", clauses = [],
                               name_pos = {{0, 0}, 2},
                               exported = false}],
                    []},
                   test_parse("?f() -> ok."))].

parsing_when_clauses_test_() ->
    S = "" ++
            "foo() ->\n"++
            "case A of\n"++
            "    ?CHST_HRL when is_integer(A);\n"++
            "                   is_list(A) ->\n",
    [?_assertEqual({[#function{pos = {{0,3,0},86},
                               name = foo,arity = 0,args = [],head = [],
                               clauses = [],
                               name_pos = {{0,0},3},
                               exported = false}],
                    []},
                   test_parse(S))].

% this test is not relevant, since the function doc fixing is moved to java
function_comments_only_toplevel_test_() ->
    %% http://www.assembla.com/spaces/erlide/tickets/891-wrong-function-comment-in-edoc-view-and-hover
    S = "" ++
            "f1()->\n"++
            "    %some comment here \n"++
            "    foo:bar().\n"++
            "f2() ->\n"++
            "    ok.",
    [?_assertEqual({[#function{pos = {{0,2,0},46},
                               name = f1,arity = 0,args = [],head = [],clauses = [],
                               name_pos = {{0,0},2},
                               exported = false},
                     #function{pos = {{3,4,46},16},
                               name = f2,arity = 0,args = [],head = [],clauses = [],
                               name_pos = {{3,46},2},
                               exported = false}],
                    []},
                   test_parse(S))].

%%
%% Local Functions
%%

test_parse(S) ->
    {ok, RawTokens, _EndPos} = erlide_scan:string(S, {0, 0}),
    Tokens = erlide_scanner:convert_tokens(erlide_scan:filter_ws(RawTokens)),
    {Forms, Comments, _Refs} = erlide_np:parse(Tokens),
    {Forms, Comments}.

%% t() ->
%%     erlide_noparse:initial_parse(testing, ModuleFileName, StateDir, UpdateCaches, UpdateSearchServer),
