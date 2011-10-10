%% Author: jakob
%% Created: 30 aug 2010
%% Description: TODO: Add description to erlide_parsing_test
-module(erlide_parsing_tests).

%%
%% Include files
%%

-include_lib("eunit/include/eunit.hrl").
-include("erlide_scanner.hrl").

%%
%% Exported Functions
%%

-export([test_parse/1]).

%%
%% API Functions
%%

%% records from erlide_noparse
%% TODO: should we move this to an interface somehow?

-record(model, {forms, comments}).

-record(function, {pos, name, arity, args, head, clauses, name_pos, comment, exported}).
%% -record(clause, {pos, name, args, head, name_pos}).
-record(attribute, {pos, name, args, extra}).
%% -record(other, {pos, name, tokens}).

parse_directive_test_() ->
    [?_assertEqual(#model{forms = [#attribute{pos = {{0,0,0},51},
                                              name = compile,
                                              args = u,
                                              extra = "[inline,{hipe,[{regalloc,linear_scan}]}]"}],
                          comments = []},
                   test_parse("-compile([inline,{hipe,[{regalloc,linear_scan}]}])."))].

parse_small_functions_test_() ->
    [?_assertEqual(#model{forms=[#function{pos = {{0,1,0},14},
                                           name = f, arity = 0,
                                           args = [], head = "", clauses = [],
                                           name_pos = {{0, 0}, 1},
                                           comment = undefined,
                                           exported = false}],
                          comments=[]},
                   test_parse("f() ->\n    a."))].

parsing_define_with_record_ref_test_() ->
    %% http://www.assembla.com/spaces/erlide/tickets/602-parser-can-t-handle-record-definitions-in-defines
    [?_assertEqual(#model{forms=[#attribute{pos={{0,0,0},18},
                                            name=define,
                                            args=xx,
                                            extra="xx, #x{}"}],
                          comments=[]},
                   test_parse("-define(xx, #x{})."))].

parsing_record_def_test_() ->
    Expected = #model{forms = [#attribute{pos = {{0, 0, 0}, 32},
                                          name = record,
                                          args = {a, [{b, {{0, 0, 12}, 1}, ""},
                                                      {c, {{0, 0, 15}, 1}, ":: integer()"}]},
                                          extra = "a, {b, c :: integer()}"}],
                      comments = []},
    Value = test_parse("-record(a, {b, c :: integer()})."),
    [?_assertEqual(Expected, Value)].

parsing_function_with_macro_test_() ->
    %% http://www.assembla.com/spaces/erlide/tickets/571-functions-defined-with-macros-confuses-the-model
    [?_assertEqual(#model{forms=[#function{pos = {{0, 0, 0},12},
                                           name = '?f', arity = 0,
                                           args = [], head = "", clauses = [],
                                           name_pos = {{0, 0}, 2},
                                           comment = undefined,
                                           exported = false}],
                          comments = []},
                   test_parse("?f() -> ok."))].

parsing_when_clauses_test_() ->
    S = "" ++
            "foo() ->\n"++
            "case A of\n"++ 
            "    ?CHST_HRL when is_integer(A);\n"++
            "                   is_list(A) ->\n",
    [?_assertEqual(#model{forms = [#function{pos = {{0,3,0},86},
                                             name = foo,arity = 0,args = [],head = [],
                                             clauses = [],
                                             name_pos = {{0,0},3},
                                             comment = undefined,exported = false}],
                          comments = []},
                   test_parse(S))].

function_comments_only_toplevel_test_() ->
    %% http://www.assembla.com/spaces/erlide/tickets/891-wrong-function-comment-in-edoc-view-and-hover 
    S = "" ++
            "f1()->\n"++
            "    %some comment here \n"++
            "    foo:bar().\n"++
            "f2() ->\n"++
            "    ok.",
    [?_assertEqual(#model{forms = [#function{pos = {{0,2,0},46},
                                             name = f1,arity = 0,args = [],head = [],clauses = [],
                                             name_pos = {{0,0},2},
                                             comment = undefined,exported = false},
                                   #function{pos = {{3,4,46},16},
                                             name = f2,arity = 0,args = [],head = [],clauses = [],
                                             name_pos = {{3,46},2},
                                             comment = undefined,exported = false}],
                          comments = [#token{kind = comment,line = 1,offset = 11,
                                             length = 19,value = <<"%some comment here ">>,text = u,
                                             last_line = u}]},
                   test_parse(S))].

%%
%% Local Functions
%%

test_parse(S) ->
    erlide_scanner_server:initialScan(testing, "", S, "/tmp", false),
    {ok, Res, unused} = erlide_noparse:reparse(testing),
    Res.

%% t() ->
%%     erlide_noparse:initial_parse(testing, ModuleFileName, StateDir, UpdateCaches, UpdateSearchServer),
