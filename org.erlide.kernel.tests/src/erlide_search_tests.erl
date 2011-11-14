


%% Author: jakob
%% Created: 20 dec 2010
%% Description: TODO: Add description to erlide_search_tests
-module(erlide_search_tests).

%%
%% Include files
%%

-include_lib("eunit/include/eunit.hrl").

%%
%% Exported Functions
%%

-compile(export_all).
%%
%% API Functions
%%

-define(ARI_TYPESPEC, -2).
-define(ARI_ATTRIBUTE, -3).
-define(ARI_RECORD_DEF, -4).
-define(ARI_MACRO_DEF, -5).
-define(ARI_INCLUDE, -6).
-define(ARI_RECORD_FIELD_DEF, -7).

record_ref_within_type_spec_test_() ->
    S = "-record(a, {b, c :: #rec{}}).",
    Expected = {ok, [{"xxx", a, ?ARI_RECORD_DEF, [], false, 20, 4, false}]},
    Value = test_refs(S, {record_ref, rec}),
    [?_assertEqual(Expected, Value)].

record_field_ref_test_() ->
    S = "f() -> #record{a=ok}, #record.a.",
    Expected = {ok, [{"xxx",f,0,[],false,30,1,false},
                     {"xxx", f, 0, [], false, 15, 1, false}]},
    Value = test_refs(S, {record_field_ref, record, a}),
    [?_assertEqual(Expected, Value)].

external_call_after_record_dot_field_test_() ->
    S = "f() ->\n    #a.b,\n    a:f().\n",
    Expected = {ok, [{"xxx", f, 0, [], false, 21, 3, false}]},
    Value = test_refs(S, {external_call, a, f, 0}),
    [?_assertEqual(Expected, Value)].

local_call_in_record_test_() ->
    S = "a() ->\n    y(),\n    A#b{x = y()}.\n",
    Expected = {ok, [{"xxx", a, 0, [], false, 11, 1, false},
                     {"xxx", a, 0, [], false, 28, 1, false}]},
    Value = test_refs(S, {external_call, xxx, y, 0}),
    [?_assertEqual(Expected, Value)].

external_call_after_empty_record_test_() ->
    S = "f() ->\n    #x{},\n    a:f().\n",
    Expected1 = {ok, [{"xxx", f, 0, [], false, 21, 3, false}]},
    Value1 = test_refs(S, {external_call, a, f, 0}),
    Expected2 = {ok, [{"xxx", f, 0, [], false, 11, 2, false}]},
    Value2 = test_refs(S, {record_ref, x}),
    [?_assertEqual(Expected1, Value1),
     ?_assertEqual(Expected2, Value2)].

%%
%% Local Functions
%%

test_refs(S, SearchPattern) ->
    erlide_parsing_tests:test_parse(S),
    erlide_search_server:find_refs([SearchPattern], [{testing, "xxx"}], "/tmp").
