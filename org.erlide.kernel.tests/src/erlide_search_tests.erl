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

%%
%% API Functions
%%

-define(ARI_TYPESPEC, -2).
-define(ARI_ATTRIBUTE, -3).
-define(ARI_RECORD_DEF, -4).
-define(ARI_MACRO_DEF, -5).
-define(ARI_INCLUDE, -6).
-define(ARI_RECORD_FIELD_DEF, -7).

record_ref_within_type_spec_test() ->
    S = "-record(a, {b, c :: #rec{}}).",
    Expected = {ok, [{"xxx", a, ?ARI_RECORD_DEF, [], false, 20, 4, false}]},
    Value = test_refs(S, {record_ref, rec}),
    [?_assertEqual(Expected, Value)].

record_field_ref_test() ->
    S = "f() -> #record{a=ok}, #record.a.",
    Expected = {ok, [{"xxx",f,0,[],false,30,1,false},
                     {"xxx", f, 0, [], false, 15, 1, false}]},
    Value = test_refs(S, {record_field_ref, record, a}),
    [?_assertEqual(Expected, Value)].

%%
%% Local Functions
%%

test_refs(S, SearchPattern) ->
    erlide_parsing_tests:test_parse(S),
    erlide_search_server:find_refs([SearchPattern], [{testing, "xxx"}], "/tmp").
