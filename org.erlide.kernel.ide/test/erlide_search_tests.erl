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

-define(ARI_TYPESPEC, -2).
-define(ARI_ATTRIBUTE, -3).
-define(ARI_RECORD_DEF, -4).
-define(ARI_MACRO_DEF, -5).
-define(ARI_INCLUDE, -6).
-define(ARI_RECORD_FIELD_DEF, -7).

record_ref_within_type_spec_test_() ->
    S = "-record(a, {b, c :: #rec{}}).",
    Expected = [{"xxx", a, ?ARI_RECORD_DEF, [], false, 20, 4, false}],
    Value = test_refs(S, {record_ref, rec}),
    [?_assertEqual(Expected, Value)].

record_field_ref_test_() ->
    S = "f() -> #record{a=ok}, #record.a.",
    Expected = [{"xxx",f,0,[],false,30,1,false},
                {"xxx", f, 0, [], false, 15, 1, false}],
    Value = test_refs(S, {record_field_ref, record, a}),
    [?_assertEqual(Expected, Value)].

external_call_after_record_dot_field_test_() ->
    S = "f() ->\n    #a.b,\n    a:f().\n",
    Expected = [{"xxx", f, 0, [], false, 21, 3, false}],
    Value = test_refs(S, {external_call, a, f, 0}),
    [?_assertEqual(Expected, Value)].

local_call_in_record_test_() ->
    S = "a() ->\n    y(),\n    A#b{x = y()}.\n",
    Expected = [{"xxx", a, 0, [], false, 11, 1, false},
                {"xxx", a, 0, [], false, 28, 1, false}],
    Value = test_refs(S, {external_call, xxx, y, 0}),
    [?_assertEqual(Expected, Value)].

external_call_after_empty_record_test_() ->
    S = "f() ->\n    #x{},\n    a:f().\n",
    Expected1 = [{"xxx", f, 0, [], false, 21, 3, false}],
    Value1 = test_refs(S, {external_call, a, f, 0}),
    Expected2 = [{"xxx", f, 0, [], false, 11, 2, false}],
    Value2 = test_refs(S, {record_ref, x}),
    [?_assertEqual(Expected1, Value1),
     ?_assertEqual(Expected2, Value2)].

find_second_field_in_record_match_test_() ->
    %% http://www.assembla.com/spaces/erlide/tickets/1268-searching---can-t-find-fields-in-record-match---construction
    S = "x(A, B) ->\n    #r{field1=A, field2=B}.",
    Value = test_refs(S, {record_field_ref, r, field2}),
    Expected = [{"xxx",x,2,"(A, B)",false,28,6,false}],
    [?_assertEqual(Expected, Value)].


%%
%% Local Functions
%%

test_refs(S, SearchPattern) ->
    {ok, Tokens, _EndPos} = erlide_scan:string(S),
    {_Forms, _Comments, Refs} = erlide_np:parse(Tokens),
    erlide_search:find_data(Refs, [SearchPattern], xxx, "xxx").
