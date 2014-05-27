%% Author: jakob
%% Created: 7 may 2013
-module(erlide_open_tests).

%%
%% Include files
%%

-include_lib("eunit/include/eunit.hrl").
-include_lib("erlide_open.hrl").

%%
%% test Functions
%%

internal_split_test_() ->
    [
     ?_assertEqual({"abcd",2}, split("ab§cd")),
     ?_assertEqual({"cd",0}, split("§cd")),
     ?_assertEqual({"ab",2}, split("ab§")),
     ?_assertEqual({"ab",-1}, split("ab"))
    ].

open_test_() ->
    [
     ?_assertEqual({record, rec},
                   open_test("-r§ecord(rec, {aa, bb}).")),
     ?_assertEqual({local, foo, 1},
                   open_test("-module(a).\nf§oo(x)-> ok.")),
     ?_assertEqual({include, "foo"},
                   open_test("-includ§e(\"foo\").")),
     ?_assertEqual({include, "foo"},
                   open_test("-include(\"fo§o\").")),
     ?_assertEqual({include_lib,"file.hrl",
                   code:lib_dir(stdlib)++"/file.hrl"},
                   open_test("-includ§e_lib(\"stdlib/file.hrl\").")),
     ?_assertEqual({external,a,b,0,not_found},
                   open_test("foo()-> a§:b().")),
     ?_assertEqual({macro, '?HELLO'},
                   open_test("foo()-> ?HEL§LO.")),
     ?_assertEqual({variable,'AA'},
                   open_test("foo()-> A§A.")),
     ?_assertEqual({variable, 'BB'},
                   open_test("-a() -> #r{field1=A, field2=B§B}.")),
     ?_assertEqual({local, aa, 0},
                   open_test("-type a§a()::integer()."))
    ].

open_record_field_test_() ->
    [
     ?_assertEqual({field, rec, bb},
                   open_test("-a() -> #rec{aa, b§b}).")),
     ?_assertEqual({field, rec, aa},
                   open_test("-record(rec, {a§a, bb}).")),
     ?_assertEqual({field, rec, bb},
                   open_test("-record(rec, {aa, b§b}).")),
     ?_assertEqual({field, rec, aa},
                   open_test("-a() -> #rec{a§a, bb}).")),
     ?_assertEqual({field, r, field2},
                   open_test("-a() -> #r{field1=A, fiel§d2=B}.")),
     ?_assertEqual({field, r, field1},
                   open_test("-a() -> #r{fiel§d1=A, field2=B}."))
    ].

open_test(S) ->
    {S1, Offset} = split(S),
    erlide_scanner:create(test),
    erlide_scanner:initial_scan(test, "", S1, "", false),
    R = erlide_open:open(test, Offset, #open_context{imports=[]}),
    erlide_scanner:dispose(test),
    R.

split(S) ->
    split(S, [], 0).

split([], Acc, _N) ->
    {lists:reverse(Acc), -1};
split([$§ | T], Acc, N) ->
    {lists:reverse(Acc,T), N};
split([H|T], Acc, N) ->
    split(T, [H|Acc], N+1).
