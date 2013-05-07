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

open_record_def_test_() ->
    [?_assertEqual({record, rec},
                  open_test("-module(a).\n-record(rec, {aa, bb}).\n", 13))
     ].

open_test(S, Offset) ->
    erlide_scanner:create(test),
    erlide_scanner:initial_scan(test, "", S, "", false, off),
    R = erlide_open:open(test, Offset, #open_context{imports=[]}),
    erlide_scanner:dispose(test),
    R.
