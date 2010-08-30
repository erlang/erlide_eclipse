%% Author: jakob
%% Created: 30 aug 2010
%% Description: TODO: Add description to erlide_parsing_test
-module(erlide_parsing_test).

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

%% records from erlide_noparse
%% TODO: should we move this to an interface somehow?

-record(model, {forms, comments}).

-record(function, {pos, name, arity, args, head, clauses, name_pos, comment, exported}).
-record(clause, {pos, name, args, head, name_pos}).
-record(attribute, {pos, name, args, extra}).
-record(other, {pos, name, tokens}).

parse_directive_test_() ->
    [?_assertEqual(#model{forms = [#attribute{pos = {{0,0,0},51},
                                              name = compile,
                                              extra = "[inline,{hipe,[{regalloc,linear_scan}]}]"}],
                          comments = []},
                   test_parse("-compile([inline,{hipe,[{regalloc,linear_scan}]}])."))].


%%
%% Local Functions
%%

test_parse(S) ->
    erlide_scanner_server:initialScan(testing, "", S, "/tmp", false),
    {ok, Res, unused} = erlide_noparse:reparse(testing),
    Res.

%% t() ->
%%     erlide_noparse:initial_parse(testing, ModuleFileName, StateDir, UpdateCaches, UpdateSearchServer),
