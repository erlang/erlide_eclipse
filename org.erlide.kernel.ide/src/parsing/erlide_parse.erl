-module(erlide_parse).

%% API
-export([consult/1]).

%% for tests
-export([split/1]).

consult(B) when is_binary(B) ->
    consult(binary_to_list(B));
consult(L) when is_list(L) ->
    {ok, Toks, _} = erl_scan:string(L),
    FormToks = split(Toks),
    lists:map(fun parse1/1, FormToks).

parse1(Toks) ->
    R = erl_parse:parse_exprs(Toks),
    case R of
        {ok, [Fs]} ->
            erl_parse:normalise(Fs);
        Err ->
            throw(Err)
    end.

split(L) ->
    split(L , [], []).

split([], R, []) ->
    lists:reverse(R);
split([], R, Acc) ->
    lists:reverse(R, [lists:reverse(Acc)]);
split([{dot, _}=H|T], R, Acc) ->
    split(T, [lists:reverse(Acc, [H])|R], []);
split([H|T], R, Acc) ->
    split(T, R, [H|Acc]).

