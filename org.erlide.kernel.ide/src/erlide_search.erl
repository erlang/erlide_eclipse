%% @author jakob
%% the functional part of searching

-module(erlide_search).

%%
%% Include files
%%

%% -define(DEBUG, 1).

-include("erlide.hrl").

-include("erlide_search.hrl").

%%
%% Exported Functions
%%

-export([find_data/4]).

%%
%% API Functions
%%

find_data(Refs, Pattern, ModuleAtom, ModuleName) ->
    find_data(Refs, Pattern, ModuleAtom, ModuleName, []).

%%
%% Local Functions
%%

find_data([], _, _, _, Acc) ->
    Acc;
find_data([#ref{function=F, arity=A, clause=C, data=D, offset=O, length=L, sub_clause=S} | Rest],
          Pattern, Mod, M, Acc) ->
    NewAcc = case check_pattern(Pattern, Mod, D, F, A, C) of
                 true ->
                     [{M, F, A, C, S, O, L, is_def(D)} | Acc];
                 false ->
                     Acc
             end,
    find_data(Rest, Pattern, Mod, M, NewAcc).

%% is_def(Ref) returns true if the ref is a definition
is_def(#function_def{}) -> true;
is_def(#macro_def{}) -> true;
is_def(#type_def{}) -> true;
is_def(#module_def{}) -> true;
is_def(#var_def{}) -> true;
is_def(#record_field_def{}) -> true;
is_def(_) -> false.

check_pattern(Pattern, Mod, #local_call{function=F, arity=A}, _, _, _)->
    check_function_ref(#external_call{module=Mod, function=F, arity=A}, Pattern);
check_pattern(Pattern, Mod, #function_def{function=F, arity=A} = FD, _, _, _)->
    check_function_ref(FD, Pattern) orelse
        check_function_ref(#function_def_mod{module=Mod, function=F, arity=A}, Pattern);
check_pattern(Pattern, Mod, #type_ref{module='_', type=T}, _, _, _) ->
    lists:member(#type_ref{module=Mod, type=T}, Pattern);
check_pattern(Pattern, _Mod, #var_ref{}=VR, F, A, C) ->
    check_var_pattern(Pattern, VR, F, A, C);
check_pattern(Pattern, _Mod, #var_def{}=VD, F, A, C) ->
    check_var_pattern(Pattern, VD, F, A, C);
check_pattern(Pattern, _Mod, D, _, _, _) ->
    lists:member(D, Pattern).

check_function_ref(_, []) ->
    false;
check_function_ref(#external_call{module=Mod, function=F, arity=A1}, [#external_call{module=Mod, function=F, arity=A2}|_]) ->
    A1==A2 orelse A2==undefined;
check_function_ref(#function_def{function=F, arity=A1}, [#function_def{function=F, arity=A2}|_]) ->
    A1==A2 orelse A2==undefined;
check_function_ref(#function_def_mod{module=Mod, function=F, arity=A1}, [#function_def_mod{module=Mod, function=F, arity=A2}|_]) ->
    A1==A2 orelse A2==undefined;
check_function_ref(X, [_|Tail]) ->
    check_function_ref(X, Tail).



check_var_pattern([], _, _, _, _) ->
    false;
check_var_pattern([#var_pattern{vardefref=VL, function=F, arity=A, clause=C} | Rest], V, F, A, C) ->
    ?D({VL, F, A, C}),
    case lists:member(V, VL) of
        true -> true;
        false -> check_var_pattern(Rest, V, F, A, C)
    end;
check_var_pattern([#var_pattern{vardefref=VL, function='', arity=-1, clause=""} | Rest], V, F, A, C) ->
    ?D({VL, F, A, C}),
    case lists:member(V, VL) of
        true -> true;
        false -> check_var_pattern(Rest, V, F, A, C)
    end;
check_var_pattern([_ | Rest], V, F, A, C) ->
    check_var_pattern(Rest, V, F, A, C).
