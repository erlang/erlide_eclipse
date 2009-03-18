%% This library is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation; either version 2 of the
%% License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%% USA
%%

%% @doc Wrangler's interpretation of EUnit symbolic test representation

-module(wrangler_eunit).

-export([test_iterator/1]).

-define(DEFAULT_SETUP_PROCESS, spawn).

%% @type tests() =
%%            SimpleTest  
%%          | [tests()]
%%          | moduleName()   
%%          | {module, moduleName()}
%%          | {application, appName()}
%%          | {application, appName(), [term()]}
%%          | fileName()         ;;
%%          | {file, fileName()}
%%          | {string(), tests()}  ;;
%%          | {generator, () -> tests()}
%%          | {generator, M::moduleName(), F::functionName()}
%%          | {spawn, tests()}
%%          | {spawn, Node::atom(), tests()}
%%          | {timeout, T::number(), tests()}
%%          | {inorder, tests()}
%%          | {inparallel, tests()}
%%          | {inparallel, N::integer(), tests()}
%%          | {with, X::any(), [AbstractTestFunction]}
%%          | {setup, Where::local | spawn | {spawn, Node::atom()},
%%                    Setup::() -> (R::any()),
%%                    Cleanup::(R::any()) -> any(),
%%                    tests() | Instantiator
%%            }
%%          | {setup, Setup, Cleanup, tests() | Instantiator}
%%          | {setup, Where, Setup, tests() | Instantiator}
%%          | {setup, Setup, tests() | Instantiator}
%%          | {foreach, Where::local | spawn | {spawn, Node::atom()},
%%                      Setup::() -> (R::any()),
%%                      Cleanup::(R::any()) -> any(),
%%                      [tests() | Instantiator]
%%            }
%%          | {foreach, Setup, Cleanup, [tests() | Instantiator]}
%%          | {foreach, Where, Setup, [tests() | Instantiator]}
%%          | {foreach, Setup, [tests() | Instantiator]}
%%          | {foreachx, Where::local | spawn | {spawn, Node::atom()},
%%                       SetupX::(X::any()) -> (R::any()),
%%                       CleanupX::(X::any(), R::any()) -> any(),
%%                       Pairs::[{X::any(),
%%                                (X::any(), R::any()) -> tests()}]
%%            }
%%          | {foreachx, SetupX, CleanupX, Pairs}
%%          | {foreachx, Where, SetupX, Pairs}
%%          | {foreachx, SetupX, Pairs}
%%          | {node, Node::atom(), tests() | Instantiator}
%%          | {node, Node, Args::string(), tests() | Instantiator}
%%
%% SimpleTest = TestFunction | {Line::integer(), SimpleTest}
%%
%% TestFunction = () -> any()
%%              | {M::moduleName(), F::functionName()}.
%%
%% AbstractTestFunction = (X::any()) -> any()
%%
%% Instantiator = (R::any()) -> tests()
%%              | {with, [AbstractTestFunction]}
%%
%% Note that `{string(), ...}' is a short-hand for `{string(), {...}}'
%% if the tuple contains more than two elements.
%%
%% @type moduleName() = atom()
%% @type functionName() = atom()
%% @type arity() = integer()
%% @type appName() = atom()
%% @type fileName() = string()

test_iterator(Tests) ->
    case eunit_lib:dlist_next(Tests) of 
	[T |Ts] ->
	    T1 = wrangler_parse(T),
	    {T1, test_iterator(Ts)};
	[] ->
	    none
    end.

wrangler_parse({foreach,_S, Fs}) ->
    {foreach, setup_fun,wrangler_parse_1(Fs)};
wrangler_parse({foreach, _S, _C, Fs}) ->
    {foreach, setup_fun, clean_fun, wrangler_parse_1(Fs)};
wrangler_parse({foreach, P, _S, _C, Fs}) ->
    {foreach,P, setup_fun, clean_fun, wrangler_parse_1(Fs)};
wrangler_parse({foreachx, _S, P}) ->
    {foreachx, setup_fun, wrangler_parse_2(P)};
wrangler_parse({foreachx, _S, _C, P}) ->
    {foreachx, setup_fun, clean_fun, wrangler_parse_2(P)};
wrangler_parse({foreachx, P, S1, C1, Ps}) ->
    {foreachx, P, S1, C1, wrangler_parse_2(Ps)};
wrangler_parse({generator, F}) -> {generator, F};
wrangler_parse({generator,F, M}) when is_atom(M), is_atom(F) ->
    {generator, M, F};
wrangler_parse({inorder, T}) ->
    {inorder, test_iterator(T)};
wrangler_parse({inparallel, T}) ->
    {inparallel, test_iterator(T)};
wrangler_parse({inparallel, N, T}) ->
    {inparallel, N, test_iterator(T)};
wrangler_parse({timeout, N, T}) ->
    {timeout, N, test_iterator(T)};
wrangler_parse({spawn, T}) ->
    {spawn, test_iterator(T)};
wrangler_parse({spawn, N, T}) ->
    {spawn, N, test_iterator(T)};
wrangler_parse({setup, S, T}) ->
    {setup, S, wrangler_parse_1(T)};
wrangler_parse({setup, _S, _C, T}) ->
    {setup, setup_fun, clean_fun, wrangler_parse_1(T)};
wrangler_parse({setup, _P, _S, _C, T} ) ->
    {setup, setup_place, setup_fun, clean_fun, wrangler_parse_1(T)};
wrangler_parse({node, N, T}) ->
    {node, N, wrangler_parse_1(T)};
wrangler_parse({node, N, A, T}) ->
    {node, N, A, wrangler_parse_1(T)};
wrangler_parse({module, M})  when is_atom(M) ->
	     {"module '" ++ atom_to_list(M) ++ "'"};
wrangler_parse({application, A}) ->
	     {application, A};
wrangler_parse({application, A, Info}) ->
	     {application, A, Info};
wrangler_parse({file, F}) ->
    {file, F};
wrangler_parse({dir, D}) ->
    {dir, D};
wrangler_parse({with, _X, _As}) -> 
    simple_test_function;

wrangler_parse({S, T}) when is_list(S) ->
    case is_string(S) of
	true -> 
	    {S, test_iterator(T)};
	_ ->
	    {S, T}
    end;
wrangler_parse(T) when is_tuple(T), size(T)>2, is_list(element(1, T)) ->
    [S|Es] = tuple_to_list(T),
    wrangler_parse({S, list_to_tuple(Es)});
wrangler_parse(M)  when is_atom(M) ->
   {module,M};
wrangler_parse(T) when is_list(T) ->
    case is_string(T) of 
	true ->
	    T;
	_ -> T
    end;
wrangler_parse(T) ->
    wrangler_parse_simple(T).



wrangler_parse_1({with, _As}) ->					
    simple_test_function;
wrangler_parse_1(T) when is_function(T)->
    case erlang:fun_info(T, arity) of
	{arity, 1} ->
	    try T(undefined) of 
		Val -> test_iterator(Val)
	    catch		
		_:_ -> bad_test
	    end;
	_ -> bad_test
    end;
wrangler_parse_1(T) ->
    test_iterator(T).

wrangler_parse_2([{_X,F1}|Ps]) ->
    case erlang:fun_info(F1, arity) of 
	{arity, 2} ->
	    try F1(undefined, undefined) of 
		Val -> [test_iterator(Val) | wrangler_parse_2(Ps)]
	    catch
		_:_ -> [bad_test|wrangler_parse_2(Ps)]
	    end;
	_ -> [bad_test|wrangler_parse_2(Ps)]
    end;
wrangler_parse_2([]) ->
    [].
	    

wrangler_parse_simple({L, F}) when is_integer(L), L>=0, is_function(F)->
    {0, simple_test_function};
wrangler_parse_simple({{M, N, A}, F}) when  is_atom(M), is_atom(N), is_integer(A) ->
    {{M, N,A},F};
wrangler_parse_simple(F) -> wrangler_parse_function(F).

wrangler_parse_function(F) when is_function(F) ->
    simple_test_function;
wrangler_parse_function({M, F}) when is_atom(M), is_atom(F) ->
    {M, F};
wrangler_parse_function(_F) -> bad_test.
    

is_string([C| Cs]) when is_integer(C), C >= 0, C =< 1114111 ->
    is_string(Cs);
is_string([_| _]) ->
    false;
is_string([]) ->
    true;
is_string(_) ->
    false.


