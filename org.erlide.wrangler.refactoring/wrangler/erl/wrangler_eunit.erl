%% Copyright (c) 2010, Huiqing Li, Simon Thompson
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%     %% Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%     %% Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%     %% Neither the name of the copyright holders nor the
%%       names of its contributors may be used to endorse or promote products
%%       derived from this software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ''AS IS''
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
%% BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR 
%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF 
%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

%% @doc Wrangler's interpretation of EUnit symbolic test representation

-module(wrangler_eunit).

-export([test_iterator/1, test_iterator_renamer/3]).

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
%% @type functionarity() = integer()
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


test_iterator_renamer(Tests, OldName, NewName) ->
    case eunit_lib:dlist_next(Tests) of 
	[T |Ts] ->
	    T1 = wrangler_parse_rename(T, OldName, NewName),
	    {T1, test_iterator_renamer(Ts, OldName, NewName)};
	[] ->
	    none
    end.

wrangler_parse_rename({foreach,_S, Fs}, OldName, NewName) ->
    {foreach, setup_fun,wrangler_parse_rename_1(Fs, OldName, NewName)};
wrangler_parse_rename({foreach, _S, _C, Fs}, OldName, NewName) ->
    {foreach, setup_fun, clean_fun, wrangler_parse_rename_1(Fs, OldName, NewName)};
wrangler_parse_rename({foreach, P, _S, _C, Fs}, OldName, NewName) ->
    {foreach,P, setup_fun, clean_fun, wrangler_parse_rename_1(Fs, OldName, NewName)};
wrangler_parse_rename({foreachx, _S, P}, OldName, NewName) ->
    {foreachx, setup_fun, wrangler_parse_rename_2(P, OldName, NewName)};
wrangler_parse_rename({foreachx, _S, _C, P}, OldName, NewName) ->
    {foreachx, setup_fun, clean_fun, wrangler_parse_rename_2(P, OldName, NewName)};
wrangler_parse_rename({foreachx, P, S1, C1, Ps}, OldName, NewName) ->
    {foreachx, P, S1, C1, wrangler_parse_rename_2(Ps, OldName, NewName)};
wrangler_parse_rename({generator, F},_OldName, _NewName) -> {generator, F};
wrangler_parse_rename({generator,F, M}, OldName, NewName) when is_atom(M), is_atom(F) ->
    case M == OldName of
	true ->{generator, NewName, F};
	_ -> {generator, M, F}
    end;
wrangler_parse_rename({inorder, T}, OldName, NewName) ->
    {inorder, test_iterator_renamer(T, OldName, NewName)};
wrangler_parse_rename({inparallel, T}, OldName, NewName) ->
    {inparallel, test_iterator_renamer(T, OldName, NewName)};
wrangler_parse_rename({inparallel, N, T}, OldName, NewName) ->
    {inparallel, N, test_iterator_renamer(T, OldName, NewName)};
wrangler_parse_rename({timeout, N, T}, OldName, NewName) ->
    {timeout, N, test_iterator_renamer(T, OldName, NewName)};
wrangler_parse_rename({spawn, T}, OldName, NewName) ->
    {spawn, test_iterator_renamer(T, OldName, NewName)};
wrangler_parse_rename({spawn, N, T}, OldName, NewName) ->
    {spawn, N, test_iterator_renamer(T, OldName, NewName)};
wrangler_parse_rename({setup, S, T}, OldName, NewName) ->
    {setup, S, wrangler_parse_rename_1(T, OldName, NewName)};
wrangler_parse_rename({setup, _S, _C, T}, OldName, NewName) ->
    {setup, setup_fun, clean_fun, wrangler_parse_rename_1(T, OldName, NewName)};
wrangler_parse_rename({setup, _P, _S, _C, T}, OldName, NewName) ->
    {setup, setup_place, setup_fun, clean_fun, wrangler_parse_rename_1(T, OldName, NewName)};
wrangler_parse_rename({node, N, T}, OldName, NewName) ->
    {node, N, wrangler_parse_rename_1(T, OldName, NewName)};
wrangler_parse_rename({node, N, A, T}, OldName, NewName) ->
    {node, N, A, wrangler_parse_rename_1(T, OldName, NewName)};
wrangler_parse_rename({module, M}, OldName, NewName)  when is_atom(M) ->
    case M==OldName of 
	true ->{"module '" ++ atom_to_list(NewName) ++ "'"};
	_ -> {"module '" ++ atom_to_list(M) ++ "'"}
    end;	
wrangler_parse_rename({application, A}, _OldName, _NewName) ->
    {application, A};
wrangler_parse_rename({application, A, Info}, _OldName, _NewName) ->
	     {application, A, Info};
wrangler_parse_rename({file, F}, _OldName, _NewName) ->
    {file, F};
wrangler_parse_rename({dir, D}, _OldName, _NewName) ->
    {dir, D};
wrangler_parse_rename({with, _X, _As}, _OldName, _NewName) -> 
    simple_test_function;

wrangler_parse_rename({S, T}, OldName, NewName) when is_list(S) ->
    case is_string(S) of
	true -> 
	    {S, test_iterator_renamer(T, OldName, NewName)};
	_ ->
	    {S, T}
    end;
wrangler_parse_rename(T, OldName, NewName) when is_tuple(T), size(T)>2, is_list(element(1, T)) ->
    [S|Es] = tuple_to_list(T),
    wrangler_parse_rename({S, list_to_tuple(Es)}, OldName, NewName);
wrangler_parse_rename(M, OldName, NewName)  when is_atom(M) ->
    case M of
	OldName -> {module, NewName};
	_ -> {module,M}
    end;
wrangler_parse_rename(T, _OldName, _NewName) when is_list(T) ->
    case is_string(T) of 
	true ->
	    T;
	_ -> T
    end;
wrangler_parse_rename(T, OldName, NewName) ->
    wrangler_parse_rename_simple(T, OldName, NewName).


wrangler_parse_rename_1({with, _As}, _OldName, _NewName) ->					
    simple_test_function;
wrangler_parse_rename_1(T, _OldName, _NewName) when is_function(T)->
    case erlang:fun_info(T, arity) of
	{arity, 1} ->
	    try T(undefined) of 
		Val -> test_iterator_renamer(Val, _OldName, _NewName)
	    catch		
		_:_ -> bad_test
	    end;
	_ -> bad_test
    end;
wrangler_parse_rename_1(T, OldName, NewName) ->
    test_iterator_renamer(T, OldName, NewName).

wrangler_parse_rename_2([{_X,F1}|Ps], OldName, NewName) ->
    case erlang:fun_info(F1, arity) of 
	{arity, 2} ->
	    try F1(undefined, undefined) of 
		Val -> [test_iterator_renamer(Val, OldName, NewName) | wrangler_parse_rename_2(Ps, OldName, NewName)]
	    catch
		_:_ -> [bad_test|wrangler_parse_rename_2(Ps, OldName, NewName)]
	    end;
	_ -> [bad_test|wrangler_parse_rename_2(Ps, OldName, NewName)]
    end;
wrangler_parse_rename_2([], _OldName, _NewName) ->
    [].
	    

wrangler_parse_rename_simple({L, F}, _OldName, _NewName) when is_integer(L), L>=0, is_function(F)->
    {0, simple_test_function};
wrangler_parse_rename_simple({{M, N, A}, F}, OldName, NewName) when  is_atom(M), is_atom(N), is_integer(A) ->
    case M of 
	OldName -> {{NewName, N,A},F};
	_ ->  {{M, N,A},F}
    end;	
wrangler_parse_rename_simple(F, OldName, NewName) -> wrangler_parse_rename_function(F, OldName, NewName).

wrangler_parse_rename_function(F, _OldName, _NewName) when is_function(F) ->
    simple_test_function;
wrangler_parse_rename_function({M, F}, OldName, NewName) when is_atom(M), is_atom(F) ->
    case M of
	OldName -> {NewName, F};
	_ -> {M, F}
    end;	
wrangler_parse_rename_function(_F, _OldName, _NewName) -> bad_test.
    


