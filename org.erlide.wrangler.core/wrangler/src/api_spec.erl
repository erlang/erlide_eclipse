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
%% =====================================================================
%% Some utility functions for manipulating type specs.
%%
%% Author contact: hl@kent.ac.uk
%% =====================================================================

%% @private
%% @hidden
-module(api_spec).

-export([rename_in_spec/2, add_arg_type_to_spec/3,
         tuple_arg_types_in_spec/3, 
         rm_arg_type_from_spec/2,
         swap_arg_types_in_spec/3,
         replace_arg_type_in_spec/3,
         replace_ret_type_in_spec/2,
         is_type_spec/1,
         is_type_spec/2]).

-include("../include/wrangler_internal.hrl"). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rename_in_spec(Spec, {F,A}) when is_atom(F) andalso is_integer(A) -> 
      case is_type_spec(Spec) of 
          true ->
              NewFunSpec = wrangler_syntax:tuple([wrangler_syntax:atom(F),
                                                  wrangler_syntax:integer(A)]),
              update_funspec(Spec, NewFunSpec);
          false ->
              Spec
      end;      
rename_in_spec(Spec, {M, F, A}) when is_atom(M), is_atom(F), is_integer(A) ->
    NewFunSpec = wrangler_syntax:tuple([wrangler_syntax:atom(M), wrangler_syntax:atom(F),
                                        wrangler_syntax:integer(A)]),
    update_funspec(Spec, NewFunSpec).
    
update_funspec(Spec, NewFunSpec) ->
    AttrName = wrangler_syntax:attribute_name(Spec),
    [FunSpec, TypeSpecs] = wrangler_syntax:attribute_arguments(Spec),
    NewFunSpec1 = wrangler_misc:rewrite(FunSpec, NewFunSpec),
    NewSpec =wrangler_syntax:attribute(
               AttrName, [NewFunSpec1, TypeSpecs]),
    wrangler_misc:rewrite(Spec, NewSpec).


add_arg_type_to_spec(Spec, Type, Index)->
    AttrName = wrangler_syntax:attribute_name(Spec),
    [FunSpec, TypeSpecs] = wrangler_syntax:attribute_arguments(Spec),
    SpecCs = wrangler_syntax:list_elements(TypeSpecs),
    NewSpecCs = [add_arg_type_to_spec_clause(C, Type, Index)||C<-SpecCs],
    NewFunSpec = case wrangler_syntax:tuple_elements(FunSpec) of 
                     [Mod, Fun, Arity] ->
                         NewArity = wrangler_syntax:integer(
                                      wrangler_syntax:concrete(Arity)+1),
                         wrangler_syntax:tuple([Mod, Fun, NewArity]);
                     [Fun, Arity] ->
                         NewArity = wrangler_syntax:integer(
                                      wrangler_syntax:concrete(Arity)+1),
                         wrangler_syntax:tuple([Fun, NewArity]) 
                 end,
    NewTypeSpecs=wrangler_misc:rewrite(TypeSpecs, 
                                       wrangler_syntax:list(NewSpecCs)),
    NewSpec =wrangler_syntax:attribute(
               AttrName, [NewFunSpec, NewTypeSpecs]),
    wrangler_misc:rewrite(Spec, NewSpec).

add_arg_type_to_spec_clause(C, Type, Index) ->
    T1 = wrangler_syntax:revert(C), 
    case T1 of 
        {type, Line, bounded_fun, [T, Gs]} ->
            NewT = add_arg_type_to_spec_clause(T, Type, Index),
            {type, Line, bounded_fun, [NewT, Gs]};
        {type, Line, 'fun', [FType, Ret]} ->
            NewFType = add_arg_type_to_spec_clause_1(FType, Type, Index),
            {type, Line, 'fun', [NewFType, Ret]}
    end.

add_arg_type_to_spec_clause_1(T={type, _Line, any}, _, _)->
    T;
add_arg_type_to_spec_clause_1({type, Line, product, Ts}, Type, Index) ->
    Ts1 = lists:sublist(Ts, Index),
    Ts2 = lists:nthtail(Index, Ts),
    NewTs =Ts1 ++ [Type]++Ts2,
    {type, Line, product, NewTs}.
                  

tuple_arg_types_in_spec(Spec, Index1, Index2) when Index2>=Index1 ->
    AttrName = wrangler_syntax:attribute_name(Spec),
    [FunSpec, TypeSpecs] = wrangler_syntax:attribute_arguments(Spec),
    SpecCs = wrangler_syntax:list_elements(TypeSpecs),
    NewSpecCs = [tuple_args_in_spec_clause(C, Index1, Index2)||C<-SpecCs],
    NewFunSpec = case wrangler_syntax:tuple_elements(FunSpec) of 
                     [Mod, Fun, Arity] ->
                         NewArity = wrangler_syntax:integer(
                                      wrangler_syntax:concrete(Arity)-Index2+Index1),
                         wrangler_syntax:tuple([Mod, Fun, NewArity]);
                     [Fun, Arity] ->
                         NewArity = wrangler_syntax:integer(
                                      wrangler_syntax:concrete(Arity)-Index2+Index1),
                         wrangler_syntax:tuple([Fun, NewArity]) 
                 end,
    NewTypeSpecs=wrangler_misc:rewrite(TypeSpecs, 
                                       wrangler_syntax:list(NewSpecCs)),
    NewSpec =wrangler_syntax:attribute(
               AttrName, [NewFunSpec, NewTypeSpecs]),
    wrangler_misc:rewrite(Spec, NewSpec).

tuple_args_in_spec_clause(C, Index1, Index2) ->
    T1 = wrangler_syntax:revert(C), 
    case T1 of 
        {type, Line, bounded_fun, [T, Gs]} ->
            NewT = tuple_args_in_spec_clause(T, Index1, Index2),
            {type, Line, bounded_fun, [NewT, Gs]};
        {type, Line, 'fun', [FType, Ret]} ->
            NewFType = tuple_args_in_spec_clause_1(FType, Index1, Index2),
            {type, Line, 'fun', [NewFType, Ret]}
    end.

tuple_args_in_spec_clause_1(T={type, _Line, any}, _Index1, _Index2)->
    T;
tuple_args_in_spec_clause_1({type, Line, product, Ts}, Index1, Index2) ->
    Ts1 = lists:sublist(Ts, Index1-1),
    Ts2 = lists:sublist(Ts, Index1, Index2),
    Ts3 = lists:nthtail(Index2, Ts),
    TupledT = {type, Line, tuple, Ts2},
    {type, Line, product, Ts1++[TupledT|Ts3]}.
    

rm_arg_type_from_spec(Spec, Index) ->
    AttrName = wrangler_syntax:attribute_name(Spec),
    [FunSpec, TypeSpecs] = wrangler_syntax:attribute_arguments(Spec),
    SpecCs = wrangler_syntax:list_elements(TypeSpecs),
    NewSpecCs = [rm_arg_type_from_spec_clause(C,Index)||C<-SpecCs],
    NewFunSpec = case wrangler_syntax:tuple_elements(FunSpec) of 
                     [Mod, Fun, Arity] ->
                         NewArity = wrangler_syntax:integer(
                                      wrangler_syntax:concrete(Arity)-1),
                         wrangler_syntax:tuple([Mod, Fun, NewArity]);
                     [Fun, Arity] ->
                         NewArity = wrangler_syntax:integer(
                                      wrangler_syntax:concrete(Arity)-1),
                         wrangler_syntax:tuple([Fun, NewArity]) 
                 end,
    NewTypeSpecs=wrangler_misc:rewrite(TypeSpecs, 
                                       wrangler_syntax:list(NewSpecCs)),
    NewSpec =wrangler_syntax:attribute(
               AttrName, [NewFunSpec, NewTypeSpecs]),
    wrangler_misc:rewrite(Spec, NewSpec).

rm_arg_type_from_spec_clause(C, Index) ->
    T1 = wrangler_syntax:revert(C), 
    case T1 of 
        {type, Line, bounded_fun, [T, Gs]} ->
            NewT = rm_arg_type_from_spec_clause(T, Index),
            {type, Line, bounded_fun, [NewT, Gs]};
        {type, Line, 'fun', [FType, Ret]} ->
            NewFType = rm_arg_type_from_spec_clause_1(FType, Index),
            {type, Line, 'fun', [NewFType, Ret]}
    end.

rm_arg_type_from_spec_clause_1(T={type, _Line, any}, _)->
    T;
rm_arg_type_from_spec_clause_1({type, Line, product, Ts}, Index) ->
    Ts1 = lists:sublist(Ts, Index-1),
    Ts2 = lists:nthtail(Index, Ts),
    NewTs =Ts1 ++ Ts2,
    {type, Line, product, NewTs}.
       
swap_arg_types_in_spec(Spec, Index1, Index2) ->
    AttrName = wrangler_syntax:attribute_name(Spec),
    [FunSpec, TypeSpecs] = wrangler_syntax:attribute_arguments(Spec),
    SpecCs = wrangler_syntax:list_elements(TypeSpecs),
    NewSpecCs = [swap_args_in_spec_clause(C, Index1, Index2)||C<-SpecCs],
    NewTypeSpecs=wrangler_misc:rewrite(TypeSpecs, 
                                       wrangler_syntax:list(NewSpecCs)),
    NewSpec =wrangler_syntax:attribute(
               AttrName, [FunSpec, NewTypeSpecs]),
    wrangler_misc:rewrite(Spec, NewSpec).

swap_args_in_spec_clause(C, Index1, Index2) ->
    T1 = wrangler_syntax:revert(C), 
    case T1 of 
        {type, Line, bounded_fun, [T, Gs]} ->
            NewT = swap_args_in_spec_clause(T, Index1, Index2),
            {type, Line, bounded_fun, [NewT, Gs]};
        {type, Line, 'fun', [FType, Ret]} ->
            NewFType = swap_args_in_spec_clause_1(FType, Index1, Index2),
            {type, Line, 'fun', [NewFType, Ret]}
    end.

swap_args_in_spec_clause_1(T={type, _Line, any}, _Index1, _Index2)->
    T;
swap_args_in_spec_clause_1({type, Line, product, Ts}, Index1, Index2) ->
    Ith = lists:nth(Index1, Ts),
    Jth = lists:nth(Index2, Ts),
    T = list_to_tuple(Ts),
    T1=setelement(Index2, setelement(Index1, T, Jth), Ith),
    {type, Line, product, tuple_to_list(T1)}.
    
replace_arg_type_in_spec(Spec, Type, Index) ->
    AttrName = wrangler_syntax:attribute_name(Spec),
    [FunSpec, TypeSpecs] = wrangler_syntax:attribute_arguments(Spec),
    SpecCs = wrangler_syntax:list_elements(TypeSpecs),
    NewSpecCs = [replace_arg_type_in_spec_clause(C, Type, Index)||C<-SpecCs],
    NewTypeSpecs=wrangler_misc:rewrite(TypeSpecs, 
                                       wrangler_syntax:list(NewSpecCs)),
    NewSpec =wrangler_syntax:attribute(
               AttrName, [FunSpec, NewTypeSpecs]),
    wrangler_misc:rewrite(Spec, NewSpec).

replace_arg_type_in_spec_clause(C, Type, Index) ->
    T1 = wrangler_syntax:revert(C), 
    case T1 of 
        {type, Line, bounded_fun, [T, Gs]} ->
            NewT = replace_arg_type_in_spec_clause(T, Type, Index),
            {type, Line, bounded_fun, [NewT, Gs]};
        {type, Line, 'fun', [FType, Ret]} ->
            NewFType = replace_arg_type_in_spec_clause_1(FType, Type, Index),
            {type, Line, 'fun', [NewFType, Ret]}
    end.

replace_arg_type_in_spec_clause_1(T={type, _Line, any}, _, _)->
    T;
replace_arg_type_in_spec_clause_1({type, Line, product, Ts}, Type, Index) ->
    Ts1 = lists:sublist(Ts, Index-1),
    Ts2 = lists:nthtail(Index, Ts),
    NewTs =Ts1 ++ [Type]++Ts2,
    {type, Line, product, NewTs}.
                  

replace_ret_type_in_spec(Spec, Type) ->
    AttrName = wrangler_syntax:attribute_name(Spec),
    [FunSpec, TypeSpecs] = wrangler_syntax:attribute_arguments(Spec),
    SpecCs = wrangler_syntax:list_elements(TypeSpecs),
    NewSpecCs = [replace_ret_type_in_spec_clause(C, Type)||C<-SpecCs],
    NewTypeSpecs=wrangler_misc:rewrite(TypeSpecs, 
                                       wrangler_syntax:list(NewSpecCs)),
    NewSpec =wrangler_syntax:attribute(
               AttrName, [FunSpec, NewTypeSpecs]),
    wrangler_misc:rewrite(Spec, NewSpec).

replace_ret_type_in_spec_clause(C, Type) ->
    T1 = wrangler_syntax:revert(C), 
    case T1 of 
        {type, Line, bounded_fun, [T, Gs]} ->
            NewT = replace_ret_type_in_spec_clause(T, Type),
            {type, Line, bounded_fun, [NewT, Gs]};
        {type, Line, 'fun', [FType, _Ret]} ->
            {type, Line, 'fun', [FType, Type]}
    end.


is_type_spec(Form) ->
    case wrangler_syntax:type(Form) of 
        attribute -> 
            AttrName =wrangler_syntax:attribute_name(Form),
            wrangler_syntax:type(AttrName)==atom andalso 
                wrangler_syntax:atom_value(AttrName)==spec;
        _ ->
            false
    end.
              
is_type_spec(Form, {F, A}) ->
    case is_type_spec(Form) of 
        true ->
            [FunSpec, _TypeSpecs] = wrangler_syntax:attribute_arguments(Form),
            case wrangler_syntax:tuple_elements(FunSpec) of 
                [Fun, Arity] ->
                    {F, A}=={wrangler_syntax:concrete(Fun), 
                             wrangler_syntax:concrete(Arity)};
                [_Mod, Fun, Arity] ->
                    {F, A}=={wrangler_syntax:concrete(Fun), 
                             wrangler_syntax:concrete(Arity)};
                _ -> false
            end;
        _ -> false
    end;
is_type_spec(Form, {M, F, A}) ->
    case is_type_spec(Form) of 
        true ->
            [FunSpec, _TypeSpecs] = wrangler_syntax:attribute_arguments(Form),
            case wrangler_syntax:tuple_elements(FunSpec) of 
                [Mod, Fun, Arity] ->
                    {M, F, A}=={wrangler_syntax:concrete(Mod),
                                wrangler_syntax:concrete(Fun), 
                                wrangler_syntax:concrete(Arity)};
                _ -> false
            end;
        _ -> false
    end.


          
