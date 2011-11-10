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
%% The extended refactoring command interface.
%%
%% Author contact: H.Li@kent.ac.uk
%%
%% =====================================================================
%% @doc This module specifies a suite of refactoring command generators.
-module(wrangler_gen).

-export([rename_fun/4, rename_fun/5,
         rename_var/5, rename_var/6,
         rename_mod/3, rename_mod/4,
         swap_args/5,  swap_args/6,
         tuple_args/5, tuple_args/6,
         fold_expr/5,  fold_expr/6,
         gen_fun/5,    gen_fun/6,gen_fun/7,    
         move_fun/4,   move_fun/5,
         unfold_fun_app/4,unfold_fun_app/5]).

-compile(export_all).

-include("../include/wrangler.hrl").

-type (refac_name():: atom()).

-type (pars():: [term()]).

-type (elementary_refac()::{refactoring, refac_name(), pars()}).

-type (file_filter()::{file,fun((File::filename()) -> boolean())}).

-type (module_filter():: {module, fun((Mod::atom()) -> boolean())}).

-type (mod_or_file()::file_filter() | module_filter() | atom() |filename()).

-type (fa()::fun(({FunName::atom(), Arity::integer()})->boolean())
           |{atom(), integer()}).

-type (generator()::{lazy_gen, function()}).

-type (lazy_refac()::{elementary_refac(), generator()}).

-type (search_paths()::[filename()|dir()]).

-define(context, composite_emacs).

%%@hidden
rename_fun(ModOrFile,FA, NewFunName, SearchPaths)->
    rename_fun(ModOrFile,FA, NewFunName, true, SearchPaths).

%% @doc Command generator for renaming function names.
-spec rename_fun(ModOrFile::mod_or_file(),
                Fa:: fa(),
                NewFunName::{generator, fun(({M::atom(),FA::{atom(),integer()}})->atom())}
                          | {user_input, Prompt::fun(({M::atom(),FA::{atom(),integer()}})->
                                                            string())}
                          |atom(),
                 Lazy :: boolean(),
                 SearchPaths::search_paths()) ->
                        [elementary_refac()]|lazy_refac().
rename_fun(ModOrFile,FA, NewFunName, false, SearchPaths)->
    Files= gen_file_names(ModOrFile, SearchPaths),
    CmdLists=[rename_fun_1(File, FA, NewFunName, SearchPaths)
              ||File<-Files],
    lists:append(CmdLists);
rename_fun(ModOrFile,FA, NewFunName, true, SearchPaths)->
    Files=gen_file_names(ModOrFile, true, SearchPaths),
    case Files of
        [] -> [];
        [F] ->get_next_rename_fun_command(
                {F, none}, FA, NewFunName, SearchPaths);
        {F, NextFileGen} ->
            get_next_rename_fun_command(
              {F, NextFileGen}, FA, NewFunName, SearchPaths)
    end.

rename_fun_1(File, FA, NewFunName, SearchPaths) ->
    FAs= get_fun_arity(File, FA),
    [{refactoring,  rename_fun, 
      [File, {F, A}, new_name_gen(File, {F, A}, NewFunName),
       SearchPaths, ?context]}
     ||{F, A}<-FAs].
   
get_next_rename_fun_command({File, NextFileGen}, FA, NewFunName, SearchPaths) ->
    Res=get_fun_arity({File, NextFileGen}, FA, true),
    case Res of
        []->[]; 
        [{F,A}] ->
            [{refactoring, rename_fun, 
              [File, {F, A}, new_name_gen(File, {F, A}, NewFunName), SearchPaths, ?context]}];
        {{File1, F,A},  {File2, NextFileGen2}, NextFAGen}->
            Refac={refactoring, rename_fun, 
                   [File1, {F, A}, new_name_gen(File1, {F, A}, NewFunName),
                    SearchPaths, ?context]},
            {Refac, {lazy_gen, fun()-> get_next_rename_fun_command(
                                         {File2, NextFileGen2}, NextFAGen, 
                                         NewFunName, SearchPaths)
                               end}}
    end.

test_rename_fun(SearchPaths, Lazy) ->
    rename_fun({file, fun(_File)-> true end}, 
               fun({F, _A}) ->
                       remove_underscore(F) /= F  end,
               {generator, fun({_File, {F,_A}}) ->
                                   remove_underscore(F)
                           end},
               Lazy,
               SearchPaths).


test_rename_fun_0(SearchPaths, Lazy) ->
    rename_fun({file, fun(_File)-> false end}, 
               fun({F, _A}) ->
                       remove_underscore(F) /= F  end,
               {generator, fun({_File, {F,_A}}) ->
                                   remove_underscore(F)
                           end},
               Lazy,
               SearchPaths).

test_rename_fun_1(SearchPaths, Lazy) ->
    rename_fun({module, fun(_M)-> true end}, 
               fun({F, _A}) ->
                       remove_underscore(F) /= F  end,
               {generator, fun({_File, {F,_A}}) ->
                                   remove_underscore(F)
                           end},
               Lazy,
               SearchPaths).

test_rename_fun_2(SearchPaths, Lazy) ->
    rename_fun(test, 
               fun({F, _A}) ->
                       remove_underscore(F) /= F  end,
               {generator, fun({_File, {F,_A}}) ->
                                   remove_underscore(F)
                           end},
               Lazy,
               SearchPaths).

test_rename_fun_3(SearchPaths, Lazy) ->
    rename_fun("c:/cygwin/home/hl/git_repos/test/test.erl", 
               fun({F, _A}) ->
                       remove_underscore(F) /= F  end,
               {generator, fun({_File, {F,_A}}) ->
                                   remove_underscore(F)
                           end},
               Lazy,
               SearchPaths).

test_rename_fun_4(SearchPaths, Lazy) ->
    rename_fun("c:/cygwin/home/hl/git_repos/test/test.erl", 
               {test1, 2},
               {generator, fun({_File, {F,_A}}) ->
                                   remove_underscore(F)
                           end},
               Lazy,
               SearchPaths).

remove_underscore(Name) ->
    list_to_atom(remove_underscore(
                   atom_to_list(Name),[])).
remove_underscore([], Acc) ->
    lists:reverse(Acc);
remove_underscore([H|T], Acc) ->
    case H of 
        95 -> remove_underscore(T, Acc);
        _ -> remove_underscore(T, [H|Acc])
    end.
  


% @doc Command generator for renaming module names.
%%@hidden
rename_mod(ModOrFile, NewModName, SearchPaths) ->
    rename_mod(ModOrFile, NewModName, true, SearchPaths).

%%@doc Command generator for renaming module names.
-spec rename_mod(ModOrFile::mod_or_file(),
                 NewModName::{generator, fun((M::atom())->atom())}
                           | {user_input, Prompt::fun((M::atom())->
                                                             string())}
                           |atom(),
                 Lazy :: boolean(),
                 SearchPaths::search_paths()) ->
                        [elementary_refac()]|lazy_refac().
rename_mod(ModOrFile, NewModName, false, SearchPaths) ->
    Files= gen_file_names(ModOrFile, SearchPaths),
    [rename_mod_1(File, NewModName, SearchPaths)
              ||File<-Files];
rename_mod(ModOrFile, NewModName, true, SearchPaths) ->
    case gen_file_names(ModOrFile, true, SearchPaths) of
        [] -> [];
        [F] ->get_next_rename_mod_command(
                 {F, none}, NewModName, SearchPaths);
        {F, NextFileGen} ->
            get_next_rename_mod_command(
              {F, NextFileGen}, NewModName, SearchPaths)
    end.

rename_mod_1(File, NewModName, SearchPaths) ->
    {refactoring, rename_mod_by_name, 
     [File, new_name_gen(File, NewModName), SearchPaths, ?context]}.
 
   
get_next_rename_mod_command({File, NextFileGen}, NewFunName, SearchPaths) ->
    Refac= rename_mod_1(File, NewFunName, SearchPaths), 
    case NextFileGen of 
        {lazy_file_gen, Gen} ->
            case Gen() of 
                [] -> [Refac];
                {File1, Gen1} ->
                    {Refac, {lazy_gen, fun()-> get_next_rename_mod_command(
                                                 {File1, Gen1}, NewFunName, SearchPaths)
                                       end}}
            end;
        _ -> [Refac]
    end.
   
test_rename_mod(SearchPaths, Lazy) ->
    rename_mod({file, fun(_File)-> true end}, 
               {generator, fun(M) -> 
                                   list_to_atom(lists:reverse(atom_to_list(M)))
                           end},
               Lazy,
                SearchPaths).
 

%% @doc Command generator for renaming variable names.
%%@hidden
rename_var(ModOrFile, FA, OldVarName, NewVarName, SearchPaths) ->
    rename_var(ModOrFile, FA, OldVarName, NewVarName, true, SearchPaths).

%% @doc Command generator for renaming variable names.
-spec rename_var(ModOrFile::mod_or_file(), 
                 FA::fa(),
                 OldVarName::fun((VarName::atom())-> boolean())
                           |atom(),
                 NewVarName::{generator, fun(({M::atom(),FA::{atom(),integer()},V::atom()})->atom())}
                           |{user_input, Prompt::fun(({M::atom(), FA::{atom(),integer()}, V::atom()})->
                                                            string())}
                           |atom(),
                 Lazy :: boolean(),
                 SearchPaths::search_paths()) ->
                        [elementary_refac()] | lazy_refac().                         
rename_var(ModOrFile, FA, OldVarName, NewVarName, false, SearchPaths) ->
    Files= gen_file_names(ModOrFile, SearchPaths),
    CmdLists=[rename_var_1(File, FA, OldVarName, NewVarName, SearchPaths)
              ||File<-Files],
    lists:append(CmdLists);
rename_var(ModOrFile, FA, OldVarName, NewVarName, true, SearchPaths) ->
    case gen_file_names(ModOrFile, true, SearchPaths) of
        [] -> [];
        [F] ->get_next_rename_var_command(
                {F, none}, FA, OldVarName, NewVarName, SearchPaths, -1);
        {F, NextFileGen} ->
            get_next_rename_var_command(
              {F, NextFileGen}, FA, OldVarName, NewVarName, SearchPaths, -1)
    end.

get_next_rename_var_command({_File, none}, _FA, 
                            _OldVarName, _NewVarName, _SearchPaths, 0) ->
    [];
get_next_rename_var_command({_File, {lazy_file_gen,NextFileGen}}, FA, 
                            OldVarName, NewVarName, SearchPaths, 0) ->
    case NextFileGen() of
        [] -> [];
        {File1, NextFileGen1} ->
            get_next_rename_var_command({File1, NextFileGen1}, FA, 
                                        OldVarName, NewVarName, SearchPaths, -1)
    end;
get_next_rename_var_command({File, NextFileGen}, FA, 
                            OldVarName, NewVarName, SearchPaths, N) ->
    Cmds=rename_var_1(File, FA, OldVarName, NewVarName, SearchPaths),
    case Cmds of 
        [] ->
            get_next_rename_var_command({File, NextFileGen}, FA, 
                                        OldVarName, NewVarName, SearchPaths, 0);
        [R|Rs] when N==-1 ->
            {R,{lazy_gen, fun()->
                                  get_next_rename_var_command(
                                    {File, NextFileGen}, FA,
                                    OldVarName, NewVarName, SearchPaths, length(Rs))
                          end}};
        _ ->
            Nth = length(Cmds)-N+1,
            {lists:nth(Nth, Cmds), 
             {lazy_gen, fun()->
                                get_next_rename_var_command(
                                  {File, NextFileGen}, FA, 
                                  OldVarName, NewVarName, SearchPaths, N-1)
                        end}}        
    end.

 
rename_var_1(File, FA, VarFilter, NewVarName, SearchPaths) ->
    FAs= get_fun_arity(File, FA),
    [{refactoring, rename_var, 
      [File, {F, A}, V, new_name_gen(File, {F, A}, V, NewVarName), 
       SearchPaths, ?context]}
     ||{F, A}<-FAs, V<-get_vars(File, F, A, VarFilter)].

test_rename_var(SearchPaths, Lazy) ->
    rename_var({file, fun(_File)-> true end}, 
               fun({_F,_A}) -> true end,
               fun(V) ->
                       length(atom_to_list(V))>5 end,
               {generator, fun({_File, {_F,_A}, V}) ->
                                   list_to_atom(lists:sublist(atom_to_list(V), 5))
                           end},
               Lazy,
               SearchPaths).


%%@hidden
move_fun(SrcModOrFile, FA, TgtModOrFile, SearchPaths) ->
    move_fun(SrcModOrFile, FA, TgtModOrFile, true, SearchPaths).

%% @doc Command generator for moving functions from one module to another.
-spec move_fun(SrcModOrFile::mod_or_file(),
               Fa::fa(),
               TagertModOrFile::mod_or_file()|
                                {user_input, Prompt::fun(({M::atom(),FA::{atom(),integer()}})->
                                                                string())},
               Lazy:: boolean(),
               SearchPaths::search_paths()) ->
                      [elementary_refac()]|lazy_refac().

move_fun(SrcModOrFile, FA, TgtModOrFile, false, SearchPaths) ->
    [{refactoring, move_fun_by_name,
      [File, {F, A}, gen_target_file_name(
                       {File, {F, A}}, TgtModOrFile,SearchPaths),
       SearchPaths, ?context]}
     ||File<-gen_file_names(SrcModOrFile, SearchPaths),
       {F,A}<-get_fun_arity(File, FA)];
move_fun(SrcModOrFile, FA, TgtModOrFile, true, SearchPaths) ->
    case gen_file_names(SrcModOrFile, true, SearchPaths) of
        [] -> [];
        [F] ->get_next_move_fun_command(
                {F, none}, FA, TgtModOrFile, SearchPaths);
        {F, NextFileGen} ->
            get_next_move_fun_command(
              {F, NextFileGen}, FA, TgtModOrFile, SearchPaths)
    end.
   
get_next_move_fun_command({File, NextFileGen}, FA, TgtModOrFile, SearchPaths) ->
    case get_fun_arity({File, NextFileGen}, FA, true) of
        []->[]; 
        [{F,A}] ->
            [{refactoring, move_fun_by_name, 
              [File, {F, A}, gen_target_file_name({File, {F,A}}, TgtModOrFile,SearchPaths), 
               SearchPaths, ?context]}];
        {{File1, F,A},  {File2, NextFileGen2}, NextFAGen}->
            Refac={refactoring, move_fun_by_name, 
                   [File1, {F, A}, gen_target_file_name({File, {F, A}}, TgtModOrFile,SearchPaths),
                    SearchPaths, ?context]},
            {Refac, {lazy_gen, fun()-> get_next_move_fun_command(
                                         {File2, NextFileGen2}, NextFAGen, 
                                         TgtModOrFile, SearchPaths)
                               end}}
    end.

gen_target_file_name(PreArgs, TgtModOrFile, SearchPaths) ->
    case TgtModOrFile of
        {user_input, GenPrompt} ->
            {prompt, GenPrompt(PreArgs)};
        _ -> 
            case gen_file_names(TgtModOrFile, SearchPaths) of 
                [File] ->
                    File;
                _ ->
                    throw({error, "Invalid specification of the target file "
                          "for `move function' refactoring."})
            end
    end.

test_move_fun(SearchPaths, Lazy) ->
    move_fun({file, fun(_File)-> true end}, 
               fun({F, _A}) ->
                       remove_underscore(F) /= F  end,
               {user_input, fun({File, {F, A}}) ->
                                    ModName=list_to_atom(filename:basename(File, ".erl")),
                                    lists:flatten(io_lib:format(
                                                    "Target file/module name for function ~p:~p/~p to:",
                                                    [ModName, F, A]))
                            end},
               Lazy,
               SearchPaths).

%% @doc Command generator for function generalisation.
%%@hidden
gen_fun(ModOrFile, FAFilter, ExprStr, NewParName, SearchPaths) ->
    gen_fun(ModOrFile, FAFilter, ExprStr, NewParName, true, SearchPaths, textual).

-spec gen_fun(ModOrFile::mod_or_file(),
              FA::fa(),
              ExprStr::string(),
              NewParName::{user_input, Prompt::fun(({M::atom(), FA::{atom(),integer()}, 
                                                     ExprStr::string()})->
                                                          string())}
                        |atom()
                        |string(),
              Lazy ::boolean(),
              SearchPaths::search_paths()) ->[elementary_refac()]|lazy_refac().
%%@hidden
gen_fun(ModOrFile, FAFilter, ExprStr, NewParName, Lazy, SearchPaths) ->
    gen_fun(ModOrFile, FAFilter, ExprStr, NewParName, Lazy, SearchPaths, textual).

%% @doc Command generator for function generalisation.
-spec gen_fun(ModOrFile::mod_or_file(),
              FA::fa(),
              ExprStr::string(),
              NewParName::{user_input, Prompt::fun(({M::atom(), FA::{atom(),integer()}, 
                                                     ExprStr::string()})->
                                                          string())}
                        |atom()
                        |string(),
              Lazy ::boolean(),
              SearchPaths::search_paths(), 
              GenOrder:: textual|bu|td) ->[elementary_refac()]|lazy_refac().
gen_fun(ModOrFile, FAFilter, ExprStr, NewParName, false, SearchPaths, GenOrder) ->
    Files= gen_file_names(ModOrFile, SearchPaths),
    FFAs = [{File,FA}||File<-Files, FA<-get_fun_arity(File, FAFilter, false, GenOrder)],
    Cmds=[gen_fun_1(FFA, ExprStr, NewParName, SearchPaths)||FFA<-FFAs],
    lists:append(Cmds);
gen_fun(ModOrFile, FA, ExprStr, NewParName, true, SearchPaths, GenOrder) ->
    case gen_file_names(ModOrFile, true, SearchPaths) of
        [] -> [];
        [F] ->get_next_gen_fun_command(
                {F, none}, FA, ExprStr, NewParName, SearchPaths, GenOrder);
        {F, NextFileGen} ->
            get_next_gen_fun_command(
              {F, NextFileGen}, FA, ExprStr, NewParName, SearchPaths, GenOrder)
    end.
gen_fun_1({File, FA}, ExprStr, NewParName, SearchPaths) ->
    case get_exprs(File, FA, ExprStr) of 
        [] ->[];
        Ranges ->
            Exprs={range, {File, Ranges}},
            [{refactoring, generalise_composite,
              [File, FA, Exprs, hd(Ranges), 
               new_name_gen(File, FA, Exprs, NewParName),
               SearchPaths, ?context]}]
    end.

get_next_gen_fun_command({File, NextFileGen}, FA, ExprStr, NewParName, SearchPaths, GenOrder) ->
    case get_fun_arity({File, NextFileGen}, FA, true, GenOrder) of
        []->[]; 
        [{F,A}] ->
            case get_exprs(File, {F,A}, ExprStr) of 
                [] ->[];
                Ranges ->
                    Exprs={range, {File, Ranges}},
                    [{refactoring, generalise_composite,
                      [File, FA, Exprs, hd(Ranges), 
                       new_name_gen(File, FA, Exprs, NewParName),
                       SearchPaths, ?context]}]
            end;
        {{File1, F,A},  {File2, NextFileGen2}, NextFAGen}->
            case get_exprs(File1, {F,A}, ExprStr) of 
                [] ->
                    get_next_gen_fun_command({File2, NextFileGen2}, NextFAGen, 
                                             ExprStr, NewParName, SearchPaths, GenOrder);
                Ranges ->
                    Exprs={range, {File, Ranges}},
                    Refac ={refactoring, generalise_composite,
                            [File, {F, A}, Exprs, hd(Ranges), 
                             new_name_gen(File, {F, A}, Exprs, NewParName),
                             SearchPaths, ?context]},
                    {Refac, {lazy_gen, fun()-> get_next_gen_fun_command(
                                                 {File2, NextFileGen2}, NextFAGen, 
                                                 ExprStr, NewParName, SearchPaths, GenOrder)
                                       end}}
            end                                       
    end.
              
get_exprs(File, {FunName, Arity}, ExprStr) ->
    ModName=list_to_atom(filename:basename(File, ".erl")),
    FunDef=api_refac:mfa_to_fun_def({ModName, FunName, Arity}, File),
    case FunDef of
        none -> none;
        _ ->
            ?FULL_TD_TU(
               [?COLLECT(?T("E@"),  
                         api_refac:start_end_loc(E@),
                         api_refac:is_expr(E@) andalso
                         ?PP(E@)==ExprStr)], FunDef)
    end.

test_gen_fun(SearchPaths, Lazy) ->
    gen_fun({file, fun(_File)-> true end}, 
            fun({_F,_A}) ->
                    true
            end,
            "1",
            {user_input, fun(_) ->"New parameter name: " end},
            Lazy,
            SearchPaths, bu).


%%@hidden
fold_expr(CurModOrFile, TgtModOrFile, FA, ClauseIndex, SearchPaths) ->
    fold_expr(CurModOrFile, TgtModOrFile, FA, ClauseIndex, true, SearchPaths).

%% @doc Command generator for folding expressions against a function definition. 
-spec fold_expr(CurModOrFile::mod_or_file(), 
                ModOrFile::mod_or_file(), 
                FA::fa(), 
                ClauseIndex::integer(), 
                Lazy::boolean(),
                SearhPaths::search_paths()) ->
                       [elementary_refac()] | lazy_refac().

fold_expr(CurModOrFile, TgtModOrFile, FA, ClauseIndex, false, SearchPaths) ->
    Files1= gen_file_names(CurModOrFile, SearchPaths),
    Files2= gen_file_names(TgtModOrFile, SearchPaths),
    [fold_expr_1(File1,  File2, F, A, ClauseIndex, SearchPaths)
         ||File1<-Files1, File2<-Files2, {F, A}<-get_fun_arity(File2, FA)]; 
fold_expr(CurModOrFile, TgtModOrFile, FA, ClauseIndex, true, SearchPaths) ->
    case gen_file_names(CurModOrFile, true, SearchPaths) of
        [] -> [];
        [F] ->get_next_fold_expr_command(
                {F, none}, TgtModOrFile, FA, ClauseIndex, SearchPaths);
        {F, NextFileGen} ->
            get_next_fold_expr_command(
              {F, NextFileGen}, TgtModOrFile, FA, ClauseIndex, SearchPaths)
    end.
get_next_fold_expr_command({File, NextFileGen}, TgtModOrFileFilter, FA, ClauseIndex, SearchPaths) ->
    case gen_file_names(TgtModOrFileFilter, true, SearchPaths) of 
        [] -> [];
        [TgtFile] ->
            get_next_fold_expr_command_1(
              {File, NextFileGen}, {TgtFile,none}, FA, ClauseIndex, SearchPaths,
              {TgtModOrFileFilter, FA});
        {TgtFile, NextTgtFileGen} ->
            get_next_fold_expr_command_1(
              {File, NextFileGen}, {TgtFile, NextTgtFileGen}, FA,  ClauseIndex, SearchPaths,
              {TgtModOrFileFilter, FA})
    end.

get_next_fold_expr_command_1({File, NextFileGen}, {TgtFile, NextTgtFileGen}, 
                             FA, ClauseIndex, SearchPaths, {OrigTgtModOrFile, OrigFA}) ->
    case get_fun_arity({TgtFile, NextTgtFileGen}, FA, true) of
        [] ->
            case NextFileGen of
                {lazy_file_gen, Gen} ->
                    case Gen() of 
                        [] ->
                           [];
                        {File1, NextFileGen1} ->
                            get_next_fold_expr_command({File1, NextFileGen1},OrigTgtModOrFile, 
                                                      OrigFA, ClauseIndex, SearchPaths)
                    end;
                none -> []
            end;
        [{F,A}] ->
            Refac= fold_expr_1(File, TgtFile, F, A, ClauseIndex, SearchPaths),
            case NextFileGen of 
                {lazy_file_gen, Gen} ->
                    case Gen() of 
                        [] ->
                            [Refac];
                        {File1, NextFileGen1} ->
                            {Refac, {lazy_gen, fun() ->
                                                       get_next_fold_expr_command(
                                                         {File1, NextFileGen1},OrigTgtModOrFile, 
                                                         OrigFA, ClauseIndex, SearchPaths)
                                               end}}
                    end;
                none -> [Refac]
            end;
        {{TgtFile1, F,A},  {TgtFile2, NextTgtFileGen2}, NextFAGen}->
            Refac= fold_expr_1(File, TgtFile1, F, A, ClauseIndex, SearchPaths),
            {Refac, {lazy_gen, fun()->                                   
                                       get_next_fold_expr_command_1(
                                         {File, NextFileGen}, {TgtFile2, NextTgtFileGen2},
                                         NextFAGen, ClauseIndex, SearchPaths, 
                                         {OrigTgtModOrFile, OrigFA})
                               end}}
    end.
           
fold_expr_1(CurFile, TgtFile, FunName, Arity, ClauseIndex, SearchPaths) ->
    ModName=list_to_atom(filename:basename(TgtFile, ".erl")),
    {refactoring, fold_expr_by_name, 
     [CurFile, atom_to_list(ModName), atom_to_list(FunName), 
      integer_to_list(Arity), integer_to_list(ClauseIndex), 
      SearchPaths, ?context]}.
        
test_fold_expr(SearchPaths, Lazy) ->
    fold_expr({file, fun(_File)-> true end},
              {file, fun(_File)-> true end},
              fun({_F, A}) ->
                      A==0
              end, 
              1, Lazy, SearchPaths).
                      
%%@hidden
unfold_fun_app(ModOrFile, FA, MFAorPos, SearchPaths) ->
    unfold_fun_app(ModOrFile, FA, MFAorPos, true, SearchPaths).

%% @doc Command generator for unfolding a function application.
-spec unfold_fun_app(ModOrFile::mod_or_file(), 
                     FA:: fa(),
                     MFAorPos:: fun((mfa())->boolean()) |mfa()|pos(),
                     Lazy :: boolean(),
                     SearchPaths::search_paths())->
                            [elementary_refac()] | lazy_refac().
unfold_fun_app(ModOrFile, FA, MFAorPos, false, SearchPaths) ->
    Files= gen_file_names(ModOrFile, SearchPaths),
    Refacs=[unfold_fun_app_1(File, {F, A}, MFAorPos, SearchPaths)
            ||File<-Files, {F, A}<-get_fun_arity(File, FA)],
    lists:append(Refacs);
unfold_fun_app(ModOrFile, FA, MFAorPos, true, SearchPaths) ->
    Res=gen_file_names(ModOrFile, true, SearchPaths),
    case Res of
        [] -> [];
        [F] ->get_next_unfold_app_command(
                {F, none}, FA, MFAorPos, SearchPaths, -1);
        {F, NextFileGen} ->
            get_next_unfold_app_command(
              {F, NextFileGen}, FA, MFAorPos, SearchPaths, -1)
    end.
get_next_unfold_app_command({File, NextFileGen}, FA, 
                            MFAorPos, SearchPaths, N) ->
    FAs= get_fun_arity({File, NextFileGen}, FA, true),
    case FAs of
        []->[]; 
        [{F, A}] ->  
            Cmds=unfold_fun_app_1(File, {F, A}, MFAorPos, SearchPaths),
            case Cmds of 
                [] -> [];
                [R|Rs] when N==-1 ->
                    {R,{lazy_gen, fun()->
                                          get_next_unfold_app_command(
                                            {File, none}, FA, MFAorPos, SearchPaths, length(Rs))
                                  end}};
                _ ->
                    Nth = length(Cmds)-N+1,
                    case N == 1 of 
                        true ->
                            [lists:nth(Nth, Cmds)];
                        _ ->
                            {lists:nth(Nth, Cmds), 
                             {lazy_gen, fun()->
                                                get_next_unfold_app_command(
                                                  {File, none}, FA, MFAorPos, SearchPaths, N-1)
                                        end}}        
                    end
            end;
        {{File1, F,A},  {File2, NextFileGen2}, FAGen}->
            Cmds=unfold_fun_app_1(File1, {F, A}, MFAorPos, SearchPaths),
            case Cmds of 
                [] ->
                    get_next_unfold_app_command({File2, NextFileGen2}, FAGen, MFAorPos, SearchPaths, -1);
                [R] when N==-1 ->
                    {R,{lazy_gen, fun()->
                                          get_next_unfold_app_command(
                                            {File2, NextFileGen2}, FAGen, MFAorPos, SearchPaths, -1)
                                   end}};
                [R|Rs] when N==-1 ->
                    {R,{lazy_gen, fun()->
                                          get_next_unfold_app_command(
                                            {File1, NextFileGen}, FA, MFAorPos, SearchPaths, length(Rs))
                                  end}};
                _ ->Nth = length(Cmds)-N+1,
                    case N==1 of 
                        true ->
                            {lists:nth(Nth, Cmds), 
                             {lazy_gen, fun()->
                                                get_next_unfold_app_command(
                                                   {File2, NextFileGen2}, FAGen, MFAorPos, SearchPaths, -1)
                                        end}};     
                        false ->
                            {lists:nth(Nth, Cmds), 
                             {lazy_gen, fun()->
                                                get_next_unfold_app_command(
                                                  {File1, NextFileGen}, FA, MFAorPos, SearchPaths, N-1)
                                        end}}        
                    end
            end
    end.
         
        
        
unfold_fun_app_1(File, {F, A}, MFAorPos, SearchPaths) ->
    case MFAorPos of 
        {Line, Col} when is_integer(Line) andalso is_integer(Col) ->
            {refactoring, unfold_fun_app,
             [File, {Line, Col}, SearchPaths, ?context]};
        MFA ->
            Ps = get_app_locs(File, {F, A}, MFA),
            [{refactoring, unfold_fun_app,
              [File, Pos, SearchPaths, ?context]} || Pos<-Ps]
    end.
        
test_unfold_fun(SearchPaths, Lazy) ->
    unfold_fun_app({file, fun(_File)-> true end},
                   fun({_F, _A}) ->
                           true
                   end,
                   {test, t, 0}, Lazy, SearchPaths).

%%@hidden
swap_args(ModOrFile, FA, Index1,Index2, SearchPaths)->
    swap_args(ModOrFile, FA, Index1,Index2, true, SearchPaths).

%% @doc Command generator for for swapping function arguments.
-spec swap_args(ModOrFile::mod_or_file(),
                 FA::fa(),
                 Index1:: integer()| 
                          {user_input, Prompt::fun(({M::atom(), FA::{atom(),integer()}})->
                                                          string())},
                 Index2:: integer()|
                          {user_input,Prompt::fun(({M::atom(), FA::{atom(),integer()}})->
                                                         string())},
                 Lazy::boolean(),
                 SearchPaths::search_paths())->
                        [elementary_refac()]|lazy_refac().
swap_args(ModOrFile, FA, Index1,Index2, false, SearchPaths)->
    Files= gen_file_names(ModOrFile, SearchPaths),
    [swap_args_1(File, {F, A}, Index1, Index2, SearchPaths)
     ||File<-Files, {F, A}<-get_fun_arity(File, FA)];

swap_args(ModOrFile, FA, Index1, Index2, true, SearchPaths) ->
    case gen_file_names(ModOrFile, true, SearchPaths) of
        [] -> [];
        [F] ->get_next_swap_args_command(
                {F, none}, FA, Index1, Index2, SearchPaths);
        {F, NextFileGen} ->
            get_next_swap_args_command(
              {F, NextFileGen}, FA, Index1, Index2, SearchPaths)
    end.

get_next_swap_args_command({File, NextFileGen}, FA, Index1, Index2, SearchPaths) ->
    case get_fun_arity({File, NextFileGen}, FA, true) of
        []->[]; 
        [{F,A}] ->
            [swap_args_1(File, {F, A}, Index1, Index2, SearchPaths)]; 
        {{File1, F,A},  {File2, NextFileGen2}, NextFAGen}->
            Refac=swap_args_1(File1, {F, A}, Index1, Index2, SearchPaths),
            {Refac, {lazy_gen, fun()-> get_next_swap_args_command(
                                         {File2, NextFileGen2}, NextFAGen, 
                                         Index1, Index2, SearchPaths)
                               end}}
    end.
 
swap_args_1(File, {F, A}, Index1, Index2, SearchPaths) ->
    {refactoring, swap_args, 
     [File, {F, A},  index_gen(Index1, {File, {F, A}}), 
      index_gen(Index2, {File, {F, A}}), SearchPaths, ?context]}.
    
test_swap_args(SearchPaths, Lazy) ->
    swap_args({file, fun(_File)-> true end}, 
               fun({_F, A}) ->
                       A>= 3 
               end,
               2, 3, Lazy, SearchPaths).
    
%%@hidden
tuple_args(ModOrFile, FA, Index1,Index2, SearchPaths)->
    tuple_args(ModOrFile, FA, Index1,Index2, true, SearchPaths).

%% @doc Command generator for tupling function arguments.
-spec tuple_args(ModOrFile::mod_or_file(),
                 FA::fa(),
                 Index1:: integer()| 
                          {user_input, Prompt::fun(({M::atom(), FA::{atom(),integer()}})->
                                                          string())},
                 Index2:: integer()|
                          {user_input,Prompt::fun(({M::atom(), FA::{atom(),integer()}})->
                                                         string())},
                 Lazy::boolean(),
                 SearchPaths::search_paths())->
                        [elementary_refac()]|lazy_refac().
tuple_args(ModOrFile, FA, Index1,Index2, false, SearchPaths)->
    Files= gen_file_names(ModOrFile, SearchPaths),
    [tuple_args_1(File, {F, A}, Index1, Index2, SearchPaths)
     ||File<-Files, {F, A}<-get_fun_arity(File, FA)];
tuple_args(ModOrFile, FA, Index1, Index2, true, SearchPaths) ->
    case gen_file_names(ModOrFile, true, SearchPaths) of
        [] -> [];
        [F] ->get_next_tuple_args_command(
                {F, none}, FA, Index1, Index2, SearchPaths);
        {F, NextFileGen} ->
            get_next_tuple_args_command(
              {F, NextFileGen}, FA, Index1, Index2, SearchPaths)
    end.

get_next_tuple_args_command({File, NextFileGen}, FA, Index1, Index2, SearchPaths) ->
    case get_fun_arity({File, NextFileGen}, FA, true) of
        []->[]; 
        [{F,A}] ->
            [tuple_args_1(File, {F, A}, Index1, Index2, SearchPaths)]; 
        {{File1, F,A},  {File2, NextFileGen2}, NextFAGen}->
            Refac=tuple_args_1(File1, {F, A}, Index1, Index2, SearchPaths),
            {Refac, {lazy_gen, fun()-> get_next_tuple_args_command(
                                         {File2, NextFileGen2}, NextFAGen, 
                                         Index1, Index2, SearchPaths)
                               end}}
    end.
 
tuple_args_1(File, {F, A}, Index1, Index2, SearchPaths) ->
    {refactoring, tuple_args, 
     [File, {F, A},  index_gen(Index1, {File, {F, A}}), 
      index_gen(Index2, {File, {F, A}}), SearchPaths, ?context]}.
    
test_tuple_args(SearchPaths, Lazy) ->
    tuple_args({file, fun(_File)-> true end}, 
               fun({_F, A}) ->
                       A>= 3 
               end,
               2, 3, Lazy, SearchPaths).
               
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                              %%
%%                Utility functions.                            %%
%%                                                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gen_file_names(ModOrFile, SearchPaths) ->
    gen_file_names(ModOrFile, false, SearchPaths).
gen_file_names(ModOrFile, Lazy, SearchPaths) ->
    Files = wrangler_misc:expand_files(SearchPaths, ".erl"),    
    case ModOrFile of 
        {file, FileFilter} ->
            case Lazy of 
                true ->
                    get_next_file(Files, {file,FileFilter});
                false ->[F||F<-Files, FileFilter(F)]
            end;
        {module, ModFilter} ->
            case Lazy of 
                true ->
                    get_next_file(Files, {module, ModFilter});
                false ->
                    [F||F<-Files, ModFilter(filename:basename(F, ".erl"))]
            end;
        _ when is_atom(ModOrFile) ->
            Fs=[F||F<-Files, filename:basename(F,".erl")==atom_to_list(ModOrFile)],
            case Fs of 
                [] -> error("None of the files specified in the "
                            "searchpaths matches the module name given.");
                [F|_] -> [F]
            end;
        _ when is_list(ModOrFile)->
            case filelib:is_regular(ModOrFile) of 
                true ->
                    [ModOrFile];
                false ->
                    throw({error, lists:flatten(
                                    io_lib:format(
                                      "Invalid argument:~p.", [ModOrFile]))})
            end;
        _ -> 
            throw({error, lists:flatten(io_lib:format(
                                          "Invalid argument:~p.", [ModOrFile]))})
    end.

get_next_file([], _FileOrModFilter) -> [];
get_next_file([F|Fs], Filter) ->
    case Filter of 
        {module, Fun} -> 
            M = filename:basename(F, ".erl"),
            case Fun(M) of 
                true ->
                    {F, {lazy_file_gen, fun()-> get_next_file(Fs, Filter) end}};
                false ->
                    get_next_file(Fs, Filter)
            end;
        {file, Fun} ->
            case Fun(F) of 
                true ->
                    {F, {lazy_file_gen, fun()-> get_next_file(Fs, Filter) end}};
                false ->
                    get_next_file(Fs, Filter)
            end
    end.
            
                        

locate_one_file(ModOrFile, SearchPaths) ->
    Res= gen_file_names(ModOrFile, SearchPaths),
    case Res of 
        [] ->
            throw({error, lists:flatten(
                            io_lib:format("File/Module does not exist in the "
                                          "searchpaths specified: ~p.\n", 
                                          [ModOrFile]))});
        [File] ->
            File;
        [File|_] ->
            throw({error,lists:flatten(
                           io_lib:format("File/module specified is not unique "
                                         "in the searchpaths specified.:~p\n", 
                                         [File]))})
    end.


get_funs(File, Order) ->
    case Order == td orelse  Order == bu of
        true ->
            SortedFuns=wrangler_callgraph_server:get_sorted_funs(File),
            {MFAs, _} = lists:unzip(SortedFuns),
            case Order of
                td ->
                    lists:reverse([{F,A}||{_M,F,A} <- MFAs]);
                bu ->
                    [{F,A}||{_M,F,A} <- MFAs]
            end;
        false ->
            {ok, ModuleInfo} = api_refac:get_module_info(File),
            case lists:keyfind(functions, 1, ModuleInfo) of
                {functions, Fs} ->
                    Fs;
                false ->
                    []
            end
    end.

 
get_fun_arity(File, FA) ->
    get_fun_arity(File, FA, false).
get_fun_arity(File, FA, Lazy) ->
    get_fun_arity(File, FA, Lazy, default).

get_fun_arity(File, FA, false, Order) ->
    Funs = get_funs(File, Order),
    if is_function(FA) ->
            [{F,A}||{F,A}<-Funs, FA({F,A})];
       is_tuple(FA) ->
            normalise_fa(FA);
       true ->
            throw({error, "Invalid function-arity."})
    end;

get_fun_arity({File, NextFileGen}, FA, true, Order) ->
    FAs = get_fun_arity(File, FA, true, Order),
    case FAs of
        [] ->
            case NextFileGen of 
                {lazy_file_gen, FileGen} ->
                    case FileGen() of
                        [] -> [];
                        {File1, NextFileGen1}->
                            case File1 of 
                                [] -> [];
                                _ ->
                                    case FA of 
                                        {lazy_fa_gen, OriginFA, _} ->
                                            get_fun_arity({File1, NextFileGen1},OriginFA, true, Order);
                                        _ ->
                                            get_fun_arity({File1, NextFileGen1},FA, true, Order)
                                    end
                            end
                    end;
                none -> []
            end;
        [{F, A}] ->
            case NextFileGen of 
                {lazy_file_gen, FileGen} ->
                    case FileGen() of
                        [] ->  [];
                        {File1, NextFileGen1} ->
                            case File1 of 
                                [] -> [{F,A}];
                                _ ->
                                    {{File, F, A}, {File1, NextFileGen1}, FA}
                            end
                    end;
                none -> [{F,A}]
            end;
        {{F,A}, {lazy_fa_gen, OriginFA, NextFAGen}} ->
            {{File, F,A}, {File, NextFileGen}, {lazy_fa_gen, OriginFA, NextFAGen}}
    end;
get_fun_arity(_File, {lazy_fa_gen, _OriginFA, Gen}, true, _Order) ->
      Gen();
get_fun_arity(File, FA, true, Order) ->
    Funs = get_funs(File, Order),
    if is_function(FA) ->
            get_next_fun_arity(Funs, FA);
       is_tuple(FA) ->
            normalise_fa(FA);
       true ->
            throw({error, "Invalid function-arity."})
    end.

normalise_fa(F,A) -> normalise_fa({F,A}).

normalise_fa({F, A}) ->
    F1 = case is_list(F) of
           true -> list_to_atom(F);
           _ -> F
         end,
    A1 = case is_list(A) of
           true -> list_to_integer(A);
           _ -> A
         end,
    [{F1, A1}].

get_next_fun_arity([], _FA) -> [];
get_next_fun_arity([{F,A}|Fs], FA)->
    case FA({F,A}) of
        true ->
            {{F,A}, {lazy_fa_gen, FA, fun() -> get_next_fun_arity(Fs, FA) end}};
        false ->
            get_next_fun_arity(Fs, FA)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%                         
  
get_vars(File, FunName, Arity, VarFilter) ->
    ModName=list_to_atom(filename:basename(File, ".erl")),
    FunDef=api_refac:mfa_to_fun_def({ModName, FunName, Arity}, File),
    case FunDef of 
        none -> 
            [];
        _ ->   
            ?FULL_TD_TU(
               [?COLLECT(?T("V@"), 
                         {range, {File, [api_refac:start_end_loc(V@)]},list_to_atom(?PP(V@))},
                         api_refac:type(V@)==variable andalso
                         api_refac:bound_vars(V@)/=[] andalso 
                         case VarFilter of 
                             atom ->
                                 atom_to_list(VarFilter)==?PP(V@);
                             _ when is_function(VarFilter) ->
                                 VarFilter(list_to_atom(?PP(V@)));
                             _ -> false
                         end)], FunDef)
    end.
 
get_app_locs(File, {FunName, Arity}, AppFilter) ->
    ModName=list_to_atom(filename:basename(File, ".erl")),
    FunDef=api_refac:mfa_to_fun_def({ModName, FunName, Arity}, File),
    case FunDef of 
        none -> 
            [];
        _ ->   
            ?FULL_TD_TU(
               [?COLLECT(?T("F@(Args@@)"), 
                         element(1, api_refac:start_end_loc(_This@)),
                         api_refac:type(_This@)==application  andalso
                         case AppFilter of 
                              {M, F, A} -> {M, F, A}==api_refac:fun_define_info(F@);
                              _ -> AppFilter(api_refac:fun_define_info(F@))
                          end
                        )], FunDef)
    end.
%%@hidden
gen_question(rename_fun,[File,{F,A}|_]) ->
    M=list_to_atom(filename:basename(File, ".erl")),
    lists:flatten(io_lib:format("Do you want to rename function ~p:~p/~p?", [M,F,A]));
gen_question(rename_mod,[File|_]) ->
    M=list_to_atom(filename:basename(File, ".erl")),
    lists:flatten(io_lib:format("Do you want to rename module '~p'?", [M]));
gen_question(rename_var,[File,{F,A},{range, {_File, _Loc}, V}|_]) ->
    M=list_to_atom(filename:basename(File, ".erl")),
    lists:flatten(io_lib:format("Do you want to rename variable ~s in function ~p:~p/~p?", [V,M,F,A]));
gen_question(swap_args, [File,{F,A}|_]) ->
    M=list_to_atom(filename:basename(File, ".erl")),
    lists:flatten(io_lib:format("Do you want to swap the parameters of function ~p:~p/~p?", 
                                [M,F,A]));
gen_question(gen_fun, [File, {F, A}|_]) ->
    M=list_to_atom(filename:basename(File, ".erl")),
    lists:flatten(io_lib:format("Do you want to generalise function ~p:~p/~p over the expression(s) highlighted?",
                                [M,F,A]));
gen_question(move_fun_by_name, [SrcFile, {F, A}, TgtFile|_]) ->
    M=list_to_atom(filename:basename(SrcFile, ".erl")),
    lists:flatten(io_lib:format("Do you want to move function ~p:~p/~p to file ~p?",
                                [M,F,A, TgtFile]));
gen_question(unfold_fun_app, [File, Loc|_]) ->
    M=list_to_atom(filename:basename(File, ".erl")),
    lists:flatten(io_lib:format("Do you want to unfold the function application at "
                                "location ~p in module ~p?",
                                [Loc, M]));
gen_question(fold_expr_by_name, [File, M, {F, A}|_]) ->
    CurMod=list_to_atom(filename:basename(File, ".erl")),
    lists:flatten(io_lib:format("Do you want to fold expressions in module ~p,against function ~p:~p/~p?",
                                [CurMod, M, F, A]));
gen_question(tuple_args, [File, {F,A}, Index1, Index2|_]) ->
    M=list_to_atom(filename:basename(File, ".erl")),
    lists:flatten(io_lib:format("Do you want to turn the ~p arguments, starting from index ~p, "
                                "of function ~p:~p/~p into a tuple?",
                                [Index2-Index1+1, Index1, M, F, A])).


index_gen(Index, PreArgs) ->
    case Index of
        {generator, GenFun} ->
            GenFun(PreArgs);
        {user_input, GenPrompt} ->
            {prompt, GenPrompt(PreArgs)};
        _ when is_integer(Index) ->
            Index;
        _ ->
            throw({error, "Invalid Index."})
    end.


pos_gen(PosGen) ->
    case PosGen of 
        {Line, Col} when is_integer(Line) andalso
                         is_integer(Col)->
            {Line, Col};
        _ when is_function(PosGen)->
            PosGen()
    end.

new_name_gen(File, NewName) ->
    ModName=list_to_atom(filename:basename(File, ".erl")),
    case NewName of
        {generator, GenFun} ->
            GenFun(ModName);
        {user_input, GenPrompt} ->
            {prompt, GenPrompt(ModName)};
        _ when is_atom(NewName) ->
            NewName;
        _ ->
            throw({error, "Invalid new funname."})
    end.
        
new_name_gen(File, FA, NewName) ->
    ModName=list_to_atom(filename:basename(File, ".erl")),
    case NewName of
        {generator, GenFun} ->
            GenFun({ModName, FA});
        {user_input, GenPrompt} ->
            {prompt, GenPrompt({ModName, FA})};
        _ when is_atom(NewName) ->
            NewName;
        _ ->
            throw({error, "Invalid new funname."})
    end.
        
new_name_gen(File, FA, {range, {_File, _Loc}, V}, NewName) ->
    ModName=list_to_atom(filename:basename(File, ".erl")),
    case NewName of
        {generator, GenFun} ->
            GenFun({File, FA, V});
        {user_input, GenPrompt} ->
            {prompt, GenPrompt({ModName, FA,V})};
        _ when is_atom(NewName) ->
            NewName;
        _ ->
            throw({error, "Invalid new variable name."})
    end;
new_name_gen(File, FA, OldName, NewName) ->
    ModName=list_to_atom(filename:basename(File, ".erl")),
    case NewName of
        {generator, GenFun} ->
            GenFun({ModName, FA,  OldName});
        {user_input, GenPrompt} ->
            {prompt, GenPrompt({ModName, FA, OldName})};
        _ when is_atom(NewName) ->
            NewName;
        _ ->
            throw({error, "Invalid new variable name."})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                     %%
%%                       Testing                                       %% 
%%                                                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test_refac_1(TestFun) ->
    apply(?MODULE, TestFun, [["c:/cygwin/home/hl/git_repos/test"], false]).

test_refac_2(TestFun) ->
    case apply(?MODULE, TestFun, [["c:/cygwin/home/hl/git_repos/test"], true]) of
        [] -> [];
        [Refac] ->
            wrangler_io:format("Refac:\n~p\n", [Refac]);
        {Refac, {lazy_gen, Gen}} ->
            wrangler_io:format("Refac0:\n~p\n\n", [Refac]),
            test_refac_3({lazy_gen, Gen})
    end.
          
test_refac_3({lazy_gen, Gen}) ->  
    case Gen() of 
        {Refac, Gen1} ->
            wrangler_io:format("Refac1:\n~p\n", [Refac]),
            test_refac_3(Gen1);
        [Refac] ->
            wrangler_io:format("Refac2:\n~p\n", [Refac]);
        [] -> []
    end.
