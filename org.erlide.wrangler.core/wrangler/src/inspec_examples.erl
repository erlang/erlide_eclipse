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
%%
%%@hidden
%%@private
%%@author  Huiqing Li <H.Li@kent.ac.uk>
%%
%%
%%@doc 
%% Some example code inspection functions using code templates.
%%
%% This module demonstrates how to use code templates and the predefined 
%% macros, ?COLLECT and ?COLLECT_LOC, to collect information about specific 
%% code fragments of interest. 
%% To invoke a user-defined code inspection function from the `Inspector' menu, select 
%% `Apply Adhoc Code Inspector' first, then Wrangler will prompt you to input the 
%% the name of the module in which the code inspection function is defined, and 
%% the code inspection function name; if the code inspection function requires 
%% user-inputs, Wrangler will also prompt you to input the parameter values. After 
%% that, the code inspection function will be run by Wrangler, and the results are 
%% shown in the *erl-output* buffer.
%%
%% In order for a user-defined code inspection function to be invoked by the 
%% `Apply Adhoc Code Inspector' command, a number of coding rules should be 
%% followed by the code inspection function: 
%%
%% -- The code inspection function should have an arity of 1; 
%%
%% -- The code inspection function should have two function clauses: one 
%% function clause takes atom `input_pars' as the parameter, and the other 
%% takes record `args' as the parameter. If the code inspection function 
%% does not require any user-inputs before hand, 
%% the function clause with `input_pars' as the parameter should return an empty list,
%% otherwise it should return the list of prompt strings to be used when 
%% the Wrangler asks the user for input, and there should be one prompt string
%% for each user-input. The `args' record taken as parameter by the other 
%% function clause contains information that is passed to the code inspection
%% function. To be consistent, the definition of the `args' record is the same 
%% as the one used by the `gen_refac' behaviour, although record fields `cursor_pos'
%% ,`highlight_range' and 'focus_sel' are generally not use by code inspection 
%% functions. Here is the definition of record `args':
%%
%%  ```-record(args,{current_file_name :: filename(),         %% the file name of the current Erlang buffer.
%%                   cursor_pos        :: pos(),              %% the current cursor position.
%%                   highlight_range   :: {pos(), pos()},     %% the start and end location of the highlighted code if there is any.
%%                   user_inputs       :: [string()],         %% the data inputted by the user.
%%                   focus_sel         :: any(),              %% the focus of interest selected by the user.
%%                   search_paths      ::[dir()|filename()],  %% the list of directories or files which specify the scope of the project.
%%                   tabwidth =8        ::integer()           %% the number of white spaces denoted by a tab key.
%%                  }).'''
%%
%% -- In the case that the code inspection function returns location range information about 
%%    code fragments of interest, in order for the locations to be mouse-clickable, the 
%%   location range should be of the format: `{filename(), {pos(),pos()}}'. 
%%    
%% Source code for this module:
%%<ul>
%%<li>
%%<a href="file:code_inspector_examples.erl" > code_inspector_examples.erl.</a>.
%%</li>
%%</ul>

-module(inspec_examples).

-include("../include/wrangler.hrl").

-export([top_level_if/1, 
         append_two_lists/1,
         unnecessary_match/1,
         non_tail_recursive_function/1,
         is_non_tail_recursive/1,
         calls_to_specific_function/1]).
         
-export([test/1,test1/1,test2/1,test3/1,test4/1, test5/1]).

-import(api_refac, [fun_define_info/1]).

%%===================================================================
%% In the current Erlang file, collects those function definitions 
%% consisting of a single function clause that is an `if' expression,
%% and returns the MFA of each function definition detected.
%% Note: by including `Guard@@' in the template, this function will 
%% collect functions both with guard expressions and without guard 
%% expression (i.e. `Guard@@' is `[]').

-spec(top_level_if/1::(#args{}) ->[{modulename(), functionname(), arity()}]).
%% No user inputs needed.   
top_level_if(input_par_prompts) ->
    [];
top_level_if(_Args=#args{search_paths=SearchPaths})->
    ?FULL_TD_TU([?COLLECT(?T("f@(Args@@) when Guard@@ ->Body@."), 
                          api_refac:fun_define_info(f@),
                          api_refac:type(Body@) == if_expr)],
                [SearchPaths]).

%%=====================================================================
%% Collects the uses of `lists:append/2' in the project, and returns 
%% the location information of each application instance found.

-spec(append_two_lists/1::(#args{}) ->[{filename(), {pos(),pos()}}]).
%% No user inputs needed.
append_two_lists(input_par_prompts) -> [];
append_two_lists(_Args=#args{search_paths=SearchPaths}) ->
    ?STOP_TD_TU([?COLLECT(?T("F@(L1@, L2@)"), 
                          {_File@, wrangler_misc:start_end_loc(_This@)},
                          {lists, append, 2} == api_refac:fun_define_info(F@))],
                SearchPaths). 

%%=====================================================================
%% Collects the uses of a specific function, and returns the location
%% information of each application instance found, and returns the 
%% location information of each application instance found.

%% Ask the user to input the MFA information of the function to check.
-spec(calls_to_specific_function/1::(#args{}) ->[{filename(), {pos(),pos()}}]).
calls_to_specific_function(input_par_prompts) ->
    ["Module name: ", "Function name: ", "Arity: "];

calls_to_specific_function(_Args=#args{user_inputs=[M,F,A], 
                                       search_paths=SearchPaths}) ->
    {M1, F1, A1}={list_to_atom(M), list_to_atom(F), list_to_integer(A)},                              
    ?FULL_TD_TU([?COLLECT_LOC(?FUN_APPLY(M1, F1, A1), true)],
                [SearchPaths]).

 
%%===================================================================
%% Collects clause bodies that ends in the format of `Var=Expr, Var',
%% and returns the location information of `Var=Expr, Var'.

%% No user inputs are needed.
-spec(unnecessary_match/1::(#args{}) ->[{filename(), {pos(),pos()}}]).
                                    
unnecessary_match(input_par_prompts) ->[];
  
%% Instead of collecting the location of the whole matching node, this
%% function only returns the location of the last two expressions; therefore
%% we cannot use the ?COLLECT_LOC macro. `_File@' is a meta variable 
%% generated automatically  to represent the name of the file to which
%% the matching code belong, and is visible to both the second and the third 
%% arguments of the ?COLLECT macro.
%% When the collector returns the data collected in the format of 
%% {File,{StartPos, EndPos}}, Wrangler will display the result in such a way 
%% that the location information is mouse clickable. 
unnecessary_match(_Args=#args{search_paths=SearchPaths}) ->
    ?FULL_TD_TU([?COLLECT(?T("Body@@, V@=Expr@, V@"), 
                          {_File@, wrangler_misc:start_end_loc(lists:nthtail(length(Body@@), _This@))},
                          api_refac:type(V@) == variable)],
                SearchPaths).


%%===================================================================
%% Collects the recursive function definitions that are not tail-recursive,
%% and returns the MFA information of those functions.
-spec(non_tail_recursive_function/1::(#args{}) ->[{modulename(), functionname(), arity()}]).
                                
non_tail_recursive_function(input_par_prompts)-> [];
non_tail_recursive_function(_Args=#args{search_paths=SearchPaths}) ->
    ?FULL_TD_TU([?COLLECT(?T("f@(Args@@@) when Guard@@@-> Body@@@."), 
                          fun_define_info(f@),
                          is_non_tail_recursive(_This@))],
                [SearchPaths]).

%% Returns `true' if a recursive function definition is not tail recursive. 
is_non_tail_recursive(FunDef) ->
    MFA= fun_define_info(FunDef),
    Cond= fun(This, Last) ->
                  AllApps=collect_apps(This,MFA),
                  LastApps=collect_last_apps(Last, MFA),
                  SimpleExprs=collect_simple_exprs(Last),
                  EnclosedApps=apps_enclosed_in_simple_exprs(LastApps,SimpleExprs),
                  LastExprLoc=wrangler_misc:start_end_loc(Last),
                  AllApps /= [] andalso (AllApps--[LastExprLoc]/= LastApps 
                                         orelse EnclosedApps /= [])
         end,
    %% collect each function clause that is not tail recursive.
    Res=?FULL_TD_TU([?COLLECT(?T("f@(Args@@) when Guard@@ -> Body@@, Last@;"),
                              true,
                              Cond(_This@, Last@))],
                    FunDef),
    %% A function is not tail-recursive if any of its function 
    %% clause is recursive, but not tail-recursive.
    lists:member(true, Res).

%% collect all the function application instances of 
%% a specific function.
collect_apps(FunDef, MFA) ->
    ?FULL_TD_TU([?COLLECT(?T("F@(Args@@)"),
                          wrangler_misc:start_end_loc(_This@),
                          fun_define_info(F@) == MFA)],
                 FunDef).

%% collect all the last clause body expressions that 
%% are function applications.
collect_last_apps(Last, MFA) ->
    ?FULL_TD_TU([?COLLECT(?T("Body@@, F@(Args@@)"),
                          wrangler_misc:start_end_loc(lists:last(_This@)),
                          fun_define_info(F@)==MFA
                         )],
                Last).
%% collect all the expressions that is part of `LastExpr', but
%% is not a `case'/`if'/`receive'/`block'/'parentheses' expression.
collect_simple_exprs(LastExpr) ->
    ?FULL_TD_TU([?COLLECT(?T("E@"),
                          wrangler_misc:start_end_loc(E@),
                          api_refac:is_expr(E@) andalso
                          not (lists:member(api_refac:type(E@),
                                            [case_expr, receive_expr, 
                                             if_expr,parentheses,
                                             block_expr]))
                         )
                          ],
                LastExpr).

%% returns the subset of `Apps' that each of which is  
%% locationally enclosed by at least one member of `Exprs'.
apps_enclosed_in_simple_exprs(Apps, Exprs) ->
    lists:filter(fun(AppLoc) ->
                         lists:any(fun(ExprLoc) ->
                                           enclose(ExprLoc, AppLoc)
                                   end,Exprs)
                 end, Apps).

%% returns `true' is location `Loc1' is enclosed by 
%% location location `Loc', but only equal to `Loc2'.
enclose(_Loc1={Start1, End1},_Loc2={Start2,End2}) ->
    (Start1 =< Start2 andalso End2 < End1) orelse
        (Start1 < Start2 andalso End2 =< End1).

  
%%===================================================================
%% Collects all function clauses whose clause body have a sequence of 
%% two or more expressions.
%%@private
test(input_par_prompts)->
    [];
test(_Args=#args{search_paths=SearchPaths}) ->
    ?FULL_TD_TU([?COLLECT(?T("f@(Args@@) when Guard@@-> First@, Second@,Body@@;"),
                          api_refac:fun_define_info(f@),
                          true)],
                [SearchPaths]).


%%===================================================================
%% Collects all functions, and returns the MFA info of each function.
%%@private
test1(input_par_prompts)->
    [];
test1(_Args=#args{search_paths=SearchPaths}) ->
     ?FULL_TD_TU([?COLLECT(?T("f@(Args@@@) when Guard@@@-> Body@@@."),
                           api_refac:fun_define_info(f@),
                          true)],
                 [SearchPaths]).

%%===================================================================
%% Collects all function clauses, and returns the MFA info of each 
%% function clause collected.
%%@private
test2(input_par_prompts)->
    [];
test2(_Args=#args{search_paths=SearchPaths}) ->
    ?FULL_TD_TU([?COLLECT(?T("f@(Args@@)when Guard@@-> Body@@;"),
                         api_refac:fun_define_info(f@),
                         true)],
                [SearchPaths]).

%%===================================================================
%% Collects all function clause without guards, and returns the MFA 
%% info of each function clause collected.
%%@private
test3(input_par_prompts)->
    [];
test3(_Args=#args{search_paths=SearchPaths}) ->
    ?FULL_TD_TU([?COLLECT(?T("f@(Args@@)-> Body@@;"),
                          api_refac:fun_define_info(f@),
                          true)],
                [SearchPaths]).


%%@private 
test4(input_par_prompts) ->
    [];
test4(_Args=#args{search_paths=SearchPaths}) ->
    ?FULL_TD_TU([?COLLECT(?T("case lists:keysearch(Key@, N@, TupleList@) of 
                         Pats@@@ when Guards@@@ ->
                             Body@@@
                    end"),
                          {_File@, wrangler_misc:start_end_loc(_This@)},
                          true),
                 ?COLLECT(?T("f@(Args@@) when Guard@@ ->Body@."), 
                          api_refac:fun_define_info(f@),
                          api_refac:type(Body@) == if_expr)],
                [SearchPaths]).



test5(input_par_prompts)->
    [];
test5(_Args=#args{current_file_name=CurFileName}) ->
    ?FULL_TD_TU([?COLLECT_LOC(?T("[1,2,F@]"),
                              true)],
                [CurFileName]).
