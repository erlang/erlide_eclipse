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
%%@author  Huiqing Li <H.Li@kent.ac.uk>
%%
%%
%% @doc This module defines the `gen_composite_refac' behaviour. It provides a high-level
%% abstraction of the logic work-flow of a composite refactoring process. The  behaviour 
%% defines two callback functions that are to be implemented by the user as explained next.
%%
%% Callback function `input_par_prompts/0':
%% ```input_par_prompts() 
%%      ===> [string()]'''
%%  `input_par_prompts' returns the list of prompt strings to be used when 
%%   the refactorer asks the user for input at the very beginning of the composite 
%%   refactoring. There should be one prompt string for each input.
%% Callback function `composite_refac/1':
%% ```composite_refac(Args::#args()) 
%%       ===> composite_refac_script()'''
%%   `composite_refac' is the function in which the user could script a 
%%   composite refactoring. The definition of record `args' is the same as the 
%%   the record using by the `gen_refac' behaviour to be consistent, but the 
%%   `selective' field should not be used. 
%%
%% Record `args' defines the data structure that is passed to `composite_refac'.
%%  ```-record(args,{current_file_name :: filename(),         %% the file name of the current Erlang buffer.
%%                   cursor_pos        :: pos(),              %% the current cursor position.
%%                   highlight_range   :: {pos(), pos()},     %% the start and end location of the highlighted code if there is any.
%%                   user_inputs       :: [string()],         %% the data inputted by the user.
%%                   focus_sel         :: any(),              %% the focus of interest selected by the user.
%%                   search_paths      ::[dir()|filename()],  %% the list of directories or files which specify the scope of the project.
%%                   tabwidth =8        ::integer()           %% the number of white spaces denoted by a tab key.
%%                  }).'''
%%
%% Before explaining how to scripting composite refactorings, we clarify the meaning 
%% of some terms we use.
%%
%% <b> Precondition </b>. A precondition is a predicate over a program that returns 
%% either `true' or `false' when applied to a program.
%%
%% <b> Transformation rule</b>. A transformation rule maps a program from one state into another.
%%
%% <b> Refactoring</b>. A refactoring is either a elementary refactoring or a composite refactoring.
%%
%% <b>Elementary refactoring</b>. A elementary refactoring is an elementary behaviour-preserving 
%% source-to-source program transformation that consists of a set of preconditions C,  
%% and a set of transformation rules T.  When a elementary refactoring is applied to a 
%% program, all the preconditions are checked before the programming is actually transformed. 
%% We say a elementary refactoring <i> fails </i>  if the conjunction of the  set of 
%% preconditions returns false; otherwise we say the elementary refactoring <i> succeeds </i>. 
%%
%% <b>Composite Refactoring</b>. A composite refactoring is either an atomic composite 
%%refactoring or a non-atomic composite refactoring.
%%
%% <b> Atomic composite refactoring </b> Given a sequence of elementary refactorings, `R_1, ..., R_n(n>=1)',
%% the atomic composition of `R_1, ..., R_n', creates a new refactoring consisting of 
%% the sequential application of refactorings from `R_1' to `R_n'.  If any `R_i(1=<i=<n)' fails, 
%% then the whole refactoring fails and the original program returns unchanged.
%% The composite refactoring succeeds if all `R_i(1=<i=<n)' succeeds, and the result 
%% program is the program returned after applying `R_n'.
%%
%% <b> Non-atomic composite refactoring </b>
%% Given a sequence of refactorings `R_1, ..., Rn(n>=1)', the non-atomic composition of `R_1, ..., R_n',  
%% creates a new refactoring consisting of the sequential application of refactorings from 
%% `R_1 to R_n'. If refactoring `R_i' fails, the execution of `R_{i+1}' continues. A failed refactoring 
%% does not change the status of the program. The program returned by applying `R_n' is the final result 
%% of the application of the composite refactoring. As a convention, we say that a non-atomic
%% composite refactoring always succeeds.
%%
%% === Refactoring Command Generators ===
%% Each primitive refactoring in Wrangler has been extended with a refactoring command generator. The
%% interface of a command generator is enriched in such a way that it accepts not only concrete values 
%% as what a primitive refactoring does, but also structures that specify the constraints that a parameter 
%% should meet or structures that specify how the value for a parameter should be generated.
%%
%% When applied to an Erlang program, a command generator searches the AST representation of the program 
%% for refactoring candidates according to the constraints on arguments. A command generator can also be 
%% instructed to run lazily or strictly; if applied strictly, it returns the complete list of primitive 
%% refactoring commands that can be generated in one go; otherwise, it returns a single refactoring 
%% command together with another command generator wrapped in a  function closure, or an empty list 
%% if no more commands can be generated.  Lazy refactoring command generation is especially useful 
%% when the primitive refactoring command refers some  program entities by locations, or the 
%% effect of a previous refactoring could affect the refactorings after.
%%
%% Each primitive refactoring command  generated is  represented as a tuple in  the format of:
%% `{refactoring,  RefacName, Args}', where `RefacName' is the name of the refactoring command,
%%  and `Args' is the list representation of the arguments for that refactoring command. 
%% A refactoring command generator is also syntactically represented as a three-element tuple, 
%% but with a different tag, in the format of `{refac_, RefacName, Args}', where `RefacName' is 
%% the name of the refactoring, and `Args' are the arguments that supplied to the command generator. 
%%
%% Take the `rename function' refactoring as an example, the type specification of the refactoring
%% command is:
%% ``` -spec rename_fun(File::filename(), FunNameArity::{atom(), integer()},
%%                  NewName::atom()) -> ok | {error, Reason::string()}.'''
%% which should be clear enough to explain itself.
%% The type specification of the command generator is:
%% ```-spec rename_fun(File::filename() | fun((filename()) -> boolean()),
%%                  FunNameArity::{atom(), integer()}
%%                               | fun(({atom(),integer()}) -> boolean()),
%%                  NewName::atom() 
%%                    |{generator, fun(({filename(), {atom(), integer()}})
%%                                     -> atom())}
%% 	                   |{user_input,fun(({filename(), {atom(), integer()}})
%%                                    -> string())},
%%                  Lazy :: boolean())
%%            -> [{refactoring, rename_fun, Args::[term()]}] |
%%               {{refactoring, rename_fun, Args::[term()]}, function()}.'''
%% As it shows, a command generator accepts not only actual values, but also function closures 
%% that allow values to be generated by analyzing the code to be refactored . The first parameter 
%% of the generator accepts either a file name, or a condition that a file should satisfy to be
%% refactored. In the latter case, Wrangler will search the whole program for files that meet 
%% the condition specified, and only those files are further analyzed in order to generate values 
%% for the remaining parameters. The second parameter accepts either a function name tupled with 
%% its arity, or a condition that a function should meet in order to be refactored. In the latter 
%% case, every function in an Erlang file will be checked, and those functions that do not meet 
%% the condition are filtered out, and a primitive refactoring command is generated for each function
%% that meets the condition. The third argument specifies how the new function name should be generated. 
%% It could be a fixed function name, a generator function that generates the new function based on the 
%% previous parameter values, or a name that will be supplied by the user before the execution of the 
%% refactoring, in which case the function closure is used to generate the prompt string that will be 
%% shown to the user when prompting for input. Finally, the last parameter allows the user to choose 
%% whether to generate the commands lazily or not.
%%
%% The following example illustrates the generation of refactoring commands that rename all functions 
%% in a program whose name is in `camelCase' format to `camel _case' format.
%%   ```{refac_, rename_fun, [fun(_File)-> true end,
%%                            fun({FunName, _Arity}) -> is_camelCase(FunName) end,
%%                            {generator, fun({_File,{FunName,_Arity}}) ->
%%                                           camelCase_to_camel_case(FunName)
%%                                        end}, false]}'''
%% As the condition for the first parameter always returns true, every  file in the program should be checked.
%% The second argument checks if the function name is in `camelCase' format using the utility function 
%% `is_camelCase', and a refactoring command is generated for each function whose name is in `camelCase' format.
%%  The new function name is generated  by applying the utility function `camelCase_to_camel_case' to the old 
%% function name. In this example, we choose to generate the refactoring commands in a strict way.
%%
%% For some command generators, it is also possible to specify the order in which the functions in an 
%% Erlang file are visited. By default, functions are visited by their textual order in the file, but it 
%% is also possible for them to be visited according to the function callgraph in either top-down or 
%% bottom-up order.
%%
%% For the type specification of refactoring command generators, please see 
%% <a href="wrangler_extended.html">wrangler_extended</a>.
%%
%% To allow fine control over the generation of refactoring commands and the way a refactoring command 
%% to be run, we have defined a small language for scripting composite refactorings. The DSL, as shown 
%% below, is defined in Erlang syntax. In the definition, `ER' denotes a primitive refactoring, and `CR' 
%% denotes a composite refactoring. we explain the definition of `CR' in more details now. 
%%
%%     ``` RefacName ::= rename_fun | rename_mod | rename_var | new_fun |gen_fun | ... 	
%%         ER ::= {refactoring, RefacName, Args}
%%         CR ::= ER 
%%               |{interactive, Qualifier, [ERs]}
%%               |{repeat_interactive, Qualifier, [ERs]} 
%%               |{if_then, fun()-> Cond end, CR} 	
%%               |{while, fun()-> Cond end, Qualifier, CR} 
%%               |{Qualifier, [CRs]}
%%         ERs::= ER | ERs, ER 
%%         CRs ::= CR |CRs, CR 	
%%         Qualifier ::= atomic | non_atomic 
%%         Args ::= ...A list of Erlang terms ...
%%         Cond ::=  ...An Erlang expression that evaluates to a  `boolean' value ... '''
%%
%% The definition of `CR' is explained in more details below.  
%% A primitive refactorings is an atomic composite refactoring by definition.
%% `{interactive, Qualifier, [ERs]}' represents a list of primitive refactorings that to be 
%%  executed in an interactive way, that is, before the execution of every primitive refactoring,
%%  Wrangler asks the user for confirmation that he/she really wants that refactoring to be applied.
%% `{repeat_interactive, Qualifier, [ERs]}' also represents a list of primitive refactorings to be 
%% executed in an interactive way, but different from the previous one, it allows user to repeatedly 
%% apply one refactoring, with different parameters supplied, multiple times.
%% `{if_then, Cond, CR}' represents the conditional application of `CR', i.e. `CR' is applied only 
%% if `Cond', which is an Erlang expression, evaluates to `true'.
%% `{while, Cond, Qualifier, CR}' allows `CR', which should be generated dynamically, to be continually 
%% applied until $Cond$ evaluates to `false'. `Qualifier' specifies the way in which 
%% the refactoring between each loop should be composed.
%% `{Qualifier, [CRs]}'  represents the composition of a list of composite refactorings into a new composite refactoring. 
%%
%% === Macro Definitions ===
%% In practice, instead of writing tuples with tags in a composite refactoring script, we use macros. A suite of macros 
%% have been defined for this purpose as follows.
%%   
%%  `?interactive([ERs])'  represents  `{interactive, atomic, [ERs]}'.
%%        
%%  `?interactive(Qualifier,[ERs])' represents  `{interactive, Qualifier, [ERs]}'.
%% 
%%  `?repeat_interactive([ERs])'  represents  `{repeat_interactive, atomic, [ERs]}'.
%%        
%%  `?repeat_interactive(Qualifier,[ERs])' represents  `{repeat_interactive, Qualifier, [ERs]}'.
%% 
%%  `?if_then(Cond, CR)' represents `{if_then, fun()-> Cond end, CR}'.
%%     
%%  `?while(Cond, CR)' represents `{while, fun() -> Cond end, atomic, CR}'.
%%
%%  `?while(Cond, Qualifier, CR)' represents `{while, fun() -> Cond end, Qualifier, CR}'.
%%
%%  `?try_refac([CRs])' represents `{non_atomic, [CRs]}'.
%%
%%  `?refac([CRs])'    represents `{atomic, [CRs]} '.
%%
%% ===Tracking of Entity Names===
%%In a composite refactoring, it is possible that a refactoring needs to refer to a program entity that might have be 
%% renamed by previous refactoring steps. Tracking the change of names statically is problematic given the dynamic nature 
%% of a refactoring process.Wrangler allows users to refer to a program entity through its initial name, i.e. the name of 
%% the entity before the refactoring process is started.  For this purpose, we have defined a macro `?current'.  An entity 
%% name wrapped in a `?current' macro tells Wrangler that this entity might have been renamed, therefore Wrangler needs to 
%% search its record of renaming history, and replaces the macro application with the entity's latest name. If no renaming 
%% history can be found for that entity, its original name is used.
%%
%% In the background, Wrangler monitors application of `renaming'  refactorings, and keeps a record of the mapping between 
%% the old and new names. Currently, only module names and function names are tracked, as these are the entities that can 
%% be referred by name and also can be renamed by the refactorings supported by Wrangler. Syntactically, a module name is 
%% represented as an Erlang atom, and a function name is represented by a tuple consisting of the module name, function name 
%% and arity. The data format returned by the application of the `?current' is the same as its input.
%%
%%%% Some example composite refactorings:
%%<li>
%%<a href="file:refac_batch_clone_removal.erl" >Batch clone elimination;</a>.
%%</li>
%%<li>
%%<a href="file:examples/refac_batch_tuple_args.erl" >Batch tupling of function arguments;</a>.
%%</li>
%%<li>
%%<a href="file:examples/refac_batch_rename_fun.erl">Batch renaming of functions. </a>
%%</li>
%%
%% === How to apply an own-defined composite refactoring ===
%% To invoke a user's own-defined `gen_composite_refac' refactoring, select menu item Wrangler -> Refactor -> Apply Composite Refactoring, 
%% Wrangler will then prompt you to input the name of the callback module, and the values for parameters needed by the 
%% refactoring. 
%%
%% === How to add an own-defined composite refactoring to the menu ===
%% To add a user-defined `gen_composite_refac' refactoring to the `My gen_composite_refac Refacs' menu, set the file buffer implementing the 
%% refactoring as the current buffer, then select Wrangler -> Refactor -> Add to My gen_composite_refac Refacs. 

-module(gen_composite_refac).

-export([init_composite_refac/2,
         get_next_command/1, 
         input_par_prompts/1]).

-export([behaviour_info/1]).

-compile(export_all).

-include("../include/wrangler.hrl").

%%@private
-spec behaviour_info(atom()) ->[{atom(), arity()}].
behaviour_info(callbacks) ->
    [{composite_refac,1}, 
     {input_par_prompts, 0},
     {select_focus,1}].

%%@private
input_par_prompts(CallBackMod) ->
    Res =input_pars_1(CallBackMod),
    {ok, Res}.

input_pars_1(CallBackMod) when is_atom(CallBackMod)->
    erlang:apply(CallBackMod, input_par_prompts, []);
input_pars_1(CallBackMod) when is_list(CallBackMod)->
    erlang:apply(list_to_atom(CallBackMod), input_par_prompts, []);
input_pars_1(_) ->
    throw:error(badarg).

-spec(select_focus(Module::module(), Args::args()) ->
             {ok, term()} | {error, term()}).
select_focus(Module, Args) ->
    case apply(Module, select_focus, [Args]) of 
        {ok, Term} ->
            {ok, Term};
        {error, Reason} ->
            {error, Reason};
        _ ->
            {error, "The value returned by callback function select_focus/1 "
             "is different from the return type expected."}
    end.

%%@private
init_composite_refac(ModName, Args=[CurFileName, [Line,Col],
                                    [[StartLine, StartCol],
                                     [EndLn, EndCol]], UserInputs,
                                    SearchPaths, TabWidth])->
    ?wrangler_io("\nCMD: ~p:init_composite_refac(~p,~p).\n",
		 [?MODULE, ModName, Args]),
    Module = if is_list(ModName) ->
                     list_to_atom(ModName);
                true ->
                     ModName
             end,
    Args0=#args{current_file_name=CurFileName,
                cursor_pos = {Line, Col},
                highlight_range = {{StartLine, StartCol},
                                   {EndLn, EndCol}},
                user_inputs = UserInputs,
                search_paths = SearchPaths,
                tabwidth = TabWidth},
    case select_focus(Module, Args0) of
        {ok, Sel}->
            Args1 = Args0#args{focus_sel=Sel},
            case apply(Module, composite_refac, [Args1]) of
                {error, Reason} ->
                    {error, Reason};
                CR ->
                    try start_cmd_server(CR)
                    catch
                        E1:E2 ->
                            erlang:error({E1,E2})
                    end
            end;
        {error, Reason} ->
            {error,Reason}
    end.
%%@private
start_cmd_server(Cmds) ->
    wrangler_backup_server:reset_backups(),
    case whereis(wrangler_cmd_server) of 
        undefined ->
            ok;
        _ -> wrangler_cmd_server!stop
    end,
    wrangler_cmd_server:start_link(Cmds).

stop_cmd_server() ->
    wrangler_cmd_server:stop().
   
%%@private
get_next_command(PrevResult) ->
    try get_next_command_1(PrevResult) of 
        Val -> Val
    catch
        throw:Error -> 
            Error;   
        _E1:E2 ->
            {error, lists:flatten(io_lib:format("\n~p",[{E2, erlang:get_stacktrace()}]))}
    end.
get_next_command_1(PrevResult) ->
    case PrevResult of 
        none -> ok;
        _ -> wrangler_io:format("The result returned by the previous refactoring:\n ~p\n", 
                                [PrevResult])
    end,
    Cmd=wrangler_cmd_server:get_next_command(PrevResult),
    case Cmd of 
        {ok, none, _ChangedFiles, [error, _Reason]} ->
            stop_cmd_server(),
            wrangler_backup_server:recover_backups(),
            Cmd;
        {ok, none, _ChangedFiles, _Msg} ->
            stop_cmd_server(),
            {ok, PreviewPairs}=wrangler_backup_server:recover_backups(),
            wrangler_preview_server:add_files({PreviewPairs, ""}),
            Cmd;
        {ok,_C} ->
            %% wrangler_io:format("Next refactoring command:\n~p\n", [C]),
            Cmd;
        _ ->
            Cmd
    end.
    
