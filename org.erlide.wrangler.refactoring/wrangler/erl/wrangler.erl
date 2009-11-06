%% Copyright (c) 2009, Huiqing Li, Simon Thompson
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
%% Refactoring Interface Functions.
%%
%% Author contact: H.Li@kent.ac.uk, Simon.J.Thompson@kent.ac.uk
%%
%% =====================================================================

%% @copyright 2006-2009 Huiqing Li, Simon Thompson
%%
%% @author Huiqing Li, Simon Thompson
%%   [http://www.cs.kent.ac.uk/projects/wrangler]

%% @version 0.8.2
%% @end
%%
%% @doc This module describes the refactorings that are currently supported by Wrangler.
-module(wrangler).

-export([rename_var/6, rename_fun/6, rename_mod/4,
	 rename_process/6, rename_mod_batch/4, generalise/6,
	 move_fun/7, duplicated_code_in_buffer/4,
	 duplicated_code_in_dirs/4,
	 identical_expression_search/4,
	 similar_expression_search/6, fun_extraction/5,
	 fold_expr/1, fold_expr_by_loc/5, fold_expr_by_name/7,
	 instrument_prog/3, similar_code_detection_in_buffer/6,
	 similar_code_detection/6, uninstrument_prog/3,
	 add_a_tag/6, tuple_funpar/5, tuple_funpar_1/5,
	 tuple_to_record/9, register_pid/6, fun_to_process/6,
	 new_macro/6, fold_against_macro/5,
	 normalise_record_expr/6, unfold_fun_app/4]).

-export([rename_var_eclipse/6, rename_fun_eclipse/6, rename_fun_1_eclipse/6,
	 rename_mod_eclipse/4, rename_mod_1_eclipse/5,
	 generalise_eclipse/6,
	 move_fun_eclipse/7, fun_extraction_eclipse/5,
	 gen_fun_1_eclipse/8, gen_fun_2_eclipse/8,
	 tuple_funpar_eclipse/5, tuple_funpar_eclipse_1/5,tuple_to_record_eclipse/9,
	 fold_expr_by_loc_eclipse/5, fold_expr_by_name_eclipse/7,
	 fold_expression_1_eclipse/5,fold_expression_2_eclipse/7,
	 new_macro_eclipse/6, rename_process_eclipse/6, rename_process_1_eclipse/5, 
	 fun_to_process_eclipse/6, fun_to_process_1_eclipse/6,
	 unfold_fun_app_eclipse/4]).

-export([try_refactoring/3, try_inspector/3]).
-include("../include/wrangler.hrl").

%% ====================================================================================================
%% @doc Rename a variable.
%% <p> This refactoring has a local effect, i.e., it only affects the function clause in which the refactoring is initialized. 
%% </p>
%% <p> The following <em> side-conditions </em> (or <em>pre-conditions</em>) apply to this refactoring: 
%% <li> The new variable name should not conflict with any of the declared variable names in the same scope;</li>
%% <li> The new variable name should not shadow any of the existing variables in the outer scopes, or be shadowed by any 
%% of existing variables in the inner scopes, i.e., renaming to the new name should not change the semantics of the 
%% program.</li>
%% </p>
%% <p> Usage: to apply this refactoring, point the cursor to any occurrence of this variable, then select
%% <em> Rename Variable Name </em> from <em> Refactor </em>, after that, Wrangler will prompt
%% to enter the new variable name in the mini-buffer. 
%% </p>
%% @spec rename_var(FileName::filename(), Line::integer(), Col::integer(), NewName::string(), SearchPaths::[dir()], TabWidth:: integer())
%%  ->{error, string()} | {ok, string()}
-spec(rename_var/6::(filename(), integer(), integer(), string(), [dir()], integer()) ->
	     {error, string()} | {ok, filename()}).
rename_var(FileName, Line, Col, NewName, SearchPaths, TabWidth) ->
    try_refactoring(refac_rename_var, rename_var, [FileName, Line, Col, NewName, SearchPaths, TabWidth]).
   

%% @private
-spec(rename_var_eclipse/6::(filename(), integer(), integer(), string(), [dir()], integer()) ->
	     {error, string()} | {ok, [{filename(), filename(), string()}]}).
rename_var_eclipse(FileName, Line, Col, NewName, SearchPaths, TabWidth) ->
    try_refactoring(refac_rename_var, rename_var_eclipse, [FileName, Line, Col, NewName, SearchPaths, TabWidth]).

%%=========================================================================================
%% @doc Rename a function.
%% <p>
%% When renaming an exported function, this refactoring has a global effect, that is, 
%% it affects all those modules in which this function is imported/used.
%% </p>
%% <p> The following <em> side-conditions </em> (or <em>pre-conditions</em>}  apply to this refactoring:
%% <li> The new function name should not cause confliction with any of the functions which are in scope in the 
%% current module;</li>
%% <li> In the case that the function to be renamed is imported by another module, the new function name (with the same 
%% arity) should not be already in scope (either defined or imported) in that module. </li>
%% </p>
%% <p> Usage: to apply this refactoring, point the cursor to any occurrence of this 
%% function name, then select <em> Rename Function Name </em> from the <em> Refactor </em> menu, 
%% after that, Wrangler will prompt to enter  the new function name in the mini-buffer.
%% </p>
%% @spec rename_fun(FileName::filename(), Line::integer(), Col::integer(), NewName::string(), SearchPaths::[dir()], TabWidth:: integer())
%% -> {error, string()} | {ok, [filename()]}
-spec(rename_fun/6::(string(), integer(), integer(), string(), [dir()], integer()) ->
	     {error, string()} |{warning, string()}| {ok, [filename()]}).
rename_fun(FileName, Line, Col, NewName, SearchPaths, TabWidth) ->
    try_refactoring(refac_rename_fun, rename_fun, [FileName, Line, Col, NewName, SearchPaths, TabWidth]).

%%@private
-spec(rename_fun_eclipse/6::(string(), integer(), integer(), string(), [dir()], integer()) ->
	     {error, string()} | {warning, string()} | {ok, [{filename(), filename(), string()}]}).
rename_fun_eclipse(FileName, Line, Col, NewName, SearchPaths, TabWidth) ->
    try_refactoring(refac_rename_fun, rename_fun_eclipse, [FileName, Line, Col, NewName, SearchPaths, TabWidth]).

%%@private
-spec(rename_fun_1_eclipse/6::(string(), integer(), integer(), string(), [dir()], integer()) ->
	     {error, string()} | {ok, [filename()]}).
rename_fun_1_eclipse(FileName, Line, Col, NewName, SearchPaths, TabWidth) ->
    try_refactoring(refac_rename_fun, rename_fun_1_eclipse, [FileName, Line, Col, NewName, SearchPaths, TabWidth]).
    

%%======================================================================================
%% @doc Rename a module.
%% <p> This refactoring affects all those modules in which the module name is used.
%% </p>
%% <p>
%% The following <em> side-conditions </em> apply to this refactoring:
%% <li> The new module name should not have been used as a module name in the program under consideration. </li>
%% <li> This refactoring assume that the file basename is always the same as the module name, therefore this 
%% refactoring changes the filename as well. </li>
%% </p>
%% <p> Usage: to apply this refactoring, point the cursor anywhere in the module to be renamed, then select 
%% <em> Rename Module Name </em> from the <em> Refactor </em> menu, after that, the refactorer will prompt to enter 
%% the new module name in the mini-buffer.
%% </p>
%%@spec rename_mod/4::(filename(), string(), [dir()], integer()) -> 
%%	     {error, string()} | {question, string()} | {warning, string()} |{ok, [filename()]}
-spec(rename_mod/4::(filename(), string(), [dir()], integer()) -> 
	     {error, string()} | {question, string()} | {warning, string()} |{ok, [filename()]}).
rename_mod(FileName, NewName, SearchPaths, TabWidth) ->
    try_refactoring(refac_rename_mod, rename_mod,  [FileName, NewName, SearchPaths, TabWidth]).

%%@private
-spec(rename_mod_eclipse/4::(FileName::filename(), NewName::string(), SearchPaths::[dir()], TabWidth::integer()) ->
	     {error, string()} | {question, string()} | {warning, string()} |
		 {ok, [{filename(), filename(), string()}]}).
rename_mod_eclipse(FileName, NewName, SearchPaths, TabWidth) ->
    try_refactoring(refac_rename_mod, rename_mod_eclipse, [FileName, NewName, SearchPaths, TabWidth]).

%%@private
-spec(rename_mod_1_eclipse/5::(FileName::filename(), NewName::string(), SearchPaths::[dir()], TabWith::integer(), RenameTestMod::bool())
      ->{ok, [{filename(), filename(), string()}]}).
rename_mod_1_eclipse(FileName, NewName, SearchPaths, TabWidth, RenameTestMod) ->
    try_refactoring(refac_rename_mod, rename_mod_1_eclipse, [FileName, NewName, SearchPaths, TabWidth, RenameTestMod]).

%% =====================================================================
%%@private
%% @doc Rename a collection of modules in batch mode. 
%% <p> This refactoring has a global effect. </p>
%% <p> The following <em> side-conditions </em> apply to this refactoring:
%% <li> The new module names should not conflict with each other, or any existing module names 
%% in the same scope which will not be renamed. </li>
%% <li> This refactoring assumes that the file basename is always the same as the module name, therefore 
%% filenames will be changed along with module names. </li>
%% </p>
%% <p> Usage: this refactoring is supposed to be run from the Erlang shell. For example, 
%% to rename all those module names which match the regular expression "foo_*" to 
%% "foo_*_1_0" in the directory <code> c:/wrangler/test </code>, just type the following command:
%% <code> wrangler:rename_mod_batch("foo_*, "foo_*_1_0", ["c:/wrangler/test"]) </code>.
%% </p>
% @spec rename_mod_batch(OldNamePattern::string(), NewNamePattern::string(), 
%%                        SearchPaths::[dir()], TabWidth:: integer())-> {ok, string()}| {error, string()}
   
-spec(rename_mod_batch/4::(string(), string(), [dir()], integer())->
	     {ok, string()} | {error, string()}).
rename_mod_batch(OldNamePattern, NewNamePattern, SearchPaths, TabWidth) ->
    try_refactoring(refac_batch_rename_mod, batch_rename_mod, [OldNamePattern, NewNamePattern, SearchPaths, TabWidth]).


%% ==========================================================================================
%% @doc  Generalise a function definition.
%% <p>Generalise a function definition by selecting a sub-expression of its right-hand 
%% side and making this the value of a new argument added to the definition of the function. 
%% The sub-expression becomes the actual parameter at the call sites. </p>
%%
%% <p> Here is an example of generalisation, in which the function <code> add_one </code> defined 
%% on the left-hand side is generalised on the expression <code>1 </code>, and the result is 
%% shown on the right-hand side. 
%%
%%        ```    -module (test).                          -module (test). 
%%               -export([f/1]).                          -export([f/1]).
%%        
%%               add_one ([H|T]) ->                       add_one (N, [H|T]) ->
%%                  [H+1 | add_one(T)];                      [H+N | add_one(N,T)];
%%               add_one ([]) -> [].                      add_one (N, []) -> [].
%%
%%               f(X) -> add_one(X).                      f(X) -> add_one(1,X)
%%        ''' 
%%  </p>
%%
%% <p> In the case that the selected expression has a side-effect, the refactorer will wrap this expression 
%% in an function expression before passing it as the actual parameter to the call-sites. This is illustrated 
%% in the following example, in which function <code>repeat/1</code> is generalised on the expression 
%% <code>io:format("Hello\n")</code>.
%% 
%%         ```   -module (test).                          -module (test).                          
%%               -export([f/0]).                          -export([f/0]).
%%
%%               repeat(0) -> ok;                         repeat(A, 0) -> ok;
%%               repeat(N) ->                             repeat(A, N) ->
%%                 io:format("Hello\n"),                    A( ),
%%                 repeat(N-1).                             repeat(A,N-1).
%%
%%               f() -> repeat(5).                        f( ) -> 
%%                                                           repeat (fun( )->io:format ("Hello\n") end, 5).
%%          '''
%% </p>
%%
%% <p> This refactoring <em>only </em> affects the module in which the refactoring is initialised. In the case that 
%% the generalised function is exported by the module, an auxiliary function will be created 
%% to wrap the generalised function up, so that the module's interface is not changed.
%% </p>
%% <p> The following <em> side-conditions </em> apply to this refactoring:
%% <li> Suppose the function to be generalised is <code>foo/n </code>, then <code>foo/n+1</code> should not  
%% be in scope before the generalisation;</li>
%% <li> The new parameter name provided by the user should not conflict with the existing parameters or
%% change the semantics of the function to be generalised. </li>
%% </p>
%% <p> Usage: to apply this refactoring, highlight the expression first, then  select 
%% <em> Generalise Function Definition </em> from the <em>Refactor</em> menu, after 
%% that the refactorer will prompt to enter the parameter name in the mini-buffer. </p>
%% 
%%@spec generalise(FileName::filename(), Start::Pos, End::Pos, ParName::string(), SearchPaths::[dir()], TabWidth:: integer())-> {ok, string()} | {error, string()}
%%         Pos = {integer(), integer()}
-spec(generalise/6::(filename(),pos(), pos(),string(), [dir()], integer()) -> {ok, string()} | {error, string()}).
generalise(FileName, Start, End, ParName, SearchPaths, TabWidth) ->
    try_refactoring(refac_gen, generalise, [FileName, Start, End, ParName,  SearchPaths, TabWidth]).

%%@private
-spec(generalise_eclipse/6::(filename(),pos(), pos(),string(), [dir()], integer()) -> {ok, [{filename(), filename(), string()}]}).
generalise_eclipse(FileName, Start, End, ParName, SearchPaths, TabWidth) ->
    try_refactoring(refac_gen, generalise_eclipse, [FileName, Start, End, ParName,  SearchPaths, TabWidth]).

%%@private
-spec(gen_fun_1_eclipse/8::(bool(), filename(),atom(), atom(), integer(), pos(), syntaxTree(), integer()) -> {ok, [{filename(), filename(),string()}]}).
gen_fun_1_eclipse(SideEffect, FileName, ParName, FunName, Arity, DefPos, Expr, TabWidth) ->
    try_refactoring(refac_gen, gen_fun_1_eclipse, [SideEffect, FileName, ParName, FunName, Arity, DefPos, Expr, TabWidth]).

%%@private
-spec(gen_fun_2_eclipse/8::(filename(),atom(), atom(), integer(), pos(), syntaxTree(), [dir()], integer()) -> {ok, [{filename(), filename(), string()}]}).
gen_fun_2_eclipse(FileName, ParName, FunName, Arity, DefPos, Expr, SearchPaths, TabWidth) ->
    try_refactoring(refac_gen, gen_fun_2_eclipse, [FileName, ParName, FunName, Arity, DefPos, Expr, SearchPaths, TabWidth]).

%% ================================================================================
%% @doc Move a function definition from its current module to another.
%% <p> This refactoring has a global effect, i.e., it affects all the modules in which 
%%     the function is imported/used.
%% </p>
%% <p> This refactoring assumes that an Erlang module name always matches its file name.
%% </p>
%% <p> Suppose we move function <em> foo/n </em> from its current module <em> M </em> 
%%     to module <em> N </em>, then the following <em> side-conditions </em> apply to 
%%     this refactoring: 
%% <li> If <em> foo/n </em> is already in scope in module <em> N </em>, then its defining 
%%      module should be  <em> M </em>.
%% </li>
%% <li> Function <em> foo/n </em> should not contain any uses of <em> implicit fun expressions </em> (Note: move a 
%% collection of functions together to another module will be supported by another refactoring).
%% </li>
%% </p>
%% <p> Usage: to apply this refactoring, point the cursor at the function definition, then 
%% select <em> Move Definition to Another Module</em> from the <em> Refactor </em> menu, 
%% Wrangler will then  prompt to enter the target module name in the mini-buffer. 
%% </p>
%% @spec move_fun(FileName::filename(),Line::integer(),Col::integer(),TargetModName::string(), 
%%                CreateNewFile::bool(),SearchPaths::[dir()], TabWidth:: integer())-> {ok, [filename()]} | {error, string()}

-spec(move_fun/7::(filename(),integer(),integer(), string(), atom(),[dir()], integer())
        -> {ok, [filename()]} | {error, string()}).
move_fun(FileName, Line, Col, TargetModName, CreateNewFile, SearchPaths, TabWidth) ->
    try_refactoring(refac_move_fun, move_fun, [FileName, Line, Col, TargetModName, CreateNewFile, SearchPaths, TabWidth]).


%%@private
-spec(move_fun_eclipse/7::(filename(),integer(),integer(), string(), atom(),[dir()], integer())
        -> {ok, [{filename(), filename(), string()}]}
           | {error, string()}).
move_fun_eclipse(FileName, Line, Col, TargetModName, CreateNewFile, SearchPaths, TabWidth) ->
    try_refactoring(refac_move_fun, move_fun_eclipse, [FileName, Line, Col, TargetModName, CreateNewFile, SearchPaths, TabWidth]).


%% ==================================================================================
%% @doc An identical code detector that searches for identical code within the current Erlang buffer.
%% <p> This function reports duplicated code fragments found in the current Erlang buffer, it does 
%% not remove those code clones though. The user will be prompted for two parameters: the minimum number of 
%% tokens a cloned code fragment should have, and the minimum number of times a code fragment appears in the program.
%% </p>
%% <p> The current version of the identical code detector reports clones that are syntactically 
%% identical after consistent renaming of variables, except for variations in literals, layout and comments.
%% </p>
%% <p>
%% Usage: simply select <em> Detect Identical Code in Current Buffer </em> from <em> Refactor</em>, 
%% Wrangler will prompt to input the parameters.
%% </p>
%% @spec duplicated_code_in_buffer(FileName::filename(),MinToks::integer(),MinClones::integer(), TabWidth::integer()) -> {ok, string()}
%% 
-spec(duplicated_code_in_buffer/4::(filename(), string(), string(), integer()) ->{ok, string()}).     
duplicated_code_in_buffer(FileName, MinToks, MinClones, TabWidth) -> 
    try_refactoring(refac_duplicated_code, duplicated_code, [[FileName], MinToks, MinClones, TabWidth]).


%% =====================================================================================
%% @doc An identical code detector that searches for identical code across multiple Erlang modules.
%% <p> This function reports duplicated code fragments found in the directories specified by SearchPaths, it does
%% not remove those code clones though.
%% The user will be prompted for two parameters: the minimum number of 
%% tokens that a cloned code fragment should have, and the minimum number of times a code fragment appears in the program.
%% </p>
%% <p> The current version of the duplicated code detector reports clones that are syntactically 
%% identical after consistent renaming of variables, except for variations in literals, layout and comments.
%% </p>
%% <p>
%% Usage: first check the SearchPaths specified in the customisation page to make sure that the directory (or directories) specified is 
%% the place where you want to search for duplicated code, then select <em> Detect Identical Code in Dirs </em> from 
%% <em> Refactor</em>, Wrangler will then prompt to input the parameters.
%% </p>
%% @spec duplicated_code_in_dirs(FileNameList::[filename()|dir()], MinToks::string(), MinClones::string(),TabWidth:: integer()) -> {ok, string()}
-spec(duplicated_code_in_dirs/4::([dir()], string(), string(), integer()) ->{ok, string()}).
duplicated_code_in_dirs(FileDirList, MinToks, MinClones, TabWidth) ->
    try_refactoring(refac_duplicated_code, duplicated_code, [FileDirList, MinToks, MinClones, TabWidth]).
    


%% ==================================================================================================
%% @doc A similar code detector that searches for similar code in the current Erlang buffer.
%% <p> This function reports similar expression sequences found in the current Erlang buffer, but does not 
%% remove those clones though. The user needs to provide three parameters to be used by the clone detector, and 
%% they are: the minimum length of an expression sequence, the minimum number of duplication times, and 
%% a similarity score which should be between 0.1 and 1.0.
%% </p>
%% <p> 
%% Usage: select <em> Detect Similar Code in Buffer </em> from <em> Refactor</em>, Wrangler will then prompt to 
%% input the parameters needed.
%% </p>
%% @spec similar_code_detection_in_buffer(FileName::filename(), MinLen::string(), MinFreq::string(), MinScore::string(), 
%%					   SearchPaths::[dir()], TabWidth::integer()) ->  {ok, string()}
-spec(similar_code_detection_in_buffer/6::(FileName::filename(), MinLen::string(), MinFreq::string(), MinScore::string(), 
					   SearchPaths::[dir()], TabWidth::integer()) ->  {ok, string()}).
similar_code_detection_in_buffer(FileName, MinLen, MinFreq, SimiScore, SearchPaths, TabWidth) ->
    try_refactoring(refac_sim_code, sim_code_detection, [[FileName],MinLen, MinFreq, SimiScore, SearchPaths, TabWidth]).

 
%% ==================================================================================================
%%@doc A similar code detector that searches for similar code across multiple Erlang modules.
%% <p> This function reports similar expression sequences found in the directories specified, but does not 
%% remove those clones though. The algorithm is based on the notion of anti-unification, or the least common generalisation.
%% The user needs to provide three parameters to be used by the clone detector, and 
%% they are: the minimum length of an expression sequence, the minimum number of duplication times, and 
%% a similarity score which should be between 0.1 and 1.0.
%% </p>
%% <p> 
%% Usage: select <em> Detect Similar Code in Dirs </em> from <em> Refactor</em>, Wrangler will then prompt to 
%% input the parameters needed.
%% </p>
%%@spec similar_code_detection(DirFileList::[filename()|dir()], MinLen::string(), MinFreq::string(), MinScore::string(), 
%%				 SearchPaths::[dir()], TabWidth::integer()) -> {ok, string()}
-spec(similar_code_detection/6::(DirFileList::[filename()|dir()], MinLen::string(), MinFreq::string(), MinScore::string(), 
				 SearchPaths::[dir()], TabWidth::integer()) -> {ok, string()}).
similar_code_detection(DirFileList, MinLen, MinFreq, SimiScore1, SearchPaths, TabWidth) ->
    refac_sim_code:sim_code_detection(DirFileList, MinLen, MinFreq, SimiScore1, SearchPaths, TabWidth) .
  

%% ==================================================================================================
%% @doc Search for expressions/expression sequences that are identical to the expression/expression 
%% sequence selected after consistent renaming of variables and literal substitution.
%% 
%% <p> This functionality allows searching for clones of a selected expression or expression 
%% sequence.  The found clones are syntactically identical to the code fragment selected  after consistent renaming of variables, 
%% except for variations in literals, layout and comments. 
%% </p>
%% <p> When the selected code contains multiple, but non-continuous sequence of, expressions, the first
%% continuous sequence of expressions is taken as the user-selected expression. A continuous sequence of
%% expressions is a sequence of expressions separated by ','. </p>
%% Usage: highlight the expression/expression sequence of interest, then selected  <em> Indentical Expression Search </em> from 
%% <em> Refactor</em>.
%% @spec identical_expression_search(FileName::filename(),Start::Pos, End::Pos, TabWidth:: integer()) -> {ok, [{integer(), integer(), integer(), integer()}]} | {error, string()}
-spec(identical_expression_search/4::(filename(), pos(), pos(), integer()) -> {ok, [{integer(), integer(), integer(), integer()}]} | {error, string()}).
identical_expression_search(FileName, Start, End, TabWidth) ->
    try_refactoring(refac_expr_search, expr_search, [FileName, Start, End, TabWidth]).



%% ==================================================================================================
%% @doc Search for expression/expression sequences in the current buffer that are similar to the expression/expression sequence selected by the user.
%% <p> This functionality allows searching for expression sequence that are <em>similar</em> to the expression sequence selected.  In this context, 
%% two expressions, A and B say, are similar if there exists an anti-unifier, say C,  of A and B, and C satisfies the similarity score specified
%% by the user (the calculation calculated of similarity score is going to be further explored).
%% </p>
%% Usage: highlight the expression sequence of interest, then selected  <em> Similar Expression Search </em> from  Refactor.
%% @spec similar_expression_search(FileName::filename(), Start::pos(), End::pos(), SimiScore::string(), SearchPaths::[dir()], TabWidth::integer())
%%    -> {ok, [{integer(), integer(), integer(), integer()}]} | {error, string()}
-spec(similar_expression_search/6::(filename(), pos(), pos(), string(), [dir()], integer()) -> {ok, [{integer(), integer(), integer(), integer()}]} | {error, string()}).
similar_expression_search(FileName, Start, End, SimiScore, SearchPaths, TabWidth) ->
    try_refactoring(refac_sim_expr_search, sim_expr_search, [FileName, Start, End, SimiScore, SearchPaths, TabWidth]).

%% =====================================================================================================
%%@doc Introduce a new function to represent an expression or expression sequence.
%% <p> This refactoring allows the user to introduce a new function to represent a selected expression or expression 
%% sequence, and replace the selected code with a call to the new function.  Free variables
%% within the selected code become the formal parameters of the function definition.
%% </p>
%% <p>
%% Usage: highlight the expression/expression sequence of interest, then selected the <em>Function Extraction</em> 
%% from  <em>Refactor</em>, Wrangler will then prompt for the new function name.
%% </p>
%% @spec fun_extraction(FileName::filename(), Start::Pos, End::Pos, FunName::string(), TabWidth:: integer()) ->{error, string()} | {ok, string()}
-spec(fun_extraction/5::(filename(), pos(), pos(), string(), integer()) ->
	      {error, string()} | {ok, string()}).
fun_extraction(FileName, Start, End, FunName, TabWidth) -> 
    try_refactoring(refac_new_fun, fun_extraction, [FileName, Start, End, FunName, TabWidth]).

%%@private
-spec(fun_extraction_eclipse/5::(filename(), pos(), pos(), string(), integer()) ->
	      {error, string()} | {ok, [{filename(), filename(), string()}]}).
fun_extraction_eclipse(FileName, Start, End, FunName, TabWidth) -> 
    try_refactoring(refac_new_fun, fun_extraction_eclipse, [FileName, Start, End, FunName, TabWidth]).

%% =====================================================================================================
%%@doc Unfold a function application to an instance of the function's body.
%% <p> This refactoring replaces a function application with an instance of the function body.
%% With the current implementation, Wrangler unfolds a function application only if the function 
%% is defined in the same module, and Wrangler could work out which function clause to use (in case 
%% the function definition contains multiple function clauses).
%% </p>
%% <p>
%% Usage: Point the cursor to the function name in the function application to unfold, then 
%% select <em>Unfold Function Application</em> from <em>Refactor</em>.
%% </p>
%%-spec(unfold_fun_app/4::(FileName::filename(), Pos::pos(), SearchPaths::[dir()], TabWidth::integer)
%%      ->{error, string()} |{'ok', [string()]}).
unfold_fun_app(FileName, Pos, SearchPaths, TabWidth) ->
    try_refactoring(refac_unfold_fun_app, unfold_fun_app, [FileName, Pos, SearchPaths, TabWidth]).


%%@private
-spec(unfold_fun_app_eclipse/4::(FileName::filename(), Pos::pos(), SearchPaths::[dir()], TabWidth::integer)
      ->{error, string()} | {ok, [{filename(), filename(), string()}]}).
unfold_fun_app_eclipse(FileName, Pos, SearchPaths, TabWidth) ->
    try_refactoring(refac_unfold_fun_app, unfold_fun_app_eclipse, [FileName, Pos, SearchPaths, TabWidth]).

%% =============================================================================================
%% @doc Fold expressions against a function definition.
%% <p>
%% This refactoring replaces instances of the right-hand side of a function clause definition by
%% the corresponding left-hand side with necessary parameter substitutions. The function clause can 
%% be defined in either the current module or another module.
%% </p>
%% <p> In the case that a candidate expression/expression sequence  needs to export some variables which 
%% are used by the code following code, that expression/expression sequence will be replaced by a match 
%% expression, whose left-hand side it the exported variable(s), and right-hand side is the function
%% application.
%%</p>
%% <p> This refactoring does not support folding against function clauses with guard expressions, or 
%% function clauses with complex formal parameters, such as tuples, lists, or records. 
%% </p>
%% <p> Usage: first point the cursor to the function clause against which expressions will be 
%% folded if the function is defined in the current module, or leave the cursor anywhere if you would like to 
%% fold against a function clause defined in another module; then select <em> Fold Expression Against Function </em> 
%% from the <em> Refactor </em> menu; after that, Wrangler then asks to confirm that you want to fold against 
%% the function clause pointed to by the cursor, if you answer 'no', Wrangler will prompt to provide the 
%% module name, function name, arity of the function and the index of the function clause (starting from 1). After all these initial interaction, 
%% Wrangler will search 
%% the current module for expressions which are instances of the right-hand side of the selected function clause.
%% </p>
%% <p> If no candidate expression has been found, a message will be given, and the refactoring 
%% finishes; otherwise, Wrangler will go through the found candidate expressions one by one, and ask 
%% the user whether she/he wants to replace the expression with an application of selected function.
%% If the user answers 'yes' to one instance,  that instance will be replaced by function application,
%% otherwise it will remain unchanged.
%% </p>
%%@spec fold_expr({{FileName::filename(), Line::integer(), Col::integer(), SearchPaths::[dir()], TabWidth:: integer()}
%%                  |{FileName::filename(), ModName::modulename(), Arity::integer(), ClauseIndex::integer(), SearchPaths::[dir()], TabWidth:: integer()}}) ->
%%           {ok, [{{{integer(), integer()}, {integer(), integer()}}, syntaxTree()}]} | {error, string()}

%% This function is just for documentation purpose; and should not be called by any other functions.
fold_expr({FileName, Line, Col, SearchPaths, TabWidth}) -> 
    fold_expr_by_loc(FileName, Line, Col, SearchPaths, TabWidth);
fold_expr({FileName, ModName, FunName, Arity, ClauseIndex, SearchPaths, TabWidth}) ->
    fold_expr_by_name(FileName, ModName, FunName, Arity, ClauseIndex, SearchPaths, TabWidth).

%%@private
-spec(fold_expr_by_loc/5::
      (filename(), integer(), integer(), [dir()], integer()) -> {ok, [{integer(), integer(), integer(), integer(), syntaxTree(), {syntaxTree(), integer()}}]}
						 | {error, string()}).
fold_expr_by_loc(FileName, Line, Col, SearchPaths, TabWidth) ->
    try_refactoring(refac_fold_expression, fold_expr_by_loc, [FileName, Line, Col, SearchPaths, TabWidth]).

%%@private
-spec(fold_expr_by_loc_eclipse/5::(filename(), integer(), integer(), [dir()], integer()) -> 
	     {ok, {syntaxTree(),[{{{integer(), integer()}, {integer(), integer()}}, syntaxTree()}]}}| {error, string()}).
fold_expr_by_loc_eclipse(FileName, Line, Col, SearchPaths, TabWidth) ->
    try_refactoring(refac_fold_expression, fold_expr_by_loc_eclipse, [FileName, Line, Col, SearchPaths, TabWidth]).

%%@private
-spec(fold_expr_by_name/7::(filename(), string(), string(), string(), string(), [dir()], integer()) ->
	     {ok, [{integer(), integer(), integer(), integer(), syntaxTree(), {syntaxTree(), integer()}}]}
		 | {error, string()}).
fold_expr_by_name(FileName, ModName, FunName, Arity, ClauseIndex, SearchPaths, TabWidth) ->
    try_refactoring(refac_fold_expression, fold_expr_by_name, [FileName, ModName, FunName, Arity, ClauseIndex, SearchPaths, TabWidth]).

%%@private
-spec(fold_expr_by_name_eclipse/7::(filename(), string(), string(), string(), string(), [dir()], integer()) ->
	     {ok, [{{{integer(), integer()}, {integer(), integer()}}, syntaxTree()}]} 
		 | {error, string()}).
fold_expr_by_name_eclipse(FileName, ModName, FunName, Arity, ClauseIndex, SearchPaths, TabWidth) ->
    try_refactoring(refac_fold_expression, fold_expr_by_name_eclipse, [FileName, ModName, FunName, Arity, ClauseIndex, SearchPaths, TabWidth]).

%%@private
-spec(fold_expression_1_eclipse/5::(filename(), syntaxTree(), [{{{integer(), integer()}, {integer(), integer()}}, syntaxTree}],[dir()], integer()) ->
	     {ok, [{filename(), filename(), string()}]}).
fold_expression_1_eclipse(FileName, FunClauseDef, StartEndExpList, SearchPaths, TabWidth)->  %% StartEndExpList: {{{StartLine, StartCol}, {EndLine, EndCol}}, NewExp}
    try_refactoring(refac_fold_expression, fold_expression_1_eclipse, [FileName, FunClauseDef, StartEndExpList, SearchPaths, TabWidth]).

%%@private
-spec(fold_expression_2_eclipse/7::(filename(), atom(),integer(), integer(), integer(), [dir()],integer()) -> 
	     {ok, [{integer(), integer(), integer(), integer(), syntaxTree(), {syntaxTree(), integer()}}]}
             | {error, string()}).
fold_expression_2_eclipse(FileName, FunName, Arity, ClauseIndex, StartLine, SearchPaths, TabWidth) ->
    try_refactoring(refac_fold_expression, fold_expression_2_eclipse, [FileName, FunName, Arity, ClauseIndex, StartLine, SearchPaths, TabWidth]).


%%@private
%%@spec instrument_prog(FileName::filename(), SearchPaths::[dir()], TabWidth:: integer()) -> term()
-spec(instrument_prog/3::(filename(), [dir()], integer()) ->{ok, [filename()]} | {error, string()}).  
instrument_prog(FileName, SearchPaths, TabWidth) ->
     try_refactoring(refac_instrument, instrument_prog, [FileName, SearchPaths,TabWidth]).


%%@private
%%@spec uninstrument_prog(FileName::filename(), SearchPaths::[dir()], TabWidth:: integer()) -> term()
-spec(uninstrument_prog/3::(filename(), [dir()], integer()) ->{ok, [filename()]} | {error, string()}).
uninstrument_prog(FileName, SearchPaths, TabWidth) ->
     try_refactoring(refac_instrument, uninstrument_prog, [FileName, SearchPaths, TabWidth]).


%%=========================================================================================
%% @doc Add a tag to all the messages received by a server process (Beta).
%% <p> This refactoring should be initiated from the main receive function of a server process.
%% The current implementation is still in an experimental stage, and has a number of limitations:
%% <li> The current implementation assumes that the process does not send enquiries, 
%%      using the <code> send ... receive </code> pattern, to other processes 
%% </li>
%% <li> The current implementation only handles processes spawned using <code>spawn</code> 
%%      or <code>spawn_link</code>
%% </li>
%% </p>
%%<p>
%% Usage: to apply this refactoring, point the cursor to the function name, then select <em> Add a 
%% Tag to Messages </em> from the <em> Refactor </em> menu, Wrangler will then prompt to 
%% enter the tag.
%%</p>
%%@spec add_a_tag(Filename::filename(), Line::integer(), Col::integer(), Tag::string(), SearchPaths::[dir()], TabWidth:: integer()) -> 
%%   {error, string()} | {ok, [filename()]}
-spec(add_a_tag/6::(filename(), integer(), integer(), string(), [dir()], integer()) ->
	     {error, string()} | {ok, [filename()]}).
add_a_tag(FileName, Line, Col, Tag, SearchPaths, TabWidth) ->
    try_refactoring(refac_add_a_tag, add_a_tag, [FileName, Line, Col, Tag, SearchPaths, TabWidth]).



%%=========================================================================================
%% @doc Register a process (Beta).
%% <p>This refactoring registers a process id, <code>Pid</code> say, with a name, regname say, and replaces
%% the uses of <code>Pid ! Msg </code> with  <code>regname ! Msg</code> whenever it is possible. 
%% </p>
%% <p>
%% The following side-conditions apply to this refactoring:
%% <li> The process name provided by the user should be lexically valid.</li>
%% <li>  The name provided by the user should not have been used as a process name. </li>
%% <li> The process under consideration should not have been registered. </li>
%% <li> Only one process spawned by the spawn expression selected should exist anytime during the running of the system. </li>
%% </p>
%% <p>
%% Usage: First select a match expression whose left-hand side is a process identifier, and right-hand side is a spawn expression,
%% then select <em> Register a Process</em> command from the <em>Refactor</em> menu, after that, Wrangler will prompt for the 
%% process name.
%% </p>
%%@spec register_pid(Filename::filename(), Start::pos(), End::pos(), RegName::string(), SearchPaths::[dir()], TabWidth:: integer()) 
%%    ->{error, string()} | {ok, [filename()]}
-spec(register_pid/6::(filename(), pos(), pos(), string(), [dir()], integer()) ->
    {error, string()}|{ok, [filename()]}).
register_pid(FileName, Start, End, RegName, SearchPaths, TabWidth) ->
    try_refactoring(refac_register_pid, register_pid, [FileName, Start, End, RegName, SearchPaths, TabWidth]).
    
%%=========================================================================================
%% @doc Group a consecutive sequence of parameters of a function into a tuple.
%% <p>
%% When the function under consideration is exported by the module where it is defined, this refactoring has a global effect. 
%% </p>
%% <p> Suppose the new function after refactoring is <code>f/n</code>, then the following <em> side-conditions </em> apply:
%% <li> <code> f/n</code> should not cause confliction with any of the functions which are in scope in the 
%% current module;</li>
%% <li> In the case that the function is imported by another module, then <code>f/n</code> 
%% should not be already in scope (either defined or imported) in that module. </li>
%% </p>
%% <p> Usage: to apply this refactoring, highlight the arguments to be grouped into a tuple from 
%% the function definition, then select <em> Tuple Function Arguments </em> from <em> Refactor</em>.
%% </p>
%% @spec tuple_funpar(FileName::filename(), StartLoc::pos(), EndLoc::pos(), SearchPaths::[dir()], TabWidth:: integer())
%% -> {error, string()} | {ok, [filename()]}
-spec(tuple_funpar/5::(filename(), pos(), pos, [dir()], integer()) ->
	     {error, string()} | {ok, [filename()]}).
tuple_funpar(FileName, StartLoc, EndLoc,SearchPaths, TabWidth) ->
    try_refactoring(refac_tuple, tuple_funpar, [FileName, StartLoc, EndLoc, SearchPaths, TabWidth]).


%%@private
tuple_funpar_1(FileName, StartLoc, EndLoc,SearchPaths, TabWidth) ->
    try_refactoring(refac_tuple, tuple_funpar_1, [FileName, StartLoc, EndLoc, SearchPaths, TabWidth]).


%%@private
-spec(tuple_funpar_eclipse/5::(filename(), pos(), pos, [dir()], integer()) ->
     {error, string()} | {ok, [{filename(), filename(), string()}]}).
tuple_funpar_eclipse(FileName, StartLoc, EndLoc, SearchPaths, TabWidth) ->
    try_refactoring(refac_tuple, tuple_funpar_eclipse, [FileName, StartLoc, EndLoc, SearchPaths, TabWidth]).

%%@private
tuple_funpar_eclipse_1(FileName, StartLoc, EndLoc, SearchPaths, TabWidth) ->
    try_refactoring(refac_tuple, tuple_funpar_eclipse_1, [FileName, StartLoc, EndLoc, SearchPaths, TabWidth]).



%%=========================================================================================
%%@private
%% @doc From tuple to record representation.
%%  NOTE: this refactoring is still in an experimental stage.
%% <p>
%% This refactoring has a global effect, i.e., it affects all those modules in 
%% which this function is imported/used.
%% </p>
%% <p> The following <em> side-conditions </em> apply to this refactoring:
%% <li> The record and field names must be lexically legal; </li>
%% <li> The number of record fields must equal to the selected tuple size; </li>
%% <li> The function must be defined in the current module; </li>
%% <li> The selected part must be a tuple.  </li>
%% </p>
%% <p> To apply this refactoring, highlight the tuple, which should be a function parameter,  
%% then select <em> From Tuple To Record </em> from <em> Refactor</em>, 
%% Wrangler will then prompt to enter the record name and  the record field names.
%% </p>
%% @spec tuple_to_record(File::filename(),FLine::integer(),FCol::integer(),
%%           LLine::integer(),LCol::integer(), RecName::string(),
%%           FieldString::[string()], SearchPaths::[dir()], TabWidth:: integer()) ->  {error, string()} | {ok, [filename()]} 
%% @end
-spec(tuple_to_record/9::(filename(), integer(), integer(), integer(), integer(), string(), [string()], [dir()], integer()) ->
	     {error, string()} | {ok, [filename()]}).
tuple_to_record(File, FLine, FCol, LLine, LCol, RecName, FieldString, SearchPaths, TabWidth) ->
    try_refactoring(refac_tuple_to_record, tuple_to_record, [File, FLine, FCol, LLine, LCol, RecName,
					  FieldString, SearchPaths, TabWidth]).

%%@private
%% @spec tuple_to_record_eclipse(File::filename(),FLine::integer(),FCol::integer(),
%%           LLine::integer(),LCol::integer(), RecName::string(),
%%           FieldString::[string()], SearchPaths::[dir()], TabWidth:: integer()) -> term()
-spec(tuple_to_record_eclipse/9::(filename(), integer(), integer(), integer(), integer(), string(), [string()], [dir()], integer()) ->
	     {error, string()} | {ok, [{filename(), filename(), string()}]}).
tuple_to_record_eclipse(File, FLine, FCol, LLine, LCol, RecName, FieldString,SearchPaths, TabWidth) ->
    try_refactoring(refac_tuple_to_record,tuple_to_record_eclipse, [File, FLine, FCol, LLine, LCol,
								    RecName, FieldString, SearchPaths, TabWidth]).


%%=========================================================================================
%% @doc Turn a function into a server process.
%%<p>
%% This refactoring turns a function into a server process. 
%% Turning a function into a server process provides potential for memorisation of calculated values, adding states to the process, etc.
%% </p>
%% <p> The following example shows the application of this refactoring to the function <code>f/2</code> on the 
%% left-hand side, and the result is shown on the right-hand side. 
%%    ``` f(add, X, Y) ->  X + Y;                     f(add, X, Y) -> f_rpc(f, {add, X, Y});
%%        f(sub, X, Y) ->  X - Y.                     f(sub, X, Y) -> f_rpc(f, {sub, X, Y}).
%%
%%        g(X, Y) ->                                  f() ->
%%             f(add, X, Y)*f(sub, X, Y).		   receive
%%                                                          {From, {add, X, Y}} -> From ! {f, X + Y}, f();
%%                                                          {From, {sub, X, Y}} -> From ! {f, X - Y}, f()
%%                                                         end.
%%
%%                                                    f_rpc(RegName, Request) ->
%%                                                         Sender = self(),
%%                                                         Fun = fun () ->
%% 		                                                    try
%% 		                                                       register(RegName, self())
%% 		                                                    of
%% 		                                                      true -> Sender ! {started, self()}, f()
%% 		                                                    catch
%% 		                                                      error:_ ->
%% 			                                                   Sender ! {already_running, self()}, 
%%                                                                         already_running
%% 		                                                    end
%% 	                                                         end,
%%                                                         Pid = spawn(Fun),
%%                                                         receive {_, Pid} -> ok end,
%%                                                         RegName ! {self(), Request},
%%                                                         receive {RegName, Response} -> Response end.
%%
%%                                                    g(X, Y) ->
%%                                                         f(add, X, Y) *f(sub, X, Y).
%%    '''
%% </p>
%% The following side-conditions apply to this refactoring:
%% <p>
%% <li> The process name provided by the user should be lexically legal, and not conflict with existing process names. </li>   
%% <li> The function should not be a recursive function, either directly or indirectly. </li>
%% <li> The current function or functions called by this function should not register the <code>Pid</code> returned by <code>self()</code>. </li>
%% </p>
%% <p>
%% Wrangler generates the new function name and the rpc function name automatically, but the user could always rename it afterwards.
%% Suppose the original function is <code>f/n</code>, then the new function name would be <code>f/0</code> and the <code>rpc</code> 
%% function name would be <code>f_rpc/2</code>; if any conflicts occur, <code>'_i'</code> will be attached to the end of the function
%% name where <code>i</code> is a smallest number that makes the name fresh.
%% </p>
%% <p> Usage: To apply this refactoring, point the cursor to the function of interest, then select 
%% <em> From Function to Process </em> from <em> Refactor</em>, after that Wrangler will prompt 
%%  to enter the process registration name  in the mini-buffer.
%% </p>
%% @spec fun_to_process(FileName::filename(), Line::integer(), Col::integer(), ProcessName::string(), SearchPaths::[dir()], TabWidth:: integer()) ->
%%  {ok, [filename()]} | {error, string()}
-spec(fun_to_process/6::(filename(), integer(), integer(), string(), [dir()], integer()) -> {ok, [filename()]} |  {undecidables, string()} | {error, string()}).
fun_to_process(FileName, Line, Col, ProcessName, SearchPaths, TabWidth) ->
    try_refactoring(refac_fun_to_process, fun_to_process, [FileName, Line, Col, ProcessName, SearchPaths, TabWidth]).

%%@private
-spec(fun_to_process_eclipse/6::(filename(), integer(), integer(), string(), [dir()], integer()) -> 
	     {ok, [{filename(), filename(), string()}]} | 
		 {undecidables, string()} | {error, string()}).
fun_to_process_eclipse(FName, Line, Col, ProcessName, SearchPaths, TabWidth) ->
    try_refactoring(refac_fun_to_process, fun_to_process_eclipse, [FName, Line, Col, ProcessName, SearchPaths, TabWidth]).
    
%%@private
-spec(fun_to_process_1_eclipse/6::(filename(), integer(), integer(), string(), [dir()], integer()) -> {ok, [{filename(), filename(), string()}]}).
fun_to_process_1_eclipse(FName, Line, Col, ProcessName, SearchPaths, TabWidth) ->
    try_refactoring(refac_fun_to_process, fun_to_process_1_eclipse, [FName, Line, Col, ProcessName, SearchPaths, TabWidth]).
    
%%=========================================================================================
%% @doc Rename a registered process (Beta).
%% <p> This refactoring has a global effect, i.e. it needs to check the whole program for places where the 
%% original process name is used.
%% </p>
%% <p> The following <em> side-conditions </em> apply to this refactoring:
%% <li> The new process name should not be the atom 'undefined' </li>
%% <li> The new process name should not have been used as a process name in the program under consideration.  </li>
%% <li> Since there are some cases where Wrangler cannot infer whether an atom represents a process name or not, for example, 
%% a process name in a message, it would be help the refactoring process to select the process name from expressions, such as 
%% registration expressions, where it is easier for Wrangler to infer that the atom is a process name.</li>
%% </p>
%% <p> Usage: To apply this refactoring, point the cursor to the process name, then select
%% <em> Rename a Process </em> from the  <em> Refactor </em> menu, after that, Wrangler will prompt
%% to enter the new process name in the mini-buffer. 
%% </p>
%% @spec rename_process(FileName::filename(), Line::integer(), Col::integer(),NewName::string(), SearchPaths::[dir()], TabWidth:: integer()) ->
%%     {error, string()} | {undecidables, string()}| {ok, [filename()]}
-spec(rename_process/6::(filename(), integer(), integer(), string(), [dir()], integer()) ->
	       {error, string()} | {undecidables, string()} | {ok, [filename()]}).
rename_process(FileName, Line, Col, NewName, SearchPaths, TabWidth) ->
    try_refactoring(refac_rename_process, rename_process, [FileName, Line, Col, NewName, SearchPaths, TabWidth]).


%%@private
-spec(rename_process_eclipse/6::(filename(), integer(), integer(), string(), [dir()], integer()) ->
	       {error, string()} | {undecidables, string()} |  {ok, [{filename(), filename(), string()}]}).
rename_process_eclipse(FileName, Line, Col, NewName, SearchPaths, TabWidth) ->
    try_refactoring(refac_rename_process, rename_process_eclipse, [FileName, Line, Col, NewName, SearchPaths, TabWidth]).

%%@private
-spec(rename_process_1_eclipse/5::(string(), string(), string(), [dir()], integer()) -> {ok, [{filename(), filename(), string()}]}).
rename_process_1_eclipse(FileName, OldProcessName, NewProcessName, SearchPaths, TabWidth) ->
    try_refactoring(refac_rename_process, rename_process_1_eclipse, [FileName, OldProcessName, NewProcessName, SearchPaths, TabWidth]).

%% =====================================================================================================
%% @doc Introduce a macro to represent a syntactically well-formed expression/pattern or a sequence of expressions/patterns.
%% <p> This refactoring allows the user to define a new macro to represent a expression/pattern or sequence 
%% of expressions/patterns selected by the user, and replace the selected code with an application of the macro.
%% Free variables within the selected code become the formal parameters of the macro definition.
%% </p>
%% <p>
%% Usage: Highlight the expression of interest, then selected the <em>Introduce a Macro</em> 
%% from <em>Refactor</em>, Wrangler will then prompt for the new macro name.
%% </p> 
%% @spec new_macro(FileName::filename(),Start::pos(), End::pos(), NewMacroName::string(),SearchPaths::[dir()], TabWidth:: integer()) ->{error, string()} | {ok, string()}
-spec(new_macro/6::(filename(), pos(), pos(), string(), [dir()], integer()) ->
	      {error, string()} | {ok, string()}).
new_macro(FileName, Start, End, MacroName, SearchPaths, TabWidth) -> 
    try_refactoring(refac_new_macro, new_macro, [FileName, Start, End, MacroName, SearchPaths, TabWidth]).


%%@private
-spec(new_macro_eclipse/6::(filename(), pos(), pos(), string(), [dir()], integer()) ->
	     {error, string()} | {ok, [{filename(), filename(), string()}]}).
new_macro_eclipse(FileName, Start, End, NewMacroName, SearchPaths, TabWidth) ->
    try_refactoring(refac_new_macro, new_macro_eclipse, [FileName, Start, End, NewMacroName, SearchPaths, TabWidth]).

%% =============================================================================================
%% @doc Fold expressions/patterns against a macro definition.
%% <p>
%% This refactoring replaces instances of the right-hand side of a macro definition by the corresponding
%% left-hand side with necessary parameter substitutions. 
%% </p>
%% <p> Usage: to apply this refactoring, first point the cursor to the macro definition against which candidate 
%%  expressions/candidates will be folded; then select <em> Fold Against Macro Definition </em> from the 
%% <em> Refactor </em> menu; after that, Wrangler will search the current module for expressions/patterns
%% which are instances of the right-hand side of the selected macro definition; and direct you through the 
%% refactoring process.
%% </p>
%%@spec fold_against_macro(FileName::filename(), Line::integer(), Col::integer(), SearchPaths::[dir()], TabWidth::integer()) ->
%%	      {error, string()} | {ok, [{{{integer(), integer()}, {integer(), integer()}}, syntaxTree()}]}
-spec(fold_against_macro/5::(filename(), integer(), integer(), [dir()], integer()) ->
	      {error, string()} | {ok, [{{{integer(), integer()}, {integer(), integer()}}, syntaxTree()}]}).
fold_against_macro(FileName, Line, Col, SearchPaths, TabWidth) ->
    try_refactoring(refac_fold_against_macro, fold_against_macro, [FileName, Line, Col, SearchPaths, TabWidth]).


%% =============================================================================================
%% @doc Reorder the record fields in a record expression to be consistent with the record definition.
%%<p>
%% Usage: point cursor to the record expression interested, then select <em>Normalise Record Expression</em> from <em>Refactor</em>.
%%</p>
%% @spec normalise_record_expr(FileName::filename(), Line::integer(), Col::integer(), ShowDefault::bool(), SearchPaths::[dir()], TabWidth::integer())
%%         -> {error, string()} | {ok, [filename()]}
-spec(normalise_record_expr/6::(filename(), integer(), integer(), bool(), [dir()], integer()) -> {error, string()} | {ok, [filename()]}).
normalise_record_expr(FileName, Line, Col, ShowDefault, SearchPaths, TabWidth) ->
    try_refactoring(refac_sim_expr_search, normalise_record_expr, [FileName, {Line, Col}, ShowDefault,SearchPaths, TabWidth]).


%%@private
try_to_apply(Mod, Fun, Args, Msg) -> 
    try apply(Mod, Fun, Args)
    catch
 	throw:Error -> 
  	    Error;
	  _E1:_E2->
	    {error, Msg}
    end.

%%@private
try_refactoring(Mod, Fun, Args) ->
    Msg = "Wrangler failed to perform this refactoring, "
	"please report error to erlang-refactor@kent.ac.uk.",
    try_to_apply(Mod, Fun, Args, Msg).

%%@private
try_inspector(Mod, Fun, Args) -> 
    Msg ="Wrangler failed to perform this functionality, "
	"please report error to erlang-refactor@kent.ac.uk.",
    try_to_apply(Mod, Fun, Args, Msg).

