%% Copyright (c) 2011, Huiqing Li, Simon Thompson 
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

%% @copyright 2006-2011 Huiqing Li, Simon Thompson
%%
%% @author Huiqing Li, Simon Thompson
%%   [http://www.cs.kent.ac.uk/projects/wrangler]

%% @end
%%
%% @doc This module describes the refactorings that are currently supported by Wrangler.
%%     The refactoring functions listed in this module are NOT supposed to be run in an 
%%     Erlang shell. Interface for refactorings that can be run in an Erlang shell are 
%%     documented in module <a href="wrangler_api.html">wrangler_api</a>. 
-module(wrangler_refacs).

-export([rename_var/7, 
         rename_fun/6,
         rename_fun/7, 
         rename_fun_1/7,
         rename_mod/5,
	 rename_process/7, 
         generalise/7, 
         gen_fun_1/12, 
         gen_fun_clause/11,
	 move_fun/7, 
         move_fun_1/8,
	 duplicated_code_in_buffer/5,
	 duplicated_code_in_dirs/5,
	 similar_expression_search_in_buffer/6, 
         similar_expression_search_in_dirs/6,
	 fun_extraction/6, 
         fun_extraction_1/8,
	 fold_expr/1, 
         fold_expr_by_loc/6, 
         fold_expr_by_name/8,
	 similar_code_detection_in_buffer/6,
	 similar_code_detection/6, 
	 add_a_tag/7, 
         tuple_funpar/6,
         tuple_funpar_1/6,
         tuple_args/7,
         swap_args/7,
       	 register_pid/7, 
         fun_to_process/7,
	 new_macro/7,
         fold_against_macro/6,
	 normalise_record_expr/7,
         unfold_fun_app/5,
	 new_let/7, 
         new_let_1/7, 
         merge_let/4, 
         merge_let_1/6,
	 merge_forall/4, 
         merge_forall_1/6,
	 eqc_statem_to_record/4, 
         eqc_statem_to_record_1/8,
 	 eqc_fsm_to_record/4,
         eqc_fsm_to_record_1/8, 
	 gen_fsm_to_record/4, 
         gen_fsm_to_record_1/8,
	 eqc_statem_to_fsm/5,
	 partition_exports/5,
	 intro_new_var/7,
	 inline_var/6,
         inline_var_1/8, 
         add_to_export/5]).

-export([rename_var_eclipse/6, rename_fun_eclipse/6,
	 rename_fun_1_eclipse/6, rename_mod_eclipse/4,
	 rename_mod_1_eclipse/5, generalise_eclipse/6,
	 move_fun_eclipse/6, move_fun_1_eclipse/6,
	 fun_extraction_eclipse/5, fun_extraction_1_eclipse/5,
	 gen_fun_1_eclipse/11, gen_fun_clause_eclipse/10,
	 tuple_funpar_eclipse/5, tuple_funpar_eclipse_1/5,
	 fold_expr_by_loc_eclipse/5,
	 fold_expr_by_name_eclipse/7, fold_expr_1_eclipse/5,
	 new_macro_eclipse/6, 
         rename_process_eclipse/6, 
	 fold_against_macro_eclipse/5, fold_against_macro_1_eclipse/5,
	 rename_process_1_eclipse/5, fun_to_process_eclipse/6,
	 fun_to_process_1_eclipse/6, unfold_fun_app_eclipse/4,
	 duplicated_code_eclipse/5, sim_code_detection_eclipse/8,
	 expr_search_eclipse/4,
	 simi_expr_search_in_buffer_eclipse/6, 
	 simi_expr_search_in_dirs_eclipse/6,
	 normalise_record_expr_eclipse/5,
	 eqc_statem_to_fsm_eclipse/4, 
	 new_let_eclipse/6, new_let_1_eclipse/6,
	 merge_forall_eclipse/3, merge_forall_1_eclipse/4, 
         merge_let_eclipse/3, merge_let_1_eclipse/4,
	 eqc_statem_to_record_eclipse/3,eqc_statem_to_record_1_eclipse/7,
	 eqc_fsm_to_record_eclipse/3,eqc_fsm_to_record_1_eclipse/7,
	 gen_fsm_to_record_eclipse/3,gen_fsm_to_record_1_eclipse/7,
	 partition_exports_eclipse/4, intro_new_var_eclipse/6,
	 inline_var_eclipse/5, inline_var_eclipse_1/6,
         get_var_name_eclipse/5, get_fun_name_eclipse/5
	]).
 
-export([try_refac/3, get_log_msg/0]).

-export([init_eclipse/0]).

-include("../include/wrangler_internal.hrl").

-type(context():: emacs | composite_emacs).

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
-spec (rename_var/7::(filename(), integer(), integer(), string(), [dir()], context(), integer()) ->
			   {error, string()}|{ok, filename()}).
rename_var(FileName, Line, Col, NewName, SearchPaths, Context, TabWidth) ->
    try_refac(refac_rename_var, rename_var, [FileName, Line, Col, NewName, SearchPaths, Context, TabWidth]).
   

%% @private
-spec(rename_var_eclipse/6::(filename(), integer(), integer(), string(), [dir()], integer()) ->
	     {error, string()} | {ok, [{filename(), filename(), string()}]}).
rename_var_eclipse(FileName, Line, Col, NewName, SearchPaths, TabWidth) ->
    try_refac(refac_rename_var, rename_var_eclipse, [FileName, Line, Col, NewName, SearchPaths, TabWidth]).

%%-spec get_var_name_eclipse/5::(filename(), integer(), integer(), [dir()], integer()) -> string().
get_var_name_eclipse(FileName, Line, Col, SearchPaths, TabWidth) ->
    try_refac(refac_rename_var, get_var_name, [FileName, Line, Col, SearchPaths, TabWidth]).


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

rename_fun(ModOrFileName, {OldFunName, Arity}, NewFunName, SearchPaths, Editor, TabWidth) ->
    try_refac(refac_rename_fun, rename_fun_by_name, 
              [ModOrFileName, OldFunName, Arity, NewFunName, SearchPaths, Editor, TabWidth]).

-spec(rename_fun/7::(string(), integer(), integer(), string(), [dir()], context(), integer()) ->
	     {error, string()} |{warning, string()}| {ok, [filename()]}).
rename_fun(FileName, Line, Col, NewName, SearchPaths, Context, TabWidth) ->
    try_refac(refac_rename_fun, rename_fun, [FileName, Line, Col, NewName, SearchPaths, Context, TabWidth]).

%%@private
-spec(rename_fun_eclipse/6::(string(), integer(), integer(), string(), [dir()], integer()) ->
	     {error, string()} | {warning, string()} | {ok, [{filename(), filename(), string()}]}).
rename_fun_eclipse(FileName, Line, Col, NewName, SearchPaths, TabWidth) ->
    try_refac(refac_rename_fun, rename_fun_eclipse, [FileName, Line, Col, NewName, SearchPaths, TabWidth]).

%%@private
-spec(rename_fun_1/7::(string(), integer(), integer(), string(), [dir()], context(), integer()) ->
	     {error, string()} | {ok, [filename()]}).
rename_fun_1(FileName, Line, Col, NewName, SearchPaths, Context, TabWidth) ->
    try_refac(refac_rename_fun, rename_fun_1, [FileName, Line, Col, NewName, SearchPaths, Context, TabWidth]).
    

%%@private
-spec(rename_fun_1_eclipse/6::(string(), integer(), integer(), string(), [dir()], integer()) ->
	     {error, string()} | {ok, [filename()]}).
rename_fun_1_eclipse(FileName, Line, Col, NewName, SearchPaths, TabWidth) ->
    try_refac(refac_rename_fun, rename_fun_1_eclipse, [FileName, Line, Col, NewName, SearchPaths, TabWidth]).
    

%%-spec(get_fun_name_eclipse/5::(string(), integer(), integer(), [dir()], integer()) ->
%% string().
get_fun_name_eclipse(FileName, Line, Col, SearchPaths, TabWidth) ->
    try_refac(refac_rename_fun, get_fun_name, [FileName, Line, Col, SearchPaths, TabWidth]).


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

-spec(rename_mod/5::(filename(), string(), [dir()], context(), integer()) -> 
	     {error, string()} | {question, string()} | {warning, string()} |{ok, [filename()]}).
rename_mod(FileName, NewName, SearchPaths, Context, TabWidth) ->
    try_refac(refac_rename_mod, rename_mod, [FileName, NewName, SearchPaths, Context, TabWidth]).

%%@private
-spec(rename_mod_eclipse/4::(FileName::filename(), NewName::string(), SearchPaths::[dir()], TabWidth::integer()) ->
	     {error, string()} | {question, string()} | {warning, string()} |
		 {ok, [{filename(), filename(), string()}]}).
rename_mod_eclipse(FileName, NewName, SearchPaths, TabWidth) ->
    try_refac(refac_rename_mod, rename_mod_eclipse, [FileName, NewName, SearchPaths, TabWidth]).

%%@private
-spec(rename_mod_1_eclipse/5::(FileName::filename(), NewName::string(), SearchPaths::[dir()], TabWith::integer(), RenameTestMod::boolean())
      ->{ok, [{filename(), filename(), string()}]}).
rename_mod_1_eclipse(FileName, NewName, SearchPaths, TabWidth, RenameTestMod) ->
    try_refac(refac_rename_mod, rename_mod_1_eclipse, [FileName, NewName, SearchPaths, TabWidth, RenameTestMod]).

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
%% <em> Generalise Function Definitio5n </em> from the <em>Refactor</em> menu, after 
%% that the refactorer will prompt to enter the parameter name in the mini-buffer. </p>
%% 
%% NOTE: in Erlang some literal expressions can not be replaced by variables. For example, the atom <code>fields</code>
%% in the expression <code>record_info(fields, Record)</code> should not be replaced by a variable or other expressions.
%% This kind of checking is NOT supported by Wrangler yet.
-spec(generalise/7::(FileName::filename(),Start::[integer()], End::[integer()],ParName::string(),
		     SearchPaths::[dir()], context(), TabWidth::integer()) ->
	     {ok, [filename()]}
		 |{error, string()}
                |{multiple_instances, {atom(), atom(), integer(), pos(), syntaxTree(), boolean(),[{pos(), pos()}], string()}}
		 |{unknown_side_effect, {atom(), atom(),integer(), pos(), syntaxTree(), integer(),
					 [{pos(), pos()}], [{pos(),pos()}], string()}}
		 |{more_than_one_clause, {atom(), atom(), integer(), pos(), syntaxTree(), boolean(),
					  [{pos(), pos()}], [{pos(),pos()}], string()}}). 
generalise(FileName, _Start=[SLn, SCol], _End=[ELn, ECol], ParName, SearchPaths,  Context, TabWidth) ->
    try_refac(refac_gen, generalise, [FileName, {SLn, SCol}, {ELn, ECol}, ParName, SearchPaths, Context, TabWidth]).


%%@private
-spec(gen_fun_1/12::(SideEffect::boolean(), FileName::filename(),ParName::atom(), FunName::atom(),
		     Arity::integer(), FunDefPos::pos(), Exp::syntaxTree(), DupsInFun::[{pos(), pos()}],SearchPaths::[dir()],
		     TabWidth::integer(), Context::atom(),LogCmd::string())
     -> {ok, [filename()]} | {error, string()}).
gen_fun_1(SideEffect, FileName,ParName, FunName, Arity, DefPos, Exp, DupsInFun,SearchPaths, Context, TabWidth, LogCmd) ->
    try_refac(refac_gen, gen_fun_1, [SideEffect, FileName, ParName, FunName, Arity, DefPos, Exp,
                                     SearchPaths, TabWidth, DupsInFun, Context, LogCmd]).

%%@private
-spec(gen_fun_clause/11::(FileName::filename(), ParName::atom(), FunName::atom(), Arity::integer(), DefPos::pos(), 
			  Exp::syntaxTree(), TabWidth::integer(), SideEffect::boolean(), 
			  Dups::[{{integer(), integer()},{integer(), integer()}}], Context::atom(), LogCmd::string()) 
     ->{ok, [filename()]} | {error, string()}).

gen_fun_clause(FileName, ParName, FunName, Arity, DefPos, Exp, TabWidth, SideEffect, Dups, Context, LogCmd) ->
    try_refac(refac_gen, gen_fun_clause, [FileName, ParName, FunName, Arity, DefPos, Exp, TabWidth, SideEffect, Dups, Context, LogCmd]).
 

%%@private
-spec(generalise_eclipse/6::(FileName::filename(),Start::pos(), End::pos(),ParName::string(), 
			     SearchPaths::[dir()], TabWidth::integer()) -> 
	     {error, string()} |{ok, [{filename(), filename(), string()}]}
		 |{multiple_instances,  {ParName:: atom(), FunName::atom(), Arity::integer(),
					 FunDefPos::pos(), Exp::syntaxTree(), SideEffect::boolean(),
				 	 DupsInFun::[{pos(), pos()}], LogCmd::string()}}
		 |{unknown_side_effect, {ParName::atom(), FunName::atom(),Arity::integer(), 
				  	 FunDefPos::pos(), Exp::syntaxTree(), NoOfClauses::integer(),
					 DupsInFun::[{pos(), pos()}],DupsInClause::[{pos(), pos()}], LogCmd::string()}}
		 |{more_than_one_clause,{ParName::atom(), FunName::atom(), Arity::integer(), 
					 FunDefPos::pos(), Exp::syntaxTree(), SideEffect::boolean(),
					 DupsInFun::[{pos(), pos()}],DupsInClause::[{pos(), pos()}], Logcmd::string()}}).
generalise_eclipse(FileName, Start, End, ParName, SearchPaths, TabWidth) ->
    try_refac(refac_gen, generalise_eclipse, [FileName, Start, End, ParName, SearchPaths, TabWidth]).

%%@private
-spec(gen_fun_1_eclipse/11::(SideEffect::boolean(), FileName::filename(),ParName::atom(), FunName::atom(), 
			     Arity::integer(), FunDefPos::pos(), Expr::syntaxTree(),  Dups::[{pos(), pos()}], SearchPaths::[dir()],
			     TabWidth::integer(), LogCmd::string()) 
     -> {ok, [{filename(), filename(),string()}]} | {error, string()}).
gen_fun_1_eclipse(SideEffect, FileName, ParName, FunName, Arity, FunDefPos, Expr,Dups, SearchPaths, TabWidth, LogCmd) ->
    try_refac(refac_gen, gen_fun_1_eclipse, [SideEffect, FileName, ParName, FunName, Arity, FunDefPos, Expr, SearchPaths, TabWidth, Dups, LogCmd]).


%%@private
-spec(gen_fun_clause_eclipse/10::(FileName::filename(), ParName::atom(), FunName::atom(), Arity::integer(), DefPos::pos(), 
			 Exp::syntaxTree(), TabWidth::integer(), SideEffect::boolean(),  Dups::[{pos(), pos()}], LogCmd::string()) ->
	     {ok, [{filename(), filename(), string()}]} | {error, string()}).
gen_fun_clause_eclipse(FileName, ParName, FunName, Arity, DefPos, Exp, TabWidth, SideEffect, Dups, LogCmd) ->
    try_refac(refac_gen, gen_fun_clause_eclipse, [FileName, ParName, FunName, Arity, DefPos, Exp, TabWidth, SideEffect, Dups, LogCmd]).


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
-spec(move_fun/7::(filename(),integer(),integer(), string(), [dir()], context(), integer())
        -> {ok, [filename()]} | {question, string()} |{error, string()}).
move_fun(FileName, Line, Col, TargetModName,SearchPaths, Context, TabWidth) ->
    try_refac(refac_move_fun, move_fun, [FileName, Line, Col, TargetModName, SearchPaths, Context, TabWidth]).


%%@private
-spec(move_fun_1/8::(filename(),integer(),integer(), string(),  boolean(),[dir()], context(), integer())
      -> {ok, [filename()]} |{error, string()}).
move_fun_1(FileName, Line, Col, TargetModName, CheckCond, SearchPaths, Context, TabWidth) ->
    try_refac(refac_move_fun, move_fun_1, [FileName, Line, Col, TargetModName, CheckCond, SearchPaths, Context, TabWidth]).

%%@private
-spec(move_fun_eclipse/6::(filename(),integer(),integer(), string(), [dir()], integer())
        -> {ok, [{filename(), filename(), string()}]} | {question, string()} | {error, string()}).
move_fun_eclipse(FileName, Line, Col, TargetModName,SearchPaths, TabWidth) ->
    try_refac(refac_move_fun, move_fun_eclipse, [FileName, Line, Col, TargetModName, SearchPaths, TabWidth]).

%%@private
-spec(move_fun_1_eclipse/6::(filename(),integer(),integer(), string(), [dir()], integer())
        -> {ok, [{filename(), filename(), string()}]} |{error, string()}).
move_fun_1_eclipse(FileName, Line, Col, TargetModName,SearchPaths, TabWidth) ->
    try_refac(refac_move_fun, move_fun_1_eclipse, [FileName, Line, Col, TargetModName, SearchPaths, TabWidth]).


%% ==================================================================================
%% @private
%% @doc An identical code detector that searches for identical code within the current Erlang buffer.
%% <p> This function reports duplicated code fragments found in the current Erlang buffer, it does 
%% not remove those code clones though. The user will be prompted for two parameters: the minimum number of 
%% tokens a cloned code fragment should have, and the minimum number of times a code fragment appears in the program.
%% </p>
%% <p> The current version of the identical code detector reports clones that are syntactically 
%% identical after consistent renaming of variables, except for variations in literals, layout and comments.
%% </p>
%% <p>
%% Usage: simply select <em> Detect Identical Code in Current Buffer </em> from <em> Identical Code Detection</em>, 
%% Wrangler will prompt to input the parameters.
%% </p>
-spec(duplicated_code_in_buffer/5::(filename(), string(), string(), string(), integer()) ->{ok, string()}).     
duplicated_code_in_buffer(FileName, MinToks, MinClones, MaxPars, TabWidth) -> 
    try_refac(refac_duplicated_code, duplicated_code, [[FileName], MinToks, MinClones, MaxPars, TabWidth]).

%%@private
-spec(duplicated_code_eclipse/5::(DirFileList::dir(), MinLength::integer(), MinClones::integer(), 
				  TabWidth::integer(),  SuffxiTreeExec::dir()) ->
 	     [{[{{filename(), integer(), integer()},{filename(), integer(), integer()}}], integer(), integer(), string()}]).
duplicated_code_eclipse(DirFileList, MinLength, MinClones, TabWidth, SuffixTreeExec) ->
    try_refac(refac_duplicated_code, duplicated_code_eclipse, [DirFileList, MinLength, MinClones, TabWidth, SuffixTreeExec]).

%%@private
-spec(sim_code_detection_eclipse/8::(DirFileList::[filename()|dir()], MinLen::integer(), MinToks::integer(),
					 MinFreq::integer(),  MaxVars:: integer(),SimiScore::float(), 
					 SearchPaths::[dir()], TabWidth::integer()) ->
					      [{[{{{File::filename(), StartLine::integer(), StartCol::integer()},
						   {File::filename(), StartLine::integer(), StartCol::integer()}}, FunCall::string()}], 
						Len::integer(), Freq::integer(), AntiUnifier::string()}]).
sim_code_detection_eclipse(DirFileList, MinLen,MinToks,MinFreq,MaxNewVars,SimiScore,SearchPaths,TabWidth) ->
    try_refac(refac_inc_sim_code, inc_sim_code_detection_eclipse,
	      [DirFileList, MinLen, MinToks, MinFreq, MaxNewVars, SimiScore, SearchPaths, TabWidth]).


%% =====================================================================================
%%@private
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
%% <em> Identical Clone Detection</em>, Wrangler will then prompt to input the parameters.
%% </p>
-spec(duplicated_code_in_dirs/5::([dir()], string(), string(), string(),  integer()) ->{ok, string()}).
duplicated_code_in_dirs(FileDirList, MinToks, MinClones, MaxPars, TabWidth) ->
    try_refac(refac_duplicated_code, duplicated_code, [FileDirList, MinToks, MinClones, MaxPars, TabWidth]).
    


%% ==================================================================================================
%% @doc A similar code detector that searches for similar code in the current Erlang buffer.
%% <p> This function reports similar expression sequences found in the current Erlang buffer, but does not 
%% remove those clones though. The user needs to provide three parameters to be used by the clone detector, and 
%% they are: the minimum length of an expression sequence, the minimum number of duplication times, and 
%% a similarity score which should be between 0.1 and 1.0.
%% </p>
%% <p>  
%% Usage: select <em> Detect Similar Code in Buffer </em> from <em> Similar Code Detection</em>, Wrangler will then prompt to 
%% input the parameters needed.
%% </p>   

-spec(similar_code_detection_in_buffer/6::(FileName::filename(), MinLen::string(), MinFreq::string(), MinScore::string(), 
					   SearchPaths::[dir()], TabWidth::integer()) ->  {ok, string()}).
similar_code_detection_in_buffer(FileName, MinLen, MinFreq, SimiScore, SearchPaths, TabWidth) ->
    try_refac(refac_incsim_code, sim_code_detection, [[FileName], MinLen, MinFreq, SimiScore, SearchPaths, TabWidth]).
       
 
%% ==================================================================================================
%%@doc A similar code detector that searches for similar code across multiple Erlang modules.
%% <p> This function reports similar expression sequences found in the directories specified, but does not 
%% remove those clones though. The algorithm is based on the notion of anti-unification, or the least common generalisation.
%% The user needs to provide three parameters to be used by the clone detector, and 
%% they are: the minimum length of an expression sequence, the minimum number of duplication times, and 
%% a similarity score which should be between 0.1 and 1.0.
%% </p>
%% <p> 
%% Usage: select <em> Detect Similar Code in Dirs </em> from <em> Similar Code Detection</em>, Wrangler will then prompt to 
%% input the parameters needed.
%% </p>

-spec(similar_code_detection/6::(DirFileList::[filename()|dir()], MinLen::string(), MinFreq::string(), MinScore::string(), 
				 SearchPaths::[dir()], TabWidth::integer()) -> {ok, string()}).
similar_code_detection(DirFileList, MinLen, MinFreq, SimiScore1, SearchPaths, TabWidth) ->
    refac_sim_code:sim_code_detection(DirFileList, MinLen, MinFreq, SimiScore1, SearchPaths, TabWidth) .
  
%%@private
-spec(expr_search_eclipse/4::(filename(), pos(), pos(), integer()) ->
				   {ok, [{{integer(), integer()}, {integer(), integer()}}]} | {error, string()}).
expr_search_eclipse(FileName, Start, End, TabWidth) ->
    try_refac(refac_expr_search, expr_search_eclipse, [FileName, Start, End, TabWidth]).

%% ==================================================================================================
%% @doc Search expression search in the current Erlang buffer.
%% <p> This functionality allows searching for expression sequence that are <em>similar</em> to the expression sequence selected.  In this context, 
%% two expressions, A and B say, are similar if there exists an anti-unifier, say C,  of A and B, and C satisfies the similarity score specified
%% by the user (the calculation calculated of similarity score is going to be further explored).
%% </p>
%% Usage: highlight the expression sequence of interest, then selected  <em> Similar Expression Search in Current Buffer </em> 
%% from  <em> Similar Code Detection </em>.

-spec(similar_expression_search_in_buffer/6::(filename(), [integer()], [integer()], string(),[dir()],integer())
      -> {ok, [{filename(), {{integer(), integer()}, {integer(), integer()}}}]}).   
similar_expression_search_in_buffer(FileName, _Start=[SLn, SCol], _End=[ELn, ECol], SimiScore, SearchPaths, TabWidth) ->
    try_refac(refac_sim_expr_search, sim_expr_search_in_buffer, 
              [FileName, {SLn, SCol}, {ELn, ECol}, SimiScore, SearchPaths, TabWidth]).


%% ==================================================================================================
%% @doc Simiar expression search across multiple Erlang modules.
%% <p> This functionality allows searching for expression sequence that are <em>similar</em> to the expression sequence selected.  In this context, 
%% two expressions, A and B say, are similar if there exists an anti-unifier, say C,  of A and B, and C satisfies the similarity score specified
%% by the user (the calculation calculated of similarity score is going to be further explored).
%% </p>
%% Usage: highlight the expression sequence of interest, then selected  <em> Similar Expression Search in Dirs</em> from  <em>Simiar Code Detection</em>.

-spec(similar_expression_search_in_dirs/6::(filename(), [integer()], [integer()], string(),[dir()],integer())
      ->{ok, [{filename(), {{integer(), integer()}, {integer(), integer()}}}]}).
similar_expression_search_in_dirs(FileName, _Start=[SLn, SCol], _End=[ELn, ECol], SimiScore, SearchPaths, TabWidth) ->
    try_refac(refac_sim_expr_search, sim_expr_search_in_dirs,
              [FileName,  {SLn, SCol}, {ELn, ECol}, SimiScore, SearchPaths, TabWidth]).

%%@private
-spec(simi_expr_search_in_buffer_eclipse/6::(filename(), pos(), pos(), float(),[dir()],integer())
      -> {[{{filename(), integer(), integer()}, {filename(), integer(), integer()}}], string()}).
simi_expr_search_in_buffer_eclipse(FName, Start, End, SimiScore0, SearchPaths, TabWidth) ->
    try_refac(refac_sim_expr_search, sim_expr_search_in_buffer_eclipse, [FName, Start, End, SimiScore0, SearchPaths, TabWidth]).

%%@private
-spec(simi_expr_search_in_dirs_eclipse/6::(filename(), pos(), pos(), float(),[dir()],integer())
      -> {[{{filename(), integer(), integer()}, {filename(), integer(), integer()}}], string()}).
simi_expr_search_in_dirs_eclipse(FName, Start, End, SimiScore0, SearchPaths, TabWidth) ->
    try_refac(refac_sim_expr_search, sim_expr_search_in_dirs_eclipse, [FName, Start, End, SimiScore0, SearchPaths, TabWidth]).

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
-spec(fun_extraction/6::(filename(), [integer()], [integer()], string(), context(), integer()) ->
	      {error, string()} | {warning, string()} |{ok, [filename()]}).
fun_extraction(FileName, _Start=[SLn, SCol], _End=[ELn, ECol], FunName, Context, TabWidth) -> 
    try_refac(refac_new_fun, fun_extraction, [FileName, {SLn,SCol}, {ELn, ECol}, FunName, Context, TabWidth]).

%%@private
-spec(fun_extraction_1/8::(filename(), integer(), integer(), integer(), integer(), string(), context(), integer()) ->
	      {error, string()} | {ok, [filename()]}).
fun_extraction_1(FileName, StartLine, StartCol, EndLine, EndCol, FunName, Context, TabWidth) ->
    try_refac(refac_new_fun, fun_extraction_1, [FileName, {StartLine, StartCol}, {EndLine, EndCol}, FunName, Context, TabWidth]).

%%@private
-spec(fun_extraction_eclipse/5::(FileName::filename(), Start::pos(), End::pos(), FunName::string(), TabWidth::integer()) ->
	      {error, string()} | {warning, string()} |{ok, [{filename(), filename(), string()}]}).
fun_extraction_eclipse(FileName, Start, End, FunName, TabWidth) -> 
    try_refac(refac_new_fun, fun_extraction_eclipse, [FileName, Start, End, FunName, TabWidth]).

%%@private
-spec(fun_extraction_1_eclipse/5::(FileName::filename(), Start::pos(), End::pos(), FunName::string(), TabWidth::integer()) ->
	      {error, string()} |{ok, [{filename(), filename(), string()}]}).
fun_extraction_1_eclipse(FileName, Start, End, FunName, TabWidth) -> 
    try_refac(refac_new_fun, fun_extraction_1_eclipse, [FileName, Start, End, FunName, TabWidth]).



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

-spec(unfold_fun_app/5::(FileName::filename(), Pos::[integer()], SearchPaths::[dir()], context(), TabWidth::integer)
      ->{error, string()} |{'ok', [string()]}).
unfold_fun_app(FileName, _Pos=[Ln, Col], SearchPaths, Context, TabWidth) ->
    try_refac(refac_unfold_fun_app, unfold_fun_app, [FileName, {Ln, Col}, SearchPaths, Context, TabWidth]).


%%@private
-spec(unfold_fun_app_eclipse/4::(FileName::filename(), Pos::pos(), SearchPaths::[dir()], TabWidth::integer)
      ->{error, string()} | {ok, [{filename(), filename(), string()}]}).
unfold_fun_app_eclipse(FileName, Pos, SearchPaths, TabWidth) ->
    try_refac(refac_unfold_fun_app, unfold_fun_app_eclipse, [FileName, Pos, SearchPaths, TabWidth]).

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
fold_expr({FileName, Line, Col, SearchPaths, Context, TabWidth}) -> 
    fold_expr_by_loc(FileName, Line, Col, SearchPaths, Context, TabWidth);
fold_expr({FileName, ModName, FunName, Arity, ClauseIndex, SearchPaths, Context, TabWidth}) ->
    fold_expr_by_name(FileName, ModName, FunName, Arity, ClauseIndex, SearchPaths, Context,TabWidth).

%%@private
-spec(fold_expr_by_loc/6::(filename(), integer(), integer(), [dir()], context(), integer()) -> 
	     {ok, [{integer(), integer(), integer(), integer(), syntaxTree(), {syntaxTree(), integer()}}]}
		 | {error, string()}).
fold_expr_by_loc(FileName, Line, Col, SearchPaths, Context, TabWidth) ->
    try_refac(refac_fold_expression, fold_expr_by_loc, [FileName, Line, Col, SearchPaths, Context, TabWidth]).

%%@private
-spec(fold_expr_by_loc_eclipse/5::(FileName::filename(), Line::integer(), Col::integer(), SearchPaths::[dir()], TabWidth::integer()) -> 
	     {ok, {syntaxTree(),[{{{integer(), integer()}, {integer(), integer()}}, syntaxTree()}]}}
		 | {error, string()}).
fold_expr_by_loc_eclipse(FileName, Line, Col, SearchPaths, TabWidth) ->
    try_refac(refac_fold_expression, fold_expr_by_loc_eclipse, [FileName, Line, Col, SearchPaths, TabWidth]).

%%@private
-spec(fold_expr_by_name/8::(filename(), string(), string(), string(), string(), [dir()], context(), integer()) ->
	     {ok, [{integer(), integer(), integer(), integer(), syntaxTree(), {syntaxTree(), integer()}}]}
		 | {error, string()}).
fold_expr_by_name(FileName, ModName, FunName, Arity, ClauseIndex, SearchPaths, Context, TabWidth) ->
    try_refac(refac_fold_expression, fold_expr_by_name, [FileName, ModName, FunName, Arity, ClauseIndex, SearchPaths, Context, TabWidth]).

%%@private
-spec(fold_expr_by_name_eclipse/7::(FileName::filename(), ModName::string(), FunName::string(), Arity::integer(), ClauseIndex::integer(), 
				    SearchPaths::[dir()], TabWidth::integer()) ->
	     {ok, {syntaxTree(), [{{{integer(), integer()}, {integer(), integer()}}, syntaxTree()}]} 
		 | {error, string()}}).
fold_expr_by_name_eclipse(FileName, ModName, FunName, Arity, ClauseIndex, SearchPaths, TabWidth) ->
    try_refac(refac_fold_expression, fold_expr_by_name_eclipse, [FileName, ModName, FunName, Arity, ClauseIndex, SearchPaths, TabWidth]).

%%@private
-spec(fold_expr_1_eclipse/5::(FileName::filename(), FunClauseDef::syntaxTree(), 
                     StartEndExpList::[{{{integer(), integer()}, {integer(), integer()}}, syntaxTree()}],
                     SearchPaths::[dir()], TabWidth::integer()) ->
	     {ok, [{filename(), filename(), string()}]}).
fold_expr_1_eclipse(FileName, FunClauseDef, StartEndExpList, SearchPaths, TabWidth) ->
    try_refac(refac_fold_expression, fold_expr_1_eclipse, [FileName, FunClauseDef, StartEndExpList, SearchPaths, TabWidth]).


%%=========================================================================================
%% @doc Add a tag to all the messages received by a server process (Beta).
%% <p> This refactoring should be initiated from the main receive function of a server process.
%% The current implementation is still in an experimental stage, and has a number of limitations:
%% <li> The current implementation assumes that the process does not send inquiries, 
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

-spec(add_a_tag/7::(filename(), integer(), integer(), string(), [dir()], context(), integer()) ->
	     {error, string()} | {ok, [filename()]}).
add_a_tag(FileName, Line, Col, Tag, SearchPaths, Context, TabWidth) ->
    try_refac(refac_add_a_tag, add_a_tag, [FileName, Line, Col, Tag, SearchPaths, Context, TabWidth]).



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

-spec(register_pid/7::(filename(), [integer()], [integer()], string(), [dir()], context(), integer()) ->
    {error, string()}|{ok, [filename()]}).
register_pid(FileName, _Start=[SLn, SCol], _End=[ELn, ECol], RegName, SearchPaths, Context, TabWidth) ->
    try_refac(refac_register_pid, register_pid, [FileName, {SLn, SCol}, {ELn, ECol}, 
                                                 RegName, SearchPaths, Context, TabWidth]).
    
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

-spec(tuple_funpar/6::(filename(), [integer()], [integer()], [dir()], context(), integer()) ->
	     {error, string()} | {ok, [filename()]}).
tuple_funpar(FileName, _StartLoc=[SLn, SCol], _EndLoc=[ELn, ECol],SearchPaths, Context, TabWidth) ->
    try_refac(refac_tuple, tuple_funpar, [FileName, {SLn, SCol}, {ELn, ECol}, SearchPaths, Context, TabWidth]).

tuple_funpar_1(FileName, _StartLoc=[SLn, SCol], _EndLoc=[ELn, ECol], SearchPaths, Context, TabWidth) ->
    try_refac(refac_tuple, tuple_funpar_1, [FileName, {SLn, SCol}, {ELn, ECol}, SearchPaths, Context, TabWidth]).


tuple_args(ModOrFile, FA, Index1,Index2, SearchPaths, Context, TabWidth)->
    try_refac(refac_tuple, tuple_args, [ModOrFile, FA, Index1, Index2, SearchPaths, Context, TabWidth]).


%%@private
-spec(tuple_funpar_eclipse/5::(filename(), pos(), pos, [dir()], integer()) ->
     {error, string()} | {ok, [{filename(), filename(), string()}]}).
tuple_funpar_eclipse(FileName, StartLoc, EndLoc, SearchPaths, TabWidth) ->
    try_refac(refac_tuple, tuple_funpar_eclipse, [FileName, StartLoc, EndLoc, SearchPaths, TabWidth]).

%%@private
tuple_funpar_eclipse_1(FileName, StartLoc, EndLoc, SearchPaths, TabWidth) ->
    try_refac(refac_tuple, tuple_funpar_eclipse_1, [FileName, StartLoc, EndLoc, SearchPaths, TabWidth]).

%%@private
add_to_export(FileName, {FunName, Arity}, SearchPaths, Editor, TabWidth) ->
    try_refac(refac_add_to_export, add_to_export, [FileName, {FunName, Arity}, SearchPaths, Editor, TabWidth]).

%%@private
swap_args(FileName, {FunName, Arity}, Index1, Index2, SearchPaths, Editor, TabWidth) ->
    try_refac(refac_swap_function_arguments, swap_args, [FileName, {FunName, Arity}, Index1, Index2, SearchPaths, Editor, TabWidth]).
%%=========================================================================================
%% @doc Turn a function into a server process (Beta).
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

-spec(fun_to_process/7::(filename(), integer(), integer(), string(), [dir()], context(), integer()) -> 
                        {ok, [filename()]} |  {undecidables, string()} | {error, string()}).
fun_to_process(FileName, Line, Col, ProcessName, SearchPaths, Context, TabWidth) ->
    try_refac(refac_fun_to_process, fun_to_process, 
              [FileName, Line, Col, ProcessName, SearchPaths, Context, TabWidth]).

%%@private
-spec(fun_to_process_eclipse/6::(filename(), integer(), integer(), string(), [dir()], integer()) -> 
	     {ok, [{filename(), filename(), string()}]} | 
		 {undecidables, string()} | {error, string()}).
fun_to_process_eclipse(FName, Line, Col, ProcessName, SearchPaths, TabWidth) ->
    try_refac(refac_fun_to_process, fun_to_process_eclipse, [FName, Line, Col, ProcessName, SearchPaths, TabWidth]).
    
%%@private
-spec(fun_to_process_1_eclipse/6::(filename(), integer(), integer(), string(), [dir()], integer()) -> {ok, [{filename(), filename(), string()}]}).
fun_to_process_1_eclipse(FName, Line, Col, ProcessName, SearchPaths, TabWidth) ->
    try_refac(refac_fun_to_process, fun_to_process_1_eclipse, [FName, Line, Col, ProcessName, SearchPaths, TabWidth]).
    
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

-spec(rename_process/7::(filename(), integer(), integer(), string(), [dir()], context(), integer()) ->
	       {error, string()} | {undecidables, string()} | {ok, [filename()]}).
rename_process(FileName, Line, Col, NewName, SearchPaths, Context, TabWidth) ->
    try_refac(refac_rename_process, rename_process, [FileName, Line, Col, NewName, SearchPaths, Context, TabWidth]).



%%@private
-spec(rename_process_eclipse/6::(filename(), integer(), integer(), string(), [dir()], integer()) ->
	       {error, string()} | {undecidables, string()} |  {ok, [{filename(), filename(), string()}]}).
rename_process_eclipse(FileName, Line, Col, NewName, SearchPaths, TabWidth) ->
    try_refac(refac_rename_process, rename_process_eclipse, [FileName, Line, Col, NewName, SearchPaths, TabWidth]).

%%@private
-spec(rename_process_1_eclipse/5::(string(), string(), string(), [dir()], integer()) -> {ok, [{filename(), filename(), string()}]}).
rename_process_1_eclipse(FileName, OldProcessName, NewProcessName, SearchPaths, TabWidth) ->
    try_refac(refac_rename_process, rename_process_1_eclipse, [FileName, OldProcessName, NewProcessName, SearchPaths, TabWidth]).

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
-spec(new_macro/7::(filename(), [integer()], [integer()], string(), [dir()], context(), integer()) ->
	      {error, string()} | {ok, string()}).
new_macro(FileName, _Start=[SLn, SCol], _End=[ELn, ECol], MacroName, SearchPaths, Context, TabWidth) -> 
    try_refac(refac_new_macro, new_macro, [FileName, {SLn, SCol}, {ELn, ECol},
                                           MacroName, SearchPaths, Context, TabWidth]).

%%@private
-spec(new_macro_eclipse/6::(filename(), pos(), pos(), string(), [dir()], integer()) ->
	     {error, string()} | {ok, [{filename(), filename(), string()}]}).
new_macro_eclipse(FileName, Start, End, NewMacroName, SearchPaths, TabWidth) ->
    try_refac(refac_new_macro, new_macro_eclipse, [FileName, Start, End, NewMacroName, SearchPaths, TabWidth]).


%% =====================================================================================================
%% @doc Introduce a new variable to represent an expression selected.
%% <p>
%% Usage: Highlight the expression of interest, then selected the <em>Introduce New Variable</em> 
%% from <em>Refactor</em>, Wrangler will then prompt for the new variable name.
%% </p> 
-spec(intro_new_var/7::(filename(), [integer()], [integer()], string(), [dir()], context(), integer()) ->
	      {error, string()} | {ok, string()}).
intro_new_var(FileName, _Start=[SLn, SCol], _End=[ELn, ECol], NewVarName, SearchPaths, Context, TabWidth) -> 
    try_refac(refac_intro_new_var, intro_new_var, [FileName, {SLn, SCol}, {ELn, ECol}, 
                                                   NewVarName, SearchPaths, 
                                                   Context, TabWidth]).

%%@private
-spec(intro_new_var_eclipse/6::(filename(), pos(), pos(), string(), [dir()], integer()) ->
	      {error, string()} | {ok, [{filename(), filename(), string()}]}).
intro_new_var_eclipse(FileName, Start, End, NewVarName, SearchPaths, TabWidth) -> 
    try_refac(refac_intro_new_var, intro_new_var_eclipse, [FileName, Start, End, NewVarName, SearchPaths, TabWidth]).



%% =====================================================================================================
%% @doc Inline a variable definition.
%% <p> To unfold a particular use instance of a variable, point the cursor to that instance, and then select
%% <em> Inline Variable </em> from the <em> Refactor </em> menu; to unfold some, or all, use instances of 
%% a variable, point the cursor to the define occurrence of the variable, then select <em> Inline Variable </em> 
%% from the <em> Refactor </em> menu, Wrangler will search for the use occurrences of the variable selected, and 
%% let you choose which instances to unfold. Only variables defined via a match expression of the 
%% format: VarName=Expr can be inlined.
%% </p>
-spec (inline_var/6::(filename(), integer(), integer(),[dir()], context(), integer()) ->
			   {error, string()} | {ok, string()} | 
			   {ok, [{pos(), pos()}], string()}).
inline_var(FName, Line, Col, SearchPaths, Context, TabWidth) ->
    try_refac(refac_inline_var, inline_var, [FName, Line, Col, SearchPaths, Context, TabWidth]).

-spec (inline_var_1/8::(FileName::filename(), Line::integer(), Col::integer(), 
				Candidates::[{{StartLine::integer(), StartCol::integer()},{EndLine::integer(), EndCol::integer()}}], 
				SearchPaths::[dir()], Editor::context(), TabWidth::integer(), LogMsg::string()) ->
				     {error, string()}| {ok, [{filename(), filename(), string()}]}).
inline_var_1(FileName, Line, Col, Candidates, SearchPaths, Editor, TabWidth, Cmd) ->
    try_refac(refac_inline_var, inline_var_1, [FileName, Line, Col, Candidates, SearchPaths, TabWidth, Cmd, Editor]).
  
%%@private
-spec (inline_var_eclipse/5::(FileName::filename(), Line::integer(), Col::integer(), SearchPaths::[dir()], TabWidth::integer()) ->
				   {error, string()} |{ok, [{filename(), filename(), string()}]}|
				   {ok, Candidates::[{{StartLine::integer(), StartCol::integer()},{EndLine::integer(), EndCol::integer()}}]}).
inline_var_eclipse(FileName, Line, Col, SearchPaths, TabWidth) ->
     try_refac(refac_inline_var, inline_var_eclipse, [FileName, Line, Col, SearchPaths, TabWidth]).

%%@private
-spec (inline_var_eclipse_1/6::(FileName::filename(), Line::integer(), Col::integer(), 
				Candidates::[{{StartLine::integer(), StartCol::integer()},{EndLine::integer(), EndCol::integer()}}], 
				SearchPaths::[dir()], TabWidth::integer()) ->
				     {error, string()}| {ok, [{filename(), filename(), string()}]}).				   
inline_var_eclipse_1(FileName, Line, Col, Candidates, SearchPaths, TabWidth) ->
    try_refac(refac_inline_var, inline_var_eclipse_1, [FileName, Line, Col, Candidates, SearchPaths, TabWidth]).
  
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

-spec(fold_against_macro/6::(filename(), integer(), integer(), [dir()], context(), integer()) ->
				  {error, string()} |{ok, [{integer(), integer(), integer(), integer(), 
							    syntaxTree(), syntaxTree()}], string()}).
fold_against_macro(FileName, Line, Col, SearchPaths, Context, TabWidth) ->
    try_refac(refac_fold_against_macro, fold_against_macro, [FileName, Line, Col, SearchPaths, Context, TabWidth]).



%%@private
-spec(fold_against_macro_eclipse/5::(filename(), integer(), integer(), [dir()], integer()) ->
					  {error, string()} |
                                          {ok, {syntaxTree(), [{{{integer(), integer()},
                                                                 {integer(), integer()}}, syntaxTree()}]}}).
fold_against_macro_eclipse(FileName, Line, Col, SearchPaths, TabWidth) ->
    try_refac(refac_fold_against_macro, fold_against_macro_eclipse, [FileName, Line, Col, SearchPaths, TabWidth]).

%%@private
-spec(fold_against_macro_1_eclipse/5::(filename(), [{{{integer(), integer()}, {integer(), integer()}}, syntaxTree()}], syntaxTree(), 
				       [dir()], integer()) -> {error, string()} | {ok, [{filename(), filename(), string()}]}).
fold_against_macro_1_eclipse(FileName, CandidatesToFold, MacroDef, SearchPaths, TabWidth) ->
    try_refac(refac_fold_against_macro, fold_against_macro_1_eclipse, [FileName, CandidatesToFold, MacroDef, SearchPaths, TabWidth]).


%% =============================================================================================
%% @doc Reorder the record fields in a record expression to be consistent with the record definition.
%%<p>
%% Usage: point cursor to the record expression interested, then select <em>Normalise Record Expression</em> from <em>Refactor</em>.
%%</p>

-spec(normalise_record_expr/7::(filename(), integer(), integer(), boolean(), [dir()], context(), integer()) 
             -> {error, string()} | {ok, [filename()]}).
normalise_record_expr(FileName, Line, Col, ShowDefault, SearchPaths, Context, TabWidth) ->
    try_refac(refac_sim_expr_search, normalise_record_expr, [FileName, {Line, Col}, ShowDefault, SearchPaths, Context, TabWidth]).

%%@private
-spec normalise_record_expr_eclipse/5::(filename(), pos(), boolean(), [dir()], integer()) ->
				{error, string()}| {ok, [{filename(), filename(), string()}]}.
normalise_record_expr_eclipse(FileName, Pos, ShowDefault, SearchPaths, TabWidth) ->
    try_refac(refac_sim_expr_search, normalise_record_expr_eclipse, [FileName, Pos, ShowDefault, SearchPaths, TabWidth]).


%% =============================================================================================
%% @doc Introduce ?LET.
%% <p>Bind the values generated by a generator to a variable, so that the values generated by 
%% the generator can be referred by other generators. The refactoring helps to introduce dependency 
%% between QuickCheck generators. This refactoring is especially for QuickCheck users.
%%</p>
%%<p>
%% Usage: highlight the expression, which should be a QuickCheck generator, then select the 
%% refactoring command, you will be prompted for the pattern variable name.
%%</p>

-spec(new_let/7::(filename(), [integer()], [integer()], string(), [dir()], context(), integer()) ->
	      {error, string()} | {ok, [filename()]} |{question, string(), list(), list(),string()}).
new_let(FileName, _Start=[SLn, SCol], _End=[ELn, ECol], PatName, SearchPaths, Context, TabWidth) -> 
    try_refac(refac_new_let, new_let, [FileName, {SLn, SCol}, {ELn, ECol}, PatName, SearchPaths, Context, TabWidth]).

%%@private
-spec(new_let_1/7::(filename(), string(), list(), list(), [dir()], integer(), string()) ->			 
			 {error, string()} |{ok,[filename()]}).
new_let_1(FileName, NewPatName, Expr, ParentExpr, SearchPaths, TabWidth, Cmd) ->
    try_refac(refac_new_let, new_let_1, [FileName, NewPatName, Expr, ParentExpr, SearchPaths, TabWidth, Cmd]).

%%@private
-spec(new_let_eclipse/6::(filename(), pos(), pos(), string(), [dir()], integer()) ->
	    {error, string()}|{'ok', [{filename(), filename(),string()}]} | {question, string(), {syntaxTree(), syntaxTree()}}).
new_let_eclipse(FileName, Start, End, NewPatName, SearchPaths, TabWidth) ->
    try_refac(refac_new_let, new_let_eclipse, [FileName, Start, End, NewPatName, SearchPaths, TabWidth]).

%%@private
-spec(new_let_1_eclipse/6::(filename(), string(), syntaxTree(), syntaxTree(), [dir()], integer()) ->			 
				 {error, string()}|{'ok', [{filename(), filename(),string()}]}).
new_let_1_eclipse(FileName, NewPatName, Expr, ParentExpr, SearchPaths, TabWidth) ->
    try_refac(refac_new_let, new_let_1_eclipse, [FileName, NewPatName, Expr, ParentExpr, SearchPaths, TabWidth]).


%% =============================================================================================
%% @doc Merge nested but independent ?LETs into one ?LET.
%% <p> This refactoring combines multiple nested, but undependent, ?LETs into one; the latter representation has 
%% better shrinking performance. This refactoring is especially for QuickCheck users.
%% </p>
%%<p>
%% Usage: Select the refactoring command, and Wrangler will search for candidates to merge automatically, 
%% guide you through the candidates found one by one, and ask you to decide whether to merge.
%%</p>

-spec(merge_let/4::(FileName::filename, SearchPaths::[dir()], context(), TabWidth::integer()) ->
	         {error, string()} | {not_found, string()} |
		 {ok, [{integer(), integer(), integer(), integer(), string()}], string()}).
merge_let(FileName, SearchPaths, Context, TabWidth) ->
    try_refac(refac_new_let, merge_let, [FileName, SearchPaths, Context, TabWidth]).

%%@private
-spec(merge_let_1/6::(FileName::filename, Candidates::[{{integer(), integer(), integer(), integer()}, string()}],
		      SearchPaths::[dir()],context(), TabWidth::integer(), Cmd::string()) -> {ok, [filename()]}).
merge_let_1(FileName, Candidates, SearchPaths, Context, TabWidth, Cmd) ->
    try_refac(refac_new_let, merge_let_1, [FileName, Candidates, SearchPaths, Context, TabWidth, Cmd]).


%%@private
-spec(merge_let_eclipse/3::(FileName::filename(), SearchPaths::[dir()], TabWidth::integer()) ->
			{error, string()}| {not_found, string()} |{ok, [{{{integer(), integer()}, {integer(), integer()}}, syntaxTree()}]}).
merge_let_eclipse(FileName, SearchPaths, TabWidth) ->
    try_refac(refac_new_let, merge_let_eclipse, [FileName, SearchPaths, TabWidth]).

%%@private
-spec(merge_let_1_eclipse/4::(FileName::filename(), Candidates::[{{{integer(), integer()}, {integer(), integer()}}, syntaxTree()}],
				 SearchPaths::[dir()], TabWidth::integer()) -> 
		{error, string()}|{'ok', [{filename(), filename(),string()}]}).
merge_let_1_eclipse(FileName, Candidates, SearchPaths, TabWidth) ->
    try_refac(refac_new_let, merge_let_1_eclipse, [FileName, Candidates, SearchPaths, TabWidth]).


%% =============================================================================================
%% @doc Merge nested but undependent ?FORALLs into one ?FORALL.
%% <p> This refactoring combines multiple nested, but undependent, ?FORALLs into one; the latter representation has 
%% better shrinking performance. This refactoring is especially for QuickCheck users.
%% </p>
%%<p>
%% Usage: Select the refactoring command, and Wrangler will search for candidates to merge automatically, 
%% guide you through the candidates found one by one, and ask you to decide whether to merge.
%%</p>

-spec(merge_forall/4::(FileName::filename, SearchPaths::[dir()], context(), TabWidth::integer()) ->
	     {error, string()} |{not_found, string()} |{ok, [{integer(), integer(), integer(), integer(), string()}], string()}).
merge_forall(FileName, SearchPaths, Context, TabWidth) ->
    try_refac(refac_new_let, merge_forall, [FileName, SearchPaths, Context, TabWidth]).

%%@private
-spec(merge_forall_1/6::(FileName::filename, Candidates::[{{integer(), integer(), integer(), integer()}, string()}],
		      SearchPaths::[dir()], context(), TabWidth::integer(), Cmd::string()) -> {ok, [filename()]}).
merge_forall_1(FileName, Candidates, SearchPaths, Context, TabWidth, Cmd) ->
    try_refac(refac_new_let, merge_forall_1, [FileName, Candidates, SearchPaths, Context, TabWidth, Cmd]).

%%@private
-spec(merge_forall_eclipse/3::(FileName::filename(), SearchPaths::[dir()], TabWidth::integer()) ->
			{error, string()}| {not_found, string()} |{ok, [{{{integer(), integer()}, {integer(), integer()}}, syntaxTree()}]}).
merge_forall_eclipse(FileName, SearchPaths, TabWidth) ->
    try_refac(refac_new_let, merge_forall_eclipse, [FileName, SearchPaths, TabWidth]).

%%@private
-spec(merge_forall_1_eclipse/4::(FileName::filename(), Candidates::[{{{integer(), integer()}, {integer(), integer()}}, syntaxTree()}],
				 SearchPaths::[dir()], TabWidth::integer()) -> 
		{error, string()}|{'ok', [{filename(), filename(),string()}]}).
merge_forall_1_eclipse(FileName, Candidates, SearchPaths, TabWidth) ->
    try_refac(refac_new_let, merge_forall_1_eclipse, [FileName, Candidates, SearchPaths, TabWidth]).


%% =============================================================================================
%% @doc Turn a non-record representation of eqc_statem state into a record representation.
%% <p> This refactoring introduce a record to represent the state used by eqc_statem. 
%% This refactoring is especially for QuickCheck users.
%% </p>
%% <p> Usage: Select the refactoring command, and Wrangler will check the current type of the state machine, 
%% and prompt you to input the record and field names if Wrangler is able to proceed the refactoring.
%% </p>

-spec(eqc_statem_to_record/4::(filename(),[dir()], context(), integer()) ->
	         {error, string()} |
		 {'ok', non_tuple, [{atom(), atom(), integer()}]} |
		 {'ok', {tuple, integer()}, [{atom(), atom(), integer()}]}).
eqc_statem_to_record(FileName, SearchPaths, Context, TabWidth) ->
    try_refac(refac_state_to_record, eqc_statem_to_record, [FileName, SearchPaths, Context, TabWidth]).

%@private
eqc_statem_to_record_1(FileName, RecordName, RecordFields, StateFuns, IsTuple, SearchPaths, Context, TabWidth) ->
    try_refac(refac_state_to_record, eqc_statem_to_record_1,
	      [FileName, RecordName, RecordFields, StateFuns, IsTuple, SearchPaths, Context, TabWidth]).

%%@private
-spec(eqc_statem_to_record_eclipse/3::(filename(),[dir()], integer()) -> 
					     {error, string()} |
						{'ok', non_tuple, [{atom(), atom(), integer()}]} | 
						{'ok', {tuple, integer()}, [{atom(), atom(), integer()}]}).
eqc_statem_to_record_eclipse(FileName, SearchPaths, TabWidth) ->
    try_refac(refac_state_to_record, eqc_statem_to_record_eclipse, [FileName, SearchPaths, TabWidth]).

%%@private
-spec(eqc_statem_to_record_1_eclipse/7::(FileName::filename(), RecordName::string(), RecordFields::[string()],
					StateFuns::[{atom(), atom(), integer()}], IsTuple::boolean(),
					SearchPaths::[dir()], TabWidth::integer()) ->
					      {error, string()} |
					      {ok, [{filename(), filename(), string()}]}).
eqc_statem_to_record_1_eclipse(FileName, RecordName, RecordFields, StateFuns, IsTuple, SearchPaths, TabWidth) ->
    try_refac(refac_state_to_record, eqc_statem_to_record_1_eclipse,
	      [FileName, RecordName, RecordFields, StateFuns, IsTuple, SearchPaths, TabWidth]).
    
%% =============================================================================================
%% @doc Turn a non-record representation of eqc_fsm state into a record representation.
%% <p> This refactoring introduce a record to represent the state used by eqc_fsm. 
%% This refactoring is especially for QuickCheck users.
%% </p>
%% <p> Usage: Select the refactoring command, and Wrangler will check the current type of the state machine, 
%% and prompt you to input the record and field names if Wrangler is able to proceed the refactoring.
%% </p>

-spec(eqc_fsm_to_record/4::(filename(),[dir()], context(), integer()) -> 
	         {error, string()}|
		 {'ok', non_tuple, [{atom(), atom(), integer()}]} | 
		 {'ok', {tuple, integer()}, [{atom(), atom(),integer()}]}).
eqc_fsm_to_record(FileName, SearchPaths, Context, TabWidth) ->
    try_refac(refac_state_to_record, eqc_fsm_to_record, [FileName, SearchPaths, Context, TabWidth]).

%@private
eqc_fsm_to_record_1(FileName, RecordName, RecordFields, StateFuns, IsTuple, SearchPaths, Context, TabWidth) ->
    try_refac(refac_state_to_record, eqc_fsm_to_record_1,
	      [FileName, RecordName, RecordFields, StateFuns, IsTuple, SearchPaths, Context, TabWidth]).


%%@private
-spec(eqc_fsm_to_record_eclipse/3::(filename(),[dir()], integer()) -> 
					 {error, string()} |
					 {'ok', non_tuple, [{atom(), atom(), integer()}]} | 
					     {'ok', {tuple, integer()}, [{atom(), atom(), integer()}]}).
eqc_fsm_to_record_eclipse(FileName, SearchPaths, TabWidth) ->
    try_refac(refac_state_to_record, eqc_fsm_to_record_eclipse, [FileName, SearchPaths, TabWidth]).

%%@private
-spec(eqc_fsm_to_record_1_eclipse/7::(FileName::filename(), RecordName::string(), RecordFields::[string()],
					StateFuns::[{atom(), atom(), integer()}], IsTuple::boolean(),
					SearchPaths::[dir()], TabWidth::integer()) ->
					   {error, string()} |
					       {ok, [{filename(), filename(), string()}]}).
eqc_fsm_to_record_1_eclipse(FileName, RecordName, RecordFields, StateFuns, IsTuple, SearchPaths, TabWidth) ->
    try_refac(refac_state_to_record, eqc_fsm_to_record_1_eclipse,
	      [FileName, RecordName, RecordFields, StateFuns, IsTuple, SearchPaths, TabWidth]).
    


%% =============================================================================================
%% @doc Turn a non-record representation of gen_fsm state into a record representation.
%% <p> This refactoring introduce a record to represent the state used by eqc_statem. 
%% </p>
%% <p> Usage: Select the refactoring command, and Wrangler will check the current type of the state machine, 
%% and prompt you to input the record and field names if Wrangler is able to proceed the refactoring.
%% </p>

-spec(gen_fsm_to_record/4::(filename(),[dir()], context(), integer()) -> 
	         {error, string()} |
		 {'ok', non_tuple, [{atom(), atom(), integer()}]} | 
		 {'ok', {tuple, integer()}, [{atom(), atom(), integer()}]}).
gen_fsm_to_record(FileName, SearchPaths, Context, TabWidth) ->
    try_refac(refac_state_to_record, gen_fsm_to_record, [FileName, SearchPaths, Context, TabWidth]).

%@private  
gen_fsm_to_record_1(FileName, RecordName, RecordFields, StateFuns, IsTuple, SearchPaths, Context, TabWidth) ->
    try_refac(refac_state_to_record, gen_fsm_to_record_1,
	      [FileName, RecordName, RecordFields, StateFuns, IsTuple, SearchPaths, Context, TabWidth]).


%%@private
-spec(gen_fsm_to_record_eclipse/3::(filename(),[dir()], integer()) ->
					  {error, string()} |
					 {'ok', non_tuple, [{atom(), atom(), integer()}]} | 
					     {'ok', {tuple, integer()}, [{atom(), atom(), integer()}]}).
gen_fsm_to_record_eclipse(FileName, SearchPaths, TabWidth) ->
    try_refac(refac_state_to_record, gen_fsm_to_record_eclipse, [FileName, SearchPaths, TabWidth]).

%%@private
-spec(gen_fsm_to_record_1_eclipse/7::(FileName::filename(), RecordName::string(), RecordFields::[string()],
					StateFuns::[{atom(), atom(), integer()}], IsTuple::boolean(),
					SearchPaths::[dir()], TabWidth::integer()) ->
					   {error, string()} |
					       {ok, [{filename(), filename(), string()}]}).
gen_fsm_to_record_1_eclipse(FileName, RecordName, RecordFields, StateFuns, IsTuple, SearchPaths, TabWidth) ->
    try_refac(refac_state_to_record, gen_fsm_to_record_1_eclipse,
	      [FileName, RecordName, RecordFields, StateFuns, IsTuple, SearchPaths, TabWidth]).
  

%@private
eqc_statem_to_fsm(FileName, StateName, SearchPaths, Context, TabWidth) ->
    try_refac(refac_statem_to_fsm, eqc_statem_to_fsm, [FileName, StateName, SearchPaths, Context, TabWidth]).


%@private
eqc_statem_to_fsm_eclipse(FileName, StateName, SearchPaths, TabWidth) ->
    try_refac(refac_statem_to_fsm, eqc_statem_to_fsm_eclipse, [FileName, StateName, SearchPaths, TabWidth]).



%=========================================================================================
%%@doc Partition the exports of a module.
-spec(partition_exports/5::(File::filename(), DistTreshold::string(), 
			    SearchPaths::[filename()|dir()], context(), TabWidth::integer()) ->
				 {error, string()}|{ok, [filename()]}). 		  
partition_exports(File, DistThreshold, SearchPaths, Context, TabWidth)->
     try_refac(wrangler_modularity_inspection, partition_exports,
	       [File, DistThreshold, SearchPaths, Context, TabWidth]).
   
%%@private; Interface function for eclipse.
%% DisTreshold is a parameter inputted by the user:
%% "Please input a distance threshould between 0 and 1.0 (default value: 0.8):"
%%@private
-spec(partition_exports_eclipse/4::(File::filename(), DistTreshold::float(), 
				    SearchPaths::[filename()|dir()], TabWidth::integer()) ->
					 {error, string()}| {ok, [{filename(), filename(), string()}]}). 	  
partition_exports_eclipse(File, DistThreshold, SearchPaths, TabWidth)->
    try_refac(wrangler_modularity_inspection, partition_exports_eclipse,
	      [File, DistThreshold, SearchPaths, TabWidth]).


%%@private
try_to_apply(Mod, Fun, Args, Msg) -> 
    try apply(Mod, Fun, Args)
     catch
	 throw:Error -> 
	     Error;    %% wrangler always throws Error in the format of '{error, string()}';
	_E1:E2 ->
	    %% ?wrangler_io("Error:\n~p\n", [{_E1,E2}]),
             {error, Msg ++ lists:flatten(io_lib:format("\n~p",[{E2, erlang:get_stacktrace()}]))}
    end.

%%@private
try_refac(Mod, Fun, Args) ->
    Msg = "Wrangler failed to perform this refactoring, "
	"please report error to erlang-refactor@kent.ac.uk.",
    try_to_apply(Mod, Fun, Args, Msg).

%%@private
init_eclipse() ->
    application:start(wrangler).
    
%%@private
get_log_msg() ->
    Errors = wrangler_error_logger:get_logged_info(),
    FileErrors = [{File, Error} || {File, Error} <- Errors, File /= warning],
    WarningMsg = [Error || {File, Error} <- Errors, File == warning],
    case FileErrors of 
	[] -> lists:flatten(WarningMsg);
	_ ->
	    Msg1=io_lib:format("There are syntax errors, or syntaxes not supported by Wrangler;"
			       " functions/attribute containing these syntaxes may not be affected by the refactoring.\n", []),
	    Msg2 = lists:flatmap(fun ({FileName, Errs}) ->
			 		 Str = io_lib:format("File:\n ~p\n", [FileName]),
					 Str1 = Str ++ io_lib:format("Error(s):\n", []),
					 Str1 ++ lists:flatmap(fun (E) ->
								       case E of
									   {Pos, _Mod, Msg} -> io_lib:format(" ** ~p:~p **\n", [Pos, Msg]);
									   M -> io_lib:format("**~p**\n", [M])
								       end
							       end,
							       lists:reverse(Errs))
				 end, FileErrors),
	    Msg1++Msg2 ++ lists:flatten(WarningMsg)
    end.
 
 
