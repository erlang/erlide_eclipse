%% =====================================================================
%% Refactoring Interface Functions.
%%
%% Copyright (C) 2006-2008  Huiqing Li, Simon Thompson

%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.

%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.

%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%%
%% =====================================================================

%% @copyright 2006-2008 Huiqing Li, Simon Thompson
%%
%% @author Huiqing Li <hl@kent.ac.uk>
%%   [http://www.cs.kent.ac.uk/projects/forse]

%% @version  0.3
%% @end

%%
%% @doc The collection of refactorings supported by Wrangler.
%%
%% @end
%% ============================================


-module(wrangler).

-export([rename_var/5, 
	 rename_fun/5, 
	 rename_mod/3, 
	 rename_mod_batch/3,
	 generalise/5,
	 move_fun/6,
	 duplicated_code/3,
	 clone_detector/3,
	 expression_search/3,
	 fun_extraction/4,
	 fold_expression/3,
	 instrument_prog/2,
	 uninstrument_prog/2,
	 add_a_tag/5,
	 tuple_funpar/5,
         tuple_to_record/8,
	 register_pid/5,
	 fun_to_process/5]).

-export([rename_var_eclipse/5, 
	 rename_fun_eclipse/5, 
	 rename_mod_eclipse/3, 
	 generalise_eclipse/5,
	 move_fun_eclipse/6,
	 fun_extraction_eclipse/4,
	 gen_fun_1_eclipse/7,
	 gen_fun_2_eclipse/7,
	 tuple_funpar_eclipse/5,
         tuple_to_record_eclipse/8,
	 fold_expression_eclipse/3,
	 fold_expression_1_eclipse/3,
	 fold_expression_2_eclipse/5]).

-export([trace_send/4, trace_spawn/4]).
%% ====================================================================================================
%% @doc Rename a variable name with a user-supplied new name.
%% <p> To apply this refactoring, point the cursor to  any occurrence of this variable, then select
%% <em> Rename Variable Name </em> from the <em> Refactor </em> menu, after that the refactorer will prompt
%% to enter the new parameter name in the mini-buffer. 
%% </p>
%% <p> This refactoring has a local effect, i.e., it only affects the function in which the refactoring is initialised. 
%% </p>
%% <p> The following <em> side-conditions </em> apply to this refactoring: 
%% <li> The new variable name should not conflict with any of the declared variable names in the same scope;</li>
%% <li> The new name should not shadow any of the existing variables in the outer scopes, or be shadowed by any of 
%% of existing variables in the inner scopes, i.e., renaming to the new name should not change the semantics of the 
%% program.</li>
%% </p>
%% ================================================================================================
%% @spec rename_var(FileName::filename(), Line::integer(), Col::integer(), NewName::string(),SearchPaths::[string()])
%% -> term()
%%    
rename_var(FileName, Line, Col, NewName, SearchPaths) ->
    refac_rename_var:rename_var(FileName, Line, Col, NewName, SearchPaths).


rename_var_eclipse(FileName, Line, Col, NewName, SearchPaths) ->
    refac_rename_var:rename_var_eclipse(FileName, Line, Col, NewName, SearchPaths).

%%=========================================================================================
%% @doc Rename a function name with a user-supplied new name.
%% <p> To apply this refactoring, point the cursor to any occurrence of this 
%% function name, then select <em> Rename Function Name </em> from the <em> Refactor </em> menu, 
%% after that the refactorer will prompt to enter  the new function name in the mini-buffer.
%% </p>
%% <p>
%% When renaming an exported function name, this refactoring has a global effect, i.e.,
%% it affects all those modules in which this function is imported/used.
%% </p>
%% <p> The following <em> side-conditions </em> apply to this refactoring:
%% <li> The new function name should not cause confliction with any of the functions which are in scope in the 
%% current module;</li>
%% <li> In the case that the function to be renamed is imported by another module, the new function name (with the same 
%% arity) should not be already in scope (either defined or imported) in that module. </li>
%% </p>
%% ========================================================================================
%% @spec rename_fun(FileName::filename(), Line::integer(), Col::integer(), NewName::string(), SearchPaths::[string()])
%% -> term()
rename_fun(FileName, Line, Col, NewName, SearchPaths) ->
    refac_rename_fun:rename_fun(FileName, Line, Col, NewName, SearchPaths).


rename_fun_eclipse(FileName, Line, Col, NewName, SearchPaths) ->
    refac_rename_fun:rename_fun_eclipse(FileName, Line, Col, NewName, SearchPaths).

%%======================================================================================
%% @doc Rename a module name with a user-supplied new name.
%% <p> To apply this refactoring, point the cursor to anywhere in the module to be renamed, then select 
%% <em> Rename Module Name </em> from the <em> Refactor </em> menu, after that, the refactorer will prompt to enter 
%% the new module name in the mini-buffer.
%% </p>
%% <p> This refactoring has a global effect, i.e., it affects all those modules in which the module to be renamed is 
%% imported, or used as a module qualifier.
%% </p>
%% <p>
%% The following <em> side-conditions </em> apply to this refactoring:
%% <li> The new module name should be a fresh name. </li>
%% <li> This refactoring assume that the file basename is always the same as the module name, therefore this 
%% refactoring changes the filename as well. </li>
%% </p>
%% =====================================================================
%% @spec rename_mod(FileName::filename(), NewName::string(), SearchPaths::[string()])-> term()
%%   
rename_mod(FileName, NewName, SearchPaths) ->
    refac_rename_mod:rename_mod(FileName, NewName, SearchPaths).


rename_mod_eclipse(FileName, NewName, SearchPaths) ->
    refac_rename_mod:rename_mod_eclipse(FileName, NewName, SearchPaths).

%% =====================================================================
%% @doc Rename a collection of module names in batch mode. 
%% <p> This refactoring is supposed to be run from the Erlang shell. For example, 
%% to rename all those module names which match the regular expression "foo_*" to 
%% "foo_*_1_0" in the directory <code> c:/wrangler/test </code>, just type the following command:
%% <code> wrangler:rename_mod_batch("foo_*, "foo_*_1_0", ["c:/wrangler/test"]) </code>.
%% </p>
%% <p> This refactoring has a global effect. </p>
%% <p> The following <em> side-conditions </em> apply to this refactoring:
%% <li> The new module names should not conflict with each other, or any existing module names 
%% in the same scope which will not be renamed. </li>
%% <li> This refactorings assumes that the file basename is always the same as the module name. </li>
%% </p>
%% =====================================================================
%% @spec rename_mod_batch(OldNamePattern::string(), NewNamePattern::string(), 
%%                        SearchPaths::[string()])-> ok | {error, string()}
%%   
rename_mod_batch(OldNamePattern, NewNamePattern, SearchPaths) ->
    refac_batch_rename_mod:batch_rename_mod(OldNamePattern, NewNamePattern, SearchPaths).


%% ==========================================================================================
%% @doc  Generalise a function definition.
%% <p>Generalise a function definition by selecting a sub-expression of its right-hand 
%% side and making this the value of a new argument added to the definition of the function. 
%% The sub-expression becomes the actual parameter at the call sites. </p>
%%
%% <p> To apply this refactoring, highlight the expression first, then  select 
%% <em> Generalise Function Definition </em> from the <em>Refactor</em> menu, after 
%% that the refactorer will prompt to enter the parameter name in the mini-buffer. </p>
%% 
%% <p> Here is an example of generalisation, in which the function <code> add_one </code> defined 
%% on the left-hand side is generalised on the expression <code> 1 </code>, and the result is 
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
%% in an function expression before passing it at the actual parameter to the call-sites. This is illustrated 
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
%% <li> The selected expression should not contain any locally declared variable(s), unless the selected expression 
%% has side effect, in which case the locally declared variables will become the parameters of the function expression.
%% </li>
%% <li> The user-provided parameter name should not conflict with the existing parameters or
%% change the semantics of the function to be generalised. </li>
%% </p>
%% ==============================================================================
%% @spec generalise(FileName::filename(), Start::Pos, End::Pos, ParName::string(), SearchPaths::[string()])-> term()
%%         Pos = {integer(), integer()}

generalise(FileName, Start, End, ParName, SearchPaths) ->
    refac_gen:generalise(FileName, Start, End, ParName,  SearchPaths).


generalise_eclipse(FileName, Start, End, ParName, SearchPaths) ->
    refac_gen:generalise_eclipse(FileName, Start, End, ParName,  SearchPaths).

gen_fun_1_eclipse(SideEffect, FileName, ParName, FunName, Arity, DefPos, Expr) ->
    refac_gen:gen_fun_1_eclipse(SideEffect, FileName, ParName, FunName, Arity, DefPos, Expr).

gen_fun_2_eclipse(FileName, ParName, FunName, Arity, DefPos, Expr, SearchPaths) ->
    refac_gen:gen_fun_2_eclipse(FileName, ParName, FunName, Arity, DefPos, Expr, SearchPaths).

%% ================================================================================
%% @doc Move a function definition from its current module to another module.
%% <p> To apply this refactoring, point the cursor at the function definition, then 
%% select <em> Move Definition to Another Module</em> from the <em> Refactor </em> menu, 
%% after that the refactorer will prompt to enter the target module name in the mini-buffer. 
%% </p>
%% <p> This refactoring has a global effect, i.e., it affects all the modules in which 
%%     the function is imported/used.
%% </p>
%% <p> This refactoring assumes that an Erlang module name always matches it file name.
%% </p>
%% <p> Suppose we move functin <em> foo/n </em> from its current module <em> M </em> 
%%     to module <em> N </em>, then the following <em> side-conditions </em> apply to 
%%     this refactoring: 
%% <li> If <em> foo/n </em> is already in scope in module <em> N </em>, then its defining 
%%      module should be  <em> M </em>.
%% </li>
%% <li> Function <em> foo/n </em> should not contain any uses of <em> implicit fun expressions </em> (Note: move a 
%% collection of modules together to another module will be supported by another refactoring).
%% </li>
%% </p>
%% ===================================================================================
%% @spec move_fun(FileName::filename(),Line::integer(),Col::integer(),ModName::string(), 
%%                CreateNewFile::boolean(),SearchPaths::[string()])-> term()
%%         
move_fun(FileName, Line, Col, TargetModName, CreateNewFile, SearchPaths) ->
    refac_move_fun:move_fun(FileName, Line, Col, TargetModName, CreateNewFile, SearchPaths).



move_fun_eclipse(FileName, Line, Col, TargetModName, CreateNewFile, SearchPaths) ->
    refac_move_fun:move_fun_eclipse(FileName, Line, Col, TargetModName, CreateNewFile, SearchPaths).


%% ==================================================================================
%% @doc The duplicated code detector that only works with the current Erlang buffer.
%% <p> This function reports the duplicated code fragments found in the current Erlang buffer. It does 
%% not remove those code clones. The user will be prompted for two parameters: the minimum number of 
%% tokens the a code clone should have, and the minimum number of the times a code fragment has been 
%% duplicated.
%% </p>
%% <p> The current version of the duplicated code detector can report clones that are syntactically 
%% identical after consistent variable renaming, except for variations in literals, layout and comments.
%% </p>
%% =====================================================================================
%% @spec duplicated_code(FileName::filename(),MinToks::integer(),MinClones::integer()) -> term()
%%                
duplicated_code(FileName, MinToks, MinClones) -> 
    refac_duplicated_code:duplicated_code([FileName], MinToks, MinClones).


%% =====================================================================================
%% @doc The duplicated code detector that works with multiple Erlang modules.
%% <p> This is the duplicated code detector that works with multiple Erlang modules, which should be used from 
%% the command line.</p>
%% <p> This function only reports the found duplicated code fragments. It does not remove them.
%% The user have to  will supply three parameters: the lists of Erlang files to check,  the minimum number of 
%% tokens the a code clone should have, and the minimum number of the times a code fragment has been 
%% duplicated.
%% </p>
%% <p> The current version of the duplicated code detector can report clones that are syntactically 
%% identical after consistent variable renaming, except for variations in literals, layout and comments.
%% </p>
%%======================================================================================
%% @spec clone_detector(FileNameList::[filename()], MinToks::integer(), MinClones::integer()) -> term()

clone_detector(FileNameList, MinToks, MinClones) ->
    refac_duplicated_code:duplicated_code(FileNameList, MinToks, MinClones).
    

%% ==================================================================================================
%% @doc Search for clones of a user-selected expression/expression sequence in the current file.
%% 
%% <p> This functionality allows to search for clones of a selected expression or expression 
%% sequence.  The found clones are syntactically identical to the user-selected code fragment after consistent variable 
%% renaming, except for variations in literals, layout and comments. 
%% </p>
%% <p> When the selected code contains multiple, but non-continuous sequence of, expressions, the first
%% continuous sequence of expressions is taken as the user-selected expression. A continuous sequence of
%% expressions is a sequence of expressions separated by ','. </p>
%% ==================================================================================================
%% @spec expression_search(FileName::filename(),Start::Pos, End::Pos) -> term()

expression_search(FileName, Start, End) ->
    refac_expr_search:expr_search(FileName, Start, End).



%% =====================================================================================================
%%@doc Introduce a new function to represent a user-selected expression or expression sequence.
%% <p> This refactoring allows the user to introduce a new function to represent a selected expression or expression 
%% sequence, and replace the selected expression/expression sequence with a call to the newly introduced function.  Those free variables
%% within the expression/expression sequence become the formal parameters of the function. </p>
%% =====================================================================================================
%% @spec fun_extraction(FileName::filename(), Start::Pos, End::Pos, FunName::string()) -> term()
fun_extraction(FileName, Start, End, FunName) -> 
    refac_new_fun:fun_extraction(FileName, Start, End, FunName).

fun_extraction_eclipse(FileName, Start, End, FunName) -> 
    refac_new_fun:fun_extraction_eclipse(FileName, Start, End, FunName).

%% =============================================================================================
%% @doc Fold expressions against a function definition.
%% This refactoring replaces instances of the right-hand side of a function clause definition by
%% the corresponding left-hand side with necessary parameter substitutions.

%% <p> To apply this refactoring, move the cursor to the function clause against which expressions 
%% will be folded, then select <em> Fold Expression Against Function</em> from the <em> Refactor</em>
%% menu, after that the refactor will search the current module for expressions which are instances 
%% of the right-hand side of the selected function clause. </p>
%%
%% <p> If no candidate expression has been found, a message will be given, and the refactoring 
%% finishes; otherwise, Wrangler will go through the found candidate expressions one by one, and ask 
%% the user whether she/he wants to replace the expression with an application of selected function.
%% If the user answers 'yes' to one instance,  that instance will be replaced by function application,
%% otherwise it will remain unchanged. </p>
%%
%% <p> In the case that a candidate expression/expression sequence  need to export some variables which 
%% are used by the following code, that expression/expression sequence will be replaced by a match 
%% expression, whose left-hand side it the exported variable(s), and right-hand side is the function
%% application.</p>
%% 
%% <p> This refactoring does not support folding against function clauses with guard expressions, or 
%% function clauses with complex formal parameters, such as tuples, lists, or records. </p>

%% This refactoring currrently only works with a single module, but will be extended to multiple modules.
%% =============================================================================================
%% @spec fold_expression(FileName::filename(), Line::integer(), Col::integer())-> term()
fold_expression(FileName, Line, Col) ->
    refac_fold_expression:fold_expression(FileName, Line, Col).


fold_expression_eclipse(FileName, Line, Col) ->
    refac_fold_expression:fold_expression_eclipse(FileName, Line, Col).

fold_expression_1_eclipse(FileName, FunClauseDef, StartEndExpList)->  %% StartEndExpList: {{{StartLine, StartCol}, {EndLine, EndCol}}, NewExp}
    refac_fold_expression:fold_expression_1_eclipse(FileName, FunClauseDef, StartEndExpList).

fold_expression_2_eclipse(FileName, FunName, Arity, ClauseIndex, StartLine) ->
    refac_fold_expression:fold_expression_2_eclipse(FileName, FunName, Arity, ClauseIndex, StartLine).

instrument_prog(FileName, SearchPaths) ->
    refac_instrument:instrument_prog(FileName, SearchPaths).


uninstrument_prog(FileName, SearchPaths) ->
    refac_instrument:uninstrument_prog(FileName, SearchPaths).


add_a_tag(FileName, Line, Col, Tag, SearchPaths) ->
    refac_add_a_tag:add_a_tag(FileName, Line, Col, Tag, SearchPaths).


register_pid(FileName, Start, End, RegName, SearchPaths) ->
    refac_register_pid:register_pid(FileName, Start, End, RegName, SearchPaths).
    

trace_send({ModName, FunName, Arity}, Index, Pid, TraceCacheFile) ->
    PInfo = erlang:process_info(Pid),
    {value, InitialCall} = lists:keysearch(initial_call, 1, PInfo),
    {value, CurrentFun} = lists:keysearch(current_function,1, PInfo),
    SendInfo =case lists:keysearch(registered_name, 1, PInfo) of 
		  {value, RegisteredName} ->
		      {send, {ModName, FunName, Arity, Index}, Pid, InitialCall, CurrentFun, RegisteredName};
		  false ->
		      {send, {ModName, FunName, Arity, Index}, Pid, InitialCall, CurrentFun}
	      end,
    case dets:open_file(TraceCacheFile, [{type, bag}]) of 
	{ok, TraceCacheFile} -> 
	   dets:insert(TraceCacheFile, SendInfo),
	   dets:close(TraceCacheFile);
	{error, Reason}  -> eralng:error(Reason)
    end.


trace_spawn({ModName, FunName, Arity}, Index, Pid, TraceCacheFile) ->
    SpawnInfo = {spawn, {ModName, FunName, Arity, Index}, Pid},
    case dets:open_file(TraceCacheFile, [{type, bag}]) of 
	{ok, TraceCacheFile} -> 
	    dets:insert(TraceCacheFile, SpawnInfo),
	    dets:close(TraceCacheFile);
	{error, Reason}  -> eralng:error(Reason)
    end.    


%%=========================================================================================
%% @doc Some consecutive arguments of a function are contracted into a tuple
%% <p> To apply this refactoring, point the cursor to a function parameter, 
%% or an application argument.
%% Then select <em> Tuple Function Arguments </em> from the <em> Refactor </em> menu, 
%% after that the refactorer will prompt to enter  the new tuple elements number in the mini-buffer.
%% </p>
%% <p>
%% When tupling an exported function parameters, this refactoring has a global effect, i.e.,
%% it affects all those modules in which this function is imported/used.
%% </p>
%% <p> The following <em> side-conditions </em> apply to this refactoring:
%% <li> The new function arity should not cause confliction with any of the functions which are in scope in the 
%% current module;</li>
%% <li> In the case that the function is imported by another module, the new function arity and the same name 
%% should not be already in scope (either defined or imported) in that module. </li>
%% </p>
%% ========================================================================================
%% @spec tuple_funpar(FileName::filename(), Line::integer(), Col::integer(), Number::string(), SearchPaths::[string()])
%% -> term()
tuple_funpar(FileName, Line, Col, Number, SearchPaths) ->
    ref_tuple:tuple_funpar(FileName, Line, Col, list_to_integer(Number), SearchPaths).

tuple_funpar_eclipse(FileName, Line, Col, Number, SearchPaths) ->
    ref_tuple:tuple_funpar_eclipse(FileName, Line, Col, list_to_integer(Number), SearchPaths).


%%=========================================================================================
%% @doc A record expression created from the selected tuple.
%% <p> To apply this refactoring mark the tuple in the editor, which is a function 
%% parameter or an application argument.
%% Then select <em> From Tuple To Record </em> from the <em> Refactor </em> menu, 
%% after that the refactorer will prompt to enter the record name and  the record field names.
%% </p>
%% <p>
%% This refactoring has a global effect, i.e., it affects all those modules in 
%% which this function is imported/used.
%% </p>
%% <p>
%% WARNING: After the transformation please check the implicit functions, 
%% and create the record if it is necessary!
%% </p>
%% <p> The following <em> side-conditions </em> apply to this refactoring:
%% <li> The record and field names must be legal names; </li>
%% <li> The number of record fields must equal to the selected tuple size; </li>
%% <li> The function definition must defined in the current module; </li>
%% <li> The selected part must be a tuple.  </li>
%% </p>
%% ========================================================================================
%% @spec tuple_to_record(File::string(),FLine::integer(),FCol::integer(),
%%           LLine::integer(),LCol::integer(), RecName::string(),
%%           FieldString::[string()], SearchPaths::[string()]) -> term()
%% @end
%% ========================================================================================
tuple_to_record(File,FLine,FCol,LLine,LCol,RecName,FieldString,SearchPaths)->
    ref_tuple_to_record:tuple_to_record(File, FLine, FCol, LLine, LCol, 
                RecName, FieldString, SearchPaths).

tuple_to_record_eclipse(File,FLine,FCol,LLine,LCol,RecName,FieldString,SearchPaths)->
    ref_tuple_to_record:tuple_to_record_eclipse(File, FLine, FCol, LLine, LCol, 
                RecName, FieldString, SearchPaths).


fun_to_process(FileName, Line, Col, ProcessName, SearchPaths) ->
    refac_fun_to_process:fun_to_process(FileName, Line, Col, ProcessName, SearchPaths).
