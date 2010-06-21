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
%% =====================================================================
%%                      Wrangler's Code Inspector.
%% =====================================================================

%% @copyright 2006-2011 Huiqing Li, Simon Thompson
%%
%% @author Huiqing Li, Simon Thompson
%%   [http://www.cs.kent.ac.uk/projects/wrangler]
%%
%% @version 0.1
%% @end
%%
%% @doc This module describes the code inspection functions 
%%      that are currently supported by Wrangler. 

-module(wrangler_code_inspector).

-export([find_var_instances/3,
	 find_var_instances/5,
	 
         nested_exprs/3,
	 nested_exprs_in_file/5, 
	 nested_exprs_in_dirs/4,
	 nested_exprs_in_file_eclipse/5,
	 nested_exprs_in_dirs_eclipse/4,

	 long_functions/2,
	 long_functions_in_file/4, 
	 long_functions_in_dirs/3,
	 long_functions_in_file_eclipse/4, 
	 long_functions_in_dirs_eclipse/3,

         large_modules/2,
	 large_modules/3, 
	 large_modules_eclipse/3,

	 calls_to_fun/4,
	 calls_to_fun/5, 
	 calls_to_fun_eclipse/5,

         dependencies_of_a_module/2,
	 dependencies_of_a_module_eclipse/2,

	 non_tail_recursive_servers/1,
	 non_tail_recursive_servers_in_file/3,
	 non_tail_recursive_servers_in_dirs/2,
	 non_tail_recursive_servers_in_file_eclipse/3, 
	 non_tail_recursive_servers_in_dirs_eclipse/2,

         not_flush_unknown_messages/1,
	 not_flush_unknown_messages_in_file/3,
	 not_flush_unknown_messages_in_dirs/2,
	 not_flush_unknown_messages_in_file_eclipse/3,
	 not_flush_unknown_messages_in_dirs_eclipse/2,

	 gen_function_callgraph/3,
	 gen_module_graph/3,
	 cyclic_dependent_modules/3,
	 improper_inter_module_calls/2]).

-export([nested_exprs_1/5,
	 long_functions_1/4,
	 large_modules_1/3,
	 non_flush_unknown_messages_1/3,
	 non_tail_recursive_servers_1/3,
	 dependencies_of_a_module_1/2]).  

-include("../include/wrangler.hrl").

%% @type filename()=string().
%% @type modulename()=atom().
%% @type functionname()=atom().
%% @type functionarity()=integer().
%% @type dir()=string().
%% @type pos()={integer(), integer()}.
%% @type line()=integer().
%% @type col()=integer().


%%==========================================================================================
%% @doc Find all the instances of a variable, including both defining and use instances.
%%@spec find_var_instances(FileName::filename(), {Line::integer(), Col::integer()}, 
%%			 TabWidth:: integer()) ->
%%	     {error, string()} | {ok, [{pos(), pos()}], [pos()]}
-spec(find_var_instances(FileName::filename(), {Line::integer(), Col::integer()}, 
			 TabWidth:: integer()) ->
	     {error, string()} | {ok, [{pos(), pos()}], [pos()]}).
find_var_instances(FileName, {Line, Col},TabWidth) ->
   try_inspector(wrangler_code_inspector_lib, find_var_instances, 
		  [FileName, Line, Col, [], TabWidth]).

%% @private interface function for Emacs & Eclipse.
%% This function return all the places where the variable selected occurs. With 
%% Emacs, these places are hightlighted, but defining and use occurrances hightlighted
%% in different color.
-spec(find_var_instances(FileName::filename(), Line::integer(), Col::integer(), 
			 SearchPaths::[dir()], TabWidth:: integer()) ->
	     {error, string()} | {ok, [StartEnd::{pos(), pos()}], [VarDefPos::pos()]}).
find_var_instances(FileName, Line, Col, SearchPaths, TabWidth) ->
    try_inspector(wrangler_code_inspector_lib, find_var_instances, 
		  [FileName, Line, Col, SearchPaths, TabWidth]).


%%==========================================================================================
%% @doc Find all the functions that contain certain type of expressions that are nested a 
%% specified number of levels or more.
%% @end
%% Command line API.
%%@spec nested_exprs(FileOrDirNames::[filename()|dir()], NestLevel::integer(), 
%%		   ExprType::'if'|'case'|'receive') 
%%      ->{error, string()} | ok
-spec(nested_exprs(FileOrDirNames::[filename()|dir()], NestLevel::integer(), 
		   ExprType::'if'|'case'|'receive') 
      ->{error, string()} | ok).
nested_exprs(FileOrDirNames, NestLevel, ExprType) ->
    try_inspector(wrangler_code_inspector, nested_exprs_1,
		  [FileOrDirNames, NestLevel, ExprType, FileOrDirNames, ?DEFAULT_TABWIDTH]).

%% @private  interface function for emacs.
-spec(nested_exprs_in_file(FileName::filename(), NestLevel::string(), 
			   ExprType::'if'|'case'|'receive',
			   SearchPaths::[dir()], TabWidth::integer()) ->
	     {error, string()} | ok).
nested_exprs_in_file(FileName, NestLevel, ExprType, SearchPaths, TabWidth) ->
    try
	list_to_integer(NestLevel)
    of
	NestLevel1 when NestLevel1 >= 0 ->
	    try_inspector(wrangler_code_inspector, nested_exprs_1, 
			  [[FileName], NestLevel1, ExprType, SearchPaths, TabWidth]);
	_ -> {error, "Invalid nest level!"}
    catch
	_:_ -> {error, "Invalid nest level!"}
    end.

%% @private interface function for emacs.
-spec(nested_exprs_in_dirs(NestLevel::string(), ExprType::'if'|'case'|'receive',
			      SearchPaths::[dir()],TabWidth::integer()) ->
	     {error, string()} | ok).
nested_exprs_in_dirs(NestLevel, ExprType, SearchPaths, TabWidth) ->
    try list_to_integer(NestLevel)  of
      NestLevel1 when NestLevel1 >= 0 ->
	    try_inspector(wrangler_code_inspector, nested_exprs_1, 
			[SearchPaths, NestLevel1, ExprType, SearchPaths, TabWidth]);
      _ -> {error, "Invalid nest level!"}
    catch
      _:_ -> {error, "Invalid nest level!"}
    end.

%%@private
nested_exprs_1(Files, NestLevel, ExprType, SearchPaths, TabWidth) ->
    {ok, Funs} = wrangler_code_inspector_lib:nested_exprs(
		   Files, NestLevel, ExprType, SearchPaths, TabWidth),
    format_nested_expr_result(Funs, NestLevel, ExprType).
	

%% @private  interface function for eclipse.
-spec(nested_exprs_in_file_eclipse(FileName::filename(), NestLevel::integer(), 
				   ExpType::'if'|'case'|'receive',
				   SearchPaths::[dir()], TabWidth::integer()) ->
	     {error, string()} | {ok, [{modulename(), functionname(), functionarity()}]}).
%% The value of 'NestLevel' is inputted by user.
nested_exprs_in_file_eclipse(FileName, NestLevel, ExprType, SearchPaths, TabWidth) ->
    try_inspector(wrangler_code_inspector_lib, nested_exprs,
		  [[FileName], NestLevel, ExprType, SearchPaths, TabWidth]).


%% @private interface function for eclipse.
-spec(nested_exprs_in_dirs_eclipse(NestLevel::integer(), ExprType::'if'|'case'|'receive',
				      SearchPaths::[dir()],TabWidth::integer()) ->
	     {error, string()} | {ok, [{modulename(), functionname(), functionarity()}]}).
%% The value of 'NestLevel' is inputted by user.
nested_exprs_in_dirs_eclipse(NestLevel, ExprType, SearchPaths, TabWidth) ->
    try_inspector(wrangler_code_inspector_lib, nested_exprs,
		  [SearchPaths, NestLevel, ExprType, SearchPaths, TabWidth]).

%%==========================================================================================
%% @doc Report all the functions that consist of more than specified number of lines of code.          
%%@spec long_functions(FileOrDirNames::[filename()|dir()], Lines::integer()) ->
%%	     {error, string()} | ok
%% @end
%% Command line API.
-spec(long_functions(FileOrDirNames::[filename()|dir()], Lines::integer()) ->
	     {error, string()} | ok).
long_functions(FileOrDirNames, Lines) ->
    try_inspector(wrangler_code_inspector, long_functions_1, 
		  [FileOrDirNames, Lines, FileOrDirNames,?DEFAULT_TABWIDTH]).

%% @private interface function for emacs.
-spec(long_functions_in_file(FileName::filename(), Lines::string(), SearchPaths::[dir()], 
			     TabWidth::integer()) -> {error, string()} | ok).
long_functions_in_file(FileName, Lines, SearchPaths, TabWidth) ->
    try
	list_to_integer(Lines)
    of
	Lines1 when Lines > 0 ->
	    try_inspector(wrangler_code_inspector, long_functions_1, 
			  [[FileName], Lines1, SearchPaths, TabWidth]);	
        _ -> {error, "Invalid number of lines!"}		
    catch
	_:_ ->
	    {error, "Invalid number of lines"}
    end.


%% @private interface function for emacs.
-spec(long_functions_in_dirs(Lines::string(), SearchPaths::[dir()], TabWidth::integer()) ->
	     {error, string()} | ok).
long_functions_in_dirs(Lines, SearchPaths, TabWidth) ->
    try
	list_to_integer(Lines)
    of
      Lines1 when Lines > 0 ->
	  try_inspector(wrangler_code_inspector, long_functions_1,
			[SearchPaths, Lines1, SearchPaths, TabWidth]);
      _ -> {error, "Invalid number of lines!"}
    catch
      _:_ ->
	  {error, "Invalid number of lines!"}
    end.

%%@private
long_functions_1(Files, Lines, SearchPaths, TabWidth) ->
    {ok, LongFuns} = wrangler_code_inspector_lib:long_functions(Files, Lines, SearchPaths, TabWidth),
    long_funs_format_results(LongFuns, Lines).
	
    
%% @private interface function for Eclipse.
-spec(long_functions_in_file_eclipse(FileName::filename(), Lines::integer(), SearchPaths::[dir()], 
			     TabWidth::integer()) -> 
	     {error, string()} | {ok, [{modulename(), functionname(), functionarity()}]}).
%% The value of 'Lines' is inputted by user.
long_functions_in_file_eclipse(FileName, Lines, SearchPaths, TabWidth) ->
    try_inspector(wrangler_code_inspector_lib, long_functions,
		  [[FileName], Lines, SearchPaths, TabWidth]).

%% @private interface function for Eclipse.
-spec(long_functions_in_dirs_eclipse(Lines::integer(), SearchPaths::[dir()], TabWidth::integer()) ->
	     {error, string()} | {ok, [{modulename(), functionname(), functionarity()}]}).
%% The value of 'Lines' is inputted by user.
long_functions_in_dirs_eclipse(Lines, SearchPaths, TabWidth) ->
    try_inspector(wrangler_code_inspector_lib,
		  long_functions,
		  [SearchPaths, Lines, SearchPaths, TabWidth]).

%%==========================================================================================
%% @doc Report all the modules that consist of more than a specified number of lines of code.
%% @spec large_modules(SearchPaths::[dir()],Lines::integer()) ->				  
%%	     {error, string()} | ok
%% @end
%% Command line API.
-spec(large_modules(SearchPaths::[dir()],Lines::integer()) ->				  
	     {error, string()} | ok).
large_modules(SearchPaths, Lines) ->
    try_inspector(wrangler_code_inspector, large_modules_1,
		  [Lines, SearchPaths, ?DEFAULT_TABWIDTH]).


%%@private interface function for emacs.
-spec(large_modules(Lines::string(), SearchPaths::[dir()], TabWidth::integer()) ->				  
	     {error, string()} | ok).
large_modules(Lines, SearchPaths, TabWidth) ->
    try
      list_to_integer(Lines)
    of
      Lines1 ->
	  case Lines1 >= 0 of
	    true -> 
		  try_inspector(wrangler_code_inspector, large_modules_1,
				[Lines1, SearchPaths, TabWidth]);
	    false -> {error, "Invalid number of lines!"}
	  end
    catch
      _:_ ->
	  {error, "Invalid number of lines!"}
    end.
%%@private
large_modules_1(Lines,SearchPaths, TabWidth) ->
    {ok, LargeMods} = wrangler_code_inspector_lib:large_modules(Lines, SearchPaths, TabWidth),
    large_modules_format_results(LargeMods, Lines).

%%@private interface function for Eclipse.
%% The value of 'Lines' is inputted by user.
-spec(large_modules_eclipse(Lines::integer(), SearchPaths::[dir()], TabWidth::integer()) ->				  
	     {error, string()} | {ok, [modulename()]}).
large_modules_eclipse(Lines, SearchPaths, TabWidth) ->
    try_inspector(wrangler_code_inspector_lib, large_modules, [Lines, SearchPaths, TabWidth]).


%%=========================================================================================
%% @doc Report the functions by which the function specified is called. A function is reported 
%% only if Wrangler is certain that it calls the function specified.
%% @spec calls_to_fun(FileName::filename(), FunctionName::functionname(), Arity::integer(), SearchPaths::[dir()]) ->
%%	     {error, string()} | ok
%%@end
%% Command line API. 
-spec(calls_to_fun(FileName::filename(), FunctionName::functionname(), Arity::integer(), SearchPaths::[dir()]) ->
	     {error, string()} | ok).
calls_to_fun(FileName, FunctionName, Arity, SearchPaths) ->
    case try_inspector(wrangler_code_inspector_lib, calls_to_fun_1, 
		  [FileName, FunctionName, Arity, SearchPaths, ?DEFAULT_TABWIDTH]) of
	{ok, MFAs} ->
	    calls_to_fun_format_results(MFAs);
	Other ->
	    Other
    end.
	    
     

%% @private  interface function for emacs. 
-spec(calls_to_fun(FileName::filename(), Line::integer(), Col::integer(), SearchPaths::[dir()], TabWidth::integer()) ->
	     {error, string()} | ok).
calls_to_fun(FileName, Line, Col, SearchPaths, TabWidth) ->
    case try_inspector(wrangler_code_inspector_lib, calls_to_fun,
		       [FileName, Line, Col, SearchPaths, TabWidth]) of
	{ok, MFAs} ->
	    calls_to_fun_format_results(MFAs);
	Other ->
	    Other
    end.

%% @private  Interface function for Eclipse. 
-spec(calls_to_fun_eclipse(FileName::filename(), Line::integer(), Col::integer(), SearchPaths::[dir()], TabWidth::integer()) ->
	     {error, string()} | {ok, {[{filename(), functionname(), functionarity()}]}}).
calls_to_fun_eclipse(FileName, Line, Col, SearchPaths, TabWidth) ->
    try_inspector(wrangler_code_inspector_lib, calls_to_fun,
		  [FileName, Line, Col, SearchPaths, TabWidth]).

  
%%==========================================================================================
%%@doc Report modules that depend on the module specified, as well as modules on which the 
%%      module specified in dependent.
%%@spec dependencies_of_a_module (modulename()|filename(), [filename()|dir()]) -> ok
%%@end
%% Command line API and interface function for emacs.
-spec(dependencies_of_a_module(ModOrFileName::modulename()|filename(), 
			       SearchPahts::[filename()|dir()]) -> {error, string()}|ok).
dependencies_of_a_module(ModOrFileName, SearchPaths) ->
    try_inspector(wrangler_code_inspector, dependencies_of_a_module_1, [ModOrFileName, SearchPaths]).

%%@private
dependencies_of_a_module_1(ModOrFileName, SearchPaths) ->
    {ok, {ClientMods, ServerMods}} = wrangler_code_inspector_lib:dependencies_of_a_module(ModOrFileName, SearchPaths),
    module_deps_result(ClientMods, ServerMods).

    
%% @private  Interface function for eclipse. 
-spec(dependencies_of_a_module_eclipse(FileName::filename(), SearchPaths::[dir()]) -> 
	     {error, string} |{ok, {[modulename()], [modulename()]}}).
dependencies_of_a_module_eclipse(FileName, SearchPaths) ->
    try_inspector(wrangler_code_inspector_lib, dependencies_of_a_module, [FileName, SearchPaths]).


%%==========================================================================================
%% @doc Report non tail-recursive server functions.
%% <p>
%% The Erlang Programming Rules says:
%% All servers must be tail-recursive, otherwise the server will consume memory until the system runs out of it.
%%
%% Don't program like this:
%%
%% ``` loop() ->
%%       receive
%%         {msg1, Msg1} -> 
%%           ...,
%%           loop();
%%         stop ->
%%           true;
%%         Other ->
%%           error_logger:log({error, {process_got_other, self(), Other}}),
%%           loop()
%%       end,
%%       io:format("Server going down").                 
%%                 
%% The above is not tail-recusive. This is a correct solution:
%%
%%     loop() ->
%%       receive
%%         {msg1, Msg1} -> 
%%           ...,
%%           loop();
%%         stop ->
%%           io:format("Server going down");
%%         Other ->
%%           error_logger:log({error, {process_got_other, self(), Other}}),
%%           loop()
%%       end. 
%% '''
%% </p>
%%@spec non_tail_recursive_servers(FileOrDirNames::[filename()|dir()])-> ok
%%@end
%% command line API.
-spec(non_tail_recursive_servers(FileOrDirNames::[filename()|dir()])-> ok).
non_tail_recursive_servers(FileOrDirNames) ->
    try_inspector(wrangler_code_inspector, non_tail_recursive_servers_1,
		  [refac_util:expand_files(FileOrDirNames, ".erl"), 
		   FileOrDirNames, ?DEFAULT_TABWIDTH]).
 

%%@private interface function for emacs.
-spec(non_tail_recursive_servers_in_file(FileName::filename(), SearchPaths::[dir()], TabWidth::integer()) -> ok).
non_tail_recursive_servers_in_file(FileName, SearchPaths, TabWidth) ->
    try_inspector(wrangler_code_inspector, non_tail_recursive_servers_1,
		  [[FileName], SearchPaths, TabWidth]).

%%@private interface function for emacs.
-spec(non_tail_recursive_servers_in_dirs(SearchPaths::[dir()], TabWidth::integer()) -> ok).
non_tail_recursive_servers_in_dirs(SearchPaths, TabWidth) ->
    try_inspector(wrangler_code_inspector, non_tail_recursive_servers_1,
		  [SearchPaths, SearchPaths, TabWidth]).

%%@private
non_tail_recursive_servers_1(Files, SearchPaths, TabWidth) ->
    {ok, Funs} = wrangler_code_inspector_lib:non_tail_recursive_servers(Files, SearchPaths, TabWidth),
    non_tail_format_results(Funs).
 
%%@private Interface function for eclipse.
-spec(non_tail_recursive_servers_in_file_eclipse(FileName::filename(), SearchPaths::[dir()], TabWidth::integer()) -> 
	     {ok, [{modulename(), functionname(), functionarity()}]}).
non_tail_recursive_servers_in_file_eclipse(FileName, SearchPaths, TabWidth) ->
    try_inspector(wrangler_code_inspector_lib, non_tail_recursive_servers,
		  [[FileName], SearchPaths, TabWidth]).
   
%%@private Interface function for eclipse.
-spec(non_tail_recursive_servers_in_dirs_eclipse(SearchPaths::[dir()], TabWidth::integer()) -> 
	     {ok, [{modulename(), functionname(), functionarity()}]}).
non_tail_recursive_servers_in_dirs_eclipse(SearchPaths, TabWidth) ->
    try_inspector(wrangler_code_inspector_lib,
		  non_tail_recursive_servers,[SearchPaths, SearchPaths, TabWidth]).

 
%%==========================================================================================
%%@doc Report functions whose receive expression, if there is one,  does not flush unknown messages.
%% <p>
%% The Erlang Programming Rules says:
%% Every server should have an Other alternative in at least one receive statement.
%% This is to avoid filling up message queues. Example:
%%
%%```  main_loop() ->
%%       receive
%%         {msg1, Msg1} -> 
%%           ...,
%%           main_loop();
%%         {msg2, Msg2} ->
%%           ...,
%%           main_loop();
%%         Other -> % Flushes the message queue.
%%           error_logger:error_msg(
%%               "Error: Process ~w got unknown msg ~w~n.", 
%%               [self(), Other]),
%%           main_loop()
%%       end.
%% '''
%%</p>
%%@spec not_flush_unknown_messages(FileOrDirNames::[filename()|dir()]) -> ok   
%%@end
%%command line API.
-spec(not_flush_unknown_messages(FileOrDirNames::[filename()|dir()]) -> ok).    
not_flush_unknown_messages(FileOrDirNames) ->
    try_inspector(wrangler_code_inspector, non_flush_unknown_messages_1,
		  [FileOrDirNames, FileOrDirNames, ?DEFAULT_TABWIDTH]).

%%@private interface function for emacs.
-spec(not_flush_unknown_messages_in_file(FileName::filename(), SearchPaths::[dir()], TabWidth::integer()) -> ok).    
not_flush_unknown_messages_in_file(FileName, SearchPaths, TabWidth) ->
    try_inspector(wrangler_code_inspector, non_flush_unknown_messages_1, 
		  [[FileName], SearchPaths, TabWidth]).
    
%%@private Interface function for emacs.
-spec(not_flush_unknown_messages_in_dirs(SearchPaths::[dir()], TabWidth::integer()) -> ok).
not_flush_unknown_messages_in_dirs(SearchPaths, TabWidth) ->
    try_inspector(wrangler_code_inspector, non_flush_unknown_messages_1,
		  [SearchPaths, SearchPaths, TabWidth]).
%%@private
non_flush_unknown_messages_1(Files, SearchPaths, TabWidth) ->
    {ok, Funs} = wrangler_code_inspector_lib:not_flush_unknown_messages(
		   Files, SearchPaths,TabWidth),
    non_flush_format_result(Funs).

%%@private Interface function for eclipse.
-spec(not_flush_unknown_messages_in_file_eclipse(FileName::filename(), SearchPaths::[dir()], TabWidth::integer())
      -> {error, string()}|{ok, [{modulename(), functionname(), functionarity()}]}).
not_flush_unknown_messages_in_file_eclipse(FileName, SearchPaths, TabWidth) ->
    try_inspector(wrangler_code_inspector_lib, not_flush_unknown_messages,
		  [[FileName], SearchPaths, TabWidth]).
  
%%@private Interface function for eclipse.
-spec(not_flush_unknown_messages_in_dirs_eclipse(SearchPaths::[dir()], TabWidth::integer()) 
      ->{error, string()}|{ok, [{modulename(), functionname(), functionarity()}]}).
not_flush_unknown_messages_in_dirs_eclipse(SearchPaths, TabWidth) ->
    try_inspector(wrangler_code_inspector_lib, not_flush_unknown_messages,
		  [SearchPaths, SearchPaths, TabWidth]).
   

%%==========================================================================================
%% @doc Generate the function callgraph for a given Erlang file.
%% @spec gen_function_callgraph (filename(), filename(),[filename()|dir()]) ->true
%% @end
%% command line API; also interface function for emacs and eclipse.
-spec(gen_function_callgraph/3::(filename(), filename(),[filename()|dir()]) ->true).
gen_function_callgraph(OutputDotFileName, FileName, SearchPaths)->
    try_inspector(wrangler_modularity_inspection, gen_function_callgraph, 
		  [OutputDotFileName, FileName, SearchPaths]).

%%==========================================================================================
%% @doc Generate the module graph for a given list of directories of Erlang source code. 
%% @spec gen_module_graph(filename(), [filename()|dir()], boolean()) ->true
%% @end
%% command line API, and interface function for emacs and eclipse.
%% 'OutputDotFileName' is a user selected .dot file name.
-spec (gen_module_graph/3::(filename(), [filename()|dir()], boolean()) ->true).
gen_module_graph(OutputDotFileName, SearchPaths, WithLabel) ->
    try_inspector(wrangler_modularity_inspection, gen_module_graph, 
		  [OutputDotFileName, [], SearchPaths, WithLabel]).

%%==========================================================================================
%% @doc Report the cyclic module dependencies, if there is any, for a given list of 
%%      directories of Erlang source code.
%%@spec cyclic_dependent_modules (filename(), [filename()|dir()], boolean()) ->true
%% @end 
%% command line API and interface function for emacs and eclipse.
%% 'OutputDotFileName' is a user selected .dot file name.
-spec(cyclic_dependent_modules/3::(filename(),[filename()|dir()], boolean()) ->true).
cyclic_dependent_modules(OutputDotFileName, SearchPaths, WithLabel)->
    try_inspector(wrangler_modularity_inspection, cyclic_dependent_modules,
		  [OutputDotFileName, SearchPaths, WithLabel]).


%%==========================================================================================
%% @doc Report improper module dependencies, if there is any, for a given list of 
%%      directories of Erlang source code.
%%@spec improper_inter_module_calls (filename(), [filename()|dir()]) ->true
%% @end 
%% command line API; Interface function for emacs and eclipse.
%% 'OutputDotFileName' is a user selected .dot file name.
-spec(improper_inter_module_calls/2::(filename(), [filename()|dir()]) ->true).
improper_inter_module_calls(OutputDotFileName, SearchPaths)->
    try_inspector(wrangler_modularity_inspection, improper_inter_module_calls, 
		  [OutputDotFileName, SearchPaths]).

%%==========================================================================================
%%             Some internal functions.
%%==========================================================================================
format_nested_expr_result(Funs, NestLevel, ExprType) ->
    case Funs of
      [] ->
	  ?wrangler_io("\nNo function with ~p expressions nested ~p or more levels has been found.\n",
		       [ExprType, NestLevel]);
      _ -> ?wrangler_io("\nThe following function(s) contains ~p expressions nested ~p or more levels:\n",
			[ExprType, NestLevel]),
	   format_result_1(Funs)
    end.


format_result_1([]) ->
    ?wrangler_io(".\n",[]);
format_result_1([{M, F, A}|Fs]) ->
    case Fs of 
	[] -> ?wrangler_io("~p:~p/~p", [M, F, A]);
	_ -> ?wrangler_io("~p:~p/~p,\n", [M, F, A])
    end,
    format_result_1(Fs).


calls_to_fun_format_results(Callers)->
    case Callers of
	[] ->
	    ?wrangler_io("The function selected is not called by any other functions.\n", []);
	_ ->
	    ?wrangler_io("The function selected is called by the following function(s):\n", []),
	    lists:foreach(fun ({File, F, A}) -> ?wrangler_io("File:~p, function/arity: ~p/~p\n", [File, F, A]) end, Callers)
    end.

long_funs_format_results(LongFuns, Lines) ->
    case LongFuns of
	[] ->
	    ?wrangler_io("\n No Function in this module has more than ~p lines.\n",
		       [Lines]);
	_ ->
	    ?wrangler_io("\n The following function(s) have more than ~p lines of code:\n",
			 [Lines]),
	    format_result_1(LongFuns)
    end.

large_modules_format_results(LargeModules, Lines)->
    case LargeModules of
	[] ->
	    ?wrangler_io("\n No module with more than ~p lines of code has been found.\n", 
			 [Lines]);
	_ ->
	    ?wrangler_io("The following modules have more than ~p lines of code:\n",
			 [Lines]),
	    ?wrangler_io("~p\n", [LargeModules])
    end.

module_deps_result(ClientMods, ServerMods)->
    case ClientMods of
	[] -> ?wrangler_io("\nNo other modules directly depend on the this module.\n", []);
	_ -> ?wrangler_io("\nThe following modules directly depend on the this module:\n", []),
	     ?wrangler_io("~p\n", [ClientMods])
    end,
    case ServerMods of
	[] -> ?wrangler_io("\nThis module does not directly depend on any other modules.\n", []);
	_ -> ?wrangler_io("\nThis module directly depends on the following modules:\n", []),
	     ?wrangler_io("~p\n", [ServerMods])
    end.

non_tail_format_results(Funs) ->
    case Funs of
      [] ->
	  ?wrangler_io("\n No non-tail recursive server has been found.\n",[]);
      _ ->
	    ?wrangler_io("\n The following functions are recursive servers, but not tail-recursive:\n",[]),
	    format_result_1(Funs)    
    end.
       

non_flush_format_result(Funs) ->
    case Funs of
      [] ->
	    ?wrangler_io("\n No server without flushing unknown messages has been found.\n",[]);
	_ ->
	    ?wrangler_io("\n The following functions are servers without flush of unknown messages:\n",[]),
	    format_result_1(Funs)
    end.

%%@private
try_inspector(Mod, Fun, Args) -> 
    Msg ="Wrangler failed to perform this functionality, "  
	"please report error to erlang-refactor@kent.ac.uk.",
    try_to_apply(Mod, Fun, Args, Msg).


%%@private
try_to_apply(Mod, Fun, Args, Msg) -> 
    try apply(Mod, Fun, Args)
     catch
	 throw:Error -> 
	     Error;    %% wrangler always throws Error in the format of '{error, string()}';
	 _E1:_E2->
     	     %%refac_io:format("E1E2:\n~p\n", [{E1, E2}]),
	     {error, Msg}
     end.

