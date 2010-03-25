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
%% A module for detecting bad coding smells.
%%
%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%% 
%% =====================================================================


-module(wrangler_code_inspector).

-export([find_var_instances/5, 
	 nested_if_exprs_in_file/4, nested_case_exprs_in_file/4, nested_receive_exprs_in_file/4,
	 nested_if_exprs_in_dirs/3, nested_case_exprs_in_dirs/3, nested_receive_exprs_in_dirs/3,
	 caller_called_modules/3, caller_funs/5,
	 long_functions_in_file/4, long_functions_in_dirs/3,
	 large_modules/3, non_tail_recursive_servers_in_file/3, non_tail_recursive_servers_in_dirs/2,
	 not_flush_unknown_messages_in_file/3, not_flush_unknown_messages_in_dirs/2]).

-export([gen_module_graph/4, gen_module_scc_graph/3, gen_function_callgraph/3]).

-include("../include/wrangler.hrl").

-define (not_sure_atom,'*wrangler-not-able-to-decide*').
%%==========================================================================================
-spec(find_var_instances(FName::filename(), Line::integer(), Col::integer(), SearchPaths::[dir()], TabWidth:: integer()) ->
	     {error, string()} | {ok, [{pos(), pos()}], [pos()]}).
find_var_instances(FName, Line, Col, SearchPaths, TabWidth) ->
    {ok, {AnnAST, _Info0}} = refac_util:parse_annotate_file(FName, true, SearchPaths, TabWidth),
    case interface_api:pos_to_var_name(AnnAST, {Line, Col}) of
      {ok, {_VarName, DefinePos, _C}} ->
	  if DefinePos == [{0, 0}] ->
		 {error, "The identifier selected is a macro, or not defined!"};
	     true ->
		 F = fun (T, S) ->
			     case refac_syntax:type(T) of
			       variable ->
				   case lists:keysearch(def, 1, refac_syntax:get_ann(T)) of
				     {value, {def, DefinePos}} ->
					 Range = refac_misc:get_start_end_loc(T),
					 [Range| S];
				     _ -> S
				   end;
			       _ -> S
			     end
		     end,
		 Locs = lists:usort(refac_syntax_lib:fold(F, [], AnnAST)),
		 {ok, Locs, DefinePos}
	  end;
      {error, Reason} -> {error, Reason}
    end.

%%==========================================================================================
-spec(nested_if_exprs_in_file(FName::filename(), NestLevel::[integer()], SearchPaths::[dir()], TabWidth::integer()) ->
	     {error, string()} | ok).
nested_if_exprs_in_file(FName, NestLevel, SearchPaths, TabWidth) ->
    ?wrangler_io("\nCheck nested 'if' expressions in  current buffer ...\n",[]),
    nested_exprs([FName], NestLevel,if_expr, SearchPaths, TabWidth).


-spec(nested_if_exprs_in_dirs(NestLevel::[integer()], SearchPaths::[dir()], TabWidth::integer()) ->
	     {error, string()} | ok).
nested_if_exprs_in_dirs(NestLevel, SearchPaths, TabWidth) ->
    ?wrangler_io("\nCheck nested 'if' expressions in directories: \n~p\n", [SearchPaths]),
    nested_exprs(SearchPaths, NestLevel, if_expr, SearchPaths, TabWidth).
    
%%==========================================================================================
-spec(nested_case_exprs_in_file(FName::filename(), NestLevel::[integer()], SearchPaths::[dir()], TabWidth::integer()) ->
	     {error, string()} | ok).
nested_case_exprs_in_file(FName, NestLevel, SearchPaths, TabWidth) -> 
    ?wrangler_io("\nCheck nested 'case' expressions in  current buffer ...\n",[]),
    nested_exprs([FName], NestLevel, case_expr, SearchPaths, TabWidth).


-spec(nested_case_exprs_in_dirs(NestLevel::[integer()], SearchPaths::[dir()], TabWidth::integer()) ->
	     {error, string()} | ok).
nested_case_exprs_in_dirs(NestLevel, SearchPaths, TabWidth) ->
    ?wrangler_io("\nCheck nested 'case' expressions in directories: \n~p\n", [SearchPaths]),
    nested_exprs(SearchPaths, NestLevel, case_expr, SearchPaths, TabWidth).

%%==========================================================================================
-spec(nested_receive_exprs_in_file(FName::filename(), NestLevel::[integer()], SearchPaths::[dir()], TadWidth::integer()) ->
	     {error, string()} | ok).
nested_receive_exprs_in_file(FName, NestLevel, SearchPaths, TabWidth) ->
    ?wrangler_io("\nCheck nested 'receive' expressions in  current buffer ...\n",[]),
    nested_exprs([FName], NestLevel, receive_expr, SearchPaths, TabWidth).

-spec(nested_receive_exprs_in_dirs(NestLevel::[integer()], SearchPaths::[dir()], TabWidth::integer()) ->
	     {error, string()} |ok).
nested_receive_exprs_in_dirs(NestLevel, SearchPaths, TabWidth) ->
    ?wrangler_io("\nCheck nested 'receive' expressions in directories: \n~p\n", [SearchPaths]),
    nested_exprs(SearchPaths, NestLevel, receive_expr, SearchPaths, TabWidth).


%%==========================================================================================
nested_exprs(DirFileNames, NestLevel, ExprType, SearchPaths, TabWidth)->
    try list_to_integer(NestLevel) of 
	Val ->
	    case Val >0 of 
		true ->
		    Files = refac_util:expand_files(DirFileNames, ".erl"),
		    Funs = lists:flatmap(fun(F) ->
						 nested_exprs_1(F, Val, ExprType, SearchPaths, TabWidth)
					 end, Files),
		    format_result(Funs, Val, ExprType);
		_ -> {error, "Invalid nest level"}
	    end
    catch 
	_:_ -> {error, "Invalid nest level!"}
    end.

nested_exprs_1(FName, NestLevel, ExprType, SearchPaths, TabWidth) ->
    {ok, {AnnAST, Info}} = refac_util:parse_annotate_file(FName, true, SearchPaths, TabWidth),
    ModName = get_module_name(FName, Info),
    Fun = fun (T, S) ->
		  case refac_syntax:type(T) of
		    function ->
			FunName = refac_syntax:atom_value(refac_syntax:function_name(T)),
			Arity = refac_syntax:function_arity(T),
			Fun1 = fun (Node, S1) ->
				       case refac_syntax:type(Node) of
					 ExprType ->
					     Range = refac_misc:get_start_end_loc(Node),
					     [{ModName, FunName, Arity, Range}| S1];
					 _ -> S1
				       end
			       end,
			refac_syntax_lib:fold(Fun1, S, T);
		    _ -> S
		  end
	  end,
    Ranges = lists:usort(refac_syntax_lib:fold(Fun, [], AnnAST)),
    SortedRanges = sort_ranges(Ranges),
    ResRanges = lists:filter(fun (R) -> length(R) >= NestLevel end, SortedRanges),
    lists:usort(lists:map(fun (R) -> {M, F, A, _R} = hd(R), {M, F, A} end,
			  ResRanges)).
 
get_module_name(FName, Info) ->
    case lists:keysearch(module, 1, Info) of
	{value, {module, Mod}} -> Mod;
	_ -> list_to_atom(filename:basename(FName, ".erl"))
    end.

  
	
format_result(Funs, NestLevel, ExprType) -> 
    ExprType1 = case ExprType of 
		    case_expr -> 'case';
		    if_expr -> 'if';
		    receive_expr -> 'receive'
		end,
    case Funs of 
	[] -> ?wrangler_io("\nNo function with ~p expressions nested ~p or more levels has been found.\n", [ExprType1, NestLevel]);
	_ -> ?wrangler_io("\nThe following function(s) contains ~p expressions nested ~p or more levels:\n ", [ExprType1, NestLevel]),
	     format_result_1(Funs)
    end.

format_result_1([]) ->
    ?wrangler_io(".\n",[]);
format_result_1([{M, F, A}|Fs]) ->
    case Fs of 
	[] -> ?wrangler_io("~p:~p/~p", [M, F, A]);
	_ -> ?wrangler_io("~p:~p/~p,", [M, F, A])
    end,
    format_result_1(Fs).

	    
sort_ranges(Ranges) ->
    sort_ranges(Ranges, []).
sort_ranges([], Acc) ->
    Acc;
sort_ranges(Rs, Acc) ->
    [Hd|Tail] = Rs,
    Nested = get_enclosed([Hd], Tail),
    sort_ranges(Rs--Nested, [lists:reverse(Nested)|Acc]).

get_enclosed(Cur, Rs) ->
    Lst = hd(Cur),
    {_ModName1, _FName1, _Arity1, {Start1, End1}} = Lst,
    Enclosed = lists:usort(lists:filter(fun (R) ->
						{_ModName2, _FName2, _Arity2, {Start2, End2}} = R,
						Start1 =< Start2 andalso End2 =< End1
					end,
					Rs)),
    case Enclosed of
      [] -> Cur;
      [Hd | _] -> get_enclosed([Hd | Cur], Rs -- [Hd])
    end.

%%=========================================================================================
-spec(caller_funs(FName::filename(), Line::integer(), Col::integer(), SearchPaths::[dir()], TabWidth::integer()) ->
	     {error, string()} | {ok, {[{modulename(), functionname(), functionarity()}],
				       [{modulename(), functionname(), functionarity()}]}}).
caller_funs(FName, Line, Col, SearchPaths, TabWidth) ->
    {ok, {AnnAST, Info}} = refac_util:parse_annotate_file(FName, true, SearchPaths, TabWidth),
    case interface_api:pos_to_fun_def(AnnAST, {Line, Col}) of
      {ok, Def} ->
	  case lists:keysearch(fun_def, 1, refac_syntax:get_ann(Def)) of
	    {value, {fun_def, {M, F, A, _, _}}} ->
		?wrangler_io("\nSearching for caller function of ~p:~p/~p ...\n", [M, F, A]),
		{Res1, Res2} = get_caller_funs(FName, {M, F, A}, SearchPaths, TabWidth),
		case refac_misc:is_exported({F, A}, Info) of
		  true ->
		      ?wrangler_io("\nChecking client modules in the following paths: \n~p\n",
				   [SearchPaths]),
		      ClientFiles = refac_util:get_client_files(FName, SearchPaths),
		      ResultsFromClients = get_caller_funs_in_client_modules(ClientFiles, {M, F, A}, SearchPaths, TabWidth),
		      {Callers, Unsures} = lists:unzip([{Res1, Res2}| ResultsFromClients]),
		      display_results(lists:append(Callers), lists:append(Unsures)),
		      {ok, {Callers, Unsures}};
		  false -> display_results(Res1, Res2), {ok, {Res1, Res2}}
		end;
	    false ->
		{error, "Sorry, Wrangler could not infer which function has been selected."}
	  end;
      {error, _Reason} -> {error, "You have not selected a function!"}
    end.

display_results(Callers, UnSures) ->
    case {Callers, UnSures} of
      {[], []} ->
	  ?wrangler_io("The function selected is not called by any other functions.\n",[]);
      {_, []} ->
	  ?wrangler_io("The function selected is called by the following function(s):\n",[]),
	    lists:foreach(fun({File, F, A}) -> ?wrangler_io("{~p:~p: , ~p}\n", [File, F, A]) end, Callers);
      {[], [_H | _]} ->
	    ?wrangler_io("The function selected is not explicitly called by any other functions, \n"
			 "but please check the following expressions:\n", []),
	    lists:foreach(fun({File, Line, Exp}) -> ?wrangler_io("{~p:~p: , ~p}\n", [File, Line, Exp]) end, UnSures);
	{[_H1 | _], [_H2 | _]} ->
	    ?wrangler_io("The function selected is called by the following function(s):\n",[]),
	    lists:foreach(fun({File, F, A}) -> ?wrangler_io("{File:~p, function name: ~p/~p }\n", [File, F, A]) end, Callers),
	    ?wrangler_io("Please also check the following expressions:\n", []),
	    lists:foreach(fun({File, Line, Exp}) -> ?wrangler_io("~p:~p: , ~p\n", [File, Line, Exp]) end, UnSures)		 
    end.

get_caller_funs_in_client_modules(FileNames, {M, F, A}, SearchPaths, TabWidth) ->
    [get_caller_funs(FName, {M,F,A}, SearchPaths, TabWidth)||FName<-FileNames].
    
    
get_caller_funs(FileName, {M, F, A}, SearchPaths, TabWidth) ->		  
    %% 'true' is used in the following function call, so macros are not expanded.
    %% erxpanding macros does work not properly at the moment.
    {ok, {AnnAST, Info}} = refac_util:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    ModName = get_module_name(FileName, Info),
    Fun = fun(Node, {S1, S2}) ->
		  case refac_syntax:type(Node) of 
		      function -> 
			  FunName = refac_syntax:data(refac_syntax:function_name(Node)),
			  FunArity = refac_syntax:function_arity(Node),		
			  case {ModName, FunName, FunArity} == {M, F, A} of
			      false ->
				  {Sure, UnSure}= collect_apps(FileName, Node, {M, F, A});
			      true ->
				  {Sure1,UnSure1} = lists:unzip([collect_apps(FileName, C, {M,F,A})
								 ||C<-refac_syntax:function_clauses(Node)]),
				  {Sure, UnSure} ={lists:append(Sure1), lists:append(UnSure1)}
			  end,
			  case {Sure, UnSure} of 
			      {[], []} -> {S1, S2};
			      {[], [_H|_]} -> {S1, UnSure++S2};
			      {[_H|_], []} -> {[{FileName, FunName, FunArity}|S1], S2};
			      _ -> {[{FileName, FunName, FunArity}|S1], UnSure++S2}
			  end;
		      _ -> {S1, S2}
		  end
	  end,
    refac_syntax_lib:fold(Fun, {[], []}, AnnAST).

collect_apps(FileName, Node, {M, F, A}) ->
    Fun = fun (T, {S1,S2}) ->
		  case refac_syntax:type(T) of
		      atom ->
			  case refac_syntax:atom_value(T) of
			      F -> 
				  Ann =refac_syntax:get_ann(T),
				  case lists:keysearch(type, 1,Ann) of
				      {value, {type, {f_atom, [M, F, A]}}} ->
					  {[true|S1], S2};
				      {value, {type, {f_atom, [M1, F1, A1]}}} ->
					  case is_atom(M1) andalso  M1 /= M orelse 
					      is_atom(F1) andalso F1/=M orelse 
					      is_integer(A1) andalso A1/=A of 
					      true ->
						  {S1, S2};
					      false ->
						  {Line, _Col} = refac_syntax:get_pos(T),
						  {S1, [{FileName, Line, refac_prettypr:format(T)} | S2]}
					  end;
				      {value, {type, _}} ->
					  {S1, S2};
				      false ->
					  {Line, _Col} = refac_syntax:get_pos(T),
					  {S1, [{FileName, Line, refac_prettypr:format(T)} | S2]}
				  end;
			      _ ->{S1, S2}
			  end;
		      _ ->{S1, S2}
		  end
	  end,
    refac_syntax_lib:fold(Fun, {[], []}, Node).
    
%%==========================================================================================
-spec(caller_called_modules(FName::filename(), SearchPaths::[dir()], TabWidth::integer()) -> ok).
caller_called_modules(FName, SearchPaths, _TabWidth) ->
    %% I use 'false' in the following function call, so that macro can get expanded;
    AbsFileName = filename:absname(filename:join(filename:split(FName))),
    ClientFiles = wrangler_modulegraph_server:get_client_files(AbsFileName, SearchPaths),
    ClientMods = [M || {M, _Dir} <- refac_util:get_modules_by_file(ClientFiles)],
    case ClientFiles of
	[] -> ?wrangler_io("\nThis module does not have any caller modules.\n", []);
	_ -> ?wrangler_io("\nThis module is called by the following modules:\n", []),
	     ?wrangler_io("~p\n", [ClientMods])
    end,
    CalledMods = wrangler_modulegraph_server:get_called_modules(FName, SearchPaths),
    case CalledMods of
	[] -> ?wrangler_io("\nThis module does not have any called modules.\n", []);
	_ -> ?wrangler_io("\nThis module calls the following modules:\n", []),
	     ?wrangler_io("~p\n", [CalledMods])
    end.


%%==========================================================================================
-spec(long_functions_in_file(FName::filename(), Lines::[integer()], SearchPaths::[dir()], TabWidth::integer()) ->
	     {error, string()} | ok).
long_functions_in_file(FName, Lines, SearchPaths, TabWidth) ->
    ?wrangler_io("\n Search for long functions in the current buffer... \n",[]),
    long_functions_1([FName], Lines, SearchPaths, TabWidth).

%%==========================================================================================
-spec(long_functions_in_dirs(Lines::[integer()], SearchPaths::[dir()], TabWidth::integer()) ->
	     {error, string()} | ok).
long_functions_in_dirs(Lines, SearchPaths, TabWidth) ->
    ?wrangler_io("\n Search for long functions in the following directories:\n~p\n", [SearchPaths]),
    long_functions_1(SearchPaths, Lines, SearchPaths, TabWidth).

long_functions_1(DirFileNames, Lines, SearchPaths, TabWidth) ->
    try list_to_integer(Lines) of 
	Val ->
	    case Val>=0 of 
		true ->
		    Files = refac_util:expand_files(DirFileNames, ".erl"),
		    Funs = lists:flatmap(fun(F) ->
						 long_functions_2(F, Val, SearchPaths, TabWidth)
					 end, Files),
		    long_funs_format_results(Funs, Val);
		false ->{error, "Invalid number of lines!"}
	    end
    catch 
	_:_ ->
	     {error, "Invalid number of lines"}
    end.

long_functions_2(FName, Lines, SearchPaths, TabWidth) ->
    %% I don't want to expand macro definitions here, as macro is also a kind of abstraction.
    {ok, {AnnAST, Info}} = refac_util:parse_annotate_file(FName, true, SearchPaths, TabWidth),
    ModName = get_module_name(FName, Info),
    Fun = fun (Node, S) ->
		  case refac_syntax:type(Node) of
		    function ->
			Toks = refac_misc:get_toks(Node),
			CodeLines = [element(1, element(2, T)) || T <- Toks, element(1, T) /= whitespace, element(1, T) /= comment],
			CodeLines1 = refac_misc:remove_duplicates(CodeLines),
			case length(CodeLines1) > Lines of
			  true ->
			      FunName = refac_syntax:atom_value(refac_syntax:function_name(Node)),
			      Arity = refac_syntax:function_arity(Node),
			      [{ModName, FunName, Arity}| S];
			  _ -> S
			end;
		    _ -> S
		  end
	  end,
    lists:usort(refac_syntax_lib:fold(Fun, [], AnnAST)).
   

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
    
%%==========================================================================================
-spec(large_modules(Lines::[integer()], SearchPaths::[dir()], TabWidth::integer()) ->				  
	     {error, string()} | ok).
large_modules(Lines, SearchPaths, TabWidth) ->
    try list_to_integer(Lines) of 
	Val -> 
	    case  Val>=0 of 
		true ->
		    large_modules_1(Val, SearchPaths, TabWidth);
		false ->{error, "Invalid number of lines!"}
	    end
    catch
	_:_ ->
	    {error, "Invalid number of lines!"}
    end.


large_modules_1(Lines, SearchPaths, TabWidth) ->
    ?wrangler_io("\nSearching for large modules in the following paths: \n~p\n",
		 [SearchPaths]),    
    Files = refac_util:expand_files(SearchPaths, ".erl"),
    LargeModules = lists:filter(fun(File) ->
					is_large_module(File, Lines, TabWidth)
				end, Files),
    case LargeModules of 
	[] ->
	    ?wrangler_io("\n No Module with more than ~p line of code has been found.\n", [Lines]);
	_ ->
	    ?wrangler_io("The following modules have more than ~p lines of code:\n",
		      [Lines]),
	    ?wrangler_io("~p\n", [LargeModules])
    end.
is_large_module(FName, Lines, TabWidth) ->
    Toks = refac_util:tokenize(FName, false, TabWidth),
    CodeLines = refac_misc:remove_duplicates([element(1, element(2, T)) || T <- Toks]),
    length(CodeLines) >= Lines.

%%==========================================================================================
%% Non tail-recursive servers or non tail-recursive functions? It is certainly easier to 
%% detect non tail-recursive function;  how do you decide whether a function functions as 
%% a server or not?

-spec(non_tail_recursive_servers_in_file(FName::filename(), SearchPaths::[dir()], TabWidth::integer()) -> ok).
non_tail_recursive_servers_in_file(FName, SearchPaths, TabWidth) ->
    Funs = non_tail_recursive_servers(FName, SearchPaths, TabWidth),
    non_tail_format_results(Funs).
 
-spec(non_tail_recursive_servers_in_dirs(SearchPaths::[dir()], TabWidth::integer()) -> ok).
non_tail_recursive_servers_in_dirs(SearchPaths, TabWidth) ->
    Files = refac_util:expand_files(SearchPaths, ".erl"),
    Funs = lists:flatmap(fun(F) ->
				 non_tail_recursive_servers(F, SearchPaths, TabWidth)
			 end, Files),
    non_tail_format_results(Funs).


non_tail_recursive_servers(FName, SearchPaths, TabWidth) ->
    {ok, {AnnAST, Info}} = refac_util:parse_annotate_file(FName, true, SearchPaths, TabWidth),
    ModName = get_module_name(FName, Info),
    Fun = fun (T, S) ->
		  case refac_syntax:type(T) of
		    function ->
			FunName = refac_syntax:atom_value(refac_syntax:function_name(T)),
			Arity = refac_syntax:function_arity(T),
			case has_receive_expr(T) of
			  {true, Line} ->
			      case is_non_tail_recursive_server(T, {ModName, FunName, Arity}, Line, SearchPaths) of
				true -> [{ModName, FunName, Arity} | S];
				_ -> S
			      end;
			  false -> S
			end;
		    _ -> S
		  end
	  end,
   refac_syntax_lib:fold(Fun, [], AnnAST).
 
   
non_tail_format_results(Funs) ->
    case Funs of
      [] ->
	  ?wrangler_io("\n No non-tail recursive server has been found.\n",[]);
      _ ->
	    ?wrangler_io("\n The following functions are recursive servers, but not tail-recursive:\n",[]),
	    format_result_1(Funs)    
    end.
    

has_receive_expr(FunDef) ->
    F = fun (T, S) ->
		case refac_syntax:type(T) of
		  receive_expr ->
		      {{StartLine, _}, _} = refac_misc:get_start_end_loc(T),
		      [StartLine| S];
		  _ -> S
		end
	end,
    LineNums = refac_syntax_lib:fold(F, [], FunDef),
    case LineNums of
      [] -> false;
      _ -> {true, lists:min(LineNums)}
    end.

is_non_tail_recursive_server(FunDef, {ModName, FunName, Arity}, Line, SearchPaths) ->
    Fun = fun (T, S) ->
		  case refac_syntax:type(T) of
		    receive_expr ->
			ResSccs = wrangler_callgraph_server:get_sccs_including_fun({ModName, FunName, Arity}, SearchPaths),
			ResSccs ++ S;
		    _ -> S
		  end
	  end,
    CandidateSccs = refac_syntax_lib:fold(Fun, [], FunDef),
    case CandidateSccs of
      [] -> false;
      _ -> lists:any(fun (Scc) -> check_candidate_scc(FunDef, Scc, Line) end, CandidateSccs)
    end.



check_candidate_scc(FunDef, Scc, Line) ->
    %%InscopeFuns = refac_util:auto_imported_bifs() ++ refac_util:inscope_funs(Info), 
    MFAs = [MFA || {MFA, _} <- Scc],
    DummyExp = refac_syntax:atom(undefined),
    F = fun (T, Acc) ->
		case refac_syntax:type(T)      %% To think: any other cases here?
		    of
		  clause -> Exprs = refac_syntax:clause_body(T),
			    Acc ++ [Exprs];
		  application -> Exprs = refac_syntax:application_arguments(T),
				 Acc ++ [Exprs ++ [DummyExp]];
		  tuple -> Exprs = refac_syntax:tuple_elements(T),
			   Acc ++ [Exprs ++ [DummyExp]];
		  list -> Exprs = refac_syntax:list_prefix(T),
			  Acc ++ [Exprs ++ [DummyExp]];
		  list_comp ->
		      Acc ++ [[T, DummyExp]];
		  block_expr ->
		      Exprs = refac_syntax:block_expr_body(T),
		      Acc ++ [Exprs];
		  infix_expr ->
		      Acc ++ [[T, DummyExp]];
		  prefix_expr ->
		      Acc ++ [[T, DummyExp]];
		  _ -> Acc
		end
	end,
    F1 = fun (Es) ->
		 F11 = fun (E) ->
			       {_, {EndLine, _}} = refac_misc:get_start_end_loc(E),
			       case EndLine >= Line of
				 true ->
				     CalledFuns = wrangler_callgraph_server:called_funs(E),
				     case lists:subtract(CalledFuns, MFAs) of
				       CalledFuns -> false;
				       _ -> true
				     end;
				 _ -> false
			       end
		       end,
		 R = [F11(E) || E <- Es],
		 lists:any(fun (E) -> E == true end, tl(lists:reverse(R)))
	 end,
    ListOfExpLists = refac_syntax_lib:fold(F, [], FunDef),
    ExpLists1 = lists:map(F1, ListOfExpLists),
    lists:any(fun (E) -> E == true end, ExpLists1).
       


%%==========================================================================================
-spec(not_flush_unknown_messages_in_file(FName::filename(), SearchPaths::[dir()], TabWidth::integer()) -> ok).    
not_flush_unknown_messages_in_file(FName, SearchPaths, TabWidth) ->
    Funs = not_flush_unknown_messages(FName, SearchPaths, TabWidth),
    non_flush_format_result(lists:usort(Funs)).
    
-spec(not_flush_unknown_messages_in_dirs(SearchPaths::[dir()], TabWidth::integer()) -> ok).
not_flush_unknown_messages_in_dirs(SearchPaths, TabWidth) ->
    Files = refac_util:expand_files(SearchPaths, ".erl"),
    Funs = lists:flatmap(fun(F) ->
				 not_flush_unknown_messages(F, SearchPaths, TabWidth)
			 end, Files),
    non_flush_format_result(lists:usort(Funs)).

not_flush_unknown_messages(FName, SearchPaths, TabWidth) ->
    {ok, {AnnAST, Info}} = refac_util:parse_annotate_file(FName, true, SearchPaths, TabWidth),
    ModName = get_module_name(FName, Info),
    Fun = fun (T, S) ->
		  case refac_syntax:type(T) of
		    function ->
			FunName = refac_syntax:atom_value(refac_syntax:function_name(T)),
			Arity = refac_syntax:function_arity(T),
			case has_receive_expr(T) of 
			    {true, Line} ->
				case has_receive_expr_without_flush(FName, Info, ModName, T, Line, SearchPaths) of
				    true -> [{ModName, FunName, Arity} | S];
				    false -> S
				end;
			    false -> S
			end;
		      _ -> S
		  end
	  end,
    refac_syntax_lib:fold(Fun, [], AnnAST).
    

has_receive_expr_without_flush(FileName, Info, ModName, FunDef, Line, SearchPaths) ->
    FunName = refac_syntax:atom_value(refac_syntax:function_name(FunDef)),
    Arity = refac_syntax:function_arity(FunDef),
    F = fun(T,S) ->
		case refac_syntax:type(T) of 
		    receive_expr ->
			ResSccs = wrangler_callgraph_server:get_sccs_including_fun({ModName, FunName, Arity}, SearchPaths),
     			case ResSccs of 
			    [] -> S;  %% This should not happen;
			    _ -> [lists:all(fun(Scc) -> not_has_flush_scc(FileName, Info, FunDef, Scc, Line) end, ResSccs)|S]				
			end;
		    _ -> S
		end
	end,
    lists:member(true, refac_syntax_lib:fold(F, [], FunDef)).

not_has_flush_scc(FileName, Info, FunDef, Scc, Line) ->
    case is_server(FileName, Info, FunDef, Scc, Line) of 
	 true -> lists:all(fun({_MFA, Def}) ->
			 not_has_flush_fun(Def) end, Scc);
	_ -> false
    end.

is_server(_FileName, _Info, FunDef, Scc, Line) ->
    %% ModName = get_module_name(FileName, Info),
    %%InscopeFuns = refac_util:auto_imported_bifs() ++ refac_util:inscope_funs(Info), 
    MFAs = [MFA || {MFA, _} <- Scc],
    F = fun (T, Acc) ->
		case
		  refac_syntax:type(T)      %% To think: any other cases here?
		    of
		  application -> Acc ++ [T];
		  _ -> Acc
		end
	end,
    F1 = fun (E) ->
		 {_, {EndLine, _}} = refac_misc:get_start_end_loc(E),
		 case EndLine >= Line of
		   true ->
		       CalledFuns = wrangler_callgraph_server:called_funs(E),
		       case lists:subtract(CalledFuns, MFAs) of
			 CalledFuns -> false;
			 _ -> true
		       end;
		   _ -> false
		 end
	 end,
    ListOfApps = refac_syntax_lib:fold(F, [], FunDef),
    lists:member(true, lists:map(F1, ListOfApps)).
  

not_has_flush_fun(FunDef) ->
    F = fun (T, S) ->
		case refac_syntax:type(T) of
		  receive_expr ->
		      Cs = refac_syntax:receive_expr_clauses(T),
		      R = lists:any(fun (C) ->
					    Pat = refac_syntax:clause_patterns(C),
					    case length(Pat) of
					      1 -> P = hd(Pat),
						   case refac_syntax:type(P) of
						     variable ->
							 case refac_misc:get_free_vars(P) of
							   [] -> true;
							   _ -> false
							 end;
						     under_score -> true;
						     _ -> false
						   end;
					      _ -> false
					    end
				    end, Cs),
		      [R =/= true| S];
		  _ -> S
		end
	end,
    lists:member(true, refac_syntax_lib:fold(F, [], FunDef)).


non_flush_format_result(Funs) ->
    case Funs of
      [] ->
	    ?wrangler_io("\n No server without flushing unknown messages has been found.\n",[]);
	_ ->
	    ?wrangler_io("\n The following functions are servers without flush of unknown messages:\n",[]),
	    format_result_1(Funs)
    end.



-spec (gen_module_graph/4::(filename(), string(), [filename()|dir()], boolean()) ->true).
gen_module_graph(OutFile, NotCareMods, SearchPaths, WithLabel) ->
    ?wrangler_io("\nCMD: ~p:gen_module_graph(~p, ~p, ~p, ~p).\n",
		 [?MODULE, OutFile, NotCareMods, SearchPaths, WithLabel]),
    NotCareMods1 = [list_to_atom(T)|| T<-string:tokens(NotCareMods, ", ")],
    refac_module_graph:module_graph_to_dot(OutFile, NotCareMods1, SearchPaths, WithLabel).
   
-spec(gen_module_scc_graph/3::(filename(), [filename()|dir()], boolean()) ->true).
gen_module_scc_graph(OutFile, SearchPaths, WithLabel)->
    ?wrangler_io("\nCMD: ~p:gen_module_scc_graph(~p, ~p, ~p).\n",
		 [?MODULE, OutFile, SearchPaths, WithLabel]),
    refac_module_graph:scc_graph_to_dot(OutFile, SearchPaths, WithLabel).
    


   
-spec(gen_function_callgraph/3::(filename(), filename(),[filename()|dir()]) ->true).
gen_function_callgraph(OutFile, FileName, SearchPaths)->
     ?wrangler_io("\nCMD: ~p:gen_function_callgraph(~p, ~p, ~p).\n",
 		 [?MODULE, OutFile, FileName, SearchPaths]),
     wrangler_callgraph_server:fun_callgraph_to_dot(OutFile, FileName).
   



 

			  
