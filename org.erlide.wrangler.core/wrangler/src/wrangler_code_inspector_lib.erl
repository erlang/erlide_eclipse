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
%%
-module(wrangler_code_inspector_lib).

-export([find_var_instances/5, nested_exprs/5,
	 calls_to_fun/5, calls_to_fun_1/5,
	 dependencies_of_a_module/2, long_functions/4,
	 large_modules/3, non_tail_recursive_servers/3,
	 not_flush_unknown_messages/3]).

-include("../include/wrangler.hrl").

%%==========================================================================================
%%-spec(find_var_instances(FileName::filename(), Line::integer(), Col::integer(),
%%			 SearchPaths::[dir()], TabWidth:: integer()) ->
%%	     {ok, [StartEnd::{pos(), pos()}], [VarDefPos::pos()]}).
find_var_instances(FName, Line, Col, SearchPaths, TabWidth) ->
    {ok, {AnnAST, _Info0}} = wrangler_ast_server:parse_annotate_file(FName, true, SearchPaths, TabWidth),
    case interface_api:pos_to_var_name(AnnAST, {Line, Col}) of
	{ok, {_VarName, DefinePos, _C}} ->
	    if DefinePos == [{0, 0}] ->
		   throw({error, "The identifier selected is a macro, or not defined!"});
	       true ->
		   F = fun (T, S) ->
			       case refac_syntax:type(T) of
				   variable ->
				       case lists:keysearch(def, 1, refac_syntax:get_ann(T)) of
					   {value, {def, DefinePos}} ->
					       Range = refac_util:get_start_end_loc(T),
					       [Range| S];
					   _ -> S
				       end;
				   _ -> S
			       end
		       end,
		   Locs = lists:usort(ast_traverse_api:fold(F, [], AnnAST)),
		   {ok, Locs, DefinePos}
	    end;
	{error, Reason} -> throw({error, Reason})
    end.

%%==========================================================================================
nested_exprs(DirFileNames, NestLevel, ExprType, SearchPaths, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:nested_exprs(~p, ~p, ~p,~p,~p).\n",
		 [?MODULE, DirFileNames, NestLevel, ExprType, SearchPaths, TabWidth]),
    Files = refac_util:expand_files(DirFileNames, ".erl"),
    Funs = lists:flatmap(fun (F) ->
				 nested_exprs_1(F, NestLevel, ExprType, SearchPaths, TabWidth)
			 end, Files),
    {ok, Funs}.

nested_exprs_1(FName, NestLevel, ExprType, SearchPaths, TabWidth) ->
    ExprType1 = list_to_atom(atom_to_list(ExprType)++"_expr"),
    {ok, {AnnAST, Info}} = wrangler_ast_server:parse_annotate_file(FName, true, SearchPaths, TabWidth),
    ModName = get_module_name(FName, Info),
    Fun = fun (T, S) ->
		  case refac_syntax:type(T) of
		      function ->
			  FunName = refac_syntax:atom_value(refac_syntax:function_name(T)),
			  Arity = refac_syntax:function_arity(T),
			  Fun1 = fun (Node, S1) ->
					 case refac_syntax:type(Node) of
					     ExprType1 ->
						 Range = refac_util:get_start_end_loc(Node),
						 [{ModName, FunName, Arity, Range}| S1];
					     _ -> S1
					 end
				 end,
			  ast_traverse_api:fold(Fun1, S, T);
		      _ -> S
		  end
	  end,
    Ranges = lists:usort(ast_traverse_api:fold(Fun, [], AnnAST)),
    SortedRanges = sort_ranges(Ranges),
    ResRanges = lists:filter(fun (R) -> length(R) >= NestLevel end, SortedRanges),
    lists:usort(lists:map(fun (R) -> {M, F, A, _R} = hd(R),{M, F, A} end,
			  ResRanges)).

	    
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

%%==========================================================================================
%%-spec(calls_to_fun(FileName::filename(), Line::integer(), Col::integer(), SearchPaths::[dir()], TabWidth::integer()) ->
%%	     {ok, {[{modulename(), functionname(), functionarity()}]}}).
calls_to_fun(FName, Line, Col, SearchPaths, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:caller_funs(~p, ~p, ~p,~p,~p).\n",
		 [?MODULE, FName, Line, Col, SearchPaths, TabWidth]),
    check_search_paths(FName, SearchPaths),
    {ok, {AnnAST, Info}} = wrangler_ast_server:parse_annotate_file(FName, true, SearchPaths, TabWidth),
    case interface_api:pos_to_fun_def(AnnAST, {Line, Col}) of
	{ok, Def} ->
	    case lists:keysearch(fun_def, 1, refac_syntax:get_ann(Def)) of
		{value, {fun_def, {M, F, A, _, _}}} ->
		    caller_funs_2(FName, M, F, A, Info, SearchPaths, TabWidth);
		false ->
		    throw({error, "Sorry, Wrangler could not infer which function has been selected."})
	    end;
	{error, _Reason} -> throw({error, "You have not selected a function!"})
    end.

%%-spec(calls_to_fun_1(FileName::filename(), FunctionName::functionname(), Arity::integer(), SearchPaths::[dir()], TabWidth::integer()) ->
%%	     {ok, {[{modulename(), functionname(), functionarity()}]}}).
calls_to_fun_1(FName, FunctionName, Arity, SearchPaths, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:caller_funs_1(~p, ~p, ~p,~p,~p).\n",
		 [?MODULE, FName, FunctionName, Arity, SearchPaths, TabWidth]),
    {ok, {_AnnAST, Info}} = wrangler_ast_server:parse_annotate_file(FName, true, SearchPaths, TabWidth),
    ModName = get_module_name(FName, Info),
    caller_funs_2(FName, ModName, FunctionName, Arity, Info, SearchPaths, TabWidth).

caller_funs_2(FName, M, F, A, Info, SearchPaths, TabWidth) ->
    ?wrangler_io("\nSearching for caller function of ~p:~p/~p ...\n", [M, F, A]),
    {Res1, Res2} = get_caller_funs(FName, {M, F, A}, SearchPaths, TabWidth),
    case refac_util:is_exported({F, A}, Info) of
	true ->
	    ?wrangler_io("\nChecking client modules in the following paths: \n~p\n",
			 [SearchPaths]),
	    ClientFiles = wrangler_modulegraph_server:get_client_files(FName, SearchPaths),
	    ResultsFromClients = get_caller_funs_in_client_modules(ClientFiles, {M, F, A}, SearchPaths, TabWidth),
	    {Callers, _Unsures} = lists:unzip([{Res1, Res2}| ResultsFromClients]),
	    %% display_results(lists:append(Callers), lists:append(Unsures)),
	    {ok, lists:append(Callers)};
	%%{ok, {Callers, Unsures}};
	false ->
	    {ok, Res1}
    end.
%%{ok, {Res1, Res2}}

get_caller_funs_in_client_modules(FileNames, {M, F, A}, SearchPaths, TabWidth) ->
    [get_caller_funs(FName, {M,F,A}, SearchPaths, TabWidth)||FName<-FileNames].

get_caller_funs(FileName, {M, F, A}, SearchPaths, TabWidth) ->
    %% 'true' is used in the following function call, so macros are not expanded.
    %% erxpanding macros does work not properly at the moment.
    {ok, {AnnAST, Info}} = wrangler_ast_server:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    ModName = get_module_name(FileName, Info),
    Fun = fun (Node, {S1, S2}) ->
		  case refac_syntax:type(Node) of
		      function ->
			  FunName = refac_syntax:data(refac_syntax:function_name(Node)),
			  FunArity = refac_syntax:function_arity(Node),
			  case {ModName, FunName, FunArity} == {M, F, A} of
			      false ->
				  {Sure, UnSure} = collect_apps(FileName, Node, {M, F, A});
			      true ->
				  {Sure1, UnSure1} = lists:unzip([collect_apps(FileName, C, {M, F, A})
								  || C <- refac_syntax:function_clauses(Node)]),
				  {Sure, UnSure} = {lists:append(Sure1), lists:append(UnSure1)}
			  end,
			  case {Sure, UnSure} of
			      {[], []} -> {S1, S2};
			      {[], [_H| _]} -> {S1, UnSure ++ S2};
			      {[_H| _], []} -> {[{FileName, FunName, FunArity}| S1], S2};
			      _ -> {[{FileName, FunName, FunArity}| S1], UnSure ++ S2}
			  end;
		      _ -> {S1, S2}
		  end
	  end,
    ast_traverse_api:fold(Fun, {[], []}, AnnAST).

collect_apps(FileName, Node, {M, F, A}) ->
    Fun = fun (T, {S1,S2}) ->
		  case refac_syntax:type(T) of
		      atom ->
			  case refac_syntax:atom_value(T) of
			      F ->
				  Ann = refac_syntax:get_ann(T),
				  case lists:keysearch(type, 1, Ann) of
				      {value, {type, {f_atom, [M, F, A]}}} ->
					  {[true| S1], S2};
				      {value, {type, {f_atom, [M1, F1, A1]}}} ->
					  case is_atom(M1) andalso M1 /= M orelse 
						 is_atom(F1) andalso F1/=M orelse 
						   is_integer(A1) andalso A1/=A
					      of
					      true ->
						  {S1, S2};
					      false ->
						  {Line, _Col} = refac_syntax:get_pos(T),
						  {S1, [{FileName, Line, refac_prettypr:format(T)}| S2]}
					  end;
				      {value, {type, _}} ->
					  {S1, S2};
				      false ->
					  {Line, _Col} = refac_syntax:get_pos(T),
					  {S1, [{FileName, Line, refac_prettypr:format(T)}| S2]}
				  end;
			      _ -> {S1, S2}
			  end;
		      _ -> {S1, S2}
		  end
	  end,
    ast_traverse_api:fold(Fun, {[], []}, Node).

%%==========================================================================================
%%-spec(dependencies_of_a_module(FileName::filename(), SearchPaths::[dir()]) -> 
%%	     {ok, {[modulename()], [modulename()]}}).
dependencies_of_a_module(FName, SearchPaths) ->
    ?wrangler_io("\nCMD: ~p:dependencies_of_a_module(~p, ~p).\n",
		 [?MODULE, FName, SearchPaths]),
    case filelib:is_file(FName) of
	true ->
	    check_search_paths(FName, SearchPaths),
	    %% I use 'false' in the following function call, so that macro can get expanded;
	    AbsFileName = filename:absname(filename:join(filename:split(FName))),
	    ClientFiles = wrangler_modulegraph_server:get_client_files_basic(AbsFileName, SearchPaths),
	    ClientMods = [M || {M, _Dir} <- refac_util:get_modules_by_file(ClientFiles)],
	    CalledMods = wrangler_modulegraph_server:get_called_modules(FName, SearchPaths),
	    {ok, {ClientMods, CalledMods}};
	false ->
	    throw({error, "Invalid filename!"})
    end.



 %%==========================================================================================


%%-spec(long_functions(DirFileNames::[filename()|dir()], Lines::integer(), SearchPaths::[dir()], TabWidth::integer()) ->
%%	    {ok, [{modulename(), functionname(), functionarity()}]}).
long_functions(DirFileNames, Lines, SearchPaths, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:long_functions(~p, ~p,~p,~p).\n",
		 [?MODULE, DirFileNames, Lines, SearchPaths, TabWidth]),
    Files = refac_util:expand_files(DirFileNames, ".erl"),
    MFAs = lists:flatmap(fun (F) ->
				 long_functions_2(F, Lines, SearchPaths, TabWidth)
			 end, Files),
    {ok,MFAs}.

long_functions_2(FName, Lines, SearchPaths, TabWidth) ->
    %% I don't want to expand macro definitions here, as macro is also a kind of abstraction.
    {ok, {AnnAST, Info}} = wrangler_ast_server:parse_annotate_file(FName, true, SearchPaths, TabWidth),
    ModName = get_module_name(FName, Info),
    Fun = fun (Node, S) ->
		  case refac_syntax:type(Node) of
		      function ->
			  Toks = refac_util:get_toks(Node),
			  CodeLines = [element(1, element(2, T)) || T <- Toks, element(1, T) /= whitespace, element(1, T) /= comment],
			  CodeLines1 = refac_util:remove_duplicates(CodeLines),
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
    lists:usort(ast_traverse_api:fold(Fun, [], AnnAST)).

%%==========================================================================================
%%-spec(large_modules(Lines::integer(), SearchPaths::[dir()], TabWidth::integer()) ->				  
%%	     {ok, [modulename()]}).
large_modules(Lines, SearchPaths, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:large_modules(~p,~p,~p).\n",
		 [?MODULE, Lines, SearchPaths, TabWidth]),
    Files = refac_util:expand_files(SearchPaths, ".erl"),
    LargeMods = lists:filter(fun (File) ->
				     is_large_module(File, Lines, TabWidth)
			     end, Files),
    {ok, LargeMods}.

is_large_module(FName, Lines, TabWidth) ->
    Toks = refac_util:tokenize(FName, false, TabWidth),
    CodeLines = refac_util:remove_duplicates([element(1, element(2, T)) || T <- Toks]),
    length(CodeLines) >= Lines.

%%==========================================================================================
%%-spec(non_tail_recursive_servers(FileOrDirs::[filename()], SearchPaths::[dir()], TabWidth::integer()) -> 
%%	     {ok, [{modulename(), functionname(), functionarity()}]}).
non_tail_recursive_servers(FileOrDirs, SearchPaths, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:non_tail_recursive_servers(~p, ~p, ~p).\n",
		 [?MODULE, FileOrDirs, SearchPaths, TabWidth]),
    Files = refac_util:expand_files(FileOrDirs, ".erl"),
    MFAs = lists:flatmap(fun (F) ->
				 non_tail_recursive_servers_1(F, SearchPaths, TabWidth)
			 end, Files),
    {ok, MFAs}.

non_tail_recursive_servers_1(FName, SearchPaths, TabWidth) ->
    {ok, {AnnAST, Info}} = wrangler_ast_server:parse_annotate_file(FName, true, SearchPaths, TabWidth),
    ModName = get_module_name(FName, Info),
    Fun = fun (T, S) ->
		  case refac_syntax:type(T) of
		      function ->
			  FunName = refac_syntax:atom_value(refac_syntax:function_name(T)),
			  Arity = refac_syntax:function_arity(T),
			  case has_receive_expr(T) of
			      {true, Line} ->
				  case is_non_tail_recursive_server(FName, T, {ModName, FunName, Arity}, Line, SearchPaths) of
				      true -> [{ModName, FunName, Arity}| S];
				      _ -> S
				  end;
			      false -> S
			  end;
		      _ -> S
		  end
	  end,
    ast_traverse_api:fold(Fun, [], AnnAST).

is_non_tail_recursive_server(FileName, FunDef, {ModName, FunName, Arity}, Line, _SearchPaths) ->
    Fun = fun (T, S) ->
		  case refac_syntax:type(T) of
		      receive_expr ->
			  %% it is too slow to search the whole directory for those very rare cases
			  %% when a server function is defined across multiple modules; so SearchPaths
			  %% is not used here.
			  ResSccs = wrangler_callgraph_server:get_sccs_including_fun(
				      {ModName, FunName, Arity}, [FileName]),
			  ResSccs ++ S;
		      _ -> S
		  end
	  end,
    CandidateSccs = ast_traverse_api:fold(Fun, [], FunDef),
    case CandidateSccs of
	[] -> false;
	_ -> lists:any(fun (Scc) ->
			       check_candidate_scc(FunDef, Scc, Line)
		       end, CandidateSccs)
    end.

check_candidate_scc(FunDef, Scc, Line) ->
    MFAs = [MFA || {MFA, _} <- Scc],
    DummyExp = refac_syntax:atom(undefined),
    F = fun (T, Acc) ->
		case refac_syntax:type(T) of
		    clause ->
			Exprs = refac_syntax:clause_body(T),
			Acc ++ [Exprs];
		    try_expr -> Exprs = refac_syntax:try_expr_body(T),
				Acc ++ [Exprs ++ [DummyExp]];
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
		    _ -> Acc   %% Any other cases here?
		end
	end,
    F1 = fun (Es) ->
		 F11 = fun (E) ->
			       {_, {EndLine, _}} = refac_util:get_start_end_loc(E),
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
    ListOfExpLists = ast_traverse_api:fold(F, [], FunDef),
    ExpLists1 = lists:map(F1, ListOfExpLists),
    lists:any(fun (E) -> E == true end, ExpLists1).

%%==========================================================================================
%%-spec(not_flush_unknown_messages(FileOrDirss::[filename()|dir()], SearchPaths::[dir()], TabWidth::integer()) -> 
%%	     {ok, [{modulename(), functionname(), functionarity()}]}).
not_flush_unknown_messages(FileOrDirs, SearchPaths, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:not_flush_unknown_messages(~p, ~p, ~p).\n",
		 [?MODULE, FileOrDirs, SearchPaths, TabWidth]),
    Files = refac_util:expand_files(FileOrDirs, ".erl"),
    Funs = lists:flatmap(fun (F) ->
				 not_flush_unknown_messages_1(F, SearchPaths, TabWidth)
			 end, Files),
    {ok, lists:usort(Funs)}.

not_flush_unknown_messages_1(FName, SearchPaths, TabWidth) ->
    {ok, {AnnAST, Info}} = wrangler_ast_server:parse_annotate_file(FName, true, SearchPaths, TabWidth),
    ModName = get_module_name(FName, Info),
    Fun = fun (T, S) ->
		  case refac_syntax:type(T) of
		      function ->
			  FunName = refac_syntax:atom_value(refac_syntax:function_name(T)),
			  Arity = refac_syntax:function_arity(T),
			  case has_receive_expr(T) of
			      {true, Line} ->
				  case has_receive_expr_without_flush(FName, Info, ModName, T, Line, SearchPaths) of
				      true -> [{ModName, FunName, Arity}| S];
				      false -> S
				  end;
			      false -> S
			  end;
		      _ -> S
		  end
	  end,
    ast_traverse_api:fold(Fun, [], AnnAST).

has_receive_expr_without_flush(FileName, Info, ModName, FunDef, Line, _SearchPaths) ->
    FunName = refac_syntax:atom_value(refac_syntax:function_name(FunDef)),
    Arity = refac_syntax:function_arity(FunDef),
    F = fun (T,S) ->
		case refac_syntax:type(T) of
		    receive_expr ->
			ResSccs = wrangler_callgraph_server:get_sccs_including_fun(
				    {ModName, FunName, Arity}, [FileName]),
			case ResSccs of
			    [] -> S;  %% This should not happen;
			    _ -> [lists:all(fun (Scc) ->
						    not_has_flush_scc(FileName, Info, FunDef, Scc, Line)
					    end, ResSccs)| S]
			end;
		    _ -> S
		end
	end,
    lists:member(true, ast_traverse_api:fold(F, [], FunDef)).

not_has_flush_scc(FileName, Info, FunDef, Scc, Line) ->
    case is_server(FileName, Info, FunDef, Scc, Line) of 
	 true -> lists:all(fun({_MFA, Def}) ->
			 not_has_flush_fun(Def) end, Scc);
	_ -> false
    end.

is_server(_FileName, _Info, FunDef, Scc, Line) ->
    MFAs = [MFA || {MFA, _} <- Scc],
    F = fun (T, Acc) ->
		case refac_syntax:type(T)      %% Any other cases here?
		    of
		    application -> Acc ++ [T];
		    _ -> Acc
		end
	end,
    F1 = fun (E) ->
		 {_, {EndLine, _}} = refac_util:get_start_end_loc(E),
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
    ListOfApps = ast_traverse_api:fold(F, [], FunDef),
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
							       case refac_util:get_free_vars(P) of
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
    lists:member(true, ast_traverse_api:fold(F, [], FunDef)).

check_search_paths(FileName, SearchPaths) ->
    InValidSearchPaths = lists:filter(fun (X) ->  not  filelib:is_dir(X) end, SearchPaths),
    case InValidSearchPaths of
	[] -> ok;
	_ -> ?wrangler_io("\n===============================WARNING===============================\n",[]),
	     ?wrangler_io("The following directories specified in the search paths do not exist:\n~s", [InValidSearchPaths]),
	     throw({error, "Some directories specified in the search paths do not exist!"})
    end,
    Files = refac_util:expand_files(SearchPaths, ".erl") ++ refac_util:expand_files(SearchPaths, ".hrl"),
    case lists:member(FileName, Files) of
	true ->
	    ok;
	_ ->
	    throw({error, "The current Erlang file does not belong to any of the search paths specified; please check!"})
    end.


get_module_name(FName, Info) ->
    case lists:keysearch(module, 1, Info) of
	{value, {module, Mod}} -> Mod;
	_ -> list_to_atom(filename:basename(FName, ".erl"))
    end.

has_receive_expr(FunDef) ->
    F = fun (T, S) ->
		case refac_syntax:type(T) of
		    receive_expr ->
			{{StartLine, _}, _} = refac_util:get_start_end_loc(T),
			[StartLine| S];
		    _ -> S
		end
	end,
    LineNums = ast_traverse_api:fold(F, [], FunDef),
    case LineNums of
	[] -> false;
	_ -> {true, lists:min(LineNums)}
    end.