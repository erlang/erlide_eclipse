%% =====================================================================
%% Refactoring: Rename a process name.
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

%% TOADD: functionalities to check OTP behaviours.
-module(refac_rename_process).

-export([rename_process/5, rename_process_eclipse/5, rename_process_1/4, rename_process_1_eclipse/4]).

-include("../hrl/wrangler.hrl").

-spec(rename_process/5::(string(), integer(), integer(), string(), [dir()]) ->
	     {error, string()} | {undecidables, string()}| {ok, [filename()]}).
rename_process(FileName, Line, Col, NewName, SearchPaths) ->
    rename_process(FileName, Line, Col, NewName, SearchPaths, emacs).

-spec(rename_process_eclipse/5::(string(), integer(), integer(), string(), [dir()]) ->
	     {error, string()} | {undecidables, string()} | {ok, [{filename(), filename(), string()}]}).
rename_process_eclipse(FileName, Line, Col, NewName, SearchPaths) ->
    rename_process(FileName, Line, Col, NewName, SearchPaths, eclipse).


rename_process(FileName, Line, Col, NewName, SearchPaths, Editor) ->
    ?wrangler_io("\nCMD: ~p:rename_process( ~p, ~p, ~p, ~p,~p).\n", [?MODULE, FileName, Line, Col, NewName, SearchPaths]),
    case is_process_name(NewName) of
      true ->
	    _Res = refac_annotate_pid:ann_pid_info(SearchPaths),  %%TODO: check whether asts are already annotated.
	    {ok, {AnnAST, _Info}} = refac_util:parse_annotate_file(FileName, true, SearchPaths), %% TODO: rename to get_ast.
	    NewProcessName = list_to_atom(NewName),
	    case pos_to_process_name(AnnAST, {Line, Col}) of
		{ok, ProcessName} ->
		  case NewProcessName =/= ProcessName of
			       true ->
				  case pre_cond_check(NewProcessName,SearchPaths) of 
				      ok ->
					  Results = do_rename_process(FileName, AnnAST,  ProcessName, NewProcessName, SearchPaths),
					  check_atoms(FileName, ProcessName, SearchPaths),
					  case Editor of 
					      emacs ->
						  refac_util:write_refactored_files(Results),
						  ChangedFiles = lists:map(fun ({{F, _F}, _AST}) -> F end, Results),
						  ?wrangler_io("The following files have been changed by this refactoring:\n~p\n",
							    [ChangedFiles]),
						  {ok, ChangedFiles};
					      eclipse ->
						  Res = lists:map(fun({{FName, NewFName}, AST}) -> {FName, NewFName, refac_prettypr:print_ast(AST)} end, Results),
						  {ok, Res}
					  end;
				      undecidables -> {undecidables, atom_to_list(ProcessName)};
				      {error, Reason} ->
					  {error, Reason}
				  end;
			      _ -> {error, "The new process name is the same as the process name selected!"}
			  end;
		{error, Reason} -> {error, Reason}
	    end;
	false -> {error, "Invalid new process name."}
    end.

-spec(rename_process_1/4::(string(), string(), string(), [dir()]) -> {ok, [filename()]}).
rename_process_1(FileName, OldProcessName, NewProcessName, SearchPaths) ->
    rename_process_1(FileName, OldProcessName, NewProcessName, SearchPaths, emacs).

-spec(rename_process_1_eclipse/4::(string(), string(), string(), [dir()]) -> {ok, [{filename(), filename(), string()}]}).
rename_process_1_eclipse(FileName, OldProcessName, NewProcessName, SearchPaths) ->
    rename_process_1(FileName, OldProcessName, NewProcessName, SearchPaths, eclipse).

-spec(rename_process_1/5::(string(), string(), string(), [dir()], editor()) -> {ok, [filename()]}|{ok, [{filename(), filename(), string()}]}).
rename_process_1(FileName, OldProcessName1, NewProcessName1, SearchPaths, Editor) ->
    OldProcessName = list_to_atom(OldProcessName1),
    NewProcessName = list_to_atom(NewProcessName1),
    {ok, {AnnAST, _Info}} = refac_util:parse_annotate_file(FileName, true, SearchPaths),
    Results = do_rename_process(FileName, AnnAST, OldProcessName, NewProcessName, SearchPaths),
    check_atoms(FileName, OldProcessName, SearchPaths),
    case Editor of 
	emacs ->
	    refac_util:write_refactored_files(Results),
	    ChangedFiles = lists:map(fun ({{F, _F}, _AST}) -> F end, Results),
	    ?wrangler_io("The following files have been changed by this refactoring:\n~p\n",
		      [ChangedFiles]),
	    {ok, ChangedFiles};
	eclipse ->
	    Res = lists:map(fun({{FName, NewFName}, AST}) -> {FName, NewFName, refac_prettypr:print_ast(AST)} end, Results),
	    {ok, Res}
    end.


pos_to_process_name(Node, Pos) ->
    case refac_util:once_tdTU(fun pos_to_process_name_1/2, Node, Pos) of
      {_, false} -> {error, "Wrangler could not infer that the atom selected represents a process name!"};
      {R, true} -> {ok, R}
    end. 

pos_to_process_name_1(Node, Pos) ->
    As = refac_syntax:get_ann(Node),
    case refac_syntax:type(Node) of 
	atom -> 
	    {Start, End} = refac_util:get_range(Node),
	    case (Start =<Pos) andalso (Pos =< End) of 
		true -> 	    
		    case lists:keysearch(pname,1, As) of
			{value, {pname, _V}} ->
			    {refac_syntax:atom_value(Node), true};
			_ -> {[], false}
		    end;
		_-> {[], false}
	    end;
	_ -> {[], false}
    end.

pre_cond_check(NewProcessName, SearchPaths) ->
    {ExistingProcessNames, UnDecidables} = collect_process_names(SearchPaths),
    case lists:member(NewProcessName, ExistingProcessNames) of 
	true -> {error, "The new process name provided is alreay in use, please choose another name."};
	false -> case UnDecidables of 
		     [] -> ok;
		     _ -> ?wrangler_io("\n*************************************Warning****************************************\n",[]),
			  ?wrangler_io("Wrangler could not decide whether the new process name provided conflicts with the process name(s) "
				    "used by the following registeration expression(s):\n",[]),
			  UnDecidables1 = lists:map(fun({_, V}) -> V end, UnDecidables),
			  lists:foreach(fun({M, F,A, {L,_}}) -> ?wrangler_io("Location: module: ~p, function:~p/~p, line:~p\n", [M, F, A, L])
					end, UnDecidables1),
			  undecidables
		 end
    end.

collect_process_names(DirList) ->
    Files = refac_util:expand_files(DirList, ".erl"),
    F = fun(File, FileAcc) ->
		{ok, {AnnAST, Info}} = refac_util:parse_annotate_file(File, true, DirList),
		{value, {module, ModName}} = lists:keysearch(module, 1, Info),
		F1 = fun(Node, ModAcc) ->
			     case refac_syntax:type(Node) of 
				 function ->
				     F2= fun (Node1, FunAcc) ->    
						 case refac_syntax:type(Node1) of 
						     application ->
							 case is_register_app(Node1) of 
							     true -> 
								 [RegName, _Pid] = refac_syntax:application_arguments(Node1),
								 RegNameValues = evaluate_expr(Files, ModName, AnnAST, Node, RegName),
								 RegNameValues++FunAcc;
							     _ -> FunAcc
							 end; 
						     atom ->
							 case lists:keysearch(pname, 1, refac_syntax:get_ann(Node1)) of 
							     {value, {pname, _V}} ->
								 [{value, refac_syntax:atom_value(Node1)}|FunAcc];
							     _ -> FunAcc
							 end;
						     _ ->  FunAcc
						 end
					 end,
				     refac_syntax_lib:fold(F2, [], Node)++ModAcc;
				 _-> ModAcc 
			     end
		     end,
		refac_syntax_lib:fold(F1, [], AnnAST) ++ FileAcc
	   end,			 
    Acc =lists:foldl(F, [], Files),
    {Names, UnKnowns} = lists:partition(fun({Tag,_V})-> Tag==value end, Acc),
    {lists:usort(lists:map(fun({value, P}) -> P end, Names)), lists:usort(UnKnowns)}.
    
is_register_app(T) ->
     case refac_syntax:type(T) of
       application ->
 	  Operator = refac_syntax:application_operator(T),
 	  Ann = refac_syntax:get_ann(Operator),
 	  case lists:keysearch(fun_def, 1, Ann) of
 	    {value, {fun_def, {erlang, register, 2, _, _}}} -> true;
 	    _ -> false
 	  end;
       _ -> false
     end.


evaluate_expr(Files, ModName, AnnAST, FunDef, Expr) ->
    F = fun(E) ->
		Es = [refac_syntax:revert(E)],
		case catch erl_eval:exprs(Es, []) of 
		    {value, V, _} -> {value, V};
		    _ ->
			FunName = refac_syntax:data(refac_syntax:function_name(FunDef)),
			Arity = refac_syntax:function_arity(FunDef),
			{StartPos, _} = refac_util:get_range(Expr),
			{unknown, {ModName, FunName, Arity, StartPos}}
		end
	end,
    Exprs = case refac_util:get_free_vars(Expr) of 
		[] -> [Expr];
		_ ->  refac_slice:backward_slice(Files, AnnAST, ModName, FunDef, Expr)
	    end,
    Values = lists:map(F, Exprs),
    Values.

do_rename_process(CurrentFileName, AnnAST, OldProcessName, NewProcessName, SearchPaths) ->
    {AnnAST1, _Changed} = refac_util:stop_tdTP(fun do_rename_process/2, AnnAST, {OldProcessName, NewProcessName}),
    OtherFiles = refac_util:expand_files(SearchPaths, ".erl") -- [CurrentFileName],
    Results = do_rename_process_in_other_modules(OtherFiles, OldProcessName, NewProcessName, SearchPaths),
    [{{CurrentFileName, CurrentFileName}, AnnAST1} | Results].

do_rename_process_in_other_modules(Files, OldProcessName, NewProcessName, SearchPaths) ->
    case Files of 
	[] ->
	    [];
	[F |Fs] ->
	     ?wrangler_io("The current file under refactoring is:\n~p\n", [F]),
	    {ok, {AnnAST, _Info}} = refac_util:parse_annotate_file(F, true, SearchPaths),
	    {AnnAST1, Changed} = do_rename_process(AnnAST, {OldProcessName, NewProcessName}),
	    if Changed ->
		    [{{F, F}, AnnAST1} | do_rename_process_in_other_modules(Fs, OldProcessName, NewProcessName, SearchPaths)];
		true -> do_rename_process_in_other_modules(Fs, OldProcessName, NewProcessName, SearchPaths)
	    end
    end.
    
 
do_rename_process(Node, {OldProcessName, NewProcessName}) ->
    case refac_syntax:type(Node) of 
	atom ->
	    As = refac_syntax:get_ann(Node),
	    case lists:keysearch(pname, 1, As) of 
		{value, {pname, _}} ->
		    case refac_syntax:atom_value(Node) of 
			OldProcessName -> {refac_syntax:copy_attrs(Node, refac_syntax:atom(NewProcessName)), true};
			_ -> {Node, false}
		    end;
		_ -> {Node, false}
	    end;
	_ -> {Node, false}
    end.


stop_tdTU(Function, S, Node) ->
    {Res, _} = stop_tdTU_1(Function, S, Node),
    Res.

stop_tdTU_1(Function, S, Node) ->
    case Function(Node, S) of
      {R, true} -> {R, true};
      {_R, false} ->
	  case erl_syntax:subtrees(Node) of
	    [] -> {[], false};
	    Gs ->
		Flattened_Gs = [T || G <- Gs, T <- G],
		case Flattened_Gs of
		  [] -> {[], false};
		  [_H | _T1] -> S1 = [[stop_tdTU_1(Function, [], T) || T <- G] || G <- Gs],
			      S2 = [S12 || G<-S1, {S12, _B} <- G],
				{S++lists:append(S2), true}
		end
	  end
    end.

check_atoms(CurrentFile, AtomName, SearchPaths) ->
    Atoms = collect_atoms(CurrentFile, AtomName, SearchPaths),
    case Atoms of 
	[] ->
	    ok;
	_ -> ?wrangler_io("\n*************************************Warning****************************************\n",[]),
	     ?wrangler_io("Wrangler could not decide whether to rename atom(s) occuring at the followng location(s):\n",[]),
	     lists:foreach(fun({M, Pos,_}) ->
				      ?wrangler_io("Location: module:~p, {line,col}:~p\n", [M,Pos]) end, Atoms)
    end.
    
collect_atoms(CurrentFile, AtomName, SearchPaths) ->
    Files = [CurrentFile|(refac_util:expand_files(SearchPaths, ".erl")--[CurrentFile])],
    Res=lists:flatmap(fun(F) ->
			  collect_atoms(F, SearchPaths) end, Files),
    lists:filter(fun({_F, _Pos, Name}) ->
			    Name == AtomName end, Res).
    
collect_atoms(File, SearchPaths)->
    {ok, {AnnAST,Info}} = refac_util:parse_annotate_file(File, true, SearchPaths),     
    {value, {module, ModName}}= lists:keysearch(module,1, Info),
    F= fun(Node, S) ->
	       case refac_syntax:type(Node) of
		   attribute -> {S, true};
		   module_qualifier -> {S, true};
		   arity_qualifier -> {S, true};
		   atom -> 
		       case lists:keysearch(fun_def,1, refac_syntax:get_ann(Node)) of 
			       {value, _} -> {S, true};
			   _ -> case lists:keysearch(pname,1, refac_syntax:get_ann(Node)) of 
				    {value, _} ->
					{S, true};
				    _ -> {[Node|S], true}
				end
		       end;
		   _ -> {S, false}					  
	       end
       end,
    F2 = fun(Node, S) ->
		 case refac_syntax:type(Node) of 
		     application -> Op = refac_syntax:application_operator(Node),
				    Args = list_to_tuple(refac_syntax:application_arguments(Node)),
				    case lists:keysearch(fun_def, 1, refac_syntax:get_ann(Op)) of 
 				      {value, {fun_def, {erlang, FunName, 3, _, _}}}-> 
					    case lists:member(FunName, [apply, spawn, spawn_link]) of 
						true ->
						    Mod = element(1, Args),
						    Fun = element(2, Args),
						    S1 = case refac_syntax:type(Mod) of 
							     atom -> [Mod|S];
							     _ -> S
							 end,
						    case refac_syntax:type(Fun) of 
							atom ->[Fun|S1];
							_ -> S1
						    end;
						_ ->
						    S
					    end;
					{value, {fun_def, {erlang, FunName, 4, _, _}}}-> 
					    case lists:member(FunName, [spawn, spawn_link]) of 
						true ->
						    Mod = element(2, Args),
						    Fun = element(3, Args),
						    S1 = case refac_syntax:type(Mod) of 
							     atom -> [Mod|S];
							     _ -> S
							 end,
						    case refac_syntax:type(Fun) of 
							atom ->[Fun|S1];
							_ -> S1
						    end;
						_ ->
						    S
					    end;
					_ ->
					    S
				    end;
		     _ -> S
		 end
	 end,
    Res1 = stop_tdTU(F, [], AnnAST), 
    Res2 = refac_syntax_lib:fold(F2, [], AnnAST),
    Res11 = lists:map(fun(A) -> {ModName, refac_syntax:get_pos(A), refac_syntax:atom_value(A)} end, Res1),
    Res21 = lists:map(fun(A) -> {ModName, refac_syntax:get_pos(A), refac_syntax:atom_value(A)} end, Res2),
    lists:subtract(lists:usort(Res11), lists:usort(Res21)).

is_process_name(Name) ->
    refac_util:is_fun_name(Name) and (list_to_atom(Name) =/= undefined).

