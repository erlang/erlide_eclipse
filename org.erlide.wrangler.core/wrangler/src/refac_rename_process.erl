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
%% Refactoring: Rename a process name.
%%
%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%%
%% =====================================================================

%% TOADD: functionalities to check OTP behaviours.
%% @private
-module(refac_rename_process).

-export([rename_process/7, rename_process_eclipse/6, rename_process_1/7, rename_process_1_eclipse/5]).

-include("../include/wrangler_internal.hrl").

%%-spec(rename_process_eclipse/6::(string(), integer(), integer(), string(), [dir()], integer()) ->
%%	     {error, string()} | {undecidables, string()} | {ok, [{filename(), filename(), string()}]}).
rename_process_eclipse(FileName, Line, Col, NewName, SearchPaths, TabWidth) ->
    rename_process(FileName, Line, Col, NewName, SearchPaths, eclipse, TabWidth).

rename_process(FileName, Line, Col, NewName, SearchPaths, Editor, TabWidth) ->
     ?wrangler_io("\nCMD: ~p:rename_process( ~p, ~p, ~p, ~p,~p, ~p, ~p).\n", 
                  [?MODULE, FileName, Line, Col, NewName, SearchPaths, Editor, TabWidth]),
     Cmd = "CMD: " ++ atom_to_list(?MODULE) ++ ":rename_process(" ++ "\"" ++
	     FileName ++ "\", " ++ integer_to_list(Line) ++
	       ", " ++ integer_to_list(Col) ++ ", " ++ "\"" ++ NewName ++ "\","
      ++ "[" ++ wrangler_misc:format_search_paths(SearchPaths) ++ "]," ++ integer_to_list(TabWidth) ++ ").",
    case is_process_name(NewName) of
	true ->
	    _Res = wrangler_annotate_pid:ann_pid_info(SearchPaths, TabWidth),  %%TODO: check whether asts are already annotated.
	    {ok, {AnnAST, _Info}} = wrangler_ast_server:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
            NewProcessName = list_to_atom(NewName),
	    case pos_to_process_name(AnnAST, {Line, Col}) of
		{ok, ProcessName} ->
		    case NewProcessName =/= ProcessName of
			true ->
			    case pre_cond_check(NewProcessName, SearchPaths) of
				ok ->
				    Results = do_rename_process(FileName, AnnAST, ProcessName, NewProcessName, SearchPaths, TabWidth),
				    check_atoms(FileName, ProcessName, SearchPaths, TabWidth),
				    wrangler_write_file:write_refactored_files(Results, Editor, TabWidth, Cmd);
				undecidables ->
				    case Editor of
					emacs -> {undecidables, atom_to_list(ProcessName), Cmd};
					eclipse -> {undecidables, atom_to_list(ProcessName)}
				    end;
				{error, Reason} ->
				    {error, Reason}
			    end;
			_ -> {error, "The new process name is the same as the process name selected!"}
		    end;
		{error, Reason} -> {error, Reason}
	    end;
	false -> {error, "Invalid new process name."}
    end.

%%-spec(rename_process_1_eclipse/5::(string(), string(), string(), [dir()], integer()) -> 
%%                      {ok, [{filename(), filename(), string()}]}).
rename_process_1_eclipse(FileName, OldProcessName, NewProcessName, SearchPaths, TabWidth) ->
    rename_process_1(FileName, OldProcessName, NewProcessName, SearchPaths, eclipse, TabWidth, "").

%%-spec(rename_process_1/7::(string(), string(), string(), [dir()], editor(), integer(), string()) ->
%%                        {ok, [filename()]}|{ok, [{filename(), filename(), string()}]}).
rename_process_1(FileName, OldProcessName1, NewProcessName1, SearchPaths, Editor, TabWidth, Cmd) ->
     OldProcessName = list_to_atom(OldProcessName1),
     NewProcessName = list_to_atom(NewProcessName1),
     {ok, {AnnAST, _Info}} = wrangler_ast_server:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
     Results = do_rename_process(FileName, AnnAST, OldProcessName, NewProcessName, SearchPaths, TabWidth),
     check_atoms(FileName, OldProcessName, SearchPaths, TabWidth),
     wrangler_write_file:write_refactored_files(Results, Editor, TabWidth, Cmd).


pos_to_process_name(Node, Pos) ->
    case api_ast_traverse:once_tdTU(fun pos_to_process_name_1/2, Node, Pos) of
      {_, false} -> {error, "Wrangler could not infer that the atom selected represents a process name!"};
      {R, true} -> {ok, R}
    end.

pos_to_process_name_1(Node, Pos) ->
    As = wrangler_syntax:get_ann(Node),
    case wrangler_syntax:type(Node) of
	atom ->
	    {Start, End} = wrangler_misc:start_end_loc(Node),
	    case Start =< Pos andalso Pos =< End of
		true ->
		    case lists:keysearch(type, 1, As) of
			{value, {type, p_atom}} ->
			    {wrangler_syntax:atom_value(Node), true};
			_ -> {[], false}
		    end;
		_ -> {[], false}
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
			  lists:foreach(fun({_M, _F, _A, {_L,_}}) -> ?wrangler_io("Location: module: ~p, function:~p/~p, line:~p\n", [_M, _F, _A, _L])
					end, UnDecidables1),
			  undecidables
		 end
    end.

collect_process_names(DirList) ->
    Files = wrangler_misc:expand_files(DirList, ".erl"),
    F = fun (File, FileAcc) ->
		{ok, {AnnAST, Info}} = wrangler_ast_server:parse_annotate_file(File, true, DirList),
		{value, {module, ModName}} = lists:keysearch(module, 1, Info),
		F1 = fun (Node, ModAcc) ->
			     case wrangler_syntax:type(Node) of
				 function ->
				     F2 = fun (Node1, FunAcc) ->
						  case wrangler_syntax:type(Node1) of
						      application ->
							  case is_register_app(Node1) of
							      true ->
								  [RegName, _Pid] = wrangler_syntax:application_arguments(Node1),
								  RegNameValues = evaluate_expr(Files, ModName, AnnAST, Node, RegName),
								  RegNameValues++FunAcc;
							      _ -> FunAcc
							  end;
						      atom ->
							  case lists:keysearch(p_name, 1, wrangler_syntax:get_ann(Node1)) of
							      {value, {p_name, _V}} ->
								  [{value, wrangler_syntax:atom_value(Node1)}| FunAcc];
							      _ -> FunAcc
							  end;
						      _ -> FunAcc
						  end
					  end,
				     api_ast_traverse:fold(F2, [], Node) ++ ModAcc;
				 _ -> ModAcc
			     end
		     end,
		api_ast_traverse:fold(F1, [], AnnAST) ++ FileAcc
	end,
    Acc = lists:foldl(F, [], Files),
    {Names, UnKnowns} = lists:partition(fun ({Tag,_V}) -> Tag==value end, Acc),
    {lists:usort(lists:map(fun ({value, P}) -> P end, Names)), lists:usort(UnKnowns)}.
    
is_register_app(T) ->
     case wrangler_syntax:type(T) of
       application ->
           Operator = wrangler_syntax:application_operator(T),
           Ann = wrangler_syntax:get_ann(Operator),
 	  case lists:keysearch(fun_def, 1, Ann) of
 	    {value, {fun_def, {erlang, register, 2, _, _}}} -> true;
 	    _ -> false
 	  end;
       _ -> false
     end.

do_rename_process(CurrentFileName, AnnAST, OldProcessName, NewProcessName, SearchPaths, TabWidth) ->
    {AnnAST1, _Changed} = api_ast_traverse:stop_tdTP(fun do_rename_process/2, AnnAST, {OldProcessName, NewProcessName}),
    OtherFiles = wrangler_misc:expand_files(SearchPaths, ".erl") -- [CurrentFileName],
    Results = do_rename_process_in_other_modules(OtherFiles, OldProcessName, NewProcessName, SearchPaths, TabWidth),
    [{{CurrentFileName, CurrentFileName}, AnnAST1}| Results].

do_rename_process_in_other_modules(Files, OldProcessName, NewProcessName, SearchPaths, TabWidth) ->
    case Files of
	[] ->
	    [];
	[F| Fs] ->
	    ?wrangler_io("The current file under refactoring is:\n~p\n", [F]),
	    {ok, {AnnAST, _Info}} = wrangler_ast_server:parse_annotate_file(F, true, SearchPaths, TabWidth),
	    {AnnAST1, Changed} = do_rename_process(AnnAST, {OldProcessName, NewProcessName}),
	    if Changed ->
		   [{{F, F}, AnnAST1}| do_rename_process_in_other_modules(Fs, OldProcessName, NewProcessName, SearchPaths, TabWidth)];
	       true -> do_rename_process_in_other_modules(Fs, OldProcessName, NewProcessName, SearchPaths, TabWidth)
	    end
    end.
    
 
do_rename_process(Node, {OldProcessName, NewProcessName}) ->
    case wrangler_syntax:type(Node) of
	atom ->
	    As = wrangler_syntax:get_ann(Node),
	    case lists:keysearch(type, 1, As) of 
		{value, {type, p_atom}} ->
		    case wrangler_syntax:atom_value(Node) of
			OldProcessName -> {wrangler_syntax:copy_attrs(Node, wrangler_syntax:atom(NewProcessName)), true};
			_ -> {Node, false}
		    end;
		_ -> {Node, false}
	    end;
	_ -> {Node, false}
    end.

%% TODO: Check this error!!
%% refac_rename_process.erl:233: The pattern [] can never match the type [any(),...]
check_atoms(CurrentFile, AtomName, SearchPaths, TabWidth) ->
    Atoms = collect_atoms(CurrentFile, AtomName, SearchPaths, TabWidth),
    case Atoms of 
	[] ->
	    ok;
	_ -> ?wrangler_io("\n*************************************Warning****************************************\n",[]),
	     ?wrangler_io("Wrangler could not decide whether to rename atom(s) occuring at the followng location(s):\n",[]),
	     lists:foreach(fun({_M, _Pos, _}) ->
				      ?wrangler_io("Location: module:~p, {line,col}:~p\n", [_M,_Pos]) end, Atoms)
    end.

collect_atoms(CurrentFile, AtomName, SearchPaths, TabWidth) ->
    Files = [CurrentFile| wrangler_misc:expand_files(SearchPaths, ".erl") -- [CurrentFile]],
    lists:flatmap(fun (F) ->
			  {ok, {AnnAST,_Info}} = wrangler_ast_server:parse_annotate_file(F, true, SearchPaths, TabWidth),
			  wrangler_atom_utils:collect_unsure_atoms_in_file(AnnAST, [AtomName], p_atom)
		  end, Files).

is_process_name(Name) ->
    api_refac:is_fun_name(Name) and (list_to_atom(Name) =/= undefined).

%% An duplication of function defined in refac_register_pid. 
%% Need to be refactored.
evaluate_expr(Files, ModName, AnnAST, FunDef, Expr) ->
    F = fun (E) ->
		Es = [wrangler_syntax:revert(E)],
		case catch erl_eval:exprs(Es, []) of
		    {value, V, _} -> {value, V};
		    _ ->
			FunName = wrangler_syntax:data(wrangler_syntax:function_name(FunDef)),
			Arity = wrangler_syntax:function_arity(FunDef),
			{StartPos, _} = wrangler_misc:start_end_loc(Expr),
			{unknown, {ModName, FunName, Arity, StartPos}}
		end
	end,
    Exprs = case api_refac:free_vars(Expr) of
		[] -> [Expr];
		_ -> wrangler_slice:backward_slice(Files, AnnAST, ModName, FunDef, Expr)
	    end,
    lists:map(F, Exprs).
