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
-module(refac_rename_process).

-export([rename_process/6, rename_process_eclipse/6, rename_process_1/6, rename_process_1_eclipse/5]).

-include("../include/wrangler.hrl").

%%-spec(rename_process/6::(string(), integer(), integer(), string(), [dir()], integer()) ->
%%	     {error, string()} | {undecidables, string(), string()}| {ok, [filename()]}).
rename_process(FileName, Line, Col, NewName, SearchPaths, TabWidth) ->
    rename_process(FileName, Line, Col, NewName, SearchPaths, TabWidth, emacs).

%%-spec(rename_process_eclipse/6::(string(), integer(), integer(), string(), [dir()], integer()) ->
%%	     {error, string()} | {undecidables, string()} | {ok, [{filename(), filename(), string()}]}).
rename_process_eclipse(FileName, Line, Col, NewName, SearchPaths, TabWidth) ->
    rename_process(FileName, Line, Col, NewName, SearchPaths, TabWidth, eclipse).


rename_process(FileName, Line, Col, NewName, SearchPaths, TabWidth, Editor) ->
    ?wrangler_io("\nCMD: ~p:rename_process( ~p, ~p, ~p, ~p,~p, ~p).\n", [?MODULE, FileName, Line, Col, NewName, SearchPaths, TabWidth]),
    Cmd = "CMD: " ++ atom_to_list(?MODULE) ++ ":rename_process(" ++ "\"" ++
	    FileName ++ "\", " ++ integer_to_list(Line) ++
	      ", " ++ integer_to_list(Col) ++ ", " ++ "\"" ++ NewName ++ "\","
		++ "[" ++ refac_misc:format_search_paths(SearchPaths) ++ "]," ++ integer_to_list(TabWidth) ++ ").",
    case is_process_name(NewName) of
      true ->
	  _Res = refac_annotate_pid:ann_pid_info(SearchPaths, TabWidth),  %%TODO: check whether asts are already annotated.
	  {ok, {AnnAST, _Info}} = refac_util:parse_annotate_file(FileName, true, SearchPaths, TabWidth), %% TODO: rename to get_ast.
	  NewProcessName = list_to_atom(NewName),
	  case pos_to_process_name(AnnAST, {Line, Col}) of
	    {ok, ProcessName} ->
		case NewProcessName =/= ProcessName of
		  true ->
		      case pre_cond_check(NewProcessName, SearchPaths) of
			ok ->
			    Results = do_rename_process(FileName, AnnAST, ProcessName, NewProcessName, SearchPaths, TabWidth),
			    check_atoms(FileName, ProcessName, SearchPaths, TabWidth),
			    refac_util:write_refactored_files(Results, Editor, Cmd);
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

%%-spec(rename_process_1/6::(string(), string(), string(), [dir()], integer(), string()) -> {ok, [filename()]}).
rename_process_1(FileName, OldProcessName, NewProcessName, SearchPaths, TabWidth, Cmd) ->
    rename_process_1(FileName, OldProcessName, NewProcessName, SearchPaths, TabWidth, emacs, Cmd).

%%-spec(rename_process_1_eclipse/5::(string(), string(), string(), [dir()], integer()) -> {ok, [{filename(), filename(), string()}]}).
rename_process_1_eclipse(FileName, OldProcessName, NewProcessName, SearchPaths, TabWidth) ->
    rename_process_1(FileName, OldProcessName, NewProcessName, SearchPaths, TabWidth, eclipse, "").

%%-spec(rename_process_1/7::(string(), string(), string(), [dir()], integer(), editor(), string()) -> {ok, [filename()]}|{ok, [{filename(), filename(), string()}]}).
rename_process_1(FileName, OldProcessName1, NewProcessName1, SearchPaths, TabWidth, Editor, Cmd) ->
    OldProcessName = list_to_atom(OldProcessName1),
    NewProcessName = list_to_atom(NewProcessName1),
    {ok, {AnnAST, _Info}} = refac_util:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    Results = do_rename_process(FileName, AnnAST, OldProcessName, NewProcessName, SearchPaths, TabWidth),
    check_atoms(FileName, OldProcessName, SearchPaths, TabWidth),
    refac_util:write_refactored_files(Results, Editor, Cmd).


pos_to_process_name(Node, Pos) ->
    case ast_traverse_api:once_tdTU(fun pos_to_process_name_1/2, Node, Pos) of
      {_, false} -> {error, "Wrangler could not infer that the atom selected represents a process name!"};
      {R, true} -> {ok, R}
    end.

pos_to_process_name_1(Node, Pos) ->
    As = refac_syntax:get_ann(Node),
    case refac_syntax:type(Node) of
      atom ->
	  {Start, End} = refac_misc:get_start_end_loc(Node),
	  case Start =< Pos andalso Pos =< End of
	    true ->
		case lists:keysearch(pname, 1, As) of
		  {value, {pname, _V}} ->
		      {refac_syntax:atom_value(Node), true};
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
								 RegNameValues = refac_register_pid:evaluate_expr(Files, ModName, AnnAST, Node, RegName),
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


do_rename_process(CurrentFileName, AnnAST, OldProcessName, NewProcessName, SearchPaths, TabWidth) ->
    {AnnAST1, _Changed} = ast_traverse_api:stop_tdTP(fun do_rename_process/2, AnnAST, {OldProcessName, NewProcessName}),
    OtherFiles = refac_util:expand_files(SearchPaths, ".erl") -- [CurrentFileName],
    Results = do_rename_process_in_other_modules(OtherFiles, OldProcessName, NewProcessName, SearchPaths, TabWidth),
    [{{CurrentFileName, CurrentFileName}, AnnAST1}| Results].

do_rename_process_in_other_modules(Files, OldProcessName, NewProcessName, SearchPaths, TabWidth) ->
    case Files of 
	[] ->
	    [];
	[F |Fs] ->
	     ?wrangler_io("The current file under refactoring is:\n~p\n", [F]),
	    {ok, {AnnAST, _Info}} = refac_util:parse_annotate_file(F, true, SearchPaths, TabWidth),
	    {AnnAST1, Changed} = do_rename_process(AnnAST, {OldProcessName, NewProcessName}),
	    if Changed ->
		    [{{F, F}, AnnAST1} | do_rename_process_in_other_modules(Fs, OldProcessName, NewProcessName, SearchPaths, TabWidth)];
		true -> do_rename_process_in_other_modules(Fs, OldProcessName, NewProcessName, SearchPaths, TabWidth)
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

%% TODO: Check this error!!
%% refac_rename_process.erl:233: The pattern [] can never match the type [any(),...]
check_atoms(CurrentFile, AtomName, SearchPaths, TabWidth) ->
    Atoms = collect_atoms(CurrentFile, AtomName, SearchPaths, TabWidth),
    case Atoms of 
	[] ->
	    ok;
	_ -> ?wrangler_io("\n*************************************Warning****************************************\n",[]),
	     ?wrangler_io("Wrangler could not decide whether to rename atom(s) occuring at the followng location(s):\n",[]),
	     lists:foreach(fun({M, Pos,_}) ->
				      ?wrangler_io("Location: module:~p, {line,col}:~p\n", [M,Pos]) end, Atoms)
    end.
    
collect_atoms(CurrentFile, AtomName, SearchPaths, TabWidth) ->
    Files = [CurrentFile| refac_util:expand_files(SearchPaths, ".erl") -- [CurrentFile]],
    lists:flatmap(fun (F) ->
			  {ok, {AnnAST,_Info}} = refac_util:parse_annotate_file(F, true, SearchPaths, TabWidth),
			  refac_atom_utils:collect_unsure_atoms_in_file(AnnAST, AtomName, p_atom)
		  end, Files).
  
is_process_name(Name) ->
    refac_misc:is_fun_name(Name) and (list_to_atom(Name) =/= undefined).
