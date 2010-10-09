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

-module(side_effect_api).

-export([has_side_effect/1, has_side_effect/3, build_local_side_effect_tab/2, build_lib_side_effect_tab/1]).

-include("../include/wrangler.hrl").


%%=================================================================

has_side_effect(Node) ->
    has_side_effect("unknown", Node, []).
    
%% @doc Return true if the abstract syntax tree represented by Node has side effect, 
%%      otherwise return false. As to parameters, File represents filename of the
%%      code to which Node belongs,  Node is the abstract syntax tree representaion of 
%%      the syntax phrase of interest, and SearchPaths specifies the directories to 
%%      search for related local Erlang source files.
%% @spec has_side_effect(File::filename(), Node::syntaxTree(), SearchPaths::[dir()])-> true|false|unknown

%%-spec(has_side_effect(File::filename(), Node::syntaxTree(), SearchPaths::[dir()])-> true|false|unknown).
has_side_effect(_File, Node, _SearchPaths) ->
    LibSideEffectFile = list_to_atom(filename:join(?WRANGLER_DIR, "plt/side_effect_plt")),
    LibPlt = from_dets(lib_side_effect_plt, LibSideEffectFile),
    Res = check_side_effect(Node, LibPlt, none),
    case Res of
      true -> dets:close(LibSideEffectFile),
	      ets:delete(LibPlt),
	      true;
      false -> dets:close(LibSideEffectFile),
	       ets:delete(LibPlt),
	       false;
      unknown ->
	  dets:close(LibSideEffectFile),
	  ets:delete(LibPlt),
	  unknown
    end.

 %% The following is too slow for a large project.
	    %% CurrentDir = filename:dirname(normalise_file_name(File)),
	    %% LocalSideEffectFile = filename:join(CurrentDir, "local_side_effect_tab"),
	    %% build_local_side_effect_tab(LocalSideEffectFile, SearchPaths),
	    %% LocalPlt = from_dets(local_side_effect_plt, LocalSideEffectFile),
	    %% Res1 = check_side_effect(Node, LibPlt, LocalPlt),
	    %% dets:close(LibSideEffectFile),
	    %% dets:close(list_to_atom(LocalSideEffectFile)),
	    %% ets:delete(LocalPlt),
	    %% ets:delete(LibPlt),
	    %% Res1	
           	



%%=================================================================
%% @spec build_local_side_effect_tab(File::filename(), SearchPaths::[dir()]) -> true.
%% @doc Build a local side effect table for File and the files contained in SearchPaths, and
%% put the result to the dets file: local_side_effect_tab. 
%%
%% @see build_lib_side_effect_tab/2.
%%-spec(build_local_side_effect_tab(File::filename(), SearchPaths::[dir()]) -> true).
build_local_side_effect_tab(File, SearchPaths) ->
    ValidSearchPaths = lists:all(fun (X) -> filelib:is_dir(X) end, SearchPaths),
    case ValidSearchPaths of
      true -> ok;
      false ->
	  throw("One of the directories sepecified in the search paths does not exist, please check the customization!")
    end,
    CurrentDir = filename:dirname(normalise_file_name(File)),
    SideEffectFile = filename:join(CurrentDir, "local_side_effect_tab"),
    LibSideEffectFile = filename:join(?WRANGLER_DIR, "plt/side_effect_plt"),
    LibPlt = from_dets(lib_side_effect_plt, LibSideEffectFile),
    Dirs = lists:usort([CurrentDir| SearchPaths]),
    Files = refac_util:expand_files(Dirs, ".erl"),
    SideEffectFileModifiedTime = filelib:last_modified(SideEffectFile),
    FilesToAnalyse = [F || F <- Files, SideEffectFileModifiedTime < filelib:last_modified(F)],
    LocalPlt = case filelib:is_file(SideEffectFile) of
		 true -> from_dets(local_side_effect_tab, SideEffectFile);
		 _ -> ets:new(local_side_effect_tab, [set, public])
	       end,
    #callgraph{callercallee = _CallerCallee, scc_order = Sccs, external_calls = _E} = 
	wrangler_callgraph_server:build_scc_callgraph(FilesToAnalyse),
    build_side_effect_tab(Sccs, LocalPlt, LibPlt),
    to_dets(LocalPlt, SideEffectFile),
    dets:close(list_to_atom(LibSideEffectFile)),
    ets:delete(LocalPlt),
    ets:delete(LibPlt).

%%=================================================================
%% @spec build_lib_side_effect_tab(FileOrDirs::[fileName()|dir()]) -> true.
%% @doc Build the side effect table for Erlang libraries specified in FileOrDirs, and
%% put the result to the dets file: plt/side_effect_plt. 
%%
%% @see build_local_side_effect_tab/2.
%%-spec(build_lib_side_effect_tab([dir()]) -> true).
build_lib_side_effect_tab(SearchPaths) ->
    Plt = ets:new(side_effect_table, [set, public]),
    #callgraph{callercallee = _CallerCallee, scc_order = Sccs, external_calls = _E} =
	wrangler_callgraph_server:build_scc_callgraph(SearchPaths),
    build_side_effect_tab(Sccs, Plt, ets:new(dummy_tab, [set, public])),
    ets:insert(Plt, bifs_side_effect_table()),
    File = filename:join(?WRANGLER_DIR, "plt/side_effect_plt"),
    to_dets(Plt, File),
    ets:delete(Plt).

from_dets(Name, Dets) when is_atom(Name) ->
    Plt = ets:new(Name, [set, public]),
    case dets:open_file(Dets, [{access, read}]) of
      {ok, D} ->
	  true = ets:from_dets(Plt, D),
	  ok = dets:close(D),
	  Plt;
      {error, Reason} -> erlang:error(Reason)
    end.

to_dets(Plt, Dets) ->
    file:delete(Dets),
    MinSize = ets:info(Plt, size),
	{ok, DetsRef} = dets:open_file(Dets, [{min_no_slots, MinSize}]),
	ok = dets:from_ets(DetsRef, Plt),
    ok = dets:sync(DetsRef),
    ok = dets:close(DetsRef).

build_side_effect_tab([Scc| Left], Side_Effect_Tab, OtherTab) ->
    R = side_effect_scc(Scc, Side_Effect_Tab, OtherTab),
    true = ets:insert(Side_Effect_Tab,
		      [{{Mod, Fun, Arg}, R} || {{Mod, Fun, Arg}, _F} <- Scc]),
    build_side_effect_tab(Left, Side_Effect_Tab, OtherTab);
build_side_effect_tab([], Side_Effect_Tab, _) -> Side_Effect_Tab.

side_effect_scc([{{_M, _F, _A}, Def}, F| Left], Side_Effect_Tab, OtherTab) ->
    case check_side_effect(Def, Side_Effect_Tab, OtherTab) of
      true -> true;
      _ -> side_effect_scc([F| Left], Side_Effect_Tab, OtherTab)
    end;
side_effect_scc([{{_M, _F, _A}, Def}], Side_Effect_Tab, OtherTab) ->
    check_side_effect(Def, Side_Effect_Tab, OtherTab).



check_side_effect(Node, LibPlt, LocalPlt) ->
    LookUp = fun (MFA) ->
		     case lookup(LibPlt, MFA) of
		       {value, S} -> S;
		       _ ->
			   case LocalPlt of
			     none -> unknown;
			     _ -> case lookup(LocalPlt, MFA) of
				    {value, S} -> S;
				    _ -> unknown
				  end
			   end
		     end
	     end,
    case refac_syntax:type(Node) of
      receive_expr -> true;
      infix_expr -> Op = refac_syntax:operator_literal(refac_syntax:infix_expr_operator(Node)),
		    Op == "!";
      fun_expr -> false;
      implicit_fun -> false;
      application ->
	  Operator = refac_syntax:application_operator(Node),
	  Arity = length(refac_syntax:application_arguments(Node)),
	  case refac_syntax:type(Operator) of
	    atom ->
		Op = refac_syntax:atom_value(Operator),
		{value, {fun_def, {M, _N, _A, _P1, _P}}} = lists:keysearch(fun_def, 1, refac_syntax:get_ann(Operator)),
		LookUp({M, Op, Arity});
	    module_qualifier ->
		Mod = refac_syntax:module_qualifier_argument(Operator),
		Body = refac_syntax:module_qualifier_body(Operator),
		case {refac_syntax:type(Mod), refac_syntax:type(Body)} of
		  {atom, atom} ->
		      M = refac_syntax:atom_value(Mod),
		      Op = refac_syntax:atom_value(Body),
		      LookUp({M, Op, Arity});
		  _ -> unknown
		end;
	    _ -> unknown
	  end;
      arity_qualifier ->
	  Fun = refac_syntax:arity_qualifier_body(Node),
	  A = refac_syntax:arity_qualifier_argument(Node),
	  case {refac_syntax:type(Fun), refac_syntax:type(A)} of
	    {atom, integer} ->
		FunName = refac_syntax:atom_value(Fun),
		Arity = refac_syntax:integer_value(A),
		{value, {fun_def, {M, _N, _A, _P1, _P}}} = lists:keysearch(fun_def, 1, refac_syntax:get_ann(FunName)),
		LookUp({M, FunName, Arity});
	    _ -> unknown
	  end;
      atom -> false;
      _ ->
	  case refac_syntax:subtrees(Node) of
	    [] -> false;
	    Ts ->
		Res = lists:flatten([[check_side_effect(T, LibPlt, LocalPlt) || T <- G] || G <- Ts]),
		case lists:member(true, Res) of
		  true -> true;
		  false ->
		      case lists:member(unknown, Res) of
			true -> unknown;
			_ -> false
		      end
		end
	  end
    end.

normalise_file_name(Filename) ->
    filename:join(filename:split(Filename)).

lookup(Plt, {M, F, A}) ->
    case ets:lookup(Plt, {M, F, A}) of
      [] -> none;
      [{_MFA, S}] -> {value, S}
    end.


%% =====================================================================
%% @spec bifs_side_effect_table()->[{{atom(), atom(), integer()}, boolean()}]
%% @doc The side effect table of BIFs.
%%-spec(bifs_side_effect_table()->[{{atom(), atom(), integer()}, boolean()}]).
bifs_side_effect_table() ->
    [{{erlang, abs, 1}, false}, {{erlang, append_element, 2}, false}, {{erlang, atom_to_list, 1}, false},
     {{erlang, binary_to_list, 1}, false}, {{erlang, binary_to_list, 3}, false}, {{erlang, binary_to_term, 1}, false},
     {{erlang, bump_reductions, 1}, false}, {{erlang, cancel_timer, 1}, true}, {{erlang, check_process_code, 1}, false},
     {{erlang, concat_binary, 1}, false}, {{erlang, data, 3}, false}, {{erlang, delete_module, 1}, true},
     {{erlang, demonitor, 1}, false}, {{erlang, disconnect_node, 1}, true}, {{erlang, display, 1}, true},
     {{erlang, element, 2}, false}, {{erlang, erase, 0}, true}, {{erlang, erase, 1}, true}, {{erlang, error, 1}, true},
     {{erlang, error, 2}, true}, {{erlang, exit, 1}, true}, {{erlang, exit, 2}, true}, {{erlang, fault, 1}, true},
     {{erlang, fault, 2}, true}, {{erlang, float, 1}, false}, {{erlang, float_to_list, 1}, false},
     {{erlang, fun_info, 2}, false}, {{erlang, fun_info, 1}, false}, {{erlang, fun_to_list, 1}, false},
     {{erlang, function_exported, 3}, true}, {{erlang, garbage_collect, 1}, true}, {{erlang, garbage_collect, 0}, true},
     {{erlang, get, 0}, true}, {{erlang, get, 1}, true}, {{erlang, get_cookie, 0}, true},{{erlang, get_keys, 1}, true},
     {{erlang, get_stacktrace, 0}, true}, {{erlang, group_leader, 0}, true}, {{erlang, group_leader, 2}, true},
     {{erlang, halt, 0}, true}, {{erlang, halt, 1}, true}, {{erlang, hash, 2}, false}, {{erlang, hd, 1}, false},
     {{erlang, hibernate, 3}, true}, {{erlang, info, 1}, true}, {{erlang, integer_to_list, 1}, false},
     {{erlang, iolist_to_binary, 1}, false}, {{erlang, iolist_size, 1}, false}, {{erlang, is_atom, 1}, false},
     {{erlang, is_binary, 1}, false}, {{erlang, is_boolean, 1}, false}, {{erlang, is_builtin, 3}, false},
     {{erlang, is_float, 1}, false}, {{erlang, is_function, 1}, false}, {{erlang, is_function, 2}, false},
     {{erlang, is_integer, 1}, false}, {{erlang, is_list, 1}, false}, {{erlang, is_number, 1}, false},
     {{erlang, is_pid, 1}, true}, {{erlang, is_port, 1}, false}, {{erlang, is_process_alive, 1}, true},
     {{erlang, is_record, 2}, false}, {{erlang, is_record, 3}, false}, {{erlang, is_reference, 1}, false},
     {{erlang, is_tuple, 1}, false}, {{erlang, length, 1}, false}, {{erlang, link, 1}, true},
     {{erlang, list_to_atom, 1}, false}, {{erlang, list_to_binary, 1}, false},
     {{erlang, list_to_existing_atom, 1}, false}, {{erlang, list_to_float, 1}, false},
     {{erlang, list_to_integer, 1}, false}, {{erlang, list_to_integer, 2}, false}, {{erlang, list_to_pid, 1}, false},
     {{erlang, list_to_tuple, 1}, false}, {{erlang, load_module, 2}, true}, {{erlang, loaded, 0}, true},
     {{erlang, localtime, 0}, true}, {{erlang, localtime_to_universaltime, 1}, false},
     {{erlang, localtime_to_iniversaltime, 2}, false}, {{erlang, make_ref, 0}, true}, {{erlang, make_tuple, 2}, true},
     {{erlang, md5, 1}, false}, {{erlang, md5_final, 1}, false}, {{erlang, md5_init, 0}, false},
     {{erlang, md5_update, 2}, false}, {{erlang, memory, 0}, true}, {{erlang, memory, 1}, true},
     {{erlang, module_loaded, 1}, true}, {{erlang, monitor, 2}, true}, {{erlang, monitor_node, 2}, true},
     {{erlang, node, 0}, true}, {{erlang, node, 1}, true}, {{erlang, nodes, 0}, true}, {{erlang, nodes, 1}, true},
     {{erlang, now, 0}, true}, {{erlang, open_port, 2}, true}, {{erlang, phash, 2}, false}, {{erlang, phash2, 2}, false},
     {{erlang, pid_to_list, 1}, true}, {{erlang, port_close, 1}, true}, {{erlang, port_command, 2}, true},
     {{erlang, port_connect, 2}, true}, {{erlang, port_control, 3}, true}, {{erlang, port_call, 3}, true},
     {{erlang, port_info, 1}, true}, {{erlang, port_info, 2}, true}, {{erlang, port_to_list, 1}, true},
     {{erlang, ports, 0}, true}, {{erlang, pre_loaded, 0}, true}, {{erlang, process_diaplay, 2}, true},
     {{erlang, process_flag, 2}, true}, {{erlang, process_flag, 3}, true}, {{erlang, process_info, 1}, true},
     {{erlang, process_info, 2}, true}, {{erlang, processes, 0}, true}, {{erlang, purge_module, 1}, true},
     {{erlang, put, 2}, true}, {{erlang, raise, 3}, true}, {{erlang, read_timer, 1}, true},
     {{erlang, ref_to_list, 1}, false}, {{erlang, register, 2}, true}, {{erlang, registered, 0}, true},
     {{erlang, resume_process, 1}, true}, {{erlang, round, 1}, false}, {{erlang, self, 0}, true},
     {{erlang, send, 2}, true}, {{erlang, send, 3}, true}, {{erlang, send_after, 3}, true},
     {{erlang, send_nosuspend, 2}, true}, {{erlang, send_nosuspend, 3}, true}, {{erlang, set_cookie, 2}, true},
     {{erlang, setelement, 3}, false}, {{erlang, size, 1}, false}, {{erlang, spawn, 1}, true}, {{erlang, spawn, 2}, true},
     {{erlang, spawn, 3}, true}, {{erlang, spawn, 4}, true}, {{erlang, spawn_link, 1}, true},
     {{erlang, spawn_link, 2}, true}, {{erlang, spawn_link, 3}, true}, {{erlang, spawn_link, 4}, true},
     {{erlang, spawn_opt, 2}, true}, {{erlang, spawn_opt, 3}, true}, {{erlang, spawn_opt, 4}, true},
     {{erlang, spawn_opt, 5}, true}, {{erlang, aplit_binary, 2}, false}, {{erlang, start_timer, 3}, true},
     {{erlang, statistics, 1}, true}, {{erlang, suspend_process, 1}, false}, {{erlang, system_flag, 2}, true},
     {{erlang, system_info, 1}, true}, {{erlang, system_monitor, 0}, true}, {{erlang, system_monitor, 1}, true},
     {{erlang, system_monitor, 2}, true}, {{erlang, term_to_binary, 1}, false}, {{erlang, term_to_binary, 2}, false},
     {{erlang, throw, 1}, true}, {{erlang, time, 1}, true}, {{erlang, tl, 1}, false}, {{erlang, trace, 1}, true},
     {{erlang, trace_info, 2}, true}, {{erlang, trace_pattern, 2}, true}, {{erlang, trace_pattern, 3}, true},
     {{erlang, trunc, 1}, false}, {{erlang, unregister, 1}, false}, {{erlang, unregister, 1}, true},
     {{erlang, tuple_to_list, 1}, false}, {{erlang, universaltime, 1}, false},
     {{erlang, universaltime_to_localtime, 1}, false}, {{erlang, unlink, 1}, true}, {{erlang, whereis, 1}, true},
     {{erlang, yield, 1}, true}].

