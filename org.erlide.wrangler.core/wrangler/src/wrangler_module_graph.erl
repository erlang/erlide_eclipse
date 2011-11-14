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
%% Module-level call graph construction.
%%
%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%%======================================================================

%% @private
%% @hidden
-module(wrangler_module_graph). 

-export([module_graph/1, get_called_mods/2, module_graph_to_dot/3, module_graph_to_dot/4,
	 module_subgraph_to_dot/4, module_graph_with_funs/1,module_sccs_to_dot/4,
	 module_callergraph_to_dot/4]). 

-export([add_edges/3]).

-include("../include/wrangler_internal.hrl").


%%-spec(module_graph/1::([dir()]) -> [{filename(), [filename()]}]).
module_graph(SearchPaths) ->
    ModCallerCallee = create_caller_callee_graph(SearchPaths),
    reverse_module_graph(ModCallerCallee).

create_caller_callee_graph(SearchPaths) ->
    Files = wrangler_misc:expand_files(SearchPaths, ".erl"),
    ModMap = wrangler_misc:get_modules_by_file(Files),
    analyze_all_files(ModMap, SearchPaths).

analyze_all_files([], _SearchPaths) ->
    ets:foldr(fun ({{Mod, Dir}, CalledMods, _CheckSum}, S) ->
		      FileName = filename:join(Dir, atom_to_list(Mod) ++ ".erl"),
		      case filelib:is_file(FileName) of
			  true ->
			      [{{Mod, Dir}, CalledMods}| S];
			  _ -> S
		      end
	      end, [], ?ModuleGraphTab);

analyze_all_files([{Mod, Dir}| Left], SearchPaths) ->
    FileName = filename:join(Dir, atom_to_list(Mod) ++ ".erl"),
    NewCheckSum = wrangler_misc:filehash(FileName),
    case ets:lookup(?ModuleGraphTab, {Mod, Dir}) of
	[] ->
	    Called = get_called_mods(FileName, SearchPaths),
	    ets:insert(?ModuleGraphTab, {{Mod, Dir}, Called, NewCheckSum}),
	    analyze_all_files(Left, SearchPaths);
	[{{Mod, Dir}, _CalledMods, OldCheckSum}] ->
	    case NewCheckSum =:= OldCheckSum of
		true ->
		    analyze_all_files(Left, SearchPaths);
		false ->
		    ets:delete(?ModuleGraphTab, {Mod, Dir}),
		    CalledMods1 = get_called_mods(FileName, SearchPaths),
		    ets:insert(?ModuleGraphTab, {{Mod, Dir}, CalledMods1, NewCheckSum}),
		    analyze_all_files(Left, SearchPaths)
	    end
    end.

get_called_mods(File, SearchPaths) ->
    Files = wrangler_misc:expand_files(SearchPaths, ".erl"),
    ModNames = [M || {M, _} <- wrangler_misc:get_modules_by_file(Files)],
    {ok, {AnnAST, Info}} = wrangler_ast_server:parse_annotate_file(File, true, SearchPaths),
    ImportedMods0 = case lists:keysearch(imports, 1, Info) of
			{value, {imports, Imps}} ->
			    [M || {M, _Funs} <- Imps];
			false -> []
		    end,
    ImportedMods1 = case lists:keysearch(module_imports, 1, Info) of
			{value, {module_imports, Mods}} ->
			    Mods;
			_ -> []
		    end,
    %% I took a conservative approach here.
    {CalledMods, PossibleCalledMods} = do_collect_called_mods(AnnAST, ModNames),
    AllCalledMods = ImportedMods0 ++ ImportedMods1 ++ CalledMods ++ PossibleCalledMods,
    lists:usort([M || M <- AllCalledMods, lists:member(M, ModNames)]).

%%-spec(do_collect_called_mods(AnnAST::syntaxTree(), ModNames::[atom()])
%%      ->{[modulename()], [modulename()]}).
do_collect_called_mods(AnnAST, ModNames) ->
    Fun1 = fun (T, Acc) ->
		   case wrangler_syntax:type(T) of
		       atom ->
			   As = wrangler_syntax:get_ann(T),
			   case lists:keysearch(type, 1, As) of
			       {value, {type, m_atom}} ->
				   ModName = wrangler_syntax:atom_value(T),
				   ordsets:add_element(ModName, Acc);
			       _ -> Acc
			   end;
		       _ -> Acc
		   end
	   end,
    CalledMods = api_ast_traverse:fold(Fun1, ordsets:new(), AnnAST),
    UnSures = wrangler_atom_utils:collect_unsure_atoms_in_file(AnnAST, ModNames, m_atom),
    UnSures1 = [Name || {atom, _Pos, Name} <- UnSures,
			 not  lists:member(Name, CalledMods)],
    {CalledMods, ordsets:from_list(UnSures1)}.


reverse_module_graph(List) ->
    reverse_module_graph_1(List,List, []).
reverse_module_graph_1([], _List,Acc) ->
    Acc;
reverse_module_graph_1([{{Mod, Dir},_Called_Mods}|T], List, Acc) ->
    Client_Modules = get_client_modules({Mod,Dir}, List),
    reverse_module_graph_1(T,List, [Client_Modules|Acc]).

get_client_modules({Mod, Dir}, List) ->
    F = fun ({{M, Dir1}, Called_Modules}) ->
		case lists:member(Mod, Called_Modules) of
		  true ->
		      case filelib:is_file(filename:join(Dir1, atom_to_list(Mod) ++ ".erl"))
			     andalso Dir =/= Dir1
			  of
			true ->
			    [];
			_ -> [filename:join([Dir1, atom_to_list(M) ++ ".erl"])]
		      end;
		  false -> []
		end
	end,
    {filename:join([Dir, atom_to_list(Mod) ++ ".erl"]), lists:flatmap(F, List)}.

%%=========================================================================================%%
%%
%% Functionalities for  module re-structure.
%%
%%==========================================================================================

module_graph_with_funs(SearchPaths) ->
    create_caller_callee_graph_with_called_funs(SearchPaths).

create_caller_callee_graph_with_called_funs(SearchPaths) ->
    Files = wrangler_misc:expand_files(SearchPaths, ".erl"),
    ModMap = wrangler_misc:get_modules_by_file(Files),
    analyze_all_files_with_called_funs(ModMap, SearchPaths).

analyze_all_files_with_called_funs(ModDirs, SearchPaths) ->
    Files = wrangler_misc:expand_files(SearchPaths, ".erl"),
    ModNames = [M || {M, _} <- wrangler_misc:get_modules_by_file(Files)],
    [{{Mod, Dir},analyze_mod_with_called_funs({Mod, Dir}, ModNames, SearchPaths)}
     || {Mod, Dir} <- ModDirs].

analyze_mod_with_called_funs({Mod, Dir}, ModNames, SearchPaths) ->
    File = filename:join(Dir, atom_to_list(Mod) ++ ".erl"),
    {ok, {AnnAST, Info}} = wrangler_ast_server:parse_annotate_file(File, true, SearchPaths),
    ImportedMods0 = case lists:keysearch(imports, 1, Info) of
			{value, {imports, Imps}} ->
			    [{M, []} || {M, _Funs} <- Imps, lists:member(M, ModNames)];
			false -> []
		    end,
    ImportedMods1 = case lists:keysearch(module_imports, 1, Info) of
			{value, {module_imports, Mods}} ->
			    [{M,[]} || M <- Mods, lists:member(M, ModNames)];
			_ -> []
		    end,
    CalledModFuns = collect_called_modules_with_called_funs(Mod, AnnAST, ModNames),
    Res = ImportedMods0 ++ ImportedMods1 ++ CalledModFuns,
    ordsets:from_list(group_by_mod_names(Res)).
   

%%-spec(collect_called_modules_with_called_funs(ModName::modulename(),AnnAST::syntaxTree(), ModNames::[atom()])
%%      ->[{modulename(), {functionname(), functionarity()}}]).

collect_called_modules_with_called_funs(ModName, AnnAST, ModNames) ->
    CalledFuns= lists:append([wrangler_callgraph_server:called_funs(F)
			      ||F <- wrangler_syntax:form_list_elements(AnnAST)]),
    [{M, [{F,A}]}||{M, F, A}<-CalledFuns, M/=ModName, lists:member(M, ModNames)].

group_by_mod_names(ModFuns) ->
    ModFuns1 = wrangler_misc:group_by(1, ModFuns),
    [{hd(Ms), lists:append(Fs)} || MFs <- ModFuns1, {Ms, Fs} <- [lists:unzip(MFs)]].
   
  
%%-spec (module_subgraph_to_dot/4::(filename(), [modulename()],[filename()|dir()], boolean()) ->true).
module_subgraph_to_dot(OutFile, ModNames, SearchPaths, WithLabel) ->
    DotFile = filename:dirname(OutFile)++filename:rootname(OutFile)++".dot",
    ModCallerCallees = create_caller_callee_graph_with_called_funs(SearchPaths),
    MG = digraph:new(),
    add_edges(ModCallerCallees, [], MG),
    SG=digraph_utils:subgraph(MG, ModNames, []),
    to_dot(SG,DotFile, WithLabel, [], false),
    digraph:delete(SG),
    digraph:delete(MG).


%%-spec (module_callergraph_to_dot/4::(filename(), [modulename()],[filename()|dir()], boolean()) ->true).
module_callergraph_to_dot(OutFile, ModNames, SearchPaths, WithLabel) ->
    DotFile = filename:rootname(OutFile)++".dot",
    ModCallerCallees = create_caller_callee_graph_with_called_funs(SearchPaths),
    ModCallerCallees1=[{Caller, [{CalleeMod, CalleeFuns}]}||
			  {Caller, Callees}<-ModCallerCallees, 
			  {CalleeMod, CalleeFuns}<-Callees,
			  lists:member(CalleeMod, ModNames)],
    MG = digraph:new(),
    add_edges(ModCallerCallees1, [], MG),
    to_dot(MG,DotFile, WithLabel, [], false),
    digraph:delete(MG).


%%-spec (module_graph_to_dot/4::(filename(), [modulename()], [filename()|dir()], boolean()) ->true). 			  
module_graph_to_dot(OutFile, NotCareMods, SearchPaths, WithLabel) -> 
    graph_to_dot(OutFile, NotCareMods, SearchPaths, WithLabel, false).


module_sccs_to_dot(OutFile, NotCareMods, SearchPaths, WithLabel) -> 
    graph_to_dot(OutFile, NotCareMods, SearchPaths,WithLabel, true).


graph_to_dot(OutFile, NotCareMods, SearchPaths, WithLabel, OnlySccs) ->
    DotFile = filename:rootname(OutFile) ++ ".dot",
    ModCallerCallees = create_caller_callee_graph_with_called_funs(SearchPaths),
    MG = digraph:new(),
    add_edges(ModCallerCallees, NotCareMods, MG),
    Sccs = digraph_utils:strong_components(MG),
    Sccs1 = [Scc || Scc <- Sccs, length(Scc) > 1],
    to_dot(MG, DotFile, WithLabel, Sccs1, OnlySccs),
    digraph:delete(MG).


module_graph_to_dot(DotFile, ModGraph, WithLabel) ->
    MG = digraph:new(),
    add_edges(ModGraph, [], MG),
    to_dot(MG, DotFile, WithLabel, [], false),
    digraph:delete(MG).

add_edges([], _NotCareMods, MG) ->
    MG;
add_edges([{{CallerMod,_}, CalleeMods}|Left], NotCareMods, MG) ->
    Edges =[{CallerMod, {CalleeMod, CallerFuns}} || {CalleeMod, CallerFuns} <- CalleeMods],
    add_edges(Left, NotCareMods, digraph_add_edges(Edges, NotCareMods, MG)).

digraph_add_edges([], _NotCareMods, MG)-> 
    MG;
digraph_add_edges([{From, {To, CallerFuns}}|Left], NotCareMods, MG) ->
    case lists:member(From, NotCareMods) orelse 
	lists:member(To, NotCareMods) of 
	true ->
	    digraph_add_edges(Left, NotCareMods, MG);
	false ->
	    digraph_add_edges(Left, NotCareMods, digraph_add_edge(From, To, CallerFuns, MG))
    end.
    
    
digraph_add_edge(From, To, Label, MG) ->
    case digraph:vertex(MG, From) of 
	false ->
	    digraph:add_vertex(MG, From);
	{From, _} ->
	    ok
    end,
    case digraph:vertex(MG, To) of 
	false ->
	    digraph:add_vertex(MG, To);
	{To,_} -> ok
    end,
    digraph:add_edge(MG, {From, To}, From, To, Label),
    MG.

to_dot(MG, File, WithLabel, SccNodes, OnlySccs) ->
    Edges = [digraph:edge(MG, X) || X <- digraph:edges(MG)],
    EdgeList=[{X, Y, Label} || {_, X, Y, Label} <- Edges],
    edge_list_to_dot(EdgeList, File, "ModuleGraph", WithLabel, SccNodes, OnlySccs).
    
edge_list_to_dot(Edges, OutFileName, GraphName, WithLabel, Sccs, OnlySccs) ->
    {NodeList1, NodeList2, _} = lists:unzip3(Edges),
    NodeList = NodeList1 ++ NodeList2,
    NodeSet = ordsets:from_list(NodeList),
    Start = ["digraph ",GraphName ," {"],
    VertexList = case OnlySccs of
		     true ->
			 [node_format(V, []) ||V <- NodeSet, is_scc_node(V, Sccs)];
		     false ->
			 [node_format(V, Sccs) ||V <- NodeSet]
		 end,
    End = ["graph [", GraphName, "=", GraphName, "]}"],
    EdgeList = case OnlySccs of 
		   true ->
		       [edge_format(X, Y, Label, WithLabel)
			|| {X,Y, Label} <- Edges,
			   is_scc_edge({X, Y}, Sccs)];
		   false ->
		       [edge_format(X, Y, Label, WithLabel) || {X,Y, Label} <- Edges]
	       end,
    String = [Start, VertexList, EdgeList, End],
    ok = file:write_file(OutFileName, list_to_binary(String)).


is_scc_node(V, Sccs) ->
    lists:member(V, lists:append(Sccs)).

is_scc_edge({V1, V2}, Sccs) ->
    lists:any(fun(Scc) ->
		      lists:member(V1, Scc) andalso
			  lists:member(V2, Scc)
	      end, Sccs).

node_format(V, Sccs) ->
    String = io_lib:format("~p", [V]),
    {Width, Heigth} = calc_dim(String),
    W = (Width div 7 + 1) * 0.55,
    H = Heigth * 0.4,
    SL = io_lib:format("~f", [W]),
    SH = io_lib:format("~f", [H]),
    case is_scc_node(V, Sccs) of 
	true ->
	    [String, " [width=", SL, " heigth=", SH, " color=purple, style=filled", " ", "", "];\n"];
	false ->
	    [String, " [width=", SL, " heigth=", SH, " ", "", "];\n"]
    end.

calc_dim(String) ->
  calc_dim(String, 1, 0, 0).

calc_dim("\\n" ++ T, H, TmpW, MaxW) ->
    calc_dim(T, H + 1, 0, wrangler_misc:max(TmpW, MaxW));
calc_dim([_| T], H, TmpW, MaxW) ->
    calc_dim(T, H, TmpW+1, MaxW);
calc_dim([], H, TmpW, MaxW) ->
    {wrangler_misc:max(TmpW, MaxW), H}.

edge_format([],_,_,_) ->
    "";
edge_format(_,[],_,_) ->
    "";
edge_format(V1, V2, Label, WithLabel) ->
    String = [io_lib:format("~p", [V1]), " -> ",
	      io_lib:format("~p", [V2])],
    case WithLabel of 
	true ->
	    [String, " [", "label=", "\"", format_label(Label),  "\"", "];\n"];
	_ ->
	    [String, " [", "];\n"]
    end.
  

format_label([]) ->
    "";
format_label([{F,A}|T]) ->
    case T of 
	[] ->
	    io_lib:format("~p/~p.", [F, A]);
	[{F1, A1}|T1] ->
	    case T1 of 
		[] ->
		    io_lib:format("~p/~p,~p/~p,", [F, A, F1, A1])++format_label(T1);
		_ ->
		    io_lib:format("~p/~p,~p/~p,", [F, A, F1, A1])++"\\n"++format_label(T1)
		end
    end.


