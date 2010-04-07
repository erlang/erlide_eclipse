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


-module(wrangler_modularity_inspection).


-export([gen_module_graph/4, gen_module_scc_graph/3, gen_function_callgraph/3]).

-export([improper_inter_module_calls/3, mutual_recursive_modules/3, 
	 partition_a_module/1, modules_with_big_fanin/4, modules_with_big_fanout/4]).

-include("../include/wrangler.hrl").

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
   

improper_inter_module_calls(OutFile, SearchPaths, NotCareMods) ->
    ModCallerCallees = refac_module_graph:module_graph_with_funs(SearchPaths),
    Files = refac_util:expand_files(SearchPaths, ".erl"),
    FunCallees = lists:append(lists:flatmap(fun process_a_file/1, Files)),
    Res = [[{CallerMod, [{CalleeMod, CalledFuns}]} || {CalleeMod, CalledFuns} <- CalleeMods,
						      not lists:member(CalleeMod, NotCareMods),
						      [{CalleeMod, F, A} || {F, A} <- CalledFuns] -- FunCallees == []]
	   || {CallerMod, CalleeMods} <- ModCallerCallees,
	      not lists:member(CallerMod, NotCareMods)],
    refac_module_graph:module_graph_to_dot(OutFile, lists:append(Res), true).
    

process_a_file(F) ->
    #callgraph{callercallee = CallerCallee, scc_order = Sccs} =
	wrangler_callgraph_server:get_callgraph([F]),
    Sccs1 = [[MFA || {MFA, _Def} <- Sc] || Sc <- Sccs],
    [[{M1, F1, A1} || {M1, F1, A1} <- CalledFuns,
		      M0 == M1,
		      not lists:any(fun (S) ->
					    lists:member({M0, F0, A0}, S)
						andalso
						lists:member({M1, F1, A1}, S)
				    end, Sccs1)]
     || {{M0, F0, A0}, CalledFuns} <- CallerCallee].


    
mutual_recursive_modules(OutFile, SearchPaths, WithLabel) ->
    ?wrangler_io("\nCMD: ~p:gen_module_scc_graph(~p, ~p, ~p).\n",
		 [?MODULE, OutFile, SearchPaths, WithLabel]),
    refac_module_graph:scc_graph_to_dot(OutFile, SearchPaths, WithLabel).

partition_a_module(FileName) ->
    CallerCalleesWithDef = wrangler_callgraph_server:build_callercallee_callgraph([FileName]),
    CallerCallees = [{{M,F,A}, Callee}||{{{M,F,A}, _}, Callee}<-CallerCalleesWithDef],
    MG = digraph:new(),
    add_edges(CallerCallees,  MG).


modules_with_big_fanin(DotFile, SearchPaths, MinInDegree, NotCareMods) ->
    ModCallerCallees = refac_module_graph:module_graph_with_funs(SearchPaths),
    CalleeMods1 = lists:append([element(1, lists:unzip(CalleeMods)) || {_CallerMod, CalleeMods} <- ModCallerCallees]),
    refac_io:format("CalleeMods1:\n~p\n", [CalleeMods1]),
    Res = [[{CallerMod, [{CalleeMod, CalledFuns}]} || {CalleeMod, CalledFuns} <- CalleeMods,
						      not lists:member(CalleeMod, NotCareMods),
						      length([M || M <- CalleeMods1, M == CalleeMod]) >= MinInDegree]
	   || {CallerMod, CalleeMods} <- ModCallerCallees],
    refac_io:format("Res:\n~p\n", [Res]),
    refac_module_graph:module_graph_to_dot(DotFile, lists:append(Res), true).
    
    
modules_with_big_fanout(OutFile, SearchPaths, MinOutDegree, NotCareMods) ->
    ModCallerCallees = refac_module_graph:module_graph_with_funs(SearchPaths),
    Res = [{CallerMod, CalleeMods} || {CallerMod, CalleeMods} <- ModCallerCallees,
				      length(CalleeMods) > MinOutDegree, not lists:member(CallerMod, NotCareMods)],
    refac_io:format("Res:\n~p\n", [Res]),
    refac_module_graph:module_graph_to_dot(OutFile, Res, false).


add_edges([], MG) ->
    MG;
add_edges([{Caller, Callees}|Left], MG) ->
    Edges =[{Caller, Callee} ||  Callee <- Callees],
    add_edges(Left, digraph_add_edges(Edges, MG)).

digraph_add_edges([], MG)-> 
    MG;
digraph_add_edges([{From, To}|Left],  MG) ->
    digraph_add_edges(Left,digraph_add_edge(From, To, MG)).   
    
    
digraph_add_edge(From, To,  MG) ->
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
    digraph:add_edge(MG, From, To),
    MG.

