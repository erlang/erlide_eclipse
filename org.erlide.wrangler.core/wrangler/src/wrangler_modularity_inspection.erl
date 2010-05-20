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

-export([improper_inter_module_calls/3, cyclic_dependent_modules/3, 
	 modules_with_big_fanin/4, modules_with_big_fanout/4,
	 improper_located_apis/3]).

-compile(export_all).
-define(SimiScore, 0.4).

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
   

%% Detetect those module dependencies where the dependeny is only caused by those non-API functions.
%% Use ibrowse (part of couchdb) as an example.

%% Check cases studies: counch db; wrangler; refactorerl; and ibrowse.


%% Question: structure it is possible to find a target module, but how about semantically?.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                     %%
%%  Detecte and Remove Improper Inter-module calles.                   %%
%%                                                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

improper_inter_module_calls(OutFile, SearchPaths, NotCareMods) ->
    ModCallerCallees = refac_module_graph:module_graph_with_funs(SearchPaths),
    FullMG = digraph:new(), 
    refac_module_graph:add_edges(ModCallerCallees,[],FullMG),
    Files = refac_util:expand_files(SearchPaths, ".erl"),
    FunCallees = lists:flatmap(fun non_api_funs/1, Files),
    MG = lists:append([[{CallerMod, [{CalleeMod, CalledFuns}]}|| 
			   {CalleeMod, CalledFuns} <- CalleeMods,
			   not lists:member(CalleeMod, NotCareMods),
			   is_sub_list([{CalleeMod, F, A} || {F, A} <- CalledFuns],FunCallees)]
		       || {CallerMod, CalleeMods} <- ModCallerCallees,
			  not lists:member(CallerMod, NotCareMods)]),
    
    Edges = [{CallerMod, CalleeMod, CalleeFun} || 
		{{CallerMod,_}, CalleeMods}<-MG,
		{CalleeMod, CalleeFuns}<-CalleeMods, 
		CalleeFun<-CalleeFuns],
    GroupedEdges=refac_misc:group_by(3, Edges),
    Res =[find_best_target_module(EdgeGroup, FullMG)||EdgeGroup<-GroupedEdges],
    refac_module_graph:module_graph_to_dot(OutFile, MG, true),
    digraph:delete(FullMG),
    Res.

find_best_target_module(EdgeGroup, MG) ->
    {CalledMods, DefMod1, Fun1}=lists:unzip3(EdgeGroup),
    find_best_target_module_1(hd(Fun1), hd(DefMod1), [hd(DefMod1)|CalledMods], MG).

find_best_target_module_1({F, A}, DefMod, UsedMods, MG) ->
    RNB =lists:append([digraph:out_neighbours(MG, Mod)--[DefMod]||Mod<-UsedMods]),
    RNB1 =sort_list_by_freq(RNB),
    case RNB1 of 
	[] ->
	    {{DefMod, F, A}, target_module_not_found};
	_ ->
	   {{DefMod, F, A}, element(1, hd(RNB1))}
    end.

	
sort_list_by_freq(L) ->
    L1 = sort_list_by_freq(L, []),
    lists:reverse(lists:keysort(2, L1)).
		 
sort_list_by_freq([], Acc) ->
    Acc;
sort_list_by_freq([H|L], Acc) ->
    case lists:keysearch(H,1, Acc) of
	{value, {H, F}} ->
	    sort_list_by_freq(L, lists:keyreplace(H, 1, Acc, {H, F+1}));
	false ->
	    sort_list_by_freq(L, lists:keystore(H,1, Acc, {H,1}))
    end.
%% Not very interesting. (too many false positives).
improper_located_apis(OutFile, SearchPaths, NotCareMods) ->
    ModCallerCallees = refac_module_graph:module_graph_with_funs(SearchPaths),
    Files = refac_util:expand_files(SearchPaths, ".erl"),
    APIFuns = lists:flatmap(fun api_funs/1, Files),
    LabelFuns = lists:append([[{CalleeMod, F, A} || {F, A} <- CalledFuns]
			      || {_CallerMod, CalleeMods} <- ModCallerCallees,
				 {CalleeMod, CalledFuns} <- CalleeMods]),
    FunsUsedByOnlyOneMod = get_uniques(LabelFuns),
    APIFunsUsedByOnlyOneMod = list_insection(APIFuns, FunsUsedByOnlyOneMod),
    Res = [[{CallerMod, [{CalleeMod, [{F, A} || 
					 {F, A} <- CalledFuns, 
					 lists:member({CalleeMod, F, A}, APIFunsUsedByOnlyOneMod)]}]}
	    || {CalleeMod, CalledFuns} <- CalleeMods]
	   || {CallerMod, CalleeMods} <- ModCallerCallees],
    MG = lists:append(Res),
    MG1 = [{{CallerMod, CallerFile}, [{CalleeMod, CalledFuns} || {CalleeMod, CalledFuns} <- CalleeMods, CalledFuns /= [],
								 not lists:member(CalleeMod, NotCareMods)]}
	   || {{CallerMod, CallerFile}, CalleeMods} <- MG, not lists:member(CallerMod, NotCareMods)],
    refac_module_graph:module_graph_to_dot(OutFile, MG1, true).


get_uniques(L) ->    
    get_uniques(L, [],[]).

get_uniques([], Acc, _) ->
     Acc;
get_uniques([H|L], Acc, Acc1) ->
    case lists:member(H, Acc) of 
	true ->
	    get_uniques(L, Acc--[H], [H|Acc1]);
	false ->
	    case lists:member(H, Acc1) of 
		true ->
		    get_uniques(L, Acc, Acc1);
		false ->
		    get_uniques(L, [H|Acc], Acc1)
	    end
    end.

%%TODO: compare the result of using NonAPIs and ClosedNonAPIs.
non_api_funs(File) -> 
    element(3, api_and_non_api_funs(File)).


api_funs(File) ->
    element(1, api_and_non_api_funs(File)).
    
    
api_and_non_api_funs(File) ->
    ExpFuns = exported_funs(File),
    #callgraph{callercallee = CallerCallees} =
	wrangler_callgraph_server:get_callgraph([File]),
    CG = digraph:new(),
    add_edges(CallerCallees, CG),
    Vs = digraph:vertices(CG),
    SCCs = [SCC || SCC <- digraph_utils:strong_components(CG), length(SCC) > 1],
    Reaching = [{V, digraph_utils:reaching([V], CG)}
		|| V <- Vs, lists:member(V, ExpFuns)],
    APIs = [V || {V, Rs} <- Reaching, lists:member(V, ExpFuns), Rs == [V] orelse
								  lists:any(fun (SCC) -> Rs == SCC
									    end, SCCs)],
    SemiAPIs = [V || V <- ExpFuns -- APIs, similar_to_some_api(V, APIs, CG)],
    NonAPIs = ExpFuns -- APIs ++ SemiAPIs,
    ClosedNonAPIs = [V || V <- NonAPIs, is_closed(V, CG)],
    digraph:delete(CG),
    {APIs, SemiAPIs, NonAPIs, ClosedNonAPIs}.


is_closed(V= {M, _F, _A}, CG) ->
    Rs = [{M1, F1, A1} || {M1, F1, A1} <- digraph_utils:reachable([V], CG),
			  M == M1],
    InEdges=lists:sort([{V1, V2}|| R<-Rs, R/=V, E<-digraph:in_edges(CG,R), 
				   {_, V1, V2, _}<-[digraph:edge(CG, E)]]),
    OutEdges=lists:sort([{V1, {M2, F2, A2}}|| R<-Rs, E<-digraph:out_edges(CG,R), 
					      {_, V1, {M2, F2,A2}, _}<-[digraph:edge(CG, E)], M2==M]),
    InEdges == OutEdges.


%% is_closed(V = {M, _F, _A}, File) ->
%%     #callgraph{callercallee = CallerCallees} =
%%  	wrangler_callgraph_server:get_callgraph([File]),
%%     CG = digraph:new(),
%%     add_edges(CallerCallees, CG),
%%     Rs = [V|[{M1, F1, A1} || {M1, F1, A1} <- digraph_utils:reachable([V], CG),
%% 			  M == M1]],
%%     Rs1 = lists:usort(lists:append([digraph_utils:reaching([R],CG) || R <- Rs]) -- [V]),
%%     refac_io:format("V:\n~p\n", [V]),
%%     refac_io:format("Rs:\n~p\n", [Rs]),
%%     refac_io:format("Rs1:\n~p\n", [Rs1]),
%%     Res=lists:usort(Rs1)--lists:usort(Rs) ==[],    
%%     refac_io:format("Closed:\n~p\n", [Res]).
   

%% moveability(V={M,_F, _A}, File) ->
%%     #callgraph{callercallee = CallerCallees} =
%% 	wrangler_callgraph_server:get_callgraph([File]),
%%     CG = digraph:new(),
%%     add_edges(CallerCallees, CG),
%%     Rs = [{M1, F1, A1} || {M1, F1, A1} <- digraph_utils:reachable([V], CG),
%% 			  M == M1],
%%     Rs1 = lists:append([digraph:in_neighbours(CG, R) || R <- Rs]) -- [V],
%%     Vs=[{M1, F, A}||{M1, F, A}<-digraph:vertices(CG), M==M1],
%%     refac_io:format("Rs:\n~p\n", [Rs]),
%%     refac_io:format("Vs:\n~p\n", [Vs]),
%%     case lists:usort(Rs1)--lists:usort(Rs)==[] of 
%% 	true ->
%% 	    1-length(Rs)/length(Vs);
%% 	false ->
%% 	    0
%%     end.
	  

unmoveability(V={M,_F, _A}, CG) ->
    Rs = [{M1, F1, A1} || {M1, F1, A1} <- digraph_utils:reachable([V], CG),
			  M == M1],
    Vs=[{M1, F, A}||{M1, F, A}<-digraph:vertices(CG), M==M1],
    case is_closed(V, CG) of 
	true ->
	    length(Rs)/length(Vs);
	false ->
	    1
    end.
	  


 
%% What other metrics to use: use of records.
similar_to_some_api(V, APIs, MG) ->
    VReachable=digraph_utils:reachable([V], MG),
    Reaching =digraph_utils:reaching([V], MG),
    ReachingAPIs = [R||R<-Reaching, lists:member(R, APIs)],
    lists:any(fun(R)->
		      SimiScore=length(VReachable)/length(digraph_utils:reachable([R], MG)),
		      SimiScore>=?SimiScore
	      end, ReachingAPIs).



exported_funs(File) ->
    {ok, {_, Info}} = refac_util:parse_annotate_file(File, true),
    {value, {module, ModName}} =lists:keysearch(module,1,Info),
    ImpExports = 
	case lists:keysearch(attributes, 1, Info) of
	    {value, {attributes, Attrs}} ->
		case lists:member({compile, export_all}, Attrs) of
		    true -> 
			case lists:keysearch(functions, 1, Info) of
			    {value, {functions, Funs}} ->
				[{ModName, F, A}||{F, A}<-Funs];
			    false ->
				[]
			end;
		    false ->
			[]
		end;
	    false -> []
	end,
    case ImpExports of
	[] ->
	    case lists:keysearch(exports, 1, Info) of
	      {value, {exports, ExpFuns}} ->
		    [{ModName, F, A}||{F, A}<-ExpFuns];
		false ->
		    []
	    end;
	_ -> ImpExports
    end.

   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                     %%
%%  Detecte and  removed cyclic module dependences.                    %%
%%                                                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Instead of trying to break a SCC, I should try to break cycles.
cyclic_dependent_modules(OutFile, SearchPaths, WithLabel) ->
    ?wrangler_io("\nCMD: ~p:cyclic_dependent_modules(~p, ~p, ~p).\n",
		 [?MODULE, OutFile, SearchPaths, WithLabel]),
    ModCallerCallees = refac_module_graph:module_graph_with_funs(SearchPaths),
    MG = digraph:new(),
    refac_module_graph:add_edges(ModCallerCallees,[],MG),
    SCCs = digraph_utils:cyclic_strong_components(MG),
    Cycles = [digraph:get_short_cycle(MG, V) || SCC<-SCCs, V <-SCC],
    Cycles1 = remove_duplicated_circles(Cycles),
    CycleWeakestLinkPairs=[{C, break_a_circle(C, ModCallerCallees)}|| C<-Cycles1],
    cycles_to_dot(MG, OutFile,CycleWeakestLinkPairs).


remove_duplicated_circles(Cs) ->
    remove_duplicated_circles(Cs, []).

remove_duplicated_circles([], Acc) ->
    Acc;
remove_duplicated_circles([C|Cs], Acc) ->
    case lists:any(fun(C1) ->
			   lists:usort(C)==lists:usort(C1) 
		   end, Acc) of 
	true ->
	    remove_duplicated_circles(Cs, Acc);
	false ->
	    remove_duplicated_circles(Cs, [C|Acc])
    end.
	

cycles_to_dot(MG, OutFile, CycleWeakestLinkPairs)->
    CycleEdges=[get_cycle_edges(MG, C, WeakestLink)||{C, WeakestLink}<-CycleWeakestLinkPairs],
    NewEdgeList=get_renamed_edge_list(CycleEdges),
    edge_list_to_dot(lists:append(NewEdgeList), OutFile, "ModuleGraph", true).
    
get_cycle_edges(MG, C, WeakestLink) ->
    get_cycle_edges(MG, C, WeakestLink, []).

get_cycle_edges(MG, T, _WeakestLink={WE, Score}, Edges)  when length(T)=<1->
    Edges1 =lists:reverse(Edges),
    [{X, Y, Label, {X,Y}==WE, Score}|| E<-digraph:edges(MG),
				       {_, X, Y, Label} <- [digraph:edge(MG, E)],
				       lists:member({X, Y}, Edges1)];
get_cycle_edges(MG,[V1, V2|T], WeakestLink,  Edges) ->
    get_cycle_edges(MG, [V2|T], WeakestLink, [{V1,V2}|Edges]).

			
get_renamed_edge_list(Cycles) ->
    get_renamed_edge_list_1(Cycles, [], []).
get_renamed_edge_list_1([], _Vs, Acc) ->
    lists:reverse(Acc);
get_renamed_edge_list_1([C|Cs],Vs,Acc) ->
    {NewVs, NewEdges}=get_renamed_edge_list_2(C, Vs, []),
    get_renamed_edge_list_1(Cs, NewVs, [NewEdges|Acc]).

get_renamed_edge_list_2(C, Vs, Edges) ->
    get_renamed_edge_list_3(C, Vs, [], Edges).

get_renamed_edge_list_3([],Vs, NewVs,  NewEdges) ->
    {Vs++lists:usort(NewVs), lists:reverse(NewEdges)};
get_renamed_edge_list_3([{X, Y, Label, Weak, Score}|T], Vs, NewVs, NewEdges) ->
    N=length([X||V<-Vs, X==V]),
    X1 = atom_to_list(X)++"("++integer_to_list(N)++")",
    N1 =length([Y||V<-Vs, Y==V]),
    Y1 = atom_to_list(Y)++"("++integer_to_list(N1)++")",
    get_renamed_edge_list_3(T, Vs, [X,Y,X1, Y1|NewVs], [{X1, Y1, Label, Weak, Score}|NewEdges]).    
	    

break_a_circle(Cycle, ModCallerCallees) ->
    CycleCallerCallees = get_circle_caller_callees(Cycle, ModCallerCallees),
    find_weakest_link(CycleCallerCallees).
   

get_circle_caller_callees(Cycle, ModCallerCallees) ->
    get_circle_caller_callees(Cycle, ModCallerCallees,[]).

get_circle_caller_callees(Vs, _ ,Acc) when length(Vs) =<1 ->
    lists:reverse(Acc);
get_circle_caller_callees([V1, V2|T], ModCallerCallees, Acc) ->
    CalleeFileName=hd([filename:join([Dir, atom_to_list(V2)++".erl"])||{{CallerMod, Dir}, _}<-ModCallerCallees, CallerMod==V2]),
    CalledFAs = [CalledFuns||{{CallerMod, _}, CalleeMods}<- ModCallerCallees, 
			      CallerMod==V1, 
			      {CalleeMod, CalledFuns}<-CalleeMods,
			      CalleeMod == V2],
    get_circle_caller_callees([V2|T], ModCallerCallees, [{V1,V2, {CalleeFileName, hd(CalledFAs)}}|Acc]).
    

find_weakest_link(CycleCallerCallees) ->    
    Scores =[score_a_link(Link) || Link<-CycleCallerCallees],
    refac_io:format("LinkScores:\n~p\n", [Scores]),
    SortedScores=lists:keysort(2, Scores),
    hd(SortedScores).

score_a_link({M1, M2, {File, FAs}}) ->    
    APIScorePairs= api_score_pairs(File),
    refac_io:format("FA:\n~p\n", [FAs]),
    refac_io:format("APIScorePairs:\n~p\n", [APIScorePairs]),
    Scores=[{{F, A}, APIScore, MoveScore}|| {{_M, F, A}, APIScore, MoveScore}<-APIScorePairs, lists:member({F, A}, FAs)],
    refac_io:format("Scores:\n~p\n", [Scores]),
    {_LinkFuns, APIScores, MoveScores} = lists:unzip3(Scores),
    {{M1, M2}, {lists:sum(APIScores), lists:sum(MoveScores)/length(MoveScores)}}.
		      
  
api_score_pairs(File) -> 
    ExpFuns =exported_funs(File),
    #callgraph{callercallee = CallerCallees} =
	wrangler_callgraph_server:get_callgraph([File]),
    CG = digraph:new(),
    add_edges(CallerCallees, CG),
    Vs=digraph:vertices(CG),
    SCCs = [SCC||SCC<-digraph_utils:strong_components(CG), length(SCC)>1],
    Reaching =[{V, digraph_utils:reaching([V], CG)} ||
		  V <-Vs, lists:member(V, ExpFuns)],
    APIs = [V|| {V, Rs}<-Reaching, lists:member(V, ExpFuns), Rs==[V] orelse 
		    lists:any(fun(SCC)->Rs==SCC end, SCCs)],
    [{V, get_api_score(V,APIs,CG), unmoveability(V, CG)}||V<-ExpFuns].
  
get_api_score(V, APIs, MG) ->
    VReachable=digraph_utils:reachable([V], MG),
    Reaching =digraph_utils:reaching([V], MG),
    ReachingAPIs = [R||R<-Reaching, lists:member(R, APIs)],
    Scores = [case lists:member(V, APIs) of 
		  true -> 1;
		  false->
		      length(VReachable)/length(digraph_utils:reachable([R], MG))
	      end
	      ||R<-ReachingAPIs],
    case Scores of 
	[] -> 1;
	_ -> lists:last(lists:sort(Scores))
    end.

remove_a_link(File, Funs) ->
    case filelib:is_regular(File) of
      true -> ok;
      false -> throw:error("The file name given does not refer to a file.")
    end,
    #callgraph{callercallee = CallerCallees} =
	wrangler_callgraph_server:get_callgraph([File]),
    CG = digraph:new(),
    add_edges(CallerCallees, CG),
    Vs = digraph_utils:reaching(Funs, CG),
    case Vs of
      Funs ->
	  throw({error, "None of the functions given are used by the file specified."});
      _ ->
	  ModName = element(1, hd(Vs)),
	  Vs2 = digraph_utils:reachable(Vs, CG),
	  Vs3 = [{M2, F2, A2} || {M2, F2, A2} <- Vs2, M2 == ModName],
	  APIs = exported_funs(File),
	  Vs4 = [V || V <- Vs3, is_sub_list(digraph_utils:reaching([V], CG), Vs2)],
	  Vs5 = [V || V <- Vs4, lists:member(V, APIs), not lists:member(V, Vs)],
	  FunsToMove = Vs4 -- Vs5,
	  refac_io:format("FunsToMove:\n~p\n", [FunsToMove]),
	  FunsToMove
    end.
				       

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                     %%
%%  Detect and eliminate modules with very big fanin.                  %%
%%                                                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

modules_with_big_fanin(DotFile, SearchPaths, MinInDegree, NotCareMods) ->
    ModCallerCallees = refac_module_graph:module_graph_with_funs(SearchPaths),
    CalleeMods1 = lists:append([element(1, lists:unzip(CalleeMods)) || {_CallerMod, CalleeMods} <- ModCallerCallees]),
    Res = [[{CallerMod, [{CalleeMod, CalledFuns}]} || {CalleeMod, CalledFuns} <- CalleeMods,
						      not lists:member(CalleeMod, NotCareMods),
						      length([M || M <- CalleeMods1, M == CalleeMod]) >= MinInDegree]
	   || {CallerMod, CalleeMods} <- ModCallerCallees],
    refac_io:format("Res:\n~p\n", [Res]),
    refac_module_graph:module_graph_to_dot(DotFile, lists:append(Res), true).

    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                     %%
%%  Detect and eliminate modules with very big fanout.                 %%
%%                                                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   
modules_with_big_fanout(OutFile, SearchPaths, MinOutDegree, NotCareMods) ->
    ModCallerCallees = refac_module_graph:module_graph_with_funs(SearchPaths),
    Res = [{CallerMod, CalleeMods} || {CallerMod, CalleeMods} <- ModCallerCallees,
				      length(CalleeMods) > MinOutDegree, not lists:member(CallerMod, NotCareMods)],
    refac_io:format("Res:\n~p\n", [Res]),
    refac_module_graph:module_graph_to_dot(OutFile, Res, false).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                     %%
%%  Some utility functions.                                            %%
%%                                                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




list_insection(L1, L2) ->
    ordsets:to_list(ordsets:intersection(ordsets:from_list(L1), ordsets:from_list(L2))).

list_subtract(L1, L2) ->
    ordsets:to_list(ordsets:subtract(ordsets:from_list(L1), ordsets:from_list(L2))).

is_sub_list(L1, L2) ->
    ordsets:is_subset(ordsets:from_list(L1), ordsets:from_list(L2)).

remove_sub_lists(ListOfLists) ->
    ListOfLists1 = lists:sort(fun(L1, L2)-> length(L1)>= length(L2) end, ListOfLists),
    remove_sub_lists(ListOfLists1,[]).

remove_sub_lists([], Acc) ->
    lists:reverse(Acc);
remove_sub_lists([L|T], Acc) ->
    case lists:any(fun(A)->
			   is_sub_list(L, A) 
		   end, Acc) of 
	true ->
	    remove_sub_lists(T, Acc);
	false ->
	    remove_sub_lists(T, [L|Acc])
    end.
		  


is_prop_sub_list(L1, L2) ->
    S1 = ordsets:from_list(L1),
    S2 = ordsets:from_list(L2),
    (ordsets:size(S1) < ordsets:size(S2))
	andalso ordsets:is_subset(S1, S2).

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

   
edge_list_to_dot(Edges, OutFileName, GraphName, WithLabel) ->
    NodeList=ordsets:to_list(
	       ordsets:from_list(
		 lists:append(
		   [[V1, V2]||{V1, V2, _Label, _Weak, _Score} <- Edges]))),
    refac_io:format("\nEdges:\n~p\n", [Edges]),
    NodesToColor = ordsets:to_list(
		      ordsets:from_list(
			[V1||{V1, _V2, _Label, Weak, {_, Score}} <- Edges, Weak, Score>0.2])),
    refac_io:format("NodesToColor:\n~p\n", [NodesToColor]),
    Start = ["digraph ",GraphName ," {"],
    VertexList = [node_format(V, NodesToColor) ||V <- NodeList],
    End = ["graph [", GraphName, "=", GraphName, "]}"],
    EdgeList = [edge_format(X, Y, Label,WithLabel, Weak) || {X,Y, Label, Weak, _Score} <- Edges],
    String = [Start, VertexList, EdgeList, End],
    ok = file:write_file(OutFileName, list_to_binary(String)).


is_scc_node(V, Sccs) ->
    lists:member(V, Sccs).

is_scc_edge({V1, V2}, Sccs) ->
    lists:member(V1, Sccs) andalso
	lists:member(V2, Sccs).
	

node_format(V, NodesToColor) ->
    String = io_lib:format("~p", [V]),
    {Width, Heigth} = calc_dim(String),
    W = (Width div 7 + 1) * 0.55,
    H = Heigth * 0.4,
    SL = io_lib:format("~f", [W]),
    SH = io_lib:format("~f", [H]),
    case lists:member(V, NodesToColor) of 
	true ->
	    [String, " [width=", SL, " heigth=", SH, " color=red, style=filled", " ", "", "];\n"];
	false ->
	    [String, " [width=", SL, " heigth=", SH, " ", "", "];\n"]
    end.

calc_dim(String) ->
  calc_dim(String, 1, 0, 0).

calc_dim("\\n" ++ T, H, TmpW, MaxW) ->
  calc_dim(T, H+1, 0, erlang:max(TmpW, MaxW));
calc_dim([_|T], H, TmpW, MaxW) ->
  calc_dim(T, H, TmpW+1, MaxW);
calc_dim([], H, TmpW, MaxW) ->
  {erlang:max(TmpW, MaxW), H}.

edge_format([],_,_,_,_) ->
    "";
edge_format(_,[],_,_, _) ->
    "";
edge_format(V1, V2, Label, WithLabel,WithColor) ->
    String = [io_lib:format("~p", [V1]), " -> ",
	      io_lib:format("~p", [V2])],
    case  WithColor of 
	true ->
	    case WithLabel of 
		true ->
		    [String, " [", "label=", "\"", format_label(Label),  "\"", " color=red", "];\n"];
		false ->
		    [String, " ["," color=red", "];\n"]
	    end;
	false ->
	    case WithLabel of 
		true ->
		    [String, " [", "label=", "\"", format_label(Label),  "\"", "];\n"];
		false ->
		    [String, " [", "];\n"]
	    end
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                        %%
%%                    Clustering API functions                            %%
%%                                                                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


cluster(File, Sum, SearchPaths) ->
    Files = refac_util:expand_files(SearchPaths, ".erl"),
    refac_io:format("Files:\n~p\n", [Files]),
    Files1=case Files of 
	       [] -> [File];
	       _ -> Files
	   end,	
    #callgraph{callercallee = CallerCallees} =
	wrangler_callgraph_server:get_callgraph(Files1),
    CG = digraph:new(),
    add_edges(CallerCallees, CG),
    Coms =gen_components(File),
    refac_io:format("Coms:\n~p\n", [Coms]),
    Matrix = generate_fun_dist_matrix(File, CG, Coms),
    Funs = exported_funs(File),
    refac_io:format("Funs:\n~p\n", [Funs]),
    %% refac_io:format("Matrix:\n~p\n", [Matrix]),
    Cs =agglomerative_cluster(Matrix, CG, Coms),
     refac_io:format("Cs:\n~p\n", [Cs]),
    Cs1= lists:usort(lists:append(Cs)),
    FunSizePairs = get_fun_size_pairs(File, SearchPaths),
    Cs2 =[analyze_a_group(C, CG, FunSizePairs)||C<-Cs1],
    Cs3 = [{C, {IsClosed, SizeRatio}}||{C, IsClosed, SizeRatio}<-Cs2, 
				     IsClosed/=1],
    refac_io:format("Cs2:\n~p\n", [Cs2]),  
    refac_io:format("Cs3:\n~p\n", [Cs3]),  
    digraph:delete(CG),
    Cs4=remove_sub_clusters(Cs3),
    refac_io:format("Cs4:\n~p\n", [Cs4]),
    Cs5=lists:sublist(Cs4, Sum-1),
    RemFuns=Funs--lists:append([element(1,C)||C<-Cs5]),
    Res =[RemFuns|Cs5],
    refac_io:format("Res:\n~p\n", [Res]),
    Res.
 

gen_components(FileName) ->
    CallerCalleesWithDef = wrangler_callgraph_server:build_callercallee_callgraph([FileName]),
    CallerCallees = [{{M,F,A}, [{M1,F1,A1}||{M1,F1,A1}<-Callee, M1==M]}
		     || {{{M,F,A}, _}, Callee}<-CallerCalleesWithDef],
    CallerCallees1=[{Caller, Callees}||{Caller, Callees}<-CallerCallees, 
				       Callees/=[]],
    CG = digraph:new(),
    add_edges(CallerCallees1,  CG),
    Coms=digraph_utils:components(CG),
    digraph:delete(CG),
    Coms.

generate_fun_dist_matrix(File, CG, Coms) ->
    Funs = exported_funs(File),
    IntialMatrix = [{[F], [{[F1], undefined}||F1<-Funs]}||F<- Funs],
    update_dist(IntialMatrix, CG, Coms).

agglomerative_cluster(Matrix, CG, Coms) ->
    agglomerative_cluster(Matrix, CG, Coms,[]).

agglomerative_cluster(_Matric=[_], _CG,  _Coms,Acc) ->
    lists:reverse(Acc);
agglomerative_cluster(Matrix, CG, Coms, Acc) ->
    %% refac_io:format("Matrix:\n~p\n", [Matrix]),
    {FG, Dist} = find_min_dist(Matrix),
    refac_io:format("FG:\n~p\n", [FG]),
    refac_io:format("Dist:\n~p\n", [Dist]),
    Matrix1=group_funs(Matrix, FG),
    Matrix2 = update_dist(Matrix1, CG, Coms),
    Cs = [F||{F, _}<- Matrix2],
   %% refac_io:format("Cs:\n~p\n", [Cs]),
    agglomerative_cluster(Matrix2, CG, Coms, [Cs|Acc]).
    
find_min_dist(Matrix) ->
    {FG, Dist}=
	lists:foldl(fun({RowKey, RowElems}, {Acc, Min})->
			    lists:foldl(fun({E, Dist}, {Acc0, Min0}) ->
						case Dist>=Min0 of
						    true -> {Acc0,Min0};
						    false when Dist/={0,0,0}->
							{RowKey++E, Dist};
						    _ -> {Acc0, Min0}
						end
					end, {Acc, Min}, RowElems)
		    end, {[], {1,1,1}}, Matrix),
    {lists:usort(FG), Dist}.


group_funs(Matrix, FunsToGroup) ->
    Matrix1 =[{RowKey1, group_cols(RowElems1, FunsToGroup)}
	      || {RowKey, RowElems} <- Matrix, 
		 {RowKey1, RowElems1}<-
		     [case is_sub_list(RowKey, FunsToGroup) of 
			  true -> {FunsToGroup, [{Es, undefined}||
						    {Es, _Dist}<-RowElems]};
			  false -> {RowKey, RowElems}
		      end]],
    remove_duplicated_keys(Matrix1).

						
group_cols(RowElems, FunsToGroup) ->
    NewRowElems =[{ColKey1, Dist1}||
		     {ColKey, Dist}<-RowElems,
		     {ColKey1, Dist1} <-[case is_sub_list(ColKey, FunsToGroup) of
					     true -> {FunsToGroup, undefined};
					     false -> {ColKey, Dist}
					 end]],
    remove_duplicated_keys(NewRowElems).

remove_duplicated_keys(Tuples) ->
    remove_duplicated_keys(Tuples, [], []).

remove_duplicated_keys([],_, Tuples1) -> 
    lists:reverse(Tuples1);
remove_duplicated_keys([{Key, V}|T], Keys, Tuples1) ->
    case lists:member(Key, Keys) of
	true ->
	    remove_duplicated_keys(T, Keys, Tuples1);
	false ->
	    remove_duplicated_keys(T, [Key|Keys],[{Key, V}|Tuples1])
    end.

update_dist(Matrix, CG, Coms) ->
    [{RowKey, RowElems1} ||{RowKey, RowElems}<-Matrix,
			   RowElems1<-[[{ColKey, Dist1}
					||{ColKey, Dist}<-RowElems,
					  Dist1<-[update_dist(RowKey, ColKey, Dist, CG, Coms)]]]].

update_dist(RowKey, ColKey, Dist, CG, Coms) ->
    case Dist of 
	undefined ->
	    calculate_dist(RowKey, ColKey, CG, Coms);
	_ -> Dist
    end.

calculate_dist(FunGroup1=[{M, _F, _A}|_], FunGroup2, CG, Coms) ->
    case lists:usort(FunGroup1) == lists:usort(FunGroup2) of
      true -> {0, 0,0};
      false ->
	    FunGroup1Reachable = digraph_utils:reachable(FunGroup1, CG),
	    FunGroup1ReachingMods =lists:usort([M1||{M1, _F1, _A1}<-digraph_utils:reaching(FunGroup1, CG), M1/=M]),
	    FunGroup2Reachable = digraph_utils:reachable(FunGroup2, CG),
	    FunGroup2ReachingMods =lists:usort([M1||{M1, _F1, _A1}<-digraph_utils:reaching(FunGroup2, CG), M1/=M]),
	    CommonReachable = list_insection(FunGroup1Reachable, FunGroup2Reachable),
	    IsSubList=case is_sub_list(FunGroup2Reachable, FunGroup1Reachable) orelse 
			  is_sub_list(FunGroup1Reachable, FunGroup2Reachable) of 
			  true -> 1;
			  false ->0
		      end,
	    CommonReachableScore=1-(0.5*2 * length(CommonReachable) / (length(FunGroup1Reachable) + length(FunGroup2Reachable))+
				    0.5 * IsSubList),
	    CommonReaching=list_insection(FunGroup1ReachingMods, FunGroup2ReachingMods),
	    CommonReachingScore=case FunGroup1ReachingMods==[] andalso FunGroup2ReachingMods==[] of 
				    true -> 0;
				    false ->
					1-2 * length(CommonReaching) / 
					    (length(FunGroup1ReachingMods) + length(FunGroup2ReachingMods))
				end,
	    InSameCom = is_in_same_component(FunGroup1, FunGroup2, Coms),
	    {InSameCom, CommonReachableScore,CommonReachingScore}
    end.

is_in_same_component(FunGroup1, FunGroup2, Coms) ->
    R =lists:any(fun(C) ->
			 is_sub_list(FunGroup1, C) andalso 
			     is_sub_list(FunGroup2, C)
		 end, Coms),
    case R of 
	true ->
	    0;
	false ->
	    1
    end.

analyze_a_group(C, CG, FunSizePairs) ->
    Res ={C, is_component(C, CG), cluster_size_ratio(C, CG, FunSizePairs)},
    refac_io:format("GroupData:\n~p\n", [Res]),
    Res.

is_component([], _CG) ->
    false;
is_component(Vs=[{M, _F, _A}|_T], CG) ->
    Rs = [{M1, F1, A1} || {M1, F1, A1} <- digraph_utils:reachable(Vs, CG),
			  M == M1],
    InEdges=lists:usort([{V1, V2}|| R<-Rs,  E<-digraph:in_edges(CG,R), 
				   {_, V1, V2, _}<-[digraph:edge(CG, E)]]),
    OutEdges=lists:usort([{V1, {M2, F2, A2}}|| R<-Rs, E<-digraph:out_edges(CG,R), 
					      {_, V1, {M2, F2,A2}, _}<-[digraph:edge(CG, E)], M2==M]),
    refac_io:format("Vs:\n~p\n", [Vs]),
    %%TODO: REFINE THIS !!
    case list_subtract(InEdges, OutEdges)==[] andalso list_subtract(OutEdges, InEdges)==[] of 
	true -> 0;
	_ -> case not(list_subtract(InEdges, OutEdges)/=[] andalso list_subtract(OutEdges, InEdges)/=[]) of 
		 true ->
		      0.5;
		 false ->
		     1
	     end
    end.

cluster_size_ratio(C=[{M, _F, _A}|_T], CG, FunSizePairs) ->
    Rs = [{M1, F1, A1} || {M1, F1, A1} <- digraph_utils:reachable(C, CG),
			  M == M1],
    AllVs=lists:usort(C++Rs),
    CSize=lists:sum([Size||{F, Size}<-FunSizePairs, lists:member(F, AllVs)]),
    TotalSize =lists:sum([Size||{_, Size}<-FunSizePairs]),
    refac_io:format("CSize:\n~p\n", [CSize]),
    refac_io:format("TotlaSize:\n~p\n", [TotalSize]),    
    abs(CSize/TotalSize-0.5).
				 
get_fun_size_pairs(File, SearchPaths)->    
    {ok, {AnnAST, _Info}} = refac_util:parse_annotate_file(File, true, SearchPaths),
    Forms = refac_syntax:form_list_elements(AnnAST),
    Fun = fun(Form) ->
		  case lists:keysearch(fun_def,1,refac_syntax:get_ann(Form)) of 
		      {value, {fun_def, {M, F, A, _,_}}} ->
			  Toks = refac_misc:get_toks(Form),
			  CodeLines = [element(1, element(2, T)) ||
					  T <- Toks, element(1, T) /= whitespace, 
					  element(1, T) /= comment],
			  [{{M, F, A}, length(refac_misc:remove_duplicates(CodeLines))}];
		      false ->
			  []
		  end
	  end,
    lists:append([Fun(F)||F<-Forms]).


remove_sub_clusters(C) ->
    remove_sub_clusters(lists:keysort(2,C),[]).

remove_sub_clusters([], Acc)->
    lists:reverse(Acc);
remove_sub_clusters([{C, {Closed, SizeRatio}}|T], Acc) ->
    case lists:any(fun({C1, _})->
			   C--C1/=C 
		   end, Acc) of 
	true ->
	    remove_sub_clusters(T, Acc);
	false ->
	    remove_sub_clusters(T, [{C, {Closed, SizeRatio}}|Acc])
    end.
			  
								 
			  
				 
				
    


 
%% wrangler_modularity_inspection:gen_module_graph("ibrowse_mg.dot", [],["c:/cygwin/home/hl/modularity_casestudy/apache-couchdb-0.11.0/src/ibrowse"], true).
%% wrangler_modularity_inspection:gen_module_graph("couchdb_mg.dot", [],["c:/cygwin/home/hl/modularity_casestudy/apache-couchdb-0.11.0/src/couchdb"], true). 
%% wrangler_modularity_inspection:gen_module_graph("wrangler_mg.dot", [], ["c:/cygwin/home/hl/modularity_casestudy/wrangler-0.8.7/src"], true).


%% wrangler_modularity_inspection:improper_inter_module_calls("ibrowse_improper_mc.dot", ["c:/cygwin/home/hl/modularity_casestudy/apache-couchdb-0.11.0/src/ibrowse"], []).
%% wrangler_modularity_inspection:improper_inter_module_calls("couchdb_improper__mc.dot", ["c:/cygwin/home/hl/modularity_casestudy/apache-couchdb-0.11.0/src/couchdb"], []). 
%% wrangler_modularity_inspection:improper_inter_module_calls("wrangler_improper__mc.dot", ["c:/cygwin/home/hl/modularity_casestudy/wrangler-0.8.7/src"], []).


%% wrangler_modularity_inspection:cyclic_dependent_modules("ibrowse_cycles.dot", ["c:/cygwin/home/hl/modularity_casestudy/apache-couchdb-0.11.0/src/ibrowse"],true).
%% wrangler_modularity_inspection:cyclic_dependent_modules("couchdb_cycles.dot", ["c:/cygwin/home/hl/modularity_casestudy/apache-couchdb-0.11.0/src/couchdb"],true).
%% wrangler_modularity_inspection:cyclic_dependent_modules("wrangler_cycles.dot", ["c:/cygwin/home/hl/modularity_casestudy/wrangler-0.8.7/src"],true).


%%wrangler_modularity_inspection:find_min_dist([{[a], [{[a], 1},{[b],0.1}, {[c], 0.9}]},{[b], [{[a],0.1}, {[b],1}, {[c], 0.9}]}, {[c], [{[a],0.9}, {[b], 0.9}, {[c],1}]}]).

%% wrangler_modularity_inspection:cluster("c:/cygwin/home/hl/modularity_casestudy/apache-couchdb-0.11.0/src/ibrowse/ibrowse_lib.erl", ["c:/cygwin/home/hl/modularity_casestudy/%%apache-couchdb-0.11.0/src/ibrowse"]).
