%%%-------------------------------------------------------------------
%%% File    : dialyzer_module_callgraph.erl
%%% Author  : Tobias Lindahl <tobiasl@csd.uu.se>
%%% Description : Build a callgraph over modules in an application.
%%%
%%% Created :  9 Jun 2004 by Tobias Lindahl <tobiasl@csd.uu.se>
%%% Modified: 17 Jan 2007 by Huiiqng Li <hl@kent.ac.uk>
%%%-------------------------------------------------------------------

-module(refac_callgraph).

-export([construct/1]).

-include("../hrl/wrangler.hrl").

construct(List) ->
  {Calls, FunDefMap, ExternalCalls} = preprocess(List),
  Edges = get_edges(Calls),  
  Nodes = gb_trees:keys(Calls),
  {SCCs, SccMap} = get_sccs(Nodes, Edges),
  SCCEdges = get_scc_edges(Nodes, SccMap, Calls),
  SCCOrder = order_sccs(SCCs, SCCEdges),
  FinalOrder = finalize_order(SCCOrder, SCCs, FunDefMap), 
  #callgraph{scc_order=FinalOrder, external_calls=ExternalCalls}.

preprocess(List1) ->
  List2 = [{Fun, CalledFuns} || {{Fun, _FunDef}, CalledFuns} <- List1],
  FunCallsOrdDict = orddict:from_list(List2),
  Funs = ordsets:from_list([X || {X, _} <- FunCallsOrdDict]),
  List3 = [{Fun, FunDef} || {{Fun, FunDef}, _CalledFuns} <- List1],
  FunDefOrdDict = gb_trees:from_orddict(orddict:from_list(List3)),
  ExternalCalls = find_ext_called_funs(Funs, FunCallsOrdDict),
  {filter_calls(FunCallsOrdDict, Funs), FunDefOrdDict, ExternalCalls}.

filter_calls(FunCallsOrdDict, Funs) ->
  FilterFun = fun(X)->ordsets:is_element(X, Funs)end,
  MapFun = fun(_Mod, Calls)-> ordsets:filter(FilterFun,Calls)end,
  Filtered = orddict:map(MapFun, FunCallsOrdDict),
  gb_trees:from_orddict(Filtered).

find_ext_called_funs(Funs, FunCallsOrdDict) ->
  find_ext_called_funs(Funs, FunCallsOrdDict, []).

find_ext_called_funs(Funs, [{_, Calls}|Left], Acc) ->
  ExtCalls = 
    ordsets:filter(fun(X)-> not ordsets:is_element(X, Funs)end, Calls),
  NewAcc = ordsets:union(ExtCalls, Acc),
  find_ext_called_funs(Funs, Left, NewAcc);
find_ext_called_funs(_Funs, [], Acc) -> Acc.

%%---------------------------------------------------------------------
%% Find the edges in the callgraph.

get_edges(Calls) ->
  get_edges(gb_trees:to_list(Calls), []).

get_edges([{MFA, Set}|Left], Edges) ->  
  EdgeList = [{MFA, X} || X <- Set],
  EdgeSet = ordsets:from_list(EdgeList),
  get_edges(Left, ordsets:union(EdgeSet, Edges));
get_edges([], Edges) ->
  Edges.

%%---------------------------------------------------------------------
%% Find the strongly connected components in the callgraph.

get_sccs(Nodes, Edges) ->
  get_sccs(Nodes, Edges, 0, gb_trees:empty(), gb_trees:empty()).

get_sccs([Node|Left], Edges, Id, SccMap, Acc) ->  
  {PreOrder, VisitedDFS} = dfs(Node, Edges),
  {_RDFS, VisitedRDFS} = rdfs(Node, Edges),
  SCC = ordsets:intersection(VisitedDFS, VisitedRDFS),
  OrderedSCC = lists:filter(fun(X) -> ordsets:is_element(X, SCC)end, 
			    lists:reverse(PreOrder)),
  NewSccMap = lists:foldl(fun(X, Map)->gb_trees:insert(X, Id, Map)end,
			  SccMap, SCC),
  get_sccs(ordsets:subtract(Left, SCC), Edges, 
	   Id + 1, NewSccMap, gb_trees:insert(Id, OrderedSCC, Acc));
get_sccs([], _Edges, _Id, SccMap, Acc) ->
  {Acc, SccMap}.

  
%%---------------------------------------------------------------------
%% Find the edges between the strongly connected components.
  
get_scc_edges(Nodes, SccMap, Calls) ->
  get_scc_edges(Nodes, SccMap, Calls, []).

get_scc_edges([Node|Left], SccMap, Calls, Acc) ->
  SccId = gb_trees:get(Node, SccMap),
  NodeCalls = gb_trees:get(Node, Calls),
  NewEdges = [{SccId, gb_trees:get(X, SccMap)} || X <- NodeCalls],
  NewEdgeSet = ordsets:from_list(NewEdges),
  NewEdgeSet1 = ordsets:del_element({SccId, SccId}, NewEdgeSet),
  get_scc_edges(Left, SccMap, Calls, ordsets:union(Acc, NewEdgeSet1));
get_scc_edges([], _SccMap, _Calls, Acc) ->
  Acc.


%%---------------------------------------------------------------------
%% Order the SCCs in a reverse preordering to ensure that all
%% successors to a node has been treated before the node is treated
%% itself.

order_sccs(SCCs, Edges) ->
  Nodes = gb_trees:keys(SCCs),
  lists:flatten(order_sccs_1(Nodes, Edges, [])).

order_sccs_1([Node|Left], Edges, Visited) ->
  {PreOrder, DFSVisited} = dfs([Node], Edges, Visited, []),
  NewLeft = ordsets:subtract(Left, Visited),
  NewVisited = ordsets:union(DFSVisited, Visited),
  [lists:reverse(PreOrder) | order_sccs_1(NewLeft, Edges, NewVisited)];
order_sccs_1([], _Edges, _Visited) ->
  [].


%%---------------------------------------------------------------------
%% Put the actual function names in the order to process them.

finalize_order([Id|Left], SCCs, FunToDefMap)->
  Funs = gb_trees:get(Id, SCCs),
  FunDefs = [{Fun, gb_trees:get(Fun, FunToDefMap)} || Fun <- Funs],
  [FunDefs | finalize_order(Left, SCCs, FunToDefMap)];
finalize_order([], _SCCs, _FunToDefMap) ->
  [].


%%---------------------------------------------------------------------
%% dfs/2 returns a preordered depth first search and the nodes visited.

dfs(Node, Edges) ->
  dfs([Node], Edges, [], []).

dfs([Node|Left], Edges, Visited, Order)->
  case ordsets:is_element(Node, Visited) of
    true ->
      dfs(Left, Edges, Visited, Order);
    false ->
      Filter = fun({F, _T}) -> Node =:= F end,
      Succ = [X || {_, X} <- ordsets:filter(Filter, Edges)],
      {NewOrder, NewVisited} = dfs(Succ, Edges, 
				   ordsets:add_element(Node, Visited), Order),
      dfs(Left, Edges, NewVisited, [Node|NewOrder])
  end;
dfs([], _Edges, Visited, Order) ->
  {Order, Visited}.


%%---------------------------------------------------------------------
%% rdfs/2 returns a preordered depth first search of the inverted graph,
%% and the nodes visited.

rdfs(Node, Edges) ->
  rdfs([Node], Edges, [], []).

rdfs([Node|Left], Edges, Visited, Order)->
  case ordsets:is_element(Node, Visited) of
    true ->
      rdfs(Left, Edges, Visited, Order);
    false ->
      Filter = fun({_F, T}) -> Node =:= T end,
      Succ = [X || {X, _} <- ordsets:filter(Filter, Edges)],
      {NewOrder, NewVisited} = rdfs(Succ, Edges, 
				    ordsets:add_element(Node, Visited), Order),
      rdfs(Left, Edges, NewVisited, [Node|NewOrder])
  end;
rdfs([], _Edges, Visited, Order) ->
  {Order, Visited}.
