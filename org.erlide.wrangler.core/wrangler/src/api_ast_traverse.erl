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
%% @private

-module(api_ast_traverse).

-export([once_tdTU/3, stop_tdTU/3, full_tdTU/3, 
         stop_tdTP/3, full_tdTP/3, full_buTP/3]).

-export([map/2, map_subtrees/2, mapfold/3,
	 mapfold_subtrees/3, fold/3, fold_subtrees/3]).

-export([foldl_listlist/3, mapfoldl_listlist/3]).

-include("../include/wrangler_internal.hrl").


%% =====================================================================
%% @spec once_tdTU(Function, Tree::syntaxTree(), Others::term())-> {term(), boolean()}
%%       Function = (syntaxTree(), term()) -> {term(), boolean()}
%%
%% @doc Once-topdown type-unifying traversal of the abstract syntax tree with some
%% information collected. This function does a pre-order traversal of the
%% abstract syntax tree, and collects the first node, X say, such that
%% Function(X, Others) returns {term(), true}. Function must has a arity of 2, with 
%% the first parameter by the AST node, and all the other necessary information put 
%% into a tupe as the second parameter.
%%
%% @see full_buTP/2
%% @see stop_tdTP/3 		
%% @see refac_syntax_lib:fold/3.

-spec(once_tdTU/3::(fun((syntaxTree(), any()) ->
			       {anyterm(), boolean()}), syntaxTree(), anyterm()) ->
	     {anyterm(), boolean()}).
once_tdTU(Function, Node, Others) ->
    case Function(Node, Others) of
      {R, true} -> {R, true};
      {_R, false} ->
	  case wrangler_syntax:subtrees(Node) of
	    [] -> {[], false};
	    Gs ->
		Flattened_Gs = [T || G <- Gs, T <- G],
		case Flattened_Gs of
		  [] -> {[], false};
		  [H| T1] -> until(Function, [H| T1], Others)
		end
	  end
    end.


 until(_F, [], _Others) -> {[], false};
 until(F, [H| T], Others) ->
     case once_tdTU(F, H, Others) of
       {_R, true} -> {_R, true};
       {_Rq, false} -> until(F, T, Others)
     end.

 %% =====================================================================
 %% @spec stop_tdTP(Function, Tree::syntaxTree(), Others::[term()])->  syntaxTree()
 %%       Function = (syntaxTree(),{term()}) -> {syntaxTree(), boolean()}
 %%
 %% @doc Stop-topdown type-preserving traversal of the abstract syntax tree.
 %% This function does a pre-order traversal of the abstract syntax tree, and
 %% modifies certain nodes according to Function. Once a node has been modified, 
 %% its subtrees are not going to be traversed.
 %% 'Function' must have a arity of two, with the first being the AST node, and 
 %% the second being a tuple containing all the other needed info; 'Function' 
 %% should returns a tuple containing the possibly modified node and a bool value, 
 %% with the bool value indicating whether the node has been modified.
 %%
 %% @see full_buTP/2
 %% @see once_tdTU/3

-spec(stop_tdTP/3::(fun((syntaxTree(), anyterm()) ->
 			       {syntaxTree(), boolean()}), syntaxTree(), anyterm()) ->
 	     {syntaxTree(), boolean()}).
stop_tdTP(Function, Node, Others) ->
     case Function(Node, Others) of
       {Node1, true} -> {Node1, true};
       {Node1, false} ->
           case wrangler_syntax:subtrees(Node1) of
             [] -> {Node1, false};
             Gs ->
                 Gs1 = [[stop_tdTP(Function, T, Others) || T <- G] || G <- Gs],
                 Gs2 = [[N || {N, _B} <- G] || G <- Gs1],
                 G = [[B || {_N, B} <- G] || G <- Gs1],
                 Node2 = wrangler_syntax:make_tree(wrangler_syntax:type(Node1), Gs2),
                 {rewrite(Node1, Node2), lists:member(true, lists:flatten(G))}
           end
     end.


-spec(full_tdTP/3::(fun((syntaxTree(), anyterm()) ->
			       {syntaxTree(), boolean()}), syntaxTree(), anyterm()) ->
	     {syntaxTree(), boolean()}).
full_tdTP(Function, Node, Others) ->
    case Function(Node, Others) of
      {Node1, Changed} ->
	  case wrangler_syntax:subtrees(Node1) of
	    [] -> {Node1, Changed};
	    Gs ->
		Gs1 = [[full_tdTP(Function, T, Others) || T <- G] || G <- Gs],
		Gs2 = [[N || {N, _B} <- G] || G <- Gs1],
		G = [[B || {_N, B} <- G] || G <- Gs1],
		Node2 = wrangler_syntax:make_tree(wrangler_syntax:type(Node1), Gs2),
		{rewrite(Node1, Node2), Changed or lists:member(true, lists:flatten(G))}
	  end
    end.


%% =====================================================================
%% @spec full_buTP(Function, Tree::syntaxTree(), {term()})-> syntaxTree()
%%       Function = (syntaxTree(), {term()}) -> syntaxTree()
%%
%% @doc Full bottom_up type-preserving traversal of the abstract syntax tree.
%% This function does a bottom_up traversal of the abstract syntax tree, and 
%% modifies certain nodes according to Function. Different from stop_tdTP, all 
%% the nodes in the abstract syntax tree are traversed by this function. 
%% @see stop_tdTP/2
%% @see once_tdTU/3

-spec(full_buTP/3::(fun((syntaxTree(), any()) -> syntaxTree()), syntaxTree(), anyterm())->
	     syntaxTree()).       
full_buTP(Fun, Tree, Others) ->
    case wrangler_syntax:subtrees(Tree) of
      [] -> Fun(Tree, Others); 
      Gs ->
	  Gs1 = [[full_buTP(Fun, T, Others) || T <- G] || G <- Gs],
	  Tree1 = wrangler_syntax:make_tree(wrangler_syntax:type(Tree), Gs1),
	  Fun(rewrite(Tree, Tree1), Others)
    end.

rewrite(Tree, Tree1) ->
    wrangler_syntax:copy_attrs(Tree, Tree1).



stop_tdTU(F, S, Node) ->
    case F(Node, S) of
        {R, true} -> R;
        {R, false} ->
            Gs = wrangler_syntax:subtrees(Node),
            stop_tdTU_1(F, R, Gs)
    end.

stop_tdTU_1(F, S, [L | Ls]) ->
    stop_tdTU_1(F, stop_tdTU_2(F, S, L), Ls);
stop_tdTU_1(_, S, []) -> S.

stop_tdTU_2(F, S, [T | Ts]) -> stop_tdTU_2(F, stop_tdTU(F, S, T), Ts);
stop_tdTU_2(_, S, []) -> S.

       
full_tdTU(F, S, Tree) ->
    R = F(Tree, S),
    case wrangler_syntax:subtrees(Tree) of
        [] -> 
            R;
        Gs -> 
            full_tdTU_1(F, R, Gs)
    end.

full_tdTU_1(F, S, [L | Ls]) ->
    full_tdTU_1(F, full_tdTU_2(F, S, L), Ls);
full_tdTU_1(_, S, []) -> S.

full_tdTU_2(F, S, [T | Ts]) -> full_tdTU_2(F, full_tdTU(F, S, T), Ts);
full_tdTU_2(_, S, []) -> S.

%% =====================================================================
%% @spec map(Function, Tree::syntaxTree()) -> syntaxTree()
%%
%%          Function = (syntaxTree()) -> syntaxTree()
%%
%% @doc Applies a function to each node of a syntax tree. The result of
%% each application replaces the corresponding original node. The order
%% of traversal is bottom-up.
%%
%% @see map_subtrees/2

map(F, Tree) ->
    case wrangler_syntax:subtrees(Tree) of
      [] -> F(Tree);
      Gs ->
	  Tree1 = wrangler_syntax:make_tree(wrangler_syntax:type(Tree),
				          [[map(F, T) || T <- G] || G <- Gs]),
	  F(wrangler_syntax:copy_attrs(Tree, Tree1))
    end.

%% =====================================================================
%% @spec map_subtrees(Function, syntaxTree()) -> syntaxTree()
%%
%%          Function = (Tree) -> Tree1
%%
%% @doc Applies a function to each immediate subtree of a syntax tree.
%% The result of each application replaces the corresponding original
%% node.
%%
%% @see map/2

map_subtrees(F, Tree) ->
    case wrangler_syntax:subtrees(Tree) of
      [] -> Tree;
      Gs ->
	  Tree1 = wrangler_syntax:make_tree(wrangler_syntax:type(Tree),
				          [[F(T) || T <- G] || G <- Gs]),
	  wrangler_syntax:copy_attrs(Tree, Tree1)
    end.

%% =====================================================================
%% @spec fold(Function, Start::term(), Tree::syntaxTree()) -> term()
%%
%%          Function = (syntaxTree(), term()) -> term()
%%
%% @doc Folds a function over all nodes of a syntax tree. The result is
%% the value of <code>Function(X1, Function(X2, ... Function(Xn, Start)
%% ... ))</code>, where <code>[X1, X2, ..., Xn]</code> are the nodes of
%% <code>Tree</code> in a post-order traversal.
%%
%% @see fold_subtrees/3
%% @see foldl_listlist/3

fold(F, S, Tree) ->
    case wrangler_syntax:subtrees(Tree) of
      [] -> F(Tree, S);
      Gs -> F(Tree, fold_1(F, S, Gs))
    end.

fold_1(F, S, [L | Ls]) ->
    fold_1(F, fold_2(F, S, L), Ls);
fold_1(_, S, []) -> S.

fold_2(F, S, [T | Ts]) -> fold_2(F, fold(F, S, T), Ts);
fold_2(_, S, []) -> S.

%% =====================================================================
%% @spec fold_subtrees(Function, Start::term(), Tree::syntaxTree()) ->
%%           term()
%%
%%          Function = (syntaxTree(), term()) -> term()
%%
%% @doc Folds a function over the immediate subtrees of a syntax tree.
%% This is similar to <code>fold/3</code>, but only on the immediate
%% subtrees of <code>Tree</code>, in left-to-right order; it does not
%% include the root node of <code>Tree</code>.
%%
%% @see fold/3


fold_subtrees(F, S, Tree) ->
    foldl_listlist(F, S, wrangler_syntax:subtrees(Tree)).

%% =====================================================================
%% @spec mapfold(Function, Start::term(), Tree::syntaxTree()) ->
%%           {syntaxTree(), term()}
%%
%%          Function = (syntaxTree(), term()) -> {syntaxTree(), term()}
%%
%% @doc Combines map and fold in a single operation. This is similar to
%% <code>map/2</code>, but also propagates an extra value from each
%% application of the <code>Function</code> to the next, while doing a
%% post-order traversal of the tree like <code>fold/3</code>. The value
%% <code>Start</code> is passed to the first function application, and
%% the final result is the result of the last application.
%%
%% @see map/2
%% @see fold/3

mapfold(F, S, Tree) ->
    case wrangler_syntax:subtrees(Tree) of
      [] -> F(Tree, S);
      Gs ->
	  {Gs1, S1} = mapfold_1(F, S, Gs),
	  Tree1 = wrangler_syntax:make_tree(wrangler_syntax:type(Tree),
				          Gs1),
	  F(wrangler_syntax:copy_attrs(Tree, Tree1), S1)
    end.

mapfold_1(F, S, [L | Ls]) ->
    {L1, S1} = mapfold_2(F, S, L),
    {Ls1, S2} = mapfold_1(F, S1, Ls),
    {[L1 | Ls1], S2};
mapfold_1(_, S, []) -> {[], S}.

mapfold_2(F, S, [T | Ts]) ->
    {T1, S1} = mapfold(F, S, T),
    {Ts1, S2} = mapfold_2(F, S1, Ts),
    {[T1 | Ts1], S2};
mapfold_2(_, S, []) -> {[], S}.

%% =====================================================================
%% @spec mapfold_subtrees(Function, Start::term(),
%%                        Tree::syntaxTree()) -> {syntaxTree(), term()}
%%
%%          Function = (syntaxTree(), term()) -> {syntaxTree(), term()}
%%
%% @doc Does a mapfold operation over the immediate subtrees of a syntax
%% tree. This is similar to <code>mapfold/3</code>, but only on the
%% immediate subtrees of <code>Tree</code>, in left-to-right order; it
%% does not include the root node of <code>Tree</code>.
%%
%% @see mapfold/3


mapfold_subtrees(F, S, Tree) ->
    case wrangler_syntax:subtrees(Tree) of
	[] -> {Tree, S};
	Gs ->
	    {Gs1, S1} = mapfoldl_listlist(F, S, Gs),
	    Tree1 = wrangler_syntax:make_tree(wrangler_syntax:type(Tree),
					      Gs1),
	    {wrangler_syntax:copy_attrs(Tree, Tree1), S1}
    end.

%% =====================================================================
%% @spec foldl_listlist(Function, Start::term(), [[term()]]) -> term()
%%
%%          Function = (term(), term()) -> term()
%%
%% @doc Like <code>lists:foldl/3</code>, but over a list of lists.
%%
%% @see fold/3
%% @see lists:foldl/3

foldl_listlist(F, S, [L | Ls]) ->
    foldl_listlist(F, foldl(F, S, L), Ls);
foldl_listlist(_, S, []) -> S.

foldl(F, S, [T | Ts]) -> foldl(F, F(T, S), Ts);
foldl(_, S, []) -> S.

%% =====================================================================
%% @spec mapfoldl_listlist(Function, State, [[term()]]) ->
%%           {[[term()]], term()}
%%
%%          Function = (term(), term()) -> {term(), term()}
%%
%% @doc Like <code>lists:mapfoldl/3</code>, but over a list of lists.
%% The list of lists in the result has the same structure as the given
%% list of lists.

mapfoldl_listlist(F, S, [L | Ls]) ->
    {L1, S1} = mapfoldl(F, S, L),
    {Ls1, S2} = mapfoldl_listlist(F, S1, Ls),
    {[L1 | Ls1], S2};
mapfoldl_listlist(_, S, []) -> {[], S}.

mapfoldl(F, S, [L | Ls]) ->
    {L1, S1} = F(L, S),
    {Ls1, S2} = mapfoldl(F, S1, Ls),
    {[L1 | Ls1], S2};
mapfoldl(_, S, []) -> {[], S}.
