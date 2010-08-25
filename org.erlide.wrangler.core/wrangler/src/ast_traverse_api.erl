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

-module(ast_traverse_api).

-export([once_tdTU/3, stop_tdTP/3, full_tdTP/3, full_buTP/3]).

-include("../include/wrangler.hrl").

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

%%-spec(once_tdTU/3::(fun((syntaxTree(), any()) ->
%%			       {anyterm(), boolean()}), syntaxTree(), anyterm()) ->
%%	     {anyterm(), boolean()}).
once_tdTU(Function, Node, Others) ->
    case Function(Node, Others) of
      {R, true} -> {R, true};
      {_R, false} ->
	  case refac_syntax:subtrees(Node) of
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
%%-spec(stop_tdTP/3::(fun((syntaxTree(), anyterm()) ->
%%			       {syntaxTree(), boolean()}), syntaxTree(), anyterm()) ->
%%	     {syntaxTree(), boolean()}).
stop_tdTP(Function, Node, Others) ->
    case Function(Node, Others) of
      {Node1, true} -> {Node1, true};
      {Node1, false} ->
	  case refac_syntax:subtrees(Node1) of
	    [] -> {Node1, false};
	    Gs ->
		Gs1 = [[stop_tdTP(Function, T, Others) || T <- G] || G <- Gs],
		Gs2 = [[N || {N, _B} <- G] || G <- Gs1],
		G = [[B || {_N, B} <- G] || G <- Gs1],
		Node2 = refac_syntax:make_tree(refac_syntax:type(Node1), Gs2),
		{refac_misc:rewrite(Node1, Node2), lists:member(true, lists:flatten(G))}
	  end
    end.


%%-spec(full_tdTP/3::(fun((syntaxTree(), anyterm()) ->
%%			       {syntaxTree(), boolean()}), syntaxTree(), anyterm()) ->
%%	     {syntaxTree(), boolean()}).
full_tdTP(Function, Node, Others) ->
    case Function(Node, Others) of
      {Node1, Changed} ->
	  case refac_syntax:subtrees(Node1) of
	    [] -> {Node1, Changed};
	    Gs ->
		Gs1 = [[full_tdTP(Function, T, Others) || T <- G] || G <- Gs],
		Gs2 = [[N || {N, _B} <- G] || G <- Gs1],
		G = [[B || {_N, B} <- G] || G <- Gs1],
		Node2 = refac_syntax:make_tree(refac_syntax:type(Node1), Gs2),
		{refac_misc:rewrite(Node1, Node2), Changed or lists:member(true, lists:flatten(G))}
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
%%-spec(full_buTP/3::(fun((syntaxTree(), any()) -> syntaxTree()), syntaxTree(), anyterm())->
%%	     syntaxTree()).       
full_buTP(Fun, Tree, Others) ->
    case refac_syntax:subtrees(Tree) of
      [] -> Fun(Tree, Others);
      Gs ->
	  Gs1 = [[full_buTP(Fun, T, Others) || T <- G] || G <- Gs],
	  Tree1 = refac_syntax:make_tree(refac_syntax:type(Tree), Gs1),
	  Fun(refac_misc:rewrite(Tree, Tree1), Others)
    end.



%% stop_tdTU(Function, S, Node) ->
%%     {Res, _} = stop_tdTU_1(Function, S, Node),
%%     Res.

%% stop_tdTU_1(Function, S, Node) ->
%%     case Function(Node, S) of
%%       {R, true} -> {R, true};
%%       {_R, false} ->
%% 	  case erl_syntax:subtrees(Node) of
%% 	    [] -> {[], false};
%% 	    Gs ->
%% 		Flattened_Gs = [T || G <- Gs, T <- G],
%% 		case Flattened_Gs of
%% 		  [] -> {[], false};
%% 		  [_H | _T1] -> S1 = [[stop_tdTU_1(Function, [], T) || T <- G] || G <- Gs],
%% 			      S2 = [S12 || G<-S1, {S12, _B} <- G],
%% 				{S++lists:append(S2), true}
%% 		end
%% 	  end
%%     end.
