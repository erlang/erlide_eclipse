%% =====================================================================
%% Support library for abstract Erlang syntax trees.
%%
%% Copyright (C) 1997-2002 Richard Carlsson
%%
%% This library is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation; either version 2 of the
%% License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%% USA
%%
%% Author contact: richardc@csd.uu.se
%%
%% Modified: 17 Jan 2007 by  Huiqing Li <hl@kent.ac.uk>
%% $Id: refac_syntax_lib.erl,v 1.3 2008-04-30 09:28:12 hl Exp $
%%
%% =====================================================================
%%
%% @doc Support library for abstract Erlang syntax trees.
%%
%% <p> This module contains utility functions for working with the
%% abstract data type defined in the module <a
%% href="erl_syntax.html"><code>erl_syntax</code></a>.</p>
%%
%% @type syntaxTree() = refac_syntax:syntaxTree(). An abstract syntax
%% tree. See the <code>erl_syntax</code> module for details.

-module(refac_syntax_lib).

-export([ analyze_application/1,
	 analyze_attribute/1, analyze_export_attribute/1,
	 analyze_file_attribute/1, analyze_form/1,
	 analyze_forms/1, analyze_function/1,
	 analyze_function_name/1, analyze_implicit_fun/1,
	 analyze_import_attribute/1, analyze_module_attribute/1,
	 analyze_record_attribute/1, analyze_record_expr/1,
	 analyze_record_field/1, analyze_rule/1,
	 analyze_wild_attribute/1, annotate_bindings/1,
	 annotate_bindings/2, fold/3, fold_subtrees/3,
	 foldl_listlist/3, function_name_expansions/1,
	 is_fail_expr/1, limit/2, limit/3, map/2, map_subtrees/2,
	 mapfold/3, mapfold_subtrees/3, mapfoldl_listlist/3,
	 new_variable_name/1, new_variable_name/2,
	 new_variable_names/2, new_variable_names/3,
	 strip_comments/1, to_comment/1, to_comment/2,
	 to_comment/3, variables/1, vann_clauses/2, vann_case_expr/2, vann_block_expr/2]).

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
    case refac_syntax:subtrees(Tree) of
      [] -> F(Tree);
      Gs ->
	  Tree1 = refac_syntax:make_tree(refac_syntax:type(Tree),
				       [[map(F, T) || T <- G] || G <- Gs]),
	  F(refac_syntax:copy_attrs(Tree, Tree1))
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
    case refac_syntax:subtrees(Tree) of
      [] -> Tree;
      Gs ->
	  Tree1 = refac_syntax:make_tree(refac_syntax:type(Tree),
				       [[F(T) || T <- G] || G <- Gs]),
	  refac_syntax:copy_attrs(Tree, Tree1)
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
    case refac_syntax:subtrees(Tree) of
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
    foldl_listlist(F, S, refac_syntax:subtrees(Tree)).

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
    case refac_syntax:subtrees(Tree) of
      [] -> F(Tree, S);
      Gs ->
	  {Gs1, S1} = mapfold_1(F, S, Gs),
	  Tree1 = refac_syntax:make_tree(refac_syntax:type(Tree),
				       Gs1),
	  F(refac_syntax:copy_attrs(Tree, Tree1), S1)
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
    case refac_syntax:subtrees(Tree) of
      [] -> {Tree, S};
      Gs ->
	  {Gs1, S1} = mapfoldl_listlist(F, S, Gs),
	  Tree1 = refac_syntax:make_tree(refac_syntax:type(Tree),
				       Gs1),
	  {refac_syntax:copy_attrs(Tree, Tree1), S1}
    end.

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

%% =====================================================================
%% @spec variables(syntaxTree()) -> set(atom())
%%
%%        set(T) = sets:set(T)
%%
%% @doc Returns the names of variables occurring in a syntax tree, The
%% result is a set of variable names represented by atoms.
%%
%% @see sets

variables(Tree) ->
    F = fun (T, S) ->
		case refac_syntax:type(T) of
		  variable ->
		      sets:add_element(refac_syntax:variable_name(T), S);
		  _ -> S
		end
	end,
    fold(F, sets:new(), Tree).

-define(MINIMUM_RANGE, 100).

-define(START_RANGE_FACTOR, 100).

-define(MAX_RETRIES,
	3).    % retries before enlarging range

-define(ENLARGE_ENUM,
	8).   % range enlargment enumerator

-define(ENLARGE_DENOM,
	1).  % range enlargment denominator

-define(DEFAULT_LOC, 
        {0, 0}).  %% default defining location.

default_variable_name(N) ->
    list_to_atom("V" ++ integer_to_list(N)).




%% =====================================================================
%% @spec new_variable_name(Used::set(atom())) -> atom()
%%
%% @doc Returns an atom which is not already in the set
%% <code>Used</code>. This is equivalent to
%% <code>new_variable_name(Function, Used)</code>, where
%% <code>Function</code> maps a given integer <code>N</code> to the atom
%% whose name consists of "<code>V</code>" followed by the numeral for
%% <code>N</code>.
%%
%% @see new_variable_name/2

new_variable_name(S) ->
    new_variable_name(fun default_variable_name/1, S).

%% =====================================================================
%% @spec new_variable_name(Function, Used::set(atom())) -> atom()
%%
%%          Function = (integer()) -> atom()
%%
%% @doc Returns a user-named atom which is not already in the set
%% <code>Used</code>. The atom is generated by applying the given
%% <code>Function</code> to a generated integer. Integers are generated
%% using an algorithm which tries to keep the names randomly distributed
%% within a reasonably small range relative to the number of elements in
%% the set.
%%
%% <p>This function uses the module <code>random</code> to generate new
%% keys. The seed it uses may be initialized by calling
%% <code>random:seed/0</code> or <code>random:seed/3</code> before this
%% function is first called.</p>
%%
%% @see new_variable_name/1
%% @see sets
%% @see random

new_variable_name(F, S) ->
    R = start_range(S), new_variable_name(R, F, S).

new_variable_name(R, F, S) ->
    new_variable_name(generate(R, R), R, 0, F, S).

new_variable_name(N, R, T, F, S)
    when T < (?MAX_RETRIES) ->
    A = F(N),
    case sets:is_element(A, S) of
      true ->
	  new_variable_name(generate(N, R), R, T + 1, F, S);
      false -> A
    end;
new_variable_name(N, R, _T, F, S) ->
    %% Too many retries - enlarge the range and start over.
    R1 = R * (?ENLARGE_ENUM) div (?ENLARGE_DENOM),
    new_variable_name(generate(N, R1), R1, 0, F, S).

%% Note that we assume that it is very cheap to take the size of
%% the given set. This should be valid for the stdlib
%% implementation of `sets'.

start_range(S) ->
    max(sets:size(S) * (?START_RANGE_FACTOR),
	?MINIMUM_RANGE).

max(X, Y) when X > Y -> X;
max(_, Y) -> Y.

%% The previous number might or might not be used to compute the
%% next number to be tried. It is currently not used.
%%
%% It is important that this function does not generate values in
%% order, but (pseudo-)randomly distributed over the range.

generate(_Key, Range) ->
    random:uniform(Range).    % works well

%% =====================================================================
%% @spec new_variable_names(N::integer(), Used::set(atom())) -> [atom()]
%%
%% @doc Like <code>new_variable_name/1</code>, but generates a list of
%% <code>N</code> new names.
%%
%% @see new_variable_name/1

new_variable_names(N, S) ->
    new_variable_names(N, fun default_variable_name/1, S).

%% =====================================================================
%% @spec new_variable_names(N::integer(), Function,
%%                          Used::set(atom())) -> [atom()]
%%
%%          Function = (integer()) -> atom()
%%
%% @doc Like <code>new_variable_name/2</code>, but generates a list of
%% <code>N</code> new names.
%%
%% @see new_variable_name/2

new_variable_names(N, F, S) when is_integer(N) ->
    R = start_range(S), new_variable_names(N, [], R, F, S).

new_variable_names(N, Names, R, F, S) when N > 0 ->
    Name = new_variable_name(R, F, S),
    S1 = sets:add_element(Name, S),
    new_variable_names(N - 1, [Name | Names], R, F, S1);
new_variable_names(0, Names, _, _, _) -> Names.

%% =====================================================================
%% @spec annotate_bindings(Tree::syntaxTree(),
%%                         Bindings::ordset(atom())) -> syntaxTree()
%%
%%          ordset(T) = ordsets:ordset(T)
%%
%% @doc Adds or updates annotations on nodes in a syntax tree.
%% <code>Bindings</code> specifies the set of bound variables in the
%% environment of the top level node. The following annotations are
%% affected:
%% <ul>
%%     <li><code>{env, Vars}</code>, representing the input environment
%%     of the subtree.</li>
%%
%%     <li><code>{bound, Vars}</code>, representing the variables that
%%     are bound in the subtree.</li>
%%
%%     <li><code>{free, Vars}</code>, representing the free variables in
%%     the subtree.</li>
%% </ul>
%% <code>Bindings</code> and <code>Vars</code> are ordered-set lists
%% (cf. module <code>ordsets</code>) of atoms representing variable
%% names.
%%
%% @see annotate_bindings/1
%% @see ordsets

annotate_bindings(Tree, Env) ->
    {Tree1, _, _} = vann(Tree, Env), Tree1.

%% =====================================================================
%% @spec annotate_bindings(Tree::syntaxTree()) -> syntaxTree()
%%
%% @doc Adds or updates annotations on nodes in a syntax tree.
%% Equivalent to <code>annotate_bindings(Tree, Bindings)</code> where
%% the top-level environment <code>Bindings</code> is taken from the
%% annotation <code>{env, Bindings}</code> on the root node of
%% <code>Tree</code>. An exception is thrown if no such annotation
%% should exist.
%%
%% @see annotate_bindings/2

annotate_bindings(Tree) ->
    As = refac_syntax:get_ann(Tree),
    case lists:keysearch(env, 1, As) of
      {value, {env, InVars}} ->
	  annotate_bindings(Tree, InVars);
      _ -> erlang:error(badarg)
    end.

vann(Tree, Env) ->
    case refac_syntax:type(Tree) of
      variable ->
	  V = refac_syntax:variable_name(Tree),
	  P = refac_syntax:get_pos(Tree),
	  case [V2 || V2 <- Env, vann_1(V2, V)] of
	   [] ->
		Bound = [],
		Free = [{V, P}], %% ?DEFAULT_LOC}],
		Def = [?DEFAULT_LOC];
	    L ->
		Bound = [],
		Free = L,
		Def = [vann_1(V3) || V3 <- L]
	  end,
	  %%?wrangler_io("Tree, Env, Bound, Free, Def:\n~p\n", [{Tree, Env, Bound, Free, Def}]),
          %%?wrangler_io("R:\n~p\n", [ann_bindings(Tree, Env, Bound, Free, Def)]),
	  {ann_bindings(Tree, Env, Bound, Free, Def), Bound, Free};
      match_expr -> vann_match_expr(Tree, Env);
      case_expr -> vann_case_expr(Tree, Env);
      if_expr -> vann_if_expr(Tree, Env);
      cond_expr -> vann_cond_expr(Tree, Env);
      receive_expr -> vann_receive_expr(Tree, Env);
      try_expr -> vann_try_expr(Tree, Env);
      function -> vann_function(Tree, Env);
      rule -> vann_rule(Tree, Env);
      fun_expr -> vann_fun_expr(Tree, Env);
      list_comp -> vann_list_comp(Tree, Env);
      generator -> vann_generator(Tree, Env);
      block_expr -> vann_block_expr(Tree, Env);
      macro -> vann_macro(Tree, Env);
      %% Added by HL, begin.
      attribute -> case  refac_syntax:atom_value(refac_syntax:attribute_name(Tree)) of 
		       define -> vann_define(Tree,Env);
		       ifdef -> {Tree, [], []};  
		       inndef ->{Tree, [], []};
		       undef -> {Tree, [], []};
		       _ -> F = vann_list_join(Env),
			    {Tree1, {Bound, Free}} = mapfold_subtrees(F, {[],[]}, Tree),
			    {ann_bindings(Tree1, Env, Bound, Free), Bound, Free}
    		   end;
      %% Added by HL, end.
      _Type ->
	  F = vann_list_join(Env),
	  {Tree1, {Bound, Free}} = mapfold_subtrees(F, {[], []},
						    Tree),
	  {ann_bindings(Tree1, Env, Bound, Free), Bound, Free}
    end.

vann_1({V1, _L}, V) -> V == V1.

vann_1({_V, P}) -> P.

vann_list_join(Env) ->
    fun (T, {Bound, Free}) ->
	    {T1, Bound1, Free1} = vann(T, Env),
	    {T1,
	     {ordsets:union(Bound, Bound1),
	      ordsets:union(Free, Free1)}}
    end.

vann_list(Ts, Env) ->
    lists:mapfoldl(vann_list_join(Env), {[], []}, Ts).

vann_function(Tree, Env) ->
    Cs = refac_syntax:function_clauses(Tree),
    {Cs1, {_, Free}} = vann_clauses(Cs, Env),
    N = refac_syntax:function_name(Tree),
    {N1, _, _} = vann(N, Env),
    Tree1 = rewrite(Tree, refac_syntax:function(N1, Cs1)),
    Bound = [],
    {ann_bindings(Tree1, Env, Bound, Free), Bound, Free}.

vann_rule(Tree, Env) ->
    Cs = refac_syntax:rule_clauses(Tree),
    {Cs1, {_, Free}} = vann_clauses(Cs, Env),
    N = refac_syntax:rule_name(Tree),
    {N1, _, _} = vann(N, Env),
    Tree1 = rewrite(Tree, refac_syntax:rule(N1, Cs1)),
    Bound = [],
    {ann_bindings(Tree1, Env, Bound, Free), Bound, Free}.

vann_fun_expr(Tree, Env) ->
    Cs = refac_syntax:fun_expr_clauses(Tree),
    {Cs1, {_, Free}} = vann_fun_expr_clauses(Cs, Env),
    Tree1 = rewrite(Tree, refac_syntax:fun_expr(Cs1)),
    Bound = [],
    {ann_bindings(Tree1, Env, Bound, Free), Bound, Free}.

vann_match_expr(Tree, Env) ->
    E = refac_syntax:match_expr_body(Tree),
    {E1, Bound1, Free1} = vann(E, Env),
    Env1 = ordsets:union(Env, Bound1),
    P = refac_syntax:match_expr_pattern(Tree),
    {P1, Bound2, Free2} = vann_pattern(P, Env1),
    Bound = ordsets:union(Bound1, Bound2),
    Free = ordsets:union(Free1, Free2),
    Tree1 = rewrite(Tree, refac_syntax:match_expr(P1, E1)),
    {ann_bindings(Tree1, Env, Bound, Free), Bound, Free}.

vann_case_expr(Tree, Env) ->
    E = refac_syntax:case_expr_argument(Tree),
    {E1, Bound1, Free1} = vann(E, Env),
    Env1 = ordsets:union(Env, Bound1),
    Cs = refac_syntax:case_expr_clauses(Tree),
    {Cs1, {Bound2, Free2}} = vann_clauses(Cs, Env1),
    Bound = ordsets:union(Bound1, Bound2),
    Free = ordsets:union(Free1, Free2),
    Tree1 = rewrite(Tree, refac_syntax:case_expr(E1, Cs1)),
    {ann_bindings(Tree1, Env, Bound, Free), Bound, Free}.

vann_if_expr(Tree, Env) ->
    Cs = refac_syntax:if_expr_clauses(Tree),
    {Cs1, {Bound, Free}} = vann_clauses(Cs, Env),
    Tree1 = rewrite(Tree, refac_syntax:if_expr(Cs1)),
    {ann_bindings(Tree1, Env, Bound, Free), Bound, Free}.

vann_cond_expr(Tree, Env) ->
    {ann_bindings(Tree, Env, [], []), [],[]}.   %% TODO: NEED TO CHANGE THIS BACK.
    %% erlang:error({not_implemented, cond_expr}).

%% TODO: NEED TO CHECK THE SEMANTICS OF try-expression!!!.
vann_try_expr(Tree, Env) ->  
    Body = refac_syntax:try_expr_body(Tree),
    {Body1, {Bound1, Free1}} = vann_body(Body, Env),
    Cs = refac_syntax:try_expr_clauses(Tree),
    {Cs1, {Bound2, Free2}} = case Cs of 
				 [] -> {Cs, {[], []}};
				 _ -> vann_clauses(Cs, Env)
			     end,
    Handlers = refac_syntax:try_expr_handlers(Tree),
    {Handlers1, {Bound3, Free3}} = case Handlers of 
				       [] -> {Handlers, {[],[]}};
				       _  ->vann_clauses(Handlers,Env)
				   end,
    After = refac_syntax:try_expr_after(Tree),
    {After1, {Bound4, Free4}} = case After of 
				    [] -> {After, {[],[]}};
				    _  -> vann_body(After, Env)
				end,
    Tree1 = rewrite(Tree, refac_syntax:try_expr(Body1, Cs1, Handlers1, After1)),
    Bound = ordsets:union(ordsets:union(ordsets:union(Bound1, Bound2), Bound3),Bound4),
    Free  = ordsets:union(ordsets:union(ordsets:union(Free1, Free2), Free3),Free4),
    {ann_bindings(Tree1, Env, Bound, Free), Bound,Free}.    %% TODO: NEED TO CHANGE THIS BACK.
    %% erlang:error({not_implemented, try_expr}).

vann_receive_expr(Tree, Env) ->
    %% The timeout action is treated as an extra clause.
    %% Bindings in the expiry expression are local only.
    Cs = refac_syntax:receive_expr_clauses(Tree),
    Es = refac_syntax:receive_expr_action(Tree),
    case Es of 
	[] ->
	    {Cs1,{Bound, Free}} = vann_clauses(Cs, Env),
	    Tree1 = rewrite(Tree, refac_syntax:receive_expr(Cs1, none, [])),
	    {ann_bindings(Tree1, Env, Bound, Free), Bound, Free};
	_ ->
	    C = refac_syntax:clause([], Es),
	    {[C1 | Cs1], {Bound, Free1}} = vann_clauses([C | Cs],
							Env),
	    Es1 = refac_syntax:clause_body(C1),
	    {T1, _, Free2} = case
		       refac_syntax:receive_expr_timeout(Tree)
			 of
		       none -> {none, [], []};
		       T -> vann(T, Env)
		     end,
	    Free = ordsets:union(Free1, Free2),
	    Tree1 = rewrite(Tree,
			    refac_syntax:receive_expr(Cs1, T1, Es1)),
	    {ann_bindings(Tree1, Env, Bound, Free), Bound, Free}
    end.

vann_list_comp(Tree, Env) ->
    Es = refac_syntax:list_comp_body(Tree),
    {Es1, {Bound1, Free1}} = vann_list_comp_body(Es, Env),
    % begin of modification by Huiqing Li
    Env0 = ordsets:filter(fun ({V, _}) ->
				  case lists:keysearch(V, 1,
						       ordsets:to_list(Bound1))
				      of
				    {value, _} -> false;
				    false -> true
				  end
			  end,
			  Env),
    Env1 = ordsets:union(Env0, Bound1),
    % Original code:
    % Env1 = ordsets:union(Env, Bound1),
    % end of modification by Huiqing Li
    T = refac_syntax:list_comp_template(Tree),
    {T1, _, Free2} = vann(T, Env1),
    Free = ordsets:union(Free1,
			 ordsets:subtract(Free2, Bound1)),
    Bound = [],
    Tree1 = rewrite(Tree, refac_syntax:list_comp(T1, Es1)),
    {ann_bindings(Tree1, Env, Bound, Free), Bound, Free}.

vann_list_comp_body_join() ->
    fun (T, {Env, Bound, Free}) ->
	    {T1, Bound1, Free1} = case refac_syntax:type(T) of
				    generator -> vann_generator(T, Env);
				    _ ->
					%% Bindings in filters are not
					%% exported to the rest of the
					%% body.
					{T2, _, Free2} = vann(T, Env),
					{T2, [], Free2}
				  end,
	    % Begin of modification by Huiqing
	    Env0 = ordsets:filter(fun ({V, _}) ->
					  case lists:keysearch(V, 1,
							       ordsets:to_list(Bound1))
					      of
					    {value, _} -> false;
					    false -> true
					  end
				  end,
				  Env),
	    Env1 = ordsets:union(Env0, Bound1),
	    % Original code.
	    %Env1 = ordsets:union(Env, Bound1),
	    % End of modificaiton by Huiqing Li
	    % Begin of modification by Huiqing
	    Bound2 = ordsets:filter(fun ({V, _}) ->
					    case lists:keysearch(V, 1,
								 ordsets:to_list(Bound1))
						of
					      {value, _} -> false;
					      false -> true
					    end
				    end,
				    Bound),
	    {T1,
	     {Env1, ordsets:union(Bound2, Bound1),
	      ordsets:union(Free, ordsets:subtract(Free1, Bound))}}
    end.

            %End of modificaiton by Huiqing Li
	    % original code.
	    %%  {T1, {Env1, ordsets:union(Bound, Bound1),
	    %         ordsets:union(Free,
	    %                     ordsets:subtract(Free1, Bound))}}

vann_list_comp_body(Ts, Env) ->
    F = vann_list_comp_body_join(),
    {Ts1, {_, Bound, Free}} = lists:mapfoldl(F,
					     {Env, [], []}, Ts),
    {Ts1, {Bound, Free}}.

%% In list comprehension generators, the pattern variables are always
%% viewed as new occurrences, shadowing whatever is in the input
%% environment (thus, the pattern contains no variable uses, only
%% bindings). Bindings in the generator body are not exported.

vann_generator(Tree, Env) ->
    P = refac_syntax:generator_pattern(Tree),
    {P1, Bound, _} = vann_pattern(P, []),
    E = refac_syntax:generator_body(Tree),
    {E1, _, Free} = vann(E, Env),
    Tree1 = rewrite(Tree, refac_syntax:generator(P1, E1)),
    {ann_bindings(Tree1, Env, Bound, Free), Bound, Free}.

vann_block_expr(Tree, Env) ->
    Es = refac_syntax:block_expr_body(Tree),
    {Es1, {Bound, Free}} = vann_body(Es, Env),
    Tree1 = rewrite(Tree, refac_syntax:block_expr(Es1)),
    {ann_bindings(Tree1, Env, Bound, Free), Bound, Free}.

vann_body_join() ->
    fun (T, {Env, Bound, Free}) ->
	    {T1, Bound1, Free1} = vann(T, Env),
	    Env1 = ordsets:union(Env, Bound1),
	    {T1,
	     {Env1, ordsets:union(Bound, Bound1),
	      ordsets:union(Free, ordsets:subtract(Free1, Bound))}}
    end.

vann_body(Ts, Env) ->
    {Ts1, {_, Bound, Free}} =
	lists:mapfoldl(vann_body_join(), {Env, [], []}, Ts),
    {Ts1, {Bound, Free}}.

%% Macro names must be ignored even if they happen to be variables,
%% lexically speaking.

vann_macro(Tree, Env) ->
    {As, {Bound, Free}} = case
			    refac_syntax:macro_arguments(Tree)
			      of
			    none -> {none, {[], []}};
			    As1 -> vann_list(As1, Env)
			  end,
    N = refac_syntax:macro_name(Tree),
    N1 = ann_bindings(N, Env, [],[]),   %% TODO: CHECK THIS. How TO refer to the define locations of the macro?
    Tree1 = rewrite(Tree, refac_syntax:macro(N1, As)),
    {ann_bindings(Tree1, Env, Bound, Free), Bound, Free}.

vann_pattern(Tree, Env) ->
    case refac_syntax:type(Tree) of
      variable ->
	  V = refac_syntax:variable_name(Tree),
	  Pos = refac_syntax:get_pos(Tree),
	  case lists:keysearch(V, 1, Env) of
	    {value, {V, L}} ->
		%% apply occurrence
		Bound = [],
		Free = [{V, L}],
		Def =[L],
		{ann_bindings(Tree, Env, Bound, Free, Def), Bound, Free};
	    false ->
		%% binding occurrence
		Bound = [{V, Pos}],
		Free = [],
		Def = [Pos],
		{ann_bindings(Tree, Env, Bound, Free, Def), Bound, Free}
	  end;
      match_expr ->
	  %% Alias pattern
	  P = refac_syntax:match_expr_pattern(Tree),
	  {P1, Bound1, Free1} = vann_pattern(P, Env),
	  E = refac_syntax:match_expr_body(Tree),
	  {E1, Bound2, Free2} = vann_pattern(E, Env),
	  Bound = ordsets:union(Bound1, Bound2),
	  Free = ordsets:union(Free1, Free2),
	  Tree1 = rewrite(Tree, refac_syntax:match_expr(P1, E1)),
	  {ann_bindings(Tree1, Env, Bound, Free), Bound, Free};
      macro ->
	  %% The macro name must be ignored. The arguments are treated
	  %% as patterns.
	  {As, {Bound, Free}} = case
				  refac_syntax:macro_arguments(Tree)
				    of
				  none -> {none, {[], []}};
				  As1 -> vann_patterns(As1, Env)
				end,
	  N = refac_syntax:macro_name(Tree),
	  Tree1 = rewrite(Tree, refac_syntax:macro(N, As)),
	  {ann_bindings(Tree1, Env, Bound, Free), Bound, Free};
      _Type ->
	  F = vann_patterns_join(Env),
	  {Tree1, {Bound, Free}} = mapfold_subtrees(F, {[], []},
						    Tree),
	  {ann_bindings(Tree1, Env, Bound, Free), Bound, Free}
    end.

vann_fun_expr_pattern(Tree, Env) ->
    case refac_syntax:type(Tree) of
      variable ->
	  V = refac_syntax:variable_name(Tree),
	  Pos = refac_syntax:get_pos(Tree),
	  case lists:keysearch(V, 1, Env) of
	    {value, {V, _}} ->
		%% shandowing
		Bound = [{V, Pos}],
		Free = [],
		Def = [Pos],
		{ann_bindings(Tree, Env, Bound, Free, Def), Bound, Free};
	    false ->
		%% binding occurrence
		Bound = [{V, Pos}],
		Free = [],
		Def = [Pos],
		{ann_bindings(Tree, Env, Bound, Free, Def), Bound, Free}
	  end;
      match_expr ->
	  %% Alias pattern
	  P = refac_syntax:match_expr_pattern(Tree),
	  {P1, Bound1, Free1} = vann_pattern(P, Env),
	  E = refac_syntax:match_expr_body(Tree),
	  {E1, Bound2, Free2} = vann_pattern(E, Env),
	  Bound = ordsets:union(Bound1, Bound2),
	  Free = ordsets:union(Free1, Free2),
	  Tree1 = rewrite(Tree, refac_syntax:match_expr(P1, E1)),
	  {ann_bindings(Tree1, Env, Bound, Free), Bound, Free};
      macro ->
	  %% The macro name must be ignored. The arguments are treated
	  %% as patterns.
	  {As, {Bound, Free}} = case
				  refac_syntax:macro_arguments(Tree)
				    of
				  none -> {none, {[], []}};
				  As1 -> vann_patterns(As1, Env)
				end,
	  N = refac_syntax:macro_name(Tree),
	  Tree1 = rewrite(Tree, refac_syntax:macro(N, As)),
	  {ann_bindings(Tree1, Env, Bound, Free), Bound, Free};
      _Type ->
	  F = vann_patterns_join(Env),
	  {Tree1, {Bound, Free}} = mapfold_subtrees(F, {[], []},
						    Tree),
	  {ann_bindings(Tree1, Env, Bound, Free), Bound, Free}
    end.

vann_patterns_join(Env) ->
    fun (T, {Bound, Free}) ->
	    {T1, Bound1, Free1} = vann_pattern(T, Env),
	    {T1,
	     {ordsets:union(Bound, Bound1),
	      ordsets:union(Free, Free1)}}
    end.

vann_fun_expr_patterns_join(Env) ->
    fun (T, {Bound, Free}) ->
	    {T1, Bound1, Free1} = vann_fun_expr_pattern(T, Env),
	    {T1,
	     {ordsets:union(Bound, Bound1),
	      ordsets:union(Free, Free1)}}
    end.

vann_patterns(Ps, Env) ->
    lists:mapfoldl(vann_patterns_join(Env), {[], []}, Ps).

vann_fun_expr_patterns(Ps, Env) ->
    lists:mapfoldl(vann_fun_expr_patterns_join(Env),
		   {[], []}, Ps).

vann_define(D, Env) -> 
    Name = refac_syntax:attribute_name(D),
    Args = refac_syntax:attribute_arguments(D),    
    MacroHead = hd(Args),
    MacroBody = hd(tl(Args)),
    {{MacroHead1, Bound, _Free}, _Bs}= 
	case refac_syntax:type(MacroHead) of 
	    application -> Operator = refac_syntax:application_operator(MacroHead),
			   Arguments = refac_syntax:application_arguments(MacroHead),
			   {Operator1, Bs1, _Fs1} = vann_pattern(Operator, Env),
			   {Arguments1, {Bs2, _Fs2}} = vann_patterns(Arguments, Env), 
	                   %%Bs3= ordsets:union(Bs1, Bs2),
			   H = rewrite(MacroHead, refac_syntax:application(Operator1, Arguments1)),
			   %%{{ann_bindings(H, Env, Bs3, []), Bs3, []}, Bs1};
			   {{ann_bindings(H, Env, Bs2, []), Bs2, []}, Bs1};
	    _  -> {vann_pattern(MacroHead, Env),[]}
	end,
   Env1 = ordsets:union(Env, Bound),
   %%?wrangler_io("Env1:\n~p\n", [Env1]),
   %%?wrangler_io("MarcoBody:\n~p\n", [MacroBody]),
   {MacroBody1, _Bound1, _Free1} = vann(MacroBody, Env1),
   %%?wrangler_io("MacroBody1:\n~p\n", [MacroBody1]),
   MacroBody2 = adjust_define_body(MacroBody1, Env1),
   %%?wrangler_io("MacroBody2:\n~p\n", [MacroBody2]),
   D1 = rewrite(D, refac_syntax:attribute(Name, [MacroHead1, MacroBody2])),
   %%or  {ann_bindings(D1, Env, Bs, Free2), Bs, Free2}.  ?
   {ann_bindings(D1, Env, [], []), [], []}.   

adjust_define_body(Tree, Env) ->
    F = fun (Tree1) ->
		case refac_syntax:type(Tree1) of
		    variable ->
			V = refac_syntax:variable_name(Tree1),
			%% Pos = refac_syntax:get_pos(Tree1),
			case lists:keysearch(V, 1, Env) of
			    {value, {V, L}} ->
				%% apply occurrence
				Bound = [],
				Free = [{V, L}],
				Def =[L],
				ann_bindings(Tree1, Env, Bound, Free, Def);
			    false ->
				%% Free occurrence; 
				Bound = [],
				Free = [{V, ?DEFAULT_LOC}],
				Def = [?DEFAULT_LOC],
				ann_bindings(Tree1, Env, Bound, Free, Def)
			end;
		   _  -> Tree1
		end
	end,
   map(F, Tree).
	    
vann_clause(C, Env) ->
    {Ps, {Bound1, Free1}} =
	vann_patterns(refac_syntax:clause_patterns(C), Env),
    Env1 = ordsets:union(Env, Bound1),
    %% Guards cannot add bindings
    {G1, _, Free2} = case refac_syntax:clause_guard(C) of
		       none -> {none, [], []};
		       G -> vann(G, Env1)
		     end,
    {Es, {Bound2, Free3}} =
	vann_body(refac_syntax:clause_body(C), Env1),
    Bound = ordsets:union(Bound1, Bound2),
    Free = ordsets:union(Free1,
			 ordsets:subtract(ordsets:union(Free2, Free3), Bound1)),
    Tree1 = rewrite(C, refac_syntax:clause(Ps, G1, Es)),
    {ann_bindings(Tree1, Env, Bound, Free), Bound, Free}.

vann_fun_expr_clause(C, Env) ->
    {Ps, {Bound1, Free1}} =
	vann_fun_expr_patterns(refac_syntax:clause_patterns(C),
			       Env),
    Env0 = ordsets:filter(fun ({V, _}) ->
				  case lists:keysearch(V, 1,
						       ordsets:to_list(Bound1))
				      of
				    {value, _} -> false;
				    false -> true
				  end
			  end,
			  Env),
    Env1 = ordsets:union(Env0, Bound1),
    %% Guards cannot add bindings
    {G1, _, Free2} = case refac_syntax:clause_guard(C) of
		       none -> {none, [], []};
		       G -> vann(G, Env1)
		     end,
    {Es, {Bound2, Free3}} =
	vann_body(refac_syntax:clause_body(C), Env1),
    Bound = ordsets:union(Bound1, Bound2),
    Free = ordsets:union(Free1,
			 ordsets:subtract(ordsets:union(Free2, Free3), Bound1)),
    Tree1 = rewrite(C, refac_syntax:clause(Ps, G1, Es)),
    {ann_bindings(Tree1, Env, Bound, Free), Bound, Free}.

vann_clauses_join(Env) ->
    fun (C, {Bound, Free}) ->
	    {C1, Bound1, Free1} = vann_clause(C, Env),
	    Bd1 = ordsets:filter(fun ({V, _}) ->
					 case lists:keysearch(V, 1,
							      ordsets:to_list(Bound1))
					     of
					   {value, _} -> true;
					   false -> false
					 end
				 end,
				 Bound),
	    Bd2 = ordsets:filter(fun ({V, _}) ->
					 case lists:keysearch(V, 1,
							      ordsets:to_list(Bound))
					     of
					   {value, _} -> true;
					   false -> false
					 end
				 end,
				 Bound1),
	    {C1,
	     {ordsets:union(Bd1, Bd2), ordsets:union(Free, Free1)}};
	%%    originl code.
	%%       {C1, {ordsets:intersection(Bound, Bound1),
	%%             ordsets:union(Free, Free1)}};
	(C, false) ->
	    {C1, Bound, Free} = vann_clause(C, Env),
	    {C1, {Bound, Free}}
    end.

vann_fun_expr_clauses_join(Env) ->
    fun (C, {Bound, Free}) ->
	    {C1, Bound1, Free1} = vann_fun_expr_clause(C, Env),
	    {C1,
	     {ordsets:intersection(Bound, Bound1),
	      ordsets:union(Free, Free1)}};
	(C, false) ->
	    {C1, Bound, Free} = vann_fun_expr_clause(C, Env),
	    {C1, {Bound, Free}}
    end.

vann_clauses(Cs, Env) ->
    lists:mapfoldl(vann_clauses_join(Env), false, Cs).

vann_fun_expr_clauses(Cs, Env) ->
    lists:mapfoldl(vann_fun_expr_clauses_join(Env), false,
		   Cs).

ann_bindings(Tree, Env, Bound, Free) ->
    As0 = refac_syntax:get_ann(Tree),
    As1 = [{env, Env}, {bound, Bound}, {free, Free}
	   | delete_binding_anns(As0)],
    refac_syntax:set_ann(Tree, As1).

ann_bindings(Tree, Env, Bound, Free, Def) ->
    As0 = refac_syntax:get_ann(Tree),
    As1 = [{env, Env}, {bound, Bound}, {free, Free}, {def, Def}  
	   | delete_binding_anns(As0)],
    refac_syntax:set_ann(Tree, As1).

delete_binding_anns([{env, _} | As]) ->
    delete_binding_anns(As);
delete_binding_anns([{bound, _} | As]) ->
    delete_binding_anns(As);
delete_binding_anns([{free, _} | As]) ->
    delete_binding_anns(As);
delete_binding_anns([{def,_} | As]) ->
    delete_binding_anns(As);
delete_binding_anns([A | As]) ->
    [A | delete_binding_anns(As)];
delete_binding_anns([]) -> [].

%% =====================================================================
%% @spec is_fail_expr(Tree::syntaxTree()) -> bool()
%%
%% @doc Returns <code>true</code> if <code>Tree</code> represents an
%% expression which never terminates normally. Note that the reverse
%% does not apply. Currently, the detected cases are calls to
%% <code>exit/1</code>, <code>throw/1</code>,
%% <code>erlang:fault/1</code> and <code>erlang:fault/2</code>.
%%
%% @see erlang:exit/1
%% @see erlang:throw/1
%% @see erlang:fault/1
%% @see erlang:fault/2

is_fail_expr(E) ->
    case refac_syntax:type(E) of
      application ->
	  N = length(refac_syntax:application_arguments(E)),
	  F = refac_syntax:application_operator(E),
	  case catch {ok, analyze_function_name(F)} of
	    syntax_error -> false;
	    {ok, exit} when N == 1 -> true;
	    {ok, throw} when N == 1 -> true;
	    {ok, {erlang, exit}} when N == 1 -> true;
	    {ok, {erlang, throw}} when N == 1 -> true;
	    {ok, {erlang, error}} when N == 1 -> true;
	    {ok, {erlang, error}} when N == 2 -> true;
	    {ok, {erlang, fault}} when N == 1 -> true;
	    {ok, {erlang, fault}} when N == 2 -> true;
	    _ -> false
	  end;
      _ -> false
    end.


%% =====================================================================
%% @spec analyze_forms(Forms) -> [{Key, term()}]
%%
%%          Forms = syntaxTree() | [syntaxTree()]
%%          Key = attributes | errors | exports | functions | imports
%%                | module | records | rules | warnings
%%
%% @doc Analyzes a sequence of "program forms". The given
%% <code>Forms</code> may be a single syntax tree of type
%% <code>form_list</code>, or a list of "program form" syntax trees. The
%% returned value is a list of pairs <code>{Key, Info}</code>, where
%% each value of <code>Key</code> occurs at most once in the list; the
%% absence of a particular key indicates that there is no well-defined
%% value for that key.
%%
%% <p>Each entry in the resulting list contains the following
%% corresponding information about the program forms:
%% <dl>
%%     <dt><code>{attributes, Attributes}</code></dt>
%%       <dd><ul>
%% 	   <li><code>Attributes = [{atom(), term()}]</code></li>
%%       </ul>
%% 	 <code>Attributes</code> is a list of pairs representing the
%% 	 names and corresponding values of all so-called "wild"
%% 	 attributes (as e.g. "<code>-compile(...)</code>") occurring in
%% 	 <code>Forms</code> (cf. <code>analyze_wild_attribute/1</code>).
%% 	 We do not guarantee that each name occurs at most once in the
%% 	 list. The order of listing is not defined.</dd>
%%
%%     <dt><code>{errors, Errors}</code></dt>
%%       <dd><ul>
%% 	   <li><code>Errors = [term()]</code></li>
%%       </ul>
%% 	 <code>Errors</code> is the list of error descriptors of all
%% 	 <code>error_marker</code> nodes that occur in
%% 	 <code>Forms</code>. The order of listing is not defined.</dd>
%%
%%     <dt><code>{exports, Exports}</code></dt>
%%       <dd><ul>
%% 	    <li><code>Exports = [FunctionName]</code></li>
%% 	    <li><code>FunctionName = atom()
%%                    | {atom(), integer()}
%% 		      | {ModuleName, FunctionName}</code></li>
%% 	    <li><code>ModuleName = atom()</code></li>
%%       </ul>
%% 	 <code>Exports</code> is a list of representations of those
%% 	 function names that are listed by export declaration attributes
%% 	 in <code>Forms</code> (cf.
%% 	 <code>analyze_export_attribute/1</code>). We do not guarantee
%% 	 that each name occurs at most once in the list. The order of
%% 	 listing is not defined.</dd>
%%
%%     <dt><code>{functions, Functions}</code></dt>
%%       <dd><ul>
%% 	    <li><code>Functions = [{atom(), integer()}]</code></li>
%%       </ul>
%% 	 <code>Functions</code> is a list of the names of the functions
%% 	 that are defined in <code>Forms</code> (cf.
%% 	 <code>analyze_function/1</code>). We do not guarantee that each
%% 	 name occurs at most once in the list. The order of listing is
%% 	 not defined.</dd>
%%
%%     <dt><code>{imports, Imports}</code></dt>
%%       <dd><ul>
%% 	    <li><code>Imports = [{Module, Names}]</code></li>
%% 	    <li><code>Module = atom()</code></li>
%% 	    <li><code>Names = [FunctionName]</code></li>
%% 	    <li><code>FunctionName = atom()
%%                    | {atom(), integer()}
%% 		      | {ModuleName, FunctionName}</code></li>
%% 	    <li><code>ModuleName = atom()</code></li>
%%       </ul>
%% 	 <code>Imports</code> is a list of pairs representing those
%% 	 module names and corresponding function names that are listed
%% 	 by import declaration attributes in <code>Forms</code> (cf.
%% 	 <code>analyze_import_attribute/1</code>), where each
%% 	 <code>Module</code> occurs at most once in
%% 	 <code>Imports</code>. We do not guarantee that each name occurs
%% 	 at most once in the lists of function names. The order of
%% 	 listing is not defined.</dd>
%%
%%     <dt><code>{module, ModuleName}</code></dt>
%%       <dd><ul>
%% 	    <li><code>ModuleName = atom()</code></li>
%%       </ul>
%% 	 <code>ModuleName</code> is the name declared by a module
%% 	 attribute in <code>Forms</code>. If no module name is defined
%% 	 in <code>Forms</code>, the result will contain no entry for the
%% 	 <code>module</code> key. If multiple module name declarations
%% 	 should occur, all but the first will be ignored.</dd>
%%
%%     <dt><code>{records, Records}</code></dt>
%%       <dd><ul>
%% 	    <li><code>Records = [{atom(), Fields}]</code></li>
%% 	    <li><code>Fields = [{atom(), Default}]</code></li>
%% 	    <li><code>Default = none | syntaxTree()</code></li>
%%       </ul>
%% 	 <code>Records</code> is a list of pairs representing the names
%% 	 and corresponding field declarations of all record declaration
%% 	 attributes occurring in <code>Forms</code>. For fields declared
%% 	 without a default value, the corresponding value for
%% 	 <code>Default</code> is the atom <code>none</code> (cf.
%% 	 <code>analyze_record_attribute/1</code>). We do not guarantee
%% 	 that each record name occurs at most once in the list. The
%% 	 order of listing is not defined.</dd>
%%
%%     <dt><code>{rules, Rules}</code></dt>
%%       <dd><ul>
%% 	    <li><code>Rules = [{atom(), integer()}]</code></li>
%%       </ul>
%% 	 <code>Rules</code> is a list of the names of the rules that are
%% 	 defined in <code>Forms</code> (cf.
%% 	 <code>analyze_rule/1</code>). We do not guarantee that each
%% 	 name occurs at most once in the list. The order of listing is
%% 	 not defined.</dd>
%%
%%     <dt><code>{warnings, Warnings}</code></dt>
%%       <dd><ul>
%% 	    <li><code>Warnings = [term()]</code></li>
%%       </ul>
%% 	 <code>Warnings</code> is the list of error descriptors of all
%% 	 <code>warning_marker</code> nodes that occur in
%% 	 <code>Forms</code>. The order of listing is not defined.</dd>
%% </dl></p>
%%
%% <p>The evaluation throws <code>syntax_error</code> if an ill-formed
%% Erlang construct is encountered.</p>
%%
%% @see analyze_wild_attribute/1
%% @see analyze_export_attribute/1
%% @see analyze_import_attribute/1
%% @see analyze_record_attribute/1
%% @see analyze_function/1
%% @see analyze_rule/1
%% @see refac_syntax:error_marker_info/1
%% @see refac_syntax:warning_marker_info/1

analyze_forms(Forms) when is_list(Forms) ->
    finfo_to_list(lists:foldl(fun collect_form/2,
			      new_finfo(), Forms));
analyze_forms(Forms) ->
    analyze_forms(refac_syntax:form_list_elements(refac_syntax:flatten_form_list(Forms))).

collect_form(Node, Info) ->
    case analyze_form(Node) of
      {attribute, {Name, Data}} ->
	  collect_attribute(Name, Data, Info);
      {attribute, preprocessor} -> Info;
      {function, Name} -> finfo_add_function(Name, Info);
      {rule, Name} -> finfo_add_rule(Name, Info);
      {error_marker, Data} -> finfo_add_error(Data, Info);
      {warning_marker, Data} -> finfo_add_warning(Data, Info);
      _ -> Info
    end.

collect_attribute(module, M, Info) ->
    finfo_set_module(M, Info);
collect_attribute(export, L, Info) ->
    finfo_add_exports(L, Info);
collect_attribute(import, {M, L}, Info) ->
    finfo_add_imports(M, L, Info);
collect_attribute(import, M, Info) ->
    finfo_add_module_import(M, Info);
collect_attribute(file, _, Info) -> Info;
collect_attribute(record, {R, L}, Info) ->
    finfo_add_record(R, L, Info);
collect_attribute(_, {N, V}, Info) ->
    finfo_add_attribute(N, V, Info);
collect_attribute(_, _, Info) -> Info.  %% Added by Huiqing

    

%% Abstract datatype for collecting module information.

-record(forms,
	{module, exports, module_imports, imports, attributes,
	 records, errors, warnings, functions, rules}).

new_finfo() ->
    #forms{module = none, exports = [], module_imports = [],
	   imports = [], attributes = [], records = [],
	   errors = [], warnings = [], functions = [], rules = []}.

finfo_set_module(Name, Info) ->
    case Info#forms.module of
      none -> Info#forms{module = {value, Name}};
      {value, _} -> Info
    end.

finfo_add_exports(L, Info) ->
    Info#forms{exports = L ++ Info#forms.exports}.

finfo_add_module_import(M, Info) ->
    Info#forms{module_imports =
		   [M | Info#forms.module_imports]}.

finfo_add_imports(M, L, Info) ->
    Es = Info#forms.imports,
    case lists:keysearch(M, 1, Es) of
      {value, {_, L1}} ->
	  Es1 = lists:keyreplace(M, 1, Es, {M, L ++ L1}),
	  Info#forms{imports = Es1};
      false -> Info#forms{imports = [{M, L} | Es]}
    end.

finfo_add_attribute(Name, Val, Info) ->
    Info#forms{attributes =
		   [{Name, Val} | Info#forms.attributes]}.

finfo_add_record(R, L, Info) ->
    Info#forms{records = [{R, L} | Info#forms.records]}.

finfo_add_error(R, Info) ->
    Info#forms{errors = [R | Info#forms.errors]}.

finfo_add_warning(R, Info) ->
    Info#forms{warnings = [R | Info#forms.warnings]}.

finfo_add_function(F, Info) ->
    Info#forms{functions = [F | Info#forms.functions]}.

finfo_add_rule(F, Info) ->
    Info#forms{rules = [F | Info#forms.rules]}.

finfo_to_list(Info) ->
    [{Key, Value}
     || {Key, {value, Value}}
	    <- [{module, Info#forms.module},
		{exports, list_value(Info#forms.exports)},
		{imports, list_value(Info#forms.imports)},
		{module_imports, list_value(Info#forms.module_imports)},
		{attributes, list_value(Info#forms.attributes)},
		{records, list_value(Info#forms.records)},
		{errors, list_value(Info#forms.errors)},
		{warnings, list_value(Info#forms.warnings)},
		{functions, list_value(Info#forms.functions)},
		{rules, list_value(Info#forms.rules)}]].

list_value([]) -> none;
list_value(List) -> {value, List}.

%% =====================================================================
%% @spec analyze_form(Node::syntaxTree()) -> {atom(), term()} | atom()
%%
%% @doc Analyzes a "source code form" node. If <code>Node</code> is a
%% "form" type (cf. <code>refac_syntax:is_form/1</code>), the returned
%% value is a tuple <code>{Type, Info}</code> where <code>Type</code> is
%% the node type and <code>Info</code> depends on <code>Type</code>, as
%% follows:
%% <dl>
%%   <dt><code>{attribute, Info}</code></dt>
%%
%%      <dd>where <code>Info = analyze_attribute(Node)</code>.</dd>
%%
%%   <dt><code>{error_marker, Info}</code></dt>
%%
%% 	<dd>where <code>Info =
%% 	refac_syntax:error_marker_info(Node)</code>.</dd>
%%
%%   <dt><code>{function, Info}</code></dt>
%%
%% 	    <dd>where <code>Info = analyze_function(Node)</code>.</dd>
%%
%%   <dt><code>{rule, Info}</code></dt>
%%
%% 	    <dd>where <code>Info = analyze_rule(Node)</code>.</dd>
%%
%%   <dt><code>{warning_marker, Info}</code></dt>
%%
%% 	    <dd>where <code>Info =
%% 	    refac_syntax:warning_marker_info(Node)</code>.</dd>
%% </dl>
%% For other types of forms, only the node type is returned.
%%
%% <p>The evaluation throws <code>syntax_error</code> if
%% <code>Node</code> is not well-formed.</p>
%%
%% @see analyze_attribute/1
%% @see analyze_function/1
%% @see analyze_rule/1
%% @see refac_syntax:is_form/1
%% @see refac_syntax:error_marker_info/1
%% @see refac_syntax:warning_marker_info/1

analyze_form(Node) ->
    case refac_syntax:type(Node) of
      attribute -> {attribute, analyze_attribute(Node)};
      function -> {function, analyze_function(Node)};
      rule -> {rule, analyze_rule(Node)};
      error_marker ->
	  {error_marker, refac_syntax:error_marker_info(Node)};
      warning_marker ->
	  {warning_marker, refac_syntax:warning_marker_info(Node)};
      _ ->
	  case refac_syntax:is_form(Node) of
	    true -> refac_syntax:type(Node);
	    false -> 
		  throw(syntax_error)
	  end
    end.

%% =====================================================================
%% @spec analyze_attribute(Node::syntaxTree()) ->
%%           preprocessor | {atom(), atom()}
%%
%% @doc Analyzes an attribute node. If <code>Node</code> represents a
%% preprocessor directive, the atom <code>preprocessor</code> is
%% returned. Otherwise, if <code>Node</code> represents a module
%% attribute "<code>-<em>Name</em>...</code>", a tuple <code>{Name,
%% Info}</code> is returned, where <code>Info</code> depends on
%% <code>Name</code>, as follows:
%% <dl>
%%     <dt><code>{module, Info}</code></dt>
%%
%% 	    <dd>where <code>Info =
%% 	    analyze_module_attribute(Node)</code>.</dd>
%%
%%     <dt><code>{export, Info}</code></dt>
%%
%% 	    <dd>where <code>Info =
%% 	    analyze_export_attribute(Node)</code>.</dd>
%%
%%     <dt><code>{import, Info}</code></dt>
%%
%% 	    <dd>where <code>Info =
%% 	    analyze_import_attribute(Node)</code>.</dd>
%%
%%     <dt><code>{file, Info}</code></dt>
%%
%% 	    <dd>where <code>Info =
%% 	    analyze_file_attribute(Node)</code>.</dd>
%%
%%     <dt><code>{record, Info}</code></dt>
%%
%% 	    <dd>where <code>Info =
%% 	    analyze_record_attribute(Node)</code>.</dd>
%%
%%     <dt><code>{Name, Info}</code></dt>
%%
%% 	    <dd>where <code>{Name, Info} =
%% 	    analyze_wild_attribute(Node)</code>.</dd>
%% </dl>
%% The evaluation throws <code>syntax_error</code> if <code>Node</code>
%% does not represent a well-formed module attribute.
%%
%% @see analyze_module_attribute/1
%% @see analyze_export_attribute/1
%% @see analyze_import_attribute/1
%% @see analyze_file_attribute/1
%% @see analyze_record_attribute/1
%% @see analyze_wild_attribute/1

analyze_attribute(Node) ->
    Name = refac_syntax:attribute_name(Node),
    case refac_syntax:type(Name) of
      atom ->
	  case refac_syntax:atom_value(Name) of
	    define -> preprocessor;
	    undef -> preprocessor;
	    include -> preprocessor;
	    include_lib -> preprocessor;
	    ifdef -> preprocessor;
	    ifndef -> preprocessor;
	    else -> preprocessor;
	    endif -> preprocessor;
	    A -> {A, analyze_attribute(A, Node)}
	  end;
      _ -> throw(syntax_error)
    end.

analyze_attribute(module, Node) ->
    analyze_module_attribute(Node);
analyze_attribute(export, Node) ->
    analyze_export_attribute(Node);
analyze_attribute(import, Node) ->
    analyze_import_attribute(Node);
analyze_attribute(file, Node) ->
    analyze_file_attribute(Node);
analyze_attribute(record, Node) ->
    analyze_record_attribute(Node);
analyze_attribute(define, _Node) -> define;
analyze_attribute(spec, _Node) -> spec;
analyze_attribute(type, _Node) -> type;
analyze_attribute(_, Node) ->
    %% A "wild" attribute (such as e.g. a `compile' directive).
    analyze_wild_attribute(Node).

%% =====================================================================
%% @spec analyze_module_attribute(Node::syntaxTree()) -> atom()
%%
%% @doc Returns the module name declared by a module attribute.
%%
%% <p>The evaluation throws <code>syntax_error</code> if
%% <code>Node</code> does not represent a well-formed module
%% attribute.</p>
%%
%% @see analyze_attribute/1

analyze_module_attribute(Node) ->
    case refac_syntax:type(Node) of
      attribute ->
	  case refac_syntax:attribute_arguments(Node) of
	    [M] -> module_name_to_atom(M);
	    [M, L] ->
		M1 = module_name_to_atom(M),
		L1 = analyze_variable_list(L),
		{M1, L1};
	    _ -> throw(syntax_error)
	  end;
      _ -> throw(syntax_error)
    end.

analyze_variable_list(Node) ->
    case refac_syntax:is_proper_list(Node) of
      true ->
	  [refac_syntax:variable_name(V)
	   || V <- refac_syntax:list_elements(Node)];
      false -> throw(syntax_error)
    end.

%% =====================================================================
%% @spec analyze_export_attribute(Node::syntaxTree()) -> [FunctionName]
%%
%%          FunctionName = atom() | {atom(), integer()}
%%                       | {ModuleName, FunctionName}
%%          ModuleName = atom()
%%
%% @doc Returns the list of function names declared by an export
%% attribute. We do not guarantee that each name occurs at most once in
%% the list. The order of listing is not defined.
%%
%% <p>The evaluation throws <code>syntax_error</code> if
%% <code>Node</code> does not represent a well-formed export
%% attribute.</p>
%%
%% @see analyze_attribute/1

analyze_export_attribute(Node) ->
    case refac_syntax:type(Node) of
      attribute ->
	  case refac_syntax:attribute_arguments(Node) of
	    [L] -> analyze_function_name_list(L);
	    _ -> throw(syntax_error)
	  end;
      _ -> throw(syntax_error)
    end.

analyze_function_name_list(Node) ->
    case refac_syntax:is_proper_list(Node) of
      true ->
	  [analyze_function_name(F)
	   || F <- refac_syntax:list_elements(Node)];
      false -> throw(syntax_error)
    end.

%% =====================================================================
%% @spec analyze_function_name(Node::syntaxTree()) -> FunctionName
%%
%%          FunctionName = atom() | {atom(), integer()}
%%                       | {ModuleName, FunctionName}
%%          ModuleName = atom()
%%
%% @doc Returns the function name represented by a syntax tree. If
%% <code>Node</code> represents a function name, such as
%% "<code>foo/1</code>" or "<code>bloggs:fred/2</code>", a uniform
%% representation of that name is returned. Different nestings of arity
%% and module name qualifiers in the syntax tree does not affect the
%% result.
%%
%% <p>The evaluation throws <code>syntax_error</code> if
%% <code>Node</code> does not represent a well-formed function name.</p>

analyze_function_name(Node) ->
    case refac_syntax:type(Node) of
      atom -> refac_syntax:atom_value(Node);
      arity_qualifier ->
	  A = refac_syntax:arity_qualifier_argument(Node),
	  case refac_syntax:type(A) of
	    integer ->
		F = refac_syntax:arity_qualifier_body(Node),
		F1 = analyze_function_name(F),
		append_arity(refac_syntax:integer_value(A), F1);
	    _ -> throw(syntax_error)
	  end;
      module_qualifier ->
	  M = refac_syntax:module_qualifier_argument(Node),
	  case refac_syntax:type(M) of
	    atom ->
		F = refac_syntax:module_qualifier_body(Node),
		F1 = analyze_function_name(F),
		{refac_syntax:atom_value(M), F1};
	    _ -> throw(syntax_error)
	  end;
      _ -> throw(syntax_error)
    end.

append_arity(A, {Module, Name}) ->
    {Module, append_arity(A, Name)};
append_arity(A, Name) when is_atom(Name) -> {Name, A};
append_arity(A, A) -> A;
append_arity(_A, Name) ->
    Name.    % quietly drop extra arity in case of conflict

%% =====================================================================
%% @spec analyze_import_attribute(Node::syntaxTree()) ->
%%           {atom(), [FunctionName]} | atom()
%%
%%          FunctionName = atom() | {atom(), integer()}
%%                       | {ModuleName, FunctionName}
%%          ModuleName = atom()
%%
%% @doc Returns the module name and (if present) list of function names
%% declared by an import attribute. The returned value is an atom
%% <code>Module</code> or a pair <code>{Module, Names}</code>, where
%% <code>Names</code> is a list of function names declared as imported
%% from the module named by <code>Module</code>. We do not guarantee
%% that each name occurs at most once in <code>Names</code>. The order
%% of listing is not defined.
%%
%% <p>The evaluation throws <code>syntax_error</code> if
%% <code>Node</code> does not represent a well-formed import
%% attribute.</p>
%%
%% @see analyze_attribute/1

analyze_import_attribute(Node) ->
    case refac_syntax:type(Node) of
      attribute ->
	  case refac_syntax:attribute_arguments(Node) of
	    [M] -> module_name_to_atom(M);
	    [M, L] ->
		M1 = module_name_to_atom(M),
		L1 = analyze_function_name_list(L),
		{M1, L1};
	    _ -> throw(syntax_error)
	  end;
      _ -> throw(syntax_error)
    end.

%% =====================================================================
%% @spec analyze_wild_attribute(Node::syntaxTree()) -> {atom(), term()}
%%
%% @doc Returns the name and value of a "wild" attribute. The result is
%% the pair <code>{Name, Value}</code>, if <code>Node</code> represents
%% "<code>-Name(Value)</code>".
%%
%% <p>Note that no checking is done whether <code>Name</code> is a
%% reserved attribute name such as <code>module</code> or
%% <code>export</code>: it is assumed that the attribute is "wild".</p>
%%
%% <p>The evaluation throws <code>syntax_error</code> if
%% <code>Node</code> does not represent a well-formed wild
%% attribute.</p>
%%
%% @see analyze_attribute/1

analyze_wild_attribute(Node) ->
    case refac_syntax:type(Node) of
      attribute ->
	  N = refac_syntax:attribute_name(Node),
	  case refac_syntax:type(N) of
	    atom ->
		case refac_syntax:attribute_arguments(Node) of
		  [V] ->
		      {refac_syntax:atom_value(N), refac_syntax:concrete(V)};
		  _ -> throw(syntax_error)
		end;
	    _ -> throw(syntax_error)
	  end;
      _ -> throw(syntax_error)
    end.

%% =====================================================================
%% @spec analyze_record_attribute(Node::syntaxTree()) ->
%%           {atom(), Fields}
%%
%%          Fields = [{atom(), none | syntaxTree()}]
%%
%% @doc Returns the name and the list of fields of a record declaration
%% attribute. The result is a pair <code>{Name, Fields}</code>, if
%% <code>Node</code> represents "<code>-record(Name, {...}).</code>",
%% where <code>Fields</code> is a list of pairs <code>{Label,
%% Default}</code> for each field "<code>Label</code>" or "<code>Label =
%% <em>Default</em></code>" in the declaration, listed in left-to-right
%% order. If the field has no default-value declaration, the value for
%% <code>Default</code> will be the atom <code>none</code>. We do not
%% guarantee that each label occurs at most one in the list.
%%
%% <p>The evaluation throws <code>syntax_error</code> if
%% <code>Node</code> does not represent a well-formed record declaration
%% attribute.</p>
%%
%% @see analyze_attribute/1
%% @see analyze_record_field/1

analyze_record_attribute(Node) ->
    case refac_syntax:type(Node) of
      attribute ->
	  case refac_syntax:attribute_arguments(Node) of
	    [R, T] ->
		case refac_syntax:type(R) of
		  atom ->
		      Es = analyze_record_attribute_tuple(T),
		      {refac_syntax:atom_value(R), Es};
		  _ -> throw(syntax_error)
		end;
	    _ -> throw(syntax_error)
	  end;
      _ -> throw(syntax_error)
    end.

analyze_record_attribute_tuple(Node) ->
    case refac_syntax:type(Node) of
      tuple ->
	  [analyze_record_field(F)
	   || F <- refac_syntax:tuple_elements(Node)];
      _ -> throw(syntax_error)
    end.

%% =====================================================================
%% @spec analyze_record_expr(Node::syntaxTree()) ->
%%     {atom(), Info} | atom()
%%
%%    Info = {atom(), [{atom(), Value}]} | {atom(), atom()} | atom()
%%    Value = none | syntaxTree()
%%
%% @doc Returns the record name and field name/names of a record
%% expression. If <code>Node</code> has type <code>record_expr</code>,
%% <code>record_index_expr</code> or <code>record_access</code>, a pair
%% <code>{Type, Info}</code> is returned, otherwise an atom
%% <code>Type</code> is returned. <code>Type</code> is the node type of
%% <code>Node</code>, and <code>Info</code> depends on
%% <code>Type</code>, as follows:
%% <dl>
%%   <dt><code>record_expr</code>:</dt>
%%     <dd><code>{atom(), [{atom(), Value}]}</code></dd>
%%   <dt><code>record_access</code>:</dt>
%%     <dd><code>{atom(), atom()} | atom()</code></dd>
%%   <dt><code>record_index_expr</code>:</dt>
%%     <dd><code>{atom(), atom()}</code></dd>
%% </dl>
%%
%% <p>For a <code>record_expr</code> node, <code>Info</code> represents
%% the record name and the list of descriptors for the involved fields,
%% listed in the order they appear. (See
%% <code>analyze_record_field/1</code> for details on the field
%% descriptors). For a <code>record_access</code> node,
%% <code>Info</code> represents the record name and the field name (or
%% if the record name is not included, only the field name; this is
%% allowed only in Mnemosyne-query syntax). For a
%% <code>record_index_expr</code> node, <code>Info</code> represents the
%% record name and the name field name.</p>
%%
%% <p>The evaluation throws <code>syntax_error</code> if
%% <code>Node</code> represents a record expression that is not
%% well-formed.</p>
%%
%% @see analyze_record_attribute/1
%% @see analyze_record_field/1

analyze_record_expr(Node) ->
    case refac_syntax:type(Node) of
      record_expr ->
	  A = refac_syntax:record_expr_type(Node),
	  case refac_syntax:type(A) of
	    atom ->
		Fs = [analyze_record_field(F)
		      || F <- refac_syntax:record_expr_fields(Node)],
		{record_expr, {refac_syntax:atom_value(A), Fs}};
	    _ -> throw(syntax_error)
	  end;
      record_access ->
	  F = refac_syntax:record_access_field(Node),
	  case refac_syntax:type(F) of
	    atom ->
		case refac_syntax:record_access_type(Node) of
		  none -> {record_access, refac_syntax:atom_value(F)};
		  A ->
		      case refac_syntax:type(A) of
			atom ->
			    {record_access,
			     {refac_syntax:atom_value(A),
			      refac_syntax:atom_value(F)}};
			_ -> throw(syntax_error)
		      end
		end;
	    _ -> throw(syntax_error)
	  end;
      record_index_expr ->
	  F = refac_syntax:record_index_expr_field(Node),
	  case refac_syntax:type(F) of
	    atom ->
		A = refac_syntax:record_index_expr_type(Node),
		case refac_syntax:type(A) of
		  atom ->
		      {record_index_expr,
		       {refac_syntax:atom_value(A), refac_syntax:atom_value(F)}};
		  _ -> throw(syntax_error)
		end;
	    _ -> throw(syntax_error)
	  end;
      Type -> Type
    end.

%% =====================================================================
%% @spec analyze_record_field(Node::syntaxTree()) -> {atom(), Value}
%%
%%          Value = none | syntaxTree()
%%
%% @doc Returns the label and value-expression of a record field
%% specifier. The result is a pair <code>{Label, Value}</code>, if
%% <code>Node</code> represents "<code>Label = <em>Value</em></code>" or
%% "<code>Label</code>", where in the first case, <code>Value</code> is
%% a syntax tree, and in the second case <code>Value</code> is
%% <code>none</code>.
%%
%% <p>The evaluation throws <code>syntax_error</code> if
%% <code>Node</code> does not represent a well-formed record field
%% specifier.</p>
%%
%% @see analyze_record_attribute/1
%% @see analyze_record_expr/1

analyze_record_field(Node) ->
    case refac_syntax:type(Node) of
      record_field ->
	  A = refac_syntax:record_field_name(Node),
	  case refac_syntax:type(A) of
	    atom ->
		T = refac_syntax:record_field_value(Node),
		{refac_syntax:atom_value(A), T};
	    _ -> throw(syntax_error)
	  end;
      _ -> throw(syntax_error)
    end.

%% =====================================================================
%% @spec analyze_file_attribute(Node::syntaxTree()) ->
%%           {string(), integer()}
%%
%% @doc Returns the file name and line number of a <code>file</code>
%% attribute. The result is the pair <code>{File, Line}</code> if
%% <code>Node</code> represents "<code>-file(File, Line).</code>".
%%
%% <p>The evaluation throws <code>syntax_error</code> if
%% <code>Node</code> does not represent a well-formed <code>file</code>
%% attribute.</p>
%%
%% @see analyze_attribute/1

analyze_file_attribute(Node) ->
    case refac_syntax:type(Node) of
      attribute ->
	  case refac_syntax:attribute_arguments(Node) of
	    [F, N] ->
		case (refac_syntax:type(F) == string) and
		       (refac_syntax:type(N) == integer)
		    of
		  true ->
		      {refac_syntax:string_value(F),
		       refac_syntax:integer_value(N)};
		  false -> throw(syntax_error)
		end;
	    _ -> throw(syntax_error)
	  end;
      _ -> throw(syntax_error)
    end.

%% =====================================================================
%% @spec analyze_function(Node::syntaxTree()) -> {atom(), integer()}
%%
%% @doc Returns the name and arity of a function definition. The result
%% is a pair <code>{Name, A}</code> if <code>Node</code> represents a
%% function definition "<code>Name(<em>P_1</em>, ..., <em>P_A</em>) ->
%% ...</code>".
%%
%% <p>The evaluation throws <code>syntax_error</code> if
%% <code>Node</code> does not represent a well-formed function
%% definition.</p>
%%
%% @see analyze_rule/1

analyze_function(Node) ->
    case refac_syntax:type(Node) of
      function ->
	  N = refac_syntax:function_name(Node),
	  case refac_syntax:type(N) of
	    atom ->
		{refac_syntax:atom_value(N),
		 refac_syntax:function_arity(Node)};
	    _ -> throw(syntax_error)
	  end;
      _ -> throw(syntax_error)
    end.

%% =====================================================================
%% @spec analyze_rule(Node::syntaxTree()) -> {atom(), integer()}
%%
%% @doc Returns the name and arity of a Mnemosyne rule. The result is a
%% pair <code>{Name, A}</code> if <code>Node</code> represents a rule
%% "<code>Name(<em>P_1</em>, ..., <em>P_A</em>) :- ...</code>".
%%
%% <p>The evaluation throws <code>syntax_error</code> if
%% <code>Node</code> does not represent a well-formed Mnemosyne
%% rule.</p>
%%
%% @see analyze_function/1

analyze_rule(Node) ->
    case refac_syntax:type(Node) of
      rule ->
	  N = refac_syntax:rule_name(Node),
	  case refac_syntax:type(N) of
	    atom ->
		{refac_syntax:atom_value(N), refac_syntax:rule_arity(Node)};
	    _ -> throw(syntax_error)
	  end;
      _ -> throw(syntax_error)
    end.

%% =====================================================================
%% @spec analyze_implicit_fun(Node::syntaxTree()) -> FunctionName
%%
%%          FunctionName = atom() | {atom(), integer()}
%%                       | {ModuleName, FunctionName}
%%          ModuleName = atom()
%%
%% @doc Returns the name of an implicit fun expression "<code>fun
%% <em>F</em></code>". The result is a representation of the function
%% name <code>F</code>. (Cf. <code>analyze_function_name/1</code>.)
%%
%% <p>The evaluation throws <code>syntax_error</code> if
%% <code>Node</code> does not represent a well-formed implicit fun.</p>
%%
%% @see analyze_function_name/1

analyze_implicit_fun(Node) ->
    case refac_syntax:type(Node) of
      implicit_fun ->
	  analyze_function_name(refac_syntax:implicit_fun_name(Node));
      _ -> throw(syntax_error)
    end.

%% =====================================================================
%% @spec analyze_application(Node::syntaxTree()) -> FunctionName | Arity
%%
%%          FunctionName = {atom(), Arity}
%%                       | {ModuleName, FunctionName}
%%          Arity = integer()
%%          ModuleName = atom()
%%
%% @doc Returns the name of a called function. The result is a
%% representation of the name of the applied function <code>F/A</code>,
%% if <code>Node</code> represents a function application
%% "<code><em>F</em>(<em>X_1</em>, ..., <em>X_A</em>)</code>". If the
%% function is not explicitly named (i.e., <code>F</code> is given by
%% some expression), only the arity <code>A</code> is returned.
%%
%% <p>The evaluation throws <code>syntax_error</code> if
%% <code>Node</code> does not represent a well-formed application
%% expression.</p>
%%
%% @see analyze_function_name/1

analyze_application(Node) ->
    case refac_syntax:type(Node) of
      application ->
	  A = length(refac_syntax:application_arguments(Node)),
	  F = refac_syntax:application_operator(Node),
	  case catch {ok, analyze_function_name(F)} of
	    syntax_error -> A;
	    {ok, N} -> append_arity(A, N);
	    _ -> throw(syntax_error)
	  end;
      _ -> throw(syntax_error)
    end.

%% =====================================================================
%% @spec function_name_expansions(Names::[Name]) -> [{ShortName, Name}]
%%
%%          Name = ShortName | {atom(), Name}
%%          ShortName = atom() | {atom(), integer()}
%%
%% @doc Creates a mapping from corresponding short names to full
%% function names. Names are represented by nested tuples of atoms and
%% integers (cf. <code>analyze_function_name/1</code>). The result is a
%% list containing a pair <code>{ShortName, Name}</code> for each
%% element <code>Name</code> in the given list, where the corresponding
%% <code>ShortName</code> is the rightmost-innermost part of
%% <code>Name</code>. The list thus represents a finite mapping from
%% unqualified names to the corresponding qualified names.
%%
%% <p>Note: the resulting list can contain more than one tuple
%% <code>{ShortName, Name}</code> for the same <code>ShortName</code>,
%% possibly with different values for <code>Name</code>, depending on
%% the given list.</p>
%%
%% @see analyze_function_name/1

function_name_expansions(Fs) ->
    function_name_expansions(Fs, []).

function_name_expansions([F | Fs], Ack) ->
    function_name_expansions(Fs,
			     function_name_expansions(F, F, Ack));
function_name_expansions([], Ack) -> Ack.

function_name_expansions({A, N}, Name, Ack)
    when is_integer(N) ->
    [{{A, N}, Name} | Ack];
function_name_expansions({_, N}, Name, Ack) ->
    function_name_expansions(N, Name, Ack);
function_name_expansions(A, Name, Ack) ->
    [{A, Name} | Ack].

%% =====================================================================
%% @spec strip_comments(Tree::syntaxTree()) -> syntaxTree()
%%
%% @doc Removes all comments from all nodes of a syntax tree. All other
%% attributes (such as position information) remain unchanged.

strip_comments(Tree) ->
    map(fun (T) -> refac_syntax:remove_comments(T) end, Tree).

%% =====================================================================
%% @spec to_comment(Tree) -> syntaxTree()
%% @equiv to_comment(Tree, "% ")

to_comment(Tree) -> to_comment(Tree, "% ").

%% =====================================================================
%% @spec to_comment(Tree::syntaxTree(), Prefix::string()) ->
%%           syntaxTree()
%%
%% @doc Equivalent to <code>to_comment(Tree, Prefix, F)</code> for a
%% default formatting function <code>F</code>. The default
%% <code>F</code> simply calls <code>erl_prettypr:format/1</code>.
%%
%% @see to_comment/3
%% @see erl_prettypr:format/1

to_comment(Tree, Prefix) ->
    F = fun (T) -> erl_prettypr:format(T) end,
    to_comment(Tree, Prefix, F).

%% =====================================================================
%% @spec to_comment(Tree::syntaxTree(), Prefix::string(), Printer) ->
%%           syntaxTree()
%%
%%          Printer = (syntaxTree()) -> string()
%%
%% @doc Transforms a syntax tree into an abstract comment. The lines of
%% the comment contain the text for <code>Node</code>, as produced by
%% the given <code>Printer</code> function. Each line of the comment is
%% prefixed by the string <code>Prefix</code> (this does not include the
%% initial "<code>%</code>" character of the comment line).
%%
%% <p>For example, the result of
%% <code>to_comment(refac_syntax:abstract([a,b,c]))</code> represents
%% <pre>
%%         %% [a,b,c]</pre>
%% (cf. <code>to_comment/1</code>).</p>
%%
%% <p>Note: the text returned by the formatting function will be split
%% automatically into separate comment lines at each line break. No
%% extra work is needed.</p>
%%
%% @see to_comment/1
%% @see to_comment/2

to_comment(Tree, Prefix, F) ->
    refac_syntax:comment(split_lines(F(Tree), Prefix)).

%% =====================================================================
%% @spec limit(Tree, Depth) -> syntaxTree()
%%
%% @doc Equivalent to <code>limit(Tree, Depth, Text)</code> using the
%% text <code>"..."</code> as default replacement.
%%
%% @see limit/3
%% @see refac_syntax:text/1

limit(Tree, Depth) ->
    limit(Tree, Depth, refac_syntax:text("...")).

%% =====================================================================
%% @spec limit(Tree::syntaxTree(), Depth::integer(),
%%             Node::syntaxTree()) -> syntaxTree()
%%
%% @doc Limits a syntax tree to a specified depth. Replaces all non-leaf
%% subtrees in <code>Tree</code> at the given <code>Depth</code> by
%% <code>Node</code>. If <code>Depth</code> is negative, the result is
%% always <code>Node</code>, even if <code>Tree</code> has no subtrees.
%%
%% <p>When a group of subtrees (as e.g., the argument list of an
%% <code>application</code> node) is at the specified depth, and there
%% are two or more subtrees in the group, these will be collectively
%% replaced by <code>Node</code> even if they are leaf nodes. Groups of
%% subtrees that are above the specified depth will be limited in size,
%% as if each subsequent tree in the group were one level deeper than
%% the previous. E.g., if <code>Tree</code> represents a list of
%% integers "<code>[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]</code>", the result
%% of <code>limit(Tree, 5)</code> will represent <code>[1, 2, 3, 4,
%% ...]</code>.</p>
%%
%% <p>The resulting syntax tree is typically only useful for
%% pretty-printing or similar visual formatting.</p>
%%
%% @see limit/2

limit(_Tree, Depth, Node) when Depth < 0 -> Node;
limit(Tree, Depth, Node) -> limit_1(Tree, Depth, Node).

limit_1(Tree, Depth, Node) ->
    %% Depth is nonnegative here.
    case refac_syntax:subtrees(Tree) of
      [] ->
	  if Depth > 0 -> Tree;
	     true ->
		 case is_simple_leaf(Tree) of
		   true -> Tree;
		   false -> Node
		 end
	  end;
      Gs ->
	  if Depth > 1 ->
		 Gs1 = [[limit_1(T, Depth - 1, Node)
			 || T <- limit_list(G, Depth, Node)]
			|| G <- Gs],
		 rewrite(Tree,
			 refac_syntax:make_tree(refac_syntax:type(Tree), Gs1));
	     Depth == 0 ->
		 %% Depth is zero, and this is not a leaf node
		 %% so we always replace it.
		 Node;
	     true ->
		 %% Depth is 1, so all subtrees are to be cut.
		 %% This is done groupwise.
		 Gs1 = [cut_group(G, Node) || G <- Gs],
		 rewrite(Tree,
			 refac_syntax:make_tree(refac_syntax:type(Tree), Gs1))
	  end
    end.

cut_group([], _Node) -> [];
cut_group([T], Node) ->
    %% Only if the group contains a single subtree do we try to
    %% preserve it if suitable.
    [limit_1(T, 0, Node)];
cut_group(_Ts, Node) -> [Node].

is_simple_leaf(Tree) ->
    case refac_syntax:type(Tree) of
      atom -> true;
      char -> true;
      float -> true;
      integer -> true;
      nil -> true;
      operator -> true;
      tuple -> true;
      underscore -> true;
      variable -> true;
      _ -> false
    end.

%% If list has more than N elements, take the N - 1 first and
%% append Node; otherwise return list as is.

limit_list(Ts, N, Node) ->
    if length(Ts) > N -> limit_list_1(Ts, N - 1, Node);
       true -> Ts
    end.

limit_list_1([T | Ts], N, Node) ->
    if N > 0 -> [T | limit_list_1(Ts, N - 1, Node)];
       true -> [Node]
    end;
limit_list_1([], _N, _Node) -> [].

%% =====================================================================
%% Utility functions

rewrite(Tree, Tree1) ->
    refac_syntax:copy_attrs(Tree, Tree1).

module_name_to_atom(M) ->
    case refac_syntax:type(M) of
      atom -> refac_syntax:atom_value(M);
      qualified_name ->
	  list_to_atom(packages:concat([refac_syntax:atom_value(A)
					|| A
					       <- refac_syntax:qualified_name_segments(M)]));
      _ -> throw(syntax_error)
    end.

%% This splits lines at line terminators and expands tab characters to
%% spaces. The width of a tab is assumed to be 8.

% split_lines(Cs) ->
%     split_lines(Cs, "").

split_lines(Cs, Prefix) -> split_lines(Cs, Prefix, 0).

split_lines(Cs, Prefix, N) ->
    lists:reverse(split_lines(Cs, N, [], [], Prefix)).

split_lines([$\r, $\n | Cs], _N, Cs1, Ls, Prefix) ->
    split_lines_1(Cs, Cs1, Ls, Prefix);
split_lines([$\r | Cs], _N, Cs1, Ls, Prefix) ->
    split_lines_1(Cs, Cs1, Ls, Prefix);
split_lines([$\n | Cs], _N, Cs1, Ls, Prefix) ->
    split_lines_1(Cs, Cs1, Ls, Prefix);
split_lines([$\t | Cs], N, Cs1, Ls, Prefix) ->
    split_lines(Cs, 0, push(8 - N rem 8, $\s, Cs1), Ls,
		Prefix);
split_lines([C | Cs], N, Cs1, Ls, Prefix) ->
    split_lines(Cs, N + 1, [C | Cs1], Ls, Prefix);
split_lines([], _, [], Ls, _) -> Ls;
split_lines([], _N, Cs, Ls, Prefix) ->
    [Prefix ++ lists:reverse(Cs) | Ls].

split_lines_1(Cs, Cs1, Ls, Prefix) ->
    split_lines(Cs, 0, [],
		[Prefix ++ lists:reverse(Cs1) | Ls], Prefix).

push(N, C, Cs) when N > 0 -> push(N - 1, C, [C | Cs]);
push(0, _, Cs) -> Cs.
