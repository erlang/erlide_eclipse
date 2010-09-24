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


-module(interface_api).


-export([pos_to_fun_name/2,pos_to_fun_def/2,pos_to_var_name/2,pos_to_var/2,pos_to_expr/3,
	 pos_to_expr_list/3,pos_to_expr_or_pat_list/3,expr_to_fun/2]).

-include("../include/wrangler.hrl").
%% ==========================================================================
%% @spec pos_to_fun_name(Node::syntaxTree(), Pos::{integer(), integer()}) ->
%%                        {ok, {Mod, Fun, Arity, OccurPos, DefPos}} | {error, string()}
%%    Mod = atom()
%%    Fun = atom()
%%    Arity = integer()
%%    OccurPos = {integer(), integer()}
%%    DefPos = {integer(), integer()}
%% @doc Get information about the function name which occurs at the specified
%% position in the code. If successful, the returned information contains: 
%% the module in which the function is defined, the function name, the 
%% function's arity, the occurrence position (same as Pos), and the defining 
%% position of this function.
%%
%% @see pos_to_var_name/2
%% @see pos_to_expr/3
%% @see pos_to_fun_def/2.

%%-spec (pos_to_fun_name(Node::syntaxTree(), Pos::pos()) ->
%%	      {ok, {atom(), atom(), integer(), pos(), pos()}} | {error, string()}).
pos_to_fun_name(Node, Pos) ->
    case
      ast_traverse_api:once_tdTU(fun pos_to_fun_name_1/2, Node, Pos)
	of
      {_, false} -> {error, "You have not selected a function name,"
			    "or the function/attribute containing the "
			    "function name selected does not parse!"};
      {R, true} -> {ok, R}
    end.

pos_to_fun_name_1(Node, Pos = {Ln, Col}) ->
    As = refac_syntax:get_ann(Node),
    case lists:keysearch(fun_def, 1, As) of
	{value, {fun_def, {Mod, Fun, Arity, {Ln, Col1}, DefPos}}} when is_atom(Fun)->
	    case (Col1 =< Col) and (Col =< Col1 + length(atom_to_list(Fun)) - 1) of
		true -> {{Mod, Fun, Arity, Pos, DefPos}, true};
		false -> {[], false}
	    end;
	_ -> {[], false}
    end.


%%============================================================================
%% @spec pos_to_fun_def(Node::syntaxTree(), Pos::{integer(), integer()}) 
%%                     -> {ok, syntaxTree()} | {error, string()}
%% @doc Get the AST representation of the function definition in which the 
%% location specified by Pos falls.
%%               
%% @see pos_to_fun_name/2.
%%-spec(pos_to_fun_def(Node::syntaxTree(), Pos::pos()) 
%%      -> {ok, syntaxTree()} | {error, string()}).
pos_to_fun_def(Node, Pos) ->
    case
      ast_traverse_api:once_tdTU(fun pos_to_fun_def_1/2, Node, Pos)
	of
      {_, false} -> {error, "You have not selected a function definition, "
			    "or the function definition selected does not parse."};
      {R, true} -> {ok, R}
    end.

pos_to_fun_def_1(Node, Pos) ->
    case refac_syntax:type(Node) of
      function ->
	  {S, E} = refac_misc:get_start_end_loc(Node),
	  if (S =< Pos) and (Pos =< E) ->
		 {Node, true};
	     true -> {[], false}
	  end;
      _ -> {[], false}
    end.




%% =====================================================================
%% @spec pos_to_var_name(Node::syntaxTree(), Pos::{integer(), integer()})->
%%                      {ok, {VarName,DefPos, Category}} | {error, string()}
%%
%%      VarName = atom()
%%      DefPos = [{integer(), integer()}]
%%      Category = expression | pattern | macro_name
%%
%% @doc Get the variable name that occurs at the position specified by Pos.
%% Apart from the variable name, this function all returns other information 
%% including its defining position and its syntax category information.
%%
%% @see pos_to_fun_name/2
%% @see pos_to_fun_def/2
%% @see pos_to_expr/3

%%TODO: This function returns more than what the function name indicates, try to refactor it.

%%-type(category()::expression|pattern|macro_name|application_op|operator).
%%-spec(pos_to_var_name(Node::syntaxTree(), Pos::pos())->
%%	     {ok, {atom(), [pos()], category()}} | {error, string()}).
pos_to_var_name(Node, UsePos) ->
    case
      ast_traverse_api:once_tdTU(fun pos_to_var_name_1/2, Node, UsePos)
	of
      {_, false} -> {error, "You have not selected a variable name, "
			    "or the function containing the variable does not parse."};
      {R, true} -> {ok, R}
    end.

pos_to_var_name_1(Node, _Pos = {Ln, Col}) ->
    case refac_syntax:type(Node) of
      variable ->
	  {Ln1, Col1} = refac_syntax:get_pos(Node),
	  case (Ln == Ln1) and (Col1 =< Col) and
		 (Col =< Col1 + length(atom_to_list(refac_syntax:variable_name(Node))) - 1)
	      of
	    true ->
		case lists:keysearch(def, 1, refac_syntax:get_ann(Node)) of
		  {value, {def, DefinePos}} ->
			{value, {category, C}} = lists:keysearch(category, 1, refac_syntax:get_ann(Node)),
			C1 =case C of 
				{macro_name, _, _} -> macro_name;
				_ -> C
			    end,
			{{refac_syntax:variable_name(Node), DefinePos, C1}, true};
		    false ->
			{value, {category, C}} = lists:keysearch(category, 1, refac_syntax:get_ann(Node)),
			C1 =case C of 
				{macro_name, _, _} -> macro_name;
				_ -> C
			    end,
			{{refac_syntax:variable_name(Node), [?DEFAULT_LOC], C1}, true}
		end;
	      false -> {[], false}
	  end;
	_ -> {[], false}
    end.



-spec(pos_to_var(Node::syntaxTree(), Pos::pos())->
	     {ok, syntaxTree()} | {error, string()}).
pos_to_var(Node, Pos) ->
    case
	ast_traverse_api:once_tdTU(fun pos_to_var_1/2, Node, Pos)
    of
	{_, false} -> {error, "You have not selected a variable, "
		       "or the function containing the variable does not parse."};
	{R, true} -> {ok, R}
    end.

pos_to_var_1(Node, _Pos = {Ln, Col}) ->
    case refac_syntax:type(Node) of
	variable ->
	    {Ln1, Col1} = refac_syntax:get_pos(Node),
	    case (Ln == Ln1) and (Col1 =< Col) and
		(Col =< Col1 + length(atom_to_list(refac_syntax:variable_name(Node))) - 1)
	    of
		true ->
		    {value, {category, C}} = lists:keysearch(category, 1, refac_syntax:get_ann(Node)),
		    case C of 
			{macro_name, _, _} -> {[], false};
			_ -> {Node, true}
		    end;
		false -> {[], false}
	    end;
	_ -> {[], false}
    end.

%% =====================================================================
%% @spec pos_to_expr(Tree::syntaxTree(), Start::Pos, End::Pos) ->
%%                  {ok, syntaxTree()} | {error, string()}
%%
%%       Pos={integer(), integer()}
%% @doc Get the largest, left-most expression enclosed by the start and end locations.
%%
%% @see pos_to_fun_name/2
%% @see pos_to_fun_def/2
%% @see pos_to_var_name/2
%%-spec(pos_to_expr(Tree::syntaxTree(), Start::pos(), End::pos()) ->{ok, syntaxTree()} | {error, string()}).
pos_to_expr(Tree, Start, End) ->
    Es = lists:flatten(pos_to_expr_1(Tree, Start, End)),
    case Es of
      [] -> {error, "You have not selected an expression, "
		    "or the function containing the expression selected does not parse."};
      _ -> {ok, hd(Es)}
    end.

pos_to_expr_1(Tree, Start, End) ->
    {S, E} = refac_misc:get_start_end_loc(Tree),
    if (S >= Start) and (E =< End) ->
	   case refac_misc:is_expr(Tree) of
	     true ->
		   [Tree];
	     _ ->
		 Ts = refac_syntax:subtrees(Tree),
		 R0 = [[pos_to_expr_1(T, Start, End) || T <- G] || G <- Ts],
		 lists:append(R0)
	   end;
       (S > End) or (E < Start) -> [];
       (S < Start) or (E > End) ->
	   Ts = refac_syntax:subtrees(Tree),
	   R0 = [[pos_to_expr_1(T, Start, End) || T <- G] || G <- Ts],
	    lists:append(R0);
       true -> []
    end.

%% =====================================================================
%% get the list expressions enclosed by start and end locations.
%%-spec(pos_to_expr_list(Tree::syntaxTree(), Start::pos(), End::pos()) ->
%%	     [syntaxTree()]).
pos_to_expr_list(AnnAST, Start, End) ->
    Es = pos_to_expr_list_1(AnnAST, Start, End, fun refac_misc:is_expr_or_match/1),
    get_expr_list(Es).

pos_to_expr_list_1(Tree, Start, End, F) ->
    {S, E} = refac_misc:get_start_end_loc(Tree),
    if (S >= Start) and (E =< End) ->
	    case F(Tree) of
	       true ->
		   [Tree];
	       _ ->
		   Ts = refac_syntax:subtrees(Tree),
		   [[lists:append(pos_to_expr_list_1(T, Start, End, F)) || T <- G]
		  || G <- Ts]
	   end;
       (S > End) or (E < Start) -> [];
       (S < Start) or (E > End) ->
	    Ts = refac_syntax:subtrees(Tree),
	   [[lists:append(pos_to_expr_list_1(T, Start, End, F)) || T <- G]
	    || G <- Ts]
    end.
get_expr_list(Es) ->
    case [append(E)|| E <- Es, lists:flatten(E) /= []] of
	[] ->
	  [];
      [H|_T] ->
	    get_expr_list_1(H)
    end.

append(Es) ->
    case lists:all(fun(E)->
			 is_list(E)
		   end, Es) of
	true->
	    lists:append(Es);
	false ->
	    [E || E <- Es, not is_list(E)]
    end.

get_expr_list_1(L) ->
    case lists:any(fun(F)-> not is_list(F) end, L) of
      true ->
	  [E || E <- L, not is_list(E)];
      false ->
	  get_expr_list(L)
    end.


pos_to_expr_or_pat_list(AnnAST, Start, End) ->
    F = fun
	  (E) -> refac_misc:is_expr_or_match(E) orelse refac_misc:is_pattern(E)
	end,
    Es = pos_to_expr_list_1(AnnAST, Start, End, F),
    get_expr_list(Es).

%% ===========================================================================
%% @spec expr_to_fun(Tree::syntaxTree(), Exp::syntaxTree())->
%%                   {ok, syntaxTree()} | {error, none}
%%
%% @doc Return the AST of the function to which Exp (an expression node) belongs.
%%-spec(expr_to_fun(Tree::syntaxTree(), Exp::syntaxTree())->{ok, syntaxTree()} | {error, none}).
expr_to_fun(Tree, Exp) ->
    Res = expr_to_fun_1(Tree, Exp),
    case Res of
      [H| _T] -> {ok, H};
      _ -> {error, none}
    end.
    
expr_to_fun_1(Tree, Exp) ->
    {Start, End} = refac_misc:get_start_end_loc(Exp),
    {S, E} = refac_misc:get_start_end_loc(Tree),
    if (S < Start) and (E >= End) ->
	   case refac_syntax:type(Tree) of
	     function -> [Tree];
	     _ ->
		 Ts = refac_syntax:subtrees(Tree),
		 R0 = [[expr_to_fun_1(T, Exp) || T <- G] || G <- Ts],
		 lists:flatten(R0)
	   end;
       true -> []
    end.
