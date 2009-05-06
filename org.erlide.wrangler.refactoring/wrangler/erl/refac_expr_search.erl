%% ===========================================================================================
%% Refactoring: Search an user-selected expression/expression sequence from the current buffer.
%%
%% Copyright (C) 2006-2008  Huiqing Li, Simon Thompson

%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.

%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.

%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%% 
-module(refac_expr_search).

-export([expr_search/4, expr_search_eclipse/4, var_binding_structure/1]).

-include("../include/wrangler.hrl").
%% ================================================================================================
%% @doc Search a user-selected expression or a sequence of expressions from an Erlang source file.
%%
%% <p> This functionality allows the user to search an selected expression or a sequence of expressions
%% from the current Erlang buffer. The searching ignores variables names and literals, but it takes
%% the binding structure of variables into account. Therefore the found expressions are the same to the 
%% highlighted expression up to variable renaming and literal substitution. Layout and comments are ignored 
%% by the searching.
%% </p>
%% <p> When the selected code contains multiple, but non-continuous sequence of, expressions, the first
%% continuous sequence of expressions is taken as the user-selected expression. A continuous sequence of
%% expressions is a sequence of expressions separated by ','.
%% <p>
%% =================================================================================================
%% @spec expr_search(FileName::filename(), Start::Pos, End::Pos)-> term().
%% =================================================================================================         

-spec(expr_search/4::(filename(), pos(), pos(), integer()) -> {ok, [{integer(), integer(), integer(), integer()}]} | {error, string()}).    
expr_search(FileName, Start={Line, Col}, End={Line1, Col1}, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:expr_search(~p, {~p,~p},{~p,~p},~p).\n", [?MODULE, FileName, Line, Col, Line1, Col1, TabWidth]),
    {ok, {AnnAST, _Info}} =refac_util:parse_annotate_file(FileName,true, [], TabWidth),
    case refac_util:pos_to_expr_list(FileName, AnnAST, Start, End, TabWidth) of 
	[E|Es] -> 
	    Res = case Es == [] of 
		      true ->
			  search_one_expr(AnnAST, E);
		      _ -> 
			  search_expr_seq(AnnAST, [E|Es])
		  end,
	    case length(Res) of  
		0 -> ?wrangler_io("No identical expression has been found.\n",[]), %% This shouldn't happen.
		     {ok, []};
		1 -> ?wrangler_io("No identical expression has been found. \n",[]),
		     {ok, []};
		N -> ?wrangler_io("~p identical expressions (including the selected expression,and up to variable renaming and literal substitution) "
				       " have been found. \n", [N]),
		     {ok, Res}
	    end;
	_   -> {error, "You have not selected an expression!"}
    end.     
	  
-spec(expr_search_eclipse/4::(filename(), pos(), pos(), integer()) -> {ok, [{integer(), integer(), integer(), integer()}]} | {error, string()}).
expr_search_eclipse(FileName, Start, End, TabWidth) ->
    {ok, {AnnAST, _Info}} =refac_util:parse_annotate_file(FileName,true, [], TabWidth),
    case refac_util:pos_to_expr_list(FileName, AnnAST, Start, End, TabWidth) of 
	[E|Es] -> 
	    Res = case Es == [] of 
		      true ->
			  search_one_expr(AnnAST, E);
		      _ -> 
			  search_expr_seq(AnnAST, [E|Es])
		  end,
	    {ok, Res};	
	_   -> {error, "You have not selected an expression!"}
    end.     

%% Search the clones of an expression from Tree.
search_one_expr(Tree, Exp) ->
    Exp1  = simplify_expr(Exp),
    BdStructExp = var_binding_structure([Exp]),
    F = fun(T, Acc) ->
		case refac_util:is_expr(T) of 
		    true -> T1 = simplify_expr(T),
			    case Exp1 == T1 of 
				true -> BdStructT = var_binding_structure([T]),
					case BdStructExp == BdStructT of 
					    true ->
						{{StartLn, StartCol}, {EndLn, EndCol}}= refac_util:get_range(T),
						Acc ++ [{StartLn, StartCol, EndLn, EndCol+1}];
					    _  -> Acc
					end;
				_ -> Acc
			    end;
		    _ -> Acc
		end
	end,
    refac_syntax_lib:fold(F, [], Tree).
    

%% search the clones of an expresion sequence from Tree.			
search_expr_seq(Tree, ExpList) ->
    AllExpList = contained_exprs(Tree, length(ExpList)),
   %% ExpList1 = lists:map(fun(T) ->simplify_expr(T) end, ExpList),
    Res =lists:flatmap(fun(EL) ->
			       get_clone(ExpList, EL) end, AllExpList),
    Res.
    
    
get_clone(List1, List2) ->    
    Len1 = length(List1),
    Len2 = length(List2),
    case Len1 =< Len2 of 
	true -> 
	    List11 = lists:map(fun(T) -> simplify_expr(T) end, List1),
	    List21 = lists:map(fun(T) -> simplify_expr(T) end, List2),
	    case lists:prefix(List11, List21) of 
		true ->
		    List22 = lists:sublist(List2, Len1),
		    BdList1 = var_binding_structure(List1),
		    BdList2 = var_binding_structure(List22),
		    case BdList1 == BdList2 of 
			true -> E1 = hd(List22),
				En = lists:last(List22),
			        {{StartLn, StartCol}, _EndLoc} = refac_util:get_range(E1),
				{_StartLoc1, {EndLn, EndCol}} = refac_util:get_range(En),
				[{StartLn, StartCol, EndLn, EndCol+1}] ++ get_clone(List1, tl(List2));
			_ -> get_clone(List1, tl(List2))
		    end;				       
		    _ -> get_clone(List1, tl(List2))
		end;			
	_  -> []
    end.
	    

%% simplify an expression by masking variable names and literal variables, and also replace
%% the concrete location values with the default value.
simplify_expr(Exp) ->
    refac_util:full_buTP(fun(Node,_Others)->do_simplify_expr(Node) end, Exp,{}).

do_simplify_expr(Node) ->
    Node1 = case refac_syntax:type(Node) of 
		macro -> 
		    refac_syntax:default_literals_vars(Node, '*');
		variable ->
		    refac_syntax:default_literals_vars(Node, '&');
		integer ->
		    refac_syntax:default_literals_vars(Node, 0);
		float ->
		    refac_syntax:default_literals_vars(Node, 0);
		char ->
		    refac_syntax:default_literals_vars(Node, '%');
		string ->
		    refac_syntax:default_literals_vars(Node, '%');
		atom -> case lists:keysearch(fun_def,1,refac_syntax:get_ann(Node)) of 
			    false ->refac_syntax:default_literals_vars(Node, '%') ;
			    _ -> refac_syntax:default_literals_vars(Node, refac_syntax:atom_value(Node))
			end;
		nil -> refac_syntax:default_literals_vars(Node, nil);
		underscore ->refac_syntax:default_literals_vars(Node, '&');
		_ ->
		    Node
	    end,
    set_default_ann(Node1).
			  

set_default_ann(Node) ->
    refac_syntax:set_pos(refac_syntax:remove_comments(refac_syntax:set_ann(Node, [])), {0,0}).

%% get all the expression sequences contained in Tree.	    
contained_exprs(Tree, MinLen) ->
    F = fun(T, Acc) ->
		case refac_syntax:type(T) of
		    clause -> Exprs = refac_syntax:clause_body(T),  %% HOW ABOUT CLAUSE_GUARD?
			     Acc ++ [Exprs];
		    application -> Exprs = refac_syntax:application_arguments(T),
			     Acc++ [Exprs];
		    tuple -> Exprs = refac_syntax:tuple_elements(T),
			     Acc++ [Exprs];
		    lists -> Exprs = refac_syntax:list_prefix(T),
			     Acc++ [Exprs];
		    block_expr -> Exprs = refac_syntax:block_expr_body(T),
			      Acc++ [Exprs];    
		    _  -> Acc
		end
	end,
    Es = refac_syntax_lib:fold(F, [], Tree),
    [E ||  E <- Es, length(E) >= MinLen].
		 


      

%% get the binding structure of variables.
%%-spec(var_binding_structure/1::([syntaxTree()]) -> [{integer(), integer()}]).      
var_binding_structure(ASTList) ->
    Fun1 = fun (T, S) ->
		   case refac_syntax:type(T) of
		     variable ->
			 Name = refac_syntax:variable_name(T),
			 SrcLoc = refac_syntax:get_pos(T),
			 As = refac_syntax:get_ann(T),
			 case lists:keysearch(def, 1, As) of
			   {value, {def, DefLoc}} ->
			       ordsets:add_element({atom_to_list(Name), SrcLoc,
						    DefLoc},
						   S);
			   _ -> S
			 end;
		     _ -> S
		   end
	   end,
    %% collect all variables including their defining and occuring locations. 
    B = lists:flatmap(fun(T) -> refac_syntax_lib:fold(Fun1, ordsets:new(), T) end, ASTList),
    %% collect defining locations.
  %%  DefLocs = lists:usort(lists:flatten(lists:map(fun ({_Name, _SrcLoc, DefLoc}) ->  DefLoc end, B))),
    %% collect occuring locations.
    SrcLocs = lists:map(fun ({_Name, SrcLoc, _DefLoc}) -> SrcLoc end, B),
    Res = case SrcLocs of 
	      [] -> 
		  [];
	      _ ->  IndexedLocs = lists:zip(SrcLocs, lists:seq(1, length(SrcLocs))),
		    B1 = lists:usort(lists:map(fun ({_Name, SrcLoc, DefLoc}) ->
						       DefLoc1 = hd(DefLoc),             %% DefLoc is  a list, does this cause problems? .
						       {value, {SrcLoc, Index1}} = lists:keysearch(SrcLoc, 1, IndexedLocs),
						       Index2 = case lists:keysearch(DefLoc1, 1, IndexedLocs) of 
								    {value, {_, Ind}} -> Ind;
								    _ -> 0 %% free variable
								end,
						       {Index1, Index2}
					       end,
					       B)),
		    lists:keysort(1, B1)
	  end,
    Res.

