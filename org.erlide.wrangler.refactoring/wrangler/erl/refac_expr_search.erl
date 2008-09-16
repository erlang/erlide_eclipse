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

-export([expr_search/3, var_binding_structure/1, pos_to_expr/3]).


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
%% @spec serch(FileName::filename(), Start::Pos, End::Pos)-> term().
%% =================================================================================================         
expr_search(FileName, Start, End) ->
    case refac_util:parse_annotate_file(FileName,true, []) of 
	{ok, {AnnAST, _Info}} -> 
	      case pos_to_expr(FileName, AnnAST, {Start, End}) of 
		[E|Es] -> 
		   %%io:format("Selected Expression:\n~p\n", [[E|Es]]),
		    Res = case Es == [] of 
			      true ->
				  search_one_expr(AnnAST, E);
			      _ -> 
				  search_expr_seq(AnnAST, [E|Es])
			  end,
		      case length(Res) of  
			0 -> io:format("No identical expression has been found.\n"), %% This shouldn't happen.
			     {ok, []};
			1 -> io:format("No identical expression has been found. \n"),
			     {ok, []};
		  %%	2 -> io:format(" One expression which is identical (up to variable renaming and literal substitution) to the selected"
 			%%	       " expression has been found. \n"), 
 			  %%   {ok, Res};	
	  
			N -> io:format("~p identical expressions (including the selected expression,and up to variable renaming and literal substitution) "
				       " have been fould. \n", [N]),
			     {ok, Res}
		    end;
		_   -> {error, "You have not selected an expression!"}
	    end;	    
	{error, Reason} -> {error, Reason}
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
						Acc ++ [{StartLn, StartCol, EndLn, EndCol}];
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
    Res =lists:concat(lists:map(fun(EL) ->
				   get_clone(ExpList, EL) end, AllExpList)),
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
		    BdList22 = var_binding_structure(List22),
		    case BdList1 == BdList22 of 
			true -> E1 = hd(List22),
				En = lists:last(List22),
			        {{StartLn, StartCol}, _EndLoc} = refac_util:get_range(E1),
				{_StartLoc1, {EndLn, EndCol}} = refac_util:get_range(En),
				[{StartLn, StartCol, EndLn, EndCol}] ++ get_clone(List1, tl(List2));
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
		variable ->
		    refac_syntax:default_literals_vars(Node, '&');
		integer ->
		    refac_syntax:default_literals_vars(Node, 0);
		float ->
		    refac_syntax:default_literals_vars(Node, 0);
		char ->
		    refac_syntax:default_literals_vars(Node, '%');
		string ->
		    refac_syntax:default_literals_vars(Node, "*");
		atom -> case lists:keysearch(fun_def,1,refac_syntax:get_ann(Node)) of 
			    %% or  refac_syntax:default_literals_vars(Node, '%')?  TODO: Think again.
			    false ->refac_syntax:default_literals_vars(Node, refac_syntax:atom_value(Node)) ;
			    _ ->    refac_syntax:default_literals_vars(Node, refac_syntax:atom_value(Node))
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
		


%% get the list sequence of expressions contained in Tree between locations Start and End.
pos_to_expr(FName, Tree, {Start, End}) ->
    {ok, Toks} = refac_epp:scan_file(FName, [], []),
    Exprs = pos_to_expr(Tree, {Start, End}),
    filter_exprs(Toks, Exprs).

filter_exprs(_Toks, []) ->
    [];
filter_exprs(_Toks, [E]) ->
    [E];
filter_exprs(Toks, [E1,E2|Es]) ->
    {_StartLoc, EndLoc} = refac_util:get_range(E1),
    {StartLoc1, _EndLoc1} = refac_util:get_range(E2),
    Toks1 = lists:dropwhile(fun(T) ->
				    token_loc(T) =< EndLoc end, Toks),
    Toks2 = lists:takewhile(fun(T) ->
				    token_loc(T) < StartLoc1 end, Toks1),
    case lists:any(fun(T) -> token_val(T) =/= ',' end, Toks2) of 
	false ->
	    [E1]++ filter_exprs(Toks, [E2|Es]);
	_  -> [E1]
    end.

%% get the list of expressions contained in Tree between locations Start and End.		
pos_to_expr(Tree, {Start, End}) ->
    {S, E} = refac_util:get_range(Tree),
    if (S >= Start) and (E =< End) ->
	    case refac_util:is_expr(Tree) of
		true -> [Tree];
		_ ->
		    Ts = refac_syntax:subtrees(Tree),
		    R0 = [[pos_to_expr(T, {Start, End}) || T <- G]
			  || G <- Ts],
		    lists:flatten(R0)
	    end;
       (S > End) or (E < Start) -> [];
       (S < Start) or (E > End) ->
	    Ts = refac_syntax:subtrees(Tree),
	    R0 = [[pos_to_expr(T, {Start, End}) || T <- G]
		  || G <- Ts],
	    lists:flatten(R0);
       true -> []
    end.

%% get the binding structure of variables.
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
    B = lists:concat(lists:map(fun(T) -> refac_syntax_lib:fold(Fun1, ordsets:new(), T) end, ASTList)),
    %% collect defining locations.
    DefLocs = lists:usort(lists:flatten(lists:map(fun ({_Name, _SrcLoc,
					  DefLoc}) ->
					    DefLoc
				    end,
				    B))),
    %% collect occuring locations.
    SrcLocs = lists:map(fun ({_Name, SrcLoc, _DefLoc}) ->
				SrcLoc
			end,
			B),
    Locs = lists:usort(lists:flatten(DefLocs ++ SrcLocs)),
    Res = case Locs of 
	      [] -> 
		  [];
	      _ ->  IndexedLocs = lists:zip(Locs, lists:seq(1, length(Locs))),
		    B1 = lists:usort(lists:map(fun ({_Name, SrcLoc, DefLoc}) ->
						       DefLoc1 = hd(DefLoc),             %% DefLoc is  a list, does this cause problems? CHECK IT.
						       {value, {SrcLoc, Index1}} = lists:keysearch(SrcLoc, 1, IndexedLocs),
						       {value, {DefLoc1, Index2}} = lists:keysearch(DefLoc1, 1, IndexedLocs),
						       {Index1, Index2}
					       end,
					       B)),
		    lists:keysort(1, B1)
	  end,
    Res.


%% groupby(N, TupleList) ->
%%     TupleList1 = lists:keysort(N, TupleList),
%%     groupby_1(N, TupleList1, []).

%% groupby_1(_N, [], Acc) ->
%%     Acc;
%% groupby_1(N,TupleList=[T|_Ts], Acc) ->
%%     E = element(N, T),
%%     {TupleList1, TupleList2} = lists:partition(fun(T1) ->
%% 						       element(N,T1) == E end, TupleList),
%%     TupleList3 = lists:map(fun(L) -> {L1, L2} = lists:split(N-1, tuple_to_list(L)),
%% 				    L1 ++ [0] ++ tl(L2)
%% 			  end, TupleList1),
%%     groupby_1(N, TupleList2, Acc++[TupleList3]).
			     
		    
%% get the location of a token.
token_loc(T) ->
      case T of 
	{_, L, _V} -> L;
	{_, L1} -> L1
      end.

%% get the value of a token.
token_val(T) ->
    case T of 
	{_, _, V} -> V;
	{V, _} -> V
    end.
