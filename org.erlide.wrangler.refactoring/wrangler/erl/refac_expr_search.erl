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
%% ===========================================================================================
%% Refactoring: Search an user-selected expression/expression sequence from the current buffer.
%%
%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%% 
-module(refac_expr_search).

-export([expr_search/4, expr_search_eclipse/4]).

-export([contained_exprs/2, var_binding_structure/1, compose_search_result_info/2]).

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

%%-spec(expr_search/4::(filename(), pos(), pos(), integer()) -> {ok, [{integer(), integer(), integer(), integer()}]} | {error, string()}).    
expr_search(FileName, Start={Line, Col}, End={Line1, Col1}, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:expr_search(~p, {~p,~p},{~p,~p},~p).\n", [?MODULE, FileName, Line, Col, Line1, Col1, TabWidth]),
    {ok, {AnnAST, _Info}} =refac_util:parse_annotate_file(FileName,true, [], TabWidth),
    case refac_util:pos_to_expr_list(AnnAST, Start, End) of 
	[E|Es] -> 
	    Res = case Es == [] of 
		      true ->
			  SE = refac_sim_expr_search:get_start_end_loc(E),
			  SearchRes = search_one_expr(AnnAST, E),
			  [SE|(SearchRes--[SE])];
		      _ -> 
			  SE =refac_sim_expr_search:get_start_end_loc([E|Es]),
			  SearchRes = search_expr_seq(AnnAST, [E|Es]),
			  [SE|(SearchRes--[SE])]
		  end,
	    Num = length(Res),
	    case Num=<1 of  
		true -> ?wrangler_io("No identical expression has been found. \n",[]),
		     {ok, []};
		false-> ?wrangler_io("\n~p identical expressions(including the expression selected) have been found.\n", [Num]),
		     ?wrangler_io(compose_search_result_info(FileName, Res),[]),
		     ?wrangler_io("\nUse 'C-c C-e' to remove highlights!\n",[]),
		     {ok, Res}
	    end;
	    _   -> {error, "You have not selected an expression!"}
    end.     
	  
%%-spec(expr_search_eclipse/4::(filename(), pos(), pos(), integer()) -> {ok, [{integer(), integer(), integer(), integer()}]} | {error, string()}).
expr_search_eclipse(FileName, Start, End, TabWidth) ->
    {ok, {AnnAST, _Info}} =refac_util:parse_annotate_file(FileName,true, [], TabWidth),
    case refac_util:pos_to_expr_list(AnnAST, Start, End) of 
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
				true -> 
				    BdStructT = var_binding_structure([T]),
				    case BdStructExp == BdStructT of 
					true ->
					    {{StartLn, StartCol}, {EndLn, EndCol}}= refac_util:get_range(T),
					    Acc ++ [{{StartLn, StartCol}, {EndLn, EndCol}}];
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
   lists:flatmap(fun(EL) ->get_clone(ExpList, EL) end, AllExpList).
   
    
    
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
			true ->
			    E1 = hd(List22),
			    En = lists:last(List22),
			    {{StartLn, StartCol}, _EndLoc} = refac_util:get_range(E1),
			    {_StartLoc1, {EndLn, EndCol}} = refac_util:get_range(En),
			    [{{StartLn, StartCol}, {EndLn, EndCol}}] ++ get_clone(List1, tl(List2));
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
		    case refac_syntax:macro_arguments(Node) of 
			none -> 
			    refac_syntax:default_literals_vars(Node, '*');
			_ -> Node
		    end;
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
		    clause ->
			Exprs = refac_syntax:clause_body(T),  %% HOW ABOUT CLAUSE_GUARD?
			Acc ++ [Exprs];
		    application -> 
			Exprs = refac_syntax:application_arguments(T),
			Acc++ [Exprs];
		    tuple -> 
			Exprs = refac_syntax:tuple_elements(T),
			Acc++ [Exprs];
		    lists -> 
			Exprs = refac_syntax:list_prefix(T),
			Acc++ [Exprs];
		    block_expr ->
			Exprs = refac_syntax:block_expr_body(T),
			Acc++ [Exprs];    
		    try_expr ->
			Exprs = refac_syntax:try_expr_body(T),
			Acc ++ [Exprs];
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
			        [{atom_to_list(Name), SrcLoc,
						    DefLoc}|S];
			   _ -> S
			 end;
		     _ -> S
		   end
	   end,
    %% collect all variables including their defining and occuring locations. 
    B = lists:keysort(2, lists:flatmap(fun(T) -> refac_syntax_lib:fold(Fun1, [], T) end, ASTList)),
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

compose_search_result_info(FileName, Ranges) ->
    compose_search_result_info(FileName, Ranges, "").
compose_search_result_info(_FileName, [], Str) ->
    Str;
compose_search_result_info(FileName, [{{StartLine, StartCol}, {EndLine, EndCol}}|Ranges], Str) ->
    Str1 =Str ++ "\n"++FileName++io_lib:format(":~p.~p-~p.~p: \n", [StartLine, StartCol, EndLine, EndCol]),
    compose_search_result_info(FileName, Ranges, Str1).
					       
    
    
    
