%% =====================================================================
%% Duplicated Code Detection.
%%
%% Copyright (C) 2010  Huiqing Li, Simon Thompson

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
%% =====================================================================

-module(refac_duplicated_code).

-export([duplicated_code/5, 
	 duplicated_code_eclipse/5,
	 duplicated_code_command_line/5]).

-include("../include/wrangler.hrl").

%% minimal number of tokens.
-define(DEFAULT_CLONE_LEN, 40).

%% Minimal number of class members.
-define(DEFAULT_CLONE_MEMBER, 2).

%% Maximal number of new parameters of the anti-unifier.
-define(DEFAULT_MAX_PARS, 5).

%% ==================================================================================
%% @doc Find duplicated code in a Erlang source files.
%% <p> This function only reports the duplicated code fragments found. It does not actually 
%% remove those duplicated code. Two parameters can be provided 
%% by the user to specify the minimum code clones to report, and they are:  
%% \emph{the minimum number of lines of a code clone} and \emph{the minimum number of 
%% duplicated times}, the default values 2 for both parameters.
%% </p>
%% ====================================================================================

-spec(duplicated_code_eclipse/5::([dir()|filename()], integer(),integer(), integer(), filename()) ->
	[{[{{filename(), integer(), integer()},{filename(), integer(), integer()}}], 
	  integer(), integer(), string()}]).
duplicated_code_eclipse(DirFileList, MinLength1, MinClones1, TabWidth, SuffixTreeExec) ->
    MinLength = case MinLength1 =< 1 of
		  true ->
		      ?DEFAULT_CLONE_LEN;
		  _ -> MinLength1
		end,
    MinClones = case MinClones1 < ?DEFAULT_CLONE_MEMBER of
		    true -> ?DEFAULT_CLONE_MEMBER;
		    _ -> MinClones1
		end,
    %%TODO: replace 10 with parameter, and let George know the change.
    Cs = duplicated_code_detection(DirFileList, MinClones, MinLength, 10, SuffixTreeExec, TabWidth),
    refac_code_search_utils:remove_sub_clones(Cs).

-spec(duplicated_code/5::([dir()|filename()],string(),string(), string(),integer()) ->{ok, string()}).
duplicated_code(DirFileList, MinLength1, MinClones1, MaxPars1, TabWidth) ->
    {MinClones, MinLength, MaxPars} = get_parameters(MinLength1, MinClones1, MaxPars1),
    ?wrangler_io("\nCMD: ~p:duplicated_code(~p,~p,~p,~p,~p).\n",
		 [?MODULE, DirFileList, MinLength1, MinClones1, MaxPars1, TabWidth]),
    Cmd =io_lib:format("\nCMD: ~p:duplicated_code(~p,~p,~p,~p,~p).\n",
		 [?MODULE, DirFileList, MinLength, MinClones, MaxPars, TabWidth]),
    ?debug("current time:~p\n", [time()]),
    SuffixTreeExec = filename:join(?WRANGLER_DIR, "bin/suffixtree"),
    Cs = duplicated_code_detection(DirFileList, MinClones, MinLength, MaxPars, SuffixTreeExec, TabWidth),
    ?debug("Filtering out sub-clones.\n", []),
    Cs1 = refac_code_search_utils:remove_sub_clones(Cs),
    ?debug("current time:~p\n", [time()]),
    refac_code_search_utils:display_clone_result(Cs1, "Duplicated"),
    NumOfClones = length(Cs1),
    LogMsg = Cmd ++ " Num of clones detected: "++ integer_to_list(NumOfClones) ++ "\n",
    {ok, lists:flatten(LogMsg)}.


-spec(duplicated_code_command_line/5::([dir()|filename()],string(),string(), string(),integer()) ->{ok, string()}).
duplicated_code_command_line(DirFileList, MinLength1, MinClones1, MaxPars, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:duplicated_code(~p,~p,~p,~p,~p).\n",
		 [?MODULE, DirFileList, MinLength1, MinClones1, MaxPars, TabWidth]),
    MinLength = case MinLength1 =< 1 of
		    true ->
			?DEFAULT_CLONE_LEN;
		    _ -> MinLength1
		end,
    MinClones = case MinClones1 < ?DEFAULT_CLONE_MEMBER of
		    true -> ?DEFAULT_CLONE_MEMBER;
		    _ -> MinClones1
		end,
    SuffixTreeExec = filename:join(?WRANGLER_DIR, "bin/suffixtree"),
    Cs = duplicated_code_detection(DirFileList, MinClones, MinLength, MaxPars, SuffixTreeExec, TabWidth),
    Cs1 = refac_code_search_utils:remove_sub_clones(Cs),
    refac_code_search_utils:display_clone_result(Cs1, "Duplicated"),
    {ok, "Duplicated code detection finished."}.

-spec(duplicated_code_detection/6::([dir()|filename()], integer(),integer(), integer(), filename(),integer()) ->
					 [{[{{filename(), integer(), integer()},{filename(), integer(), integer()}}], 
					   integer(), integer(), string()}]).
duplicated_code_detection(DirFileList, MinClones, MinLength, MaxPars, SuffixTreeExec, TabWidth) ->
    FileNames = refac_util:expand_files(DirFileList, ".erl"),
    case FileNames of 
	[] -> throw({error, "No .erl files were found"});
	_ -> ok
    end,
    ?debug("Files:\n~p\n", [FileNames]),
    ?debug("Constructing suffix tree and collecting clones from the suffix tree.\n", []),
    {Toks, ProcessedToks} = tokenize(FileNames, TabWidth),
    Dir = filename:dirname(hd(FileNames)),
    Cs= suffix_tree:get_clones_by_suffix_tree(Dir, ProcessedToks,MinLength, MinClones,alphabet(), 0, SuffixTreeExec),
    ?debug("Initial numberclones from suffix tree:~p\n", [length(Cs)]),
    %% This step is necessary to reduce large number of sub-clones.
    ?debug("Type 4 clones:\n~p\n", [length(Cs)]),
    ?debug("Putting atoms back.\n",[]),
    Cs1 = clones_with_atoms(Cs, Toks, MinLength, MinClones),
    ?debug("Filtering out sub-clones.\n", []),
    Cs2 = refac_code_search_utils:remove_sub_clones(Cs1),
    Cs3 = combine_neighbour_clones(Cs2, MinLength, MinClones),
    ?debug("Type3 without trimming:~p\n", [length(Cs3)]),
    ?debug("Trimming clones.\n", []),
    trim_clones(Cs3, MinLength, MinClones, MaxPars, TabWidth).
    

%%=====================================================================================
%% process the parameters input by the user.
-spec(get_parameters/3::(string(), string(), string()) ->
			      {integer(), integer(), integer()}).
get_parameters(MinLengthStr, MinClonesStr, MinParsStr) ->
    MinLength = try
		    case MinLengthStr == [] orelse list_to_integer(MinLengthStr) =< 1 of
			true ->
			    ?DEFAULT_CLONE_LEN;
			_ -> list_to_integer(MinLengthStr)
		    end
		catch
		    Val -> Val;
		    _:_ -> throw({error, "Parameter input is invalid."})
		end,
    MinClones =  get_parameters_1(MinClonesStr, ?DEFAULT_CLONE_MEMBER),
    MaxPars =  get_parameters_1(MinParsStr, ?DEFAULT_MAX_PARS),
    {MinClones, MinLength, MaxPars}.

get_parameters_1(MinClonesStr, DefaultVal) ->
    try
	case MinClonesStr == [] orelse
	    list_to_integer(MinClonesStr) < DefaultVal
	of
	    true -> DefaultVal;
	    _ -> list_to_integer(MinClonesStr)
	end
    catch
	Val1 -> Val1;
	_:_ -> throw({error, "Parameter input is invalid."})
    end.
%% =====================================================================
%% tokenize a collection of concatenated Erlang files.

-spec(tokenize/2::([filename()], integer()) ->
		{[token()], string()}).
tokenize(FileList, TabWidth) ->
    Toks = lists:flatmap(fun(F) -> 
				 Toks= refac_util:tokenize(F, false, TabWidth),
				 [add_filename_to_token(F,T)||T<-Toks]
			 end, FileList),
    R = [T1 || T <- Toks, T1 <- [process_a_tok(T)]],
    {Toks, lists:concat(R)}.


process_a_tok(T) -> 
    case T of 
	{var, _L, _V} -> 'V';       
	{atom, _L1, _V1} ->'A';     
	{integer, _L2,_V2} -> 'I';  
	{string, _L3, _V3} -> 'S';  
	{float, _, _} ->'F';        
	{char, _, _} ->'C' ;        
	{A, _} -> 
	    case lists:keysearch(A, 1, alphabet_1()) of
		{value, {A, Value}} -> Value;
		_  -> ' '
	    end
    end.

%% ==================================================================================
%% This phase brings back those atoms back into the token stream, and get the resulted clones.

clones_with_atoms(Cs, Toks, MinLength, MinClones) ->
    Cs1 = lists:flatmap(fun({Range, _Len, _F}) ->
				clones_with_atoms_1(Range, Toks, MinLength, MinClones) 
			end, Cs),
    Cs2 = simplify_filter_results(Cs1, [], MinLength, MinClones),
    [{R, L, F} || {R, L, F} <-Cs2, L >= MinLength, F >=MinClones].

clones_with_atoms_1(Range, Toks, _MinLength, MinClones) ->
    ListsOfSubToks =[lists:sublist(Toks, S, E - S+1) || {S, E}<-Range],
    ZippedToks = zip_list(ListsOfSubToks),
    Cs = clones_with_atoms_2([ZippedToks], []),
    [C || C <- Cs, length(hd(C)) >= MinClones].

 

clones_with_atoms_2([], Acc) -> Acc;
clones_with_atoms_2([ZippedToks| Others], Acc) ->
    {Cs1, Cs2} = lists:splitwith(
		   fun (L) ->
			   length(lists:usort(fun (T1, T2) ->
						      rm_loc_var_and_literal_in_tok(T1)
							== rm_loc_var_and_literal_in_tok(T2)
					      end, L)) == 1
		   end, ZippedToks),
    case Cs2 of
      [] -> clones_with_atoms_2(Others, Acc ++ [Cs1]);
      _ -> SubCs = lists:keysort(2, lists:zip(lists:seq(1, length(hd(Cs2)), 1), hd(Cs2))),
	   SubCsIndexes = lists:filter(fun (L) -> length(L) >= 2 end, group_toks(SubCs)),
	   Cs3 = get_sub_cs(ZippedToks, SubCsIndexes, []),
	   clones_with_atoms_2(Others ++ [tl(Cs2)] ++ Cs3, Acc ++ [Cs1])
    end.

get_sub_cs(_ZippedToks, [], Acc) -> Acc;
get_sub_cs(ZippedToks, [H|L], Acc) ->
    H1 = [A||{A, _T}<-H],
    Cs1 = [[lists:nth(I, T) || I<-H1]|| T <-ZippedToks],
    get_sub_cs(ZippedToks, L, [Cs1]++Acc).


%% use locations intead of tokens to represent the clones.
simplify_filter_results([], Acc, _MinLength, _MinClones) -> Acc;
simplify_filter_results([C | Cs], Acc, MinLength, MinClones) ->
    StartEndTokens = lists:zip(hd(C), lists:last(C)),
    Ranges = lists:map(fun ({StartTok, EndTok}) ->
			       Start = token_loc(StartTok),
			       {File, EndLine, EndCol} = token_loc(EndTok),
			       {Start,{File, EndLine, EndCol+token_len(EndTok)-1}}
		       end, StartEndTokens),
    case length(hd(C)) >= MinClones of
      true -> simplify_filter_results(Cs, Acc ++ [{Ranges, length(C), length(StartEndTokens)}], MinLength, MinClones);
	_ -> simplify_filter_results(Cs, Acc, MinLength, MinClones)
    end.


%%===========================================================================

%% combine clones which are next to each other. (ideally a fixpoint should be reached).
combine_neighbour_clones(Cs, MinLength, MinClones) ->
     Cs1 = combine_neighbour_clones(Cs, []),
    Cs2= [{R, L, F} || {R, L, F} <-Cs1, L >= MinLength, F >=MinClones],
    refac_code_search_utils:remove_sub_clones(Cs2).

combine_neighbour_clones([], Acc) -> Acc;
combine_neighbour_clones([C|Cs], Acc) ->
    C1 = case Acc of 
 	     [] -> [C] ;
 	     _ -> [connect_clones(C, Clone)||Clone<-Acc]
 	 end,
     C2 = lists:usort(C1),
    combine_neighbour_clones(Cs, Acc++C2).


connect_clones(C1={Range1, Len1, F1}, {Range2, Len2, F2}) ->  
    {StartLocs1, EndLocs1} = lists:unzip(Range1),
    {StartLocs2, EndLocs2} = lists:unzip(Range2),
    case F1 =< F2 of 
	true -> 
	    case lists:subtract(StartLocs1, EndLocs2) of 
		[] -> R1 = lists:filter(fun({_S, E}) -> lists:member(E, StartLocs1) end, Range2),
		      R2= [S||{S, _E}<-R1],
		      {lists:zip(R2, EndLocs1), Len1+Len2, F1};
		_  -> 
		    case lists:subtract(EndLocs1, StartLocs2) of 
			[] -> R3= lists:filter(fun({S,_E}) ->lists:member(S, EndLocs1) end, Range2),
			      R4 = [E||{_S,E}<-R3],
			      {lists:zip(StartLocs1, R4), Len1+Len2, F1};
			_ -> C1
		    end
	    end; 
	_  -> C1
    end.
	  
%% ================================================================================== 
%% trim both end of each clones to exclude those tokens that does not form a meaninhful syntax phrase.
%% This phase needs to get access to the abstract syntax tree.

trim_clones(Cs, MinLength, MinClones, MaxPars, TabWidth) ->
    Files0 = [File || C <- Cs, {Range, _Len, _Freq} <- [C],
		      {File, _, _} <- element(1, lists:unzip(Range))],
    Files = refac_misc:remove_duplicates(Files0),
    Fun = fun (File, Cs0) -> process_a_file(File, Cs0, MinLength, TabWidth) end,
    Fun2 = fun (ListsOfUnitsList) ->
		   case lists:usort(lists:map(fun length/1, ListsOfUnitsList)) of
		     [_N] -> ZippedUnitsList = zip_list(ListsOfUnitsList),
			     NewCs = lists:append([group_by_index(4, ZippedUnits) || ZippedUnits <- ZippedUnitsList]),
			     lists:append([get_anti_unifier(C, MaxPars, MinLength) || C <- NewCs, C /= [], length(C) >= MinClones,
										      element(5, hd(C)) >= MinLength]);
		     _ -> []
		   end
	   end,
    Cs1 = lists:foldl(Fun, Cs, Files),
    lists:append([Fun2(Ranges) || {Ranges, _, _} <- Cs1]).

process_a_file(File, Cs, MinLength, TabWidth) ->
    {ok, {AnnAST, _}} = refac_util:parse_annotate_file(File, true, [], TabWidth),
    Vars = refac_misc:collect_var_source_def_pos_info(AnnAST),
    Fun0 = fun (Node) ->
		   case refac_syntax:type(Node) of
		     function ->
			 true;
		     _ ->
			 (refac_misc:is_expr(Node) orelse refac_syntax:type(Node)==match_expr) 
			       andalso refac_syntax:type(Node) /= guard_expression
		   end
	   end,
    Fun1 = fun (Range) ->
		   case Range of
		     {{File1, L1, C1}, {File2, L2, C2}} ->
			 case File1 /= File2 of
			   true -> [];
			   _ when File == File1 ->
			       Units = pos_to_syntax_units(AnnAST, {L1, C1}, {L2, C2}, Fun0, MinLength),
			       [process_a_unit(Vars, File1, U) || U <- Units];
			   _ -> Range
			 end;
		     _ -> Range
		   end
	   end,
    [{NewRanges, Len, Freq}
     || C <- Cs,
	{Range, Len, Freq} <- [C],
	NewRanges <- [lists:map(Fun1, Range)], NewRanges /= []].

process_a_unit(VarsUsed, FileName, Unit) ->
    {{StartLn, StartCol}, _} = refac_misc:get_start_end_loc(hd(Unit)),
    {_, {EndLn, EndCol}} = refac_misc:get_start_end_loc(lists:last(Unit)),
    BdStruct = refac_code_search_utils:var_binding_structure(Unit),
    Range = {{FileName, StartLn, StartCol}, {FileName, EndLn, EndCol}},
    ExprBdVarsPos = [Pos || {_Var, Pos} <- refac_misc:get_bound_vars(Unit)],
    VarsToExport = [{V, DefPos} || {V, SourcePos, DefPos} <- VarsUsed,
				   SourcePos > {EndLn, EndCol},
				   lists:subtract(DefPos, ExprBdVarsPos) == []],
    {Range, Unit, VarsToExport, BdStruct, num_of_tokens(Unit)}.


%%-spec(pos_to_syntax_units(syntaxTree(),pos(),pos(),function(), integer()) ->[syntaxTree()]).
pos_to_syntax_units(Tree, Start, End, F, MinLength) ->
    Type = refac_syntax:type(Tree),
    Res = pos_to_syntax_units_1(Tree, Start, End, F, Type),
    filter_syntax_units(Res, MinLength).

pos_to_syntax_units_1(Tree, Start, End, F, Type) ->
    Fun = fun (Node) ->
		  Ts = refac_syntax:subtrees(Node),
		  Type1 = refac_syntax:type(Node),
		  [[lists:append(pos_to_syntax_units_1(T, Start, End, F, Type1)) || T <- G]
		   || G <- Ts]
	  end,
    {S, E} = refac_misc:get_start_end_loc(Tree),
    if (S >= Start) and (E =< End) ->
	   case F(Tree) of
	     true ->
		 [{Tree, Type}];
	     _ -> []
	   end;
       (S > End) or (E < Start) -> [];
       (S < Start) or (E > End) ->
	   Fun(Tree);
       true -> []
    end.

filter_syntax_units(Es, MinLength) ->
    Fun = fun (ExprList) ->
		  {Exprs, Type} = lists:unzip(ExprList),
		  L = [tuple, list],
		  case L -- Type =/= L of
		    true -> case length(Exprs) > 1 of
			      true ->
				  [[E] || E <- Exprs];
			      false ->
				  [Exprs]
			    end;
		    false ->
			case lists:usort(Type) of
			  [form_list] ->
			      [[E] || E <- Exprs];
			  _ ->
			      [Exprs]
			end
		  end
	  end,
    Fun2 = fun (Exprs) ->
		   BlockExpr = refac_syntax:block_expr(Exprs),
		   Str = refac_prettypr:format(BlockExpr),
		   num_of_tokens_in_string(Str)
	   end,
    Es1 = filter_syntax_units_1(Es),
    [E1 || E <- Es1, E1 <- Fun([E0 || E0 <- E, E0 /= []]), E1 /= [], Fun2(E1) >= MinLength + 2].


filter_syntax_units_1([]) ->[[]];
filter_syntax_units_1(Es) ->
    F = fun (E) -> not (is_list(E) andalso lists:flatten(E) /= []) end,
    {Es1, Es2} = lists:splitwith(F, Es),
    Es11 = lists:flatten(Es1),
    case Es2 of 
	[] -> [Es11];
	[H|T] ->
	    lists:append([[Es11], filter_syntax_units_1(H), filter_syntax_units_1(T)])
    end.

%% ===========================================================
%% Some utility functions:

%% number of tokens in AST(s).
num_of_tokens(Exprs) ->
    BlockExpr = refac_syntax:block_expr(Exprs),
    Str = refac_prettypr:format(BlockExpr),
    num_of_tokens_in_string(Str).


num_of_tokens_in_string(Str) ->
    case refac_scan:string(Str, {1,1}, 8, 'unix') of
	{ok, Ts, _} -> 
	    length(Ts);
	_ ->
	    0
    end.

group_toks([]) -> [];
group_toks(SortedList = [{_Seq, Tok}| _T]) ->
    {Es1, Es2} = lists:splitwith(fun ({_S, T}) ->
					 rm_loc_var_and_literal_in_tok(T) ==
					   rm_loc_var_and_literal_in_tok(Tok)
				 end, SortedList),
    [Es1| group_toks(Es2)].
    

group_by_index(N, TupleList) ->
    group_by_1(N, lists:keysort(N, TupleList)).

group_by_1(_N, []) -> [];
group_by_1(N, TupleList=[E|_Es]) ->
    NthEle = element(N, E),
    {E1,E2} = lists:splitwith(fun(T) -> element(N,T) == NthEle end, TupleList),
    [E1 | group_by_1(N, E2)].
    

%% zip n lists.
zip_list(ListOfLists) ->    
    zip_list_1(ListOfLists, []).

zip_list_1([[]|_T], Acc)  ->
    Acc;
zip_list_1(ListOfLists, Acc)->      
    zip_list_1([tl(L) || L <-ListOfLists],
	       Acc ++ [[hd(L)|| L  <- ListOfLists]]).


rm_loc_var_and_literal_in_tok(T) ->
    rm_loc_in_tok(rm_var_literal_in_tok(T)).

rm_var_literal_in_tok(T) ->
    case T of
      {var, L, _} -> {var, L, 'V'};
      {integer, L, _} -> {integer, L, 0};
      {string, L, _} -> {string, L, "_"};
      {float, L, _} -> {float, L, 0};
      {char, L, _} -> {char, L, 'C'};
      {A, L} -> {A, L};
      Other -> Other
    end.

%% remove location info. in the token stream.
rm_loc_in_tok(T) ->
    D = {0,0},
    case T of 
	{var, _L, V} -> {var, D, V};
	{integer, _L, V} -> {integer, D, V};
	{string, _L, V} -> {string, D, V};
	{float, _L, V} -> {float, D, V}; 
	{char, _L, V}  -> {char, D, V};
	{atom, _L, V} -> {atom, D, V};
	{A, _L} ->{A, D};
	Other  -> 
	    erlang:error(?wrangler_io("Unhandled token:\n~p\n", [Other]))
    end.

token_loc(T) ->
    case T of 
	{_, L, _V} -> L;
	{_, L1} -> L1
    end.

token_val(T) ->
    case T of 
	{_, _, V} -> V;
	{V, _} -> V
    end.


token_len(T) ->
    V = token_val(T),
    token_len_1(V).

token_len_1(V) when is_atom(V) ->
    length(atom_to_list(V));
token_len_1(V) when is_integer(V) ->
    length(integer_to_list(V));
token_len_1(V) when is_list(V)->
     length(V)+2;
token_len_1(V) when is_float(V) ->
    %% this is not what I want; but have not figure out how to do it.
    length(float_to_list(V)); 
token_len_1(V) when is_binary(V) ->
    length(binary_to_list(V));
token_len_1(_V) -> 1.  %5 unhandled token;
       
 
add_filename_to_token(FileName,T) ->
    case T of 
	{C, {Ln,Col}, V} ->
	    {C,{FileName, Ln, Col}, V};
	{V, {Ln,Col}} ->
	    {V, {FileName, Ln, Col}}
    end.

%% =====================================================================
%% get the alphabet on which the suffix tree is going to be built.
%%-spec(alphabet/0::()-> string()).
alphabet() ->
    lists:concat([Y||{_X, Y}<-alphabet_1()]) ++ "ACFISV&".
		  
alphabet_1() ->
   [{'after',a},{'andalso',b}, {'and',c},{'begin',d},{'bnot',e},{'band',f},
    {'bor',g},{'bxor',h},{'bsl',i},{'bsr',j},{'case',k}, {'cond',l}, {'catch',m},
    {'div',n},{'dot', o}, {'end',p},{'fun',q},{'if',r},{'let',s},{'not',t}, 
    {'orelse',u},{'of',v}, {'or',w},{'query',x},{'receive',y}, {'rem',z},
    {'try','B'},{'when','D'},{'xor','E'}, {'<<', 'G'}, {'<-', 'H'},{'<=', 'J'},
    {'>>', 'K'}, {'>=', 'L'},{'->', 'M'},{'--', 'N'},{'++', 'O'},{'=:=', 'P'},
    {'=/=', 'Q'},{'=<',  'R'}, {'==',  'T'},{'/=',  'U'},{'||',  'W'},
    {':-',  'X'},{'spec', 'Y'},{'::', 'Z'},{'(','('}, {')',')'}, {'{','{'},
    {'}','}'},{'[', '['}, {']', ']'},{'.', '.'}, {':',':'},{'|','|'},
    {';',';'}, {',',','},{'?','?'},{'#','#'},{'+','+'},{'-','-'}, {'*','*'},
    {'/','/'}, {'<','<'}, {'>','>'}, {'=','='}, {'!','!'}].

%% =====================================================================

%%-spec(get_anti_unifier([{{{filename(), integer(), integer()},{filename(), integer(), integer()}}, 
%%			 syntaxTree(), any(), any(), integer()}],integer(), integer()) ->
%%	     [{[{{filename(), integer(), integer()},{filename(), integer(), integer()}}], 
%%	       integer(), integer(), string()}]).
get_anti_unifier(C, MaxPars, MinLength) ->
    Freq = length(C),
    Len =  element(5, hd(C)),
    C1 ={lists:unzip([{R, {U, Vars}}||{R, U, Vars, _Bd, _Len}<-C]), Len, Freq},
    get_anti_unifier_0(C1, MaxPars, MinLength).
		  
get_anti_unifier_0(_C={{Range, CodeEVsPairs}, Len, F}, MaxPars, MinLength) ->
    try
        get_anti_unifier_1(CodeEVsPairs)
    of
	{Res, Pars} -> 
	    case Pars > MaxPars orelse num_of_tokens_in_string(Res)< MinLength of
		true -> [];
		_ ->
		    [{Range, Len, F, Res}] 
	    end
    catch
	_Error_ -> []
    end.

get_anti_unifier_1([]) ->
    throw({error, anti_unification_failed});
get_anti_unifier_1([{Expr, EVs}]) -> generalise_expr({Expr, EVs}, []);
get_anti_unifier_1([{Expr, EVs}| Exprs]) ->
    try 
	Res = [expr_anti_unification(Expr, E, ExportedVars)
	       || {E, ExportedVars} <- Exprs],
	{Nodes1, EVs1} = lists:unzip(Res),
	GroupedNodes = group_subst_nodes(Nodes1),
	Pid = refac_code_search_utils:start_counter_process(),
	NodeVarPairs = lists:append([lists:zip(Ns, lists:duplicate(length(Ns),
								   refac_code_search_utils:gen_new_var_name(Pid)))
				     || Ns <- GroupedNodes]),
	refac_code_search_utils:stop_counter_process(Pid),
	generalise_expr({Expr, EVs}, {NodeVarPairs, lists:usort(lists:append(EVs1))})
    of
	Result ->
	    Result
    catch
	_ ->
	    throw({error, "anti_unification_failed"})
    end.

expr_anti_unification(Exp1, Exp2, Expr2ExportedVars) ->
    try
      do_expr_anti_unification(Exp1, Exp2)
    of
      SubSt ->
	    EVs1 = [{refac_syntax:variable_name(E1), get_var_define_pos(E1)}
		    || {E1, E2} <- SubSt, refac_syntax:type(E2) == variable,
		       lists:member({refac_syntax:variable_name(E2), get_var_define_pos(E2)}, Expr2ExportedVars)],
	    SubSt1 = [{E1, E2} || {E1, E2} <- SubSt, refac_syntax:type(E1) /= variable orelse is_macro_name(E1)],
	    Nodes = group_substs(SubSt1),
	    {Nodes, EVs1}
    catch
      _ ->
	  throw({error, "anti_unification_failed"})
    end.

group_substs(Subst) ->
    SubSt1 = [{E1, E2, {refac_prettypr:format(E1), refac_prettypr:format(E2)}} || {E1, E2} <- Subst],
    SubSt2 = group_by_index(3, SubSt1),
    [[E1 || {E1, _E2, _} <- S] || S <- SubSt2].


group_subst_nodes(GroupedNodeLists) ->
    H = hd(GroupedNodeLists),
    group_subst_nodes(tl(GroupedNodeLists),[ordsets:from_list(L)|| L<-H]).
group_subst_nodes([], Acc) ->
    Res = [{L1, lists:min([refac_syntax:get_pos(E) || E <- L1])}
	   || L <- Acc, L1 <- [ordsets:to_list(L)], L1 /= []],
    element(1, lists:unzip(lists:keysort(2, Res)));
group_subst_nodes([L| T], Acc) ->
    L1 = [ordsets:from_list(E) || E <- L],
    Insets = [E || E <- [ordsets:intersection(E1, E2) || E1 <- L1, E2 <- Acc],
		   ordsets:to_list(E) /= []],
    {L11, Acc1} = case Insets of
		    [] -> {L1, Acc};
		    _ -> {sets_subtraction(L1, Insets),
			  sets_subtraction(Acc, Insets)}
		  end,
    group_subst_nodes(T, Insets ++ L11 ++ Acc1).

sets_subtraction(L, Insets) ->
    [E1 || E <- L, E1 <- [lists:foldl(fun (I, E0) ->
					      ordsets:subtract(E0, I)
				      end, E, Insets)], E1 /= []].

do_expr_anti_unification(Exp1, Exp2) ->
    case {is_list(Exp1), is_list(Exp2)} of
	{true, true} ->
	    case length(Exp1) == length(Exp2) of
		true ->
		    lists:flatmap(fun ({E1, E2}) ->
					  do_expr_anti_unification(E1, E2)
				  end, lists:zip(Exp1, Exp2));
		_ ->
		    throw({error, anti_unification_failed})
	    end;
	{false, false} ->  %% both are single expressions.
	    do_expr_anti_unification_1(Exp1, Exp2)
    end.

do_expr_anti_unification_1(Exp1, Exp2) ->
    T1 = refac_syntax:type(Exp1),
    T2 = refac_syntax:type(Exp2),
    case refac_syntax:is_literal(Exp1) andalso refac_syntax:is_literal(Exp2) of
	true ->
	  do_anti_unify_literals(Exp1, Exp2);
	false ->
	    case {T1, T2} of
		{macro, macro} -> do_anti_unify_macros(Exp1, Exp2);
		{macro, _} -> throw({error, anti_unification_failed});
		{variable, variable} ->
		    case is_macro_name(Exp1) or is_macro_name(Exp2) of
			true ->
			    throw({error, anti_unification_failed});
			false ->
			    [{Exp1, Exp2}]
		    end;
		{binary, binary} ->  %% choose not to generalise over binary fields.
		    SubTrees1 = erl_syntax:subtrees(Exp1),
		    SubTrees2 = erl_syntax:subtrees(Exp2),
		    try do_expr_anti_unification(SubTrees1, SubTrees2) of
			[] ->
			    [];
			_ ->
			    throw({error, anti_unification_failed})
		    catch
			_E1:_E2 ->
			    throw({error, anti_unification_failed})
		    end;
		_ when T1/=T2 ->
		    throw({error, anti_unification_failed});
		_ ->
		    SubTrees1 = erl_syntax:subtrees(Exp1),
		    SubTrees2 = erl_syntax:subtrees(Exp2),
		    do_expr_anti_unification(SubTrees1, SubTrees2)
	    end
    end.

do_anti_unify_macros(Exp1, Exp2) ->
    MacroName1 =refac_code_search_utils:identifier_name(refac_syntax:macro_name(Exp1)),
    MacroName2 =refac_code_search_utils:identifier_name(refac_syntax:macro_name(Exp2)),
    case MacroName1 == MacroName2 of 
	true ->
	    case refac_syntax:macro_arguments(Exp1) of
		none -> [];
		_ -> SubTrees1 = erl_syntax:macro_arguments(Exp1),
		     SubTrees2 = erl_syntax:macro_arguments(Exp2),
		     do_expr_anti_unification(SubTrees1, SubTrees2)
	    end;
	false ->
	    case refac_syntax:macro_arguments(Exp1) of
		none ->
		    [{Exp1, Exp2}];
		_ ->
		    throw({error, anti_unification_failed})
	    end
    end.
       
do_anti_unify_literals(Exp1, Exp2) ->
    T1 = refac_syntax:type(Exp1),
    T2 = refac_syntax:type(Exp2),
    case {T1, T2} of
	{atom, atom} -> do_anti_unify_atoms(Exp1, Exp2);
	{_, _} -> 
	    case has_the_same_value(Exp1, Exp2) of
		true -> [];
		_ -> [{Exp1, Exp2}]
	    end
    end.


do_anti_unify_atoms(Exp1, Exp2) ->
    case has_the_same_value(Exp1, Exp2) of
      true ->
	  case lists:keysearch(fun_def, 1, refac_syntax:get_ann(Exp1)) of
	    {value, {fun_def, {M, _, A, _, _}}} ->
		case lists:keysearch(fun_def, 1, refac_syntax:get_ann(Exp2)) of
		  {value, {fun_def, {M, _, A, _, _}}} ->
		      [];
		  _ -> [{Exp1, Exp2}]
		end;
	    false ->
		case lists:keysearch(fun_def, 1, refac_syntax:get_ann(Exp2)) of
		  {value, {fun_def, _}} ->
		      [{Exp1, Exp2}];
		  false ->
		      []
		end
	  end;
      _ ->
	  case refac_code_search_utils:generalisable(Exp1) of
	    true ->
		[{Exp1, Exp2}];
	    _ ->
		throw({error, anti_unification_failed})
	  end
    end.

has_the_same_value(Node1, Node2) ->
    refac_syntax:concrete(Node1)==refac_syntax:concrete(Node2).


%%===============================================
generalise_expr({[], _}, _) ->
    {"", 0};
generalise_expr({Exprs = [H| _T], EVs}, {NodeVarPairs, VarsToExport}) ->
    case refac_syntax:type(H) of
      function ->
	  generalise_fun(H, NodeVarPairs);
      _ ->
	  FunName = refac_syntax:atom(new_fun),
	  FVs = lists:ukeysort(2, refac_misc:get_free_vars(Exprs)),
	  EVs1 = lists:ukeysort(2, EVs ++ VarsToExport),
	  NewExprs = generalise_expr_1(Exprs, NodeVarPairs),
	  NewExprs1 = case EVs1 of
			[] -> NewExprs;
			[{V, _}] -> E = refac_syntax:variable(V),
				    NewExprs ++ [E];
			[_V| _Vs] -> E = refac_syntax:tuple([refac_syntax:variable(V) || {V, _} <- EVs1]),
				     NewExprs ++ [E]
		      end,
	  NewVars = refac_misc:collect_var_names(NewExprs) -- refac_misc:collect_var_names(Exprs),
	  Pars = [refac_syntax:variable(V) || {V, _} <- FVs] ++
		   [refac_syntax:variable(V) || V <- NewVars],
	  Pars1 = refac_misc:remove_duplicates(Pars),
	  C = refac_syntax:clause(Pars1, none, NewExprs1),
	  {refac_prettypr:format(refac_syntax:function(FunName, [C])), length(Pars1)}
    end.

generalise_fun(F, NodesToGen) ->
    FunName = refac_syntax:function_name(F),
    Cs = refac_syntax:function_clauses(F),
    {Cs1, NewVars} = lists:unzip(
		       lists:map(
			 fun (C) ->
				 C1 = generalise_expr_2(C, NodesToGen),
				 NewVars = refac_misc:collect_var_names(C1) --
					     refac_misc:collect_var_names(C),
				 {C1, NewVars}
			 end, Cs)),
    NewVars1 = refac_misc:remove_duplicates(lists:append(NewVars)),
    NewCs = [generalise_clause(C, NewVars1) || C <- Cs1],
    %% Here only count the new vars.
    {refac_prettypr:format(refac_syntax:function(FunName, NewCs)), length(NewVars)}.

generalise_clause(C, NewVars) ->
    Pats = refac_syntax:clause_patterns(C),
    NewPats = Pats ++ [refac_syntax:variable(V) || V <- NewVars],
    Guards = refac_syntax:clause_guard(C),
    Body = refac_syntax:clause_body(C),
    refac_syntax:clause(NewPats, Guards, Body).

generalise_expr_1(Expr, NodesToGen) when is_list(Expr)->
    refac_syntax:block_expr_body(generalise_expr_1(refac_syntax:block_expr(Expr), NodesToGen));
generalise_expr_1(Expr, NodesToGen) ->
    generalise_expr_2(Expr, NodesToGen).
   
generalise_expr_2(Expr, NodesToGen) ->
    element(1, ast_traverse_api:stop_tdTP(fun do_replace_expr_with_var_1/2, Expr, NodesToGen)).
 
do_replace_expr_with_var_1(Node, NodeVarPairs) ->
    case lists:keysearch(Node,1, NodeVarPairs) of
	{value, {Node, Var}}-> 
	    {refac_syntax:variable(Var), true};
	_ -> {Node, false}
    end.
    
get_var_define_pos(V) ->
    case  lists:keysearch(def,1, refac_syntax:get_ann(V)) of
	{value, {def, DefinePos}} -> DefinePos;
	false -> []
    end.
               

is_macro_name(Exp) ->
    case lists:keysearch(category, 1, refac_syntax:get_ann(Exp)) of
	{value, {category, {macro_name, _, _}}} ->
	    true;
	_ ->
	    false
    end.
 

