%% =====================================================================
%% Duplicated Code Detection.
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
%% =====================================================================

-module(refac_duplicated_code_in_files).

-export([duplicated_code_in_files/3, test/0]).

-compile(export_all).              %% should be removed later.

%% TODO:  
%% 1) does recusive function calls affect the result?
%% 2) does qualifed names affect the result? 
%% 3) ****when an erlang file does not compile, the detector should still return the result before filter_5.

%% ==================================================================================
%% @doc Find duplicated code in a Erlang source file.
%% <p> This function only reports the duplicated code fragments found in the current 
%% source file. It does not remove those duplicated code. Two parameters can be provided 
%% by the user to specify the minimum code clones to report, and they are:  
%% \emph{the  minimum number of lines of a code clone} and \emph{the minimum number of 
%% duplicated times}, the default values 2 for both parameters.
%% </p>
%% ====================================================================================
%% @spec duplicated_code(FileName::filename(),MinLines::integer(),MinClones::integer()) -> term().
%%  

-define(DEFAULT_MIN_CLONE_LEN, 5).
test() ->
    duplicated_code_in_files(["c:/distel-wrangler-0.2/wrangler/refac_duplicated_code.erl", 
			      "c:/distel-wrangler-0.2/wrangler/refac_duplicated_code_in_files.erl"], "50","2").


duplicated_code(FileName, MinLength1, MinClones1) ->
    MinLength = list_to_integer(MinLength1),
    MinClones = list_to_integer(MinClones1),
    {Toks, S} = tokenize(FileName),
    Tree = suffix_tree(alphabet()++"&",S++"&"),  %% '&' does not occur in program source.
    Cs=lists:flatten(lists:map(fun(B) -> collect_clones(MinLength,MinClones,B) end, Tree)),
    %% filter out sub-clones.
    Cs1 = filter_1(Cs),
    %% put atom names back, and then get the new result.
    Cs2 = filter_2(Cs1,Toks, MinLength, MinClones),
    %% filter our sub-clones again.
    Cs3 = filter_1(Cs2), 
    %% combine neighbouring clones.
    Cs4 = combine_neighbour_clones(Cs3, MinLength, MinClones),
     %% trim both ends of a clone to remove those parts that do not constitute a meaning syntax phrase.
    Cs5 = filter_5(Cs4, FileName, Toks, MinLength, MinClones),
    Cs6 = remove_sub_clones(Cs5),
    display_clones(Cs6),
    {ok, "Duplicated code detection finished."}.


duplicated_code_in_files(FileNames, MinLength1, MinClones1) ->
    MinLength = list_to_integer(MinLength1),
    MinClones = list_to_integer(MinClones1),
    T = lists:map(fun(F) -> {ok, Toks}= refac_epp:scan_file(F, [],[]),
			    Toks1 = lists:map(fun(T)->add_filename_to_token(F,T) end, Toks),
			    Toks1
		  end, FileNames),
    Toks = lists:concat(T),
    S = process_toks(Toks),
    Tree = suffix_tree(alphabet()++"&",S++"&"),  %% '&' does not occur in program source.
    Cs=lists:flatten(lists:map(fun(B) -> collect_clones(MinLength,MinClones,B) end, Tree)),
    %% filter out sub-clones.
    Cs1 = filter_1(Cs),
    Cs2 = filter_2(Cs1, Toks, MinLength, MinClones),
    Cs3 = filter_1(Cs2),
    Cs4 = combine_neighbour_clones(Cs3, MinLength, MinClones),
    Cs5 = filter_5(Cs4, FileNames, Toks, MinLength, MinClones),
    Cs6 = remove_sub_clones(Cs5),
    display_clones1(Cs6),
    {ok, "Duplicated code detection finished."}.
   
			  
%% =====================================================================
%% tokennization of an Erlang file.

tokenize(File) -> {ok, Toks} = refac_epp:scan_file(File, [],[]),
		  {Toks, process_toks(Toks)}.

process_toks(Toks) ->
    R = [T1 || T <- Toks, T1 <- [process_a_tok(T)]],
    lists:concat(R).

process_a_tok(T) -> 
    case T of 
	{var, _L, _V} -> 'V';       
	{atom, _L1, _V1} ->'A';     
	{integer, _L2,_V2} -> 'I';  
	{string, _L3, _V3} -> 'S';  
	{float, _, _} ->'F';        
	{char, _, _} ->'C' ;        
	{A, _} -> case lists:keysearch(A, 1, alphabet_1()) of 
		      {value, {A, Value}} -> Value;
		      _  -> erlang:error(io:format("Unhandled token:\n~p\n", [T]))
		  end
    end.

%% =====================================================================
%% Construction of suffix tree.

suffix_tree(Alpha, T) -> 
     Tree = suffix_tree_1(Alpha, length(T),suffixes(T)),
     post_process_suffix_tree(Tree).
  

suffix_tree_1(_Alpha, _Len, [[]]) -> leaf;
suffix_tree_1(Alpha, Len, Suffixes)->
     [{branch, {SubLens, 1+CP1 }, suffix_tree_1(Alpha, Len, SSR)} 
      || A <- Alpha, 
 	[SA | SSA] <- [select(Suffixes, A)],
 	Lens <- [lists:map(fun(S) -> length(S)+1 end, [SA|SSA])],
 	{CP1, SSR} <- [edge_cst([SA|SSA])],
        SubLens <- [lists:map(fun(L) -> {Len-L, Len-L+CP1} end, Lens)]].

select(SS,A) -> 
       [U || [C|U] <- SS, A==C].

suffixes(S=[_H|T]) -> [S|suffixes(T)];
suffixes([]) -> [].

edge_cst([S]) -> {length(S), [[]]};
edge_cst(AWSS=[[A|W]|SS]) ->
    case [0 || [H|_T] <- SS, H=/=A] of 
	[] -> {Cp1, Rss} = edge_cst([W | [ U || [_T1|U] <- SS]]),
	      {1+Cp1, Rss};
	_  -> {0, AWSS}
    end.

post_process_suffix_tree(Tree) ->
    NewTree1 = lists:map(fun(B) ->add_frequency(B) end, Tree),
    lists:map(fun(B)->extend_range(B) end, NewTree1).


% =====================================================================

%% Add the number of duplicated times to each branch.
add_frequency({branch, {Range, Len}, leaf}) 
               -> {branch, {Range, {Len, 1}}, leaf};
add_frequency({branch, {Range, Len}, Branches})
 	      -> Bs = lists:map(fun(B) -> add_frequency(B) end, Branches),
 		 F1 = lists:foldl(fun({branch,{_R, {_L, F}}, _C}, Sum)-> F + Sum end,0, Bs),
 		 {branch, {Range, {Len, F1}}, Bs}.


%% ===========================================================================
%% combine the ranges within two continuous branches if possible.
extend_range(SuffixTree) ->
    extend_range([],SuffixTree).
extend_range(_Prev_Range, {branch, {Range, {Len, Freq}}, leaf}) ->
     {branch, {Range, {Len,Freq}}, leaf};
extend_range(Prev_Range,{branch, {Range, {Len, Freq}}, Bs}) -> 
     ExtendedRange = lists:map(fun(R) -> combine_range(Prev_Range, R) end, Range),
     case overlapped_range(ExtendedRange) orelse
 	 (lists:subtract(ExtendedRange, Range) =/= ExtendedRange) of 
 	true -> Bs1=lists:map(fun(B) -> extend_range(Range, B) end, Bs), 
 		{branch, {Range, {Len, Freq}}, Bs1};
 	false -> Bs1 = lists:map(fun(B) -> extend_range(ExtendedRange,B) end, Bs),
 		 {S, E} = hd(ExtendedRange),
 		 {branch, {ExtendedRange, {E-S+1, Freq}}, Bs1}
     end.

overlapped_range(R) -> overlapped_range_1(lists:usort(R)).

overlapped_range_1([]) -> false;
overlapped_range_1([_R]) -> false;
overlapped_range_1([{_S, E},{S1, E1}|Rs]) -> (S1 < E) or overlapped_range_1([{S1,E1}|Rs]).
			
    
combine_range(Prev_Range, {StartLoc, EndLoc}) ->
    case lists:keysearch(StartLoc-1, 2, Prev_Range) of 
	{value, {StartLoc1, _EndLoc1}} ->
	    {StartLoc1, EndLoc};
	_  -> {StartLoc, EndLoc}
    end.    

%% =====================================================================
%% Collect those clones that satisfy the selecting criteria from the suffix trees.

%% Problem: here 'Len' refers to the no. of tokens instead of no. of lines.
collect_clones(MinLength, MinFreq, {branch, {Range, {Len, F}}, Others}) -> 
    MinLength1 = case (MinLength >=?DEFAULT_MIN_CLONE_LEN) of    
		    true -> ?DEFAULT_MIN_CLONE_LEN;
		    _ -> MinLength    %% in the case that the user-specified length is less t
		 end,		 
    C = case (F >= MinFreq) andalso (Len>= MinLength1) of 
	    true   -> [{Range,Len, F}];
	    false  -> []
	end,
    case Others of 
	leaf -> C;
	Bs -> lists:foldl(fun(B, C1)-> collect_clones(MinLength, MinFreq, B) ++ C1 end, C, Bs)
    end.
    

%% ==================================================================================
%% This phase filters out sub-clones.

filter_1(Cs) ->
    Cs1 =lists:sort(fun({_Range1, Len1, F1},{_Range2, Len2, F2})
			-> {Len1, F1} >= {Len2,F2}
		     end, Cs),
    filter_1_1(Cs1, [],[]).
filter_1_1([], Acc, _ExistingRanges) -> Acc;
filter_1_1([C={Range, _Len, _F}|Cs], Acc,ExistingRanges) -> 
     case lists:subtract(Range, ExistingRanges) of 
	[] -> filter_1_1(Cs, Acc, ExistingRanges);
	Range  ->
	    case lists:any(fun(E) -> sub_range(C, E) end, Acc) of   
		     true ->  filter_1_1(Cs, Acc, ExistingRanges);  %% C is a sub-clone of the existing clones.
		     false -> filter_1_1(Cs, Acc++[C], Range++ExistingRanges)
		 end;
	_  -> Acc1 = lists:map(fun({R, L, F}) ->
				       R1 = lists:subtract(Range, R),
				       case R1 =/= Range of 
					   true -> NewRange = lists:usort(Range++R),
						   {NewRange, L, length(NewRange)};
					   _  -> {R, L, F}
				       end
			       end, Acc),
	      filter_1_1(Cs, Acc1, ExistingRanges++Range)   
   end.

sub_range({Range1, Len1, F1}, {Range2, Len2, F2}) ->		       
    case (F1  =<F2) andalso (Len1=<Len2) of 
	true ->
            lists:all(fun({S, E}) -> lists:any(fun({S1,E1})-> (S1=<S) andalso (E=<E1) end, Range2) end, Range1);
	    false -> false
    end.   

%% ==================================================================================
%% This phase brings back those atoms back into the token stream, and get the resulted clones.

filter_2(Cs, Toks, MinLength, MinClones) ->
    Cs1 = lists:append(lists:map(fun({Range, _Len, _F}) ->
			    filter_2_1(Range, Toks, MinLength, MinClones) end, Cs)),
    simplify_filter_2_results(Cs1, [], MinLength, MinClones).
  
 
filter_2_1(Range, Toks, _MinLength, MinClones) ->
    ListsOfSubToks=lists:map(fun({S, E}) ->
			 Toks1 = lists:sublist(Toks, S+1, (E-S+1)),
		         remove_var_literals(Toks1) 
		      end, Range),
    ZippedToks = zip_list(ListsOfSubToks),
    Cs =filter_2_2([ZippedToks], []),
    [ C || C <- Cs, length(hd(C)) >= MinClones].
	    
	    
   %%  [ C || C <- Cs, length(C) >=MinLength, length(hd(C))>=MinClones].
    

filter_2_2([], Acc) -> Acc;
filter_2_2([ZippedToks|Others], Acc) ->
    {Cs1, Cs2} = lists:splitwith(fun(L) ->
	  length(lists:usort(fun(T1,T2) -> rm_loc_in_tok(T1)==rm_loc_in_tok(T2) end, L)) == 1 end, ZippedToks),    
    case Cs2 of 
	[] -> filter_2_2(Others, Acc++[Cs1]);
	_ ->  SubCs = lists:keysort(2,lists:zip(lists:seq(1, length(hd(Cs2)),1),hd(Cs2))),
	      SubCsIndexes =  lists:filter(fun(L)-> length(L) >=2 end, group_by(SubCs)),
	      Cs3 = get_sub_cs(ZippedToks, SubCsIndexes, []),
	      filter_2_2(Others++[tl(Cs2)]++Cs3, Acc++[Cs1])
    end.

group_by([]) -> [];
group_by(SortedList=[{_Seq, Tok}|_T]) -> 
    {Es1, Es2} =lists:splitwith(fun({_S,T})-> rm_loc_in_tok(T) == rm_loc_in_tok(Tok) end, SortedList),
    [Es1 | group_by(Es2)]. 
    

get_sub_cs(_ZippedToks, [], Acc) -> Acc;
get_sub_cs(ZippedToks, [H|L], Acc) ->
    H1 = lists:map(fun({A, _T}) -> A end, H),
    Cs1 = [[lists:nth(I, T) || I<-H1]|| T <-ZippedToks],
    get_sub_cs(ZippedToks, L, [Cs1]++Acc).


%% remove variables and literals (leaving atom names unchanged) from the token stream.
remove_var_literals(Toks) ->
    [remove_var_literals_1(T) || T <- Toks].

remove_var_literals_1(T) ->
    case T of 
	{var, L, _} -> {var, L, 'V'};
	{integer, L, _} -> {integer, L, 0};
	{string, L, _} -> {string, L, "_"};
	{float, L, _} -> {float, L, 0}; 
	{char, L, _}  -> {char, L, 'C'};
	{atom, L, V} -> {atom, L, V};
	{A, L} ->{A, L};
	Other  -> erlang:error(io:format("Unhandled token:\n~p\n", [Other]))
    end.    
    
%% use locations intead of tokens to represent the clones.
simplify_filter_2_results([],Acc, _MinLength, _MinClones) ->
    Acc;
simplify_filter_2_results([C|Cs], Acc, MinLength, MinClones) ->
   StartEndTokens = lists:zip(hd(C), lists:last(C)),
   Ranges = lists:map(fun({StartTok, EndTok}) ->
			      {token_loc(StartTok), token_loc(EndTok)} end, StartEndTokens),
   %% case (length(C) >= MinLines) andalso (length(hd(C))>=MinClones) of 
    case (length(hd(C)) >= MinClones) of 
       true -> simplify_filter_2_results(Cs, Acc++[{Ranges, length(C), length(StartEndTokens)}], MinLength, MinClones);
       _ ->   simplify_filter_2_results(Cs, Acc, MinLength, MinClones)
   end.

%% covered_lines(C) ->
%%     C1= lists:map(fun(E) ->hd(E) end, C),
%%     length(lists:usort(lists:map(fun(T) ->{Ln, _Col} = token_loc(T), Ln end, C1))).
    

%%===========================================================================

%% combine  clones which are next to each other. (ideally a fixpoint should be reached).
combine_neighbour_clones(Cs, MinLength, MinClones) ->
    Cs1 = remove_small_clones(combine_neighbour_clones(Cs, []), MinLength, MinClones),
    remove_sub_clones(Cs1).

combine_neighbour_clones([], Acc) -> Acc;
combine_neighbour_clones([C|Cs], Acc) ->
    C1 = case Acc of 
	     [] -> [C] ;
	     _ -> lists:map(fun(Clone)-> connect_clones(C, Clone) end, Acc)
	 end,
    C2 = lists:usort(C1),
    combine_neighbour_clones(Cs, Acc++C2).
			   

connect_clones(C1={Range1, Len1, F1}, {Range2, Len2, F2}) ->  %% assume F1=<F2.
     StartLocs1 = lists:map(fun({S,_E}) -> S  end, Range1),
     EndLocs1 = lists:map(fun({_S, E}) -> E end, Range1),
     StartLocs2 = lists:map(fun({S,_E}) -> S  end, Range2),
     EndLocs2 = lists:map(fun({_S, E}) -> E end, Range2),
     case F1 =< F2 of 
	 true -> case lists:subtract(StartLocs1, EndLocs2) of 
		     [] -> R1 = lists:filter(fun({_S, E}) -> lists:member(E, StartLocs1) end, Range2),
		           R2= lists:map(fun({S, _E}) -> S end, R1),
			   {lists:zip(R2, EndLocs1), Len1+Len2, F1};
		     _ -> case lists:subtract(EndLocs1, StartLocs2) of 
			      [] -> R3= lists:filter(fun({S,_E}) ->lists:member(S, EndLocs1) end, Range2),
				    R4 = lists:map(fun({_S,E}) -> E end, R3),
				    {lists:zip(StartLocs1, R4), Len1+Len2, F1};
			      _ -> C1
			  end
		 end; 
	 _  -> C1
     end.


remove_small_clones(Cs, MinLength, MinFreq) ->
    [{R, L, F} || {R, L, F} <-Cs, L >= MinLength, F >=MinFreq].
		   
 
remove_sub_clones(Cs) ->
    Cs1 = lists:sort(fun({_Range1, Len1, F1},{_Range2, Len2, F2})
			-> {F1, Len1} >= {F2, Len2}
		     end, Cs),
    remove_sub_clones(Cs1,[]).

remove_sub_clones([], Acc_Cs) ->
       Acc_Cs;
remove_sub_clones([C|Cs], Acc_Cs) ->
    R = lists:any(fun(C1)-> sub_range(C, C1) end, Acc_Cs),
    case R of 
	true ->remove_sub_clones(Cs, Acc_Cs);
	_ -> remove_sub_clones(Cs, Acc_Cs++[C])
    end.	     
		  
%% ================================================================================== 
%% trim both end of each clones to exclude those tokens that does not form a meaninhful syntax phrase.
%% This phase needs to get access to the abstract syntax tree.

compile_files(Files) ->
    compile_files(Files, []).
compile_files([], Acc) -> Acc; 
compile_files([F|Fs], Acc) -> 
    case refac_util:parse_annotate_file(F,2) of
	{ok, {AnnAST, _Info}} -> compile_files(Fs, [{F, AnnAST}|Acc]);
	{error, _Reason} -> compile_files(Fs, Acc)
    end.
	     
filter_5(Cs, FileNames, Toks, MinLength, MinClones) -> 
    AnnASTs = compile_files(FileNames),
    Cs2= lists:map(fun({Range, Len, F}) -> 
			   {{File, L1, C1}, {File, L2, C2}}= hd(Range),
			   S = {File, L1, C1},
                           E = {File, L2, C2},
			   case lists:keysearch(File, 1, AnnASTs) of 
			       {value, {File, AnnAST}} ->
				   Exprs = pos_to_tree(AnnAST, {{L1, C1},{L2, C2}}),
				   case Exprs =/= [] of 
				       true -> {{StartLn, StartCol}, _} = hd(Exprs),
					       {_, {EndLn, EndCol}} = lists:last(Exprs),			       
					       Expr1StartLoc = {File, StartLn, StartCol},
					       ExprnEndLoc = {File, EndLn, EndCol},
					       {StartLoc, EndLoc} = trim_ends({Expr1StartLoc, ExprnEndLoc}, Toks),
					       Toks1 =lists:dropwhile(fun(T) ->token_loc(T) =/=S end, Toks),
					       Toks11 = lists:takewhile(fun(T) ->token_loc(T) =/= StartLoc end, Toks1),
					       Toks2 = lists:dropwhile(fun(T) ->token_loc(T) =/= EndLoc end, Toks1),
					       Toks21 = lists:takewhile(fun(T) ->token_loc(T) =/= E end, Toks2),
					       {Len1, Len2} ={length(Toks11), length(Toks21)},
                                               NewRange = trim_range(tl(Range), {Len1, Len2}, Toks),
					       Toks3 = lists:dropwhile(fun(T) -> token_loc(T) =/= StartLoc end, Toks1),
					       Toks31= lists:takewhile(fun(T)-> token_loc(T) =/= EndLoc end, Toks3),
					       {true, {[{StartLoc, EndLoc}|NewRange], length(Toks31)+1, F}};
				       _ -> {false, {Range, Len, F}}
				   end;
			       false -> {false, {Range, Len, F}}
			   end			   
		   end, Cs),
    Cs3 =[{Range, Len, F} || {Cons, {Range, Len, F}} <- Cs2, Cons==true, Len>=MinLength, F>=MinClones],
    lists:usort(Cs3).
    


trim_range(Range, {Len1, Len2}, Toks) ->
    lists:map(fun({S,E}) -> trim_range_1({S,E}, {Len1, Len2}, Toks) end, Range).
trim_range_1({S,E}, {Len1, Len2}, Toks) ->
    Toks1 = lists:dropwhile(fun(T) -> token_loc(T) =/=S end, Toks),
    Toks2 = lists:nthtail(Len1, Toks1),
    Toks3 = lists:takewhile(fun(T) -> token_loc(T) =/=E end, Toks1) ++
	    [hd(lists:dropwhile(fun(T) ->token_loc(T) =/= E end,Toks1))],    
    Toks4 = lists:nthtail(Len2, lists:reverse(Toks3)),
   {token_loc(hd(Toks2)), token_loc(hd(Toks4))}.			    

trim_ends({StartLoc={File, Ln1, _Col1},EndLoc={File, _Ln2, _Col2}}, Toks) ->
    Toks0 = lists:dropwhile(fun(T) -> {F, _L, _C} = token_loc(T),
				      F =/= File end, Toks),
    Toks1 = lists:dropwhile(fun(T) -> {F, L, _C} = token_loc(T), 
				      ((F==File) andalso (L<Ln1)) end, Toks0),
    Toks2 = lists:takewhile(fun(T) -> token_loc(T) =/= StartLoc end, Toks1),
    StartLoc1 = case Toks2 of 
		    [] -> StartLoc;
		    _ -> Toks21 = lists:dropwhile(fun(T) ->
							  V = token_val(T),
							  (V == '(') or (V=='{') or (V=='[') or (V==',')
							      or (V==')') or (V=='}') or (V==']') end, 
						  lists:reverse(Toks2)),
			 case Toks21 of 
			     [] -> Toks3 = lists:dropwhile(fun(T) -> {F, L, _C} = token_loc(T),
								     (F==File) andalso (L==Ln1) end, Toks1),
				   token_loc(hd(Toks3));
			     _  -> case is_atom_or_var(hd(Toks21)) of 
				       true -> Toks3 = lists:dropwhile(fun(T) -> {F, L, _C} = token_loc(T),
										 (F==File) andalso (L==Ln1) end, Toks1),
					       token_loc(hd(Toks3));
				       false -> StartLoc
				   end
			 end
		end,
    %% TODO : TRIM THE END AS WELL???
    {StartLoc1, EndLoc}.

%% ===========================================================================


%% =====================================================================
%% display the found-out clones to the user.
display_clones(Cs) ->
   io:format("\nCode detection finished with *** ~p *** clone(s) found.\n", [length(Cs)]),
   display_clones_1(Cs).
display_clones_1([]) ->		      
    io:format("\n");
display_clones_1([{[{{StartLine, _StartCol}, {EndLine, _EndCol}}|Range], _Len, F}|Cs]) ->
    io:format("\nThe code between lines ~p-~p has been duplicated ~p time(s) at the following"
               " location(s):",[StartLine, EndLine, F-1]),
    display_clones_2(Range),
    display_clones_1(Cs).

display_clones_2([]) ->
      io:format("\n");
display_clones_2([{{StartLine, _StartCol}, {EndLine, _EndCol}}|Rs]) ->
    case Rs == [] of 
	true ->
	    io:format(" line ~p-~p.", [StartLine, EndLine]);
	false ->
	    io:format(" line ~p-~p,", [StartLine, EndLine])
    end,
    display_clones_2(Rs).


%% display the found-out clones to the user.
display_clones1(Cs) ->
   io:format("\nCode detection finished with *** ~p *** clone(s) found.\n", [length(Cs)]),
   display_clones1_1(Cs).
display_clones1_1([]) ->		      
    io:format("\n");
display_clones1_1([{[{{File, StartLine, _StartCol}, {File,EndLine, _EndCol}}|Range], _Len, F}|Cs]) ->
    io:format("\nThe code in ~p between lines: ~p-~p has been duplicated ~p time(s) at the following"
               " location(s):",[File, StartLine, EndLine, F-1]),
    display_clones1_2(Range),
    display_clones1_1(Cs).

display_clones1_2([]) ->
      io:format("\n");
display_clones1_2([{{File, StartLine, _StartCol}, {File, EndLine, _EndCol}}|Rs]) ->
    case Rs == [] of 
	true ->
	    io:format(" File: ~p, line ~p-~p.", [File, StartLine, EndLine]);
	false ->
	    io:format(" File: ~p, line ~p-~p,", [File, StartLine, EndLine])
    end,
    display_clones1_2(Rs).

%% =====================================================================
%% Some utility functions:

%% zip n lists.
zip_list(ListOfLists) ->    
    zip_list_1(ListOfLists, []).
		      
zip_list_1([[]|_T], Acc)  ->
    Acc;
zip_list_1(ListOfLists, Acc)->      
    zip_list_1([tl(L) || L <-ListOfLists], Acc ++ [[hd(L)|| L  <- ListOfLists]]).

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
	Other  -> erlang:error(io:format("Unhandled token:\n~p\n", [Other]))
    end.

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

add_filename_to_token(FileName,T) ->
    case T of 
	{C, {Ln,Col}, V} ->
	    {C,{FileName, Ln, Col}, V};
	{V, {Ln,Col}} ->
	    {V, {FileName, Ln, Col}}
    end.
	     
	     
				
%% returns true if a token represents an atom or a variable.      
is_atom_or_var(T) ->  
    case T of
	{var, _L, _V} -> true;
	{atom, _L1, _V1} ->true;    
	_  -> false
    end. 

concat_tokens(Toks) ->
    F = fun(T) -> case T of
		      {var, _L, V} -> V;
		      {atom, _L1, V1} -> V1;
		      {integer, _L2, V2} -> V2;
		      {string, _L3, V3} -> V3;
		      {float, _, V4} -> V4;
		      {char, _, V5} -> V5;
		      {A, _} -> A
		  end
	end,
    lists:concat(lists:map(F, Toks)).

is_valid_syntax_phrase(Node) ->
    case refac_syntax:type(Node) of 
	list -> true;
	application -> true;
	case_expr -> true;
	clause -> true;
	cath_expr -> true;
	if_expr -> true;
	cond_expr -> true;
	infix_expr -> true;
	prefix_expr -> true;
	conjunction -> true;
	disjunction -> true;
	funtion -> true;
	fun_expr -> true;
	list_comp ->true;
	block_expr ->true;
	receive_expr ->true;
	match_expr -> true;
	form_list -> true;
	_ -> false
    end.

pos_to_tree(Tree, {Start, End}) ->
    {S, E} = refac_util:get_range(Tree),
    if (S >= Start) and (E =< End) ->
	    case is_valid_syntax_phrase(Tree) of %% orelse refac_util:is_pattern(Tree) of
	     true -> [{S,E}];
	     _ ->
		 Ts = refac_syntax:subtrees(Tree),
		 R0 = [[pos_to_tree(T, {Start, End}) || T <- G]
		       || G <- Ts],
		 lists:flatten(R0)
	   end;
       (S > End) or (E < Start) -> [];
       (S < Start) or (E > End) ->
	   Ts = refac_syntax:subtrees(Tree),
	   R0 = [[pos_to_tree(T, {Start, End}) || T <- G]
		 || G <- Ts],
	   lists:flatten(R0);
       true -> []
    end.

%% =====================================================================
%% get the alphabet on which the suffix tree is going to be built.
alphabet() ->
     lists:concat(lists:map(fun({_X, Y}) -> Y end, alphabet_1())) ++ "ACFISV".
		  
alphabet_1() ->
   [{'after',a},{'andalso',b}, {'and',c},
    {'begin',d},{'bnot',e},{'band',f},{'bor',g},{'bxor',h},{'bsl',i},{'bsr',j},
    {'case',k}, {'cond',l}, {'catch',m},
    {'div',n},{'dot', o},
    {'end',p},
    {'fun',q},
    {'if',r},
    {'let',s},
    {'not',t}, 
    {'orelse',u},{'of',v}, {'or',w},
    {'query',x},
    {'receive',y}, {'rem',z},
    {'try','B'},
    {'when','D'},
    {'xor','E'},     
    {'<<', 'G'},
    {'<-', 'H'},
    {'<=', 'J'},
    {'>>', 'K'},
    {'>=', 'L'},
    {'->', 'M'},
    {'--', 'N'},
    {'++', 'O'},
    {'=:=', 'P'},
    {'=/=', 'Q'},
    {'=<',  'R'},
    {'==',  'T'},
    {'/=',  'U'},
    {'||',  'W'},
    {':-',  'X'},
    {'(','('},
    {')',')'}, 
    {'{','{'},
    {'}','}'},
    {'[', '['},
    {']', ']'},
    {'.', '.'},
    {':',':'},
    {'|','|'},
    {';',';'},
    {',',','},
    {'?','?'},
    {'#','#'},
    {'+','+'},
    {'-','-'},
    {'*','*'},
    {'/','/'},
    {'<','<'},
    {'>','>'},
    {'=','='},
    {'!','!'}].
%% =====================================================================

     


		    
    
				    
				    
    
    
