%% =====================================================================
%% Duplicated Code Detection.
%%
%% Copyright (C) 2006-2009  Huiqing Li, Simon Thompson

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

-export([duplicated_code/4, duplicated_code_eclipse/5]).

-export([duplicated_code_1/4]).

-export([init/1, collect_vars/1]).

-export([get_clones_by_suffix_tree/6,
	 get_clones_by_erlang_suffix_tree/5, 
	 display_clone_result/2, remove_sub_clones/1]).

-compile(export_all).
-import(refac_sim_expr_search, [start_counter_process/0, stop_counter_process/1, gen_new_var_name/1,variable_replaceable/1]).

%% TODO:  
%% 1) does recusive function calls affect the result?
%% 2) does qualifed names affect the result? 
%% 3) ****when an erlang file does not compile, the detector should still return the result before trim_clones.


-include("../include/wrangler.hrl").

%% minimal number of tokens.
-define(DEFAULT_CLONE_LEN, 20).

%% Minimal number of class members.
-define(DEFAULT_CLONE_MEMBER, 2).


%%-define(DEBUG, true).

-ifdef(DEBUG).
-define(debug(__String, __Args), ?wrangler_io(__String, __Args)).
-else.
-define(debug(__String, __Args), ok).
-endif.

start_suffix_tree_clone_detector() ->
    SuffixTreeExec = filename:join(?WRANGLER_DIR,"bin/suffixtree"),
    ?debug("suffixTreeExec:\n~p\n", [SuffixTreeExec]),
    start(SuffixTreeExec).


start(ExtPrg) ->
    process_flag(trap_exit, true),
    Pid= spawn_link(?MODULE, init, [ExtPrg]),
    ?debug("Pid:\n~p\n", [Pid]),
    Pid.

stop_suffix_tree_clone_detector() -> case catch (?MODULE ! stop) of 
					 _ -> ok
				     end.


get_clones_by_suffix_tree(Dir, ProcessedToks,MinLength, MinClones, Alphabet, AllowOverLap) ->
    start_suffix_tree_clone_detector(),
    OutFileName = filename:join(Dir, "wrangler_suffix_tree"), 
    case file:write_file(OutFileName, ProcessedToks) of 
	ok -> case  catch call_port({get, MinLength, MinClones, AllowOverLap, OutFileName}) of
		  {ok, _Res} ->
		      ?debug("Initial clones are calculated using C suffixtree implementation.\n", []),
		      stop_suffix_tree_clone_detector(),
		      {ok, Res} = file:consult(OutFileName), 
		      file:delete(OutFileName),
		      case Res of 
			  [] -> []; 
			  [Cs] -> Cs
		      end;
		  _E -> ?debug("Reason:\n~p\n", [_E]),
			stop_suffix_tree_clone_detector(),
			file:delete(OutFileName),
			get_clones_by_erlang_suffix_tree(ProcessedToks, MinLength, MinClones, Alphabet, AllowOverLap)
		    
	      end;	
	_ ->  get_clones_by_erlang_suffix_tree(ProcessedToks, MinLength, MinClones, Alphabet, AllowOverLap)
    end.

get_clones_by_erlang_suffix_tree(ProcessedToks, MinLength, MinClones, Alphabet, AllowOverLap) ->
    ?wrangler_io("\nWrangler failed to use the C suffixtree implementation;"
                 " Initial clones are calculated using Erlang suffixtree implementation.\n",[]),
    Tree = suffix_tree(Alphabet, ProcessedToks ++ "&", AllowOverLap),
    Cs = lists:flatten(lists:map(fun (B) ->
					 collect_clones(MinLength, MinClones, B)
				 end,
				 Tree)),
    remove_sub_clones(Cs).
   
    
call_port(Msg) ->
    (?MODULE) ! {call, self(), Msg},
    receive Result -> Result end.

init(ExtPrg) ->
    case whereis(?MODULE) of 
	undefined -> ok;
	_ -> unregister(?MODULE)
    end,
    register(?MODULE, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg}, [{packet, 2}, binary, exit_status]),
    ?debug("Port:~p\n", [Port]),
    loop(Port).

loop(Port) ->
    receive
    {call, Caller, Msg} ->
	    ?debug("Calling port with ~p~n", [Msg]),
	    erlang:port_command(Port, term_to_binary(Msg)),
	    receive
		{Port, {data, Data}} ->
		    ?debug("Data received:~p~n", [{data, binary_to_term(Data)}]),
		    Caller ! binary_to_term(Data);
		{Port, {exit_status, Status}} when Status > 128 ->
		    ?debug("Port terminated with signal: ~p~n", [Status-128]),
		    exit({port_terminated, Status});
		{Port, {exit_status, Status}} ->
		    ?debug("Port terminated with status: ~p~n", [Status]),
		    exit({port_terminated, Status});
		{'EXIT', Port, Reason} ->
		    exit(Reason)
	    end,
	    loop(Port);
    stop ->
        erlang:port_close(Port)
    end.

 

%% ==================================================================================
%% @doc Find duplicated code in a Erlang source files.
%% <p> This function only reports the duplicated code fragments found. It does not actually 
%% remove those duplicated code. Two parameters can be provided 
%% by the user to specify the minimum code clones to report, and they are:  
%% \emph{the minimum number of lines of a code clone} and \emph{the minimum number of 
%% duplicated times}, the default values 2 for both parameters.
%% </p>
%% ====================================================================================

%%-spec(duplicated_code_eclipse/5::(dir(), integer(), integer(), integer(),  dir()) ->
%% 	     [{[{{filename(), integer(), integer()},{filename(), integer(), integer()}}], integer(), integer()}]).

duplicated_code_eclipse(DirFileList, MinLength1, MinClones1, TabWidth, SuffixTreeExec) ->
     MinLength = case MinLength1 =< 1 of
		     true -> 
			 ?DEFAULT_CLONE_LEN;
		     _ -> MinLength1
		end,
    MinClones = case MinClones1< ?DEFAULT_CLONE_MEMBER of
		    true -> ?DEFAULT_CLONE_MEMBER;
		    _ -> MinClones1
		end,
    start(SuffixTreeExec),
    Cs5= duplicated_code_detection(DirFileList, MinClones, MinLength, TabWidth),
    remove_sub_clones(Cs5).
    
duplicated_code(DirFileList, MinLength1, MinClones1, TabWidth) ->
    MinLength = try 
		    case MinLength1 == [] orelse
			list_to_integer(MinLength1) =< 1
			of
			true -> 
			    ?DEFAULT_CLONE_LEN;
			_ -> list_to_integer(MinLength1)
		    end
		catch
		    Val -> Val;
		      _:_ -> throw({error, "Parameter input is invalid."})
		end,
        %% By 'MinClones', I mean the minimal number of members in a clone class,
    %% therefore it should be one more than the number of times a piece of code is cloned.
    MinClones = try 
		    case MinClones1 == [] orelse
			list_to_integer(MinClones1) < ?DEFAULT_CLONE_MEMBER
			of
			true -> ?DEFAULT_CLONE_MEMBER;
			_ -> list_to_integer(MinClones1)
		    end
		catch
		    V -> V;
		      _:_ ->throw({error, "Parameter input is invalid."})
		end,
    ?wrangler_io("\nCMD: ~p:duplicated_code(~p,~p,~p,~p).\n", [?MODULE, DirFileList, MinLength1, MinClones1, TabWidth]),
    ?debug("current time:~p\n", [time()]),
    Cs5 = duplicated_code_detection(DirFileList, MinClones, MinLength, TabWidth),
    ?debug("Filtering out sub-clones.\n", []),
    Cs6 = remove_sub_clones(Cs5),
    ?debug("current time:~p\n", [time()]),
    display_clone_result(Cs6, "Duplicated"),
    ?debug("No of Clones found:\n~p\n", [length(Cs6)]),
    ?debug("Clones:\n~p\n", [Cs6]),
    {ok, "Duplicated code detection finished."}.

%%-spec(duplicated_code_1/4::(dir(), [integer()], [integer()], integer()) ->
%%	     [{[{{filename(), integer(), integer()},{filename(), integer(), integer()}}], integer(), integer()}]).

duplicated_code_1(DirFileList, MinLength, MinClones, TabWidth) ->
    Cs5 = duplicated_code_detection(DirFileList, MinClones, MinLength, TabWidth),
    remove_sub_clones(Cs5).
 

duplicated_code_detection(DirFileList, MinClones, MinLength, TabWidth) ->
    FileNames = refac_util:expand_files(DirFileList, ".erl"),
    ?debug("Files:\n~p\n", [FileNames]),
    ?debug("Constructing suffix tree and collecting clones from the suffix tree.\n", []),
    {Toks, ProcessedToks} = tokenize(FileNames, TabWidth),
    Dir = filename:dirname(hd(FileNames)),
    Cs= get_clones_by_suffix_tree(Dir, ProcessedToks,MinLength, MinClones,alphabet() ++ "&", 0),
    ?debug("Initial numberclones from suffix tree:~p\n", [length(Cs)]),
    %% This step is necessary to reduce large number of sub-clones.
    ?debug("Type 4 clones:\n~p\n", [length(Cs)]),
    ?debug("Putting atoms back.\n",[]),
    Cs1 = clones_with_atoms(Cs, Toks, MinLength, MinClones),
    ?debug("Filtering out sub-clones.\n", []),
    Cs2 = remove_sub_clones(Cs1),
    ?debug("Combine with neighbours\n",[]),
    Cs3 = combine_neighbour_clones(Cs2, MinLength, MinClones),
    ?debug("Type3 without trimming:~p\n", [length(Cs3)]),
    ?debug("Trimming clones.\n", []),
    trim_clones(FileNames, Cs3, MinLength, MinClones, TabWidth).
    


%% =====================================================================
%% tokenize a collection of concatenated Erlang files.

tokenize(FileList, TabWidth) ->
    TokLists = lists:map(fun(F) -> Toks= refac_util:tokenize(F, false, TabWidth),
			    lists:map(fun(T)->add_filename_to_token(F,T) end, Toks)			    
		  end, FileList),
    Toks = lists:append(TokLists),
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
	{A, _} -> case lists:keysearch(A, 1, alphabet_1()) of
		      {value, {A, Value}} -> Value;
		      _  -> ' '
		  end
    end.

%% =====================================================================
%% Construction of suffix tree.
suffix_tree(Alpha, T, AllowOverLap) -> 
    Tree = suffix_tree_1(Alpha, length(T),suffixes(T)),
    post_process_suffix_tree(Tree, AllowOverLap).
  

suffix_tree_1(_Alpha, _Len, [[]]) -> leaf;
suffix_tree_1(Alpha, Len, Suffixes)->
     [{branch, {SubLens, 1+CP1 }, suffix_tree_1(Alpha, Len, SSR)} 
      || A <- Alpha, 
 	[SA | SSA] <- [select(Suffixes, A)],
 	Lens <- [lists:map(fun(S) -> length(S)+1 end, [SA|SSA])],
 	{CP1, SSR} <- [edge_cst([SA|SSA])],
        SubLens <- [lists:map(fun(L) -> {Len-L+1, Len-L+CP1+1} end, Lens)]].

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

post_process_suffix_tree(Tree, AllowOverLap) ->
    NewTree1 = lists:map(fun(B) ->add_frequency(B) end, Tree),
    lists:map(fun(B)->extend_range(B, AllowOverLap) end, NewTree1).


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
extend_range(SuffixTree, AllowOverLap) ->
    extend_range([],SuffixTree, AllowOverLap).

extend_range(Prev_Range, {branch, {Range, {_Len, Freq}}, leaf}, _AllowOverLap) ->
    ExtendedRange = lists:map(fun (R) -> combine_range(Prev_Range, R) end, Range),
    {S, E} = hd(ExtendedRange),
    {branch, {ExtendedRange, {E-S+1, Freq}}, leaf};
extend_range(Prev_Range, {branch, {Range, {Len, Freq}}, Bs}, AllowOverLap) ->
    ExtendedRange = lists:map(fun (R) -> combine_range(Prev_Range, R) end, Range),
    case (AllowOverLap==0) andalso overlapped_range(ExtendedRange) of 
	true ->  Bs1 = lists:map(fun (B) -> extend_range(Range, B, AllowOverLap) end, Bs),
		 {branch, {Range, {Len, Freq}}, Bs1};
	_ -> 
	    Bs1 = lists:map(fun (B) -> extend_range(ExtendedRange, B, AllowOverLap) end, Bs),
	    {S, E} = hd(ExtendedRange),
	    {branch, {ExtendedRange, {E - S + 1, Freq}}, Bs1}
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
    C = case F >= MinFreq andalso Len >= MinLength of
	    true -> [{Range, Len, F}];
	    false -> []
	end,
    case Others of
	leaf -> C;
	Bs -> lists:foldl(fun (B, C1) -> collect_clones(MinLength, MinFreq, B) ++ C1 end, C, Bs)
    end.
    

%% ==================================================================================
%% This phase brings back those atoms back into the token stream, and get the resulted clones.

clones_with_atoms(Cs, Toks, MinLength, MinClones) ->
    Cs1 = lists:append(lists:map(fun({Range, _Len, _F}) ->
			    clones_with_atoms_1(Range, Toks, MinLength, MinClones) end, Cs)),
    Cs2 = simplify_filter_results(Cs1, [], MinLength, MinClones),
    [{R, L, F} || {R, L, F} <-Cs2, L >= MinLength, F >=MinClones].

clones_with_atoms_1(Range, Toks, _MinLength, MinClones) ->
    ListsOfSubToks = lists:map(fun ({S, E}) ->
				       Toks1 = lists:sublist(Toks, S, E - S),
				       remove_var_literals(Toks1)
			       end,
			       Range),
    ZippedToks = zip_list(ListsOfSubToks),
    Cs = clones_with_atoms_2([ZippedToks], []),
    [C || C <- Cs, length(hd(C)) >= MinClones].
	    
 

clones_with_atoms_2([], Acc) -> Acc;
clones_with_atoms_2([ZippedToks|Others], Acc) ->
    {Cs1, Cs2} = lists:splitwith(fun(L) ->
	  length(lists:usort(fun(T1,T2) -> rm_loc_in_tok(T1)==rm_loc_in_tok(T2) end, L)) == 1 end, ZippedToks),    
    case Cs2 of 
	[] -> clones_with_atoms_2(Others, Acc++[Cs1]);
	_ ->  SubCs = lists:keysort(2,lists:zip(lists:seq(1, length(hd(Cs2)),1),hd(Cs2))),
	      SubCsIndexes =  lists:filter(fun(L)-> length(L) >=2 end, group_by(SubCs)),
	      Cs3 = get_sub_cs(ZippedToks, SubCsIndexes, []),
	      clones_with_atoms_2(Others++[tl(Cs2)]++Cs3, Acc++[Cs1])
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


remove_var_literals(Toks) -> [remove_var_literals_1(T)||T<-Toks].
remove_var_literals_1(T) ->
     case T of 
	 {var, L, _} -> {var, L, 'V'};
	 {integer, L, _} -> {integer, L, 0};
	 {string, L, _} -> {string, L, "_"};
	 {float, L, _} -> {float, L, 0}; 
	 {char, L, _}  -> {char, L, 'C'};
	 {A, L} ->{A, L};
	 Other  -> Other
     end.

%% use locations intead of tokens to represent the clones.
simplify_filter_results([], Acc, _MinLength, _MinClones) -> Acc;
simplify_filter_results([C | Cs], Acc, MinLength, MinClones) ->
    StartEndTokens = lists:zip(hd(C), lists:last(C)),
    Ranges = lists:map(fun ({StartTok, EndTok}) -> {token_loc(StartTok), token_loc(EndTok)} end, StartEndTokens),
    %% case (length(C) >= MinLines) andalso (length(hd(C))>=MinClones) of
    case length(hd(C)) >= MinClones of
      true -> simplify_filter_results(Cs, Acc ++ [{Ranges, length(C), length(StartEndTokens)}], MinLength, MinClones);
      _ -> simplify_filter_results(Cs, Acc, MinLength, MinClones)
    end.


%%===========================================================================

%% combine clones which are next to each other. (ideally a fixpoint should be reached).
combine_neighbour_clones(Cs, MinLength, MinClones) ->
     Cs1 = combine_neighbour_clones(Cs, []),
     Cs2= [{R, L, F} || {R, L, F} <-Cs1, L >= MinLength, F >=MinClones],
     remove_sub_clones(Cs2).
		   
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
 	 true
 	   -> case lists:subtract(StartLocs1, EndLocs2) of 
 		  [] -> R1 = lists:filter(fun({_S, E}) -> lists:member(E, StartLocs1) end, Range2),
 			R2= lists:map(fun({S, _E}) -> S end, R1),
 			{lists:zip(R2, EndLocs1), Len1+Len2, F1};
 		  _  -> 
 		      case lists:subtract(EndLocs1, StartLocs2) of 
 			  [] -> R3= lists:filter(fun({S,_E}) ->lists:member(S, EndLocs1) end, Range2),
 				R4 = lists:map(fun({_S,E}) -> E end, R3),
 				{lists:zip(StartLocs1, R4), Len1+Len2, F1};
 			  _ -> C1
 		      end
 	      end; 
 	 _  -> C1
      end.


%%================================================================================

remove_sub_clones(Cs) ->
    Cs1 = lists:sort(fun(C1,C2)
			-> {element(3, C1), element(2, C1)} >= {element(3, C2), element(2, C2)}
		     end, Cs),
    remove_sub_clones(Cs1,[]).

remove_sub_clones([], Acc_Cs) ->
    Acc_Cs;
remove_sub_clones([C|Cs], Acc_Cs) ->
    R = lists:any(fun(C1)-> sub_clone(C, C1) end, Acc_Cs),
    case R of 
	true ->remove_sub_clones(Cs, Acc_Cs);
	_ -> remove_sub_clones(Cs, Acc_Cs++[C])
    end.

sub_clone(C1, C2) ->
    Range1 = element(1, C1),
    Len1 = element(2, C1),
    F1 = element(3, C1),
    Range2 = element(1, C2),
    Len2 = element(2, C2),
    F2 = element(3, C2),
    case F1=<F2 andalso Len1=<Len2 of 
	true ->
	    lists:all(fun ({S, E}) -> 
			      lists:any(fun ({S1, E1}) ->
						S1 =< S andalso E =< E1 
					end, Range2) 
		      end, Range1);
	false ->
	    false
    end.

		  
%% ================================================================================== 
%% trim both end of each clones to exclude those tokens that does not form a meaninhful syntax phrase.
%% This phase needs to get access to the abstract syntax tree.
compile_files(Files, TabWidth) ->
    compile_files(Files, TabWidth,[]).
compile_files([], _, Acc) -> Acc; 
compile_files([F|Fs], TabWidth, Acc) -> 
    {ok, {AnnAST, _Info}} =  refac_util:parse_annotate_file(F,true,[], TabWidth),
    compile_files(Fs, TabWidth, [{F, AnnAST}|Acc]).

     
tokenize_files(Files, WithLayout, TabWidth) ->
    tokenize_files(Files, WithLayout, TabWidth, []).
tokenize_files([],_, _, Acc) ->
     Acc;
tokenize_files([F|Fs], WithLayout, TabWidth, Acc) ->
     Toks= refac_util:tokenize(F, WithLayout, TabWidth),
     tokenize_files(Fs, WithLayout, TabWidth, [{F, Toks}|Acc]).
    
   
trim_clones(FileNames, Cs, MinLength, MinClones, TabWidth) -> 
    AnnASTs = compile_files(FileNames, TabWidth),
    ToksLists = tokenize_files(FileNames, false, TabWidth),
    Fun0 = fun(R={{File, StartLn, StartCol},{File, EndLn, EndCol}})->
		  case lists:keysearch(File, 1, AnnASTs) of
		      {value, {File, AnnAST}} -> 
			  case refac_util:pos_to_fun_def(AnnAST, ({StartLn+ (EndLn-StartLn) div 2, StartCol})) of
			      {ok, FunDef} ->
				  Phrases =  refac_util:pos_to_syntax_units(FunDef, {StartLn, StartCol}, {EndLn, EndCol}, fun is_expr_or_fun/1),
				  BdStruct = refac_expr_search:var_binding_structure(Phrases),
				  VarsToExport = refac_sim_expr_search:vars_to_export(FunDef, {EndLn, EndCol}, Phrases),
			%% 	  refac_io:format("VarsToExport:\n~p\n", [VarsToExport]),
				  {R, Phrases, VarsToExport, BdStruct};	   
			      {error, _} ->
				  {R, [],[],[]}
			  end;
		      _  -> {R, [], [],[]}
		  end
	  end,
    Fun = fun ({Range, _Len, F}) ->
		  {{File1, L1, C1}, {File2, L2, C2}}= hd(Range),
		  case File1 =/= File2 of 
		      true -> [];
		      _ ->  S = {L1, C1},
			    E = {L2, C2},
			    case lists:keysearch(File1, 1, AnnASTs) of
				{value, {File1, AnnAST}} ->
				    {value, {File1, Toks}} = lists:keysearch(File1, 1, ToksLists),
				    Units = refac_util:pos_to_syntax_units(File1, AnnAST, {L1, C1},{L2, C2}, fun is_expr_or_fun/1, TabWidth), 
				    case Units =/= [] of 
					true -> 
					    Fun2 = fun(U) ->
							   {StartLoc, _} = refac_util:get_range(hd(U)),
							   {_, EndLoc} = refac_util:get_range(lists:last(U)),
							   Toks1 =lists:dropwhile(fun(T) ->token_loc(T)=< S end, Toks),
							   Toks11 = lists:takewhile(fun(T) ->token_loc(T) =< StartLoc end, Toks1),
							   Toks2 = lists:dropwhile(fun(T) ->token_loc(T) =< EndLoc end, Toks1),
							   Toks21 = lists:takewhile(fun(T) ->token_loc(T) =< E end, Toks2),
							   {Len1, Len2} ={length(Toks11), length(Toks21)},
							   Toks3 = lists:dropwhile(fun(T) -> token_loc(T) < StartLoc end, Toks1),
							   Toks31= lists:takewhile(fun(T)-> token_loc(T) =< EndLoc end, Toks3),
							   NewLen = length(Toks31),
							   case NewLen >=MinLength of 
							       true ->  
								   NewRange = trim_range(tl(Range), {Len1, Len2}, TabWidth),
								   {StartLn, StartCol} = StartLoc,
								   {EndLn, EndCol} = EndLoc,
								   [{[{{File1, StartLn, StartCol}, {File1, EndLn, EndCol}}|NewRange], NewLen, F}];
							       false -> 
								   []
							   end
						   end,
					      lists:append(lists:map(Fun2, Units));
					_ -> []
				    end;
				false -> []
			    end	
		  end
	  end,
    Cs2= lists:append(lists:map(Fun, Cs)),
    Cs3 =[lists:map(fun(C) -> {C, Len, length(C)} end, group_by(4, lists:map(Fun0, Range)))
		    || {Range, Len, _F}<- Cs2],
    Cs4 = lists:append(Cs3),
    Cs5 =[{lists:unzip([{R, {Code, EVs}}||{R,Code, EVs, _Bd} <-Range]), Len, F} 
	  || {Range, Len, F} <- Cs4, Len>=MinLength, F>=MinClones],
    Cs6 = [try get_anti_unifier(CodeEVsPairs) of 
	       Res -> [{Range, Len, F, Res}]
	   catch
	       _Error_ -> []
	   end ||{{Range, CodeEVsPairs}, Len, F} <-Cs5],
    lists:usort(lists:append(Cs6)).


group_by(N, TupleList) ->
    group_by_1(N, lists:keysort(N, TupleList)).

group_by_1(_N, []) -> [];
group_by_1(N, TupleList=[E|_Es]) ->
    NthEle = element(N, E),
    {E1,E2} = lists:splitwith(fun(T) -> element(N,T) == NthEle end, TupleList),
    [E1 | group_by_1(N, E2)].
    
    
trim_range(Range, {Len1, Len2}, TabWidth) ->
    lists:flatmap(fun({S,E}) -> trim_range_1({S,E}, {Len1, Len2}, TabWidth) end, Range).
trim_range_1(_Range={{File1, StartLn, StartCol}, {File2, EndLn, EndCol}}, {Len1, Len2}, TabWidth) -> 
    case (File1==File2) andalso ({StartLn, StartCol}<{EndLn, EndCol}) of   %% The second condition is a temporary bugfix. 
	true ->
	    S = {StartLn, StartCol},
	    E = {EndLn, EndCol},
	    Toks =refac_util:tokenize(File1, false, TabWidth),
	    Toks1 = lists:dropwhile(fun(T) -> token_loc(T) =/=S end, Toks),
	    case Len1 <length(Toks1) of   
		true -> Toks2 = lists:nthtail(Len1, Toks1),
			{StartLn1, StartCol1} = token_loc(hd(Toks2)),
			{Toks3, _Toks4} = lists:splitwith(fun(T) -> token_loc(T) =< E end, Toks1),
			case Len2< length(Toks3) of 
			    true ->
				LastTok = hd(lists:nthtail(Len2, lists:reverse(Toks3))),
				{L, C} = token_loc(LastTok),
				{EndLn1, EndCol1} = {L, C+token_len(LastTok)-1},
				[{{File1, StartLn1, StartCol1}, {File2, EndLn1, EndCol1}}];
			    _ -> [] %% This should not happen, but it does very rarely; need to find out why.
			end;			    
		_ -> []  %% ditto.
	    end;
	_ -> []
    end.

token_len(T) ->
    V = token_val(T),
    token_len_1(V).

token_len_1(V) when is_atom(V) ->
    length(atom_to_list(V));
token_len_1(V) when is_integer(V) ->
    length(integer_to_list(V));
token_len_1(V) when is_list(V)->
     length(V);
token_len_1(V) when is_float(V) ->
    %% this is not what I want; but have not figure out how to do it.
    length(float_to_list(V)); 
token_len_1(V) when is_binary(V) ->
    length(binary_to_list(V));
token_len_1(_V) -> 1.  %5 unhandled token;
       
%% ===========================================================
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
	Other  -> erlang:error(?wrangler_io("Unhandled token:\n~p\n", [Other]))
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
	    				

is_expr_or_fun(Node) ->
    case refac_syntax:type(Node) of 
	function -> true;
	_ -> refac_util:is_expr(Node) 
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
    {'spec', 'Y'},
    {'::', 'Z'},
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



%% ===========================================================================
%% display the found-out clones to the user.

display_clone_result(Cs, Str) ->
    case length(Cs) >=1  of 
	true -> display_clones_by_freq(Cs, Str),
		display_clones_by_length(Cs, Str),
		?wrangler_io("\n\n NOTE: Use 'M-x compilation-minor-mode' to make the result mouse clickable if this mode is not already enabled.\n\n",[]);	
	false -> ?wrangler_io("\n"++Str++" code detection finished with no clones found.\n", [])
    end.
    
display_clones_by_freq(Cs, Str) ->
    ?wrangler_io("\n===================================================================\n",[]),
    ?wrangler_io(Str++" Code Detection Results Sorted by the Number of Code Instances.\n",[]),
    ?wrangler_io("======================================================================\n",[]),		 
    Cs1 = lists:sort(fun({_Range1, _Len1, F1, _},{_Range2, _Len2, F2,_})
			-> F1 >= F2
		     end, Cs),
    ?wrangler_io(display_clones(Cs1, Str),[]).

display_clones_by_length(Cs, Str) ->
    ?wrangler_io("\n===================================================================\n",[]),
    ?wrangler_io(Str ++ " Code Detection Results Sorted by Code Size.\n",[]),
    ?wrangler_io("======================================================================\n",[]),		 
    Cs1 = lists:sort(fun({_Range1, Len1, _F1, _},{_Range2, Len2, _F2,_})
			-> Len1 =< Len2
		     end, Cs),
    ?wrangler_io(display_clones(Cs1, Str),[]).


%% display the found-out clones to the user.
display_clones(Cs, Str) ->
    Num = length(Cs),
    ?wrangler_io("\n"++Str++" detection finished with *** ~p *** clone(s) found.\n", [Num]),
    case Num of 
	0 -> ok;
	_ -> display_clones_1(Cs)
    end.


display_clones_1(Cs) ->
    ?wrangler_io(display_clones_1(Cs, ""),[]).

display_clones_1([], Str) -> Str ++ "\n";
display_clones_1([{Ranges, _Len, F, Code}| Cs], Str) ->
    [{{File, StartLine, StartCol}, {File, EndLine, EndCol}}| Range] = lists:keysort(1, Ranges),
    NewStr = compose_clone_info({File, StartLine, StartCol}, {File, EndLine, EndCol}, F, Range, Str),
    NewStr1 = NewStr ++ "The cloned expression/function after generalisation:\n\n"++ io_lib:format("~s", [Code]) ++ "\n",
    ?wrangler_io(NewStr1, []),
    display_clones_1(Cs, "").

compose_clone_info({File, StartLine, StartCol}, {File, EndLine, EndCol}, F, Range, Str) ->
    case F - 1 of
	1 -> Str1 = Str ++ "\n" ++ File ++ io_lib:format(":~p.~p-~p.~p: This code has been cloned once:\n",
							 [StartLine, lists:max([1, StartCol-1]), EndLine, EndCol]),
	     display_clones_2(Range, Str1);
	2 -> Str1 = Str ++ "\n" ++ File ++ io_lib:format(":~p.~p-~p.~p: This code has been cloned twice:\n",
							 [StartLine, lists:max([1, StartCol-1]), EndLine, EndCol]),
	     display_clones_2(Range, Str1);
	_ -> Str1 = Str ++ "\n" ++ File ++ io_lib:format(":~p.~p-~p.~p: This code has been cloned ~p times:\n",
							 [StartLine, lists:max([1, StartCol-1]), EndLine, EndCol, F - 1]),
	     display_clones_2(Range, Str1)
    end.

display_clones_2([], Str) -> Str ++ "\n";
display_clones_2([{{File, StartLine, StartCol}, {File, EndLine, EndCol}}|Rs], Str) ->
    Str1 = case Rs == [] of 
	       true ->
		   Str ++ File++io_lib:format(":~p.~p-~p.~p:  \n", [StartLine,lists:max([1, StartCol-1]),EndLine, EndCol]);
	       _ ->
		   Str ++ File++io_lib:format(":~p.~p-~p.~p:  \n", [StartLine, lists:max([1,StartCol-1]), EndLine, EndCol])
	   end,
    display_clones_2(Rs, Str1).

get_anti_unifier([]) ->
    throw({error, anti_unification_failed});
get_anti_unifier([{Expr, EVs}]) -> generalise_expr({Expr, EVs}, []);
get_anti_unifier([{Expr, EVs}| Exprs]) ->
    {Nodes1, EVs1} = lists:unzip(lists:map(fun ({E, ExportedVars}) ->
						    expr_unification(Expr, E, ExportedVars)
					    end, Exprs)),
    generalise_expr({Expr, EVs}, {lists:usort(lists:append(Nodes1)), lists:usort(lists:append(EVs1))}).

 
expr_unification(Exp1, Exp2, Expr2ExportedVars) ->
    try 
	do_expr_unification(Exp1, Exp2) 
    of
	SubSt -> 
	    EVs1 = [{refac_syntax:variable_name(E1), get_var_define_pos(E1)} 
		    || {E1, E2} <- SubSt, refac_syntax:type(E2)== variable,
		       lists:member({refac_syntax:variable_name(E2), get_var_define_pos(E2)}, Expr2ExportedVars)],
	    Nodes = [E1 || {E1, _E2} <- SubSt, refac_syntax:type(E1)=/=variable],
	    {Nodes, EVs1}
    catch 
	_ ->
	     {[],[]}
    end.
	    
    
do_expr_unification(Exp1, Exp2) ->
    case {is_list(Exp1), is_list(Exp2)} of
      {true, true} ->
	  case length(Exp1) == length(Exp2) of
	    true ->
		lists:flatmap(fun ({E1, E2}) ->
				      do_expr_unification(E1, E2)
			      end, lists:zip(Exp1, Exp2));
	    _ ->
		throw({error, anti_unification_failed})
	  end;
      {false, false} ->  %% both are single expressions.
	       T1 = refac_syntax:type(Exp1),
	    T2 = refac_syntax:type(Exp2),
	    case T1  of 
		atom -> case (T2 == atom) andalso (refac_syntax:atom_value(Exp1) == refac_syntax:atom_value(Exp2)) of 
			    true -> [];
			    _ -> case variable_replaceable(Exp1) of 
				     true ->[{Exp1, Exp2}];
				     _ -> throw({error, anti_unification_failed})
				 end
			end;
		char -> case (T2==char) andalso (refac_syntax:char_value(Exp1) == refac_syntax:char_value(Exp2)) of 
			    true -> [];
			    _ -> [{Exp1, Exp2}]
			end;
		integer -> case (T2==integer orelse T2==float) andalso 
			       (refac_syntax:integer_value(Exp1) ==refac_syntax:integer_value(Exp2)) of 
			       true -> [];
			       _ -> [{Exp1,Exp2}]
			   end;
		string -> case (T2==string) andalso refac_syntax:string_value(Exp1) == refac_syntax:string_value(Exp2) of 
			      true -> [];
			      _ -> [{Exp1, Exp2}]
			  end;
		float -> case (T2==float orelse T2==integer) andalso 
			     refac_syntax:float_value(Exp1) == refac_syntax:float_value(Exp2) of 
			     true -> [];
			     _ -> [{Exp1, Exp2}]			 
			 end;
		nil -> case T2==nil of 
			   true -> [];
			   _ -> [{Exp1, Exp2}]
		       end;
		_ -> 
		    case (refac_syntax:type(Exp1)==variable) andalso (not is_macro_name(Exp1)) of
			true ->
			    case refac_syntax:type(Exp2) of
				variable -> 
				    [{Exp1, Exp2}];
				_ ->  throw({error, anti_unification_failed})
			    end;
			_ ->
			    SubTrees1 = erl_syntax:subtrees(Exp1),
			    SubTrees2 = erl_syntax:subtrees(Exp2),
			    do_expr_unification(SubTrees1, SubTrees2)
		    end
	    end
    end.


generalise_expr([], _) ->
    "";
generalise_expr({Exprs = [H| _T], EVs}, {NodesToGen, VarsToExport}) ->
    case refac_syntax:type(H) of
	function -> generalise_fun(H, NodesToGen);
	_ -> FunName = refac_syntax:atom(new_fun),
	     FVs = lists:ukeysort(2, refac_util:get_free_vars(Exprs)),
	     EVs1 = lists:ukeysort(2, EVs++VarsToExport),
	     NewExprs = generalise_expr_1(Exprs, NodesToGen),
	     NewExprs1 = case EVs1 of
			     [] -> NewExprs;
			     [{V, _}] -> E = refac_syntax:variable(V),
					 NewExprs ++ [E];
			     [_V| _Vs] -> E = refac_syntax:tuple([refac_syntax:variable(V) || {V, _} <- EVs1]),
					  NewExprs ++ [E]
			 end,
	     NewVars = collect_vars(NewExprs)--collect_vars(Exprs),
	     Pars = lists:map(fun ({V, _}) ->
				      refac_syntax:variable(V)
			      end,
			      FVs)
		 ++ lists:map(fun (V) ->
				      refac_syntax:variable(V)
			      end, NewVars),
	     C = refac_syntax:clause(refac_util:remove_duplicates(Pars), none, NewExprs1),
	     refac_prettypr:format(refac_syntax:function(FunName, [C]))
    end.

generalise_fun(F, NodesToGen) ->
    FunName = refac_syntax:function_name(F),
    Cs = refac_syntax:function_clauses(F),
    {Cs1, NewVars} = lists:unzip(lists:map(fun (C) ->
						   C1 = generalise_expr_1(C, NodesToGen),
						   NewVars = collect_vars(C1)--collect_vars(C),
						   {C1, NewVars}
					   end, Cs)),
    NewVars1 = lists:append(NewVars),
    NewCs = lists:map(fun (C) ->
			      Pats = refac_syntax:clause_patterns(C),
			      NewPats = Pats ++ lists:map(fun (V) ->
								  refac_syntax:variable(V)
							  end,
							  refac_util:remove_duplicates(NewVars1)),
			      Guards = refac_syntax:clause_guard(C),
			      Body = refac_syntax:clause_body(C),
			      refac_syntax:clause(NewPats, Guards, Body)
		      end, Cs1),
    refac_prettypr:format(refac_syntax:function(FunName, NewCs)).


generalise_expr_1(Expr, NodesToGen) when is_list(Expr)->
     refac_syntax:block_expr_body(generalise_expr_1(refac_syntax:block_expr(Expr), NodesToGen));
generalise_expr_1(Expr, NodesToGen) ->
    generalise_expr_2(Expr, NodesToGen).

generalise_expr_2(Expr, NodesToGen) ->
    Pid = start_counter_process(),
    {Expr1, _}= refac_util:stop_tdTP(fun do_replace_expr_with_var_1/2, Expr, {NodesToGen,Pid}),
    stop_counter_process(Pid),
    Expr1.

do_replace_expr_with_var_1(Node, {NodesToGen,Pid}) ->
    case lists:member(Node,NodesToGen) of
	true ->  NewVar = gen_new_var_name(Pid),
		 {refac_syntax:variable(NewVar), true};
	_ -> {Node, false}
    end.

collect_vars(Node) when is_list(Node) ->
    collect_vars_1(refac_syntax:block_expr(Node));
collect_vars(Node) ->
    collect_vars_1(Node).
collect_vars_1(Node) ->
    F = fun(N,S) ->
		case refac_syntax:type(N) of 
		    variable ->
			case lists:keysearch(category, 1, refac_syntax:get_ann(N)) of
			    {value, {category, macro_name}} -> S;
			    _ ->[refac_syntax:variable_name(N)|S]
			end;
		    _  -> S
		end
	end,
    refac_util:remove_duplicates(lists:reverse(refac_syntax_lib:fold(F, [], Node))).
    
		
			   
get_var_define_pos(V) ->
    case  lists:keysearch(def,1, refac_syntax:get_ann(V)) of
	{value, {def, DefinePos}} -> DefinePos;
	false -> []
    end.
               
is_macro_name(Exp) ->
    {value, {category, macro_name}} == 
	lists:keysearch(category, 1, refac_syntax:get_ann(Exp)).
