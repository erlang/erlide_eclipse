-module(suffix_tree).

-include("../include/wrangler.hrl").

-export([get_clones_by_suffix_tree/7, get_clones_by_suffix_tree_inc/6]).

-export([init/1]).


-spec(get_clones_by_suffix_tree_inc/6::(dir(), string(),integer(), integer(), integer(), filename()) ->
					 [{[{integer(),integer()}], integer(), integer()}]).					   
get_clones_by_suffix_tree_inc(Dir, ProcessedToks, MinLength, MinClones, AllowOverLap, SuffixTreeExec) ->
    start_suffix_tree_clone_detector(SuffixTreeExec),
    OutFileName = filename:join(Dir, "wrangler_suffix_tree"),
    case file:write_file(OutFileName, ProcessedToks) of
      ok ->
	  case catch call_port({get, MinLength, MinClones, AllowOverLap, OutFileName}) of
	    {ok, _Res} ->
		?debug("Initial clones are calculated using C suffixtree implementation.\n", []),
		stop_suffix_tree_clone_detector(),
		{ok, Res} = file:consult(OutFileName),
		file:delete(OutFileName),
		case Res of
		  [] -> [];
		  [Cs] -> Cs
		end;
	      E -> 
		  file:delete(OutFileName)
		  throw({error, lists:flatten(io_lib:format("Clone detection failed for reason:~p.", [E]))})
	  end;
	{error, Reason} -> 
	    throw({error, lists:flatten(io_lib:format("Clone detection failed for reason:~p.", [Reason]))})   
    end.


-spec(get_clones_by_suffix_tree/7::(dir(), string(),integer(), integer(), string(), integer(), filename()) ->
					 [{[{integer(),integer()}], integer(), integer()}]).					   
get_clones_by_suffix_tree(Dir, ProcessedToks, MinLength, MinClones, Alphabet, AllowOverLap, SuffixTreeExec) ->
    start_suffix_tree_clone_detector(SuffixTreeExec),
    OutFileName = filename:join(Dir, "wrangler_suffix_tree"),
    case file:write_file(OutFileName, ProcessedToks) of
      ok ->
	  case catch call_port({get, MinLength, MinClones, AllowOverLap, OutFileName}) of
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
      _ -> get_clones_by_erlang_suffix_tree(ProcessedToks, MinLength, MinClones, Alphabet, AllowOverLap)
    end.

start_suffix_tree_clone_detector(SuffixTreeExec) ->
    process_flag(trap_exit, true),
    spawn_link(?MODULE, init, [SuffixTreeExec]).

stop_suffix_tree_clone_detector() ->
    case catch (?MODULE) ! stop of _ -> ok end.

get_clones_by_erlang_suffix_tree(ProcessedToks, MinLength, MinClones, Alphabet, AllowOverLap) ->
    ?wrangler_io("\nWrangler failed to use the C suffixtree implementation;"
                 " Initial clones are calculated using Erlang suffixtree implementation.\n",[]),
    Tree = suffix_tree(Alphabet, ProcessedToks ++ "&", AllowOverLap),
    Cs = lists:flatten([collect_clones(MinLength, MinClones, B)||B<-Tree]),
    refac_code_search_utils:remove_sub_clones(Cs).

    
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

 
%% =====================================================================
%% Construction of suffix tree using Erlang.
suffix_tree(Alpha, T, AllowOverLap) -> 
    Tree = suffix_tree_1(Alpha, length(T),suffixes(T)),
    post_process_suffix_tree(Tree, AllowOverLap).


suffix_tree_1(_Alpha, _Len, [[]]) -> leaf;
suffix_tree_1(Alpha, Len, Suffixes)->
    [{branch, {SubLens, 1+CP1 }, suffix_tree_1(Alpha, Len, SSR)} 
     || A <- Alpha, 
 	[SA | SSA] <- [select(Suffixes, A)],
 	Lens <- [[length(S)+1||S<-[SA|SSA]]],
 	{CP1, SSR} <- [edge_cst([SA|SSA])],
        SubLens <- [[{Len-L+1, Len-L+CP1+1}||L<-Lens]]].

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
    NewTree1 = [add_frequency(B)||B<-Tree],
    [extend_range(B, AllowOverLap)||B<-NewTree1].


%% Add the number of duplicated times to each branch.
add_frequency({branch, {Range, Len}, leaf}) ->
    {branch, {Range, {Len, 1}}, leaf};
add_frequency({branch, {Range, Len}, Branches}) ->
    Bs = [add_frequency(B)||B<-Branches],
    F1 = lists:foldl(fun({branch,{_R, {_L, F}}, _C}, Sum)-> F + Sum end,0, Bs),
    {branch, {Range, {Len, F1}}, Bs}.

  

%% combine the ranges within two continuous branches if possible.
extend_range(SuffixTree, AllowOverLap) ->
    extend_range([],SuffixTree, AllowOverLap).

extend_range(Prev_Range, {branch, {Range, {_Len, Freq}}, leaf}, _AllowOverLap) ->
    ExtendedRange = [combine_range(Prev_Range, R)||R<-Range],
    {S, E} = hd(ExtendedRange),
    {branch, {ExtendedRange, {E - S + 1, Freq}}, leaf};
extend_range(Prev_Range, {branch, {Range, {Len, Freq}}, Bs}, AllowOverLap) ->
    ExtendedRange = [combine_range(Prev_Range, R)||R<-Range],
    case AllowOverLap == 0 andalso overlapped_range(ExtendedRange) of
	true -> 
	    Bs1 = [extend_range(Range, B, AllowOverLap)||B<-Bs],
	    {branch, {Range, {Len, Freq}}, Bs1};
	_ ->
	    Bs1 = [extend_range(ExtendedRange, B, AllowOverLap)||B<-Bs],
	    {S, E} = hd(ExtendedRange),
	    {branch, {ExtendedRange, {E - S + 1, Freq}}, Bs1}
    end.


overlapped_range(R) -> overlapped_range_1(lists:usort(R)).

overlapped_range_1([]) -> false;
overlapped_range_1([_R]) -> false;
overlapped_range_1([{_S, E},{S1, E1}|Rs]) -> 
    (S1 < E) or overlapped_range_1([{S1,E1}|Rs]).
			
    
combine_range(Prev_Range, {StartLoc, EndLoc}) ->
    case lists:keysearch(StartLoc-1, 2, Prev_Range) of 
	{value, {StartLoc1, _EndLoc1}} ->
	    {StartLoc1, EndLoc};
	_  -> {StartLoc, EndLoc}
    end.

%% Collect those clones that satisfy the selecting criteria from the suffix trees.
collect_clones(MinLength, MinFreq, {branch, {Range, {Len, F}}, Others}) ->
    C = case F >= MinFreq andalso Len >= MinLength of
	    true -> 
		[{Range, Len, F}];
	    false -> 
		[]
	end,
    case Others of
	leaf -> C;
	Bs -> lists:foldl(fun (B, C1) -> 
				  collect_clones(MinLength, MinFreq, B) ++ C1 
			  end, C, Bs)
    end.
    

