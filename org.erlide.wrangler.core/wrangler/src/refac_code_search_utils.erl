-module(refac_code_search_utils).


-compile(export_all).

-export([var_binding_structure/1,
	 display_search_results/3, display_clone_result/2,
	 start_counter_process/0, stop_counter_process/1,
	 add_new_export_var/2, get_new_export_vars/1,
	 identifier_name/1, gen_new_var_name/1,
	 remove_sub_clones/1]).

-include("../include/wrangler.hrl").



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
%%                                                                      %%
%%  A server for bookkeeping and generating variable names              %%
%%                                                                      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% A refactoring candidate: from non-gen_server to gen_server.

-spec start_counter_process() -> pid(). 			   
start_counter_process() ->
    start_counter_process(sets:new()).

-spec start_counter_process(set()) -> pid(). 
start_counter_process(UsedNames) ->
    spawn_link(fun () -> counter_loop({1, UsedNames, []}) end).

-spec stop_counter_process(atom() | pid() | port() | {atom(),atom()}) -> 'stop'.
stop_counter_process(Pid) ->
    Pid!stop.

counter_loop({SuffixNum, UsedNames, NewExportVars}) ->
    receive
      {From, next} ->
	  {NewSuffixNum, NewName} = make_new_name(SuffixNum, UsedNames),
	  From ! {self(), NewName},
	  counter_loop({NewSuffixNum, sets:add_element(NewName, UsedNames), NewExportVars});
      {add, Name} ->
	  counter_loop({SuffixNum, UsedNames, [Name| NewExportVars]});
      {From, get} ->
	  From ! {self(), lists:reverse(NewExportVars)},
	  counter_loop({SuffixNum, UsedNames, NewExportVars});
      stop ->
	  ok
    end.

-spec add_new_export_var(atom() | pid() | port() | {atom(),atom()},string()) -> {'add', string()}.
add_new_export_var(Pid, VarName) ->
    Pid ! {add, VarName}.



-spec get_new_export_vars(atom() | pid() | port() | {atom(),atom()}) -> [string()].
get_new_export_vars(Pid) ->
    Pid !{self(), get},
    receive
	{Pid, Vars} ->
	     Vars
    end.
-spec gen_new_var_name(atom() | pid() | port() | {atom(),atom()}) -> string(). 
gen_new_var_name(Pid) -> 
    Pid ! {self(), next},
    receive
	{Pid, V} ->
	     V
    end.

make_new_name(SuffixNum, UsedNames) ->
    NewName = "NewVar_"++integer_to_list(SuffixNum),
    case sets:is_element(NewName, UsedNames) of 
	true ->
	    make_new_name(SuffixNum+1, UsedNames);
	_ -> {SuffixNum, NewName}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
%%                                                                      %%
%%  Remove sub clones from a clone set.                                 %%
%%                                                                      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec remove_sub_clones([{[{{filename(), integer(), integer()},{filename(), integer(), integer()}}]
			  ,integer(), integer()}]) -> any().
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
%%                                                                          %%
%%  Returns the name of the identifier represented by an atom/variable node %%
%%                                                                          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec identifier_name(syntaxTree()) -> atom().
identifier_name(Exp) ->
    case refac_syntax:type(Exp) of
	atom ->
	    refac_syntax:atom_value(Exp);
	variable ->
	    refac_syntax:variable_name(Exp);
	_ ->throw({error, "Not an identifier"})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
%%                                                                      %%
%%  Generate variable binding structure                                 %%
%%                                                                      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec(var_binding_structure/1::([syntaxTree()]) -> [{integer(), integer()}]).      
var_binding_structure(ASTList) ->
    VarLocs = lists:keysort(2, refac_misc:collect_var_source_def_pos_info(ASTList)),
    case VarLocs of
      [] ->
	  [];
      _ -> var_binding_structure_1(VarLocs)
    end.

var_binding_structure_1(VarLocs) ->
    SrcLocs = [SrcLoc || {_Name, SrcLoc, _DefLoc} <- VarLocs],
    IndexedLocs = lists:zip(SrcLocs, lists:seq(1, length(SrcLocs))),
    Fun = fun ({_Name, SrcLoc, DefLoc}) ->
		  DefLoc1 = hd(DefLoc),
		  {value, {SrcLoc, Index1}} = lists:keysearch(SrcLoc, 1, IndexedLocs),
		  Index2 = case lists:keysearch(DefLoc1, 1, IndexedLocs) of
			       {value, {_, Ind}} -> Ind;
			       _ -> 0 %% free variable
			   end,
		  {Index1, Index2}
	  end,
    BS = [Fun(VL) || VL <- VarLocs],
    lists:keysort(1, lists:usort(BS)).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
%%                                                                      %%
%%  Display clone detection results                                     %%
%%                                                                      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec display_clone_result([{[{{filename(), integer(), integer()},{filename(), integer(), integer()}}], integer(), integer(), string()}],
			   string()) -> ok.
display_clone_result(Cs, Str) ->
    case length(Cs) >=1  of 
     	true -> display_clones_by_freq(Cs, Str),
     		display_clones_by_length(Cs, Str),
     		?wrangler_io("\n\n NOTE: Use 'M-x compilation-minor-mode' to make the result "
     			     "mouse clickable if this mode is not already enabled.\n\n",[]);	
     	false -> ?wrangler_io("\n"++Str++" code detection finished with no clones found.\n", [])
     end.
    
display_clones_by_freq(Cs, Str) ->
    ?wrangler_io("\n===================================================================\n",[]),
    ?wrangler_io(Str++" Code Detection Results Sorted by the Number of Code Instances.\n",[]),
    ?wrangler_io("======================================================================\n",[]),		 
    Cs1 = lists:reverse(lists:keysort(3, Cs)),
    ?wrangler_io(display_clones(Cs1, Str),[]).

display_clones_by_length(Cs, Str) ->
    ?wrangler_io("\n===================================================================\n",[]),
    ?wrangler_io(Str ++ " Code Detection Results Sorted by Code Size.\n",[]),
    ?wrangler_io("======================================================================\n",[]),		 
    Cs1 = lists:keysort(2,Cs),
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
    lists:foreach(fun(C) -> 
			  display_a_clone(C)
		  end, Cs).

display_a_clone(_C={Ranges, _Len, F,Code}) ->
    [R| _Rs] = lists:keysort(1, Ranges),
    NewStr = compose_clone_info(R, F, Ranges, ""),
    NewStr1 = NewStr ++ "The cloned expression/function after generalisation:\n\n" ++ Code,
    ?wrangler_io("~s", [NewStr1]).

%% display_clones_1(Cs) ->
%%     Str = display_clones_1(Cs, ""),
%%     refac_io:format("~s", [Str]).

%% display_clones_1([], Str) -> Str ++ "\n";
%% display_clones_1([{Ranges, _Len, F, Code}| Cs], Str) ->
%%     [R| _Rs] = lists:keysort(1, Ranges),
%%     NewStr = compose_clone_info(R, F, Ranges, Str),
%%     NewStr1 = NewStr ++ "The cloned expression/function after generalisation:\n\n" ++ Code,
%%     display_clones_1(Cs, NewStr1).

%% io:format("~s",["dddd~p\nddd"])
compose_clone_info({{File, StartLine, StartCol}, {File, EndLine, EndCol}}, F, Range, Str) ->
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
    end;
compose_clone_info({{{File, StartLine, StartCol}, {File, EndLine, EndCol}}, _FunCall}, F, Range, Str) ->
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
    Str1 =Str ++ File++io_lib:format(":~p.~p-~p.~p:  \n", [StartLine, lists:max([1,StartCol-1]), EndLine, EndCol]),
    display_clones_2(Rs, Str1);
display_clones_2([{{{File, StartLine, StartCol}, {File, EndLine, EndCol}}, FunCall}|Rs], Str) ->
    Str1 = Str ++ File++io_lib:format(":~p.~p-~p.~p:  ", [StartLine,lists:max([1, StartCol-1]),EndLine, EndCol])++
	" \n   "++ FunCall ++ "\n",
    display_clones_2(Rs, Str1).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
%%                                                                      %%
%%  Display expression search results                                   %%
%%                                                                      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec display_search_results([{filename(),{{integer(), integer()}, {integer(), integer()}}}],
			     syntaxTree()|none, string()) ->
				    {ok, [{filename(),{{integer(), integer()}, {integer(), integer()}}}]}.
display_search_results(Ranges, AntiUnifier, Type) ->
    case Ranges of
	[_] -> 
	    ?wrangler_io("No "++Type++" expression has been found.\n", []),
	    {ok, Ranges};
	_ -> 
	    ?wrangler_io("~p expressions (including the expression selected)"
			 " which are "++Type++" to the expression selected have been found. \n", [length(Ranges)]),
	    ?wrangler_io(compose_search_result_info(Ranges), []),
	    case AntiUnifier of 
		none -> ok;
		_ ->
		    ?wrangler_io("\nThe generalised expression would be:\n\n~s\n\n", [refac_prettypr:format(AntiUnifier)])
	    end,
	    ?wrangler_io("\n\nNOTE: Use 'M-x compilation-minor-mode' to make the result "
			 "mouse clickable if this mode is not already enabled.\n",[]),
	    ?wrangler_io("      Use 'C-c C-e' to remove highlights!\n", []),
	    {ok, Ranges}
    end.

compose_search_result_info(Ranges) ->
    compose_search_result_info(Ranges, "").
compose_search_result_info([], Str) ->
    Str;
compose_search_result_info([{FileName, {{StartLine, StartCol}, {EndLine, EndCol}}}|Ranges], Str) ->
    Str1 =Str ++ "\n"++FileName++io_lib:format(":~p.~p-~p.~p: ", [StartLine, StartCol, EndLine, EndCol]),
    compose_search_result_info(Ranges, Str1).


 
