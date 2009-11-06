%% Copyright (c) 2009, Huiqing Li, Simon Thompson
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
-module(refac_sim_code).

-export([sim_code_detection/6, sim_code_detection_in_buffer/6]).

-include("../include/wrangler.hrl").

-include_lib("stdlib/include/ms_transform.hrl").

-define(DefaultSimiScore, 0.8).

-import(refac_duplicated_code, [get_clones_by_suffix_tree/6, display_clone_result/2]).

-define(DEBUG, true).

-ifdef(DEBUG).
-define(debug(__String, __Args), ?wrangler_io(__String, __Args)).
-else.
-define(debug(__String, __Args), ok).
-endif.

-define(DEFAULT_LEN, 5).
-define(DEFAULT_FREQ, 2).
-define(DEFAULT_SIMI_SCORE, 0.8).


-spec(sim_code_detection_in_buffer/6::(FileName::filename(), MinLen::string(), MinFreq::string(), MinScore::string(), 
				       SearchPaths::[dir()], TabWidth::integer()) ->  {ok, string()}).
sim_code_detection_in_buffer(FileName, MinLen, MinFreq, SimiScore, SearchPaths, TabWidth) ->
    sim_code_detection([FileName],MinLen, MinFreq, SimiScore, SearchPaths, TabWidth).

    
-spec(sim_code_detection/6::(DirFileList::[filename()|dir()], MinLen::string(), MinFreq::string(), MinScore::string(), 
			     SearchPaths::[dir()], TabWidth::integer()) -> {ok, string()}). 			     
sim_code_detection(DirFileList, MinLen1, MinFreq1, SimiScore1, SearchPaths, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:sim_code_detection(~p,~p,~p,~p,~p,~p).\n", 
		 [?MODULE, DirFileList, MinLen1, MinFreq1, SimiScore1, SearchPaths, TabWidth]),
    
    MinLen = try
		 case MinLen1 == [] orelse list_to_integer(MinLen1) <1 of
		     true -> 
			 ?DEFAULT_LEN;
		     _ -> list_to_integer(MinLen1)
		 end
	     catch 
		 V -> V;
		   _:_ -> throw({error, "Parameter input is invalid."})
	     end,
    MinFreq = try 
		  case MinFreq1 == [] orelse list_to_integer(MinFreq1) < ?DEFAULT_FREQ of
		      true -> ?DEFAULT_FREQ;
		      _ -> list_to_integer(MinFreq1)
		  end
	      catch 
		      V1 -> V1;
		    _:_ ->throw({error, "Parameter input is invalid."})
	      end,
    SimiScore = try 
		    case SimiScore1 of 
			[] -> ?DefaultSimiScore;
			_ -> S = list_to_float(SimiScore1),
			     case (S>=0.1) andalso (S =<1.0) of 
				 true ->S;
				 _ -> ?DefaultSimiScore
			     end
		    end
		catch V2 -> V2;
		      _:_ -> throw({error, "Parameter input is invalid."})
		end,
    Pid = start_hash_process(),
    ASTTab = ets:new(ast_tab, [set, public]),
    VarTab = ets:new(var_tab, [set, public]),
    RangeTab= ets:new(range_tab, [set, public]),
    Files = refac_util:expand_files(DirFileList, ".erl"),
    case Files of
	[] -> ?wrangler_io("Warning: No files found in the searchpaths specified.",[]);
	_ ->
	    _Time1 = now(),
	    ?debug("Searching for initial clone candidates...\n", []),
	    %% ?debug("current time:~p\n", [time()]),
	    generalise_and_hash_ast(Files, Pid, SearchPaths, TabWidth, ASTTab, VarTab),
	    %%?debug("\ngeneralise and hash ast done.\n",[]),
	    Dir = filename:dirname(hd(Files)),
	    Cs = get_clones(Pid, MinLen, MinFreq, Dir, RangeTab),
	    stop_hash_process(Pid),
	    %% ?debug("\nInitial candiates finished\n",[]),
	    ?debug("\nNumber of initial clone candidates: ~p\n", [length(Cs)]),
 	    CloneCheckPid = start_clone_check_process(),		   
 	    Cs2 = examine_clone_sets(Cs, MinFreq, SimiScore, ASTTab, VarTab, RangeTab,CloneCheckPid, 1),
 	    _Time2 = now(),		
	    %% ?debug("current time:~p\n", [time()]),
	    %% ?debug("total time used:~p\n", [timer:now_diff(_Time2,_Time1)/1000000]),
	    stop_clone_check_process(CloneCheckPid),
	    ets:delete(ASTTab),
	    ets:delete(VarTab),
	    ets:delete(RangeTab),
	    display_clone_result(remove_fun_info(Cs2), "Similar")
    end,
    {ok, "Similar code detection finished."}.
    

generalise_and_hash_ast(Files, Pid, SearchPaths, TabWidth, ASTTab, VarTab) ->
    lists:foreach(fun(F) ->
			  generalise_and_hash_ast_1(F, Pid, SearchPaths, TabWidth, ASTTab, VarTab)
		  end, Files).


generalise_and_hash_ast_1(FName, Pid, SearchPaths, TabWidth, ASTTab, VarTab) ->
    Fun0= fun(T, S) ->
		  case refac_syntax:type(T) of 
		      variable ->
			  SourcePos = refac_syntax:get_pos(T),
			  case lists:keysearch(def, 1, refac_syntax:get_ann(T)) of
			      {value, {def, DefinePos}} ->
				  VarName = refac_syntax:variable_name(T),
				  S++[{VarName, SourcePos, DefinePos}];
			      _ -> S
			  end;
		      _  -> S
		  end
	  end,
    Fun = fun(Form) ->
		  case refac_syntax:type(Form) of 
		      function ->
			  FunName = refac_syntax:atom_value(refac_syntax:function_name(Form)),
			  Arity = refac_syntax:function_arity(Form),
			  AllVars = refac_syntax_lib:fold(Fun0, [], Form),
			  ets:insert(VarTab, {{FName, FunName, Arity}, AllVars}),
			  refac_util:full_tdTP(fun generalise_and_hash_ast_2/2, 
					       Form, {FName, FunName, Arity, ASTTab,Pid});
			  _ -> ok
		  end
	  end,
    {ok, {AnnAST, _Info}} = parse_annotate_file(FName,SearchPaths, TabWidth),
    refac_syntax:form_list_elements(AnnAST),
    lists:foreach(fun(F)->Fun(F) end, refac_syntax:form_list_elements(AnnAST)),
    insert_dummy_entry(Pid).



generalise_and_hash_ast_2(Node, {FName, FunName, Arity, ASTTab,Pid}) ->
    F0 = fun(T, _Others) ->
		case variable_replaceable(T) of 
		    true ->
			{refac_syntax:variable('Var'), true};
		    false -> {T, false}
		end
	end,
    F1 = fun(T) ->
		 {T1, _} =refac_util:stop_tdTP(F0, T, []),
		 HashVal = erlang:md5(refac_prettypr:format(T1)),
		 {S, E} = refac_util:get_range(T),
		 insert_hash(Pid, HashVal, {{FName, FunName, Arity}, S, E}),
		 T1
	 end,
    case refac_syntax:type(Node) of 
	clause -> 
	    Body = refac_syntax:clause_body(Node),
	    [ets:insert(ASTTab, {{FName, FunName, Arity, StartLoc, EndLoc}, E})
	     ||E <- Body,
	       {StartLoc, EndLoc} <-[refac_util:get_range(E)]],
	    insert_dummy_entry(Pid),
	    _NewBody = [F1(E) || E <-Body],
	    {Node, true};
	block_expr ->
	    insert_dummy_entry(Pid),
	    Body = refac_syntax:block_expr_body(Node),
	    [ets:insert(ASTTab, {{FName, FunName, Arity, StartLoc, EndLoc}, E})
	     ||E <- Body,
	       {StartLoc, EndLoc} <-[refac_util:get_range(E)]],
	    _NewBody = [F1(E) || E <-Body],
	    {Node,true};
	try_expr  ->
	    insert_dummy_entry(Pid),
	    Body = refac_syntax:try_expr_body(Node),
	    [ets:insert(ASTTab, {{FName, FunName, Arity, StartLoc, EndLoc}, E})
	     ||E <- Body,
	       {StartLoc, EndLoc} <-[refac_util:get_range(E)]],
	    _NewBody=[F1(E) || E <-Body],
	    {Node, true};
	_ -> {Node, false}
    end.



variable_replaceable(Exp) ->
    case lists:keysearch(category,1, refac_syntax:get_ann(Exp)) of 
	{value, {category, record_field}} -> false;
	{value, {category, record_type}} -> false;	 
	{value, {category, guard_expression}} -> false;
	{value, {category, macro_name}} -> false;
	{value, {category, pattern}} -> 
 	    refac_syntax:type(Exp)==variable;
	_ -> T = refac_syntax:type(Exp),
	     (not (lists:member(T, [match_expr, operator, case_expr, 
				    block_expr, catch_expr, fun_expr,
 				    receive_expr, try_expr, clause,
				    binary_field, query_expr])))
    end.



examine_clone_sets([], _MinFreq, _SimiScore, ASTTab, _VarTab, RangeTab,Pid, _Num)->
    Pid ! {get_clones, self()},
    receive
	{Pid, Cs} ->
	    [get_generalised_form(ASTTab, RangeTab, C)||C<-Cs]
    end;
examine_clone_sets([C|Cs],MinFreq, SimiScore, ASTTab, VarTab, RangeTab, Pid, Num) ->
    ?debug("\nChecking the ~pth clone candidate...", [Num]),
    Res =examine_a_clone_set(C, MinFreq, SimiScore, ASTTab, VarTab, RangeTab,Pid),
    add_new_clones(Pid, Res),
    examine_clone_sets(Cs, MinFreq, SimiScore, ASTTab, VarTab, RangeTab, Pid, Num+1).

examine_a_clone_set(Cs, MinFreq, SimiScore, ASTTab, VarTab, RangeTab, Pid) ->
    case Cs of 
	[] -> []; %% why should this happen?
	_ ->
	    C={_, {_, Freq}} = lists:last(Cs), 
	    Res =examine_a_clone_class(C, MinFreq, SimiScore, ASTTab, VarTab, RangeTab),
	    case Res of
		[R]  when element(2, R)==Freq ->
		    Res;
		_ -> Cs1 = lists:sublist(Cs, 1, length(Cs)-1),
		     examine_a_clone_set(group_cs(Cs1,[]), MinFreq, SimiScore, ASTTab, VarTab, RangeTab, Pid, Res)
	    end
    end.

examine_a_clone_set([], _MinFreq, _SimiScore, _ASTTab, _VarTab, _RangeTab,  _Pid, Acc) ->
    remove_sub_clones(lists:keysort(2,Acc));  
examine_a_clone_set([Cs1|Cs2], MinFreq, SimiScore, ASTTab, VarTab, RangeTab,Pid, Acc) ->
    Res = lists:append([examine_a_clone_class(C, MinFreq, SimiScore, ASTTab, VarTab, RangeTab) || C <-Cs1]),
    case Res of
	[] -> remove_sub_clones(lists:keysort(2,Acc));    
    	_ ->examine_a_clone_set(Cs2, MinFreq, SimiScore, ASTTab, VarTab, RangeTab,Pid, Res++Acc)
    end.
  

group_cs([], Acc) ->
     lists:reverse(Acc);
group_cs([{_Ranges, {Len, _Freq}}], Acc) ->
    group_cs([],[[{_Ranges, {Len, _Freq}}]|Acc]);
group_cs(Cs=[{_Ranges, {Len, _Freq}}|_T], Acc) ->
    {Cs1, Cs2} =lists:splitwith(fun(C)-> element(1,element(2, C))==Len end, Cs),
    group_cs(Cs2, [Cs1|Acc]).


remove_fun_info(Cs) ->
    [remove_fun_info_1(C) || C <-Cs].
remove_fun_info_1({Ranges, {Len, Freq}, C}) ->
    Ranges1=[{{F, SLine, SCol}, {F, EndLine, EndCol}} || 
		{{F,_, _},{SLine, SCol}, {EndLine, EndCol}} <-Ranges],
    {Ranges1, Len, Freq, C}.


remove_sub_clones(Cs) ->
    remove_sub_clones(lists:reverse(Cs),[]).

remove_sub_clones([], Acc_Cs) ->
    lists:reverse(Acc_Cs);
remove_sub_clones([C|Cs], Acc_Cs) ->
    case is_sub_clone(C, Acc_Cs) of
	true -> 
	    remove_sub_clones(Cs, Acc_Cs);
	false ->remove_sub_clones(Cs, [C|Acc_Cs])
    end.

is_sub_clone({Ranges, {Len, Freq},Str}, ExistingClones) ->
    case ExistingClones of 
	[] -> false;
	[{Ranges1, {Len1, Freq1}, _}|T] ->
	    case {Len, Freq} =<{Len1, Freq1} of
		true ->
		    case is_sub_ranges(Ranges, Ranges1) of 
			true -> 
			    true;
			false -> is_sub_clone({Ranges, {Len, Freq},Str}, T)
		    end;
		_ ->
		    false
	    end
    end;
is_sub_clone({Ranges, {Len, Freq}}, ExistingClones) ->
    case ExistingClones of 
	[] -> false;
	[{Ranges1, {Len1, Freq1}}|T] ->
	    case {Len, Freq} =<{Len1, Freq1} of
		true ->
		    case is_sub_ranges(Ranges, Ranges1) of 
			true -> 
			    true;
			false -> is_sub_clone({Ranges, {Len, Freq}}, T)
		    end;
		_ ->
		    false
	    end
    end.
is_sub_ranges(Ranges1, Ranges2) ->
    lists:all(fun ({FMA,S, E}) -> 
		      lists:any(fun ({FMA1,S1, E1}) ->
					FMA == FMA1 andalso
					    S1 =< S andalso 
					    E =< E1 
				end, Ranges2) 
	      end, Ranges1).
	 
    
    

examine_a_clone_class({Ranges, {Len, Freq}}, MinFreq, SimiScore, ASTTab, VarTab, RangeTab) ->
    examine_clone_members(Ranges, {Ranges, {Len, Freq}}, MinFreq, SimiScore, ASTTab, VarTab, RangeTab,[]).
  
 
examine_clone_members([], _,_, _, _, _, _, Acc) ->
    Acc;    
examine_clone_members([R|Rs], C={Ranges,{Len, Freq}}, MinFreq, SimiScore, ASTTab, VarTab,RangeTab,Acc) ->
    Res = examine_a_clone_member(R, C,  MinFreq, SimiScore, ASTTab, VarTab, RangeTab),
    case Res of 
	[] -> 
	    Ranges1 =Ranges--[R],
	    examine_clone_members(Rs, {Ranges1, {Len, Freq}}, 
				  MinFreq, SimiScore, ASTTab, VarTab, RangeTab, Acc);
	{Rs1, {_Len1, _Freq1}, _Str} ->
	    examine_clone_members(Rs--Rs1, {Ranges--Rs1,{Len, Freq}}, MinFreq, 
					     SimiScore, ASTTab, VarTab, RangeTab, [Res|Acc])
	end.

examine_a_clone_member(Range={_FName, _Start, _End}, {Rs, {Len, _Freq}},  MinFreq, SimiScore, ASTTab, VarTab, RangeTab) ->
    {Exprs1, VarsToExport} = get_expr_list_and_vars_to_export(Range, ASTTab, VarTab, RangeTab),
    Res = pmap(fun(R) ->
		       case R==Range of
			   true -> [];
			   _ ->
			       find_anti_unifier(Exprs1, R, SimiScore, ASTTab, VarTab, RangeTab)
		       end
	       end, Rs),
    Res1 = lists:append(Res),
    case length(Res1) < MinFreq-1 of
	true ->[];
	_ ->
	    {Ranges, ExportVars, SubSt} = lists:unzip3(Res1),
	    ExportVars1 = {element(1,lists:unzip(VarsToExport)), lists:usort(lists:append(ExportVars))},
	    {[Range|Ranges], {Len, length(Ranges)+1}, {Range, SubSt, ExportVars1}}
    end.
    

find_anti_unifier(Exprs1, Range,SimiScore, ASTTab, VarTab,RangeTab)->
    {Exprs2, VarsToExport} = get_expr_list_and_vars_to_export(Range, ASTTab, VarTab, RangeTab),
    Res =refac_sim_expr_search:find_anti_unifier(Exprs1, Exprs2, SimiScore, VarsToExport),
    case Res of 
	[] ->
	     Res;
	[{_, EVs, SubSt}] -> [{Range, EVs, SubSt}]
    end.



get_generalised_form(ASTTab, RangeTab, {Ranges, {Len, Freq}, {Range, SubSt, ExportVars}}) ->
    Exprs1 = get_expr_list(Range, ASTTab, RangeTab),
    AntiUnifier=refac_sim_expr_search:generalise_expr(Exprs1,SubSt, ExportVars),
    {Ranges, {Len, Freq}, refac_prettypr:format(AntiUnifier)}.
   

get_expr_list({{FName, FunName, Arity}, StartLoc, EndLoc}, ASTTab, RangeTab) ->
    Ranges =ets:lookup(RangeTab, {{FName, FunName, Arity}, StartLoc, EndLoc}),
    case Ranges of 
	[] ->
	     throw({error, "Range EST table error"});
	[{_Key,Rs}] -> 
	    Es =lists:append([ets:lookup(ASTTab, {File, Fun, Ari, SLoc, ELoc})
			      ||{{File, Fun, Ari}, SLoc, ELoc}<-Rs]),
		    [element(2, E)||E<-Es]
	     
    end.  


get_expr_list_and_vars_to_export({{FName, FunName, Arity}, StartLoc, EndLoc}, ASTTab, VarTab, RangeTab) ->
    Es = get_expr_list({{FName, FunName, Arity}, StartLoc, EndLoc}, ASTTab, RangeTab),
    AllVars = ets:lookup(VarTab, {FName, FunName, Arity}),
    case AllVars of 
	[] -> {Es, []};
	[{_, Vars}] ->
	    ExprBdVarsPos = [Pos || {_Var, Pos}<-refac_util:get_bound_vars(Es)],
	    VarsToExport =[{V, DefPos} || {V, SourcePos, DefPos} <- Vars,
					  SourcePos > EndLoc,
					  lists:subtract(DefPos, ExprBdVarsPos) == []],
	    {Es, VarsToExport}
    end.


    
start_clone_check_process() ->
    spawn_link(fun()->clone_check_loop([]) end).

stop_clone_check_process(Pid) ->
    Pid ! stop.


add_new_clones(Pid, Clones) ->
    Pid ! {add_clone, Clones}.


clone_check_loop(Cs) ->
    receive
	{add_clone,  Clones} ->
	    clone_check_loop(lists:keysort(2, Clones++Cs));
	{is_sub_clone, From, {Ranges,{Len, Freq}}} ->
	    From! {self(),is_sub_clone({Ranges, {Len, Freq},none}, Cs)},
	    clone_check_loop(Cs);
	{get_clones, From} ->
	    From ! {self(), remove_sub_clones(Cs)},
	    clone_check_loop(Cs);       
	stop ->
	    ok;
	_Msg -> 
	    ?debug("Unexpected message:\n~p\n",[_Msg]),
	     ok	
    end.
    

start_hash_process() ->		     
    HashTab = ets:new(hash_tab, [set, public]),
    spawn_link(fun()->hash_loop({1,HashTab,[]}) end).

stop_hash_process(Pid) ->
    Pid!stop.

insert_hash(Pid, HashVal,Range) ->
    Pid ! {add, HashVal, Range}.

insert_dummy_entry(Pid) ->
    Pid ! add_dummy.

get_clones(Pid, MinLen, MinFreq, Dir, RangeTab) ->
    Pid! {get_clones, self(), MinLen, MinFreq, Dir, RangeTab},
    receive
	{Pid, Cs} -> 
	    Cs
    end.
    
hash_loop({Index, HashTab, Data}) ->
    receive
	{add, Key, Range} ->
	    case ets:lookup(HashTab, Key) of
		[{Key, I}] ->
		    hash_loop({Index, HashTab, [{I, Range}| Data]});
		[] -> ets:insert(HashTab, {Key, Index}),
		      hash_loop({Index + 1, HashTab, [{Index, Range}| Data]})
	    end;
	add_dummy ->
	    hash_loop({Index, HashTab, [{'#', {'_', {0, 0}, {0, 0}}}| Data]});
	{get_clones, From, MinLen, MinFreq, Dir, RangeTab} ->
	    Cs = search_for_clones(Dir,lists:reverse(Data), MinLen, MinFreq, RangeTab),
	    From ! {self(), Cs},
	    hash_loop({Index, HashTab, Data});
	stop ->
	    ets:delete(HashTab),
	    ok
    end.


search_for_clones(Dir, Data, MinLen, MinFreq, RangeTab) ->
    F0 = fun(I) ->
		 case is_integer(I) of 
		     true -> integer_to_list(I) ++ ",";
		     false -> atom_to_list(I)
		 end
	 end,
    F =fun({I, Range}) ->
	       lists:duplicate(length(F0(I)), {I, Range})
       end,
    IndexStr = lists:append([F0(I)|| {I, _}<-Data]),
    NewData =lists:append([F(Elem) ||Elem <-Data]),
    Cs= get_clones_by_suffix_tree(Dir, IndexStr++"&",MinLen, MinFreq,"0123456789,#&", 1),
  %%  ?debug("Num of clones from suffix tree:\n~p\n", [length(Cs)]),
    Cs1 = lists:append([strip_a_clone({[{S,E} |Ranges], {Len, Freq}}, SubStr, MinLen, MinFreq)
			|| {[{S,E} |Ranges], Len, Freq} <- Cs, 
			      SubStr <-[lists:sublist(IndexStr, S, E-S+1)]]),
    Cs2 =refac_duplicated_code:remove_sub_clones([{R,Len,Freq}||{R, {Len, Freq}}<-Cs1]),
  %%  ?debug("num of clones after stripping clones:\n~p\n", [length(Cs2)]),
    get_clones_in_ranges([{R,{Len, Freq}}||{R, Len, Freq}<-Cs2], 
			 NewData, MinLen, MinFreq, RangeTab).
   
strip_a_clone({Ranges, {Len, F}}, Str, MinLen, MinFreq) ->
    {Str1, Str2} = lists:splitwith(fun(C) ->C==$# orelse C ==$, end, Str),
    {Str21, Str22} = lists:splitwith(fun(C) ->C==$# orelse C ==$, end, lists:reverse(Str2)),
    case Str22=="" of 
	true -> [];
	_ -> NewRanges = [{S+length(Str1), E-length(Str21)}|| {S, E} <-Ranges],
	     NewLen = Len-length(Str1) -length(Str21),
	     case NewLen >= MinLen*2-1 of
		 true ->
		     split_a_clone({NewRanges,{NewLen, F}},lists:reverse(Str22), MinLen, MinFreq);
		 _ -> []
	     end	    
    end.

split_a_clone(_, "", _, _)->[];
split_a_clone({Ranges, {Len, F}}, Str, MinLen, MinFreq) ->
    {Str1, Str2} = lists:splitwith(fun(C) -> C=/=$# end, Str),
    Len1 = case lists:last(Str1) of 
	       $, ->length(Str1)-1;
	       _ -> length(Str1)
	   end,
    {Str21, Str22} = lists:splitwith(fun(C) -> C==$# end, Str2),
    {NewRanges, RemainedRanges} = lists:unzip([{{S, S+Len1-1}, {S+length(Str1)+length(Str21), E}}
					       ||{S, E} <-Ranges]),
    case Len1 >= MinLen*2-1 of 
	true ->
	    NewRanges1 = remove_overlapped_ranges(NewRanges),
	    NewFreq = length(NewRanges1),
	    case NewFreq>=MinFreq of
		true ->
		    case Str22 of 
			"" ->[{NewRanges1, {Len1, NewFreq}}];
			_ ->   [{NewRanges1, {Len1, NewFreq}} | split_a_clone({RemainedRanges, {Len, F}}, Str22, MinLen,MinFreq)]
		    end;
		false ->
		    case Str22 of 
			"" ->
			    [];
			_ -> 
			    split_a_clone({RemainedRanges, {Len, F}}, Str22, MinLen,MinFreq)
		    end
	    end;
	false ->
	    case Str22 of 
		"" ->
		    [];
		_ -> 
		    split_a_clone({RemainedRanges, {Len, F}}, Str22, MinLen,MinFreq)
	    end
    end.

   
remove_overlapped_ranges(Rs) ->
    Rs1 = lists:sort(Rs),
    R = hd(Rs1),
    remove_overlapped_ranges(tl(Rs1), R, [R]).
remove_overlapped_ranges([], _ , Acc) ->
    lists:reverse(Acc);
remove_overlapped_ranges([{S, E}|Rs], {S1, E1},Acc) ->
      case (S1=<S) andalso (S=<E1) of 
	true ->
	   remove_overlapped_ranges(Rs, {S1, E1}, Acc);
	false ->
	    remove_overlapped_ranges(Rs, {S, E}, [{S, E}|Acc])
    end.
	     
    
get_clones_in_ranges(Cs, Data, MinLen, MinFreq, RangeTab) ->
    F0 = fun({S, E}) ->
		 {_, Ranges}=lists:unzip(lists:sublist(Data, S, E-S+1)),
		 Ranges1=refac_util:remove_duplicates(Ranges),
		 sub_list(Ranges1, MinLen, length(Ranges1))
	 end,
    F1 = fun(Rs) ->
		 [begin {MFA, S1, _E1} = hd(R),
			{_, _, E2} = lists:last(R),
			ets:insert(RangeTab, {{MFA, S1, E2}, R}),
			{MFA, S1, E2}
		  end ||R<-Rs]
	 end,	
    F= fun({Ranges, {_Len, Freq}}) ->
	       case Freq>=MinFreq of 
		   true -> 
		       NewRanges0 = [F0(R)|| R <- Ranges],
		       NewRanges = zip(NewRanges0),
		       [{F1(Rs),{length(hd(Rs)), Freq}}|| Rs <-NewRanges];
		   _ ->[]
	       end
       end, 
    Res =[lists:reverse(F(C)) || C<-Cs],
    remove_duplicated_clones(Res,[]).


remove_duplicated_clones([], Acc) ->
     lists:reverse(Acc);
remove_duplicated_clones([Cs|T], Acc) ->
    Cs1 =[{Ranges, {Len, Freq}}|| {Ranges, {Len, Freq}}<-Cs,
		not(lists:any(fun(Cs2) ->
				      lists:any(fun({Ranges1, {Len1, Freq1}}) ->
							(Len==Len1) andalso (Freq =<Freq1) 
							    andalso  (Ranges--Ranges1==[])
						end, Cs2)
			      end, Acc))],
    remove_duplicated_clones(T, [Cs1|Acc]).
			  



zip(L) ->[nested_tuple_to_list(E)||E<-zip_1(L)].
zip_1([]) ->[];
zip_1([L]) ->L;
zip_1([L,H|T]) ->
    zip_1([lists:zip(L,H)|T]).

nested_tuple_to_list({A,B}) ->
			  nested_tuple_to_list(A)++[B];
nested_tuple_to_list(A) ->
    [A].

sub_list(List, Len, MaxLen) ->
   sub_list_1(List, Len, MaxLen, []).
sub_list_1(_List, Len, MaxLen, Acc) when Len>MaxLen ->
    Acc;
sub_list_1(List, Len, MaxLen, Acc) when Len==MaxLen ->
    [List|Acc];
sub_list_1(List, Len, MaxLen, Acc) ->
    SubLists = [lists:sublist(List, I, Len)||I<- lists:seq(1, length(List)-Len+1)],    
    sub_list_1(List, Len+1, MaxLen, SubLists++Acc).



pmap(F, L) ->
    S = self(),
    Pids = lists:map(fun(I) -> spawn(fun() -> pmap_f(S, F, I) end) end, L),
    pmap_gather(Pids).

pmap_gather([H|T]) ->
    receive
        {H, Ret} -> [Ret|pmap_gather(T)]
    end;
pmap_gather([]) ->
    [].

pmap_f(Parent, F, I) ->
    Parent ! {self(), (catch F(I))}.


parse_annotate_file(FName, SearchPaths, TabWidth) ->
    FileFormat=refac_util:file_format(FName),
    case refac_epp_dodger:parse_file(FName, [{tab, TabWidth}, {format, FileFormat}]) of
	{ok, Forms} -> 
	    Dir = filename:dirname(FName),
	    DefaultIncl1 = [".","..", "../hrl", "../incl", "../inc", "../include"],
	    DefaultIncl2 = [filename:join(Dir, X) || X <-DefaultIncl1],
	    Includes = SearchPaths++DefaultIncl2,
	    Ms =case refac_epp:parse_file(FName, Includes, [], TabWidth, FileFormat) of
		    {ok, _, {MDefs, MUses}}  -> 
			{dict:from_list(MDefs), dict:from_list(MUses)};
		    _ -> []
		end,	
	    SyntaxTree = refac_recomment:recomment_forms(Forms, []),
	    Info = refac_syntax_lib:analyze_forms(SyntaxTree),
	    AnnAST = annotate_bindings(FName, SyntaxTree, Info, Ms, TabWidth),
	    {ok, {AnnAST, Info}};
	{error, Reason} -> erlang:error(Reason)
    end.
annotate_bindings(FName, AST, Info, Ms, TabWidth) ->
    Toks = refac_util:tokenize(FName, true, TabWidth),
    AnnAST0 = refac_syntax_lib:annotate_bindings(refac_util:add_range(AST, Toks), ordsets:new(), Ms),
    refac_util:add_category(refac_util:add_fun_define_locations(AnnAST0, Info)).
 




