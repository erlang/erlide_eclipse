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
%% Purpose: Incremental, and on-line, similar code detection for Erlang programs.
%%
%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%% 
-module(refac_inc_sim_code).

-export([inc_sim_code_detection/7]).

-compile(export_all).

-include("../include/wrangler.hrl").

-include_lib("stdlib/include/ms_transform.hrl").

-define(DefaultSimiScore, 0.8).

-define(DEFAULT_LEN, 5).
-define(DEFAULT_TOK, 20).
-define(DEFAULT_FREQ, 2).
-define(DEFAULT_SIMI_SCORE, 0.8).

-define(ASTTab, list_to_atom(filename:join(?WRANGLER_DIR, "plt/ast_tab"))).
-define(FileHashTab, list_to_atom(filename:join(?WRANGLER_DIR, "plt/file_hash_tab"))).
-define(VarTab, list_to_atom(filename:join(?WRANGLER_DIR, "plt/var_tab"))).
-define(ExpHashTab, list_to_atom(filename:join(?WRANGLER_DIR, "plt/exp_hash_tab"))).
-define(ExpSeqFile, list_to_atom(filename:join(?WRANGLER_DIR, "plt/exp_seq_file"))).


-spec(inc_sim_code_detection/7::(DirFileList::[filename()|dir()], MinLen::float(), MinToks::integer(),MinFreq::integer(), MinScore::float(), 
			     SearchPaths::[dir()], TabWidth::integer()) -> {ok, string()}). 			     
inc_sim_code_detection(DirFileList, MinLen, MinToks, MinFreq, SimiScore, SearchPaths, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:sim_code_detection(~p,~p,~p,~p, ~p,~p,~p).\n",
		 [?MODULE, DirFileList, MinLen, MinToks, MinFreq, SimiScore, SearchPaths, TabWidth]),
    Files = refac_util:expand_files(DirFileList, ".erl"),
    ASTTab =from_dets(ast_tab, ?ASTTab),
    VarTab= from_dets(var_tab, ?VarTab),
    FileHashTab=from_dets(file_hash_tab,?FileHashTab),
    ExpHashTab =from_dets(expr_hash_tab, ?ExpHashTab),
    ASTPid = start_ast_process(ASTTab),
    case Files of
      [] ->
	  ?wrangler_io("Warning: No files found in the searchpaths specified.", []);
      _ ->
	    _Time1 = now(),
	    generalise_and_hash_ast(Files, FileHashTab, ASTPid, MinLen, VarTab, SearchPaths, TabWidth),
	    ?wrangler_io("Generalise and has hash finished.\n",[]),
	    Dir = filename:dirname(hd(Files)),
	    Cs = get_clone_candidates(ASTPid, MinLen, MinToks, MinFreq, Dir),
	    ?debug("\nInitial candiates finished\n", []),
	    ?wrangler_io("\nNumber of initial clone candidates: ~p\n", [length(Cs)]),
	    CloneCheckerPid = start_clone_check_process(),
	    Cs2 = examine_clone_candidates(Cs, MinLen, MinFreq, SimiScore, ASTTab, VarTab, CloneCheckerPid, 1),
	    stop_clone_check_process(CloneCheckerPid),
	    _Time2 = now(),
	    refac_code_search_utils:display_clone_result(Cs2, "Similar")
    end,
    stop_ast_process(ASTPid),
    to_dets(ASTTab, ?ASTTab),
    to_dets(VarTab, ?VarTab),
    to_dets(FileHashTab, ?FileHashTab),
    to_dets(ExpHashTab, ?ExpHashTab).
    
 
%% Serialise, in breath-first order, and generalise each expression in the AST,
%% and insert them into the AST table. Each object in the AST table has the following format:
%% {{FileName, FunName, Arity, Index}, ExprAST}, where Index is used to identify a specific 
%% expression in the function. 

%%QUESTION: how to check whether a file/function has been changed or not.
%% for files, use MD5; for functions, a function has to be parsed and prettyprinted before
%% calculating its MD5 value.

generalise_and_hash_ast(Files, FileHashTab, ASTPid, MinLen, VarTab, SearchPaths, TabWidth) ->	     
    lists:foreach(fun(File) ->
			  NewCheckSum=refac_misc:filehash(File),
			  case ets:lookup(FileHashTab, File) of
			      [{File, NewCheckSum}]->
				  ok;
			      _ ->
				  generalise_and_hash_ast_1(File, ASTPid, MinLen, VarTab, SearchPaths, TabWidth)
			  end
		  end, Files).


generalise_and_hash_ast_1(FName, ASTPid, MinLen, VarTab, SearchPaths, TabWidth) ->
    Fun = fun (Form) ->
		  case refac_syntax:type(Form) of
		    function ->
			  FunName = refac_syntax:atom_value(refac_syntax:function_name(Form)),
			  Arity = refac_syntax:function_arity(Form),
			  AllVars = refac_misc:collect_var_source_def_pos_info(Form),
			  ets:insert(VarTab, {{FName, FunName, Arity}, AllVars}),
			  ast_traverse_api:full_tdTP(fun generalise_and_hash_ast_2/2,
						     Form, {FName, FunName, Arity, ASTPid, MinLen});
		      _ -> ok
		  end
	  end,
    {ok, {AnnAST, _Info}} = refac_util:quick_parse_annotate_file(FName, SearchPaths, TabWidth),
    refac_syntax:form_list_elements(AnnAST),
    lists:foreach(fun (F) -> Fun(F) end, refac_syntax:form_list_elements(AnnAST)).


generalise_and_hash_ast_2(Node, {FName, FunName, Arity, ASTPid, MinLen}) ->
    F=fun(Body) ->
	       case length(Body)>=MinLen of
	       	  true ->  
		       %% only store those expression sequences whose length is 
		      %% greater than the threshold specified.
		       insert_to_ast_tab(ASTPid, {{FName, FunName, Arity}, Body});
		   false ->
		       ok
	       end
      end,
    case refac_syntax:type(Node) of
	clause ->
	    Body = refac_syntax:clause_body(Node),
	    F(Body),
	    {Node, false};
	block_expr ->
	    Body = refac_syntax:block_expr_body(Node),
	    F(Body),		
	    {Node, false};
	try_expr ->
	    Body = refac_syntax:try_expr_body(Node),
	    F(Body),
	    {Node, false};
	_ -> {Node, false}
    end.

%% returns true if the expression can be replaced by a variable. 
%% Note: we don't replace arbitary expressions with variables!
variable_replaceable(Exp) ->
    case lists:keysearch(category, 1, refac_syntax:get_ann(Exp)) of
	{value, {category, record_field}} -> false;
	{value, {category, record_type}} -> false;
	{value, {category, guard_expression}} -> false;
	{value, {category, macro_name}} -> false;
	{value, {category, pattern}} ->
	    refac_syntax:is_literal(Exp) orelse
		refac_syntax:type(Exp) == variable;
	_ ->
	    T = refac_syntax:type(Exp),
	    not lists:member(T, [match_expr, operator, generator,case_expr, if_expr, fun_expr, 
				 receive_expr, clause, query_expr, try_expr,
				 catch_expr, cond_expr, block_expr]) andalso
		refac_misc:get_var_exports(Exp) == []
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                    %%
%% Store the AST representation of expression statements in ETS table %%
%%                                                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_ast_process(ASTTab) ->
    HashPid = start_hash_process(),
    spawn_link(fun()-> 
		       ast_loop(ASTTab, {'_','_','_', 1, HashPid})
	       end).

stop_ast_process(Pid)->
    Pid ! stop.

insert_to_ast_tab(Pid, {{M, F, A}, ExprASTs}) ->
    Pid ! {add, {{M, F, A}, ExprASTs}}.

get_clone_candidates(Pid, MinLen, MinToks, MinFreq, Dir) ->
    Pid ! {get_clone_candidates, self(), MinLen, MinToks, MinFreq, Dir},
    receive
      {Pid, Cs} ->
	  Cs
    end.

ast_loop(ASTTab, {CurM, CurF, CurA, Index, HashPid}) ->
    receive
      {add, {{M, F, A}, ExprASTs}} ->
	  case {M, F, A} == {CurM, CurF, CurA} of
	    true ->
		Len = length(ExprASTs),
		ExprASTsWithIndex = lists:zip(ExprASTs, lists:seq(0, Len - 1)),
		[begin
		   ets:insert(ASTTab, {{M, F, A, Index + I}, remove_env_attr(E)}),
		   NoOfToks = no_of_tokens(E),
		   E1 = do_generalise(E),
		   HashVal = erlang:md5(refac_prettypr:format(E1)),
		   insert_hash(HashPid, {HashVal, {{M, F, A, Index + I}, NoOfToks}})
		 end || {E, I} <- ExprASTsWithIndex],
		insert_dummy_entry(HashPid),
		ast_loop(ASTTab, {CurM, CurF, CurA, Index + Len, HashPid});
	    false ->
		Len = length(ExprASTs),
		ExprASTsWithIndex = lists:zip(ExprASTs, lists:seq(1, Len)),
		[begin
		   ets:insert(ASTTab, {{M, F, A, I}, remove_env_attr(E)}),
		   NoOfToks = no_of_tokens(E),
		   E1 = do_generalise(E),
		   HashVal = erlang:md5(refac_prettypr:format(E1)),
		   insert_hash(HashPid, {HashVal, {{M, F, A, I}, NoOfToks}})
		 end || {E, I} <- ExprASTsWithIndex],
		insert_dummy_entry(HashPid),
		ast_loop(ASTTab, {M, F, A, Len + 1, HashPid})
	  end;
      {get_clone_candidates, From, MinLen, MinToks, MinFreq, Dir} ->
	  Cs = get_clone_candidates(HashPid, MinLen, MinToks, MinFreq, Dir),
	  From ! {self(), Cs},
	  ast_loop(ASTTab, {CurM, CurF, CurA, Index, HashPid});
      stop ->
	    stop_hash_process(HashPid),
	    ok;
	_Msg -> 
	    ?wrangler_io("Unexpected message:\n~p\n",[_Msg]),
	    ok
    end.
	    
    
do_generalise(Node) ->
    F0 =fun (T, _Others) ->
		case variable_replaceable(T) of
		    true ->
			{refac_syntax:variable('Var'), true};
		    false -> {T, false}
		end
	end,
    element(1, ast_traverse_api:stop_tdTP(F0, Node, [])).
    
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                    %%
%%  Hash the AST representation of expressions using MD5, and map     %%
%%  sequence of expression into sequences of indexes.                 %%
%%                                                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_hash_process() ->	
    ExpHashTab =from_dets(expr_hash_tab, ?ExpHashTab),
    case file:consult(?ExpSeqFile) of
	{ok, Data} ->
	    Data;
	_ -> Data=[{{{'_','_','_','_'},0},'#'}],
	     Data
    end,
    spawn_link(fun()->hash_loop({1,ExpHashTab,Data}) end).

stop_hash_process(Pid) ->
    Pid!stop.

insert_hash(Pid, {HashVal, Elem={{_M,_F, _A, _Index}, _NumTok}}) ->
    Pid ! {add, {HashVal, Elem}}.

insert_dummy_entry(Pid) ->
    Pid ! add_dummy.

hash_loop({Index, ExpHashTab, Data}) ->
    receive
      {add, {Key, {{M, F, A, Index1}, NumOfToks}}} ->
	  case ets:lookup(ExpHashTab, Key) of
	    [{Key, I}] ->
		hash_loop({Index, ExpHashTab, [{{{M, F, A, Index1}, NumOfToks}, I}| Data]});
	    [] -> ets:insert(ExpHashTab, {Key, Index}),
		  hash_loop({Index + 1, ExpHashTab, [{{{M, F, A, Index1}, NumOfToks}, Index}| Data]})
	  end;
      add_dummy ->
	  hash_loop({Index, ExpHashTab, [{{{'_', '_', '_', '_'}, 0}, '#'}| Data]});
      {get_clone_candidates, From, MinLen, MinToks, MinFreq, Dir} ->
	    Cs = search_for_clones(Dir, lists:reverse(Data), MinLen, MinToks, MinFreq),
	    From ! {self(), Cs},
	    hash_loop({Index, ExpHashTab, Data});
	stop ->
	    to_dets(ExpHashTab, ?ExpHashTab),
	    file:write_file(?ExpSeqFile, term_to_binary(Data)),
	    ok
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                    %%
%%  Hash the AST representation of expressions using MD5, and map     %%
%%  sequence of expression into sequences of indexes.                 %%
%%                                                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_clone_check_process() ->
    spawn_link(fun()->clone_check_loop([]) end).

stop_clone_check_process(Pid) ->
    Pid ! stop.

add_new_clones(Pid, Clones) ->
    Pid ! {add_clone, Clones}.

get_final_clone_classes(Pid, ASTTab) ->
    Pid ! {get_clones, self(), ASTTab},
    receive
      {Pid, Cs} ->
	  Cs
    end.

clone_check_loop(Cs) ->
    receive
	{add_clone,  Clones} ->
	    clone_check_loop(Clones++Cs);
	{get_clones, From, ASTTab} ->
	    Cs1 = remove_sub_clones(lists:keysort(2,Cs)),
	    Cs2=[get_clone_class_in_absolute_locs(C, ASTTab)||C<-Cs1],
	    From ! {self(), Cs2},
	    clone_check_loop(Cs1);       
	stop ->
	    ok;
	_Msg -> 
	    ?wrangler_io("Unexpected message:\n~p\n",[_Msg]),
	    clone_check_loop(Cs)
    end.
 
%%=============================================================================
examine_clone_candidates([], _MinLen, _MinFreq, _SimiScore, ASTTab, _VarTab, Pid, _Num) ->
    get_final_clone_classes(Pid, ASTTab);
examine_clone_candidates([C| Cs], MinLen, MinFreq, SimiScore, ASTTab, VarTab, Pid, Num) ->
    ?wrangler_io("\nChecking the ~pth clone candidate...", [Num]),
    case examine_a_clone_candidate(C, MinLen, MinFreq, SimiScore, ASTTab, VarTab) of
	[] ->
	    ok;
	Clones ->
	    ClonesWithAntiUnifier=[{Ranges, {Len, Freq}, get_anti_unifier(ASTTab, Info)}
				   ||{Ranges, {Len, Freq}, Info}<-Clones],
	    add_new_clones(Pid, ClonesWithAntiUnifier)
    end,
    examine_clone_candidates(Cs, MinLen, MinFreq, SimiScore, ASTTab, VarTab, Pid, Num + 1).

examine_a_clone_candidate(C, MinLen, MinFreq, SimiScore, ASTTab, VarTab) ->
    {Ranges, {_Len, _Freq}} = C,
    examine_clone_class_members(Ranges, C, MinLen, MinFreq, SimiScore, ASTTab, VarTab, []).
  
examine_clone_class_members([], _, _, _, _, _,_, Acc) ->
    remove_sub_clones(lists:keysort(2, Acc));
examine_clone_class_members([Range1| Rs], _C = {Ranges, {Len, Freq}}, MinLen, MinFreq, SimiScore, ASTTab, VarTab, Acc) ->
    {Exprs1, _VarsToExport} = get_expr_list_and_vars_to_export(Range1, ASTTab, VarTab),
    %% refac_io:format("Rs:\n~p\n", [Rs]),
    Res = lists:map(fun (Range2) ->
		       case Range2 == Range1 of
			   true ->  [];
			   _ ->
			       %% refac_io:format("Range2:\n~p\n", [Range2]),
			       {Exprs2, _VarsToExport2} = get_expr_list_and_vars_to_export(Range2, ASTTab, VarTab),
			       do_anti_unification({Range1, Exprs1}, {Range2, Exprs2})
		       end
	       end, Rs),
    Length=length(Range1),
    Clones=process_au_result(Res, Length, MinLen, MinFreq, SimiScore, ASTTab, VarTab,[]),
    case Clones==[] orelse element(1, element(2, (hd(Clones))))/=Length of
	true ->
  	    RemainedRanges=Ranges--[Range1],
	    case length(RemainedRanges)>=MinFreq of 
		true ->
		    %% refac_io:format("RemainedRanges:\n~p\n",[RemainedRanges]),
		    examine_clone_class_members(Rs, {RemainedRanges, {Len, Freq}}, MinLen,
						MinFreq, SimiScore, ASTTab, VarTab, Clones++Acc);
		false ->
		    Clones++Acc
	    end;
	false->
	    Rs1 = element(1, hd(Clones)),
	    RemainedRanges=Ranges--Rs1,
	    case length(RemainedRanges)>=MinFreq of
		true ->
		    %% refac_io:format("RemainedRanges1:\n~p\n",[RemainedRanges]),
		    examine_clone_class_members(Rs -- Rs1, {RemainedRanges, {Len, Freq}}, MinLen, 
						MinFreq, SimiScore, ASTTab, VarTab, Clones++Acc);
		false ->
		    Clones++Acc
	    end			
    end.
    

process_au_result(_AURes, CurLen, MinLen, _MinFreq, _SimiScore, _ASTTab, _VarTab, Acc)
  when CurLen<MinLen ->
    Res =lists:reverse(remove_sub_clones(lists:keysort(2, Acc))),
    Res;
process_au_result(AURes, CurLen, MinLen, MinFreq, SimiScore, ASTTab, VarTab, Acc) ->
    AUSubReses=sub_list(AURes, CurLen),
    Clones=lists:append([process_a_sub_au_result(AUSubRes, MinFreq, SimiScore, ASTTab, VarTab)||AUSubRes<-AUSubReses]),
    case Clones/=[] andalso length(element(1,hd(Clones)))== length(AURes)+1 of
	true -> 
	    TotalLen=length(hd(AURes)),
	    %% refac_io:format("CurLen:\n~p\n", [CurLen]),
	    %% case TotalLen-CurLen<CurLen of
	    %% 	true ->
	    %% 	    process_au_result(AURes, TotalLen-CurLen,MinLen, MinFreq, SimiScore, ASTTab, VarTab, Clones++Acc);
	    %% 	false ->
	    process_au_result(AURes, MinLen-1,MinLen, MinFreq, SimiScore, ASTTab, VarTab, Clones++Acc);
	    %%end;		    
	false ->
	    process_au_result(AURes, CurLen-1,MinLen, MinFreq, SimiScore, ASTTab, VarTab, Clones++Acc)
    end.
    
process_a_sub_au_result(Res, MinFreq,SimiScore, ASTTab, VarTab) ->
    {Range, _, _}=lists:unzip3(hd(Res)),
    {_, VarsToExport} = get_expr_list_and_vars_to_export(Range, ASTTab, VarTab),			       
    NewRes=lists:append([process_a_sub_au_result_1(R, SimiScore, ASTTab, VarTab)||R<-Res]),
    case length(NewRes)< MinFreq-1 of
	true->[];
	_ ->
	    {Ranges, ExportVars, SubSt} = lists:unzip3(NewRes),
	    ExportVars1 = {element(1, lists:unzip(VarsToExport)), lists:usort(lists:append(ExportVars))},
	    [{[Range| Ranges], {length(hd(Res)), length(Ranges) + 1}, {Range,SubSt, ExportVars1}}]
    end.
    
    
process_a_sub_au_result_1(AURes, SimiScore, ASTTab, VarTab) ->
    {Range1, Range2, Subst} = lists:unzip3(AURes),
   %% refac_io:format("R1R2S:\n~p\n", [{Range1, Range2, Subst}]),
    {Exprs1, _VarsToExport1} = get_expr_list_and_vars_to_export(Range1, ASTTab, VarTab),
    {Exprs2, VarsToExport2} = get_expr_list_and_vars_to_export(Range2, ASTTab, VarTab),
    case lists:member(none, Subst) of
	true ->
	    %% refac_io:format("Subst:\n~p\n", [Subst]),
	    [];
	false ->
	    Subst1=lists:append(Subst),
	    {SubExprs1, SubExprs2}=lists:unzip(Subst1),
	    %% refac_io:format("SubExprs2::\n~p\n", [SubExprs2]),
	    %% refac_io:format("Exprs1:~p\n", [Exprs1]),
	    Score1 = simi_score(Exprs1, SubExprs1),
	    Score2 = simi_score(Exprs2, SubExprs2),
	    %% refac_io:format("Score1:\n~p\n", [Score1]),
	    %% refac_io:format("Score2:\n~p\n", [Score2]),
	    case Score1 >= SimiScore andalso Score2 >= SimiScore of
		true ->
		    case anti_unification:subst_sanity_check(Exprs1, Subst1) of
			false ->
			    [];
			_ ->
			    EVs = [E1 || {E1, E2} <- Subst1, refac_syntax:type(E2) == variable,
					 lists:member({refac_syntax:variable_name(E2), get_var_define_pos(E2)}, 
						      VarsToExport2)],
			    [{Range2, EVs, Subst1}]
		    end;
		_ -> []
	    end
    end.

do_anti_unification({Range1,Exprs1}, {Range2, Exprs2}) ->
    %% refac_io:format("Range1 Range2:\n~p\n", [{Range1, Range2}]),
    ZippedExprs1=lists:zip(Range1, Exprs1),
    ZippedExprs2=lists:zip(Range2, Exprs2),
    ZippedExprs=lists:zip(ZippedExprs1, ZippedExprs2),
    [begin
	 %% refac_io:format("Index1, Index2:\n~p\n", [{Index1, Index2}]),
	 %% refac_io:format("anti_unification:\n~p\n", [ anti_unification:anti_unification(E1,E2)]),
	 {Index1, Index2,
	  anti_unification:anti_unification(E1,E2)}
     end|| {{Index1,E1}, {Index2, E2}}<-ZippedExprs].
    
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
    end;
is_sub_clone(_,_) ->
     false.
is_sub_ranges(Ranges1, Ranges2) ->
    lists:all(fun (R1)  -> 
		      lists:any(fun (R2) ->
					R1--R2==[]
				end, Ranges2) 
	      end, Ranges1).


get_var_define_pos(V) ->
    {value, {def, DefinePos}} = lists:keysearch(def,1, refac_syntax:get_ann(V)),
    DefinePos.


get_anti_unifier(ASTTab, {ExprKeys, SubSt, ExportVars}) ->
    Exprs1 = [ExpAST || {ExprKey, _} <- ExprKeys, {_Key, ExpAST} <- ets:lookup(ASTTab, ExprKey)],
    refac_prettypr:format(anti_unification:generate_anti_unifier(Exprs1, SubSt, ExportVars)).


	    
get_clone_class_in_absolute_locs({Ranges, {Len, Freq}, AntiUnifier}, ASTTab)->
    StartEndLocs = [get_clone_member_start_end_loc(R, ASTTab) || R <- Ranges],
    {StartEndLocs, Len, Freq, AntiUnifier}.
   
get_clone_member_start_end_loc(ExprKeys, ASTTab) ->
    {{File, Fun, Arity, Index}, _NumToks} = hd(ExprKeys),
    {{File, Fun, Arity, Index1}, _} = lists:last(ExprKeys),
    [{_, Expr1}] = ets:lookup(ASTTab, {File, Fun, Arity, Index}),
    [{_, Expr2}] = ets:lookup(ASTTab,  {File, Fun, Arity, Index1}),
    {{StartLine, StartCol}, _} = refac_misc:get_start_end_loc(Expr1),
    {_, {EndLine, EndCol}} = refac_misc:get_start_end_loc(Expr2),
    {{File, StartLine, StartCol}, {File, EndLine, EndCol}}.
    
    


get_expr_list_and_vars_to_export(ExprKeys=[{{FName, FunName, Arity, _Index}, _}|_T], ASTTab, VarTab) ->
    Es = [ExpAST||{ExprKey,_}<-ExprKeys, {_Key, ExpAST}<-ets:lookup(ASTTab, ExprKey)],
    AllVars = ets:lookup(VarTab, {FName, FunName, Arity}),
    {_, EndLoc} = refac_misc:get_start_end_loc(lists:last(Es)),
    case AllVars of
	[] -> {Es, []};
	[{_, Vars}] ->
	    ExprBdVarsPos = [Pos || {_Var, Pos} <- refac_misc:get_bound_vars(Es)],
	    VarsToExport = [{V, DefPos} || {V, SourcePos, DefPos} <- Vars,
					   SourcePos > EndLoc,
					   lists:subtract(DefPos, ExprBdVarsPos) == []],
	    {Es, VarsToExport}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                            %%
%%        Search for cloned candidates                        %%
%%                                                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
search_for_clones(Dir, Data, MinLen, MinToks, MinFreq) ->
    F0 = fun (I) ->
		 case is_integer(I) of
		   true -> integer_to_list(I) ++ ",";
		   false -> atom_to_list(I)
		 end
	 end,
    F = fun ({Elem={{_M,_F,_A, _Index},_NumOfToks}, I}) ->
		lists:duplicate(length(F0(I)), {I, Elem})
	end,
    IndexStr = lists:append([F0(I) || {_, I} <- Data]),
   %% refac_io:format("IndexStr:\n~p\n", [IndexStr]),
    SuffixTreeExec = filename:join(?WRANGLER_DIR, "bin/suffixtree"),
    Cs = suffix_tree:get_clones_by_suffix_tree(Dir, IndexStr ++ "&", MinLen, 
					       MinFreq, "0123456789,#&", 1, SuffixTreeExec),
    %%refac_io:format("Cs:\n~p\n", [length(Cs)]),
    Cs1 = lists:append([strip_a_clone({[{S, E}| Ranges], {Len, Freq}}, SubStr, MinLen, MinFreq)
			|| {[{S, E}| Ranges], Len, Freq} <- Cs,
			   SubStr <- [lists:sublist(IndexStr, S, E - S + 1)]]),
    %%refac_io:format("Cs1:\n~p\n", [length(Cs1)]),
    Cs2 = refac_code_search_utils:remove_sub_clones([{R, Len, Freq} || {R, {Len, Freq}} <- Cs1]),
    %%refac_io:format("Cs2:\n~p\n", [length(Cs2)]),
    NewData = lists:append([F(Elem) || Elem <- Data]),
    get_clones_in_ranges([{R, {Len, Freq}} || {R, Len, Freq} <- Cs2],
			 NewData, MinLen, MinToks, MinFreq).
   
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
remove_overlapped_ranges([], _, Acc) ->
    lists:reverse(Acc);
remove_overlapped_ranges([{S, E}| Rs], {S1, E1}, Acc) ->
    case S1 =< S andalso S =< E1 of
      true ->
	  remove_overlapped_ranges(Rs, {S1, E1}, Acc);
      false ->
	  remove_overlapped_ranges(Rs, {S, E}, [{S, E}| Acc])
    end.
get_clones_in_ranges(Cs, Data, MinLen, MinToks, MinFreq) ->
    F0 = fun ({S, E}) ->
		 {_, Ranges} = lists:unzip(lists:sublist(Data, S, E - S + 1)),
		 Ranges1=refac_misc:remove_duplicates(Ranges)
		 %% %% get all the clones with length between MinLen and length(Ranges1);
		%% sub_list(Ranges1, MinLen, length(Ranges1))
	 end,
    F = fun ({Ranges, {_Len, Freq}}) ->
		case Freq >= MinFreq of
		  true ->
			NewRanges = [F0(R) || R <- Ranges],
			%%refac_io:format("NewRanges:\n~p\n", [NewRanges]),
			{NewRanges, {length(hd(NewRanges)), Freq}};
		    _ -> []
		end
	end,
    NewCs=[F(C) || C <- Cs],
    NewCs1=remove_short_clones(NewCs, MinToks,MinFreq),
    NewCs1.
  
remove_short_clones(Cs, MinToks, MinFreq) ->
    F= fun({Rs, {Len, _Freq}}) ->
	       Rs1=[R||R<-Rs, {_Ranges, NumToks}<-[lists:unzip(R)] ,
			    lists:sum(NumToks)>=MinToks],
	       Freq1 = length(Rs1),
	       case Freq1>=MinFreq of
		   true ->
		       [{Rs1, {Len, Freq1}}];
		   false->
		       []
	       end
       end,
    lists:append([F({Rs, {Len, Freq}})||{Rs, {Len, Freq}}<-Cs]).




sub_list(ListOfList, Len) ->
    zip_list([sub_list_1(L, Len) ||L<-ListOfList]).
    
sub_list_1(List, Len)->
    [lists:sublist(List, I, Len)||I<- lists:seq(1, length(List)-Len+1)].

zip_list(ListOfLists) ->    
    zip_list_1(ListOfLists, []).

zip_list_1([[]|_T], Acc)  ->
    Acc;
zip_list_1(ListOfLists, Acc)->      
    zip_list_1([tl(L) || L <-ListOfLists],
	       Acc ++ [[hd(L)|| L  <- ListOfLists]]).


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
    Parent ! {self(), catch F(I)}.

no_of_tokens(Node) when is_list(Node)->
    Str = refac_prettypr:format(refac_syntax:block_expr(Node)),
    {ok, Toks,_}=refac_scan:string(Str, {1,1}, 8, unix),
    length(Toks)-2;
no_of_tokens(Node) ->
    Str = refac_prettypr:format(Node),
    {ok, Toks,_} =refac_scan:string(Str, {1,1}, 8, unix),
    length(Toks).


from_dets(Ets, Dets) when is_atom(Ets) ->
    EtsRef = ets:new(Ets, [set, public]),
    case dets:open_file(Dets, [{access, read}]) of
      {ok, D} ->
	  true = ets:from_dets(EtsRef, D),
	  ok = dets:close(D),
	  EtsRef;
      {error, _Reason} -> 
	    EtsRef
    end.

to_dets(Ets, Dets) ->
    try file:delete(Dets) 
    catch 
	{error, {file_error, _, enoent}} -> ok;
	{error, Reason} -> throw({error, Reason})
    end,
    MinSize = ets:info(Ets, size),
    Res= dets:open_file(Dets, [{min_no_slots, MinSize}]),
    {ok, DetsRef} = Res,
    ok = dets:from_ets(DetsRef, Ets),
    ok = dets:sync(DetsRef),
    ok = dets:close(DetsRef),
    ets:delete(Ets).
	



simi_score(Expr, SubExprs) ->
    %% refac_io:format("Expr:\n~p\n", [Expr]),
    %% refac_io:format("SubExprs:\n~p\n", [SubExprs]),
    %% refac_io:format("Size1:\n~p\n", [no_of_nodes(Expr)]),
    %% refac_io:format("Size2:\n~p\n", [no_of_nodes(SubExprs)]),
    case no_of_nodes(Expr) of
      0 -> 0;
      ExprSize ->
	    NonVarExprs = [E || E <- SubExprs, refac_syntax:type(E) =/= variable],
	    NoOfNewVars = length(NonVarExprs),
	    Res = 1 - (no_of_nodes(SubExprs) - length(SubExprs)
		       + NoOfNewVars * (NoOfNewVars + 1) / 2) / ExprSize,
	    %% Res =1 -((no_of_nodes(SubExprs)-length(SubExprs))/ExprSize),
	    Res
    end.
   
no_of_nodes(Nodes) when is_list(Nodes) ->
    lists:sum([no_of_nodes(N)||N<-Nodes]);
no_of_nodes(Node) ->
    case refac_syntax:is_leaf(Node) of
	true -> 1;
	_ ->
	    lists:sum([no_of_nodes(T)||
			  T<-refac_syntax:subtrees(Node)])
    end.

remove_env_attr(Node) ->
    ast_traverse_api:full_buTP(fun (T, _Others) -> refac_misc:delete_from_ann(T, env)
			       end, Node, {}).

%% Some notes:

%% Threshold parameters:
%% MinLen; MinToks; MinFreq; Similairty Score.
%% Data to cache:
%% Dets/ets tables: AST table, Var table, HashVal table;
%% file:  expression seq/index file. list-> to file; 



%% refac_inc_sim_code:inc_sim_code_detection(["c:/cygwin/home/hl/suites/bearer_handling_and_qos/test"],5, 20, 2, 0.8,[],8).
