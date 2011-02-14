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
%% Incremental similar code detection for Erlang programs.
%% 
%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%% 
-module(refac_inc_sim_code).

%% API for emacs use.
-export([inc_sim_code_detection/8, 
	 inc_sim_code_detection_in_buffer/8]).

%% API for eclipse use.
-export([inc_sim_code_detection_eclipse/8]).

%% API for command line use.
-export([inc_sim_code_detection_command/8]).

%% API.
-export([inc_sim_code_detection/6]). 

%%-define(debug, true).

-define(INC, false).

-include("../include/wrangler.hrl").

-include_lib("stdlib/include/ms_transform.hrl").

%% default threshold values.
-define(DefaultSimiScore, 0.8).
-define(DEFAULT_LEN, 5).
-define(DEFAULT_TOKS, 40).
-define(DEFAULT_FREQ, 2).
-define(DEFAULT_SIMI_SCORE, 0.8).
-define(DEFAULT_NEW_VARS, 4).
-define(MIN_TOKS, 10).

%% record to store the threshold values.
-record(threshold, 
	{min_len = ?DEFAULT_LEN,
	 min_freq= ?DEFAULT_FREQ,
	 min_toks= ?DEFAULT_TOKS,
	 max_new_vars =?DEFAULT_NEW_VARS,
	 simi_score=?DEFAULT_SIMI_SCORE}).

%% Ets tables uses to cache data to avoid re-evaluation.
-define(ASTTab, get_temp_file_path("ast_tab")).
-define(FileHashTab, get_temp_file_path("file_hash_tab")).
-define(VarTab, get_temp_file_path("var_tab")).
-define(ExpHashTab, get_temp_file_path("exp_hash_tab")).
-define(ExpSeqFile, get_temp_file_path("exp_seq_file")).
-define(CloneTab,  get_temp_file_path("clone_tab")).
-define(Threshold, get_temp_file_path("threshold")).
%% record the store the ets/dets table names.

get_temp_file_path(Tab) ->
    list_to_atom(case wrangler_ast_server:get_temp_dir() of
			 none ->
			     "none";
			 Dir ->filename:join(Dir, Tab)
		     end).
-record(tabs, 
	{ast_tab,
	 var_tab, 
	 file_hash_tab,
	 exp_hash_tab,
	 clone_tab
	}).

%% A record representing a clone class.
%% -record(clone, 
%% 	 {ranges,  %% start end end locations/expression of each clone instance.
%% 	  len,     %% the length of each clone instance.
%% 	  freq,    %% the number of duplication times.
%% 	  au       %% the anti-unification of the clone class.
%% 	 }).

%% -spec(inc_sim_code_detection_in_buffer/8::(FileName::filename(), MinLen::float(), MinToks::integer(),
%%  				 MinFreq::integer(),  MaxVars:: integer(),SimiScore::float(), 
%%  				 SearchPaths::[dir()], TabWidth::integer()) -> {ok, string()}).
%% 
inc_sim_code_detection_in_buffer(FileName, MinLen1, MinToks1, MinFreq1, MaxVars1, SimiScore1, SearchPaths, TabWidth)->
    inc_sim_code_detection([FileName], MinLen1, MinToks1, MinFreq1, MaxVars1, SimiScore1, SearchPaths, TabWidth).
  
%% -spec(inc_sim_code_detection/8::(DirFileList::[filename()|dir()], MinLen::float(), MinToks::integer(),
%%  				 MinFreq::integer(),  MaxVars:: integer(),SimiScore::float(), 
%%  				 SearchPaths::[dir()], TabWidth::integer()) -> {ok, string()}).

inc_sim_code_detection(DirFileList,MinLen1,MinToks1,MinFreq1,MaxVars1,SimiScore1,SearchPaths,TabWidth) ->
    ?wrangler_io("\nCMD: ~p:inc_sim_code_detection(~p,~p,~p,~p,~p, ~p,~p,~p).\n",
		 [?MODULE,DirFileList,MinLen1,MinToks1,MinFreq1,MaxVars1,SimiScore1,SearchPaths,TabWidth]),
    {MinLen,MinToks,MinFreq,MaxVars,SimiScore} = get_parameters(MinLen1,MinToks1,MinFreq1,MaxVars1,SimiScore1),
    Cmd = io_lib:format("\nCMD: ~p:inc_sim_code_detection(~p,~p,~p,~p,~p, ~p,~p,~p).",
			[?MODULE,DirFileList,MinLen,MinToks,MinFreq,MaxVars,SimiScore,SearchPaths,TabWidth]),
    Files = refac_util:expand_files(DirFileList,".erl"),
    Cs = case Files of
	     [] ->
		 ?wrangler_io("Warning: No files found in the searchpaths specified.",[]),
		 [];
	     _ -> inc_sim_code_detection(Files, {MinLen, MinToks, MinFreq, MaxVars, SimiScore},
					 SearchPaths, TabWidth, emacs, ?INC)
	 end,
    CloneReport = gen_clone_report(Cs),
    LogMsg = Cmd ++ " \nNum of clones detected: "++ integer_to_list(length(Cs)) ++ ".\n",
    {ok, lists:flatten(LogMsg)++CloneReport}.


%%-spec(inc_sim_code_detection_command/8::(DirFileList::[filename()|dir()], MinLen::integer(), MinToks::integer(),
%%					      MinFreq::integer(),  MaxVars::integer(),SimiScore::float(), 
%%					      SearchPaths::[dir()], TabWidth::integer()) -> {ok, string()}).

inc_sim_code_detection_command(DirFileList,MinLen1,MinToks1,MinFreq1,MaxVars1,SimiScore1,SearchPaths,TabWidth) ->
    {MinLen,MinToks,MinFreq,MaxVars,SimiScore} = get_parameters_eclipse(MinLen1,MinToks1,MinFreq1,MaxVars1,SimiScore1),
    Files = refac_util:expand_files(DirFileList,".erl"),
    case Files of
	[] ->
	    ?wrangler_io("Warning: No files found in the searchpaths specified.",[]);
	_ -> Cs = inc_sim_code_detection(Files, {MinLen, MinToks, MinFreq, MaxVars, SimiScore},
					 SearchPaths, TabWidth, command, ?INC),
	     refac_code_search_utils:display_clone_result(lists:reverse(Cs), "Similar")
    end,
    {ok, "Similar code detection finished."}.


%%-spec(inc_sim_code_detection_eclipse/8::(DirFileList::[filename()|dir()], MinLen::integer(), MinToks::integer(),
%%				 MinFreq::integer(),  MaxVars:: integer(),SimiScore::float(), 
%%				 SearchPaths::[dir()], TabWidth::integer())->
%%				       [{[{{{filename(), integer(), integer()},
%%					    {filename(), integer(), integer()}}, string()}], 
%%					 integer(), integer(), string()}]).

inc_sim_code_detection_eclipse(DirFileList,MinLen1,MinToks1,MinFreq1,MaxVars1,SimiScore1,SearchPaths,TabWidth) ->
    ?wrangler_io("\nCMD: ~p:inc_sim_code_detection(~p,~p,~p,~p,~p, ~p,~p,~p).\n",
		 [?MODULE,DirFileList,MinLen1,MinToks1,MinFreq1,MaxVars1,SimiScore1,SearchPaths,TabWidth]),
    {MinLen,MinToks,MinFreq,MaxVars,SimiScore} = get_parameters_eclipse(MinLen1,MinToks1,MinFreq1,MaxVars1,SimiScore1),
    Files = refac_util:expand_files(DirFileList,".erl"),
    case Files of
	[] ->
	    [];
	_ -> inc_sim_code_detection(Files, {MinLen, MinToks, MinFreq, MaxVars, SimiScore},
				    SearchPaths, TabWidth, eclipse, ?INC)
    end.

inc_sim_code_detection(Files, {MinLen, MinToks, MinFreq, MaxVars, SimiScore},
		       SearchPaths, TabWidth, Editor, Inc) ->
    %% dets tables used to cache information.
    Tabs = #tabs{ast_tab = from_dets(ast_tab,?ASTTab,Inc),
		 var_tab = from_dets(var_tab,?VarTab,Inc),
		 file_hash_tab = from_dets(file_hash_tab,?FileHashTab,Inc),
		 exp_hash_tab = from_dets(expr_hash_tab,?ExpHashTab,Inc),
		 clone_tab = from_dets(expr_clone_tab,?CloneTab,Inc)},
    %% Threshold parameters.
    Threshold = #threshold{min_len = MinLen,
			   min_freq = MinFreq,
			   min_toks = MinToks,
			   max_new_vars = MaxVars,
			   simi_score = SimiScore},
    %% Clone detection.
    Cs = inc_sim_code_detection(Files,Threshold,Tabs,SearchPaths,TabWidth,Editor,Inc),
    case Inc of
	true ->
	    %% output cache information to dets tables.
	    to_dets(Tabs#tabs.ast_tab,?ASTTab),
	    to_dets(Tabs#tabs.var_tab,?VarTab),
	    to_dets(Tabs#tabs.file_hash_tab,?FileHashTab),
	    to_dets(Tabs#tabs.exp_hash_tab,?ExpHashTab),
	    to_dets(Tabs#tabs.clone_tab,?CloneTab),
	    write_file(?Threshold,term_to_binary(Threshold));
	false ->
	    ok
    end,
    Cs.

%% incremental clone detection.
inc_sim_code_detection(Files, Thresholds, Tabs, SearchPaths, TabWidth, Editor, Inc) ->
    {FilesDeleted, _FilesChanged, _NewFiles} = get_file_status_info(Files, Tabs, Thresholds),
    ?debug("FileChanged: ~p\n", [length(FilesDeleted++FilesChanged++NewFiles)]),
    _Time1 = time(),
    ?debug("Time1:\n~p\n", [Time1]),
    
    %% remove information related to files that no longer exist from ets tables.
    remove_old_file_entries(FilesDeleted, Tabs),
    
    HashPid = start_hash_process(FilesDeleted,Inc),
    
    ASTPid = start_ast_process(Tabs#tabs.ast_tab, HashPid),
    
    %% generate and hash those new, or changed, files.
    generalise_and_hash_ast(Files, Thresholds, Tabs, ASTPid, HashPid, SearchPaths, TabWidth),
    ?wrangler_io("Generalisation and hash finished.\n", []),
    
    %% Generate clone candidates using suffix tree based clone detection techniques.
    Dir = filename:dirname(hd(Files)),
    {ok, OutFileName} = get_clone_candidates(ASTPid, Thresholds, Dir),
    {ok, Res} = file:consult(OutFileName),
    file:delete(OutFileName),
    Cs0 = case Res of
	      [] -> [];
	      [R] -> R
	  end,
    %%This will be removed once I've fixed the problem with the C 
    %%suffix tree implementation.
    %% The problem was that that are some duplications in the 
    %% list of {Strid, StartIndex}.
    Cs = process_initial_clones(Cs0),
    
    %%?wrangler_io("\nInitial candiates finished\n", []),
    refac_io:format("Number of initial clone candidates: ~p\n", [length(Cs)]),
    CloneCheckerPid = start_clone_check_process(Tabs),
    %% examine each clone candiate and filter false positives.
    Cs2 = examine_clone_candidates(Cs, Thresholds, Tabs, CloneCheckerPid, ASTPid, 1),
    _Time2 = time(),
    Cs3 = combine_clones_by_au(Cs2),
    ?debug("\n Time Used: ~p\n", [{Time1, Time2}]),
    case Editor of
	emacs ->
	    refac_code_search_utils:display_clone_result(lists:reverse(Cs3), "Similar"),
	    stop_clone_check_process(CloneCheckerPid),
	    stop_hash_process(HashPid),
	    stop_ast_process(ASTPid),
	    Cs3;
	_ ->
	    stop_clone_check_process(CloneCheckerPid),
	    stop_hash_process(HashPid),
	    stop_ast_process(ASTPid),
	    Cs3
    end.
   
process_initial_clones(Cs) ->
    [begin
	 Rs1 =sets:to_list(sets:from_list(Rs)),
	 {Rs1, Len, length(Rs1)}
     end
     ||{Rs, Len, _Freq}<-Cs].

%% Remove data for deleted files.
remove_old_file_entries(FilesDeleted, Tabs) ->
    FileHashTab=Tabs#tabs.file_hash_tab,
    [ets:delete(FileHashTab, File)||File<-FilesDeleted],
    VarTab = Tabs#tabs.var_tab,
    [ets:match_delete(VarTab, {{File, '_', '_'},'_','_'})
     || File<-FilesDeleted],
    ASTTab = Tabs#tabs.ast_tab,
    [ets:match_delete(ASTTab, {{File, '_','_','_'}, '_'}) 
     || File <-FilesDeleted].
    
%% Serialise, in breath-first order, and generalise each expression in the AST,
%% and insert them into the AST table. Each object in the AST table has the following format:
%% {{FileName, FunName, Arity, Index}, ExprAST}, where Index is used to identify a specific 
%% expression in the function. 
generalise_and_hash_ast(Files, Threshold, Tabs, ASTPid, HashPid ,SearchPaths, TabWidth) ->
    lists:foreach(fun(File) ->
			  generalise_and_hash_file_ast(
			    File, Threshold, Tabs, ASTPid, HashPid, SearchPaths, TabWidth)
		  end, Files).

generalise_and_hash_file_ast(File, Threshold, Tabs, ASTPid, HashPid, SearchPaths, TabWidth) ->
    NewCheckSum = refac_util:filehash(File),
    case ets:lookup(Tabs#tabs.file_hash_tab, File) of
	[{File, NewCheckSum}] ->
	    ?debug("\nFile not changed:~p\n", [File]),
	    ok;
	[{File, _NewCheckSum1}] ->
	    ?debug("\n File changed:~p\n", [File]),
	    ets:insert(Tabs#tabs.file_hash_tab, {File, NewCheckSum}),
	    generalise_and_hash_file_ast_1(
	      File, Threshold, Tabs, ASTPid, HashPid, false, SearchPaths, TabWidth);
	[] ->
	    ?debug("New file:~p\n",[File]),
	    ets:insert(Tabs#tabs.file_hash_tab, {File, NewCheckSum}),
	    generalise_and_hash_file_ast_1(
	      File, Threshold, Tabs, ASTPid, HashPid, true, SearchPaths, TabWidth)
    end.

%% %% Generalise and hash the AST for an single Erlang file.
%% generalise_and_hash_file_ast_1(FName, Threshold, Tabs, ASTPid, IsNewFile, SearchPaths, TabWidth) ->
%%     case {ok, {AnnAST, _Info}} = refac_util:quick_parse_annotate_file(FName, SearchPaths, TabWidth),
%%     Forms = case refac_syntax:form_list_elements(AnnAST),
%%     F =fun (Form) -> 
%% 	       case refac_syntax:type(Form) of 
%% 		   function ->
%% 		       %% only process function definitions.
%% 		       generalise_and_hash_function_ast(Form, FName, IsNewFile, Threshold, Tabs, ASTPid);
%% 		   _ -> ok
%% 	       end
%%        end,
%%     lists:foreach(fun(Form) -> F(Form) end, Forms).


%% Generalise and hash the AST for an single Erlang file.
generalise_and_hash_file_ast_1(FName, Threshold, Tabs, ASTPid, HashPid, IsNewFile, SearchPaths, TabWidth) ->
    Forms = try wrangler_ast_server:quick_parse_annotate_file(FName, SearchPaths, TabWidth) of
		{ok, {AnnAST, _Info}} ->
		    refac_syntax:form_list_elements(AnnAST)
	    catch
		_E1:_E2 -> []
	    end,
    F = fun (Form) ->
		case refac_syntax:type(Form) of
		    function ->
			%% only process function definitions.
			generalise_and_hash_function_ast(Form, FName, IsNewFile, Threshold, Tabs, ASTPid, HashPid);
		    _ -> ok
		end
	end,
    lists:foreach(fun (Form) -> F(Form) end, Forms).


%% generalise and hash the AST of a single function.
generalise_and_hash_function_ast(Form, FName, IsNewFile, Threshold, Tabs, ASTPid, HashPid) ->
    FunName = refac_syntax:atom_value(refac_syntax:function_name(Form)),
    Arity = refac_syntax:function_arity(Form),
    HashVal = erlang:md5(format(Form)),
    case IsNewFile of
	true ->
	  %% a new file;
	    generalise_and_hash_function_ast_1(FName, Form, FunName, Arity, HashVal, Threshold, Tabs, ASTPid, HashPid);
	false ->
	    %% a chnanged file;
	    case ets:lookup(Tabs#tabs.var_tab, {FName, FunName, Arity}) of
		[{{FName, FunName, Arity}, HashVal, _VarInfo}] ->
		    %% Same Hash value means that this function has not be
		    %% syntactically changed, but location might be changed.
		    %% StartLine is used to get the absolute location of the function.
		    {StartLine, _} = refac_syntax:get_pos(Form),
		    quick_hash_function(ASTPid, {{FName, FunName, Arity}, StartLine});
		%% function has been changed since last run of clone detection.
		[{{FName, FunName, Arity}, _HashVal, _VarInfo}] ->
		    remove_entry(HashPid, {FName, FunName, Arity}),
		    generalise_and_hash_function_ast_1(FName, Form, FunName, Arity, HashVal, Threshold, Tabs, ASTPid, HashPid);
		_ ->
		    %% Function is new
		    generalise_and_hash_function_ast_1(FName, Form, FunName, Arity, HashVal, Threshold, Tabs, ASTPid, HashPid)
	    end
    end.

%% generalise and hash a function that is either new or has been changed since last run of clone detection.
generalise_and_hash_function_ast_1(FName, Form, FunName, Arity, HashVal, Threshold, Tabs, ASTPid, HashPid) ->
    {StartLine, _} = refac_syntax:get_pos(Form),
    %% Turn absolute locations to relative locations, so 
    %% so that the result can be reused.
    Form1 = absolute_to_relative_loc(Form, StartLine),
    %% all locations are relative locations.
    %% variable binding information is needed by the anti-unification process.
    AllVars = refac_util:collect_var_source_def_pos_info(Form1),
    %% I also put the Hashvalue of a function in var_tab.
    ets:insert(Tabs#tabs.var_tab, {{FName, FunName, Arity}, HashVal, AllVars}),
    ast_traverse_api:full_tdTP(fun generalise_and_hash_function_ast_2/2,
			       Form1, {FName, FunName, Arity, ASTPid, HashPid, Threshold, StartLine}).
   
%% generalise and has the function AST.
generalise_and_hash_function_ast_2(Node, {FName, FunName, Arity, ASTPid, _HashPid,  Threshold, StartLine}) ->
    F = fun (Body) ->
		case length(Body) >= Threshold#threshold.min_len of
		    true ->
			%% only store those expression sequences whose  
			%% length is greater than the threshold specified.
			insert_to_ast_tab(ASTPid, {{FName, FunName, Arity}, Body, StartLine});
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                    %%
%% Store the AST representation of expression statements in ETS table %%
%%                                                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_ast_process(ASTTab, HashPid) ->
    %% Dummy entries are used to sparate entries from different functions.
    spawn_link(fun () -> 
		       ast_loop(ASTTab, {'_', '_', '_', 1, HashPid})
	       end).

%% stop the ast process.
stop_ast_process(Pid)->
    Pid ! stop.

%% Insert a sequence of expressions into the AST table. 
%% The sequence of expressions to be inserted are from 
%% the same expression body (clause_expr, block_expr, try_expr).
insert_to_ast_tab(Pid, {{M, F, A}, ExprASTs, StartLine}) ->
    Pid ! {add, {{M, F, A}, ExprASTs, StartLine}}.

%% Quick hash only updates the location information, as the 
%% actual entries already exist.
quick_hash_function(Pid, {{FName, FunName, Arity}, StartLine}) ->
    Pid ! {quick_hash, {{FName, FunName, Arity}, StartLine}}.

%% Get initial clone candidates.    
get_clone_candidates(Pid, Thresholds, Dir) ->
    Pid ! {get_clone_candidates, self(), Thresholds, Dir},
    receive
	{Pid, {ok, OutFileName}}->
	    {ok, OutFileName}
    end.
get_clone_in_range(Pid, C) ->
    Pid! {get_clone_in_range, self(), C},
    receive
	{Pid, C1} ->
	    C1
    end.

    
ast_loop(ASTTab, {CurM, CurF, CurA, Index, HashPid}) ->
    receive
      {add, {{M, F, A}, ExprASTs, StartLine}} ->
	    NewIndex = case {M, F, A} == {CurM, CurF, CurA} of
			 true ->
			     Index;  %% Same function.
			 false ->
			     1   %% A new function.
		     end,
	    Len = length(ExprASTs),
	    %% Each expression is assigned with an index.
	    ExprASTsWithIndex = lists:zip(ExprASTs, lists:seq(0, Len - 1)),
	    insert_and_hash_exprs(ASTTab, HashPid, {M,F,A}, StartLine, 
				NewIndex, ExprASTsWithIndex),
	    ast_loop(ASTTab, {M, F, A, NewIndex + Len, HashPid});
	{quick_hash, {{FName, FunName, Arity}, StartLine}} ->
	    update_hash(HashPid, {{FName, FunName, Arity}, StartLine}),
	    ast_loop(ASTTab, {FName, FunName, Arity, Index, HashPid});
	{get_clone_candidates, From, Thresholds, Dir} ->
	    {ok, OutFileName} = get_clone_candidates(HashPid, Thresholds, Dir),
	    From ! {self(), {ok, OutFileName}},
	    ast_loop(ASTTab, {CurM, CurF, CurA, Index, HashPid});
	{get_clone_in_range, From, C} ->
	    C1 = get_clone_in_range(HashPid, C),
	    From !{self(), C1},
	    ast_loop(ASTTab, {CurM, CurF, CurA, Index, HashPid});
	stop ->
	    ok;
	_Msg ->
	    ?wrangler_io("Unexpected message:\n~p\n", [_Msg]),
	  ok
    end.

%% Insert an expression into the AST table, generalise it, and 
%% hash the result.
insert_and_hash_exprs(ASTTab, HashPid, {M,F,A}, StartLine, 
		      NewIndex, ExprASTsWithIndex) ->
    HashValExprPairs=[generalise_and_hash_expr(ASTTab, {M, F, A}, StartLine,
					       NewIndex, {E, I})
		      ||{E, I}<-ExprASTsWithIndex],
    insert_hash(HashPid, {{M, F, A}, HashValExprPairs}).

generalise_and_hash_expr(ASTTab, {M, F, A}, StartLine,
			 StartIndex, {Expr, RelativeIndex}) ->
    %% Num of tokens is used to chech the size of a clone candidate.
    NoOfToks = no_of_tokens(Expr),
    %% insert the AST of an expression into the ast table.
    ets:insert(ASTTab, {{M, F, A, StartIndex + RelativeIndex}, Expr}),
    E1 = do_generalise(Expr),
    %% get the hash values of the generalised expression.
    HashVal = erlang:md5(format(E1)),
    %% the location here is relative location.
    StartEndLoc = refac_util:get_start_end_loc(Expr),
    {HashVal, {StartIndex + RelativeIndex,
	       NoOfToks, StartEndLoc, StartLine}}.

%% replace an AST node if the node can be generalised.
do_generalise(Node) ->
    F0 = fun (T, _Others) ->
		 case refac_code_search_utils:generalisable(T) of
		   true ->
		       {refac_syntax:variable('Var'), true};
		   false -> {T, false}
		 end
	 end,
    element(1, ast_traverse_api:stop_tdTP(F0, Node, [])).
    
   
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                    %%
%%  Hash the AST representation of generalised expressions using MD5, %%
%%  and map sequences of expressions into sequences of indexes.       %%
%%                                                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


start_hash_process(FilesDeleted,Inc) ->
    %% I put NewFiles here too! 
    %ObsoleteFiles = FilesDeleted ++ FilesChanged ++ NewFiles,  
    ExpHashTab = from_dets(expr_hash_tab, ?ExpHashTab, Inc),
    case file:read_file(?ExpSeqFile) of
	{ok, Binary} ->
	    Data = binary_to_term(Binary),
	    %% remove obsolete entries.
	    NewData = filter_out_data_to_reuse(Data, FilesDeleted),
	    NextSeqNo = case NewData of
			    [] -> 1;
			    [{No, _, _}| _] -> No+1
			end,
	    spawn_link(fun () -> hash_loop({NextSeqNo, ExpHashTab, NewData},Inc)
		       end);
	_Res ->
	    ets:delete_all_objects(ExpHashTab),
	    spawn_link(fun () -> hash_loop({1, ExpHashTab, []},Inc) end)
    end.

filter_out_data_to_reuse(Data, FilesDeleted)->
    [case lists:member(File, FilesDeleted) of 
	 true ->{Seq, {File, F, A}, []};
	 false ->{Seq, {File, F, A}, 
		  [{{Index, Toks, Local, StartLine, false}, HashValIndex}
		   ||{{Index, Toks, Local, StartLine, _}, HashValIndex}<-ExprHashPairs]}
     end
     ||{Seq, {File, F, A}, ExprHashPairs}<-Data].

 
stop_hash_process(Pid) ->
    Pid!stop.

insert_hash(Pid, {{M, F, A}, HashExprPairs}) ->
    Pid ! {add, {{M, F, A}, HashExprPairs}}.

update_hash(Pid, {{FileName, FunName, Arity}, StartLine})->
    Pid ! {quick_hash,{{FileName, FunName, Arity}, StartLine}}.

remove_entry(Pid, {M, F, A}) ->
    Pid !{remove_entry, {M, F, A}}.

get_index(ExpHashTab, Key) ->
    case ets:lookup(ExpHashTab, Key) of 
	[{Key, I}]->
	    I;
	[] ->
	    NewIndex = ets:info(ExpHashTab, size)+1,
	    ets:insert(ExpHashTab, {Key, NewIndex}),
	    NewIndex
    end.

hash_loop({NextSeqNo, ExpHashTab, NewData},Inc) ->
    receive
	%% add a new entry.
	{add, {{M, F, A}, KeyExprPairs}} ->
	    KeyExprPairs1 =
		[{{Index1, NumOfToks, StartEndLoc, StartLine, true}, HashIndex}
		 || {Key, {Index1, NumOfToks, StartEndLoc, StartLine}} <- KeyExprPairs,
		    HashIndex <- [get_index(ExpHashTab, Key)]],
	    hash_loop({NextSeqNo+1, ExpHashTab, [{NextSeqNo, {M,F,A}, KeyExprPairs1}| NewData]},Inc);
	{quick_hash,{{FileName, FunName, Arity}, StartLine}} ->
	    NewData1 = [case {M, F, A} of
			    {FileName, FunName, Arity} ->
				{Seq, {M, F, A}, [{{Index1, NumOfToks, StartEndLoc, StartLine, false}, HashKey}
						  || {{Index1, NumOfToks, StartEndLoc, _StartLine, _}, HashKey} <- KeyExprPairs]};
			    _ ->
				{Seq, {M, F, A}, KeyExprPairs}
			end || {Seq, {M, F, A}, KeyExprPairs} <- NewData],
	    hash_loop({NextSeqNo, ExpHashTab, NewData1},Inc);
	{get_clone_candidates, From, Thresholds, Dir} ->
	    {ok, OutFileName} = search_for_clones(Dir, lists:reverse(NewData), Thresholds),
	    From ! {self(), {ok, OutFileName}},
	    case Inc of
		true ->
		    write_file(?ExpSeqFile, term_to_binary(NewData));
		false ->
		    ok
	    end,
	    hash_loop({NextSeqNo, ExpHashTab, lists:reverse(NewData)},Inc);%%!!!! Data Reorded!!!
	{get_clone_in_range, From, {Ranges, Len, Freq}} ->
	    F0 = fun ({ExprSeqId, ExprIndex}, L) ->
			 {ExprSeqId, {M, F, A}, Exprs} = lists:nth(ExprSeqId, NewData),
			 Es = lists:sublist(Exprs, ExprIndex, L),
			 [{{M,F,A,Index}, Toks, {{StartLoc, EndLoc}, StartLine}, IsNew}
			  || {{Index, Toks, {StartLoc, EndLoc}, StartLine, IsNew}, _HashIndex} <- Es]
		 end,
	    C1 = {[F0(R, Len) || R <- Ranges], {Len, Freq}},
	    From ! {self(), C1},
	    hash_loop({NextSeqNo, ExpHashTab, NewData},Inc);
	{remove_entry, {M, F, A}} ->
	    NewData1 = [case {M,F,A}/={M1, F1, A1} of
			    true ->
				{Seq, {M1, F1, A1}, KeyExprPairs};
			    false ->
				{Seq, {M1, F1, A1}, []}
			end
			|| {Seq, {M1, F1, A1}, KeyExprPairs} <- NewData],
	    hash_loop({NextSeqNo, ExpHashTab, NewData1},Inc);
	stop ->
	    %%file:write_file(?ExpSeqFile, term_to_binary(NewData)),
	    ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                    %%
%%  Hash the AST representation of expressions using MD5, and map     %%
%%  sequence of expression into sequences of indexes.                 %%
%%                                                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_clone_check_process(Tabs) ->
    spawn_link(fun()->clone_check_loop([],[], Tabs) end).

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

clone_check_loop(Cs, CandidateClassPairs, Tabs) ->
    receive
	{add_clone,  {Candidate, Clones}} ->
	    Clones1=[get_clone_class_in_absolute_locs(Clone) 
		     || Clone <- Clones],
	    clone_check_loop(Clones1++Cs, [{hash_a_clone_candidate(Candidate), Clones}|CandidateClassPairs], Tabs);
	{get_clones, From, _ASTTab} ->
	    ?debug("TIME3:\n~p\n", [time()]),
	    Cs0=remove_sub_clones(Cs),
	    Cs1=[{AbsRanges, Len, Freq, AntiUnifier}||
		    {_, {Len, Freq}, AntiUnifier,AbsRanges}<-Cs0],
	    From ! {self(), Cs1},
	    clone_check_loop(Cs, CandidateClassPairs, Tabs);       
	stop ->
	    ets:insert(Tabs#tabs.clone_tab, CandidateClassPairs),	
	    ok;
	_Msg -> 
	    ?wrangler_io("Unexpected message:\n~p\n",[_Msg]),
	    clone_check_loop(Cs,  CandidateClassPairs, Tabs)
    end.
 
%%=============================================================================
%% check each candidate clone, and drive real clone classes.

examine_clone_candidates([],_Thresholds,Tabs,CloneCheckerPid,_ASTPid,_Num) ->
    get_final_clone_classes(CloneCheckerPid,Tabs#tabs.ast_tab);
examine_clone_candidates([C| Cs],Thresholds,Tabs,CloneCheckerPid,ASTPid,Num) ->
    output_progress_msg(Num), 
    C1 = get_clone_in_range(ASTPid,C),
    MinToks = Thresholds#threshold.min_toks, 
    MinFreq = Thresholds#threshold.min_freq, 
    case remove_short_clones(C1,MinToks,MinFreq) of
      [] ->
	  ok;
      [C2] ->
            case examine_a_clone_candidate(C2,Thresholds,Tabs) of
	    [] ->
		ok;
	    ClonesWithAU ->
                  add_new_clones(CloneCheckerPid,{C2, ClonesWithAU})
	  end
    end, 
    examine_clone_candidates(Cs,Thresholds,Tabs,CloneCheckerPid,ASTPid,Num+1).

output_progress_msg(Num) ->
    case Num rem 10 of
     	1 -> 
     	    ?wrangler_io("\nChecking clone candidate no. ~p ... ", [Num]);
     	_-> ok
     end.
	

%% check a clone clandidate.
examine_a_clone_candidate(C={Ranges, {_Len, _Freq}}, Thresholds,Tabs) ->
    case has_new_exprs(C) of
	false ->
	    %% the clone candidate does not contain any new expressions, 
	    %% so try to use the cached info.
	    Hash = hash_a_clone_candidate(C),
	    case ets:lookup(Tabs#tabs.clone_tab, Hash) of
		[{Hash, Clones}] ->
		    %% update the StartLine offset in Clones.
		    update_clone_class_locations(Ranges, Clones);
		_->
		    %% no cached result found.
		    examine_a_clone_candidate_1(C,Thresholds,Tabs)
	    end;
	true ->
	    %% the clone candidate contains new expressions,
	    %% so re-calculation is needed.
	    examine_a_clone_candidate_1(C,Thresholds,Tabs)
    end.

has_new_exprs(_C={Ranges, _})->
    lists:member(true, [element(4, R)||R<-lists:append(Ranges)]).

hash_a_clone_candidate(_C={Ranges, {_Len, _Freq}}) ->
    F = fun({MFAI, Toks, {Loc, _StartLine}, _IsNew}) ->
		{MFAI, Toks, Loc}
	end,
    erlang:md5(lists:usort(
		 [erlang:md5(lists:flatten(
			       io_lib:format(
				 "~p", [[F(E)||E<-R]])))
		  ||R<-Ranges])).

%% update the StartLine Offset in Clones.
update_clone_class_locations(Ranges, Clones) ->
    [{update_clone_class_locations_1(lists:append(Ranges), Ranges1),
      {Len, Freq}, AU}||{Ranges1, {Len, Freq}, AU}<-Clones].

update_clone_class_locations_1(RangesWithNewStartLine, RangesWithOldStartLine) ->
    [{[begin
	  {value, {MFAI, Toks, {StartEndLoc, NewStartLine}, _}} =
	      lists:keysearch(MFAI, 1, RangesWithNewStartLine),
	  {MFAI, Toks, {StartEndLoc, NewStartLine}, IsNew}
      end
      || {MFAI, Toks, {StartEndLoc, _StartLine}, IsNew} <- Rs], FunCall}
     || {Rs, FunCall} <- RangesWithOldStartLine].


%% examine a new clone candidate.
examine_a_clone_candidate_1(_C={Ranges, {_Len, _Freq}}, Thresholds, Tabs) ->
    ASTTab = Tabs#tabs.ast_tab,
    RangesWithExprAST=[attach_expr_ast_to_ranges(R, ASTTab)|| R<-Ranges],
    Clones = examine_clone_class_members(RangesWithExprAST, Thresholds, Tabs,[]),
    ClonesWithAU = [
		    begin
			FromSameFile=from_same_file(Rs),
                        AU= get_anti_unifier(Info, FromSameFile),
                        {Rs1, AU1} = attach_fun_call_to_range(Rs, AU, FromSameFile),
			{Rs1, {Len, Freq}, AU1}
		    end
		    || {Rs, {Len, Freq}, Info} <- Clones],
    ClonesWithAU.
  

attach_expr_ast_to_ranges(Rs, ASTTab) ->
    [{R, ExpAST}||R={ExprKey, _Toks, _Loc, _IsNew}<-Rs, 
		  {_Key, ExpAST}<-ets:lookup(ASTTab, ExprKey)].


%% check the clone members of a clone candidate using 
%% anti-unification techniques.   
examine_clone_class_members(RangesWithExprAST, Thresholds, _, Acc) 
  when length(RangesWithExprAST)< Thresholds#threshold.min_freq ->
    %% The number of clone memebers left is less 
    %% than the min_freq threshold, so the examination
    %% finishes, and sub-clones are removed.
    remove_sub_clones(Acc);

examine_clone_class_members(RangesWithExprAST, Thresholds,Tabs, Acc) ->
    %% Take the first clone member and  try to anti_unify other 
    %% clone members with this member. If there is a real clone 
    %% class found, then the anti-unifier of the class is derrived 
    %% by generalisation of the first clone member.

    [RangeWithExprAST1|Rs]=RangesWithExprAST,

    %% try to anti_unify each of the remaining candidate clone members 
    %% with the first candidate clone member.

    Res = [do_anti_unification(RangeWithExprAST1, RangeWithExprAST2)
	   || RangeWithExprAST2<-Rs],


    %% process the anti_unification result.
    Clones = process_au_result(Res, Thresholds, Tabs),

    %% get the maximal length of the clone clone members returned.
    MaxCloneLength= case Clones ==[] of 
			true -> 
			    0;
			_-> 
			    %% make sure the clones returned are ordered!!!
			    element(1, element(2, hd(Clones)))
		    end,

    InitialLength = length(RangeWithExprAST1),

    case MaxCloneLength /= InitialLength of
	true ->
	    %% the original expression sequences have been chopped into shorter ones.
	    examine_clone_class_members(Rs, Thresholds,Tabs, Clones ++ Acc);
	false ->
	    %% the original expression still a class member of the clones returned.
	    Rs1 = element(1, hd(Clones)),
	    RemainedRanges = RangesWithExprAST -- Rs1,
	    examine_clone_class_members(RemainedRanges, Thresholds,Tabs, Clones ++ Acc)

    end.

%% try to anti-unify two expression sequences.
do_anti_unification(RangeWithExpr1, RangeWithExpr2) ->
    ZippedExprs=lists:zip(RangeWithExpr1, RangeWithExpr2),
    [begin
	 {{Index1,E1}, {Index2, E2},
	  do_anti_unification_1(E1,E2)}
     end|| {{Index1,E1}, {Index2, E2}}<-ZippedExprs].
    
%% try to anti_unift two expressions.
do_anti_unification_1(E1, E2) ->
    SubSt=anti_unification:anti_unification(E1,E2),
    case SubSt of 
	none -> none;
	_ -> case subst_sanity_check(E1, SubSt) of
		 true ->
		     SubSt;
		 false ->
		     none
	     end
    end.

subst_sanity_check(Expr1, SubSt) ->
    BVs = refac_util:get_bound_vars(Expr1),
    F = fun ({E1, E2}) ->
		case refac_syntax:type(E1) of
		    variable ->
			E1Ann = refac_syntax:get_ann(E1),
			case lists:keysearch(category, 1, E1Ann) of
			    {value, {category, {macro_name, _, _}}} ->
				false;
			    _ -> has_same_subst(E1, E2, SubSt)
			end;
		    _ ->
			%% the expression to be replaced should not contain local variables.
			BVs -- refac_util:get_free_vars(E1) == BVs
		end
	end,
    lists:all(F, SubSt).

has_same_subst(E1, E2, SubSt) ->
    E1Ann = refac_syntax:get_ann(E1),
    {value, {def, DefPos}} = lists:keysearch(def, 1, E1Ann),
    %% local vars should have the same substitute.
     not  lists:any(
	    fun ({E11, E21}) ->
		    refac_syntax:type(E11) == variable andalso 
		      {value, {def, DefPos}} == lists:keysearch(
						  def, 1, refac_syntax:get_ann(E11))
			 andalso 
		      format(E2)
			=/= format(E21)
	  end, SubSt).

%% process anti-unification result.
process_au_result(AURes, Thresholds, Tabs) ->
    Res = [process_one_au_result(OneAURes, Thresholds, Tabs)
	   || OneAURes <- AURes],
    ClonePairs = lists:append(Res),
    get_clone_classes(ClonePairs, Thresholds, Tabs).

%% process one anti_unification pair. In case the whose 
%% pair of expression sequences do not anti-unify, get those 
%% pairs of sub sequences that do anti-unify.
process_one_au_result(OneAURes, Thresholds, _Tabs) ->
    SubAULists=group_au_result(OneAURes, Thresholds),
    ClonePairs =lists:append([get_clone_pairs(SubAUList, Thresholds)
			      ||SubAUList<-SubAULists]),
    ClonePairs1 =[lists:unzip3(CP)||CP<-ClonePairs],
    remove_sub_clone_pairs(ClonePairs1).

%% examine the result of anti-unifying a pair of expression sequences and 
%% get the sub expression sequences pairs that are anti-unifiable.
group_au_result([], _Thresholds)->
    [];
group_au_result(AURes, Thresholds) ->
    %% here 'none' means the two expressions E1 an E2 do not anti-unify.
    {AUResList1,AUResList2} =
	lists:splitwith(fun({_E1,_E2, S}) ->S/=none end, AURes),
    AUResList3 = case AUResList2 of
		     [] -> [];
		     [_|T] -> T
		 end,
    case clone_pair_above_min_size(AUResList1, Thresholds) of
	true ->
	    [AUResList1]++group_au_result(AUResList3, Thresholds);
	false ->
	    group_au_result(AUResList3, Thresholds)
    end.
  
get_clone_pairs(AURes, Thresholds) ->
    get_clone_pairs(AURes, Thresholds, {[],[]},[]).

get_clone_pairs([],Thresholds,{_VarSubAcc,ClonePairAcc},Acc) ->
    case clone_pair_above_min_size(ClonePairAcc,Thresholds) of
      true ->
	    ClonePairs = decompose_clone_pair(lists:reverse(ClonePairAcc),Thresholds), 
	    ClonePairs++Acc;
	false ->
	    Acc
    end;
get_clone_pairs([CurPair = {_E1,_E2,SubSt}| AURes],Thresholds,
		{VarSubAcc,ClonePairAcc},Acc) ->
    %% check the subsitution of variables. 
    %% variables with the same defining location should 
    %% has the same substitution.
    CurVarSubsts = get_var_subst(SubSt), 
    case var_sub_conflicts(CurVarSubsts,VarSubAcc) of
      true ->
	  %% conflicting variable substitution.
	  case clone_pair_above_min_size(ClonePairAcc,Thresholds) of
	    true ->
		NewClonePairs = decompose_clone_pair(lists:reverse(ClonePairAcc),Thresholds), 
		NewAcc = NewClonePairs++Acc, 
		get_clone_pairs(AURes,Thresholds,{[],[]},NewAcc);
	    false ->
		%% the clone pairs is too short.
		get_clone_pairs(AURes,Thresholds,{[],[]},Acc)
	  end;
      false ->
	  get_clone_pairs(AURes,Thresholds,
			  {CurVarSubsts++VarSubAcc,[CurPair]++ClonePairAcc},Acc)
    end.

get_var_subst(SubSt) ->
    F = fun ({E1, E2}) ->
		{value, {def, DefPos}} =
		    lists:keysearch(def, 1, refac_syntax:get_ann(E1)),
		{DefPos, format(E2)}
	end,
    [F({E1,E2})
     || {E1,E2} <- SubSt,
	refac_syntax:type(E1)==variable,
	 not  is_macro_name(E1)].

is_macro_name(Exp) ->
    case lists:keysearch(category, 1, refac_syntax:get_ann(Exp)) of
	 {value, {category, {macro_name,_,_}}} ->
	    true;
	_ -> false
    end.

var_sub_conflicts(SubSts, ExistingVarSubsts) ->
    lists:any(fun ({DefPos, E}) ->
		      case lists:keysearch(DefPos, 1, ExistingVarSubsts) of
			{value, {DefPos, E1}} ->
			    E /= E1;
			false ->
			    false
		      end
	      end, SubSts).

%% decompose a clone pairs so that each new clone pairs' simi score 
%% is above the threshold specified.
decompose_clone_pair(ClonePair,Thresholds) ->
    ListOfClonePairs=decompose_clone_pair_by_new_vars(ClonePair, Thresholds),
    Res=[decompose_clone_pair_by_simi_score(CP, Thresholds)||CP<-ListOfClonePairs],
    lists:append(Res).

decompose_clone_pair_by_simi_score(ClonePair, Thresholds) ->
    case clone_pair_above_min_simi_score(ClonePair, Thresholds) of 
	true ->
	    [ClonePair];
	false->
	    decompose_clone_pair_by_simi_score_1(ClonePair, Thresholds)
    end.
    
decompose_clone_pair_by_simi_score_1(ClonePair,Thresholds) ->
    ClonePairWithSimiScore = 
	[{R1, R2, Subst, {simi_score([R1], SubEs1), simi_score([R2], SubEs2)}}
	  ||{R1, R2, Subst}<-ClonePair, {SubEs1, SubEs2}<-[lists:unzip(Subst)]],
	 decompose_clone_pair_by_simi_score_2(ClonePairWithSimiScore, Thresholds).

decompose_clone_pair_by_simi_score_2(ClonePairWithSimiScore, Thresholds) ->
    Scores = [(Score1+Score2)/2||Pair<-ClonePairWithSimiScore,
				 {Score1,Score2}<-[element(4, Pair)]],
    MinScore = lists:min(Scores),
    %%spliting the clone pairs at the pair of expressions with the lowest 
    %% similarity score.
    {ClonePair1, [_P|ClonePair2]} = lists:splitwith(
				      fun({_, _, _, {Score1, Score2}}) ->
					      (Score1+Score2)/2 /= MinScore
				      end, ClonePairWithSimiScore),
    decompose_clone_pair_by_simi_score_3(ClonePair1, Thresholds)
	++ decompose_clone_pair_by_simi_score_3(ClonePair2, Thresholds).


decompose_clone_pair_by_simi_score_3(ClonePair, Thresholds)->
    case not clone_pair_above_min_size(ClonePair, Thresholds) of 
	true ->
	    [];
	false ->
	    CP =[{R1, R2, Subst}||{R1,R2, Subst, _}<-ClonePair],
	    case clone_pair_above_min_simi_score(CP, Thresholds) of 
		true ->
		    [CP];
		false ->
		    decompose_clone_pair_by_simi_score_2(ClonePair, Thresholds)
	    end
    end.

decompose_clone_pair_by_new_vars(ClonePair, Thresholds)->
    MinLen = Thresholds#threshold.min_len,
    MaxNewVars = Thresholds#threshold.max_new_vars,
    {{CurLen, _}, CurClonePair, ClonePairs}=
	lists:foldl(fun({R1,R2, Subst}, {{Len, SubstAcc}, Acc1,  Acc2})->
			    case Subst of 
				[] ->
				    {{Len+1, SubstAcc}, [{R1,R2,Subst}|Acc1], Acc2};
				_ -> 
				    NewVars=num_of_new_vars(Subst++SubstAcc),
				    case NewVars> MaxNewVars of 
					true ->
					    {NewAcc1, NewSubst} = get_sub_clone_pair(lists:reverse([{R1,R2,Subst}|Acc1]), MaxNewVars),
					    NewLen = length(NewAcc1),
					    case Len>=MinLen of 
						true ->
						    {{NewLen, NewSubst}, lists:reverse(NewAcc1), [lists:reverse(Acc1)|Acc2]};
						false ->
						    {{NewLen, NewSubst}, lists:reverse(NewAcc1), Acc2}
					    end;
					false ->
					    {{Len+1, Subst++SubstAcc}, [{R1,R2,Subst}|Acc1], Acc2}
				    end
			    end
		    end, 
		    {{0, []}, [], []}, ClonePair),
    case CurLen>=MinLen of 
	true ->
	    lists:reverse([lists:reverse(CurClonePair)|ClonePairs]);
	false ->
	    lists:reverse(ClonePairs)
    end.
    

get_sub_clone_pair([{_R1,_R2, Subst}|CPs], NumOfNewVars) ->
    case Subst of
	[] ->
	    get_sub_clone_pair(CPs, NumOfNewVars);
	_ ->
	    {_,_, ListOfSubSt} = lists:unzip3(CPs),
	    NewSubst = lists:append(ListOfSubSt),
	    case num_of_new_vars(NewSubst) =< NumOfNewVars of
		true ->
		    {CPs, NewSubst};
		false ->
		    get_sub_clone_pair(CPs, NumOfNewVars)
	    end
    end.

clone_pair_above_min_simi_score(ClonePair, Thresholds)->
    SimiScoreThreshold = Thresholds#threshold.simi_score,
    {Range1, Range2, Subst} = lists:unzip3(ClonePair),
    {SubExprs1, SubExprs2} = lists:unzip(lists:append(Subst)),
    Score1 = simi_score(Range1, SubExprs1),
    Score2 = simi_score(Range2, SubExprs2),
    Score1 >= SimiScoreThreshold  andalso
	Score2>= SimiScoreThreshold.

clone_pair_above_min_size(CP, Thresholds) ->
    length(CP)>=Thresholds#threshold.min_len andalso
	lists:sum([element(2, E1)||{{E1,_}, _E2, _S}<-CP])
	>=Thresholds#threshold.min_toks.

simi_score(ExprRanges, SubExprs) ->
    ExprToks = lists:sum([element(2, (element(1,R)))||R<-ExprRanges]),
    case ExprToks of 
	0 ->
	    0;
	_ ->
	    1-((num_of_tokens(SubExprs)-length(SubExprs))/ExprToks)
    end.
    
num_of_tokens(Exprs) ->
   lists:sum([num_of_tokens_in_string(format(E))
	      ||E<-Exprs]).


num_of_tokens_in_string(Str) ->
    case refac_scan:string(Str, {1,1}, 8, 'unix') of
	{ok, Ts, _} -> 
            Ts1 = [T||T<-Ts, not is_whitespace_or_comment(T)],
	    length(Ts1);
	_ ->
	    0
    end.

remove_sub_clone_pairs([]) ->[];
remove_sub_clone_pairs(CPs) ->
    SortedCPs = lists:sort(fun({Rs1,_,_}, {Rs2, _, _}) ->
					  length(Rs1)>length(Rs2)
				  end, CPs),
    remove_sub_clone_pairs(SortedCPs, []).
remove_sub_clone_pairs([], Acc) ->
    lists:reverse(Acc);
remove_sub_clone_pairs([CP={Rs, _,_}|CPs], Acc) ->
    case lists:any(fun({Rs1, _,_}) ->
			   Rs--Rs1==[] 
		   end, Acc) of
	true ->
	    remove_sub_clone_pairs(CPs,Acc);
	_ -> remove_sub_clone_pairs(CPs, [CP|Acc])
    end.
	
%% derive clone classes from clone pairs.	
get_clone_classes(ClonePairs,Thresholds, Tabs) ->
    RangeGroups = lists:usort([Rs1 || {Rs1, _Rs2, _Subst} <- ClonePairs]),
    CloneClasses = lists:append([get_one_clone_class(Range, ClonePairs, Thresholds, Tabs) 
				 || Range <- RangeGroups]),
    lists:keysort(2, CloneClasses).
 
get_one_clone_class(RangeWithExprAST, ClonePairs, Thresholds, Tabs) ->
    Res = lists:append([get_one_clone_class_1(RangeWithExprAST, ClonePair, Tabs)
			|| ClonePair <- ClonePairs]),
    CloneClasses =group_clone_pairs(Res, Thresholds),
    [begin
	 {Range, Exprs} = lists:unzip(RangeWithExprAST),
	 [{{FName, FunName, Arity, _}, _, _,_}| _] = Range,
	 VarTab = Tabs#tabs.var_tab,
	 VarsToExport = get_vars_to_export(Exprs, {FName, FunName, Arity}, VarTab),
	 {Ranges, ExportVars, SubSt} = lists:unzip3(C),
	 %% VarstoExport format : [{name, pos}].
	 ExportVars1 = {element(1, lists:unzip(VarsToExport)), 
			lists:usort(lists:append(ExportVars))},
	 {[RangeWithExprAST| Ranges], {length(Range), length(Ranges) + 1}, 
	  {Exprs, SubSt, ExportVars1}}
     end
     || C<-CloneClasses].


    
get_one_clone_class_1(RangeWithExprAST, _ClonePair = {Range1, Range2, Subst}, Tabs) ->
    case RangeWithExprAST -- Range1 == [] of
      true ->
	    %% Range is a sub list of Range1.
	    Len = length(RangeWithExprAST),
	    R = hd(RangeWithExprAST),
	    StartIndex=length(lists:takewhile(fun (R0) -> R0 /= R end, Range1))+1,
	    SubRange2 = lists:sublist(Range2, StartIndex, Len),
	    SubSubst = lists:append(lists:sublist(Subst, StartIndex, Len)),
	    {_, Exprs2} = lists:unzip(SubRange2),
	    [{{{FName, FunName, Arity, _}, _, _, _},_}| _] = SubRange2,
	    VarTab = Tabs#tabs.var_tab,
	    VarsToExport2 = get_vars_to_export(Exprs2, {FName, FunName, Arity}, VarTab),
	    %% Exprs from the first member of the clone pair which are going to 
            %% be replaced by new variables, and the new variables will be exported.
	    EVs = [E1 || {E1, E2} <- SubSubst, refac_syntax:type(E2) == variable,
			 lists:member({refac_syntax:variable_name(E2), get_var_define_pos(E2)},
				      VarsToExport2)],
	    %% EVs are variables from Exprs1 that need to be exported.
	    NumOfNewVars = num_of_new_vars(SubSubst),
	    [{{SubRange2, EVs, SubSubst},NumOfNewVars}];
      	false ->
	    []
    end.

group_clone_pairs(ClonePairs, Thresholds) ->
    ClonePairs1=lists:keysort(2,ClonePairs),
    group_clone_pairs(ClonePairs1, Thresholds, []).

group_clone_pairs([], _, Acc) ->
    lists:reverse(Acc);
group_clone_pairs(ClonePairs, Thresholds, Acc) ->
    MinFreq= Thresholds#threshold.min_freq -1,
    {NewCs, LeftPairs}=group_clone_pairs(ClonePairs,Thresholds, sets:new(),[],[]),
    NewAcc = case length(NewCs)>=MinFreq of
		 true->[NewCs|Acc];
		 false ->
		     Acc
	     end,
    case length(LeftPairs)<MinFreq of 
	true ->
	    NewAcc;
	false ->
	    group_clone_pairs(LeftPairs, Thresholds, NewAcc)
    end.

group_clone_pairs([], _, _, Acc, LeftPairs) ->
    {lists:reverse(Acc), lists:reverse(LeftPairs)};
group_clone_pairs([CP={C={_R, _EVs, Subst}, NumOfNewVars}|T], Thresholds, ExprsToBeGenAcc, Acc, LeftPairs) ->
    MaxNewVars = Thresholds#threshold.max_new_vars,
    ExprsToBeGen=exprs_to_be_generalised(Subst),
    NewExprsToBeGenAcc =sets:union(ExprsToBeGen, ExprsToBeGenAcc),
    case sets:size(NewExprsToBeGenAcc)=<MaxNewVars of
    	true ->
	    group_clone_pairs(T, Thresholds, NewExprsToBeGenAcc, [C|Acc], LeftPairs);
	false ->
	    case NumOfNewVars>MaxNewVars of 
		true ->
		    group_clone_pairs([], Thresholds, ExprsToBeGenAcc, Acc, LeftPairs);
		false ->
		    group_clone_pairs(T, Thresholds, ExprsToBeGenAcc, Acc, [CP|LeftPairs])
	    end
    end.

%% This is not accurate, and will be improved!
exprs_to_be_generalised(SubSt) ->
    sets:from_list([format(E1)
		    || {E1,_E2} <- SubSt, refac_syntax:type(E1)/=variable]).

num_of_new_vars(SubSt) ->
    length(lists:usort([{format(E1), format(E2)}
			|| {E1,E2} <- SubSt, refac_syntax:type(E1)/=variable])).


format(Node) ->
    refac_prettypr:format(refac_util:reset_ann_and_pos(Node)).
    
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                  %%
%%  Attach function call to each class member                       %%
%%                                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
attach_fun_call_to_range(RangesWithAST,{AU, Pars}, FromSameFile) ->
    RangesWithFunCalls=[generate_fun_call_1(RangeWithAST, AU, FromSameFile) 
			|| RangeWithAST <- RangesWithAST],
    {RangesWithFunCalls,{simplify_anti_unifier(AU),Pars}}.

generate_fun_call_1(RangeWithAST, AUForm, FromSameFile) ->
    {Range, Exprs} = lists:unzip(RangeWithAST),
    AUFunClause=hd(refac_syntax:function_clauses(AUForm)),
    Pats = refac_syntax:clause_patterns(AUFunClause),
    AUBody = refac_syntax:clause_body(AUFunClause),
    try 
	%% it would be a bug if this does not match. 
	{true, Subst} = 
	    case length(AUBody) - length(Exprs) of 
		1 ->
		    SubAUBody = lists:reverse(tl(lists:reverse(AUBody))),
		    unification:expr_unification_extended(SubAUBody, Exprs);
		0 ->
		    unification:expr_unification_extended(AUBody, Exprs)
	    end,
	%% Need to check side-effect here. but it is a bit slow!!!
	FunCall=make_fun_call(new_fun, Pats, Subst, FromSameFile),
	{Range, format(FunCall)}
    catch 
	_E1:_E2 ->
	    "wrangler-failed-to-generate-the-function-application."
    end.

make_fun_call(FunName, Pats, Subst, FromSameFile) ->
    Fun = fun (P) ->
		  case refac_syntax:type(P) of
		      variable ->
			  PName = refac_syntax:variable_name(P),
			  case lists:keysearch(PName, 1, Subst) of
			      {value, {PName, Par}} ->
				  case refac_syntax:type(Par) of
				      atom ->
					  case FromSameFile of
					      true -> Par;
					      false ->
						  As = refac_syntax:get_ann(Par),
						  case lists:keysearch(fun_def, 1, As) of
						      {value, {fun_def, {M, _F, A, _, _}}} ->
							  case M== erlang orelse M=='_' of
							      true ->
								  Par;
							      false ->
								  Mod = refac_syntax:atom(M),
								  ModQualifier = refac_syntax:module_qualifier(Mod, Par),
								  refac_syntax:implicit_fun(ModQualifier, refac_syntax:integer(A))
							  end;
						      _ -> Par
						  end
					  end;
				      module_qualifier ->
					  As = refac_syntax:get_ann(Par),
					  case lists:keysearch(fun_def, 1, As) of
					      {value, {fun_def, {_M, _F, A, _, _}}} ->
						  refac_syntax:implicit_fun(Par, refac_syntax:integer(A));
					      _ -> Par   %% This should not happen!
					  end;
				      application ->
					  refac_syntax:fun_expr([refac_syntax:clause([], none, [Par])]);
				      _ -> Par
				  end;
			      _ ->
				  refac_syntax:atom(undefined)
			  end;
		      underscore ->
			  refac_syntax:atom(undefined);
		      _ -> P
		  end
	  end,
    Pars = lists:map(Fun, Pats),
    Op = refac_syntax:atom(FunName),
    refac_util:reset_attrs(refac_syntax:application(Op, [P || P <- Pars])).

	 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                  %%
%%  Remove sub-clones                                               %%
%%                                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_sub_clones(Cs) ->
    remove_sub_clones(lists:reverse(lists:keysort(2,Cs)),[]).
remove_sub_clones([], Acc_Cs) ->
    lists:reverse(Acc_Cs);
remove_sub_clones([C|Cs], Acc_Cs) ->
    case is_sub_clone(C, Acc_Cs) of
	true -> 
	    remove_sub_clones(Cs, Acc_Cs);
	false ->remove_sub_clones(Cs, [C|Acc_Cs])
    end.

is_sub_clone({Ranges, {Len, Freq},Str,AbsRanges}, ExistingClones) ->
    case ExistingClones of 
	[] -> false;
	[{Ranges1, {_Len1, _Freq1}, _, _AbsRanges1}|T] ->
	    case is_sub_ranges(Ranges, Ranges1) of 
		true -> 
		    true;
		false -> is_sub_clone({Ranges, {Len, Freq},Str, AbsRanges}, T)
	    end
	end;

is_sub_clone({Ranges, {Len, Freq},Str}, ExistingClones) ->
    case ExistingClones of 
	[] -> false;
	[{Ranges1, {_Len1, _Freq1}, _}|T] ->
	    case is_sub_ranges(Ranges, Ranges1) of 
		true -> 
		    true;
		false -> is_sub_clone({Ranges, {Len, Freq},Str}, T)
	    end
    end;
is_sub_clone({Ranges, {Len, Freq}}, ExistingClones) ->
    case ExistingClones of 
	[] -> false;
	[{Ranges1, {_Len1, _Freq1}}|T] ->
	    case is_sub_ranges(Ranges, Ranges1) of 
		true -> 
		    true;
		false -> is_sub_clone({Ranges, {Len, Freq}}, T)
	    end
    end.

is_sub_ranges(Ranges1, Ranges2) ->
    lists:all(fun (R1)  -> 
		      lists:any(fun (R2) ->
					R1--R2==[]
				end, Ranges2) 
	      end, Ranges1).


get_var_define_pos(V) ->
    {value, {def, DefinePos}} = lists:keysearch(def,1, refac_syntax:get_ann(V)),
    DefinePos.

get_anti_unifier({Exprs, SubSt, ExportVars}, FromSameFile) ->
    {AU, {NumOfPars, NumOfNewVars}} =anti_unification:generate_anti_unifier_and_num_of_new_vars(
                                       Exprs, SubSt, ExportVars),
    case FromSameFile of
	true -> 
	    {AU,{NumOfPars, NumOfNewVars}};
	false ->
	    {post_process_anti_unifier(AU),{NumOfPars, NumOfNewVars}}
    end.
    
from_same_file(RangesWithAST) ->   
    Files = [element(1,element(1,(element(1,hd(Rs)))))||Rs<-RangesWithAST],
    length(lists:usort(Files)) ==1.

post_process_anti_unifier(FunAST) ->
    {FunAST1, _} = ast_traverse_api:stop_tdTP(fun do_post_process_anti_unifier/2, FunAST, none),
    FunAST1.

do_post_process_anti_unifier(Node, _Others) ->
    case refac_syntax:type(Node) of
	application ->
	    Operator = refac_syntax:application_operator(Node),
	    Arguments = refac_syntax:application_arguments(Node),
	    case refac_syntax:type(Operator) of
		atom ->
		    As = refac_syntax:get_ann(Operator),
		    {value, {fun_def, {M, _F, _A, _, _}}} = lists:keysearch(fun_def,1,As),
		    case M== erlang orelse M=='_' of
			true ->
			    {Node, false};
			false ->
			    Mod = refac_syntax:atom(M),
			    Operator1 = refac_util:rewrite(Operator, refac_syntax:module_qualifier(Mod, Operator)),
			    Node1 = refac_util:rewrite(Node, refac_syntax:application(Operator1, Arguments)),
			    {Node1, false}
		    end;
		_ ->
		    {Node, false}
	    end;
	_ -> {Node, false}
    end.


get_clone_member_start_end_loc(Range)->
    {{File, _, _, _}, _Toks, {{{Line1, Col1},_},StartLine},_} = hd(Range),
    {_ExprKey1, _Toks1,{{_, {Line2, Col2}}, StartLine},_}= lists:last(Range),
    {{File, Line1+StartLine-1, Col1}, {File, Line2+StartLine-1, Col2}}.
  
get_clone_class_in_absolute_locs({Ranges, {Len, Freq}, AntiUnifier}) ->
    StartEndLocsWithFunCall = [{get_clone_member_start_end_loc(R),FunCall}|| {R, FunCall} <- Ranges],
    RangesWithoutFunCalls=[R||{R,_}<-Ranges],
    {RangesWithoutFunCalls, {Len, Freq}, AntiUnifier,StartEndLocsWithFunCall}.

get_vars_to_export(Es, {FName, FunName, Arity}, VarTab) ->
    AllVars = ets:lookup(VarTab, {FName, FunName, Arity}),
    {_, EndLoc} = refac_util:get_start_end_loc(lists:last(Es)),
    case AllVars of
	[] -> [];
	[{_, _, Vars}] ->
	    ExprBdVarsPos = [Pos || {_Var, Pos} <- refac_util:get_bound_vars(Es)],
	    [{V, DefPos} || {V, SourcePos, DefPos} <- Vars,
			    SourcePos > EndLoc,
			    lists:subtract(DefPos, ExprBdVarsPos) == []]
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                            %%
%%        Search for cloned candidates                        %%
%%                                                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

integer_list_to_string(Is) ->
    integer_list_to_string(Is, "").
integer_list_to_string([], Acc) ->
    lists:reverse("\n\r$,"++Acc);
integer_list_to_string([I], Acc) ->
    S= case is_integer(I) of
	   true ->
	       integer_to_list(I);
	   false ->
	       atom_to_list(I)
       end,
    integer_list_to_string([], lists:reverse(S)++Acc);
integer_list_to_string([I|Is], Acc) ->
     S= case is_integer(I) of
	   true ->
		","++lists:reverse(integer_to_list(I));
	    false ->
		lists:reverse(atom_to_list(I))
	end,
    integer_list_to_string(Is, S++Acc).

    
search_for_clones(Dir, [], _Thresholds) ->
    OutFileName = filename:join(Dir, "wrangler_suffix_tree"),
    write_file(OutFileName, []),
    {ok, OutFileName};
search_for_clones(Dir, Data, Thresholds) ->
    MinLen = Thresholds#threshold.min_len,
    MinFreq= Thresholds#threshold.min_freq,
    NumOfIndexStrs=integer_to_list(length(Data))++"\r\n",
    IndexStr = NumOfIndexStrs++lists:append([integer_list_to_string(Is)
					     ||{_SeqNo, _FFA, ExpHashIndexPairs} <- Data,
						{_, Is}<-[lists:unzip(ExpHashIndexPairs)]]),
    SuffixTreeExec = filename:join(?WRANGLER_DIR, "bin/gsuffixtree"),
    suffix_tree:get_clones_by_suffix_tree_inc(Dir, IndexStr, MinLen, 
     					  MinFreq, 1, SuffixTreeExec).
   
    

    
remove_short_clones(_C={Rs, {Len, _Freq}}, MinToks, MinFreq) ->
    Rs1=[R||R<-Rs, NumToks<-[[element(2, Elem)||Elem<-R]],
	    lists:sum(NumToks)>=MinToks],
    Freq1 = length(Rs1),
    case Freq1>=MinFreq of
	true ->
	    [{Rs1, {Len, Freq1}}];
	false->
	    []
    end.

   
no_of_tokens(Node) when is_list(Node)->
    Str = format(refac_syntax:block_expr(Node)),
    {ok, Toks,_}=refac_scan:string(Str, {1,1}, 8, unix),
    length(Toks)-2;
no_of_tokens(Node) ->
    Str = format(Node),
    {ok, Toks,_} =refac_scan:string(Str, {1,1}, 8, unix),
    length(Toks).

combine_clones_by_au([]) -> [];
combine_clones_by_au(Cs = [{_Ranges, _Len, _F, _Code}| _T]) ->
    Cs1 = refac_util:group_by(4, Cs),
    combine_clones_by_au_1(Cs1,[]).

combine_clones_by_au_1([], Acc) ->
    Acc;
combine_clones_by_au_1([Cs=[{_Ranges, Len, _Freq, Code}|_]|T], Acc) ->
    NewRanges=sets:to_list(sets:from_list(lists:append([Rs||{Rs, _, _, _}<-Cs]))),
    NewFreq = length(NewRanges),
    combine_clones_by_au_1(T, [{NewRanges, Len, NewFreq, Code}|Acc]).
    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%     transform the absolute locations in an AST to          %%
%%     relative locations                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
absolute_to_relative_loc(AST, OffLine) ->
    {AST1, _} = ast_traverse_api:full_tdTP(fun do_abs_to_relative_loc/2, 
					   AST, OffLine),
    AST1.
do_abs_to_relative_loc(Node, OffLine) ->
    As = refac_syntax:get_ann(Node),
    As1 = [abs_to_relative_loc_in_ann(A, OffLine) || A <- As],
    {L, C} = refac_syntax:get_pos(Node),
    Node1 = refac_syntax:set_pos(Node, {to_relative(L, OffLine), C}),
    {refac_syntax:set_ann(Node1, As1), true}.

abs_to_relative_loc_in_ann(Ann, StartLine) ->
    case Ann of
	{range, {{L1, C1},{L2, C2}}} ->
	    {range, {{to_relative(L1,StartLine), C1}, 
		     {to_relative(L2,StartLine), C2}}};
	{bound, Vars} ->
	    {bound, [{V, {to_relative(L,StartLine),C}}||{V, {L,C}}<-Vars]};
	{free, Vars} ->
	    {free, [{V, {to_relative(L,StartLine),C}}||{V, {L,C}}<-Vars]};
	{def, Locs} ->
	    {def, [{to_relative(L,StartLine),C}||{L, C}<-Locs]};
	{fun_def, {M, F, A,{L1, C1},{L2, C2}}} ->
	    {fun_def, {M, F, A, {to_relative(L1,StartLine),C1}, 
		       {to_relative(L2,StartLine), C2}}};
	%% the following has nothing to do with locations,
	%% just remove some information not to be used from 
        %% from the AST.
	{toks, _} ->  
	    {toks, []};
	{env, _} ->
	    {env, []};
	_ -> Ann
    end.
to_relative(Line, StartLine) when Line>0->
    Line-StartLine+1;
to_relative(Line, _StartLine) -> 
    Line.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%       Simplify the anti unifier generated                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% simplify the anti unifier generated. 
%% currently, only check the last two expressions, and simplify:
%% Pats = Expr, Pats  to  Expr.
simplify_anti_unifier(AUForm) ->
    AUFunClause=hd(refac_syntax:function_clauses(AUForm)),
    FunName = refac_syntax:function_name(AUForm),
    Pats = refac_syntax:clause_patterns(AUFunClause),
    AUBody = refac_syntax:clause_body(AUFunClause),
    AUBody1 = simplify_anti_unifier_body(AUBody),
    C = refac_syntax:clause(Pats, none, AUBody1),
    NewAU=refac_syntax:function(FunName, [C]),
    format(NewAU).
    
simplify_anti_unifier_body(AUBody) when length(AUBody)<2 ->
    AUBody;
simplify_anti_unifier_body(AUBody) ->
    [E1,E2|Exprs] = lists:reverse(AUBody),
    case refac_syntax:type(E2) of
	match_expr ->
	    {E2Pat, E2Body} = {refac_syntax:match_expr_pattern(E2),
			       refac_syntax:match_expr_body(E2)},
	    case same_expr(E2Pat, E1) of
		true ->
		    lists:reverse([E2Body|Exprs]);
		false ->
		    AUBody
	    end;
	_ -> AUBody
    end.

same_expr(Expr1, Expr2) ->
    {ok, Ts1, _} = erl_scan:string(format(Expr1)),
    {ok, Ts2, _} = erl_scan:string(format(Expr2)),
    refac_util:concat_toks(Ts1)==refac_util:concat_toks(Ts2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                            %%
%%       between Ets and Dets                                 %%
%%                                                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


from_dets(Ets, Dets, Inc) when is_atom(Ets) ->
    EtsRef = ets:new(Ets, [set, public]),
    case Inc of
	true ->
	    case Dets of
		none -> EtsRef;
		_ ->
		    case dets:open_file(Dets, [{access, read}]) of
			{ok, D} ->
			    true = ets:from_dets(EtsRef, D),
			    ok = dets:close(D),
			    EtsRef;
			{error, _Reason} ->
			    EtsRef
		    end
	    end;
	false ->
	    EtsRef
    end.

to_dets(Ets, none) ->
    ets:delete(Ets);   
to_dets(Ets, Dets) ->
    try file:delete(Dets), 
	 MinSize = ets:info(Ets, size),
	 Res= dets:open_file(Dets, [{min_no_slots, MinSize}]),
	 {ok, DetsRef} = Res,
	 ok = dets:from_ets(DetsRef, Ets),
	 ok = dets:sync(DetsRef),
	 ok = dets:close(DetsRef),
	 ets:delete(Ets)
    catch
	_E1:_E2 ->
	    ok
    end.
	
	 

%%-spec(get_parameters_eclipse/5::(MinLen::integer(), MinToks::integer(), MinFreq::integer(), 
%%					 MaxNewVars::integer(),SimiScore::float())->
%%					      {integer(), integer(), integer(), integer(),float()}).
get_parameters_eclipse(MinLen1,MinToks1,MinFreq1,MaxNewVars1,SimiScore1) ->
    MinLen = case MinLen1<1 of
	       true ->
		   ?DEFAULT_LEN;
	       _ -> MinLen1
	     end, 
    MinFreq = case MinFreq1<?DEFAULT_FREQ of
		true ->
		    ?DEFAULT_FREQ;
		_ -> MinFreq1
	      end, 
    MinToks = case MinToks1< ?MIN_TOKS of
		true -> ?MIN_TOKS;
		_ -> MinToks1
	      end, 
    MaxNewVars = case MaxNewVars1<0 of
		   true ->
		       ?DEFAULT_NEW_VARS;
		   _ -> MaxNewVars1
		 end, 
    SimiScore = case SimiScore1>=0.1 andalso SimiScore1=<1.0 of
		  true -> SimiScore1;
		  _ -> ?DefaultSimiScore
		end, 
    {MinLen,MinToks,MinFreq,MaxNewVars,SimiScore}.

get_parameters(MinLen1,MinToks1,MinFreq1,MaxVars1,SimiScore1) ->
    MinLen = get_parameters_1(MinLen1,?DEFAULT_LEN,1), 
    MinToks = get_parameters_1(MinToks1,?DEFAULT_TOKS, ?MIN_TOKS), 
    MinFreq = get_parameters_1(MinFreq1,?DEFAULT_FREQ,?DEFAULT_FREQ), 
    MaxVars = get_parameters_1(MaxVars1,?DEFAULT_NEW_VARS,0), 
    SimiScore = try
		  case SimiScore1 of
		    [] -> ?DefaultSimiScore;
		    _ -> S = list_to_float(SimiScore1), 
			 case S>=0.1 andalso S=<1.0 of
			   true -> S;
			   _ -> ?DefaultSimiScore
			 end
		  end
		catch
		    _:_ -> throw({error,"Parameter input is invalid."})
		end, 
    {MinLen,MinToks,MinFreq,MaxVars,SimiScore}.


get_parameters_1(Input, DefaultVal, MinVal) ->
    try
      case Input == [] orelse list_to_integer(Input) < MinVal of
	true -> DefaultVal;
	_ -> list_to_integer(Input)
      end
    catch
	_:_ -> throw({error, "Parameter input is invalid."})
    end.

get_file_status_info(Files, Tabs, Threshold) ->
    FileHashTab = Tabs#tabs.file_hash_tab,
    %% all the files process during the last run of clone detection.
    OldFiles = lists:append(ets:match(FileHashTab,{'$1', '_'})),
    case file:read_file(?Threshold) of
	{ok, Binary} ->
	    LastThreshold = binary_to_term(Binary),
	    case LastThreshold == Threshold of
		true ->
		    Fun = fun (F) ->
				  CheckSum = refac_util:filehash(F),
				  case ets:lookup(Tabs#tabs.file_hash_tab, F) of
				      [{F, CheckSum1}] when CheckSum/=CheckSum1 ->
					  true;
				      _ -> false
				  end
			  end,
		    DeletedFiles = OldFiles -- Files,
		    NewFiles = Files -- OldFiles,
		    ChangedFiles = lists:filter(Fun, Files),
		    {DeletedFiles, ChangedFiles, NewFiles};
		false ->
		    file:delete(?ExpSeqFile),
		    OldFiles = lists:append(ets:match(FileHashTab,{'$1', '_'})),
		    {OldFiles, [], Files}
	    end;
	{error, _Reason} ->
	    file:delete(?ExpSeqFile),
	    OldFiles = lists:append(ets:match(FileHashTab,{'$1', '_'})),
	    {OldFiles, [], Files}
    end.
    

write_file(File, Data) ->
    case File of 
	none ->
	    ok;
	_->
	    file:write_file(File, Data)
    end.

gen_clone_report(Cs) ->
     gen_clone_report(Cs, 1, "").
gen_clone_report([], _Num, Str) ->
     lists:reverse(Str);
gen_clone_report([_C={_Ranges, Len, F, {Code, {Pars,NewVars}}}|Cs], Num, Str) ->
    Str1 =io_lib:format("Clone ~p: number of duplications: ~p, number of expressions: ~p, "
			"number of parameters in AU: ~p, number of NewVars in AU: ~p, AU:\n",
			[Num,F,Len, Pars, NewVars])++Code++"\n",
    gen_clone_report(Cs, Num+1, lists:reverse(lists:flatten(Str1))++Str).


is_whitespace_or_comment({whitespace, _, _}) ->
    true;
is_whitespace_or_comment({comment, _, _}) ->
    true;
is_whitespace_or_comment(_) -> false.

