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

-export([inc_sim_code_detection/8, inc_sim_code_detection_in_buffer/8]).

-export([inc_sim_code_detection_eclipse/8]).

-include("../include/wrangler.hrl").

%% default threshold values.
-define(DefaultSimiScore, 0.8).
-define(DEFAULT_LEN, 5).
-define(DEFAULT_TOKS, 40).
-define(DEFAULT_FREQ, 2).
-define(DEFAULT_SIMI_SCORE, 0.8).
-define(DEFAULT_NEW_VARS, 3).
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
	 clone_tab,
	 tmp_ast_tab	 
	}).

%% A record representing a clone class.
 %% -record(clone, 
 %% 	 {ranges,  %% start end end locations/expression of each clone instance.
 %% 	  len,     %% the length of each clone instance.
 %% 	  freq,    %% the number of duplication times.
 %% 	  au       %% the anti-unification of the clone class.
 %% 	 }).

%%-spec(inc_sim_code_detection_in_buffer/8::(FileName::filename(), MinLen::float(), MinToks::integer(),
%%				 MinFreq::integer(),  MaxVars:: integer(),SimiScore::float(), 
%%				 SearchPaths::[dir()], TabWidth::integer()) -> {ok, string()}).
inc_sim_code_detection_in_buffer(FileName, MinLen1, MinToks1, MinFreq1, MaxVars1, SimiScore1, SearchPaths, TabWidth)->
    inc_sim_code_detection([FileName], MinLen1, MinToks1, MinFreq1, MaxVars1, SimiScore1, SearchPaths, TabWidth).
  
%%-spec(inc_sim_code_detection/8::(DirFileList::[filename()|dir()], MinLen::float(), MinToks::integer(),
%%				 MinFreq::integer(),  MaxVars:: integer(),SimiScore::float(), 
%%				 SearchPaths::[dir()], TabWidth::integer()) -> {ok, string()}).

inc_sim_code_detection(DirFileList,MinLen1,MinToks1,MinFreq1,MaxVars1,SimiScore1,SearchPaths,TabWidth) ->
    ?wrangler_io("\nCMD: ~p:inc_sim_code_detection(~p,~p,~p,~p,~p, ~p,~p,~p).\n",
		 [?MODULE,DirFileList,MinLen1,MinToks1,MinFreq1,MaxVars1,SimiScore1,SearchPaths,TabWidth]), 
    {MinLen,MinToks,MinFreq,MaxVars,SimiScore} = get_parameters(MinLen1,MinToks1,MinFreq1,MaxVars1,SimiScore1), 
    Files = refac_util:expand_files(DirFileList,".erl"), 
    case Files of
      [] ->
	    ?wrangler_io("Warning: No files found in the searchpaths specified.",[]);
	_ -> inc_sim_code_detection_0(Files, MinLen, MinToks, MinFreq, MaxVars, SimiScore, SearchPaths, TabWidth, emacs)
    end, 
    {ok,"Clone detection finish."}.


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
	_ -> inc_sim_code_detection_0(Files, MinLen, MinToks, MinFreq, MaxVars, 
				    SimiScore, SearchPaths, TabWidth, eclipse)
    end.

inc_sim_code_detection_0(Files, MinLen, MinToks, MinFreq, MaxVars, 
			 SimiScore, SearchPaths, TabWidth, Editor) ->
    %% dets tables used to cache information.
    Tabs = #tabs{ast_tab = from_dets(ast_tab,?ASTTab), 
		 var_tab = from_dets(var_tab,?VarTab), 
		 file_hash_tab = from_dets(file_hash_tab,?FileHashTab), 
		 exp_hash_tab = from_dets(expr_hash_tab,?ExpHashTab), 
		 clone_tab = from_dets(expr_clone_tab,?CloneTab), 
		 tmp_ast_tab = ets:new(tmp_ast_tab,[set,public])}, 
    
    %% Threshold parameters.
    Threshold = #threshold{min_len = MinLen, 
			   min_freq = MinFreq, 
			   min_toks = MinToks, 
			   max_new_vars = MaxVars, 
			   simi_score = SimiScore}, 
    %% Clone detection.
    Cs = inc_sim_code_detection(Files,Threshold,Tabs,SearchPaths,TabWidth, Editor), 
    
    %% output cache information to dets tables.
    to_dets(Tabs#tabs.ast_tab,?ASTTab), 
    to_dets(Tabs#tabs.var_tab,?VarTab), 
    to_dets(Tabs#tabs.file_hash_tab,?FileHashTab), 
    to_dets(Tabs#tabs.exp_hash_tab,?ExpHashTab), 
    to_dets(Tabs#tabs.clone_tab,?CloneTab), 
    file:write_file(?Threshold,term_to_binary(Threshold)), 
    ets:delete(Tabs#tabs.tmp_ast_tab), 
    Cs.
 
%% incremental clone detection.
inc_sim_code_detection(Files, Thresholds, Tabs, SearchPaths, TabWidth, Editor) ->
   
    %%Time1 = time(),
    %% get file status change info.
    {FilesDeleted, FilesChanged, NewFiles} = get_file_status_info(Files, Tabs,Thresholds),
    
    %% remove information related to files that no longer exist from ets tables.
    remove_old_file_entries(FilesDeleted, Tabs),
    
    ASTPid = start_ast_process(Tabs#tabs.ast_tab, {FilesDeleted, FilesChanged, NewFiles}),
    
    %% generate and hash those new, or changed, files.
    generalise_and_hash_ast(Files, Thresholds, Tabs, ASTPid, SearchPaths, TabWidth),
   %% ?wrangler_io("Generalise and hash finished.\n", []),
    
    %% Generate clone candidates using suffix tree based clone detection techniques.
    Dir = filename:dirname(hd(Files)),
    Cs = get_clone_candidates(ASTPid, Thresholds, Dir),
    %% Time2 = time(),
    %%?wrangler_io("\nInitial candiates finished\n", []),
    
    ?wrangler_io("\nNumber of initial clone candidates: ~p\n", [length(Cs)]),
    CloneCheckerPid = start_clone_check_process(),
 
    %% examine each clone candiate and filter false positives.
    Cs2 = examine_clone_candidates(Cs, Thresholds, Tabs, CloneCheckerPid, 1),
    stop_clone_check_process(CloneCheckerPid),
    stop_ast_process(ASTPid),
    Cs3 = combine_clones_by_au(Cs2),
   %% ?wrangler_io("Time used: \n~p\n", [{Time1, Time2}]).
    case Editor of 
	emacs ->
	    refac_code_search_utils:display_clone_result(Cs3, "Similar");
	_ -> Cs3
    end.
   
%% Get files that are deleted/changed/newly added since the last 
%% run of the clone detection.
%% This function 
get_file_status_info(Files, Tabs, Threshold) ->
    FileHashTab=Tabs#tabs.file_hash_tab,
    %% all the files process during the last run of clone detection.
    OldFiles = lists:append(ets:match(FileHashTab,{'$1', '_'})),
    case file:read_file(?Threshold) of
	{ok, Binary} ->
	    LastThreshold = binary_to_term(Binary),
	    case LastThreshold == Threshold of
		true ->
		    Fun =fun(F)->
				 CheckSum= refac_misc:filehash(F),
				 case ets:lookup(Tabs#tabs.file_hash_tab, F) of
				     [{F, CheckSum1}] when CheckSum/=CheckSum1 ->
					 true;
				     _ ->false
				 end
			 end,
		    DeletedFiles = OldFiles -- Files,
		    NewFiles = Files -- OldFiles, 
		    ChangedFiles = lists:filter(Fun, Files),
		    {DeletedFiles, ChangedFiles, NewFiles};
		false ->
		    OldFiles = lists:append(ets:match(FileHashTab,{'$1', '_'})),
		    {OldFiles, [], Files}
	    end;
	{error, _Reason} ->
	    OldFiles = lists:append(ets:match(FileHashTab,{'$1', '_'})),
	    {OldFiles, [], Files}
    end.

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
generalise_and_hash_ast(Files, Threshold, Tabs, ASTPid, SearchPaths, TabWidth) ->
    lists:foreach(fun(File) ->
			  generalise_and_hash_file_ast(
			    File, Threshold, Tabs, ASTPid,SearchPaths, TabWidth)
		  end, Files).

generalise_and_hash_file_ast(File, Threshold, Tabs, ASTPid, SearchPaths, TabWidth) ->
    NewCheckSum = refac_misc:filehash(File),
    case ets:lookup(Tabs#tabs.file_hash_tab, File) of
	[{File, NewCheckSum}] ->
	    ?debug("\nFile not changed:~p\n", [File]),
	    ok;
	[{File, _NewCheckSum1}] ->
	    ?debug("\n File changed:~p\n", [File]),
	    ets:insert(Tabs#tabs.file_hash_tab, {File, NewCheckSum}),
	    generalise_and_hash_file_ast_1(
	      File, Threshold, Tabs,ASTPid, false,SearchPaths, TabWidth);
	[] ->
	    ?debug("New file:~p\n",[File]),
	    ets:insert(Tabs#tabs.file_hash_tab, {File, NewCheckSum}),
	    generalise_and_hash_file_ast_1(
	      File, Threshold, Tabs, ASTPid, true,SearchPaths, TabWidth)
    end.

%% Generalise and hash the AST for an single Erlang file.
generalise_and_hash_file_ast_1(FName, Threshold, Tabs, ASTPid, IsNewFile, SearchPaths, TabWidth) ->
    {ok, {AnnAST, _Info}} = refac_util:quick_parse_annotate_file(FName, SearchPaths, TabWidth),
    Forms =refac_syntax:form_list_elements(AnnAST),
    F =fun (Form) -> 
	       case refac_syntax:type(Form) of 
		   function ->
		       %% only process function definitions.
		       generalise_and_hash_function_ast(Form, FName, IsNewFile, Threshold, Tabs, ASTPid);
		   _ -> ok
	       end
       end,
    lists:foreach(fun(Form) -> F(Form) end, Forms).

%% generalise and hash the AST of a single function.
generalise_and_hash_function_ast(Form, FName, IsNewFile, Threshold, Tabs, ASTPid) ->
    FunName = refac_syntax:atom_value(refac_syntax:function_name(Form)),
    Arity = refac_syntax:function_arity(Form),
    HashVal = erlang:md5(refac_prettypr:format(Form)),
    case IsNewFile of
	true ->
	  %% a new file;
	    generalise_and_hash_function_ast_1(FName, Form, FunName, Arity, HashVal, Threshold, Tabs, ASTPid);
	false ->
	    %% a chnanged file;
	    case ets:lookup(Tabs#tabs.var_tab, {FName, FunName, Arity}) of
		[{{FName, FunName, Arity}, HashVal, _VarInfo}] ->
		    %% Same Hash value means that this function has not be
		    %% syntactically changed, but location might be changed.
		    %% StartLine is used to get the absolute location of the function.
		    {StartLine, _} = refac_syntax:get_pos(Form),
		    quick_hash_function(ASTPid, {{FName, FunName, Arity}, StartLine});
		_ ->
		    %% Function is new, or has been edited since last run of clone detection.
		    generalise_and_hash_function_ast_1(FName, Form, FunName, Arity, HashVal, Threshold, Tabs, ASTPid)
	    end
    end.
%% generalise and hash a function that is either new or has been changed since last run of clone detection.
generalise_and_hash_function_ast_1(FName, Form, FunName, Arity, HashVal, Threshold, Tabs, ASTPid) ->
    {StartLine, _} = refac_syntax:get_pos(Form),
    %% Turn absolute locations to relative locations, so 
    %% so that the result can be reused.
    Form1 = absolute_to_relative_loc(Form, StartLine),
    %% all locations are relative locations.
    %% variable binding information is needed by the anti-unification process.
    AllVars = refac_misc:collect_var_source_def_pos_info(Form1),
    %% I also put the Hashvalue of a function in var_tab.
    ets:insert(Tabs#tabs.var_tab, {{FName, FunName, Arity}, HashVal, AllVars}),
    ast_traverse_api:full_tdTP(fun generalise_and_hash_function_ast_2/2,
			       Form1, {FName, FunName, Arity, ASTPid, Threshold, StartLine}).

%% generalise and has the function AST.
generalise_and_hash_function_ast_2(Node, {FName, FunName, Arity, ASTPid, Threshold, StartLine}) ->
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
start_ast_process(ASTTab, {FilesDeleted, FilesChanged, NewFiles}) ->
    HashPid = start_hash_process({FilesDeleted, FilesChanged, NewFiles}),
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
      {Pid, Cs} ->
	  Cs
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
	  [insert_and_hash_expr(ASTTab, HashPid, {M, F, A}, StartLine,
				NewIndex, {E, I})
	   || {E, I} <- ExprASTsWithIndex],
	    insert_dummy_entry(HashPid, M),
	    ast_loop(ASTTab, {M, F, A, NewIndex + Len, HashPid});
	{quick_hash, {{FName, FunName, Arity}, StartLine}} ->
	    update_hash(HashPid, {{FName, FunName, Arity}, StartLine}),
	    ast_loop(ASTTab, {FName, FunName, Arity, Index, HashPid});
	{get_clone_candidates, From, Thresholds, Dir} ->
	    Cs = get_clone_candidates(HashPid, Thresholds, Dir),
	    From ! {self(), Cs},
	    ast_loop(ASTTab, {CurM, CurF, CurA, Index, HashPid});
	stop ->
	    stop_hash_process(HashPid),
	    ok;
	_Msg ->
	    ?wrangler_io("Unexpected message:\n~p\n", [_Msg]),
	  ok
    end.

%% Insert an expression into the AST table, generalise it, and 
%% hash the result.
insert_and_hash_expr(ASTTab, HashPid, {M, F, A}, StartLine,
		     StartIndex, {Expr, RelativeIndex}) ->
    %% Num of tokens is used to chech the size of a clone candidate.
    NoOfToks = no_of_tokens(Expr),
    %% insert the AST of an expression into the ast table.
    ets:insert(ASTTab, {{M, F, A, StartIndex + RelativeIndex}, Expr}),
    E1 = do_generalise(Expr),
    %% get the hash values of the generalised expression.
    HashVal = erlang:md5(refac_prettypr:format(E1)),
    %% the location here is relative location.
    StartEndLoc = refac_misc:get_start_end_loc(Expr),
    insert_hash(HashPid, {HashVal, {{M, F, A, StartIndex + RelativeIndex},
				    NoOfToks, {StartEndLoc, StartLine}}}).

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

start_hash_process({FilesDeleted, FilesChanged, NewFiles}) ->
    %% I put NewFiles here too! 
    ObsoleteFiles = FilesDeleted ++ FilesChanged ++ NewFiles,  
    ExpHashTab = from_dets(expr_hash_tab, ?ExpHashTab),
    case file:read_file(?ExpSeqFile) of
      {ok, Binary} ->
	    Data = binary_to_term(Binary),
	    %% remove obsolete entries.
	    NewData =
		[{{{File, F, A, I}, Toks, Loc, false}, HashValIndex}
		 || {{{File, F, A, I}, Toks, Loc, _IsNew}, HashValIndex} <- Data,
		    not lists:member(File, ObsoleteFiles)],
	    %% Data of those files that has been changed. 
            %% Store it sparately for reuse.
	    ChangedData = [Elem || Elem = {{{File, _F, _A, _I}, _Toks, _Loc, _IsNew}, _HashValIndex}
				       <- Data,
				   lists:member(File, FilesChanged)],
	    NextIndex= ets:info(ExpHashTab, size)+1,
	    spawn_link(fun () -> hash_loop({NextIndex, ExpHashTab, {NewData, lists:reverse(ChangedData)}}) end);
	_Res ->
	    Data = {[{{{'_', '_', '_', '_'}, 0, {{0, 0}, 0}, false}, '#'}], []},
	    spawn_link(fun () -> hash_loop({1, ExpHashTab, Data}) end)
    end.
  
stop_hash_process(Pid) ->
    Pid!stop. 

insert_hash(Pid, {HashVal, Elem={_MFAI, _NumofToks, _Loc}}) ->
    Pid ! {add, {HashVal, Elem}}.

update_hash(Pid, {{FileName, FunName, Arity}, StartLine})->
    Pid ! {quick_hash,{{FileName, FunName, Arity}, StartLine}}.

insert_dummy_entry(Pid,FName) ->
    Pid ! {add_dummy, FName}.

hash_loop({Index, ExpHashTab, {NewData, OldData}}) ->
    receive
	%% add a new entry.
	{add, {Key, {{M, F, A, Index1}, NumOfToks,{StartEndLoc, StartLine}}}} ->
	    case ets:lookup(ExpHashTab, Key) of
		[{Key, I}] ->
		    %% an existing key.
		    %% here 'true' means that the entry is completely new.
		    Elem={{{M, F, A, Index1}, NumOfToks,{StartEndLoc, StartLine}, true}, I},
		    hash_loop({Index, ExpHashTab,{[Elem| NewData],OldData}});
		[] -> 
		    %% New Key.
		    ets:insert(ExpHashTab, {Key, Index}),
		    Elem={{{M, F, A, Index1}, NumOfToks, {StartEndLoc, StartLine}, true}, Index},
		    hash_loop({Index + 1, ExpHashTab, {[Elem|NewData], OldData}})
	    end;
	{quick_hash,{{FileName, FunName, Arity}, StartLine}} ->
	    %%?debug("OldData:\n~p\n", [OldData]),
	    OldData1 = lists:dropwhile(fun({{{M,F,A,_}, _,_,_},_}) ->
					       {M,F,A} /={FileName, FunName, Arity}
				       end, OldData),
	    %%?debug("OldData1:\n~p\n", [OldData1]),
	    DataNeeded=lists:takewhile(fun({{{M,F,A,_}, _,_,_},_}) ->
					       {M,F,A} =={FileName, FunName, Arity}
						   orelse
						   {M,F,A} =={FileName,'_','_'}
				       end, OldData1),
	    %% here 'false' means that the entry is from an existing entry.
	    DataToAdd=[{{{M,F,A, I}, NumOfToks, {StartEndLoc, StartLine},false}, ExprHashIndex}
		       ||{{{M,F,A, I}, NumOfToks, {StartEndLoc, _}, _},ExprHashIndex}<-DataNeeded],
	    %%?debug("DataToAdd:\n~p\n", [DataToAdd]),
	    hash_loop({Index, ExpHashTab, {lists:reverse(DataToAdd)++NewData, OldData}}); 
	{add_dummy, FName} ->
	    DummyElem={{{FName, '_', '_', '_'}, 0, {{0,0},0}, false}, '#'},
	    hash_loop({Index, ExpHashTab, {[DummyElem|NewData], OldData}});
	{get_clone_candidates, From, Thresholds, Dir} ->
	    Cs = search_for_clones(Dir, lists:reverse(NewData), Thresholds),
	    From ! {self(), Cs},
	    hash_loop({Index, ExpHashTab, {NewData, OldData}});
	stop ->
	    file:write_file(?ExpSeqFile, term_to_binary(NewData)),
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
	{get_clones, From, _ASTTab} ->
	    Cs0=remove_sub_clones(Cs),
	    Cs1=[{AbsRanges, Len, Freq, AntiUnifier}||
		    {_, {Len, Freq}, AntiUnifier,AbsRanges}<-Cs0],
	    From ! {self(), Cs1},
	    clone_check_loop(Cs);       
	stop ->
	    ok;
	_Msg -> 
	    ?wrangler_io("Unexpected message:\n~p\n",[_Msg]),
	    clone_check_loop(Cs)
    end.
 
%%=============================================================================
%% check each candidate clone, and drive real clone classes.
examine_clone_candidates([], _Thresholds, Tabs, Pid, _Num) ->
    get_final_clone_classes(Pid, Tabs#tabs.ast_tab);
examine_clone_candidates([C| Cs], Thresholds, Tabs, Pid, Num) ->
    output_progress_msg(Num),
    case examine_a_clone_candidate(C, Thresholds, Tabs) of
	[] ->
	    %% the candidate clone does not contain any 
	    %% real clone classes.
	    ok;
	ClonesWithAU ->
	    %% some real clone classes are returned from 
            %% the candidate clone.
	    add_new_clones(Pid, ClonesWithAU)
    end,
    examine_clone_candidates(Cs, Thresholds, Tabs, Pid, Num + 1).

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
		    %% used cached info. Examination for this clone candidate 
                    %% finishes, and delete the temporary data.
		    ets:delete_all_objects(Tabs#tabs.tmp_ast_tab),
		    %% update the StartLine offset in Clones.
		    Res =update_clone_class_locations(Ranges, Clones),
		    [get_clone_class_in_absolute_locs(Clone)||Clone<-Res];
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
examine_a_clone_candidate_1(C={Ranges, {_Len, _Freq}}, Thresholds, Tabs) ->
    ASTTab = Tabs#tabs.ast_tab,
    %% Put ASTs related to the current clone in a temporary ets table.
    lists:foreach(fun ({Key, _, _, _}) ->
			  ets:insert(Tabs#tabs.tmp_ast_tab, ets:lookup(ASTTab, Key))
		  end, lists:append(Ranges)),
    %% check candidate clone class memebers using anti-unification.
    Clones = examine_clone_class_members(Ranges, Thresholds, Tabs, []),
    %% generate anti-unifier for each real clone class.
    ClonesWithAU = [{Rs, {Len, Freq}, get_anti_unifier(Rs, Tabs, Info)}
		    || {Rs, {Len, Freq}, Info} <- Clones],
    %% generate function call for each member of a clone class.
    ClonesWithAUAndFunCalls = [attach_fun_call_to_range(Clone, Tabs) || Clone <- ClonesWithAU],
    %% insert the clone result into the clone table for resue.
    ets:insert(Tabs#tabs.clone_tab, {hash_a_clone_candidate(C), ClonesWithAUAndFunCalls}),
    %% delete the temporary data.
    ets:delete_all_objects(Tabs#tabs.tmp_ast_tab),
    %% clone classes with absolute locations for displaying results.
    [get_clone_class_in_absolute_locs(Clone) 
     || Clone <- ClonesWithAUAndFunCalls].

%% check the clone members of a clone candidate using 
%% anti-unification techniques.   
examine_clone_class_members(Rs, Thresholds, _, Acc) 
  when length(Rs)< Thresholds#threshold.min_freq ->
    %% The number of clone memebers left is less 
    %% than the min_freq threshold, so the examination
    %% finishes, and sub-clones are removed.
    remove_sub_clones(Acc);

examine_clone_class_members(Ranges, Thresholds,Tabs, Acc) ->
    %% Take the first clone member and  try to anti_unify other 
    %% clone members with this member. If there is a real clone 
    %% class found, then the anti-unifier of the class is derrived 
    %% by generalisation of the first clone member.
    [Range1|Rs]=Ranges,
    %% get the expression ASTs for the first candidate clone member.
    Exprs1 = get_expr_list(Range1, Tabs#tabs.tmp_ast_tab),
    %% try to anti_unify each of the remaining candidate clone members 
    %% with the first candidate clone member.
    Res = [begin 
	       Exprs2= get_expr_list(Range2, Tabs#tabs.tmp_ast_tab),
	       do_anti_unification({Range1, Exprs1}, {Range2, Exprs2})
	   end|| Range2<-Rs],
    %% process the 3anti_unification result.
    Clones = process_au_result(Res, Thresholds, Tabs),
    %% get the maximal length of the clone clone members returned.
    MaxCloneLength= case Clones ==[] of 
			true -> 
			    0;
			_-> 
			    %% make sure the clones returned are ordered!!!
			    element(1, element(2, hd(Clones)))
		    end,
    InitialLength = length(Range1),
    case MaxCloneLength /= InitialLength of
	true ->
	    %% the original expression sequences have been chopped into shorter ones.
	    examine_clone_class_members(Rs, Thresholds,Tabs, Clones ++ Acc);
	false ->
	    %% the original expression still a class member of the clones returned.
	    Rs1 = element(1, hd(Clones)),
	    RemainedRanges = Ranges -- Rs1,
	    examine_clone_class_members(RemainedRanges, Thresholds,Tabs, Clones ++ Acc)

    end.

%% try to anti-unify two expression sequences.
do_anti_unification({Range1,Exprs1}, {Range2, Exprs2}) ->
    ZippedExprs1=lists:zip(Range1, Exprs1),
    ZippedExprs2=lists:zip(Range2, Exprs2),
    ZippedExprs=lists:zip(ZippedExprs1, ZippedExprs2),
    [begin
	 {Index1, Index2,
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
    BVs = refac_misc:get_bound_vars(Expr1),
    F = fun ({E1, E2}) ->
		case refac_syntax:type(E1) of
		  variable ->
			E1Ann = refac_syntax:get_ann(E1),
			case lists:keysearch(category, 1, E1Ann) of
			    {value, {category, {macro_name, _,_}}} ->
				false;
			    _ -> has_same_subst(E1, E2, SubSt)
			end;
		    _ ->
			%% the expression to be replaced should not contain local variables.
			BVs -- refac_misc:get_free_vars(E1) == BVs
		end
	end,
    lists:all(F, SubSt).

has_same_subst(E1, E2, SubSt) ->
    E1Ann = refac_syntax:get_ann(E1),
    {value, {def, DefPos}} = lists:keysearch(def, 1, E1Ann),
    %% local vars should have the same substitute.
    not lists:any(
	  fun ({E11, E21}) ->
		  refac_syntax:type(E11) == variable andalso
		      {value, {def, DefPos}} == lists:keysearch(
						  def, 1, refac_syntax:get_ann(E11))
		      andalso
		      refac_prettypr:format(refac_misc:reset_attrs(E2))
		      =/= refac_prettypr:format(refac_misc:reset_attrs(E21))
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
process_one_au_result(OneAURes, Thresholds, Tabs) ->
    SubAULists=group_au_result(OneAURes, Thresholds),
    ClonePairs =lists:append([get_clone_pairs(SubAUList, Thresholds, Tabs)
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
  

clone_pair_above_min_size(CP, Thresholds) ->
    length(CP)>=Thresholds#threshold.min_len andalso
	lists:sum([element(2, E1)||{E1,_E2, _S}<-CP])
	>=Thresholds#threshold.min_toks.

get_clone_pairs(AURes, Thresholds, Tabs) ->
    get_clone_pairs(AURes, Thresholds, Tabs, {[],[]},[]).

get_clone_pairs([], Thresholds, Tabs, {_, ClonePairAcc}, Acc) ->
    case clone_pair_above_min_size(ClonePairAcc, Thresholds) of
	true ->
	    ClonePairs = decompose_clone_pair_by_simi_score(
			   lists:reverse(ClonePairAcc), Thresholds, Tabs),
	    ClonePairs ++ Acc;
	false ->
	    Acc
    end;
get_clone_pairs([CurPair = {_E1, _E2, SubSt}| AURes], Thresholds, Tabs,
		{VarSubAcc, ClonePairAcc}, Acc) ->
    %% check the subsitution of variables. 
    %% variables with the same defining location should 
    %% has the same substitution.
    CurVarSubsts = get_var_subst(SubSt),
    case var_sub_conflicts(CurVarSubsts, VarSubAcc) of
	true ->
	    %% conflicting variable substitution.
	  case clone_pair_above_min_size(ClonePairAcc, Thresholds) of
	      true ->
		  NewClonePairs = decompose_clone_pair_by_simi_score(
				    lists:reverse(ClonePairAcc), Thresholds, Tabs),
		  NewAcc = NewClonePairs ++ Acc,
		  get_clone_pairs(AURes, Thresholds, Tabs, {[], []}, NewAcc);
	      false ->
		  %% the clone pairs is two short.
		  get_clone_pairs(AURes, Thresholds, Tabs, {[], []}, Acc)
	  end;
	false ->
	    get_clone_pairs(AURes, Thresholds, Tabs,
			    {CurVarSubsts ++ VarSubAcc, [CurPair] ++ ClonePairAcc}, Acc)
    end.


get_var_subst(SubSt) ->
    F=fun({E1, E2}) ->
	      {value, {def, DefPos}} = 
		  lists:keysearch(def, 1, refac_syntax:get_ann(E1)),
	      {DefPos, refac_prettypr:format(refac_misc:reset_attrs(E2))}
      end,	      
    [F({E1,E2})||
	{E1,E2}<-SubSt, 
	refac_syntax:type(E1)==variable, 
	not is_macro_name(E1)].

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
decompose_clone_pair_by_simi_score(ClonePair, Thresholds, Tabs) ->
    {Range1, Range2, Subst} = lists:unzip3(ClonePair),
    Exprs1 = get_expr_list(Range1, Tabs#tabs.tmp_ast_tab),
    Exprs2 = get_expr_list(Range2, Tabs#tabs.tmp_ast_tab),
    {SubExprs1, SubExprs2} = lists:unzip(lists:append(Subst)),
    Score1 = simi_score(Exprs1, SubExprs1),
    SimiScoreThreshold = Thresholds#threshold.simi_score,
    MaxNewVars = Thresholds#threshold.max_new_vars,
    case Score1 >= SimiScoreThreshold  andalso
	simi_score(Exprs2, SubExprs2) >= SimiScoreThreshold  andalso
	sets:size(exprs_to_be_generalised(lists:append(Subst))) =< MaxNewVars of	%%% ERROR HERE SHOULD NOT BE SUBST.
      true ->
	    [ClonePair];
      false ->
	    decompose_clone_pairs_by_simi_score_1(
	      {Range1, Range2, Subst}, {Exprs1, Exprs2}, Thresholds)
    end.

decompose_clone_pairs_by_simi_score_1({Range1, Range2, Subst}, {Exprs1, Exprs2},
				      Thresholds) ->
    RangeExprPairs1 = lists:zip(Range1, Exprs1),
    RangeExprPairs2 = lists:zip(Range2, Exprs2),
    ClonePairsWithExpr = lists:zip3(RangeExprPairs1, RangeExprPairs2, Subst),
    %% ClonePairWithSimiScore = 
    %% 	[{{ExprKey1, Expr1}, {ExprKey2, Expr2}, Sub,
    %% 	  {simi_score(Expr1, SubEs1), simi_score(Expr2, SubEs2)}}
    %% 	  || {{ExprKey1, Expr1}, {ExprKey2, Expr2}, Sub} <- ClonePairsWithExpr,
    %% 	    {SubEs1, SubEs2} <- [lists:unzip(Sub)]],
    decompose_clone_pair_by_simi_score_2(ClonePairsWithExpr, Thresholds, []).
 
decompose_clone_pair_by_simi_score_2(ClonePairsWithSimiScore, Thresholds) ->
    decompose_clone_pair_by_simi_score_2(ClonePairsWithSimiScore, Thresholds,[]).
decompose_clone_pair_by_simi_score_2([], _, Acc)->
    Acc;
decompose_clone_pair_by_simi_score_2(ClonePairWithSimiScore, Thresholds, Acc) ->
    Len= length(ClonePairWithSimiScore),
    ClonePair1 = lists:sublist(ClonePairWithSimiScore, Len-1),
    ClonePair2 = lists:sublist(ClonePairWithSimiScore, 2, Len-1),
    decompose_clone_pair_by_simi_score_3(ClonePair1, Thresholds)
	++ decompose_clone_pair_by_simi_score_3(ClonePair2, Thresholds) 
	++ Acc.


decompose_clone_pair_by_simi_score_3(ClonePairs, Thresholds)->
    SimiScoreThreshold = Thresholds#threshold.simi_score,
    case not clone_pair_above_min_size_1(ClonePairs, Thresholds) of 
	true ->
	    [];
	false ->
	    {Exprs1SubEs1Pairs, Exprs2SubEs2Pairs}
		=lists:unzip([{{Expr1, SubEs1}, {Expr2, SubEs2}}
			      ||{{_, Expr1}, {_, Expr2}, Sub}
				    <-ClonePairs, {SubEs1, SubEs2}<-[lists:unzip(Sub)]]),
	    {Exprs1, SubEs1} = lists:unzip(Exprs1SubEs1Pairs),
	    {Exprs2, SubEs2} = lists:unzip(Exprs2SubEs2Pairs),
	    SimiScore1 = simi_score(Exprs1,lists:append(SubEs1)),
	    case SimiScore1>=SimiScoreThreshold andalso 
		simi_score(Exprs2, lists:append(SubEs2))>=SimiScoreThreshold of
		true ->
		    NewClonePairs=[{ExprKey1, ExprKey2, Sub}
				   ||{{ExprKey1,_}, {ExprKey2,_}, Sub}<-ClonePairs],
		    [NewClonePairs];
		false ->
		    decompose_clone_pair_by_simi_score_2(ClonePairs, Thresholds)
	    end
    end.
		
clone_pair_above_min_size_1(CP, Thresholds) ->
    length(CP)>=Thresholds#threshold.min_len andalso
	lists:sum([element(2, (element(1,E1)))||{E1,_E2, _S}<-CP])
	>=Thresholds#threshold.min_toks.



simi_score(Expr, SubExprs) ->
    case no_of_nodes(Expr) of
      0 -> 0;
      ExprSize ->
	    %% NonVarExprs = [E || E <- SubExprs, refac_syntax:type(E) =/= variable],
	    %% NoOfNewVars = length(NonVarExprs),
	    %% Res = 1 - (no_of_nodes(SubExprs) - length(SubExprs)
	    %%  	       + NoOfNewVars * (NoOfNewVars + 1) / 2) / ExprSize
	    1 -((no_of_nodes(SubExprs)-length(SubExprs))/ExprSize)
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
 
get_one_clone_class(Range, ClonePairs, Thresholds, Tabs) ->
    Res = lists:append([get_one_clone_class_1(Range, ClonePair, Tabs)
			|| ClonePair <- ClonePairs]),
    CloneClasses =group_clone_pairs(Res, Thresholds),
    [begin
	 Exprs = get_expr_list(Range, Tabs#tabs.tmp_ast_tab),
	 [{{FName, FunName, Arity, _}, _, _,_}| _] = Range,
	 VarTab = Tabs#tabs.var_tab,
	 VarsToExport = get_vars_to_export(Exprs, {FName, FunName, Arity}, VarTab),
	 {Ranges, ExportVars, SubSt} = lists:unzip3(C),
	 %% VarstoExport format : [{name, pos}].
	 ExportVars1 = {element(1, lists:unzip(VarsToExport)), 
			lists:usort(lists:append(ExportVars))},
	 {[Range| Ranges], {length(Range), length(Ranges) + 1}, 
	  {Range, SubSt, ExportVars1}}
     end
     || C<-CloneClasses].


    
get_one_clone_class_1(Range, _ClonePair = {Range1, Range2, Subst}, Tabs) ->
    case Range -- Range1 == [] of
      true ->
	    %% Range is a sub list of Range1.
	    Len = length(Range),
	    R = hd(Range),
	    StartIndex=length(lists:takewhile(fun (R0) -> R0 /= R end, Range1))+1,
	    SubRange2 = lists:sublist(Range2, StartIndex, Len),
	    SubSubst = lists:append(lists:sublist(Subst, StartIndex, Len)),
	    Exprs2 = get_expr_list(SubRange2, Tabs#tabs.tmp_ast_tab),
	    [{{FName, FunName, Arity, _}, _, _, _}| _] = SubRange2,
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
    sets:from_list([refac_prettypr:format(refac_misc:reset_attrs(E1))||
		       {E1,_E2}<-SubSt, refac_syntax:type(E1)/=variable]).

   
num_of_new_vars(SubSt) ->
     length(lists:usort([{refac_prettypr:format(refac_misc:reset_attrs(E1)),
 			 refac_prettypr:format(refac_misc:reset_attrs(E2))}
 			||{E1,E2}<-SubSt, refac_syntax:type(E1)/=variable])).


    
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                  %%
%%  Attach function call to each class member                       %%
%%                                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
attach_fun_call_to_range(_C = {Ranges, {Len,Freq}, AU}, Tabs) ->
    RangesWithFunCalls=[{Range, generate_fun_call_1(Range, AU, Tabs, from_same_file(Ranges))} 
			|| Range <- Ranges],
    {RangesWithFunCalls, {Len, Freq}, 
     simplify_anti_unifier(AU)}.

generate_fun_call_1(Range, AUForm, Tabs, FromSameFile) ->
    Exprs=get_expr_list(Range, Tabs#tabs.tmp_ast_tab),
    AUFunClause=hd(refac_syntax:function_clauses(AUForm)),
    Pats = refac_syntax:clause_patterns(AUFunClause),
    AUBody = refac_syntax:clause_body(AUFunClause),
    try 
	%% it would be a bug if this does not match. 
	{true, Subst} = 
	    case length(AUBody) - length(Exprs) of 
		1 ->
		    SubAUBody = lists:sublist(AUBody, 1, length(AUBody) - 1),
		    Res=unification:expr_unification_extended(SubAUBody, Exprs),
		    case Res of 
			false -> 
			    ?wrangler_io("\n not same length\n",[]),
			    ?wrangler_io(" Case1:\n",[]),
			    ?wrangler_io("\nAUBody:\n",[]),
			    ?wrangler_io("~s", [refac_prettypr:format(refac_syntax:block_expr(AUBody))]),
			    ?wrangler_io("\nExprs:\n",[]),		   
			    ?wrangler_io("~s", [refac_prettypr:format(refac_syntax:block_expr(Exprs))]),
			    Res;
			_ -> Res
		end;
		0 ->
		Res=unification:expr_unification_extended(AUBody, Exprs),
		    case Res of
			false ->
			    ?wrangler_io("\nsame length\n",[]),
			    ?wrangler_io("\nAUBody:\n",[]),
			    ?wrangler_io("~s", [refac_prettypr:format(refac_syntax:block_expr(AUBody))]),
			    ?wrangler_io("\nExprs:\n",[]),		   
			    ?wrangler_io("~s", [refac_prettypr:format(refac_syntax:block_expr(Exprs))]),
			    Res;
			_ -> Res
		    end
	    end,
	%% Need to check side-effect here. but it is a bit slow!!!
	FunCall=make_fun_call(new_fun, Pats, Subst, FromSameFile),
	refac_prettypr:format(FunCall)
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
						  case lists:keysearch(fun_def,1, As) of
						      {value, {fun_def, {M, _F, A, _, _}}} ->
							  case M== erlang orelse M=='_' of 
							      true ->
								  Par;
							      false ->
								  Mod =refac_syntax:atom(M),
								  ModQualifier =refac_syntax:module_qualifier(Mod, Par),
								  refac_syntax:implicit_fun(ModQualifier, refac_syntax:integer(A))
							  end;
						      _ -> Par
						  end
					  end;
				      module_qualifier ->
					  As = refac_syntax:get_ann(Par),
					  case lists:keysearch(fun_def,1, As) of
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
    refac_misc:reset_attrs(refac_syntax:application(Op, [P|| P <- Pars])).

	 

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


get_anti_unifier(Ranges, Tabs, {ExprKeys, SubSt, ExportVars}) ->
    TmpASTTab=Tabs#tabs.tmp_ast_tab,
    Exprs1 = [ExpAST || {ExprKey, _,_,_} <- ExprKeys, {_Key, ExpAST} <- ets:lookup(TmpASTTab, ExprKey)],
    AU=anti_unification:generate_anti_unifier(Exprs1, SubSt, ExportVars),
    case from_same_file(Ranges) of
	true ->
	    AU;
	_ -> 
	    post_process_anti_unifier(AU)
    end.
 
from_same_file(Ranges) ->   
    Files = [element(1,element(1,hd(Rs)))||Rs<-Ranges],
    length(lists:usort(Files)) ==1.

post_process_anti_unifier(FunAST) ->
    {FunAST1, _} = ast_traverse_api:stop_tdTP(fun do_post_process_anti_unifier/2, FunAST, none),
    FunAST1. 

do_post_process_anti_unifier(Node, _Others) ->
    case refac_syntax:type(Node) of
	application ->
	    Operator = refac_syntax:application_operator(Node),
	    Arguments =refac_syntax:application_arguments(Node),
	    case refac_syntax:type(Operator) of
		atom ->
		    As = refac_syntax:get_ann(Operator),
		    {value, {fun_def, {M, _F, _A, _, _}}} = lists:keysearch(fun_def,1,As),
		    case M== erlang orelse M=='_' of 
			true ->
			    {Node, false};
			false ->
			    Mod = refac_syntax:atom(M),
			    Operator1 = refac_misc:rewrite(Operator, refac_syntax:module_qualifier(Mod, Operator)),
			    Node1 = refac_misc:rewrite(Node, refac_syntax:application(Operator1, Arguments)),
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
    %% ?debug("Ranges:\n~p\n", [Ranges]),
    %% ?debug("RangesWithoutFunCalls:\n~p\n",[RangesWithoutFunCalls]),
    {RangesWithoutFunCalls, {Len, Freq}, AntiUnifier,StartEndLocsWithFunCall}.

get_expr_list(ExprKeys=[{{_FName, _FunName, _Arity, _Index}, _Toks, _, _IsNew}|_T], ASTTab)->
    [ExpAST||{ExprKey,_,_,_}<-ExprKeys, {_Key, ExpAST}<-ets:lookup(ASTTab, ExprKey)].
 

get_vars_to_export(Es, {FName, FunName, Arity}, VarTab) ->
    AllVars = ets:lookup(VarTab, {FName, FunName, Arity}),
    {_, EndLoc} = refac_misc:get_start_end_loc(lists:last(Es)),
    case AllVars of
	  [] -> [];
	  [{_, _, Vars}] ->
	      ExprBdVarsPos = [Pos || {_Var, Pos} <- refac_misc:get_bound_vars(Es)],
	      [{V, DefPos} || {V, SourcePos, DefPos} <- Vars,
			    SourcePos > EndLoc,
			      lists:subtract(DefPos, ExprBdVarsPos) == []]
      end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                            %%
%%        Search for cloned candidates                        %%
%%                                                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
search_for_clones(Dir, Data, Thresholds) ->
    MinLen = Thresholds#threshold.min_len,
    MinToks= Thresholds#threshold.min_toks,
    MinFreq= Thresholds#threshold.min_freq,
    F0 = fun (I) ->
		 case is_integer(I) of
		     true -> integer_to_list(I) ++ ",";
		     false -> atom_to_list(I)
		 end
	 end,
    F = fun ({Elem, I}) ->
		lists:duplicate(length(F0(I)), {I, Elem})
	end,
    IndexStr = lists:append([F0(I) || {_, I} <- Data]),
    SuffixTreeExec = filename:join(?WRANGLER_DIR, "bin/suffixtree"),
    Cs = suffix_tree:get_clones_by_suffix_tree(Dir, IndexStr ++ "&", MinLen, 
					       MinFreq, "0123456789,#&", 1, SuffixTreeExec),
    Cs1 = lists:append([strip_a_clone({[{S, E}| Ranges], {Len, Freq}}, SubStr, MinLen, MinFreq)
			|| {[{S, E}| Ranges], Len, Freq} <- Cs,
			   SubStr <- [lists:sublist(IndexStr, S, E - S + 1)]]),
    Cs2 = refac_code_search_utils:remove_sub_clones([{R, Len, Freq} || {R, {Len, Freq}} <- Cs1]),
    NewData = lists:append([F(Elem) || Elem <- Data]),
    get_clones_in_ranges([{R, {Len, Freq}} || {R, Len, Freq} <- Cs2],
			 NewData, MinLen, MinToks, MinFreq).
   
%% This is necessary when the min_len is very small, e.g 1.
remove_duplicates_in_ranges({Ranges, {Len, _Freq}}) ->
    Ranges1=sets:to_list(sets:from_list(Ranges)),
    Freq1 = length(Ranges1),
    {Ranges1, {Len, Freq1}}.

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
			_ -> [{NewRanges1, {Len1, NewFreq}} | 
			      split_a_clone({RemainedRanges, {Len, F}}, Str22, MinLen,MinFreq)]
		    end;
		false when Str22==""->
		    [];
		false ->
		    split_a_clone({RemainedRanges, {Len, F}}, Str22, MinLen,MinFreq)
	    end;
	false when Str22==""->
	    [];
	false->split_a_clone({RemainedRanges, {Len, F}}, Str22, MinLen,MinFreq)
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
get_clones_in_ranges(Cs, Data, _MinLen, MinToks, MinFreq) ->
    F0 = fun ({S, E}) ->
		 {_, Ranges} = lists:unzip(lists:sublist(Data, S, E - S + 1)),
		 refac_misc:remove_duplicates(Ranges)
	 end,
    F = fun ({Ranges, {_Len, Freq}}) ->
		case Freq >= MinFreq of
		  true ->
			NewRanges = [F0(R) || R <- Ranges],
			remove_duplicates_in_ranges({NewRanges, {length(hd(NewRanges)), Freq}});
		    _ -> []
		end
	end,
    NewCs=[F(C) || C <- Cs],
    remove_short_clones(NewCs, MinToks,MinFreq).
    
  
remove_short_clones(Cs, MinToks, MinFreq) ->
    F= fun({Rs, {Len, _Freq}}) ->
	       Rs1=[R||R<-Rs, NumToks<-[[element(2, Elem)||Elem<-R]],
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

no_of_tokens(Node) when is_list(Node)->
    Str = refac_prettypr:format(refac_syntax:block_expr(Node)),
    {ok, Toks,_}=refac_scan:string(Str, {1,1}, 8, unix),
    length(Toks)-2;
no_of_tokens(Node) ->
    Str = refac_prettypr:format(Node),
    {ok, Toks,_} =refac_scan:string(Str, {1,1}, 8, unix),
    length(Toks).

combine_clones_by_au([]) ->[];
combine_clones_by_au(Cs=[{_Ranges, _Len, _F, _Code}|_T]) ->
    Cs1 =refac_misc:group_by(4, Cs),
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
    refac_prettypr:format(NewAU).
    
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
     {ok, Ts1, _} = erl_scan:string(refac_prettypr:format(Expr1)),
     {ok, Ts2, _} = erl_scan:string(refac_prettypr:format(Expr2)),
     refac_util:concat_toks(Ts1)==refac_util:concat_toks(Ts2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                            %%
%%       between Ets and Dets                                 %%
%%                                                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
from_dets(Ets, Dets) when is_atom(Ets) ->
    EtsRef = ets:new(Ets, [set, public]),
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
    MinToks = get_parameters_1(MinToks1,?MIN_TOKS, ?MIN_TOKS), 
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
		  V2 -> V2;
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
      V -> V;
      _:_ -> throw({error, "Parameter input is invalid."})
    end.
    

%% refac_inc_sim_code:inc_sim_code_detection(["c:/cygwin/home/hl/suites/bearer_handling_and_qos/test"],"5", "20", "2", "5","0.8",[],8).
%% refac_inc_sim_code:inc_sim_code_detection(["c:/cygwin/home/hl/test/ch2.erl"],3, 0, 2, 5,0.8,[],8).
