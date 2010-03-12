%% =============================================================================================

%% This module makes use of functionalities from TypEr and Dialyzer to get some type information
%% for an Erlang module.
%% =============================================================================================
-module(refac_type_info).

-export([get_type_info_using_typer/1]).

-include("../include/wrangler.hrl").

-record(tmpAcc, {file,		
		 module,		
		 funcAcc=[],	
		 incFuncAcc=[],	
		 dialyzerObj=[]}).

get_type_info_using_typer(File) ->
    PLT = filename:join(?WRANGLER_DIR, "plt/dialyzer_plt"),
    Analysis = #typer_analysis{plt=PLT},
    Analysis1 = Analysis#typer_analysis{ana_files = [File]},
    Analysis2 = collect(Analysis1),
    TypeInfo = get_type_info(Analysis2),
    ?debug("FinalFiles1:\n~p\n", [TypeInfo#typer_analysis.final_files]),
    Fun = fun({F, Module}) -> 
		  RecMap=lookup(F, TypeInfo#typer_analysis.record),
		  TypeInfoPlt = TypeInfo#typer_analysis.trust_plt,
		  case dialyzer_plt:lookup_module(TypeInfoPlt, Module) of
		      none -> [];
		      {value, List} -> {F, List, RecMap}
		  end
	  end,
    lists:map(Fun, TypeInfo#typer_analysis.final_files).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  The following functions are from Typer with slight modifications.         %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-spec get_type_info(#typer_analysis{}) -> #typer_analysis{}.

get_type_info(#typer_analysis{callgraph = CallGraph,
			      trust_plt = TrustPLT,
			      code_server = CodeServer} = Analysis) ->
    {StrippedCG0, _Ext} = dialyzer_callgraph:remove_external(CallGraph),
    StrippedCallGraph =  dialyzer_callgraph:finalize(StrippedCG0),
    try 
	NewPlt = dialyzer_succ_typings:analyze_callgraph(StrippedCallGraph, 
							 TrustPLT, CodeServer),
	Analysis#typer_analysis{callgraph = StrippedCallGraph, trust_plt = NewPlt}
    catch
	_E1:_E2 ->
	    throw({error, "Wrangler failed to infer the type information needed."})
    end.


-spec collect(#typer_analysis{}) -> #typer_analysis{}.
collect(Analysis) ->
    NewPlt =dialyzer_plt:merge_plts([Analysis#typer_analysis.trust_plt, 
				     Analysis#typer_analysis.plt]),
    NewAnalysis = lists:foldl(fun collect_one_file_info/2, 
			      Analysis#typer_analysis{trust_plt = NewPlt}, 
			      Analysis#typer_analysis.ana_files),
    %% Process Remote Types
    TmpCServer = NewAnalysis#typer_analysis.code_server,
    NewCServer =
	try
	    NewRecords = dialyzer_codeserver:get_temp_records(TmpCServer),
	    OldRecords = dialyzer_plt:get_types(NewPlt),
	    MergedRecords = dialyzer_utils:merge_records(NewRecords, OldRecords),
	    TmpCServer1 = dialyzer_codeserver:set_temp_records(MergedRecords, TmpCServer),
	    TmpCServer2 = dialyzer_utils:process_record_remote_types(TmpCServer1),
	    dialyzer_contracts:process_contract_remote_types(TmpCServer2)
	catch
	    throw:{error, ErrorMsg} ->
		throw({error, ErrorMsg})
	end,
    NewAnalysis#typer_analysis{code_server = NewCServer}.

collect_one_file_info(File, Analysis) ->
    ?debug("File:\n~p\n", [File]),
    Ds = [{d,Name,Val} || {Name,Val} <- Analysis#typer_analysis.macros],
    %% Current directory should also be included in "Includes".
    Includes = [filename:dirname(File)|Analysis#typer_analysis.includes],
    Is = [{i,Dir} || Dir <- Includes],
    Options = dialyzer_utils:src_compiler_opts() ++ Is ++ Ds,
    case dialyzer_utils:get_abstract_code_from_src(File, Options) of
	{error, Reason} ->
	    throw({error, Reason});
	{ok, AbstractCode} ->
	    case dialyzer_utils:get_core_from_abstract_code(AbstractCode, Options) of
		error -> throw({error, "Could not get core erlang for "++File});
		{ok, Core} ->
		    case dialyzer_utils:get_record_and_type_info(AbstractCode) of
			{error, Reason} -> throw({error, Reason});
			{ok, Records} -> 
			    analyze_core_tree(Core, Records, dict:new(), Analysis, File)
		    end
	    end
    end.

analyze_core_tree(Core, Records, SpecInfo, Analysis, File) ->
  Module = list_to_atom(filename:basename(File, ".erl")),
  TmpTree = cerl:from_records(Core),
  CS1 = Analysis#typer_analysis.code_server,
  NextLabel = dialyzer_codeserver:get_next_core_label(CS1),
  {Tree, NewLabel} = cerl_trees:label(TmpTree, NextLabel),
  CS2 = dialyzer_codeserver:insert(Module, Tree, CS1),
  CS3 = dialyzer_codeserver:set_next_core_label(NewLabel, CS2),
  CS4 = dialyzer_codeserver:store_temp_records(Module, Records, CS3),
  CS5 = dialyzer_codeserver:store_temp_contracts(Module, SpecInfo, CS4),
  Ex_Funcs = [{0,F,A} || {_,_,{F,A}} <- cerl:module_exports(Tree)],
  TmpCG = Analysis#typer_analysis.callgraph,
  CG = dialyzer_callgraph:scan_core_tree(Tree, TmpCG),
  Fun = fun analyze_one_function/2,
   All_Defs = cerl:module_defs(Tree),
  Acc = lists:foldl(Fun, #tmpAcc{file=File, module=Module}, All_Defs),
  Exported_FuncMap = typer_map:insert({File, Ex_Funcs},
				      Analysis#typer_analysis.ex_func),

  Sorted_Functions = lists:keysort(1, Acc#tmpAcc.funcAcc),
  FuncMap = typer_map:insert({File, Sorted_Functions},
			     Analysis#typer_analysis.func),
  IncFuncMap = typer_map:insert({File, Acc#tmpAcc.incFuncAcc}, 
				Analysis#typer_analysis.inc_func),
  Final_Files = Analysis#typer_analysis.final_files ++ [{File, Module}],
    RecordMap = typer_map:insert({File, Records}, Analysis#typer_analysis.record),
  Analysis#typer_analysis{final_files=Final_Files,
			  callgraph=CG,
			  code_server=CS5,
			  ex_func=Exported_FuncMap,
			  inc_func=IncFuncMap,
			  record=RecordMap,
			  func=FuncMap}.

analyze_one_function({Var, FunBody} = Function, Acc) ->
    F = cerl:fname_id(Var),
    A = cerl:fname_arity(Var),
    TmpDialyzerObj = {{Acc#tmpAcc.module, F, A}, Function},
    NewDialyzerObj = Acc#tmpAcc.dialyzerObj ++ [TmpDialyzerObj],
    [_, LineNo, {file, FileName}] = cerl:get_ann(FunBody),
    BaseName = filename:basename(FileName),
    FuncInfo = {LineNo, F, A},
    OriginalName = Acc#tmpAcc.file,
    {FuncAcc, IncFuncAcc} =
	case FileName =:= OriginalName orelse BaseName =:= OriginalName of
	    true ->
		{Acc#tmpAcc.funcAcc ++ [FuncInfo], Acc#tmpAcc.incFuncAcc};
	    false ->
		{Acc#tmpAcc.funcAcc, Acc#tmpAcc.incFuncAcc ++ [{FileName, FuncInfo}]}
	end,
    Acc#tmpAcc{funcAcc = FuncAcc,
	       incFuncAcc = IncFuncAcc,
	       dialyzerObj = NewDialyzerObj}.

lookup(Key, Dict) ->
  try dict:fetch(Key, Dict) catch error:_ -> none end.
