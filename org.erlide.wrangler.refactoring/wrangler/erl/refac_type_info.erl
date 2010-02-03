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
%%       names of its contributors may be used to endoorse or promote products
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
%% ============================================================================================
%% Refactoring: Introduce a ?LET.
%%
%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%% 
%% =============================================================================================

%% =============================================================================================
-module(refac_type_info).

-export([get_type_info_using_typer/1]).

-include("../include/wrangler.hrl").

-define(Msg, "Wrangler failed to infer the current data type of the state.").    
    
get_type_info_using_typer(File) ->
  Analysis = #typer_analysis{},
  Analysis1 = extract(Analysis),
  Analysis2 = Analysis1#typer_analysis{ana_files = [File]},
  Analysis3 = collect(Analysis2),
  Analysis4 = get_type_info(Analysis3),
  TypeInfoPlt = Analysis4#typer_analysis.trust_plt,
  Fun = fun({F, Module}) ->
	    case dialyzer_plt:lookup_module(TypeInfoPlt, Module) of
	      none -> [];
	      {value, List} -> {F, List}
	    end
	end,
  lists:map(Fun, Analysis4#typer_analysis.final_files).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  The following functions are from Typer with slight modifications.         %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extract(#typer_analysis{macros = _Macros, includes = Includes,
			t_files = TFiles, trust_plt = TrustPLT} = Analysis) ->
    Ds = [],  %% [{d, Name, Value} || {Name, Value} <- Macros],
    CodeServer = dialyzer_codeserver:new(),
    Fun =
	fun(File, CS) ->
	    AllIncludes = [filename:dirname(filename:dirname(File)) | Includes],
	    Is = [{i, Dir} || Dir <- AllIncludes],
	    CompOpts = dialyzer_utils:src_compiler_opts() ++ Is ++ Ds,
	    case dialyzer_utils:get_abstract_code_from_src(File, CompOpts) of
		{ok, AbstractCode} -> 
		    case dialyzer_utils:get_record_and_type_info(AbstractCode) of
			{ok, RecDict} ->
			    Mod = list_to_atom(filename:basename(File, ".erl")),
			    case dialyzer_utils:get_spec_info(Mod, AbstractCode, RecDict) of
				{ok, SpecDict} ->
				    CS1 = dialyzer_codeserver:store_temp_records(Mod, RecDict, CS),
				    dialyzer_codeserver:store_temp_contracts(Mod, SpecDict, CS1);
				{error, Reason} -> throw({error, Reason})
			    end;
			{error, Reason} -> throw({error, Reason})
		    end;
		{error, Reason} -> throw({error, Reason})
	    end
    end,
    CodeServer1 = lists:foldl(Fun, CodeServer, TFiles),
    NewCodeServer =
	try
	    NewRecords = dialyzer_codeserver:get_temp_records(CodeServer1),
	    OldRecords = dialyzer_plt:get_types(TrustPLT), % XXX change to the PLT?
	    MergedRecords = dialyzer_utils:merge_records(NewRecords, OldRecords),
	    CodeServer2 = dialyzer_codeserver:set_temp_records(MergedRecords, CodeServer1),
	    CodeServer3 = dialyzer_utils:process_record_remote_types(CodeServer2),
	    dialyzer_contracts:process_contract_remote_types(CodeServer3)
	catch
	    throw:{error, ErrorMsg} ->
		throw({error, ErrorMsg})
	end,
    Contracts = dialyzer_codeserver:get_contracts(NewCodeServer),
    Modules = dict:fetch_keys(Contracts),
    FoldFun =
	fun(Module, TmpPlt) ->
		{ok, ModuleContracts} = dict:find(Module, Contracts),
		SpecList = [{MFA, Contract} 
			    || {MFA, {_FileLine, Contract}} <- dict:to_list(ModuleContracts)],
		dialyzer_plt:insert_contract_list(TmpPlt, SpecList)
	end,
    NewTrustPLT = lists:foldl(FoldFun, TrustPLT, Modules),
    Analysis#typer_analysis{trust_plt = NewTrustPLT}.

%%--------------------------------------------------------------------

%%-spec get_type_info(#typer_analysis{}) -> #typer_analysis{}.

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
	    throw({error, "Wrangler failed to infer type information needed."})
    end.


-record(tmpAcc, {file,		
		 module,		
		 funcAcc=[],	
		 incFuncAcc=[],	
		 dialyzerObj=[]}).

%%-spec collect(#typer_analysis{}) -> #typer_analysis{}.
collect(Analysis) ->
  NewPlt =
	try get_dialyzer_plt(Analysis) of
	    DialyzerPlt ->
		dialyzer_plt:merge_plts([Analysis#typer_analysis.trust_plt, DialyzerPlt])
	catch
	    _E1:_E2 ->
		throw({error, "Dialyzer's PLT is missing or is not up-to-date; please (re)create it"})
	end,
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
			    Mod = list_to_atom(filename:basename(File, ".erl")),
			    case dialyzer_utils:get_spec_info(Mod, AbstractCode, Records) of
				{error, Reason} -> throw({error, Reason});
				{ok, SpecInfo} -> 
				    analyze_core_tree(Core, Records, SpecInfo, Analysis, File)
			    end
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
  %% NOTE: we must sort all functions in the file which
  %% originate from this file by *numerical order* of lineNo
  Sorted_Functions = lists:keysort(1, Acc#tmpAcc.funcAcc),
  FuncMap = typer_map:insert({File, Sorted_Functions},
			     Analysis#typer_analysis.func),
  %% NOTE: However we do not need to sort functions
  %% which are imported from included files.
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
	  true -> %% Coming from original file
	      %% io:format("Added function ~p\n", [{LineNo, F, A}]),
	      {Acc#tmpAcc.funcAcc ++ [FuncInfo], Acc#tmpAcc.incFuncAcc};
	  false ->
	      %% Coming from other sourses, including:
	      %%     -- .yrl (yecc-generated file)
	      %%     -- yeccpre.hrl (yecc-generated file)
	      %%     -- other cases
	      {Acc#tmpAcc.funcAcc, Acc#tmpAcc.incFuncAcc ++ [{FileName, FuncInfo}]}
	end,
    Acc#tmpAcc{funcAcc = FuncAcc,
	       incFuncAcc = IncFuncAcc,
	       dialyzerObj = NewDialyzerObj}.

get_dialyzer_plt(#typer_analysis{plt =_PltFile0}) ->
    PltFile = dialyzer_plt:get_default_plt(),
	%% case PltFile0 =:= none of
	%%     true -> dialyzer_plt:get_default_plt();
	%%     false -> PltFile0
	%% end,
    dialyzer_plt:from_file(PltFile).


