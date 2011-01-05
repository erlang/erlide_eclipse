%% Author: Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
%% Created: 22-12-2010
%% Description: coverage files
%%----------------------------------------------
-module(coverage).

%%----------------------------------------------
%% Include files
%%----------------------------------------------
-include("coverage.hrl").


%%----------------------------------------------
%% Exported Functions
%%----------------------------------------------
-export([compile/2, compile_dir/1, prepare/3, create_report/2]).

%%----------------------------------------------
%% API Functions
%%----------------------------------------------

%compile module
compile(Module, Path) ->
	io:format("inside prepare~n"),
	erlide_jrpc:event(?EVENT, {Module, Path}),
	case cover:compile(Path) of		%%TODO: include files	
			{ok, _M} -> 
				io:format("compilation ok~n"),
				ok;
			{error, Err} -> 
				io:format("~n~p", [Err]),
				erlide_jrpc:event(?EVENT, #cover_error{place = Module, 
													   type = compiling,
													   info = Err}),
				{error, compilation}
	end.

%compile directory
compile_dir(Dir) ->
	case cover:compile_directory(Dir) of
		{error, Reason} ->
			erlide_jrpc:event(?EVENT, #cover_error{place = Dir,
												   type = compiling,
												   info = Reason}),
			{error, compilation};
		Res -> {ok, Res}
	end.

%prepare module
prepare(eunit, Module, _Path) ->
	io:format("inside prepare~n"),
	erlide_jrpc:event(?EVENT, {Module, eunit}),
	case eunit:test(Module) of
			ok ->
				erlide_jrpc:event(?EVENT, {Module, test_ok}),
				ok;
			Er ->
				erlide_jrpc:event(?EVENT, #cover_error{place = Module,
													   type = testing,
													   info = Er}),
				{error, testing}
	end.

%creates report
create_report(_Dir, Module) ->
	ModRes = cover:analyse(Module, module),
	FunRes = cover:analyse(Module, function),
	LineRes = cover:analyse(Module, calls, line), %%calls!
	io:format("~p~n~p~n~p~n", [ModRes, FunRes, LineRes]),
	
	erlide_jrpc:event(?EVENT, {Module, ModRes, FunRes}),
	case { ModRes, FunRes, LineRes} of
		{{ok, _}, {ok, _}, {ok, _}} ->
			Res = prepare_result(ModRes, FunRes, LineRes, Module),
			{ok, Res};
		_ -> 
			{error, analyse}
	end.
			
	

%%----------------------------------------------
%% Local Functions
%%----------------------------------------------


prepare_result(ModRes, FunRes, LineRes, Module) ->
	FileHtml = filename:join([".",  "mod_" ++ atom_to_list(Module) ++ ".html"]),
    ResHtml = cover:analyse_to_file(Module, FileHtml, [html]),
    %%filename:basename(FileHtml), %%
	Out = case ResHtml of
			{ok, OutFile} -> OutFile;
			{error, _} -> ?NO_FILE
		  end,
			
	
	%% functions
	FunList = get_fun_stats(FunRes),
	%% lines
	LineList = get_line_stats(LineRes),
	
	%%module
	{ok, {Name, {Cov, Uncov}}} = ModRes,
	Result = #module_res{name = Name,
				name_html = filename:absname(Out),
				line_num = Cov + Uncov,
				covered_num = Cov,
				percentage = count_percent(Cov, Cov + Uncov),
				functions = FunList,
				lines = LineList},
	io:format("~p~n", [Result]),
	Result.

count_percent(Covered, Total) ->
	Covered/Total * 100.


get_fun_stats({ok, FunRes}) ->
	lists:map(fun({{_, Name, Arity}, {Cov, Uncov}}) ->
								#unit_res{name = Name,
										  arity = Arity,
										  total_l = Cov + Uncov,
										  covered_l = Cov,
										  percentage = count_percent(Cov, Cov + Uncov)
										  }
								end,
								FunRes).

get_line_stats({ok, Calls}) ->
	
	lists:map(fun({{_, No}, CallNum}) ->
					#line_res{num = No, calls = CallNum}
					end,
					Calls).

