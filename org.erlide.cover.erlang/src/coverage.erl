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
-export([compile/2,
		 compile_dir/1,
		 prepare/3,
		 create_report/2,
		 create_index/1]).

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

create_index(Results) ->
	IndexPath = filename:join([?COVER_DIR,  "index.html"]),
	case filelib:ensure_dir(IndexPath) of
		{error, Res} ->
			erlide_jrpc:event(?EVENT, #cover_error{type = 'creating index'},
							  	info = Res),
		   	#cover_error{};
		_ -> 
			Total_percent = percentage_total(Results),
			output_index(IndexPath, Results, Total_percent),
			filename:absname(IndexPath)
	end.
			
%create new index file - at the begining
%%new_index_file(IndexFile) ->
%%	filelib:ensure_dir(IndexFile),		%TODO: error handling
%%	IoDevice = case file:open(IndexFile, [write]) of
%%                   {ok, IoD}       -> IoD;
%%                   {error, Reason} -> exit({invalid_file, Reason}) %%!
%%               end,
%%    io:format(IoDevice, output_header(IoDevice), []),
%%	io:format(IoDevice, output_footer(IoDevice), []),
%%	file:close(IoDevice),
%%    ok.

%%----------------------------------------------
%% Local Functions
%%----------------------------------------------


prepare_result(ModRes, FunRes, LineRes, Module) ->
	FileHtml = filename:join([?COVER_DIR,  "mod_" ++ atom_to_list(Module) ++ ".html"]),
	filelib:ensure_dir(FileHtml),		%TODO: error handling
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

output_index(Path, Results, Total) ->
    IoDevice = case file:open(Path, [write]) of
                   {ok, IoD}       -> IoD;
                   {error, Reason} -> 
					   exit({invalid_file, Reason})
               end,
    io:format(IoDevice, output_header(IoDevice), []),
    lists:foreach(fun(#module_res{name = Module,
								  name_html = File, 
								  percentage = Percentage}) ->
                          io:format(IoDevice, "~s~n", [
                                                [ "<li><a href=\"",
                                                  File,
                                                  "\">",
                                                  atom_to_list(Module),
                                                  "</a> Covered: ",
                                                  io_lib:format("~.2f",[Percentage]),
                                                  "%",
                                                  "</a>"
                                                ]
                                               ])
                  end, lists:keysort(2, Results)),
    io:format(IoDevice, "~s~n", [["<p>Total percentage: ", io_lib:format("~.2f",[Total]), "%</p>"]]),
    io:format(IoDevice, output_footer(IoDevice), []),
    file:close(IoDevice).

output_header(IoDevice) ->
    "<html>~n<head></head><body>".

output_footer(IoDevice) ->
    "</body>~n</html>~n".


percentage_total(Results) ->
    {Covered, All} = 
		lists:foldl(
          fun(#module_res{covered_num = C, line_num = A}, {Covered, All}) ->
                  {Covered + C, All + A}
          end, {0, 0}, Results),
    case All of
        0 -> 0.0;
        _ -> 100 * Covered / All
    end.


%%previous_content(IndexFile) ->
%%	IODevice = case file:open(IndexFile, [read]) of
%%				   {ok, IoD}       -> IoD;
%%                   {error, Reason} -> exit({invalid_file, Reason}) %%!
%%               end,
%%	Data = case file:read().
