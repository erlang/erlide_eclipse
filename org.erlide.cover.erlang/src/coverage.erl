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
-export([compile/3,
		 compile_dir/1,
		 prepare/2,
		 create_report/2,
		 create_index/2]).

%%----------------------------------------------
%% API Functions
%%----------------------------------------------

%compile module
compile(Module, Path, Includes) ->
	IOptions = [{i, Dir} || Dir <- Includes],
	Options = [{d, 'TEST'} | IOptions],
	case cover:compile(Path, Options) of		%%TODO: include files	
			{ok, _M} -> 
				ok;
			{error, Err} -> 
				{error, #cover_error{place = Module, 
									 type = compiling,
									 info = Err}}
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
prepare(eunit, Arg) ->	
	case eunit:test(Arg, [{report, {test_listener, []}}]) of
			ok ->
				erlide_jrpc:event(?EVENT, {Arg, test_ok});
			_Err ->
				erlide_jrpc:event(?EVENT, {Arg, test_failed})
	end.

%creates html report
create_report(Modules, ReportDir) when is_list(Modules) ->
	io:format("~p~n", [Modules]),
	lists:foldl(fun (Module,Acc) ->
			io:format("~p~n", [Module]),
			Mod = if
					  is_list(Module) ->
						  list_to_atom(Module);
					  true ->
						  Module
				  end,
			Res = create_report(Mod, ReportDir), 
			case Res of
			  {ok,Result} ->
			      erlide_jrpc:event(?EVENT,Result), 
			      [Result| Acc];
			  {error,Reason} ->
			      erlide_jrpc:event(?EVENT,
						#cover_error{place = Mod, 
							     type = 'creating report', 
							     info = Reason}), 
				  io:format("error: ~p~n", [Reason]),
			      Acc
			end
		end,[],Modules);

create_report(Module, ReportDir) ->
    io:format("~p~n", [Module]), 
    ModRes = cover:analyse(Module,module), 
    FunRes = cover:analyse(Module,function), 
    LineRes = cover:analyse(Module,calls,line),  %%calls! 
    case {ModRes,FunRes,LineRes} of
      {{ok,_},{ok,_},{ok,_}} ->
	  		Res = prepare_result(ModRes,FunRes,LineRes,Module, ReportDir), 
	  		{ok,Res};
      Error ->
		  	io:format("~p~n", [Error]),
			{error,analyse}
    end.

% create index.html file
create_index(Results, ReportDir) ->
	IndexPath = filename:join([ReportDir,  "index.html"]),
	case filelib:ensure_dir(IndexPath) of
		{error, Res} ->
			erlide_jrpc:event(?EVENT, #cover_error{type = 'creating index',
							  	info = Res}),
		   	no_file;
		_ -> 
			Total_percent = percentage_total(Results),
			case output_index(IndexPath, Results, Total_percent) of
				ok ->
					filename:absname(IndexPath);
				no_file ->
					no_file
			end
	end.
			

%%----------------------------------------------
%% Local Functions
%%----------------------------------------------


prepare_result(ModRes, FunRes, LineRes, Module, ReportDir) ->
	FileHtml = filename:join([ReportDir,  "mod_" ++ atom_to_list(Module) ++ ".html"]),
	filelib:ensure_dir(FileHtml),		%TODO: error handling
    ResHtml = cover:analyse_to_file(Module, FileHtml, [html]),
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
    case file:open(Path, [write]) of
           {ok, IoDevice}       -> 
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
   				file:close(IoDevice),
				ok;
           {error, Reason} -> 
				erlide_jrpc:event(?EVENT, #cover_error{type = 'creating index',
				info = Reason}),
				no_file
    end.

output_header(_IoDevice) ->
    "<html>~n<head></head><body>".

output_footer(_IoDevice) ->
    "</body>~n</html>~n".


percentage_total(Results) when length(Results) > 0 ->
    {Covered, All} = 
		lists:foldl(
          fun(#module_res{covered_num = C, line_num = A}, {Covered, All}) ->
                  {Covered + C, All + A}
          end, {0, 0}, Results),
    case All of
        0 -> 0.0;
        _ -> 100 * Covered / All
    end;
percentage_total(_Results)  ->
	0.0.


