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
-export([prepare/3, create_report/2]).

%%----------------------------------------------
%% API Functions
%%----------------------------------------------

%prepare module
prepare(eunit, Module, Path) ->
	io:format("inside prepare~n"),
	ok = case cover:compile(Path) of		%%TODO: include files	
				{ok, M} -> 
					io:format("compilation ok~n"),
					ok;
				{error, Err} -> 
					io:format("~n~p", [Err]),
					erlide_jrpc:event(?EVENT, {?ERROR, compilation}),
					error
			end, % whar really should be done in backend?
	io:format("2"),
	case eunit:test(Module) of
			ok ->
				ok;
			Er ->
				erlide_jrpc:event(?EVENT, {?ERROR, testing, Er}),
				{error, failed_tests}
	end.

%creates report
create_report(Dir, Module) ->
	ModRes = cover:analyse(Module, module),
	FunRes = cover:analyse(Module, function),
	LineRes = cover:analyse(Module, line),
	LineResCalls = cover:analyse(Module, calls, line),
	io:format("~p~n~p~n~p~n~p~n", [ModRes, FunRes, LineRes, LineResCalls]),
	
	ok = case { ModRes, FunRes, LineRes, LineResCalls } of
		{{ok, _}, {ok, _}, {ok, _}, {ok, _}} ->
			ok;
		_ -> 
			erlide_jrpc:event(?EVENT, {?ERROR, analyse}),
			error
	end,
			
	FileHtml = filename:join([".",  "mod_" ++ atom_to_list(Module) ++ ".html"]),
    Res = cover:analyse_to_file(Module, FileHtml, [html]),
    filename:basename(FileHtml), %%
	
	
	
	ok.

%%----------------------------------------------
%% Local Functions
%%----------------------------------------------

percentage(Covered, Total) ->
	Covered/Total * 100.


