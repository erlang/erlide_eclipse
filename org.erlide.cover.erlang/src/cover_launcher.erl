%% Author: wirenth
%% Created: 08-11-2010
%% Description: TODO: Add description to cover_launcher
-module(cover_launcher).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start/3]).


-define(EVENT, cover_event).
-define(OK, cover_ok).
-define(ERROR, cover_error).
-define(COVER_DIR, "cover_report").

%%
%% API Functions
%%
start(Type, Module, SrcDir) ->
	TypeAtom = list_to_atom(Type),
	erlide_jrpc:event(?EVENT, ok),
	cover(TypeAtom, Module, SrcDir),
	init:stop(0).




%%
%% Local Functions
%%

%%TODO directory & application version
cover(module, Module, SrcDir) ->
	WithExt = Module ++ ".erl",
	Path = filename:join(SrcDir, WithExt),
	erlide_jrpc:event(?EVENT, [Path, Module]),
	case cover:compile(Path) of		%%TODO: include files	
				{ok, M} -> erlide_jrpc:event(?EVENT, M);
				{error, Err} -> 
					erlide_jrpc:event(?EVENT, ?ERROR),
					init:stop(1)
			 end,
	
	Module1 = list_to_atom(Module),
	
	erlide_jrpc:event(?EVENT, "unit testing..."),
	Res = case eunit:test(Module1) of
				ok ->
					create_report(?COVER_DIR, [Module1]);
				Er ->
					erlide_jrpc:event(?EVENT, Er),
					{error, failed_tests}
			 end,
	case Res of
		{ok, Report} ->
			erlide_jrpc:event(?EVENT, {?OK, Report}),
			ok;
		{error, Reason} ->
			erlide_jrpc:event(?EVENT, ?ERROR),
			{error, Reason}
	end.

create_report(Path, Modules) ->
	erlide_jrpc:event(?EVENT, "creating report"),
	Results = lists:foldl(fun(Module, Res) ->
                                case {cover:analyse(Module, module), is_list(Res)} of
                                    {_, false} -> 
										erlide_jrpc:event(?EVENT, "bla"),
										Res;
									{{ok, Result}, _}    -> 
										erlide_jrpc:event(?EVENT, Result),
										[Result | Res];
                                    {{error, Reason}, _} -> 
										erlide_jrpc:event(?EVENT, Reason),
										{invalid_module, Reason}
                                end
                        	end,
						  	[],
						    Modules),
	case Results of
		{invalid_module, Reason} ->
			{error, {invalid_module, Reason}};
		_ ->
			lists:map(fun(Module) ->
                              File = filename:join([".",  "mod_" ++ atom_to_list(Module) ++ ".html"]),
							  erlide_jrpc:event(?EVENT, File),
                              Res = cover:analyse_to_file(Module, File, [html]),
							  erlide_jrpc:event(?EVENT, Res),
                              filename:basename(File)
                      end, Modules),
			{ok, 
	 		{{total, percentage_total(Results)},
	  		{per_file, lists:zip(Modules, percentage_per_file(Results))},
	  		{all, Results}}}
	end.
	

percentage_total(Results) ->
    {Covered, Total} =
        lists:foldl(
          fun({_Module, {C, NC}}, {Covered, Total}) ->
                  {Covered + C, Total + C + NC}
          end, {0, 0}, Results),
    case Total of
        0 -> 0;
        _ -> round(100 * Covered / Total)
    end.

percentage_per_file(Results) ->
    lists:map(fun
                ({_, {0, 0}}) -> 100;
                ({_Module, {Covered, NotCovered}}) ->
                      Total = Covered + NotCovered,
                      round(100 * Covered / Total)
              end, Results).



