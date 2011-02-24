%% Author: jakob
%% Created: 17 feb 2010
%% Description: TODO: Add description to erlide_dialyze
-module(erlide_dialyze).

%%
%% Include files
%%

%% -define(DEBUG, 1).

-include("erlide.hrl").

%%
%% Exported Functions
%%
-export([dialyze/4, format_warning/1, check_plt/1, get_plt_files/1]).

%%
%% API Functions
%%

dialyze(Files, Plts, Includes, FromSource) ->
    ?D([Files, Plts, Includes]),
    From = case FromSource of
	       true -> src_code;
	       false -> byte_code
	   end,
    PltOption = case Plts of
                    [Plt] ->
                        {init_plt, Plt};
                    _ ->
                        {plts, Plts}
                end,
    case catch dialyzer:run([{files_rec, Files}, 
			     PltOption, 
			     {check_plt, false},
			     {from, From},
			     {include_dirs, Includes}]) of
	{_ErrorOrExit, E} ->
	    {error, flat(E)};
	Result ->
	    Result
    end.

format_warning(Msg) ->
    dialyzer:format_warning(Msg).

check_plt(Plt) ->
    dialyzer:run([{analysis_type, plt_check},
		  {init_plt, Plt}]).

get_plt_files(PltFiles) ->
    Files = string:tokens(PltFiles, ","),
    case get_plt_files(Files, []) of
		[L | _]=R when is_list(L) ->
			{ok, R};
		_ ->
			no_files_found
	end.

%%
%% Local Functions
%%

flat({{dialyzer_error, E}, _}) ->
    flat(E);
flat({dialyzer_error, E}) ->
    flat(E);
flat(L) ->
    lists:flatten(L).

get_plt_files([], Acc) -> 
    lists:reverse(Acc);
get_plt_files([File | Rest], Acc) ->
    case filename:extension(File) of
        ".plt" ->
            get_plt_files(Rest, [File | Acc]);
        _ ->
            case file:read_file(File) of
                {ok, B} ->
                    L = erlide_util:split_lines(binary_to_list(B)),
                    get_plt_files(L ++ Rest, Acc);
                _ ->
                    get_plt_files(Rest, [File | Acc])
            end
    end.


        
        
        