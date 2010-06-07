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
-export([dialyze/4, format_warning/1, check_plt/1]).

%%
%% API Functions
%%

dialyze(Files, Plt, Includes, FromSource) ->
    ?D([Files, Plt, Includes]),
    From = case FromSource of
	       true -> src_code;
	       false -> byte_code
	   end,
    case catch dialyzer:run([{files_rec, Files}, 
			     {init_plt, Plt}, 
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

%%
%% Local Functions
%%

flat({{dialyzer_error, E}, _}) ->
    flat(E);
flat({dialyzer_error, E}) ->
    flat(E);
flat(L) ->
    lists:flatten(L).
