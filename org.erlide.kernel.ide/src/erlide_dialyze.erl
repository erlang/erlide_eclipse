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
-export([dialyze/3, format_warning/1]).

%%
%% API Functions
%%

dialyze(Files, Plt, Includes) ->
    ?D([Files, Plt, Includes]),
    catch dialyzer:run([{files_rec, Files}, 
			{init_plt, Plt}, 
			{check_plt, false},
			{from, src_code},
			{include_dirs, Includes}]).

format_warning(Msg) ->
    dialyzer:format_warning(Msg).

%%
%% Local Functions
%%

