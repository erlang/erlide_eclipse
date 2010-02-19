%% Author: jakob
%% Created: 17 feb 2010
%% Description: TODO: Add description to erlide_dialyze
-module(erlide_dialyze).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([dialyze/2]).

%%
%% API Functions
%%

dialyze(Files, Plt) ->
    dialyzer:run([{files_rec, Files}, 
		  {init_plt, Plt}, 
		  {check_plt, false}]).

%%
%% Local Functions
%%

