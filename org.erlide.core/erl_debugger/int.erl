%% Author: jakob
%% Created: 21 aug 2008
-module(int).
-author(jakobce@gmail.com).

%% Since the error_handler (in R12 and earlier) hard-codes the int
%% module, we replace it to call erlide_int
%% this should be improved in R13

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([eval/3]).

%%
%% API Functions
%%

eval(Mod, Func, Args) -> 
	erlide_int:eval(Mod, Func, Args).

%%
%% Local Functions
%%

