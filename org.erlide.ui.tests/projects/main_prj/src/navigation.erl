-module(navigation).
-export([fff/0]).

%% Instructions:
%% try all combinations of defines and check if navigation and autocompletion
%% work as expected

%% TODO expand this to separate tests, mossibly in different files
%% we aim to automate these
   
-define(mmm, navigation). 
%-define(mmm, ?MODULE).
 
-record(navigation, {aaa, bbb, ccc}).
%-record(?mmm, {aaa, bbb, ccc}).
%-record(?MODULE, {aaa, bbb, ccc}).

fff() ->
	x:g(),
	
       ?mmm:fff(),
       ?MODULE:fff(),

	   #navigation{aaa=3},
       #?mmm{aaa=3},
       #?MODULE{bbb=3},

       ok.
