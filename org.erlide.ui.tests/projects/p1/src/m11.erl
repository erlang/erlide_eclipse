-module(m11).

-export([f/0]).

-include("h11.hrl").
-include("h12.hrl").
-include("h21.hrl"). 
-include("h31.hrl"). 

f() ->
	%% in project
	m12:f(), 
	?H11,
	?H12,

	%% in workspace
    m21:f(), 
	?H21,
	?H22, % not to be found!
	
	%% in externals
	m31:f(), 
	?H31,
	?H32, % not to be found!
	
	%% not found
	m41:f(),
	?H41,
	
    ok.

