-module(m11).

-export([f/0]).

-include("h11.hrl").
-include("h21.hrl").

f() ->
	m12:f(),
    m21:f(),
    ok.

