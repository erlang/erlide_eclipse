-module(mod).

-export([f/0]).

-include("hdr.hrl").

f() -> 
    4+5,
	ok.
