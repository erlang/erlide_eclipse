-module(m12).

-include("h31.hrl").


-export([f/0]).

f() ->
    m31:f(),
    ok.

