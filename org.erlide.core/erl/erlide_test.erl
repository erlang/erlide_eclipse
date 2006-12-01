-module(erlide_test).
%% hej och hå

-compile(export_all).

-define(Z, "xxxxx\045\045gggg").
-define(QZ(Y), Y).

func(X) when is_integer(X) ->
	?Z, $\n,
	aaa;
func(X) ->
	?QZ(555) + X.

tt() ->
	spawn(fun() -> ttt() end).

ttt() ->
	error_logger:error_msg("#1"),

	dbg:tracer(),
	dbg:p(rex),

	ok.

