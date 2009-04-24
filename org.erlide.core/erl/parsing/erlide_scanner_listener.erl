-module(erlide_scanner_listener).

-export([
		 start/0
		]).

start() ->
	Pid = spawn(fun()->loop([]) end),
	erlide_log:logp({scanner_listener, Pid}),
	register(?MODULE, Pid),
	erlide_log:logp({scanner_listener, ?MODULE}),
	ok.

loop(L) ->
	receive
		stop ->
			ok;
		{change, _Module, _Offset, _Length, _Text} = Msg ->
			erlide_log:logp({scanner_listener, Msg}),
			loop(aggregate(Msg, L));
		{new, _Module} = Msg ->
			erlide_log:logp({scanner_listener, Msg}),
			loop(L);
		Msg ->
			erlide_log:logp({scanner_listener, unknown, Msg}),
			loop(L)
	
		after 600 ->
			case L of 
				[] -> 
					ok;
				_ ->
					[erlide_log:logp({reconcile, X}) || X <- lists:reverse(L)]
			end,
			loop([])
	end.

aggregate(Msg={change, Module, Offset, 0, Text}, L=[{change, Module, Offset2, 0, Text2}|T]) ->
	case Offset2+length(Text2) of
		Offset ->
			[{change, Module, Offset2, 0, Text2++Text}|T];
		_ ->
			[Msg|L]
	end;
aggregate(Msg, L) ->
	[Msg|L].

