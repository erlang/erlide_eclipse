-module(start).

-compile(export_all).

%%% connect to slave
setup() ->
	Peer = 'remote_debug_slave@127.0.0.1',
	net_adm:ping(Peer),
	ok.

run() ->
	hello, 
	demo(),
	[rpc:call(N, ?MODULE, demo, []) || N<-nodes()].


%% hello world
-spec demo() -> term().
demo() ->
	{node(), hello}.

