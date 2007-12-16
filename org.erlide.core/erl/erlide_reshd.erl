%%%----------------------------------------------------------------------
%%% Purpose : Remote erlang shell daemon -- a telnet interface to the shell
%%% File    : reshd.erl
%%% Author  : Tomas Abrahamsson <tab@lysator.liu.se>
%%% Created : 12 Apr 2001 by Tomas Abrahamsson <tab@lysator.liu.se>
%%%
%%% COPYRIGHT
%%%
%%% These programs are released into the public domain.  You may do
%%% anything you like with them, including modifying them and selling
%%% the binaries without source for ridiculous amounts of money without
%%% saying who made them originally.
%%% 
%%% However, I would be happy if you release your works with complete
%%% source for free use.
%%%----------------------------------------------------------------------
-module(erlide_reshd).
-author('tab@lysator.liu.se').
-rcs('$Id: reshd.erl,v 1.9 2007/12/16 03:13:48 tab Exp $').	% '

%% API
-export([start/1, start/2]).
-export([stop/1, stop/2]).
-export([build_regname/1, build_regname/2]).

%% exports due to spawns/rpcs
-export([server_init/3]).
-export([clienthandler_init/3]).

%% ----------------------------------------------------------------------
%% ----------------------------------------------------------------------
%% API
%% ----------------------------------------------------------------------
%% ----------------------------------------------------------------------

%% ----------------------------------------------------------------------
%% start(PortNumber) -> {ok, UsedPortNumber} | {error, Reason}
%% start(IP, PortNumber) -> {ok, UsedPortNumber} | {error, Reason}
%%   Portnumber = UsedPortNumber = integer(0..65535)
%%   IP = any | {Byte,Byte,Byte,Byte}
%%   Byte = integer(0..255)
%%
%% Start the reshd server to listen for connections on TCP/IP port PortNumber.
%%
%% The special port number 0 means "use any available TCP/IP port".
%% The port that is actually used is returned. If PortNumber != 0, then
%% UsedPortNumber == PortNumber.
%%
%% Optionally, an IP address to bind to can also be specified.
%% The default is that IP any, which means to bind to all ip addresses
%% on the machine.
%%
%% The process that listens for and handles incoming connections is
%% locally registred under the name reshd_<IP>_<UsedPortNumber>.
%% build_regname is used to build the name.
%% ----------------------------------------------------------------------

start(PortNumber) ->
    start(any, PortNumber).
start(IP, PortNumber) ->
    server_start(IP, PortNumber).

%% ----------------------------------------------------------------------
%% stop(PortNumber) -> void()
%% stop(IP, PortNumber) -> void()
%%   Portnumber = UsedPortNumber = integer(0..65535)
%%   IP = any | {Byte,Byte,Byte,Byte}
%%   Byte = integer(0..255)
%% 
%% Stops the reshd server and any open connections associated to it. 
%% ----------------------------------------------------------------------
stop(PortNumber) ->
    stop(any, PortNumber).
stop(IP, PortNumber) ->
    server_stop(IP, PortNumber).


%% ----------------------------------------------------------------------
%% build_regname(PortNumber) -> atom()
%% build_regname(IP, PortNumber) -> atom()
%%   Portnumber = UsedPortNumber = integer(0..65535)
%%   IP = any | {Byte,Byte,Byte,Byte}
%%   Byte = integer(0..255)
%% 
%% Build a name under which the reshd server may be registered.
%% ----------------------------------------------------------------------
build_regname(PortNumber) ->
    build_regname(any, PortNumber).

build_regname(any, PortNumber) ->
    Name = atom_to_list(?MODULE) ++ "_any_" ++ integer_to_list(PortNumber),
    list_to_atom(Name);
build_regname({IP1, IP2, IP3, IP4}, PortNumber) ->
    Name = atom_to_list(?MODULE) ++ "_" ++
	integer_to_list(IP1) ++ "_" ++
	integer_to_list(IP2) ++ "_" ++
	integer_to_list(IP3) ++ "_" ++
	integer_to_list(IP4) ++ "_" ++
	"_" ++ integer_to_list(PortNumber),
    list_to_atom(Name);
build_regname(HostNameOrIP, PortNumber) ->
    Name = atom_to_list(?MODULE) ++
	"_" ++ HostNameOrIP ++ "_" ++
	integer_to_list(PortNumber),
    list_to_atom(Name).


%% ----------------------------------------------------------------------
%% ----------------------------------------------------------------------
%% Internal functions: the server part
%% ----------------------------------------------------------------------
%% ----------------------------------------------------------------------
server_start(IP, PortNumber) ->
    Server = proc_lib:spawn(?MODULE, server_init, [self(), IP, PortNumber]),
    receive
	{ok, UsedPortNumber} ->
	    RegName = build_regname(IP, UsedPortNumber),
	    register(RegName, Server),
	    {ok, UsedPortNumber};
	{error, {Symptom, Diagnostics}} ->
	    {error, {Symptom, Diagnostics}}
    end.

server_stop(IP, PortNumber) ->
    RegName = build_regname(IP, PortNumber),
    case whereis(RegName) of
	undefined ->
	    do_nothing;
	Pid ->
	    Pid ! stop
    end.

server_init(From, IP, PortNumber) ->
    IPOpt = ip_to_opt(IP),
    ListenOpts = [list,
		  {packet, 0},
		  {active, true},		% this is the default
		  {nodelay, true},
		  {reuseaddr, true}] ++ IPOpt,
    erlang:display(ListenOpts),
    case gen_tcp:listen(PortNumber, ListenOpts) of
	{ok, ServerSocket} ->
	    {ok, UsedPortNumber} = inet:port(ServerSocket),
	    From ! {ok, UsedPortNumber},
	    process_flag(trap_exit, true),
	    server_loop(From, ServerSocket);
	{error, Reason} ->
	    From ! {error, {listen_failed, Reason}}
    end.


ip_to_opt(any) ->
    [];
ip_to_opt({_IP1, _IP2, _IP3, _IP4}=IPNumber) ->
    [{ip, IPNumber}];
ip_to_opt(HostNameOrIPAsString) ->
    case inet:getaddr(HostNameOrIPAsString, inet) of
	{ok, IPNumber} ->
	    [{ip, IPNumber}];
	{error, Error} ->
	    loginfo("~p: IP lookup failed for ~p: ~p. Binding to any ip.",
		    [?MODULE, HostNameOrIPAsString, Error]),
	    []
    end.


server_loop(From, ServerSocket) ->
    server_loop(From, ServerSocket, []).

server_loop(From, ServerSocket, Clients) ->
    case gen_tcp:accept(ServerSocket, 250) of
	{ok, ClientSocket} ->
	    ClientHandler = clienthandler_start(From, self(), ClientSocket),
	    gen_tcp:controlling_process(ClientSocket, ClientHandler),
	    server_loop(From, ServerSocket, [ClientHandler | Clients]);
	{error, timeout} ->
	    %% Check for signals now and then
	    receive
		stop ->
		    lists:foreach(fun(Client) -> Client ! stop end, Clients),
		    done;
		{client_stop, Client} ->
		    RemainingClients = [C || C <- Clients, C /= Client],
		    server_loop(From, ServerSocket, RemainingClients);
		{'EXIT', Client, _Reason} ->
		    RemainingClients = [C || C <- Clients, C /= Client],
		    server_loop(From, ServerSocket, RemainingClients);
		Unexpected ->
		    loginfo("~p:server_loop: unexpected message:~p",
			    [?MODULE, Unexpected]),
		    server_loop(From, ServerSocket, Clients)
	    after 0 ->
		    server_loop(From, ServerSocket, Clients)
	    end;
	{error, Reason} ->
	    logerror("~p:server_loop: Error: accepting on ~p: ~p.",
		     [?MODULE, ServerSocket, Reason])
    end.


%% ----------------------------------------------------------------------
%% ----------------------------------------------------------------------
%% The client handler part -- handles a user of the reshd.
%% ----------------------------------------------------------------------
%% ----------------------------------------------------------------------
clienthandler_start(From, Server, ClientSocket) ->
    proc_lib:spawn_link(?MODULE, clienthandler_init, [From, Server, ClientSocket]).

-record(io_request,
	{
	  prompt,
	  mod, fn, args,
	  from, reply_as
	 }).
	  

clienthandler_init(_From, Server, ClientSocket) ->
    %% Announce ourself as group leader.
    %% This causes all calls to io:format(...) and such alike
    %% to send their output to us.
    group_leader(self(), self()),

    %% Next, start the shell
    %% and link to it, so we know when it exits.
    process_flag(trap_exit, true),
    Reshd = shell:start(true),
    link(Reshd),

    %% Go ahead and take care of user input!
    case catch clienthandler_loop(idle, Reshd, Server, ClientSocket) of
	{'EXIT', Reason} ->
	    %% This is not a very good way of relaying a crash, but it
	    %% is the best we know
	    exit(Reshd, kill),
	    exit(Reason);
	_ ->
	    exit(Reshd, kill)
    end.
    

clienthandler_loop(State, Reshd, Server, ClientSocket) ->
    erlang:display(looping),
    receive
	{tcp, _Socket, Input} ->
	    NativeInput = nl_network_to_native(Input),
    erlang:display({msg, Input}),
        	    case handle_input(ClientSocket, State, NativeInput) of
		{ok, NewState} ->
		    clienthandler_loop(NewState, Reshd, Server, ClientSocket);
		close ->
		    gen_tcp:close(ClientSocket)
	    end;

	{tcp_closed, _Socket} ->
	    Server ! {client_stop, self()},
	    done;

	{tcp_error, _Socket, _Reason} ->
	    Server ! {client_stop, self()},
	    done;

	stop ->
	    gen_tcp:close(ClientSocket),
	    done;

	{io_request, From, ReplyAs, Req} ->
            erlang:display({req, Req}),
        
	    case handle_io_request(ClientSocket, State, From, ReplyAs, Req) of
		{ok, NewState} ->
		    clienthandler_loop(NewState, Reshd, Server, ClientSocket);
		close ->
		    gen_tcp:close(ClientSocket)
	    end;

	{'EXIT', Reshd, normal} ->
	    gen_tcp:close(ClientSocket);

	{'EXIT', Reshd, _OtherReason} ->
	    gen_tcp:close(ClientSocket);

	_Other ->
	    clienthandler_loop(State, Reshd, Server, ClientSocket)
    end.


%% Returns:
%%   {ok, NewState} |
%%   close
handle_input(ClientSocket, State, Input) ->
    case State of
	idle ->
	    {ok, {pending_input, Input}};
	{pending_input, PendingInput} ->
	    NewInput = PendingInput ++ Input,
	    {ok, {pending_input, NewInput}};
	{pending_request, Cont, [FirstReq | RestReqs] = Requests} ->
	    #io_request{prompt = Prompt,
			mod = Mod,
			fn = Fun,
			args = Args} = FirstReq,
	    case catch apply(Mod, Fun, [Cont, Input|Args]) of
		{more, NewCont} ->
		    print_prompt(ClientSocket, Prompt),
		    {ok, {pending_request, NewCont, Requests}};
		{done, Result, []} ->
		    #io_request{from = From,
				reply_as = ReplyAs} = FirstReq,
		    io_reply(From, ReplyAs, Result),
		    case length(RestReqs) of
			0 ->
			    {ok, idle};
			_N ->
			    [#io_request{prompt = NextPrompt}|_] = RestReqs,
			    print_prompt(ClientSocket, NextPrompt),
			    InitCont = init_cont(),
			    {ok, {pending_request, InitCont, RestReqs}}
		    end;
		{done, Result, RestChars} ->
		    #io_request{from = From,
				reply_as = ReplyAs} = FirstReq,
		    io_reply(From, ReplyAs, Result),
		    case length(RestReqs) of
			0 ->
			    {ok, {pending_input, RestChars}};
			_N ->
			    InitCont = init_cont(),
			    TmpState = {pending_request, InitCont, RestReqs},
			    handle_input(ClientSocket, RestChars, TmpState)
		    end;
		Other ->
		    logerror("~p:handle_input: Unexpected result: ~p~n",
			     [?MODULE, Other]),
		    close
	    end
    end.

%% Returns:
%%   {ok, NewState} |
%%   close
handle_io_request(ClientSocket, State, From, ReplyAs, IoRequest) ->
    case IoRequest of
	{put_chars, Mod, Fun, Args} ->
	    Text = case catch apply(Mod, Fun, Args) of
		      {'EXIT', _Reason} -> "";
		      Txt -> Txt
		   end,
	    NWText = nl_native_to_network(string_flatten(Text)),
	    gen_tcp:send(ClientSocket, NWText),
	    io_reply(From, ReplyAs, ok),
	    {ok, State};

	{put_chars, Text} ->
	    NWText = nl_native_to_network(string_flatten(Text)),
	    gen_tcp:send(ClientSocket, NWText),
	    io_reply(From, ReplyAs, ok),
	    {ok, State};

	{get_until, Prompt, Mod, Fun, Args} ->
	    NewReq = #io_request{prompt = Prompt,
				 mod = Mod,
				 fn = Fun,
				 args = Args,
				 from = From,
				 reply_as = ReplyAs},
	    case State of
		{pending_request, Cont, PendingReqs} ->
		    NewState = {pending_request, Cont, PendingReqs++[NewReq]},
		    {ok, NewState};

		idle ->
		    print_prompt(ClientSocket, Prompt),
		    InitContinuation = init_cont(),
		    NewState = {pending_request, InitContinuation, [NewReq]},
		    {ok, NewState};

		{pending_input, Input} ->
		    InitContinuation = init_cont(),
		    TmpState = {pending_request, InitContinuation, [NewReq]},
		    handle_input(ClientSocket, TmpState, Input)
	    end;

	{get_geometry, _} ->
	    io_reply(From, ReplyAs, {error,enotsup}),
	    {ok, State};

	{requests, IoReqests} ->
	    handle_io_requests(ClientSocket, State, From, ReplyAs, IoReqests);

	UnexpectedIORequest ->
	    loginfo("~p:handle_io_request: Unexpected IORequest:~p~n",
		    [?MODULE, UnexpectedIORequest]),
	    io_reply(From, ReplyAs, ok),
	    {ok, State}
    end.
    

handle_io_requests(ClientSocket, State0, From, ReplyAs, [LastIoReq]) ->
    handle_io_request(ClientSocket, State0, From, ReplyAs, LastIoReq);
handle_io_requests(ClientSocket, State0, From, ReplyAs, [IoReq|Rest]) ->
    case handle_io_request(ClientSocket, State0, none, ReplyAs, IoReq) of
	{ok, State1} ->
	    handle_io_requests(ClientSocket, State1, From, ReplyAs, Rest);
	close ->
	    close
    end;
handle_io_requests(_ClientSocket, State, _From, _ReplyAs, []) ->
    {ok, State}.


init_cont() ->
    [].

io_reply(none, _ReplyAs, _Result) ->
    ok;
io_reply(From, ReplyAs, Result) ->
    From ! {io_reply, ReplyAs, Result}.

print_prompt(ClientSocket, Prompt) ->
        erlang:display({prompt, Prompt}),
    
    PromptText = case Prompt of
		     TxtAtom when atom(TxtAtom) ->
			 io_lib:format('~s', [TxtAtom]);
		     {IoFun, PromptFmtStr, PromptArgs} ->
			 case catch io_lib:IoFun(PromptFmtStr, PromptArgs) of
			     {'EXIT',Err} ->     erlang:display(Err),
                     "???";
			     T ->     erlang:display(T),
                     T
			 end;
		     {IoFun, PromptFmtStr} ->
			 case catch io_lib:IoFun(PromptFmtStr, []) of
			     {'EXIT',_} -> "???";
			     T -> T
			 end;
		     Term ->
			 io_lib:write(Term)
		 end,
    NWPromptText = nl_native_to_network(string_flatten(PromptText)),
    erlang:display({'--->',NWPromptText}),
                     R=gen_tcp:send(ClientSocket, NWPromptText),    erlang:display(R),R
                 .

%% Convert network newline (cr,lf) to native (\n)
nl_network_to_native(Input) ->
    nl_network_to_native(Input, "").


nl_network_to_native("\r\n" ++ Rest, Acc) ->
    nl_network_to_native(Rest, [$\n | Acc]);
nl_network_to_native([C | Rest], Acc) ->
    nl_network_to_native(Rest, [C | Acc]);
nl_network_to_native("", Acc) ->
    lists:reverse(Acc).

				    

%% Convert native newline \n to network (cr,lf)
nl_native_to_network(Input) ->
    nl_native_to_network(Input, "").


nl_native_to_network("\n" ++ Rest, Acc) ->
    %% Here we put \r\n in reversed order.
    %% It'll be put in correct order by the lists:reverse() call
    %% in the last clause.
    nl_native_to_network(Rest, lists:reverse("\r\n", Acc));
nl_native_to_network([C | Rest], Acc) ->
    nl_native_to_network(Rest, [C | Acc]);
nl_native_to_network("", Acc) ->
    lists:reverse(Acc).


loginfo(FmtStr, Args) ->
    %% FIXME: Invent a way to log info.
    %% Can't use the error_log module since someone may
    %% add a log handler that does io:format. Then there
    %% will be a deadlock, I think, if this is function
    %% is called from within code that handles the client.
    Txt = fmt(FmtStr, Args),
    error_logger:info_msg("~s", [Txt]),
    fixme.
logerror(FmtStr, Args) ->
    %% See loginfo/2.
    Txt = fmt(FmtStr, Args),
    error_logger:error_msg("~s", [Txt]),
    fixme.

fmt(FmtStr, Args) ->
    case catch io_lib:format(FmtStr, Args) of
	{'EXIT', _Reason} ->
	    string_flatten(io_lib:format("Badly formatted text: ~p, ~p~n",
					[FmtStr, Args]));
	DeepText ->
	    string_flatten(DeepText)
    end.

string_flatten(IoList) ->
    binary_to_list(list_to_binary([IoList])).
