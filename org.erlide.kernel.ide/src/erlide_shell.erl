%%%----------------------------------------------------------------------
%%% Purpose : Message-driven shell
%%% File    : erlide_shell.erl
%%% Author  : Vlad Dumitrescu
%%%----------------------------------------------------------------------
-module(erlide_shell).

%% API
-export([start/1, stop/1]).

-export([shell_init/1]).

-include("erlide.hrl").

%% ----------------------------------------------------------------------
%% start(Pid) -> {ok, Pid} | {error, Reason}

start(Client) ->
	Server = proc_lib:spawn(?MODULE, shell_init, [Client]),
	{ok, Server}.

%% ----------------------------------------------------------------------
%% stop(PortNumber) -> void()
%% ----------------------------------------------------------------------
stop(Server) ->
	Server ! stop.


%% ----------------------------------------------------------------------

-record(io_request,
		{
		 prompt,
		 mod, fn, args,
		 from, reply_as
		}).


shell_init(Client) ->
	?SAVE_CALLS,
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
	case catch shell_loop(idle, Reshd, Client) of
		{'EXIT', Reason} ->
			%% This is not a very good way of relaying a crash, but it
			%% is the best we know
			exit(Reshd, kill),
			exit(Reason);
		_ ->
			exit(Reshd, kill)
	end.


shell_loop(State, Reshd, Client) ->
	receive
		{input, Input} ->
			case handle_input(Client, State, Input) of
				{ok, NewState} ->
					shell_loop(NewState, Reshd, Client);
				close ->
					done
			end;

		stop ->
			done;

		{io_request, From, ReplyAs, Req} ->
			case handle_io_request(Client, State, From, ReplyAs, Req) of
				{ok, NewState} ->
					shell_loop(NewState, Reshd, Client);
				close ->
					done
			end;

		{'EXIT', Reshd, normal} ->
			done;

		{'EXIT', Reshd, _OtherReason} ->
			done;

		_Other ->
			shell_loop(State, Reshd, Client)
	end.


%% Returns:
%%   {ok, NewState} |
%%   close
handle_input(Client, State, Input) ->
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
					print_prompt(Client, Prompt, self()),
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
							print_prompt(Client, NextPrompt, self()),
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
							handle_input(Client, RestChars, TmpState)
					end;
				Other ->
					logerror("~p:handle_input: Unexpected result: ~p~n",
							 [?MODULE, Other]),
					close
			end
	end.


put_chars(From, ReplyAs, State, _Encoding, Mod, Fun, Args) ->
	Text = case catch apply(Mod, Fun, Args) of
			   {'EXIT', _Reason} -> "";
			   Txt -> Txt
		   end,
	FlatText = string_flatten(Text),
	send_event(FlatText, From),
	io_reply(From, ReplyAs, ok),
	{ok, State}.


get_until(From, ReplyAs, Client, State, _Encoding, Prompt, Mod, Fun, Args) ->
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
			print_prompt(Client, Prompt, From),
			InitContinuation = init_cont(),
			NewState = {pending_request, InitContinuation, [NewReq]},
			{ok, NewState};

		{pending_input, Input} ->
			InitContinuation = init_cont(),
			TmpState = {pending_request, InitContinuation, [NewReq]},
			handle_input(Client, TmpState, Input)
	end.

put_chars(From, ReplyAs, State, _Encoding, Text) ->
	FlatText = string_flatten(Text),
	send_event(FlatText, From),
	io_reply(From, ReplyAs, ok),
	{ok, State}.


%% Returns:
%%   {ok, NewState} |
%%   close
handle_io_request(Client, State, From, ReplyAs, IoRequest) ->
	case IoRequest of
		{put_chars, Mod, Fun, Args} ->
			put_chars(From, ReplyAs, State, latin1, Mod, Fun, Args);
		{put_chars, Encoding, Mod, Fun, Args} ->
			put_chars(From, ReplyAs, State, Encoding, Mod, Fun, Args);

		{put_chars, Text} ->
			put_chars(From, ReplyAs, State, latin1, Text);
		{put_chars, Encoding, Text} ->
			put_chars(From, ReplyAs, State, Encoding, Text);

		{get_until, Prompt, Mod, Fun, Args} ->
			get_until(From, ReplyAs, Client, State, latin1, Prompt, Mod, Fun, Args);
		{get_until, Encoding, Prompt, Mod, Fun, Args} ->
			get_until(From, ReplyAs, Client, State, Encoding, Prompt, Mod, Fun, Args);

		{get_geometry, _} ->
			io_reply(From, ReplyAs, {error,enotsup}),
			{ok, State};

		{requests, IoReqests} ->
			handle_io_requests(Client, State, From, ReplyAs, IoReqests);

		UnexpectedIORequest ->
			loginfo("~p:handle_io_request: Unexpected IORequest:~p~n",
					[?MODULE, UnexpectedIORequest]),
			io_reply(From, ReplyAs, ok),
			{ok, State}
	end.

send_event(String, From) ->
	%%Client ! {String, group_leader(), From, erlang:now()},
	erlide_jrpc:event(io_server, {String, group_leader(), From, erlang:now()}).


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

print_prompt(_Client, Prompt, From) ->
	PromptText = case Prompt of
					 TxtAtom when is_atom(TxtAtom) ->
						 io_lib:format('~s', [TxtAtom]);
					 {IoFun, PromptFmtStr, PromptArgs} ->
						 case catch io_lib:IoFun(PromptFmtStr, PromptArgs) of
							 {'EXIT',_Err} ->
								 "???";
							 T ->
								 T
						 end;
					 {IoFun, PromptFmtStr} ->
						 case catch io_lib:IoFun(PromptFmtStr, []) of
							 {'EXIT',_} -> "???";
							 T -> T
						 end;
					 List when is_list(List) ->
						 List;
					 Term ->
						 io_lib:write(Term)
				 end,
	FlatText = string_flatten(PromptText),
	send_event(FlatText, From),
	ok.

loginfo(FmtStr, Args) ->
	%% TODO: Invent a way to log info.
	%% Can't use the error_log module since someone may
	%% add a log handler that does io:format. Then there
	%% will be a deadlock, I think, if this is function
	%% is called from within code that handles the client.
	Txt = fmt(FmtStr, Args),
	error_logger:info_msg("~s", [Txt]),
	erlide_log:log(Txt),
	fixme.

logerror(FmtStr, Args) ->
	%% See loginfo/2.
	Txt = fmt(FmtStr, Args),
	error_logger:error_msg("~s", [Txt]),
	erlide_log:log(Txt),
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
