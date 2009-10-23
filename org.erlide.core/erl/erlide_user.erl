%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%

-module(erlide_user).

%% Basic standard i/o server for user interface port.

-export([start/0, start/1, start_out/0]).
-export([interfaces/1]).

-define(NAME, user).

%% Internal exports
-export([server/1, server/2]).

%% Defines for control ops
-define(CTRL_OP_GET_WINSIZE,100).

%%
%% The basic server and start-up.
%%

start() ->
    start_port([eof,binary]).

start([Mod,Fun|Args]) ->
    %% Mod,Fun,Args should return a pid. That process is supposed to act
    %% as the io port.
    Pid = apply(Mod, Fun, Args),  % This better work!
    Id = spawn(?MODULE, server, [Pid]),
    register(?NAME, Id),
    Id.

start_out() ->
    %% Output-only version of start/0
    start_port([out,binary]).

start_port(PortSettings) ->
    Id = spawn(?MODULE,server,[{fd,0,1},PortSettings]),
    register(?NAME,Id),
    Id.

%% Return the pid of the shell process.
%% Note: We can't ask the user process for this info since it
%% may be busy waiting for data from the port.
interfaces(User) ->
    case process_info(User, dictionary) of
	{dictionary,Dict} ->
	    case lists:keysearch(shell, 1, Dict) of
		{value,Sh={shell,Shell}} when is_pid(Shell) ->
		    [Sh];
		_ ->
		    []
	    end;
	_ ->
	    []
    end.


server(Pid) when is_pid(Pid) ->
    process_flag(trap_exit, true),
    link(Pid),
    run(Pid).

server(PortName,PortSettings) ->
    process_flag(trap_exit, true),
    Port = open_port(PortName,PortSettings),
    run(Port).

run(P) ->
    put(read_mode,list),
    case init:get_argument(noshell) of
	%% non-empty list -> noshell
	{ok, [_|_]} -> 
	    put(shell, noshell),
	    server_loop(P, queue:new());
	_ ->
	    group_leader(self(), self()),
	    catch_loop(P, start_init_shell())
    end.

catch_loop(Port, Shell) ->
    catch_loop(Port, Shell, queue:new()).

catch_loop(Port, Shell, Q) ->
    case catch server_loop(Port, Q) of
	new_shell ->
	    exit(Shell, kill),
	    catch_loop(Port, start_new_shell());
	{unknown_exit,{Shell,Reason},_} ->			% shell has exited
	    case Reason of
		normal ->
		    put_chars("*** ", Port, []);
		_ ->
		    put_chars("*** ERROR: ", Port, [])
	    end,
	    put_chars("Shell process terminated! ***\n", Port, []),
	    catch_loop(Port, start_new_shell());
	{unknown_exit,_,Q1} ->
	    catch_loop(Port, Shell, Q1);	     
	{'EXIT',R} ->
	    exit(R)
    end.

link_and_save_shell(Shell) ->
    link(Shell),
    put(shell, Shell),
    Shell.        

start_init_shell() ->
    link_and_save_shell(shell:start(init)).

start_new_shell() ->
    link_and_save_shell(shell:start()).

server_loop(Port, Q) ->
    receive
	{Port,{data,Bytes}} ->
	    case get(shell) of
		noshell ->
		    server_loop(Port, queue:snoc(Q, Bytes));
		_ ->
		    case contains_ctrl_g_or_ctrl_c(Bytes) of
			false ->
			    server_loop(Port, queue:snoc(Q, Bytes));
			_ ->
			    throw(new_shell)
		    end
	    end;
	{io_request,From,ReplyAs,Request}=Msg when is_pid(From) ->
		erlide_log:logp(Msg),
		
	    server_loop(Port, do_io_request(Request, From, ReplyAs, Port, Q));
	{Port, eof} ->
	    put(eof, true),
	    server_loop(Port, Q);

	%% Ignore messages from port here.
	{'EXIT',Port,badsig} ->			% Ignore badsig errors
	    server_loop(Port, Q);
	{'EXIT',Port,What} ->			% Port has exited
	    exit(What);

	%% Check if shell has exited
	{'EXIT',SomePid,What} ->
	    case get(shell) of
		noshell ->
		    server_loop(Port, Q);	% Ignore
		_ ->
		    throw({unknown_exit,{SomePid,What},Q})
	    end;
	
	_Other ->				% Ignore other messages
	    server_loop(Port, Q)
    end.


get_fd_geometry(Port) ->
    case (catch port_control(Port,?CTRL_OP_GET_WINSIZE,[])) of
	List when is_list(List), length(List) =:= 8 -> 
	    <<W:32/native,H:32/native>> = list_to_binary(List),
	    {W,H};
	_ ->
	    error
    end.


%% NewSaveBuffer = io_request(Request, FromPid, ReplyAs, Port, SaveBuffer)

do_io_request(Req, From, ReplyAs, Port, Q0) ->
    case io_request(Req, Port, Q0) of
	{_Status,Reply,Q1} ->
	    io_reply(From, ReplyAs, Reply),
	    Q1;
	{exit,What} ->
	    send_port(Port, close),
	    exit(What)
    end.

io_request({put_chars,Chars}, Port, Q) -> % Binary new in R9C
    put_chars(Chars, Port, Q);
io_request({put_chars,Mod,Func,Args}, Port, Q) ->
    put_chars(catch apply(Mod,Func,Args), Port, Q);
io_request({get_chars,Prompt,N}, Port, Q) -> % New in R9C
    get_chars(Prompt, io_lib, collect_chars, N, Port, Q);
%% New in R12
io_request({get_geometry,columns},Port,Q) ->
    case get_fd_geometry(Port) of
	{W,_H} ->
	    {ok,W,Q};
	_ ->
	    {error,{error,enotsup},Q}
    end;
io_request({get_geometry,rows},Port,Q) ->
    case get_fd_geometry(Port) of
	{_W,H} ->
	    {ok,H,Q};
	_ ->
	    {error,{error,enotsup},Q}
    end;
%% These are new in R9C
io_request({get_chars,Prompt,Mod,Func,XtraArg}, Port, Q) ->
%    erlang:display({?MODULE,?LINE,Q}),
    get_chars(Prompt, Mod, Func, XtraArg, Port, Q);
io_request({get_line,Prompt}, Port, Q) ->
%    erlang:display({?MODULE,?LINE,Q}),
    get_chars(Prompt, io_lib, collect_line, [], Port, Q);
io_request({setopts,Opts}, Port, Q) when is_list(Opts) ->
    setopts(Opts, Port, Q);
%% End of new in R9C
io_request({get_until,Prompt,M,F,As}, Port, Q) ->
    get_chars(Prompt, io_lib, get_until, {M,F,As}, Port, Q);
io_request({requests,Reqs}, Port, Q) ->
    io_requests(Reqs, {ok,ok,Q}, Port);
io_request(R, _Port, Q) ->			%Unknown request
    {error,{error,{request,R}},Q}.		%Ignore but give error (?)

%% Status = io_requests(RequestList, PrevStat, Port)
%%  Process a list of output requests as long as the previous status is 'ok'.

io_requests([R|Rs], {ok,_Res,Q}, Port) ->
    io_requests(Rs, io_request(R, Port, Q), Port);
io_requests([_|_], Error, _) ->
    Error;
io_requests([], Stat, _) ->
    Stat.

%% put_port(DeepList, Port)
%%  Take a deep list of characters, flatten and output them to the
%%  port.

put_port(List, Port) ->
    send_port(Port, {command, List}).

%% send_port(Port, Command)

send_port(Port, Command) ->
    Port ! {self(),Command}.

%% io_reply(From, ReplyAs, Reply)
%%  The function for sending i/o command acknowledgement.
%%  The ACK contains the return value.

io_reply(From, ReplyAs, Reply) ->
    From ! {io_reply,ReplyAs,Reply}.

%% put_chars
put_chars(Chars, Port, Q) when is_binary(Chars) ->
    put_port(Chars, Port),
    {ok,ok,Q};
put_chars(Chars, Port, Q) ->
    case catch list_to_binary(Chars) of
	Binary when is_binary(Binary) ->
	    put_chars(Binary, Port, Q);
	_ ->
	    {error,{error,put_chars},Q}
    end.

%% setopts
setopts(Opts0, _Port, Q) ->
    Opts = proplists:substitute_negations([{list,binary}], Opts0),
    case proplists:get_value(binary, Opts) of
	true ->
	    put(read_mode,binary),
	    {ok,ok,Q};
	false ->
	    put(read_mode,list),
	    {ok,ok,Q};
	_ ->
	    {error,{error,badarg},Q}
    end.

%% get_chars(Prompt, Module, Function, XtraArg, Port, Queue)
%%  Gets characters from the input port until the applied function
%%  returns {stop,Result,RestBuf}. Does not block output until input 
%%  has been received.
%%  Returns:
%%	{Status,Result,NewQueue}
%%	{exit,Reason}

%% Entry function.
get_chars(Prompt, M, F, Xa, Port, Q) ->
    prompt(Port, Prompt),
    case {get(eof),queue:is_empty(Q)} of
	{true,true} ->
	    {ok,eof,Q};
	_ ->
	    get_chars(Prompt, M, F, Xa, Port, Q, start)
    end.

%% First loop. Wait for port data. Respond to output requests.
get_chars(Prompt, M, F, Xa, Port, Q, State) ->
    case queue:is_empty(Q) of
	true ->
	    receive
		{Port,{data,Bytes}} ->
		    get_chars_bytes(State, M, F, Xa, Port, Q, Bytes);
		{Port, eof} ->
		    put(eof, true),
		    {ok, eof, []};
		%%{io_request,From,ReplyAs,Request} when is_pid(From) ->
		%%    get_chars_req(Prompt, M, F, Xa, Port, queue:new(), State,
		%%		  Request, From, ReplyAs);
                {io_request,From,ReplyAs,{get_geometry,_}=Req} when is_pid(From) ->
                    do_io_request(Req, From, ReplyAs, Port, 
                                  queue:new()), %Keep Q over this call
                    %% No prompt.
                    get_chars(Prompt, M, F, Xa, Port, Q, State);
		{io_request,From,ReplyAs,Request} when is_pid(From) ->
		    get_chars_req(Prompt, M, F, Xa, Port, Q, State,
				  Request, From, ReplyAs);
		{'EXIT',From,What} when node(From) =:= node() ->
		    {exit,What}
	    end;
	false ->
	    get_chars_apply(State, M, F, Xa, Port, Q)
    end.

get_chars_req(Prompt, M, F, XtraArg, Port, Q, State,
	      Req, From, ReplyAs) ->
    do_io_request(Req, From, ReplyAs, Port, queue:new()), %Keep Q over this call
    prompt(Port, Prompt),
    get_chars(Prompt, M, F, XtraArg, Port, Q, State).

%% Second loop. Pass data to client as long as it wants more.
%% A ^G in data interrupts loop if 'noshell' is not undefined.
get_chars_bytes(State, M, F, Xa, Port, Q, Bytes) ->
    case get(shell) of
	noshell ->
	    get_chars_apply(State, M, F, Xa, Port, queue:snoc(Q, Bytes));
	_ ->
	    case contains_ctrl_g_or_ctrl_c(Bytes) of
		false ->
		    get_chars_apply(State, M, F, Xa, Port, 
				    queue:snoc(Q, Bytes));
		_ ->
		    throw(new_shell)
	    end
    end.

get_chars_apply(State0, M, F, Xa, Port, Q) ->
    case catch M:F(State0, cast(queue:head(Q)), Xa) of
	{stop,Result,<<>>} ->
	    {ok,Result,queue:tail(Q)};
	{stop,Result,[]} ->
	    {ok,Result,queue:tail(Q)};
	{stop,Result,eof} ->
	    {ok,Result,queue:tail(Q)};
	{stop,Result,Buf} ->
	    {ok,Result,queue:cons(Buf, queue:tail(Q))};
	{'EXIT',_} ->
	    {error,{error,err_func(M, F, Xa)},[]};
	State1 ->
	    get_chars_more(State1, M, F, Xa, Port, queue:tail(Q))
    end.

get_chars_more(State, M, F, Xa, Port, Q) ->
    case queue:is_empty(Q) of
	true ->
	    case get(eof) of
		undefined ->
		    receive
			{Port,{data,Bytes}} ->
			    get_chars_bytes(State, M, F, Xa, Port, Q, Bytes);
			{Port,eof} ->
			    put(eof, true),
			    get_chars_apply(State, M, F, Xa, Port, 
					    queue:snoc(Q, eof));
			{'EXIT',From,What} when node(From) =:= node() ->
			    {exit,What}
		    end;
		_ ->
		    get_chars_apply(State, M, F, Xa, Port, queue:snoc(Q, eof))
	    end;
	false ->
	    get_chars_apply(State, M, F, Xa, Port, Q)
    end.


%% prompt(Port, Prompt)
%%  Print Prompt onto Port

%% common case, reduces execution time by 20%
prompt(_Port, '') -> ok;

prompt(Port, Prompt) ->
    put_port(io_lib:format_prompt(Prompt), Port).

%% Convert error code to make it look as before
err_func(io_lib, get_until, {_,F,_}) ->
    F;
err_func(_, F, _) ->
    F.

%% using regexp reduces execution time by >50% compared to old code
%% running two regexps in sequence is much faster than \\x03|\\x07
contains_ctrl_g_or_ctrl_c(BinOrList)->
    case {re:run(BinOrList, <<3>>),re:run(BinOrList, <<7>>)} of
	{nomatch, nomatch} -> false;
	_ -> true
    end.

%% Convert a buffer between list and binary
cast(Data) ->
    cast(Data, get(read_mode)).

cast(List, binary) when is_list(List) ->
    list_to_binary(List);
cast(Binary, list) when is_binary(Binary) ->
    binary_to_list(Binary);
cast(Data, _) ->
    Data.
