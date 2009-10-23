-module(erlide_yecc_msgs).

%%-define(DEBUG, 1).  

-include("erlide.hrl").

%% dummy i/o server to capture output from yecc and convert it to warnings/errors

-export([start/0]).
-export([interfaces/1]).

-export([server_loop/1]).

start() ->
    case whereis(?MODULE) of
        undefined ->
            Id = spawn(fun server/0),
            register(?MODULE,Id),
       	    Id;
        Pid ->
            Pid
    end.

server() ->
    group_leader(self(), self()),
	?SAVE_CALLS,
    server_loop([]).

server_loop(Msgs) ->
    receive
	{io_request, From, ReplyAs, Request} ->	
	    From ! {io_reply, ReplyAs, ok},	            
            Msg = get_msg(Request),
	    ?MODULE:server_loop([Msg|Msgs]);
        {get_msgs, From} ->
            From ! {msgs, lists:flatten(lists:reverse(Msgs))},
            ?MODULE:server_loop([]);
	_Other ->	
	    ?MODULE:server_loop(Msgs)
    end.
 
%% Return the pid of the shell process.
interfaces(_User) ->
	    [].


get_msg({put_chars, io_lib, format, ["~s", [A]]}) ->
    [parse(A)];
get_msg({put_chars, io_lib, format, _}) ->
    [];
get_msg(_Request) ->
    [].

-define(ERROR, 0).
-define(WARNING, 1).
-define(INFO, 2).

parse(Str) ->
    T0 = string:tokens(Str, ":\n"),
    T = case length(hd(T0)) of
            1 ->
                [A,B|C] = T0,
                [A++":"++B|C];
            _ ->
                T0
        end,
    [File, Line|Msg] = T,
    {list_to_integer(Line), File, string:strip(lists:flatten(Msg)), ?ERROR}.

