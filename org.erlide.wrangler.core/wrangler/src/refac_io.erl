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
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklingsrefa
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%
-module(refac_io).

-export([scan_erl_form/5]).

-export([format/1,format/2,format/3]).

scan_erl_form(Io, Prompt, Pos0, TabWidth, FileFormat) ->
    request_1(Io, {get_until,Prompt,refac_scan,tokens,[{Pos0, TabWidth, FileFormat}]}).

request_1(standard_io, Request) ->
    request_1(group_leader(), Request);
request_1(Pid, Request) when is_pid(Pid) ->
    Mref = erlang:monitor(process,Pid),
    Pid ! {io_request,self(),Pid,Request},
    wait_io_mon_reply(Pid,Mref);
request_1(Name, Request) when is_atom(Name) ->
    case whereis(Name) of
	undefined ->
	    {error, arguments};
	Pid ->
	    request_1(Pid, Request)
    end.

wait_io_mon_reply(From, Mref) ->
    receive
	{io_reply,From,Reply} ->
	    erlang:demonitor(Mref),
	    receive 
		{'DOWN', Mref, _, _, _} -> true
	    after 0 -> true
	    end,
	    Reply;
	{'EXIT', From, _What} ->
	    receive
		{'DOWN', Mref, _, _, _} -> true
	    after 0 -> true
	    end,
	    {error,terminated};
	{'DOWN', Mref, _, _, _} ->
	    receive
		{'EXIT', From, _What} -> true
	    after 0 -> true
	    end,
	    {error,terminated}
    end.


to_tuple(T) when is_tuple(T) -> T;
to_tuple(T) -> {T}.

%% Problem: the variables Other, Name and Args may collide with surrounding
%% ones.
%% Give extra args to macro, being the variables to use.
-define(O_REQUEST(Io, Request),
    case request(Io, Request) of
	{error, Reason} ->
	    [Name | Args] = tuple_to_list(to_tuple(Request)),
	    erlang:error(conv_reason(Name, Reason), [Name, Io | Args]);
	Other ->
	    Other
    end).

o_request(Io, Request) ->
    case request(Io, Request) of
	{error, Reason} ->
	    [Name | Args] = tuple_to_list(to_tuple(Request)),
	    {'EXIT',{undef,[_Current|Mfas]}} = (catch erlang:error(undef)),
	    MFA = {io, Name, [Io | Args]},
	    exit({conv_reason(Name, Reason),[MFA|Mfas]});
%	    erlang:error(conv_reason(Name, Reason), [Name, Io | Args]);
	Other ->
	    Other
    end.


conv_reason(_, arguments) -> badarg;
conv_reason(_, terminated) -> ebadf;
conv_reason(_, _Reason) -> badarg.

format(Format) ->
    format(Format, []).

format(Format, Args) ->
    format(default_output(), Format, Args).

format(Io, Format, Args) ->
    o_request(Io, {format,Format,Args}).



request(standard_io, Request) ->
    request(group_leader(), Request);
request(Pid, Request) when is_pid(Pid) ->
    Mref = erlang:monitor(process,Pid),
    Pid ! {io_request,self(),Pid,io_request(Pid, Request)},
    wait_io_mon_reply(Pid,Mref);
request(Name, Request) when is_atom(Name) ->
    case whereis(Name) of
	undefined ->
	    {error, arguments};
	Pid ->
	    request(Pid, Request)
    end.

default_output() ->
    group_leader().


%% io_request(_Pid, {write,Term}) ->
%%     {put_chars,io_lib,write,[Term]};
io_request(_Pid, {format,Format,Args}) ->
    {put_chars,io_lib,format,[Format,Args]};
%% io_request(_Pid, {fwrite,Format,Args}) ->
%%     {put_chars,io_lib,fwrite,[Format,Args]};
%% io_request(_Pid, nl) ->
%%     {put_chars,io_lib:nl()};
%% io_request(Pid, {put_chars,Chars}=Request0) 
%%   when is_list(Chars), node(Pid) =:= node() ->
%%     %% Convert to binary data if the I/O server is guaranteed to be new
%%     Request =
%% 	case catch list_to_binary(Chars) of
%% 	    Binary when is_binary(Binary) ->
%% 		{put_chars,Binary};
%% 	    _ ->
%% 		Request0
%% 	end,
%%     Request;
%% io_request(_Pid, {fread,Prompt,Format}) ->
%%     {get_until,Prompt,io_lib,fread,[Format]};
io_request(_Pid, R) ->				%Pass this straight through
    R.


