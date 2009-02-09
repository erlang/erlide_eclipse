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
-module(refac_io).

-export([put_chars/1,put_chars/2,nl/0,nl/1,
	 get_chars/2,get_chars/3,get_line/1,get_line/2,
	 get_password/0, get_password/1,
	 setopts/1, setopts/2]).
-export([write/1,write/2,read/1,read/2,read/3]).
-export([columns/0,columns/1,rows/0,rows/1]).
-export([fwrite/1,fwrite/2,fwrite/3,fread/2,fread/3,
	 format/1,format/2,format/3]).
-export([scan_erl_exprs/1,scan_erl_exprs/2,scan_erl_exprs/3,
	 scan_erl_form/1,scan_erl_form/2,scan_erl_form/3,
	 parse_erl_exprs/1,parse_erl_exprs/2,parse_erl_exprs/3,
	 parse_erl_form/1,parse_erl_form/2,parse_erl_form/3]).
-export([request/1,request/2,requests/1,requests/2]).

%%
%% User interface.
%%

%% Writing and reading characters.

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

put_chars(Chars) ->
    put_chars(default_output(), Chars).

put_chars(Io, Chars) ->
    o_request(Io, {put_chars,Chars}).

nl() ->
    nl(default_output()).

nl(Io) ->
%    o_request(Io, {put_chars,io_lib:nl()}).
    o_request(Io, nl).

columns() ->
    columns(default_output()).
columns(Io) ->
    case request(Io,{get_geometry,columns}) of
	N  when is_integer(N), N > 0 ->
	    {ok,N};
	_ ->
	    {error,enotsup}
    end.
	    
rows() ->
    rows(default_output()).
rows(Io) ->
    case request(Io,{get_geometry,rows}) of
	N  when is_integer(N), N > 0 ->
	    {ok,N};
	_ ->
	    {error,enotsup}
    end.
	    


get_chars(Prompt, N) ->
    get_chars(default_input(), Prompt, N).

get_chars(Io, Prompt, N) when is_integer(N), N >= 0 ->
    request(Io, {get_chars,Prompt,N}).

get_line(Prompt) ->
    get_line(default_input(), Prompt).

get_line(Io, Prompt) ->
    request(Io, {get_line,Prompt}).

get_password() ->
    get_password(default_input()).

get_password(Io) ->
    request(Io, get_password).

setopts(Opts) ->
    setopts(default_input(), Opts).

setopts(Io, Opts) ->
    request(Io, {setopts, Opts}).

%% Writing and reading Erlang terms.

write(Term) ->
    write(default_output(), Term).

write(Io, Term) ->
    o_request(Io, {write,Term}).

read(Prompt) ->
    read(default_input(), Prompt).

read(Io, Prompt) ->
    case request(Io, {get_until,Prompt,refac_scan,tokens,[{1,1}]}) of  %% modified by Huiqing Li
	{ok,Toks,_EndLine} ->
	    refac_parse:parse_term(Toks);
%	{error, Reason} when atom(Reason) ->
%	    erlang:error(conv_reason(read, Reason), [Io, Prompt]);
	{error,E,_EndLine} ->
	    {error,E};
	{eof,_EndLine} ->
	    eof;
	Other ->
	    Other
    end.

read(Io, Prompt, StartLine) when is_integer(StartLine) ->
    case request(Io, {get_until,Prompt,refac_scan,tokens,[StartLine]}) of
	{ok,Toks,EndLine} ->
            case refac_parse:parse_term(Toks) of
                {ok,Term} -> {ok,Term,EndLine};
                {error,ErrorInfo} -> {error,ErrorInfo,EndLine}
            end;
	{error,E,EndLine} ->
	    {error,E,EndLine};
	{eof,EndLine} ->
	    {eof,EndLine};
	Other ->
	    Other
    end.

%% Formatted writing and reading.

conv_reason(_, arguments) -> badarg;
conv_reason(_, terminated) -> ebadf;
conv_reason(_, _Reason) -> badarg.

fwrite(Format) ->
    format(Format).

fwrite(Format, Args) ->
    format(Format, Args).

fwrite(Io, Format, Args) ->
    format(Io, Format, Args).

fread(Prompt, Format) ->
    fread(default_input(), Prompt, Format).

fread(Io, Prompt, Format) ->
    case request(Io, {fread,Prompt,Format}) of
%	{error, Reason} when atom(Reason) ->
%	    erlang:error(conv_reason(fread, Reason), [Io, Prompt, Format]);
	Other ->
	    Other
    end.

format(Format) ->
    format(Format, []).

format(Format, Args) ->
    format(default_output(), Format, Args).

format(Io, Format, Args) ->
    o_request(Io, {format,Format,Args}).

%% Scanning Erlang code.

scan_erl_exprs(Prompt) ->
    scan_erl_exprs(default_input(), Prompt, {1,1}).

scan_erl_exprs(Io, Prompt) ->
    scan_erl_exprs(Io, Prompt, {1,1}).

scan_erl_exprs(Io, Prompt, Pos0) ->
    request(Io, {get_until,Prompt,refac_scan,tokens,[Pos0]}).

scan_erl_form(Prompt) ->
    scan_erl_form(default_input(), Prompt, {1,1}).

scan_erl_form(Io, Prompt) ->
    scan_erl_form(Io, Prompt, {1,1}).

scan_erl_form(Io, Prompt, Pos0) ->
    request(Io, {get_until,Prompt,refac_scan,tokens,[Pos0]}).

%% Parsing Erlang code.

parse_erl_exprs(Prompt) ->
    parse_erl_exprs(default_input(), Prompt, {1,1}).

parse_erl_exprs(Io, Prompt) ->
    parse_erl_exprs(Io, Prompt, {1,1}).

parse_erl_exprs(Io, Prompt, Pos0) ->
    case request(Io, {get_until,Prompt,refac_scan,tokens,[Pos0]}) of
	{ok,Toks,EndPos} ->
	    case refac_parse:parse_exprs(Toks) of
		{ok,Exprs} -> {ok,Exprs,EndPos};
		{error,E} -> {error,E,EndPos}
	    end;
	Other ->
	    Other
    end.

parse_erl_form(Prompt) ->
    parse_erl_form(default_input(), Prompt, {1,1}).

parse_erl_form(Io, Prompt) ->
    parse_erl_form(Io, Prompt, {1,1}).

parse_erl_form(Io, Prompt, Pos0) ->
    case request(Io, {get_until,Prompt,refac_scan,tokens,[Pos0]}) of
	{ok,Toks,EndPos} ->
	    case refac_parse:parse_form(Toks) of
		{ok,Exprs} -> {ok,Exprs,EndPos};
		{error,E} -> {error,E,EndPos}
	    end;
	Other ->
	    Other
    end.

%% Miscellaneous functions.

request(Request) ->
    request(default_output(), Request).

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

requests(Requests) ->				%Requests as atomic action
    requests(default_output(), Requests).

requests(standard_io, Requests) ->              %Requests as atomic action
    requests(group_leader(), Requests);
requests(Pid, Requests) when is_pid(Pid) ->
    request(Pid, {requests,io_requests(Pid, Requests)});
requests(Name, Requests) when is_atom(Name) ->
    case whereis(Name) of
	undefined ->
	    {error, arguments};
	Pid ->
	    requests(Pid, Requests)
    end.


default_input() ->
    group_leader().

default_output() ->
    group_leader().

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


    
%% io_requests(Requests)
%%  Transform requests into correct i/o server messages. Only handle the
%%  one we KNOW must be changed, others, including incorrect ones, are
%%  passed straight through. Perform a flatten on the request list.

io_requests(Pid, Rs) ->
    io_requests(Pid, Rs, [], []).

io_requests(Pid, [{requests,Rs1}|Rs], Cont, Tail) ->
    io_requests(Pid, Rs1, [Rs|Cont], Tail);
io_requests(Pid, [R|Rs], Cont, Tail) ->
    [io_request(Pid, R)|io_requests(Pid, Rs, Cont, Tail)];
io_requests(Pid, [], [Rs|Cont], Tail) ->
    io_requests(Pid, Rs, Cont, Tail);
io_requests(_Pid, [], [], _Tail) -> 
    [].

io_request(_Pid, {write,Term}) ->
    {put_chars,io_lib,write,[Term]};
io_request(_Pid, {format,Format,Args}) ->
    {put_chars,io_lib,format,[Format,Args]};
io_request(_Pid, {fwrite,Format,Args}) ->
    {put_chars,io_lib,fwrite,[Format,Args]};
io_request(_Pid, nl) ->
    {put_chars,io_lib:nl()};
io_request(Pid, {put_chars,Chars}=Request0) 
  when is_list(Chars), node(Pid) =:= node() ->
    %% Convert to binary data if the I/O server is guaranteed to be new
    Request =
	case catch list_to_binary(Chars) of
	    Binary when is_binary(Binary) ->
		{put_chars,Binary};
	    _ ->
		Request0
	end,
    Request;
io_request(Pid, {get_chars,Prompt,N}) when node(Pid) =/= node() ->
    %% Do not send new I/O request to possibly old I/O server
    {get_until,Prompt,io_lib,collect_chars,[N]};
io_request(Pid, {get_line,Prompt}) when node(Pid) =/= node() ->
    %% Do not send new I/O request to possibly old I/O server
    {get_until,Prompt,io_lib,collect_line,[]};
io_request(_Pid, {fread,Prompt,Format}) ->
    {get_until,Prompt,io_lib,fread,[Format]};
io_request(_Pid, R) ->				%Pass this straight through
    R.

