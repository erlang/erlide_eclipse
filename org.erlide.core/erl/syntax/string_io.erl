%% =====================================================================
%% Allow io operations on an Erlang string
%%
%% Copyright (C) 2005 Bengt Kleberg
%%
%% This library is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation; either version 2 of the
%% License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%% USA
%%
%% Author contact: bengt.kleberg@ericsson.com
%%
%% Modified by Vlad Dumitrescu to use the regular standard libraries.
%% ===============================================================%%
%% @doc Allow io operations on an Erlang string.
%%
%%%		<p>This module makes it possible to open an Erlang
%%%		string. The pid() returned can be passed to the io
%%%		library module <code>io</code>, for reading and
%%%		writing into the string.</p>

-module( string_io ).

-export( [
	close/1,
	open/2
] ).


%% @spec close(Server::pid()) -> string()
close( Server ) ->
	Self = self(),
	send_to( Server, {stop, Self} ),
	case receive_all(  ) of
	{stop, Self, String} ->
		String;
	Other ->
		throw( {error, Other} )
	end.


%% @spec open(String::string(), Modes::list()) -> tuple()
% input
%	Modes : list() of atom(). atoms are => read, write, append
% returns
%	{ok, pid()}
% execption
open( String, Modes )
		when is_list( String ) ->
	case decode( Modes ) of
	error ->
		throw( {error, {mode, Modes}} );
	Mode ->
		Caller = self(),
		Fun = fun ( ) ->
				% the new process will be informed about the ''death'' of
				% the process that created it
				erlang:monitor( process, Caller ),
				String_buffer = string_buffer( String, Mode ),
				loop( String_buffer )
			end,
		{ok, spawn( Fun )}
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% input
%	Modes : list() of atom(). read, write, append are the atoms of interest.
% returns
%	read | write | append | read_write | read_append | error
%
%	check with string_buffer/2 if any return values are changed
decode( Modes ) ->
	case {lists:member( read, Modes ),
			lists:member( write, Modes ),
			lists:member( append, Modes )} of
	{true, false, false} ->
		read;
	{false, true, false} ->
		write;
	{true, true, false} ->
		read_write;
	{false, _, true} ->
		append;
	{true, _, true} ->
		read_append;
	_Other ->
		error
	end.


% input
%	How_many : integer(), how many chars that should be read
%	String_buffer : #string_buffer{}
% return
%	{Read, #string_buffer{}}
%		Read : string() | eof
get_chars( How_many, String_buffer ) ->
	case string_buffer_remaining_length( String_buffer ) of
	0 ->
		{eof, String_buffer};
	Length ->
		case (Length > How_many) of
		true ->
			% take some of the remaining
			Read = string_buffer_remaining( String_buffer, How_many ),
			{Read, string_buffer_after_read( String_buffer, How_many )};
		false ->
			% take all of the remaining
			Remaining = string_buffer_remaining( String_buffer ),
			{Remaining, string_buffer_after_read( String_buffer, all_read )}
		end
	end.


% input
%	Module : module
%	Function : function
%	Arguments : arguments
%	Cont : the part of a result from previous
%		apply(Mod, Func, [Cont, String | Args])
%		that Module:Function() wants to have again,
%		to be able to build the whole result
%	String_buffer : #string_buffer{}
% return
%	{Result, #string_buffer{}}
%		Result : ok | {error, Reason}
get_until( Module, Function, Arguments, Cont, String_buffer ) ->
	String = string_buffer_remaining( String_buffer ),
	% FIXME apply => erl.lang:apply
	case catch erlang:apply(Module, Function, [Cont, String | Arguments]) of
	{done, Result, eof} ->
		{Result, string_buffer_after_read( String_buffer, all_read )};
	{done, Result, Chars_remaining} ->
		{Result,
			string_buffer_after_read( String_buffer, Chars_remaining )};
	{more, Cont_New} ->
		% we do not have any more characters.
		% try again with emptied String_buffer
		get_until( Module,
			Function,
			Arguments,
			Cont_New,
			string_buffer_after_read( String_buffer, all_read ) );
	_Other ->
		{{error, {apply, Module, Function, Arguments}}, String_buffer}
	end.


% input
%	Request : tuple()
%	Acc : {Status, #string_buffer{}}
% return
%	{Result, #string_buffer{}}
%		Result : ok | {error, Reason}
io_request( {put_chars, Chars}, {ok, String_buffer} ) ->
	case string_buffer_is_writeable( String_buffer ) of
	true ->
		put_chars( Chars, String_buffer );
	false ->
		{{error, {mode, is_readonly}}, String_buffer}
	end;
io_request( {put_chars, Module, Func, Args}, String_buffer ) ->
	% FIXME apply => erl.lang:apply
	io_request( {put_chars, catch erlang:apply( Module, Func, Args )}, String_buffer );
io_request( {get_chars, _Prompt, How_many}, {ok, String_buffer} ) ->
	case string_buffer_is_readable( String_buffer ) of
	true ->
		get_chars( How_many, String_buffer );
	false ->
		{{error, {mode, is_writeonly}}, String_buffer}
	end;
io_request( {get_until, _Prompt, Module, Func, Args}, {ok, String_buffer} ) ->
	case string_buffer_is_readable(String_buffer) of
	true ->
		get_until( Module, Func, Args, [], String_buffer );
	false ->
		{{error, {mode, is_writeonly}}, String_buffer}
	end;
io_request( {requests, Requests}, {ok, String_buffer} ) ->
	Fun = fun( Request, Acc ) ->
			io_request( Request, Acc )
		end,
	lists:foldl( Fun, {ok, String_buffer}, Requests );
io_request( Request, {_Status, String_buffer} ) ->
	{{error, {request_unknown, Request}}, String_buffer}.


loop( String_buffer ) ->
	case receive_all(  ) of
	{stop, Sender} ->
		send_to( Sender,
			{stop, Sender, string_buffer_to_string( String_buffer )} ),
		exit(normal);
	{io_request, Sender, Reply_as, Request} ->
		{Result, New} = io_request( Request, {ok, String_buffer} ),
		send_to( Sender, {io_reply, Reply_as, Result} ),
		loop( New );
	{'DOWN', _Ref, _Type, _Object, _Info} ->
		exit(normal);
	_Other ->
		loop( String_buffer )
	end.


% handle io reqest of type put chars
% transform binary to list in input
% return tuple
%	{Result, #string_buffer{}}
put_chars( Binary, String_buffer )
		when is_binary(Binary) ->
	put_chars( binary_to_list(Binary), String_buffer );
put_chars( Chars, String_buffer )
		when is_list(Chars) ->
	{ok, string_buffer_after_write( String_buffer, Chars )};
put_chars( Error, String_buffer ) ->
	{{error, Error}, String_buffer}.


receive_all(  ) ->
	receive
	All ->
%io:format( "receive_all ~w ~w~n", [All, erl.lang.proc:self()]),
		All
	end.


send_to( Receiver, Data ) ->
%io:format( "send_to ~w ~w ~w~n", [ Receiver, Data, erl.lang.proc:self() ]),
	Receiver ! Data.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record( string_buffer, {
	is_readonly=false, % is it possible to write?
	is_writeonly=false, % is it possible to read?
	is_append=false, % should all writes go to the end of the string?
	remaining_length=0, % length of initial string that remains to be read/written
	remaining=eof, % part of initial string that remains to be read/written
		% eof is used by get_until()
	accumulator=[] % part of initial string that has been read/written
		% it can no longer change
} ).


% create new #string_buffer{} with String value
% depending upon Mode the value goes in various places
% return
%	#string_buffer{}
string_buffer( String, Mode )->
	case Mode of
	read ->
		#string_buffer{is_readonly=true, remaining=String,
			remaining_length=length( String )};
	write ->
		% file:open() truncates file on write only. same here
		#string_buffer{is_writeonly=true};
	append ->
		#string_buffer{is_writeonly=true, is_append=true, accumulator=String};
	read_write ->
		#string_buffer{remaining=String,
			remaining_length=length( String )};
	read_append ->
		% remember to put all of remaining into accumulator at first append
		#string_buffer{is_append=true, remaining=String,
			remaining_length=length( String )}
	end.


% do the things neccessary to modify string buffer after a _read of Chars
% return
%	#string_buffer{}
% first all chars read
string_buffer_after_read( String_buffer, all_read ) ->
	#string_buffer{remaining=Remaining} = String_buffer,
	string_buffer_modify( String_buffer, Remaining );
% second is a clause with How_many chars read
string_buffer_after_read( String_buffer, How_many )
		when is_integer( How_many ) ->
	#string_buffer{remaining=Remaining, remaining_length=Remaining_length} =
		String_buffer,
	{Read, Rest} = lists:split( How_many, Remaining ),
	string_buffer_modify( String_buffer, Read, Rest, Remaining_length-How_many );
% last is a clause with New_remaining chars,
% after read of some of the remaining chars.
string_buffer_after_read( String_buffer, New_remaining ) ->
	#string_buffer{remaining=Remaining, remaining_length=Remaining_length} =
		String_buffer,
	New_remaining_length = erl.lang.list:length( New_remaining ),
	Read_length = Remaining_length - New_remaining_length,
	Read = lists:sublist( Remaining, Read_length ),
	string_buffer_modify( String_buffer, Read, New_remaining, New_remaining_length ).


% do the things neccessary to modify string buffer after a write of Chars
% return
%	#string_buffer{}
string_buffer_after_write( String_buffer, Chars ) ->
	#string_buffer{is_append=Append, remaining_length=Remaining_length} =
		String_buffer,
	case {Append, Remaining_length} of
	{true, 0} ->
		% append with nothing left in remaining
		string_buffer_modify( String_buffer, Chars );
	{true, _Remaining_length} ->
		% append with some chars left in remaining
		% save what we have left in remaining first
		#string_buffer{remaining=Remaining} = String_buffer,
		string_buffer_modify( String_buffer,
			lists:append(Remaining, Chars) );
	{false, _Remaining_length} ->
		% no append. write over chars in remining
		Length = lists:length( Chars ),
		case (Length > Remaining_length) of
		true ->
			% write over all (just ignore them)
			string_buffer_modify( String_buffer, Chars );
		false ->
			% write over some. save the rest.
			#string_buffer{remaining=Remaining} = String_buffer,
			Rest = lists:drop( Length, Remaining ),
			string_buffer_modify( String_buffer, Chars, Rest, Remaining_length-Length )
		end
	end.


% check if write is ok in string buffer
% return
%	true | false
string_buffer_is_writeable( String_buffer ) ->
	#string_buffer{is_readonly=Readonly} = String_buffer,
	not Readonly.

% check if read is ok from string buffer
% return
%	true | false
string_buffer_is_readable( String_buffer ) ->
	#string_buffer{is_writeonly=Writeonly} = String_buffer,
	not Writeonly.


% this is a simpler interface to string_buffer_modify/4
% Done is the new addition to the accumulator
% if Done is eof we sanitise input
% return
%	#string_buffer{}
string_buffer_modify( String_buffer, eof ) ->
	string_buffer_modify( String_buffer, [], eof, 0 );
string_buffer_modify( String_buffer, Done ) ->
	string_buffer_modify( String_buffer, Done, eof, 0 ).

% modify existing string buffer.
% Done is the new addition to the accumulator
% if Remaining is [] we sanitise input
% if Done is [] we skip append/2
% return
%	#string_buffer{}
string_buffer_modify( String_buffer, Done, [], _Remaining_length ) ->
	string_buffer_modify( String_buffer, Done, eof, 0 );
string_buffer_modify( String_buffer, [], Remaining, Remaining_length ) ->
	String_buffer#string_buffer{
		remaining=Remaining,
		remaining_length=Remaining_length};
string_buffer_modify( String_buffer, Done, Remaining, Remaining_length ) ->
	#string_buffer{accumulator=Accumulator}=String_buffer,
	String_buffer#string_buffer{
		accumulator=lists:append( Accumulator, Done ),
		remaining=Remaining,
		remaining_length=Remaining_length}.


% get remaining part from string_buffer.
% return
%	string() | eof
string_buffer_remaining( #string_buffer{remaining=String} ) ->
	String.

% get How_many characters of remaining part from string_buffer.
% return
%	string()
string_buffer_remaining( #string_buffer{remaining=String}, How_many ) ->
	lists:sublist( String, How_many ).

% get length of remaining part from string_buffer.
% return
%	integer()
string_buffer_remaining_length( #string_buffer{remaining_length=Length} ) ->
	Length.


% get all strings from string_buffer as one string.
% return
%	string()
string_buffer_to_string( #string_buffer{accumulator=A, remaining=R} ) ->
	Remaining = case R of
		eof ->
			[];
		String ->
			String
		end,
	lists:flatten( lists:append( A, Remaining ) ).


