%% =====================================================================
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
%% @copyright 1997-2006 Richard Carlsson
%% @author Richard Carlsson <richardc@it.uu.se>
%% @end
%% =====================================================================

%% @doc Functions for reading comment lines from Erlang source code.

%%@private
-module(wrangler_comment_scan).

-export([file/1, file/2, join_lines/1, scan_lines/1, string/1]).


%% =====================================================================
%% @spec file(FileName::file:filename()) -> [Comment]
%%
%%	    Comment = {Line, Column, Indentation, Text}
%%	    Line = integer()
%%          Column = integer()
%%          Indentation = integer()
%%          Text = [string()]
%%
%% @doc Extracts comments from an Erlang source code file. Returns a
%% list of entries representing <em>multi-line</em> comments, listed in
%% order of increasing line-numbers. For each entry, `Text'
%% is a list of strings representing the consecutive comment lines in
%% top-down order; the strings contain <em>all</em> characters following
%% (but not including) the first comment-introducing `%'
%% character on the line, up to (but not including) the line-terminating
%% newline.
%%
%% Furthermore, `Line' is the line number and
%% `Column' the left column of the comment (i.e., the column
%% of the comment-introducing `%' character).
%% `Indent' is the indentation (or padding), measured in
%% character positions between the last non-whitespace character before
%% the comment (or the left margin), and the left column of the comment.
%% `Line' and `Column' are always positive
%% integers, and `Indentation' is a nonnegative integer.
%%
%% Evaluation exits with reason `{read, Reason}' if a read
%% error occurred, where `Reason' is an atom corresponding to
%% a Posix error code; see the module {@link //kernel/file} for details.


file(Name) -> file (Name,8).

file(Name,TabWidth) ->
    Name1 = filename(Name),
    case catch {ok, file:read_file(Name1)} of
	{ok, V} ->
	    case V of
		{ok, B} ->
                    string(binary_to_list(B),TabWidth);
		{error, E} ->
		    error_read_file(Name1),
		    exit({read, E})
	    end;
	{'EXIT', E} ->
	    error_read_file(Name1),
	    exit(E);
	R ->
	    error_read_file(Name1),
	    throw(R)
    end.



%% =====================================================================
%% string(string()) -> [Comment]
%%
%%	    Comment = {Line, Column, Indentation, Text}
%%	    Line = integer()
%%          Column = integer()
%%          Indentation = integer()
%%          Text = [string()]
%%
%% @doc Extracts comments from a string containing Erlang source code.
%% Except for reading directly from a string, the behaviour is the same
%% as for {@link file/1}.
%%
%% @see file/1


string(Text) -> string (Text,8).

string(Text,TabWidth) ->
    lists:reverse(join_lines(scan_lines(Text,TabWidth))).


%% =====================================================================
%% @spec scan_lines(string()) -> [CommentLine]
%%
%%	    CommentLine = {Line, Column, Indent, Text}
%%	    Line = integer()
%%	    Column = integer()
%%	    Indent = integer()
%%	    Text = string()
%%
%% @doc Extracts individual comment lines from a source code string.
%% Returns a list of comment lines found in the text, listed in order of
%% <em>decreasing</em> line-numbers, i.e., the last comment line in the
%% input is first in the resulting list. `Text' is a single
%% string, containing all characters following (but not including) the
%% first comment-introducing `%' character on the line, up
%% to (but not including) the line-terminating newline. For details on
%% `Line', `Column' and `Indent', see {@link file/1}.


scan_lines(Text) -> scan_lines (Text,8).

scan_lines(Text,TabWidth) ->
    scan_lines(Text, 1, 0, 0, [], [], TabWidth).

scan_lines([$\s  | Cs], L, Col, M, Str, Ack, TabWidth) ->
    scan_lines(Cs, L, Col + 1, M, [$\s |Str], Ack, TabWidth);
scan_lines([$\t  | Cs], L, Col, M, Str, Ack, TabWidth) ->
    S=lists:append(lists:duplicate(TabWidth, " ")),
    scan_lines(Cs, L, tab(Col,TabWidth), M, S ++ Str, Ack, TabWidth);
scan_lines([$\n  | Cs], L, _Col, _M, _Str, Ack, TabWidth) ->
    scan_lines(Cs, L + 1, 0, 0, [], Ack, TabWidth);
scan_lines([$\r, $\n  | Cs], L, _Col, _M, _Str, Ack, TabWidth) ->
    scan_lines(Cs, L + 1, 0, 0, [], Ack, TabWidth);
scan_lines([$\r  | Cs], L, _Col, _M, _Str, Ack, TabWidth) ->
    scan_lines(Cs, L + 1, 0, 0, [], Ack, TabWidth);
scan_lines([$%  | Cs], L, Col, M, Str, Ack, TabWidth) ->
    scan_comment(Cs, "", L, Col, M, Str, Ack, TabWidth);
scan_lines([$$  | Cs], L, Col, _M, _Str, Ack, TabWidth) ->
    scan_char(Cs, L, Col + 1, Ack, TabWidth);
scan_lines([$"  | Cs], L, Col, _M, _Str, Ack, TabWidth) ->
    scan_string(Cs, $", L, Col + 1, Ack, TabWidth);
scan_lines([$'  | Cs], L, Col, _M, _Str, Ack, TabWidth) ->
    scan_string(Cs, $', L, Col + 1, Ack, TabWidth);
scan_lines([C | Cs], L, Col, _M, Str, Ack, TabWidth) ->
    N = Col + 1,
    scan_lines(Cs, L, N, N, [C|Str], Ack, TabWidth);
scan_lines([], _L, _Col, _M, _Str, Ack, _TabWidth) ->
    Ack.

tab(Col,TabWidth) ->
    Col - (Col rem TabWidth) + TabWidth.

scan_comment([$\n  | Cs], Cs1, L, Col, M, Str, Ack, TabWidth) ->
    seen_comment(Cs, Cs1, L, Col, M, Str, Ack, TabWidth);
scan_comment([$\r, $\n  | Cs], Cs1, L, Col, M, Str, Ack, TabWidth) ->
    seen_comment(Cs, Cs1, L, Col, M, Str, Ack, TabWidth);
scan_comment([$\r  | Cs], Cs1, L, Col, M, Str, Ack, TabWidth) ->
    seen_comment(Cs, Cs1, L, Col, M, Str, Ack, TabWidth);
scan_comment([C | Cs], Cs1, L, Col, M, Str, Ack, TabWidth) ->
    scan_comment(Cs, [C | Cs1], L, Col, M, Str, Ack, TabWidth);
scan_comment([], Cs1, L, Col, M, Str, Ack, TabWidth) ->
    seen_comment([], Cs1, L, Col, M, Str, Ack, TabWidth).

%% Add a comment line to the ackumulator and return to normal
%% scanning. Note that we compute column positions starting at 0
%% internally, but the column values in the comment descriptors
%% should start at 1.

seen_comment(Cs, Cs1, L, Col, M, _Str, Ack, TabWidth) ->
    %% Compute indentation and strip trailing spaces
    N = Col - M,
    Text = lists:reverse(Cs1),  %% not strip following whitespaces; changed by HL;
    NextTok = none,
    Ack1 = [{L, Col + 1, N, NextTok, Text} | Ack],
    scan_lines(Cs, L + 1, 0, 0, [], Ack1, TabWidth).

scan_string([Quote | Cs], Quote, L, Col, Ack, TabWidth) ->
    N = Col + 1,
    scan_lines(Cs, L, N, N, [], Ack, TabWidth);
scan_string([$\t  | Cs], Quote, L, Col, Ack, TabWidth) ->
    scan_string(Cs, Quote, L, tab(Col,TabWidth), Ack, TabWidth);
scan_string([$\n  | Cs], Quote, L, _Col, Ack, TabWidth) ->
    %% Newlines should really not occur in strings/atoms, but we
    %% want to be well behaved even if the input is not.
    scan_string(Cs, Quote, L + 1, 0, Ack, TabWidth);
scan_string([$\r, $\n  | Cs], Quote, L, _Col, Ack, TabWidth) ->
    scan_string(Cs, Quote, L + 1, 0, Ack, TabWidth);
scan_string([$\r  | Cs], Quote, L, _Col, Ack, TabWidth) ->
    scan_string(Cs, Quote, L + 1, 0, Ack, TabWidth);
scan_string([$\\, _C | Cs], Quote, L, Col, Ack, TabWidth) ->
    scan_string(Cs, Quote, L, Col + 2, Ack, TabWidth);  % ignore character C
scan_string([_C | Cs], Quote, L, Col, Ack, TabWidth) ->
    scan_string(Cs, Quote, L, Col + 1, Ack, TabWidth);
scan_string([], _Quote, _L, _Col, Ack, _TabWidth) ->
    %% Finish quietly.
    Ack.

scan_char([$\t  | Cs], L, Col, Ack, TabWidth) ->
    N = tab(Col,TabWidth),
    scan_lines(Cs, L, N, N, [], Ack, TabWidth);    % this is not just any whitespace
scan_char([$\n  | Cs], L, _Col, Ack, TabWidth) ->
    scan_lines(Cs, L + 1, 0, 0, [], Ack, TabWidth);    % handle this, just in case
scan_char([$\r, $\n  | Cs], L, _Col, Ack, TabWidth) ->
    scan_lines(Cs, L + 1, 0, 0, [], Ack, TabWidth);
scan_char([$\r  | Cs], L, _Col, Ack, TabWidth) ->
    scan_lines(Cs, L + 1, 0, 0, [], Ack, TabWidth);
scan_char([$\\, _C | Cs], L, Col, Ack, TabWidth) ->
    N = Col + 2,    % character C must be ignored
    scan_lines(Cs, L, N, N, [], Ack, TabWidth);
scan_char([_C | Cs], L, Col, Ack, TabWidth) ->
    N = Col + 1,    % character C must be ignored
    scan_lines(Cs, L, N, N, [], Ack, TabWidth);
scan_char([], _L, _Col, Ack, _TabWidth) ->
    %% Finish quietly.
    Ack.


%% =====================================================================
%% @spec join_lines([CommentLine]) -> [Comment]
%%
%%	    CommentLine = {Line, Column, Indent, string()}
%%	    Line = integer()
%%	    Column = integer()
%%	    Indent = integer()
%%	    Comment = {Line, Column, Indent, Text}
%%	    Text = [string()]
%%
%% @doc Joins individual comment lines into multi-line comments. The
%% input is a list of entries representing individual comment lines,
%% <em>in order of decreasing line-numbers</em>; see
%% {@link scan_lines/1} for details. The result is a list of
%% entries representing <em>multi-line</em> comments, <em>still listed
%% in order of decreasing line-numbers</em>, but where for each entry,
%% `Text' is a list of consecutive comment lines in order of
%% <em>increasing</em> line-numbers (i.e., top-down).
%%
%% @see scan_lines/1

join_lines([{L, Col, Ind, PreTok, Txt} | Lines]) ->
    join_lines(Lines, [Txt], L, Col, Ind, PreTok);
join_lines([]) ->
    [].

%% In the following, we assume that the current `Txt' is never empty.
%% Recall that the list is in reverse line-number order.

join_lines([{L1, Col1, Ind1, PreTok1, Txt1} | Lines], Txt, L, Col, Ind, PreTok) ->
    if L1 =:= L - 1, Col1 =:= Col, Ind + 1 =:= Col ->
	    %% The last test above checks that the previous
	    %% comment was alone on its line; otherwise it won't
	    %% be joined with the current; this is not always what
	    %% one wants, but works well in general.
	    join_lines(Lines, [Txt1 | Txt], L1, Col1, Ind1, PreTok1);
       true ->
	    %% Finish the current comment and let the new line
	    %% start the next one.
	    [{L, Col, Ind, PreTok, Txt}
	     | join_lines(Lines, [Txt1], L1, Col1, Ind1, PreTok1)]
    end;
join_lines([], Txt, L, Col, Ind, PreTok) ->
    [{L, Col, Ind, PreTok, Txt}].


%% =====================================================================
%% Utility functions for internal use

filename([C|T]) when is_integer(C), C > 0, C =< 255 ->
    [C | filename(T)];
filename([H|T]) ->
    filename(H) ++ filename(T);
filename([]) ->
    [];
filename(N) when is_atom(N) ->
    atom_to_list(N);
filename(N) ->
    report_error("bad filename: `~P'.", [N, 25]),
    exit(error).

error_read_file(Name) ->
    report_error("error reading file `~s'.", [Name]).

report_error(S, Vs) ->
    error_logger:error_msg(lists:concat([?MODULE, ": ", S, "\n"]), Vs).


  
%% =====================================================================
