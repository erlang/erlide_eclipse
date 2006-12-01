%% =====================================================================
%% epp_dodger - bypasses the Erlang preprocessor.
%%
%% Copyright (C) 2001 Richard Carlsson
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
%% Author contact: richardc@csd.uu.se
%%
%% $Id$
%%
%% =====================================================================
%%
%% @doc <code>epp_dodger</code> - bypasses the Erlang preprocessor.
%%
%% <p>This module tokenises and parses most Erlang source code without
%% expanding preprocessor directives and macro applications, as long as
%% these are syntactically "well-behaved". Because the normal parse
%% trees of the <code>erl_parse</code> module cannot represent these
%% things (normally, they are expanded by the Erlang preprocessor
%% "<code>epp</code>" before the parser sees them), an extended syntax
%% tree is created, using the <code>erl_syntax</code> module.</p>
%%
%% @end
%% =====================================================================

%% TODO: document the quick-parse functions properly.

-module(erlide_epp_dodger).

-export([parse_file/1, quick_parse_file/1, parse/1, quick_parse/1,
     parse/2, quick_parse/2, parse_form/2, quick_parse_form/2,
     format_error/1]).

-export([parse_string/1]).

parse_string(String) ->
    parse_string(String, fun parse/1).

parse_string(String, Parser) ->
    case string_io:open(String, [read]) of
    {ok, Dev} ->
            V = Parser(Dev),
            string_io:close(Dev),
            V;
        Other ->
            Other
    end.

%% =====================================================================
%% @spec parse_file(File) -> {ok, Forms} | {error, ErrorInfo}
%%       File = file:filename()
%%       Forms = [erl_syntax:syntaxTree()]
%%       ErrorInfo = term()
%%
%% @doc Reads and parses a file. If successful, <code>{ok, Forms}</code>
%% is returned, where <code>Forms</code> is a list of abstract syntax
%% trees representing the "program forms" of the file (cf.
%% <code>erl_syntax:is_form/1</code>). Otherwise, <code>{error,
%% ErrorInfo}</code> is returned, where <code>ErrorInfo</code> is an
%% Erlang I/O ErrorInfo structure (see module <code>io</code>.)
%%
%% @see erl_syntax:is_form/1
%% @see io

parse_file(File) ->
    parse_file(File, fun parse/1).

quick_parse_file(File) ->
    parse_file(File, fun quick_parse/1).

parse_file(File, Parser) ->
    case file:open(File, [read]) of
        {ok, Dev} ->
            V = Parser(Dev),
            file:close(Dev),
            V;
        Other ->
            Other
    end.


%% =====================================================================
%% @spec parse(IODevice) -> {ok, Forms} | {error, ErrorInfo}
%% @equiv parse(IODevice, 1)

parse(Dev) ->
    parse(Dev, {1, 1}).

quick_parse(Dev) ->
    quick_parse(Dev, {1, 1}).


%% =====================================================================
%% @spec parse(IODevice, StartLine) -> {ok, Forms} | {error, ErrorInfo}
%%       IODevice = pid()
%%       StartLine = integer()
%%       Forms = [erl_syntax:syntaxTree()]
%%       ErrorInfo = term()
%%
%% @doc Reads and parses program text from an I/O stream. Characters are
%% read from <code>IODevice</code> until end-of-file; apart from this,
%% the behaviour is the same as for <code>parse_file/1</code>.
%% <code>StartLine</code> is the initial line number, which should be a
%% positive integer.
%%
%% @see parse_file/1

parse(Dev, L0) ->
    parse(Dev, L0, fun parse_form/2).

quick_parse(Dev, L0) ->
    parse(Dev, L0, fun quick_parse_form/2).

parse(Dev, L0, Parser) ->
    parse(Dev, L0, [], Parser).

parse(Dev, L0, Fs, Parser) ->
    case Parser(Dev, L0) of
        {ok, none, L1} ->
            parse(Dev, L1, Fs, Parser);
        {ok, F, L1} ->
            parse(Dev, L1, [F | Fs], Parser);
        {error, R, L1} ->
            parse(Dev, L1, [{error, R} | Fs], Parser);
        {eof, _L1} ->
        	%% we get an error if there's a comment last in file; ignore it
            Fs1 = case hd(Fs) of
            		{error, _} ->
            			tl(Fs);
            		_ ->
            			Fs
            	end,
            {ok, lists:reverse(Fs1)}
    end.


%% =====================================================================
%% @spec parse_form(IODevice, StartLine) -> {ok, Form, LineNo}
%%                                        | {eof, LineNo}
%%                                        | {error, ErrorInfo, LineNo}
%%       IODevice = pid()
%%       StartLine = integer()
%%       Form = erl_syntax:syntaxTree() | none
%%       ErrorInfo = term()
%%       LineNo = integer()
%%
%% @doc Reads and parses a single program form from an I/O stream.
%% Characters are read from <code>IODevice</code> until an end-of-form
%% marker is found (a period character followed by whitespace), or until
%% end-of-file; apart from this, the behaviour is similar to that of
%% <code>parse/2</code>, except that the return values also contain the
%% final line number, given that <code>StartLine</code> is the initial
%% line number, and that <code>{eof, LineNo}</code> may be returned. If
%% the scanning/parsing determines that the form should be discarded,
%% `{ok, none, LineNo}' will be returned.
%%
%% @see parse/2

parse_form(Dev, L0) ->
    parse_form(Dev, L0, fun normal_parser/1).

quick_parse_form(Dev, L0) ->
    parse_form(Dev, L0, fun quick_parser/1).


parse_form(Dev, L0, Parser) ->
    case io:request(Dev, {get_until,"",erlide_scan,tokens,[L0]}) of
        {ok, Ts, L1} ->
            Toks1 = erlide_scan:filter_ws(Ts),
            Toks0 = erlide_scan:filter_comments(Toks1),
            Toks = erlide_scan:filter1(Toks0),
            case catch Parser(Toks) of
                {'EXIT', E} ->
                    {error, {L1, ?MODULE, E}, L1};
                {error, R} ->
                    {error, R, L1};
                F ->
                    {ok, F, L1}
            end;
        Other ->
            Other
    end.

%% The standard Erlang parser stage

parse_tokens(Ts) ->
    case erlide_parse:parse_form(Ts) of
        {ok, Form} ->
            Form;
        {error, R} ->
            throw({error, R})
    end.

%% ---------------------------------------------------------------------
%% Quick scanning/parsing - deletes macro definitions and other
%% preprocessor directives, and replaces all macro calls with atoms.

quick_parser(Ts) ->
    filter_form(parse_tokens(quickscan_form(Ts))).

quickscan_form([{'-', _L}, {atom, La, 'define'} | _Ts]) ->
    kill_form(La);
quickscan_form([{'-', _L}, {atom, La, 'undef'} | _Ts]) ->
    kill_form(La);
quickscan_form([{'-', _L}, {atom, La, 'include'} | _Ts]) ->
    kill_form(La);
quickscan_form([{'-', _L}, {atom, La, 'include_lib'} | _Ts]) ->
    kill_form(La);
quickscan_form([{'-', _L}, {atom, La, 'ifdef'} | _Ts]) ->
    kill_form(La);
quickscan_form([{'-', _L}, {atom, La, 'ifndef'} | _Ts]) ->
    kill_form(La);
quickscan_form([{'-', _L}, {atom, La, 'else'} | _Ts]) ->
    kill_form(La);
quickscan_form([{'-', _L}, {atom, La, 'endif'} | _Ts]) ->
    kill_form(La);
quickscan_form(Ts) ->
    quickscan_macros(Ts).

kill_form(L) ->
    [{atom, L, '?pp'}, {'(', L}, {')', L}, {'->', L}, {atom, L, kill},
     {dot, L}].

quickscan_macros([{'?',_}, {atom, L, A} | Ts]) ->
    A1 = list_to_atom("?" ++ atom_to_list(A)),
    [{atom,L,A1} | quickscan_macros(skip_macro_args(Ts))];
quickscan_macros([{'?',_}, {var, L, A} | Ts]) ->
    A1 = list_to_atom("?" ++ atom_to_list(A)),
    [{atom,L,A1} | quickscan_macros(skip_macro_args(Ts))];
quickscan_macros([T | Ts]) ->
    [T | quickscan_macros(Ts)];
quickscan_macros([]) ->
    [].

%% Skipping to the end of a macro call, tracking open/close constructs.

skip_macro_args([{'(',_} | Ts]) ->
    skip_macro_args(Ts, [')']);
skip_macro_args(Ts) ->
    Ts.

skip_macro_args([{'(',_} | Ts], Es) ->
    skip_macro_args(Ts, [')' | Es]);
skip_macro_args([{'{',_} | Ts], Es) ->
    skip_macro_args(Ts, ['}' | Es]);
skip_macro_args([{'[',_} | Ts], Es) ->
    skip_macro_args(Ts, [']' | Es]);
skip_macro_args([{'<<',_} | Ts], Es) ->
    skip_macro_args(Ts, ['>>' | Es]);
skip_macro_args([{'begin',_} | Ts], Es) ->
    skip_macro_args(Ts, ['end' | Es]);
skip_macro_args([{'if',_} | Ts], Es) ->
    skip_macro_args(Ts, ['end' | Es]);
skip_macro_args([{'case',_} | Ts], Es) ->
    skip_macro_args(Ts, ['end' | Es]);
skip_macro_args([{'receive',_} | Ts], Es) ->
    skip_macro_args(Ts, ['end' | Es]);
skip_macro_args([{'try',_} | Ts], Es) ->
    skip_macro_args(Ts, ['end' | Es]);
skip_macro_args([{'cond',_} | Ts], Es) ->
    skip_macro_args(Ts, ['end' | Es]);
skip_macro_args([{E,_} | Ts], [E]) ->        %Found final close
    Ts;
skip_macro_args([{E,_} | Ts], [E | Es]) ->    %Found matching close
    skip_macro_args(Ts, Es);
skip_macro_args([_T | Ts], Es) ->
    skip_macro_args(Ts, Es);
skip_macro_args([], _Es) ->
    throw({error, nonterminated_macro}).

filter_form({function, _, '?pp', _,
         [{clause, _, [], [], [{atom, _, kill}]}]}) ->
    none;
filter_form(T) ->
    T.


%% ---------------------------------------------------------------------
%% Normal parsing - try to preserve all information

normal_parser(Ts) ->
    rewrite_form(parse_tokens(scan_form(Ts))).

scan_form([{'-', L}, {atom, La, 'define'} | Ts]) ->
    [{atom, L, '?pp'}, {'(', La}, {')', La}, {'->', La},
     {atom, La, 'define'} | scan_macros(Ts)];
scan_form([{'-', _L}, {atom, La, 'undef'} | Ts]) ->
    [{atom, La, '?pp'}, {'(', La}, {')', La}, {'->', La},
     {atom, La, 'undef'} | scan_macros(Ts)];
scan_form([{'-', _L}, {atom, La, 'include'} | Ts]) ->
    [{atom, La, '?pp'}, {'(', La}, {')', La}, {'->', La},
     {atom, La, 'include'} | scan_macros(Ts)];
scan_form([{'-', _L}, {atom, La, 'include_lib'} | Ts]) ->
    [{atom, La, '?pp'}, {'(', La}, {')', La}, {'->', La},
     {atom, La, 'include_lib'} | scan_macros(Ts)];
scan_form([{'-', _L}, {atom, La, 'ifdef'} | Ts]) ->
    [{atom, La, '?pp'}, {'(', La}, {')', La}, {'->', La},
     {atom, La, 'ifdef'} | scan_macros(Ts)];
scan_form([{'-', _L}, {atom, La, 'ifndef'} | Ts]) ->
    [{atom, La, '?pp'}, {'(', La}, {')', La}, {'->', La},
     {atom, La, 'ifndef'} | scan_macros(Ts)];
scan_form([{'-', _L}, {atom, La, 'else'} | Ts]) ->
    [{atom, La, '?pp'}, {'(', La}, {')', La}, {'->', La},
     {atom, La, 'else'} | scan_macros(Ts)];
scan_form([{'-', _L}, {atom, La, 'endif'} | Ts]) ->
    [{atom, La, '?pp'}, {'(', La}, {')', La}, {'->', La},
     {atom, La, 'endif'} | scan_macros(Ts)];
scan_form(Ts) ->
    scan_macros(Ts).

scan_macros([{'?', L}, {atom, La, _} = A, {'(', _}, {')', _} | Ts]) ->
    [{'(', L}, {atom, L, '?macro_call'}, {'(', La}, A, {')', La}, {')', La}
     | scan_macros(Ts)];
scan_macros([{'?', L}, {atom, La, _} = A, {'(', _} | Ts]) ->
    %% We don't try to find the closing parenthesis here
    [{atom, L, '?macro_call'}, {'(', La}, A, {',', La}
     | scan_macros(Ts)];
scan_macros([{'?', L}, {atom, La, _} = A | Ts]) ->
    [{'(', L}, {atom, L, '?macro'}, {'(', La}, A, {')', La}, {')', La}
     | scan_macros(Ts)];
scan_macros([{'?', L}, {var, Lv, _} = V, {'(', _}, {')', _} | Ts]) ->
    [{'(', L}, {atom, L, '?macro_call'}, {'(', Lv}, V, {')', Lv}, {')', Lv}
     | scan_macros(Ts)];
scan_macros([{'?', L}, {var, Lv, _} = V, {'(', _} | Ts]) ->
    %% We don't try to find the closing parenthesis here
    [{atom, L, '?macro_call'}, {'(', Lv}, V, {',', Lv}
     | scan_macros(Ts)];
scan_macros([{'?', L}, {var, Lv, _} = V | Ts]) ->
    [{'(', L}, {atom, L, '?macro'}, {'(', L}, V, {')', Lv}, {')', Lv}
     | scan_macros(Ts)];
scan_macros([T | Ts]) ->
    [T | scan_macros(Ts)];
scan_macros([]) ->
    [].

rewrite_form({function, L, '?pp', _,
              [{clause, _, [], [], [{call, _, A, As}]}]}) ->
    erlide_syntax:set_pos(erlide_syntax:attribute(erlide_syntax:abstract(erlide_parse:normalise(A)),
    	rewrite_list(As)), L);
rewrite_form({function, L, '?pp', _, [{clause, _, [], [], [A]}]}) ->
    erlide_syntax:set_pos(erlide_syntax:attribute(erlide_syntax:abstract(erlide_parse:normalise(A))),
    	L);
rewrite_form(T) ->
    rewrite(T).

rewrite_list([T | Ts]) ->
    [rewrite(T) | rewrite_list(Ts)];
rewrite_list([]) ->
    [].

rewrite({call, _, {atom, L, '?macro'}, [A]}) ->
    erlide_syntax:set_pos(erlide_syntax:macro(A), L);
rewrite({call, _, {atom, L, '?macro_call'}, [A | As]}) ->
    erlide_syntax:set_pos(erlide_syntax:macro(A, rewrite_list(As)), L);
rewrite(Node) ->
    case erlide_syntax:subtrees(Node) of
        [] ->
            Node;
        Gs ->
            Node1 = erlide_syntax:make_tree(erlide_syntax:type(Node),
                                         [[rewrite(T) || T <- Ts]
                                          || Ts <- Gs]),
            erlide_syntax:copy_pos(Node, Node1)
    end.


%% @doc Callback function for formatting error descriptors.
%% @spec (term()) -> string()

format_error(unknown) -> "epp_dodger: unknown error".


%% =====================================================================
