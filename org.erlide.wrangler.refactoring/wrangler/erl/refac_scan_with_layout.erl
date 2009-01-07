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
%%     $Id: refac_scan.erl,v 1.5 2008-04-30 09:28:12 hl Exp $
%%
%% Modified: 17 Jan 2007 by  Huiqing Li <hl@kent.ac.uk>
%% 
%% 
%% Erlang token scanning functions of io library. This Lexer has been changed by
%% Huiqing Li to keep the comments and whitespaces in the token stream.

%% For handling ISO 8859-1 (Latin-1) we use the following type
%% information:
%%
%% 000 - 037	NUL - US	control
%% 040 - 057	SPC - /		punctuation
%% 060 - 071	0 - 9		digit
%% 072 - 100	: - @		punctuation
%% 101 - 132	A - Z		uppercase
%% 133 - 140	[ - `		punctuation
%% 141 - 172	a - z		lowercase
%% 173 - 176	{ - ~		punctuation
%% 177		DEL		control
%% 200 - 237			control
%% 240 - 277	NBSP - ¿	punctuation
%% 300 - 326	À - Ö		uppercase
%% 327		×		punctuation
%% 330 - 336	Ø - Þ		uppercase
%% 337 - 366	ß - ö		lowercase
%% 367		÷		punctuation
%% 370 - 377	ø - ÿ		lowercase
%%
%% Many punctuation characters region have special meaning.  Must
%% watch using × \327, bvery close to x \170

-module(refac_scan_with_layout).

-export([string/1,string/2, tokens/3]).

-import(lists, [member/2, reverse/1]).

-define(TabWidth,8).  %% Added by Huiqing Li (THIS SHOULD BE READ FORM EMACS!!).


%% string(CharList, StartPos)
%%  Takes a list of characters and tries to tokenise them.
%%
%%  Returns:
%%	{ok,[Tok]}
%%	{error,{ErrorPos,?MODULE,What},EndPos}

string(Cs) -> string(Cs, {1, 1}).

string(Cs, {Line, Col})
    when is_list(Cs), is_integer(Line), is_integer(Col) ->
    %     %% Debug replacement line for chopping string into 1-char segments
    %     scan([], [], [], Pos, Cs, []).
    scan(Cs, [], [], {Line, Col}, [], []).


%% tokens(Continuation, CharList, StartPos) ->
%%	{done, {ok, [Tok], EndPos}, Rest} |
%%	{done, {error,{ErrorPos,?MODULE,What}, EndPos}, Rest} |
%%	{more, Continuation'}
%%  This is the main function into the re-entrant scanner.
%%
%%  The continuation has the form:
%%      {RestChars,ScanStateStack,ScannedTokens,
%%       CurrentPos,ContState,ErrorStack,ContFunArity5}

tokens([], Chars, {Line, Col}) ->
    tokens({[], [], [], {Line, Col}, io, [], fun scan/6},
	   Chars, {Line, Col});
tokens({Cs, _Stack, _Toks, {Line, Col}, eof, _Fun}, eof,
       _) ->
    {done, {eof, {Line, Col}}, Cs};
tokens({Cs, Stack, Toks, {Line, Col}, _State, Errors,
	Fun},
       eof, _) ->
    Fun(Cs ++ eof, Stack, Toks, {Line, Col}, eof, Errors);
tokens({Cs, Stack, Toks, {Line, Col}, State, Errors,
	Fun},
       Chars, _) ->
    Fun(Cs ++ Chars, Stack, Toks, {Line, Col}, State,
	Errors).

%% Scan loop.
%%
%% The scan_*/6 and sub_scan_*/6 functions does tail recursive calls
%% between themselves to change state. State data is kept on the Stack.
%% Results are passed on the Stack and on the stream (Cs). The variable
%% State in this loop is not the scan loop state, but the state for
%% instream handling by more/7 and done/5. The variable Stack is not
%% always a stack, it is just stacked state data for the scan loop, and
%% the variable Errors is a reversed list of scan error {Error,Pos} tuples.
%%
%% All the scan_*/6 functions have the same arguments (in the same order),
%% to keep the tail recursive calls (jumps) fast.
%%
%% When more data is needed from the stream, the tail recursion loop is
%% broken by calling more/7 that either returns to the I/O-server to
%% get more data or fetches it from a string, or by calling done/5 when
%% scanning is done.
%%
%% The last argument to more/7 is a fun to jump back to with more data
%% to continue scanning where it was interrupted.
%%
%% more/7 and done/5 handles scanning from I/O-server (Stream) or from String.
%%

%% String
more(Cs, Stack, Toks, {Line, Col}, eos, Errors, Fun) ->
    erlang:error(badstate,
		 [Cs, Stack, Toks, {Line, Col}, eos, Errors, Fun]);
% %% Debug clause for chopping string into 1-char segments
% more(Cs, Stack, Toks, Pos, [H|T], Errors, Fun) ->
%     Fun(Cs++[H], Stack, Toks, Pos, T, Errors);
more(Cs, Stack, Toks, {Line, Col}, [], Errors, Fun) ->
    Fun(Cs ++ eof, Stack, Toks, {Line, Col}, eos, Errors);
%% Stream
more(Cs, Stack, Toks, {Line, Col}, eof, Errors, Fun) ->
    erlang:error(badstate,
		 [Cs, Stack, Toks, {Line, Col}, eof, Errors, Fun]);
more(Cs, Stack, Toks, {Line, Col}, io, Errors, Fun) ->
    {more, {Cs, Stack, Toks, {Line, Col}, io, Errors, Fun}}.

%% String
done(eof, [], Toks, {Line, Col}, eos) ->
    {ok, reverse(Toks), {Line, Col}};
done(eof, Errors, _Toks, {Line, Col}, eos) ->
    {Error, ErrorPos} = lists:last(Errors),
    {error, {ErrorPos, ?MODULE, Error}, {Line, Col}};
done(Cs, Errors, Toks, {Line, Col}, eos) ->
    scan(Cs, [], Toks, {Line, Col}, eos, Errors);
% %% Debug clause for chopping string into 1-char segments
% done(Cs, Errors, Toks, Pos, [H|T]) ->
%    scan(Cs++[H], [], Toks, Pos, T, Errors);
done(Cs, Errors, Toks, {Line, Col}, []) ->
    scan(Cs ++ eof, [], Toks, {Line, Col}, eos, Errors);
%% Stream
done(Cs, [], [{dot, _} | _] = Toks, {Line, Col}, io) ->
    {done, {ok, reverse(Toks), {Line, Col}}, Cs};
done(Cs, [], [_ | _], {Line, Col}, io) ->
    {done,
     {error, {{Line, Col}, ?MODULE, scan}, {Line, Col}}, Cs};
done(Cs, [], [], {Line, Col}, eof) ->
    {done, {eof, {Line, Col}}, Cs};
done(Cs, [], [{dot, _} | _] = Toks, {Line, Col}, eof) ->
    {done, {ok, reverse(Toks), {Line, Col}}, Cs};
done(Cs, [], _Toks, {Line, Col}, eof) ->
    {done,
     {error, {{Line, Col}, ?MODULE, scan}, {Line, Col}}, Cs};
done(Cs, Errors, _Toks, {Line, Col}, io) ->
    {Error, ErrorPos} = lists:last(Errors),
    {done, {error, {ErrorPos, ?MODULE, Error}, {Line, Col}},
     Cs};
done(Cs, Errors, _Toks, {Line, Col}, eof) ->
    {Error, ErrorPos} = lists:last(Errors),
    {done, {error, {ErrorPos, ?MODULE, Error}, {Line, Col}},
     Cs}.

%% The actual scan loop
%% Stack is assumed to be [].

scan([$\n | Cs], Stack, Toks, {Line, Col}, State,
     Errors) ->      % Newline - skip
    scan(Cs, Stack, [{whitespace, {Line, Col}, '\n'}|Toks], {Line + 1, 1}, State, Errors);  

%%Begin of Adding by Huiqing
scan([C | Cs], Stack, Toks, {Line, Col}, State, Errors)
    when C == $\t
	  ->                          
    scan(Cs, Stack, [{whitespace, {Line, Col}, '\t'}|Toks], {Line, Col + ?TabWidth}, State, Errors);
%% End of adding by Huiqing

scan([C | Cs], Stack, Toks, {Line, Col}, State, Errors)
    when C >= $\000,
	 C =<
	   $\s ->                          % Control chars - skip
    case C of 
	$\s  -> scan(Cs, Stack, [{whitespace, {Line,Col}, ' '}|Toks], {Line, Col + 1}, State, Errors);
	_ ->scan(Cs, Stack, Toks, {Line, Col + 1}, State, Errors)
    end;

scan([C | Cs], Stack, Toks, {Line, Col}, State, Errors)
    when C >= $\200,
	 C =< $\240 ->                        % Control chars -skip
    scan(Cs, Stack, [{whitespace, {Line,Col}, C}|Toks], {Line, Col + 1}, State, Errors);
scan([C | Cs], _Stack, Toks, {Line, Col}, State, Errors)
    when C >= $a,
	 C =< $z ->                              % Atoms
    sub_scan_name(Cs, [C, fun scan_atom/6], Toks,
		  {Line, Col}, State, Errors);
scan([C | Cs], _Stack, Toks, {Line, Col}, State, Errors)
    when C >= $ß, C =< $ÿ,
	 C /= $÷ ->                     % Atoms
    sub_scan_name(Cs, [C, fun scan_atom/6], Toks,
		  {Line, Col}, State, Errors);
scan([C | Cs], _Stack, Toks, {Line, Col}, State, Errors)
    when C >= $A,
	 C =< $Z ->                              % Variables
    sub_scan_name(Cs, [C, fun scan_variable/6], Toks,
		  {Line, Col}, State, Errors);
scan([C | Cs], _Stack, Toks, {Line, Col}, State, Errors)
    when C >= $À, C =< $Þ,
	 C /= $× ->                     % Variables
    sub_scan_name(Cs, [C, fun scan_variable/6], Toks,
		  {Line, Col}, State, Errors);
scan([$_ | Cs], _Stack, Toks, {Line, Col}, State,
     Errors) ->      % _Variables
    sub_scan_name(Cs, [$_, fun scan_variable/6], Toks,
		  {Line, Col}, State, Errors);
scan([C | Cs], _Stack, Toks, {Line, Col}, State, Errors)
    when C >= $0,
	 C =< $9 ->                            % Numbers
    scan_number(Cs, [C], Toks, {Line, Col}, State, Errors);
scan([$$ | Cs], Stack, Toks, {Line, Col}, State,
     Errors) ->        % Character constant
    scan_char(Cs, Stack, Toks, {Line, Col+2}, State, Errors);
scan([$' | Cs], _Stack, Toks, {Line, Col}, State,
     Errors) ->      % Quoted atom
    scan_qatom(Cs, [$', {Line, Col}], Toks, {Line, Col+1},
	       State, Errors);
scan([$" | Cs], _Stack, Toks, {Line, Col}, State,
     Errors) ->      % String
    scan_string(Cs, [$", {Line, Col}], Toks, {Line, Col+1},
		State, Errors);
scan([$% | Cs], _Stack, Toks, {Line, Col}, State,
     Errors) ->       % Comment
    scan_comment(Cs, [$%, {Line, Col}], Toks, {Line, Col+1}, State,
		 Errors);
%% Punctuation characters and operators, first recognise multiples.
%% Clauses are rouped by first character (a short with the same head has
%% to come after a longer).
%%
%% << <- <=
scan("<<" ++ Cs, Stack, Toks, {Line, Col}, State,
     Errors) ->
    scan(Cs, Stack, [{'<<', {Line, Col}} | Toks],
	 {Line, Col + 2}, State, Errors);
scan("<-" ++ Cs, Stack, Toks, {Line, Col}, State,
     Errors) ->
    scan(Cs, Stack, [{'<-', {Line, Col}} | Toks],
	 {Line, Col + 2}, State, Errors);
scan("<=" ++ Cs, Stack, Toks, {Line, Col}, State,
     Errors) ->
    scan(Cs, Stack, [{'<=', {Line, Col}} | Toks],
	 {Line, Col + 2}, State, Errors);
scan("<" = Cs, Stack, Toks, {Line, Col}, State,
     Errors) ->
    more(Cs, Stack, Toks, {Line, Col}, State, Errors,
	 fun scan/6);
%% >> >=
scan(">>" ++ Cs, Stack, Toks, {Line, Col}, State,
     Errors) ->
    scan(Cs, Stack, [{'>>', {Line, Col}} | Toks],
	 {Line, Col + 2}, State, Errors);
scan(">=" ++ Cs, Stack, Toks, {Line, Col}, State,
     Errors) ->
    scan(Cs, Stack, [{'>=', {Line, Col}} | Toks],
	 {Line, Col + 2}, State, Errors);
scan(">" = Cs, Stack, Toks, {Line, Col}, State,
     Errors) ->
    more(Cs, Stack, Toks, {Line, Col}, State, Errors,
	 fun scan/6);
%% -> --
scan("->" ++ Cs, Stack, Toks, {Line, Col}, State,
     Errors) ->
    scan(Cs, Stack, [{'->', {Line, Col}} | Toks],
	 {Line, Col + 2}, State, Errors);
scan("--" ++ Cs, Stack, Toks, {Line, Col}, State,
     Errors) ->
    scan(Cs, Stack, [{'--', {Line, Col}} | Toks],
	 {Line, Col + 2}, State, Errors);
scan("-" = Cs, Stack, Toks, {Line, Col}, State,
     Errors) ->
    more(Cs, Stack, Toks, {Line, Col}, State, Errors,
	 fun scan/6);
%% ++
scan("++" ++ Cs, Stack, Toks, {Line, Col}, State,
     Errors) ->
    scan(Cs, Stack, [{'++', {Line, Col}} | Toks],
	 {Line, Col + 2}, State, Errors);
scan("+" = Cs, Stack, Toks, {Line, Col}, State,
     Errors) ->
    more(Cs, Stack, Toks, {Line, Col}, State, Errors,
	 fun scan/6);
%% =:= =/= =< ==
scan("=:=" ++ Cs, Stack, Toks, {Line, Col}, State,
     Errors) ->
    scan(Cs, Stack, [{'=:=', {Line, Col}} | Toks],
	 {Line, Col + 3}, State, Errors);
scan("=:" = Cs, Stack, Toks, {Line, Col}, State,
     Errors) ->
    more(Cs, Stack, Toks, {Line, Col}, State, Errors,
	 fun scan/6);
scan("=/=" ++ Cs, Stack, Toks, {Line, Col}, State,
     Errors) ->
    scan(Cs, Stack, [{'=/=', {Line, Col}} | Toks],
	 {Line, Col + 3}, State, Errors);
scan("=/" = Cs, Stack, Toks, {Line, Col}, State,
     Errors) ->
    more(Cs, Stack, Toks, {Line, Col}, State, Errors,
	 fun scan/6);
scan("=<" ++ Cs, Stack, Toks, {Line, Col}, State,
     Errors) ->
    scan(Cs, Stack, [{'=<', {Line, Col}} | Toks],
	 {Line, Col + 2}, State, Errors);
scan("==" ++ Cs, Stack, Toks, {Line, Col}, State,
     Errors) ->
    scan(Cs, Stack, [{'==', {Line, Col}} | Toks],
	 {Line, Col + 2}, State, Errors);
scan("=" = Cs, Stack, Toks, {Line, Col}, State,
     Errors) ->
    more(Cs, Stack, Toks, {Line, Col}, State, Errors,
	 fun scan/6);
%% /=
scan("/=" ++ Cs, Stack, Toks, {Line, Col}, State,
     Errors) ->
    scan(Cs, Stack, [{'/=', {Line, Col}} | Toks],
	 {Line, Col + 2}, State, Errors);
scan("/" = Cs, Stack, Toks, {Line, Col}, State,
     Errors) ->
    more(Cs, Stack, Toks, {Line, Col}, State, Errors,
	 fun scan/6);
%% ||
scan("||" ++ Cs, Stack, Toks, {Line, Col}, State,
     Errors) ->
    scan(Cs, Stack, [{'||', {Line, Col}} | Toks],
	 {Line, Col + 2}, State, Errors);
scan("|" = Cs, Stack, Toks, {Line, Col}, State,
     Errors) ->
    more(Cs, Stack, Toks, {Line, Col}, State, Errors,
	 fun scan/6);
%% :-
scan(":-" ++ Cs, Stack, Toks, {Line, Col}, State,
     Errors) ->
    scan(Cs, Stack, [{':-', {Line, Col}} | Toks],
	 {Line, Col + 2}, State, Errors);
%% :: for typed records
scan("::"++Cs, Stack, Toks, {Line, Col}, State, Errors) ->
    scan(Cs, Stack, [{'::',{Line, Col}}|Toks], {Line, Col+2}, State, Errors);

scan(":" = Cs, Stack, Toks, {Line, Col}, State,
     Errors) ->
    more(Cs, Stack, Toks, {Line, Col}, State, Errors,
	 fun scan/6);
%% Full stop and plain '.'
scan("." ++ Cs, Stack, Toks, {Line, Col}, State,
     Errors) ->
    scan_dot(Cs, Stack, Toks, {Line, Col}, State, Errors);
%% All single-char punctuation characters and operators (except '.')
scan([C | Cs], Stack, Toks, {Line, Col}, State,
     Errors) ->
    scan(Cs, Stack,
	 [{list_to_atom([C]), {Line, Col}} | Toks],
	 {Line, Col + 1}, State, Errors);
%%
scan([], Stack, Toks, {Line, Col}, State, Errors) ->
    more([], Stack, Toks, {Line, Col}, State, Errors,
	 fun scan/6);
scan(Eof, _Stack, Toks, {Line, Col}, State, Errors) ->
    done(Eof, Errors, Toks, {Line, Col}, State).

scan_atom(Cs, Name, Toks, {Line, Col}, State, Errors) ->
    case catch list_to_atom(Name) of
      Atom when is_atom(Atom) ->
	  case reserved_word(Atom) of
	    true ->
		scan(Cs, [], [{Atom, {Line, Col}} | Toks],
		     {Line, Col + length(Name)}, State, Errors);
	    false ->
		scan(Cs, [], [{atom, {Line, Col}, Atom} | Toks],
		     {Line, Col + length(Name)}, State, Errors)
	  end;
      _ ->
	  scan(Cs, [], Toks, {Line, Col}, State,
	       [{{illegal, atom}, {Line, Col}} | Errors])
    end.

scan_variable(Cs, Name, Toks, {Line, Col}, State,
	      Errors) ->
    case catch list_to_atom(Name) of
      A when is_atom(A) ->
	  scan(Cs, [], [{var, {Line, Col}, A} | Toks],
	       {Line, Col + length(Name)}, State, Errors);
      _ ->
	  scan(Cs, [], Toks, {Line, Col}, State,
	       [{{illegal, var}, {Line, Col}} | Errors])
    end.

%% Scan for a name - unqouted atom or variable, after the first character.
%%
%% Stack argument: return fun.
%% Returns the scanned name on the stack, unreversed.
%%
sub_scan_name([C | Cs] = Css, Stack, Toks, {Line, Col},
	      State, Errors) ->
    case name_char(C) of
      true ->
	  sub_scan_name(Cs, [C | Stack], Toks, {Line, Col}, State,
			Errors);
      false ->
	  [Fun | Name] = reverse(Stack),
	  Fun(Css, Name, Toks, {Line, Col}, State, Errors)
    end;
sub_scan_name([], Stack, Toks, {Line, Col}, State,
	      Errors) ->
    more([], Stack, Toks, {Line, Col}, State, Errors,
	 fun sub_scan_name/6);
sub_scan_name(Eof, Stack, Toks, {Line, Col}, State,
	      Errors) ->
    [Fun | Name] = reverse(Stack),
    Fun(Eof, Name, Toks, {Line, Col}, State, Errors).

name_char(C) when C >= $a, C =< $z -> true;
name_char(C) when C >= $ß, C =< $ÿ, C /= $÷ -> true;
name_char(C) when C >= $A, C =< $Z -> true;
name_char(C) when C >= $À, C =< $Þ, C /= $× -> true;
name_char(C) when C >= $0, C =< $9 -> true;
name_char($_) -> true;
name_char($@) -> true;
name_char(_) -> false.

scan_char([$\\ | Cs], Stack, Toks, {Line, Col}, State,
	  Errors) ->
    sub_scan_escape(Cs, [fun scan_char_escape/6 | Stack],
		    Toks, {Line, Col}, State, Errors);
scan_char([$\n | Cs], Stack, Toks, {Line, Col}, State,
	  Errors) ->
    scan(Cs, Stack, [{char, {Line, Col}, $\n} | Toks],
	 {Line + 1, Col}, State, Errors);
scan_char([], Stack, Toks, {Line, Col}, State,
	  Errors) ->
    more([], Stack, Toks, {Line, Col}, State, Errors,
	 fun scan_char/6);
scan_char(Cs, Stack, Toks, {Line, Col}, State,
	  Errors) ->
    scan_char_escape(Cs, Stack, Toks, {Line, Col}, State,
		     Errors).

scan_char_escape([nl | Cs], Stack, Toks, {Line, Col},
		 State, Errors) ->
    scan(Cs, Stack, [{char, {Line, Col}, $\n} | Toks],
	 {Line + 1, Col}, State, Errors);
scan_char_escape([C | Cs], Stack, Toks, {Line, Col},
		 State, Errors) ->
    scan(Cs, Stack, [{char, {Line, Col}, C} | Toks],
	 {Line, Col + 1}, State, Errors);
scan_char_escape(Eof, _Stack, _Toks, {Line, Col}, State,
		 Errors) ->
    done(Eof, [{char, {Line, Col}} | Errors], [],
	 {Line, Col + 1}, State).

scan_string([$" | Cs], Stack, Toks, {Line, Col}, State,
	    Errors) ->
    [StartPos, $" | S] = reverse(Stack),
    scan(Cs, [], [{string, StartPos, S} | Toks],
	 {Line, Col + length(S) + 1 + length([C||C<-S, C==$\n])}, State, Errors);
scan_string([$\n | Cs], Stack, Toks, {Line, _Col}, State,
	    Errors) ->
    scan_string(Cs, [$\n | Stack], Toks, {Line + 1, 1},
		State, Errors);
scan_string([$\\ | Cs], Stack, Toks, {Line, Col}, State,
	    Errors) ->
    sub_scan_escape(Cs, [fun scan_string_escape/6 | Stack],
		    Toks, {Line, Col}, State, Errors);
scan_string([C | Cs], Stack, Toks, {Line, Col}, State,
	    Errors) ->
    scan_string(Cs, [C | Stack], Toks, {Line, Col}, State,
		Errors);
scan_string([], Stack, Toks, {Line, Col}, State,
	    Errors) ->
    more([], Stack, Toks, {Line, Col}, State, Errors,
	 fun scan_string/6);
scan_string(Eof, Stack, _Toks, {Line, Col}, State,
	    Errors) ->
    [StartPos, $" | S] = reverse(Stack),
    SS = string:substr(S, 1, 16),
    done(Eof, [{{string, $", SS}, StartPos} | Errors], [],
	 {Line, Col + length(S) + 2+ length([C||C<-S, C==$\n])}, State).

scan_string_escape([nl | Cs], Stack, Toks, {Line, _Col},
		   State, Errors) ->
    scan_string(Cs, [$\n | Stack], Toks, {Line + 1, 1},
		State, Errors);
scan_string_escape([C | Cs], Stack, Toks, {Line, Col},
		   State, Errors) ->
    scan_string(Cs, [C | Stack], Toks, {Line, Col}, State,
		Errors);
scan_string_escape(Eof, Stack, _Toks, {Line, Col},
		   State, Errors) ->
    [StartPos, $" | S] = reverse(Stack),
    SS = string:substr(S, 1, 16),
    done(Eof, [{{string, $", SS}, StartPos} | Errors], [],
	 {Line, Col + length(S) + 2}, State).

scan_qatom([$' | Cs], Stack, Toks, {Line, Col}, State,
	   Errors) ->
    [StartPos, $' | S] = reverse(Stack),
    case catch list_to_atom(S) of
      A when is_atom(A) ->
	  scan(Cs, [], [{qatom, StartPos, A} | Toks],
	       {Line, Col + length(S) + 1}, State, Errors);
      _ ->
	  scan(Cs, [], Toks, {Line, Col}, State,
	       [{{illegal, atom}, StartPos} | Errors])
    end;
scan_qatom([$\n | Cs], Stack, Toks, {Line, _Col}, State,
	   Errors) ->
    scan_qatom(Cs, [$\n | Stack], Toks, {Line + 1, 1},
	       State, Errors);
scan_qatom([$\\ | Cs], Stack, Toks, {Line, Col}, State,
	   Errors) ->
    sub_scan_escape(Cs, [fun scan_qatom_escape/6 | Stack],
		    Toks, {Line, Col}, State, Errors);
scan_qatom([C | Cs], Stack, Toks, {Line, Col}, State,
	   Errors) ->
    scan_qatom(Cs, [C | Stack], Toks, {Line, Col}, State,
	       Errors);
scan_qatom([], Stack, Toks, {Line, Col}, State,
	   Errors) ->
    more([], Stack, Toks, {Line, Col}, State, Errors,
	 fun scan_qatom/6);
scan_qatom(Eof, Stack, _Toks, {Line, Col}, State,
	   Errors) ->
    [StartPos, $' | S] = reverse(Stack),
    SS = string:substr(S, 1, 16),
    done(Eof, [{{string, $', SS}, StartPos} | Errors], [],
	 {Line, Col + length(S) + 1}, State).

scan_qatom_escape([nl | Cs], Stack, Toks, {Line, _Col},
		  State, Errors) ->
    scan_qatom(Cs, [$\n | Stack], Toks, {Line + 1, 1},
	       State, Errors);
scan_qatom_escape([C | Cs], Stack, Toks, {Line, Col},
		  State, Errors) ->
    scan_qatom(Cs, [C | Stack], Toks, {Line, Col}, State,
	       Errors);
scan_qatom_escape(Eof, Stack, _Toks, {Line, Col}, State,
		  Errors) ->
    [StartPos, $' | S] = reverse(Stack),
    SS = string:substr(S, 1, 16),
    done(Eof, [{{string, $', SS}, StartPos} | Errors], [],
	 {Line, Col + length(S) + 1}, State).

%% Scan for a character escape sequence, in character literal or string.
%% A string is a syntactical sugar list (e.g "abc")
%% or a quoted atom (e.g 'EXIT').
%%
%% Stack argument: return fun.
%% Returns the resulting escape character on the stream.
%% The return atom 'nl' means that the escape sequence Backslash Newline
%% was found, i.e an actual Newline in the input.
%%
%% \<1-3> octal digits
sub_scan_escape([O1, O2, O3 | Cs], [Fun | Stack], Toks,
		{Line, Col}, State, Errors)
    when O1 >= $0, O1 =< $7, O2 >= $0, O2 =< $7, O3 >= $0,
	 O3 =< $7 ->
    Val = (O1 * 8 + O2) * 8 + O3 - 73 * $0,
    Fun([Val | Cs], Stack, Toks, {Line, Col}, State,
	Errors);
sub_scan_escape([O1, O2] = Cs, Stack, Toks, {Line, Col},
		State, Errors)
    when O1 >= $0, O1 =< $7, O2 >= $0, O2 =< $7 ->
    more(Cs, Stack, Toks, {Line, Col}, State, Errors,
	 fun sub_scan_escape/6);
sub_scan_escape([O1, O2 | Cs], [Fun | Stack], Toks,
		{Line, Col}, State, Errors)
    when O1 >= $0, O1 =< $7, O2 >= $0, O2 =< $7 ->
    Val = O1 * 8 + O2 - 9 * $0,
    Fun([Val | Cs], Stack, Toks, {Line, Col}, State,
	Errors);
sub_scan_escape([O1] = Cs, Stack, Toks, {Line, Col},
		State, Errors)
    when O1 >= $0, O1 =< $7 ->
    more(Cs, Stack, Toks, {Line, Col}, State, Errors,
	 fun sub_scan_escape/6);
sub_scan_escape([O1 | Cs], [Fun | Stack], Toks,
		{Line, Col}, State, Errors)
    when O1 >= $0, O1 =< $7 ->
    Val = O1 - $0,
    Fun([Val | Cs], Stack, Toks, {Line, Col}, State,
	Errors);
%% \^X -> CTL-X
sub_scan_escape([$^, C | Cs], [Fun | Stack], Toks,
		{Line, Col}, State, Errors) ->
    Val = C band 31,
    Fun([Val | Cs], Stack, Toks, {Line, Col}, State,
	Errors);
sub_scan_escape([$^] = Cs, Stack, Toks, {Line, Col},
		State, Errors) ->
    more(Cs, Stack, Toks, {Line, Col}, State, Errors,
	 fun sub_scan_escape/6);
sub_scan_escape([$^ | Eof], [Fun | Stack], Toks,
		{Line, Col}, State, Errors) ->
    Fun(Eof, Stack, Toks, {Line, Col}, State, Errors);
%% \NL (backslash newline)
sub_scan_escape([$\n | Cs], [Fun | Stack], Toks,
		{Line, Col}, State, Errors) ->
    Fun([nl | Cs], Stack, Toks, {Line, Col}, State, Errors);
%% \X - familiar escape sequences
sub_scan_escape([C | Cs], [Fun | Stack], Toks,
		{Line, Col}, State, Errors) ->
    Val = escape_char(C),
    Fun([Val | Cs], Stack, Toks, {Line, Col}, State,
	Errors);
%%
sub_scan_escape([], Stack, Toks, {Line, Col}, State,
		Errors) ->
    more([], Stack, Toks, {Line, Col}, State, Errors,
	 fun sub_scan_escape/6);
sub_scan_escape(Eof, [Fun | Stack], Toks, {Line, Col},
		State, Errors) ->
    Fun(Eof, Stack, Toks, {Line, Col}, State, Errors).

escape_char($n) -> $\n;                         %\n = LF
escape_char($r) -> $\r;                         %\r = CR
escape_char($t) ->
    $\t;                         %\t = TAB
escape_char($v) -> $\v;                         %\v = VT
escape_char($b) -> $\b;                         %\b = BS
escape_char($f) -> $\f;                         %\f = FF
escape_char($e) ->
    $\e;                         %\e = ESC
escape_char($s) ->
    $\s;                         %\s = SPC
escape_char($d) ->
    $\d;                         %\d = DEL
escape_char(C) -> C.

scan_number([$., C | Cs], Stack, Toks, {Line, Col},
	    State, Errors)
    when C >= $0, C =< $9 ->
    scan_fraction(Cs, [C, $. | Stack], Toks, {Line, Col},
		  State, Errors);
scan_number([$.] = Cs, Stack, Toks, {Line, Col}, State,
	    Errors) ->
    more(Cs, Stack, Toks, {Line, Col}, State, Errors,
	 fun scan_number/6);
scan_number([C | Cs], Stack, Toks, {Line, Col}, State,
	    Errors)
    when C >= $0, C =< $9 ->
    scan_number(Cs, [C | Stack], Toks, {Line, Col}, State,
		Errors);
scan_number([$# | Cs], Stack, Toks, {Line, Col}, State,
	    Errors) ->
    case catch list_to_integer(reverse(Stack)) of
      B when is_integer(B), B >= 2, B =< 1 + $Z - $A + 10 ->
	  scan_based_int(Cs, [B], Toks, {Line, Col}, State,
			 Errors);
      B ->
	  scan(Cs, [], Toks, {Line, Col}, State,
	       [{{base, B}, {Line, Col}} | Errors])
    end;
scan_number([], Stack, Toks, {Line, Col}, State,
	    Errors) ->
    more([], Stack, Toks, {Line, Col}, State, Errors,
	 fun scan_number/6);
scan_number(Cs, Stack, Toks, {Line, Col}, State,
	    Errors) ->
    case catch list_to_integer(reverse(Stack)) of
      N when is_integer(N) ->
	  scan(Cs, [], [{integer, {Line, Col}, N} | Toks],
	       {Line, Col + length(Stack)}, State, Errors);
      _ ->
	  scan(Cs, [], Toks, {Line, Col}, State,
	       [{{illegal, integer}, {Line, Col}} | Errors])
    end.

scan_based_int([C | Cs], [B | Stack], Toks, {Line, Col},
	       State, Errors)
    when C >= $0, C =< $9, C < $0 + B ->
    scan_based_int(Cs, [B, C | Stack], Toks, {Line, Col},
		   State, Errors);
scan_based_int([C | Cs], [B | Stack], Toks, {Line, Col},
	       State, Errors)
    when C >= $A, B > 10, C < $A + B - 10 ->
    scan_based_int(Cs, [B, C | Stack], Toks, {Line, Col},
		   State, Errors);
scan_based_int([C | Cs], [B | Stack], Toks, {Line, Col},
	       State, Errors)
    when C >= $a, B > 10, C < $a + B - 10 ->
    scan_based_int(Cs, [B, C | Stack], Toks, {Line, Col},
		   State, Errors);
scan_based_int([], Stack, Toks, {Line, Col}, State,
	       Errors) ->
    more([], Stack, Toks, {Line, Col}, State, Errors,
	 fun scan_based_int/6);
scan_based_int(Cs, [B | Stack], Toks, {Line, Col},
	       State, Errors) ->
    case catch erlang:list_to_integer(reverse(Stack), B) of
      N when is_integer(N) ->
	  scan(Cs, [], [{integer, {Line, Col}, N} | Toks],
	       {Line, Col + length(Stack)}, State, Errors);
      _ ->
	  scan(Cs, [], Toks, {Line, Col}, State,
	       [{{illegal, integer}, {Line, Col}} | Errors])
    end.

scan_fraction([C | Cs], Stack, Toks, {Line, Col}, State,
	      Errors)
    when C >= $0, C =< $9 ->
    scan_fraction(Cs, [C | Stack], Toks, {Line, Col}, State,
		  Errors);
scan_fraction([$e | Cs], Stack, Toks, {Line, Col},
	      State, Errors) ->
    scan_exponent_sign(Cs, [$E | Stack], Toks, {Line, Col},
		       State, Errors);
scan_fraction([$E | Cs], Stack, Toks, {Line, Col},
	      State, Errors) ->
    scan_exponent_sign(Cs, [$E | Stack], Toks, {Line, Col},
		       State, Errors);
scan_fraction([], Stack, Toks, {Line, Col}, State,
	      Errors) ->
    more([], Stack, Toks, {Line, Col}, State, Errors,
	 fun scan_fraction/6);
scan_fraction(Cs, Stack, Toks, {Line, Col}, State,
	      Errors) ->
    case catch list_to_float(reverse(Stack)) of
      F when is_float(F) ->
	  scan(Cs, [], [{float, {Line, Col}, F} | Toks],
	       {Line, Col + length(Stack)}, State, Errors);
      _ ->
	  scan(Cs, [], Toks, {Line, Col}, State,
	       [{{illegal, float}, {Line, Col}} | Errors])
    end.

scan_exponent_sign([$+ | Cs], Stack, Toks, {Line, Col},
		   State, Errors) ->
    scan_exponent(Cs, [$+ | Stack], Toks, {Line, Col},
		  State, Errors);
scan_exponent_sign([$- | Cs], Stack, Toks, {Line, Col},
		   State, Errors) ->
    scan_exponent(Cs, [$- | Stack], Toks, {Line, Col},
		  State, Errors);
scan_exponent_sign([], Stack, Toks, {Line, Col}, State,
		   Errors) ->
    more([], Stack, Toks, {Line, Col}, State, Errors,
	 fun scan_exponent_sign/6);
scan_exponent_sign(Cs, Stack, Toks, {Line, Col}, State,
		   Errors) ->
    scan_exponent(Cs, Stack, Toks, {Line, Col}, State,
		  Errors).

scan_exponent([C | Cs], Stack, Toks, {Line, Col}, State,
	      Errors)
    when C >= $0, C =< $9 ->
    scan_exponent(Cs, [C | Stack], Toks, {Line, Col}, State,
		  Errors);
scan_exponent([], Stack, Toks, {Line, Col}, State,
	      Errors) ->
    more([], Stack, Toks, {Line, Col}, State, Errors,
	 fun scan_exponent/6);
scan_exponent(Cs, Stack, Toks, {Line, Col}, State,
	      Errors) ->
    case catch list_to_float(reverse(Stack)) of
      F when is_float(F) ->
	  scan(Cs, [], [{float, {Line, Col}, F} | Toks],
	       {Line, Col + length(Stack)}, State, Errors);
      _ ->
	  scan(Cs, [], Toks, {Line, Col}, State,
	       [{{illegal, float}, {Line, Col}} | Errors])
    end.

scan_comment([$\n | Cs], Stack, Toks, {Line, _Col},
	     State, Errors) ->
    [StartPos|S] = reverse([$\n|Stack]),
    scan(Cs, [], [{comment, StartPos, S}|Toks], {Line + 1, 1}, State, Errors);
scan_comment([C | Cs], Stack, Toks, {Line, Col}, State,
	     Errors) ->
    scan_comment(Cs, [C|Stack], Toks, {Line, Col + 1}, State,
		 Errors);
scan_comment([], Stack, Toks, {Line, Col}, State,
	     Errors) ->
    more([], Stack, Toks, {Line, Col}, State, Errors,
	 fun scan_comment/6);
scan_comment(Eof, Stack, Toks, {Line, Col}, State,
	     Errors) ->
    [StartPos|S] = reverse(Stack),
    done(Eof, Errors, [{comment, StartPos, S}|Toks], {Line, Col}, State).

scan_dot([$% | _] = Cs, _Stack, Toks, {Line, Col},
	 State, Errors) ->
    done(Cs, Errors, [{dot, {Line, Col}} | Toks],
	 {Line, Col + 1}, State);
scan_dot([$\n | Cs], _Stack, Toks, {Line, Col}, State,
	 Errors) ->
    done(Cs, Errors, [{whitespace, {Line, Col+1}, '\n'}, {dot, {Line, Col}} | Toks],
	 {Line + 1, 1}, State);
scan_dot([C | Cs], _Stack, Toks, {Line, Col}, State,
	 Errors)
    when C >= $\000, C =< $\s ->
    done(Cs, Errors, [{dot, {Line, Col}} | Toks],
	 {Line, Col + 1}, State);
scan_dot([C | Cs], _Stack, Toks, {Line, Col}, State,
	 Errors)
    when C >= $\200, C =< $\240 ->
    done(Cs, Errors, [{dot, {Line, Col}} | Toks],
	 {Line, Col + 1}, State);
scan_dot([], Stack, Toks, {Line, Col}, State, Errors) ->
    more([], Stack, Toks, {Line, Col}, State, Errors,
	 fun scan_dot/6);
scan_dot(eof, _Stack, Toks, {Line, Col}, State,
	 Errors) ->
    done(eof, Errors, [{dot, {Line, Col}} | Toks],
	 {Line, Col}, State);
scan_dot(Cs, Stack, Toks, {Line, Col}, State, Errors) ->
    scan(Cs, Stack, [{'.', {Line, Col}} | Toks],
	 {Line, Col + 1}, State, Errors).

%% reserved_word(Atom) -> Bool
%%   return 'true' if Atom is an Erlang reserved word, else 'false'.

reserved_word('after') -> true;
reserved_word('begin') -> true;
reserved_word('case') -> true;
reserved_word('try') ->
    Opts = get_compiler_options(),
    not member(disable_try, Opts);
reserved_word('cond') ->
    Opts = get_compiler_options(),
    not member(disable_cond, Opts);
reserved_word('catch') -> true;
reserved_word('andalso') -> true;
reserved_word('orelse') -> true;
reserved_word('end') -> true;
reserved_word('fun') -> true;
reserved_word('if') -> true;
reserved_word('let') -> true;
reserved_word('of') -> true;
reserved_word('query') -> true;
reserved_word('receive') -> true;
reserved_word('when') -> true;
reserved_word('bnot') -> true;
reserved_word('not') -> true;
reserved_word('div') -> true;
reserved_word('rem') -> true;
reserved_word('band') -> true;
reserved_word('and') -> true;
reserved_word('bor') -> true;
reserved_word('bxor') -> true;
reserved_word('bsl') -> true;
reserved_word('bsr') -> true;
reserved_word('or') -> true;
reserved_word('xor') -> true;
reserved_word('spec') -> true;
reserved_word(_) -> false.

get_compiler_options() ->
    %% Who said that Erlang has no global variables?
    case get(compiler_options) of
      undefined ->
	  Opts = case catch ets:lookup(compiler__tab,
				       compiler_options)
		     of
		   [{compiler_options, O}] -> O;
		   _ -> []
		 end,
	  put(compiler_options, Opts),
	  Opts;
      Opts -> Opts
    end.
