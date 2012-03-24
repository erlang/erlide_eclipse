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

%% Erlang token scanning functions of io library.

%% Modified erl_scan.erl by Vlad Dumitrescu (vladdu55 at gmail dot com):
%%   - Pos values are now {{line number, offset in file}, token length}
%%   - return matched text where meaningful (last element in token tuple)
%%   - added comment and whitespace tokens, returned by functions with "ws" in the name
%%
%% For handling ISO 8859-1 (Latin-1) we use the following type
%% information:
%%
%% 000 - 037    NUL - US    control
%% 040 - 057    SPC - /        punctuation
%% 060 - 071    0 - 9        digit
%% 072 - 100    : - @        punctuation
%% 101 - 132    A - Z        uppercase
%% 133 - 140    [ - `        punctuation
%% 141 - 172    a - z        lowercase
%% 173 - 176    { - ~        punctuation
%% 177        DEL        control
%% 200 - 237            control
%% 240 - 277    NBSP - ¿    punctuation
%% 300 - 326    À - Ö        uppercase
%% 327        ×        punctuation
%% 330 - 336    Ø - Þ        uppercase
%% 337 - 366    ß - ö        lowercase
%% 367        ÷        punctuation
%% 370 - 377    ø - ÿ        lowercase
%%

-module(erlide_scan).

-export([string/1,string_ws/1,string/2,tokens/3,tokens_ws/3,
     format_error/1,reserved_word/1,
     filter/1, filter1/1, filter_comments/1, filter_ws/1]).

-import(lists, [reverse/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%

%%-define(DEBUG, 1).

-include("erlide.hrl").


%% format_error(Error)
%%  Return a string describing the error.

format_error({string,Quote,Head}) ->
    ["unterminated " ++ string_thing(Quote) ++
     " starting with " ++ io_lib:write_string(Head,Quote)];
format_error({illegal,Type}) -> io_lib:fwrite("illegal ~w", [Type]);
format_error(char) -> "unterminated character";
format_error(scan) -> "premature end";
format_error({base,Base}) -> io_lib:fwrite("illegal base '~w'", [Base]);
format_error(float) -> "bad float";
%%
format_error(Other) -> io_lib:write(Other).

string_thing($') -> "atom";
string_thing(_) -> "string".


%% string(CharList, StartPos)
%%  Takes a list of characters and tries to tokenise them.
%%
%%  Returns:
%%    {ok,[Tok]}
%%    {error,{ErrorPos,?MODULE,What},EndPos}

string(Cs) ->
    case string_ws(Cs) of
        {ok, Toks, Pos} ->
             {ok, filter_ws(Toks), Pos};
        Error ->
            Error
    end.

string_ws(Cs) ->
    string(Cs, {1, 1}).

string(Cs, Pos) when is_list(Cs) ->
    %%S = unicode:characters_to_list(Cs),
    scan(Cs, [], [], Pos, [], []).

%% tokens(Continuation, CharList, StartPos) ->
%%    {done, {ok, [Tok], EndPos}, Rest} |
%%    {done, {error,{ErrorPos,?MODULE,What}, EndPos}, Rest} |
%%    {more, Continuation'}
%%  This is the main function into the re-entrant scanner.
%%
%%  The continuation has the form:
%%      {RestChars,ScanStateStack,ScannedTokens,
%%       CurrentPos,ContState,ErrorStack,ContFunArity5}

tokens([], Chars, Pos) ->
    tokens({[],[],[],Pos,io,[],fun scan/6}, Chars, Pos);
tokens({Cs,_Stack,_Toks,Pos,eof,_Fun}, eof, _) ->
    {done,{eof,Pos},Cs};
tokens({Cs,Stack,Toks,Pos,_State,Errors,Fun}, eof, _) ->
    Fun(Cs++eof, Stack, Toks, Pos, eof, Errors);
tokens({Cs,Stack,Toks,Pos,State,Errors,Fun}, Chars, _) ->
    Fun(Cs++Chars, Stack, Toks, Pos, State, Errors).

tokens_ws([], Chars, Pos) ->
    tokens({[],[],[],Pos,io,[],fun scan/6}, Chars, Pos);
tokens_ws({Cs,_Stack,_Toks,Pos,eof,_Fun}, eof, _) ->
    {done,{eof,Pos},Cs};
tokens_ws({Cs,Stack,Toks,Pos,_State,Errors,Fun}, eof, _) ->
    Fun(Cs++eof, Stack, Toks, Pos, eof, Errors);
tokens_ws({Cs,Stack,Toks,Pos,State,Errors,Fun}, Chars, _) ->
    Fun(Cs++Chars, Stack, Toks, Pos, State, Errors).


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
more(Cs, Stack, Toks, Pos, eos, Errors, Fun) ->
    erlang:error(badstate, [Cs,Stack,Toks,Pos,eos,Errors,Fun]);
% %% Debug clause for chopping string into 1-char segments
% more(Cs, Stack, Toks, Pos, [H|T], Errors, Fun) ->
%     Fun(Cs++[H], Stack, Toks, Pos, T, Errors);
more(Cs, Stack, Toks, Pos, [], Errors, Fun) ->
    Fun(Cs++eof, Stack, Toks, Pos, eos, Errors);
%% Stream
more(Cs, Stack, Toks, Pos, eof, Errors, Fun) ->
    erlang:error(badstate, [Cs,Stack,Toks,Pos,eof,Errors,Fun]);
more(Cs, Stack, Toks, Pos, io, Errors,Fun) ->
    {more,{Cs,Stack,Toks,Pos,io,Errors,Fun}}.

%% String
done(eof, [], Toks, Pos, eos) ->
    {ok,reverse(Toks),Pos};
done(eof, Errors, _Toks, Pos, eos) ->
    {Error,ErrorPos} = lists:last(Errors),
    {error,{ErrorPos,?MODULE,Error},Pos};
done(Cs, Errors, Toks, Pos, eos) ->
    scan(Cs, [], Toks, Pos, eos, Errors);
% %% Debug clause for chopping string into 1-char segments
% done(Cs, Errors, Toks, Pos, [H|T]) ->
%    scan(Cs++[H], [], Toks, Pos, T, Errors);
done(Cs, Errors, Toks, Pos, []) ->
    scan(Cs++eof, [], Toks, Pos, eos, Errors);
%% Stream
done(Cs, [], [{dot,_}|_]=Toks, Pos, io) ->
    {done,{ok,reverse(Toks),Pos},Cs};
done(Cs, [], [_|_], Pos, io) ->
    {done,{error,{Pos,?MODULE,scan},Pos},Cs};
done(Cs, [], [], Pos, eof) ->
    {done,{eof,Pos},Cs};
done(Cs, [], [{dot,_}|_]=Toks, Pos, eof) ->
    {done,{ok,reverse(Toks),Pos},Cs};
done(Cs, [], _Toks, Pos, eof) ->
    {done,{error,{Pos,?MODULE,scan},Pos},Cs};
done(Cs, Errors, _Toks, Pos, io) ->
    {Error,ErrorPos} = lists:last(Errors),
    {done,{error,{ErrorPos,?MODULE,Error},Pos},Cs};
done(Cs, Errors, _Toks, Pos, eof) ->
    {Error,ErrorPos} = lists:last(Errors),
    {done,{error,{ErrorPos,?MODULE,Error},Pos},Cs}.


%% The actual scan loop
%% Stack is assumed to be [].

scan([$\n|Cs], Stack, Toks, Pos, State, Errors) ->
    scan(Cs, Stack, [{ws, {Pos, 1}, "\n"}|Toks], incrow(Pos), State, Errors);
scan([C|Cs], _Stack, Toks, Pos, State, Errors)
  when C >= $\000, C =< $\s; C >= $\200, C =< $\240 ->  % Control chars
    scan_white(Cs, [C], Toks, Pos, State, Errors);
scan([C|Cs], _Stack, Toks, Pos, State, Errors)
  when C >= $a, C =< $z; C >= $ß, C =< $ÿ, C =/= $÷ ->   % Atoms
    sub_scan_name(Cs, [C,fun scan_atom/6], Toks, Pos, State, Errors);
scan([C|Cs], _Stack, Toks, Pos, State, Errors)
  when C >= $A, C =< $Z ->                              % Variables
    sub_scan_name(Cs, [C,fun scan_variable/6], Toks, Pos, State, Errors);
scan([$_|Cs], _Stack, Toks, Pos, State, Errors) ->      % _Variables
    sub_scan_name(Cs, [$_,fun scan_variable/6], Toks, Pos, State, Errors);
scan([C|Cs], _Stack, Toks, Pos, State, Errors)
    when C >= $0, C =< $9 ->                            % Numbers
    scan_number(Cs, [C], Toks, Pos, State, Errors);
scan([$$|Cs], Stack, Toks, Pos, State, Errors) ->    % Character constant
    scan_char(Cs, Stack, Toks, Pos, State, Errors);
scan([$'|Cs], _Stack, Toks, Pos, State, Errors) ->      % Quoted atom
    scan_qatom(Cs, [$',Pos], Toks, Pos, State, Errors);
scan([$"|Cs], _Stack, Toks, Pos, State, Errors) ->      % String
    scan_string(Cs, [$",Pos], Toks, Pos, State, Errors);
scan([$%|Cs], _Stack, Toks, Pos, State, Errors) ->       % Comment
    scan_comment(Cs, [$%], Toks, Pos, State, Errors);
%% scan([$?|Cs], _Stack, Toks, Pos, State, Errors) ->       % Macros
%%     sub_scan_name(Cs, [$?,fun scan_macro/6], Toks, Pos, State, Errors);
%% Punctuation characters and operators, first recognise multiples.
%% Clauses are rouped by first character (a short with the same head has
%% to come after a longer).
%%
%% << <- <=
scan("<<"++Cs, Stack, Toks, Pos, State, Errors) ->
    scan(Cs, Stack, [{'<<', {Pos, 2}}|Toks], inc(Pos,2), State, Errors);
scan("<-"++Cs, Stack, Toks, Pos, State, Errors) ->
    scan(Cs, Stack, [{'<-', {Pos, 2}}|Toks], inc(Pos,2), State, Errors);
scan("<="++Cs, Stack, Toks, Pos, State, Errors) ->
    scan(Cs, Stack, [{'<=', {Pos, 2}}|Toks], inc(Pos,2), State, Errors);
scan("<"=Cs, Stack, Toks, Pos, State, Errors) ->
    more(Cs, Stack, Toks, Pos, State, Errors, fun scan/6);
%% >> >=
scan(">>"++Cs, Stack, Toks, Pos, State, Errors) ->
    scan(Cs, Stack, [{'>>', {Pos, 2}}|Toks], inc(Pos,2), State, Errors);
scan(">="++Cs, Stack, Toks, Pos, State, Errors) ->
    scan(Cs, Stack, [{'>=', {Pos, 2}}|Toks], inc(Pos,2), State, Errors);
scan(">"=Cs, Stack, Toks, Pos, State, Errors) ->
    more(Cs, Stack, Toks, Pos, State, Errors, fun scan/6);
%% -> --
scan("->"++Cs, Stack, Toks, Pos, State, Errors) ->
    scan(Cs, Stack, [{'->', {Pos, 2}}|Toks], inc(Pos,2), State, Errors);
scan("--"++Cs, Stack, Toks, Pos, State, Errors) ->
    scan(Cs, Stack, [{'--', {Pos, 2}}|Toks], inc(Pos,2), State, Errors);
scan("-"=Cs, Stack, Toks, Pos, State, Errors) ->
    more(Cs, Stack, Toks, Pos, State, Errors, fun scan/6);
%% ++
scan("++"++Cs, Stack, Toks, Pos, State, Errors) ->
    scan(Cs, Stack, [{'++', {Pos, 2}}|Toks], inc(Pos,2), State, Errors);
scan("+"=Cs, Stack, Toks, Pos, State, Errors) ->
    more(Cs, Stack, Toks, Pos, State, Errors, fun scan/6);
%% =:= =/= =< ==
scan("=:="++Cs, Stack, Toks, Pos, State, Errors) ->
    scan(Cs, Stack, [{'=:=', {Pos, 3}}|Toks], inc(Pos,3), State, Errors);
scan("=:"=Cs, Stack, Toks, Pos, State, Errors) ->
    more(Cs, Stack, Toks, Pos, State, Errors, fun scan/6);
scan("=/="++Cs, Stack, Toks, Pos, State, Errors) ->
    scan(Cs, Stack, [{'=/=', {Pos, 3}}|Toks], inc(Pos,3), State, Errors);
scan("=/"=Cs, Stack, Toks, Pos, State, Errors) ->
    more(Cs, Stack, Toks, Pos, State, Errors, fun scan/6);
scan("=<"++Cs, Stack, Toks, Pos, State, Errors) ->
    scan(Cs, Stack, [{'=<', {Pos, 2}}|Toks], inc(Pos,2), State, Errors);
scan("=="++Cs, Stack, Toks, Pos, State, Errors) ->
    scan(Cs, Stack, [{'==', {Pos, 2}}|Toks], inc(Pos,2), State, Errors);
scan("="=Cs, Stack, Toks, Pos, State, Errors) ->
    more(Cs, Stack, Toks, Pos, State, Errors, fun scan/6);
%% /=
scan("/="++Cs, Stack, Toks, Pos, State, Errors) ->
    scan(Cs, Stack, [{'/=', {Pos, 2}}|Toks], inc(Pos,2), State, Errors);
scan("/"=Cs, Stack, Toks, Pos, State, Errors) ->
    more(Cs, Stack, Toks, Pos, State, Errors, fun scan/6);
%% ||
scan("||"++Cs, Stack, Toks, Pos, State, Errors) ->
    scan(Cs, Stack, [{'||', {Pos, 2}}|Toks], inc(Pos,2), State, Errors);
scan("|"=Cs, Stack, Toks, Pos, State, Errors) ->
    more(Cs, Stack, Toks, Pos, State, Errors, fun scan/6);
%% :-
scan(":-"++Cs, Stack, Toks, Pos, State, Errors) ->
    scan(Cs, Stack, [{':-', {Pos, 2}}|Toks], inc(Pos,2), State, Errors);
%% :: for typed records
scan("::"++Cs, Stack, Toks, Pos, State, Errors) ->
    scan(Cs, Stack, [{'::',{Pos, 2}}|Toks], inc(Pos, 2), State, Errors);
%%
scan(":"=Cs, Stack, Toks, Pos, State, Errors) ->
    more(Cs, Stack, Toks, Pos, State, Errors, fun scan/6);
%% Full stop and plain '.'
scan("."++Cs, Stack, Toks, Pos, State, Errors) ->
    scan_dot(Cs, Stack, Toks, Pos, State, Errors);
%% All other single-char punctuation characters and operators
scan([C|Cs], Stack, Toks, Pos, State, Errors) ->
    scan(Cs, Stack, [{list_to_atom([C]), {Pos, 1}}|Toks], inc(Pos,1), State, Errors);
%%
scan([], Stack, Toks, Pos, State, Errors) ->
    more([], Stack, Toks, Pos, State, Errors, fun scan/6);
scan(Eof, _Stack, Toks, Pos, State, Errors) ->
    done(Eof, Errors, Toks, Pos, State).



scan_atom(Cs, Name, Toks, Pos, State, Errors) ->
    case catch list_to_atom(Name) of
    Atom when is_atom(Atom) ->
        Val = case reserved_word(Atom) of
              true ->
              {Atom,{Pos, length(Name)}};
              false ->
              {atom,{Pos, length(Name)}, Atom}
          end,
        scan(Cs, [], [Val|Toks], inc(Pos, length(Name)), State, Errors);
    _ ->
        scan(Cs, [], Toks, Pos, State, [{{illegal,atom},Pos}|Errors])
    end.

scan_variable(Cs, Name, Toks, Pos, State, Errors) ->
    case catch list_to_atom(Name) of
    A when is_atom(A) ->
        scan(Cs, [], [{var,{Pos, length(Name)},list_to_atom(Name)}|Toks], inc(Pos,length(Name)), State, Errors);
    _ ->
        scan(Cs, [], Toks, Pos, State, [{{illegal,var},Pos}|Errors])
    end.

%% scan_macro(Cs, Name, Toks, Pos, State, Errors) ->
%%     case catch list_to_atom(Name) of
%%     A when is_atom(A) ->
%%         scan(Cs, [], [{macro,{Pos, length(Name)},list_to_atom(Name)}|Toks], inc(Pos,length(Name)), State, Errors);
%%     _ ->
%%         scan(Cs, [], Toks, Pos, State, [{{illegal,macro},Pos}|Errors])
%%     end.

%% Scan for a name - unqouted atom or variable, after the first character.
%%
%% Stack argument: return fun.
%% Returns the scanned name on the stack, unreversed.
%%
sub_scan_name([C|Cs]=Css, Stack, Toks, Pos, State, Errors) ->
	case name_char(C) of
		true ->
			sub_scan_name(Cs, [C|Stack], Toks, Pos, State, Errors);
		false ->
			[Fun|Name] = reverse(Stack),
			Fun(Css, Name, Toks, Pos, State, Errors)
	end;
sub_scan_name([], Stack, Toks, Pos, State, Errors) ->
	more([], Stack, Toks, Pos, State, Errors, fun sub_scan_name/6);
sub_scan_name(Eof, Stack, Toks, Pos, State, Errors) ->
	[Fun|Name] = reverse(Stack),
	Fun(Eof, Name, Toks, Pos, State, Errors).

name_char(C) when C >= $a, C =< $z -> true;
name_char(C) when C >= $ß, C =< $ÿ, C =/= $÷ -> true;
name_char(C) when C >= $A, C =< $Z -> true;
name_char(C) when C >= $À, C =< $Þ, C =/= $× -> true;
name_char(C) when C >= $0, C =< $9 -> true;
name_char($_) -> true;
name_char($@) -> true;
name_char(_) -> false.


scan_char([$\\|Cs], Stack, Toks, Pos, State, Errors) ->
    case sub_scan_escape(Cs, Pos) of
    {Rest, Val, StrVal, NewPos} ->
        scan(Rest, Stack, [{char, {Pos, length(StrVal)+2}, Val, [$$, $\\]++StrVal}|Toks], inc(NewPos, 2), State, Errors);
    more ->
        more(Cs, Stack, Toks, Pos, State, Errors, fun scan_char/6);
    continue ->
        scan(Cs, Stack, Toks, Pos, State, Errors)
    end;
scan_char([], Stack, Toks, Pos, State, Errors) ->
    more([], Stack, Toks, Pos, State, Errors, fun scan_char/6);
scan_char([C|Cs], Stack, Toks, Pos, State, Errors) ->
    scan(Cs, Stack, [{char,{Pos, 2},C, [$$, C]}|Toks], inc(Pos,2), State, Errors);
scan_char(Eof, _Stack, _Toks, Pos, State, Errors) ->
    done(Eof, [{char,Pos}|Errors], [], Pos, State).

%% Scan for a character escape sequence, in character literal or string.
%% A string is a syntactical sugar list (e.g "abc")
%% or a quoted atom (e.g 'EXIT').
%%
%% Returns the resulting escape character on the stream.
%% The return atom 'nl' means that the escape sequence Backslash Newline
%% was found, i.e an actual Newline in the input.
%%
%% \<1-3> octal digits
sub_scan_escape([O1,O2,O3|Cs], Pos)
  when O1 >= $0, O1 =< $7, O2 >= $0, O2 =< $7, O3 >= $0, O3 =< $7 ->
    Val = ((O1-$0)*8 + (O2-$0))*8 + O3 - $0,
    StrVal = [O1, O2, O3],
    {Cs, Val, StrVal, inc(Pos, 3)};
sub_scan_escape([O1,O2]=_Cs, _Pos)
  when O1 >= $0, O1 =< $7, O2 >= $0, O2 =< $7 ->
    more;
sub_scan_escape([O1,O2|Cs], Pos)
  when O1 >= $0, O1 =< $7, O2 >= $0, O2 =< $7 ->
    Val = (O1-$0)*8 + O2 - $0,
    StrVal = [O1, O2],
    {Cs, Val, StrVal, inc(Pos, 2)};
sub_scan_escape([O1]=_Cs, _Pos)
  when O1 >= $0, O1 =< $7 ->
    more;
sub_scan_escape([O1|Cs], Pos)
  when O1 >= $0, O1 =< $7 ->
    Val = O1 - $0,
    StrVal = [O1],
    {Cs, Val, StrVal, inc(Pos, 1)};
%% \^X -> CTL-X
sub_scan_escape([$^,C|Cs], Pos) ->
    Val = C band 31,
    StrVal = [$^, C],
    {Cs, Val, StrVal, inc(Pos, 2)};
sub_scan_escape([$^]=_Cs, _Pos) ->
    more;
sub_scan_escape([$^|_Eof], _Pos) ->
    continue;
%% \nl
sub_scan_escape([$\n|Cs], Pos) ->
    Val = $\n,
    StrVal = [Val],
    {Cs, Val, StrVal, incrow(Pos)};
%% \X - familiar escape sequences
sub_scan_escape([$n=C|Cs], Pos) ->
    Val = escape_char(C),
    StrVal = [C],
    {Cs, Val, StrVal, incrow(Pos)};
sub_scan_escape([C|Cs], Pos) ->
    Val = escape_char(C),
    StrVal = [C],
    {Cs, Val, StrVal, inc(Pos, 1)};
%%
sub_scan_escape([], _Pos) ->
    more;
sub_scan_escape(_Eof, _Pos) ->
    continue.

escape_char($n) -> $\n;                %\n = LF
escape_char($r) -> $\r;                %\r = CR
escape_char($t) -> $\t;                %\t = TAB
escape_char($v) -> $\v;                %\v = VT
escape_char($b) -> $\b;                %\b = BS
escape_char($f) -> $\f;                %\f = FF
escape_char($e) -> $\e;                %\e = ESC
escape_char($s) -> $\s;                %\s = SPC
escape_char($d) -> $\d;                %\d = DEL
escape_char(C) -> C.

utf8_str_length(List) ->
   case get_length(List, 0) of
       -1 ->
          length(List);
       Int ->
          Int
    end.

get_length([], Len) ->
   Len;
get_length([C|Rest], Len) when C <16#80 ->
   get_length(Rest, Len +1);
get_length([C1,C2|Rest], Len) when C1 =< 16#FF, C2 =< 16#FF, C1 band 16#E0 =:= 16#C0, C2 band 16#C0 =:= 16#80 ->
   get_length(Rest, Len+1);
get_length([C1,_C2,_C3|Rest], Len) when C1 =< 16#FF, C1 band 16#F0 =:= 16#E0 ->
   get_length(Rest, Len +1);
get_length([C1,_C2,_C3,_C4|Rest], Len) when C1 =< 16#FF, C1 band 16#F8 =:= 16#F0 ->
   get_length(Rest, Len +1);
get_length(_Bad, _Len) ->
   -1.

scan_string([$"|Cs], Stack, Toks, Pos, State, Errors) ->
    [StartPos,$"|S] = reverse(Stack),
    {VS, SS} = unstack(S),
    Len = utf8_str_length(SS),
    %%io:format("the ss: ~p and length :~p ~n", [SS, Len]),
    scan(Cs, [], [{string,{StartPos, Len+2},VS,[$"|SS]++[$"]}|Toks], inc(Pos,Len+2), State, Errors);
scan_string([$\n|Cs]=_ACs, Stack, Toks, Pos, State, Errors) ->
    scan_string(Cs, [{$\n,"\n"}|Stack], Toks, incrow0(Pos), State, Errors);
%%     [StartPos, $"|S] = reverse(Stack),
%%     {VS, SS} = unstack(S),
%%     scan(ACs, [], [{string,{StartPos, length(SS)+1},VS,[$"|SS]}|Toks], inc(Pos,length(SS)+1), State, Errors);
scan_string([$\\|Cs], Stack, Toks, Pos, State, Errors) ->
    case sub_scan_escape(Cs, Pos) of
    {Rest, Val, StrVal, _NewPos} ->
        scan_string(Rest, [{Val, [$\\|StrVal]}|Stack], Toks, Pos, State, Errors);
    more ->
        more(Cs, Stack, Toks, Pos, State, Errors, fun scan_string/6);
    continue ->
        scan(Cs, Stack, Toks, Pos, State, Errors)
    end;
scan_string([C|Cs], Stack, Toks, Pos, State, Errors) ->
    scan_string(Cs, [{C,[C]}|Stack], Toks, Pos, State, Errors);
scan_string([], Stack, Toks, Pos, State, Errors) ->
    more([], Stack, Toks, Pos, State, Errors, fun scan_string/6);
scan_string(Eof, Stack, _Toks, Pos, State, Errors) ->
    [StartPos,$"|S] = reverse(Stack),
    {_VS, SS} = unstack(S),
    SS1 = string:substr(SS, 1, 16),
    done(Eof, [{{string,$",SS1},StartPos}|Errors], [], inc(Pos,length(Stack)), State).

scan_qatom([$'|Cs], Stack, Toks, Pos, State, Errors) ->
    [StartPos,$'|S] = reverse(Stack),
    {VS, SS} = unstack(S),
    case catch list_to_atom(VS) of
    A when is_atom(A) ->
        StrVal = [$'|SS]++[$'],
        scan(Cs, [], [{atom,{StartPos, length(StrVal)},A,StrVal}|Toks], inc(Pos,length(StrVal)), State, Errors);
    _ ->
        scan(Cs, [], Toks, Pos, State, [{{illegal,atom},StartPos}|Errors])
    end;
scan_qatom([$\n|Cs], Stack, Toks, Pos, State, Errors) ->
%    scan_qatom(Cs, [{$\n,"\n"}|Stack], Toks, incrow(Pos), State, Errors);
    [StartPos,$'|S] = reverse(Stack),
    {VS, SS} = unstack(S),
    case catch list_to_atom(VS) of
    A when is_atom(A) ->
        StrVal = [$'|SS],
        scan(Cs, [], [{atom,{StartPos, length(StrVal)},A,StrVal}|Toks], inc(Pos,length(StrVal)), State, Errors);
    _ ->
        scan(Cs, [], Toks, Pos, State, [{{illegal,atom},StartPos}|Errors])
    end;
scan_qatom([$\\|Cs], Stack, Toks, Pos, State, Errors) ->
    case sub_scan_escape(Cs, Pos) of
    {Rest, Val, StrVal, _NewPos} ->
        scan_qatom(Rest, [{Val, [$\\|StrVal]}|Stack], Toks, Pos, State, Errors);
    more ->
        more(Cs, Stack, Toks, Pos, State, Errors, fun scan_qatom/6);
    continue ->
        scan(Cs, Stack, Toks, Pos, State, Errors)
    end;
scan_qatom([C|Cs], Stack, Toks, Pos, State, Errors) ->
    scan_qatom(Cs, [{C,[C]}|Stack], Toks, Pos, State, Errors);
scan_qatom([], Stack, Toks, Pos, State, Errors) ->
    more([], Stack, Toks, Pos, State, Errors, fun scan_qatom/6);
scan_qatom(Eof, Stack, _Toks, Pos, State, Errors) ->
    [StartPos,$'|S] = reverse(Stack),
    {_VS, SS} = unstack(S),
    SS1 = string:substr(SS, 1, 16),
    done(Eof, [{{atom,$',SS1},StartPos}|Errors], [], inc(Pos,length(Stack)), State).

unstack(L) ->
    Fun = fun({V,S}, {VL, SL}) ->
          {[V|VL], [S|SL]}
      end,
    {VL, SL} = lists:foldl(Fun, {[], []}, L),
    {lists:reverse(VL), lists:flatten(lists:reverse(SL))}.


scan_number([$.,C|Cs], Stack, Toks, Pos, State, Errors) when C >= $0, C =< $9 ->
    scan_fraction(Cs, [C,$.|Stack], Toks, Pos, State, Errors);
scan_number([$.]=Cs, Stack, Toks, Pos, State, Errors) ->
    more(Cs, Stack, Toks, Pos, State, Errors, fun scan_number/6);
scan_number([C|Cs], Stack, Toks, Pos, State, Errors) when C >= $0, C =< $9 ->
    scan_number(Cs, [C|Stack], Toks, Pos, State, Errors);
scan_number([$#|Cs], Stack, Toks, Pos, State, Errors) ->
    case catch list_to_integer(reverse(Stack)) of
    B when is_integer(B), B >= 2, B =< 1+$Z-$A+10 ->
        scan_based_int(Cs, [B], Toks, Pos, State, Errors);
    B ->
        scan(Cs, [], Toks, Pos, State, [{{base,B},Pos}|Errors])
    end;
scan_number([], Stack, Toks, Pos, State, Errors) ->
    more([], Stack, Toks, Pos, State, Errors, fun scan_number/6);
scan_number(Cs, Stack, Toks, Pos, State, Errors) ->
    case catch list_to_integer(reverse(Stack)) of
    N when is_integer(N) ->
        scan(Cs, [], [{integer,{Pos, length(Stack)},N,reverse(Stack)}|Toks], inc(Pos,length(Stack)), State, Errors);
    _ ->
        scan(Cs, [], Toks, Pos, State, [{{illegal,integer},Pos}|Errors])
    end.

scan_based_int([C|Cs], [B|Stack], Toks, Pos, State, Errors)
  when C >= $0, C =< $9, C < $0+B ->
    scan_based_int(Cs, [B,C|Stack], Toks, Pos, State, Errors);
scan_based_int([C|Cs], [B|Stack], Toks, Pos, State, Errors)
  when C >= $A, B > 10, C < $A+B-10 ->
    scan_based_int(Cs, [B,C|Stack], Toks, Pos, State, Errors);
scan_based_int([C|Cs], [B|Stack], Toks, Pos, State, Errors)
  when C >= $a, B > 10, C < $a+B-10 ->
    scan_based_int(Cs, [B,C|Stack], Toks, Pos, State, Errors);
scan_based_int([], Stack, Toks, Pos, State, Errors) ->
    more([], Stack, Toks, Pos, State, Errors, fun scan_based_int/6);
scan_based_int(Cs, [B|Stack], Toks, Pos, State, Errors) ->
    case catch erlang:list_to_integer(reverse(Stack), B) of
    N when is_integer(N) ->
        Len = length(Stack)+1+length(integer_to_list(B)),
        scan(Cs, [], [{integer,{Pos, Len},N,integer_to_list(B)++[$#]++reverse(Stack)}|Toks],
         inc(Pos,Len), State, Errors);
    _ ->
        scan(Cs, [], Toks, Pos, State, [{{illegal,integer},Pos}|Errors])
    end.

scan_fraction([C|Cs], Stack, Toks, Pos, State, Errors) when C >= $0, C =< $9 ->
    scan_fraction(Cs, [C|Stack], Toks, Pos, State, Errors);
scan_fraction([$e|Cs], Stack, Toks, Pos, State, Errors) ->
    scan_exponent_sign(Cs, [$E|Stack], Toks, Pos, State, Errors);
scan_fraction([$E|Cs], Stack, Toks, Pos, State, Errors) ->
    scan_exponent_sign(Cs, [$E|Stack], Toks, Pos, State, Errors);
scan_fraction([], Stack, Toks, Pos, State, Errors) ->
    more([], Stack, Toks, Pos, State, Errors, fun scan_fraction/6);
scan_fraction(Cs, Stack, Toks, Pos, State, Errors) ->
    case catch list_to_float(reverse(Stack)) of
    F when is_float(F) ->
        scan(Cs, [], [{float,{Pos, length(Stack)},F,reverse(Stack)}|Toks], inc(Pos,length(Stack)), State, Errors);
    _ ->
        scan(Cs, [], Toks, Pos, State, [{{illegal,float},Pos}|Errors])
    end.

scan_exponent_sign([$+|Cs], Stack, Toks, Pos, State, Errors) ->
    scan_exponent(Cs, [$+|Stack], Toks, Pos, State, Errors);
scan_exponent_sign([$-|Cs], Stack, Toks, Pos, State, Errors) ->
    scan_exponent(Cs, [$-|Stack], Toks, Pos, State, Errors);
scan_exponent_sign([], Stack, Toks, Pos, State, Errors) ->
    more([], Stack, Toks, Pos, State, Errors, fun scan_exponent_sign/6);
scan_exponent_sign(Cs, Stack, Toks, Pos, State, Errors) ->
    scan_exponent(Cs, Stack, Toks, Pos, State, Errors).

scan_exponent([C|Cs], Stack, Toks, Pos, State, Errors) when C >= $0, C =< $9 ->
    scan_exponent(Cs, [C|Stack], Toks, Pos, State, Errors);
scan_exponent([], Stack, Toks, Pos, State, Errors) ->
    more([], Stack, Toks, Pos, State, Errors, fun scan_exponent/6);
scan_exponent(Cs, Stack, Toks, Pos, State, Errors) ->
    case catch list_to_float(reverse(Stack)) of
    F when is_float(F) ->
        scan(Cs, [], [{float,{Pos,length(Stack)},F,reverse(Stack)}|Toks], inc(Pos,length(Stack)), State, Errors);
    _ ->
        scan(Cs, [], Toks, Pos, State, [{{illegal,float},Pos}|Errors])
    end.


scan_comment([$\r|Cs], Stack, Toks, Pos, State, Errors) ->
    L = lists:reverse(Stack),
    Len = utf8_str_length(L),
    %%io:format("the comment :~p and length :~p ~n" , [L, Len]),
    scan([$\r|Cs], [], [{comment, {Pos, Len}, lists:reverse(Stack)}|Toks], inc(Pos,Len), State, Errors);
scan_comment([$\n|Cs], Stack, Toks, Pos, State, Errors) ->
    L = lists:reverse(Stack),
    Len = utf8_str_length(L),
    %%io:format("the comment :~p and length :~p ~n" , [L, Len]),
    scan([$\n|Cs], [], [{comment, {Pos, Len}, lists:reverse(Stack)}|Toks], inc(Pos, Len), State, Errors);
scan_comment([C|Cs], Stack, Toks, Pos, State, Errors) ->
    scan_comment(Cs, [C|Stack], Toks, Pos, State, Errors);
scan_comment([], Stack, Toks, Pos, State, Errors) ->
    more([], Stack, Toks, Pos, State, Errors, fun scan_comment/6);
scan_comment(Eof, Stack, Toks, Pos, State, Errors) ->
%    done(Eof, Errors, Toks, Pos, State).
    done(Eof, Errors, [{comment, {Pos, length(Stack)}, lists:reverse(Stack)}|Toks], inc(Pos,length(Stack)), State).


scan_dot([$%|_]=Cs, _Stack, Toks, Pos, State, Errors) ->
    done(Cs, Errors, [{dot,{Pos, 1}}|Toks], inc(Pos ,1), State);
scan_dot([C|Cs], _Stack, Toks, Pos, State, Errors) when C>=$\000, C=<$\s; C>=$\200, C=<$\240 ->
    done([C|Cs], Errors, [{dot,{Pos, 1}}|Toks], inc(Pos ,1), State);
scan_dot([], Stack, Toks, Pos, State, Errors) ->
    more([], Stack, Toks, Pos, State, Errors, fun scan_dot/6);
scan_dot(eof, _Stack, Toks, Pos, State, Errors) ->
    done(eof, Errors, [{dot,{Pos, 1}}|Toks], inc(Pos ,1), State);
scan_dot(Cs, Stack, Toks, Pos, State, Errors) ->
    scan(Cs, Stack, [{'.', {Pos, 1}}|Toks], inc(Pos ,1), State, Errors).


scan_white([C|Cs], Stack, Toks, Pos, State, Errors)
  when C >= $\000, C =< $\s, C =/= $\n; C >= $\200, C =< $\240 ->
    scan_white(Cs, [C|Stack], Toks, Pos, State, Errors);
scan_white([], Stack, Toks, Pos, State, Errors) ->
  scan([], [], [{ws, {Pos, length(Stack)}, lists:reverse(Stack)}|Toks], inc(Pos, length(Stack)), State, Errors);
scan_white(eof, _Stack, Toks, Pos, State, Errors) ->
    done(eof, Errors, Toks, Pos, State);
scan_white(Cs, Stack, Toks, Pos, State, Errors) ->
  scan(Cs, [], [{ws, {Pos, length(Stack)}, lists:reverse(Stack)}|Toks], inc(Pos, length(Stack)), State, Errors).

%% reserved_word(Atom) -> Bool
%%   return 'true' if Atom is an Erlang reserved word, else 'false'.

reserved_word('after') -> true;
reserved_word('begin') -> true;
reserved_word('case') -> true;
reserved_word('try') -> true;
reserved_word('cond') -> true;
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

%% comments are used only for highlighting
filter_comments(L) ->
    Fun = fun({comment, _, _}) -> false;
         (_) -> true
      end,
    lists:filter(Fun, L).

filter_ws(L) ->
    Fun = fun({ws, _, _}) -> false;
         (_) -> true
      end,
    lists:filter(Fun, L).

%% when we do syntax highlighting, we need a slightly different format
filter(L) ->
    lists:flatmap(fun({A, B}) ->
              [{A, B}];
             ({A, B, D}) ->
              [{A, B, to_token(D)}];
             ({A, B, _D, E}) ->
              [{A, B, E}]
          end, L).

%% when doing normal parsing
filter1(L) ->
    lists:flatmap(fun({A, B}) ->
              [{A, B}];
             ({A, B, D}) ->
              [{A, B, D}];
             ({A, B, D, _E}) ->
              [{A, B, D}]
          end, L).

to_token(X) when is_list(X) ->
    io_lib:write_string(X);
to_token(X) ->
    io_lib:write(X).

inc({L,P}, N) ->
    {L, P+N}.

incrow({L, P}) ->
    {L+1, P+1}.

incrow0({L, P}) ->
    {L+1, P}.

