%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

%% Erlang token scanning functions of io library.

%% For handling ISO 8859-1 (Latin-1) we use the following type
%% information:
%%
%% 000 - 037    NUL - US        control
%% 040 - 057    SPC - /         punctuation
%% 060 - 071    0 - 9           digit
%% 072 - 100    : - @           punctuation
%% 101 - 132    A - Z           uppercase
%% 133 - 140    [ - `           punctuation
%% 141 - 172    a - z           lowercase
%% 173 - 176    { - ~           punctuation
%% 177          DEL             control
%% 200 - 237                    control
%% 240 - 277    NBSP - ¿        punctuation
%% 300 - 326    À - Ö           uppercase
%% 327          ×               punctuation
%% 330 - 336    Ø - Þ           uppercase
%% 337 - 366    ß - ö           lowercase
%% 367          ÷               punctuation
%% 370 - 377    ø - ÿ           lowercase
%%
%% Many punctuation characters have special meaning:
%%  $\s, $_, $", $$, $%, $', $.
%% DEL is a punctuation.
%%
%% Must watch using × \327, very close to x \170.

-module(erlide_scan_new).

%-compile({inline,[white_space_end/7]}).

%%% External exports

-export([string/1,string/2,string/3,tokens/3,tokens/4,
         format_error/1,reserved_word/1,
         token_info/1,token_info/2]).

%%% Local record.
-record(erl_scan,
        {resword_fun=fun reserved_word/1,
         ws=false,
         comment=false}).

%%%
%%% Exported functions
%%%

%-spec format_error(Error::term()) -> string().
format_error({string,Quote,Head}) ->
    lists:flatten(["unterminated " ++ string_thing(Quote) ++
                   " starting with " ++ io_lib:write_string(Head,Quote)]);
format_error({illegal,Type}) ->
    lists:flatten(io_lib:fwrite("illegal ~w", [Type]));
format_error(char) -> "unterminated character";
format_error(scan) -> "premature end";
format_error({base,Base}) ->
    lists:flatten(io_lib:fwrite("illegal base '~w'", [Base]));
format_error(Other) ->
    lists:flatten(io_lib:write(Other)).

string(Cs) ->
    string(Cs, 1, []).

string(Chars, StartLocation) ->
    string(Chars, StartLocation, []).

%% Line numbers less than zero have always been allowed:
string(Chars, Line, Options) when is_integer(Line), is_list(Chars) ->
    string1(Chars, options(Options), Line, no_col, []);
string(Chars, {Line,Column}, Options)
    when is_integer(Line), is_integer(Column), Column >= 1, is_list(Chars) ->
    string1(Chars, options(Options), Line, Column, []).

tokens(Cont, Chars, StartLocation) ->
    tokens(Cont, Chars, StartLocation, []).

tokens([], Chars, Line, Options) when is_integer(Line), is_list(Chars) ->
    tokens1(Chars, options(Options), Line, no_col, [], fun scan/6, []);
tokens([], Chars, {Line,Column}, Options)
    when is_integer(Line), is_integer(Column), Column >= 1, is_list(Chars) ->
    tokens1(Chars, options(Options), Line, Column, [], fun scan/6, []);
tokens({Cs,St,Line,Col,Toks,Fun,Any}, Chars, _Loc, _Opts)
    when is_list(Chars); Chars =:= eof ->
    tokens1(Cs++Chars, St, Line, Col, Toks, Fun, Any).

token_info(Token) ->
    Tags = [category,column,length,line,symbol,text], % sorted
    token_info(Token, Tags).

token_info(_Attrs, []) ->
    [];
token_info(Attrs, [Tag|Tags]) when is_atom(Tag) ->
    case token_info(Attrs, Tag) of
        undefined ->
            token_info(Attrs, Tags);
        TokenInfo when is_tuple(TokenInfo) ->
            [token_info(Attrs, Tag)| token_info(Attrs, Tags)]
    end;
token_info({Category,_Attrs}, category=Tag) ->
    {Tag,Category};
token_info({Category,_Attrs,_Symbol}, category=Tag) ->
    {Tag,Category};
token_info({Category,_Attrs}, symbol=Tag) ->
    {Tag,Category};
token_info({_Category,_Attrs,Symbol}, symbol=Tag) ->
    {Tag,Symbol};
token_info({_Category,Attrs}, Tag) ->
    token_info1(Attrs, Tag);
token_info({_Category,Attrs,_Symbol}, Tag) ->
    token_info1(Attrs, Tag).

%%%
%%% Local functions
%%%

string_thing($') -> "atom";   %' Stupid Emacs
string_thing(_) -> "string".

-define(WHITE_SPACE(C), C >= $\000, C =< $\s; C >= $\200, C =< $\240).
-define(DIGIT(C), C >= $0, C =< $9).

-define(STR(Col, S), if Col =:= no_col -> []; true -> S end).

options(Opts0) when is_list(Opts0) ->
    Opts = lists:foldr(fun expand_opt/2, [], Opts0),
    [RW_fun] =
        case opts(Opts, [reserved_word_fun], []) of
            badarg ->
                erlang:error(badarg, [Opts0]);
            R ->
                R
        end,
    Comment = proplists:get_bool(return_comments, Opts),
    WS = proplists:get_bool(return_white_spaces, Opts),
    #erl_scan{resword_fun = RW_fun,
              comment     = Comment,
              ws          = WS};
options(Opt) ->
    options([Opt]).

opts(Options, [Key|Keys], L) ->
    V = case lists:keysearch(Key, 1, Options) of
            {value, {reserved_word_fun, F}} when is_function(F, 1),
                                                 is_function(F) ->
                {ok, F};
            {value, {Key, _}} ->
                badarg;
            false ->
                {ok, default_option(Key)}
        end,
    case V of
        badarg ->
            badarg;
        {ok, Value} ->
            opts(Options, Keys, [Value | L])
    end;
opts(_Options, [], L) ->
    lists:reverse(L).

default_option(reserved_word_fun) ->
    fun reserved_word/1.

expand_opt(return, Os) ->
    [return_comments,return_white_spaces|Os];
expand_opt(O, Os) ->
    [O|Os].

token_info1(Attrs, column=Tag) when size(Attrs) >= 2 ->
    {Tag,element(2, Attrs)};
token_info1(_Attrs, column) ->
    undefined;
token_info1(Attrs, length=Tag) when size(Attrs) >= 3 ->
    {Tag,element(3, Attrs)};
token_info1(_Attrs, length) ->
    undefined;
token_info1(Line, line=Tag) when is_integer(Line) ->
    {Tag,Line};
token_info1(Attrs, line=Tag) when size(Attrs) >= 1 ->
    {Tag,element(1, Attrs)};
token_info1(Attrs, location=Tag) when size(Attrs) >= 2 ->
    {Tag,{element(1, Attrs),element(2, Attrs)}};
token_info1(Line, location=Tag) when is_integer(Line) ->
    {Tag,Line};
token_info1(Attrs, text=Tag) when size(Attrs) >= 4 ->
    {Tag,element(4, Attrs)};
token_info1(_Attrs, text) ->
    undefined;
token_info1(Term1, Term2) ->
    erlang:error(badarg, [Term1,Term2]).

tokens1(Cs, St, Line, Col, Toks, Fun, Any) ->
    case Fun(Cs, St, Line, Col, Toks, Any) of
        {more,{Cs0,Nline,Ncol,Ntoks,Nfun,Nany}} ->
            {more,{Cs0,St,Nline,Ncol,Ntoks,Nfun,Nany}};
        {ok,Toks0,eof,Nline,Ncol} ->
            Res = if
                      Toks0 =:= [] ->
                          {eof,location(Nline, Ncol)};
                      true ->
                          scan_error(scan, Line, Col, Nline, Ncol)
                  end,
            {done,Res,[]};
        {ok,Toks0,Rest,Nline,Ncol} ->
            {done,{ok,lists:reverse(Toks0),location(Nline, Ncol)},Rest};
        {error,_,_}=Error ->
            {done,Error,[]}
    end.

string1(Cs, St, Line, Col, Toks) ->
    case scan1(Cs, St, Line, Col, Toks) of
        {more, {Cs0,Nline,Ncol,Ntoks,Fun,Any}} ->
            case Fun(Cs0++eof, St, Nline, Ncol, Ntoks, Any) of
                {ok,Toks1,_Rest,Line2,Col2} ->
                    {ok,lists:reverse(Toks1),location(Line2, Col2)};
                {error,_,_}=Error ->
                    Error
            end;
        {ok,Ntoks,[],Nline,Ncol} ->
            {ok,lists:reverse(Ntoks),location(Nline, Ncol)};
        {ok,Ntoks,eof,Nline,Ncol} ->
            {ok,lists:reverse(Ntoks),location(Nline, Ncol)};
        {ok,Ntoks,Rest,Nline,Ncol} ->
            string1(Rest, St, Nline, Ncol, Ntoks);
        {error,_,_}=Error ->
            Error
    end.

scan(Cs, St, Line, Col, Toks, _) ->
    scan1(Cs, St, Line, Col, Toks).

scan1([$\s|Cs], St, Line, Col, Toks) when St#erl_scan.ws ->
    scan_spcs(Cs, St, Line, Col, Toks, 1);
scan1([$\s|Cs], St, Line, Col, Toks) ->
    skip_white_space(Cs, St, Line, Col, Toks, 1);
scan1([$\n|Cs], St, Line, Col, Toks) when St#erl_scan.ws ->
    scan_newline(Cs, St, Line, Col, Toks);
scan1([$\n|Cs], St, Line, Col, Toks) ->
    skip_white_space(Cs, St, Line+1, new_column(Col, 1), Toks, 0);
scan1([C|Cs], St, Line, Col, Toks) when C >= $A, C =< $Z ->
    scan_variable(Cs, St, Line, Col, Toks, [C]);
scan1([C|Cs], St, Line, Col, Toks) when C >= $a, C =< $z ->
    scan_atom(Cs, St, Line, Col, Toks, [C]);
%% Optimization: some very common punctuation characters:
scan1([$,|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, ",", ',', 1);
scan1([$(|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "(", '(', 1);
scan1([$)|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, ")", ')', 1);
scan1([${|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "{", '{', 1);
scan1([$}|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "}", '}', 1);
scan1([$[|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "[", '[', 1);
scan1([$]|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "]", ']', 1);
scan1([$;|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, ";", ';', 1);
scan1([$_=C|Cs], St, Line, Col, Toks) ->
    scan_variable(Cs, St, Line, Col, Toks, [C]);
%% More punctuation characters below.
scan1([$\%|Cs], St, Line, Col, Toks) when not St#erl_scan.comment ->
    skip_comment(Cs, St, Line, Col, Toks, 1);
scan1([$\%=C|Cs], St, Line, Col, Toks) ->
    scan_comment(Cs, St, Line, Col, Toks, [C]);
scan1([C|Cs], St, Line, Col, Toks) when ?DIGIT(C) ->
    scan_number(Cs, St, Line, Col, Toks, [C]);
scan1([$.=C|Cs], St, Line, Col, Toks) ->
    scan_dot(Cs, St, Line, Col, Toks, [C]);
scan1([$"|Cs], St, Line, Col, Toks) -> %" Emacs
    scan_string(Cs, St, Line, incr_column(Col, 1), Toks, {[],[],Line,Col});
scan1([$'|Cs], St, Line, Col, Toks) -> %' Emacs
    scan_qatom(Cs, St, Line, incr_column(Col, 1), Toks, {[],[],Line,Col});
scan1([$$|Cs], St, Line, Col, Toks) ->
    scan_char(Cs, St, Line, Col, Toks);
scan1([$\r|Cs], St, Line, Col, Toks) when St#erl_scan.ws ->
    white_space_end(Cs, St, Line, Col, Toks, 1, "\r");
scan1([C|Cs], St, Line, Col, Toks) when C >= $ß, C =< $ÿ, C =/= $÷ ->
    scan_atom(Cs, St, Line, Col, Toks, [C]);
scan1([C|Cs], St, Line, Col, Toks) when C >= $À, C =< $Þ, C /= $× ->
    scan_variable(Cs, St, Line, Col, Toks, [C]);
scan1([$\t|Cs], St, Line, Col, Toks) when St#erl_scan.ws ->
    scan_tabs(Cs, St, Line, Col, Toks, 1);
scan1([$\t|Cs], St, Line, Col, Toks) ->
    skip_white_space(Cs, St, Line, Col, Toks, 1);
scan1([C|Cs], St, Line, Col, Toks) when ?WHITE_SPACE(C) ->
    case St#erl_scan.ws of
        true ->
            scan_white_space(Cs, St, Line, Col, Toks, [C]);
        false ->
            skip_white_space(Cs, St, Line, Col, Toks, 1)
    end;
%% Punctuation characters and operators, first recognise multiples.
%% << <- <=
scan1("<<"++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "<<", '<<', 2);
scan1("<-"++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "<-", '<-', 2);
scan1("<="++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "<=", '<=', 2);
scan1("<"=Cs, _St, Line, Col, Toks) ->
    {more, {Cs,Line,Col,Toks,fun scan/6,[]}};
%% >> >=
scan1(">>"++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, ">>", '>>', 2);
scan1(">="++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, ">=", '>=', 2);
scan1(">"=Cs, _St, Line, Col, Toks) ->
    {more, {Cs,Line,Col,Toks,fun scan/6,[]}};
%% -> --
scan1("->"++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "->", '->', 2);
scan1("--"++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "--", '--', 2);
scan1("-"=Cs, _St, Line, Col, Toks) ->
    {more, {Cs,Line,Col,Toks,fun scan/6,[]}};
%% ++
scan1("++"++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "++", '++', 2);
scan1("+"=Cs, _St, Line, Col, Toks) ->
    {more, {Cs,Line,Col,Toks,fun scan/6,[]}};
%% =:= =/= =< ==
scan1("=:="++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "=:=", '=:=', 3);
scan1("=:"=Cs, _St, Line, Col, Toks) ->
    {more, {Cs,Line,Col,Toks,fun scan/6,[]}};
scan1("=/="++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "=/=", '=/=', 3);
scan1("=/"=Cs, _St, Line, Col, Toks) ->
    {more, {Cs,Line,Col,Toks,fun scan/6,[]}};
scan1("=<"++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "=<", '=<', 2);
scan1("=="++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "==", '==', 2);
scan1("="=Cs, _St, Line, Col, Toks) ->
    {more, {Cs,Line,Col,Toks,fun scan/6,[]}};
%% /=
scan1("/="++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "/=", '/=', 2);
scan1("/"=Cs, _St, Line, Col, Toks) ->
    {more, {Cs,Line,Col,Toks,fun scan/6,[]}};
%% ||
scan1("||"++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "||", '||', 2);
scan1("|"=Cs, _St, Line, Col, Toks) ->
    {more, {Cs,Line,Col,Toks,fun scan/6,[]}};
%% :-
scan1(":-"++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, ":-", ':-', 2);
%% :: for typed records
scan1("::"++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "::", '::', 2);
scan1(":"=Cs, _St, Line, Col, Toks) ->
    {more, {Cs,Line,Col,Toks,fun scan/6,[]}};
%% Optimization: some quite common punctuation characters:
scan1([$=|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "=", '=', 1);
scan1([$||Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "|", '|', 1);
scan1([$!|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "!", '!', 1);
%% End of optimization. Not recognized less than 127: #&*+-/:<>?@\^`~
scan1([C|Cs], St, Line, Col, Toks) when C >= 0 ->
    Str = [C],
    tok2(Cs, St, Line, Col, Toks, Str, list_to_atom(Str), 1);
scan1([]=Cs, _St, Line, Col, Toks) ->
    {more, {Cs,Line,Col,Toks,fun scan/6,[]}};
scan1(eof=Cs, _St, Line, Col, Toks) ->
    {ok,Toks,Cs,Line,Col}.

scan_atom(Cs0, St, Line, Col, Toks, Ncs0) ->
    case scan_name(Cs0, Ncs0) of
        {more, Ncs} ->
            {more, {[],Line,Col,Toks,fun scan_atom/6,Ncs}};
        {Wcs,Cs} ->
            case catch list_to_atom(Wcs) of
                Name when is_atom(Name) ->
                    case (St#erl_scan.resword_fun)(Name) of
                        true ->
                            tok2(Cs, St, Line, Col, Toks, Wcs, Name);
                        false ->
                            tok3(Cs, St, Line, Col, Toks, atom, Wcs, Name)
                    end;
                _Error ->
                    Ncol = incr_column(Col, length(Wcs)),
                    scan_error({illegal,atom}, Line, Col, Line, Ncol)
            end
    end.

scan_variable(Cs0, St, Line, Col, Toks, Ncs0) ->
    case scan_name(Cs0, Ncs0) of
        {more, Ncs} ->
            {more, {[],Line,Col,Toks,fun scan_variable/6,Ncs}};
        {Wcs,Cs} ->
            case catch list_to_atom(Wcs) of
                Name when is_atom(Name) ->
                    tok3(Cs, St, Line, Col, Toks, var, Wcs, Name);
                _Error ->
                    Ncol = incr_column(Col, length(Wcs)),
                    scan_error({illegal,var}, Line, Col, Line, Ncol)
            end
    end.

scan_name([C|Cs], Ncs) when C >= $a, C =< $z ->
    scan_name(Cs, [C|Ncs]);
scan_name([C|Cs], Ncs) when C >= $A, C =< $Z ->
    scan_name(Cs, [C|Ncs]);
scan_name([$_=C|Cs], Ncs) ->
    scan_name(Cs, [C|Ncs]);
scan_name([C|Cs], Ncs) when ?DIGIT(C) ->
    scan_name(Cs, [C|Ncs]);
scan_name([$@=C|Cs], Ncs) ->
    scan_name(Cs, [C|Ncs]);
scan_name([C|Cs], Ncs) when C >= $ß, C =< $ÿ, C =/= $÷ ->
    scan_name(Cs, [C|Ncs]);
scan_name([C|Cs], Ncs) when C >= $À, C =< $Þ, C =/= $× ->
    scan_name(Cs, [C|Ncs]);
scan_name([], Ncs) ->
    {more,Ncs};
scan_name(Cs, Ncs) ->
    {lists:reverse(Ncs),Cs}.

scan_dot([C|_]=Cs, _St, Line, Col, Toks, Ncs) when C =:= $%; C =:= $\n ->
    Attrs = attributes(Line, Col, Ncs),
    {ok,[{dot,Attrs}|Toks],Cs,Line,incr_column(Col, 1)};
scan_dot([C|Cs], _St, Line, Col, Toks, Ncs) when ?WHITE_SPACE(C) ->
    Attrs = attributes(Line, Col, Ncs++[C]),
    {ok,[{dot,Attrs}|Toks],Cs,Line,incr_column(Col, 2)};
scan_dot([]=Cs, _St, Line, Col, Toks, Ncs) ->
    {more, {Cs,Line,Col,Toks,fun scan_dot/6,Ncs}};
scan_dot(eof=Cs, _St, Line, Col, Toks, Ncs) ->
    Attrs = attributes(Line, Col, Ncs),
    {ok,[{dot,Attrs}|Toks],Cs,Line,incr_column(Col, 1)};
scan_dot(Cs, St, Line, Col, Toks, Ncs) ->
    tok2(Cs, St, Line, Col, Toks, Ncs, '.', 1).

%%% White space characters are very common, so it is worthwhile to
%%% scan them fast and store them compactly. (The words "whitespace"
%%% and "white space" usually mean the same thing. The Erlang
%%% specification denotes the characters with ASCII code in the
%%% interval 0 to 32 as "white space".)
%%%
%%% Convention: if there is a white newline ($\n) it will always be
%%% the first character in the text string. As a consequence, there
%%% cannot be more than one newline in a white_space token string.
%%%
%%% Some common combinations are recognized, some are not. Examples
%%% of the latter are tab(s) followed by space(s), like "\t  ".
%%% (They will be represented by two (or more) tokens.)
%%%
%%% Note: the character sequence "\r\n" is *not* recognized since it
%%% would violate the property that $\n will always be the first
%%% character. (But since "\r\n\r\n" is common, it pays off to
%%% recognize "\n\r".)

scan_newline([$\s|Cs], St, Line, Col, Toks) ->
    scan_nl_spcs(Cs, St, Line, Col, Toks, 2);
scan_newline([$\t|Cs], St, Line, Col, Toks) ->
    scan_nl_tabs(Cs, St, Line, Col, Toks, 2);
scan_newline([$\r|Cs], St, Line, Col, Toks) ->
    newline_end(Cs, St, Line, Col, Toks, 2, "\n\r");
scan_newline([$\f|Cs], St, Line, Col, Toks) ->
    newline_end(Cs, St, Line, Col, Toks, 2, "\n\f");
scan_newline([], _St, Line, Col, Toks) ->
    {more, {"\n",Line,Col,Toks,fun scan/6,[]}};
scan_newline(Cs, St, Line, Col, Toks) ->
    scan_nl_white_space(Cs, St, Line, Col, Toks, "\n").

scan_nl_spcs([$\s|Cs], St, Line, Col, Toks, N) when N < 17 ->
    scan_nl_spcs(Cs, St, Line, Col, Toks, N+1);
scan_nl_spcs([]=Cs, _St, Line, Col, Toks, N) ->
    {more, {Cs,Line,Col,Toks,fun scan_nl_spcs/6,N}};
scan_nl_spcs(Cs, St, Line, Col, Toks, N) ->
    newline_end(Cs, St, Line, Col, Toks, N, nl_spcs(N)).

scan_nl_tabs([$\t|Cs], St, Line, Col, Toks, N) when N < 11 ->
    scan_nl_tabs(Cs, St, Line, Col, Toks, N+1);
scan_nl_tabs([]=Cs, _St, Line, Col, Toks, N) ->
    {more, {Cs,Line,Col,Toks,fun scan_nl_tabs/6,N}};
scan_nl_tabs(Cs, St, Line, Col, Toks, N) ->
    newline_end(Cs, St, Line, Col, Toks, N, nl_tabs(N)).

%% Note: returning {more,Cont} is meaningless here; one could just as
%% well return several tokens. But since tokens() scans up to a full
%% stop anyway, nothing is gained by not collecting all white spaces.
scan_nl_white_space([$\n|Cs], St, Line, no_col=Col, Toks0, Ncs) ->
    Toks = [{white_space,Line,lists:reverse(Ncs)}|Toks0],
    scan_newline(Cs, St, Line+1, Col, Toks);
scan_nl_white_space([$\n|Cs], St, Line, Col, Toks, Ncs0) ->
    Ncs = lists:reverse(Ncs0),
    N = length(Ncs),
    Attrs = {Line,Col,N,Ncs},
    scan_newline(Cs, St, Line+1, N, [{white_space,Attrs,Ncs}|Toks]);
scan_nl_white_space([C|Cs], St, Line, Col, Toks, Ncs) when ?WHITE_SPACE(C) ->
    scan_nl_white_space(Cs, St, Line, Col, Toks, [C|Ncs]);
scan_nl_white_space([]=Cs, _St, Line, Col, Toks, Ncs) ->
    {more, {Cs,Line,Col,Toks,fun scan_nl_white_space/6,Ncs}};
scan_nl_white_space(Cs, St, Line, no_col=Col, Toks, Ncs) ->
    scan1(Cs, St, Line+1, Col, [{white_space,Line,lists:reverse(Ncs)}|Toks]);
scan_nl_white_space(Cs, St, Line, Col, Toks, Ncs0) ->
    Ncs = lists:reverse(Ncs0),
    N = length(Ncs),
    Attrs = {Line,Col,N,Ncs},
    scan1(Cs, St, Line+1, N, [{white_space,Attrs,Ncs}|Toks]).

newline_end(Cs, St, Line, no_col=Col, Toks, _N, Ncs) ->
    scan1(Cs, St, Line+1, Col, [{white_space,Line,Ncs}|Toks]);
newline_end(Cs, St, Line, Col, Toks, N, Ncs) ->
    Attrs = {Line,Col,N,Ncs},
    scan1(Cs, St, Line+1, N, [{white_space,Attrs,Ncs}|Toks]).

scan_spcs([$\s|Cs], St, Line, Col, Toks, N) when N < 16 ->
    scan_spcs(Cs, St, Line, Col, Toks, N+1);
scan_spcs([]=Cs, _St, Line, Col, Toks, N) ->
    {more, {Cs,Line,Col,Toks,fun scan_spcs/6,N}};
scan_spcs(Cs, St, Line, Col, Toks, N) ->
    white_space_end(Cs, St, Line, Col, Toks, N, spcs(N)).

scan_tabs([$\t|Cs], St, Line, Col, Toks, N) when N < 10 ->
    scan_tabs(Cs, St, Line, Col, Toks, N+1);
scan_tabs([]=Cs, _St, Line, Col, Toks, N) ->
    {more, {Cs,Line,Col,Toks,fun scan_tabs/6,N}};
scan_tabs(Cs, St, Line, Col, Toks, N) ->
    white_space_end(Cs, St, Line, Col, Toks, N, tabs(N)).

skip_white_space([$\n|Cs], St, Line, Col, Toks, _N) ->
    skip_white_space(Cs, St, Line+1, new_column(Col, 1), Toks, 0);
skip_white_space([C|Cs], St, Line, Col, Toks, N) when ?WHITE_SPACE(C) ->
    skip_white_space(Cs, St, Line, Col, Toks, N+1);
skip_white_space([]=Cs, _St, Line, Col, Toks, N) ->
    {more, {Cs,Line,Col,Toks,fun skip_white_space/6,N}};
skip_white_space(Cs, St, Line, Col, Toks, N) ->
    scan1(Cs, St, Line, incr_column(Col, N), Toks).

%% Maybe \t and \s should break the loop.
scan_white_space([$\n|_]=Cs, St, Line, Col, Toks, Ncs) ->
    white_space_end(Cs, St, Line, Col, Toks, length(Ncs), lists:reverse(Ncs));
scan_white_space([C|Cs], St, Line, Col, Toks, Ncs) when ?WHITE_SPACE(C) ->
    scan_white_space(Cs, St, Line, Col, Toks, [C|Ncs]);
scan_white_space([]=Cs, _St, Line, Col, Toks, Ncs) ->
    {more, {Cs,Line,Col,Toks,fun scan_white_space/6,Ncs}};
scan_white_space(Cs, St, Line, Col, Toks, Ncs) ->
    white_space_end(Cs, St, Line, Col, Toks, length(Ncs), lists:reverse(Ncs)).

white_space_end(Cs, St, Line, Col, Toks, N, Ncs) ->
    tok3(Cs, St, Line, Col, Toks, white_space, Ncs, Ncs, N).

scan_char([$\\|Cs]=Cs0, St, Line, Col, Toks) ->
    case scan_escape(Cs, incr_column(Col, 2)) of
        more ->
            {more, {[$$|Cs0],Line,Col,Toks,fun scan/6,[]}};
        {eof,Ncol} ->
            scan_error(char, Line, Col, Line, Ncol);
        {nl,Val,Str,Ncs,Ncol} ->
            Attrs = attributes(Line, Col, ?STR(Ncol, "$\\"++Str)),
            Ntoks = [{char,Attrs,Val}|Toks],
            scan1(Ncs, St, Line+1, Ncol, Ntoks);
        {Val,Str,Ncs,Ncol} ->
            Attrs = attributes(Line, Col, ?STR(Ncol, "$\\"++Str)),
            Ntoks = [{char,Attrs,Val}|Toks],
            scan1(Ncs, St, Line, Ncol, Ntoks)
    end;
scan_char([$\n=C|Cs], St, Line, Col, Toks) ->
    Attrs = attributes(Line, Col, ?STR(Col, [$$,C]), 2),
    scan1(Cs, St, Line+1, new_column(Col, 1), [{char,Attrs,C}|Toks]);
scan_char([C|Cs], St, Line, Col, Toks) when C >= 0 ->
    Attrs = attributes(Line, Col, ?STR(Col, [$$,C]), 2),
    scan1(Cs, St, Line, incr_column(Col, 2), [{char,Attrs,C}|Toks]);
scan_char([], _St, Line, Col, Toks) ->
    {more, {[$$],Line,Col,Toks,fun scan/6,[]}};
scan_char(eof, _St, Line, Col, _Toks) ->
    scan_error(char, Line, Col, Line, incr_column(Col, 1)).

scan_string(Cs, St, Line, Col, Toks, {Wcs,Str,Line0,Col0}) ->
    case scan_string0(Cs, Line, Col, $\", Str, Wcs) of
        {Ncs,Nline,Ncol,Nstr,Nwcs} ->
            Attrs = attributes(Line0, Col0, Nstr),
            scan1(Ncs, St, Nline, Ncol, [{string,Attrs,Nwcs}|Toks]);
        {more,Ncs,Nline,Ncol,Nstr,Nwcs} ->
            State = {Nwcs,Nstr,Line0,Col0},
            {more,{Ncs,Nline,Ncol,Toks,fun scan_string/6,State}};
        {error,Nline,Ncol,Nwcs} ->
            Estr = string:substr(Nwcs, 1, 16), % Expanded escape chars.
            scan_error({string,$\",Estr}, Line0, Col0, Nline, Ncol)
    end.

scan_qatom(Cs, St, Line, Col, Toks, {Wcs,Str,Line0,Col0}) ->
    case scan_string0(Cs, Line, Col, $\', Str, Wcs) of
        {Ncs,Nline,Ncol,Nstr,Nwcs} ->
            case catch list_to_atom(Nwcs) of
                A when is_atom(A) ->
                    Attrs = attributes(Line0, Col0, Nstr),
                    scan1(Ncs, St, Nline, Ncol, [{atom,Attrs,A}|Toks]);
                _ ->
                    scan_error({illegal,atom}, Line0, Col0, Nline, Ncol)
            end;
        {more,Ncs,Nline,Ncol,Nstr,Nwcs} ->
            State = {Nwcs,Nstr,Line0,Col0},
            {more,{Ncs,Nline,Ncol,Toks,fun scan_qatom/6,State}};
        {error,Nline,Ncol,Nwcs} ->
            Estr = string:substr(Nwcs, 1, 16), % Expanded escape chars.
            scan_error({string,$\',Estr}, Line0, Col0, Nline, Ncol)
    end.

scan_string0(Cs, Line, no_col=Col, Q, [], Wsc) ->
    scan_string_no_col(Cs, Line, Col, Q, Wsc);
scan_string0(Cs, Line, Col, Q, [], Wsc) ->
    scan_string_col(Cs, Line, Col, Q, Wsc);
scan_string0(Cs, Line, Col, Q, Str, Wcs) ->
    scan_string1(Cs, Line, Col, Q, Str, Wcs).

%% Optimization. Col =:= no_col.
scan_string_no_col([Q|Cs], Line, Col, Q, Wcs) ->
    {Cs,Line,Col,_DontCare=[],lists:reverse(Wcs)};
scan_string_no_col([$\n=C|Cs], Line, Col, Q, Wcs) ->
    scan_string_no_col(Cs, Line+1, Col, Q, [C|Wcs]);
scan_string_no_col([C|Cs], Line, Col, Q, Wcs) when C =/= $\\, C >= 0 ->
    scan_string_no_col(Cs, Line, Col, Q, [C|Wcs]);
scan_string_no_col(Cs, Line, Col, Q, Wcs) ->
    scan_string1(Cs, Line, Col, Q, Wcs, Wcs).

%% Optimization. Col =/= no_col.
scan_string_col([Q|Cs], Line, Col, Q, Wcs0) ->
    Wcs = lists:reverse(Wcs0),
    Str = [Q|Wcs++[Q]],
    {Cs,Line,Col+1,Str,Wcs};
scan_string_col([$\n=C|Cs], Line, _xCol, Q, Wcs) ->
    scan_string_col(Cs, Line+1, 1, Q, [C|Wcs]);
scan_string_col([C|Cs], Line, Col, Q, Wcs) when C =/= $\\, C >= 0 ->
    scan_string_col(Cs, Line, Col+1, Q, [C|Wcs]);
scan_string_col(Cs, Line, Col, Q, Wcs) ->
    scan_string1(Cs, Line, Col, Q, Wcs, Wcs).

scan_string1([Q|Cs], Line, Col, Q, Str0, Wcs0) ->
    Wcs = lists:reverse(Wcs0),
    Str = ?STR(Col, [Q|lists:reverse(Str0, [Q])]),
    {Cs,Line,incr_column(Col, 1),Str,Wcs};
scan_string1([$\n=C|Cs], Line, Col, Q, Str, Wcs) ->
    Ncol = new_column(Col, 1),
    scan_string1(Cs, Line+1, Ncol, Q, ?STR(Col, [C|Str]), [C|Wcs]);
scan_string1([$\\|Cs]=Cs0, Line, Col, Q, Str, Wcs) ->
    case scan_escape(Cs, Col) of
        more ->
            {more,Cs0,Line,Col,Str,Wcs};
        {eof,Ncol} ->
            {error,Line,incr_column(Ncol, 1),lists:reverse(Wcs)};
        {nl,Val,ValStr,Ncs,Ncol} ->
            Nstr = ?STR(Ncol, lists:reverse(ValStr, [$\\|Str])),
            Ntoks = [Val|Wcs],
            scan_string1(Ncs, Line+1, Ncol, Q, Nstr, Ntoks);
        {Val,ValStr,Ncs,Ncol} ->
            Nstr = ?STR(Ncol, lists:reverse(ValStr, [$\\|Str])),
            Ntoks = [Val|Wcs],
            scan_string1(Ncs, Line, incr_column(Ncol, 1), Q, Nstr, Ntoks)
    end;
scan_string1([C|Cs], Line, no_col=Col, Q, Str, Wcs) when C >= 0 ->
    scan_string1(Cs, Line, Col, Q, Str, [C|Wcs]);
scan_string1([C|Cs], Line, Col, Q, Str, Wcs) when C >= 0 ->
    scan_string1(Cs, Line, Col+1, Q, [C|Str], [C|Wcs]);
scan_string1([]=Cs, Line, Col, _Q, Str, Wcs) ->
    {more,Cs,Line,Col,Str,Wcs};
scan_string1(eof, Line, Col, _Q, _Str, Wcs) ->
    {error,Line,Col,lists:reverse(Wcs)}.

-define(OCT(C), C >= $0, C =< $7).

%% \<1-3> octal digits
scan_escape([O1,O2,O3|Cs], Col) when ?OCT(O1), ?OCT(O2), ?OCT(O3) ->
    Val = (O1*8 + O2)*8 + O3 - 73*$0,
    {Val,?STR(Col, [O1,O2,O3]),Cs,incr_column(Col, 3)};
scan_escape([O1,O2], _Col) when ?OCT(O1), ?OCT(O2) ->
    more;
scan_escape([O1,O2|Cs], Col) when ?OCT(O1), ?OCT(O2) ->
    Val = (O1*8 + O2) - 9*$0,
    {Val,?STR(Col, [O1,O2]),Cs,incr_column(Col, 2)};
scan_escape([O1], _Col) when ?OCT(O1) ->
    more;
scan_escape([O1|Cs], Col) when ?OCT(O1) ->
    {O1 - $0,?STR(Col, [O1]),Cs,incr_column(Col, 1)};
%% \^X -> CTL-X
scan_escape([$^=C0,$\n=C|Cs], Col) ->
    {nl,C,?STR(Col, [C0,C]),Cs,new_column(Col, 1)};
scan_escape([$^=C0,C|Cs], Col) when C >= 0 ->
    Val = C band 31,
    {Val,?STR(Col, [C0,C]),Cs,incr_column(Col, 2)};
scan_escape([$^], _Col) ->
    more;
scan_escape([$^|eof], Col) ->
    {eof,incr_column(Col, 1)};
scan_escape([$\n=C|Cs], Col) ->
    {nl,C,?STR(Col, [C]),Cs,new_column(Col, 1)};
scan_escape([C0|Cs], Col) when C0 >= 0 ->
    C = escape_char(C0),
    {C,?STR(Col, [C0]),Cs,incr_column(Col, 1)};
scan_escape([], _Col) ->
    more;
scan_escape(eof, Col) ->
    {eof,Col}.

escape_char($n) -> $\n;                         % \n = LF
escape_char($r) -> $\r;                         % \r = CR
escape_char($t) -> $\t;                         % \t = TAB
escape_char($v) -> $\v;                         % \v = VT
escape_char($b) -> $\b;                         % \b = BS
escape_char($f) -> $\f;                         % \f = FF
escape_char($e) -> $\e;                         % \e = ESC
escape_char($s) -> $\s;                         % \s = SPC
escape_char($d) -> $\d;                         % \d = DEL
escape_char(C) -> C.

scan_number([C|Cs], St, Line, Col, Toks, Ncs) when ?DIGIT(C) ->
    scan_number(Cs, St, Line, Col, Toks, [C|Ncs]);
scan_number([$.,C|Cs], St, Line, Col, Toks, Ncs) when ?DIGIT(C) ->
    scan_fraction(Cs, St, Line, Col, Toks, [C,$.|Ncs]);
scan_number([$.]=Cs, _St, Line, Col, Toks, Ncs) ->
    {more, {Cs,Line,Col,Toks,fun scan_number/6,Ncs}};
scan_number([$#|Cs], St, Line, Col, Toks, Ncs0) ->
    Ncs = lists:reverse(Ncs0),
    case catch list_to_integer(Ncs) of
        B when is_integer(B), B >= 2, B =< 1+$Z-$A+10 ->
            Bcs = ?STR(Col, Ncs++[$#]),
            scan_based_int(Cs, St, Line, Col, Toks, {B,[],Bcs});
        B ->
            Len = length(Ncs),
            scan_error({base,B}, Line, Col, Line, incr_column(Col, Len))
    end;
scan_number([]=Cs, _St, Line, Col, Toks, Ncs) ->
    {more, {Cs,Line,Col,Toks,fun scan_number/6,Ncs}};
scan_number(Cs, St, Line, Col, Toks, Ncs0) ->
    Ncs = lists:reverse(Ncs0),
    case catch list_to_integer(Ncs) of
        N when is_integer(N) ->
            tok3(Cs, St, Line, Col, Toks, integer, Ncs, N);
        _ ->
            Ncol = incr_column(Col, length(Ncs)),
            scan_error({illegal,integer}, Line, Col, Line, Ncol)
    end.

scan_based_int([C|Cs], St, Line, Col, Toks, {B,Ncs,Bcs})
    when ?DIGIT(C), C < $0+B ->
    scan_based_int(Cs, St, Line, Col, Toks, {B,[C|Ncs],Bcs});
scan_based_int([C|Cs], St, Line, Col, Toks, {B,Ncs,Bcs})
    when C >= $A, B > 10, C < $A+B-10 ->
    scan_based_int(Cs, St, Line, Col, Toks, {B,[C|Ncs],Bcs});
scan_based_int([C|Cs], St, Line, Col, Toks, {B,Ncs,Bcs})
    when C >= $a, B > 10, C < $a+B-10 ->
    scan_based_int(Cs, St, Line, Col, Toks, {B,[C|Ncs],Bcs});
scan_based_int([]=Cs, _St, Line, Col, Toks, State) ->
    {more, {Cs,Line,Col,Toks,fun scan_based_int/6,State}};
scan_based_int(Cs, St, Line, Col, Toks, {B,Ncs0,Bcs}) ->
    Ncs = lists:reverse(Ncs0),
    case catch erlang:list_to_integer(Ncs, B) of
        N when is_integer(N) ->
            tok3(Cs, St, Line, Col, Toks, integer, ?STR(Col, Bcs++Ncs), N);
        _ ->
            Len = length(Bcs)+length(Ncs),
            Ncol = incr_column(Col, Len),
            scan_error({illegal,integer}, Line, Col, Line, Ncol)
    end.

scan_fraction([C|Cs], St, Line, Col, Toks, Ncs) when ?DIGIT(C) ->
    scan_fraction(Cs, St, Line, Col, Toks, [C|Ncs]);
scan_fraction([E|Cs], St, Line, Col, Toks, Ncs) when E =:= $e; E =:= $E ->
    scan_exponent_sign(Cs, St, Line, Col, Toks, [E|Ncs]);
scan_fraction([]=Cs, _St, Line, Col, Toks, Ncs) ->
    {more, {Cs,Line,Col,Toks,fun scan_fraction/6,Ncs}};
scan_fraction(Cs, St, Line, Col, Toks, Ncs) ->
    float_end(Cs, St, Line, Col, Toks, Ncs).

scan_exponent_sign([C|Cs], St, Line, Col, Toks, Ncs) when C =:= $+; C =:= $- ->
    scan_exponent(Cs, St, Line, Col, Toks, [C|Ncs]);
scan_exponent_sign([]=Cs, _St, Line, Col, Toks, Ncs) ->
    {more, {Cs,Line,Col,Toks,fun scan_exponent_sign/6,Ncs}};
scan_exponent_sign(Cs, St, Line, Col, Toks, Ncs) ->
    scan_exponent(Cs, St, Line, Col, Toks, Ncs).

scan_exponent([C|Cs], St, Line, Col, Toks, Ncs) when ?DIGIT(C) ->
    scan_exponent(Cs, St, Line, Col, Toks, [C|Ncs]);
scan_exponent([]=Cs, _St, Line, Col, Toks, Ncs) ->
    {more, {Cs,Line,Col,Toks,fun scan_exponent/6,Ncs}};
scan_exponent(Cs, St, Line, Col, Toks, Ncs) ->
    float_end(Cs, St, Line, Col, Toks, Ncs).

float_end(Cs, St, Line, Col, Toks, Ncs0) ->
    Ncs = lists:reverse(Ncs0),
    case catch list_to_float(Ncs) of
        F when is_float(F) ->
            tok3(Cs, St, Line, Col, Toks, float, Ncs, F);
        _ ->
            Ncol = incr_column(Col, length(Ncs)),
            scan_error({illegal,float}, Line, Col, Line, Ncol)
    end.

skip_comment([C|Cs], St, Line, Col, Toks, N) when C =/= $\n, C >= 0 ->
    skip_comment(Cs, St, Line, Col, Toks, N+1);
skip_comment([]=Cs, _St, Line, Col, Toks, N) ->
    {more, {Cs,Line,Col,Toks,fun skip_comment/6,N}};
skip_comment(Cs, St, Line, Col, Toks, N) ->
    scan1(Cs, St, Line, incr_column(Col, N), Toks).

scan_comment([C|Cs], St, Line, Col, Toks, Ncs) when C =/= $\n, C >= 0 ->
    scan_comment(Cs, St, Line, Col, Toks, [C|Ncs]);
scan_comment([]=Cs, _St, Line, Col, Toks, Ncs) ->
    {more, {Cs,Line,Col,Toks,fun scan_comment/6,Ncs}};
scan_comment(Cs, St, Line, Col, Toks, Ncs0) ->
    Ncs = lists:reverse(Ncs0),
    tok3(Cs, St, Line, Col, Toks, comment, Ncs, Ncs).

tok2(Cs, St, Line, no_col=Col, Toks, _Wcs, P) ->
    scan1(Cs, St, Line, Col, [{P,Line}|Toks]);
tok2(Cs, St, Line, Col, Toks, Wcs, P) ->
    Length = length(Wcs),
    Attrs = {Line,Col,Length,Wcs},
    scan1(Cs, St, Line, Col+Length, [{P,Attrs}|Toks]).

tok2(Cs, St, Line, no_col=Col, Toks, _Wcs, P, _N) ->
    scan1(Cs, St, Line, Col, [{P,Line}|Toks]);
tok2(Cs, St, Line, Col, Toks, Wcs, P, N) ->
    Attrs = {Line,Col,N,Wcs},
    scan1(Cs, St, Line, Col+N, [{P,Attrs}|Toks]).

tok3(Cs, St, Line, no_col=Col, Toks, Tag, _String, Term) ->
    scan1(Cs, St, Line, Col, [{Tag,Line,Term}|Toks]);
tok3(Cs, St, Line, Col, Toks, Tag, String, Term) ->
    Length = length(String),
    Attrs = {Line,Col,Length,String},
    scan1(Cs, St, Line, Col+Length, [{Tag,Attrs,Term}|Toks]).

tok3(Cs, St, Line, no_col=Col, Toks, Tag, _String, Term, _Length) ->
    scan1(Cs, St, Line, Col, [{Tag,Line,Term}|Toks]);
tok3(Cs, St, Line, Col, Toks, Tag, String, Term, Length) ->
    Attrs = {Line,Col,Length,String},
    scan1(Cs, St, Line, Col+Length, [{Tag,Attrs,Term}|Toks]).

scan_error(Error, Line, Col, EndLine, EndCol) ->
    Loc = location(Line, Col),
    EndLoc = location(EndLine, EndCol),
    scan_error(Error, Loc, EndLoc).

scan_error(Error, ErrorLoc, EndLoc) ->
    {error,{ErrorLoc,?MODULE,Error},EndLoc}.

%-compile({inline,[attributes/3,attributes/4]}).

attributes(Line, no_col, _String) ->
    Line;
attributes(Line, Col, String) when is_integer(Col) ->
    {Line,Col,length(String),String}.

attributes(Line, no_col, _String, _Length) ->
    Line;
attributes(Line, Col, String, Length) when is_integer(Col) ->
    {Line,Col,Length,String}.

location(Line, no_col) ->
    Line;
location(Line, Col) when is_integer(Col) ->
    {Line,Col}.

%-compile({inline,[incr_column/2,new_column/2]}).

incr_column(no_col=Col, _N) ->
    Col;
incr_column(Col, N) when is_integer(Col) ->
    Col + N.

new_column(no_col=Col, _Ncol) ->
    Col;
new_column(Col, Ncol) when is_integer(Col) ->
    Ncol.

nl_spcs(2)  -> "\n ";
nl_spcs(3)  -> "\n  ";
nl_spcs(4)  -> "\n   ";
nl_spcs(5)  -> "\n    ";
nl_spcs(6)  -> "\n     ";
nl_spcs(7)  -> "\n      ";
nl_spcs(8)  -> "\n       ";
nl_spcs(9)  -> "\n        ";
nl_spcs(10) -> "\n         ";
nl_spcs(11) -> "\n          ";
nl_spcs(12) -> "\n           ";
nl_spcs(13) -> "\n            ";
nl_spcs(14) -> "\n             ";
nl_spcs(15) -> "\n              ";
nl_spcs(16) -> "\n               ";
nl_spcs(17) -> "\n                ".

spcs(1)  -> " ";
spcs(2)  -> "  ";
spcs(3)  -> "   ";
spcs(4)  -> "    ";
spcs(5)  -> "     ";
spcs(6)  -> "      ";
spcs(7)  -> "       ";
spcs(8)  -> "        ";
spcs(9)  -> "         ";
spcs(10) -> "          ";
spcs(11) -> "           ";
spcs(12) -> "            ";
spcs(13) -> "             ";
spcs(14) -> "              ";
spcs(15) -> "               ";
spcs(16) -> "                ".

nl_tabs(2)  -> "\n\t";
nl_tabs(3)  -> "\n\t\t";
nl_tabs(4)  -> "\n\t\t\t";
nl_tabs(5)  -> "\n\t\t\t\t";
nl_tabs(6)  -> "\n\t\t\t\t\t";
nl_tabs(7)  -> "\n\t\t\t\t\t\t";
nl_tabs(8)  -> "\n\t\t\t\t\t\t\t";
nl_tabs(9)  -> "\n\t\t\t\t\t\t\t\t";
nl_tabs(10) -> "\n\t\t\t\t\t\t\t\t\t";
nl_tabs(11) -> "\n\t\t\t\t\t\t\t\t\t\t".

tabs(1)  ->  "\t";
tabs(2)  ->  "\t\t";
tabs(3)  ->  "\t\t\t";
tabs(4)  ->  "\t\t\t\t";
tabs(5)  ->  "\t\t\t\t\t";
tabs(6)  ->  "\t\t\t\t\t\t";
tabs(7)  ->  "\t\t\t\t\t\t\t";
tabs(8)  ->  "\t\t\t\t\t\t\t\t";
tabs(9)  ->  "\t\t\t\t\t\t\t\t\t";
tabs(10) -> "\t\t\t\t\t\t\t\t\t\t".

%-spec reserved_word(atom()) -> bool().
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
