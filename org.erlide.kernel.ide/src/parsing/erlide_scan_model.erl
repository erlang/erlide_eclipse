%% @author jakob
%% @doc a functional API for the scan model, as called by the scanner server
%% and the unit test


-module(erlide_scan_model).

%%
%% Include files
%%

%% -define(DEBUG, 1).

-include("erlide.hrl").
-include("erlide_token.hrl").
-include("erlide_scanner_server.hrl").

%%
%% Exported Functions
%%

-export([do_scan/2, tokens_to_string/1, get_all_tokens/1, replace_text/4,
         get_token_window/4, get_token_at/2, get_text/1]).

%%
%% API Functions
%%

do_scan(ScannerName, InitialText) ->
    ?D(do_scan),
    %% TODO FIXME splitting by line messes up multiline strings
    Lines = erlide_scan_util:split_lines_w_lengths(InitialText),
    LineTokens = [scan_line(L) || L <- Lines],
    ?D([ScannerName]), % , InitialText, LineTokens]),
    #module{name=ScannerName, lines=Lines, tokens=LineTokens}.

tokens_to_string(T) ->
    S = tokens_to_string(T, []),
    S.

replace_text(Module, Offset, RemoveLength, NewText) ->
    ?D({Offset, RemoveLength, NewText, Module}),
    case replace_between_lines(Offset, RemoveLength, NewText, Module#module.lines) of
        {Line, NOldLines, AffectedLines, NewLines} ->
            ?D({AffectedLines, NewLines}),
            LineTokens = [scan_line(L) || L <- AffectedLines],
            ?D(LineTokens),
            NewTokens = replace_between(Line, NOldLines, LineTokens, Module#module.tokens),
            Module#module{lines=NewLines, tokens=NewTokens};
        ok ->
            Module
    end.

get_all_tokens(#module{tokens=Tokens}) ->
    get_all_tokens(Tokens, 0, 0, []).

get_token_window(Module, Offset, Before, After) ->
    A = get_tokens_at(Module, Offset, After),
    B = get_tokens_before(Module, Offset, Before),
    {A, B}.

get_token_at(Module, Offset) ->
    case erlide_scan_util:find_line_w_offset(Offset, Module#module.tokens) of
        {N, Pos, _Length, Tokens, false} ->
            case get_token_at_aux(Tokens, Offset - Pos) of
                token_not_found ->
                    token_not_found;
                T ->
                    M = fix_token(T, Pos, N),
                    {ok, M}
            end;
        _ ->
            line_not_found
    end.

get_text(#module{lines=Lines}) ->
    lists:append([L || {_, L} <- Lines]).

%%
%% Local Functions
%%

replace_between(_From, _Length, With, []) ->
    %% allow for special case when replacing empty set
    %% (the Length may be 1 from replace_between_lines)
    With;
replace_between(From, Length, With, In) ->
    ?D({From, Length, With, In}),
    {A, B} = lists:split(From, In),
    {_, C} = lists:split(Length, B),
    A++With++C.

%%
%% Nicer version of string:substring/2 accepting out-of-bounds parameters
%% (should be removed eventually)
%%
substr(Text, Start) when Start>length(Text) ->
    "";
substr(Text, Start) when Start < 1 ->
    Text;
substr(Text, Start) ->
    string:substr(Text, Start).

%%
%% Nicer version of string:substring/3 accepting out-of-bounds parameters
%% (should be removed eventually)
%%
substr(Text, Start, Length) when Start>length(Text); Length=<0 ->
    "";
substr(Text, Start, Length) when Start < 1 ->
    substr(Text, 1, Length+Start-1);
substr(Text, Start, Length) when Start+Length>length(Text) ->
    substr(Text, Start, length(Text)-(Start+Length));
substr(Text, Start, Length) ->
    string:substr(Text, Start, Length).

replace_between_lines(From, Length, With, Lines) ->
    case erlide_scan_util:find_line_w_offset(From, Lines) of
        not_found ->
            erlide_log:log({not_found, "erlide_scan_util:find_line_w_offset", From, Lines}),
            ok;
        {LineNo1, Pos1, _Length1, Line1, Beyond1} ->
            ?D({LineNo1, Pos1, _Length1, Line1, Beyond1}),
            FirstPiece = substr(Line1, 1, From-Pos1),
            LineInfo = case Length of
                           0 ->
                               {LineNo1, Pos1, unused, Line1, Beyond1};
                           _ ->
                               erlide_scan_util:find_line_w_offset(From+Length, Lines)
                       end,
            case LineInfo of
                {LineNo2, Pos2, _Length2, Line2, Beyond2} ->
                    ?D({LineNo2, Pos2, _Length2, Line2, Beyond2}),
                    LastPiece = substr(Line2, From+Length-Pos2+1),
                    ?D({FirstPiece, LastPiece}),
                    {NewText, NOldLines} =
                        case {Beyond1, Beyond2} of
                            {on_eof, on_eof} ->
                                {LastPiece++With, 1};
                            {beyond_eof, _} ->
                                {FirstPiece++With++LastPiece, 0};
                            {_, beyond_eof} ->
                                {FirstPiece++With++LastPiece, LineNo2-LineNo1};
                            _ ->
                                {FirstPiece++With++LastPiece, LineNo2-LineNo1+1}
                        end,
                    WLines = erlide_scan_util:split_lines_w_lengths(NewText),
                    ?D(WLines),
                    {LineNo1, NOldLines, WLines,
                     replace_between(LineNo1, NOldLines, WLines, Lines)};
                _ ->
                    erlide_log:log({not_found_2, "erlide_scan_util:find_line_w_offset", From+Length, Lines}),
                    ok
            end
    end.

fix_token(T = #token{offset=O, line=L, last_line=u}, Offset, Line) ->
    T#token{offset=Offset+O, line=Line+L};
fix_token(T = #token{offset=O, line=L, last_line=LL}, Offset, Line) ->
    T#token{offset=Offset+O, line=Line+L, last_line=Line+LL}.

fix_tokens(Tokens, Offset, Line) ->
    [fix_token(T, Offset, Line) || T <- Tokens].

token_to_string(#token{text=Text}) when is_list(Text) ->
    Text;
token_to_string(#token{value=Value}) when is_list(Value) ->
    Value;
token_to_string(#token{kind=atom, value=Value}) ->
    atom_to_list(Value);
token_to_string(#token{value=Value}) when Value =/= u ->
    atom_to_list(Value);
token_to_string(#token{kind=Kind}) ->
    atom_to_list(Kind).

tokens_to_string([], Acc) ->
    lists:flatten(Acc);
tokens_to_string([T], Acc) ->
    S = token_to_string(T),
    tokens_to_string([], [Acc, S]);
tokens_to_string([T1 | [T2 | _] = Rest], Acc) ->
    S = token_to_string(T1),
    Sb = space_between(T1, T2),
    tokens_to_string(Rest, [Acc, S, Sb]).

space_between(#token{offset=O1, length=Len1, line=L1}, #token{offset=O2, line=L1}) ->
    Num = case O2-O1-Len1 of
              N when N>0 -> N;
              _ -> 0
          end,
    lists:duplicate(Num, $\s);
space_between(#token{offset=O1, length=Len1}, #token{offset=O2}) ->
    Num = case O2-O1-Len1-1 of
              N when N>0 -> N;
              _ -> 0
          end,
    "\n"++lists:duplicate(Num, $\s).

get_tokens_at(Module, Offset, N) ->
    get_tokens_at(Module, Offset, N, []).

get_tokens_at(_Module, _Offset, 0, Acc) ->
    lists:reverse(Acc);
get_tokens_at(Module, Offset, N, Acc0) ->
    case erlide_scan_util:find_line_w_offset(Offset, Module#module.tokens) of
        {LineNo, Pos, Length, Tokens, false} ->
            {M, Ts} = get_tokens_at_aux(Tokens, Offset - Pos, N),
            Acc1 =
                lists:foldl(fun(T, LAcc) ->
                                    [fix_token(T, Pos, LineNo) | LAcc]
                            end, Acc0, Ts),
            get_tokens_at(Module, Pos+Length, N-M, Acc1);
        _ ->
            lists:reverse(Acc0)
    end.

%% get tokens _before_ an offset, quite complicated...
get_tokens_before(Module, Offset, N) ->
    Lines = get_lines_before_and_upto(Module#module.tokens, Offset),
    R = get_tokens_before_aux(Lines, Offset, N, []),
    lists:reverse(R).

get_lines_before_and_upto(Lines, Offset) ->
    get_lines_before_and_upto(Lines, 0, 0, Offset, []).

get_lines_before_and_upto(L, CurOfs, _, Offset, Acc) when L == []; CurOfs >= Offset ->
    Acc;
get_lines_before_and_upto([{Length, L} | Rest], CurOfs, N, Offset, Acc) ->
    get_lines_before_and_upto(Rest, CurOfs+Length, N+1, Offset, [{CurOfs, N, L} | Acc]).

get_tokens_before_aux(L, _, N, Acc) when L == []; N == 0 ->
    Acc;
get_tokens_before_aux([{LineOfs, LineNo, Tokens} | Rest], Offset, N0, Acc0) ->
    {N1, Acc1} = get_tokens_before_aux2(lists:reverse(Tokens), Offset, LineOfs, LineNo, N0, Acc0),
    get_tokens_before_aux(Rest, Offset, N1, Acc1).

get_tokens_before_aux2(L, _, _, _, N, Acc) when L == []; N == 0 ->
    {N, Acc};
get_tokens_before_aux2([T | Rest], Offset, LineOfs, LineNo, N, Acc) ->
    case T of
        #token{offset=Ofs, length=Len} when Offset >= LineOfs+Ofs+Len ->
            get_tokens_before_aux2(Rest, Offset, LineOfs, LineNo, N-1, [fix_token(T, LineOfs, LineNo) | Acc]);
        _ ->
            get_tokens_before_aux2(Rest, Offset, LineOfs, LineNo, N, Acc)
    end.

get_token_at_aux([], _) ->
    token_not_found;
get_token_at_aux([T | Rest], Offset) ->
    case T of
        #token{offset=Ofs, length=Len} when Offset >= Ofs, Offset < Ofs+Len ->
            T;
        _ ->
            get_token_at_aux(Rest, Offset)
    end.

get_tokens_at_aux(Tokens, Offset, N) ->
    get_tokens_at_aux(Tokens, Offset, 0, N, []).

get_tokens_at_aux([], _, M, _, Acc) ->
    {M, lists:reverse(Acc)};
get_tokens_at_aux(_, _, M, N, Acc) when M == N ->
    {M, lists:reverse(Acc)};
get_tokens_at_aux([T | Rest], Offset, M, N, Acc) ->
    case T of
        #token{offset=Ofs, length=Len} when Offset < Ofs+Len ->
            get_tokens_at_aux(Rest, Offset, M+1, N, [T | Acc]);
        _ ->
            get_tokens_at_aux(Rest, Offset, M, N, Acc)
    end.


get_all_tokens([], _, _, Acc) ->
    lists:flatten(Acc); % instead of append(reverse())
get_all_tokens([{Length, Tokens} | Rest], Line, Pos, Acc) ->
    T = fix_tokens(Tokens, Pos, Line),
    get_all_tokens(Rest, Line+1, Pos+Length, [Acc, T]).

scan_line({Length, S}) ->
    case erlide_scan:string(S, {0, 1}, [return_comments]) of
        {ok, T, _} ->
            {Length, T};
        {error, _, _} ->
            {Length, [#token{kind=string, line=0, offset=0, length=length(S),
                             value=S, text="\""++S++"\"", last_line=0}]}
    end.

