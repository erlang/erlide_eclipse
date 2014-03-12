-module(erlide_scan).

-export([string/1,
         string/2,
         string/3,
         reserved_word/1,
         filter_ws/1,
         filter_comments/1,
         filter_ws_comments/1]).

-include("erlide_token.hrl").

string(String) ->
    string(String, {0, 1}).

string(String, Pos) when is_tuple(Pos) ->
    string(String, Pos, []);
string(String, Opts) when is_list(Opts) ->
    string(String, {0, 1}, Opts).

string(String, {L, C}, Opts) ->
    string(String, {L, C, 0}, Opts);
string(String, {L, C, O}, Opts) ->
    case string2(String, {L, C, O}, Opts) of
        {ok, _, _}=R ->
            R;
        {error, {_, _, {_, Quote, _}}, _} ->
            case string2(String++[Quote], {L, C, O}, Opts) of
                {ok, _, _}=R1 ->
                    R1;
                _Err ->
                    _Err
            end;
        _Err2 ->
            _Err2
    end.

string2(String, {L, C, O}, Opts) ->
    case erl_scan:string(String, {L, C}, [text, return |Opts]) of
        {ok, Tokens, {L1, C1}} ->
            {ok, NewTokens, O1} = convert_tokens(Tokens, O),
            {ok, filter_tokens(NewTokens, Opts), {L1, C1, O1}};
        _Err ->
            _Err
    end.

reserved_word(Word) ->
    erl_scan:reserved_word(Word).

filter_ws(L) ->
    lists:filter(fun(#token{kind=Kind}) -> Kind =/= white_space end, L).

filter_comments(L) ->
    lists:filter(fun(#token{kind=Kind}) -> Kind =/= comment end, L).

filter_ws_comments(L) ->
    lists:filter(fun(#token{kind=Kind}) -> (Kind =/= comment) and (Kind =/= white_space) end, L).

filter_tokens(Tokens, Opts) ->
    case {lists:member(return, Opts),
          lists:member(return_comments, Opts),
          lists:member(return_white_space, Opts)} of
        {true, _, _} ->
            Tokens;
        {_, true, true} ->
            Tokens;
        {_, true, false} ->
            filter_ws(Tokens);
        {_, false, true} ->
            filter_comments(Tokens);
        {_, false, false} ->
            filter_ws_comments(Tokens)
    end.

convert_tokens(Tokens, O) ->
    convert_tokens(Tokens, O, []).

convert_tokens([], Ofs, Acc) ->
    {ok, lists:reverse(Acc), Ofs};
convert_tokens([{'?', [{line, L}, {column, O1}, {text, "?"}]}, {'?', [{line, L}, {column, O2}, {text, "?"}]}, {atom, [{line, L}, {column, O}, {text, Txt}], _V} | Rest],
               Ofs, Acc) when O2=:=O1+1; O=:=O2+1 ->
    Txt1 = [$?, $? | Txt],
    T = make_macro(L, Ofs, length(Txt1), Txt1),
    convert_tokens(Rest, Ofs+length(Txt1), [T | Acc]);
convert_tokens([{'?', [{line, L}, {column, O1}|_]}, {'?', [{line, L}, {column, O2}|_]}, {var, [{line, L}, {column, O}, {text, Txt}], _V} | Rest],
               Ofs, Acc) when O2=:=O1+1; O=:=O2+1 ->
    Txt1 = [$?, $? | Txt],
    T = make_macro(L, Ofs, length(Txt1), Txt1),
    convert_tokens(Rest, Ofs+length(Txt1), [T | Acc]);
convert_tokens([{'?', [{line, L}, {column, O}|_]}, {atom, [{line, L}, {column, O1}, {text, Txt}], _V} | Rest],
               Ofs, Acc) when O1=:=O+1->
    Txt1 = [$? | Txt],
    T = make_macro(L, Ofs, length(Txt1), Txt1),
    convert_tokens(Rest, Ofs+length(Txt1), [T | Acc]);
convert_tokens([{'?', [{line, L}, {column, O}|_]}, {var, [{line, L}, {column, O1}, {text, Txt}], _V} | Rest],
               Ofs, Acc) when O1=:=O+1->
    Txt1 = [$? | Txt],
    T = make_macro(L, Ofs, length(Txt1), Txt1),
    convert_tokens(Rest, Ofs+length(Txt1), [T | Acc]);
convert_tokens([{dot, [{line,L}, {column,_O}, {text,Txt}]} | Rest], Ofs, Acc) ->
    %% erl_scan conflates the dot with following whitespace.
    T = #token{kind=dot, line=L, offset=Ofs, length=1, text=[hd(Txt)]},
    case Txt of
        "." ->
            convert_tokens(Rest, Ofs+1, [T | Acc]);
        _ ->
            T1 = #token{kind=white_space, line=L, offset=Ofs+1, length=length(Txt)-1, text=list_to_binary(tl(Txt))},
            convert_tokens(Rest, Ofs+length(Txt), [T1, T | Acc])
    end;
convert_tokens([{What, [{line,L}, {column,_O}, {text,Txt}], _} | Rest], Ofs, Acc) when What==white_space; What==comment ->
    T = #token{kind=What, line=L, offset=Ofs, length=length(Txt), text=unicode:characters_to_binary(Txt)},
    convert_tokens(Rest, Ofs+length(Txt), [T | Acc]);
convert_tokens([{What, [{line,L}, {column,_O}, {text,Txt}], Sym} | Rest], Ofs, Acc) ->
    T = #token{kind=What, line=L, offset=Ofs, length=length(Txt), text=Txt, value=Sym},
    convert_tokens(Rest, Ofs+length(Txt), [T | Acc]);
convert_tokens([{What, [{line,L}, {column,_O}, {text,Txt}]} | Rest], Ofs, Acc) ->
    T = #token{kind=What, line=L, offset=Ofs, length=length(Txt), text=Txt},
    convert_tokens(Rest, Ofs+length(Txt), [T | Acc]).

make_macro(L, O, G, Txt) ->
    V = list_to_atom(Txt),
    #token{kind=macro, line=L, offset=O, length=G, text=Txt, value=V}.

