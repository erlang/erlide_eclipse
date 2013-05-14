-module(erlide_scan).

-export([string/1,
         string/2,
         string/3,
         reserved_word/1,
         filter_ws/1,
         filter_comment/1,
         filter_tokens/2]).

-include("erlide_token.hrl").

string(String) ->
  erlide_scan:string(String, {0, 1}).

string(String, Pos) ->
  erlide_scan:string(String, Pos, []).

string(String, Pos, Opts) ->
  {ok, Tokens, EndPos} = erl_scan:string(String, Pos, [text, return |Opts]),
  {ok, filter_tokens(convert_tokens(Tokens), Opts), EndPos}.

reserved_word(Word) ->
  erl_scan:reserved_word(Word).

filter_ws(L) ->
  lists:filter(fun(#token{kind=Kind}) -> Kind =/= white_space end, L).

filter_comment(L) ->
  lists:filter(fun(#token{kind=Kind}) -> Kind =/= comment end, L).

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
            filter_comment(Tokens);
        {_, false, false} ->
            filter_ws(filter_comment(Tokens));
        _Err ->
            io:format("~p~n", [_Err])
    end.

convert_tokens(Tokens) ->
    convert_tokens(Tokens, 0, 0).

convert_tokens(Tokens, Offset, NL) ->
    convert_tokens(Tokens, Offset, NL, []).

convert_tokens([], _Ofs, _NL, Acc) ->
    lists:reverse(Acc);
convert_tokens([{white_space, [{line, _L}, {column, _O}, {text, Txt}], _} | Rest], Ofs, NL, Acc) ->
    T = #token{kind=white_space, line=NL, offset=Ofs, length=length(Txt), text=Txt},
    NL1 = case lists:member($\n, Txt) of true -> NL+1; false -> NL end,
    convert_tokens(Rest, Ofs+length(Txt), NL1, [T | Acc]);
convert_tokens([{'?', [{line, L}, {column, _O1}, {text, "?"}]}, {'?', [{line, L}, {column, _O2}, {text, "?"}]} | Rest],
                Ofs, NL, Acc) ->
     C1 = #token{kind='?', line=NL, offset= Ofs, length=1, text="?"},
     C2 = #token{kind='?', line=NL, offset= Ofs+1, length=1, text="?"},
     convert_tokens(Rest, Ofs+2, NL, [C2, C1 | Acc]);
convert_tokens([{'?', [{line, L}, {column, O}, {text, "?"}]}, {atom, [{line, L}, {column, O1}, {text, Txt}], _V} | Rest],
                Ofs, NL, Acc) when O1=:=O+1->
     T = make_macro(NL, Ofs, length(Txt)+1, Txt),
     convert_tokens(Rest, Ofs+length(Txt)+1, NL, [T | Acc]);
convert_tokens([{'?', [{line, L}, {column, O}, {text, "?"}]}, {var, [{line, L}, {column, O1}, {text, Txt}], _V} | Rest],
                Ofs, NL, Acc) when O1=:=O+1->
     T = make_macro(NL, Ofs, length(Txt)+1, Txt),
     convert_tokens(Rest, Ofs+length(Txt)+1, NL, [T | Acc]);
convert_tokens([{dot, [{line,_L}, {column,_O}, {text,Txt}]} | Rest], Ofs, NL, Acc) ->
    %% erl_scan conflates the dot with following whitespace.
    %% Maybe we should create here a whitespace token too, but we have already filtered
    %% out these at this point
    T = #token{kind=dot, line=NL, offset=Ofs, length=1, text=[hd(Txt)]},
    case Txt of
        "." ->
            convert_tokens(Rest, Ofs+1, NL, [T | Acc]);
        _ ->
            T1 = #token{kind=white_space, line=NL, offset=Ofs+1, length=length(Txt)-1, text=tl(Txt)},
            NL1 = case lists:member($\n, Txt) of true -> NL+1; false -> NL end,
            convert_tokens(Rest, Ofs+length(Txt), NL1, [T1, T | Acc])
    end;
convert_tokens([{What, [{line,_L}, {column,_O}, {text,Txt}], Sym} | Rest], Ofs, NL, Acc) ->
    T = #token{kind=What, line=NL, offset=Ofs, length=length(Txt), text=Txt, value=Sym},
    convert_tokens(Rest, Ofs+length(Txt), NL, [T | Acc]);
convert_tokens([{What, [{line,_L}, {column,_O}, {text,Txt}]} | Rest], Ofs, NL, Acc) ->
    T = #token{kind=What, line=NL, offset=Ofs, length=length(Txt), text=Txt},
    convert_tokens(Rest, Ofs+length(Txt), NL, [T | Acc]).

make_macro(NL, O, G, V0) ->
    V = list_to_atom([$? | V0]),
    #token{kind=macro, line=NL, offset=O, length=G, text=[$? | V0], value=V}.

