%% Author: jakob
%% Created: 20 jan 2009
%% Description: Content assist stuff

-module(erlide_content_assist).

-author(jakob).

%%
%% Include files
%%

-define(DEBUG, 1).

-include("erlide.hrl").
-include("erlide_scanner.hrl").


%%
%% Exported Functions
%%
-export([get_variables/2, check_record/1, get_function_head/2]).

%%
%% API Functions
%%


%% check if the text is where to enter record field
check_record(S) ->
    case catch erlide_scan:string(S) of
	{ok, Tokens, _Pos} ->
	    {ok, check_record_tokens(erlide_scanner:convert_tokens(Tokens))};
	{_, _} ->
	    {ok, true};
	_ ->
	    none
    end.

%% get list of variables matching prefix
%% the variables are returne as tokens
get_variables(Src, Prefix) ->
    case erlide_scan:string(Src) of
	{ok, Tokens, _Pos} ->
	    ?D({ok, Tokens, _Pos}),
	    {ok, get_var_tokens(Tokens, Prefix)};
	_ ->
	    none
    end.

%% final OtpErlangObject res = b.rpcx("erlide_content_assist",
%% "get_variables", "ss", src, prefix);


%%
%% Local Functions
%%

get_var_tokens(Tokens, Prefix) ->
    get_var_tokens(Tokens, Prefix, []).

get_var_tokens([], _Prefix, Acc) ->
    Acc;
get_var_tokens([{var, _Pos, Value} | Rest], Prefix, Acc) ->
    S = atom_to_list(Value),
    case S of
	"_" ->
	    get_var_tokens(Rest, Prefix, Acc);
	_ ->
	    case lists:prefix(Prefix, S) of
		true ->
		    get_var_tokens(Rest, Prefix, [S | Acc]);
		_ ->
		    get_var_tokens(Rest, Prefix, Acc)
	    end
    end;
get_var_tokens([_ | Rest], Prefix, Acc) ->
    get_var_tokens(Rest, Prefix, Acc).


check_record_tokens(Tokens) ->
    check_record_tokens(Tokens, false).

check_record_tokens([], A) ->
    A;
check_record_tokens([eof | _], A) ->
    A;
check_record_tokens([#token{kind=eof} | _], A) ->
    A;
check_record_tokens([#token{kind='#'}, #token{kind=atom} | Rest], _) ->
    check_record_tokens(Rest, true);
check_record_tokens([#token{kind='#'}], _) ->
    true;
check_record_tokens([#token{kind='{'} | Rest], _) ->
    check_record_tokens(Rest, true);
check_record_tokens([#token{kind=','} | Rest], _) ->
    check_record_tokens(Rest, true);
check_record_tokens([#token{kind=dot} | Rest], A) ->
    check_record_tokens(Rest, A);
check_record_tokens([#token{kind=atom} | Rest], A) ->
    check_record_tokens(Rest, A);
check_record_tokens(L, _) ->
    case erlide_text:skip_expr(L) of
	L ->
	    check_record_tokens(tl(L), false);
	Rest ->
	    check_record_tokens(Rest, false)
    end.

get_function_head(Fun, Arity) ->
    erlide_otp_doc:fix_proposals([{Fun, Arity}], [""], 0).
