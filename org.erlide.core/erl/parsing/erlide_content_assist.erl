%% Author: jakob
%% Created: 20 jan 2009
%% Description: Content assist stuff

-module(erlide_content_assist).

-author(jakob).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([get_variables/2]).

%%
%% API Functions
%%

get_variables(Src, Prefix) ->
    case erlide_scan:string(Src) of
	{ok, Tokens, _Pos} ->
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


        