%% Author: jakob
%% Created: 20 jan 2009
%% Description: Content assist stuff

-module(erlide_content_assist).

-author(jakob).

%%
%% Include files
%%

%% -define(DEBUG, 1).

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
get_var_tokens([{'?', _}, {var, _Pos, _Value} | Rest], Prefix, Acc) ->
    get_var_tokens(Rest, Prefix, Acc);
get_var_tokens([{'#', _}, {var, _Pos, _Value} | Rest], Prefix, Acc) ->
    get_var_tokens(Rest, Prefix, Acc);
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

-define(NO_RECORD, 0).
-define(RECORD_NAME, 1).
-define(RECORD_FIELD, 2).

check_record_tokens(Tokens) ->
    ?D(Tokens),
    case check_record_tokens(no_record, Tokens, '', '') of
        L when is_list(L) -> check_record_tokens(L); % Shouldn't happen
        {State, Name, Prefix} -> {state_to_num(State), Name, Prefix}
    end.

state_to_num(record_want_name) -> ?RECORD_NAME;
state_to_num(record_name) -> ?RECORD_NAME;
state_to_num(record_want_dot_field) -> ?RECORD_FIELD;
state_to_num(record_dot_field) -> ?RECORD_FIELD;
state_to_num(record_want_field) -> ?RECORD_FIELD;
state_to_num(record_field) -> ?RECORD_FIELD;
state_to_num(_) -> ?NO_RECORD.

%% We have the following state transitions:
%% 1 #             record_want_name
%% 2 #r            record_name
%% 3 #rec.         record_want_dot_field
%% 4 #rec.f        record_dot_field
%% 5 #rec{         record_want_field (recurse)
%% 6 {             no_record (recurse)
%% 7 #rec{f        record_field
%% 8 #rec{field=v, record_want_field
%% 9 #rec{field=   within_record
%% 10 (all other)  no_record or within_record
%% we carry around the state, the current record name and the before, they
%% are returned with the current state if we're done
%% the position is calculated by the caller, we don't need as much 
%% syntax context for that
%% recursion is needed iff we hit a '{', to keep track of current record
%% so we keep going until we run out of tokens or hit a '}' {
%% we return a tuple when tokens are done and a list if we get a '}'

check_record_tokens(State, [], R, B) ->
    {State, R, B};
check_record_tokens(State, [#token{kind='}'} | Rest], R, B) ->
    Rest; %% either we've recursed, or we left the record, so this is safe
check_record_tokens(State, [#token{kind='#'} | Rest], R, B) -> % 1
    check_record_tokens(record_want_name, Rest, R, B);
check_record_tokens(record_want_name, [#token{kind=atom, value=V} | Rest], _R, _B) -> % 2
    check_record_tokens(record_name, Rest, V, V);
check_record_tokens(record_want_name, [#token{kind=macro, value=V} | Rest], _R, _B) -> % 2
    check_record_tokens(record_name, Rest, V, V);
check_record_tokens(record_name, [#token{kind=Dot} | Rest], R, B) % 3 
  when Dot=:='.'; Dot=:=dot->
    check_record_tokens(record_want_dot_field, Rest, B, '');
check_record_tokens(record_want_dot_field, [#token{kind=atom, value=V} | Rest],
                    R, _B) -> % 4
    check_record_tokens(record_dot_field, Rest, R, V);
check_record_tokens(record_name, [#token{kind='{'} | Rest], R, B) -> % 5
    case check_record_tokens(record_want_field, Rest, B, '') of
        L when is_list(L) -> check_record_tokens(no_record, L, R, B);
        T -> T
    end;
check_record_tokens(State, [#token{kind='{'} | Rest], R, B) -> % 6
    case check_record_tokens(no_record, Rest, B, '') of
        L when is_list(L) -> check_record_tokens(no_record, L, R, B);
        T -> T
    end;
check_record_tokens(record_want_field, [#token{kind=atom, value=V} | Rest], R, B) -> % 7
    check_record_tokens(record_field, Rest, R, V);
check_record_tokens(within_record, [#token{kind=','} | Rest], R, B) -> % 8
    check_record_tokens(record_want_field, Rest, R, B);
check_record_tokens(record_field, [#token{kind='='} | Rest], R, B) -> % 9
    check_record_tokens(within_record, Rest, R, '');
check_record_tokens(within_record, [_ | Rest], R, B) -> % 10A
    check_record_tokens(within_record, Rest, R, B);
check_record_tokens(State, [_ | Rest], R, B) -> % 10B
    check_record_tokens(no_record, Rest, R, B).

get_function_head(Fun, Arity) ->
    erlide_otp_doc:fix_proposals([{Fun, Arity}], [""], 0).
