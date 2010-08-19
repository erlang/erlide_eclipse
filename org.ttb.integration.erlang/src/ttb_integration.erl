%% Author: Piotr Dorobisz
%% Created: Jul 29, 2010
-module(ttb_integration).

%%
%% Exported Functions
%%
-export([start/0, stop/0, stop_tracing/0, str2ms/1]).


start()->
	ttbe:tracer(all, [{handler, {create_handler(), initial_state}}]).

stop() ->
	spawn(?MODULE, stop_tracing, []).

stop_tracing()->
	ttbe:stop([format]),
	erlide_jrpc:event(trace_event, stop_tracing).

create_handler() ->
	fun(Fd, Trace, _TraceInfo, State) ->
			case Trace of
				{X, Pid, call, {Mod, Fun, Arg}, Y} ->
					erlide_jrpc:event(trace_event, {X, Pid, call, {Mod, Fun,[avoid_interpreting_as_string] ++ Arg}});
				{X, Pid, spawn, Pid2, {M, F, Args}} ->
					erlide_jrpc:event(trace_event, {X, Pid, spawn, Pid2, {M, F, [avoid_interpreting_as_string] ++ Args}});
				_ ->
					erlide_jrpc:event(trace_event, Trace)
			end,
			State
	end.

str2fun(S) ->
	case erl_scan:string(S) of
		{error, ErrorInfo, _} ->
			{error, ErrorInfo};
		{ok, Tokens, _} ->
			case erl_parse:parse_exprs(Tokens) of
				{error, ErrorInfo} ->
					{error, ErrorInfo};
				{ok, Expr_list} ->
					{value, Value, _} = erl_eval:exprs(Expr_list, erl_eval:new_bindings()),
					{ok, Value}
			end
	end.

str2ms(S) ->
	try (str2fun(S)) of
		{error, ErrorInfo} ->
			{error, standard_info, ErrorInfo};
		{ok, Fun} ->
			try dbg:fun2ms(Fun) of
				{error, ErrorInfo} -> {error, standard_info, ErrorInfo};
				Result -> {ok, Result}
			catch error:function_clause -> {error, not_fun} end
	catch
		error:{unbound_var, X} -> {error, unbound_var, X}
	end.
