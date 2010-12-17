%% Author: Piotr Dorobisz
%% Created: Jul 29, 2010
-module(ttb_integration).

%%
%% Exported Functions
%%
-export([start/3, stop/0,  load/3, load_data/3, get_file_info/1, str2ms/1]).


start(NodesAndCookies, FileName, NetTicktime)->
	ttbe:stop(),
	net_kernel:set_net_ticktime(NetTicktime),
	Nodes = set_cookies(NodesAndCookies),
	ttbe:tracer(Nodes, [{file,{local, FileName}}]).

set_cookies(NodesAndCookies) ->
	F = fun({Node, Cookie}) ->
				case Cookie of
					'' ->
						ok;
					_ ->
						erlang:set_cookie(Node, Cookie)
				end,
				Node
		end,
	lists:map(F, NodesAndCookies).

stop() ->
	{stopped, Dir} = ttbe:stop([return]),
	spawn(?MODULE, get_file_info, [Dir]).

get_file_info(Path) ->
	Result = (catch ttbe:format(Path, [{handler, {create_info_handler(Path), {true, 0, '_', '_'}}}])),
	case Result of
		ok -> erlide_jrpc:event(trace_event, stop_tracing);
		{error, Reason} -> erlide_jrpc:event(trace_event, {error_loading, Reason});
		_ -> erlide_jrpc:event(trace_event, {error_loading, "Can not load data"})
	end.

load(Path, Start, Stop) ->
	spawn(?MODULE, load_data, [Path, Start, Stop]).

load_data(Path, Start, Stop) ->
	Result = (catch ttbe:format(Path, [{handler, {create_load_handler(Start, Stop), 1}}])),
	case Result  of
		ok -> erlide_jrpc:event(trace_event, stop_tracing);
		{error, Reason} -> erlide_jrpc:event(trace_event, {error_loading, Reason});
		_ -> erlide_jrpc:event(trace_event, {error_loading, "Can not load data"})
	end.

create_load_handler(Start, Stop) ->
	fun(_Fd, Trace, _TraceInfo, State) ->
			if
				State >= Start, State =< Stop ->
					case Trace of
						{trace_ts, Pid, call, {Mod, Fun, Args}, Time} ->
							erlide_jrpc:event(trace_event, {trace_ts, Pid, call, {Mod, Fun,[avoid_interpreting_as_string] ++ Args}, calendar:now_to_local_time(Time)});
						{trace_ts, Pid, spawn, Pid2, {M, F, Args}, Time} ->
							erlide_jrpc:event(trace_event, {trace_ts, Pid, spawn, Pid2, {M, F, [avoid_interpreting_as_string] ++ Args}, calendar:now_to_local_time(Time)});
						{trace_ts, _, _, _, Time} ->
							T = calendar:now_to_local_time(Time),
							erlide_jrpc:event(trace_event, setelement(tuple_size(Trace), Trace, T));
						{trace_ts, _, _, _, _, Time} ->
							T = calendar:now_to_local_time(Time),
							erlide_jrpc:event(trace_event, setelement(tuple_size(Trace), Trace, T));
						_ ->
							erlide_jrpc:event(trace_event, Trace)
					end;
				true -> ok
			end,
			State + 1
	end.

create_info_handler(Path) ->
	fun(_Fd, Trace, _TraceInfo, State) ->
			{First, Count, Start_date, End_date} = State,
			case Trace of
				end_of_trace ->
					case First of
						true ->
							erlide_jrpc:event(trace_event, {file_info, empty});
						false ->
							erlide_jrpc:event(trace_event, {file_info, calendar:now_to_local_time(Start_date), calendar:now_to_local_time(End_date), Path, Count})
					end;
				{trace_ts, _, _, _, Time} ->
					case First of
						true -> {false, Count + 1, Time, Time};
						_ -> {false, Count + 1, Start_date, Time}
					end;
				{trace_ts, _, _, _, _, Time} ->
					case First of
						true -> {false, Count + 1, Time, Time};
						_ -> {false, Count + 1, Start_date, Time}
					end;
				_ -> {First, Count + 1, Start_date, End_date}
			end
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
