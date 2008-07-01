%%% ******************************************************************************
%%%  Copyright (c) 2004 Vlad Dumitrescu and others.
%%%  All rights reserved. This program and the accompanying materials
%%%  are made available under the terms of the Eclipse Public License v1.0
%%%  which accompanies this distribution, and is available at
%%%  http://www.eclipse.org/legal/epl-v10.html
%%%
%%%  Contributors:
%%%      Vlad Dumitrescu
%%% ******************************************************************************/
%%% File    : erlide_backend.erl
%%% Author  :  Vlad Dumitrescu
%%% Description :

-module(erlide_backend).

-export([init/2,
         
         parse_term/1,
         eval/1,
         eval/2,
         
         format/2,
         pretty_print/1,
         
         scan_string/1,
         parse_string/1,
         
         execute/2,
         
         compile_string/1
]).

init(JavaNode, LinkPid) ->
    spawn(fun()->
        RpcPid = spawn(fun() -> link(LinkPid), jrpc:rpc_loop(JavaNode) end),
        register(erlide_rex, RpcPid),
        watch_eclipse(JavaNode)
    end),
    
    F = dbg:trace_port(file, "log.good"),
    dbg:tracer(port, F),
    dbg:p(all, [all]),
    
    ok.

watch_eclipse(JavaNode) ->
        spawn(fun() ->
            	monitor_node(JavaNode, true),
           	    receive
					{nodedown, JavaNode} ->
        				init:stop()
    			end
              end).


parse_term(Str) ->
    case catch parse_term_raw(Str) of
    {'EXIT', Reason} ->
        {error, Reason};
    Result ->
        Result
    end.

parse_term_raw(Str) ->
    erlang:display(Str),
    {ok, Tokens, _} = erl_scan:string(Str),
    erlang:display(Tokens),
        R=erl_parse:parse_term(Tokens),
    erlang:display(R),
        R.

eval(Str) ->
    eval(Str, erl_eval:new_bindings()).

eval(Str, Bindings) ->
    %% TODO use try...catch here!
    case catch eval_raw(Str, Bindings) of
    {'EXIT', Reason} ->
        {error, Reason};
    Result ->
        Result
    end.

eval_raw(Str, Bindings) ->
    {ok, Tokens, _} = erl_scan:string(Str),
    {ok, Result} = erl_parse:parse_exprs(Tokens),
    erl_eval:exprs(Result, Bindings).

format(Fmt, Args) ->
    lists:flatten(io_lib:format(Fmt, Args)).

pretty_print(Str) ->
    {ok, L, _} = erl_scan:string(Str),
    case erl_parse:parse_term(L) of
    {ok, Term} ->
        lists:flatten(io_lib:format("~p", [Term]));
    _ ->
        Str
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
scan_string(S) ->
    scan_string(S, [], 1).

scan_string([], Res, _) ->
    {ok, lists:reverse(Res)};
scan_string(S, Res, N) ->
    case erl_scan:tokens([], S, N) of
    {done, Result, Rest} ->
        case Result of
        {ok, Toks, End} ->
            scan_string(Rest, [Toks | Res], End);
        {eof, End} ->
            scan_string([], Res, End)
        end;
    {more, _Cont} ->
        scan_string([], Res, N)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_string(S) ->
    {ok, L} = scan_string(S),
    case catch {ok, lists:map(fun(X) ->
                 {ok, Form} = erl_parse:parse_exprs(X),
                 Form
             end,
             L)} of
    {ok, Res} -> {ok, Res};
    Err -> Err
    end.


%%%%%%%%%%%%%%%%%%%%%%
execute(StrFun, Args) ->
  StrMod = "-module(erlide_execute_tmp).\n"
      "-export([exec/1]).\n"
      "exec(ZZArgs) -> Fun = "++StrFun++",\n"
      " catch Fun(ZZArgs).\n",
  catch case parse_string(StrMod) of
    {ok, Mod} ->
      {ok, erlide_execute_tmp,Bin} = compile:forms(Mod, [report,binary]),
      code:load_binary(erlide_execute_tmp, "erlide_execute_tmp.erl", Bin),
      Res = erlide_execute_tmp:exec(Args),
      code:delete(erlide_execute_tmp),
      code:purge(erlide_execute_tmp),
      Res;
    Err ->
      Err
  end.

%%%%%%%%%%%%%%%%%%%%%%%%


parse(Toks) ->
    Parts = split_dot(Toks),
    Fun = fun(E) ->
		  case erl_parse:parse(E) of
		      {ok, X} ->
			  X;
		      Err ->
			  Err
		  end
	  end,
    Res = lists:map(Fun, Parts),
    {ok, Res}.

split_dot(L) ->
    split_dot(L, [], []).

split_dot([], R, []) ->
    lists:reverse(R);
split_dot([], R, V) ->
    lists:reverse([V|R]);
split_dot([{eof}|T], R, V) ->
    split_dot(T, R, V);
split_dot([{dot, _}=H|T], R, V) ->
    split_dot(T, [lists:reverse([H|V])|R], []);
split_dot([H|T], R, V) ->
    split_dot(T, R, [H|V]).

compile_string(Str) ->
    {ok, T, _} = erl_scan:string(Str),
    {ok, Code} = parse(T),
    case compile:forms(Code, [return]) of
        {ok, Mod, Bin} ->
            code:load_binary(Mod, atom_to_list(Mod), Bin);
        {ok, Mod, Bin, _} ->
            code:load_binary(Mod, atom_to_list(Mod), Bin);
         Err ->
            Err
    end.
