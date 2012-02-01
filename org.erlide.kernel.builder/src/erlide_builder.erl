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
%%% File    : erlide_builder.erl
%%% Author  :  Vlad Dumitrescu
%%% Description :
%%% Created : 08 Aug 2005 by  Vlad Dumitrescu

-module(erlide_builder).

-export([
		 compile/1,
		 compile/3,
		 compile/4,
		 load/2,
		 compile_yrl/2,
		 code_clash/0,
		 source_clash/1,

		 build_resources/5
		]).

%%-define(DEBUG, 1).

-include("erlide.hrl").

-define(ERROR, 0).
-define(WARNING, 1).
-define(INFO, 2).

%% Compile file
compile(F) ->
	compile_options(F, [return], ".").

%% Compile file, given output directory and include directory
compile(F, OutputDir, IncludeDirs) ->
	compile(F, OutputDir, IncludeDirs, []).

compile(F, OutputDir, IncludeDirs, Options) ->
	erlide_batch:call(?MODULE, fun compile_options/3,[F, [return, binary | mk_includes(IncludeDirs)]++Options, OutputDir]).

%% Compile Erlang file taking various compile options into account
compile_options(F, Options, OutputDir) ->
	FN = list_to_atom(filename:basename(F, ".erl")),
	case compile:file(F, Options) of
		{error, E, W} ->
			{error, lists:sort(format_compile_msg(E, ?ERROR)++ format_compile_msg(W, ?WARNING))};
		{ok, FN, Bin, W} ->
			F1 = OutputDir++"/"++atom_to_list(FN)++".beam",
			file:write_file(F1, Bin),
			{ok, lists:sort(format_compile_msg(W, ?WARNING)), [F1]};
		{ok, FN, Bin} ->
			F1 = OutputDir++"/"++atom_to_list(FN)++".beam",
			file:write_file(F1, Bin),
			{ok, [], [F1]};
		{ok, Mod, _Bin, W} ->
			Msg = {1, F, lists:flatten(io_lib:format("declared module name '~p' doesn't match file name '~p'", [Mod, FN])), ?ERROR},
			{error, lists:sort([Msg | format_compile_msg(W, ?WARNING)])};
		{ok, Mod, _Bin} ->
			Msg = {1, F, lists:flatten(io_lib:format("declared module name '~p' doesn't match file name '~p'", [Mod, FN])), ?ERROR},
			{error, [Msg]}
	end.

format_compile_msg(L, Marker) when is_list(L) ->
	lists:flatten([format_compile_msg(X, Marker) || X <- L]);
format_compile_msg({File, L}, Marker) ->
	[{Ln, File, iolist_to_binary(M:format_error(D)), Marker} || {Ln, M, D} <- L].

load(Mod, All) ->
	case code:is_sticky(Mod) of
		true ->
			ok;
		false ->
			if All ->
                   c:nl(Mod);
               true ->
                   c:l(Mod)
            end
	end.

mk_includes(L) ->
	[{i, X} || X <- L].

compile_yrl(In, Out) ->
	erlide_batch:call(?MODULE, fun do_compile_yrl/2, [In, Out]).

do_compile_yrl(In, Out) ->
	erlide_yecc_msgs:start(),
	group_leader(whereis(erlide_yecc_msgs), self()),
	process_flag(trap_exit, true),
	Args = [In, [{parserfile, Out}, {verbose, false}, {return, true}]],
	Result = (catch apply(yecc, file, Args)),
	case Result of
		{'EXIT', {undef,_}} ->
			{error, "notR11"};
		{error, Errors, Warnings} ->
			Err = lists:flatmap(
					fun({_Obj, Errs}) -> [ {Le,Mde,stringify(Mse),0} || {Le,Mde,Mse} <- Errs] end,
					Errors),
			Wrn = lists:flatmap(
					fun({_Obj, Warn}) -> [ {Lw,Mdw,stringify(Msw),1} || {Lw,Mdw,Msw} <- Warn] end,
					Warnings),
			{error, Err++Wrn};
		{ok,_,Warnings} ->
			Wrn = lists:flatmap(
					fun({_Obj, Warn}) -> [ {Lw,Mdw,stringify(Msw),1} || {Lw,Mdw,Msw} <- Warn] end,
					Warnings),
			{ok, Wrn, [Out]}
	end.

stringify(X) -> lists:flatten(io_lib:format("~p", [X])).

%%% this part is adapted from the standard code module

code_clash()  ->
	Path = code:get_path(),
	Struct = lists:flatten(build(Path, code:objfile_extension())),
	search(Struct).

search([]) -> [];
search([{Dir,File} | Tail]) ->
	case lists:keysearch(File,2,Tail) of
		false ->
			search(Tail);
		{value,{Dir2,File}} ->
			[{filename:join(Dir,File),
			  filename:join(Dir2,File)} | search(Tail)]
	end.

build([], _Ext) -> [];
build([Dir|Tail], Ext) ->
	Files = filter(Ext, Dir, file:list_dir(Dir)),
	[decorate(Files, Dir) | build(Tail, Ext)].

decorate([], _) -> [];
decorate([File|Tail], Dir) ->
	[{Dir, File} | decorate(Tail, Dir)].

filter(_Ext, _Dir, {error,_}) ->
	[];
filter(Ext, _, {ok,Files}) ->
	filter2(Ext, length(Ext), Files).

filter2(_Ext, _Extlen, []) -> [];
filter2(Ext, Extlen,[File|Tail]) ->
	case has_ext(Ext,Extlen, File) of
		true -> [File | filter2(Ext, Extlen, Tail)];
		false -> filter2(Ext, Extlen, Tail)
	end.

has_ext(Ext, Extlen,File) ->
	L = length(File),
	case catch lists:nthtail(L - Extlen, File) of
		Ext -> true;
		_ -> false
	end.

%%%

%%
%% we also need a check for unique module names in the project

source_clash(Dirs) ->
	Struct = lists:flatten(build(Dirs, "erl")),
	search(Struct).

build_resources(Files, OutputDir, IncludeDirs, Options, Reporter) ->
	spawn(fun() ->
				  erlang:register(erlide_builder, self()),
				  receive
					  start ->
						  erlide_log:logp("Start building! ~p", [{Files, OutputDir, IncludeDirs, Options, Reporter}]),
						  do_build_resources(Files, OutputDir, IncludeDirs, Options, Reporter),
						  erlide_log:logp("Done building!")
					  after 1000 ->
						  erlide_log:logp("builder timeout!")
				  end
		  end).

do_build_resources(Files, OutputDir, IncludeDirs, Options, Reporter) ->
	Fun = fun(F) ->
				  Res = (catch build_one_file(F, OutputDir, IncludeDirs, Options)),
				  Reporter ! {compile, Res},

				  %% TODO scan tasks

				  ok
		  end,
	lists:foreach(Fun, Files),

	%% TODO check code clashes

	Reporter ! stop,
	ok.

build_one_file(F, OutputDir, IncludeDirs, Options) ->
	erlide_log:logp("COMPILE ->  ~p ", [F]),
	case filename:extension(F) of
		".erl" ->
			compile(F, OutputDir, IncludeDirs, Options);
		".yrl" ->
			%% FIXME to be handled in http://www.assembla.com/spaces/erlide/tickets/679
			ErlF = filename:join(filename:dirname(F),
								 filename:basename(F, "yrl")++"erl"),
			erlide_log:logp("YRL ->  ~p ", [{F, ErlF}]),
			case compile_yrl(F, ErlF) of
				{error, Msgs} ->
					{error, Msgs};
				{ok, Msgs, Out} ->
					case compile(ErlF, OutputDir, IncludeDirs, Options) of
						{ok, Msgs2, Out2} ->
							{ok, Msgs++Msgs2, Out++Out2};
						{error, Msgs2} ->
							{error, Msgs++Msgs2}
					end
			end;
		_ ->
			{error, [{0, F, "Don't know how to compile this file"}]}
	end.

