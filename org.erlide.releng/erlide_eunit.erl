-module(erlide_eunit).

-export([run/3, test/1]).

run(Module, OutputDir, SourceDirs) ->
	io:format(">> ~p~n", [{Module, OutputDir, SourceDirs}]),

	cover:start(),
	cover_compile(SourceDirs),
	run_tests(Module, OutputDir),
	Results = analyse(SourceDirs),
	cover:stop(),
	file:write_file(OutputDir++"/coverage.data", io_lib:format("~p.~n", [Results])),

	{Simple, Xml} = process(Results),
	file:write_file(OutputDir++"/coverage.xml", Xml),
	file:write_file(OutputDir++"/coverage.info", Simple),
	ok.

test(OutputDir) ->
	[Results] = ok(file:consult(OutputDir++"/coverage.data")),
%	Results = [{"common",
%  [{{app_meta,read,1,1},{1,2}},
%   {{app_meta,write,2,1},{2,3}},
%   {{erlide_util,get_auto_imported,1,1},{3,4}}]},
% {"builder",
%  [{{erlide_builder,compile,1,1},{4,5}},
%   {{erlide_builder,compile,3,1},{5,6}},
%   {{erlide_builder,compile,3,2},{0,7}},
%   {{erlide_builder,compile,3,3},{6,0}},
%   {{erlide_yecc_msgs,parse,1,1},{7,8}}]},
%   {"x", [{{a,b,1,1},{0,1}}]}
%],
	{Simple, Xml} = process(Results),
	file:write_file(OutputDir++"/coverage.xml", Xml),
	file:write_file(OutputDir++"/coverage.info", Simple),
	ok.

process(Results0) ->
	Results = structure_data(Results0),
	{create_simple_report(Results), create_xml_report(Results)}.

cover_compile(Dirs) ->
	Fun = fun(Dir) ->
			Rs = cover:compile_beam_directory(Dir),
			ok(Rs)
		end,
	lists:foreach(Fun, Dirs).

run_tests(Module, OutputDir) ->
	eunit:test(Module, [{report,{eunit_surefire,[{dir,OutputDir}]}}]).

analyse(SourceDirs) ->
	MDs = [ {get_project_name(Dir), modules(Dir)} || Dir<-SourceDirs],
	[ {D, lists:flatten([ok2(cover:analyse(M, coverage, clause)) || M<-Mods])} || {D, Mods}<-MDs].

ok({ok, X}) ->
	X;
ok(X) ->
	X.

ok2({ok, X}) ->
	[X];
ok2(_) ->
	[].

modules(Dir) ->
	Files = ok(file:list_dir(Dir)),
	[ list_to_atom(filename:basename(F, ".beam")) || F<-Files ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Input is a list of all coverage data
%% first element is 
%% - module
%% - {module, function, arity}
%% - {module, function, arity, clause}
%% - {module, line}
%% second element is {lines_covered, lines_not_covered}
%%
%% Output is a tree: dir/module/clause
%% {report, stats, [{dir_name, dir_stats, [{module, module_stats, [{clause, clause_stats}]}]}]}

structure_data(Input) ->
	L = group_data(Input),
%dbg:tracer(),
%dbg:p(self(), [c]),
%dbg:tpl(?MODULE, get_stats, cx),
%dbg:tpl(?MODULE, get_dir_stats, cx),
%dbg:tpl(?MODULE, get_module_stats, cx),
%dbg:tpl(?MODULE, get_clause_stats, cx),
%dbg:tpl(?MODULE, get_line_stats, cx),
%dbg:tpl(?MODULE, count, cx),
	R = fill_stats_dir(fill_stats_module(fill_stats_clause(fill_stats_line(L)))),
%	dbg:stop(),
	R.

group_data(Input) ->
	{report, erlide, [ {dir, D, lists:reverse(group_by_module(L))} || {D, L}<-Input]}.


group_by_module(L) ->
	Fun = fun({{M,F,A,C},D}, Acc) ->
		case lists:keyfind(M, 2, Acc) of
			false ->
				[{module, M, [{clause, {F,A,C}, D}]} | Acc];
			{module, M, Val} ->
				lists:keyreplace(M, 2, Acc, {module, M, [{clause, {F,A,C}, D} | Val]})
			end
	end,
	R = lists:foldl(Fun, [], L),
	[{module,MM,lists:reverse(X)} || {module, MM, X}<-R].

get_project_name(Dir) when is_list(Dir) ->
	Name = filename:basename(filename:dirname(Dir)),
	string:join(lists:nthtail(3, string:tokens(Name, [$.])), ".");
get_project_name(M) when is_atom(M) ->
	get_project_name(filename:dirname(code:which(M))).

-record(stats, {dir={0,0}, module={0,0}, clause={0,0}, line={0,0}}).

fill_stats_line({clause, Key, Val}) ->
	Stats = #stats{line=Val},
	{clause, Key, Stats, []};
fill_stats_line({Type, Key, Val}) ->
	Val1 = [fill_stats_line(X) || X<-Val],
	Stats = #stats{line=get_line_stats(Val1)},
	{Type, Key, Stats, Val1}.

fill_stats_clause({Type, Key, Stats, Val}) ->
	Val1 = [fill_stats_clause(X) || X<-Val],
	Stats1 = Stats#stats{clause=get_clause_stats(Val1)},
	{Type, Key, Stats1, Val1}.

fill_stats_module({Type, Key, Stats, Val}) ->
	Val1 = [fill_stats_module(X) || X<-Val],
	Stats1 = Stats#stats{module=get_module_stats(Val1)},
	{Type, Key, Stats1, Val1}.

fill_stats_dir({Type, Key, Stats, Val}) ->
	Val1 = [fill_stats_dir(X) || X<-Val],
	Stats1 = Stats#stats{dir=get_dir_stats(Val1)},
	{Type, Key, Stats1, Val1}.

count({0, _}, {AA, BB}) ->
	{AA, BB+1};
count({_, _}, {AA, BB}) ->
	{AA+1, BB}.

get_dir_stats(L) ->
	Fun = fun({dir, _, #stats{module=M}, _}, {AA,BB}) ->
				count(M, {AA, BB});
			({_, _, #stats{dir={A,B}}, _}, {AA,BB}) ->
				{AA+A, BB+B}
		end,
	lists:foldl(Fun, {0, 0}, L).

get_module_stats(L) ->
	Fun = fun({module, _, #stats{clause=M}, _}, {AA,BB}) ->
				count(M, {AA, BB});
			({_, _, #stats{module={A,B}}, _}, {AA,BB}) ->
				{AA+A, BB+B}
		end,
	lists:foldl(Fun, {0, 0}, L).

get_clause_stats(L) ->
	Fun = fun({clause, _, #stats{line=M}, _}, {AA,BB}) ->
				count(M, {AA, BB});
			({_, _, #stats{clause={A,B}}, _}, {AA,BB}) ->
				{AA+A, BB+B}
		end,
	lists:foldl(Fun, {0, 0}, L).

get_line_stats(L) ->
	Fun = fun({_, _, #stats{line={A,B}}, _}, {AA,BB}) ->
				{AA+A, BB+B}
		end,
	lists:foldl(Fun, {0, 0}, L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_simple_report(Results) ->
	[
	"OVERALL\n",
	"[dir]\t[module]\t[clause]\t[line]\n",
	print_stats_simple(Results),
	"\n\n",
	"COVERAGE\n",
	"dir\t[module]\t[clause]\t[line]\n",
	coverage_simple(Results),
	"\n"
	].

print_stats_simple({report, _, #stats{dir=SDir, module=SMod, clause=SCla, line=SLin}, _}) ->
	[
	pstat(SDir),"!\t",
	pstat(SMod),"!\t",
	pstat(SCla),"!\t",
	pstat(SLin),"!"
	];
print_stats_simple({dir, Name, #stats{module=SMod, clause=SCla, line=SLin}, _}) ->
	[
	Name,":\t",
	pstat(SMod),"!\t",
	pstat(SCla),"!\t",
	pstat(SLin),"!"
	].

pstat({0, 0}) ->
	"";
pstat({A, B}) ->
	io_lib:format("~w% (~w/~w)", [(100*A) div (A+B), A, A+B]).

coverage_simple({_, _, _, List}) ->
	[[print_stats_simple(X),"\n"] || X<-List].

%%%%%%%%

-include_lib("xmerl/include/xmerl.hrl").

create_xml_report(Data) ->
	xmerl:export_simple([mk_xml_report(Data)], xmerl_xml).

count({A, B}) ->
	A+B.

mk_xml_report(List) when is_list(List) ->
	[mk_xml_report(X) || X<-List];
mk_xml_report({report, Name, #stats{dir=SDir, module=SMod, clause=SCla, line=SLin}, Children}) ->
	{report, [
		{stats, [
			{packages, [{value, count(SDir)}], []},
			{classes, [{value, count(SMod)}], []},
			{methods, [{value, count(SCla)}], []},
			{srcfiles, [{value, count(SMod)}], []},
			{srclines, [{value, count(SLin)}], []}
		]}, 
		{data, [
			{all, 
				[{name, "all modules"}], 
				[
					{coverage, [{type, "class, %"}, {value,pstat(SMod)}], []},
					{coverage, [{type, "method, %"}, {value,pstat(SCla)}], []},
					{coverage, [{type, "block, %"}, {value,pstat(SLin)}], []},
					{coverage, [{type, "line, %"}, {value,pstat(SLin)}], []}
				]++
				mk_xml_report(Children)}
		]}
	]};
mk_xml_report({dir, Name, #stats{dir=SDir, module=SMod, clause=SCla, line=SLin}, Children}) ->
	{package, 
		[
			{name, Name}
		],
				[
					{coverage, [{type, "class, %"}, {value,pstat(SMod)}], []},
					{coverage, [{type, "method, %"}, {value,pstat(SCla)}], []},
					{coverage, [{type, "block, %"}, {value,pstat(SLin)}], []},
					{coverage, [{type, "line, %"}, {value,pstat(SLin)}], []}
				]++
				mk_xml_report(Children)
	};
mk_xml_report({module, Name, #stats{dir=SDir, module=SMod, clause=SCla, line=SLin}, Children}) ->
	{srcfile, 
		[
			{name, atom_to_list(Name)++".erl"}
		],
				[
					{coverage, [{type, "method, %"}, {value,pstat(SCla)}], []},
					{coverage, [{type, "block, %"}, {value,pstat(SLin)}], []},
					{coverage, [{type, "line, %"}, {value,pstat(SLin)}], []},
				{class, 
					[
						{name, Name}
					],
					[
						{coverage, [{type, "method, %"}, {value,pstat(SCla)}], []},
						{coverage, [{type, "block, %"}, {value,pstat(SLin)}], []},
						{coverage, [{type, "line, %"}, {value,pstat(SLin)}], []}
					]++
					mk_xml_report(Children)
				}]
	};
mk_xml_report({clause, Name, #stats{dir=SDir, module=SMod, clause=SCla, line=SLin}, Children}) ->
	XX = case SLin of
			{0, _} -> {0, 1};
			_ -> {1, 0}
	end,
	{method, 
		[
			{name, io_lib:format("~w", [Name])}
		],
				[
					{coverage, [{type, "method, %"}, {value,pstat(XX)}], []},
						{coverage, [{type, "block, %"}, {value,pstat(SLin)}], []},
						{coverage, [{type, "line, %"}, {value,pstat(SLin)}], []}
				]
	};
mk_xml_report(_) ->
	"?\n".

xml_stats(report, Children) ->
	[
	"<stats>",
	"</stats>"
	].
	
