%% =====================================================================
%%      Wrangler's feedback interface module to Erlang E-learning
%% =====================================================================
%%
%% @author Huiqing Li
%%   [http://www.cs.kent.ac.uk/projects/wrangler]
%%
%%
%% @doc Wrangler's feedback interface module to Erlang E-learning

%%@private
-module(inspec_feedback_wrangler).

-export([do_code_inspection/2]).

%% API for feedback tool.
-export([long_functions/2, 
         large_modules/2,
         calls_to_specific_function/2,
         calls_to_specific_functions/2,
         classify_pattern_match/1, 
         nested_if_exprs/2, 
         nested_case_exprs/2,
         nested_receive_exprs/2,
         not_flush_unknown_messages/1,
         top_level_if/1,
         unnecessary_match/1,
         append_two_lists/1,
         non_tail_recursive_function/1,
         use_of_specific_ops/2,
         use_of_export_all/1
        ]).

-export([test/1,
	 collect_function_apps/2,
	 collect_function_apps2/2]).

-include("../include/wrangler.hrl").

-type (message()::string()).
-type (tag() :: atom()).
-spec do_code_inspection([dir()|filename()], [{atom(), [any()]}]) ->
             [{message(), [{tag, list()}]}].
do_code_inspection(SearchPaths, Options) ->
    do_code_inspection(SearchPaths, Options, []).

do_code_inspection(_SearchPaths, [], Acc) ->
    lists:append(lists:reverse(Acc));
do_code_inspection(SearchPaths, [{FunName, Args}|Options], Acc) ->
    Res= try_inspector(?MODULE, FunName, 
                       Args++[SearchPaths]),
    do_code_inspection(SearchPaths, Options, [Res|Acc]).

try_inspector(Mod, Fun, Args) -> 
    case try_to_apply(Mod, Fun, Args) of
        {error, {Reason, StackTrace}} ->
            wrangler_io:format("Wrangler failed to run '~p':\n{~p, \n~s}\n",
                               [Fun, Reason, StackTrace]),
            [];
        {ok, Res} ->
            Res
    end.

try_to_apply(Mod, Fun, Args) -> 
    try apply(Mod, Fun, Args)
    catch
        throw:Error -> 
            Error;    
        _E1:E2->
            Reason=lists:flatten(
                     io_lib:format("~p",[erlang:get_stacktrace()])),
            {error, {E2,Reason}}
    end.

-spec long_functions(integer(), [dir()|filename()]) ->
                            {ok, [{message(), [{tag(), list()}]}]}.
long_functions(Lines, SearchPaths) ->
    {ok, LongFuns} = wrangler_code_inspector_lib:long_functions(
                       SearchPaths, Lines, SearchPaths, 8),
    Res=[begin
             Msg=lists:flatten(
                   io_lib:format("Function ~p:~p/~p has more than ~p "
                                 "lines of code.", [M, F, A, Lines])),
             {Msg, [{file, File}, 
                    {line, integer_to_list(Line)}]}
         end ||{{M, F, A}, {File, Line}}<-LongFuns],
    {ok, Res}.

-spec large_modules(integer(), [dir()|filename()]) ->
                            {ok, [{message(), [{tag(), list()}]}]}.
large_modules(Lines, SearchPaths) ->
    {ok, LargeMods}= wrangler_code_inspector_lib:large_modules(Lines, SearchPaths, 8),
    Res = [begin
               M=filename:basename(File, ".erl"),
               Msg=lists:flatten(
                     io_lib:format("The module '~p' has more than ~p "
                                 "lines of code.", [list_to_atom(M), Lines])),
               {Msg, [{file, File}]} 
           end ||File<-LargeMods],
    {ok, Res}.

-spec nested_case_exprs(integer(), [dir()|filename()]) ->
                            {ok, [{message(), [{tag(), list()}]}]}.
nested_case_exprs(NestLevel, SearchPaths) ->
    nested_exprs(NestLevel, SearchPaths, 'case').

-spec nested_if_exprs(integer(), [dir()|filename()]) ->
                               {ok, [{message(), [{tag(), list()}]}]}.
nested_if_exprs(NestLevel, SearchPaths) ->
    nested_exprs(NestLevel, SearchPaths, 'if').

-spec nested_receive_exprs(integer(), [dir()|filename()]) ->
                               {ok, [{message(), [{tag(), list()}]}]}.
nested_receive_exprs(NestLevel, SearchPaths) ->
    nested_exprs(NestLevel, SearchPaths, 'receive').

nested_exprs(NestLevel, SearchPaths, ExprType) ->
    Files = wrangler_misc:expand_files(SearchPaths, ".erl"),
    Funs = lists:flatmap(fun (File) ->
				 ResRanges=wrangler_code_inspector_lib:nested_exprs_1(
                                             File, NestLevel, ExprType, SearchPaths, 8),
                                 [{File, M, F, A, Ln}||{M,F, A, {{Ln, _},_}}<-ResRanges]
			 end, Files),
    Res = [begin
               Msg = lists:flatten(
                       io_lib:format("The ~p expression in function ~p:~p/~p is nested ~p times or more.",
                                     [ExprType, M, F, A, NestLevel])),
               {Msg, [{file, File},
                      {line, integer_to_list(Ln)}]}
           end || {File, M, F, A, Ln}<-Funs],
    {ok, Res}.
    
-spec not_flush_unknown_messages([dir()|filename()]) ->
                                        {ok, [{message(), [{tag(), list()}]}]}.
not_flush_unknown_messages(SearchPaths) ->
    Files = wrangler_misc:expand_files(SearchPaths, ".erl"),
    Funs = lists:flatmap(fun (F) ->
				 wrangler_code_inspector_lib:not_flush_unknown_messages_1(F, SearchPaths, 8)
			 end, Files),
    Res = [begin
               Msg = lists:flatten(
                       io_lib:format("The receive expression in function ~p:~p/~p does not flush unknown messages.",
                                     [M, F, A])),
               {Msg, [{file, File},
                      {line, integer_to_list(Ln)}]}
           end || {File, M, F, A, Ln}<-Funs],
    {ok, Res}.
    

-spec calls_to_specific_function(mfa(), [dir()|filename()]) ->    
                                        {ok, [{message(), [{tag(), list()}]}]}.
calls_to_specific_function({M, F, A}, SearchPaths) ->
    {ok, Calls} = wrangler_code_inspector_lib:calls_to_specific_function(
                    {M,F,A}, SearchPaths),
    Res=[begin
             Msg=lists:flatten(
                   io_lib:format("Oops. You've called the blacklisted function ~p:~p/~p at line ~p.",
                                 [M, F, A, Line])),
             {Msg, [{file, File}, 
                    {line, integer_to_list(Line)}]}
         end ||{File,{{Line, _}, _}}<-Calls],
    {ok, Res}.

-spec calls_to_specific_functions([mfa()], [dir()|filename()]) ->    
                                        {ok, [{message(), [{tag(), list()}]}]}.
calls_to_specific_functions(MFAs, SearchPaths) ->
    {ok, Calls} = wrangler_code_inspector_lib:calls_to_specific_functions(
                    MFAs, SearchPaths),
    Res=[begin
             Msg=lists:flatten(
                   io_lib:format("Oops. You've called the blacklisted function ~p:~p/~p at line ~p.",
                                 [M, F, A, Line])),
             {Msg, [{file, File}, 
                    {line, integer_to_list(Line)}]}
         end ||{File,{M, F, A}, {{Line, _}, _}}<-Calls],
    {ok, Res}.

-type (op():: atom()).
-spec use_of_specific_ops([op()], [dir()|filename()]) ->    
                                 {ok, [{message(), [{tag(), list()}]}]}.
use_of_specific_ops(Ops, SearchPaths) ->
    {ok, Calls} = use_of_specific_ops_1(Ops, SearchPaths),
    Res=[begin
             Msg=lists:flatten(
                   io_lib:format("Oops. You've used the blacklisted operaotr ~p.",
                                 [Op])),
             {Msg, [{file, File}, 
                    {line, integer_to_list(Line)}]}
         end ||{File,Op, {{Line, _}, _}}<-Calls],
    {ok, Res}.

use_of_specific_ops_1(Ops, SearchPaths) ->
    {ok, ?FULL_TD_TU([?COLLECT(?T("E@"),
                               {_File@, get_operator(E@),api_refac:start_end_loc(_This@)},
                               lists:member(api_refac:type(E@),
                                            [infix_expr, prefix_expr]) andalso
                               lists:member(get_operator(E@), Ops))],
                     [SearchPaths])}.

get_operator(Expr) ->
    Op=case api_refac:type(Expr) of 
        infix_expr ->
               wrangler_syntax:infix_expr_operator(Expr);
        prefix_expr ->
               wrangler_syntax:prefix_expr_operator(Expr)
       end,
    wrangler_syntax:operator_name(Op).
     
-spec use_of_export_all([dir()|filename()]) ->    
                               {ok, [{message(), [{tag(), list()}]}]}.
use_of_export_all(SearchPaths) ->
    {ok, Uses} = use_of_export_all_1(SearchPaths),
    Res=[begin
             Msg="Oops. You've used the 'export_all' directive.",
             {Msg, [{file, File}]}                  
         end ||File<-Uses],
    {ok, Res}.

use_of_export_all_1(SearchPaths) ->
    Files = wrangler_misc:expand_files(SearchPaths, ".erl"),
    {ok, [File||File<-Files, use_of_export_all_2(File)]}.

use_of_export_all_2(File)->
    try wrangler_ast_server:parse_annotate_file(File, false) of 
        {ok, {_, ModInfo}} ->
            case lists:keysearch(attributes, 1, ModInfo) of
                {value, {attributes, Attrs}} -> 
                    lists:member({compile, export_all}, Attrs);
                false -> false
            end
    catch _E1:_E2 ->
            false
    end.            
        

%% Give the type (according to syntax_tools) of the patterns
%% in a function definition.

classify_pattern_match(File) ->
    Classify = ?FULL_TD_TU([?COLLECT(?T("f@(Args@@@) when _Guard@@@ -> _Body@@@."), 
				     {api_refac:fun_define_info(f@),
				      api_refac:start_end_loc(_This@),
				      overall_classify([pattern_type(Pat) || Pat <- lists:flatten(Args@@@)])},
				     true
				    )],
			   [File]),
    MsgFun = fun (mixed)    -> " patterns made up entirely of variables or of literals.";
		 (variable) -> " mixed patterns or patterns of literals only.";
		 (literal)  -> " mixed patterns or patterns of variables only."
	  end,
    Res=[begin
             Msg=lists:flatten(
                   io_lib:format("This function could be defined differently: try it with~s~n", [MsgFun(Kind)])),
             {Msg, [{file, File}, 
                    {line_from, integer_to_list(LineFrom)},
		    {line_to, integer_to_list(LineTo)}]}
         end ||{_, {{LineFrom,_},{LineTo,_}}, Kind}<-Classify, Kind /= none],
    {ok, Res}.

-spec pattern_type(syntaxTree()) -> variable | literal | mixed.
pattern_type(Pat) ->
    case wrangler_syntax:is_literal(Pat) of
	true -> literal;
	false -> case api_refac:type(Pat) of
		     variable -> variable;
		     _ -> mixed
		 end
    end.
    
-spec overall_classify([atom()]) -> atom().
overall_classify([]) ->
     none;
overall_classify([X|Xs]) ->
    overall_classify(X,Xs).

-spec overall_classify(atom(),[atom()]) -> atom().

overall_classify(X,[]) ->
     X;
overall_classify(X,[Y|Ys]) ->
    case X==Y of
	true ->
	    overall_classify(X,Ys);
	false ->
	     mixed
    end.
    


%% Collect all the function applications within a function definition.
%% Returns {erlang,apply,3} for uses of apply.
collect_function_apps({M, F, A}, SearchPaths) ->
    ?FULL_TD_TU([?COLLECT(?T("f@(Args@@) when _Guard@@ -> Body@@;"), 
                          collect_apps_within(_This@),
			  {M, F, A} == api_refac:fun_define_info(f@))
			 ],
                [SearchPaths]).

collect_apps_within(Node) ->
    ?FULL_TD_TU([?COLLECT(?T("F@(Argss@@)"),
			  api_refac:fun_define_info(F@),
			  true)
		],
		Node).

collect_function_apps2({M, F, A}, SearchPaths) ->
    ?FULL_TD_TU([?COLLECT(?T("f@(Args@@) when _Guard@@ -> Body@@;"), 
                          collect_apps_within2(_This@),
			  {M, F, A} == api_refac:fun_define_info(f@))
			 ],
                [SearchPaths]).

collect_apps_within2(Node) ->
    ?FULL_TD_TU([?COLLECT(?T("F@(Argss@@)"),
			  ?PP(api_refac:get_app_mod(_This@)),
			  true)
		],
		Node).

-spec top_level_if([dir()|filename()]) ->
                            {ok, [{message(), [{tag(), list()}]}]}.
top_level_if(SearchPaths)->
    Funs=?FULL_TD_TU([?COLLECT(?T("f@(Args@@) when Guard@@ ->Body@."), 
                          {_File@, api_refac:fun_define_info(f@), api_refac:start_end_loc(_This@)},
                          api_refac:type(Body@) == if_expr)],
                     [SearchPaths]),
    Res = [begin
               Msg = lists:flatten(
                       io_lib:format("The function ~p:~p/~p consists of a top-level if expression.",
                                     [M, F, A])),
               {Msg, [{file, File},
                      {line, integer_to_list(Ln)}]}
           end || {File, {M, F, A}, {{Ln, _}, _}}<-Funs],
    {ok, Res}.

-spec unnecessary_match([dir()|filename()]) ->
                               {ok, [{message(), [{tag(), list()}]}]}.
unnecessary_match(SearchPaths) ->
    Funs=?FULL_TD_TU([?COLLECT(?T("Body@@, V@=Expr@, V@"), 
                               {_File@, wrangler_misc:start_end_loc(lists:nthtail(length(Body@@), _This@))},
                               api_refac:type(V@) == variable andalso length(api_refac:var_refs(V@))==1)],
                     SearchPaths),
    Res = [begin
               Msg = lists:flatten(
                       io_lib:format("This is an unnecessary match expression at line ~p.",
                                     [Ln])),
               {Msg, [{file, File},
                      {line, integer_to_list(Ln)}]}
           end || {File, {{Ln, _}, _}}<-Funs],
    {ok, Res}.

-spec append_two_lists([dir()|filename()]) ->
                              {ok, [{message(), [{tag(), list()}]}]}.
append_two_lists(SearchPaths) ->
    Uses=?STOP_TD_TU([?COLLECT(?T("F@(L1@, L2@)"), 
                              {_File@, wrangler_misc:start_end_loc(_This@)},
                              {lists, append, 2} == api_refac:fun_define_info(F@))],
                    SearchPaths),
    Res = [begin
               Msg = lists:flatten(
                       io_lib:format("This is a use of lists:append/2 at line ~p.",
                                     [Ln])),
               {Msg, [{file, File},
                      {line, integer_to_list(Ln)}]}
           end || {File, {{Ln, _}, _}}<-Uses],
    {ok, Res}.

-spec non_tail_recursive_function([dir()|filename()]) ->
                              {ok, [{message(), [{tag(), list()}]}]}.
non_tail_recursive_function(SearchPaths) ->
    Funs=?FULL_TD_TU([?COLLECT(?T("f@(Args@@@) when Guard@@@-> Body@@@."), 
                               {_File@, api_refac:fun_define_info(f@),wrangler_misc:start_end_loc(_This@)},
                               inspec_examples:is_non_tail_recursive(_This@))],
                     [SearchPaths]),
    Res = [begin
               Msg = lists:flatten(
                       io_lib:format("Function ~p:~p/~p is not tail recursive.",
                                     [M,F, A])),
               {Msg, [{file, File},
                      {line, integer_to_list(Ln)}]}
           end || {File, {M, F, A},{{Ln, _}, _}}<-Funs],
    {ok, Res}.

%% In this test, I try to detect two code code smells. One is 
%% function definitions with 40 or more lines of code, and the
%% other is the use of outdated function lists:keysearch/3.
test(SearchPaths)->
    Options=[{long_functions, [40]},
             {calls_to_specific_function, [{lists, keysearch, 3}]}],
    do_code_inspection(SearchPaths, Options).


