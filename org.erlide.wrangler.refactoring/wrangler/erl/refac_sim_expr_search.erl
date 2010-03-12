%% Copyright (c) 2010, Huiqing Li, Simon Thompson
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%     %% Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%     %% Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%     %% Neither the name of the copyright holders nor the
%%       names of its contributors may be used to endorse or promote products
%%       derived from this software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ''AS IS''
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
%% BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR 
%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF 
%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%% ===========================================================================================
%% Refactoring: Search an user-selected expression/expression sequence from the current buffer.
%%
%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%% 
-module(refac_sim_expr_search).

-export([sim_expr_search_in_buffer/6,sim_expr_search_in_dirs/6, normalise_record_expr/5]).

-include("../include/wrangler.hrl").

-define(DefaultSimiScore, 0.8).


%% ================================================================================================
%% @doc Search for expressions that are 'similar' to an expression/expression sequence selected by
%%      the user in the current Erlang buffer.
%% <p> Given expression selected by the user, A say, expression B is similar to A if there exist a least 
%% general common abstation, C, such that, substitution C(Args1) = A, and C(Arg2) == B, and 
%% Size(C) /Size(A) > the threshold specified (0.8 by default).
%% </p>
%% <p> In the case that code selected contains multiple, but non-continuous, sequence of expressions, the first
%% continuous sequence of expressions is taken as the expression selected by the user. A continuous sequence of
%% expressions is a sequence of expressions separated by ','.
%% <p>

-spec(sim_expr_search_in_buffer/6::(filename(), pos(), pos(), string(),[dir()],integer())
      -> {ok, [{integer(), integer(), integer(), integer()}]}).    
sim_expr_search_in_buffer(FName, Start = {Line, Col}, End = {Line1, Col1}, SimiScore0, SearchPaths, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:sim_expr_search(~p, {~p,~p},{~p,~p},~p, ~p, ~p).\n",
		 [?MODULE, FName, Line, Col, Line1, Col1, SimiScore0, SearchPaths, TabWidth]),
    SimiScore = get_simi_score(SimiScore0),
    {FunDef, Exprs, SE} = get_fundef_and_expr(FName, Start, End, SearchPaths, TabWidth),
    {Ranges, AntiUnifier} = search_and_gen_anti_unifier(FName, {FName, FunDef, Exprs, SE}, SimiScore, SearchPaths, TabWidth),
    refac_code_search_utils:display_search_results(Ranges, AntiUnifier, "similar").


-spec(sim_expr_search_in_dirs/6::(filename(), pos(), pos(), string(),[dir()],integer())
      -> {ok, [{integer(), integer(), integer(), integer()}]}).    
sim_expr_search_in_dirs(FileName, Start = {Line, Col}, End = {Line1, Col1}, SimiScore0, SearchPaths, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:sim_expr_search_in_dirs(~p, {~p,~p},{~p,~p},~p, ~p, ~p).\n",
		 [?MODULE, FileName, Line, Col, Line1, Col1, SearchPaths, SearchPaths, TabWidth]),
    Files = [FileName| refac_util:expand_files(SearchPaths, ".erl") -- [FileName]],
    SimiScore = get_simi_score(SimiScore0),
    {FunDef, Exprs, SE} = get_fundef_and_expr(FileName, Start, End, SearchPaths, TabWidth),
    {Ranges, AntiUnifier} = search_and_gen_anti_unifier(Files, {FileName, FunDef, Exprs, SE}, SimiScore, SearchPaths, TabWidth),
    refac_code_search_utils:display_search_results(Ranges, AntiUnifier, "similar").

search_and_gen_anti_unifier(Files, {FName, FunDef, Exprs, SE}, SimiScore, SearchPaths, TabWidth) ->
    {_Start, End} = SE,
    Res = lists:append([search_similar_expr_1(F, Exprs, SimiScore, SearchPaths, TabWidth) || F <- Files]),
    {Ranges, ExportVars, SubSt} = lists:unzip3(Res),
    ExportVars1 = {element(1, lists:unzip(vars_to_export(FunDef, End, Exprs))),
		   lists:usort(lists:append(ExportVars))},
    AntiUnifier = anti_unification:generate_anti_unifier(Exprs, SubSt, ExportVars1),
    {[{FName, SE}| Ranges -- [{FName, SE}]], AntiUnifier}.

search_similar_expr_1(FName, Exprs, SimiScore, SearchPaths, TabWidth) ->
    try refac_util:parse_annotate_file(FName, true, SearchPaths, TabWidth) of
	{ok, {AnnAST, _}} ->
	    RecordInfo = get_module_record_info(FName, SearchPaths, TabWidth),
	    do_search_similar_expr(FName, AnnAST, RecordInfo, Exprs, SimiScore)
    catch 
	_E1:_E2 ->
	    []
    end.

do_search_similar_expr(FileName, AnnAST, RecordInfo, Exprs, SimiScore) when is_list(Exprs) ->
    F0 = fun (FunNode, Acc) ->
		 F = fun (T, Acc1) ->
			     Exprs1 = get_expr_seqs(T),
			     do_search_similar_expr_1(FileName, Exprs, Exprs1, RecordInfo, SimiScore, FunNode) ++ Acc1
		     end,
		 refac_syntax_lib:fold(F, Acc, FunNode)
	 end,
    do_search_similar_expr_1(AnnAST, F0).


do_search_similar_expr_1(AnnAST, Fun) ->
    F1 = fun (Node, Acc) ->
		 case refac_syntax:type(Node) of
		   function -> Fun(Node, Acc);
		   _ -> Acc
		 end
	 end,
    lists:reverse(refac_syntax_lib:fold(F1, [], AnnAST)).


get_expr_seqs(T) ->
    case refac_syntax:type(T) of
	clause ->
	    refac_syntax:clause_body(T);
	block_expr ->
	    refac_syntax:block_expr_body(T);
	try_expr ->
	    refac_syntax:try_expr_body(T);
	_ -> []
    end.

overlapped_locs({Start1, End1}, {Start2, End2}) ->
    Start1 =< Start2 andalso End2 =< End1 orelse
      Start2 =< Start1 andalso End1 =< End2.


do_search_similar_expr_1(FileName, Exprs1, Exprs2, RecordInfo, SimiScore, FunNode) ->
    Len1 = length(Exprs1),
    Len2 = length(Exprs2),
    case Len1 =< Len2 of
      true -> 
	    Exprs21 = lists:sublist(Exprs2, Len1),
	    {S1, E1} = refac_misc:get_start_end_loc(Exprs1),
	    {S2, E2} = refac_misc:get_start_end_loc(Exprs21),
	    case overlapped_locs({S1, E1}, {S2, E2}) of
		true -> [];
		_ ->
		    NormalisedExprs21 =normalise_expr(Exprs21, RecordInfo),
		    ExportedVars = vars_to_export(FunNode, E2, Exprs21),
		    case anti_unification:anti_unification(Exprs1, NormalisedExprs21) of
			none ->
			    do_search_similar_expr_1(FileName, Exprs1, tl(Exprs2), RecordInfo, SimiScore, FunNode);
			SubSt ->
			    EVs = [SE1 || {SE1, SE2} <- SubSt, refac_syntax:type(SE2) == variable,
					  lists:member({refac_syntax:variable_name(SE2), get_var_define_pos(SE2)}, ExportedVars)],
			    [{{FileName, {S2, E2}}, EVs, SubSt}]++
				do_search_similar_expr_1(FileName, Exprs1, tl(Exprs2), RecordInfo, SimiScore, FunNode)
		    end
	    end
    end.
    
get_var_define_pos(V) ->
    {value, {def, DefinePos}} = lists:keysearch(def,1, refac_syntax:get_ann(V)),
    DefinePos.

normalise_expr(Exprs, RecordInfo) ->
    try normalise_record_expr(Exprs, RecordInfo) of
	Exprs1->
	    Exprs1
    catch 
	_E1:_E2 ->
	    Exprs
    end.
normalise_record_expr(Exprs, RecordInfo) ->
    [ast_traverse_api:full_buTP(fun do_normalise_record_expr_1/2, E, {RecordInfo, true}) || E <- Exprs].
    
get_simi_score(SimiScore0) ->
    try  case SimiScore0 of
	     [] -> ?DefaultSimiScore;
	     _ -> list_to_float(SimiScore0)
	 end
    catch
	V -> case V>=0.1 andalso V=<1.0 of 
		 true -> V;
		 _ ->?DefaultSimiScore
	     end;			      
	_:_ -> throw({error, "Parameter input is invalid."})
    end.
    

get_fundef_and_expr(FName, Start, End, SearchPaths, TabWidth) ->
    {ok, {AnnAST, _Info}} = refac_util:parse_annotate_file(FName, true, SearchPaths, TabWidth),
    case interface_api:pos_to_fun_def(AnnAST, Start) of
      {ok, FunDef} ->
	  Exprs = interface_api:pos_to_expr_list(FunDef, Start, End),
	  case Exprs of
	    [] -> throw({error, "You have not selected an expression!"});
	    _ ->
		SE = refac_misc:get_start_end_loc(Exprs),
		RecordInfo = get_module_record_info(FName, SearchPaths, TabWidth),
		Exprs1 = normalise_expr(Exprs, RecordInfo),
		{FunDef, Exprs1, SE}
	  end;
      {error, _} -> throw({error, "You have not selected an expression!"})
    end.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Refactoring: Normalise record expression.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-spec(normalise_record_expr/5::(filename(), pos(), boolean(), [dir()], integer()) -> {ok, [filename()]}).
normalise_record_expr(FName, Pos = {Line, Col}, ShowDefault, SearchPaths, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:normalise_record_expr(~p, {~p,~p},~p, ~p, ~p).\n",
		 [?MODULE, FName, Line, Col, ShowDefault, SearchPaths, TabWidth]),
    Cmd = "CMD: " ++ atom_to_list(?MODULE) ++ ":normalise_record_expr(" ++ "\"" ++
	    FName ++ "\", {" ++ integer_to_list(Line) ++ ", " ++ integer_to_list(Col) ++ "},"
											   ++ atom_to_list(ShowDefault) ++ " [" ++ refac_misc:format_search_paths(SearchPaths)
	      ++ "]," ++ integer_to_list(TabWidth) ++ ").",
    {ok, {AnnAST, _Info}} = refac_util:parse_annotate_file(FName, true, [], TabWidth),
    RecordExpr = pos_to_record_expr(AnnAST, Pos),
    case refac_syntax:type(refac_syntax:record_expr_type(RecordExpr)) of
      atom -> ok;
      _ -> throw({error, "Wrangler can only normalise a record expression with an atom as the record name."})
    end,
    {AnnAST1, _Changed} = normalise_record_expr_1(FName, AnnAST, Pos, ShowDefault, SearchPaths, TabWidth),
    refac_util:write_refactored_files_for_preview([{{FName, FName}, AnnAST1}], Cmd),
    {ok, [FName]}.


normalise_record_expr_1(FName, AnnAST, Pos, ShowDefault, SearchPaths, TabWidth) ->
    RecordInfo = get_module_record_info(FName, SearchPaths, TabWidth),
    ast_traverse_api:stop_tdTP(fun do_normalise_record_expr/2, AnnAST, {Pos, RecordInfo, ShowDefault}).
    

do_normalise_record_expr(Node, {Pos, RecordInfo, ShowDefault}) ->
    case refac_syntax:type(Node) of
      record_expr ->
	  {S, E} = refac_misc:get_start_end_loc(Node),
	  case S =< Pos andalso Pos =< E of
	    true ->
		{ast_traverse_api:full_buTP(fun do_normalise_record_expr_1/2,
					    Node, {RecordInfo, ShowDefault}), true};
	    _ -> {Node, false}
	  end;
      _ -> {Node, false}
    end.

do_normalise_record_expr_1(Node, {RecordInfo, ShowDefault}) ->
    Fun = fun ({FName, FVal}, Fields) ->
		  R = [F || F <- Fields, refac_syntax:type(refac_syntax:record_field_name(F)) == atom,
			    refac_syntax:concrete(refac_syntax:record_field_name(F)) == FName],
		  case R of
		    [F] when ShowDefault -> [F];
		    [F] -> V = refac_syntax:record_field_value(F),
			   Cond = refac_syntax:type(V) == atom andalso refac_syntax:concrete(V) == undefined orelse
				    FVal =/= none andalso refac_prettypr:format(V) == refac_prettypr:format(FVal),
			   case Cond of
			     true -> [];
			     false -> [F]
			   end;
		    [] ->
			Fs = [F || F <- Fields, refac_syntax:type(refac_syntax:record_field_name(F)) == underscore],
			case Fs of
			  [F] ->
			      [refac_syntax:record_field(refac_syntax:atom(FName), refac_syntax:record_field_value(F))];
			  [] when ShowDefault ->
			      case FVal of
				none -> [refac_syntax:record_field(
					   refac_syntax:atom(FName), set_random_pos(refac_syntax:atom(undefined)))];
				_ -> [refac_syntax:record_field(refac_syntax:atom(FName), set_random_pos(FVal))]
			      end;
			  _ -> []
			end
		  end
	  end,
    case refac_syntax:type(Node) of
      record_expr ->
	  Arg = refac_syntax:record_expr_argument(Node),
	  Type = refac_syntax:record_expr_type(Node),
	  Fields = refac_syntax:record_expr_fields(Node),
	  case refac_syntax:type(Type) of
	    atom ->
		case lists:keysearch(refac_syntax:concrete(Type), 1, RecordInfo) of
		  {value, {_, Fields1}} ->
		      Fields2 = lists:append([Fun(F, Fields) || F <- Fields1]),
		      refac_misc:rewrite(Node, refac_syntax:record_expr(Arg, Type, Fields2));
		  _ ->
		      Node
		end;
	    _ -> Node
	  end;
      _ -> Node
    end.

set_random_pos(Node) ->
    refac_syntax:set_pos(Node, {-random:uniform(200), -random:uniform(200)}).
 
pos_to_record_expr(Tree, Pos) ->
    case
      ast_traverse_api:once_tdTU(fun pos_to_record_expr_1/2, Tree, Pos)
	of
      {_, false} ->
	  throw({error, "You have not selected a record expression, "
			"or the function containing the record expression selected does not parse."});
      {R, true} ->
	  R
    end.

pos_to_record_expr_1(Node, Pos) ->
    case refac_syntax:type(Node) of
      record_expr ->
	  {S, E} = refac_misc:get_start_end_loc(Node),
	  case S =< Pos andalso Pos =< E of
	    true -> {Node, true};
	    _ -> {[], false}
	  end;
      _ -> {[], false}
    end.


get_module_record_info(FName, SearchPaths, TabWidth) ->
    Dir = filename:dirname(FName),
    DefaultIncl = [filename:join(Dir, X) || X <- refac_misc:default_incls()],
    Includes = SearchPaths ++ DefaultIncl,
    case refac_epp:parse_file(FName, Includes, [], TabWidth, refac_util:file_format(FName)) of
      {ok, Forms, _} -> Forms1 = [F || F <- Forms, case F of
						     {attribute, _, file, _} -> false;
						     {attribute, _, type, {{record, _}, _, _}} -> false;
						     _ -> true
						   end],
			SyntaxTree = refac_recomment:recomment_forms(Forms1, []),
			Info = refac_syntax_lib:analyze_forms(SyntaxTree),
			case lists:keysearch(records, 1, Info) of
			  {value, {records, Records}} -> Records;
			  _ -> []
			end;
      {error, _Reason} -> []
    end.


vars_to_export(Fun, ExprEndPos, Expr) ->
    AllVars = refac_misc:collect_var_source_def_pos_info(Fun),
    ExprBdVarsPos = [Pos || {_Var, Pos} <- refac_misc:get_bound_vars(Expr)],
    [{V, DefPos} || {V, SourcePos, DefPos} <- AllVars,
		    SourcePos > ExprEndPos,
		    lists:subtract(DefPos, ExprBdVarsPos) == []].
