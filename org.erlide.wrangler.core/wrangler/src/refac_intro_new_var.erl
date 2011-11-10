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
%%       names of its contributors may be used to endoorse or promote products
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
%% ============================================================================================
%% Refactoring: Introduce a new variable to represent an expression selected.
%%
%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%% 
%% =============================================================================================

%% =============================================================================================

%% @private
-module(refac_intro_new_var).

-export([intro_new_var/7, intro_new_var_eclipse/6]).


-include("../include/wrangler_internal.hrl").

%% =============================================================================================
%%-spec(intro_new_var/6::(filename(), pos(), pos(), string(), [dir()],atom(), integer()) ->
%% 	     {'ok', [filename()]}).
intro_new_var(FileName, Start={_SLine,_SCol}, End={_ELine, _ECol}, NewVarName, SearchPaths, Editor, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:intro_new_var(~p, {~p,~p}, {~p,~p}, ~p, ~p, ~p,~p).\n",
		 [?MODULE, FileName, _SLine, _SCol, _ELine, _ECol, NewVarName, SearchPaths, Editor, TabWidth]),
    intro_new_var_1(FileName, Start, End, NewVarName, SearchPaths, TabWidth, Editor).

%%-spec(intro_new_var_eclipse/6::(filename(), pos(), pos(), string(), [dir()], integer()) ->
%% 	      {ok, [{filename(), filename(), string()}]}).
intro_new_var_eclipse(FileName, Start, End, NewVarName, SearchPaths, TabWidth) ->
    intro_new_var_1(FileName, Start, End, NewVarName, SearchPaths, TabWidth, eclipse).

intro_new_var_1(FileName, Start = {Line, Col}, End = {Line1, Col1}, NewVarName0, SearchPaths, TabWidth, Editor) ->
     Cmd = "CMD: " ++ atom_to_list(?MODULE) ++ ":intro_new_var(" ++ "\"" ++ 
	    FileName ++ "\", {" ++ integer_to_list(Line) ++ ", " ++ 
	      integer_to_list(Col) ++ "}," ++ "{" ++ integer_to_list(Line1) ++ ", "
        ++ integer_to_list(Col1) ++ "}," ++ "\"" ++ NewVarName0 ++ "\"," ++ integer_to_list(TabWidth) 
        ++ " " ++ atom_to_list(Editor)++ ").",
    case api_refac:is_var_name(NewVarName0) of
	true -> ok;
	false -> throw({error, "Invalid new variable name."})
    end,
    NewVarName = list_to_atom(NewVarName0),
    {ok, {AnnAST, _Info}} = wrangler_ast_server:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    case api_interface:pos_to_expr(AnnAST, Start, End) of
	{ok, Exp} ->
	    Exp;
	{error, _} ->
	    throw({error, "You have not selected an expression, "
			  "or the function containing the expression does not parse."}),
	    Exp = none
    end,
    case api_interface:expr_to_fun(AnnAST, Exp) of
	{ok, Fun} ->
	    Fun;
	{error, _} ->
	    throw({error, "You have not selected an expression within a function."}),
	    Fun = none
    end,
    ok = cond_check(Fun, Exp, NewVarName),
    intro_new_var_1(FileName, AnnAST, Fun, Exp, NewVarName, Editor, TabWidth, Cmd).

intro_new_var_1(FileName, AnnAST, Fun, Expr, NewVarName, Editor, TabWidth, Cmd) ->
    AnnAST1 = do_intro_new_var(AnnAST, Fun, Expr, NewVarName),
    case Editor of
	emacs ->
	    Res = [{{FileName, FileName}, AnnAST1}],
	    wrangler_write_file:write_refactored_files_for_preview(Res, TabWidth, Cmd),
	    {ok, [FileName]};
	eclipse ->
	    FileFormat = wrangler_misc:file_format(FileName),
	    FileContent = wrangler_prettypr:print_ast(FileFormat, AnnAST1, TabWidth),
	    {ok, [{FileName, FileName, FileContent}]}
    end.

cond_check(Form, Expr, NewVarName) ->
    ok=variable_replaceable(Expr),
    {Body, Statement} = get_inmost_enclosing_body_expr(Form, Expr),
    ExprFVs = api_refac:free_vars(Expr),
    SEnvs = api_refac:env_vars(Statement),
    Vs = [V||{V, Loc}<-ExprFVs--SEnvs, Loc/={0,0}],
    case Vs of
        [] ->
	    ok;
	_ ->
	    Msg = io_lib:format("The exprssion selected contains locally "
				"declared variables(s) ~p.", [Vs]),
	    throw({error, lists:flatten(Msg)})
    end,
    BodyBdVars = get_bound_vars(wrangler_syntax:block_expr(Body)),
    ExistingVars = element(1, lists:unzip(BodyBdVars ++ SEnvs)),
    case lists:member(NewVarName, ExistingVars) of
	true ->
	    throw({error, "The new variable name conflicts with, or shadows, "
			  "existing variable declarations."});
	false -> ok
    end.

variable_replaceable(Exp) ->
    Ann = wrangler_syntax:get_ann(Exp),
    case lists:keysearch(category, 1, Ann) of
	{value, {category, guard_expression}} ->
            throw({error, "Introducing a variable in a guard expression is not supported."});
      	_ -> ok
    end.

get_bound_vars(Tree) ->
    F = fun (T, B) ->
		As = wrangler_syntax:get_ann(T),
		case lists:keysearch(bound, 1, As) of
		    {value, {bound, BdVars1}} -> BdVars1++B;
		    _ -> B
		end
	end,
    lists:usort(api_ast_traverse:fold(F, [], Tree)).

do_intro_new_var(AnnAST, FunForm, Expr, NewVarName) ->
    NewFun = do_intro_new_var_in_fun(FunForm, Expr, NewVarName),
    Forms = wrangler_syntax:form_list_elements(AnnAST),
    Fun = fun (Form) ->
		  case wrangler_syntax:type(Form) of
		    function ->
			  FormPos = wrangler_syntax:get_pos(Form),
			  FunFormPos =  wrangler_syntax:get_pos(FunForm),
			  case FormPos == FunFormPos of
			      true ->
				  NewFun;
			      false ->
				  Form
			  end;
		      _ ->
			  Form
		  end
	  end,
    wrangler_syntax:form_list([Fun(Form) || Form <- Forms]).

do_intro_new_var_in_fun(Fun, Expr, NewVarName) ->
    Body = get_inmost_enclosing_clause(Fun, Expr),
    {NewFun1, _} = api_ast_traverse:stop_tdTP(
		     fun insert_and_replace/2, Fun,
		     {Body, Expr, NewVarName}),
    NewFun1.

insert_and_replace(Node, {InMostClauseExpr, Expr, NewVarName}) ->
    case Node of 
	InMostClauseExpr ->
	    {do_insert_and_replace(Node, Expr, NewVarName), true};
	_ -> {Node, false}
    end.

do_insert_and_replace(Node, Expr, NewVarName) ->
    MatchExpr = make_match_expr(Expr, NewVarName),
    ExprPos = wrangler_syntax:get_pos(Expr),
    Body = case wrangler_syntax:type(Node) of
	       clause ->
		   wrangler_syntax:clause_body(Node);
	       block_expr ->
		   wrangler_syntax:block_expr_body(Node);
	       try_expr ->
		   wrangler_syntax:try_expr_body(Node)
	   end,
    Fun = fun (ExprStatement) ->
                  Range = wrangler_misc:get_start_end_loc_with_comment(ExprStatement),
                  NewExpr=wrangler_syntax:copy_pos(ExprStatement, wrangler_misc:update_ann(MatchExpr, {range, Range})),
                  {ExprStatement1, _} = replace_expr_with_var(Expr, NewVarName, ExprStatement),
                  [NewExpr, ExprStatement1]
          end,
    {Body1, Body2} = lists:splitwith(
                       fun(E) ->
                               {Start, End} = wrangler_misc:start_end_loc(E),
                               not (Start=<ExprPos andalso ExprPos=<End)
                       end, Body),
    NewBody =case Body2 of 
                 [] ->  
                     Body1; 
                 [B|Bs] ->
                     [B1,B2] = Fun(B),
                     case wrangler_syntax:type(B2) == variable andalso Bs /= [] of
                         true ->
                             Body1++[B1|Bs];
                         false ->
                             case {lists:reverse(Body1), Bs} of
                                 {[], []} ->
                                     [B1,wrangler_syntax:add_ann({layout, vertical}, B2)];
                                 {[], [B3|Bs1]} ->
                                     {{BL, _}, _} = wrangler_misc:start_end_loc(B),
                                     {{B3L, _},_} = wrangler_misc:start_end_loc(B3),
                                     case BL==B3L of 
                                         true ->
                                             B21=wrangler_syntax:add_ann({layout, horizontal}, B2),
                                             B31=wrangler_syntax:add_ann({layout, horizontal}, B3),
                                             [B1, B21, B31|Bs1];
                                         false ->
                                             B21=wrangler_syntax:add_ann({layout, vertical}, B2),
                                             B31=wrangler_syntax:add_ann({layout, vertical}, B3),
                                             [B1, B21, B31|Bs1]
                                     end;
                                 {[B0|_], []} ->
                                     {{B0L, _},_} =wrangler_misc:start_end_loc(B0),
                                     {{BL, _}, _} = wrangler_misc:start_end_loc(B),
                                     case B0L==BL of 
                                         true ->
                                     B21=wrangler_syntax:add_ann({layout, horizontal}, B2),
                                     Body1 ++ [B1,B21];
                                         false->
                                             B21=wrangler_syntax:add_ann({layout, vertical}, B2),
                                             Body1++[B1, B21]
                                     end;
                                 {[B0|_], [B3|Bs1]} ->
                                     {{B0L, _},_} =wrangler_misc:start_end_loc(B0),
                                     {{BL, _}, _} = wrangler_misc:start_end_loc(B),
                                     B21= case B0L==BL of 
                                              true ->
                                          wrangler_syntax:add_ann({layout, horizontal}, B2);
                                              false->
                                                  wrangler_syntax:add_ann({layout, vertical}, B2)
                                          end,
                                     {{B3L, _},_} = wrangler_misc:start_end_loc(B3),
                                     case BL==B3L of 
                                         true ->
                                             B31=wrangler_syntax:add_ann({layout, horizontal}, B3),
                                             Body1++[B1, B21, B31|Bs1];
                                         false ->
                                             B31=wrangler_syntax:add_ann({layout, vertical}, B3),
                                             Body1++[B1, B21, B31|Bs1]
                                     end
                             end
                     end
             end,
    case wrangler_syntax:type(Node) of
	clause ->
	    Pat = wrangler_syntax:clause_patterns(Node),
	    Guard = wrangler_syntax:clause_guard(Node),
	    wrangler_misc:rewrite(Node, wrangler_syntax:clause
                                          (Pat, Guard, NewBody));
	block_expr ->
	    wrangler_misc:rewrite(Node,wrangler_syntax:block_expr(NewBody));
	try_expr ->
	    C = wrangler_syntax:try_expr_clauses(Node),
	    H = wrangler_syntax:try_expr_handlers(Node),
	    A = wrangler_syntax:try_expr_after(Node),
	    wrangler_misc:rewrite(Node,wrangler_syntax:try_expr(NewBody, C, H, A))
    end.


replace_expr_with_var(Expr, NewVarName, ExprStatement) ->
    api_ast_traverse:stop_tdTP(fun do_replace_expr_with_var/2,
			       ExprStatement, {Expr, NewVarName}).

do_replace_expr_with_var(Node, {Expr, NewVarName}) ->
    case Node of
	Expr ->
	    NewVarExpr = wrangler_misc:rewrite(
			      Expr,wrangler_syntax:variable(NewVarName)),
	    {NewVarExpr, true};
	_ -> {Node, false}
    end.

make_match_expr(Expr, NewVarName) ->
    Pat = wrangler_syntax:variable(NewVarName),
    PreComs = wrangler_syntax:get_precomments(Expr),
    PostComs=wrangler_syntax:get_postcomments(Expr),
    Expr1=wrangler_syntax:set_precomments(wrangler_syntax:set_postcomments(Expr, []),[]),
    MatchExpr=wrangler_syntax:match_expr(Pat, Expr1),
    wrangler_syntax:set_precomments(wrangler_syntax:set_postcomments(MatchExpr, PostComs), PreComs).

get_inmost_enclosing_clause(Form, Expr) -> 
    ExprPos = wrangler_syntax:get_pos(Expr),
    Fun = fun (Node, S) ->
		  Type = wrangler_syntax:type(Node),
		  case lists:member(Type, [clause, block_expr, try_expr]) of
		      true ->
			  Body = case Type of
				     clause ->
					 wrangler_syntax:clause_body(Node);
				     block_expr ->
					 wrangler_syntax:block_expr_body(Node);
				     try_expr ->
					 wrangler_syntax:try_expr_body(Node)
				 end,
			  {Start, _End} = wrangler_misc:start_end_loc(hd(Body)),
			  {_, End} = wrangler_misc:start_end_loc(lists:last(Body)),
			  case Start =< ExprPos andalso ExprPos =< End of
			      true ->
				  [{Node, End}| S];
			      _ -> S
			  end;
		      _ ->
			  S
		  end
	  end,
    Res = lists:keysort(2, api_ast_traverse:fold(Fun, [], Form)),
    case Res of
	[{Node, _}| _] ->
	    Node;
	_ -> throw({error, "Wrangler internal error."})
    end.

get_inmost_enclosing_body_expr(Form, Expr) ->
    ExprPos = wrangler_syntax:get_pos(Expr),
    Fun = fun (Node, S) ->
		  Type = wrangler_syntax:type(Node),
		  case lists:member(Type, [clause, block_expr, try_expr]) of
		      true ->
			  Body = case Type of
				     clause ->
					 wrangler_syntax:clause_body(Node);
				     block_expr ->
					 wrangler_syntax:block_expr_body(Node);
				     try_expr ->
					 wrangler_syntax:try_expr_body(Node)
				 end,
			  {Start, _End} = wrangler_misc:start_end_loc(hd(Body)),
			  {_, End} = wrangler_misc:start_end_loc(lists:last(Body)),
			  case Start =< ExprPos andalso ExprPos =< End of
			      true ->
				  EnclosingExpr = get_enclosing_expr(Body, Expr),
				  [{Body, End, EnclosingExpr}| S];
			      _ -> S
			  end;
		      _ ->
			  S
		  end
	  end,
    Res = lists:keysort(2, api_ast_traverse:fold(Fun, [], Form)),
    case Res of
	[{Body, _, E}| _] ->
	    {Body, E};
	_ ->
	    throw({error, "Wrangler failed to perform this refactoring."})
    end.

get_enclosing_expr(Body, Expr) ->
    ExprPos = wrangler_syntax:get_pos(Expr),
    Fun = fun (ExprStatement) ->
		  {Start, End} = wrangler_misc:start_end_loc(ExprStatement),
		  case Start =< ExprPos andalso  ExprPos =< End of
		      true ->
			  [ExprStatement];
		      false ->
			  []
		  end
	  end,
    hd(lists:append([Fun(B) || B <- Body])).
