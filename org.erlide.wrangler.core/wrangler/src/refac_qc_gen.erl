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
%% Refactoring: Introduce a ?LET.
%%
%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%% 
%% =============================================================================================

%% =============================================================================================
%% @private
%% @hidden
-module(refac_qc_gen).

-export([test_cases_to_property/5, test_cases_to_property_eclipse/5]).


-include("../include/wrangler_internal.hrl").

%% THE CURRENT IMPLEMENTATION IS STILL A PROTOTYPE, NOT READY FOR RELEASE YET. 
%% TODO:
%%  1). to make sure the newly created function names does not conflict with existing one.
%%  2). what to do when the function paramters are not literals?
%%  3). Try to generate sensable variable names.

%% =============================================================================================
%%-spec(test_cases_to_property/5::(filename(), integer(), integer(), [dir()], integer()) -> {'ok', [filename()], boolean()} | {error, string()}).
test_cases_to_property(FileName, Line, Col, SearchPaths, TabWidth) ->
    test_cases_to_property(FileName, Line, Col, SearchPaths, TabWidth, emacs).


%%-spec(test_cases_to_property_eclipse/5::(filename(), integer(), integer(),[dir()], integer()) ->
%%	     {'ok', [{filename(), filename(),string()}]} |{error, string()}).
test_cases_to_property_eclipse(FileName, Line, Col,SearchPaths, TabWidth) ->
    test_cases_to_property(FileName, Line, Col, SearchPaths, TabWidth, eclipse).

test_cases_to_property(FileName, Line, Col, SearchPaths, TabWidth, Editor) ->
    ?wrangler_io("\nCMD: ~p:test_cases_to_property( ~p, ~p, ~p, ~p, ~p).\n",
		 [?MODULE, FileName, Line, Col, SearchPaths, TabWidth]),
    Cmd1 = "CMD: " ++ atom_to_list(?MODULE) ++ ":create_oneof(" ++ "\"" ++ 
	     FileName ++ "\", " ++ integer_to_list(Line) ++ 
	       ", " ++ integer_to_list(Col) ++ ", "
						++ "[" ++ wrangler_misc:format_search_paths(SearchPaths) ++ "]," ++ integer_to_list(TabWidth) ++ ").",
    {ok, {AnnAST, Info}} = wrangler_ast_server:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    case api_interface:pos_to_fun_name(AnnAST, {Line, Col}) of
	{ok, {Mod, Fun, Arity, _, DefPos}} ->
	    AnnAST1 = do_intro_oneof(AnnAST, {Mod, Fun, Arity, DefPos}),
	    HasWarningMsg =  not  is_quickcheck_used(Info),
	    case HasWarningMsg of
		true ->
		    ?wrangler_io("\n=================================================================\n", []),
		    ?wrangler_io("WARNING: Please include eqc.hrl in order to use QuickCheck!\n", []);
		false ->
		    ok
	    end,
	    case Editor of
		emacs ->
		    Res = [{{FileName, FileName}, AnnAST1}],
		    wrangler_write_file:write_refactored_files_for_preview(Res, TabWidth, Cmd1),
		    {ok, [FileName], HasWarningMsg};
		eclipse ->
		    FileContent = wrangler_prettypr:print_ast(wrangler_misc:file_format(FileName), AnnAST1, TabWidth),
		    {ok, [{FileName, FileName, FileContent}]}
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.


is_quickcheck_used(ModuleInfo) ->
    case lists:keysearch(imports, 1, ModuleInfo) of 
	{value, {imports, Imps}} ->
	    Ms = [M || {M, _} <-Imps],
	    lists:member(eqc, Ms) andalso lists:member(eqc_gen, Ms);
	false ->
	    false
    end.

do_collect_parameters(AnnAST, {M, F, A}) ->
    Fun = fun (Node, Acc) ->
		  case wrangler_syntax:type(Node) of
		      application ->
			  Op = wrangler_syntax:application_operator(Node),
			  As = wrangler_syntax:get_ann(Op),
			  case lists:keysearch(fun_def, 1, As) of
			      {value, {fun_def, {M, F, A, _, _}}} ->
				  Args = wrangler_syntax:application_arguments(Node),
				  case api_refac:free_vars(Args) of
				      [] ->
					  [Args| Acc];
				      _ -> Acc
				  end;
			      _ -> Acc
			  end;
		      _ -> Acc
		  end
	  end,
    lists:reverse(api_ast_traverse:fold(Fun, [], AnnAST)).


do_intro_oneof(AnnAST, {Mod, Fun, Arity, DefPos}) ->
    case Arity of
	0 ->
	    throw({error, "The function selected does not need any input data!"});
	_ -> ok
    end,
    Data= do_collect_parameters(AnnAST, {Mod, Fun, Arity}),
    case Data of
	[] -> throw({error, "No test data has been collected for the function selected."});
	_ -> ok
    end,
    ParNames =case api_interface:pos_to_fun_def(AnnAST, DefPos) of
		  {ok, FunDef} ->
		      get_parameter_names(FunDef);
		  {error, _} -> [make_new_var(I) ||I<-lists:seq(1, Arity)]
	      end,
    OneOfGenName = case Arity >1 of
		       true ->
			   wrangler_syntax:atom(list_to_atom(atom_to_list(Fun) ++ "_gen"));
		       false->
			   wrangler_syntax:atom(list_to_atom(atom_to_list(Fun) ++ "_gen"))
		   end,
    
    OneOfPropName= case Arity>1 of
		       true ->
			   wrangler_syntax:atom(list_to_atom(atom_to_list(Fun) ++ "_prop"));
		       false ->
			   wrangler_syntax:atom(list_to_atom(atom_to_list(Fun) ++ "_prop"))
		   end,
    OneOfGenForm = make_oneof_gen(OneOfGenName, Data),
    OneOfPropForm = make_oneof_prop(OneOfGenName, OneOfPropName, ParNames, {Mod, Fun, Arity, DefPos}),
    ForALLFuns = case Arity>1 of 
		     true ->
			 make_forall_gen_props(Data, ParNames, {Mod, Fun, Arity, DefPos});
		     false ->
			 []
		 end,
    Forms = wrangler_syntax:form_list_elements(AnnAST),
    NewForms = Forms++[OneOfGenForm, OneOfPropForm]++ ForALLFuns,
    wrangler_syntax:form_list(NewForms).


get_parameter_names(FunDef) ->
    Arity = wrangler_syntax:function_arity(FunDef),
    Cs = wrangler_syntax:function_clauses(FunDef),
    Pats=zip_list([wrangler_syntax:clause_patterns(C) ||C <- Cs]),
    Is = lists:seq(1, Arity),
    [get_parameter_name(I, P)||{I, P}<-lists:zip(Is, Pats)].

get_parameter_name(Index, Pars) ->
    Pars1=[case wrangler_syntax:type(P) of
	       variable ->
		   wrangler_syntax:variable_name(P);
	       _ -> '_'		  
	   end || P<- Pars],
    case lists:usort(Pars1) of
	[V] when V/='_' ->
	  wrangler_syntax:variable(V);
	_ -> make_new_var(Index)
    end.

make_oneof_gen(OneOfGenName, Data) ->
    NewData = wrangler_syntax:list([case length(D) of
				        1 -> hd(D);
				        _ -> wrangler_syntax:tuple(D)
				    end || D <- Data]),
    Op = wrangler_syntax:atom(oneof),
    App = wrangler_syntax:application(Op, [NewData]),
    Clause = wrangler_syntax:clause([], [], [App]),
    wrangler_misc:reset_attrs(wrangler_syntax:function(OneOfGenName, [Clause])).

make_oneof_prop(OneOfGenName, OneOfPropName, ParNames, {Mod, Fun, Arity, DefPos}) ->
    Pat = case Arity of
	      1 -> hd(ParNames);
	      _ ->
		  wrangler_syntax:tuple(ParNames)
	  end,
    Gen = wrangler_syntax:application(OneOfGenName, []),
    Prop = make_prop(ParNames, {Mod, Fun, Arity}, DefPos),
    Body = make_for_all(Pat, Gen, Prop),
    Clause = wrangler_syntax:clause([], [], [Body]),
    wrangler_misc:reset_attrs(wrangler_syntax:function(OneOfPropName, [Clause])).
    

make_forall_gen_props(ListOfPars1, ParNames, {Mod, Fun, Arity, DefPos}) ->
    ListOfPars=[[simplify_expr(P)||P<-Pars]||Pars<-ListOfPars1],
    Length= length(hd(ListOfPars)),
    Is = lists:seq(1, Length),
    ParMaps =[[{lists:sublist(Pars,1, I-1), lists:nth(I, Pars)}|| Pars<-ListOfPars]||I<-Is],
    NewParMaps =[create_dependency_fun(ParMap) || ParMap <- ParMaps],
    ForAllPropFun = make_forall_prop(ParNames,NewParMaps, {Mod, Fun, Arity, DefPos}),
    GenFuns =[make_generator_fun(Fun,ParMap, ParNames)||ParMap<-NewParMaps],
    [ForAllPropFun|GenFuns].

make_forall_prop(ParNames, ParMaps, {Mod, Fun, Arity, DefPos}) ->
    Prop = make_prop(ParNames, {Mod, Fun, Arity}, DefPos),
    ForAllExpr = generate_forall_expr(ParNames, Fun, lists:reverse(ParMaps), Prop),
    Clause = wrangler_syntax:clause([],[],[ForAllExpr]),
    ForAllPropName = wrangler_syntax:atom(list_to_atom(atom_to_list(Fun) ++ "_forall_prop")),
    wrangler_misc:reset_attrs(wrangler_syntax:function(ForAllPropName, [Clause])).

make_generator_fun(FunName, {Pars, Rets}, ParNames) ->
    ZippedParsRets = wrangler_misc:group_by(1, lists:zip(Pars, Rets)),
    GenFunName = make_new_gen_fun_name(FunName, length(hd(Pars))+1, ParNames),
    Clause = [make_clause(lists:unzip(ZippedParRet)) || ZippedParRet <- ZippedParsRets],
    wrangler_misc:reset_attrs(wrangler_syntax:function(GenFunName, Clause)).
    

make_prop(ParNames, {Mod, Fun, _Arity}, DefPos) ->
    Op =case DefPos of 
	    ?DEFAULT_LOC ->
		wrangler_syntax:module_qualifier(wrangler_syntax:atom(Mod),
					         wrangler_syntax:atom(Fun));
	    _ ->
		wrangler_syntax:atom(Fun)
	end,
    Body = wrangler_syntax:application(Op, ParNames),
    Cs =[wrangler_syntax:clause([wrangler_syntax:variable('Res')], [], [wrangler_syntax:atom('true')])],
    Handlers=[wrangler_syntax:clause([wrangler_syntax:class_qualifier(wrangler_syntax:underscore(),
								      wrangler_syntax:underscore())],
				      [], [wrangler_syntax:atom('false')])],
    
    wrangler_syntax:try_expr([Body], Cs, Handlers).

make_clause({[Pars| _], Ret}) ->
    Pats = [P || P <- Pars, P /= wrangler_syntax:atom('_')],
    case length(Ret)>1 of
	true ->
	    Op = wrangler_syntax:atom(oneof),
	    App = wrangler_syntax:application(Op, wrangler_misc:remove_duplicates(Ret)),
	    wrangler_syntax:clause(Pats, [], [App]);
	_ ->
	    wrangler_syntax:clause(Pats, [], Ret)
    end.
	
generate_forall_expr(ParNames, FunName, [_], Prop) ->
    Pat = hd(ParNames),
    GenFunName = make_new_gen_fun_name(FunName,1, ParNames),
    App = wrangler_syntax:application(GenFunName, []),
    make_for_all(Pat, App, Prop);

generate_forall_expr(ParNames, FunName, _NewParMaps=[{Pars, _Ret}|T], Prop) ->
    Index =length(hd(Pars))+1,
    Pat = lists:nth(Index, ParNames),
    GenFun = make_new_gen_fun_name(FunName, Index, ParNames),
    Args=lists:zip(lists:seq(1, Index-1),hd(Pars)),
    Args1=[lists:nth(I, ParNames)||{I, A} <- Args, A /= wrangler_syntax:atom('_')],
    App = wrangler_syntax:application(GenFun, Args1),
    generate_forall_expr(ParNames, FunName, T, make_for_all(Pat, App, Prop)).
    


create_dependency_fun(ParMap) ->
    {Pars, Rets} = lists:unzip(ParMap),
    case lists:usort(Rets) of
	[R] ->
	    {[[wrangler_syntax:atom('_')||_P <- hd(Pars)]],[R]};
	_ ->
	  {simplify_pars(Pars), Rets}
    end.

simplify_pars(Pars) ->
    Len=length(hd(Pars)),
    case Len =<1 of 
	true->
	    Pars;
	false -> 
	    Pars1 =[lists:sublist(P,1, Len-1)||P<-Pars],
	    Pars2 =[[lists:last(P)] || P<-Pars],
	    case length(lists:usort(Pars1))== length(lists:usort(Pars)) of
		true ->
		    [P ++ [wrangler_syntax:atom('_')]||P <- simplify_pars(Pars1)];
		_ -> NewPars=lists:zip(simplify_pars(Pars1), Pars2),
		     [lists:append(tuple_to_list(P))||P<-NewPars]
	    end
    end.

    
make_for_all(Pat, Gen, Prop) ->
    Args = [Pat, Gen, Prop],
    wrangler_syntax:macro(wrangler_syntax:variable('FORALL'),
		          Args).

make_new_var(Index) ->
    wrangler_syntax:variable(
         list_to_atom(
	   "NewPat" ++ integer_to_list(Index))).

make_new_gen_fun_name(FunName, Index, ParNames) ->
    ParName = wrangler_misc:to_lower(atom_to_list(
				       wrangler_syntax:variable_name(
				            lists:nth(Index, ParNames)))),
    wrangler_syntax:atom(list_to_atom(atom_to_list(FunName) ++ "_" ++ ParName ++ "_gen")).
 

simplify_expr(Exp) when is_list(Exp) ->
    [simplify_expr(E) || E <- Exp];
simplify_expr(Exp) ->
    api_ast_traverse:full_buTP(
      fun (Node, _Others) ->
	      do_simplify_expr(Node)
      end, Exp, {}).


do_simplify_expr(Node) ->
    Node1 = case wrangler_syntax:type(Node) of
		integer ->
		    wrangler_syntax:default_literals_vars(Node, wrangler_syntax:integer_value(Node));
		float ->
		    wrangler_syntax:default_literals_vars(Node, wrangler_syntax:float_value(Node));
		char ->
		    wrangler_syntax:default_literals_vars(Node, wrangler_syntax:char_value(Node));
		string ->
		    wrangler_syntax:default_literals_vars(Node, wrangler_syntax:string_value(Node));
		atom -> wrangler_syntax:default_literals_vars(
			     Node, wrangler_syntax:atom_value(Node));
		nil -> wrangler_syntax:default_literals_vars(Node, nil);
		underscore -> wrangler_syntax:default_literals_vars(Node, '_');
		_ ->
		    Node
	    end,
    set_default_ann(Node1).

set_default_ann(Node) ->
    wrangler_syntax:set_pos(wrangler_syntax:remove_comments(wrangler_syntax:set_ann(Node, [])), {0,0}).
			  

zip_list(ListOfLists) ->    
    zip_list_1(ListOfLists, []).

zip_list_1([[]|_T], Acc)  ->
    Acc;
zip_list_1(ListOfLists, Acc)->      
    zip_list_1([tl(L) || L <-ListOfLists],
	       Acc ++ [[hd(L)|| L  <- ListOfLists]]).

