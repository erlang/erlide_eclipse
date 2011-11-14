%%@hidden
%%@private
-module(wrangler_expand_rule).

-export([parse_transform/2]).

%% -define(ERROR(R, T, F, I),
%% 	begin
%% 	    rpt_error(R, T, F, I),
%% 	    throw({error,erl_syntax:get_pos(
%% 			   proplists:get_value(form,I)),{unknown,R}})
%% 	end).

-define(ERROR(R, T, F, I),
	begin
	    rpt_error(R, T, F, I),
	    throw({error,R})
	end).

-import(erl_syntax, [clause/3,
		     clause_patterns/1,
		     clause_body/1,
		     clause_guard/1,
		     match_expr/2,
		     function_clauses/1,
		     get_pos/1,
		     add_ann/2,
		     get_ann/1]).


parse_transform(Forms, Options) ->
    Forms0=parse_transform_1({api_refac, check_collect_template, 2},
                             fun(Form, _Context) ->
                                     [Temp, MacroName] = erl_syntax:application_arguments(Form),
                                     check_collect_template(Temp, MacroName)
                             end, Forms, Options),
    Forms1=parse_transform_1({api_refac, expand_match, 3},
                             fun(Form, _Context) ->
                                     [TempStr, Tree, Cond]=erl_syntax:application_arguments(Form), 
                                     expand_match(TempStr, Tree, Cond)
                             end, Forms0, Options),
    Forms2=parse_transform_1({api_refac, generate_bindings, 2},
                              fun(Form, _Context) ->
                                      [Temp, BindVar]=erl_syntax:application_arguments(Form), 
                                      expand_generate_bindings(Temp, BindVar)
                              end, Forms1, Options),
    Forms3=parse_transform_1({api_refac, template, 1},
                             fun(Form, _Context) ->
                                     expand_template(Form)
                             end, Forms2, Options),
    Forms4=parse_transform_1({api_refac, make_cond, 2},
                             fun(Form, _Context) ->
                                     expand_cond(Form)
                             end, Forms3, Options),
    Forms5=parse_transform_1({api_refac, expand_collector, 1},
                             fun(Form, _Context) ->
                                     expand_collector(Form)
                             end, Forms4, Options),
    Forms6=parse_transform_1({api_refac, quote, 1},
                             fun(Form, _Context) ->
                                     expand_quote(Form)
                             end, annotate_forms(Forms5), Options),
    %% wrangler_io:format("Form6:\n~s\n", [[erl_prettypr:format(F)++"\n\n"||F <- Forms6]]),
    Forms6.


annotate_forms(Forms) ->
    [begin
         F1=erl_syntax:revert(F),
         case element(1, F1) of 
             attribute -> F1;
             _ -> 
                 wrangler_syntax_lib:annotate_bindings(F1)
         end
     end||F<-Forms].
   

expand_template(TempApp) ->
    [Str] = erl_syntax:application_arguments(TempApp),
    Pos = erl_syntax:get_pos(Str),
    Ln = case Pos of
             {L, _} -> L;
             _ -> Pos
         end,
    case wrangler_syntax:type(Str) of
        string -> ok;
        _ ->
            erlang:error(lists:flatten(io_lib:format("The argument of the ?T macro, at line ~p, must be a string literal.", [Ln]))) 
    end,
    check_template_syntax(Str),
    Op= erl_syntax:module_qualifier(erl_syntax:atom(wrangler_misc), erl_syntax:atom(extended_parse_annotate_expr)),
    erl_syntax:application(Op, [Str, erl_syntax:integer(Ln)]).
   
is_meta_apply_temp(Temp) ->
    case erl_syntax:type(Temp) of 
        tuple ->
            Es = erl_syntax:tuple_elements(Temp),
            case Es of 
                [E|_] ->
                    erl_syntax:type(E) == atom andalso
                        erl_syntax:atom_value(E)=='meta_apply';
                _ -> false
            end;
        _ -> false
    end.

check_collect_template(Temp, Macro)->
    case is_meta_apply_temp(Temp) of
        true ->
            erl_syntax:atom(ok);
        false ->
            MacroName = erl_syntax:atom_value(Macro),
            case is_template_app(Temp) of 
                true ->
                    [Str] = erl_syntax:application_arguments(Temp),
                    check_template_syntax(Str),
                    erl_syntax:atom(ok);
                false ->
                    Pos = erl_syntax:get_pos(Temp),
                    Ln = case Pos of 
                             {L, _} -> L;
                             _ -> Pos
                         end,
                    erlang:error(
                      lists:flatten(
                        io_lib:format("The first argument of the ~p macro, at line ~p, " 
                                      "must be a template.", [MacroName,Ln])))
            end
    end.

is_template_app(Temp) ->
    case erl_syntax:type(Temp) of
        application ->
            {api_refac, {template, 1}} == erl_syntax_lib:analyze_application(Temp);
        _ -> false
    end.

check_template_syntax(Template) ->
    Pos = erl_syntax:get_pos(Template),
    Str = erl_syntax:string_value(Template),
    try wrangler_misc:parse_annotate_expr(Str, Pos)
    catch
        throw:Error ->
            Ln = case Pos of 
                     {L, _} -> L;
                     _ -> Pos
                 end,
            erlang:error(lists:flatten(io_lib:format("Syntax error in template at line ~p:~n~p.~n", [Ln, Error])))
    end.
 
expand_quote(QuoteApp) ->
    [Temp] = erl_syntax:application_arguments(QuoteApp),
    Pos = erl_syntax:get_pos(Temp),
    Op= erl_syntax:module_qualifier(erl_syntax:atom(wrangler_misc), erl_syntax:atom(parse_annotate_expr)),
    App =erl_syntax:application(Op, [Temp, erl_syntax:integer(Pos)]),
    EnvVars = element(1, lists:unzip(env_vars(Temp))),
    Binds=erl_syntax:list([erl_syntax:tuple([erl_syntax:atom(VarName),
                                             erl_syntax:variable(VarName)])
                           ||VarName<-EnvVars, 
                             is_meta_variable_name(VarName),
                             not is_meta_variable_value_name(VarName)]),
    Op1 = erl_syntax:module_qualifier(erl_syntax:atom(api_refac), erl_syntax:atom(subst)),
    erl_syntax:application(Op1,[App, Binds]).
   
expand_generate_bindings(Temp, BindVar) ->
    case is_meta_apply_temp(Temp) of 
        true ->
            %% [_, MetaAppTemp] = erl_syntax:tuple_elements(Temp),
            %% Es = erl_syntax:list_elements(MetaAppTemp),
            %%  Temps=[hd(erl_syntax:tuple_elements(E))||E<-Es],
            %%  get_meta_var_and_atoms(Temps);
            %% %% {[],[]};
            erl_syntax:block_expr([erl_syntax:atom(ok)]);
        false ->
            {MetaVars, MetaAtoms}=get_meta_var_and_atoms([Temp]),
            BindVarName = erl_syntax:atom_value(BindVar),
            MatchExprs = lists:append([make_match_expr(V, P, BindVarName)||
                                          {V,P} <- MetaVars, V=/='_This@', V=/='_File@']),
            MetaAtomMatchExprs=lists:append([make_match_expr_for_meta_atom(V, P,BindVarName)||
                                                {V,P}<-MetaAtoms]),
            erl_syntax:block_expr(MatchExprs++MetaAtomMatchExprs)
    end.
            

get_meta_var_and_atoms(Temps) ->
    {MetaVars, MetaAtoms}=lists:unzip([get_meta_var_and_atoms_1(Temp)
                                       ||Temp<-Temps]),
    {lists:ukeysort(1, lists:append(MetaVars)), 
     lists:ukeysort(1, lists:append(MetaAtoms))}.

get_meta_var_and_atoms_1(Temp) ->
    [TempStr]=erl_syntax:application_arguments(Temp),
    Pos = erl_syntax:get_pos(TempStr),
    TempAST = wrangler_misc:parse_annotate_expr(erl_syntax:string_value(TempStr), Pos),
    {MetaVars, MetaAtoms} =  collect_meta_vars_and_atoms(TempAST),
    {MetaVars, MetaAtoms}.


expand_match(Temp, Node, Cond) ->
    Pos = erl_syntax:get_pos(Node),
    Ln = case Pos of
             {L, _} -> L;
             _ -> Pos
         end,
    case is_template_app(Temp) of 
        true ->
            ok;
        false -> 
            erlang:error(lists:flatten(io_lib:format(
                                         "The first argument of the ?MATCH macro, at line ~p, must be template.", [Ln])))
    end,
    [TempStr]=wrangler_syntax:application_arguments(Temp),
    Pos1 = erl_syntax:get_pos(TempStr),
    Op= erl_syntax:module_qualifier(erl_syntax:atom(api_refac), 
                                    erl_syntax:atom(match)),
    NewTemp = expand_template(Temp),
    Args=[NewTemp,Node, Cond],
    TempAST = wrangler_misc:parse_annotate_expr(erl_syntax:string_value(TempStr), Pos1),
    App =erl_syntax:application(Op, Args),
    NewVar0=list_to_atom("_Res"++integer_to_list(random:uniform(1000))),
    NewVar1=list_to_atom("_Bind"++integer_to_list(random:uniform(1000))++"_@_V"),
    NewVar2=list_to_atom("_Bind"++integer_to_list(random:uniform(1000))),
    Cs =[erl_syntax:clause([erl_syntax:atom('false')], none, 
                           [erl_syntax:tuple([erl_syntax:atom('false'), 
                                              erl_syntax:list([])])]),
         erl_syntax:clause([erl_syntax:tuple([erl_syntax:atom('true'), 
                                              erl_syntax:variable(NewVar1)])], none, 
                           [erl_syntax:tuple([erl_syntax:atom('true'),
                                              erl_syntax:variable(NewVar1)])])],  
    MatchExpr= erl_syntax:match_expr(erl_syntax:tuple([erl_syntax:variable(NewVar0),
                                                        erl_syntax:variable(NewVar2)]),
                                      erl_syntax:case_expr(App, Cs)),
    {MetaVars, MetaAtoms} =  collect_meta_vars_and_atoms(TempAST),
    MatchExprs = lists:append([make_match_expr_1(V, P, NewVar2)||{V,P} <- MetaVars]),
    MetaAtomMatchExprs=lists:append([make_match_expr_for_meta_atom_1(V, P,NewVar2)|| {V,P}<-MetaAtoms]),
    NewCode=erl_syntax:block_expr([MatchExpr|MatchExprs]++MetaAtomMatchExprs++[erl_syntax:variable(NewVar0)]),
   %% refac_io:format("Newcode:\n~s\n", [erl_prettypr:format(NewCode)]),
    NewCode.


env_vars(Node) ->
    Ann = wrangler_syntax:get_ann(Node),
    case lists:keyfind(env, 1, Ann) of
        {env, Vs} ->
            Vs;
        false ->
            []
    end.

is_meta_variable_name(VarName) ->
    lists:prefix("@", lists:reverse(atom_to_list(VarName))).

is_meta_variable_value_name(VarName) ->
    lists:prefix("V_@", lists:reverse(atom_to_list(VarName))).
          

collect_meta_vars_and_atoms(Tree) when is_list(Tree) ->
    {MetaVars, MetaAtoms}=lists:unzip([collect_meta_vars_and_atoms(T)
                                       ||T<-Tree]),
    {lists:ukeysort(1,lists:append(MetaVars)),
     lists:ukeysort(1,lists:append(MetaAtoms))};

collect_meta_vars_and_atoms(Tree) ->
    F=fun(Node, {Vars, Atoms}) ->
              case erl_syntax:type(Node) of
                  variable ->
                      VarName = erl_syntax:variable_name(Node),
                      case is_meta_variable_name(VarName) of 
                          true ->
                              Pos = erl_syntax:get_pos(Node),
                              {lists:keystore(VarName, 1, Vars, {VarName, Pos}),Atoms};
                          _ ->
                              {Vars, Atoms}
                      end;
                  atom ->
                      case is_meta_atom(Node) of 
                          true ->
                              AtomValue = erl_syntax:atom_value(Node),
                              Pos = erl_syntax:get_pos(Node),
                              {Vars,lists:keystore(AtomValue, 1, Atoms, {AtomValue, Pos})};
                          _ ->
                              {Vars, Atoms}
                      end;
                  _ -> {Vars, Atoms}
              end
      end,
    api_ast_traverse:fold(F, {[],[]}, Tree).

is_meta_atom(Node) ->
    case erl_syntax:type(Node) of 
        atom ->
            AtomValue = erl_syntax:atom_value(Node),
            AtomValueList=atom_to_list(AtomValue),
            wrangler_misc:is_fun_name(AtomValueList) andalso
                lists:prefix("@", lists:reverse(AtomValueList));
        _ ->
            false
    end.

parse_str(Str, StartLoc) ->
    {ok, Toks, _} = erl_scan:string(Str, StartLoc),
    case erl_parse:parse_exprs(Toks++[{dot, {999,0}}]) of
        {ok, Exprs} ->
            Exprs0 = erl_recomment:recomment_forms(Exprs,[]),
            Exprs1=erl_syntax:form_list_elements(
                     wrangler_syntax_lib:annotate_bindings(Exprs0)),
            hd(Exprs1);
        _ ->
            erlang:error("Wrangler internal error in function: expand_rule:parse_str/2.")
    end.


make_match_expr_for_meta_atom(AtomName, Pos, BindVarName) ->
    mk_match_expr_for_meta_atom(AtomName, Pos, BindVarName,
                                "Meta atom ", " not bound.",
                                "; false -> throw({error, \"", 
                                "\"}) end").

make_match_expr_for_meta_atom_1(AtomName, Pos, BindVarName) ->
    mk_match_expr_for_meta_atom(AtomName, Pos, BindVarName,
                                "meta_atom_", "_not_bound_",
                                "; false -> erl_syntax:atom(", 
                                ") end").

mk_match_expr_for_meta_atom(AtomName, Pos, BindVarName, Str1,
                            Str2, Str3, Str4) ->
    AtomNameStr = atom_to_list(AtomName),
    VarName = "_W_" ++ AtomNameStr,
    BindVarNameStr = atom_to_list(BindVarName),
    ValueVar = VarName ++ "_V",
    VarNotBound = Str1 ++ AtomNameStr ++ Str2,
    Str = "case lists:keysearch(" ++ AtomNameStr ++
        ", 1, " ++ BindVarNameStr ++ ") of {value, {" ++
        AtomNameStr ++ "," ++ ValueVar ++
        "}} -> " ++ ValueVar ++ Str3 ++ VarNotBound ++ Str4,
    E = parse_str(Str, Pos),
    [erl_syntax:match_expr(erl_syntax:variable(VarName), E),
     erl_syntax:match_expr(erl_syntax:underscore(),
                           erl_syntax:variable(VarName))].

make_match_expr(VarName, Pos, BindVarName) ->
    mk_match_expr(VarName, Pos, BindVarName,
                  "Meta variable ",
                  " not bound.",
                  "; false -> throw({error, \"", 
                  "\"}) end").


make_match_expr_1(VarName, Pos, BindVarName) ->
    mk_match_expr(VarName, Pos, BindVarName,
                  "meta_variable_", 
                  "_not_bound_",
                  "; false -> erl_syntax:atom(", 
                  ") end").

mk_match_expr(VarName, Pos, BindVarName, Str1, Str2, Str3, Str4) ->
    VarNameStr = io_lib:write_atom(VarName),
    BindVarNameStr = atom_to_list(BindVarName),
    ValueVar = atom_to_list(VarName) ++ "_V",
    VarNotBound = Str1 ++ atom_to_list(VarName) ++ Str2,
    Str = "case lists:keysearch(" ++ VarNameStr ++ ", 1, " ++
        BindVarNameStr ++ ") of {value, {" ++
        VarNameStr ++ "," ++ValueVar ++ "}} -> " ++
          ValueVar ++ Str3 ++ VarNotBound ++ Str4,
    E = parse_str(Str, Pos),
    [erl_syntax:match_expr(erl_syntax:variable(VarName), E),
     erl_syntax:match_expr(erl_syntax:underscore(),erl_syntax:variable(VarName))].


expand_cond(Form) ->
    [Cond0, BindVar]=erl_syntax:application_arguments(Form),
    BindVarName = wrangler_syntax:variable_name(BindVar),
    Cond = convert_meta_atom_to_meta_var(Cond0),
    {MetaVars, MetaAtoms} =  collect_meta_vars_and_atoms(Cond0),
    MatchExprs = lists:append([make_match_expr(V, P, BindVarName)||
                                  {V,P} <- MetaVars, V=/='_This@', V=/='_File@']),
    MetaAtomMatchExprs=lists:append([make_match_expr_for_meta_atom(V, P,BindVarName)|| {V,P}<-MetaAtoms]),
    NewCond =erl_syntax:block_expr(MatchExprs++MetaAtomMatchExprs++[Cond]),
    %% refac_io:format("NewCond:\n~s\n", [erl_prettypr:format(NewCond)]),
    NewCond.


expand_collector(Form) ->
    [Collector]=erl_syntax:application_arguments(Form),
    NewCode=convert_meta_atom_to_meta_var(Collector),
    %% refac_io:format("NewCond:\n~s\n", [erl_prettypr:format(NewCode)]),
    NewCode.
   

convert_meta_atom_to_meta_var(Node) ->
    {Node1, _} = api_ast_traverse:stop_tdTP(fun do_convert/2, Node, {}),
    Node1.

do_convert(Node, _Others) ->
    case erl_syntax:type(Node) of 
        atom ->
            case is_meta_atom(Node) of 
                true ->
                    AtomValue = erl_syntax:atom_value(Node),
                    AtomValueList=atom_to_list(AtomValue),
                    UpperAtomValue=list_to_atom("_W_"++AtomValueList),   %% Check here!!!
                    {erl_syntax:variable(UpperAtomValue), true};
                false ->
                    {Node, false}
            end;
        _ ->
            {Node, false}
    end.
                

%%% API: function({Module, Function, Arity}, Fun, Forms, Options) ->
%%%         NewForms
%%%
%%% Forms and Options are the arguments passed to the parse_transform/2
%%% function.
%%% {Module, Function, Arity} is the function call to transform
%%% Fun(Form, Context) -> NewForm is the fun provided by the caller.
%%%
%%% Context is a property list, containing the following properties:
%%% - {file, Filename}
%%% - {module, ModuleName}
%%% - {function, FunctionName}       % name of the enclosing function
%%% - {arity, Arity :: integer()}    % arity of same
%%% - {var_names, Vars :: [atom()]}  % generated variables binding the
%%%                                  % function arguments.
%%%                                  % length(Vars) == Arity
%%%
parse_transform_1({_Module, _Function, _Arity} = MFA, F,
	          Forms, Options) when is_function(F) ->
    parse_transform(MFA, F, Forms, Options).

parse_transform(MFA, Fun, Forms, _Options) ->
    [File|_] = [F || {attribute,_,file,{F,_}} <- Forms],
    try begin
            NewTree = xform(MFA, Fun, Forms, [{file, File}]),
            revert_tree(NewTree)
	end
    catch
      	throw:{error,Ln,What} ->
            {error, [{File, [{Ln,?MODULE,What}]}], []}
    end.

revert_tree(Tree) ->
    [erl_syntax:revert(T) || T <- lists:flatten(Tree)].


xform({M,F,A}, Fun, Forms, Context0) ->
    Bef = fun(function, Form, Ctxt) ->
                  {Form, Ctxt};
	     (_, Form, Context) ->
		  {Form, Context}
	  end,
    Aft = fun(application, Form, Context) ->
		  case erl_syntax_lib:analyze_application(Form) of
		      {M, {F, A}} ->
                          Fun(Form, Context);
		      _ ->
			  Form
		  end;
             (_, Form, _Context) ->
		  Form
	  end,
    [Module] = [Mx || {attribute, _, module, Mx} <- Forms],
    transform(Forms, Bef, Aft, [{module, Module}|Context0]).


transform(Forms, Before, After, Context) ->
    F1 =
	fun(Form) ->
                Form2 =
		    case erl_syntax:subtrees(Form) of
			[] ->
			    Form;
			List ->
			    NewList =
				transform(
				  List, Before, After, Context),
			    erl_syntax:update_tree(Form, NewList)
		    end,
                Type2 = erl_syntax:type(Form2),
		try After(Type2, Form2, Context)
		catch
		    error:Reason2 ->
			?ERROR(Reason2, 'after', After, 
			       [{type, Type2},
				{context, {erlang:get_stacktrace(),Context}},
				{form, Form2}])
		end
	end,
    F2 = fun(List) when is_list(List) ->
		 map(F1, List);
	    (Form) ->
		 F1(Form)
	 end,
    map(F2, Forms).

%%% Slightly modified version of lists:mapfoldl/3
%%% Here, F/2 is able to insert forms before and after the form
%%% in question. The inserted forms are not transformed afterwards.
map(F, [Hd|Tail]) ->
    {Before, Res, After} =
	case F(Hd) of
	    {Be, _, Af} = Result when is_list(Be), is_list(Af) ->
		Result;
	    R1 ->
		{[], R1, []}
	end,
    Rs = map(F, Tail),
    Before ++ [Res| After ++ Rs];
map(F, []) when is_function(F, 1) -> [].



rpt_error(Reason, BeforeOrAfter, Fun, Info) ->
    Fmt = lists:flatten(
	    ["*** ERROR in parse_transform function:~n"
	     "*** Reason     = ~s~n"
	     "*** applying ~w fun (~p)~n",
	     ["*** ~10w = ~p~n" || _ <- Info]]),
    Args = [Reason, BeforeOrAfter, Fun | 
	    lists:foldr(
	      fun({K,V}, Acc) ->
		      [K, V | Acc]
	      end, [], Info)],
    io:format(Fmt, Args).


%% rpt_error(Reason, _BeforeOrAfter, _Fun, _Info) ->
%%     Fmt = lists:flatten(
%% 	    ["*** ERROR in parse_transformion function:~n"
%% 	     "*** Reason     = ~s~n"]),
%%     Args = [Reason],
%%     io:format(Fmt, Args).


