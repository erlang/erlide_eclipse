%%==============================================================================
%% @doc Some consecutive arguments of a function are contracted into a tuple
%% <p> To apply this refactoring, point the cursor to a function parameter, 
%% or an application argument.
%% Then select <em> Tuple Function Arguments </em> from the <em> Refactor </em>
%% menu, after that the refactorer will prompt to enter  the new tuple elements
%% number in the mini-buffer.
%% </p>
%% <p>
%% When tupling an exported function parameters, this refactoring has a global 
%% effect, i.e., it affects all those modules in which this function is 
%% imported/used.
%% </p>
%% <p> The following <em> side-conditions </em> apply to this refactoring:
%% <li> The new function arity should not cause confliction with any of the 
%% functions which are in scope in the current module;</li>
%% <li> In the case that the function is imported by another module, 
%% the new function arity and the same name should not be already in scope 
%% (either defined or imported) in that module. </li>
%% </p>
%% =============================================================================

-module(refac_tuple).

-export([tuple_funpar/6]).

-export([tuple_funpar_eclipse/6]).

-include("../include/wrangler.hrl").

-spec(tuple_funpar/6::(filename(), integer(), integer(), integer(), [dir()], integer()) ->
	     {error, string()} | {ok, [filename()]}).
tuple_funpar(FileName, ParLine, ParCol, Number, SearchPaths, TabWidth)->
  tuple_funpar(FileName, ParLine, ParCol, Number, SearchPaths, TabWidth, emacs).


-spec(tuple_funpar_eclipse/6::(filename(), integer(), integer(), integer(), [dir()], integer()) ->
	     {error, string()} | {ok, [{filename(), filename(), string()}]}).
tuple_funpar_eclipse(FileName, ParLine, ParCol, Number, SearchPaths, TabWidth)->
  tuple_funpar(FileName, ParLine, ParCol, Number, SearchPaths, TabWidth, eclipse).

%% =====================================================================
%% @spec tuple_funpar(File::string(),ParLine::integer(),ParCol::integer(),
%%                 Number::integer(), SearchPaths::[string()], 
%%                 Editor::atom()) -> term()
%%
%% @doc
%% Performs the side-condition checks and the and the collects all the 
%% necessary information for the refactoring.
%% It returns with an error message when a condition don't hold.
%% 
%% 
%% Parameter description:<pre>
%% <b>File</b> : The path of the module.
%% <b>ParLine</b> : The pointed line number in the editor.
%% <b>ParCol</b> : The pointed column number in the editor.
%% <b>Number</b> : The number of how many function parameters 
%%                     should be tupled. 
%% <b>SearchPath</b> : The list of directories to search for 
%%                     related Erlang files.
%% </pre>
%% @end
%% =====================================================================

tuple_funpar(FileName, ParLine, ParCol, Number, SearchPaths, TabWidth, Editor)->
  ?wrangler_io("\nCMD: ~p:tuple_funpar(~p, ~p, ~p, ~p,~p, ~p).\n", 
            [?MODULE,FileName, ParLine, ParCol, Number, SearchPaths, TabWidth]),
	{ok, {AnnAST, Info}} = parse_file(FileName, SearchPaths, TabWidth),
    case Number of 
	1 -> throw({error, "Tupling one argument is not supported."});
	_ ->ok
    end,    
    case check_first_pos({ParLine, ParCol}, AnnAST) of 
	{error, Reason} -> {error, Reason};
	{FirstPar, Type} ->
	    case get_fun_name_and_arity(AnnAST, {ParLine, ParCol}, Type, FirstPar)  of
		{{Mod, FunName, Arity, _, _}, FunPatterns, AppNode, AppPar, FunNode} ->
		    ModName = get_module_name(Info),
		    InscopeFuns = 
			lists:map(fun ({_M, F, A}) -> {F, A} end, refac_util:inscope_funs(Info)),
		    {Parameters, C} = 
			check_parameters(FirstPar, Number, Arity, FunPatterns, AppNode, AppPar),
		    NewArity = Arity - Number + 1,
		    case check_def_mod(Mod, ModName) of 
			ok ->
			    case check_name_clash(FunName, NewArity, InscopeFuns, Number) of 
				ok ->
				    case check_is_callback_fun(Info, FunName, Arity) of 
					ok ->  ?wrangler_io("The current file under refactoring is:\n~p\n", [FileName]),
					       performe_refactoring(AnnAST, Info, Parameters, FunName, Arity, FunNode,
								    C, Mod, SearchPaths, FileName, TabWidth, Editor);
					{error, Reason} -> {error, Reason}
				    end;
				{error, Reason} -> {error, Reason}
			    end;
			{error, Reason} ->
			    {error, Reason}
		    end;
		{error, Reason} -> {error, Reason}
	    end
    end.

%% =====================================================================
%% @spec performe_refactoring(AnnAST::syntaxtree(),Info::ModInfo,
%%       Parameters::[syntaxtree()], FunName::atom(), Arity::integer(),
%%       FunNode::syntaxtree(), C::integer(), Mod::atom(),
%%       SearchPaths::[string()], File::string(), Editor::atom())
%%                                                    -> {ok, [string()]}
%% ModInfo = [{Key, term()}]
%% Key = attributes | errors | exports | functions | imports |
%%       module | records | rules | warnings
%%
%% @end
%% =====================================================================
performe_refactoring(AnnAST, Info, Parameters, FunName, Arity, FunNode, C, Mod,
		     SearchPaths, File, TabWidth, Editor) ->
    {AnnAST1, _} = tuple_parameters(AnnAST, Parameters, FunName, Arity, C, Mod),
    AnnAST2 = check_implicit_funs(AnnAST1, FunName, Arity, FunNode),
    case refac_util:is_exported({FunName, Arity}, Info) of
      true ->
	    ?wrangler_io("\nChecking client modules in the following search paths: \n~p\n",
			 [SearchPaths]),
	    ClientFiles = refac_util:get_client_files(File, SearchPaths),
	    Results = tuple_parameters_in_client_modules(ClientFiles, FunName, Arity, C,
							 length(Parameters), Mod, TabWidth),
	    case Editor of
		emacs ->
		    refac_util:write_refactored_files_for_preview([{{File, File}, AnnAST2} | Results]),
		    ChangedClientFiles = lists:map(fun ({{F, _F}, _AST}) -> F end, Results),
		    ChangedFiles = [File | ChangedClientFiles],
		    ?wrangler_io("The following files are to be changed by this refactoring:\n~p\n", [ChangedFiles]),
		    ?wrangler_io("WARNING: Please check the implicit function calls!",[]),
		    {ok, ChangedFiles};
		eclipse ->
		    Results1 = [{{File, File}, AnnAST2} | Results],
		    Res = lists:map(fun ({{FName, NewFName}, AST}) ->
					    {FName, NewFName, refac_prettypr:print_ast(refac_util:file_format(FName),AST)}
				    end,
				    Results1),
		  {ok, Res}
	    end;
	false ->
	    case Editor of
		emacs ->
		    refac_util:write_refactored_files_for_preview([{{File, File}, AnnAST2}]),
		    ChangedFiles = [File],
		    ?wrangler_io("The following files are to be changed by this refactoring:\n~p\n", [ChangedFiles]),
		    ?wrangler_io("WARNING: Please check the implicit function calls!",[]),
		    {ok, ChangedFiles};
		eclipse -> Res = [{File, File, refac_prettypr:print_ast(refac_util:file_format(File),AnnAST2)}], {ok, Res}
	    end
    end.


%% =====================================================================
%% @spec check_implicit_funs(AnnAST::syntaxtree(), FunName::atom(),
%%       FunArity::integer(),FunNode::syntaxtree()) -> syntaxtree()
%% @end
%% =====================================================================
check_implicit_funs(AnnAST1, FunName, FunArity, FunNode)->
  case refac_util:once_tdTU(fun implicit/2, AnnAST1, {}) of
    {_, true}->
      Forms = refac_syntax:form_list_elements(AnnAST1),
      Fun = fun(Form) ->
	      case refac_syntax:type(Form) of 
		function -> 
		      Name=refac_syntax:atom_value(refac_syntax:function_name(Form)),
		      Arity = refac_syntax:function_arity(Form),
		      case {Name, Arity} == {FunName, FunArity} of
			  true -> [Form, FunNode];
			  _ -> [Form]
		      end;
		  _ -> [Form]
	      end
	    end,
	  refac_syntax:form_list([F||Form<-Forms, F <-Fun(Form)]);
    {_, false} -> AnnAST1
  end.


%% =====================================================================
%% @spec implicit(Node::syntaxtree(), {}) -> {[], atom()}
%% @end
%% =====================================================================
implicit(Node, {})->
  case refac_syntax:type(Node) of 
    implicit_fun -> {[], true};
    _ -> {[], false}
  end.

%% =====================================================================
%% @spec check_is_callback_fun(Info::ModInfo, FunName::atom(), 
%%           Arity::integer()) -> ok
%% ModInfo = [{Key, term()}] 
%% Key = attributes | errors | exports | functions | imports | 
%%       module | records | rules | warnings 
%% 
%% @end
%% =====================================================================

check_is_callback_fun(Info, FunName, Arity)->
  case is_callback_fun(Info, FunName, Arity) of 
    true -> 
      {error, "Tupling parameters of a callback function is not supported."};
    false -> ok
  end.

%% =====================================================================
%% @spec check_name_clash(FunName::atom(), NewArity::integer(),
%%       InscopeFuns::[{ModName, FunName, Arity}],Number::integer()) -> ok
%%
%% @end
%% =====================================================================
check_name_clash(FunName, NewArity, InscopeFuns, Number) ->
    %%?wrangler_io("Param: ~p ~n ~p ~n ~p ~n ~p ~n", [FunName, NewArity, InscopeFuns, Number]),
    %%?wrangler_io("Param: ~p ~n ", [lists:member({FunName, NewArity}, InscopeFuns)]),
    case (lists:member({FunName, NewArity}, InscopeFuns) or
	    lists:member({FunName, NewArity}, refac_util:auto_imported_bifs()))
	   and (Number > 1)
	of
      false -> ok;
      true ->
	  {error, atom_to_list(FunName) ++"/" ++ integer_to_list(NewArity) ++" is already in scope, or is an auto-imported builtin function."}
    end.

%% =====================================================================
%% @spec check_def_mod(DefMod::atom(),ModName::atom()) -> ok
%%
%% @end
%% =====================================================================
check_def_mod(DefMod, ModName) ->
    if DefMod == ModName -> ok;
       true ->
	   {error, "This function is not defined in this module; please go to the module where it is defined to apply this refactoring."}
    end.
    

%% =====================================================================
%% @spec parse_file(FileName::string(), SearchPaths::[string()])  
%%                                               -> {syntaxtree(), ModInfo}
%% ModInfo = [{Key, term()}] 
%% Key = attributes | errors | exports | functions | imports | 
%%       module | records | rules | warnings                           
%%
%% @end
%% =====================================================================
parse_file(FileName, SearchPaths, TabWidth)->
  refac_util:parse_annotate_file(FileName, true, SearchPaths, TabWidth).
 

%% =====================================================================
%% @spec tuple_parameters_in_client_modules(Files::[string()], Name::atom(), 
%%       Arity::integer(), C::integer(), N::integer(), Mod::atom())
%%                -> [{{string(), string()}, syntaxtree()}]
%% 
%% @end
%% =====================================================================
tuple_parameters_in_client_modules(Files, Name, Arity, C, N, Mod, TabWidth)->
  case Files of
    [] -> [];
    [F | Fs] ->
	  ?wrangler_io("The current file under refactoring is:\n~p\n", [F]),
	  {ok, {AnnAST, Info}}= refac_util:parse_annotate_file(F, true, [], TabWidth),
	  {AnnAST1, _} = tuple_parameters_in_client_modules_1({AnnAST, Info}, 
							      Name, Arity, C, N, Mod),
          [{{F, F}, AnnAST1} | 
	   tuple_parameters_in_client_modules(Fs,Name,Arity, C, N, Mod,TabWidth)]
  end.

%% =====================================================================
%% @spec tuple_parameters_in_client_modules_1({Tree::syntaxtree(),
%%       Info::ModInfo}, Name::atom(), Arity::integer(), C::integer(),
%%       N::integer(), Mod::atom()) -> syntaxtree()
%% ModInfo = [{Key, term()}]
%% Key = attributes | errors | exports | functions | imports |
%%       module | records | rules | warnings
%%
%% @end
%% =====================================================================
tuple_parameters_in_client_modules_1({Tree, Info}, Name, Arity, C, N, Mod) ->
    case lists:keysearch(module, 1, Info) of
      {value, {module, ClientModName}} ->
	  InscopeFuns = lists:map(fun ({_M, F, A}) -> {F, A} end,
				  refac_util:inscope_funs(Info)),
	  NewArity = Arity - N + 1,
	  case (lists:member({Name, NewArity}, InscopeFuns) or
		  lists:member({Name, NewArity}, refac_util:auto_imported_bifs()))
		 and (Arity > NewArity)
	      of
	    false ->
		refac_util:stop_tdTP(fun do_tuple_fun_parameters/2, Tree,
				     {C, N, Name, Arity, Mod});
	    true ->
		{error, "The new function arity causes confliction in the client module: " ++ atom_to_list(ClientModName)}
	  end;
      _ ->
	  refac_util:stop_tdTP(fun do_tuple_fun_parameters/2, Tree, {C, N, Name, Arity, Mod})
    end.


%% =====================================================================
%% @spec tuple_parameters(Tree::syntaxtree(), Parameters::[syntaxtree()], 
%%       FunName::atom(), Arity::integer(), C::integer(), Mod::atom()) 
%%                                                      -> syntaxtree()
%% 
%% @end
%% =====================================================================
tuple_parameters(Tree, Parameters, FunName, Arity, C, Mod)->
   refac_util:stop_tdTP(fun do_tuple_fun_parameters/2, 
                        Tree, {C, length(Parameters), FunName, Arity, Mod}).


%% =====================================================================
%% @spec get_arg(Parameters::[syntaxtree()], C::integer(), N::integer()) 
%%                                                      ->[syntaxtree()]
%% 
%% @end
%% =====================================================================
get_arg(Parameters, C, N)->
  ParFirst = Parameters -- lists:nthtail(C-1, Parameters),
  NewArg =lists:reverse(lists:nthtail(length(Parameters) - N - C + 1,
          lists:reverse(lists:nthtail(C-1, Parameters)))),
  ParLast = (Parameters -- ParFirst) -- NewArg,
  NewTuple = refac_syntax:tuple(NewArg),
  ParFirst ++ [NewTuple] ++ ParLast.


%% =====================================================================
%% @spec do_tuple_fun_parameters(Tree::syntaxtree(), {C::integer(),
%%       N::integer(), Name::atom(), Arity::integer(), Mod::atom()}) 
%%                                               -> syntaxtree()
%% 
%% @end
%% =====================================================================
do_tuple_fun_parameters(Tree, {C, N, Name, Arity, Mod})->
  case refac_syntax:type(Tree) of
    function ->
      FunName = refac_syntax:function_name(Tree),
      FunArity = refac_syntax:function_arity(Tree),
      DefMod = get_fun_def_mod(Tree),
      case (refac_syntax:atom_value(FunName) == Name) and 
           (FunArity == Arity) and (DefMod == Mod) of
        true ->
          Clauses = refac_syntax:function_clauses(Tree),
          NewClauses = 
            lists:map(fun(Clause)->
                        Body = refac_syntax:clause_body(Clause),
                        Guard = refac_syntax:clause_guard(Clause),
                        Patterns = refac_syntax:clause_patterns(Clause),
                        NewPat = get_arg(Patterns, C, N),
                        refac_syntax:copy_attrs(Clause, 
                                     refac_syntax:clause(NewPat, Guard, Body))
                      end, Clauses),
          {refac_syntax:copy_attrs(Tree, 
                       refac_syntax:function(FunName, NewClauses)), false};
	false -> {Tree, false}
      end;
    application -> 
      Operator = refac_syntax:application_operator(Tree),
      case application_info(Tree) of
	  {{_, apply}, 2} -> 
            transform_apply_call(Tree, {C, N, Name, Arity, Mod});
	  {{_, apply}, 3} -> 
            transform_apply_call(Tree, {C, N, Name, Arity, Mod});
	  {{_, spawn}, 3} -> 
            transform_spawn_call(Tree, {C, N, Name, Arity, Mod});
	  {{_, spawn}, 4} -> 
            transform_spawn_call(Tree, {C, N, Name, Arity, Mod});
	  {{_, spawn_link}, 3} -> 
            transform_spawn_call(Tree, {C, N, Name, Arity, Mod});
	  {{_, spawn_link}, 4} -> 
            transform_spawn_call(Tree, {C, N, Name, Arity, Mod});
	_ ->
	  case refac_syntax:type(Operator) of
	    atom ->
	      case get_fun_def_info(Operator) of
		{_ModName, Name, Arity, _} ->
                  Parameters = refac_syntax:application_arguments(Tree),
		  NewArgs = get_arg(Parameters, C, N),
		  {refac_syntax:copy_attrs(Tree, 
                         refac_syntax:application(Operator, NewArgs)), false};
		_ -> {Tree, false}
	      end;
	    module_qualifier ->
	      case get_fun_def_info(Operator) of
		{_ModName, Name, Arity, _} ->
                  Parameters = refac_syntax:application_arguments(Tree),
                  NewArgs = get_arg(Parameters, C, N),
		  {refac_syntax:copy_attrs(Tree, 
                         refac_syntax:application(Operator, NewArgs)),false};
		_ -> {Tree, false}
	      end;
	    _ -> {Tree, false}
	  end
      end;
    arity_qualifier ->
      Fun = refac_syntax:arity_qualifier_body(Tree),
      Fun_Name = refac_syntax:atom_value(Fun),
      Arg = refac_syntax:arity_qualifier_argument(Tree),
      Arg1 = refac_syntax:integer_value(Arg),
      DefMod = get_fun_def_mod(Fun),
      if (Fun_Name == Name) and (Arg1 == Arity) and (DefMod == Mod) ->
        {refac_syntax:copy_attrs(Tree, refac_syntax:arity_qualifier(Fun, 
                      refac_syntax:integer(
                      refac_syntax:integer_value(Arg) - N + 1))), false};
	true -> {Tree, false}
      end;
    implicit_fun -> {Tree, true};
    _ -> {Tree, false}
  end.


%% =====================================================================
%% @spec check_first_pos(Pos::{integer(), integer()}, AnnAST::syntaxtree())
%%          -> {syntaxtree(), function} | {syntaxtree(), application}
%% 
%% @end
%% =====================================================================

check_first_pos(Pos, AnnAST)->
    case pos_to_app(AnnAST, Pos) of
      {ok, AppNode}-> 
	    case pos_to_arg(AppNode, Pos) of 
		{error, _Reason} -> pos_to_pat(AnnAST, Pos);
		Node ->{Node, application}
	    end;
	{error, none} -> 
	    case pos_to_pat(AnnAST,  Pos) of 
		{error, Reason} -> {error, Reason};
		Node ->  {Node, function}
	    end
  end.


%% =====================================================================
%% @spec pos_to_arg(AppNode::syntaxtree(), Pos::{integer(), integer()})
%%                                                       -> syntaxtree()
%% 
%% @end
%% =====================================================================
pos_to_arg(AppNode, Pos)->
    Args = refac_syntax:application_arguments(AppNode),
    List = lists:dropwhile(fun(Pat)->
				   {_Pos11, Pos22} = refac_util:get_range(Pat),
				   Pos22 < Pos
			   end, Args),
    case List of
	[] -> {error, "You have not selected a parameter!"};
	_ -> hd(List)
    end.


%% =====================================================================
%% @spec pos_to_pat(AnnAST::syntaxtree(), Pos::{integer(), integer()})
%%                                                       -> syntaxtree()
%% 
%% @end
%% =====================================================================
pos_to_pat(AnnAST, Pos)->
  case pos_to_clause(AnnAST, Pos) of
    {ok, Clause} ->
	  FunPatterns = refac_syntax:clause_patterns(Clause),
	  List = lists:dropwhile(fun(Pat)->
					 {_Pos11, Pos22} = refac_util:get_range(Pat),
					 Pos22 < Pos
				 end, FunPatterns),
	  case List of
	      [] -> {error, "You have not selected a parameter!"};
	      _ -> hd(List)
	  end;
      {error, none} -> {error, "You have not selected a parameter!"}   
  end.


%% =====================================================================
%% @spec pos_to_clause(Node::syntaxtree(), Pos::{integer(), integer()})
%%                                -> {ok, syntaxtree()} | {error, none}
%% 
%% @end
%% =====================================================================
pos_to_clause(Node, Pos) ->
  case refac_util:once_tdTU(fun pos_to_clause_1/2, Node, Pos) of
    {_, false} -> {error, none};
    {R, true} -> {ok, R}
  end.


%% =====================================================================
%% @spec pos_to_clause_1(Node::syntaxtree(), Pos::{integer(), integer()})
%%                                        -> {syntaxtree(), atom()}
%% 
%% @end
%% =====================================================================
pos_to_clause_1(Node, Pos) ->
  case refac_syntax:type(Node) of
    clause ->
      {S, E} = refac_util:get_range(Node),
      if 
        (S =< Pos) and (Pos =< E) -> {Node, true};
	true -> {[], false}
      end;
    _ -> {[], false}
  end.

%% =====================================================================
%% @spec pos_to_app(Node::syntaxtree(),Pos::{integer(), integer()})
%%                                  -> {ok, syntaxTree()} | {error, none}
%% 
%% @end
%% =====================================================================
pos_to_app(Node, Pos) ->
  case refac_util:once_tdTU(fun pos_to_app_1/2, Node, Pos) of
    {_, false} -> {error, none};
    {R, true} -> {ok, R}
  end.



%% =====================================================================
%% @spec pos_to_app_1(Node::syntaxtree(),Pos::{integer(), integer()}) 
%%                        -> {syntaxtree(), true} | {[], false}
%% 
%% @end
%% =====================================================================
pos_to_app_1(Node, Pos) ->
  case refac_syntax:type(Node) of
    application ->
      {S, E} = refac_util:get_range(Node),
      if 
        (S =< Pos) and (Pos =< E) -> {Node, true};
	true -> {[], false}
      end;
    _ -> {[], false}
  end.

%% =====================================================================
%% @spec check_parameters(FirstPar::syntaxtree(), Number::integer(),
%%       Arity::integer(), FunPatterns::[syntaxtree()], AppNode::syntaxtree(),
%%       AppPar::[syntaxtree()])  -> {[syntaxtree()], integer()}
%%
%% @end
%% =====================================================================
check_parameters(FirstPar, Number, Arity, FunPatterns, AppNode, AppPar) ->
    case (Number < 1) or (Number > Arity) of
      true ->
	  throw({error,
		"It is not possible to tuple " ++  integer_to_list(Number) ++ " parameters for function selected."});
      false ->
	  case AppNode of
	    [] -> search_par(FunPatterns, Number, FirstPar);
	    _ -> search_par(AppPar, Number, FirstPar)
	  end
    end.

%% =====================================================================
%% @spec search_par(List::[syntaxtree()], Number::integer(),
%%       First::syntaxtree())   -> {[syntaxtree()], integer()}
%%
%% @end
%% =====================================================================
search_par(List, Number, First) ->
    ParList = lists:dropwhile(fun (Elem1) -> not (Elem1 == First) end, List),
    C = length(List -- ParList) + 1,
    case length(ParList) < Number of
      true ->
	 throw({error,
		"It is not possible to tuple  " ++ integer_to_list(Number) ++ " parameters for the function selected."});
      false ->
	  {lists:reverse(lists:nthtail(length(ParList) - Number,
				       lists:reverse(ParList))),
	   C}
    end.



%% =====================================================================
%% @spec get_fun_name_and_arity(AnnAST::syntaxtree(),Pos::{integer(),integer()},
%%       Type::atom(), Node::syntaxtree()) 
%%             -> {{Mod, Fun, Arity, OccurPos, DefPos}, [syntaxtree()],[],[]} |
%%                {{Mod, Fun, Arity, OccurPos, DefPos}, [syntaxtree()], 
%%                  syntaxtree(),[syntaxtree()], syntaxtree()}
%% @end
%% =====================================================================
get_fun_name_and_arity(AnnAST, Pos, Type, Node) ->
  case Type of
    function ->
      case expr_to_clause(AnnAST, Node) of
        {ok, FunClause}-> 
	      FunNode = 
		  case refac_util:pos_to_fun_def(AnnAST, Pos) of
		      {ok, F} -> F;
		      {error, Reason} -> 
			  {error, Reason}
		  end,
	      FunPatterns = refac_syntax:clause_patterns(FunClause),
	      {FunStartPos, _FunEndPos} = refac_util:get_range(FunClause),  
	      case refac_util:pos_to_fun_name(AnnAST, FunStartPos) of  
		  {ok, Result} -> {Result, FunPatterns, [], [], FunNode} ;
		  {error, Reason1} -> {error, Reason1}
	      end;
	  {error, Reason} -> {error, Reason}
      end;
    application ->
      case pos_to_app(AnnAST, Pos) of
        {ok, AppNode} -> 
          {AppStartPos, _AppEndPos} = refac_util:get_range(AppNode),     
	  case refac_util:pos_to_fun_name(AnnAST, AppStartPos) of  
            {ok, Result} -> 
              {_Mod, _Fun, _Arity, _OccurPos, DefPos} = Result,
              case refac_util:pos_to_fun_def(AnnAST, DefPos) of
                {ok, FunNode} -> 
                  Clause = refac_syntax:function_clauses(FunNode),
                  FunPatterns = refac_syntax:clause_patterns(hd(Clause)),    
                  AppPar =  refac_syntax:application_arguments(AppNode),  
                  {Result, FunPatterns, AppNode, AppPar, FunNode};          
                {error, Reason} -> 
                  {error, Reason}
              end;
            {error, Reason} -> {error, Reason}
          end;
        {error, Reason} -> 
	      {error, Reason}
      end
  end.


%% ===========================================================================
%% @spec expr_to_clause(Tree::syntaxTree(), Exp::syntaxTree())->
%%                   {ok, syntaxTree()} | {error, none}
%%
%% @end
%% ===========================================================================
expr_to_clause(Tree, Exp) ->
  Res = expr_to_clause_1(Tree, Exp),
  case Res of 
    [H|_T] -> {ok, H};
    _ -> {error, none}
  end.
    
%% ===========================================================================
%% @spec expr_to_clause_1(Tree::syntaxTree(), Exp::syntaxTree())->[syntaxTree()]
%%
%% @end
%% ===========================================================================
expr_to_clause_1(Tree, Exp) ->
  {Start, End} = refac_util:get_range(Exp),
  {S, E} = refac_util:get_range(Tree),
  if (S < Start) and (E >= End) ->
      case refac_syntax:type(Tree) of
        clause -> [Tree];
        _ ->
	  Ts = refac_syntax:subtrees(Tree),
	  R0 = [[expr_to_clause_1(T, Exp) || T <- G] || G <- Ts],
	  lists:flatten(R0)
      end;
    true -> []
  end.


%% =====================================================================
%% @spec get_module_name(Info::ModInfo) -> atom()
%% ModInfo = [{Key, term()}] 
%% Key = attributes | errors | exports | functions | imports | 
%%       module | records | rules | warnings 
%% 
%% @end
%% =====================================================================
get_module_name(Info) ->
  case lists:keysearch(module, 1, Info) of
    {value, {module, ModName}} -> ModName;
    false -> {error, "Wrangler could not get the current module name."}
  end.



%% =====================================================================
%% @spec get_fun_def_mod(Node::syntaxtree()) -> atom()
%% 
%% @end
%% =====================================================================
get_fun_def_mod(Node) ->
  As = refac_syntax:get_ann(Node),
  case lists:keysearch(fun_def, 1, As) of
    {value, {fun_def, {M, _N, _A, _P, _DefinePos}}} -> M;
    _ -> false
  end.



%% =====================================================================
%% @spec is_callback_fun(Info::ModInfo, FunName::atom(), Arity::integer()) 
%%                                                               -> atom()
%% ModInfo = [{Key, term()}] 
%% Key = attributes | errors | exports | functions | imports | 
%%       module | records | rules | warnings 
%% 
%% @end
%% =====================================================================
is_callback_fun(Info, Funname, Arity) ->
  case lists:keysearch(attributes, 1, Info) of
    {value, {attributes, Attrs}} ->
       case lists:keysearch(behaviour, 1, Attrs) of
	 {value, {behaviour, B}} ->
           lists:member({Funname, Arity},
	   refac_util:callback_funs(B));
	 _ -> false
       end;
     _ -> false
  end.



%% =====================================================================
%% @spec application_info(Tree::syntaxtree()) -> {{atom(), atom()}, integer()}
%% 
%% @end
%% =====================================================================
application_info(Node) ->
  case refac_syntax:type(Node) of
    application ->
      Operator = refac_syntax:application_operator(Node),
      Arguments = refac_syntax:application_arguments(Node),
      Arity = length(Arguments),
      case refac_syntax:type(Operator) of
	atom ->
	  Op = refac_syntax:atom_value(Operator), {{none, Op}, Arity};
	module_qualifier ->
	  Mod = refac_syntax:module_qualifier_argument(Operator),
          Fun = refac_syntax:module_qualifier_body(Operator),
	  T1 = refac_syntax:type(Mod),
	  T2 = refac_syntax:type(Fun),
	  case T1 of
	    atom ->
	      Mod1 = refac_syntax:atom_value(Mod),
	      case T2 of
		atom -> 
                  Fun1 = refac_syntax:atom_value(Fun), {{Mod1, Fun1}, Arity};
		_ -> {{Mod1, expressionfunname}, Arity}
	      end;
	    _ ->
	      case T2 of
		atom -> 
                  Fun1 = refac_syntax:atom_value(Fun),
	          {{expressionmodname, Fun1}, Arity};
		_ -> {{expressionmodname, expressionfunname}, Arity}
	      end
	  end;
	_ -> {{none, expressionoperator}, Arity}
      end;
    _ -> erlang:error(bagarg)
  end.


%% =====================================================================
%% @spec get_fun_def_info(Node::syntaxtree())
%%                  -> {atom(), atom(), integer(), {integer(), integer()}}
%% 
%% @end
%% =====================================================================
get_fun_def_info(Node) ->
  As = refac_syntax:get_ann(Node),
  case lists:keysearch(fun_def, 1, As) of
    {value,{fun_def, {Mod, FunName, Arity, _UsePos, DefinePos}}} ->
      {Mod, FunName, Arity, DefinePos};
    _ -> false
  end.


%% =====================================================================
%% @spec transform_apply_call(Node::syntaxtree(), {C::integer(),
%%       N::integer(), Name::atom(), Arity::integer(), ModName::atom()}) 
%%                                               -> syntaxtree()
%% 
%% @end
%% =====================================================================
transform_apply_call(Node, {C, N, Name, Arity, ModName}) ->
  Operator = refac_syntax:application_operator(Node),
  Arguments = refac_syntax:application_arguments(Node),
  case Arguments of
    [Fun, Args] ->
	  case refac_syntax:type(Fun) of
	      implicit_fun ->
		  FName = refac_syntax:implicit_fun_name(Fun),
		  B = refac_syntax:atom_value(refac_syntax:
					      arity_qualifier_body(FName)),
		  A = refac_syntax:integer_value(refac_syntax:
					      arity_qualifier_argument(FName)),
		  case {B, A} of
		      {Name, Arity} ->
			  case refac_syntax:type(Args) of 
			      list ->
				  Args1 = refac_syntax:list_elements(Args),
				  Args2 = get_arg(Args1, C, N),
				  Args3 = refac_syntax:list(Args2),
				  {refac_syntax:copy_attrs(Node, 
						   refac_syntax:application(Operator, [Fun, Args3])), false};
			      _ -> {Node, false}
			  end;
		      _ -> {Node, false}
		  end;
	      atom -> 
		  case refac_syntax:type(Args) of 
		      list -> Args1 = refac_syntax:list_elements(Args),
			      Len = length(Args1),
			      case {refac_syntax:atom_value(Fun), Len} of
				  {Name, Arity} ->
				      Args1 = refac_syntax:list_elements(Args),
				      Len = length(Args1),
				      Args2 = get_arg(Args1, C, N),
				      Args3 = refac_syntax:list(Args2),
				      {refac_syntax:copy_attrs(Node, 
							       refac_syntax:application(Operator, [Fun, Args3])), false};
				  _ -> {Node, false}
			      end;
		      _ -> {Node, false}
		  end;
	      _ -> {Node, false}
	  end;
      [Mod, Fun, Args] ->
	  case refac_syntax:type(Args) of
	      list ->Args1 = refac_syntax:list_elements(Args),
		     Len = length(Args1),	  
		     case {refac_syntax:atom_value(Fun), Len, refac_syntax:atom_value(Mod)} of
			 {Name, Arity, ModName} ->
			     Args2 = get_arg(Args1, C, N),
			     Args3 = refac_syntax:list(Args2),
			     {refac_syntax:copy_pos(Node, refac_syntax:
						    copy_attrs(Node, refac_syntax:application(Operator, 
											      [Mod, Fun, Args3]))),false};
			 _ -> {Node, false}
		     end;
	      _ -> {Node, false}
	  end;
      _ -> 
	  {Node, false}
  end.


%% =====================================================================
%% @spec transform_spawn_call(Node::syntaxtree(), {C::integer(),
%%       N::integer(), Name::atom(), Arity::integer(), ModName::atom()}) 
%%                                               -> syntaxtree()
%% 
%% @end
%% =====================================================================
transform_spawn_call(Node, {C, N, Name, Arity, ModName}) ->
  Operator = refac_syntax:application_operator(Node),
  Arguments = refac_syntax:application_arguments(Node),
  [None, Mod, Fun, Args] = if length(Arguments) == 4 -> Arguments;
			     true -> [none] ++ Arguments
	                   end,
  Args1 = refac_syntax:list_elements(Args),
  Len = length(Args1),
  case {refac_syntax:atom_value(Fun), Len, refac_syntax:atom_value(Mod)} of
    {Name, Arity, ModName} ->
       Args2 = get_arg(Args1, C, N),
       Args3 = refac_syntax:list(Args2),
       App = if length(Arguments) == 4 -> 
                  refac_syntax:application(Operator, [None, Mod, Fun, Args3]);
	        true ->  refac_syntax:application(Operator, [Mod, Fun, Args3])
		        end,
		  {refac_syntax:copy_pos(Node, 
                               refac_syntax:copy_attrs(Node, App)), false};
    _ -> {Node, false}
  end.
