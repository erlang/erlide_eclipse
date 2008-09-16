%%==============================================================================
%% @doc A record expression created from the selected tuple.
%% <p> To apply this refactoring mark the tuple in the editor, which is a 
%% function parameter or an application argument.
%% Then select <em> From Tuple To Record </em> from the <em> Refactor </em> 
%% menu, after that the refactorer will prompt to enter the record name and 
%% the record field names.
%% </p>
%% <p>
%% This refactoring has a global effect, i.e., it affects all those modules in 
%% which this function is imported/used.
%% </p>
%% <p>
%% WARNING: After the transformation please check the implicit functions, 
%% and create the record if it is necessary!
%% </p>
%% <p> The following <em> side-conditions </em> apply to this refactoring:
%% <li> The record and field names must be legal names; </li>
%% <li> The number of record fields must equal to the selected tuple size; </li>
%% <li> The function definition must defined in the current module; </li>
%% <li> The selected part must be a tuple.  </li>
%% </p>
%% =============================================================================
-module(ref_tuple_to_record).

-export([tuple_to_record/8]).

-export([tuple_to_record_eclipse/8]).

tuple_to_record(File,FLine,FCol,LLine,LCol,RecName,FieldString,SearchPaths)->
  tuple_to_record(File, FLine, FCol, LLine, LCol, 
                RecName, FieldString, SearchPaths, emacs).

tuple_to_record_eclipse(File,FLine,FCol,LLine,LCol,RecName,FieldString,SearchPaths)->
  tuple_to_record(File, FLine, FCol, LLine, LCol, 
                RecName, FieldString, SearchPaths, eclipse).

%% =====================================================================
%% @spec tuple_to_record(File::string(),FLine::integer(),FCol::integer(),
%%           LLine::integer(),LCol::integer(), RecName::string(),
%%           FieldString::[string()], SearchPaths::[string()], 
%%           Editor::atom()) -> term()
%% @end
%% =====================================================================
tuple_to_record(File, FLine, FCol, LLine, LCol, 
                RecName, FieldString, SearchPaths, Editor)->
  io:format("\n[CMD: tuple_to_record,~p,~p,~p,~p,~p,~p,~n ~p,~n ~p] \n \n", 
         [File, FLine, FCol, LLine, LCol, RecName, FieldString, SearchPaths]),
  FieldList = convert_record_names(FieldString),
  check_if_correct_names(RecName, FieldList),
  {AnnAST, Info} = parse_file(File, SearchPaths),
  {Node, Tuple, Type} = check_pos(AnnAST, {FLine, FCol}, {LLine, LCol}),
  N = tuple_pos(Node,Tuple,Type),
  check_record_field_number(Tuple, FieldList),
  ExistingRec = check_record_name_exists(Info, RecName),
  {FunName, Arity, DefMod} = get_fun_name(AnnAST, Node, Type),
  ModName = get_module_name(Info),
  check_def_mod(DefMod,ModName),
  io:format("The current file under refactoring is:\n~p\n", [File]),
  AnnAST1 = tuple_record(AnnAST, ExistingRec, RecName, FieldList,
                         FunName,Arity, DefMod,N),
  case refac_util:is_exported({FunName, Arity}, Info) of
    true ->		
      io:format("\nChecking client modules "
                "in the following search paths: \n~p\n", [SearchPaths]),
      ClientFiles = refac_util:get_client_files(File, SearchPaths),
      Results = tuple_to_record_in_client_modules(ClientFiles, FunName, Arity, 
                                                DefMod, N, RecName, FieldList),
      case Editor of 
        emacs ->
          refac_util:write_refactored_files([{{File, File}, AnnAST1} | Results]),
          ChangedClientFiles = lists:map(fun ({{F, _F}, _AST}) -> F end, Results),
          ChangedFiles = [File | ChangedClientFiles],
          io:format("The following files have been changed "
                    "by this refactoring:\n~p\n",[ChangedFiles]),
          io:format("WARNING: Please check the implicit function calls!"),
          {ok, ChangedFiles};
        eclipse ->
          Results1 = [{{File, File}, AnnAST1} | Results],
	  Res = lists:map(fun({{FName, NewFName}, AST}) -> 
                            {FName, NewFName, refac_prettypr:print_ast(AST)} 
                          end, Results1),
	  {ok, Res}
      end;
    false -> 
      case Editor of
        emacs -> 
          refac_util:write_refactored_files([{{File, File},AnnAST1}]),
          io:format("WARNING: Please check the implicit function calls!"),
          {ok, [File]};
	eclipse ->
          Res = [{File, File, refac_prettypr:print_ast(AnnAST1)}],
	  {ok, Res}
      end
  end.


%% =====================================================================
%% @spec convert_record_names(RecordParamsString::string()) 
%%                         -> [string()]
%%
%% @doc
%% Get the record field names from the string. 
%% 
%% Parameter description:<pre>
%% <b>RecordParamsString</b> : The name of the record fields.
%% </pre>
%% @end
%% =====================================================================
convert_record_names(RecordParamsString) ->
  produce_record_names([],RecordParamsString).

%% =====================================================================
%% @spec produce_record_names(RecordParamNames::[string()], 
%%                      String::string()) 
%%                              -> [string()]
%%
%% @doc
%% Produce the record field names from the string. 
%% 
%% Parameter description:<pre>
%% <b>RecordParamNames</b> : The already produced record field names.
%% <b>String</b> : The name of the record fields.
%% </pre>
%% @end
%% =====================================================================
produce_record_names(RecordParamNames, String) ->
  case io_lib:fread("~s", String) of
    {ok, [[]], []} -> lists:reverse(RecordParamNames);
    {more, "~s", 0, []} -> lists:reverse(RecordParamNames);
    {ok, [ParamName], Rest} ->
      produce_record_names([list_to_atom(ParamName) | RecordParamNames], Rest);
    Error -> exit({error, Error})
  end. 


%% =====================================================================
%% @spec check_if_correct_names( RecName::string(), 
%%                               FieldList::[string()]) -> ok
%%
%% @end
%% =====================================================================
check_if_correct_names(RecName, FieldList)->
  case refac_util:is_fun_name(RecName) of
    true -> ok;
    _ -> exit({error, "Not legal record name!"})
  end,
  lists:foreach(fun(Elem)->
                  case refac_util:is_fun_name(atom_to_list(Elem)) of
                    true -> ok;
                    _ -> exit({error, "Not legal record field name!"})
                  end
                end, FieldList).  

%% =====================================================================
%% @spec parse_file(FileName::string(), SearchPaths::[string()])  
%%                                               -> {syntaxtree(), ModInfo}
%% ModInfo = [{Key, term()}] 
%% Key = attributes | errors | exports | functions | imports | 
%%       module | records | rules | warnings                           
%%
%% @end
%% =====================================================================
parse_file(FileName, SearchPaths)->
 case refac_util:parse_annotate_file(FileName, false, SearchPaths) of
    {ok, {AnnAST, Info}}-> {AnnAST, Info};
    {error, Reason} -> erlang:exit({error,Reason})
 end.


%% =====================================================================
%% @spec check_pos(AnnAST::syntaxtree(), FirstPos::{integer(), integer()},
%%                 LastPos::{integer(), integer()}) 
%%                                   -> {syntaxtree(), syntaxtree(),atom()}
%% 
%% @end
%% =====================================================================
check_pos(AnnAST, FirstPos, LastPos)->
  case pos_to_app(AnnAST, FirstPos) of
    {ok, AppNode}-> 
      Node = pos_to_arg(AppNode, FirstPos, LastPos),
      {AppNode, Node, application};
    {error, none} -> 
      {FunClause, Node} = pos_to_pat(AnnAST,  FirstPos, LastPos),
      {FunClause, Node, function}
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
%% @spec pos_to_arg(AnnAST::syntaxtree(), FirstPos::{integer(), integer()},
%%                  LastPos::{integer(), integer()}) -> syntaxtree()
%% 
%% @end
%% =====================================================================
pos_to_arg(AppNode, FPos, LPos)->
  Args = refac_syntax:application_arguments(AppNode),
  case Args of 
    [] ->  exit({error, "You have not selected a tuple!"});
    _ -> 
      {Pos1, _Pos2} = refac_util:get_range(hd(Args)),
      if FPos >= Pos1 -> ok;
	 true -> exit({error, "You have not selected a tuple!"})
      end
  end,
  List = lists:dropwhile(fun(Pat)->
                             {Pos11, _Pos22} = refac_util:get_range(Pat),
                             Pos11 < FPos
                         end, Args),
  case List of
    [] -> exit({error, "You have not selected a tuple!"});
    _ -> 
      case refac_util:get_range(hd(List)) of
        {FPos, LPos} ->  hd(List);
	_  -> exit({error, "You have not selected a tuple!"})
      end
  end.


%% =====================================================================
%% @spec pos_to_pat(AnnAST::syntaxtree(), FirstPos::{integer(), integer()},
%%          LastPos::{integer(), integer()}) -> {syntaxtree(), syntaxtree()}
%% 
%% @end
%% =====================================================================
pos_to_pat(AnnAST, FPos, LPos)->
  case pos_to_clause(AnnAST, FPos) of
    {ok, Clause} ->
      FunPatterns = refac_syntax:clause_patterns(Clause),
      case FunPatterns of 
        [] ->  exit({error, "You have not selected a tuple!"});
        _ -> 
          {Pos1, _Pos2} = refac_util:get_range(hd(FunPatterns)),
          if FPos >= Pos1 -> ok;
	     true -> exit({error, "You have not selected a tuple!"})
          end
      end,
      List = lists:dropwhile(fun(Pat)->
                             {Pos11, _Pos22} = refac_util:get_range(Pat),
                             Pos11 < FPos
                             end, FunPatterns),
      case List of
        [] -> exit({error, "You have not selected a tuple!"});
        _ -> 
          case refac_util:get_range(hd(List)) of
            {FPos, LPos} -> {Clause, hd(List)};
	    _  -> exit({error, "You have not selected a tuple!"})
          end
      end;
    {error, none} -> exit({error, "You have not selected a tuple!"})   
  end.
    

%% =====================================================================
%% @spec pos_to_clause(Node::syntaxtree(),Pos::{integer(), integer()})
%%                                  -> {ok, syntaxTree()} | {error, none}
%% 
%% @end
%% =====================================================================
pos_to_clause(Node, Pos) ->
  case refac_util:once_tdTU(fun pos_to_clause_1/2, Node, Pos) of
    {_, false} -> {error, none};
    {R, true} -> {ok, R}
  end.


%% =====================================================================
%% @spec pos_to_clause_1(Node::syntaxtree(),Pos::{integer(), integer()}) 
%%                        -> {syntaxtree(), true} | {[], false}
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
%% @spec tuple_pos(Node::syntaxtree(),Tuple::syntaxtree(),Type::atom()) 
%%                                                           -> integer()
%% 
%% @end
%% =====================================================================
tuple_pos(Node,Tuple,Type)->
  case Type of
    application ->
      AppArgs = refac_syntax:application_arguments(Node),
      End1 = lists:dropwhile(fun(Elem)->
                               Elem /= Tuple
                             end,AppArgs),
      length(AppArgs) - length(End1) + 1;
    function ->
      FunPatts= refac_syntax:clause_patterns(Node),
      End2 = lists:dropwhile(fun(Elem)->
                               Elem /= Tuple
                             end,FunPatts),
      length(FunPatts) - length(End2) + 1
  end.


%% =====================================================================
%% @spec check_record_field_number(Tuple::syntaxtree(),
%%                                 FieldList::[string()]) -> ok
%% 
%% @end
%% =====================================================================
check_record_field_number(Tuple, FieldList)->
  FieldNumber = length(FieldList),
  case length(refac_syntax:tuple_elements(Tuple)) of
    FieldNumber -> ok;
    _ -> exit({error, "The field names number is not the same "
                      "as the tuple elements number"})
  end,
  case length(lists:usort(FieldList)) == length(FieldList) of
    true -> ok;
    false -> exit({error, "The given fields name are not unique!"})
  end.


%% =====================================================================
%% @spec check_record_name_exists(Info::ModInfo, RecName::string()) 
%%                                                       -> none | atom()
%% ModInfo = [{Key, term()}] 
%% Key = attributes | errors | exports | functions | imports | 
%%       module | records | rules | warnings 
%% 
%% @end
%% =====================================================================
check_record_name_exists(Info, RecName) ->
  case lists:keysearch(records, 1, Info) of
    {value, {records, RecList}} -> 
      ExistingRec = lists:filter(fun({Name, _Fields})->
                                   Name == list_to_atom(RecName)
                                 end, RecList),
      case ExistingRec of
        [] -> none;
        _  -> ExistingRec
      end;
    false -> none
  end.


%% =====================================================================
%% @spec get_fun_name(AnnAST::syntaxtree(),Node::syntaxtree(),
%%                    Type::atom()) -> {atom(),integer(),atom()}
%% 
%% @end
%% =====================================================================
get_fun_name(AnnAST, Node, Type)->
  case Type of
    function ->
      FunClause = Node,
      FunNode = clause_to_fun(AnnAST, FunClause),
      As = refac_syntax:get_ann(FunNode),
      case lists:keysearch(fun_def, 1, As) of
        {value, {fun_def, {Mod, Fun, Arity, _Pos, _DefPos}}} ->
          {Fun, Arity, Mod};
         _ -> exit({error, "The function definition can not locate"})
      end;
    application -> 
      AppNode = Node,
      Operator = refac_syntax:application_operator(AppNode),
      case get_fun_def_info(Operator) of 
        {ModName, Name, Arity, _} ->  {Name, Arity, ModName};
	Reason -> exit(Reason)
      end
  end.


%% =====================================================================
%% @spec clause_to_fun(Tree::syntaxtree(),Clause::syntaxtree()) 
%%                                                       -> syntaxtree()
%% 
%% @end
%% =====================================================================    
clause_to_fun(Tree, Clause) ->
  Res = clause_to_fun_1(Tree, Clause),
  case Res of 
    [H|_T] -> H;
    _ -> exit({error, "The function definition can not locate"})
  end.
    

%% =====================================================================
%% @spec clause_to_fun_1(Tree::syntaxtree(),Clause::syntaxtree()) 
%%                                                     -> [syntaxtree()]
%% 
%% @end
%% =====================================================================
clause_to_fun_1(Tree, Clause) ->
  {Start, End} = refac_util:get_range(Clause),
  {S, E} = refac_util:get_range(Tree),
  if (S =< Start) and (E >= End) ->
      case refac_syntax:type(Tree) of
        function -> [Tree];
        _ ->
	  Ts = refac_syntax:subtrees(Tree),
	  R0 = [[clause_to_fun_1(T, Clause) || T <- G] || G <- Ts],
	  lists:flatten(R0)
      end;
    true -> []
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
    _ -> {error, ""}
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
    false -> exit({error, "Can not get the current module name."})
  end.


%% =====================================================================
%% @spec check_def_mod(DefMod::atom(),ModName::atom()) -> ok
%% 
%% @end
%% =====================================================================
check_def_mod(DefMod, ModName)->
  if DefMod == ModName -> ok;
     true -> exit({error, "This function is not defined in this module;" 
                          "please go to the module where it is defined"})
  end.


%% =====================================================================
%% @spec tuple_record(AnnAST::syntaxtree(), ExistingRec:: none | atom(),
%%           RecName::string(), FieldList::[string()], Name::atom(), 
%%           Arity::integer(), Mod::atom(), N::integer()) -> syntaxtree()
%% 
%% @end
%% =====================================================================
tuple_record(AnnAST, ExistingRec, RecName, FieldList, Name, Arity,Mod,N)->
  {AnnAST1, _} = refac_util:stop_tdTP(fun do_tuple_to_record/2, AnnAST, 
             {ExistingRec, RecName, FieldList, Name, Arity,Mod,N,AnnAST}),
  case ExistingRec of
    none -> insert_record(AnnAST1, RecName, FieldList);	   
    _ -> AnnAST1      
  end.


%% =====================================================================
%% @spec do_tuple_to_record(Tree::syntaxtree(), {ExistingRec:: none | atom(),
%%                    RecName::string(), FieldList::[string()], Name::atom(), 
%%                    Arity::integer(), Mod::atom(), N::integer(), 
%%                                    AnnAST::syntaxtree()}) -> syntaxtree()
%% 
%% @end
%% =====================================================================
do_tuple_to_record(Tree, {ExistingRec, RecName, FieldList, 
                          Name, Arity, Mod, N, AnnAST})->
  case refac_syntax:type(Tree) of
    function ->
      FunName = refac_syntax:function_name(Tree),
      FunArity = refac_syntax:function_arity(Tree),
      DefMod = get_fun_def_mod(Tree),
      case (refac_syntax:atom_value(FunName) == Name) and 
           (FunArity == Arity) and (DefMod == Mod) of
        true ->
          Clauses = refac_syntax:function_clauses(Tree),
          NewClauses = new_clause(Clauses, N, RecName, FieldList, AnnAST),      
          {refac_syntax:copy_attrs(Tree, 
                       refac_syntax:function(FunName, NewClauses)), false};
	false -> {Tree, false}
      end;
    application -> 
      Operator = refac_syntax:application_operator(Tree),
      case application_info(Tree) of
	{{_, apply}, 2} -> 
         transform_apply_call(Tree,{RecName,FieldList,N,Name,Arity,Mod,AnnAST});
	{{_, apply}, 3} -> 
         transform_apply_call(Tree,{RecName,FieldList,N,Name,Arity,Mod,AnnAST});
	{{_, spawn}, 3} -> 
         transform_spawn_call(Tree,{RecName,FieldList,N,Name,Arity,Mod,AnnAST});
	{{_, spawn}, 4} -> 
         transform_spawn_call(Tree,{RecName,FieldList,N,Name,Arity,Mod,AnnAST});
	{{_, spawn_link}, 3} -> 
         transform_spawn_call(Tree,{RecName,FieldList,N,Name,Arity,Mod,AnnAST});
	{{_, spawn_link}, 4} -> 
         transform_spawn_call(Tree,{RecName,FieldList,N,Name,Arity,Mod,AnnAST});
	_ ->
	  case refac_syntax:type(Operator) of
	    atom -> modify_application(Operator, Name, Arity, Tree, N, 
                                       FieldList, RecName, AnnAST);
	    module_qualifier ->  
              modify_application(Operator, Name, Arity, 
                                 Tree, N, FieldList, RecName, AnnAST);
	    _ -> {Tree, false}
	  end
      end;
    attribute ->
      AttrName = refac_syntax:attribute_name(Tree),
      case {refac_syntax:atom_value(AttrName), ExistingRec} of
        {_, none} -> {Tree, false};
        {record, [Rec]} -> 
          Args = refac_syntax:attribute_arguments(Tree),
          EName = element(1,Rec),
          ArgName = refac_syntax:atom_value(hd(Args)),
          case ArgName == EName of
            true ->
               ArgElements = refac_syntax:tuple_elements(hd(tl(Args))),
               RecFieldsName = 
                 lists:map(fun(Elem)-> 
                             refac_syntax:atom_value(
                             refac_syntax:record_field_name(Elem))
                           end, ArgElements),
               NewRecFieldsName = FieldList -- RecFieldsName,
               NewRecFields = lists:map(fun(FName)->
                                          refac_syntax:record_field(
                                          refac_syntax:atom(FName),none)
                                        end, NewRecFieldsName),
               NewFieldArgs = refac_syntax:tuple(ArgElements ++ NewRecFields),
               NewArgs = [hd(Args), NewFieldArgs],
	       {refac_syntax:copy_attrs(Tree, 
                        refac_syntax:attribute(AttrName, NewArgs)), false};
            false ->{Tree, true}
          end;
        {_, _} -> {Tree, false}
      end;
    _ -> {Tree, false}
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
    _ -> exit({error," The function definition can not locate"})
  end.


%% =====================================================================
%% @spec new_clause(Clauses::[syntaxtree()],N::integer(),
%%        RecName::string(), FieldList::[string()], AnnAST::syntaxtree()) 
%%                                                               -> atom()
%% 
%% @end
%% =====================================================================
new_clause(Clauses, N, RecName, FieldList, AnnAST)->
  case Clauses of
    []-> [];
    [Hd|Tail]-> 
      Body = refac_syntax:clause_body(Hd),
      Guard = refac_syntax:clause_guard(Hd),
      Patterns = refac_syntax:clause_patterns(Hd),
      NewPat = create_new_patterns(Patterns, N, RecName, FieldList),
      create_new_clause(Hd, NewPat, Guard, Body, RecName, FieldList, AnnAST) ++ 
        new_clause(Tail, N, RecName, FieldList, AnnAST)
  end.


%% =====================================================================
%% @spec create_new_patterns(Patterns::[syntaxtree()], N::integer(),
%%                           RecName::string(), FieldList::[string]) 
%%    ->  [syntaxtree()] | 
%%              {not_tuple, [syntaxtree()], [syntaxtree()], syntaxtree()}
%% 
%% @end
%% =====================================================================
create_new_patterns(Patterns, N, RecName, FieldList)->
  Last = lists:nthtail(N, Patterns),
  First = Patterns -- lists:nthtail(N-1,Patterns),
  [Tuple] = (Patterns -- Last) -- First,
  case refac_syntax:type(Tuple)== tuple andalso 
       length(refac_syntax:tuple_elements(Tuple)) == length(FieldList) of 
    true ->
      TupleElements = refac_syntax:tuple_elements(Tuple),
      NewRecFields = lists:map(fun({FName, Value})->
                                 refac_syntax:record_field(
                                           refac_syntax:atom(FName),Value)
                               end, lists:zip(FieldList,TupleElements)),
      NewRecName = refac_syntax:atom(RecName),
      NewRec = [refac_syntax:record_expr(none, NewRecName, NewRecFields)],
      First ++ NewRec ++ Last;
    false -> {not_tuple, First, Last, Tuple}
  end.


%% =====================================================================
%% @spec create_new_clause(Clause::syntaxtree(),NewPat::Spec,
%%        Guard::syntaxtree(), Body::[syntaxtree()], RecName::string,
%%        FieldList::[string()], AnnAST::syntaxtree()) -> syntaxtree()
%% Spec = [syntaxtree()] | 
%%        {not_tuple, [syntaxtree()], [syntaxtree()], syntaxtree()}
%% 
%% @end
%% =====================================================================
create_new_clause(Clause, NewPat, Guard, Body, RecName, FieldList, AnnAST)->
  case NewPat of 
    {not_tuple, First, Last, NotTuple} -> 
      case analyze_tuple(AnnAST, NotTuple, length(FieldList)) of
        false -> [Clause];
        unknown ->
          NewPatterns = First ++ 
                    [create_match_expr(NotTuple, RecName)] ++ Last,
          NewGuard = 
          case Guard of 
            none -> Guard;
            _ -> 
              {NewG, _} = refac_util:stop_tdTP(fun modify_tree/2, Guard, 
                                             {NotTuple, RecName, FieldList}),
	      NewG
          end,
          NewBody = lists:map(fun(BodyElem)->
                                {New, _} = 
                                  refac_util:stop_tdTP(fun modify_tree/2, 
                                    BodyElem, {NotTuple, RecName, FieldList}),
                                New
                              end, Body),
          NewClause = refac_syntax:clause(NewPatterns, NewGuard, NewBody),
          [NewClause, Clause];
	true -> 
          case refac_syntax:type(NotTuple) of 
            match_expr -> 
              Variable = refac_syntax:match_expr_pattern(NotTuple),
              MatchBody = refac_syntax:match_expr_body(NotTuple),
              Type = refac_syntax:atom(RecName),
              TupleElements = refac_syntax:tuple_elements(MatchBody),
              RecFields = lists:map(fun({FName, Value})->
                                      refac_syntax:record_field(
                                           refac_syntax:atom(FName),Value)
                                    end, lists:zip(FieldList,TupleElements)),
              Rec = refac_syntax:record_expr(none, Type, RecFields),
              MatchExpr = refac_syntax:match_expr(Variable, Rec),
              NewPatterns = First ++ [MatchExpr] ++ Last,
              NewGuard = 
                case Guard of 
                  none -> Guard;
                  _ -> 
                    {NewG, _} = refac_util:stop_tdTP(fun modify_tree/2, Guard, 
                                             {Variable, RecName, FieldList}),
	            NewG
                end,
              NewBody = lists:map(fun(BodyElem)->
                                {New, _} = 
                                  refac_util:stop_tdTP(fun modify_tree/2, 
                                    BodyElem, {Variable, RecName, FieldList}),
                                New
                              end, Body),
              NewClause = refac_syntax:clause(NewPatterns, NewGuard, NewBody),
              [NewClause];
	    _ -> [Clause]
          end
      end;
    _ ->
      [refac_syntax:copy_attrs(Clause, 
       refac_syntax:clause(NewPat, Guard, Body))]
  end.


%% =====================================================================
%% @spec create_match_expr(Elem::syntaxtree(), RecName::string())
%%                                                         -> syntaxtree()
%% @end
%% =====================================================================
create_match_expr(Elem, RecName)->
  Type = refac_syntax:atom(RecName),
  Rec = refac_syntax:record_expr(none, Type, []),
  refac_syntax:match_expr(Elem, Rec).


%% =====================================================================
%% @spec modify_tree(Tree::syntaxtree(),{ELem::syntaxtree(), RecName::string(),
%%                   FieldList::[string]})-> {syntaxtree(), atom()}
%% @end
%% =====================================================================
modify_tree(Tree, {Elem, RecName, FieldList})->
 case refac_syntax:type(Tree) of
   variable ->
      case refac_syntax:variable_literal(Tree) == 
           refac_syntax:variable_literal(Elem) of
        true ->
           NewTuple=refac_syntax:tuple(create_tuple_elems(Elem, RecName,
                                                          FieldList, [])),
           {NewTuple, true};
        false -> {Tree, false}
      end;
   _ -> {Tree, false}
 end.


%% =====================================================================
%% @spec create_tuple_elems(Elem::syntaxtree(),Rec::atom(),
%%           List::[syntaxtree()], TupList::[syntaxtree()]) ->[syntaxtree()]
%% @end
%% =====================================================================
create_tuple_elems(Elem, Rec, List, TupList)->
  case length(List) of 
    0 ->  TupList;
    _ ->
      Type = refac_syntax:atom(Rec),
      Field = refac_syntax:record_field(refac_syntax:atom(hd(List)),none),
      TupElem = refac_syntax:record_access(Elem, Type, Field),
      create_tuple_elems(Elem, Rec, tl(List), TupList ++ [TupElem])	  
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
    _ -> erlang:error(not_an_application)
  end.


%% =====================================================================
%% @spec modify_application(Op::syntaxtree(),Name::atom(), Arity::integer(), 
%%            Tree::syntaxtree(), N::integer(),
%%            FieldList::[string()], RecName::string(), 
%%                                  AnnAST::syntaxtree()) -> syntaxtree()
%% 
%% @end
%% ===================================================================== 
modify_application(Op, Name, Arity, Tree, N, 
                   FieldList, RecName, AnnAST)->
  case get_fun_def_info(Op) of
    {_ModName, Name, Arity, _} ->
      Parameters = refac_syntax:application_arguments(Tree),
      NewArgs = create_new_patterns(Parameters, N, RecName,FieldList),
      create_new_application(AnnAST, Tree, Op, NewArgs, RecName, FieldList);
    _ -> {Tree, false}
  end.


%% =====================================================================
%% @spec create_new_application(AnnAST::syntaxtree(),Tree::syntaxtree(), 
%%            Operator::syntaxtree(), NewArgs::Spec, RecName::string(),
%%                                 FieldList::[string()]) -> syntaxtree()
%% 
%% Spec = [syntaxtree()] | 
%%        {not_tuple, [syntaxtree()], [syntaxtree()], syntaxtree()}
%%
%% @end
%% =====================================================================
create_new_application(AnnAST, Tree, Operator, NewArgs, RecName, FieldList)->
  case NewArgs of 
    {not_tuple, First, Last, Tuple} -> 
      case analyze_tuple(AnnAST, Tuple, length(FieldList)) of
        false -> {Tree, false};
        true -> 
          TupleElements = get_tuple_elements(Tuple, 1, [],length(FieldList)),
          NewRecFields = lists:map(fun({FName, Value})->
                                 refac_syntax:record_field(
                                           refac_syntax:atom(FName),Value)
                               end, lists:zip(FieldList, TupleElements)),
          NewRecName = refac_syntax:atom(RecName),
          NewRec = [refac_syntax:record_expr(none, NewRecName, NewRecFields)],
          NewAppArg = First ++ NewRec ++ Last,
          {refac_syntax:copy_attrs(Tree, 
           refac_syntax:application(Operator, NewAppArg)), false};
	unknown ->
          create_case(Tree, First, Last, Tuple, RecName, FieldList, Operator)
      end;
    _ ->
       {refac_syntax:copy_attrs(Tree, 
        refac_syntax:application(Operator, NewArgs)), false}
  end.

%% =====================================================================
%% @spec get_tuple_elements(Tuple::syntaxtree(), Number::integer(),
%%               List::[syntaxtree()], Max::integer()) -> [syntaxtree()]
%% @end
%% =====================================================================
get_tuple_elements(Tuple, Number, List, Max)->
  case Number > Max of
    true -> List;
    false ->
      FunName = refac_syntax:atom(element),
      Int = refac_syntax:integer(Number),
      App = refac_syntax:application(FunName, [Int, Tuple]),
      get_tuple_elements(Tuple, Number+1, List ++ [App], Max)
  end.

    
%% =====================================================================
%% @spec analyze_tuple(AnnAST::syntaxtree(), Tree::syntaxtree(),
%%                     N::integer()) ->  atom()
%%
%% @end
%% =====================================================================  
%%  variable -> analyze_variable
%%  match_expr -> analyze_match_expr
%%  tuple -> see the size of the tuple
%%  recor_expr, integer, list, nil, atom, binary, float, string -> false

%%  application, case_expr, if_expr, prefix_expr, block_expr -> unknown
%%  cond_expr, infix_expr, macro, query_expr, record_access -> unknown
%%  _ -> unknown, try_expr, catch_expr, receive_expr -> unknown 
%%  implicit_fun, fun_expr -> unknown 
%% =====================================================================    
analyze_tuple(AnnAST, Tree, N) ->
  case refac_syntax:type(Tree) of
    variable -> analyze_variable(AnnAST, Tree, N);
    match_expr -> analyze_match_expr(AnnAST, Tree, N);
    tuple -> 
      case length(refac_syntax:tuple_elements(Tree)) of
        N -> true;
        _ -> false
      end;
    record_expr -> false;
    integer -> false;
    list -> false;
    nil -> false; 
    atom -> false;
    binary  -> false;
    float -> false;
    string -> false;
    _ -> unknown
  end.

%% =====================================================================
%% @spec analyze_variable(AnnAST::syntaxtree(), Tree::syntaxtree(),
%%                     N::integer()) ->  atom()
%%
%% @end
%% =====================================================================    
analyze_variable(AnnAST, Tree, N)->
  Ann = refac_syntax:get_ann(Tree),
  case lists:keysearch(def, 1, Ann) of
    {value, {def, DefPosList}} ->
      DefExpr = lists:map(fun(Pos = {C,L})-> 
                            refac_util:pos_to_expr(AnnAST, Pos, {C+1,L})
                          end, DefPosList),
      case lists:keysearch(error, 1, DefExpr) of
        {value, {error, _}} -> unknown;
        _ -> 
          Result = lists:map(fun({_, Expr})->
                               case refac_syntax:type(Expr) of
                                  match_expr -> 
                                    analyze_match_expr(AnnAST, Expr, N);
			          _ -> unknown
                               end
                             end, DefExpr),
	  case length(lists:usort(Result)) of
            1 -> hd(Result);
	    _ -> unknown
          end
      end;
    _ -> unknown
  end.


%% =====================================================================
%% @spec analyze_match_expr(AnnAST::syntaxtree(), Tree::syntaxtree(),
%%                     N::integer()) ->  atom()
%%
%% @end
%% =====================================================================
analyze_match_expr(AnnAST, Tree, N)->
  Body = refac_syntax:match_expr_body(Tree),
  analyze_tuple(AnnAST, Body, N).



%% =====================================================================
%% @spec create_case(Tree::syntaxtree(), First::[syntaxtree()],
%%          Last::[syntaxtree()], Tuple::syntaxtree(), RecName::string(),
%%          FieldList::[string()], Operator::syntaxtree()) ->  atom()
%%
%% @end
%% =====================================================================    
create_case(Tree, First, Last, Tuple, RecName, FieldList, Operator)->
  TupleElements = get_tuple_elements(Tuple, 1, [],length(FieldList)),
  NewRecFields = lists:map(fun({FName, Value})->
                             refac_syntax:record_field(
                                       refac_syntax:atom(FName),Value)
                           end, lists:zip(FieldList, TupleElements)),
  NewRecName = refac_syntax:atom(RecName),
  NewRec = [refac_syntax:record_expr(none, NewRecName, NewRecFields)],
  NewAppArg = First ++ NewRec ++ Last,
  NewApp = refac_syntax:application(Operator, NewAppArg),
  True = refac_syntax:atom(true),
  TrueClause = refac_syntax:clause([True], none, [NewApp] ),
  False = refac_syntax:atom(false),
  FalseClause = refac_syntax:clause([False], none, [Tree]),
  AppAtom1 = refac_syntax:atom(is_tuple),
  App1 = refac_syntax:application(AppAtom1, [Tuple]),
  AppAtom2 = refac_syntax:atom(size),
  App2 = refac_syntax:application(AppAtom2, [Tuple]),
  FieldNumber = refac_syntax:integer(length(FieldList)),
  Right = 
     refac_syntax:infix_expr(App2, refac_syntax:operator("=="), FieldNumber),  
  AndAlso = refac_syntax:operator("andalso"),
  Argument = refac_syntax:infix_expr(App1, AndAlso, Right),
  {refac_syntax:copy_attrs(Tree, 
       refac_syntax:case_expr(Argument, [TrueClause, FalseClause])), true}.


%% =====================================================================
%% @spec insert_record(AnnAST::syntaxtree(), RecName::string(),
%%                             FieldList::[string()]) -> syntaxtree()
%% 
%%  @end
%% =====================================================================
insert_record(AnnAST, RecName, FieldList)->
  NewRecName = refac_syntax:atom(RecName),
  NewRecFields = lists:map(fun(FName)->
                             refac_syntax:record_field(
                             refac_syntax:atom(FName),none)
                           end, FieldList),
  NewArgs = [NewRecName, refac_syntax:tuple(NewRecFields)],
  NewAttribute = refac_syntax:attribute(refac_syntax:atom(record), NewArgs),
  Forms = refac_syntax:form_list_elements(AnnAST),
  Fun = fun(Form) ->
          case refac_syntax:type(Form) of 
            attribute -> 
              AttrName = refac_syntax:atom_value(
                         refac_syntax:attribute_name(Form)),
	      case AttrName of
	        module -> 
	          [Form, NewAttribute];
	        _ -> [Form]
              end;
            _ -> [Form]
        end
  end,
  refac_syntax:form_list([F||Form<-Forms, F <-Fun(Form)]).


%% =====================================================================
%% @spec tuple_to_record_in_client_modules(Files::[string()], Name::atom(), 
%%       Arity::integer(), Mod::atom(), N::integer(), RecName::string(),
%%       FieldList::[string()]) -> [{{string(), string()}, syntaxtree()}]
%% 
%% @end
%% =====================================================================
tuple_to_record_in_client_modules(Files, Name, Arity, Mod, N, 
                                  RecName, FieldList)->
  case Files of
    [] -> [];
    [F | Fs] ->
      io:format("The current file under refactoring is:\n~p\n", [F]),
      case refac_util:parse_annotate_file(F, false, []) of
	{ok, {AnnAST, Info}} ->
          ExistingRec = check_record_name_exists(Info, RecName),
	  AnnAST1 = tuple_record(AnnAST, ExistingRec, RecName, FieldList, 
                                 Name, Arity, Mod, N),
          [{{F, F}, AnnAST1} | 
             tuple_to_record_in_client_modules(Fs,Name,Arity,Mod,N,
                                               RecName,FieldList)];
	{error, Reason} -> exit({error, Reason})
      end
    end.


%% =====================================================================
%% @spec transform_apply_call(Node::syntaxtree(), {RecName::string(),
%%       FieldList::[string()], N::integer(), Name::atom(), Arity::integer(), 
%%                   ModName::atom(), AnnAST::syntaxtree()})  -> syntaxtree()
%% 
%% @end
%% =====================================================================
transform_apply_call(Node, {RecName, FieldList, N, 
                     Name, Arity, ModName, AnnAST}) ->
  Operator = refac_syntax:application_operator(Node),
  Arguments = refac_syntax:application_arguments(Node),
  case Arguments of
    [Fun, Args] ->
      Args1 = refac_syntax:list_elements(Args),
      Len = length(Args1),
      case {refac_syntax:atom_value(Fun), Len} of
	{Name, Arity} ->
         Args2 = create_new_patterns(Args1, N, RecName, FieldList),
         create_apply(Args2, AnnAST, RecName, FieldList, [Fun], Operator, Node);
        _ -> {Node, false}
      end;
    [Mod, Fun, Args] ->
      Args1 = refac_syntax:list_elements(Args),
      Len = length(Args1),
      case {refac_syntax:atom_value(Fun), Len, refac_syntax:atom_value(Mod)} of
        {Name, Arity, ModName} ->
          Args2 = create_new_patterns(Args1, N, RecName, FieldList),
          create_apply(Args2,AnnAST,RecName,FieldList,[Mod,Fun],Operator,Node);
        _ -> {Node, false}
      end;
    _ -> 
      {Node, false}
  end.


%% =====================================================================
%% @spec create_apply(Args::[syntaxtree()], AnnAST::syntaxtree(), 
%%       RecName::string(), FieldList::[string()], List::[syntaxtree()],
%%       Operator::syntaxtree(), Node::syntaxtree())  -> syntaxtree()
%% 
%% @end
%% =====================================================================
create_apply(Args, AnnAST, RecName, FieldList, List, Operator, Node)->
  case Args of
    {not_tuple, First, Last, Tuple}->
      case analyze_tuple(AnnAST, Tuple, length(FieldList)) of
        false -> 
          Args3 = List ++ [refac_syntax:list(First ++ [Tuple] ++ Last)],
          {refac_syntax:copy_attrs(Node, 
              refac_syntax:application(Operator, Args3)), false};
        true -> 
          TupleElements = get_tuple_elements(Tuple, 1, [],length(FieldList)),
          NewRecFields = lists:map(fun({FName, Value})->
                                 refac_syntax:record_field(
                                           refac_syntax:atom(FName),Value)
                               end, lists:zip(FieldList, TupleElements)),
          NewRecName = refac_syntax:atom(RecName),
          NewRec = [refac_syntax:record_expr(none, NewRecName, NewRecFields)],
          NewAppArg = First ++ NewRec ++ Last,
          Args3 = refac_syntax:list(NewAppArg),
          {refac_syntax:copy_attrs(Node, 
              refac_syntax:application(Operator, List ++ [Args3])), false};
        unknown ->
          create_case_apply(Node,First,Last,Tuple,RecName,
                           FieldList,Operator,List)
      end;
    _ -> 
      Args3 = refac_syntax:list(Args),
      {refac_syntax:copy_attrs(Node, 
              refac_syntax:application(Operator, List ++ [Args3])), false}
  end.


%% =====================================================================
%% @spec create_case_apply(Tree::syntaxtree(), First::[syntaxtree()], 
%%     Last::[syntaxtree()], Tuple::syntaxtree(), RecName::string(), 
%%     FieldList::[string()], Operator::syntaxtree(), List::[syntaxtree()])
%%                                                             -> syntaxtree()
%% 
%% @end
%% =====================================================================
create_case_apply(Tree,First,Last,Tuple,RecName,FieldList,Operator,List)->
  TupleElements = get_tuple_elements(Tuple, 1, [],length(FieldList)),
  NewRecFields = lists:map(fun({FName, Value})->
                             refac_syntax:record_field(
                                       refac_syntax:atom(FName),Value)
                           end, lists:zip(FieldList, TupleElements)),
  NewRecName = refac_syntax:atom(RecName),
  NewRec = [refac_syntax:record_expr(none, NewRecName, NewRecFields)],
  NewAppArg = refac_syntax:list(First ++ NewRec ++ Last),
  NewApp = refac_syntax:application(Operator, List ++ NewAppArg),
  True = refac_syntax:atom(true),
  TrueClause = refac_syntax:clause([True], none, [NewApp] ),
  False = refac_syntax:atom(false),
  FalseClause = refac_syntax:clause([False], none, [Tree]),
  AppAtom1 = refac_syntax:atom(is_tuple),
  App1 = refac_syntax:application(AppAtom1, [Tuple]),
  AppAtom2 = refac_syntax:atom(size),
  App2 = refac_syntax:application(AppAtom2, [Tuple]),
  FieldNumber = refac_syntax:integer(length(FieldList)),
  Right = 
     refac_syntax:infix_expr(App2, refac_syntax:operator("=="), FieldNumber),  
  AndAlso = refac_syntax:operator("andalso"),
  Argument = refac_syntax:infix_expr(App1, AndAlso, Right),
  {refac_syntax:copy_attrs(Tree, 
       refac_syntax:case_expr(Argument, [TrueClause, FalseClause])), false}.


%% =====================================================================
%% @spec transform_spawn_call(Node::syntaxtree(), {RecName::string(),
%%       FieldList::[string()], N::integer(), Name::atom(), Arity::integer(), 
%%                  ModName::atom(), AnnAST::syntaxtree()})  -> syntaxtree()
%% 
%% @end
%% =====================================================================
transform_spawn_call(Node,{RecName,FieldList,N,Name,Arity,ModName,AnnAST}) ->
  Operator = refac_syntax:application_operator(Node),
  Arguments = refac_syntax:application_arguments(Node),
  [None, Mod, Fun, Args] = if length(Arguments) == 4 -> Arguments;
			     true -> [none] ++ Arguments
	                   end,
  Args1 = refac_syntax:list_elements(Args),
  Len = length(Args1),
  case {refac_syntax:atom_value(Fun), Len, refac_syntax:atom_value(Mod)} of
    {Name, Arity, ModName} ->
      Args2 = create_new_patterns(Args1, N, RecName, FieldList),
      if length(Arguments) == 4 -> 
           create_apply(Args2, AnnAST, RecName, FieldList, 
                       [None, Mod, Fun], Operator, Node);
	   true -> 
             create_apply(Args2, AnnAST, RecName, FieldList, 
                         [Mod, Fun], Operator, Node)
	    end;
    _ -> {Node, false}
  end.

