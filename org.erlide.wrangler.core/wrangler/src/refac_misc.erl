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
%% =====================================================================
%% Some utility functions used by Wrangler.
%%
%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%%
%% =====================================================================

-module(refac_misc).

-export([group_by/2, filehash/1,collect_var_source_def_pos_info/1,
	 get_start_end_loc/1,variable_replaceable/1,apply_style_funs/0,
	 testserver_callback_funs/0,eqc_statem_callback_funs/0,
	 eqc_fsm_callback_funs/0,commontest_callback_funs/0, try_eval/4,
	 make_new_name/2,collect_var_names/1,collect_used_macros/1,
	 collect_used_records/1, ghead/2, glast/2, to_upper/1, to_lower/1, 
	 is_var_name/1,is_fun_name/1, remove_duplicates/1,
	 format_search_paths/1,default_incls/0, get_toks/1,reset_attrs/1,
	 get_env_vars/1,get_var_exports/1,get_bound_vars/1,get_free_vars/1,
	 is_expr/1,is_pattern/1, is_exported/2, inscope_funs/1,update_ann/2,
	 delete_from_ann/2, callback_funs/1, is_callback_fun/3, rewrite/2,
	 get_range/1, max/2, min/2]).

-include("../include/wrangler.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec group_by(integer(), [tuple()]) -> [[tuple()]].
group_by(N, TupleList) ->
    SortedTupleList = lists:keysort(N, lists:usort(TupleList)),
    group_by(N, SortedTupleList, []).

group_by(_N,[],Acc) -> Acc;
group_by(N,TupleList = [T| _Ts],Acc) ->
    E = element(N,T),
    {TupleList1,TupleList2} = 
	lists:partition(fun (T1) ->
				element(N,T1) == E
			end,
			TupleList),
    group_by(N,TupleList2,Acc ++ [TupleList1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec filehash(filename()) -> integer(). 	      
filehash(FileName) ->
    case file:open(FileName, [read, raw, binary]) of
      {ok, IoDevice} ->
	  Hash = filehash(IoDevice, 0),
	  file:close(IoDevice),
	  Hash;
      _ -> 0
    end.

filehash(IoDevice, Crc) ->
    case file:read(IoDevice, 1024) of
        {ok, Data} ->
            filehash(IoDevice, erlang:crc32(Crc, Data));
        eof ->
            Crc;
        {error, _Reason} ->
            0 %% TODO error handling
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec collect_var_source_def_pos_info([syntaxTree()]|syntaxTree()) ->
					     [{atom(), pos(), [pos()]}].
collect_var_source_def_pos_info(Nodes) when is_list(Nodes) ->
    lists:flatmap(fun (N) -> collect_var_source_def_pos_info(N) end, Nodes);
collect_var_source_def_pos_info(Node) ->
    F= fun(T, S) ->
	       case refac_syntax:type(T) of 
		   variable ->
		       SourcePos = refac_syntax:get_pos(T),
		       case lists:keysearch(def, 1, refac_syntax:get_ann(T)) of 
			   {value, {def, DefinePos}} ->
			       VarName = refac_syntax:variable_name(T),
			       S ++ [{VarName, SourcePos, DefinePos}];
			   _ ->
			       S
		       end;
		   _  -> S
	       end
       end,
    refac_syntax_lib:fold(F, [], Node).

-spec get_start_end_loc([syntaxTree()]|syntaxTree()) ->
 			       {pos(), pos()}.
get_start_end_loc(Exprs) when is_list(Exprs) ->
    E1 = hd(Exprs),
    En = lists:last(Exprs),
    {S, _E} = get_range(E1),
    {_S, E} = get_range(En),
    {S, E};
get_start_end_loc(Expr) ->
    get_range(Expr).

get_range(Node) ->
    As = refac_syntax:get_ann(Node),
    case lists:keysearch(range, 1, As) of
	{value, {range, {S, E}}} -> {S, E};
	_ -> {?DEFAULT_LOC,
	      ?DEFAULT_LOC} 
    end.


-spec variable_replaceable(syntaxTree()) ->boolean().
variable_replaceable(Exp) ->
    case lists:keysearch(category, 1, refac_syntax:get_ann(Exp)) of
      {value, {category, record_field}} -> false;
      {value, {category, record_type}} -> false;
      {value, {category, guard_expression}} -> false;
      {value, {category, macro_name}} -> false;
      {value, {category, pattern}} ->
	  refac_syntax:is_literal(Exp) orelse
	    refac_syntax:type(Exp) == variable;
      _ ->
	  T = refac_syntax:type(Exp),
	  not lists:member(T, [match_expr, operator, generator, receive_expr, try_expr, 
			      catch_expr]) andalso
	    get_var_exports(Exp) == [] 
		%% andalso
		%%  refac_misc:get_free_vars(Exp)/=[]
    end.

%% This function will be removed.
apply_style_funs() ->
    [{{erlang, apply, 3}, [modulename, functionname, arglist], term},
     {{erlang, spawn, 3}, [modulename, functionname, arglist], term},
     {{erlang, spawn, 4}, [node, modulename, functionname, arglist], term},
     {{erlang, spawn_link, 3}, [modulename, functionname, arglist], term},
     {{erlang, spawn_link, 4}, [term, modulename, functioname, arglist], term},
     {{erlang, spawn_monitor, 3}, [term, modulename, functionname, arglist], term},
     {{test_server, timecall, 3}, [modulename, functionname, arglist], term},
     {{test_server, do_times, 4}, [integer, modulename, functionname, arglist], term},
     {{test_server, call_crash, 3}, [modulename, functionname, arglist], term},
     {{test_server, call_crash, 4}, [term, modulename, functionname, arglist], term},
     {{test_server, call_crash, 5}, [term, term, modulename, functionname, arglist], term}].
 

-spec testserver_callback_funs()->[{atom(), integer()}].
testserver_callback_funs() ->
    [{all, 0}, {init_per_suite, 1}, {end_per_suite, 1}, {init_per_testcase, 2}, {fin_per_testcase, 2}].

-spec eqc_statem_callback_funs()->[{atom(), integer()}].
eqc_statem_callback_funs() ->
    [{initial_state, 0}, {precondition, 2}, {command, 1}, {postcondition, 3}, {next_state, 3}].

-spec eqc_fsm_callback_funs()->[{atom(), integer()}].
eqc_fsm_callback_funs() ->
    [{initial_state, 0}, {initial_state_data, 0}, {next_state_data, 5},
     {precondition, 4}, {postcondition, 5}].

-spec commontest_callback_funs()->[{atom(), integer()}].
commontest_callback_funs() ->
    [{all, 0}, {groups, 0}, {suite, 0}, {init_per_suite, 1}, {end_per_suite, 1}, {init_per_group, 2},
     {end_per_group, 2}, {init_per_testcase, 2}, {end_per_testcase, 2}, {testcase, 0}, {testcase, 1}].

-spec try_eval(filename()|none, syntaxTree(), [dir()], integer()) ->
		      term().
try_eval(none, Node, _, _) ->
    try
      erl_eval:exprs([refac_syntax:revert(Node)], [])
    of
      {value, Val, _} -> {value, Val}
    catch
      _E1:_E2 ->
	  {error, no_value}
    end;
try_eval(FileName, Node, SearchPaths, TabWidth) ->
    try
      erl_eval:exprs([refac_syntax:revert(Node)], [])
    of
      {value, Val, _} -> {value, Val}
    catch
      _:_ ->
	  case has_macros(Node) andalso get_free_vars(Node) == [] of
	    true ->
		Dir = filename:dirname(FileName),
		DefaultIncl2 = [filename:join(Dir, X) || X <- default_incls()],
		NewSearchPaths = SearchPaths ++ DefaultIncl2,
		{Ms, UMs} = case refac_epp:parse_file(FileName, NewSearchPaths, []) of
			      {ok, _, {Defs, Uses}} ->
				  {dict:from_list(Defs), dict:from_list(Uses)};
			      _ -> {[], []}
			    end,
		NodeToks = get_toks(FileName, Node, TabWidth),
		try
		  refac_epp:expand_macros(NodeToks, {Ms, UMs})
		of
		  NewToks when is_list(NewToks) ->
		      case refac_parse:parse_exprs(NewToks ++ [{dot, {999, 0}}]) of
			{ok, Exprs} ->
			    try
			      erl_eval:exprs(Exprs, [])
			    of
			      {value, Val, _} -> {value, Val}
			    catch
			      _:_ -> {error, no_value}
			    end;
			_ -> {error, no_value}
		      end
		catch
		  _:__ -> {error, no_value}
		end;
	    false ->
		{error, no_value}
	  end
    end.


-spec get_toks(filename(), syntaxTree(), integer()) ->
		      [token()].
get_toks(FileName, Node, TabWidth) ->
    Toks = refac_util:tokenize(FileName, false, TabWidth),
    {StartPos, EndPos} = get_start_end_loc(Node),
    Toks1 = lists:dropwhile(fun (T) ->
				    token_loc(T) < StartPos
			    end, Toks),
    lists:takewhile(fun (T) ->
			    token_loc(T) =< EndPos
		    end, Toks1).

token_loc(T) ->
    case T of
      {_, L, _V} -> L;
      {_, L1} -> L1
    end.

has_macros(Node) ->
    F = fun (N, _Others) ->
		case refac_syntax:type(N) of
		  macro -> {N, true};
		  _ -> {[], false}
		end
	end,
    {_, Res} = ast_traverse_api:once_tdTU(F, Node, []),
    Res.
    


-spec make_new_name(atom(), [atom()]) ->atom().			   
make_new_name(VarName, UsedVarNames) ->
    NewVarName = list_to_atom(atom_to_list(VarName)++"_1"),
    case ordsets:is_element(NewVarName, UsedVarNames) of
	true ->
	    make_new_name(NewVarName, UsedVarNames);
	_ -> 
	    NewVarName
    end.

-spec collect_var_names(syntaxTree()|[syntaxTree()]) ->
			       [atom()].
collect_var_names(Node) when is_list(Node) ->
    collect_var_names_1(refac_syntax:block_expr(Node));
collect_var_names(Node) ->
    collect_var_names_1(Node).
collect_var_names_1(Node) ->
    F = fun (N, S) ->
		case refac_syntax:type(N) of
		    variable ->
			case lists:keysearch(category, 1, refac_syntax:get_ann(N)) of
			    {value, {category, macro_name}} -> S;
			    _ ->
				VarName = refac_syntax:variable_name(N),
				ordsets:add_element(VarName, S)
			end;
		    _ -> S
		end
	end,
    ordsets:to_list(refac_syntax_lib:fold(F, ordsets:new(), Node)).


-spec collect_used_macros(syntaxTree()) ->
				 [atom()].
collect_used_macros(Node) ->
    F = fun(T, S) ->
		case refac_syntax:type(T) of
		    macro ->
			Name = refac_syntax:macro_name(T),
			case refac_syntax:type(Name) of 
			    variable -> [refac_syntax:variable_name(Name)|S];
			    atom -> [refac_syntax:atom_value(Name) |S]
			end;
		    _  -> S
		end
	end,
    lists:usort(refac_syntax_lib:fold(F, [], Node)).

-spec collect_used_records(syntaxTree())-> [atom()].
collect_used_records(Node) ->
    Fun = fun(T, S) ->
		  case refac_syntax:type(T) of
		      record_access ->
			  Type = refac_syntax:record_access_type(T),
			  case refac_syntax:type(Type) of
			      atom -> 
				  ordsets:add_element(refac_syntax:atom_value(Type), S);
			      _ -> S
			  end;
		      record_expr ->
			  Type = refac_syntax:record_expr_type(T),
			  case refac_syntax:type(Type) of
			      atom -> 
				  ordsets:add_element(refac_syntax:atom_value(Type), S);
			      _ -> S
			  end;
		      record_index_expr->
			  Type = refac_syntax:record_index_expr_type(T),
			  case refac_syntax:type(Type) of
			      atom ->
				  ordsets:add_element(refac_syntax:atom_value(Type), S);
			      _ -> S
			  end;
		      _ -> S		  
		  end
	  end,
    ordsets:to_list(refac_syntax_lib:fold(Fun, ordsets:new(), Node)).

%% =====================================================================
%% @doc Same as erlang:hd/1, except the first argument which is the
%%  error message when the list is empty.
%% @see glast/2

-spec(ghead(Info::string(),List::[any()]) -> any()).
ghead(Info, []) -> erlang:error(Info);
ghead(_Info, List) -> hd(List).

%% =====================================================================
%% @doc Same as lists:last(L), except the first argument which is the 
%%  error message when the list is empty.
%% @see ghead/2

-spec(glast(Info::string(), List::[any()]) -> any()).
glast(Info, []) -> erlang:error(Info);
glast(_Info, List) -> lists:last(List).

%% =====================================================================
%% @doc Convert a string into upper case.
%% @see to_lower/1

-spec(to_upper(Str::string()) -> string()).
to_upper(Str) ->
    to_upper(Str, []).

to_upper([C | Cs], Acc) when C >= 97, C =< 122 ->
    to_upper(Cs, [C - (97 - 65) | Acc]);
to_upper([C | Cs], Acc) -> to_upper(Cs, [C | Acc]);
to_upper([], Acc) -> lists:reverse(Acc).


%% =====================================================================
%% @doc Convert a string into lower case.
%% @see to_upper/1

-spec(to_lower(Str::string()) -> string()).
to_lower(Str) ->
    to_lower(Str, []).

to_lower([C | Cs], Acc) when C >= 65, C =< 90 ->
    to_lower(Cs, [C + (97 - 65) | Acc]);
to_lower([C | Cs], Acc) -> to_lower(Cs, [C | Acc]);
to_lower([], Acc) -> lists:reverse(Acc).

%% =====================================================================
%% @doc Return true if a string is lexically a  variable name.
-spec(is_var_name(Name:: [any()])-> boolean()).
is_var_name(Name) ->
    case Name of
      [] -> false;
      [H] -> is_upper(H) and (H =/= 95);
      [H| T] -> (is_upper(H) or (H == 95)) and is_var_name_tail(T)
    end.

is_var_name_tail(Name) ->
    case Name of
      [H| T] ->
	  (is_upper(H) or is_lower(H) or 
	   is_digit(H) or (H == 64) or (H == 95)) and
	    is_var_name_tail(T);
      [] -> true
    end.

is_upper(L) -> (L >= 65) and (90 >= L).

is_lower(L) -> (L >= 97) and (122 >= L).

is_digit(L) -> (L >= 48) and (57 >= L).
    

%% =====================================================================
%% @doc Return true if a name is lexically a function name.

-spec(is_fun_name(Name:: [any()])-> boolean()).
is_fun_name(Name) ->
    case Name of
      [H| T] -> is_lower(H) and is_var_name_tail(T);
      [] -> false
    end.


-spec remove_duplicates([any()]) ->[any()].
remove_duplicates(L) ->
    remove_duplicates(L, []).
remove_duplicates([],Acc) ->
     lists:reverse(Acc);
remove_duplicates([H|T], Acc) ->
    case lists:member(H, Acc) of
	true ->
	    remove_duplicates(T, Acc);
	_ ->
	    remove_duplicates(T, [H|Acc])
    end.


-spec format_search_paths([dir()]) -> string().				 
format_search_paths(Paths) ->
    format_search_paths(Paths, "").
format_search_paths([], Str)->
    Str;
format_search_paths([P|T], Str)->
    case Str of
	[] ->format_search_paths(T, "\""++P++"\"");
	_ ->format_search_paths(T, Str++", \""++P++"\"")
    end.
    
-spec default_incls()->[string()].			   
default_incls() ->
  [".", "..", "../hrl", "../incl", "../inc", "../include",
   "../../hrl", "../../incl", "../../inc", "../../include",
   "../../../hrl", "../../../incl", "../../../inc", "../../../include"].

%% ============================================================================
%% @doc Return the token list annoated to a form if there is any.

-spec(get_toks(Node::syntaxTree())-> [token()]).
get_toks(Node) ->
    As = refac_syntax:get_ann(Node),
    case lists:keysearch(toks, 1, As) of
      {value, {toks, Toks}} -> Toks;
      _ -> []
    end.

%% =====================================================================
%% @doc Reset all the annotations in the subtree to the default (empty) annotation.

-spec(reset_attrs(Node::syntaxTree()) -> syntaxTree()).
reset_attrs(Node) ->
    ast_traverse_api:full_buTP(fun (T, _Others) -> 
				       T1=refac_syntax:set_ann(T, []),
				       refac_syntax:remove_comments(T1)
			       end, Node, {}).

%% =====================================================================
%% @doc Return the environment variables of an AST node.

-spec(get_env_vars(Node::syntaxTree())-> [{atom(), pos()}]).
get_env_vars(Node) ->
    get_env_vars_1(refac_syntax:get_ann(Node)).

get_env_vars_1([{env, B} | _Bs]) -> B;
get_env_vars_1([_ | Bs]) -> get_env_vars_1(Bs);
get_env_vars_1([]) -> [].

%% =====================================================================
%% @doc Return the exported variables of an AST node.

-spec(get_var_exports(Node::[syntaxTree()]|syntaxTree())-> [{atom(),pos()}]).
get_var_exports(Nodes) when is_list(Nodes) ->
    lists:flatmap(fun (Node) -> get_var_exports(Node) end, Nodes);
get_var_exports(Node) ->
    get_var_exports_1(refac_syntax:get_ann(Node)).

get_var_exports_1([{bound, B} | _Bs]) -> B; %% Think about this again!!
get_var_exports_1([_ | Bs]) -> get_var_exports_1(Bs);
get_var_exports_1([]) -> [].


%% =====================================================================
%% @doc Return the bound variables of an AST node.

-spec(get_bound_vars(Node::[syntaxTree()]|syntaxTree())-> [{atom(),pos()}]).
get_bound_vars(Nodes) when is_list(Nodes) ->
    lists:usort(lists:flatmap(fun (Node) -> get_bound_vars(Node) end, Nodes));
get_bound_vars(Node) ->
    lists:usort(refac_syntax_lib:fold(fun (N, Acc) ->
					      get_bound_vars_1(refac_syntax:get_ann(N)) ++ Acc
				      end, [], Node)).
					       
get_bound_vars_1([{bound, B} | _Bs]) -> B;
get_bound_vars_1([_ | Bs]) -> get_bound_vars_1(Bs);
get_bound_vars_1([]) -> [].

%% =====================================================================
%% @doc Return the free variables of an AST node.
-spec(get_free_vars(Node::[syntaxTree()]|syntaxTree())-> [{atom(),pos()}]).
get_free_vars(Nodes) when is_list(Nodes) ->
    FBVs = lists:map(fun (Node) ->
			     {get_free_vars(Node), get_bound_vars(Node)}
		     end, Nodes),
    {FVs, BVs} = lists:unzip(FBVs),
    lists:usort(lists:append(FVs)) -- lists:usort(lists:append(BVs));
get_free_vars(Node) ->
    get_free_vars_1(refac_syntax:get_ann(Node)).

get_free_vars_1([{free, B} | _Bs]) -> B;
get_free_vars_1([_ | Bs]) -> get_free_vars_1(Bs);
get_free_vars_1([]) -> [].
     
%% =====================================================================
%% @doc Return true if an AST node represents an expression.
-spec(is_expr(Node:: syntaxTree())-> boolean()).
is_expr(Node) ->
    As = refac_syntax:get_ann(Node),
    case lists:keysearch(category, 1, As) of
      {value, {category, C}} ->
	  case C of
	      expression -> true;
	      guard_expression -> true;
	      application_op -> true;
	      generator -> true;
	    _ -> false
	  end;
      _ -> false
    end.

%% =====================================================================
%% @doc Return true if an AST node represents a pattern.
-spec(is_pattern(Node:: syntaxTree())-> boolean()).
is_pattern(Node) ->
    As = refac_syntax:get_ann(Node),
    case lists:keysearch(category, 1, As) of
	{value, {category, pattern}} ->
	    true;
	_ -> false
    end.

%%===============================================================================
%% @spec is_exported({FunName::atom(), Arity::integer()},ModuleInfo) -> boolean()
%%       ModuleInfo = [{Key, term()}]
%%       Key = attributes | errors | exports | functions | imports | module
%%             | records | rules | warnings
%% @doc Return true if the function is exported by its defining module.
%% @TODO: Think about the interface of this function again.

-spec(is_exported({FunName::atom(), Arity::integer()},ModInfo::moduleInfo()) -> boolean()).
is_exported({FunName, Arity}, ModInfo) ->
    ImpExport = case lists:keysearch(attributes, 1, ModInfo) of
		    {value, {attributes, Attrs}} -> 
			lists:member({compile, export_all}, Attrs);
		    false -> false
		end,
    ExpExport= 	case lists:keysearch(exports, 1, ModInfo) of
		    {value, {exports, ExportList}} ->
			 lists:member({FunName, Arity}, ExportList);
		    _ -> false
		end,
    ImpExport or ExpExport.



%%===============================================================================
%% @spec inscope_funs(ModuleInfo) -> [{ModName, FunName, Arity}]
%%       ModuleInfo = [{Key, term()}]
%%       Key = attributes | errors | exports | functions | imports | module
%%             | records | rules | warnings
%%       ModName = atom()
%%       FunName = atom()
%%       Arity = integer()
%%
%% @doc Returns the functions that are inscope (either imported by the 
%% module or defined within the module) in the current module.
%% @TODO: Think about the interface of this function again.

-spec(inscope_funs(moduleInfo()) -> [{atom(), atom(), integer()}]).
inscope_funs(ModuleInfo) ->
    case lists:keysearch(module, 1, ModuleInfo) of
      {value, {module, M}} ->
	  Imps = case lists:keysearch(imports, 1, ModuleInfo) of
		   {value, {imports, I}} ->
		       lists:append([lists:map(fun ({F, A}) -> {M1, F, A} end, Fs) || {M1, Fs} <- I]);
		   _ -> []
		 end,
	  Funs = case lists:keysearch(functions, 1, ModuleInfo) of
		   {value, {functions, Fs}} -> lists:map(fun ({F, A}) -> {M, F, A} end, Fs);
		   _ -> []
		 end,
	  PreDefinedFuns=[{M, module_info, 1}, {M, module_info, 2}, {M, record_info, 2}],
	  Imps ++ Funs ++ PreDefinedFuns;
      _ -> []
    end.
		

%% =====================================================================
%% @spec update_ann(Node::syntaxTree(), {Key::atom(), Val::term()}) -> syntaxTree()
%% @doc Update a specific annotation of the Node with the given one.
%% if the kind of annotation already exists in the AST node, the annotation 
%% value is replaced with the new one, otherwise the given annotation info 
%% is added to the node.

-spec(update_ann(Node::syntaxTree(), {Key::atom(), Val::anyterm()}) -> syntaxTree()).
update_ann(Tree, {Key, Val}) ->
    As0 = refac_syntax:get_ann(Tree),
    As1 = case lists:keysearch(Key, 1, As0) of
	    {value, _} -> lists:keyreplace(Key, 1, As0, {Key, Val});
	    _ -> As0 ++ [{Key, Val}]
	  end,
    refac_syntax:set_ann(Tree, As1).


-spec(delete_from_ann(Node::syntaxTree(), Key::atom()) -> syntaxTree()).
delete_from_ann(Tree, Key) ->
    As0=refac_syntax:get_ann(Tree),
    As1 = lists:keydelete(Key, 1,As0),
    refac_syntax:set_ann(Tree, As1).


%% =====================================================================
%% @spec callback_funs(Behaviour)->[{FunName, Arity}]
%%       Behaviour = gen_server | gen_event | gen_fsm | supervisor
%%       FunName = atom()
%%       Arity = integer()
%% @doc Pre-defined callback functions by the standard Erlang behaviours.

-type(behaviour()::gen_server | gen_event | gen_fsm | supervisor).
-spec(callback_funs(behaviour())->[{atom(), integer()}]).
callback_funs(Behaviour) ->
    case Behaviour of
      gen_server ->
	  [{init, 1}, {handle_call, 3}, {handle_cast, 2}, {handle_info, 2},
	   {terminate, 2}, {code_change, 3}];
      gen_event ->
	  [{init, 1}, {handle_event, 2}, {handle_call, 2}, {handle_info, 2},
	   {terminate, 2}, {code_change, 3}];
      gen_fsm ->
	  [{init, 1}, {handle_event, 3}, {handle_sync_event, 4}, {handle_info, 3},
	   {terminate, 3}, {code_change, 4}];
      supervisor -> [{init, 1}];
      _ -> []
    end.

-spec is_callback_fun(moduleInfo(), atom(), integer()) ->boolean().
is_callback_fun(ModInfo, Funname, Arity) ->
    case lists:keysearch(attributes, 1, ModInfo) of
      {value, {attributes, Attrs}} ->
	  case lists:keysearch(behaviour, 1, Attrs) of
	    {value, {behaviour, B}} ->
		lists:member({Funname, Arity},
			     callback_funs(B));
	    _ -> false
	  end;
      _ -> false
    end.

-spec rewrite(syntaxTree(), syntaxTree())->syntaxTree().
rewrite(Tree, Tree1) ->
    refac_syntax:copy_attrs(Tree, Tree1).


max(X,Y) when X>Y ->
     X;
max(_,Y) -> Y.
    
min(X,Y) when X>Y ->
     Y;
min(X,_) -> X.
