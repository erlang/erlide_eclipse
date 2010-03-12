-module(refac_atom_utils).


-export([start_atom_process/0, stop_atom_process/1, check_atoms/4, collect_atoms/2, has_warning_msg/1, 
	output_atom_warning_msg/3]).

-include("../include/wrangler.hrl").

start_atom_process() ->
    spawn_link(fun () -> atom_loop({[], []}) end).
stop_atom_process(Pid) ->
    Pid ! stop.

atom_loop({NotRenamed, Renamed}) ->
    receive
      {add_not_renamed, Data} ->
	  atom_loop({[Data| NotRenamed], Renamed});
      {add_renamed, {FileName, Pos}} ->
	  case Renamed of
	    [] -> atom_loop({NotRenamed, [{FileName, [Pos]}]});
	    [{FileName, Ps}| Others] ->
		atom_loop({NotRenamed, [{FileName, [Pos| Ps]}| Others]});
	    _ -> atom_loop({NotRenamed, [{FileName, [Pos]}| Renamed]})
	  end;
      {From, get} ->
	  From ! {self(), {lists:reverse(NotRenamed), lists:reverse(Renamed)}},
	  atom_loop({NotRenamed, Renamed});
      stop ->
	  ok
    end.

check_atoms(FileName, Tree, AtomNames, Pid) ->
    F = fun (T) ->
		case refac_syntax:type(T) of
		  function -> collect_atoms(T, AtomNames);
		  _ -> []
		end
	end,
    R = lists:usort(lists:flatten(lists:map(F, refac_syntax:form_list_elements(Tree)))),
    R1 = [X || {atom, X, _} <- R, X =/= {0, 0}],  %% X=/={0,0} only a temporay fix; should check where {0,0} is introduced.
    R2 = [X || {_, X, _} <- lists:filter(fun (X) ->
						 case X of
						   {atom, _X, _} -> false;
						   _ -> true
						 end
					 end,
					 R)],
    UndecidableAtoms = R1 -- R2,
    case UndecidableAtoms of
      [] -> ok;
      _ -> Pid ! {add_not_renamed, {FileName, UndecidableAtoms}}
    end.

collect_atoms(Tree, AtomNames) ->
    F = fun (T, S) ->
		case refac_syntax:type(T) of
		  function ->
		      N = refac_syntax:function_name(T),
		      collect_atoms_1(AtomNames, S, N, function);
		  application ->
		      Operator = refac_syntax:application_operator(T),
		      collect_atoms_1(AtomNames, S, Operator, function);
		  module_qualifier ->
		      Mod = refac_syntax:module_qualifier_argument(T),
		      Fun = refac_syntax:module_qualifier_body(T),
		      S1 = collect_atoms_1(AtomNames, S, Mod, module),
		      collect_atoms_1(AtomNames, S1, Fun, function);
		  arity_qualifier ->
		      Fun = refac_syntax:arity_qualifier_body(T),
		      case lists:member(refac_syntax:atom_value(Fun), AtomNames) of
			true -> S ++ [{function, refac_syntax:get_pos(Fun)}];
			false -> S
		      end;
		  infix_expr ->
		      Op = refac_syntax:infix_expr_operator(T),
		      Left = refac_syntax:infix_expr_left(T),
		      case refac_syntax:operator_name(Op) of
			'!' ->
			    case refac_syntax:type(Left) of
			      atom ->
				  AtomVal = refac_syntax:atom_value(Left),
				  case lists:member(AtomVal, AtomNames) of
				    true ->
					S ++ [{process, refac_syntax:get_pos(Left), AtomVal}];
				    _ -> S
				  end;
			      _ -> S
			    end;
			_ -> S
		      end;
		  record_expr -> Type = refac_syntax:record_expr_type(T),
				 collect_atoms_1(AtomNames, S, Type, record);
		  record_field -> Name = refac_syntax:record_field_name(T),
				  case refac_syntax:type(Name) of
				    atom ->
					AtomVal = refac_syntax:atom_value(Name),
					case lists:member(AtomVal, AtomNames) of
					  true ->
					      S ++ [{record, refac_syntax:get_pos(Name), AtomVal}];
					  _ -> S
					end;
				    _ -> S
				  end;
		  record_access -> Type = refac_syntax:record_access_type(T),
				   Field = refac_syntax:record_access_field(T),
				   S1 = collect_atoms_1(AtomNames, S, Type, record),
				   case refac_syntax:type(Field) of
				     atom ->
					 AtomVal = refac_syntax:atom_value(Field),
					 case lists:member(AtomVal, AtomNames) of
					   true ->
					       S ++ [{record, refac_syntax:get_pos(Field), AtomVal}];
					   false ->
					       S
					 end;
				     _ -> S1
				   end;
		  atom ->
		      AtomVal = refac_syntax:atom_value(T),
		      case lists:member(AtomVal, AtomNames) of
			true ->
			    Pos = refac_syntax:get_pos(T),
			    As = refac_syntax:get_ann(T),
			    case lists:keysearch(type, 1, As) of
			      {value, {type, {f_atom, [MName, FName, Arity]}}}
				  when not (is_atom(MName) andalso MName /= '_' andalso
					      is_atom(FName) andalso FName /= '_' andalso
						is_integer(Arity)) ->
				  Pos =
				      S ++ [{atom, Pos, AtomVal}];   %% This should be improved; as we know T is a function atom;
			      {value, _} -> S;
			      _ ->
				  S ++ [{atom, Pos, AtomVal}]
			    end;
			false -> S
		      end;
		  _ -> S
		end
	end,
    refac_syntax_lib:fold(F, [], Tree).

collect_atoms_1(AtomNames, S, Node, Type) ->
    F = fun (T, Acc) ->
		case refac_syntax:type(T) of
		  atom ->
		      AtomVal = refac_syntax:atom_value(T),
		      case lists:member(AtomVal, AtomNames) of
			true when Type == function ->
			    Ann = refac_syntax:get_ann(T),
			    case lists:keysearch(fun_def, 1, Ann) of
			      {value, {fun_def, {'_', _, _, _, _}}} ->
				  Acc ++ [{atom, refac_syntax:get_pos(T), AtomVal}];
			      {value, {fun_def, {_, '_', _, _, _}}} ->
				  Acc ++ [{atom, refac_syntax:get_pos(T), AtomVal}];
			      {value, {fun_def, {_, _, '_', _, _}}} ->
				  Acc ++ [{atom, refac_syntax:get_pos(T), AtomVal}];
			      {value, _} ->
				  Acc ++ [{function, refac_syntax:get_pos(T), AtomVal}];
			      false ->
				  Acc ++ [{atom, refac_syntax:get_pos(T), AtomVal}]
			    end;
			true ->
			    Acc ++ [{Type, refac_syntax:get_pos(T), AtomVal}];
			false ->
			    Acc
		      end;
		  _ -> Acc
		end
	end,
    refac_syntax_lib:fold(F, S, Node).


has_warning_msg(Pid) ->
    Pid! {self(), get},
    receive
	{Pid, {NotRenamed, Renamed}} ->
	    NotRenamed /= [] orelse
            Renamed /=[];
	_ -> throw({error, "Refactoring failed because of a Wrangler error."})
    end.

output_atom_warning_msg(Pid, NotRenamedWarnMsg, RenamedWarnMsg) ->
    Pid ! {self(), get},
    receive
      {Pid, {NotRenamed, Renamed}} ->
	  output_atom_warnings({NotRenamed, Renamed}, NotRenamedWarnMsg, RenamedWarnMsg);
      _ -> throw({error, "Refactoring failed because of a Wrangler error."})
    end.

output_atom_warnings({[], []}, _, _) ->
    ok;
output_atom_warnings({NotRenamed, Renamed}, NotRenamedMsg, RenamedMsg) ->
    output_atom_not_renamed_warnings(NotRenamed, NotRenamedMsg),
    output_atom_renamed_warnings(Renamed, RenamedMsg).

output_atom_not_renamed_warnings([], _Msg) -> ok;
output_atom_not_renamed_warnings(NotRenamed, Msg) ->
    ?wrangler_io(Msg, []),
    output_not_renamed_atom_info(NotRenamed).
output_atom_renamed_warnings([], _Msg) -> ok;
output_atom_renamed_warnings(Renamed, Msg) ->
    ?wrangler_io(Msg, []),
    output_renamed_atom_info(Renamed).
output_not_renamed_atom_info(FileAndPositions) ->
    Fun = fun ({FileName, Positions}) ->
		  lists:flatmap(fun (P) ->
					{Line, _Col} = P,
					FileName ++ io_lib:format(":~p: \n", [Line])
				end, Positions)
	  end,
    Msg = lists:flatmap(Fun, FileAndPositions),
    ?wrangler_io(Msg, []).

output_renamed_atom_info(FileAndExprs) ->
    Fun = fun ({FileName, Exprs}) ->
		  Fun0 = fun (Expr) ->
				 {Line, _Col} = refac_syntax:get_pos(Expr),
				 FileName ++ io_lib:format(":~p: ", [Line])
				   ++ refac_prettypr:format(Expr) ++ "\n"
			 end,
		  lists:flatmap(Fun0, Exprs)
	  end,
    Msg = lists:flatmap(Fun, FileAndExprs),
    ?wrangler_io(Msg, []).
