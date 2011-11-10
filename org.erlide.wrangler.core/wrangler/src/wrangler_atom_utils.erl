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

%%@hidden
%%@private
-module(wrangler_atom_utils).


-export([start_atom_process/0, stop_atom_process/1, 
	 check_unsure_atoms/5,collect_unsure_atoms_in_file/3,
	 has_warning_msg/1, output_atom_warning_msg/3]).

-include("../include/wrangler_internal.hrl").


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

check_unsure_atoms(FileName, FileAST, AtomNames, AtomType, Pid) ->
    UndecidableAtoms = collect_unsure_atoms_in_file(FileAST, AtomNames, AtomType),
    case UndecidableAtoms of
	[] -> 
	    ok;
	_ ->
            UndecidableLocs= [Pos || {atom, Pos, _} <- UndecidableAtoms],
            Pid ! {add_not_renamed, {FileName, UndecidableLocs}}
    end.

collect_unsure_atoms_in_file(FileAST, AtomNames, AtomType) ->
    F = fun (T) ->
		case wrangler_syntax:type(T) of
		    function ->
                        collect_unsure_atoms(T, AtomNames, AtomType);
		    _ -> []
		end
	end,
    lists:usort(lists:flatmap(F, wrangler_syntax:form_list_elements(FileAST))).
  
collect_unsure_atoms(Tree, AtomNames, AtomType) ->
    F = fun (Node,S) ->
		case wrangler_syntax:type(Node) of
		    atom ->
			AtomVal = wrangler_syntax:atom_value(Node),
			case lists:member(AtomVal, AtomNames) of
			    true ->
				Pos = wrangler_syntax:get_pos(Node),
				case Pos ==0 orelse Pos=={0,0} of
				    true -> S;
				    false ->
                                        As = wrangler_syntax:get_ann(Node),
                                        case lists:keysearch(type, 1, As) of
					    {value, {type,{f_atom, [M, _F, A]}}} ->
                                                case AtomType of 
                                                    {f_atom, {M1, _,A1}} when length(AtomNames)==1->
                                                        case unsure_match({M1,A1}, {M,A}) of 
                                                            true ->
                                                                S ++ [{atom, Pos, AtomVal}];
                                                            false ->
                                                                S
                                                        end;
                                                    f_atom ->
                                                        case unsure_match({M,A}) of
                                                            true ->
                                                                S ++ [{atom, Pos, AtomVal}];
                                                            false ->
                                                                S
                                                        end;  
                                                    _ ->S
                                                end;
                                            {value, {type, _}} ->
                                                S;
                                            _ ->
                                                S ++ [{atom, Pos, AtomVal}]
                                        end
                                end;
			    false ->
                                S
			end;
		    _ -> S
		end
	end,
    api_ast_traverse:fold(F, [], Tree).

unsure_match({M, A}, {_M1, A1}) when is_integer(A1)->
    A1==A andalso (not is_atom(M) orelse M=='_');
unsure_match({M, _A}, {M1,A1}) when not is_integer(A1)->
    (M==M1) orelse M=='_' orelse (not is_atom(M)).

unsure_match({M,A}) when is_integer(A) ->
    not (is_atom(M) andalso M/='_');
unsure_match({_M,A}) when not is_integer(A) ->
    true.


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
    ?log_warning(Msg),
    output_not_renamed_atom_info(NotRenamed).
output_atom_renamed_warnings([], _Msg) -> ok;
output_atom_renamed_warnings(Renamed, Msg) ->
    ?log_warning(Msg),
    output_renamed_atom_info(Renamed).
output_not_renamed_atom_info(FileAndPositions) ->
    Fun = fun ({FileName, Positions}) ->
		  lists:flatmap(fun (P) ->
					{Line, _Col} = P,
					FileName ++ io_lib:format(":~p: \n", [Line])
				end, Positions)
	  end,
    Msg = lists:flatmap(Fun, FileAndPositions),
    ?log_warning(Msg).

output_renamed_atom_info(FileAndExprs) ->
    Fun = fun ({FileName, Exprs}) ->
		  Fun0 = fun (Expr) ->
				 Pos = wrangler_syntax:get_pos(Expr),
				 FileName ++ io_lib:format(":~p: ", [Pos])
				  ++ wrangler_prettypr:format(Expr) ++ "\n"
			 end,
		  lists:flatmap(Fun0, Exprs)
	  end,
    Msg = lists:flatmap(Fun, FileAndExprs),
    ?log_warning(Msg).


