%%@hidden
%%@private
-module(refac_batch_clone_elimination).

-export([composite_refac/1, input_par_prompts/0, select_focus/1]).

-include("../../include/wrangler.hrl").

input_par_prompts() ->
    []. 

select_focus(_Args=#args{current_file_name=File, 
                         cursor_pos=Pos}) ->
    api_interface:pos_to_fun_def(File, Pos).

composite_refac(_Args=#args{focus_sel=FunDef,
                            search_paths=SearchPaths}) ->
    {M,F,A} = api_refac:fun_define_info(FunDef),
    ?atomic([?interactive(
                ?refac_(rename_fun,
                        [M, {F, A}, 
                         {user_input, fun(_)->
                                              "New funcion name: " 
                                      end},
                         SearchPaths])),
             ?refac_(rename_var,
                     [M,
                      begin
                          {_, F1, A1} = ?current(M,F,A),
                          {F1, A1}
                      end,
                      fun(X) ->
                              re:run(atom_to_list(X), "NewVar*")/=nomatch
                      end, 
                      {user_input, fun({_, _, V}) ->
                                           lists:flatten(io_lib:format(
                                                           "Rename variable ~p to: ", [V]))
                                   end},
                      SearchPaths]),
             ?repeat_interactive(
               ?refac_(swap_args, 
                        [M,
                         begin
                             {_, F1, A1} = ?current(M,F,A),
                             {F1, A1}
                         end,
                         {user_input, fun(_)->"Index 1: " end}, 
                         {user_input, fun(_)->"Index 2: " end},
                         SearchPaths])),
            %% {refactoring, add_to_export, [?current(M,F,A)]},
             ?non_atomic(?refac_(fold_expr,
                                 [{file, fun(_File) ->true end}, 
                                  element(1,?current(M,F,A)),
                                  {element(2, ?current(M,F,A)),element(3, ?current(M,F,A))},
                                  1, true, SearchPaths]))
            ]).
    


