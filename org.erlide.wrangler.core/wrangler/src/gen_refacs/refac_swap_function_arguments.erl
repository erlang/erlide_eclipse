%%@doc This module shows how to write refactorings 
%% use the Wrangler API. 

%% The refactoring implemented in this module
%% swaps the Ith and Jth arguments of a function 
%% selected by the user. The value of Ith and Jth 
%% are user input. 
%% To perform this refactoring, point the cursor to
%% the function definition, and then select 
%% 'Apply adhoc refactoring' from the menu, after 
%% that, Wrangler will prompts you input the 
%% the refactoring rename, which is supposed to be 
%% the module name, and then you will prompted to 
%% input the values of Ith and Jth.
%% @hidden
%% @private
-module(refac_swap_function_arguments).

-behaviour(gen_refac).

-export([input_par_prompts/0, select_focus/1,
         check_pre_cond/1, selective/0,
         transform/1]).

-export([swap_args/7]).

-include("../../include/wrangler.hrl").

-import(api_refac, [fun_define_info/1]).

%% The user needs to input the indexes of 
%% the parameters to swap.
input_par_prompts() ->
    ["Parameter Index 1: ",
     "Parameter Index 2: "].

%% The user needs to point the cursor to the 
%% function definition whose parameters is to 
%% be re-arranged. 
%% Note: the function: pos_to_fun_def will be 
%% moved to module refac_api.
select_focus(_Args=#args{current_file_name=File, 
                         cursor_pos=Pos}) ->
    api_interface:pos_to_fun_def(File, Pos).

%% Check that the values of Ith and Jth inputted 
%% by the user are valid. 
check_pre_cond(_Args=#args{focus_sel=FunDef,
                          user_inputs=[I, J]}) ->
    Ith=list_to_integer(I),
    Jth=list_to_integer(J),
    {_M,_F,A} = fun_define_info(FunDef),
    case Ith /=Jth of 
        true ->
            case Ith>=1 andalso Ith=<A of 
                true ->
                    case Jth>=1 andalso Jth=<A of 
                        true ->
                            ok;
                        false ->
                            {error, "Index 2 is invalid."}
                    end;
                false ->
                    {error, "Index 1 is invalid."}
            end;
        false ->
            {error, "Index 1 and Index 2 are the same."}
    end.
                
selective() ->
    false.
%% Do the program transformation here.
transform(Args=#args{current_file_name=File,focus_sel=FunDef, 
                     user_inputs=[I, J]}) ->
    {M,F,A} = fun_define_info(FunDef),
    I1 = list_to_integer(I),
    J1 = list_to_integer(J),
    {ok, Res}=transform_in_cur_file(Args, {M,F,A}, I1, J1),
    case api_refac:is_exported({F,A}, File) of
        true ->
            {ok, Res1}=transform_in_client_files(Args, {M,F,A}, I1, J1),
            {ok, Res++Res1};
        false ->
            {ok, Res}
    end.
    
%% transform the current file.
transform_in_cur_file(_Args=#args{current_file_name=File},MFA, I, J)->
    ?FULL_TD_TP([rule1(MFA, I, J),
                 rule2(MFA, I, J),
                 rule3(MFA, I, J)],
                [File]).

%% transform the client files.
transform_in_client_files(_Args=#args{current_file_name=File,
                                      search_paths=SearchPaths}, 
                          MFA, I, J) ->
    ?FULL_TD_TP([rule2(MFA, I, J)],
                api_refac:client_files(File, SearchPaths)).


%% transform the function definition itself.
rule1({M,F,A}, I, J) ->
    ?RULE(?T("f@(Args@@) when Guard@@ -> Bs@@;"), 
          begin NewArgs@@=swap(Args@@,I,J),
                ?TO_AST("f@(NewArgs@@) when Guard@@->Bs@@;")
          end,
          api_refac:fun_define_info(f@) == {M, F, A}).

%% Transform the different kinds of function applications.
rule2({M,F,A}, I, J) ->
    ?RULE(?FUN_APPLY(M,F,A),
          begin
              Args=api_refac:get_app_args(_This@), 
              NewArgs=swap(Args, I, J),
              api_refac:update_app_args(_This@,NewArgs)
          end,
          true).

rule3({_M, F, A}, I, J) ->
    ?RULE(?T("Spec@"), 
          api_spec:swap_arg_types_in_spec(_This@, I, J),
          api_spec:is_type_spec(Spec@, {F, A})).

%% utility functions.
swap(List, I, J) when is_list(List) ->
    Ith = lists:nth(I, List),
    Jth = lists:nth(J, List),
    T = list_to_tuple(List),
    T1=setelement(J, setelement(I, T, Jth), Ith),
    tuple_to_list(T1);
swap(Node, I, J) ->
    Str=lists:flatten(
          io_lib:format(
            "fun(List) ->
                    Ith = lists:nth(~p, List),
                    Jth = lists:nth(~p, List),
                    T = list_to_tuple(List),
                    T1=setelement(~p, setelement(~p, T, Jth), Ith),
                    tuple_to_list(T1)
            end(~s)", [I, J, J, I, ?PP(Node)])),
    ?TO_AST(Str).


swap_args(FileName, {FunName, Arity}, Index1, Index2, SearchPaths, Editor, TabWidth) ->
    {ok, {AnnAST, _Info}} = wrangler_ast_server:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    ModName=list_to_atom(filename:basename(FileName, ".erl")),
    case wrangler_misc:funname_to_defpos(AnnAST, {ModName, FunName, Arity}) of
	{ok, DefPos} ->
            {ok, FunDef} = api_interface:pos_to_fun_def(FileName, DefPos),
            Args=#args{current_file_name=FileName,
                       focus_sel=FunDef,
                       user_inputs=[Index1, Index2],
                       search_paths=SearchPaths,
                       tabwidth=TabWidth},
            case check_pre_cond(Args) of
                ok -> 
                    {ok, Res}=transform(Args),
                    wrangler_write_file:write_refactored_files(Res,Editor,TabWidth,"");
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.
