%%@hidden
%%@private
-module(refac_apply_to_remote_call).

-behaviour(gen_refac).

-export([input_par_prompts/0, select_focus/1,
         check_pre_cond/1, selective/0,
         transform/1]).

-include("../../include/wrangler.hrl").

%% The Emacs mini-buffer prompts for the user input parameters. 
-spec (input_par_prompts/0::() -> [string()]).                           
input_par_prompts() -> [].

%% Select the focus of interest. If no selection is neeeded, 
%% then return {ok, none}.
-spec (select_focus/1::(#args{}) -> {ok, syntaxTree()}|{ok, none}).  
select_focus(_Args) ->{ok, none}.

%% Pre-condition checking to ensure that the refactoring preserves the 
%% behaviour of the program.
-spec (check_pre_cond/1::(#args{}) -> ok).  
check_pre_cond(_Args) ->
    ok.

selective() ->
    true.

%%Do the actual program transformation here.
-spec (transform/1::(#args{}) -> {ok, [{filename(), filename(), syntaxTree()}]}).
transform(_Args=#args{search_paths=SearchPaths})->
    ?FULL_TD_TP([rule(),
                 rule1(),
                 rule2()
                ], [SearchPaths]).

rule() ->
    ?RULE(?T("Op@(N@@, M@, F@, [Args@@])"),
          ?TO_AST("M@:F@(Args@@)"),
          {erlang,apply,3} == api_refac:fun_define_info(Op@)).
          
rule1() ->
    ?RULE(?T("Op@(N@@, M@, F@, [])"),
          ?TO_AST("M@:F@()"),
          {erlang,apply,3} == api_refac:fun_define_info(Op@)). 
         
rule2() ->
    ?RULE(?T("Op@(Fun@, [Args@@])"),
          begin
              {M,F,_A} = api_refac:fun_define_info(Fun@),
              ?TO_AST(atom_to_list(M)++":"++atom_to_list(F)++"(Args@@)")
          end,
          case api_refac:fun_define_info(Fun@) of
              {_,_,_}->
                  true;
              _ -> 
                  false
          end).
          
