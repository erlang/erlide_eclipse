%% @private
%% @hidden
-module(refac_keysearch_to_keyfind).

-behaviour(gen_refac).

-export([input_par_prompts/0, select_focus/1,
         check_pre_cond/1, selective/0,
         transform/1]).

-include("../../include/wrangler.hrl").

%% No parameter input is required.
-spec (input_par_prompts/0::() -> [string()]).                           
input_par_prompts() -> [].

%% No focus selection is required.
-spec (select_focus/1::(#args{}) -> {ok, syntaxTree()}|{ok, none}).  
select_focus(_Args) ->{ok, none}.

%% No precondition checking is required.
-spec (check_pre_cond/1::(#args{}) -> ok).  
check_pre_cond(_Args) ->
    ok.

-spec (selective/0::()-> true).                          
selective() ->
    true.

%% Apply the transformation rules to all the Erlang files included in the 
%% SearchPaths.
-spec (transform/1::(#args{}) -> {ok, [{filename(), filename(), syntaxTree()}]}).
                                  
transform(_Args=#args{search_paths=SearchPaths})->
    ?STOP_TD_TP([rule_keysearch_to_keyfind()], SearchPaths).

rule_keysearch_to_keyfind() ->
    ?RULE(?T("case lists:keysearch(Key@, N@, TupleList@) of 
                         Pats@@@ when Guards@@@ ->
                             Body@@@
                    end"),
          begin
              NewPats@@@=make_new_pats(Pats@@@),
              ?TO_AST("case lists:keyfind(Key@, N@, TupleList@) of 
                                NewPats@@@ when Guards@@@ ->
                                   Body@@@
                    end")
          end,
          true).

make_new_pats(ListOfPats) ->
    [[make_new_pat(P)||P<-Pats]||Pats<-ListOfPats].
make_new_pat(Pat) ->
    case ?MATCH(?T("{value, T@}"), Pat) of
        true ->
            case api_refac:type(T@) of
                variable ->
                    ?TO_AST("T@={_,_}");
                underscore->
                    ?TO_AST("{_,_}");
                _ ->
                    T@
            end;
        false ->
            case ?MATCH(?T("false"), Pat) orelse 
                ?MATCH(?T("_"),Pat) of
                true ->
                    Pat;
                false ->
                    throw({error, "Transformation aborted."})
            end
    end.


