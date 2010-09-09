-module(erlide_syntax_tree).

-compile(export_all).

rewrite(List) when is_list(List) ->
    [rewrite(X) || X <-List];
                    rewrite(Tree) when is_tuple(Tree) ->
                        case erl_syntax:is_leaf(Tree) of
                            true ->
                                Type = erlide_syntax:type(Tree),
                                Ann = erlide_syntax:get_attrs(Tree),
                                Value = erlide_syntax:data(),
                                {Type, Ann, Value};
                            false ->        
                                Type = erlide_syntax:type(Tree),
                                Ann = erlide_syntax:get_attrs(Tree),
                                Children = rewrite(erlide_syntax:subtrees(Tree)),
                                {Type, Ann, Children}
                        end. 
