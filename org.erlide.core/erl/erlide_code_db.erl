-module(erlide_code_db).

-compile(export_all).

-include("erlide_code_db.hrl").

init_db() ->
    mnesia:create_table(erlide_token, [{attributes, record_info(fields, erlide_token)},
                                 {index, [kind, line, offset]},
                                 {type, bag}]),
    mnesia:create_table(erlide_form, [{attributes, record_info(fields, erlide_form)},
                                 {index, [kind, start]},
                                 {type, bag}]),
    ok.

new_module(Project, Module) ->
    Fun = fun() ->
            mnesia:write(#erlide_token{module={Project, Module}})
          end,
    mnesia:async_dirty(Fun).


