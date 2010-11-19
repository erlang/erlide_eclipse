
-record(model, {forms, comments}).

-record(function, {pos, name, arity, args, head, clauses, name_pos, comment, exported}).
-record(clause, {pos, name, args, head, name_pos}).
-record(attribute, {pos, name, args, extra}).
-record(other, {pos, name, tokens}).
%% -record(module, {name, erlide_path, model}).
