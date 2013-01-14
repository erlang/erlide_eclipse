
-record(model, {forms, comments}).

-record(function, {pos, name, arity, args, head, clauses, name_pos, exported}).
-record(clause, {pos, name, args, head, name_pos}).
-record(attribute, {pos, name, args, extra, arity}).
-record(other, {pos, name, tokens}).
%% -record(module, {name, erlide_path, model}).
