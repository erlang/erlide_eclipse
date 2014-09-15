
-record(model, {forms, comments}).

-type model() :: #model{}.

-type pos() :: {{Line::integer(), LastLine::integer(), Offset::integer()}, PosLength::integer()}.

-record(function, {pos::pos(), name::atom(), arity::integer(), args, head, clauses, name_pos, exported}).
-record(clause, {pos::pos(), name::atom(), args, head, name_pos}).
-record(attribute, {pos::pos(), name::atom(), args, extra, arity}).
-record(other, {pos::pos(), name::atom(), tokens}).
