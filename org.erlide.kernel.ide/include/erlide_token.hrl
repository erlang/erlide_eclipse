
-record(token, {kind=u, line=u, offset=u, length=u, value=u, text=u, last_line=u, column=u}).
%% IMPORTANT: changing this means ErlParser needs to be updated
%% new fields can be added at the end

-type token() :: #token{}.
