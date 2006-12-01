
-ifdef(DEBUG).
-compile(export_all).
-define(D(T), erlang:display({?MODULE, ?LINE, T})).
-else.
-define(D(T), ok).
-endif.
