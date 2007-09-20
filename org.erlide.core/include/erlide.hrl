
-ifdef(DEBUG).
-compile(export_all).
%-define(D(T), erlang:display({?MODULE, ?LINE, T})).
-define(D(T), io:format("~p\n", [{?MODULE, ?LINE, T}])).
-else.
-define(D(T), ok).
-endif.
