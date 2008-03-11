-define(Debug(T), erlide_log:erlangLog(?MODULE, ?LINE, debug, T)).
-define(Info(T), erlide_log:erlangLog(?MODULE, ?LINE, info, T)).

-ifdef(DEBUG).
-compile(export_all).
%-define(D(T), erlang:display({?MODULE, ?LINE, T})).
%-define(D(T), io:format("~p\n", [{?MODULE, ?LINE, T}])).
-define(D(T), ?Debug(T)).
-else.
-define(D(T), ok).
-endif.
