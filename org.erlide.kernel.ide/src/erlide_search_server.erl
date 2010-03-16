%% Author: jakob (jakobce at g mail dot com)
%% Created: 10 mar 2010
%% Description: 

-module(erlide_search_server).

%%
%% Include files
%%

-define(DEBUG, 1).

-include("erlide.hrl").
-include("erlide_scanner.hrl").

-include_lib("kernel/include/file.hrl").

%%
%% Exported Functions
%%

-export([start/0, 
         stop/0,
         add_modules/1,
         find_refs/1,
         find_refs/3]).

%%
%% Internal Exports
%%

-export([loop/1]).

%%
%% Macros and Records
%%

-define(SERVER, erlide_search_server).
-record(state, {modules=[], refs=[]}). %% FIXME overly simple data model
-record(ref, {module, line, kind, data, function, clause, has_clauses}).
-record(module, {path, mtime}).

%%
%% API Functions
%%

start() ->
    start(whereis(?SERVER)).

stop() ->
    server_cmd(stop).

add_modules(Modules) ->
    R = server_cmd(add_modules, Modules),
    S = server_cmd(state),
    ?D(S),
    R.

state() ->
    server_cmd(state).

find_refs(Ref) ->
    ?D(Ref),
    R = server_cmd(find_refs, Ref),
    ?D(R),
    R.

find_refs(M, F, A) ->
    find_refs({call, M, F, A}).

%%
%% Local Functions
%%

start(undefined) ->
    Self = self(),
    spawn(fun() ->
                  ?SAVE_CALLS,
                  erlang:yield(),
                  erlang:register(?SERVER, self()),
                  Self ! started,
                  loop(#state{})
          end),
    receive
        started ->
            ok
        after 10000 ->
            {error, timeout_waiting_for_search_server}
    end;
start(_) ->
    ok.

server_cmd(Command) ->
    server_cmd(Command, []).

server_cmd(Command, Args) ->
    start(),
    try
        ?SERVER ! {Command, self(), Args},
        receive
            {Command, _Pid, Result} ->
                Result
        end
    catch _:Exception ->
              {error, Exception}
    end.


loop(State) ->
    receive
        {stop, From, []} ->
            reply(stop, From, stopped);
        {Cmd, From, Args} ->
            NewState = cmd(Cmd, From, Args, State),
            ?MODULE:loop(NewState)
    end.

cmd(Cmd, From, Args, State) ->
    try
        case get(logging) of
            on ->
                put(log, get(log)++[{Cmd, Args}]);
            _ ->
                ok
        end,
        case do_cmd(Cmd, Args, State) of
            {R, NewState} ->
                reply(Cmd, From, R),
                NewState;
            ok ->
                reply(Cmd, From, ok),
                State;
            NewState ->
                reply(Cmd, From, ok),
                NewState
        end
    catch
        exit:Error ->
            reply(Cmd, From, {exit, Error}),
            State;
        error:Error ->
            reply(Cmd, From, {error, Error}),
            State
    end.

reply(Cmd, From, R) ->
    From ! {Cmd, self(), R}.

do_cmd(add_modules, Modules, State) ->
    ?D({add_modules, Modules}),
    do_add_modules(Modules, State);
do_cmd(find_refs, Ref, State) ->
    do_find_refs(Ref, State);
do_cmd(state, _, State) ->
    {State, State}.

do_find_refs(Data, State) ->
    Found = find_data(State#state.refs, Data, []),
    {{ok, Found}, State}.

find_data([], _, Acc) ->
    Acc;
find_data([#ref{module=M, line=L, function=F, clause=C, data=D, has_clauses=H} | Rest], 
          Data, Acc) ->
    case D of
        Data ->
            find_data(Rest, Data, [{M, L-1, F, format_clause(C), H} | Acc]);
        _ ->
            find_data(Rest, Data, Acc)
    end.

format_clause([], Acc) ->
    lists:flatten([Acc, ")"]);
format_clause([Arg], Acc) ->
    lists:flatten([Acc, Arg, ")"]);
format_clause([Arg | Rest], Acc) ->
    format_clause(Rest, [Acc, Arg, ", "]).

format_clause(Args) when is_list(Args) ->
    format_clause(Args, "(");
format_clause(Else) ->
    Else.

do_add_modules([], State) ->
    ?D(State),
    State;
do_add_modules([Filename | Rest], State = #state{refs = Refs, modules = Modules}) ->
    ?D(Filename),
    {Module, MoreRefs} = module_refs(Filename),
    ?D({Module, MoreRefs}),
    do_add_modules(Rest, State#state{refs = MoreRefs ++ Refs, modules = [Module | Modules]}).

get_node_kind_and_line_no({tree, Kind, {attr, Line, _, _}, _}) ->
    {Kind, Line}.

node_to_data({tree, application, _, _} = Node) ->
    case erl_syntax_lib:analyze_application(Node) of
        {M, {F, A}} -> {call, M, F, A};
        {F, A} -> {call, F, A}
    end;
node_to_data({tree, record_expr, _, _} = Node) ->
    {record_expr, {R, _}} = erl_syntax_lib:analyze_record_expr(Node),
    {record, R};
node_to_data({tree, macro, _, Macro}) ->
    {macro, {tree, _Kind, _Attrs, Value}, _} = Macro,
    {macro, Value};
node_to_data({tree, function, _, _} = Node) ->
    F = erl_syntax_lib:analyze_function(Node),
    {function, F};
node_to_data({tree, clause, _, {clause, Args, _, _Body}}) ->
    C = [erl_prettypr:format(Arg) || Arg <- Args],
    {clause, C};
node_to_data(_) ->
    false.

%node_to_ref(Module, {_, Kind, _, _} = Node, {CurFunc, Refs} = Acc) ->
node_to_ref(Module, Node, {CurFunc, CurClause, HasClauses, Refs} = Acc) ->
    case node_to_data(Node) of
        false ->
            Acc;
        {function, F} ->
            {_, _, _, Func} = Node,
            {_, _, Clauses} = Func,
            ?D(Clauses),
            {F, CurClause, length(Clauses) > 1, Refs};
        {clause, C} ->
            ?D(C),
            {CurFunc, C, HasClauses, Refs};
        Data ->
            ?D(Data),
            {Kind, Line} = get_node_kind_and_line_no(Node),
            Ref = #ref{module=Module, line=Line, kind=Kind, 
                       data=Data, function=CurFunc, clause=CurClause,
                       has_clauses=HasClauses},
            {CurFunc, CurClause, HasClauses, [Ref | Refs]}
    end.

fold_nodes(F, Acc0, Nodes) when is_list(Nodes) ->
    lists:foldl(fun(Node, Acc) ->
                        fold_nodes(F, Acc, Node)
                end, Acc0, Nodes);
fold_nodes(F, Acc0, Node) when is_tuple(Node) ->
    Acc1 = F(Node, Acc0),
    case erl_syntax:subtrees(Node) of
        [] ->
            Acc1;
        Gs ->
            fold_nodes(F, Acc1, Gs)
    end.

module_refs(Filename) when is_list(Filename) ->
    {ok, FileInfo} = file:read_file_info(Filename),
    {ok, Forms} = epp_dodger:parse_file(Filename),
    Module = #module{path=Filename, mtime=FileInfo#file_info.mtime},
    {_, _, _, MoreRefs} = fold_nodes(fun(Node, Acc) ->
                                          node_to_ref(Module, Node, Acc) 
                                  end, {none, none, false, []}, Forms),
    {Module, MoreRefs}.




