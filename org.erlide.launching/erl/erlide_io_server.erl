%%% ******************************************************************************
%%%  Copyright (c) 2004 Vlad Dumitrescu and others.
%%%  All rights reserved. This program and the accompanying materials
%%%  are made available under the terms of the Eclipse Public License v1.0
%%%  which accompanies this distribution, and is available at
%%%  http://www.eclipse.org/legal/epl-v10.html
%%%
%%%  Contributors:
%%%      Vlad Dumitrescu
%%% ******************************************************************************/

-module(erlide_io_server).

-export([start/0, add/1, remove/1, loop/2]).

start() ->
    Pid = spawn(fun() -> init() end),
    register(?MODULE, Pid),
    Pid.

add(Pid) ->
    catch ?MODULE ! {add, Pid}.

remove(Pid) ->
    catch ?MODULE ! {remove, Pid}.

init() ->
    stdio_handler:install(?MODULE),
    loop([], []).

loop(Listeners, Queue) ->
    receive
stop ->
    stdio_handler:uninstall(),
    ok;
{add, Pid} ->
    case Listeners of
        [] ->
            lists:foreach(fun(R) ->
                {request, Io, Request, Sender, Time}=R,
                Pid ! {process(Request), Io, Sender, Time} end, Queue),
            ?MODULE:loop([Pid|Listeners], []);
        _ ->
            ?MODULE:loop([Pid|Listeners], Queue)
    end;
{remove, Pid} ->
    ?MODULE:loop(lists:delete(Pid, Listeners), Queue);
status ->
    erlang:display(Listeners),
    ?MODULE:loop(Listeners, Queue);

{request, Io, Request, Sender, Time}=Req ->
    case Listeners of
        [] ->
            ?MODULE:loop(Listeners, [Req|Queue]);
        _->
            lists:foreach(fun(L) -> L ! {process(Request), Io, Sender, Time} end, Listeners),
            ?MODULE:loop(Listeners, [])
    end;

_ ->
    ?MODULE:loop(Listeners, Queue)

end.

process({format, Fmt, Args}) ->
    lists:flatten(io_lib:format(Fmt, Args));
process(nl) ->
    "\n";
process({put_chars, Bin}) when is_binary(Bin) ->
    binary_to_list(Bin);
        process({put_chars, Str}) ->
            lists:flatten(Str);
        process({put_chars, M, F, A}) ->
            lists:flatten(apply(M,F,A));
        process({get_until, {format, Fmt, Args}, _M, _F, _Xarg}) ->
            lists:flatten(io_lib:format(Fmt, Args));
        process(Req) ->
            Req.


