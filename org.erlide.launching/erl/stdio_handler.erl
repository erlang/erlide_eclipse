-module(stdio_handler).

-export([install/1,
         uninstall/0,
         is_traceable/1]).

%% IMPORTANT! changes in the implementation of io:request/2 might
%%            require reviewing this implementation!

%% IMPORTANT! only one handle can be installed (previous are silently dropped)

is_traceable(Pid) ->
    case node(Pid)==node() of
        false ->
            false;
        true ->
            case lists:member(process_info(Pid, registered_name), [erlide_io_server]) of
                true ->
                    false;
                false ->
                    case process_info(Pid, initial_call) of
                        {_, {user, _, _}} ->
                            true;
                        {_, {erlide_reshd, _, _}} ->
                            true;
                        _ ->
                            {_, GL} = process_info(Pid, group_leader),
                            case process_info(GL, initial_call) of
                                {_, {group, _, _}} ->
                                    true;
                                _ ->
                                    false
                            end
                    end
            end
    end.

%% we change the second clause of io:request/2 from
%%
%% request(Pid, Request) when pid(Pid) ->
%%     Mref = erlang:monitor(process,Pid),
%%     Pid ! {io_request,self(),Pid,io_request(Pid, Request)},
%%     wait_io_mon_reply(Pid,Mref);
%%
%% to
%%
%% request(Pid, Request) when pid(Pid) ->
%%     case stdio_handler:is_traceable(Pid) of
%%         true ->
%%             catch ~Handler ! {request, Pid, Request, self(), erlang:now()};
%%         _ ->
%%             ok
%%     end,
%%     Mref = erlang:monitor(process,Pid),
%%     Pid ! {io_request,self(),Pid,io_request(Pid, Request)},
%%     wait_io_mon_reply(Pid,Mref);
%%     %Reply = wait_io_mon_reply(Pid,Mref),
%%     %case stdio_handler:is_traceable(Pid) of
%%     %    true ->
%%     %        catch ~Handler ! {reply, Reply, self(), erlang:now()};
%%     %    _ ->
%%     %        ok
%%     %end,
%%     %Reply;

install(Handler) when is_atom(Handler) ->
    uninstall(),

    {ok, M} = smerl:for_module(io),
    {ok, F} = smerl:get_func(M, request, 2),

    [H,Clause|T] = element(5, F),
    Body0 = element(5, Clause),
    [Last|Body] = lists:reverse(Body0),
    L = element(2, hd(Body0)),

    Call = {'catch', L,
            {op,L,'!',
             erl_parse:abstract(Handler),
             {tuple, L,
              [{atom,L,request},
               {var,L,'Pid'},
               {call,L,{atom, L, io_request}, [{var, L, 'Pid'}, {var,L,'Request'}]},
               {call,L,{atom,L,self},[]},
               {call,L,
                {remote,L,
                 {atom,L,erlang},
                 {atom,L,now}},
                []}]}}},

    Case = {'case', L,
            {call,L,
             {remote,L,{atom,L,stdio_handler},{atom,L,is_traceable}},
             [{var,L,'Pid'}]},
            [{clause,L,
              [{atom,L,true}],
              [],
              [Call]},
             {clause,L,
              [{var,L,'_'}],
              [],
              [{atom,L,ok}]}]},

    %%     L2 = element(2, Last),
    %%
    %%     Call2 = {'catch', L2,
    %%       {op,L2,'!',
    %%        erl_parse:abstract(Handler),
    %%        {tuple, L2,
    %%         [{atom,L2,reply},
    %%          {var,L2,'Pid'},
    %%          {var,L2,'Reply'},
    %%          {call,L2,{atom,L,self},[]},
    %%          {call,L2,
    %%     {remote,L2,
    %%      {atom,L2,erlang},
    %%      {atom,L2,now}},
    %%     []}]}}},
    %%
    %%     Case2 = {'case', L2,
    %%       {call,L2,
    %%        {remote,L2,{atom,L,stdio_handler},{atom,L,is_traceable}},
    %%        [{var,L2,'Pid'}]},
    %%       [{clause,L2,
    %%         [{atom,L2,true}],
    %%         [],
    %%         [Call2]},
    %%        {clause,L2,
    %%         [{var,L2,'_'}],
    %%         [],
    %%         [{atom,L2,ok}]}]},
    %%
    %%   Last2 = {match, L2, {var, L2, 'Reply'}, Last},
    %%
    %%NewBody = [Case|lists:reverse([{var, L2, 'Reply'},Case2, Last2|Body])],

    NewBody = [Case|lists:reverse([Last|Body])],
    NewClause = setelement(5, Clause, NewBody),
    NewFun = setelement(5, F, [H,NewClause|T]),

    %%io:format(lists:flatten(erl_pp:function(NewFun))),

    {ok, NewM} = smerl:replace_func(M, NewFun),
    {ok, NewM} = smerl:replace_func(M, NewFun),

    code:unstick_mod(io),
    smerl:compile(NewM),
    code:stick_mod(io),
    ok.

uninstall() ->
    code:unstick_mod(io),
    c:l(io),
    code:stick_mod(io),
    ok.


