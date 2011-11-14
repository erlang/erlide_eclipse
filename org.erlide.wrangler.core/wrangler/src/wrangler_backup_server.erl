%% Copyright (c) 2010, Huiqing Li, Simon Thompson
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%     %% Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%     %% Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%     %% Neither the name of the copyright holders nor the
%%       names of its contributors may be used to endorse or promote products
%%       derived from this software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ''AS IS''
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
%% BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR 
%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF 
%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%-------------------------------------------------------------------
%%% File    : wrangler_undo_server.erl
%%% Author  :  <Huiqing>
%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%%%------------------------------------------------------------------- 
%%@private
-module(wrangler_backup_server).

-behaviour(gen_server).

%% API
-export([start_backup_server/0,
         add_to_backups/1, 
         add_atomic_cr_start_point/1,
         recover_backups/0,
         rollback_atomic_cr/1,
         reset_backups/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("../include/wrangler_internal.hrl").

-record(state, {backups=[],
                preview_pairs=[]}).

%%====================================================================
%% API
%%====================================================================
start_backup_server() ->
    process_flag(trap_exit, true),
    gen_server:start_link({local, wrangler_backup_server}, ?MODULE, [], []).

add_to_backups(Files) ->
    gen_server:cast(wrangler_backup_server, {add_backups, Files}).

%% recover backups for the whole composite refactoring.
recover_backups() ->
    gen_server:call(wrangler_backup_server, recover_backups).

%% recover backups for an atomic composite refactoring.
rollback_atomic_cr(Pid) ->
    gen_server:call(wrangler_backup_server, {rollback_atomic_cr, Pid}).

reset_backups()->
    gen_server:cast(wrangler_backup_server, reset_backups).

add_atomic_cr_start_point(Pid)->
    gen_server:cast(wrangler_backup_server, {atomic_cr_starts, Pid}).
%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(recover_backups, _From, _State=#state{backups=BackUps})->
    PreviewPairs = do_recover_backups(BackUps),
    {reply, {ok, PreviewPairs}, #state{}};
handle_call({rollback_atomic_cr, Pid}, _From, State=#state{backups=BackUps}) ->
    case lists:member(Pid, BackUps) of
        false ->
            {reply, {ok, []}, State};
        true->
            {BackUps1, BackUps2} = lists:splitwith(fun(E)-> E/=Pid end, BackUps),
            lists:foreach(fun({{FileName, NewFileName, IsNew}, Content})->
                                  case IsNew of 
                                      true -> file:delete(NewFileName);
                                      false ->
                                          file:write_file(FileName, Content)
                                  end;
                             (_)  -> ok
                          end, BackUps1),
             {reply, {ok, []}, State#state{backups=tl(BackUps2)}}
     end.


handle_cast(reset_backups, _State) ->
     {noreply, #state{}};
handle_cast({add_backups,Files},State=#state{backups=BackUps}) ->
     NewBackUps = update_backups(Files, BackUps),
     {noreply, State#state{backups=NewBackUps}};
handle_cast({atomic_cr_starts, Pid}, State=#state{backups=BackUps})->
     {noreply, State#state{backups=[Pid|BackUps]}}.


update_backups([], BackUps) ->
    BackUps;
update_backups([{FileName, NewFileName}|Fs], BackUps) ->
    update_backups([{FileName, NewFileName, false}|Fs], BackUps);
update_backups([{FileName, NewFileName, true}|Fs], BackUps) ->
    update_backups(Fs, [{{FileName, NewFileName, true}, none}|BackUps]);
update_backups([{FileName, NewFileName, false}|Fs], BackUps) ->
     {ok, Content} = file:read_file(FileName),
     update_backups(Fs, [{{FileName, NewFileName, false}, Content}|BackUps]).

do_recover_backups(BackUps) ->
    do_recover_backups(lists:reverse(BackUps), [], []).
do_recover_backups([], PreviewPairs, _RecoverdFiles) ->
     PreviewPairs;
do_recover_backups([Pid|Fs], PreviewPairs, RecoveredFiles) when is_pid(Pid) ->
    do_recover_backups(Fs, PreviewPairs, RecoveredFiles);
do_recover_backups([{{FileName, NewFileName, false}, Content}|Fs], PreviewPairs, RecoveredFiles) ->
    SwpFileName = filename:join([filename:rootname(NewFileName) ++ ".erl.swp"]),
    case filelib:is_regular(SwpFileName) of 
        true -> 
            %% make sure only the lastest version is copied to the .swp file.
            ok;
        _ ->
            file:copy(FileName, SwpFileName) 
    end,
    %% make sure only the oldest version is copied to the file.
    NewRecoveredFiles=case lists:member(FileName, RecoveredFiles) of 
                          true ->
                              RecoveredFiles;
                          false ->
                              file:write_file(FileName, Content),
                              [FileName|RecoveredFiles]
                      end,
    NewPreviewPairs=update_preview_pairs({FileName, NewFileName, SwpFileName}, PreviewPairs),
    do_recover_backups(Fs, NewPreviewPairs, NewRecoveredFiles);  
do_recover_backups([{{FileName, NewFileName, true}, _Content}|Fs], PreviewPairs,RecoveredFiles) ->
    SwpFileName = filename:join([filename:rootname(NewFileName) ++ ".erl.swp"]),
    file:copy(FileName, SwpFileName),
    NewPreviewPairs=[{{FileName, NewFileName, true}, SwpFileName}|PreviewPairs],
    do_recover_backups(Fs, NewPreviewPairs,[FileName| RecoveredFiles]).

update_preview_pairs({FileName, NewFileName, SwpFileName},PreviewPairs) ->
    case [FName||{{_F, FName, _IsNew},_Swp}<-PreviewPairs, FName==FileName] of 
        [] -> [{{FileName, NewFileName, false}, SwpFileName}|PreviewPairs];
        _ ->
            lists:map(fun(Pair) ->
                              case Pair of
                                  {{F1, FileName, IsNew}, _Swp} ->
                                      {{F1, NewFileName, IsNew}, SwpFileName};
                                  _ ->
                                      Pair
                              end
                      end, PreviewPairs)
    end.
   
%%-------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

