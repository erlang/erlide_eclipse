%%%-------------------------------------------------------------------
%%% File    : wrangler_ast_server.erl
%%% Author  :  <Huiqing Li>
%%% Description : A gen server manageing the ASTs of the program under refactoring.
%%%
%%% Created : 28 Aug 2008 by  <Huiqing Li>
%%%-------------------------------------------------------------------
-module(wrangler_ast_server).

-behaviour(gen_server).

-include("../include/wrangler.hrl").

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([start_ast_server/0, get_ast/1, update_ast/2]).

-record(state, {dets_tab=none, asts=[]}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
-spec(start_ast_server() ->
	     {ok, pid()} | ignore | {error, string()}).
start_ast_server() ->
    gen_server:start_link({local, wrangler_ast_server}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |t
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
-spec(init/1::([dir()]) ->
	      {ok, #state{}}).
init(_Args) ->
    process_flag(trap_exit, true),
    case file:get_cwd() of
	{ok, Dir} ->
	    TabDir = filename:join(Dir,"temp"),
	    FileName=filename:join(TabDir, "wrangler_dets"),
	    DetsTab = FileName,  %%  list_to_atom(FileName),
	    file:delete(FileName),
	    case file:make_dir(TabDir) of 
		ok ->
		    case dets:open_file(DetsTab,  [{type, set}, {access, read_write}, {repair, false}]) of 
			{ok, _Name} ->  {ok, #state{dets_tab=DetsTab}};
			_  ->  {ok, #state{dets_tab=none}}
		    end;
		{error, eexist} ->
		    case dets:open_file(DetsTab,  [{type, set}, {access, read_write}, {repair, false}]) of 
			{ok, _Name} ->  {ok, #state{dets_tab=DetsTab}};
			_  ->  {ok, #state{dets_tab=none}}
		    end;
		_Others -> {ok, #state{dets_tab=none}}
	    end;
	{error, _} -> {ok, #state{dets_tab=none}}
    end.
  
%%------------------------------------------------------------------
-spec(get_ast/1::({filename(), boolean(), [dir()], integer(), atom()}) ->
	     {ok, {syntaxTree(), moduleInfo()}}).
get_ast(Key={_FileName, _ByPassPreP, _SearchPaths, _TabWidth, _FileFormat}) ->
    gen_server:call(wrangler_ast_server, {get,Key}, 500000).

 
-type(modifyTime()::{{integer(), integer(), integer()},{integer(), integer(), integer()}}).
-spec(update_ast/2::({filename(),boolean(), [dir()], integer(), atom()}, {syntaxTree(), moduleInfo(), modifyTime()}) -> ok).
update_ast(Key={_FileName, _ByPassPreP, _SearchPaths, _TabWidth, _FileFormat}, {AnnAST, Info, Time}) ->
    gen_server:cast(wrangler_ast_server, {update, {Key, {AnnAST, Info, Time}}}).
 
%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

-spec(handle_call/3::({get,{filename(), boolean(), [dir()], integer(), atom()}}, any(), #state{}) -> {reply, {ok, {syntaxTree(), moduleInfo()}}, #state{}}).
handle_call({get, Key}, _From, State) ->
    {Reply, State1} = get_ast(Key, State),
    {reply, Reply, State1}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
-spec(handle_cast/2::({update, {{filename(), boolean(), [dir()], integer()}, {syntaxTree(), moduleInfo(), modifyTime()}}}, #state{}) ->
    {noreply, #state{}}).
handle_cast({update, {Key, {AnnAST, Info, Time}}}, State) ->
    update_ast_1({Key, {AnnAST, Info, Time}}, State),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
-spec(handle_info/2::(any(), #state{}) -> {noreply, #state{}}).
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
-spec(terminate/2::(any(), #state{}) -> ok).
terminate(_Reason, _State=#state{dets_tab=TabFile}) ->
    dets:close(TabFile),
    file:delete(TabFile).

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
-spec(code_change/3::(any(), #state{}, any()) ->
	      {ok, #state{}}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
-spec(get_ast/2::({filename(),boolean(), [dir()], integer(), atom()}, #state{}) -> {{ok, {syntaxTree(), moduleInfo()}}, #state{}}).      

get_ast({FileName, false, SearchPaths, TabWidth, FileFormat}, State) -> %% always re-parse; otherwise need to check the change time of .hrl files.
    {ok, {AnnAST, Info}} = refac_util:parse_annotate_file_1(FileName, false, SearchPaths, TabWidth, FileFormat),
    log_errors(FileName, Info),
    {{ok, {AnnAST, Info}}, State};	
get_ast(Key={FileName,ByPassPreP, SearchPaths, TabWidth, FileFormat}, State=#state{dets_tab=TabFile, asts=ASTs}) ->
    case TabFile of 
	none -> 
	    case lists:keysearch(Key, 1, ASTs) of
		{value, {Key, {AnnAST, Info, FileModifiedTime}}} ->
		    case FileModifiedTime >= filelib:last_modified(FileName) of
			true -> 
			    log_errors(FileName, Info), {{ok, {AnnAST, Info}}, State};
			false ->
			    {ok, {AnnAST1, Info1}} = refac_util:parse_annotate_file_1(FileName, ByPassPreP, SearchPaths, TabWidth, FileFormat),
			    log_errors(FileName, Info1),
			    {{ok, {AnnAST1, Info1}}, #state{asts=lists:keyreplace(Key, 1, ASTs, {Key, {AnnAST1, Info1, filelib:last_modified(FileName)}})}}
		    end;
		false ->
		    {ok, {AnnAST, Info}} = refac_util:parse_annotate_file_1(FileName, ByPassPreP, SearchPaths, TabWidth, FileFormat),
		    log_errors(FileName, Info),
		    {{ok, {AnnAST, Info}}, #state{asts=[{Key, {AnnAST, Info, filelib:last_modified(FileName)}} | ASTs]}}
	    end;
	_ -> 
	    case dets:lookup(TabFile, Key) of
		[{Key, {AnnAST, Info, FileModifiedTime}}]->
		    case FileModifiedTime >= filelib:last_modified(FileName) of
			true -> 
			    {{ok, {AnnAST, Info}}, State};
			false ->
			    {ok, {AnnAST1, Info1}} = refac_util:parse_annotate_file_1(FileName, ByPassPreP, SearchPaths, TabWidth, FileFormat),
			    dets:insert(TabFile, {Key, {AnnAST1, Info1, filelib:last_modified(FileName)}}),
			    log_errors(FileName, Info1),
			    {{ok, {AnnAST1, Info1}}, State}
		    end;
		_ ->
		    {ok, {AnnAST, Info}} = refac_util:parse_annotate_file_1(FileName, ByPassPreP, SearchPaths, TabWidth, FileFormat),
		    dets:insert(TabFile, {Key, {AnnAST, Info, filelib:last_modified(FileName)}}),
		    log_errors(FileName, Info),
		    {{ok, {AnnAST, Info}}, State}
	    end
    end.

update_ast_1({Key, {AnnAST, Info, Time}}, _State=#state{dets_tab=TabFile, asts=ASTs}) ->
    case TabFile of
	none ->  case lists:keysearch(Key, 1, ASTs) of
		     {value, {Key,  {_AnnAST1, _Info1, _Time}}} ->  
			 #state{asts=lists:keyreplace(Key, 1, ASTs, {Key, {AnnAST, Info, Time}})};
		     false ->
			 #state{asts=[{Key, {AnnAST, Info, Time}} | ASTs]}
		 end;
	_ ->
	    dets:delete(TabFile, Key),
	    dets:insert(TabFile, [{Key, {AnnAST, Info, Time}}])
    end.
    
log_errors(FileName, Info) ->
    case lists:keysearch(errors, 1, Info) of
      {value, {errors, Error}} ->
	  wrangler_error_logger:add_error_to_logger({FileName, Error});
      false -> ok
    end.



