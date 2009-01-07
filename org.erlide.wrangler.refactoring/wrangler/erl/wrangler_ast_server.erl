%%%-------------------------------------------------------------------
%%% File    : wrangler_ast_server.erl
%%% Author  :  <Huiqing Li>
%%% Description : A gen server manageing the ASTs of the program under refactoring.
%%%
%%% Created : 28 Aug 2008 by  <Huiqing Li>
%%%-------------------------------------------------------------------
-module(wrangler_ast_server).

-behaviour(gen_server).

-include("../hrl/wrangler.hrl").

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([start_ast_server/0, get_ast/1, update_ast/2]).

-record(state, {asts=[]}).

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
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
-spec(init/1::([dir()]) ->
	      {ok, #state{}}).
init(_Args) ->
    {ok, #state{asts=[]}}.


%%------------------------------------------------------------------
-spec(get_ast/1::({filename(), boolean(), [dir()]}) ->
	     {ok, {syntaxTree(), moduleInfo()}}).
get_ast({FileName, ByPassPreP, SearchPaths}) ->
    gen_server:call(wrangler_ast_server, {get,{FileName, ByPassPreP, SearchPaths}}, 500000).

 
-type(modifyTime()::{{integer(), integer(), integer()},{integer(), integer(), integer()}}).
-spec(update_ast/2::({filename(),boolean(), [dir()]}, {syntaxTree(), moduleInfo(), modifyTime()}) ->
	     ok).
update_ast({FileName, ByPassPreP, SearchPaths}, {AnnAST, Info, Time}) ->
    gen_server:cast(wrangler_ast_server, {update, {{FileName, ByPassPreP, SearchPaths}, {AnnAST, Info, Time}}}).
 
%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

-spec(handle_call/3::({get,{filename(), boolean(), [dir()]}}, any(), #state{}) -> {reply, {ok, {syntaxTree(), moduleInfo()}}, #state{}}).
handle_call({get, Key}, _From, State) ->
    {Reply, State1} = get_ast(Key, State),
    {reply, Reply, State1}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
-spec(handle_cast/2::({update, {{filename(), boolean(), [dir()]}, {syntaxTree(), moduleInfo(), modifyTime()}}}, #state{}) ->
    {noreply, #state{}}).
handle_cast({update, {Key, {AnnAST, Info, Time}}}, State) ->
    State1 = update_ast_1({Key, {AnnAST, Info, Time}}, State),
    {noreply, State1}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
-spec(handle_info/2::(any(), #state{}) ->
	      {noreply, #state{}}).
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
terminate(_Reason, _State) ->
    ok.

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
-spec(get_ast/2::({filename(),boolean(), [dir()]}, #state{}) -> {{ok, {syntaxTree(), moduleInfo()}}, #state{}}).	     
get_ast(Key={ FileName,ByPassPreP, SearchPaths}, State=#state{asts=ASTs}) ->
    case lists:keysearch(Key, 1, ASTs) of
      {value, {Key, {AnnAST, Info, FileModifiedTime}}} ->
	  case FileModifiedTime >= filelib:last_modified(FileName) of
	    true -> 
		  log_errors(FileName, Info), {{ok, {AnnAST, Info}}, State};
	    false ->
		  {ok, {AnnAST1, Info1}} = refac_util:parse_annotate_file_1(FileName, ByPassPreP, SearchPaths),
		  log_errors(FileName, Info),
		  {{ok, {AnnAST1, Info1}}, #state{asts=lists:keyreplace(Key, 1, ASTs, {Key, {AnnAST1, Info1, filelib:last_modified(FileName)}})}}
	  end;
      false ->
	  {ok, {AnnAST, Info}} = refac_util:parse_annotate_file_1(FileName, ByPassPreP, SearchPaths),
	  log_errors(FileName, Info),
	  {{ok, {AnnAST, Info}}, #state{asts=[{Key, {AnnAST, Info, filelib:last_modified(FileName)}} | ASTs]}}
    end.

update_ast_1({Key={_FileName, _ByPassPreP, _SearchPaths}, {AnnAST, Info, Time}}, _State=#state{asts=ASTs}) ->
    case lists:keysearch(Key, 1, ASTs) of
	{value, {Key,  {_AnnAST1, _Info1, _Time}}} ->  
	    #state{asts=lists:keyreplace(Key, 1, ASTs, {Key, {AnnAST, Info, Time}})};
	false ->
	    #state{asts=[{Key, {AnnAST, Info, Time}} | ASTs]}
    end.

log_errors(FileName, Info) ->
    case lists:keysearch(errors, 1, Info) of
      {value, {errors, Error}} ->
	  wrangler_error_logger:add_error_to_logger({FileName, Error});
      false -> ok
    end.



