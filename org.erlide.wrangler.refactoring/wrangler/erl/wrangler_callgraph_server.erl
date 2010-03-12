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
%%% File    : wrangler_callgraph_server.erl
%%% Description : A gen server manageing the callgraph of the program under refactoring.
%%% Created : 28 Aug 2008 by  <Huiqing>
%%%-------------------------------------------------------------------
-module(wrangler_callgraph_server).

-behaviour(gen_server).

-include("../include/wrangler.hrl").

%% API
-export([start_callgraph_server/0, get_callgraph/1, get_sccs_including_fun/2,
	 build_scc_callgraph/1,build_callercallee_callgraph/1, called_funs/1,
	 get_sorted_funs/2, fun_callgraph_to_dot/1, fun_callgraph_to_dot/2,
	 fun_callgraph_to_png/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {callgraph=[]}).
%%====================================================================
%% API
%%====================================================================

-spec(start_callgraph_server/0::() -> {ok, pid()} | ignore | {error, string()}).
start_callgraph_server() ->
    gen_server:start_link({local, wrangler_callgraph_server}, ?MODULE, [], []).

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
-spec(init/1::(any()) ->{ok, #state{}}).
init(_Args) ->
    process_flag(trap_exit, true),
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
-spec(handle_call/3::({atom(), [dir()]}, any(), #state{}) ->
	     {reply, #callgraph{}, #state{}}).
handle_call({get, SearchPaths}, _From, State) ->
    {Reply, State1} = get_callgraph(SearchPaths, State),
    {reply, Reply, State1};
handle_call({get_fun_sccs, MFA, SearchPaths}, _From, State) ->
    {Reply, State1} = get_sccs_including_fun(MFA, SearchPaths, State),
    {reply, Reply, State1}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
-spec(handle_cast/2::(any(), #state{}) ->
	      {noreply, #state{}}).
handle_cast(_Msg, State) ->
    {noreply, State}.

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
%-spec(terminate/2::(any(), #state{}) -> ok).
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


-spec(get_callgraph/1::([dir()])-> #callgraph{}).
get_callgraph(SearchPaths) ->
    gen_server:call(wrangler_callgraph_server, {get, SearchPaths}, infinity).

%%-spec(get_sccs_including_fun/2::({modulename(),functionname(), functionarity()}, [dir()]) -> scc_order()).
get_sccs_including_fun({M, F, A}, SearchPaths) ->    
    gen_server:call(wrangler_callgraph_server, {get_fun_sccs, {M,F, A}, SearchPaths}).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% Currently the State is not used because  it takes too much space;
%% Todo: find out a way to improve this.
get_callgraph(SearchPaths, State) ->
    CallGraph = build_scc_callgraph(SearchPaths),
    {CallGraph, State}.


%% return the sccs, of which {M, F, A} is a member.
get_sccs_including_fun({M, F, A}, SearchPaths, State) ->
    {#callgraph{scc_order = Sccs}, State1} = get_callgraph(SearchPaths, State),
    ResSccs = [Sc||Sc<-Sccs, lists:keymember({M, F, A}, 1, Sc)],
    {ResSccs, State1}.
   

    
-spec(build_scc_callgraph(DirList::[dir()]) -> #callgraph{}).
build_scc_callgraph(DirList) ->
    CallerCalleesWithDef = build_callercallee_callgraph(DirList),
    CallerCallees = lists:map(fun ({{Caller, _CallerDef}, Callee}) ->
				      {Caller, Callee} 
			      end, CallerCalleesWithDef),
    {Sccs, E} = refac_callgraph:construct(CallerCalleesWithDef),
    #callgraph{callercallee = CallerCallees, scc_order = Sccs, external_calls = E}.
    
   

-spec(build_callercallee_callgraph/1::([dir()]) -> 
	     [{{{atom(), atom(), integer()}, syntaxTree()}, [{atom(), atom(), integer()}]}]).
build_callercallee_callgraph(DirList) ->
    Files = refac_util:expand_files(DirList, ".erl"),
    lists:flatmap(fun(FName) ->do_build_callgraph(FName, DirList)
		  end, Files).
    
   
-spec(do_build_callgraph/2::(filename(), [dir()]) -> 
	     [{{{atom(), atom(), integer()}, syntaxTree()}, [{atom(), atom(), integer()}]}]).
do_build_callgraph(FName, DirList) ->
    {ok, {AnnAST, Info}} = refac_util:parse_annotate_file(FName, true, DirList),
    {value, {module, ModName}} = lists:keysearch(module, 1, Info),
    F1 = fun (T, S) ->
		 case refac_syntax:type(T) of
		     function ->
			 FunName = refac_syntax:data(refac_syntax:function_name(T)),
			 Arity = refac_syntax:function_arity(T),
			 Caller ={{ModName, FunName, Arity}, T},   %% should remove the actual function AST.!!!
			 CalledFuns = called_funs(T),
			 ordsets:add_element({Caller, CalledFuns}, S);
		     _ -> S
		 end
	 end,
    lists:usort(refac_syntax_lib:fold(F1, ordsets:new(), AnnAST)).

-spec(called_funs/1::(syntaxTree()) ->
	     [{modulename(), functionname(), functionarity()}]).
called_funs(Tree) ->
    Fun = fun (T, S) ->
		case refac_syntax:type(T) of
		  application ->
			Op = refac_syntax:application_operator(T),
			case lists:keysearch(fun_def, 1, refac_syntax:get_ann(Op)) of
			    {value, {fun_def, {M, F, A, _, _}}} 
			    when M =/= '_' andalso F =/= '_' ->
				ordsets:add_element({M, F, A}, S);
			    _ -> S
			end;
		    implicit_fun ->
			case lists:keysearch(fun_def, 1, refac_syntax:get_ann(T)) of
			    {value, {fun_def, {M, F, A, _, _}}} 
			    when M =/= '_' andalso F =/= '_' ->
				ordsets:add_element({M, F, A}, S);
			    _ -> S
			end;
		    _ -> S
		end
	end,
    refac_syntax_lib:fold(Fun, ordsets:new(), Tree).

    
get_sorted_funs(ModName, AnnAST) ->
    F1 = fun (T, S) ->
		 case refac_syntax:type(T) of
		   function ->
		       FunName = refac_syntax:data(refac_syntax:function_name(T)),
		       Arity = refac_syntax:function_arity(T),
		       Caller = {{ModName, FunName, Arity}, T},
		       CalledFuns = called_funs(T),
		       ordsets:add_element({Caller, CalledFuns}, S);
		   _ -> S
		 end
	 end,
    CallerCallees = lists:usort(refac_syntax_lib:fold(F1, ordsets:new(), AnnAST)),
    {Sccs, _E} = refac_callgraph:construct(CallerCallees),
    lists:append(Sccs).

-spec(fun_callgraph_to_png/1::([filename()|dir()]) -> ok).
fun_callgraph_to_png(FileNameDirs) ->
    Files = refac_util:expand_files(FileNameDirs, ".erl"),
    lists:foreach(fun(FName)->
			  refac_io:format("currentfile:\n~p\n", [FName]),
			  BaseName = filename:basename(FName, ".erl"),
			  DotFileName=BaseName++"_callgraph.dot",
			  PngFileName =BaseName++"_callgraph.png",
			  fun_callgraph_to_dot(DotFileName, FName),
			  os:cmd("dot -Tpng "++DotFileName++" > "++PngFileName)			  
		  end, Files).

-spec(fun_callgraph_to_dot/1::([filename()|dir()]) -> ok).
fun_callgraph_to_dot(FileNameDirs) ->
    Files = refac_util:expand_files(FileNameDirs, ".erl"),
    lists:foreach(fun(FName)->
			  refac_io:format("currentfile:\n~p\n", [FName]),
			  BaseName = filename:basename(FName, ".erl"),
			  DotFileName=BaseName++"_callgraph.dot",
			  fun_callgraph_to_dot(DotFileName, FName)
		  end, Files).

-spec(fun_callgraph_to_dot/2::(filename(), filename()) -> true).
fun_callgraph_to_dot(DotFile, FileName) ->
    CallerCalleesWithDef = build_callercallee_callgraph([FileName]),
    CallerCallees = [{{M,F,A}, [{M1,F1,A1}||{M1,F1,A1}<-Callee, M1==M]}
		     || {{{M,F,A}, _}, Callee}<-CallerCalleesWithDef],
    CallerCallees1=[{Caller, Callees}||{Caller, Callees}<-CallerCallees, 
				       Callees/=[]],
    MG = digraph:new(),
    add_edges(CallerCallees1,  MG),
    to_dot(MG,DotFile),
    digraph:delete(MG).

add_edges([], MG) ->
    MG;
add_edges([{Caller, Callees}|Left], MG) ->
    Edges =[{Caller, Callee} ||  Callee <- Callees],
    add_edges(Left, digraph_add_edges(Edges, MG)).

digraph_add_edges([], MG)-> 
    MG;
digraph_add_edges([{From, To}|Left],  MG) ->
    digraph_add_edges(Left,digraph_add_edge(From, To, MG)).   
    
    
digraph_add_edge(From, To,  MG) ->
    case digraph:vertex(MG, From) of 
	false ->
	    digraph:add_vertex(MG, From);
	{From, _} ->
	    ok
    end,
    case digraph:vertex(MG, To) of 
	false ->
	    digraph:add_vertex(MG, To);
	{To,_} -> ok
    end,
    digraph:add_edge(MG, From, To),
    MG.

to_dot(MG, File) ->
    Edges = [digraph:edge(MG, X) || X <- digraph:edges(MG)],
    EdgeList=[{X, Y} || {_, X, Y, _} <- Edges],
    edge_list_to_dot(EdgeList, File, "CallGraph").
    					
    
edge_list_to_dot(Edges, OutFileName, GraphName) ->
    {NodeList1, NodeList2} = lists:unzip(Edges),
    NodeList = NodeList1 ++ NodeList2,
    NodeSet = ordsets:from_list(NodeList),
    Start = ["digraph ",GraphName ," {"],
    VertexList = [node_format(V) ||V <- NodeSet],
    End = ["graph [", GraphName, "=", GraphName, "]}"],
    EdgeList = [edge_format(X, Y) || {X,Y} <- Edges],
    String = [Start, VertexList, EdgeList, End],
    ok = file:write_file(OutFileName, list_to_binary(String)).

node_format(V) ->
    {_M, F, A} = V,
    String = io_lib:format("~p/~p", [F,A]),
    {Width, Heigth} = calc_dim(String),
    W = (Width div 7 + 1) * 0.55,
    H = Heigth * 0.4,
    SL = io_lib:format("~f", [W]),
    SH = io_lib:format("~f", [H]),
    ["\"", String, "\"", " [width=", SL, " heigth=", SH, " ", "", "];\n"].

calc_dim(String) ->
  calc_dim(String, 1, 0, 0).

calc_dim("\\n" ++ T, H, TmpW, MaxW) ->
  calc_dim(T, H+1, 0, erlang:max(TmpW, MaxW));
calc_dim([_|T], H, TmpW, MaxW) ->
  calc_dim(T, H, TmpW+1, MaxW);
calc_dim([], H, TmpW, MaxW) ->
  {erlang:max(TmpW, MaxW), H}.


edge_format(V1, V2) ->
    {_,F1,A1} =V1, 
    {_,F2,A2} =V2,
    String = ["\"",io_lib:format("~p/~p", [F1,A1]),"\"", " -> ",
	      "\"", io_lib:format("~p/~p", [F2, A2]), "\""],
    [String, "[", "];\n"].
    
 
