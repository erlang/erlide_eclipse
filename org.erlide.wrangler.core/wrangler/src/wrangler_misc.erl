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
%% =====================================================================
%% Some utility functions used by Wrangler.
%%
%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%%
%% =====================================================================

%% @private
%% @hidden
-module(wrangler_misc).

-export([ghead/2, glast/2, 
         to_upper/1, to_lower/1,
         remove_duplicates/1,
         rewrite/2, rewrite_with_wrapper/2,
         reset_attrs/1,reset_ann_and_pos/1, 
         reset_ann/1, reset_pos/1,
         reset_pos_and_range/1,
         default_incls/0,update_ann/2,
         delete_from_ann/2, max/2, min/2, 
         spawn_funs/0,is_spawn_app/1, 
         get_start_end_loc_with_comment/1,
         start_end_loc/1,
         file_format/1, expand_files/2, 
         get_modules_by_file/1,
         concat_toks/1, get_toks/1, tokenize/3,
         format_search_paths/1,
         free_vars/1, exported_vars/1, bound_vars/1,
         modname_to_filename/2, funname_to_defpos/2,
         group_by/2,filehash/1,apply_style_funs/0,
         try_eval/4, is_macro_name/1, is_literal/1,
         is_fun_name/1, is_var_name/1]).

-export([callback_funs/1, 
         is_callback_fun/3,
         testserver_callback_funs/0,
         eqc_statem_callback_funs/0,
         eqc_fsm_callback_funs/0,
         commontest_callback_funs/0]).

-export([collect_var_names/1, 
         collect_used_macros/1,
         collect_used_records/1,
         collect_var_source_def_pos_info/1]).

-export([test_framework_used/1]).

-export([parse_annotate_expr/1, parse_annotate_expr/2,
         extended_parse_annotate_expr/1,
         extended_parse_annotate_expr/2]).

-export([extend_function_clause/1]).

-include("../include/wrangler_internal.hrl"). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%-spec group_by(integer(), [tuple()]) -> [[tuple()]].
group_by(N, TupleList) ->
    SortedTupleList = lists:keysort(N, lists:usort(TupleList)),
    group_by(N, SortedTupleList, []).

group_by(_N,[],Acc) -> Acc;
group_by(N,TupleList = [T| _Ts],Acc) ->
    E = element(N,T),
    {TupleList1,TupleList2} = 
	lists:partition(fun (T1) ->
				element(N,T1) == E
			end,
			TupleList),
    group_by(N,TupleList2,Acc ++ [TupleList1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%-spec filehash(filename()) -> integer(). 	      
filehash(FileName) ->
    case file:open(FileName, [read, raw, binary]) of
      {ok, IoDevice} ->
	  Hash = filehash(IoDevice, 0),
	  file:close(IoDevice),
	  Hash;
      _ -> 0
    end.

filehash(IoDevice, Crc) ->
    case file:read(IoDevice, 1024) of
        {ok, Data} ->
            filehash(IoDevice, erlang:crc32(Crc, Data));
        eof ->
            Crc;
        {error, _Reason} ->
            0 %% TODO error handling
    end.


%% This function will be removed.
apply_style_funs() ->
    [{{erlang, apply, 3}, [modulename, functionname, arglist], term},
     {{erlang, spawn, 3}, [modulename, functionname, arglist], term},
     {{erlang, spawn, 4}, [node, modulename, functionname, arglist], term},
     {{erlang, spawn_link, 3}, [modulename, functionname, arglist], term},
     {{erlang, spawn_link, 4}, [term, modulename, functioname, arglist], term},
     {{erlang, spawn_monitor, 3}, [term, modulename, functionname, arglist], term},
     {{test_server, timecall, 3}, [modulename, functionname, arglist], term},
     {{test_server, do_times, 4}, [integer, modulename, functionname, arglist], term},
     {{test_server, call_crash, 3}, [modulename, functionname, arglist], term},
     {{test_server, call_crash, 4}, [term, modulename, functionname, arglist], term},
     {{test_server, call_crash, 5}, [term, term, modulename, functionname, arglist], term}].

 

%%-spec testserver_callback_funs()->[{atom(), integer()}].
testserver_callback_funs() ->
    [{all, 0}, {init_per_suite, 1}, {end_per_suite, 1}, {init_per_testcase, 2}, {fin_per_testcase, 2}].

%%-spec eqc_statem_callback_funs()->[{atom(), integer()}].
eqc_statem_callback_funs() ->
    [{initial_state, 0}, {precondition, 2}, {command, 1}, {postcondition, 3}, {next_state, 3}].

%%-spec eqc_fsm_callback_funs()->[{atom(), integer()}].
eqc_fsm_callback_funs() ->
    [{initial_state, 0}, {initial_state_data, 0}, {next_state_data, 5},
     {precondition, 4}, {postcondition, 5}].

%%-spec commontest_callback_funs()->[{atom(), integer()}].
commontest_callback_funs() ->
    [{all, 0}, {groups, 0}, {suite, 0}, {init_per_suite, 1}, {end_per_suite, 1}, {init_per_group, 2},
     {end_per_group, 2}, {init_per_testcase, 2}, {end_per_testcase, 2}, {testcase, 0}, {testcase, 1}].

%% =====================================================================
%% @doc Same as erlang:hd/1, except the first argument which is the
%%  error message when the list is empty.
%% @see glast/2

%%-spec(ghead(Info::string(),List::[any()]) -> any()).
ghead(Info, []) -> erlang:error(Info);
ghead(_Info, List) -> hd(List).

%% =====================================================================
%% @doc Same as lists:last(L), except the first argument which is the 
%%  error message when the list is empty.
%% @see ghead/2

%%-spec(glast(Info::string(), List::[any()]) -> any()).
glast(Info, []) -> erlang:error(Info);
glast(_Info, List) -> lists:last(List).

%% =====================================================================
%% @doc Convert a string into upper case.
%% @see to_lower/1

%%-spec(to_upper(Str::string()) -> string()).
to_upper(Str) ->
    to_upper(Str, []).

to_upper([C | Cs], Acc) when C >= 97, C =< 122 ->
    to_upper(Cs, [C - (97 - 65) | Acc]);
to_upper([C | Cs], Acc) -> to_upper(Cs, [C | Acc]);
to_upper([], Acc) -> lists:reverse(Acc).


%% =====================================================================
%% @doc Convert a string into lower case.
%% @see to_upper/1

%%-spec(to_lower(Str::string()) -> string()).
to_lower(Str) ->
    to_lower(Str, []).

to_lower([C | Cs], Acc) when C >= 65, C =< 90 ->
    to_lower(Cs, [C + (97 - 65) | Acc]);
to_lower([C | Cs], Acc) -> to_lower(Cs, [C | Acc]);
to_lower([], Acc) -> lists:reverse(Acc).


%%-spec remove_duplicates([any()]) ->[any()].
remove_duplicates(L) ->
    remove_duplicates(L, []).
remove_duplicates([],Acc) ->
     lists:reverse(Acc);
remove_duplicates([H|T], Acc) ->
    case lists:member(H, Acc) of
	true ->
	    remove_duplicates(T, Acc);
	_ ->
	    remove_duplicates(T, [H|Acc])
    end.


%%-spec format_search_paths([dir()]) -> string().				 
format_search_paths(Paths) ->
    format_search_paths(Paths, "").
format_search_paths([], Str)->
    Str;
format_search_paths([P|T], Str)->
    case Str of
	[] ->format_search_paths(T, "\""++P++"\"");
	_ ->format_search_paths(T, Str++", \""++P++"\"")
    end.
    
%%-spec default_incls()->[string()].			   
default_incls() ->
  [".", "..", "../hrl", "../incl", "../inc", "../include",
   "../../hrl", "../../incl", "../../inc", "../../include",
   "../../../hrl", "../../../incl", "../../../inc", "../../../include"].

%% ============================================================================
%% @doc Return the token list annoated to a form if there is any.

%%-spec(get_toks(Node::syntaxTree())-> [token()]).
get_toks(Node) ->
    As = wrangler_syntax:get_ann(Node),
    case lists:keysearch(toks, 1, As) of
      {value, {toks, Toks}} -> Toks;
      _ -> []
    end.

%% =====================================================================
%% @doc Reset all the annotations in the subtree to the default (empty) annotation.

%%-spec(reset_attrs(Node::syntaxTree()) -> syntaxTree()).
reset_attrs(Node) when is_list(Node) ->
    [reset_attrs(N)||N<-Node];
reset_attrs(Node) ->
    api_ast_traverse:full_buTP(
      fun (T, _Others) ->
              T1=wrangler_syntax:set_ann(
                      wrangler_syntax:set_pos(T, {0,0}), []),
              wrangler_syntax:remove_comments(T1)
      end, Node, {}).
		

%% =====================================================================
%% @spec update_ann(Node::syntaxTree(), {Key::atom(), Val::term()}) -> syntaxTree()
%% @doc Update a specific annotation of the Node with the given one.
%% if the kind of annotation already exists in the AST node, the annotation 
%% value is replaced with the new one, otherwise the given annotation info 
%% is added to the node.

%%-spec(update_ann(Node::syntaxTree(), {Key::atom(), Val::anyterm()}) -> syntaxTree()).
update_ann(Tree, {Key, Val}) ->
    As0 = wrangler_syntax:get_ann(Tree),
    As1 = case lists:keysearch(Key, 1, As0) of
	    {value, _} -> lists:keyreplace(Key, 1, As0, {Key, Val});
	    _ -> As0 ++ [{Key, Val}]
	  end,
    wrangler_syntax:set_ann(Tree, As1).


%%-spec(delete_from_ann(Node::syntaxTree(), Key::atom()) -> syntaxTree()).
delete_from_ann(Tree, Key) ->
    As0=wrangler_syntax:get_ann(Tree),
    As1 = lists:keydelete(Key, 1,As0),
    wrangler_syntax:set_ann(Tree, As1).


%% =====================================================================
%% @spec callback_funs(Behaviour)->[{FunName, Arity}]
%%       Behaviour = gen_server | gen_event | gen_fsm | supervisor
%%       FunName = atom()
%%       Arity = integer()
%% @doc Pre-defined callback functions by the standard Erlang behaviours.

%%-type(behaviour()::gen_server | gen_event | gen_fsm | supervisor).
%%-spec(callback_funs(behaviour())->[{atom(), integer()}]).
callback_funs(Behaviour) ->
    case Behaviour of
      gen_server ->
	  [{init, 1}, {handle_call, 3}, {handle_cast, 2}, {handle_info, 2},
	   {terminate, 2}, {code_change, 3}];
      gen_event ->
	  [{init, 1}, {handle_event, 2}, {handle_call, 2}, {handle_info, 2},
	   {terminate, 2}, {code_change, 3}];
      gen_fsm ->
	  [{init, 1}, {handle_event, 3}, {handle_sync_event, 4}, {handle_info, 3},
	   {terminate, 3}, {code_change, 4}];
      supervisor -> [{init, 1}];
      _ -> []
    end.

%%-spec is_callback_fun(moduleInfo(), atom(), integer()) ->boolean().
is_callback_fun(ModInfo, Funname, Arity) ->
    case lists:keysearch(attributes, 1, ModInfo) of
      {value, {attributes, Attrs}} ->
	  case lists:keysearch(behaviour, 1, Attrs) of
	    {value, {behaviour, B}} ->
		lists:member({Funname, Arity},
			     callback_funs(B));
	    _ -> false
	  end;
      _ -> false
    end.
 
%%-spec rewrite(syntaxTree(), syntaxTree())->syntaxTree().
rewrite(Tree, Tree1) ->
    wrangler_syntax:copy_attrs(Tree, Tree1).

rewrite_with_wrapper(Tree, Tree1)->
    {Start, End} = get_start_end_loc_with_comment(Tree),
    wrangler_syntax:set_pos(
         update_ann(
           wrangler_syntax:tree(fake_parentheses, Tree1),
           {range, {Start, End}}),
         Start).

max(X,Y) when X>Y ->
     X;
max(_,Y) -> Y.
    
min(X,Y) when X>Y ->
     Y;
min(X,_) -> X.

modname_to_filename(ModName, Dirs) ->
    Files = expand_files(Dirs, ".erl"),
    Fs = [F || F <- Files,
	       list_to_atom(filename:basename(F, ".erl"))==ModName],
    case Fs of
	[] ->
	    {error, "No file with module name '" ++ atom_to_list(ModName)++"' has been found."};
	[FileName] ->
	    {ok, FileName};
	_ -> {error, "Multiple files found: " ++  format_file_names(Fs)++"\n"}
    end.
			   

format_file_names(Fs) when Fs/=[] ->
    "[" ++ format_file_names_1(Fs).
  
format_file_names_1([F|T]) ->
    case T of 
	[] ->
	    io_lib:format("~s]", [F]);
	_ ->
	    io_lib:format("~s,", [F])++
		format_file_names_1(T)
    end.	 

funname_to_defpos(AnnAST, {M, F, A}) ->
    Forms=wrangler_syntax:form_list_elements(AnnAST),
    DefPs=lists:usort(lists:append([case lists:keysearch(fun_def, 1, wrangler_syntax:get_ann(Form)) of
			   {value, {fun_def, {M, F, A, _, DefPos}}} ->
			       [DefPos];
			   _ -> []
		                    end||Form <- Forms, wrangler_syntax:type(Form) == function])),
    case length(DefPs) of 
	1 ->
	    {ok, hd(DefPs)};
	0 ->
	    {error, lists:flatten(io_lib:format("Function ~p/~p is not defined in module ~p", [F, A, M]))};
	_ ->
	    {error, lists:flatten(io_lib:format("Function ~p/~p is defined more than once in module ~p", [F, A, M]))}
    end.
		 
   
is_spawn_app(Tree) ->
    SpawnFuns1 =  spawn_funs(),
    case wrangler_syntax:type(Tree) of
	application ->
	    Operator = wrangler_syntax:application_operator(Tree),
	    Ann = wrangler_syntax:get_ann(Operator),
	    case lists:keysearch(fun_def, 1, Ann) of
		{value, {fun_def, {Mod, Fun, Arity, _, _}}} -> 
                    lists:member({Mod, Fun, Arity}, SpawnFuns1);
		_ -> false
	    end;
	_ -> false
    end.
 
spawn_funs() ->
    [{erlang, spawn, 1}, 
     {erlang, spawn, 2}, 
     {erlang, spawn, 3},
     {erlang, spawn, 4},
     {erlang, spawn_link, 1}, 
     {erlang, spawn_link, 2}, 
     {erlang, spawn_link, 3}, 
     {erlang, spawn_link, 4},
     {erlang, spawn_opt, 3}, 
     {erlang, spawn_opt, 5}].

%% =====================================================================
%% @doc Recursively collect all the files with the given file extension 
%%  in the specified directoris/files.

%%-spec(expand_files(FileDirs::[filename()|dir()], Ext::string()) -> [filename()]).
expand_files(FileDirs, Ext) ->
    expand_files(FileDirs, Ext, []).

expand_files([FileOrDir | Left], Ext, Acc) ->
    case filelib:is_dir(FileOrDir) of
        true ->
	    case file:list_dir(FileOrDir) of 
		{ok, List} ->
		    NewFiles = [filename:join(FileOrDir, X)
				|| X <- List, filelib:is_file(filename:join(FileOrDir, X)),
                                   filename:extension(X) == Ext],
		    NewDirs = [filename:join(FileOrDir, X) || 
                                  X <- List, 
                                  filelib:is_dir(filename:join(FileOrDir, X))],
		    expand_files(NewDirs ++ Left, Ext, NewFiles ++ Acc);
		{error, Reason} ->
                    Msg = io_lib:format("Wrangler could not read directory ~s: ~w \n", 
                                        [filename:dirname(FileOrDir), Reason]),
		    throw({error, lists:flatten(Msg)})
	    end;
	false ->
	    case filelib:is_regular(FileOrDir) of
		true ->
		    case filename:extension(FileOrDir) == Ext of
			true ->
                            expand_files(Left, Ext, [FileOrDir | Acc]);
			false -> 
                            expand_files(Left, Ext, Acc)
		    end;
		_ -> expand_files(Left, Ext, Acc)
	    end
    end;
expand_files([], _Ext, Acc) -> ordsets:from_list(Acc).


%% =====================================================================
%% @doc The a list of files to a list of two-element tuples, with the first 
%% element of the tuple being the module name, and the second element 
%% binding the directory name of the file to which the module belongs.

%%-spec(get_modules_by_file(Files::[filename()]) -> [{atom(), dir()}]).
get_modules_by_file(Files) ->
    get_modules_by_file(Files, []).

get_modules_by_file([File | Left], Acc) ->
    BaseName = filename:basename(File, ".erl"),
    Dir = filename:dirname(File),
    get_modules_by_file(Left, [{list_to_atom(BaseName), Dir} | Acc]);
get_modules_by_file([], Acc) -> lists:reverse(Acc).


%%-spec file_format(filename()) ->dos|mac|unix. 		 
file_format(File) -> 
    case file:read_file(File) of 
	{ok, Bin} ->
	    S = erlang:binary_to_list(Bin),
	    LEs = scan_line_endings(S),
	    case LEs of 
		[] -> unix;    %% default fileformat;
		_ -> 
                    case lists:all(fun(E) -> E=="\r\n" end, LEs) of 
                        true -> dos;
                        _ -> 
                            case lists:all(fun(E) -> E=="\r" end, LEs)  of
                                true ->
                                    mac;
                                _ ->
                                    case lists:all(fun(E)-> E=="\n" end, LEs) of
                                        true -> unix;
                                        _ -> throw({error, File ++ " uses a mixture of line endings,"
							" please normalise it to one of the standard file "
                                                    "formats (i.e. unix/dos/mac) before performing any refactorings."})
                                    end
                            end
                    end
	    end;
	{error, Reason} ->
	    Msg = io_lib:format("Wrangler could not read file ~s; reason: ~w \n", 
				[File, Reason]),
	    throw({error, lists:flatten(Msg)})
    end.

scan_line_endings(Cs)->
    scan_lines(Cs, [], []).

scan_lines([$\r|Cs], [], Acc) ->
    scan_line_endings(Cs, [$\r], Acc);
scan_lines([$\n|Cs], [], Acc) ->
    scan_lines(Cs, [], [[$\n]|Acc]);
scan_lines([_C|Cs], [], Acc) ->
    scan_lines(Cs, [], Acc);
scan_lines([],[],Acc) ->
    Acc.

scan_line_endings([$\r|Cs], Cs1,Acc) ->
    scan_line_endings(Cs,[$\r|Cs1], Acc);
scan_line_endings([$\n|Cs], Cs1, Acc) ->
    scan_lines(Cs, [],[lists:reverse([$\n|Cs1])| Acc]);
scan_line_endings([_C|Cs], Cs1, Acc)->
    scan_lines(Cs, [], [lists:usort(lists:reverse(Cs1))|Acc]);
scan_line_endings([], Cs1, Acc)->
    lists:reverse([lists:usort(lists:reverse(Cs1))|Acc]).
   

get_start_end_loc_with_comment(Node) when Node==[] ->
    {{0,0},{0,0}};
get_start_end_loc_with_comment(Node) when is_list(Node) ->
    {Start, _} = get_start_end_loc_with_comment(hd(Node)),
    {_, End} = get_start_end_loc_with_comment(lists:last(Node)),
    {Start, End};
get_start_end_loc_with_comment(Node) ->
    {Start={_StartLn, StartCol}, End} = start_end_loc(Node),
    PreCs = wrangler_syntax:get_precomments(Node),
    PostCs = wrangler_syntax:get_postcomments(Node),
    Start1 = case PreCs of
                 [] -> 
                     Start;
                 _ ->
                     {StartLn1, StartCol1}=wrangler_syntax:get_pos(hd(PreCs)),
                     {StartLn1, lists:max([StartCol, StartCol1])}
             end,
    End1 = case PostCs of
               [] ->
                   End;
               _ ->
                   LastC = lists:last(PostCs),
                   LastCText = wrangler_syntax:comment_text(LastC),
                   {L, C}=wrangler_syntax:get_pos(LastC),
                   {L+length(LastCText)-1, C+length(lists:last(LastCText))-1}
           end,
    {Start1, End1}.


reset_pos(Node) when is_list(Node) ->
    [reset_pos(N)||N<-Node];
reset_pos(Node) ->
    api_ast_traverse:full_buTP(
      fun (T, _Others) ->
              wrangler_syntax:set_pos(T, {0,0})
      end, Node, {}).

reset_ann_and_pos(Node) when is_list(Node) ->
    [reset_ann_and_pos(N)||N<-Node];
reset_ann_and_pos(Node) ->
    api_ast_traverse:full_buTP(
      fun (T, _Others) ->
              wrangler_syntax:set_ann(
                   wrangler_syntax:set_pos(T, {0,0}), [])
      end, Node, {}).

reset_ann(Node) when is_list(Node) ->
    [reset_ann(N)||N<-Node];
reset_ann(Node) ->
    api_ast_traverse:full_buTP(
      fun (T, _Others) ->
              wrangler_syntax:set_ann(T, [])
      end, Node, {}).


reset_pos_and_range(Node) when is_list(Node) ->
    [reset_pos_and_range(N)||N<-Node];
reset_pos_and_range(Node) ->
    case wrangler_syntax:is_tree(Node) orelse wrangler_syntax:is_wrapper(Node) of
        true ->
            wrangler_syntax:set_pos(
                 wrangler_misc:update_ann(Node, {range, {{0,0},{0,0}}}),
                 {0,0});
        false ->
            Node
    end.

%%-spec try_eval(filename()|none, syntaxTree(), [dir()], integer()) ->
%%		      term().
try_eval(none, Node, _, _) ->
    try
      erl_eval:exprs([wrangler_syntax:revert(Node)], [])
    of
      {value, Val, _} -> {value, Val}
    catch
      _E1:_E2 ->
	  {error, no_value}
    end;
try_eval(FileName, Node, SearchPaths, TabWidth) ->
    try
        erl_eval:exprs([wrangler_syntax:revert(Node)], [])
    of
        {value, Val, _} -> {value, Val}
    catch
      _:_ ->
	  case has_macros(Node) andalso free_vars(Node) == [] of
	    true ->
		Dir = filename:dirname(FileName),
		DefaultIncl2 = [filename:join(Dir, X) || X <- default_incls()],
		NewSearchPaths = SearchPaths ++ DefaultIncl2,
		{Ms, UMs} = case wrangler_epp:parse_file(FileName, NewSearchPaths, []) of
			      {ok, _, {Defs, Uses}} ->
				  {dict:from_list(Defs), dict:from_list(Uses)};
			      _ -> {[], []}
			    end,
		NodeToks = get_toks(FileName, Node, TabWidth),
		try
		  wrangler_epp:expand_macros(NodeToks, {Ms, UMs})
		of
		  NewToks when is_list(NewToks) ->
		      case wrangler_parse:parse_exprs(NewToks ++ [{dot, {999, 0}}]) of
			{ok, Exprs} ->
			    try
			      erl_eval:exprs(Exprs, [])
			    of
			      {value, Val, _} -> {value, Val}
			    catch
			      _:_ -> {error, no_value}
			    end;
			_ -> {error, no_value}
		      end
		catch
		  _:__ -> {error, no_value}
		end;
	    false ->
		{error, no_value}
	  end
    end.

%%-spec get_toks(filename(), syntaxTree(), integer()) ->
%%		      [token()].
get_toks(FileName, Node, TabWidth) ->
    Toks = tokenize(FileName, false, TabWidth),
    {StartPos, EndPos} = start_end_loc(Node),
    Toks1 = lists:dropwhile(fun (T) ->
				    token_loc(T) < StartPos
			    end, Toks),
    lists:takewhile(fun (T) ->
			    token_loc(T) =< EndPos
		    end, Toks1).

-spec(tokenize(File::filename(), WithLayout::boolean(), TabWidth::integer()) 
      -> [token()]|{error, term()}).
tokenize(File, WithLayout, TabWidth) ->
    case file:read_file(File) of
	{ok, Bin} ->
	    S = erlang:binary_to_list(Bin),
	    case WithLayout of 
		true -> 
		    {ok, Ts, _} = wrangler_scan_with_layout:string(
                                       S, {1,1}, TabWidth,
                                       wrangler_misc:file_format(File)),
		    Ts;
		_ -> {ok, Ts, _} = wrangler_scan:string(
                                        S, {1,1}, TabWidth,
                                        wrangler_misc:file_format(File)),
		     Ts
	    end;
	{error, Reason} ->
            {error, Reason}
    end.
token_loc(T) ->
    case T of
      {_, L, _V} -> L;
      {_, L1} -> L1
    end.

has_macros(Node) ->
    F = fun (N, _Others) ->
		case wrangler_syntax:type(N) of
		  macro -> {N, true};
		  _ -> {[], false}
		end
	end,
    {_, Res} = api_ast_traverse:once_tdTU(F, Node, []),
    Res.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%-spec collect_var_source_def_pos_info([syntaxTree()]|syntaxTree()) ->
%%					     [{atom(), pos(), [pos()]}].
collect_var_source_def_pos_info(Nodes) when is_list(Nodes) ->
    lists:flatmap(fun (N) -> collect_var_source_def_pos_info(N) end, Nodes);
collect_var_source_def_pos_info(Node) ->
    F = fun (T, S) ->
		case wrangler_syntax:type(T) of
		    variable ->
			SourcePos = wrangler_syntax:get_pos(T),
			case lists:keysearch(def, 1, wrangler_syntax:get_ann(T)) of
			    {value, {def, DefinePos}} ->
				VarName = wrangler_syntax:variable_name(T),
				S ++ [{VarName, SourcePos, DefinePos}];
			    _ ->
				S
			end;
		    _ -> S
		end
	end,
    api_ast_traverse:fold(F, [], Node).

%%-spec collect_var_names(syntaxTree()|[syntaxTree()]) ->
%%			       [atom()].
collect_var_names(Node) when is_list(Node) ->
    collect_var_names_1(wrangler_syntax:block_expr(Node));
collect_var_names(Node) ->
    collect_var_names_1(Node).

collect_var_names_1(Node) ->
    F = fun (N, S) ->
		case wrangler_syntax:type(N) of
		    variable ->
			Ann = wrangler_syntax:get_ann(N),
                        case lists:keysearch(syntax_path, 1, Ann) of
			    {value, {syntax_path, macro_name}} -> S;
			    _ ->
				VarName = wrangler_syntax:variable_name(N),
				ordsets:add_element(VarName, S)
			end;
		    _ -> S
		end
	end,
    ordsets:to_list(api_ast_traverse:fold(F, ordsets:new(), Node)).

%%-spec collect_used_macros(syntaxTree()) ->
%%				 [atom()].
collect_used_macros(Node) ->
    F = fun (T, S) ->
		case wrangler_syntax:type(T) of
		    macro ->
			Name = wrangler_syntax:macro_name(T),
			case wrangler_syntax:type(Name) of
			    variable -> [wrangler_syntax:variable_name(Name)| S];
			    atom -> [wrangler_syntax:atom_value(Name)| S]
			end;
		    _ -> S
		end
	end,
    lists:usort(api_ast_traverse:fold(F, [], Node)).

%%-spec collect_used_records(syntaxTree())-> [atom()].
collect_used_records(Node) ->
    Fun = fun (T, S) ->
		  case wrangler_syntax:type(T) of
		      record_access ->
			  Type = wrangler_syntax:record_access_type(T),
			  case wrangler_syntax:type(Type) of
			      atom ->
				  ordsets:add_element(wrangler_syntax:atom_value(Type), S);
			      _ -> S
			  end;
		      record_expr ->
			  Type = wrangler_syntax:record_expr_type(T),
			  case wrangler_syntax:type(Type) of
			      atom ->
				  ordsets:add_element(wrangler_syntax:atom_value(Type), S);
			      _ -> S
			  end;
		      record_index_expr ->
			  Type = wrangler_syntax:record_index_expr_type(T),
			  case wrangler_syntax:type(Type) of
			      atom ->
				  ordsets:add_element(wrangler_syntax:atom_value(Type), S);
			      _ -> S
			  end;
		      _ -> S
		  end
	  end,
    ordsets:to_list(api_ast_traverse:fold(Fun, ordsets:new(), Node)).

%%-spec(concat_toks(Toks::[token()]) ->string()).
concat_toks(Toks) ->
    concat_toks(Toks, "").

concat_toks([], Acc) ->
     lists:concat(lists:reverse(Acc));
concat_toks([T|Ts], Acc) ->
     case T of 
	 {atom, _,  V} -> S = io_lib:write_atom(V), 
			  concat_toks(Ts, [S|Acc]);
	 {qatom, _, V} -> S=atom_to_list(V),
			  concat_toks(Ts, [S|Acc]);
	 {string, _, V} -> concat_toks(Ts,["\"", V, "\""|Acc]);
	 {char, _, V} when is_atom(V)->concat_toks(Ts,[atom_to_list(V)|Acc]);
	 {char, _, V} when is_integer(V) and (V =< 127)-> concat_toks(Ts,[io_lib:write_char(V)|Acc]);
	 {char, _, V} when is_integer(V) ->
	     {ok, [Num], _} = io_lib:fread("~u", integer_to_list(V)),
	     [Str] = io_lib:fwrite("~.8B", [Num]),
	     S = "$\\"++Str,
	      concat_toks(Ts, [S|Acc]); 
	 {float, _, V} -> concat_toks(Ts,[io_lib:write(V)|Acc]);
     	 {_, _, V} -> concat_toks(Ts, [V|Acc]);
	 {dot, _} ->concat_toks(Ts, ['.'|Acc]);
      	 {V, _} -> 
	     concat_toks(Ts, [V|Acc])
     end.



%%-spec test_framework_used(filename()) ->[atom()]. 			 
test_framework_used(FileName) ->
    case wrangler_epp_dodger:parse_file(FileName, []) of
      {ok, Forms} ->
	  Strs = lists:flatmap(fun (F) ->
				       case wrangler_syntax:type(F) of
					   attribute ->
					       Name = wrangler_syntax:attribute_name(F),
					       Args = wrangler_syntax:attribute_arguments(F),
					       case wrangler_syntax:type(Name) of
					       atom ->
						       AName = wrangler_syntax:atom_value(Name),
						       case AName == include orelse AName == include_lib of
						           true ->
							       lists:flatmap(fun (A) -> case A of
											    {string, _, Str} -> [Str];
											    _ -> []
										        end
									     end, Args);
						           _ -> []
						       end;
					       _ -> []
					       end;
					   _ -> []
				       end
			       end, Forms),
	  Eunit = lists:any(fun (S) -> lists:suffix("eunit.hrl", S) end, Strs),
	  EQC = lists:any(fun (S) -> lists:suffix("eqc.hrl", S) end, Strs),
	  EQC_STATEM = lists:any(fun (S) -> lists:suffix("eqc_statem.hrl", S) end, Strs),
	  EQC_FSM = lists:any(fun (S) -> lists:suffix("eqc_fsm.hrl", S) end, Strs),
	  TestSever = lists:suffix(FileName, "_SUITE.erl") and
			lists:any(fun (S) -> lists:suffix("test_server.hrl", S) end, Strs),
	  CommonTest = lists:suffix(FileName, "_SUITE.erl") and
			 lists:any(fun (S) -> lists:suffix("ct.hrl", S) end, Strs),
	  lists:flatmap(fun ({F, V}) -> case V of
					  true -> [F];
					  _ -> []
					end
			end, [{eunit, Eunit}, {eqc, EQC}, {eqc_statem, EQC_STATEM},
			      {eqc_fsm, EQC_FSM},
			      {testserver, TestSever}, {commontest, CommonTest}]);
      _ -> []
    end.


is_macro_name(Node) ->
    Ann = wrangler_syntax:get_ann(Node),
    {value, {syntax_path, macro_name}} == 
        lists:keysearch(syntax_path, 1, Ann).



is_literal(Node) ->
    case wrangler_syntax:type(Node) of
        atom -> true;
        integer -> true;
        float -> true;
        char -> true;
        string -> true;
        nil -> true;
        _ -> false
    end.
 

%% ====================================================================
%%@doc Returns the start and end locations of an AST node or a sequence 
%%     of AST node. {{0,0},{0,0}} is returned if the AST nodes are not 
%%     annotated with location information.
%%@spec start_end_loc([syntaxTree()]|syntaxTree()) ->{pos(), pos()}
-spec start_end_loc([syntaxTree()]|syntaxTree()) ->{pos(), pos()}.
start_end_loc(Exprs) when is_list(Exprs) ->
    E1 = hd(Exprs),
    En = lists:last(Exprs),
    {S, _E} = get_range(E1),
    {_S, E} = get_range(En),
    {S, E};
start_end_loc(Expr) ->
    get_range(Expr).

get_range(Node) ->
    As = wrangler_syntax:get_ann(Node),
    case lists:keysearch(range, 1, As) of
	{value, {range, {S, E}}} -> 
            {S, E};
	_ -> 
            {?DEFAULT_LOC,?DEFAULT_LOC} 
    end.


%%=====================================================================
%%@doc Returns all the variables, including both variable name and define
%%      location, that are free within `Node'.
%%@spec free_vars([syntaxTree()]|syntaxTree())-> [{atom(),pos()}]
-spec(free_vars(Node::[syntaxTree()]|syntaxTree())-> [{atom(),pos()}]).
free_vars(Nodes) when is_list(Nodes) ->
    {FVs, BVs} = lists:unzip([{free_vars(Node), bound_vars(Node)}
                              ||Node<-Nodes]),
    lists:usort(lists:append(FVs)) -- lists:usort(lists:append(BVs));

free_vars(Node) ->
    Ann = wrangler_syntax:get_ann(Node),
    case lists:keyfind(free,1,Ann) of 
        {free, Vs} ->
            Vs;
        false ->
            []
    end.

%%=====================================================================
%%@doc Returns all the variables, including both variable name and define
%%      location, that are declared within `Node', and also used by the 
%%      code outside `Node'.
%%@spec exported_vars([syntaxTree()]|syntaxTree())-> [{atom(),pos()}]
-spec(exported_vars(Node::[syntaxTree()]|syntaxTree())-> [{atom(),pos()}]).
exported_vars(Nodes) when is_list(Nodes) ->
    Range = start_end_loc(Nodes),
    lists:flatmap(fun (Node) -> 
                          exported_vars_1(Node, Range)
                  end, Nodes);
exported_vars(Node) ->
    Range = start_end_loc(Node),
    exported_vars_1(Node, Range).

exported_vars_1(Node, {StartLoc, EndLoc}) ->
    Fun = fun (N, Acc) ->
                  case wrangler_syntax:type(Node) of
                      variable ->
                          Ann = wrangler_syntax:get_ann(N),
                          case lists:keyfind(bound, 1, Ann) of 
                              {use, Bound} when Bound/=[] ->
                                  case lists:keyfind(use,1,Ann) of 
                                      {use, Locs} ->
                                          case [L||L<-Locs, L>EndLoc orelse L < StartLoc] of
                                              [] -> Acc;
                                              _ ->
                                                  Name = wrangler_syntax:variable_name(N),
                                                  Pos = wrangler_syntax:get_pos(N),
                                                  ordsets:add_element({Name,Pos}, Acc)
                                          end;
                                      false ->
                                          Acc
                                  end;
                              _ -> Acc
                          end;
                      _ -> Acc        
                  end
          end,
    ordsets:to_list(api_ast_traverse:full_tdTU(Fun,ordsets:new(),Node)).

%%=====================================================================
%%@doc Returns all the variables, including both variable name and define
%%      location, that are declared within `Node'.
%%@spec bound_vars([syntaxTree()]|syntaxTree())-> [{atom(),pos()}]
-spec(bound_vars(Node::[syntaxTree()]|syntaxTree())-> [{atom(),pos()}]).
bound_vars(Nodes) when is_list(Nodes) ->
    lists:usort(lists:flatmap(fun (Node) -> 
                                      bound_vars(Node) 
                              end, Nodes));
bound_vars(Node) ->
    Fun = fun (N, Acc) ->
                  Ann = wrangler_syntax:get_ann(N),
                  case lists:keyfind(bound,1,Ann) of
                      {bound, Vs} ->
                          Vs ++ Acc;
                      false ->
                          Acc
                  end
          end,
    Vars=api_ast_traverse:fold(Fun, [], Node),
    lists:usort(Vars).


%% =====================================================================
%%@doc Returns `true' if a string is lexically a legal variable name,
%%      otherwise `false'.
%%@spec is_var_name(string())-> boolean()
-spec(is_var_name(Name:: string())-> boolean()).
is_var_name(Name) ->
    case Name of
      [] -> false;
      [H] -> is_upper(H) and (H =/= 95);
      [H| T] -> (is_upper(H) or (H == 95)) and is_var_name_tail(T)
    end.

%%@doc Returns `true' if a string is lexically a legal function name,
%%      otherwise `false'.
%%@spec is_fun_name(string())-> boolean()
-spec(is_fun_name(string())-> boolean()).
is_fun_name(Name) ->
    case Name of
      [H| T] -> is_lower(H) and is_var_name_tail(T);
      [] -> false
    end.

is_var_name_tail(Name) ->
    case Name of
      [H| T] ->
	  (is_upper(H) or is_lower(H) or 
	   is_digit(H) or (H == 64) or (H == 95)) and
	    is_var_name_tail(T);
      [] -> true
    end.

is_upper(L) -> (L >= 65) and (90 >= L).

is_lower(L) -> (L >= 97) and (122 >= L).

is_digit(L) -> (L >= 48) and (57 >= L).




%%===================================================================
%%@private
extended_parse_annotate_expr(Str) ->
    extend_function_clause(parse_annotate_expr(Str)).
%%@private
extended_parse_annotate_expr(Str, Pos) ->
    extend_function_clause(parse_annotate_expr(Str, Pos)).
%%@private
parse_annotate_expr("") ->
    wrangler_syntax:empty_node();
parse_annotate_expr(ExprStr) ->
    parse_annotate_expr(ExprStr, {1,1}).
%%@private
parse_annotate_expr("", _) ->
    wrangler_syntax:empty_node();
parse_annotate_expr(ExprStr, StartLoc) when is_integer(StartLoc) ->
    parse_annotate_expr(ExprStr, {StartLoc, 1});
parse_annotate_expr(ExprStr, StartLoc) when is_tuple(StartLoc) ->
    case wrangler_scan:string(ExprStr, StartLoc) of
        {ok, Toks, _} ->
            [T|Ts] = lists:reverse(Toks),
            Toks1 = case T of 
                        {dot, _} -> Toks;
                        {';',_} -> lists:reverse([{dot, 999}|Ts]);
                        _ -> Toks++[{dot, 999}]
                    end,
            Toks2 = wrangler_epp_dodger:scan_macros(Toks1,[]),
            case wrangler_parse:parse_form(Toks2) of 
                {ok, AbsForm} ->
                    case wrangler_syntax:type(AbsForm) of
                        function ->
                            Form1 =wrangler_epp_dodger:fix_pos_in_form(Toks, AbsForm),
                            Form2 =  wrangler_syntax_lib:annotate_bindings(Form1),
                            Cs = wrangler_syntax:function_clauses(Form2),
                            case {Cs, T} of 
                                {[C], {';',_L}} ->
                                    Name = wrangler_syntax:function_name(Form2),
                                    rewrite(C, wrangler_syntax:function_clause(Name, C));
                                _ ->
                                    Form2
                            end;
                        _ ->
                            wrangler_epp_dodger:fix_pos_in_form(Toks, AbsForm)
                    end;
                {error, Reason} ->
                   case T of 
                       {dot, _} ->
                           throw({error, Reason});
                       {';',_} -> 
                           throw({error, Reason});
                       _ ->
                           case wrangler_parse:parse_exprs(Toks2) of
                               {ok, Exprs} ->
                                   Exprs1 =wrangler_epp_dodger:rewrite_list(Exprs),
                                   Exprs2 = make_tree({block, StartLoc, Exprs1}),
                                   Exprs3=wrangler_syntax_lib:annotate_bindings(Exprs2),
                                   Exprs4 =wrangler_syntax:block_expr_body(Exprs3),
                                   case Exprs4 of 
                                       [E] -> E;
                                       _ -> Exprs4
                                   end;
                               {error, Reason1} ->
                                   throw({error, Reason1})
                           end
                   end
            end;
        {error, ErrInfo, ErrLoc} ->
            throw({error, {ErrInfo, ErrLoc}})
    end.

make_tree(Tree) ->
    case wrangler_syntax:subtrees(Tree) of
        [] ->
           Tree;
        Gs ->
            Gs1 = [[make_tree(T) || T <- G] || G <- Gs],
            wrangler_syntax:update_tree(Tree, Gs1)
    end.



%%================================================================
%%@spec(extend_function_clause(Tree::syntaxTree()) -> syntaxTree()).
%%@private             
extend_function_clause(Tree) when is_list(Tree) ->
    [extend_function_clause(T)||T<-Tree];
extend_function_clause(Tree) ->
    {Tree1, _} = api_ast_traverse:stop_tdTP(
                   fun extend_function_clause_1/2, Tree, {}),
    Tree1.

extend_function_clause_1(Node, _OtherInfo) ->
    case wrangler_syntax:type(Node) of
        function ->
            Node1=extend_function_clause_2(Node),
            {Node1, true};
        _ ->
            {Node, false}
    end.

extend_function_clause_2(Node) ->
    Name = wrangler_syntax:function_name(Node),
    Cs = wrangler_syntax:function_clauses(Node),
    Cs1= [case wrangler_syntax:type(C) of
              clause ->
                  rewrite(C,wrangler_syntax:function_clause(Name, C));
              _ ->
                  C
          end
          ||C<-Cs],
    rewrite(Node, wrangler_syntax:function(Name, Cs1)).
