%% =====================================================================
%% Some Interface Functions to Emacs
%%
%% Copyright (C) 2006-2008  Huiqing Li, Simon Thompson

%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.

%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.

%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%% 
%% =====================================================================

-module(wrangler_distel).

-include("../include/wrangler.hrl").

-compile(export_all).

-spec(rename_var/6::(filename(), integer(), integer(), string(), [dir()], integer()) ->
	     {error, string()} | {ok, string()}).

rename_var(Fname, Line, Col, NewName, SearchPaths, TabWidth) ->
    apply_refactoring(wrangler, rename_var, [Fname, Line, Col, NewName, SearchPaths, TabWidth], SearchPaths).
 
-spec(rename_fun/6::(string(), integer(), integer(), string(), [dir()], integer()) ->
	     {error, string()} | {ok, [filename()]}).

rename_fun(Fname, Line, Col, NewName, SearchPaths, TabWidth) ->
    apply_refactoring(wrangler, rename_fun, [Fname, Line, Col, NewName, SearchPaths, TabWidth], SearchPaths).


-spec(rename_mod/4::(filename(), string(), [dir()], integer()) -> {error, string()} | {ok, [filename()]}).

rename_mod(Fname, NewName, SearchPaths, TabWidth) ->
    apply_refactoring(wrangler, rename_mod, [Fname, NewName, SearchPaths, TabWidth], SearchPaths).


-spec(rename_process/6::(filename(), integer(), integer(), string(), [dir()], integer()) ->
	     {error, string()} | {undecidables, string()}| {ok, [filename()]}).

rename_process(Fname, Line, Col, NewName, SearchPaths, TabWidth) ->
    apply_refactoring(wrangler, rename_process, [Fname, Line, Col, NewName, SearchPaths, TabWidth], SearchPaths).


-spec(generalise/8::(filename(),integer(), integer(),integer(), integer(),string(), dir(), integer()) -> {ok, string()} | {error, string()}).

generalise(Fname, StartLine, StartCol, EndLine, EndCol, ParName, SearchPaths, TabWidth) ->
    apply_refactoring(wrangler, generalise, [Fname, {StartLine, StartCol}, {EndLine, EndCol}, ParName, SearchPaths, TabWidth], SearchPaths).
	

-spec(move_fun/7::(filename(),integer(),integer(), string(), atom(),[dir()], integer())
        -> {ok, [{filename(), filename()}]}
           | {error, string()}).

move_fun(FName, Line, Col, ModName, CreateNewFile, SearchPaths, TabWidth) ->
    apply_refactoring(wrangler, move_fun, [FName, Line, Col, ModName, CreateNewFile, SearchPaths, TabWidth], SearchPaths).


-spec(duplicated_code_in_buffer/4::(filename(), string(), string(), integer()) ->{ok, string()}).      
duplicated_code_in_buffer(FName, MinLines,  MinClones, TabWidth) ->
    wrangler:duplicated_code_in_buffer(FName, MinLines, MinClones, TabWidth).

-spec(duplicated_code_in_dirs/4::([dir()], string(), string(), integer()) ->{ok, string()}).
duplicated_code_in_dirs(SearchPaths, MinLines, MinClones, TabWidth) ->
    case check_searchpaths(SearchPaths) of 
 	ok ->
	    wrangler:duplicated_code_in_dirs(SearchPaths, MinLines, MinClones, TabWidth);
 	{error, Reason} -> {error, Reason}
     end.
  
-spec(expression_search/6::(filename(), integer(), integer(), integer(), integer(),integer()) -> {ok, [{integer(), integer(), integer(), integer()}]} | {error, string()}).
expression_search(FName, StartLine, StartCol, EndLine, EndCol, TabWidth) ->
    wrangler:expression_search(FName, {StartLine, StartCol}, {EndLine, EndCol}, TabWidth).

-spec(fun_extraction/7::(filename(), integer(), integer(), integer(), integer(), string(), integer()) ->
	      {error, string()} | {ok, string()}).

fun_extraction(FName, StartLine, StartCol, EndLine, EndCol, FunName, TabWidth) ->
    apply_refactoring(wrangler, fun_extraction, [FName, {StartLine, StartCol}, {EndLine, EndCol}, FunName, TabWidth], []).
	 

-spec(new_macro/8::(filename(), integer(), integer(), integer(), integer(), string(), [dir()], integer()) ->
	      {error, string()} | {ok, string()}).

new_macro(FName, StartLine, StartCol, EndLine, EndCol, MacroName, SearchPaths, TabWidth) ->
    apply_refactoring(wrangler, new_macro, [FName, {StartLine, StartCol}, {EndLine, EndCol}, MacroName, SearchPaths, TabWidth], SearchPaths).


-spec(fold_against_macro/5::(filename(), integer(), integer(), [dir()], integer()) ->
	      {error, string()} | {ok, [{{{integer(), integer()}, {integer(), integer()}}, syntaxTree()}]}).

fold_against_macro(FileName, Line, Col,  SearchPaths, TabWidth) ->
    apply_refactoring(wrangler, fold_against_macro, [FileName, Line, Col, SearchPaths, TabWidth], SearchPaths).

-spec(fold_expr_by_loc/5::
      (filename(), integer(), integer(), [dir()], integer()) -> {ok, [{integer(), integer(), integer(), integer(), syntaxTree(), {syntaxTree(), integer()}}]}
							 | {error, string()}).

fold_expr_by_loc(FName, Line, Col, SearchPaths, TabWidth) ->
    apply_refactoring(wrangler, fold_expr_by_loc, [FName, Line, Col, SearchPaths, TabWidth], SearchPaths).


-spec(fold_expr_by_name/7::(filename(), string(), string(), string(), string(), [dir()], integer()) ->
	     {ok, [{integer(), integer(), integer(), integer(), syntaxTree(), {syntaxTree(), integer()}}]}
		 | {error, string()}).

fold_expr_by_name(FileName, ModName, FunName, Arity, ClauseIndex, SearchPaths, TabWidth) ->
    apply_refactoring(wrangler, fold_expr_by_name, [FileName, ModName, FunName, Arity, ClauseIndex, SearchPaths, TabWidth], SearchPaths).


	       
-spec(instrument_prog/3::(filename(), [dir()], integer()) ->{ok, [filename()]} | {error, string()}).

instrument_prog(FName, SearchPaths, TabWidth) ->
    apply_refactoring(wrangler, instrument_prog, [FName, SearchPaths, TabWidth], SearchPaths).


-spec(tuple_funpar/6::(filename(), integer(), integer(), string(), [dir()], integer()) ->
	     {error, string()} | {ok, [filename()]}).

tuple_funpar(Fname, Line, Col, Number, SearchPaths, TabWidth) ->
    apply_refactoring(wrangler, tuple_funpar, [Fname, Line, Col, Number, SearchPaths, TabWidth], SearchPaths).


-spec(tuple_to_record/9::(filename(), integer(), integer(), integer(), integer(), string(), [string()], [dir()], integer()) ->
	     {error, string()} | {ok, [filename()]}).

tuple_to_record(File, FLine, FCol, LLine, LCol, RecName, FieldString, SearchPaths, TabWidth) ->
    apply_refactoring(wrangler, tuple_to_record, [File, FLine, FCol, LLine, LCol, RecName, FieldString, SearchPaths, TabWidth], SearchPaths).

    
-spec(uninstrument_prog/3::(filename(), [dir()], integer()) ->{ok, [filename()]} | {error, string()}).

uninstrument_prog(FName, SearchPaths, TabWidth) ->
    apply_refactoring(wrangler, uninstrument_prog, [FName, SearchPaths, TabWidth], SearchPaths).


-spec(add_a_tag/6::(filename(), integer(), integer(), string(), [dir()], filename()) ->
	     {error, string()} | {ok, [filename()]}).

add_a_tag(FileName, Line, Col, Tag, SearchPaths, TabWidth) ->
    apply_refactoring(wrangler, add_a_tag, [FileName, Line, Col, Tag, SearchPaths, TabWidth], SearchPaths).


-spec(register_pid/8::(filename(), integer(), integer(), integer(),integer(), string(), [dir()], integer()) ->
    {error, string()}|{ok, [filename()]}).

register_pid(FileName, StartLine, StartCol, EndLine, EndCol, RegName, SearchPaths, TabWidth) ->
    apply_refactoring(wrangler, register_pid, [FileName, {StartLine, StartCol}, {EndLine, EndCol}, RegName, SearchPaths, TabWidth], SearchPaths).


-spec(fun_to_process/6::(filename(), integer(), integer(), string(), [dir()], integer())->
	     {error, string()} | {undecidables, string()} | {ok, [filename()]}).

fun_to_process(Fname, Line, Col, ProcessName, SearchPaths, TabWidth ) ->
    apply_refactoring(wrangler, fun_to_process, [Fname, Line, Col, ProcessName, SearchPaths, TabWidth], SearchPaths).

apply_refactoring(Mod, Fun, Args, SearchPaths) ->
    case initial_checking(SearchPaths) of
      ok ->
	  Res = apply(Mod, Fun, Args),
	  check_wrangler_error_logger(),
	  Res;
      {error, Reason} -> {error, Reason}
    end.

initial_checking(SearchPaths1) ->
    case check_searchpaths(SearchPaths1) of
      ok ->
	  check_undo_process();
      {error, Reason} -> {error, Reason}
    end.

check_searchpaths(SearchPaths) ->
    InValidSearchPaths = lists:filter(fun (X) -> not filelib:is_dir(X) end, SearchPaths),
    case InValidSearchPaths of
	[] -> ok;
	_ -> ?wrangler_io("\n===============================WARNING===============================\n",[]), 
	     ?wrangler_io("The following directories specified in the search paths do not exist:\n~s", [InValidSearchPaths]),
	     {error, "Some directories specified in the search paths do not exist!"}
    end.
			  
check_undo_process() ->
    case erlang:whereis(refactor_undo) of
	undefined ->
	    {error, "The Wrangler application is started, or is not working properly, please restart the refactorer!"};
	_ ->
	    ok
    end.

check_wrangler_error_logger() ->
    Errors = wrangler_error_logger:get_logged_errors(),
    case Errors of 
	[] ->
	     ok;
	_ ->  ?wrangler_io("\n===============================WARNING===============================\n",[]),
	      ?wrangler_io("There are errors in the program, and functions/attribute containing errors are not affected by refactoring.\n",[]),
	      Msg =lists:flatmap(fun({FileName, Errs}) ->
				    Str =io_lib:format("File:\n ~p\n", [FileName]),
				    Str1 = Str ++ io_lib:format("Error(s):\n",[]),
				    Str1++lists:flatmap(fun(E) ->
							  case E of 
							      {Pos, _Mod, Msg} ->io_lib:format(" ** ~p:~p **\n", [Pos, Msg]);
							      M -> io_lib:format("**~s**\n", [M])
							  end
						  end,
						  lists:reverse(Errs)) 
				 end, Errors),
	      ?wrangler_io(Msg, [])
    end.
    


