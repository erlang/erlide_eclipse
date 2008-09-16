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

-export([ rename_fun/5, rename_var/5, rename_mod/3, generalise/7, move_fun/6, tuple_to_record/8,
         duplicated_code/3, expression_search/5, fun_extraction/6, fold_expression/3, tuple_funpar/5,
	 instrument_prog/2, uninstrument_prog/2, add_a_tag/5,
         undo/0, start_undo_process/0, stop_undo_process/0, undo_init/0]).


rename_var(Fname, Line, Col, NewName, SearchPaths) ->
    case check_undo_process() of
	ok -> wrangler:rename_var(Fname, Line, Col, NewName, SearchPaths);
	{error, Reason} ->
	    {error, Reason}
    end.

rename_fun(Fname, Line, Col, NewName,SearchPaths) ->
    case check_undo_process() of
	ok -> wrangler:rename_fun(Fname, Line, Col, NewName,SearchPaths);
	{error, Reason} ->
	   {error, Reason}
    end.

rename_mod(Fname, NewName,SearchPaths) ->
    case check_undo_process() of 
	ok ->  wrangler:rename_mod(Fname, NewName,SearchPaths);
	{error, Reason} ->
	    {error, Reason}
    end.

generalise(Fname, StartLine, StartCol, EndLine, EndCol, ParName, SearchPaths)->
    case check_undo_process() of 
	ok -> wrangler:generalise(Fname, {StartLine, StartCol}, {EndLine, EndCol}, ParName, SearchPaths);
	{error, Reason} ->
	    {error, Reason}
    end.

move_fun(FName, Line, Col, ModName, CreateNewFile, SearchPaths) ->
    case check_undo_process() of
	ok -> wrangler:move_fun(FName, Line, Col, ModName, CreateNewFile, SearchPaths);
	{error, Reason} ->
	    {error, Reason}
    end.

duplicated_code(FName, MinLines,  MinClones) ->
    wrangler:duplicated_code(FName, MinLines, MinClones).


expression_search(FName, StartLine, StartCol, EndLine, EndCol) ->
    wrangler:expression_search(FName, {StartLine, StartCol}, {EndLine, EndCol}).

fun_extraction(FName, StartLine, StartCol, EndLine, EndCol, FunName) ->
    case check_undo_process() of 
	ok -> wrangler:fun_extraction(FName, {StartLine, StartCol}, {EndLine, EndCol}, FunName);
	{error, Reason} ->
	    {error, Reason}
    end.


fold_expression(FName, Line, Col) ->
    case check_undo_process() of 
	ok ->  wrangler:fold_expression(FName, Line, Col);
	{error, Reason} ->
	    {error, Reason}
    end.
	       
instrument_prog(FName, SearchPaths) ->
    case check_undo_process() of 
	ok ->
	    wrangler:instrument_prog(FName, SearchPaths);
	{error, Reason} ->
	    {error, Reason}
    end.

tuple_funpar(Fname, Line, Col, Number, SearchPaths) ->
    case check_undo_process() of
	ok -> wrangler:tuple_funpar(Fname, Line, Col, Number, SearchPaths);
	{error, Reason} ->
	    {error, Reason}
    end.

tuple_to_record(File, FLine, FCol, LLine, LCol, 
                RecName, FieldString, SearchPaths)->
    case check_undo_process() of
	ok -> wrangler:tuple_to_record(File, FLine, FCol, LLine, LCol, 
                                       RecName, FieldString, SearchPaths);
	{error, Reason} ->
	    {error, Reason}
    end.
    

uninstrument_prog(FName, SearchPaths) ->
    case check_undo_process() of 
	ok ->
	    wrangler:uninstrument_prog(FName, SearchPaths);
	{error, Reason} ->
	    {error, Reason}
    end.


add_a_tag(FileName, Line, Col, Tag, SearchPaths) ->
    case check_undo_process() of 
	ok ->
	    wrangler:add_a_tag(FileName, Line, Col, Tag, SearchPaths);
	{error, Reason} ->
	    {error,Reason}
    end.
%% tuple_to_record(Fname, StartLine, StartCol, EndLine, EndCol) ->
%%     refac_record:tuple_to_record(Fname, {StartLine, StartCol}, {EndLine, EndCol}).


check_undo_process() ->
    case erlang:whereis(refactor_undo) of
	undefined ->
	    {error, "The UNDO process is not working, please restart the refactorer!"};
	_ ->
	    ok
    end.

undo() ->
    refactor_undo ! {self(),undo},
    receive
	{refactor_undo, Reply} ->
	    Reply
    end.
    
start_undo_process() ->
    spawn_link(wrangler_distel, undo_init, []).



stop_undo_process()-> 
    refactor_undo ! stop.
    
undo_init()->
    case erlang:whereis(refactor_undo) of 
	undefined -> ok;
	_         -> erlang:unregister(refactor_undo)
    end,
    register(refactor_undo, self()),
    History=[],
    undo_loop(History).
    

undo_files(Files) -> 
    case Files of 
	[] ->
	    ok;
	[{{OldFileName,NewFileName}, Content}|T] -> 
	    case OldFileName == NewFileName of
		true ->  file:write_file(OldFileName, Content),
			 undo_files(T);
		false -> file:write_file(OldFileName, Content),
			 file:delete(NewFileName),
			 undo_files(T)
	    end
    end.

undo_loop(History) ->
    receive 
	{From, undo} -> case History of 
			    [] ->
				From ! {refactor_undo, {error, "No more history to undo!"}},
				undo_loop(History);
			    [H|T] -> 
				ok = undo_files(H),
                          	Modified = lists:map(fun({{OldFileName, NewFileName}, _Con})->
							   [OldFileName, NewFileName] end,H),
				From ! {refactor_undo, {ok, Modified}},
			        undo_loop(T)
			end;
       {add, Files} ->    
	               History1=[Files|History],
		       undo_loop(lists:sublist(History1,15)); %%KEEP 15 HISTORY VERSIONS. IS THIS ENOUGH?
	stop -> ok    
    end.
		       
