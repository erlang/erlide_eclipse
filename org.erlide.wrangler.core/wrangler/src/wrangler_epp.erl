%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%@hidden
%%@private
-module(wrangler_epp).

%% An Erlang code preprocessor.

-export([parse_file/3, parse_file/5]).
-export([interpret_file_attribute/1, expand_macros/2]).
-define(DEFAULT_TABWIDTH, 8).
-define(DEFAULT_FILEFORMAT, unix).

%% Epp state record.
-record(epp, {file,				%Current file
	      line={1,1},			%Current line number  %% modified by Huiqing Li
	      name="",				%Current file name
	      istk=[],				%Ifdef stack
	      sstk=[],				%State stack
	      path=[],				%Include-path
	      macs=dict:new(),			%Macros
	      uses=dict:new(),			%Macro use structure
	      tabwidth = ?DEFAULT_TABWIDTH,
	      fileformat=?DEFAULT_FILEFORMAT
	     }).

%%% Note on representation: as tokens, both {var, Line, Name} and
%%% {atom, Line, Name} can occur as macro identifiers. However, keeping
%%% this distinction here is done for historical reasons only: previously,
%%% ?FOO and ?'FOO' were not the same, but now they are. Removing the
%%% distinction in the internal representation would simplify the code
%%% a little.

open(File, Path, Pdm, TabWidth, FileFormat) ->
    Self = self(),
    Epp = spawn(fun() -> server(Self, File, Path, Pdm, TabWidth, FileFormat) end),
    epp_request(Epp).

close(Epp) ->
    %% Make sure that close is synchronous as a courtesy to test
    %% cases that test for resource leaks.
    Ref = erlang:monitor(process, Epp),
    R = epp_request(Epp, close),
    receive {'DOWN',Ref,_,_,_} -> ok end,
    R.

parse_erl_form(Epp) ->
    Res = epp_request(Epp, scan_erl_form), 
    case Res of
	{ok,Toks} ->
            case wrangler_parse:parse_form(Toks) of
                {ok, Form} ->
                    {ok, wrangler_epp_dodger:fix_pos_in_form(Toks, Form)};
                Other ->
                    Other
            end;
	Other ->
	    Other
    end.

final_state(Epp) ->
     epp_request(Epp, state).


parse_file(Ifile, Path, PreDefs) ->
    parse_file(Ifile, Path, PreDefs, ?DEFAULT_TABWIDTH, ?DEFAULT_FILEFORMAT).

parse_file(Ifile, Path, Predefs, TabWidth, FileFormat) ->
    case open(Ifile, Path, Predefs, TabWidth, FileFormat) of
	{ok,Epp} ->
	    Forms = parse_file(Epp),
	    #epp{macs=Ms, uses=UMs}= final_state(Epp),
	    close(Epp),
	    %% Modified by Huiqing, this function returns both the AST 
	    %% and macro infomation;
	    {ok,Forms, {dict:to_list(Ms), dict:to_list(UMs)}};
	{error,E} ->
	    {error,E}
    end.

parse_file(Epp) ->
    case parse_erl_form(Epp) of
	{ok,Form} ->
	    case Form of
		{attribute,La,record,{Record, Fields}} ->
		    case normalize_typed_record_fields(Fields) of
			{typed, NewFields} ->
			    [{attribute, La, record, {Record, NewFields}},
			     {attribute, La, type, 
			      {{record, Record}, Fields, []}}
			     |parse_file(Epp)];
			not_typed ->
			    [Form|parse_file(Epp)]
		    end;
		_ ->
		    [Form|parse_file(Epp)]
	    end;
	{error,E} ->
	    [{error,E}|parse_file(Epp)];
	{eof,Line} ->
	    [{eof,Line}]
    end.

normalize_typed_record_fields(Fields) ->
    normalize_typed_record_fields(Fields, [], false).

normalize_typed_record_fields([], NewFields, Typed) ->
    case Typed of
	true -> {typed, lists:reverse(NewFields)};
	false -> not_typed
    end;
normalize_typed_record_fields([{typed_record_field,Field,_}|Rest], 
			      NewFields, _Typed) ->
    normalize_typed_record_fields(Rest, [Field|NewFields], true);
normalize_typed_record_fields([Field|Rest], NewFields, Typed) ->
    normalize_typed_record_fields(Rest, [Field|NewFields], Typed).

server(Pid, Name, Path, Pdm, TabWidth, FileFormat) ->
    process_flag(trap_exit, true),
    case file:open(Name, [read]) of
	{ok,File} ->
	    Ms0 = predef_macros(Name),
	    case user_predef(Pdm, Ms0) of
		{ok,Ms1} ->
		    epp_reply(Pid, {ok,self()}),
		    St = #epp{file=File,name=Name,path=Path,macs=Ms1, tabwidth=TabWidth, fileformat=FileFormat},
		    From = wait_request(St),
		    enter_file_reply(From, Name, 1, 1),
		    wait_req_scan(St);
		{error,E} ->
		    epp_reply(Pid, {error,E})
	    end;
	{error,E} ->
	    epp_reply(Pid, {error,E})
    end.

predef_macros(File) ->
    Ms0 = dict:new(),
    Ms1 = dict:store({atom,'FILE'}, {none,[{string,1,File}]}, Ms0),
    Ms2 = dict:store({atom,'LINE'}, {none,[{integer,1,1}]}, Ms1),
    Ms3 = dict:store({atom,'MODULE'}, undefined, Ms2),
    Ms4 = dict:store({atom,'MODULE_STRING'}, undefined, Ms3),
    Ms5 = dict:store({atom,'BASE_MODULE'}, undefined, Ms4),
    Ms6 = dict:store({atom,'BASE_MODULE_STRING'}, undefined, Ms5),
    Machine = list_to_atom(erlang:system_info(machine)),
    Ms7 = dict:store({atom,'MACHINE'}, {none,[{atom,1,Machine}]}, Ms6),
    dict:store({atom,Machine}, {none,[{atom,1,true}]}, Ms7).

%% user_predef(PreDefMacros, Macros) ->
%%	{ok,MacroDict} | {error,E}
%%  Add the predefined macros to the macros dictionary. A macro without a
%%  value gets the value 'true'.

user_predef([{M,Val,redefine}|Pdm], Ms) when is_atom(M) ->
    Exp = wrangler_parse:tokens(wrangler_parse:abstract(Val)),
    user_predef(Pdm, dict:store({atom,M}, {none,Exp}, Ms));
user_predef([{M,Val}|Pdm], Ms) when is_atom(M) ->
    case dict:find({atom,M}, Ms) of
	{ok,_Defs} when is_list(_Defs) -> %% User defined macros
	    {error,{redefine,M}};
	{ok,_Def} -> %% Predefined macros
	    {error,{redefine_predef,M}};
	error ->
	    Exp = wrangler_parse:tokens(wrangler_parse:abstract(Val)),
	    user_predef(Pdm, dict:store({atom,M}, [{none, {none,Exp}}], Ms))
    end;
user_predef([M|Pdm], Ms) when is_atom(M) ->
    case dict:find({atom,M}, Ms) of
	{ok,_Defs} when is_list(_Defs) -> %% User defined macros
	    {error,{redefine,M}};
	{ok,_Def} -> %% Predefined macros
	    {error,{redefine_predef,M}};
	error ->
	    user_predef(Pdm,
	                dict:store({atom,M}, [{none, {none,[{atom,1,true}]}}], Ms))
    end;
user_predef([Md|_Pdm], _Ms) -> {error,{bad,Md}};
user_predef([], Ms) -> {ok,Ms}.


wait_request(St) ->
    receive
	{epp_request,From,scan_erl_form} -> From;
	%% Temporally added;
	{epp_request, From, state} ->
	    epp_reply(From, St),
	    wait_request(St);
	%% End  of temporally added.
	{epp_request,From,macro_defs} ->
	    epp_reply(From, dict:to_list(St#epp.macs)),
	    wait_request(St);
	{epp_request,From,close} ->
	    ok = file:close(St#epp.file),
	    epp_reply(From, ok),
	    exit(normal);
	{'EXIT',_,R} ->
	    exit(R);
	_Other ->
	    wait_request(St)
    end.

wait_req_scan(St) ->
    From = wait_request(St),
    scan_toks(From, St).

wait_req_skip(St, Sis) ->
    From = wait_request(St),
    skip_toks(From, St, Sis).
	
%% enter_file(Path, FileName, IncludeLine, From, EppState)
%% leave_file(From, EppState)
%%  Handle entering and leaving included files. Notify caller when the
%%  current file is changed. Note it is an error to exit a file if we are
%%  in a conditional. These functions never return.

enter_file(_Path, _NewName, Li, From, St)
  when length(St#epp.sstk) >= 8 ->
    epp_reply(From, {error,{Li,epp,{depth,"include"}}}),
    wait_req_scan(St);
enter_file(Path, NewName, Li, From, St) ->
    case file:path_open(Path, NewName, [read]) of
	{ok,NewF,Pname} ->
	    wait_req_scan(enter_file2(NewF, Pname, From, St, 1));
	{error,_E} ->
	    epp_reply(From, {error,{Li,epp,{include,file,NewName}}}),
	    wait_req_scan(St)
    end.

%% enter_file2(File, FullName, From, EppState, AtLine) -> EppState.
%%  Set epp to use this file and "enter" it.

enter_file2(NewF, Pname, From, St, AtLine) ->
    enter_file2(NewF, Pname, From, St, AtLine, []).

enter_file2(NewF, Pname, From, St, AtLine, ExtraPath) ->
    enter_file_reply(From, Pname, 1, AtLine),
    Ms = dict:store({atom,'FILE'}, {none,[{string,1,Pname}]}, St#epp.macs),
    Path = St#epp.path ++ ExtraPath,
    #epp{file=NewF,name=Pname,sstk=[St|St#epp.sstk],path=Path,macs=Ms}.

enter_file_reply(From, Name, Line, AtLine) ->
    Rep = {ok, [{'-',AtLine},{atom,AtLine,file},{'(',AtLine},
  		{string,AtLine,file_name(Name)},{',',AtLine},
  		{integer,AtLine,Line},{')',Line},{dot,AtLine}]},
    epp_reply(From, Rep).
    
    

%% Flatten filename to a string. Must be a valid filename.

file_name([C | T]) when is_integer(C), C > 0, C =< 255 ->
    [C | file_name(T)];
file_name([H|T]) ->
    file_name(H) ++ file_name(T);
file_name([]) ->
    [];
file_name(N) when is_atom(N) ->
    atom_to_list(N).

leave_file(From, St) ->
    case St#epp.istk of
	[I|Cis] -> 
	    epp_reply(From,
		      {error,{St#epp.line,epp, {illegal,"unterminated",I}}}),
	    leave_file(wait_request(St),St#epp{istk=Cis});
	[] ->
	    case St#epp.sstk of
		[OldSt|Sts] ->
		    ok = file:close(St#epp.file),
		    enter_file_reply(From, OldSt#epp.name, 
                                     OldSt#epp.line, OldSt#epp.line),
		    Ms = dict:store({atom,'FILE'},
				    {none,
				     [{string,OldSt#epp.line,OldSt#epp.name}]},
				    St#epp.macs),
		    wait_req_scan(OldSt#epp{sstk=Sts,macs=Ms});
		[] ->
		    epp_reply(From, {eof,St#epp.line}),
		    wait_req_scan(St)
	    end
    end.

%% scan_toks(From, EppState)
%% scan_toks(Tokens, From, EppState)

scan_toks(From, St) ->
    Res=  wrangler_io:scan_erl_form(St#epp.file, '', St#epp.line, St#epp.tabwidth, St#epp.fileformat),
    case Res  of
	{ok,Toks,Cl} ->
	    scan_toks(Toks, From, St#epp{line=Cl});
	{error,E,Cl} ->
	    epp_reply(From, {error,E}),
	    wait_req_scan(St#epp{line=Cl});
	{eof,Cl} ->
	    leave_file(From, St#epp{line=Cl});
	{error,_E} ->
            epp_reply(From, {error,{St#epp.line,epp,cannot_parse}}),
	    leave_file(From, St)		%This serious, just exit!
    end.

scan_toks([{'-',_Lh},{atom,Ld,define}|Toks], From, St) ->
    scan_define(Toks, Ld, From, St);
scan_toks([{'-',_Lh},{atom,Ld,undef}|Toks], From, St) ->
    scan_undef(Toks, Ld, From, St);
scan_toks([{'-',_Lh},{atom,Li,include}|Toks], From, St) ->
    scan_include(Toks, {abs(element(1, Li)), element(2, Li)}, From, St);
scan_toks([{'-',_Lh},{atom,Li,include_lib}|Toks], From, St) ->
    scan_include_lib(Toks, {abs(element(1, Li)), element(2, Li)}, From, St);
scan_toks([{'-',_Lh},{atom,Li,ifdef}|Toks], From, St) ->
    scan_ifdef(Toks, Li, From, St);
scan_toks([{'-',_Lh},{atom,Li,ifndef}|Toks], From, St) ->
    scan_ifndef(Toks, Li, From, St);
scan_toks([{'-',_Lh},{atom,Le,else}|Toks], From, St) ->
    scan_else(Toks, Le, From, St);
scan_toks([{'-',_Lh},{atom,Le,'if'}|Toks], From, St) ->
    scan_if(Toks, Le, From, St);
scan_toks([{'-',_Lh},{atom,Le,elif}|Toks], From, St) ->
    scan_elif(Toks, Le, From, St);
scan_toks([{'-',_Lh},{atom,Le,endif}|Toks], From, St) ->
    scan_endif(Toks, Le, From, St);
scan_toks([{'-',_Lh},{atom,Lf,file}|Toks0], From, St) ->
    case catch expand_macros(Toks0, {St#epp.macs, St#epp.uses}) of
	Toks1 when is_list(Toks1) ->
	    scan_file_1(Toks1, Lf, From, St);
	{error,ErrL,What} ->
	    epp_reply(From, {error,{ErrL,epp,What}}),
	    wait_req_scan(St)
    end;
scan_toks(Toks0, From, St) ->
    case catch expand_macros(Toks0, {St#epp.macs, St#epp.uses}) of
	Toks1 when is_list(Toks1) ->
	    epp_reply(From, {ok,Toks1}),
	    wait_req_scan(St#epp{macs=scan_module(Toks1, St#epp.macs)});
	{error,ErrL,What} ->
	    epp_reply(From, {error,{ErrL,epp,What}}),
	    wait_req_scan(St)
    end.

scan_module([{'-',_Lh},{atom,_Lm,module},{'(',_Ll}|Ts], Ms) ->
    scan_module_1(Ts, [], Ms);
scan_module([{'-',_Lh},{atom,_Lm,extends},{'(',_Ll}|Ts], Ms) ->
    scan_extends(Ts, [], Ms);
scan_module(_Ts, Ms) -> Ms.

scan_module_1([{atom,_,_}=A,{',',L}|Ts], As, Ms) ->
    %% Parameterized modules.
    scan_module_1([A,{')',L}|Ts], As, Ms);
scan_module_1([{atom,Ln,A},{')',_Lr}|_Ts], As, Ms0) ->
    Mod = lists:concat(lists:reverse([A|As])),
    Ms = dict:store({atom,'MODULE'},
		     {none,[{atom,Ln,list_to_atom(Mod)}]}, Ms0),
    dict:store({atom,'MODULE_STRING'}, {none,[{string,Ln,Mod}]}, Ms);
scan_module_1([{atom,_Ln,A},{'.',_Lr}|Ts], As, Ms) ->
    scan_module_1(Ts, [".",A|As], Ms);
scan_module_1([{'.',_Lr}|Ts], As, Ms) ->
    scan_module_1(Ts, As, Ms);
scan_module_1(_Ts, _As, Ms) -> Ms.

scan_extends([{atom,Ln,A},{')',_Lr}|_Ts], As, Ms0) ->
    Mod = lists:concat(lists:reverse([A|As])),
    Ms = dict:store({atom,'BASE_MODULE'},
		     {none,[{atom,Ln,list_to_atom(Mod)}]}, Ms0),
    dict:store({atom,'BASE_MODULE_STRING'}, {none,[{string,Ln,Mod}]}, Ms);
scan_extends([{atom,_Ln,A},{'.',_Lr}|Ts], As, Ms) ->
    scan_extends(Ts, [".",A|As], Ms);
scan_extends([{'.',_Lr}|Ts], As, Ms) ->
    scan_extends(Ts, As, Ms);
scan_extends(_Ts, _As, Ms) -> Ms.

%% scan_define(Tokens, DefineLine, From, EppState)

scan_define([{'(',_Lp},{Type,_Lm,M}=Mac,{',',Lc}|Toks], _Def, From, St)
  when Type =:= atom; Type =:= var ->
    case catch macro_expansion(Toks, Lc) of
        Expansion when is_list(Expansion) ->
            case dict:find({atom,M}, St#epp.macs) of
                {ok, Defs} when is_list(Defs) ->
                    %% User defined macros: can be overloaded
                    case proplists:is_defined(none, Defs) of
                        true ->
                            epp_reply(From, {error,{loc(Mac),epp,{redefine,M}}}),
                            wait_req_scan(St);
                        false ->
                            scan_define_cont(From, St,
                                             {atom, M},
                                             {none, {none,Expansion}})
                    end;
                {ok, _PreDef} ->
                    %% Predefined macros: cannot be overloaded
                    epp_reply(From, {error,{loc(Mac),epp,{redefine_predef,M}}}),
                    wait_req_scan(St);
                error ->
                    scan_define_cont(From, St,
                                     {atom, M},
                                     {none, {none,Expansion}})
            end;
        {error,ErrL,What} ->
            epp_reply(From, {error,{ErrL,epp,What}}),
            wait_req_scan(St)
    end;
scan_define([{'(',_Lp},{Type,_Lm,M}=Mac,{'(',_Lc}|Toks], Def, From, St)
  when Type =:= atom; Type =:= var ->
    case catch macro_pars(Toks, []) of
        {ok, {As,Me}} ->
            Len = length(As),
            case dict:find({atom,M}, St#epp.macs) of
                {ok, Defs} when is_list(Defs) ->
                    %% User defined macros: can be overloaded
                    case proplists:is_defined(Len, Defs) of
                        true ->
                            epp_reply(From,{error,{loc(Mac),epp,{redefine,M}}}),
                            wait_req_scan(St);
                        false ->
                            scan_define_cont(From, St, {atom, M},
                                             {Len, {As, Me}})
                    end;
                {ok, _PreDef} ->
                    %% Predefined macros: cannot be overloaded
                    %% (There are currently no predefined F(...) macros.)
                    epp_reply(From, {error,{loc(Mac),epp,{redefine_predef,M}}}),
                    wait_req_scan(St);
                error ->
                    scan_define_cont(From, St, {atom, M}, {Len, {As, Me}})
            end;
	{error,ErrL,What} ->
            epp_reply(From, {error,{ErrL,epp,What}}),
            wait_req_scan(St);
        _ ->
            epp_reply(From, {error,{loc(Def),epp,{bad,define}}}),
            wait_req_scan(St)
    end;
scan_define(_Toks, Def, From, St) ->
    epp_reply(From, {error,{loc(Def),epp,{bad,define}}}),
    wait_req_scan(St).

%%% Detection of circular macro expansions (which would either keep
%%% the compiler looping forever, or run out of memory):
%%% When a macro is defined, we store the names of other macros it
%%% uses in St#epp.uses. If any macro is undef'ed, that information
%%% becomes invalid, so we redo it for all remaining macros.
%%% The circularity detection itself is done when a macro is expanded:
%%% the information from St#epp.uses is traversed, and if a circularity
%%% is detected, an error message is thrown.

scan_define_cont(F, St, M, {Arity, Def}) ->
    Ms = dict:append_list(M, [{Arity, Def}], St#epp.macs),
    try dict:append_list(M, [{Arity, macro_uses(Def)}], St#epp.uses) of
        U ->
            scan_toks(F, St#epp{uses=U, macs=Ms})
    catch
        {error, Line, Reason} ->
            epp_reply(F, {error,{Line,epp,Reason}}),
            wait_req_scan(St)
    end.

macro_uses({_Args, Tokens}) ->
    Uses0 = macro_ref(Tokens),
    lists:usort(Uses0).

macro_ref([]) ->
    [];
macro_ref([{'?', _}, {'?', _} | Rest]) ->
    macro_ref(Rest);
macro_ref([{'?', _}, {atom, _, A} | Rest]) ->
    [{atom, A} | macro_ref(Rest)];
macro_ref([{'?', _}, {var, _, A} | Rest]) ->
    [{atom, A} | macro_ref(Rest)];
macro_ref([_Token | Rest]) ->
    macro_ref(Rest).

%% all_macro_uses(D0) ->
%%     L = dict:to_list(D0),
%%     D = dict:new(),
%%     add_macro_uses(L, D).

%% add_macro_uses([], D) ->
%%     D;
%% add_macro_uses([{Key, Def} | Rest], D0) ->
%%     add_macro_uses(Rest, dict:store(Key, macro_uses(Def), D0)).

%% scan_undef(Tokens, UndefLine, From, EppState)
scan_undef([{'(',_Llp},{atom,_Lm,M},{')',_Lrp},{dot,_Ld}], _Undef, From, St) ->
    Macs = dict:erase({atom,M}, St#epp.macs),
    Uses = dict:erase({atom,M}, St#epp.uses),
    scan_toks(From, St#epp{macs=Macs, uses=Uses});
scan_undef([{'(',_Llp},{var,_Lm,M},{')',_Lrp},{dot,_Ld}], _Undef, From,St) ->
    Macs = dict:erase({atom,M}, St#epp.macs),
    Uses = dict:erase({atom,M}, St#epp.uses),
    scan_toks(From, St#epp{macs=Macs, uses=Uses});
scan_undef(_Toks, Undef, From, St) ->
    epp_reply(From, {error,{loc(Undef),epp,{bad,undef}}}),
    wait_req_scan(St).
%% scan_include(Tokens, IncludeLine, From, St)

scan_include([{'(',_Llp},{string,_Lf,NewName0},{')',_Lrp},{dot,_Ld}], Li, 
	     From, St) ->
    NewName = expand_var(NewName0),
    enter_file(St#epp.path, NewName, Li, From, St);
scan_include(_Toks, Li, From, St) ->
    epp_reply(From, {error,{Li,epp,{bad,include}}}),
    wait_req_scan(St).

%% scan_include_lib(Tokens, IncludeLine, From, EppState)
%%  For include_lib we first test if we can find the file through the
%%  normal search path, if not we assume that the first directory name
%%  is a library name, find its true directory and try with that.

find_lib_dir(NewName) ->
    [Lib | Rest] = filename:split(NewName),
    {code:lib_dir(list_to_atom(Lib)), Rest}.

scan_include_lib([{'(',_Llp},{string,_Lf,_NewName0},{')',_Lrp},{dot,_Ld}], Li,
		 From, St)
  when length(St#epp.sstk) >= 8 ->
    epp_reply(From, {error,{Li,epp,{depth,"include_lib"}}}),
    wait_req_scan(St);
scan_include_lib([{'(',_Llp},{string,_Lf,NewName0},{')',_Lrp},{dot,_Ld}], Li,
		 From, St) ->
    NewName = expand_var(NewName0),
    case file:path_open(St#epp.path, NewName, [read]) of
	{ok,NewF,Pname} ->
	    wait_req_scan(enter_file2(NewF, Pname, From, St, 1));
	{error,_E1} ->
	    case catch find_lib_dir(NewName) of
		{LibDir, Rest} when is_list(LibDir) ->
		    LibName = filename:join([LibDir | Rest]),
		    case file:open(LibName, [read]) of
			{ok,NewF} ->
			    ExtraPath = [filename:dirname(LibName)],
			    wait_req_scan(enter_file2(NewF, LibName, From, 
                                                      St, 1, ExtraPath));
			{error,_E2} ->
			    epp_reply(From,
				      {error,{Li,epp,{include,lib,NewName}}}),
			    wait_req_scan(St)
		    end;
		_Error ->
		    epp_reply(From, {error,{Li,epp,{include,lib,NewName}}}),
		    wait_req_scan(St)
	    end
    end;
scan_include_lib(_Toks, Li, From, St) ->
    epp_reply(From, {error,{Li,epp,{bad,include_lib}}}),
    wait_req_scan(St).

%% scan_ifdef(Tokens, IfdefLine, From, EppState)
%% scan_ifndef(Tokens, IfdefLine, From, EppSate)
%%  Handle the conditional parsing of a file.
%%  Report a badly formed if[n]def test and then treat as undefined macro.

scan_ifdef([{'(',_Llp},{Key,_Lm,M},{')',_Lrp},{dot,_Ld}], _Li, From, St)
    when Key == atom orelse Key==var ->
    case dict:find({atom,M}, St#epp.macs) of
	{ok,_Def} ->
	    scan_toks(From, St#epp{istk=[ifdef|St#epp.istk]});
	error ->
	    skip_toks(From, St, [ifdef])
    end;

scan_ifdef(_Toks, Li, From, St) ->
    epp_reply(From, {error,{Li,epp,{bad,ifdef}}}),
    wait_req_skip(St, [ifdef]).

scan_ifndef([{'(',_Llp},{Key,_Lm,M},{')',_Lrp},{dot,_Ld}], _Li, From, St) when
      Key == atom orelse Key == var ->
    case dict:find({atom,M}, St#epp.macs) of
	{ok,_Def} ->
	    skip_toks(From, St, [ifndef]);
	error ->
	    scan_toks(From, St#epp{istk=[ifndef|St#epp.istk]})
    end;
scan_ifndef(_Toks, Li, From, St) ->
    epp_reply(From, {error,{Li,epp,{bad,ifndef}}}),
    wait_req_scan(St).

%% scan_else(Tokens, ElseLine, From, EppState)
%%  If we are in an if body then convert to else and skip, if we are in an
%%  else or not in anything report an error.

scan_else([{dot,_Ld}], Le, From, St) ->
    case St#epp.istk of
	[else|Cis] ->
	    epp_reply(From, {error,{Le,epp,{illegal,"repeated",else}}}),
	    wait_req_skip(St#epp{istk=Cis}, [else]);
	[_I|Cis] ->
	    skip_toks(From, St#epp{istk=Cis}, [else]);
	[] ->
	    epp_reply(From, {error,{Le,epp,{illegal,"unbalanced",else}}}),
	    wait_req_scan(St)
    end;
scan_else(_Toks, Le, From, St) ->
    epp_reply(From, {error,{Le,epp,{bad,else}}}),
    wait_req_scan(St).

%% scan_if(Tokens, EndifLine, From, EppState)
%%  Handle the conditional parsing of a file.
%%  Report a badly formed if test and then treat as false macro.

scan_if(_Toks, Le, From, St) ->
    epp_reply(From, {error,{Le,epp,{'NYI','if'}}}),
    wait_req_skip(St, ['if']).

%% scan_elif(Tokens, EndifLine, From, EppState)
%%  Handle the conditional parsing of a file.
%%  Report a badly formed if test and then treat as false macro.

scan_elif(_Toks, Le, From, St) ->
    epp_reply(From, {error,{Le,epp,{'NYI','elif'}}}),
    wait_req_skip(St, ['elif']).

%% scan_endif(Tokens, EndifLine, From, EppState)
%%  If we are in an if body then exit it, else report an error.

scan_endif([{dot,_Ld}], Le, From, St) ->
    case St#epp.istk of
	[_I|Cis] ->
	    scan_toks(From, St#epp{istk=Cis});
	[] ->
	    epp_reply(From, {error,{Le,epp,{illegal,"unbalanced",endif}}}),
	    wait_req_scan(St)
    end;
scan_endif(_Toks, Le, From, St) ->
    epp_reply(From, {error,{Le,epp,{bad,endif}}}),
    wait_req_scan(St).

%% scan_file_1(Tokens, FileLine, From, EppState)
%%  Set the current file and line to the given file and line.
%%  Note that the line of the attribute itself is kept.

scan_file_1([{'(',_Llp},{string,_Ls,Name},{',',_Lc},{integer,_Li,Ln},{')',_Lrp},
           {dot,_Ld}], Lf, From, St) ->
    enter_file_reply(From, Name, Ln, {-abs(element(1, Lf)), element(2, Lf)}),
    Ms = dict:store({atom,'FILE'}, {none,[{string,1,Name}]}, St#epp.macs),
    {L, C} = St#epp.line,
    %% scan_toks(From, St#epp{name=Name,line={list_to_integer(Ln)+(L-element(1, Lf)), C},macs=Ms});
    %% the above does not work for reason I don't know.
    wait_req_scan(St#epp{name=Name,line={list_to_integer(Ln)+(L-element(1, Lf)), C},macs=Ms});
       
    
scan_file_1(_Toks, Lf, From, St) ->
    epp_reply(From, {error,{Lf,epp,{bad,file}}}),
    wait_req_scan(St).

%% skip_toks(From, EppState, SkipIstack)
%%  Skip over forms until current conditional has been exited. Handle
%%  nested conditionals and repeated 'else's.

skip_toks(From, St, [I|Sis]) ->
    case wrangler_io:scan_erl_form(St#epp.file, '', St#epp.line, St#epp.tabwidth, St#epp.fileformat) of
	{ok,[{'-',_Lh},{atom,_Li,ifdef}|_Toks],Cl} ->
	    skip_toks(From, St#epp{line=Cl}, [ifdef,I|Sis]);
	{ok,[{'-',_Lh},{atom,_Li,ifndef}|_Toks],Cl} ->
	    skip_toks(From, St#epp{line=Cl}, [ifndef,I|Sis]);
	{ok,[{'-',_Lh},{atom,_Li,'if'}|_Toks],Cl} ->
	    skip_toks(From, St#epp{line=Cl}, ['if',I|Sis]);
	{ok,[{'-',_Lh},{atom,Le,else}|_Toks],Cl}->
	    skip_else(Le, From, St#epp{line=Cl}, [I|Sis]);
	{ok,[{'-',_Lh},{atom,_Le,endif}|_Toks],Cl} ->
	    skip_toks(From, St#epp{line=Cl}, Sis);
	{ok,_Toks,Cl} ->
	    skip_toks(From, St#epp{line=Cl}, [I|Sis]);
	{error,_E,Cl} ->
	    skip_toks(From, St#epp{line=Cl}, [I|Sis]);
	{eof,Cl} ->
	    leave_file(From, St#epp{line=Cl,istk=[I|Sis]});
	{error,_E} ->
            epp_reply(From, {error,{St#epp.line,epp,cannot_parse}}),
	    leave_file(From, St)		%This serious, just exit!
    end;
skip_toks(From, St, []) ->
    scan_toks(From, St).

skip_else(Le, From, St, [else|Sis]) ->
    epp_reply(From, {error,{Le,epp,{illegal,"repeated",else}}}),
    wait_req_skip(St, [else|Sis]);
skip_else(_Le, From, St, [_I]) ->
    scan_toks (From, St#epp{istk=[else|St#epp.istk]});
skip_else(_Le, From, St, Sis) ->
    skip_toks(From, St, Sis).



%% macro_pars(Tokens, ArgStack)
%% macro_expansion(Tokens)
%%  Extract the macro parameters and the expansion from a macro definition.


macro_pars([{')',_Lp}, {',',Ld}|Ex], Args) ->
    {ok, {lists:reverse(Args), macro_expansion(Ex, Ld)}};
macro_pars([{var,_,Name}, {')',_Lp}, {',',Ld}|Ex], Args) ->
    false = lists:member(Name, Args),		%Prolog is nice
    {ok, {lists:reverse([Name|Args]), macro_expansion(Ex, Ld)}};
macro_pars([{var,_L,Name}, {',',_}|Ts], Args) ->
    false = lists:member(Name, Args),
    macro_pars(Ts, [Name|Args]).

macro_expansion([{')',_Lp},{dot,_Ld}], _L0) -> [];
macro_expansion([{dot,Ld}], _L0) -> throw({error,Ld,missing_parenthesis});
macro_expansion([T|Ts], _L0) ->
    [T|macro_expansion(Ts, element(2, T))];
macro_expansion([], L0) -> throw({error,L0,premature_end}).

%% expand_macros(Tokens, Macros)
%% expand_macro(Tokens, MacroLine, RestTokens)
%%  Expand the macros in a list of tokens, making sure that an expansion
%%  gets the same line number as the macro call.


expand_macros(Type, MacT, M, Toks, Ms0) ->
    %% (Type will always be 'atom')
    {Ms, U} = Ms0,
    Lm = loc(MacT),
    Tinfo = element(2, MacT),
    case expand_macro1(Type, Lm, M, Toks, Ms) of
	{ok,{none,Exp}} ->
            check_uses([{{Type,M}, none}], [], U, Lm),
	    Toks1 = expand_macros(expand_macro(Exp, Tinfo, [], dict:new()), Ms0),
	    expand_macros(Toks1++Toks, Ms0);
	{ok,{As,Exp}} ->
            check_uses([{{Type,M}, length(As)}], [], U, Lm),
	    {Bs,Toks1} = bind_args(Toks, Lm, M, As, dict:new()),
	    expand_macros(expand_macro(Exp, Tinfo, Toks1, Bs), Ms0)
    end.

expand_macro1(Type, Lm, M, Toks, Ms) ->
    Arity = count_args(Toks, Lm, M),
    case dict:find({Type,M}, Ms) of
        error -> %% macro not found
            throw({error,Lm,{undefined,M,Arity}});
        {ok, undefined} -> %% Predefined macro without definition
            throw({error,Lm,{undefined,M,Arity}});
        {ok, [{none, Def}]} ->
            {ok, Def};
        {ok, Defs} when is_list(Defs) ->
            case proplists:get_value(Arity, Defs) of
                undefined ->
                    throw({error,Lm,{mismatch,M}});
                Def ->
                    {ok, Def}
            end;
        {ok, PreDef} -> %% Predefined macro
            {ok, PreDef}
    end.

check_uses(undefined, _Anc, _U, _Lm) ->
    ok;
check_uses([], _Anc, _U, _Lm) ->
    ok;
check_uses([M|Rest], Anc, U, Lm) ->
    case lists:member(M, Anc) of
	true ->
	    {_, Name} = M,
	    throw({error,Lm,{circular,Name}});
	false ->
	    L = get_macro_uses(M, U),
	    check_uses(L, [M|Anc], U, Lm),
	    check_uses(Rest, Anc, U, Lm)
    end.
    
get_macro_uses({M,Arity}, U) ->
    case dict:find(M, U) of
	error ->
	    [];
	{ok, L} ->
	    proplists:get_value(Arity, L, proplists:get_value(none, L, []))
    end.


%% Macro expansio
expand_macros([{'?',_Lq},{atom,_Lm,M}=MacT|Toks], Ms) ->
    expand_macros(atom, MacT, M, Toks, Ms);
%% Special macros
expand_macros([{'?',_Lq},{var,Lm,'LINE'}|Toks], Ms) ->
    [{integer,Lm,Lm}|expand_macros(Toks, Ms)];
expand_macros([{'?',_Lq},{var,_Lm,M}=MacT|Toks], Ms) ->
    expand_macros(atom, MacT, M, Toks, Ms);
%%Illegal macros
expand_macros([{'?',_Lq},{Type,Lt}|_Toks], _Ms) ->
     throw({error,Lt,{call,[$?|atom_to_list(Type)]}});
expand_macros([{'?',_Lq},{_Type,Lt,What}|_Toks], _Ms) ->
     throw({error,Lt,{call,[$?|io_lib:write(What)]}});

expand_macros([T|Ts], Ms) ->
    [T|expand_macros(Ts, Ms)];
expand_macros([], _Ms) -> [].

%% bind_args(Tokens, MacroLine, MacroName, ArgumentVars, Bindings)
%%  Collect the arguments to a macro call and check for correct number.

bind_args([{'(',_Llp},{')',_Lrp}|Toks], _Lm, _M, [], Bs) ->
    {Bs,Toks};
bind_args([{'(',_Llp}|Toks0], Lm, M, [A|As], Bs) ->
    {Arg,Toks1} = macro_arg(Toks0, [], []),
    macro_args(Toks1, Lm, M, As, store_arg(Lm, M, A, Arg, Bs));
bind_args(_Toks, Lm, M, _As, _Bs) ->
    throw({error,Lm,{mismatch,M}}). % Cannot happen.

macro_args([{')',_Lrp}|Toks], _Lm, _M, [], Bs) ->
    {Bs,Toks};
macro_args([{',',_Lc}|Toks0], Lm, M, [A|As], Bs) ->
    {Arg,Toks1} = macro_arg(Toks0, [], []),
    macro_args(Toks1, Lm, M, As, store_arg(Lm, M, A, Arg, Bs));
macro_args([], Lm, M, _As, _Bs) ->
    throw({error,Lm,{arg_error,M}}); % Cannot happen.
macro_args(_Toks, Lm, M, _As, _Bs) ->
    throw({error,Lm,{mismatch,M}}). % Cannot happen.

store_arg(L, M, _A, [], _Bs) ->
    throw({error,L,{mismatch,M}});
store_arg(_L, _M, A, Arg, Bs) ->
    dict:store(A, Arg, Bs).

%% count_args(Tokens, MacroLine, MacroName)
%%  Count the number of arguments in a macro call.
count_args([{'(', _Llp},{')',_Lrp}|_Toks], _Lm, _M) ->
    0;
count_args([{'(', _Llp},{',',_Lc}|_Toks], Lm, M) ->
    throw({error,Lm,{arg_error,M}});
count_args([{'(',_Llp}|Toks0], Lm, M) ->
    {_Arg,Toks1} = macro_arg(Toks0, [], []),
    count_args(Toks1, Lm, M, 1);
count_args(_Toks, _Lm, _M) ->
    none.



count_args([{')',_Lrp}|_Toks], _Lm, _M, NbArgs) ->
    NbArgs;
count_args([{',',_Lc},{')',_Lrp}|_Toks], Lm, M, _NbArgs) ->
    throw({error,Lm,{arg_error,M}});
count_args([{',',_Lc}|Toks0], Lm, M, NbArgs) ->
    {_Arg,Toks1} = macro_arg(Toks0, [], []),
    count_args(Toks1, Lm, M, NbArgs+1);
count_args([], Lm, M, _NbArgs) ->
    throw({error,Lm,{arg_error,M}});
count_args(_Toks, Lm, M, _NbArgs) ->
    throw({error,Lm,{mismatch,M}}). % Cannot happen.


%% macro_arg([Tok], [ClosePar], [ArgTok]) -> {[ArgTok],[RestTok]}.
%%  Collect argument tokens until we hit a ',' or a ')'. We know a
%%  enough about syntax to recognise "open parentheses" and keep
%%  scanning until matching "close parenthesis".


macro_arg([{',',Lc}|Toks], [], Arg) ->
    {lists:reverse(Arg),[{',',Lc}|Toks]};
macro_arg([{')',Lrp}|Toks], [], Arg) ->
    {lists:reverse(Arg),[{')',Lrp}|Toks]};
macro_arg([{'(',Llp}|Toks], E, Arg) ->
    macro_arg(Toks, [')'|E], [{'(',Llp}|Arg]);
macro_arg([{'<<',Lls}|Toks], E, Arg) ->
    macro_arg(Toks, ['>>'|E], [{'<<',Lls}|Arg]);
macro_arg([{'[',Lls}|Toks], E, Arg) ->
    macro_arg(Toks, [']'|E], [{'[',Lls}|Arg]);
macro_arg([{'{',Llc}|Toks], E, Arg) ->
    macro_arg(Toks, ['}'|E], [{'{',Llc}|Arg]);
macro_arg([{'begin',Lb}|Toks], E, Arg) ->
    macro_arg(Toks, ['end'|E], [{'begin',Lb}|Arg]);
macro_arg([{'if',Li}|Toks], E, Arg) ->
    macro_arg(Toks, ['end'|E], [{'if',Li}|Arg]);
macro_arg([{'case',Lc}|Toks], E, Arg) ->
    macro_arg(Toks, ['end'|E], [{'case',Lc}|Arg]);
macro_arg([{'fun',Lc}|[{'(',_}|_]=Toks], E, Arg) ->
    macro_arg(Toks, ['end'|E], [{'fun',Lc}|Arg]);
macro_arg([{'receive',Lr}|Toks], E, Arg) ->
    macro_arg(Toks, ['end'|E], [{'receive',Lr}|Arg]);
macro_arg([{'try',Lr}|Toks], E, Arg) ->
    macro_arg(Toks, ['end'|E], [{'try',Lr}|Arg]);
macro_arg([{'cond',Lr}|Toks], E, Arg) ->
    macro_arg(Toks, ['end'|E], [{'cond',Lr}|Arg]);
macro_arg([{'query',Lr}|Toks], E, Arg) ->
    macro_arg(Toks, ['end'|E], [{'query',Lr}|Arg]);
macro_arg([{Rb,Lrb}|Toks], [Rb|E], Arg) ->	%Found matching close
    macro_arg(Toks, E, [{Rb,Lrb}|Arg]);
macro_arg([T|Toks], E, Arg) ->
    macro_arg(Toks, E, [T|Arg]);
macro_arg([], _E, Arg) ->
    {lists:reverse(Arg),[]}.

%% expand_macro(MacroDef, MacroLine, RestTokens, Bindings)
%% expand_arg(Argtokens, MacroTokens, MacroLine, RestTokens, Bindings)
%%  Insert the macro expansion replacing macro parameters with their
%%  argument values, inserting the line number of first the macro call
%%  and then the macro arguments, i.e. simulate textual expansion.

expand_macro([{var,_Lv,V}|Ts], L, Rest, Bs) ->
    case dict:find(V, Bs) of
	{ok,Val} ->
	    %% lists:append(Val, expand_macro(Ts, L, Rest, Bs));
	    expand_arg(Val, Ts, L, Rest, Bs);
	error ->
	    [{var,L,V}|expand_macro(Ts, L, Rest, Bs)]
    end;
expand_macro([{'?', _}, {'?', _}, {var,_Lv,V}|Ts], L, Rest, Bs) ->
    case dict:find(V, Bs) of
	{ok,Val} ->
	    %% lists:append(Val, expand_macro(Ts, L, Rest, Bs));
            expand_arg(stringify(Val, L), Ts, L, Rest, Bs);
	error ->
	    [{var,L,V}|expand_macro(Ts, L, Rest, Bs)]
    end;
expand_macro([T|Ts], L, Rest, Bs) ->
    [setelement(2, T, L)|expand_macro(Ts, L, Rest, Bs)];
expand_macro([], _L, Rest, _Bs) -> Rest.


expand_arg([A|As], Ts, _L, Rest, Bs) ->
    [A|expand_arg(As, Ts, element(2, A), Rest, Bs)];
expand_arg([], Ts, L, Rest, Bs) ->
    expand_macro(Ts, L, Rest, Bs).

%%% stringify(L) returns a list of one token: a string which when
%%% tokenized would yield the token list L.

%tst(Str) ->
%    {ok, T, _} = erl_scan:string(Str),
%    [{string, _, S}] = stringify(T),
%    S.

token_src({dot, _}) ->
    ".";
token_src({X, _}) when is_atom(X) ->
    atom_to_list(X);
token_src({var, _, X}) ->
    atom_to_list(X);
token_src({string, _, X}) ->
    lists:flatten(io_lib:format("~p", [X]));
token_src({_, _, X}) ->
    lists:flatten(io_lib:format("~w", [X])).

stringify1([]) ->
    [];
stringify1([T | Tokens]) ->
    [io_lib:format(" ~s", [token_src(T)]) | stringify1(Tokens)].

%% stringify(L) ->
%%    [$\s | S] = lists:flatten(stringify1(L)),
%%    [{string, 1, S}].

stringify(Ts, L) ->
    [$\s | S] = lists:flatten(stringify1(Ts)),
    [{string, L, S}].

%% epp_request(Epp)
%% epp_request(Epp, Request)
%% epp_reply(From, Reply)
%%  Handle communication with the epp.

epp_request(Epp) ->
    wait_epp_reply(Epp, erlang:monitor(process, Epp)).

epp_request(Epp, Req) ->
    Epp ! {epp_request,self(),Req},
    wait_epp_reply(Epp, erlang:monitor(process, Epp)).

epp_reply(From, Rep) ->
    From ! {epp_reply,self(),Rep},
    ok.

wait_epp_reply(Epp, Mref) ->
    receive
	{epp_reply,Epp,Rep} -> 
	    erlang:demonitor(Mref),
	    receive {'DOWN',Mref,_,_,_} -> ok after 0 -> ok end,
	    Rep;
	{'DOWN',Mref,_,_,E} -> 
	    receive {epp_reply,Epp,Rep} -> Rep
	    after 0 -> 
		    exit(E)
	    end
    end.

expand_var([$$ | _] = NewName) ->
    case catch expand_var1(NewName) of
	{ok, ExpName} ->
	    ExpName;
	_ ->
	    NewName
    end;
expand_var(NewName) ->
    NewName.

expand_var1(NewName) ->
    [[$$ | Var] | Rest] = filename:split(NewName),
    Value = os:getenv(Var),
    true = Value =/= false,
    {ok, filename:join([Value | Rest])}.

%% epp has always output -file attributes when entering and leaving
%% included files (-include, -include_lib). Starting with R11B the
%% -file attribute is also recognized in the input file. This is
%% mainly aimed at yecc, the parser generator, which uses the -file
%% attribute to get correct lines in messages referring to code
%% supplied by the user (actions etc in .yrl files).
%% 
%% In a perfect world (read: perfectly implemented applications such
%% as Xref, Cover, Debugger, etc.) it would not be necessary to
%% distinguish -file attributes from epp and the input file. The
%% Debugger for example could have one window for each referred file,
%% each window with its own set of breakpoints etc. The line numbers
%% of the abstract code would then point into different windows
%% depending on the -file attribute. [Note that if, as is the case for
%% yecc, code has been copied into the file, then it is possible that
%% the copied code diffeprer from the one referred to by the -file
%% attribute, which means that line numbers can mismatch.] In practice
%% however it is very rare with Erlang functions in included files, so
%% only one window is used per module. This means that the line
%% numbers of the abstract code have to be adjusted to refer to the
%% top-most source file. The function interpret_file_attributes/1
%% below interprets the -file attribute and returns forms where line
%% numbers refer to the top-most file. The -file attribute forms that
%% have been output by epp (corresponding to -include and
%% -include_lib) are kept, but the user's -file attributes are
%% removed. This seems sufficient for now.
%% 
%% It turns out to be difficult to distinguish -file attributes in the
%% input file from the ones added by epp unless some action is taken.
%% The (less than perfect) solution employed is to let epp assign
%% negative line number to user supplied -file attributes.

interpret_file_attribute(Forms) ->
     interpret_file_attr(Forms, 0, []).

interpret_file_attr([{attribute,L,file,{_File,Line}} | Forms], 
                     Delta, Fs) when L < 0 ->
     %% -file attribute
     interpret_file_attr(Forms, {(abs(element(1,L)) + Delta) - Line,element(2, L)}, Fs);
interpret_file_attr([{attribute,_AL,file,{File,_Line}}=Form | Forms], 
                     Delta, Fs) ->
     %% -include or -include_lib
     % true = _AL =:= _Line,
     case Fs of
         [_, Delta1, File | Fs1] -> % end of included file
             [Form | interpret_file_attr(Forms, Delta1, [File | Fs1])];
         _ -> % start of included file
             [Form | interpret_file_attr(Forms, 0, [File, Delta | Fs])]
     end;
interpret_file_attr([Form0 | Forms], Delta, Fs) ->
     Form = erl_lint:modify_line(Form0, fun(L) -> {abs(element(1, L)) + Delta, element(2, L)} end),
     [Form | interpret_file_attr(Forms, Delta, Fs)];
interpret_file_attr([], _Delta, _Fs) ->
     [].

loc(T) ->
    case T of
        {_, L, _V} -> L;
        {_, L1} -> L1
    end.

