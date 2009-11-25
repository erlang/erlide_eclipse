%% =====================================================================
%% This library is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation; either version 2 of the
%% License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%% USA
%%
%% $Id$
%%
%% @copyright 2001-2006 Richard Carlsson
%% @author Richard Carlsson <richardc@it.uu.se>
%% @end
%% =====================================================================

%% @doc `epp_dodger' - bypasses the Erlang preprocessor.
%%
%% <p>This module tokenises and parses most Erlang source code without
%% expanding preprocessor directives and macro applications, as long as
%% these are syntactically "well-behaved". Because the normal parse
%% trees of the `erl_parse' module cannot represent these things
%% (normally, they are expanded by the Erlang preprocessor {@link
%% //stdlib/epp} before the parser sees them), an extended syntax tree
%% is created, using the {@link erl_syntax} module.</p>


%% NOTES:
%%
%% * It's OK if the result does not parse - then at least nothing
%% strange happens, and the user can resort to full preprocessing.
%% However, we must avoid generating a token stream that is accepted by
%% the parser, but has a different meaning than the intended. A typical
%% example is when someone uses token-level string concatenation with
%% macros, as in `"foo" ?bar' (where `?bar' expands to a string). If we
%% replace the tokens `? bar' with `( ... )', to preserve precedence,
%% the result will be parsed as an application `"foo" ( ... )' and cause
%% trouble later on. We must detect such cases and report an error.
%% 
%% * It is pointless to add a mechanism for tracking which macros are
%% known to take arguments, and which are known to take no arguments,
%% since a lot of the time we will not have seen the macro definition
%% anyway (it's usually in a header file). Hence, we try to use
%% heuristics instead. In most cases, the token sequence `? foo ('
%% indicates that it is a call of a macro that is supposed to take
%% arguments, but e.g., in the context `: ? foo (', the argument list
%% typically belongs to a remote function call, as in `m:?f(...)' and
%% should be parsed as `m:(?f)(...)' unless it is actually a try-clause
%% pattern such as `throw:?f(...) ->'.
%% 
%% * We do our best to make macros without arguments pass the parsing
%% stage transparently. Atoms are accepted in most contexts, but
%% variables are not, so we use only atoms to encode these macros.
%% Sadly, the parsing sometimes discards even the line number info from
%% atom tokens, so we can only use the actual characters for this.
%%
%% * We recognize `?m(...' at the start of a form and prevent this from
%% being interpreted as a macro with arguments, since it is probably a
%% function definition. Likewise with attributes `-?m(...'.

-module(refac_epp_dodger).

-export([parse_file/2]).

%% The following should be: 1) pseudo-uniquely identifiable, and 2)
%% cause nice looking error messages when the parser has to give up.

-define(macro_call, '? <macro> (').
-define(atom_prefix, "? ").
-define(var_prefix, "?,").
-define(pp_form, '?preprocessor declaration?').

-define(DEFAULT_TABWIDTH, 8).
-define(DEFAULT_FILEFORMAT, unix).

%%==================================================================
%% @spec parse_file(File, Options) -> {ok, Forms} | {error, errorinfo()}
%%       File = file:filename()
%%       Options = [term()]
%%       Forms = [refac_syntax:syntaxTree()]
%% 
%% @doc Reads and parses a file. If successful, `{ok, Forms}'
%% is returned, where `Forms' is a list of abstract syntax
%% trees representing the "program forms" of the file (cf.
%% `refac_syntax:is_form/1'). Otherwise, `{error,
%% errorinfo()}' is returned, typically if the file could not be
%% opened. Note that parse errors show up as error markers in the
%% returned list of forms; they do not cause this function to fail or
%% return `{error,errorinfo()}'.
%%
%% Options:
%% <dl>
%%   <dt>{@type {no_fail, bool()@}}</dt>
%%   <dd>If `true', this makes `epp_dodger' replace any program forms
%%   that could not be parsed with nodes of type `text' (see {@link
%%   refac_syntax:text/1}), representing the raw token sequence of the
%%   form, instead of reporting a parse error. The default value is
%%   `false'.</dd>
%%   <dt>{@type {clever, bool()@}}</dt>
%%   <dd>If set to `true', this makes `epp_dodger' try to repair the
%%   source code as it seems fit, in certain cases where parsing would
%%   otherwise fail. Currently, it inserts `++'-operators between string
%%   literals and macros where it looks like concatenation was intended.
%%   The default value is `false'.</dd>
%% </dl>
%%
%% @see parse/2
%% @see refac_syntax:is_form/1

parse_file(File, Options) ->
    parse_file(File, fun parse/3, Options).


parse_file(File, Parser, Options) ->
    case file:open(File, [read]) of
        {ok, Dev} ->
            try Parser(Dev, {1,1}, Options)  %% Modified by Huiqing Li
            after file:close(Dev)
	    end;
        {error, IoErr} ->
            {error, IoErr}
    end.

%%=========================================================
%% @spec parse(IODevice, StartLine, Options) ->
%%           {ok, Forms} | {error, errorinfo()}
%%       IODevice = pid()
%%       StartLine = integer()
%%       Options = [term()]
%%       Forms = [refac_syntax:syntaxTree()]
%%
%% @doc Reads and parses program text from an I/O stream. Characters are
%% read from `IODevice' until end-of-file; apart from this, the
%% behaviour is the same as for {@link parse_file/2}. `StartLine' is the
%% initial line number, which should be a positive integer.
%%
%% @see parse/2
%% @see parse_file/2
%% @see parse_form/2
%% @see quick_parse/3

parse(Dev, L0, Options) ->
    parse(Dev, L0, fun parse_form/3, Options).

parse(Dev, L0, Parser, Options) ->
    parse(Dev, L0, [], Parser, Options).

parse(Dev, L0, Fs, Parser, Options) ->
    case Parser(Dev, L0, Options) of
        {ok, none, L1} ->
            parse(Dev, L1, Fs, Parser, Options);
        {ok, F, L1} ->
            parse(Dev, L1, [F | Fs], Parser, Options);
        {error, IoErr, L1} ->
            parse(Dev, L1, [{error, IoErr} | Fs], Parser, Options);
        {eof, _L1} ->
            {ok, lists:reverse(Fs)}
    end.


parse_form(Dev, L0, Options) ->
    parse_form(Dev, L0, fun normal_parser/2, Options).

%% @spec quick_parse_form(IODevice, StartLine) ->
%%           {ok, Form, LineNo}
%%         | {eof, LineNo}
%%         | {error, errorinfo(), LineNo}
%%       IODevice = pid()
%%       StartLine = integer()
%%       Form = refac_syntax:syntaxTree() | none
%%       LineNo = integer()
%%
%% @equiv quick_parse_form(IODevice, StartLine, [])
%%
%% @see parse_form/2


-record(opt, {clever = false}).


parse_form(Dev, L0, Parser, Options) ->
    NoFail = proplists:get_bool(no_fail, Options),
    Opt = #opt{clever = proplists:get_bool(clever, Options)},
    TabWidth= case proplists:get_value(tab, Options) of 
		  undefined -> ?DEFAULT_TABWIDTH;
		  Val -> Val
	      end,
    FileFormat = case proplists:get_value(format, Options) of 
		     undefined ->?DEFAULT_FILEFORMAT;
		     V   -> V
		 end,  
    Res = refac_io:scan_erl_form(Dev, "", L0, TabWidth, FileFormat),
    case Res of    
        {ok, Ts, L1} ->
            case catch {ok, Parser(Ts, Opt)} of
                {'EXIT', Term} ->
                    {error, io_error(L1, {unknown, Term}), L1};
                {error, Term} ->
		    IoErr = io_error(L1, Term),
		    {error, IoErr, L1};
                {parse_error, _IoErr} when NoFail ->
		    {ok, refac_syntax:set_pos(
			   refac_syntax:text(tokens_to_string(Ts)),
			   start_pos(Ts, L1)),
		     L1};
                {parse_error, IoErr} ->
		    {error, IoErr, L1};
		{parse_error, IoErr, Range} ->
		    {error, {IoErr, Range}, L1};
                {ok, F} ->
		    {ok, F, L1}
            end;
        {error, IoErr, L1} ->
            {error, IoErr, L1};
        {eof, L1} ->
            {eof, L1}
    end.

io_error(L, Desc) ->
    {L, ?MODULE, Desc}.

start_pos([T | _Ts], _L) ->
    element(2, T);
start_pos([], L) ->
    L.

%% Exception-throwing wrapper for the standard Erlang parser stage

parse_tokens(Ts) ->
    parse_tokens(Ts, fun fix_form/1).
    

parse_tokens(Ts, Fix) ->
      case refac_parse:parse_form(Ts) of
        {ok, Form} ->
	    Form;
        {error, IoErr} ->
	    case Fix(Ts) of
		{form, Form} ->
		    Form;
		{retry, Ts1, Fix1} ->
		    parse_tokens(Ts1, Fix1);
		error ->
		    H = hd(Ts),
		    L = lists:last(Ts),
		    throw({parse_error, IoErr, {token_loc(H), token_loc(L)}})
	    end
    end.
%% Skipping to the end of a macro call, tracking open/close constructs.
%% @spec (Tokens) -> {Skipped, Rest}

skip_macro_args([{'(',_}=T | Ts]) ->
    skip_macro_args(Ts, [')'], [T]);
skip_macro_args(Ts) ->
    {[], Ts}.

skip_macro_args([{'(',_}=T | Ts], Es, As) ->
    skip_macro_args(Ts, [')' | Es], [T | As]);
skip_macro_args([{'{',_}=T | Ts], Es, As) ->
    skip_macro_args(Ts, ['}' | Es], [T | As]);
skip_macro_args([{'[',_}=T | Ts], Es, As) ->
    skip_macro_args(Ts, [']' | Es], [T | As]);
skip_macro_args([{'<<',_}=T | Ts], Es, As) ->
    skip_macro_args(Ts, ['>>' | Es], [T | As]);
skip_macro_args([{'begin',_}=T | Ts], Es, As) ->
    skip_macro_args(Ts, ['end' | Es], [T | As]);
skip_macro_args([{'if',_}=T | Ts], Es, As) ->
    skip_macro_args(Ts, ['end' | Es], [T | As]);
skip_macro_args([{'case',_}=T | Ts], Es, As) ->
    skip_macro_args(Ts, ['end' | Es], [T | As]);
skip_macro_args([{'receive',_}=T | Ts], Es, As) ->
    skip_macro_args(Ts, ['end' | Es], [T | As]);
skip_macro_args([{'try',_}=T | Ts], Es, As) ->
    skip_macro_args(Ts, ['end' | Es], [T | As]);
skip_macro_args([{'cond',_}=T | Ts], Es, As) ->
    skip_macro_args(Ts, ['end' | Es], [T | As]);
skip_macro_args([{E,_}=T | Ts], [E], As) ->		%final close
    {lists:reverse([T | As]), Ts};
skip_macro_args([{E,_}=T | Ts], [E | Es], As) ->	%matching close
    skip_macro_args(Ts, Es, [T | As]);
skip_macro_args([T | Ts], Es, As) ->
    skip_macro_args(Ts, Es, [T | As]);
skip_macro_args([], _Es, _As) ->
    throw({error, macro_args}).

%% ---------------------------------------------------------------------
%% Normal parsing - try to preserve all information

normal_parser(Ts, Opt) ->
    rewrite_form(parse_tokens(scan_form(Ts, Opt))).

scan_form([{'-', _L}, {atom, La, define} | Ts], Opt) ->
    [{atom, La, ?pp_form}, {'(', La}, {')', La}, {'->', La},
     {atom, La, define} | scan_macros(Ts, Opt)];
scan_form([{'-', _L}, {atom, La, undef} | Ts], Opt) ->
    [{atom, La, ?pp_form}, {'(', La}, {')', La}, {'->', La},
     {atom, La, undef} | scan_macros(Ts, Opt)];
scan_form([{'-', _L}, {atom, La, include} | Ts], Opt) ->
    [{atom, La, ?pp_form}, {'(', La}, {')', La}, {'->', La},
     {atom, La, include} | scan_macros(Ts, Opt)];
scan_form([{'-', _L}, {atom, La, include_lib} | Ts], Opt) ->
    [{atom, La, ?pp_form}, {'(', La}, {')', La}, {'->', La},
     {atom, La, include_lib} | scan_macros(Ts, Opt)];
scan_form([{'-', _L}, {atom, La, ifdef} | Ts], Opt) ->
    [{atom, La, ?pp_form}, {'(', La}, {')', La}, {'->', La},
     {atom, La, ifdef} | scan_macros(Ts, Opt)];
scan_form([{'-', _L}, {atom, La, ifndef} | Ts], Opt) ->
    [{atom, La, ?pp_form}, {'(', La}, {')', La}, {'->', La},
     {atom, La, ifndef} | scan_macros(Ts, Opt)];
scan_form([{'-', _L}, {atom, La, else} | Ts], Opt) ->
    [{atom, La, ?pp_form}, {'(', La}, {')', La}, {'->', La},
     {atom, La, else} | scan_macros(Ts, Opt)];
scan_form([{'-', _L}, {atom, La, endif} | Ts], Opt) ->
    [{atom, La, ?pp_form}, {'(', La}, {')', La}, {'->', La},
     {atom, La, endif} | scan_macros(Ts, Opt)];
scan_form([{'-', L}, {'?', L1}, {Type, _, _}=N | [{'(', _} | _]=Ts], Opt)
  when Type == atom; Type == var ->
    %% minus, macro and open parenthesis at start of form - assume that
    %% the macro takes no arguments; e.g. `-?foo(...).'
    macro(L1, N, Ts, [{'-', L}], Opt);
scan_form([{'?', L}, {Type, _, _}=N | [{'(', _} | _]=Ts], Opt)
  when Type == atom; Type == var ->
    %% macro and open parenthesis at start of form - assume that the
    %% macro takes no arguments; probably a function declaration on the
    %% form `?m(...) -> ...', which will not parse if it is rewritten as
    %% `(?m(...)) -> ...', so it must be handled as `(?m)(...) -> ...'
    macro(L, N, Ts, [], Opt);
scan_form(Ts, Opt) ->
    scan_macros(Ts, Opt).

scan_macros(Ts, Opt) ->
    scan_macros(Ts, [], Opt).

scan_macros([{'?', _}=M, {Type, _, _}=N | Ts], [{string, L, _}=S | As],
 	    #opt{clever = true}=Opt)
  when Type == atom; Type == var ->
    %% macro after a string literal: be clever and insert ++
    scan_macros([M, N | Ts], [{'++', L}, S | As], Opt);
scan_macros([{'?', L}, {Type, _, _}=N | [{'(',_}|_]=Ts],
	    [{':',_}|_]=As, Opt)
  when Type == atom; Type == var ->
    %% macro and open parentheses after colon - probably a call
    %% `m:?F(...)' so the argument list might belong to the call, not
    %% the macro - but it could also be a try-clause pattern
    %% `...:?T(...) ->' - we need to check the token following the
    %% arguments to decide
    {Args, Rest} = skip_macro_args(Ts),
    case Rest of
	[{'->',_} | _] ->
	    macro_call(Args, L, N, Rest, As, Opt);
	[{'when',_} | _] ->
	    macro_call(Args, L, N, Rest, As, Opt);
	_ ->
	    macro(L, N, Ts, As, Opt)
    end;
scan_macros([{'?', L}, {Type, _, _}=N | [{'(',_}|_]=Ts], As, Opt)
  when Type == atom; Type == var ->
    %% macro with arguments
    {Args, Rest} = skip_macro_args(Ts),
    macro_call(Args, L, N, Rest, As, Opt);
scan_macros([{'?', L }, {Type, _, _}=N | Ts], As, Opt)
  when Type == atom; Type == var ->
    %% macro without arguments
    macro(L, N, Ts, As, Opt);
scan_macros([T | Ts], As, Opt) ->
    scan_macros(Ts, [T | As], Opt);
scan_macros([], As, _Opt) ->
    lists:reverse(As).

%% Rewriting to a call which will be recognized by the post-parse pass
%% (we insert parentheses to preserve the precedences when parsing).

macro(L, {Type, LA, A}, Rest, As, Opt) ->
    scan_macros_1([], Rest, [{atom,L,macro_atom(Type,LA,A)} | As], Opt).

macro_call([{'(',_}, {')',_}], L, {_, Ln, _}=N, Rest, As, Opt) ->
    {Open, Close} = parentheses(As, L),
    scan_macros_1([], Rest,
		  lists:reverse(Open ++ [{atom,L,?macro_call},
					 {'(',L}, N, {')',Ln}] ++ Close,
				As), Opt);
macro_call([{'(',_} | Args], L, {_, Ln, _}=N, Rest, As, Opt) ->
    {Open, Close} = parentheses(As, L),
    %% note that we must scan the argument list; it may not be skipped
    scan_macros_1(Args ++ Close,
		  Rest,
		  lists:reverse(Open ++ [{atom,L,?macro_call},
					 {'(',L}, N, {',',Ln}],
				As), Opt).

macro_atom(atom, {Ln, Col},A) ->
    list_to_atom(?atom_prefix ++ integer_to_list(Ln) ++ "_"++integer_to_list(Col) ++ "_"++atom_to_list(A));
macro_atom(var, {Ln, Col}, A) ->
    list_to_atom(?var_prefix ++ integer_to_list(Ln) ++ "_"++integer_to_list(Col) ++ "_"++atom_to_list(A)).

%% don't insert parentheses after a string token, to avoid turning
%% `"string" ?macro' into a "function application" `"string"(...)'
%% (see note at top of file)
parentheses([{string, _, _} | _], _) ->
    {[], []};
parentheses(_, L) ->
    {[{'(',L}], [{')',L}]}.

%% (after a macro has been found and the arglist skipped, if any)
scan_macros_1(Args, [{string, L, _} | _]=Rest, As,
	      #opt{clever = true}=Opt) ->
    %% string literal following macro: be clever and insert ++
    scan_macros(Args ++ [{'++', L} | Rest],  As, Opt);
scan_macros_1(Args, Rest, As, Opt) ->
    %% normal case - continue scanning
    scan_macros(Args ++ Rest, As, Opt).

rewrite_form({function, L, ?pp_form, _,
              [{clause, _, [], [], [{call, _, A, As}]}]}) ->
    refac_syntax:set_pos(refac_syntax:attribute(A, rewrite_list(As)), L);
rewrite_form({function, L, ?pp_form, _, [{clause, _, [], [], [A]}]}) ->
    refac_syntax:set_pos(refac_syntax:attribute(A), L);
rewrite_form(T={attribute, _, spec, _}) -> T;
rewrite_form(T={attribute, _, type, _}) -> 
     T;
rewrite_form(T) ->
    rewrite(T).

rewrite_list([T | Ts]) ->
    [rewrite(T) | rewrite_list(Ts)];
rewrite_list([]) ->
    [].

%% Note: as soon as we start using refac_syntax:subtrees/1 and similar
%% functions, we cannot assume that we know the exact representation of
%% the syntax tree anymore - we must use refac_syntax functions to analyze
%% and decompose the data.

rewrite(Node) ->
    case refac_syntax:type(Node) of
	atom ->
	    case atom_to_list(refac_syntax:atom_value(Node)) of
		?atom_prefix ++As ->
		    {L,_} = refac_syntax:get_pos(Node),
		    {_Ln, A1} =lists:splitwith(fun(A) -> A=/= 95 end, As), %% This can be removed;
		    {Col, A2} = lists:splitwith(fun(A) -> A=/=95 end, tl(A1)),
		    A = list_to_atom(tl(A2)),	
		    N = refac_syntax:set_pos(refac_syntax:atom(A),{L,list_to_integer(Col)-1}),
		    refac_syntax:set_pos(refac_syntax:macro(N),{L, list_to_integer(Col)-1});
		   %% refac_syntax:copy_pos(Node, refac_syntax:macro(N));
		?var_prefix ++As ->
		    {L,_} = refac_syntax:get_pos(Node),
		    {_Ln, A1} =lists:splitwith(fun(A) -> A=/= 95 end, As), %% This can be removed;
		    {Col, A2} = lists:splitwith(fun(A) -> A=/=95 end, tl(A1)),
		    A = list_to_atom(tl(A2)),		    
		    N = refac_syntax:set_pos(refac_syntax:variable(A), {L,list_to_integer(Col)-1}),
		    refac_syntax:set_pos(refac_syntax:macro(N),{L, list_to_integer(Col)-1});
		   %%  refac_syntax:copy_pos(Node, refac_syntax:macro(N));
		_ ->
		    Node
	    end;
	application ->
	    F = refac_syntax:application_operator(Node),
	    case refac_syntax:type(F) of
		atom ->
		    case refac_syntax:atom_value(F) of
			?macro_call ->
			    [A | As] = refac_syntax:application_arguments(Node),
			    M = refac_syntax:macro(A, rewrite_list(As)),
			    refac_syntax:copy_pos(Node, M);
			_ ->
			    rewrite_1(Node)
		    end;
		_ ->
		    rewrite_1(Node)
	    end;
	_ ->
	    rewrite_1(Node)
    end.

rewrite_1(Node) ->
    case refac_syntax:subtrees(Node) of
	[] ->
	    Node;
	Gs ->
	    Node1 = refac_syntax:make_tree(refac_syntax:type(Node),
					 [[rewrite(T) || T <- Ts]
					  || Ts <- Gs]),
	    refac_syntax:copy_pos(Node, Node1)
    end.

%% attempting a rescue operation on a token sequence for a single form
%% if it could not be parsed after the normal treatment

fix_form([{atom, _, ?pp_form}, {'(', _}, {')', _}, {'->', _},
	  {atom, _, define}, {'(', _} | _]=Ts) ->
    case lists:reverse(Ts) of
	[{dot, _}, {')', _} | _] ->
	    {retry, Ts, fun fix_define/1};
	[{dot, L} | Ts1] ->
	    Ts2 = lists:reverse([{dot, L}, {')', L} | Ts1]),
	    {retry, Ts2, fun fix_define/1};
	_ ->
	    error
    end;
fix_form(_Ts) ->
    error.

fix_define([{atom, L, ?pp_form}, {'(', _}, {')', _}, {'->', _},
	    {atom, La, define}, {'(', _}, N, {',', _} | Ts]) ->
    [{dot, _}, {')', _} | Ts1] = lists:reverse(Ts),
    S = tokens_to_string(lists:reverse(Ts1)),
    A = refac_syntax:set_pos(refac_syntax:atom(define), La),
    Txt = refac_syntax:set_pos(refac_syntax:text(S), La),
    {form, refac_syntax:set_pos(refac_syntax:attribute(A, [N, Txt]), L)};
fix_define(_Ts) ->
    error.

%% @spec (Tokens::[term()]) -> string()
%% 
%% @doc Generates a string corresponding to the given token sequence.
%% The string can be re-tokenized to yield the same token list again.

tokens_to_string([{atom,_,A} | Ts]) ->
    io_lib:write_atom(A) ++ " " ++ tokens_to_string(Ts);
tokens_to_string([{string, _, S} | Ts]) ->
    io_lib:write_string(S) ++ " " ++ tokens_to_string(Ts);
tokens_to_string([{float, _, F} | Ts]) ->
    float_to_list(F) ++ " " ++ tokens_to_string(Ts);
tokens_to_string([{integer, _, N} | Ts]) ->
    integer_to_list(N) ++ " " ++ tokens_to_string(Ts);
tokens_to_string([{var,_,A} | Ts]) ->
    atom_to_list(A) ++ " " ++ tokens_to_string(Ts);
tokens_to_string([{dot,_} | Ts]) ->
    ".\n" ++ tokens_to_string(Ts);
tokens_to_string([{A,_} | Ts]) ->
    atom_to_list(A) ++ " " ++ tokens_to_string(Ts);
tokens_to_string([]) ->
    "".

token_loc(T) ->
    case T of
      {_, L, _V} -> L;
      {_, L1} -> L1
    end.
%% =====================================================================
