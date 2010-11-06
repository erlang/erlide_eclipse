%% =====================================================================
%% Pretty printing of abstract Erlang syntax trees
%%
%% Copyright (C) 1997-2002 Richard Carlsson
%%
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
%% Author contact: richardc@csd.uu.se
%%
%% $Id: refac_prettypr.erl,v 1.4 2008/04/30 09:28:12 hl Exp $
%%
%% Modified: 17 Jan 2007 by  Huiqing Li <hl@kent.ac.uk>
%% =====================================================================
%%
%% @doc Pretty printing of abstract Erlang syntax trees.
%%
%% <p>This module is a front end to the pretty-printing library module
%% <code>prettypr</code>, for text formatting of abstract syntax trees
%% defined by the module <code>refac_syntax</code>.</p>


-module(refac_prettypr).

-export([format/1,print_ast/2, print_ast/3]).

 %% For casestudy purpose.
-export([no_of_changed_funs_toks/1]).

%% This functions are not actually used.
-export([format/2,best/1,best/2,layout/1,lay_string/1,
	 layout/2,get_ctxt_precedence/1,set_ctxt_precedence/2,
	 get_ctxt_paperwidth/1,set_ctxt_paperwidth/2,
	 get_ctxt_linewidth/1,set_ctxt_linewidth/2,
	 get_ctxt_hook/1,set_ctxt_hook/2,get_ctxt_user/1,
	 set_ctxt_user/2]).


-import(refac_prettypr_0,
	[text/1,nest/2,above/2,beside/2,sep/1,par/1,par/2,
	 floating/3,floating/1,break/1,follow/2,follow/3,
	 empty/0, format/3, best/3]).

-import(refac_parse,
	[preop_prec/1,inop_prec/1,func_prec/0,max_prec/0]).

-define(PADDING, 2).

-define(PAPER, 80).

-define(RIBBON, 56).

-define(NOUSER, undefined).

-define(NOHOOK, none).

-define(TabWidth, 8).

-record(ctxt,
	{prec = 0,sub_indent = 2,break_indent = 4,
	 clause = undefined,hook = ?NOHOOK,paper = ?PAPER,
	 ribbon = ?RIBBON,user = ?NOUSER,
	 tokens = [],
	 tabwidth=?TabWidth,
	 format = unknown}).


%% Start of added by HL
%% ====================================================================
%% user-program guided pretty-printing of an abstract syntax tree which
%% must be a form list.

print_ast(FileFmt,AST) ->
    print_ast(FileFmt,AST, 8).

print_ast(FileFmt, AST, TabWidth) ->
    print_ast(FileFmt, AST, [], TabWidth).

print_ast(FileFmt, AST, Options, TabWidth) ->
    Fs = refac_syntax:form_list_elements(AST),
    vertical_concat(Fs, FileFmt, Options, TabWidth).
 
print_form(F,Ctxt,Fun) ->
    {Paper,Ribbon} = get_paper_ribbon_width(F),
    best(Fun(F,Ctxt),Paper,Ribbon).

vertical_concat(Forms, FileFmt, Options, TabWidth) ->
    vertical_concat(Forms, FileFmt, Options, "", TabWidth).

vertical_concat([], _FileFormat, _Options, Acc, _TabWidth) ->
    Acc;
vertical_concat([Form| T], FileFormat, Options, Acc, TabWidth) ->
    UseOriginalCode = form_not_changed(Form),
    Delimitor = case FileFormat of
		    dos -> "\r\n";
		    mac -> "\r";
		    _ -> "\n"
		end,
    Acc1 = case UseOriginalCode orelse Acc=="" of
	       true ->
		   Acc;
	       false ->
		   Acc ++ Delimitor ++ Delimitor
	   end,
    Str = case UseOriginalCode of
	      true ->
		  refac_util:concat_toks(refac_misc:get_toks(Form));
	      false ->
		  print_a_form(Form, FileFormat, Options, TabWidth)
	  end,
    vertical_concat(T, FileFormat, Options, Acc1++Str, TabWidth).


print_a_form(Form, FileFormat, Options) ->
    print_a_form(Form, FileFormat, Options, ?TabWidth).

print_a_form(Form, FileFormat, Options, TabWidth) ->
    Ctxt = #ctxt{hook  = 
		     proplists:get_value(hook,Options,?NOHOOK),
		 paper = proplists:get_value(paper,Options,?PAPER),
		 ribbon = proplists:get_value(ribbon,Options,?RIBBON),
		 user = proplists:get_value(user,Options),
		 format = FileFormat,
		 tabwidth = TabWidth,
		 tokens = refac_misc:get_toks(Form)},
    E = print_form(Form,reset_prec(Ctxt),fun lay/2),
    Str1 = refac_prettypr_0:layout(E,FileFormat,TabWidth),
    remove_trailing_whitespace(Str1).
 
is_special_form(Form) ->
    case refac_syntax:type(Form) of
	error_marker -> true;
	comment -> true;
	attribute ->
	    case refac_syntax:atom_value(refac_syntax:attribute_name(Form)) of
		type -> true;
		spec -> true;
		record -> [_R, FieldTuple] = refac_syntax:attribute_arguments(Form),
			  Fields = refac_syntax:tuple_elements(FieldTuple),
			  lists:any(fun (Field) -> case refac_syntax:type(Field) of
						       typed_record_field ->
							   true;
						       _ -> false
						   end
				    end, Fields);
		_ -> false
	    end;
	_ -> false
    end.

form_not_changed(Form) ->
    case is_special_form(Form) of 
	true -> true;   %% These forms are not touched by Wrangler at the 
                        %% moment, but this might change!
	false ->
	    Toks = refac_misc:get_toks(Form),
	    case Toks of 
		[] -> false;
		_ ->
		    try 
			Str = refac_util:concat_toks(Toks),
			{ok, Toks1, _} = refac_scan:string(Str, {1,1}, ?TabWidth, 'unix'),
			OriginalForm = refac_epp_dodger:normal_parser(Toks1, []),
			
			NewStr = format(Form, []),
			{ok, Toks2, _} = refac_scan:string(NewStr, {1,1}, ?TabWidth, 'unix'),
			NewForm = refac_epp_dodger:normal_parser(Toks2, []),
			best(OriginalForm) == best(NewForm)
		    of
			Res -> Res
		    catch
			_E1:_E2 ->
			    false
		    end
	    end
    end.
    
%% Do this still need this function?
get_paper_ribbon_width(Form) ->
    case refac_syntax:type(Form) of
	attribute -> {?PAPER, ?RIBBON};
	_ ->
	  Fun = fun (T, Acc) ->
			{S, E} = refac_misc:get_start_end_loc(T),
			[S, E] ++ Acc
		end,
	  AllRanges = refac_syntax_lib:fold(Fun, [], Form),
	  {Start, End} = refac_misc:get_start_end_loc(Form),
	  GroupedRanges = refac_misc:group_by(1, lists:filter(fun (Loc) ->
								      (Loc >= Start) and (Loc =< End)
							      end, AllRanges)),
	  case (AllRanges == []) or (GroupedRanges == []) of
	    true -> {?PAPER, ?RIBBON};
	      _ ->
		  MinMaxCols = lists:map(fun (Rs) -> Cols = [Col||{_, Col}<-Rs],
						     case Cols of
							 [] -> {1, 80};
							 _ -> {lists:min(Cols), lists:max(Cols)}
						     end
					 end, GroupedRanges),
		  Paper = lists:max([Max||{_Min, Max}<-MinMaxCols]),
		  Ribbon = lists:max([Max-Min+1|| {Min, Max}<-MinMaxCols]),
		  Paper1 = case Paper < ?PAPER of
			       true -> ?PAPER;
			       _ -> Paper
			   end,
		  Ribbon1 = case Ribbon < ?RIBBON of
				true -> ?RIBBON;
				_ -> Ribbon
			    end,
		  {Paper1 + 5, Ribbon1 + 5}  %% adjustion to take the ending tokens such as brackets/commas into account.
	  end
    end.

%% End of added by HL.
%% =====================================================================
%% =====================================================================
%% The following functions examine and modify contexts:


%% @spec (context()) -> context()
%% @doc Returns the operator precedence field of the prettyprinter
%% context.
%%
%% @see set_ctxt_precedence/2


get_ctxt_precedence(Ctxt) -> Ctxt#ctxt.prec.

%% @spec (context(), integer()) -> context()
%%
%% @doc Updates the operator precedence field of the prettyprinter
%% context. See the <code>erl_parse</code> module for operator
%% precedences.
%%
%% @see erl_parse
%% @see get_ctxt_precedence/1


set_ctxt_precedence(Ctxt,Prec) -> set_prec(Ctxt,Prec).

set_prec(Ctxt,Prec) ->
    Ctxt#ctxt{prec = Prec}.    % used internally


reset_prec(Ctxt) ->
    set_prec(Ctxt,0).    % used internally


%% @spec (context()) -> integer()
%% @doc Returns the paper widh field of the prettyprinter context.
%% @see set_ctxt_paperwidth/2


get_ctxt_paperwidth(Ctxt) -> Ctxt#ctxt.paper.

%% @spec (context(), integer()) -> context()
%%
%% @doc Updates the paper widh field of the prettyprinter context.
%%
%% <p> Note: changing this value (and passing the resulting context to a
%% continuation function) does not affect the normal formatting, but may
%% affect user-defined behaviour in hook functions.</p>
%%
%% @see get_ctxt_paperwidth/1


set_ctxt_paperwidth(Ctxt,W) -> Ctxt#ctxt{paper = W}.

%% @spec (context()) -> integer()
%% @doc Returns the line widh field of the prettyprinter context.
%% @see set_ctxt_linewidth/2


get_ctxt_linewidth(Ctxt) -> Ctxt#ctxt.ribbon.

%% @spec (context(), integer()) -> context()
%%
%% @doc Updates the line widh field of the prettyprinter context.
%%
%% <p> Note: changing this value (and passing the resulting context to a
%% continuation function) does not affect the normal formatting, but may
%% affect user-defined behaviour in hook functions.</p>
%%
%% @see get_ctxt_linewidth/1


set_ctxt_linewidth(Ctxt,W) -> Ctxt#ctxt{ribbon = W}.

%% @spec (context()) -> hook()
%% @doc Returns the hook function field of the prettyprinter context.
%% @see set_ctxt_hook/2


get_ctxt_hook(Ctxt) -> Ctxt#ctxt.hook.

%% @spec (context(), hook()) -> context()
%% @doc Updates the hook function field of the prettyprinter context.
%% @see get_ctxt_hook/1


set_ctxt_hook(Ctxt,Hook) -> Ctxt#ctxt{hook = Hook}.

%% @spec (context()) -> term()
%% @doc Returns the user data field of the prettyprinter context.
%% @see set_ctxt_user/2


get_ctxt_user(Ctxt) -> Ctxt#ctxt.user.

%% @spec (context(), term()) -> context()
%% @doc Updates the user data field of the prettyprinter context.
%% @see get_ctxt_user/1


set_ctxt_user(Ctxt,X) -> Ctxt#ctxt{user = X}.

%% =====================================================================
%% @spec format(Tree::syntaxTree()) -> string()
%% @equiv format(Tree, [])


format(Node) -> 
    format(Node,[]).
   

%% =====================================================================
%% @spec format(Tree::syntaxTree(), Options::[term()]) -> string()
%%           syntaxTree() = refac_syntax:syntaxTree()
%%
%% @type hook() = (syntaxTree(), context(), Continuation) -> document()
%%	    Continuation = (syntaxTree(), context()) -> document().
%%
%% A call-back function for user-controlled formatting. See <a
%% href="#format-2"><code>format/2</code></a>.
%%
%% @type context(). A representation of the current context of the
%% pretty-printer. Can be accessed in hook functions.
%%
%% @doc Prettyprint-formats an abstract Erlang syntax tree as text.
%%
%% <p>Available options:
%% <dl>
%%   <dt>{hook, none | <a href="#type-hook">hook()</a>}</dt>
%%       <dd>Unless the value is <code>none</code>, the given function
%%       is called for each node whose list of annotations is not empty;
%%       see below for details. The default value is
%%       <code>none</code>.</dd>
%%
%%   <dt>{paper, integer()}</dt>
%%       <dd>Specifies the preferred maximum number of characters on any
%%       line, including indentation. The default value is 80.</dd>
%%
%%   <dt>{ribbon, integer()}</dt>
%%       <dd>Specifies the preferred maximum number of characters on any
%%       line, not counting indentation. The default value is 65.</dd>
%%
%%   <dt>{user, term()}</dt>
%%       <dd>User-specific data for use in hook functions. The default
%%       value is <code>undefined</code>.</dd>
%% </dl></p>
%%
%% <p>A hook function (cf. the <a
%% href="#type-hook"><code>hook()</code></a> type) is passed the current
%% syntax tree node, the context, and a continuation. The context can be
%% examined and manipulated by functions such as
%% <code>get_ctxt_user/1</code> and <code>set_ctxt_user/2</code>. The
%% hook must return a "document" data structure (see
%% <code>layout/2</code> and <code>best/2</code>); this may be
%% constructed in part or in whole by applying the continuation
%% function. For example, the following is a trivial hook:
%% <pre>
%%     fun (Node, Ctxt, Cont) -> Cont(Node, Ctxt) end
%% </pre>
%% which yields the same result as if no hook was given.
%% The following, however:
%% <pre>
%%     fun (Node, Ctxt, Cont) ->
%%         Doc = Cont(Node, Ctxt),
%%         prettypr:beside(prettypr:text("&lt;b>"),
%%                         prettypr:beside(Doc,
%%                                         prettypr:text("&lt;/b>")))
%%     end
%% </pre>
%% will place the text of any annotated node (regardless of the
%% annotation data) between HTML "boldface begin" and "boldface end"
%% tags.</p>
%%
%% @see refac_syntax
%% @see format/1
%% @see layout/2
%% @see best/2
%% @see get_ctxt_user/1
%% @see set_ctxt_user/2


format(Node,Options) ->
    W = proplists:get_value(paper,Options,?PAPER),
    L = proplists:get_value(ribbon,Options,?RIBBON),
    format(layout(Node,Options),W,L).

%% =====================================================================
%% @spec best(Tree::syntaxTree()) -> empty | document()
%% @equiv best(Tree, [])


best(Node) -> best(Node,[]).

%% =====================================================================
%% @spec best(Tree::syntaxTree(), Options::[term()]) ->
%%           empty | document()
%%
%% @doc Creates a fixed "best" abstract layout for a syntax tree. This
%% is similar to the <code>layout/2</code> function, except that here,
%% the final layout has been selected with respect to the given options.
%% The atom <code>empty</code> is returned if no such layout could be
%% produced. For information on the options, see the
%% <code>format/2</code> function.
%%
%% @see best/1
%% @see layout/2
%% @see format/2
%% @see prettypr:best/2


best(Node,Options) ->
    W = proplists:get_value(paper,Options,?PAPER),
    L = proplists:get_value(ribbon,Options,?RIBBON),
    best(layout(Node,Options),W,L).

%% =====================================================================
%% @spec layout(Tree::syntaxTree()) -> document()
%% @equiv layout(Tree, [])


layout(Node) -> layout(Node,[]).

%% =====================================================================
%% @spec layout(Tree::syntaxTree(), Options::[term()]) -> document()
%%	    document() = prettypr:document()
%%
%% @doc Creates an abstract document layout for a syntax tree. The
%% result represents a set of possible layouts (cf. module
%% <code>prettypr</code>). For information on the options, see
%% <code>format/2</code>; note, however, that the <code>paper</code> and
%% <code>ribbon</code> options are ignored by this function.
%%
%% <p>This function provides a low-level interface to the pretty
%% printer, returning a flexible representation of possible layouts,
%% independent of the paper width eventually to be used for formatting.
%% This can be included as part of another document and/or further
%% processed directly by the functions in the <code>prettypr</code>
%% module, or used in a hook function (see <code>format/2</code> for
%% details).</p>
%%
%% @see prettypr
%% @see format/2
%% @see layout/1


layout(Node,Options) ->
    lay(Node,
	#ctxt{hook = proplists:get_value(hook,Options,?NOHOOK),
	      paper = proplists:get_value(paper,Options,?PAPER),
	      ribbon = proplists:get_value(ribbon,Options,?RIBBON),
	      user = proplists:get_value(user,Options)}).
	    
lay(Node,Ctxt) ->
    case refac_syntax:get_ann(Node) of
      [] ->
	  %% Hooks are not called if there are no annotations.
	  lay_1(Node,Ctxt);
      _As ->
	  case Ctxt#ctxt.hook of
	    ?NOHOOK -> lay_1(Node,Ctxt);
	    Hook -> Hook(Node,Ctxt,fun lay_1/2)
	  end
    end.

%% This handles attached comments:


lay_1(Node, Ctxt) ->
    case refac_syntax:has_comments(Node) of
      true ->
	  {{NodeStartLn, _}, {NodeEndLn, _}} = refac_misc:get_start_end_loc(Node),
	  D1 = lay_2(Node, Ctxt),
	  PreCs = refac_syntax:get_precomments(Node),
	  PostCs = refac_syntax:get_postcomments(Node),
	  D2 = case PostCs of
		 [] ->
		     D1;
		 _ ->
		     PostCsLn = element(1, refac_syntax:get_pos(hd(PostCs))),
		     case PostCsLn > NodeEndLn + 1 of
		       true ->
			   lay_postcomments(refac_syntax:get_postcomments(Node), above(D1, text("")));
		       false ->
			   lay_postcomments(refac_syntax:get_postcomments(Node), D1)
		     end
	       end,
	  case PreCs of
	    [] -> D2;
	    _ -> LastPreC = lists:last(PreCs),
		 PreCLn = element(1, refac_syntax:get_pos(LastPreC)),
		 LastPreCLines = length(refac_syntax:comment_text(LastPreC)),
		 case NodeStartLn > PreCLn + LastPreCLines of
		   true ->
		       lay_precomments(refac_syntax:get_precomments(Node), above(text(""), D2));
		   false ->
		       lay_precomments(refac_syntax:get_precomments(Node), D2)
		 end
	  end;
      false -> 
	    lay_2(Node, Ctxt)
    end.

%% For pre-comments, all padding is ignored.


lay_precomments([],D) -> D;
lay_precomments(Cs,D) ->
    above(floating(break(stack_comments(Cs,false)),-1,-1),D).

%% For postcomments, individual padding is added.


lay_postcomments([],D) -> D;
lay_postcomments(Cs,D) ->
    beside(D,floating(break(stack_comments(Cs,true)),1,0)).

%% Format (including padding, if `Pad' is `true', otherwise not)
%% and stack the listed comments above each other,


stack_comments([C| Cs],Pad) ->
    D = stack_comment_lines(refac_syntax:comment_text(C)),
    D1 = case Pad of
	   true ->
	       P = case refac_syntax:comment_padding(C) of
		     none -> ?PADDING;
		     P1 -> P1
		   end,
	       beside(text(spaces(P)),D);
	   false -> D
	 end,
    case Cs of
      [] ->
	  D1;    % done
      _ -> above(D1,stack_comments(Cs,Pad))
    end;
stack_comments([],_) -> empty().

%% Stack lines of text above each other and prefix each string in
%% the list with a single `%' character.


stack_comment_lines([S| Ss]) ->
    D = text(add_comment_prefix(S)),
    case Ss of
      [] -> D;
      _ -> above(D,stack_comment_lines(Ss))
    end;
stack_comment_lines([]) -> empty().

add_comment_prefix(S) -> [$%| S].

%% This part ignores annotations and comments:

lay_2(Node, Ctxt) ->
    case refac_syntax:type(Node) of
      %% We list literals and other common cases first.
      variable -> text(refac_syntax:variable_literal(Node));
      atom ->
	    Lit = refac_syntax:atom_literal(Node),
	    case hd(Lit) of
		39 -> text("'" ++ atom_to_list(refac_syntax:atom_value(Node)) ++ "'");
	    _ -> text(Lit)
	  end;
	integer -> text(refac_syntax:integer_literal(Node));
	float ->
	    text(tidy_float(refac_syntax:float_literal(Node)));
	text -> text(refac_syntax:text_string(Node));
	char ->
	    V = refac_syntax:char_value(Node),
	   case is_integer(V) and (V > 127) of
	       true -> {ok, [Num], _} = io_lib:fread("~u", integer_to_list(V)),
		       [CharStr] = io_lib:fwrite("~.8B", [Num]),
		       text("$\\" ++ CharStr);   %% "
	       _ when is_atom(V)-> 
                 text(atom_to_list(V));
               _ -> text(refac_syntax:char_literal(Node))
	   end;
      string ->  %% done;
	  Str = refac_syntax:string_literal(Node),
	  StrVal = "\"" ++ refac_syntax:string_value(Node) ++ "\"",
	  case lists:keysearch(toks, 1, refac_syntax:get_ann(Node)) of
	    {value, {toks, StrToks}} ->
		Str1 = io_lib:write_string(lists:concat(lists:map(fun ({string, _, S}) -> S end, StrToks))),
		case Str1 == Str of
		  true -> lay_string(StrToks);
		  _ -> lay_string(StrVal, Ctxt)
		end;
	    _ -> lay_string(StrVal, Ctxt)
	  end;
      nil -> text("[]");
      tuple -> %% done;
          Es0=refac_syntax:tuple_elements(Node),
	  Sep = get_separator(Es0, Ctxt#ctxt.tokens, ","),
	  Es = seq(Es0, floating(text(Sep)), reset_prec(Ctxt), fun lay/2),
	  beside(floating(text("{")),
		 beside(lay_elems(fun refac_prettypr_0:par/1, Es, refac_syntax:tuple_elements(Node)), floating(text("}"))));
      list ->   %% done;
	  Ctxt1 = reset_prec(Ctxt),
	  Node1 = refac_syntax:compact_list(Node),
	  PrefixElems = refac_syntax:list_prefix(Node1),
	  Sep = get_separator(PrefixElems, Ctxt#ctxt.tokens, ","),
	  D0 = seq(PrefixElems, floating(text(Sep)), Ctxt1, fun lay/2),
	  D1 = lay_elems(fun refac_prettypr_0:par/1, D0, PrefixElems),
	  D = case refac_syntax:list_suffix(Node1) of
		none -> beside(D1, floating(text("]")));
		S ->
		    EndLn = get_end_line(lists:last(PrefixElems)),
		    StartLn = get_start_line(S),
		    case (EndLn == 0) or (StartLn == 0) of
		      true ->
			  follow(D1, beside(floating(text("| ")), beside(lay(S, Ctxt1), floating(text("]")))));
		      _ ->
			  case StartLn - EndLn of
			    0 ->
				beside(D1, beside(floating(text("| ")), beside(lay(S, Ctxt1), floating(text("]")))));
			    1 ->
				above(D1, beside(floating(text("| ")), beside(lay(S, Ctxt1), floating(text("]")))));
			    _ ->
				follow(D1, beside(floating(text("| ")), beside(lay(S, Ctxt1), floating(text("]")))))
			  end
		    end
	      end,
	  beside(floating(text("[")), D);
      operator ->
	  Op = refac_syntax:operator_literal(Node),
	  case lists:member(Op, ["bnot", "not", "div", "rem",
				 "band", "and", "bor", "bxor", 
                                 "bsl", "bsr", "or", "xor", 
				 "andalso","orelse", "catch"]) of
	      true ->
		  floating(text(" "++Op++" "));
	      false ->
		  Op1 = get_operator_text(Node, Ctxt#ctxt.tokens, Op),
		  floating(text(Op1))
	  end;
      infix_expr ->  %% done;
	  Left = refac_syntax:infix_expr_left(Node),
	  Operator = refac_syntax:infix_expr_operator(Node),
	  Right = refac_syntax:infix_expr_right(Node),
	  {PrecL, Prec, PrecR} = case refac_syntax:type(Operator) of
				   operator ->
				       inop_prec(refac_syntax:operator_name(Operator));
				   _ -> {0, 0, 0}
				 end,
	  D1 = maybe_parentheses_1(lay(Left, set_prec(Ctxt, PrecL)),Left, Ctxt),
	  D2 = lay(Operator, reset_prec(Ctxt)),
	  D3 = maybe_parentheses_1(lay(Right, set_prec(Ctxt, PrecR)),Right, Ctxt),
	  LeftEndLn = get_end_line(Left),
	  OpStartLn = get_start_line(Operator),
	  RightStartLn = get_start_line(Right),
	  D12 = case OpStartLn == LeftEndLn andalso OpStartLn =/= 0 of
	 	  true -> horizontal([D1, D2]);
	 	  false -> par([D1, D2], Ctxt#ctxt.sub_indent)
		end,
	  D4 = case OpStartLn == RightStartLn andalso OpStartLn =/= 0 of
		 true ->
		     case D3 of
		       {sep, [D| Ds], N, P} ->
			   {sep, [horizontal([D12, D])| Ds], N, P};
		       {beside, {sep, [D| Ds], N, P}, Ds1} ->
			   {beside, {sep, [horizontal([D12, D])| Ds], N, P}, Ds1};
		       {beside, {beside, {sep, [D| Ds], N, P}, {text, [1, 32]}}, Ds1} ->
			   {beside, {beside, {sep, [horizontal([D12, D])| Ds], N, P}, {text, [1, 32]}}, Ds1};
		       _ ->
			   horizontal([D12, D3])
		     end;
	 	 _ -> par([D12, D3], Ctxt#ctxt.sub_indent)
	        end,
	  maybe_parentheses(D4, Prec, Ctxt);
      prefix_expr ->  %% done;
	  Operator = refac_syntax:prefix_expr_operator(Node),
	  PrefixArg = refac_syntax:prefix_expr_argument(Node),
	  {{Prec, PrecR}, Name} = case refac_syntax:type(Operator) of
				    operator ->
					N = refac_syntax:operator_name(Operator),
					{preop_prec(N), N};
				    _ -> {{0, 0}, any}
				  end,
	  D1 = lay(Operator, reset_prec(Ctxt)),
	  D2 = lay(PrefixArg, set_prec(Ctxt, PrecR)),
	  D3 = case Name of
		 '+' -> beside(D1, D2);
		 '-' -> beside(D1, D2);
		 _ ->
		     OpEndLn = get_end_line(Operator),
		     ArgStartLn = get_start_line(PrefixArg),
		     case (OpEndLn == 0) or (ArgStartLn == 0) of
		       true -> par([D1, D2], Ctxt#ctxt.sub_indent);
		       _ ->
			   case ArgStartLn - OpEndLn of
			     0 -> beside(beside(D1, text(" ")), D2);
			     1 -> above(D1, nest(Ctxt#ctxt.sub_indent, D2));
			     _ -> par([D1, D2], Ctxt#ctxt.sub_indent)
			   end
		     end
	       end,
	  maybe_parentheses(D3, Prec, Ctxt);
      application ->  %% done.
	  {PrecL, Prec} = func_prec(),
	  D = lay(refac_syntax:application_operator(Node), set_prec(Ctxt, PrecL)),
	  Args = refac_syntax:application_arguments(Node),			   
	  Sep = get_separator(Args, Ctxt#ctxt.tokens, ","),
	  As = seq(Args, floating(text(Sep)), reset_prec(Ctxt), fun lay/2),
	  Op = refac_syntax:application_operator(Node),
	  D1 = case Args of
		 [] ->
		     beside(D, beside(text("("), text(")")));
		 [H| _] ->
		     EndLn = get_end_line(Op),
		     StartLn = get_start_line(H),
		     case StartLn - EndLn == 1 of
		       true ->
			   above(beside(D, text("(")),
				 nest(Ctxt#ctxt.sub_indent, beside(lay_elems(fun refac_prettypr_0:par/1,
									     As, Args), floating(text(")")))));
		       false ->
			   beside(D, beside(text("("), beside(lay_elems(fun refac_prettypr_0:par/1,
									As, Args), floating(text(")")))))
		     end
	       end,
	  maybe_parentheses(D1, Prec, Ctxt);
      match_expr ->         %% done;
	  {PrecL, Prec, PrecR} = inop_prec('='),
	  Left = refac_syntax:match_expr_pattern(Node),
	  Right = refac_syntax:match_expr_body(Node),
	  EndLn = get_end_line(Left),
	  StartLn = get_start_line(Right),
	  D1 = lay(refac_syntax:match_expr_pattern(Node), set_prec(Ctxt, PrecL)),
	  D2 = lay(refac_syntax:match_expr_body(Node), set_prec(Ctxt, PrecR)),
	  D3 = case (EndLn == 0) or (StartLn == 0) of
		 true ->
		     follow(beside(D1, floating(text(" ="))), D2, Ctxt#ctxt.break_indent);
		 false ->
		     case EndLn == StartLn of
		       true -> beside(beside(D1, text(" = ")), D2);
		       _ ->
			   above(beside(D1, text(" =")), nest(Ctxt#ctxt.break_indent, D2))
		     end
	       end,
	  maybe_parentheses(D3, Prec, Ctxt);
      underscore -> text("_");
      clause ->  %% done;
	  %% The style used for a clause depends on its context
	  Ctxt1 = (reset_prec(Ctxt))#ctxt{clause = undefined},
	  Pats = refac_syntax:clause_patterns(Node),
	  Body = refac_syntax:clause_body(Node),
	  Sep = get_separator(Pats, Ctxt#ctxt.tokens, ","),
	  PatDocs = seq(Pats, floating(text(Sep)), Ctxt1, fun lay/2),
	  D1 = lay_elems(fun refac_prettypr_0:par/1, PatDocs, Pats),
       	  D2 = case refac_syntax:clause_guard(Node) of
	  	 none -> none;
	  	 G -> lay(G, Ctxt1)
	       end,
	  BodyDocs = seq(Body, floating(text(",")), Ctxt1, fun lay/2),
	  D3 = lay_body_elems(fun refac_prettypr_0:sep/1, BodyDocs, Body),
	  HeadLastLn = case refac_syntax:clause_guard(Node) of
	  		 none -> case Pats of
	  			   [] -> get_start_line(Node);
	  			   _ ->  case get_end_line(Pats) of 
					     0 -> get_start_line(Node);
					     L ->L
					 end
	  			 end;
	  		 _ -> get_end_line(refac_syntax:clause_guard(Node))
	  	       end,
	  BodyStartLn = get_start_line(hd(Body)),
	  SameLine = {BodyStartLn, HeadLastLn},
	  case Ctxt#ctxt.clause of
	    fun_expr -> make_fun_clause(D1, D2, D3, Ctxt, SameLine);
	    {function, N} -> make_fun_clause(N, D1, D2, D3, Ctxt, SameLine);
	    if_expr -> make_if_clause(D1, D2, D3, Ctxt, SameLine);
	    cond_expr -> make_if_clause(D1, D2, D3, Ctxt, SameLine);
	    case_expr ->
		  make_case_clause(D1, D2, D3, Ctxt, SameLine);
	    receive_expr -> make_case_clause(D1, D2, D3, Ctxt, SameLine);
	    try_expr -> make_case_clause(D1, D2, D3, Ctxt, SameLine);
	    {rule, N} -> make_rule_clause(N, D1, D2, D3, Ctxt, SameLine);
	    undefined ->
	  	%% If a clause is formatted out of context, we
	  	%% use a "fun-expression" clause style.
	  	make_fun_clause(D1, D2, D3, Ctxt, SameLine)
	  end;
      function ->  %% done;
	  %% Comments on the name itself will be repeated for each
	  %% clause, but that seems to be the best way to handle it.
	  Ctxt1 = reset_prec(Ctxt),
	  D1 = lay(refac_syntax:function_name(Node), Ctxt1),
	  D2 = lay_clauses(refac_syntax:function_clauses(Node), {function, D1}, Ctxt1),
	  beside(D2, floating(text(".")));
      case_expr ->  %% done;
	  Ctxt1 = reset_prec(Ctxt),
	  D1 = lay(refac_syntax:case_expr_argument(Node), Ctxt1),
	  D2 = lay_clauses(refac_syntax:case_expr_clauses(Node), case_expr, Ctxt1),
	  sep([par([follow(text("case"), D1, Ctxt1#ctxt.break_indent), text("of")], Ctxt1#ctxt.break_indent),
	       nest(Ctxt1#ctxt.break_indent, D2), text("end")]);
      if_expr ->  %% done;
	  Ctxt1 = reset_prec(Ctxt),
	  D = lay_clauses(refac_syntax:if_expr_clauses(Node), if_expr, Ctxt1),
	  sep([follow(text("if"), D, Ctxt1#ctxt.break_indent), text("end")]);
      cond_expr ->  %% done;
	  Ctxt1 = reset_prec(Ctxt),
	  D = lay_clauses(refac_syntax:cond_expr_clauses(Node), cond_expr, Ctxt1),
	  sep([text("cond"), nest(Ctxt1#ctxt.break_indent, D), text("end")]);
      fun_expr ->  %% done;
	  Ctxt1 = reset_prec(Ctxt),
	  D = lay_clauses(refac_syntax:fun_expr_clauses(Node), fun_expr, Ctxt1),
	  sep([follow(text("fun"), D, Ctxt1#ctxt.break_indent), text("end")]);
      module_qualifier ->  %% done;
	  {PrecL, _Prec, PrecR} = inop_prec(':'),
	  D1 = lay(refac_syntax:module_qualifier_argument(Node), set_prec(Ctxt, PrecL)),
	  D2 = lay(refac_syntax:module_qualifier_body(Node), set_prec(Ctxt, PrecR)),
	  beside(D1, beside(text(":"), D2));
      qualified_name ->  %% done;
	  Ss = refac_syntax:qualified_name_segments(Node),
	  lay_qualified_name(Ss, Ctxt);
      %%
      %% The rest is in alphabetical order
      %%
      arity_qualifier ->  %% done;
	  Ctxt1 = reset_prec(Ctxt),
	  D1 = lay(refac_syntax:arity_qualifier_body(Node), Ctxt1),
	  D2 = lay(refac_syntax:arity_qualifier_argument(Node), Ctxt1),
	  beside(D1, beside(text("/"), D2));
      attribute ->  %% done;
	  %% The attribute name and arguments are formatted similar to
	  %% a function call, but prefixed with a "-" and followed by
	  %% a period. If the arguments is `none', we only output the
	  %% attribute name, without following parentheses.
	  Ctxt1 = reset_prec(Ctxt),
	  N = refac_syntax:attribute_name(Node),
	  D = case refac_syntax:attribute_arguments(Node) of
		none -> lay(N, Ctxt1);
		Args ->
		    Sep = get_separator(Args, Ctxt#ctxt.tokens, ","),
		    As = seq(Args, floating(text(Sep)), Ctxt1, fun lay/2),
		    beside(lay(N, Ctxt1), beside(text("("), beside(lay_elems(fun refac_prettypr_0:par/1, As, Args), floating(text(")")))))
	      end,
	  beside(floating(text("-")), beside(D, floating(text("."))));
      binary ->   %% done
	  Ctxt1 = reset_prec(Ctxt),
	  Fields = refac_syntax:binary_fields(Node),
	  Sep = get_separator(Fields, Ctxt#ctxt.tokens, ","),
	  Es = seq(Fields, floating(text(Sep)), Ctxt1, fun lay/2),
	  beside(floating(text("<<")), beside(lay_elems(fun refac_prettypr_0:par/1, Es, Fields), floating(text(">>"))));
      binary_field ->
	  Ctxt1 = reset_prec(Ctxt),
	  D = lay(refac_syntax:binary_field_body(Node), Ctxt1),
	  %% Begin of modification of HL
	  Body = refac_syntax:binary_field_body(Node),
	  D1 = case refac_syntax:type(Body) of
		 size_qualifier -> D;
		 _ ->
		     case refac_syntax:type(Body) == variable orelse
			    refac_syntax:type(Body) == underscore orelse refac_syntax:is_literal(Body) == true
			 of
		       true -> D;
		       false ->
			   case refac_syntax:type(Body) of
			     macro -> case lists:keysearch(with_bracket, 1, refac_syntax:get_ann(Body)) of
					{value, {with_bracket, true}} ->
					    beside(floating(text("(")), beside(D, floating(text(")"))));
					_ -> D
				      end;
			     _ -> beside(floating(text("(")), beside(D, floating(text(")"))))
			   end
		     end
	       end,
	  %% End of modification by HL
	  D2 = case refac_syntax:binary_field_types(Node) of
		 [] -> empty();
		 Ts ->
		     beside(floating(text("/")), lay_bit_types(Ts, Ctxt1))
	       end,
	  beside(D1, D2);
      block_expr -> %% done;
	  Ctxt1 = reset_prec(Ctxt),
	  Body = refac_syntax:block_expr_body(Node),
	  Es = seq(Body, floating(text(", ")), Ctxt1, fun lay/2),
	  sep([text("begin"), nest(Ctxt1#ctxt.break_indent, lay_body_elems(fun refac_prettypr_0:sep/1, Es, Body)), text("end")]);
      catch_expr ->  %% done;
	  {Prec, PrecR} = preop_prec('catch'),
	  D = lay(refac_syntax:catch_expr_body(Node), set_prec(Ctxt, PrecR)),
	  D1 = follow(text("catch"), D, Ctxt#ctxt.break_indent),
	  maybe_parentheses(D1, Prec, Ctxt);
      class_qualifier ->  %% done;
	  Ctxt1 = set_prec(Ctxt, max_prec()),
	  D1 = lay(refac_syntax:class_qualifier_argument(Node), Ctxt1),
	  D2 = lay(refac_syntax:class_qualifier_body(Node), Ctxt1),
	  beside(D1, beside(text(":"), D2));
      comment ->
	  D = stack_comment_lines(refac_syntax:comment_text(Node)),
	  %% Default padding for standalone comments is empty.
	  case refac_syntax:comment_padding(Node) of
	    none -> floating(break(D));
	    P -> floating(break(beside(text(spaces(P)), D)))
	  end;
      conjunction -> %% done;
	  Body = refac_syntax:conjunction_body(Node),
	  Sep = get_separator(Body, Ctxt#ctxt.tokens, ","),
	  Es = seq(Body, floating(text(Sep)), reset_prec(Ctxt), fun lay/2),
	  lay_elems(fun refac_prettypr_0:par/1, Es, refac_syntax:conjunction_body(Node));
      disjunction -> %% done;
	  %% For clarity, we don't paragraph-format
	  %% disjunctions; only conjunctions (see above).
	  Es = seq(refac_syntax:disjunction_body(Node), floating(text(";")), reset_prec(Ctxt), fun lay/2),
	  lay_elems(fun refac_prettypr_0:sep/1, Es, refac_syntax:disjunction_body(Node));
      error_marker ->  %% done;
	  E = refac_syntax:error_marker_info(Node),
	  beside(text("** "), beside(lay_error_info(E, reset_prec(Ctxt)), text(" **")));
      eof_marker -> empty();
      form_list ->
	  Es = seq(refac_syntax:form_list_elements(Node), none, reset_prec(Ctxt), fun lay/2),
	  %% I leave this as it is. (HL).
	  vertical_sep(text(""), Es);
      generator -> %% done;
	  Ctxt1 = reset_prec(Ctxt),
	  D1 = lay(refac_syntax:generator_pattern(Node), Ctxt1),
	  D2 = lay(refac_syntax:generator_body(Node), Ctxt1),
	  D1EndLn = get_end_line(refac_syntax:generator_pattern(Node)),
	  D2StartLn = get_start_line(refac_syntax:generator_body(Node)),
	  case (D1EndLn == 0) or (D2StartLn == 0) of
	    true ->
		par([D1, beside(text("<- "), D2)], Ctxt1#ctxt.break_indent);
	    _ ->
		case D2StartLn - D1EndLn of
		  0 -> beside(D1, beside(text(" <- "), D2));
		  1 -> above(D1, nest(Ctxt#ctxt.break_indent, beside(text("<- "), D2)));
		  _ -> par([D1, beside(text("<- "), D2)], Ctxt1#ctxt.sub_indent)
		end
	  end;
      binary_generator ->
	      Ctxt1 = reset_prec(Ctxt),
	      D1 = lay(erl_syntax:binary_generator_pattern(Node), Ctxt1),
	      D2 = lay(erl_syntax:binary_generator_body(Node), Ctxt1),
	      D1EndLn = get_end_line(refac_syntax:generator_pattern(Node)),
	      D2StartLn = get_start_line(refac_syntax:generator_body(Node)),
	      case (D1EndLn == 0) or (D2StartLn == 0) of
		  true ->
		      par([D1, beside(text("<- "), D2)], Ctxt1#ctxt.break_indent);
		  _ ->
		      case D2StartLn - D1EndLn of
			  0 -> beside(D1, beside(text(" <- "), D2));
			  1 -> above(D1, nest(Ctxt#ctxt.break_indent, beside(text("<- "), D2)));
			  _ -> par([D1, beside(text("<- "), D2)], Ctxt1#ctxt.sub_indent)
		      end
	      end;
      implicit_fun -> %%done;
	  D = lay(refac_syntax:implicit_fun_name(Node), reset_prec(Ctxt)),
	  beside(floating(text("fun ")), D);
      list_comp ->  %% done;
	  Ctxt1 = reset_prec(Ctxt),
	  D1 = lay(refac_syntax:list_comp_template(Node), Ctxt1),
	  Es0 =refac_syntax:list_comp_body(Node),
	  Sep = get_separator(Es0, Ctxt#ctxt.tokens, ","),
	  Es = seq(Es0, floating(text(Sep)), Ctxt1, fun lay/2),
	  D2 = lay_elems(fun refac_prettypr_0:par/1, Es, refac_syntax:list_comp_body(Node)),
	  D1EndLn = get_end_line(refac_syntax:list_comp_template(Node)),
	  D2StartLn = get_start_line(hd(refac_syntax:list_comp_body(Node))),
	  D3 = case (D2StartLn - D1EndLn == 0) and (D1EndLn =/= 0) of
		 true -> beside(D1, beside(floating(text(" || ")), beside(D2, floating(text("]")))));
		 _ -> par([D1, beside(floating(text("|| ")), beside(D2, floating(text("]"))))])
	       end,
	  beside(floating(text("[")), D3);
      binary_comp ->
	    Ctxt1 = reset_prec(Ctxt),
	    D1 = lay(erl_syntax:binary_comp_template(Node), Ctxt1),
	    D2 = par(seq(erl_syntax:binary_comp_body(Node),
			 floating(text(",")), Ctxt1,
			 fun lay/2)),
	    beside(floating(text("<< ")),
		   par([D1, beside(floating(text(" || ")),
				   beside(D2, floating(text(" >>"))))]));
      macro ->  %%done;
	  %% This is formatted similar to a normal function call, but
	  %% prefixed with a "?".
	  Ctxt1 = reset_prec(Ctxt),
	  N = refac_syntax:macro_name(Node),
	  Args = refac_syntax:macro_arguments(Node),
	  D = case Args of
		[H| _] ->
		    EndLn = get_end_line(N),
		    StartLn = get_start_line(H),
		    Sep = get_separator(Args, Ctxt#ctxt.tokens, ","),
		    As = seq(Args, floating(text(Sep)), reset_prec(Ctxt), fun lay/2),
		    case StartLn > EndLn of
		      true ->
			  above(beside(lay(N, Ctxt1), text("(")),
				nest(Ctxt1#ctxt.sub_indent,
				     beside(lay_elems(fun refac_prettypr_0:par/1, As, Args), floating(text(")")))));
		      false ->
			  beside(lay(N, Ctxt1), beside(text("("),
						       beside(lay_elems(fun refac_prettypr_0:par/1, As, Args), floating(text(")")))))
		    end;
		[] -> beside(lay(N, Ctxt1), text("()"));
		none -> lay(N, Ctxt1)
	      end,
	  D1 = beside(floating(text("?")), D),
	  case Ctxt#ctxt.prec of
	    P when P > 0 ->
		case lists:keysearch(with_bracket, 1, refac_syntax:get_ann(Node)) of
		  {value, {with_bracket, true}} ->
		      lay_parentheses(D1, Ctxt);
		  _ -> D1
		end;
	    _ -> D1
	  end;
      parentheses ->  %% done;
	  D = lay(refac_syntax:parentheses_body(Node), reset_prec(Ctxt)),
	  lay_parentheses(D, Ctxt);
      query_expr ->  %% done;
	  Ctxt1 = reset_prec(Ctxt),
	  D = lay(refac_syntax:query_expr_body(Node), Ctxt1),
	  sep([text("query"), nest(Ctxt1#ctxt.break_indent, D), text("end")]);
      receive_expr ->  %% done;
	  Ctxt1 = reset_prec(Ctxt),
	  D1 = lay_clauses(refac_syntax:receive_expr_clauses(Node), receive_expr, Ctxt1),
	  D2 = case refac_syntax:receive_expr_timeout(Node) of
		 none -> D1;
		 T ->
		     D3 = lay(T, Ctxt1),
		     A = refac_syntax:receive_expr_action(Node),
		     D4 = lay_elems(fun refac_prettypr_0:sep/1, seq(A, floating(text(", ")), Ctxt1, fun lay/2), A),
		     sep([D1, follow(floating(text("after")), append_clause_body(D4, D3, Ctxt1, {1, 1}), Ctxt1#ctxt.break_indent)])
	       end,
	  sep([text("receive"), nest(Ctxt1#ctxt.break_indent, D2), text("end")]);
      record_access ->
	  {PrecL, Prec, PrecR} = inop_prec('#'),
	  D1 = lay(refac_syntax:record_access_argument(Node), set_prec(Ctxt, PrecL)),
	  D2 = beside(floating(text(".")), lay(refac_syntax:record_access_field(Node), set_prec(Ctxt, PrecR))),
	  D3 = case refac_syntax:record_access_type(Node) of
		 none -> D2;
		 T ->
		     beside(beside(floating(text("#")), lay(T, reset_prec(Ctxt))), D2)
	       end,
	  maybe_parentheses(beside(D1, D3), Prec, Ctxt);
      record_expr ->  %% done;
	  {PrecL, Prec, _} = inop_prec('#'),
	  Ctxt1 = reset_prec(Ctxt),
	  D1 = lay(refac_syntax:record_expr_type(Node), Ctxt1),
	  Fields = refac_syntax:record_expr_fields(Node),
	  Sep = get_separator(Fields, Ctxt#ctxt.tokens, ","),
	  D2 = lay_elems(fun refac_prettypr_0:par/1, seq(refac_syntax:record_expr_fields(Node), floating(text(Sep)), Ctxt1, fun lay/2), Fields),
	  D3 = beside(beside(floating(text("#")), D1), beside(text("{"), beside(D2, floating(text("}"))))),
	  D4 = case refac_syntax:record_expr_argument(Node) of
		 none -> D3;
		 A -> beside(lay(A, set_prec(Ctxt, PrecL)), D3)
	       end,
	  maybe_parentheses(D4, Prec, Ctxt);
      record_field ->  %% done;
	  Ctxt1 = reset_prec(Ctxt),
	  D1 = lay(refac_syntax:record_field_name(Node), Ctxt1),
	  case refac_syntax:record_field_value(Node) of
	    none -> D1;
	    V -> D2 = lay(V, Ctxt1),
		 D1EndLn = get_end_line(refac_syntax:record_field_name(Node)),
		 D2StartLn = get_start_line(V),
		 case (D2StartLn - D1EndLn == 0) and (D1EndLn =/= 0) of
		   true -> beside(D1, beside(text(" = "), D2));
		   _ -> par([D1, floating(text(" = ")), D2], Ctxt1#ctxt.break_indent)
		 end
	  end;
      record_index_expr ->  %% done
	  {Prec, PrecR} = preop_prec('#'),
	  D1 = lay(refac_syntax:record_index_expr_type(Node), reset_prec(Ctxt)),
	  D2 = lay(refac_syntax:record_index_expr_field(Node), set_prec(Ctxt, PrecR)),
	  D3 = beside(beside(floating(text("#")), D1), beside(floating(text(".")), D2)),
	  maybe_parentheses(D3, Prec, Ctxt);
      rule -> %% done.
	  %% Comments on the name will be repeated; cf.
	  %% `function'.
	  Ctxt1 = reset_prec(Ctxt),
	  D1 = lay(refac_syntax:rule_name(Node), Ctxt1),
	  D2 = lay_clauses(refac_syntax:rule_clauses(Node), {rule, D1}, Ctxt1),
	  beside(D2, floating(text(".")));
      size_qualifier -> %%done;
	  Ctxt1 = set_prec(Ctxt, max_prec()),
	  Body = refac_syntax:size_qualifier_body(Node),
	  D1 = case refac_syntax:type(Body) == variable orelse refac_syntax:type(Body) == underscore orelse refac_syntax:is_literal(Body) == true
		   of
		 true -> lay(Body, Ctxt1);
		 false ->
		     case refac_syntax:type(Body) of
		       macro -> case lists:keysearch(with_bracket, 1, refac_syntax:get_ann(Body)) of
				  {value, {with_bracket, true}} ->
				      beside(floating(text("(")), beside(lay(Body, Ctxt1), floating(text(")"))));
				  _ -> lay(Body, Ctxt1)
				end;
		       _ -> beside(floating(text("(")), beside(lay(Body, Ctxt1), floating(text(")"))))
		     end
	       end,
	  D2 = lay(refac_syntax:size_qualifier_argument(Node), Ctxt1),
	  beside(D1, beside(text(":"), D2));
      try_expr ->
	      Ctxt1 = reset_prec(Ctxt),
	      Body = refac_syntax:try_expr_body(Node),
	      D1 = lay_body_elems(fun refac_prettypr_0:sep/1, seq(Body,floating(text(", ")),Ctxt1,fun lay/2),Body),			 
	      Es0 = [text("end")],
	      Es1 = case refac_syntax:try_expr_after(Node) of
			[] -> Es0;
			As ->
			    D2 = lay_elems(fun refac_prettypr_0:sep/1,seq(As,floating(text(", ")),Ctxt1,fun lay/2), As),
			    [text("after"),nest(Ctxt1#ctxt.break_indent,D2)| Es0]
		    end,
	      Es2 = case refac_syntax:try_expr_handlers(Node) of
			[] -> Es1;
			Hs ->
			    D3 = lay_clauses(Hs,try_expr,Ctxt1),
			    [text("catch"),nest(Ctxt1#ctxt.break_indent,D3)| Es1]
		    end,
	      {L,C}=refac_syntax:get_pos(hd(Body)),
	      TryLineNum=get_prev_keyword_line_num(Ctxt1#ctxt.tokens, {L,C}, 'try'),
	      case refac_syntax:try_expr_clauses(Node) of
		  [] -> 
		      case TryLineNum==L andalso TryLineNum/=0 of 
			  true ->
			      sep([beside(text("try "),nest(Ctxt1#ctxt.break_indent, D1))| Es2]);
			  false ->
			      sep([text("try"),nest(Ctxt1#ctxt.break_indent, D1)| Es2])
	    	      end;
		  Cs ->
		      {_, {L0,C0}} = refac_misc:get_range(lists:last(Body)),
		      OfLineNum = get_post_keyword_line_num(Ctxt1#ctxt.tokens, {L0,C0}, 'of'),
		      D4 = lay_clauses(Cs, try_expr, Ctxt1),
		      Es3 = [nest(Ctxt1#ctxt.break_indent, D4)| Es2],
		      case TryLineNum==L andalso TryLineNum/=0 of 
			  true ->
			      case OfLineNum==L0 andalso L0/=0 of
				  true ->
				      sep([beside(text("try "), beside(nest(Ctxt1#ctxt.break_indent, D1), text(" of")))| Es3]);
				  false ->
				      sep([beside(text("try "),nest(Ctxt1#ctxt.break_indent, D1)), text("of")| Es3])
			      end;
			  false ->
			      case OfLineNum==L0 andalso L0/=0 of
				  true ->
				      sep([text("try"), nest(Ctxt1#ctxt.break_indent, D1)| Es3]);
				  false ->
				      sep([text("try"), nest(Ctxt1#ctxt.break_indent, D1), text("of")| Es3])
			      end
		      end
	      end;
	  warning_marker ->
	      E = refac_syntax:warning_marker_info(Node),
	      beside(text("%% WARNING: "), lay_error_info(E, reset_prec(Ctxt)));
	   type -> empty();  %% tempory fix!!
	   typed_record_field -> empty() %% tempory fix!!!
      end.

lay_parentheses(D,_Ctxt) ->
    beside(floating(text("(")),beside(D,floating(text(")")))).

maybe_parentheses(D,Prec,Ctxt) ->
    case Ctxt#ctxt.prec of
      P when P > Prec -> lay_parentheses(D,Ctxt);
      _ -> D
    end.

maybe_parentheses_1(D, Node, Ctxt) ->
    Str=refac_prettypr_0:format(D),
    case hd(Str)==$\( andalso lists:last(Str)==$\) of 
	true ->
	    D;
	false ->
	    case has_parentheses(Node, Ctxt#ctxt.tokens) of 
		true ->		
		   lay_parentheses(D, Ctxt); 
		false ->
		    D
	    end
    end.
lay_qualified_name([S| Ss1] = Ss,Ctxt) ->
    case refac_syntax:type(S) of
      atom ->
	  case refac_syntax:atom_value(S) of
	    '' -> beside(text("."),lay_qualified_name_1(Ss1,Ctxt));
	    _ -> lay_qualified_name_1(Ss,Ctxt)
	  end;
      _ -> lay_qualified_name_1(Ss,Ctxt)
    end.

lay_qualified_name_1([S],Ctxt) -> lay(S,Ctxt);
lay_qualified_name_1([S| Ss],Ctxt) ->
    beside(lay(S,Ctxt),
	   beside(text("."),lay_qualified_name_1(Ss,Ctxt))).

%% lay_string/1 defined by Huiqing Li;
lay_string([]) -> empty();
lay_string([{string,_,Str}]) ->
    text("\""++Str++"\"");
lay_string([{string,_,Str}| Ts]) ->
    case Ts of
      [] -> text("\""++Str++"\"");
      _ ->
	  above(text("\""++Str++"\""),lay_string(Ts))
    end.

lay_string(S,Ctxt) ->
    %% S includes leading/trailing double-quote characters. The segment
    %% width is 2/3 of the ribbon width - this seems to work well.
    W = Ctxt#ctxt.ribbon * 2 div 3,
    lay_string_1(S,length(S),W).

lay_string_1(S,L,W) when L > W, W > 0 ->
    %% Note that L is the minimum, not the exact, printed length.
    case split_string(S,W - 1,L) of
      {_S1,""} -> text(S);
      {S1,S2} ->
	  above(text(S1 ++ "\""),lay_string_1([$"| S2],L - W +
							 1,W))
    end;
lay_string_1(S,_L,_W) -> text(S).

split_string(Xs,N,L) -> split_string_1(Xs,N,L,[]).

%% We only split strings at whitespace, if possible. We must make sure
%% we do not split an escape sequence.


split_string_1([$\s| Xs],N,L,As) when N =< 0, L >= 5 ->
    {lists:reverse([$\s| As]),Xs};
split_string_1([$\t| Xs],N,L,As) when N =< 0, L >= 5 ->
    {lists:reverse([$t,$\\| As]),Xs};
split_string_1([$\n| Xs],N,L,As) when N =< 0, L >= 5 ->
    {lists:reverse([$n,$\\| As]),Xs};
split_string_1([$\\| Xs],N,L,As) ->
    split_string_2(Xs,N - 1,L - 1,[$\\| As]);
split_string_1(Xs,N,L,As) when N =< -10, L >= 5 ->
    {lists:reverse(As),Xs};
split_string_1([X| Xs],N,L,As) ->
    split_string_1(Xs,N - 1,L - 1,[X| As]);
split_string_1([],_N,_L,As) -> {lists:reverse(As),""}.

split_string_2([$^,X| Xs],N,L,As) ->
    split_string_1(Xs,N - 2,L - 2,[X,$^| As]);
split_string_2([X1,X2,X3| Xs],N,L,As)
    when X1 >= $0, X1 =< $7, X2 >= $0, X2 =< $7, X3 >= $0,
	 X3 =< $7 ->
    split_string_1(Xs,N - 3,L - 3,[X3,X2,X1| As]);
split_string_2([X1,X2| Xs],N,L,As)
    when X1 >= $0, X1 =< $7, X2 >= $0, X2 =< $7 ->
    split_string_1(Xs,N - 2,L - 2,[X2,X1| As]);
split_string_2([X| Xs],N,L,As) ->
    split_string_1(Xs,N - 1,L - 1,[X| As]).

%% Note that there is nothing in `lay_clauses' that actually requires
%% that the elements have type `clause'; it just sets up the proper
%% context and arranges the elements suitably for clauses.


lay_clauses(Cs,Type,Ctxt) ->   %%done.
    CsDocs = seq(Cs,floating(text(";")),
		 Ctxt#ctxt{clause = Type},fun lay/2),
    lay_body_elems(fun vertical/1, CsDocs, Cs).

%% Note that for the clause-making functions, the guard argument
%% can be `none', which has different interpretations in different
%% contexts.


make_fun_clause(P,G,B,Ctxt,SameLine) ->
    make_fun_clause(none,P,G,B,Ctxt, SameLine).

make_fun_clause(N,P,G,B,Ctxt, SameLine) ->
    D = make_fun_clause_head(N,P,Ctxt),
    make_case_clause(D,G,B,Ctxt, SameLine).

make_fun_clause_head(N,P,Ctxt) ->
    D = lay_parentheses(P,Ctxt),
    if N == none -> D;
       true -> beside(N,D)
    end.

make_rule_clause(N,P,G,B,Ctxt, SameLine) ->
    D = make_fun_clause_head(N,P,Ctxt),
    append_rule_body(B,append_guard(G,D,Ctxt),Ctxt, SameLine).

make_case_clause(P,G,B,Ctxt, SameLine) ->
    append_clause_body(B,append_guard(G,P,Ctxt),Ctxt, SameLine).

make_if_clause(_P,G,B,Ctxt, SameLine) ->
    %% We ignore the patterns; they should be empty anyway.
    G1 = if G == none -> text("true");
	    true -> G
	 end,
    append_clause_body(B,G1,Ctxt, SameLine).

append_clause_body(B,D,Ctxt, SameLine={BodyStartLn, HeadLastLn}) ->
    Arrow = case BodyStartLn==HeadLastLn andalso BodyStartLn/=0 of 
 		true -> text(" -> ");
 		_ -> text(" ->")
 	    end,
    append_clause_body(B,D,floating(Arrow),Ctxt, SameLine).


append_rule_body(B,D,Ctxt, SameLine={BodyStartLn, HeadLastLn}) ->
    R = case BodyStartLn==HeadLastLn andalso BodyStartLn/=0 of
 	    true -> text(" :- ");
 	    _ -> text(" :-")
 	end,
    append_clause_body(B,D,floating(R),Ctxt, SameLine).

append_clause_body(B,D,S,Ctxt, _SameLine={BodyStartLn, HeadLastLn}) ->
    case BodyStartLn==0 orelse HeadLastLn==0 of 
	true ->
	     above(beside(D,S),nest(Ctxt#ctxt.break_indent,B));
	false ->
	    case BodyStartLn-HeadLastLn of 
		0 ->
		    beside(beside(D,S), nest(Ctxt#ctxt.break_indent,B));
		1 -> above(beside(D,S),nest(Ctxt#ctxt.break_indent,B));
		2 -> above(beside(D,S), nest(Ctxt#ctxt.break_indent,above(text(""),B)));
		_  -> sep([beside(D, S), nest(Ctxt#ctxt.break_indent, B)])
	    end
    end.

append_guard(none,D,_) -> D;
append_guard(G,D,Ctxt) ->
    par([D,follow(text("when"),G,Ctxt#ctxt.sub_indent)],
	Ctxt#ctxt.break_indent).

lay_bit_types([T],Ctxt) -> lay(T,Ctxt);
lay_bit_types([T| Ts],Ctxt) ->
    beside(lay(T,Ctxt),
	   beside(floating(text("-")),lay_bit_types(Ts,Ctxt))).

lay_error_info({L,M,T} = T0,Ctxt)
    when is_integer(L), is_atom(M) ->
    case catch M:format_error(T) of
      S when is_list(S) ->
	  if L > 0 ->
		 beside(text(io_lib:format("~w: ", [L])),text(S));
	     true -> text(S)
	  end;
      _ -> lay_concrete(T0,Ctxt)
    end;
lay_error_info(T,Ctxt) -> lay_concrete(T,Ctxt).

lay_concrete(T,Ctxt) ->
    lay(refac_syntax:abstract(T),Ctxt).

seq([H| T],Separator,Ctxt,Fun) ->
    case T of
	[] -> [Fun(H,Ctxt)];
	_->
	    [maybe_append(Separator,Fun(H,Ctxt))| seq(T,Separator,
						      Ctxt,Fun)]
    end;
seq([],_,_,_) -> [empty()].

maybe_append(none,D) -> D;
maybe_append(Suffix,D) -> beside(D,Suffix).

vertical([D]) -> D;
vertical([D| Ds]) -> above(D,vertical(Ds));
vertical([]) -> [].

vertical_sep(_Sep,[D]) -> D;
vertical_sep(Sep,[D| Ds]) ->
    above(above(D,Sep),vertical_sep(Sep,Ds));
vertical_sep(_Sep,[]) -> [].

spaces(N) when N > 0 -> [$\s| spaces(N - 1)];
spaces(_) -> [].

tidy_float([$.,C| Cs]) ->
    [$.,C| tidy_float_1(Cs)];  % preserve first decimal digit
tidy_float([$e| _] = Cs) -> tidy_float_2(Cs);
tidy_float([C| Cs]) -> [C| tidy_float(Cs)];
tidy_float([]) -> [].

tidy_float_1([$0,$0,$0| Cs]) ->
    tidy_float_2(Cs);    % cut mantissa at three consecutive zeros.
tidy_float_1([$e| _] = Cs) -> tidy_float_2(Cs);
tidy_float_1([C| Cs]) -> [C| tidy_float_1(Cs)];
tidy_float_1([]) -> [].

tidy_float_2([$e,$+,$0]) -> [];
tidy_float_2([$e,$+,$0| Cs]) ->
    tidy_float_2([$e,$+| Cs]);
tidy_float_2([$e,$+| _] = Cs) -> Cs;
tidy_float_2([$e,$-,$0]) -> [];
tidy_float_2([$e,$-,$0| Cs]) ->
    tidy_float_2([$e,$-| Cs]);
tidy_float_2([$e,$-| _] = Cs) -> Cs;
tidy_float_2([$e| Cs]) -> tidy_float_2([$e,$+| Cs]);
tidy_float_2([_C| Cs]) -> tidy_float_2(Cs);
tidy_float_2([]) -> [].

%% =====================================================================

get_start_line(Node) ->
    PreComs = refac_syntax:get_precomments(Node),
    {{L, _C}, _} = get_start_end_loc(Node),
    case PreComs of
	[] -> L;
	_ -> element(1, refac_syntax:get_pos(hd(PreComs)))
    end.

get_end_line(Node) ->
    PostComs = refac_syntax:get_postcomments(Node),
    {_, {L, _C}} = get_start_end_loc(Node),
    case PostComs of
	[] -> L;
      _ -> LastCom = lists:last(PostComs),
	   LastComText = refac_syntax:comment_text(LastCom),
	   element(1, refac_syntax:get_pos(LastCom)) +
	       length(LastComText) - 1
    end.

get_start_end_loc(Node) ->	     
    {Starts, Ends} = 
	case is_list(Node) of 
	    true ->
		lists:unzip([refac_misc:get_start_end_loc(N)||N<-Node]);
	    false ->
		lists:unzip([refac_misc:get_start_end_loc(N)||N<-[Node]])
	end,
    {Starts1, Ends1} = {lists:usort(Starts)--[{0,0}], lists:usort(Ends)--[{0,0}]},
    Start=case Starts1==[] of 
	      true -> {0,0};
	      false ->
		  hd(Starts1)
	  end,
    End = case Ends1==[] of 
	      true -> {0,0};
	      false ->
		  lists:last(Ends1)
	  end,
    {Start, End}.
 
horizontal([D]) -> D;
horizontal([D| Ds]) -> beside(beside(D, nil()),horizontal(Ds));
horizontal([]) -> [].

lay_elems(_Fun, _ElemDocs,[]) -> null;
lay_elems(Fun, ElemDocs,Elems) ->
    ARanges = lists:map(fun (A) ->
				{get_start_line(A), get_end_line(A)}
			end,
			Elems),
    lay_elems_1(Fun, lists:zip(ElemDocs,ARanges),[],0).

lay_elems_1(Fun, [], Acc, _LastLine) ->
    Docs = lists:map(fun (Ds) -> horizontal(Ds) end, Acc),
    Fun(lists:reverse(Docs));
lay_elems_1(Fun, [{D, {_SLn, ELn}}| Ts], [], _LastLn) ->
    lay_elems_1(Fun, Ts, [[D]], ELn);
lay_elems_1(Fun, [{D, {SLn, ELn}}| Ts], [H| T], LastLn) ->
    case SLn == 0 orelse LastLn == 0 orelse SLn < LastLn of
	true ->
	    lay_elems_1(Fun, Ts, [H ++ [D]| T], ELn);
	false ->
	    case SLn - LastLn of
		0 -> lay_elems_1(Fun, Ts, [H ++ [D]| T], ELn);
		1 ->
		lay_elems_1(Fun, Ts, [[above(horizontal(H), D)]| T], ELn);
		_ ->
		lay_elems_1(Fun, Ts, [[above(horizontal(H), above(text(""), D))]| T], ELn)
	    end
    end.


lay_body_elems(_Fun, _ElemDocs,[]) -> null;
lay_body_elems(Fun, ElemDocs,Elems) ->
    ARanges = lists:map(fun (A) ->
				{get_start_line(A), get_end_line(A)}
			end,
			Elems),
    lay_body_elems_1(Fun, lists:zip(ElemDocs,ARanges),[],0).

lay_body_elems_1(Fun, [], Acc, _LastLine) ->
    Docs = lists:map(fun (Ds) -> horizontal(Ds) end, Acc),
    Fun(lists:reverse(Docs));
lay_body_elems_1(Fun, [{D, {_SLn, ELn}}| Ts], [], _LastLn) ->
    lay_body_elems_1(Fun, Ts, [[D]], ELn);
lay_body_elems_1(Fun, [{D, {SLn, ELn}}| Ts], [H| T], LastLn) ->
    case SLn == 0 orelse LastLn == 0 orelse SLn < LastLn of
	true -> 
	    lay_body_elems_1(Fun, Ts, [[above(horizontal(H), D)]| T], ELn);
	false ->
	    case SLn - LastLn of
		0 -> 
		    lay_body_elems_1(Fun, Ts, [H ++ [D]| T], ELn);
		1 ->
		    lay_body_elems_1(Fun, Ts, [[above(horizontal(H), D)]| T], ELn);
		_ ->
		    lay_body_elems_1(Fun, Ts, [[above(horizontal(H), above(text(""), D))]| T], ELn)
	    end
    end.
  
nil() -> text("").

get_prev_keyword_line_num(FormToks, StartPos, KeyWord)->
    Ts1 = lists:takewhile(
	    fun(T) ->
		    token_loc(T)=<StartPos
	    end, FormToks),
    Ts2=lists:dropwhile(fun(T)->
				element(1, T)/=KeyWord
			end, lists:reverse(Ts1)),
    case Ts2 of 
	[] ->
	    0;
	[T1|_] ->
	    element(1, token_loc(T1))
    end.

get_post_keyword_line_num(FormToks, EndPos, KeyWord)->
    Ts1 = lists:dropwhile(
	    fun(T) ->
		    token_loc(T)=<EndPos
	    end, FormToks),
    Ts2=lists:dropwhile(fun(T)->
				element(1, T)/=KeyWord
			end, Ts1),
    case Ts2 of 
	[] ->
	    0;
	[T1|_] ->
	    element(1, token_loc(T1))
    end.
	    
token_loc(T) ->
    case T of
      {_, L, _V} -> L;
      {_, L1} -> L1
    end.

get_separator(_NodeList, [], Default) ->
    Default;
get_separator([],_, Default) ->
    Default;
get_separator(NodeList, Toks, Default) when is_list(NodeList) -> 
    NodeToks = get_node_toks(Toks,NodeList),
    NodeListToks = lists:append([get_node_toks(Toks, Elem)||
				    Elem<-NodeList]),
    SepToks = NodeToks -- NodeListToks,
    case SepToks of 
	[] ->
	    Default;
	_ ->
	    {OnlyComma, CommaWithSpace} =get_comma_tokens(SepToks),     
	    case length(OnlyComma) >length(CommaWithSpace) of 
		true ->
		    ",";
		false ->
		    ", "
	    end
    end;
get_separator(_Node, _Toks, Default) ->
    Default.


get_node_toks(Toks, Node) ->
    {Start, End} = get_start_end_loc(Node),
    case Start =={0,0} orelse End=={0,0} of
	true -> [];
	false ->
	    Toks1 = lists:dropwhile(
		      fun(T) ->
			      token_loc(T)<Start
		      end, Toks),
	    lists:takewhile(fun(T)->
				    token_loc(T)=<End
			    end, Toks1)
    end.

get_comma_tokens(Toks) ->   
    get_comma_tokens(Toks, {[],[]}).
get_comma_tokens([], {OnlyComma, CommaWithSpace}) ->
    {OnlyComma, CommaWithSpace};
get_comma_tokens([T|Ts], {OnlyComma, CommaWithSpace}) ->
    case T of
	{',', _} ->
	    case Ts  of 
		[T1={whitespace, _, _}|Ts1] ->
		    get_comma_tokens(Ts1, {OnlyComma, [{T, T1}|CommaWithSpace]});
		_ ->
		    get_comma_tokens(Ts, {[T|OnlyComma], CommaWithSpace})
	    end;
	_ ->
	    get_comma_tokens(Ts,  {OnlyComma, CommaWithSpace})
    end.
get_operator_text(Node, Toks, Default) ->
    case Toks of
	[] ->
	    Default;
	_ ->
	    {Start, End} =get_start_end_loc(Node),
	    case Start =={0,0} orelse End=={0,0} of 
		true -> Default;
		_ ->
		    Toks1 = 
			lists:takewhile(
			  fun(T) ->
				  token_loc(T)<Start
			  end, Toks),
		    Toks2 =lists:dropwhile(
			     fun(T) ->
				     token_loc(T)=<End
			     end, Toks),
		    Pre=case lists:reverse(Toks1) of
			    [{whitespace, _,_}|_] ->
				" ";
			    _ ->
				""
			end,
		    Post = case Toks2 of
			       [{whitespace, _,_}|_] ->
				   " ";
			       _ ->
				   ""
			   end,   
		    Pre ++ Default++Post
	    end
    end.

remove_trailing_whitespace(Str) ->
    remove_trailing_whitespace(Str, []).

remove_trailing_whitespace([], Acc) ->
    lists:reverse(Acc);
remove_trailing_whitespace([$,,$ ,$\r| S], Acc) ->
    remove_trailing_whitespace(S, [$\r,$,| Acc]);
remove_trailing_whitespace([$,,$ ,$\n| S], Acc) ->
    remove_trailing_whitespace(S, [$\n,$,| Acc]);
remove_trailing_whitespace([C| S], Acc) ->
    remove_trailing_whitespace(S, [C| Acc]).


has_parentheses(Node, Toks) ->
    precedent_by_open_bracket(Node, Toks) andalso 
	followed_by_close_bracket(Node, Toks).

precedent_by_open_bracket(Node, Toks)->		  
    {StartLoc,_EndLoc} = get_start_end_loc(Node),
    Toks1 = lists:reverse(lists:takewhile(fun (B) -> element(2, B) =/= StartLoc end, Toks)),
    Toks2 = lists:dropwhile(fun (B) -> case B of
					   {whitespace, _, _} -> true;
					   _ -> false
				       end
			    end, Toks1),
     case Toks2 of
	[{'(', _}|_] ->
	    true;
	_ ->
	    false
    end.


followed_by_close_bracket(Node, Toks)->		  
    {_, EndLoc} = get_start_end_loc(Node),
    Toks1 = lists:dropwhile(fun (B) -> element(2, B) =/= EndLoc end, Toks),
    case Toks1 of 
	[] -> false;
	[_|T1] ->
	    Toks2 = lists:dropwhile(fun (B) -> case B of
						   {whitespace, _, _} -> true;
						   _ -> false
					       end
				    end, T1),
	     case Toks2 of
		[{')', _}|_] ->
		    true;
		_ ->
		    false
	    end
    end.


no_of_changed_funs_toks(AST) ->
    Forms = refac_syntax:form_list_elements(AST),
    Res =[case form_not_changed(Form) of 
	      true -> {0, 0, 0};
	      false ->
		  {ToksRemoved, ToksAdded} =
		      change_of_tokens(Form),
		  {1, ToksRemoved, ToksAdded}
	  end || Form <-Forms],
    {ChangedFuns, TotalToksRemoved, TotalToksAdded} =
	lists:unzip3(Res),
    {lists:sum(ChangedFuns), lists:sum(TotalToksRemoved),
     lists:sum(TotalToksAdded)}.

change_of_tokens(Form) ->
    OriginalToks = rm_loc_in_toks(refac_misc:get_toks(Form)),
    {ok, NewToks0, _} = refac_scan:string(print_a_form(Form, undefined, [])),
    NewToks = rm_loc_in_toks(NewToks0),
    ToksRemoved = OriginalToks--NewToks,
    ToksAdded = NewToks--OriginalToks,
    {length(ToksRemoved), length(ToksAdded)}.


rm_loc_in_toks(Ts) ->
    [rm_loc_in_a_tok(T)||T<-Ts, not(is_whitespace_or_comment(T))].

rm_loc_in_a_tok(T) ->
    D = {0,0},
    case T of 
	{var, _L, V} -> {var, D, V};
	{integer, _L, V} -> {integer, D, V};
	{string, _L, V} -> {string, D, V};
	{float, _L, V} -> {float, D, V}; 
	{char, _L, V}  -> {char, D, V};
	{atom, _L, V} -> {atom, D, V};
	{A, _L} ->{A, D};
	Other  -> 
	    Other
    end.


is_whitespace_or_comment({whitespace, _, _}) ->
    true;
is_whitespace_or_comment({comment, _, _}) ->
    true;
is_whitespace_or_comment(_) -> false.

