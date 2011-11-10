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
%%
%% Modified: 17 Jan 2007 by  Huiqing Li <hl@kent.ac.uk>
%% =====================================================================
%%
%% @doc Pretty printing of abstract Erlang syntax trees.
%%
%% <p>This module is a front end to the pretty-printing library module
%% <code>prettypr</code>, for text formatting of abstract syntax trees
%% defined by the module <code>wrangler_syntax</code>.</p>

%% @hidden
%% @private
-module(wrangler_prettypr).

-export([format/1,print_ast/2, print_ast/3, 
         print_ast_and_get_changes/3, print_a_form/4]).

%% this should removed to refac_util.
-export([has_parentheses/2]).

%% used by testing.
-export([calc_levenshtein_dist/3, levenshtein_dist/3]).

-import(wrangler_prettypr_0,
	[text/1,nest/2,above/2,beside/2,sep/1,par/1,par/2,
	 floating/3,floating/1,break/1,follow/2,follow/3,
	 empty/0, format/3, best/3]).

-import(wrangler_parse,
	[preop_prec/1,inop_prec/1,func_prec/0,max_prec/0]).

-define(PADDING, 2).

-define(PAPER, 80).

-define(RIBBON, 72).

-define(NOUSER, undefined).

-define(NOHOOK, none).

-define(TabWidth, 8).

-record(ctxt,
	{prec = 0,
         sub_indent = 2,
         break_indent = 4,
	 clause = undefined,
         hook = ?NOHOOK,
         paper = ?PAPER,
	 ribbon = ?RIBBON,
         user = ?NOUSER,
 	 tokens = [],
	 tabwidth=?TabWidth,
         check_bracket = false,
	 format = unknown}).


%% ====================================================================
%% user-program guided pretty-printing of an abstract syntax tree which
%% must be a form list.

print_ast(FileFmt,AST) ->
    print_ast(FileFmt,AST, 8).

print_ast(FileFmt, AST, TabWidth) ->
    print_ast(FileFmt, AST, [], TabWidth).

print_ast(FileFmt, AST, Options, TabWidth) ->
    element(1, print_ast_and_get_changes(FileFmt, AST, Options, TabWidth)).

print_ast_and_get_changes(FileFmt, AST, TabWidth) ->
    print_ast_and_get_changes(FileFmt, AST, [], TabWidth).

print_ast_and_get_changes(FileFmt, AST, Options, TabWidth) ->
    Fs = wrangler_syntax:form_list_elements(AST),
    %% {FmStrs, C} = lists:unzip([print_a_form(reset_attrs(F), FileFmt, Options, TabWidth)|| F<-Fs]),
    {FmStrs, C} = lists:unzip([print_a_form(F, FileFmt, Options, TabWidth)|| F<-Fs]),
    Content = lists:append(FmStrs),
    {NoFunsChanged, NoToksRemoved, NoToksAdded} =lists:unzip3(C),
    Change={lists:sum(NoFunsChanged), 
            lists:sum(NoToksRemoved),
            lists:sum(NoToksAdded)},
    {Content, Change}.

print_a_form(Form, FileFmt, Options, TabWidth) ->
    case form_not_changed(Form) of
        true ->
            FormStr=wrangler_misc:concat_toks(wrangler_misc:get_toks(Form)),
            {FormStr, {0, 0, 0}};
        false ->
            print_a_form_and_get_changes(Form, FileFmt, Options, TabWidth)
    end.
 
       
print_a_form_and_get_changes(Form, FileFormat, Options, TabWidth) ->
    Ctxt = #ctxt{hook  = proplists:get_value(hook,Options,?NOHOOK),
		 paper = proplists:get_value(paper,Options,?PAPER),
		 ribbon = proplists:get_value(ribbon,Options,?RIBBON),
		 user = proplists:get_value(user,Options),
		 format = FileFormat,
		 tabwidth = TabWidth,
		 tokens = wrangler_misc:get_toks(Form)},
    OrigFormStr=wrangler_misc:concat_toks(wrangler_misc:get_toks(Form)),
    NewFormStr0= print_form(Form,reset_prec(Ctxt),fun lay/2),
    NewFormStr=repair_new_form_str(OrigFormStr, NewFormStr0, TabWidth,FileFormat),
    {ok, OrigToks, _} = wrangler_scan:string(OrigFormStr),
    {ok, NewToks, _} = wrangler_scan:string(NewFormStr),
    Change =get_changes(OrigToks, NewToks),
    {NewFormStr, Change}.
   

get_changes(OrigToks, NewToks) ->
    OrigToks1 = remove_locs_whites_and_comments(OrigToks),
    NewToks1 = remove_locs(NewToks),
    ToksRemoved = OrigToks1 -- NewToks1,
    ToksAdded = NewToks1 -- OrigToks1,
    {1, length(ToksRemoved), length(ToksAdded)}.

 
print_form(Form,Ctxt,Fun) ->
    Paper = Ctxt#ctxt.paper,
    Ribbon = Ctxt#ctxt.ribbon,
    D0 = Fun(Form, Ctxt),
    D=best(D0,Paper,Ribbon),
    FileFormat = Ctxt#ctxt.format,
    TabWidth = Ctxt#ctxt.tabwidth,
    FStr0=wrangler_prettypr_0:layout(D,FileFormat,TabWidth),
    FStr=remove_trailing_whitespace(FStr0, TabWidth, FileFormat), 
    Toks = wrangler_misc:get_toks(Form),
    if Toks ==[] ->
            Delimitor = get_delimitor(FileFormat),
            Delimitor++Delimitor++FStr; 
       true ->
            FStr
    end.


get_delimitor(FileFormat) ->        
    case FileFormat of
        dos -> "\r\n";
        mac -> "\r";
        _ -> "\n"
    end.
  
is_special_form(Form) ->
    case wrangler_syntax:type(Form) of
	error_marker -> 
            true;
	comment -> 
            true; 
	attribute ->
	    AtrName = wrangler_syntax:attribute_name(Form),
            case wrangler_syntax:atom_value(AtrName) of
		type -> true;
                opaque -> true;
                record -> 
                    [_R, FieldTuple] = wrangler_syntax:attribute_arguments(Form),
                    Fields = wrangler_syntax:tuple_elements(FieldTuple),
                    lists:any(fun(F) ->  
                                      wrangler_syntax:type(F)==typed_record_field
                              end, Fields);
		_ -> false
	    end;
	_ -> false
    end.

form_not_changed(Form) ->
    case is_special_form(Form) of
	true ->
            %% This might change!
            true;  
	false ->
            Toks = wrangler_misc:get_toks(Form),
            case Toks of
                [] -> 
                    false;
                _ ->
                    form_not_change_1(Form)
            end
    end.

%% is_spec_form(Form) ->
%%     case wrangler_syntax:type(Form) of 
%%         attribute ->
%%             spec == wrangler_syntax:atom_value(
%%                       wrangler_syntax:attribute_name(Form));
%%         _ -> false
%%     end.
        
form_not_change_1(Form) ->
    try
        Toks = wrangler_misc:get_toks(Form),
        Str = wrangler_misc:concat_toks(Toks),
        {ok,Toks1,_} = wrangler_scan:string(Str,{1,1},?TabWidth,unix),
        OriginalForm = wrangler_epp_dodger:normal_parser(Toks1,[]),
        NewStr = format(Form,[]),
        {ok,Toks2,_} =
            wrangler_scan:string(NewStr,{1,1},?TabWidth,unix),
        NewForm = wrangler_epp_dodger:normal_parser(Toks2,[]),
        B1=best(OriginalForm), 
        B2=best(NewForm), 
        B1==B2
    of 
        Res -> Res
    catch 
        _E1:_E2 ->
            false
    end.


set_prec(Ctxt,Prec) ->
    Ctxt#ctxt{prec = Prec}.   


reset_prec(Ctxt) ->
    set_prec(Ctxt,0).    

reset_check_bracket(Ctxt) ->
    Ctxt#ctxt{check_bracket=false}.

%% =====================================================================
%% @spec format(Tree::syntaxTree()) -> string()
%% @equiv format(Tree, [])


format(Node) -> 
    format(Node,[]).
   

%% =====================================================================
%% @spec format(Tree::syntaxTree(), Options::[term()]) -> string()
%%           syntaxTree() = wrangler_syntax:syntaxTree()
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
%% @see wrangler_syntax
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
    case wrangler_syntax:get_ann(Node) of
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
    case wrangler_syntax:has_comments(Node) of
	true ->
            D1 = lay_2(Node, Ctxt),
	    PreCs = wrangler_syntax:get_precomments(Node),
	    PostCs = wrangler_syntax:get_postcomments(Node),
            {{NodeStartLine, NodeStartCol},{NodeEndLn, _}} = get_start_end_loc(Node),
	    D2 = lay_postcomments_1(PostCs, D1, NodeEndLn),
            lay_precomments(PreCs, D2, {NodeStartLine, NodeStartCol});
	false ->
	    lay_2(Node, Ctxt)
    end.

%% For pre-comments, all padding is ignored.
%% this might change the layout of comments!
lay_precomments([],D, _) -> D;
lay_precomments(Cs,D, {DStartLine, DStartCol}) ->
    D0 =stack_comments(Cs,false),
    LastCom = lists:last(Cs),
    {_, Col} = wrangler_syntax:get_pos(hd(Cs)),
    {Line, _Col} = wrangler_syntax:get_pos(LastCom),
    LastComTest = wrangler_syntax:comment_text(LastCom),
    Offset = lists:min([abs(DStartCol-Col),0]),
    N = DStartLine-(Line+length(LastComTest)),
    case N=<0 orelse Line ==0 orelse DStartLine==0 of
        true ->
            above(D0, nest(Offset, D));
        false ->
            vertical([D0, white_lines(N), nest(Offset,D)])
    end.
   

lay_postcomments_1([], D, _) -> D;
lay_postcomments_1(Cs, D, DEndLn) ->
    {PostCsLn, _} = wrangler_syntax:get_pos(hd(Cs)),
    case PostCsLn >= DEndLn + 1 of
        true ->
            lay_postcomments(Cs, above(D, text("")));
        false ->
            lay_postcomments(Cs, D)
    end.
    
%% For postcomments, individual padding is added.
lay_postcomments(Cs,D) ->
    D0 =floating(break(stack_comments(Cs,true)),1,0),
    beside(D,D0).

%% Format (including padding, if `Pad' is `true', otherwise not)
%% and stack the listed comments above each other,
stack_comments([C| Cs],Pad) ->
    ComText=wrangler_syntax:comment_text(C),
    D = stack_comment_lines(ComText),
    D1 = case Pad of
             true ->
                 P = case wrangler_syntax:comment_padding(C) of
                         none -> ?PADDING;
                         P1 -> P1
                     end,
                 beside(text(spaces(P)),D);
	   false -> D
	 end,
    case Cs of
        [] ->
            D1;    % done
        _ -> 
            {L,_} = wrangler_syntax:get_pos(C),
            {L1, _} = wrangler_syntax:get_pos(hd(Cs)),
            N = L1 - (L+length(ComText)-1),
            if N>1 ->
                    vertical([D1, white_lines(N-1),stack_comments(Cs, Pad)]);
               true ->
                    above(D1,stack_comments(Cs,Pad))
            end
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
    case wrangler_syntax:type(Node) of
	%% We list literals and other common cases first.
	variable -> 
            text(wrangler_syntax:variable_literal(Node));
	atom ->
            As= wrangler_syntax:get_ann(Node),
            case lists:keysearch(qatom, 1, As) of
                {value, _} ->
                    Lit = atom_to_list(wrangler_syntax:atom_value(Node)),
                    text("'"++Lit++"'");
                false ->
                    Lit = wrangler_syntax:atom_literal(Node),
                    text(Lit)
            end;
        integer -> 
	    text(wrangler_syntax:integer_literal(Node));
	float ->
	    text(tidy_float(wrangler_syntax:float_literal(Node)));
	text -> 
	    text(wrangler_syntax:text_string(Node));
	string ->  
	    Str = wrangler_syntax:string_literal(Node),
	    StrVal = "\"" ++ wrangler_syntax:string_value(Node) ++ "\"",
	    case lists:keysearch(toks, 1, wrangler_syntax:get_ann(Node)) of
		{value, {toks, StrToks}} ->
		    Str1 = io_lib:write_string(
			     lists:concat(lists:map
					    (fun ({string, _, S}) 
						 -> S 
					     end, StrToks))),
		    case Str1 == Str of
			true -> lay_string(StrToks);
			_ -> lay_string(StrVal, Ctxt)
		    end;
		_ -> lay_string(StrVal, Ctxt)
	    end;
	nil -> 
            text("[]");
	tuple ->
            Es0 = wrangler_syntax:tuple_elements(Node),
	    Sep = get_separator(Es0, Ctxt, ","),
	    Es = seq(Es0, floating(text(Sep)), reset_check_bracket(reset_prec(Ctxt)), fun lay/2),
	    Es1=lay_elems(fun wrangler_prettypr_0:par/1, Es, Es0, Ctxt),
            {{StartLn, StartCol}, {EndLn, EndCol}} = get_start_end_loc(Node),
            case Es0 of
                [] ->
                    beside(text("{"), text("}"));
                _ ->
                    {HdStartLn, HdStartCol} = 
                        get_start_loc_with_comment(hd(Es0)),
                    {LastEndLn, _LastEndCol} = 
                        get_end_loc_with_comment(lists:last(Es0)),
                    N = HdStartLn - StartLn,
                    Offset = HdStartCol - StartCol, 
                    D1 =case N=<0 orelse HdStartLn==0 orelse StartLn==0 of 
                            true ->
                                Pad = HdStartCol-StartCol-1,
                                case Pad>0 andalso HdStartCol/=0 andalso StartCol/=0 of 
                                    true ->
                                        beside(text("{"), beside(text(spaces(Pad)), Es1));
                                    _ ->
                                        beside(text("{"), Es1)
                                end;
                            _ ->
                                above(text("{"), nest(Offset, Es1))
                        end,
                    N1 = EndLn - LastEndLn,
                    case N1=<0  orelse HdStartLn==0 orelse StartLn==0 of 
                        true ->
                            beside(D1, text("}"));
                        false ->
                            above(D1, nest(EndCol - StartCol, text("}")))
                    end
            end;
	list -> 
            lay_list(Node, Ctxt);
	operator ->
	    Op = wrangler_syntax:operator_literal(Node),
            floating(text(Op));
        infix_expr ->
            Left = wrangler_syntax:infix_expr_left(Node),
	    Operator = wrangler_syntax:infix_expr_operator(Node),
	    Right = wrangler_syntax:infix_expr_right(Node),
	    {PrecL, Prec, PrecR} = case wrangler_syntax:type(Operator) of
				       operator ->
					   inop_prec(wrangler_syntax:operator_name(Operator));
				       _ -> {0, 0, 0}
				   end,
            Ctxt2 = reset_check_bracket(set_prec(Ctxt, PrecL)),
	    D1 = maybe_parentheses_1(lay(Left, Ctxt2), Left, Ctxt2),
	    D2 = lay(Operator, reset_prec(Ctxt)),
            Ctxt3 = reset_check_bracket(set_prec(Ctxt, PrecR)),
	    D3 = maybe_parentheses_1(lay(Right, Ctxt3), Right, Ctxt3),
            {{_LeftStartLn, LeftStartCol},{LeftEndLn, _LeftEndCol}} = get_start_end_loc(Left),
	    {OpStartLn, OpStartCol} = get_start_loc_with_comment(Operator),
	    {RightStartLn, RightStartCol} = get_start_loc_with_comment(Right),
	    D12 = case OpStartLn == LeftEndLn andalso OpStartLn =/= 0 of
		      true -> wrangler_prettypr_0:horizontal([D1, D2]);
		      false when OpStartLn-LeftEndLn==1 ->
                          above(D1, nest(OpStartCol-LeftStartCol-1, D2));
		      _ -> par([D1, D2], Ctxt#ctxt.sub_indent)
		  end,
            D4 = case OpStartLn == RightStartLn andalso OpStartLn =/= 0 of
		     true ->
			 wrangler_prettypr_0:horizontal([D12, D3]);
		     false when RightStartLn-OpStartLn==1 ->
                         above(D12, nest(RightStartCol-LeftStartCol, D3));
		     _ ->
			 par([D12, D3], Ctxt#ctxt.sub_indent)
		 end,
            maybe_parentheses_2(D4, Node, Prec, Ctxt);
	prefix_expr ->  
	    %% done;
	    Operator = wrangler_syntax:prefix_expr_operator(Node),
	    PrefixArg = wrangler_syntax:prefix_expr_argument(Node),
	    {{Prec, PrecR}, Name} = case wrangler_syntax:type(Operator) of
					operator ->
					    N = wrangler_syntax:operator_name(Operator),
					    {preop_prec(N), N};
					_ -> {{0, 0}, any}
				    end,
	    D1 = lay(Operator, reset_check_bracket(reset_prec(Ctxt))),
            Ctxt1 = reset_check_bracket(set_prec(Ctxt, PrecR)),                            
            D2 = maybe_parentheses_1(lay(PrefixArg, Ctxt1), PrefixArg, Ctxt1),
            {OpEndLn, OpEndCol} = get_end_loc_with_comment(Operator),
	    {ArgStartLn, ArgStartCol} = get_start_loc_with_comment(PrefixArg),
	    D3=case ArgStartLn-OpEndLn==0 andalso ArgStartLn/=0 of 
		   true -> 
		       S=text(empty_str(lists:min([abs(ArgStartCol-OpEndCol-1), 1]))),
                       beside(beside(D1, S), D2);
		   false when  ArgStartLn-OpEndLn==1 ->
		       above(D1, nest(ArgStartCol-OpEndCol, D2));
		   _ ->
		       case Name of
			   '+' -> beside(D1, D2);
			   '-' -> beside(D1, D2);
			   _ ->
			       par([D1, D2], Ctxt#ctxt.sub_indent)
		       end
	       end,
    	    maybe_parentheses(D3, Prec, Ctxt);
	application ->  
            {PrecL, Prec} = func_prec(),
            Op =wrangler_syntax:application_operator(Node),
            Args = wrangler_syntax:application_arguments(Node),
            D = lay(Op,reset_check_bracket(set_prec(Ctxt, PrecL))),
            Sep = get_separator(Args, Ctxt, ","),
	    Ctxt1 =case length(Args)==1 andalso 
                       lists:member(wrangler_syntax:type(hd(Args)),
                                    [macro, infix_expr]) of
                       true ->
                           Ctxt#ctxt{check_bracket=true};
                       false ->
                           Ctxt
                   end,
            As =seq(Args, floating(text(Sep)), reset_prec(Ctxt1), fun lay/2),
            D1 = case Args of
                     [] ->
			 beside(D, beside(text("("), text(")")));
                     _ ->
                         ArgsD=lay_elems(fun wrangler_prettypr_0:par/1, As, Args, Ctxt),
                         {OpStartLoc,OpEndLoc}=get_start_end_loc(Op),
                         ArgsD1=make_args(Args, ArgsD,Ctxt,OpEndLoc,'(',')'),
                         LeftBracketLoc=get_keyword_loc_after('(', Ctxt, OpEndLoc),
                         append_elems(fun horizontal/1,
                                      {D, {OpStartLoc, OpEndLoc}},
                                      {ArgsD1, {LeftBracketLoc, LeftBracketLoc}})
                 end,
            maybe_parentheses(D1, Prec, Ctxt);
	match_expr ->    
            {PrecL, Prec, PrecR} = inop_prec('='),
	    Left = wrangler_syntax:match_expr_pattern(Node),
	    Right = wrangler_syntax:match_expr_body(Node),
	    D1 = lay(Left, reset_check_bracket(set_prec(Ctxt, PrecL))),
	    D2 = lay(Right, reset_check_bracket(set_prec(Ctxt, PrecR))),
	    {LStart, LEnd} = get_start_end_loc(Left),
	    {RStart, REnd} = get_start_end_loc(Right),
            EqLoc = get_keyword_loc_after('=',Ctxt, LEnd),
            LeftEq=append_elems(fun wrangler_prettypr_0:horizontal/1,
				{D1, {LStart, LEnd}}, {text("="), {EqLoc, EqLoc}}),
            D3=append_elems(fun wrangler_prettypr_0:horizontal/1,
                            {LeftEq, {LStart, EqLoc}},
			    {D2,{RStart, REnd}}),
	    maybe_parentheses(D3, Prec, Ctxt);
	underscore ->
	    text("_");
	clause ->  
	    %% Done;
	    %% The style used for a clause depends on its context
	    Ctxt1 = reset_check_bracket((reset_prec(Ctxt))#ctxt{clause = undefined}),
	    Pats = wrangler_syntax:clause_patterns(Node),
	    Body = wrangler_syntax:clause_body(Node),
	    Sep = get_separator(Pats, Ctxt, ","),
	    PatDocs = seq(Pats, floating(text(Sep)), Ctxt1, fun lay/2),
	    D1 = lay_elems(fun wrangler_prettypr_0:par/1, PatDocs, Pats, Ctxt),
	    Guard=wrangler_syntax:clause_guard(Node),
	    D2 = case Guard of
		     none -> none;
		     G -> lay(G,Ctxt1)
		 end,
	    BodyDocs = seq(Body, floating(text(",")), Ctxt1, fun lay/2),
	    D3 = lay_body_elems(BodyDocs, Body, Ctxt1),
	    HeadLastLn = case wrangler_syntax:clause_guard(Node) of
			     none -> case Pats of
					 [] -> get_start_line_with_comment(Node);
					 _ -> case get_end_line_with_comment(Pats) of
						  0 -> get_start_line_with_comment(Node);
						  L -> L
					      end
				     end;
			     _ -> get_end_line_with_comment(wrangler_syntax:clause_guard(Node))
			 end,
	    HeadStartLoc={_,HeadStartCol} = get_start_loc(Node),
            HdB=hd(Body),
            {_BodyStartLn, BodyStartCol} = get_start_loc(HdB),
            {BodyStartLn, _BodyStartCol} = get_start_loc_with_comment(HdB),
            BodyStartLoc={BodyStartLn, BodyStartCol},
	    SameLine = {BodyStartLoc, {HeadStartCol, HeadLastLn}},
            case Ctxt#ctxt.clause of
		fun_expr -> 
		    make_fun_clause(D1, D2, D3, Node, Ctxt, SameLine, HeadStartLoc);
		{function, N} -> 
		    make_fun_clause(N, D1, D2, D3, Node,Ctxt, SameLine, HeadStartLoc);
		if_expr -> 
		    make_if_clause(D1, D2, D3, Ctxt, SameLine);
		cond_expr -> 
		    make_if_clause(D1, D2, D3, Ctxt, SameLine);
		case_expr ->
		    make_case_clause(D1, D2, D3, Node,Ctxt, SameLine);
		receive_expr -> 
		    make_case_clause(D1, D2, D3, Node,Ctxt, SameLine);
		try_expr -> 
		    make_case_clause(D1, D2, D3, Node, Ctxt, SameLine);
		{rule, N} ->
		    make_rule_clause(N, D1, D2, D3, Node, Ctxt, SameLine);
		undefined ->
		    %% If a clause is formatted out of context, we
		    %% use a "fun-expression" clause style.
		    make_fun_clause(D1, D2, D3, Node, Ctxt, SameLine, HeadStartLoc)
	    end;
	function ->  
	    %% Comments on the name itself will be repeated for each
	    %% clause, but that seems to be the best way to handle it.
	    Ctxt1 = reset_check_bracket(reset_prec(Ctxt)),
	    D1 = lay(wrangler_syntax:function_name(Node), Ctxt1),
	    D2 = lay_clauses(wrangler_syntax:function_clauses(Node), {function, D1}, Ctxt1),
	    beside(D2, floating(text(".")));
	case_expr -> 
            Ctxt1 = reset_prec(Ctxt),
	    Arg = wrangler_syntax:case_expr_argument(Node),
            Cs = wrangler_syntax:case_expr_clauses(Node),
	    D1 = lay(Arg, reset_check_bracket(Ctxt1)),
            D2 = lay_clauses(Cs, case_expr, reset_check_bracket(Ctxt1)),
            {CsStartLn, CsStartCol} = get_start_loc_with_comment(hd(Cs)),
            {CaseStartLine, CaseStartCol} = wrangler_syntax:get_pos(Node),
            {{ArgStartLine, ArgStartCol}, ArgEndLoc={ArgEndLine, _ArgEndCol}} = get_start_end_loc(Arg),
            CaseArgD=case CaseStartLine==ArgStartLine orelse ArgStartLine==0 
	         	 orelse CaseStartLine==0 of 
	         	 true when CaseStartLine==0 ->
                             wrangler_prettypr_0:horizontal([text("case"), D1]);
                          true ->
                              P0 =ArgStartCol - CaseStartCol - 4,
                              P = if P0 =< 0 ->
                                          1;
                                     true -> P0
                                  end,
                              horizontal([text("case"), text(spaces(P)), D1]);
	         	 false ->
	         	     above(text("case"),nest(ArgStartCol-CaseStartCol, D1))
                     end,
	    {OfStartLn, OfStartCol} = get_keyword_loc_after('of', Ctxt, ArgEndLoc),
            CaseArgOfD = case OfStartLn -ArgEndLine==1 of 
	         	     true ->
	         		 above(CaseArgD, nest(OfStartCol-CaseStartCol, text("of")));
	         	     false ->
                                 wrangler_prettypr_0:horizontal([CaseArgD, text("of")])
	                 end,
            CaseArgOfCsD=case OfStartLn==CsStartLn andalso OfStartLn/=0 of 
                             true ->
                                 P1 = CsStartCol - OfStartCol -2,
                                 horizontal([CaseArgOfD, text(spaces(P1)), D2]);
                             false when OfStartLn/=0->
                                 above(CaseArgOfD, nest(CsStartCol-CaseStartCol, D2));
                             _->
                                 above(CaseArgOfD, nest(Ctxt1#ctxt.sub_indent, D2))
                         end,
            CsEndLoc={CsEndLn, _CsEndCol} = get_end_loc_with_comment(lists:last(Cs)),
            {EndStartLn, _EndStartCol} = get_keyword_loc_after("end", Ctxt, CsEndLoc),
            case CsEndLn == EndStartLn andalso EndStartLn/=0 of
                true ->
                    wrangler_prettypr_0:horizontal([CaseArgOfCsD, text("end")]);
                false ->
                    sep([CaseArgOfCsD, text("end")])
            end;
       	if_expr ->  
	    %% Done
	    Ctxt1 = reset_prec(Ctxt),
     Cs=wrangler_syntax:if_expr_clauses(Node),
     D = lay_clauses(Cs, if_expr, reset_check_bracket(Ctxt1)),
     append_keywords("if", "end", D, Cs, Ctxt1);
	cond_expr ->  
	    %% Done;
	    Ctxt1 = reset_prec(Ctxt),
	    Cs=wrangler_syntax:cond_expr_clauses(Node),
	    D = lay_clauses(Cs, cond_expr, reset_check_bracket(Ctxt1)),
	    append_keywords("cond", "end", D, Cs, Ctxt1);
	fun_expr ->  
	    %% Done;
	    Ctxt1 = reset_prec(Ctxt),
	    Cs=wrangler_syntax:fun_expr_clauses(Node),
	    D = lay_clauses(Cs, fun_expr, reset_check_bracket(Ctxt1)),
	    append_keywords("fun", "end", D, Cs, Ctxt1);
	module_qualifier -> 
	    %% Done;
	    {PrecL, _Prec, PrecR} = inop_prec(':'),
            Arg =wrangler_syntax:module_qualifier_argument(Node),
            Body =wrangler_syntax:module_qualifier_body(Node),
	    D1 = lay(Arg, reset_check_bracket(set_prec(Ctxt, PrecL))),
	    D2 = lay(Body, reset_check_bracket(set_prec(Ctxt, PrecR))),
	    beside(D1, beside(text(":"), D2));
	qualified_name ->  
	    Ss = wrangler_syntax:qualified_name_segments(Node),
	    lay_qualified_name(Ss, Ctxt);
	arity_qualifier ->  
	    %% Done;
	    Ctxt1 = reset_prec(Ctxt),
            B = wrangler_syntax:arity_qualifier_body(Node),
            Arg = wrangler_syntax:arity_qualifier_argument(Node),
	    D1 = lay(B, reset_check_bracket(Ctxt1)),
	    D2 = lay(Arg, reset_check_bracket(Ctxt1)),
	    beside(D1, beside(text("/"), D2));
	attribute -> 
            Ctxt1 = reset_check_bracket(reset_prec(Ctxt)),
	    N = wrangler_syntax:attribute_name(Node),
	    case wrangler_syntax:attribute_arguments(Node) of
		none -> 
		    D =lay(N, Ctxt1),
		    beside(floating(text("-")), beside(D, floating(text("."))));
		Args ->
                    Sep = get_separator(Args, Ctxt1, ","),
                    D2= case wrangler_syntax:atom_value(N) of 
                            spec ->
                                case spec_need_paren(Ctxt1#ctxt.tokens) of 
                                    true ->
                                        As =lay_spec_args(Args, Ctxt1),
                                        D = lay_elems(fun wrangler_prettypr_0:par/1, As, Args, Ctxt),
                                        beside(lay(N, Ctxt1), beside(text("("), beside(D, floating(text(")")))));
                                    false ->
                                        As =lay_spec_args(Args, Ctxt1),
                                        D = lay_elems(fun wrangler_prettypr_0:par/1, As, Args, Ctxt),
                                        wrangler_prettypr_0:horizontal([lay(N, Ctxt1), D])
                                end;
                            _ -> 
                                As=seq(Args, floating(text(Sep)), Ctxt1, fun lay/2),
                                D = lay_elems(fun wrangler_prettypr_0:par/1, As, Args, Ctxt),
                                beside(lay(N, Ctxt1), beside(text("("), beside(D, floating(text(")")))))
                        end,
                    beside(floating(text("-")), beside(D2, floating(text("."))))
	    end;
	binary ->   
	    Ctxt1 = reset_check_bracket(reset_prec(Ctxt)),
	    Fields = wrangler_syntax:binary_fields(Node),
	    Sep = get_separator(Fields, Ctxt, ","),
	    Es = seq(Fields, floating(text(Sep)), Ctxt1, fun lay/2),
	    D = lay_elems(fun wrangler_prettypr_0:par/1, Es, Fields, Ctxt1),
	    beside(floating(text("<<")), beside(D, floating(text(">>"))));
	binary_field ->
	    Ctxt1 = reset_check_bracket(reset_prec(Ctxt)),
	    D1 = lay(wrangler_syntax:binary_field_body(Node), Ctxt1),
	    D2 = case wrangler_syntax:binary_field_types(Node) of
		     [] -> 
			 empty();
		     Ts -> 
			 beside(floating(text("/")), 
                                lay_bit_types(Ts, Ctxt1))
		 end,
	    beside(D1, D2);
	block_expr -> 
	    Ctxt1 = reset_check_bracket(reset_prec(Ctxt)),
	    Body = wrangler_syntax:block_expr_body(Node),
	    Es = seq(Body, floating(text(", ")), Ctxt1, fun lay/2),
	    D=lay_body_elems(Es, Body, Ctxt1),
	    append_keywords("begin", "end", D, Body, Ctxt);
	catch_expr ->  
                {Prec, PrecR} = preop_prec('catch'),
		Body =wrangler_syntax:catch_expr_body(Node),
                Ctxt1 =reset_check_bracket(set_prec(Ctxt, PrecR)),
		D = maybe_parentheses_1(lay(Body, Ctxt), Body, Ctxt1),
		D1 = append_leading_keyword("catch", D, Body, Ctxt),
		maybe_parentheses_2(D1, Node, Prec, Ctxt);
        class_qualifier -> 
                Ctxt1 = reset_check_bracket(set_prec(Ctxt, max_prec())),
		D1 = lay(wrangler_syntax:class_qualifier_argument(Node), Ctxt1),
		D2 = lay(wrangler_syntax:class_qualifier_body(Node), Ctxt1),
		beside(D1, beside(text(":"), D2));
	comment ->
		D = stack_comment_lines(wrangler_syntax:comment_text(Node)),
                case wrangler_syntax:comment_padding(Node) of
		    none -> floating(break(D));
		    P -> floating(break(beside(text(spaces(P)), D)))
		end;
	conjunction -> 
                Body = wrangler_syntax:conjunction_body(Node),
		Sep = get_separator(Body, Ctxt, ","),
		Es = seq(Body, floating(text(Sep)), reset_check_bracket(reset_prec(Ctxt)),
                         fun lay/2),
		lay_elems(fun wrangler_prettypr_0:par/1, Es, Body, Ctxt);
	disjunction -> 
                Body = wrangler_syntax:disjunction_body(Node),
		Es = seq(Body, floating(text(";")), reset_check_bracket(reset_prec(Ctxt)),
                         fun lay/2),
		lay_elems(fun wrangler_prettypr_0:sep/1, Es, Body, Ctxt);
	error_marker -> 
                Ctxt1=reset_check_bracket(reset_prec(Ctxt)),
                E = wrangler_syntax:error_marker_info(Node),
		beside(text("** "), beside(lay_error_info(E, Ctxt1), text(" **")));
	eof_marker ->
		empty();
	form_list ->
                Forms = wrangler_syntax:form_list_elements(Node),
		Es = seq(Forms, none, reset_check_bracket(reset_prec(Ctxt)), 
                         fun lay/2),
		vertical_sep(text(""), Es);
	generator -> 
		Pat = wrangler_syntax:generator_pattern(Node),
		Body = wrangler_syntax:generator_body(Node),
		lay_generator(Ctxt, Pat, Body, "<-");
	binary_generator ->
                Pat = wrangler_syntax:binary_generator_pattern(Node),
		Body = wrangler_syntax:binary_generator_body(Node),
		lay_generator(Ctxt, Pat, Body, "<=");
	implicit_fun ->
                Ctxt1=reset_check_bracket(reset_prec(Ctxt)),
                D = lay(wrangler_syntax:implicit_fun_name(Node), Ctxt1),
		beside(floating(text("fun ")), D);
	list_comp ->  
                lay_comp(Node, Ctxt, list_comp_template, list_comp_body,
                         "[", "]");
        binary_comp ->
                lay_comp(Node, Ctxt, binary_comp_template,
                         binary_comp_body, "<< ", " >>");
	macro ->
                Ctxt1 = reset_prec(Ctxt),
                N = wrangler_syntax:macro_name(Node),
                Args = wrangler_syntax:macro_arguments(Node),
                Sep = get_separator(Args, Ctxt1, ","),
                D = case Args of
		      none ->
		          lay(N, reset_check_bracket(Ctxt1));
	              [] -> beside(lay(N, reset_check_bracket(Ctxt1)), text("()"));
                      _ ->
                          Ctxt2 = case length(Args) of
                                      1 ->
                                           Ctxt1#ctxt{check_bracket=true};
                                      _ -> reset_check_bracket(Ctxt1)
                                  end,
                          As=seq_1(Args, floating(text(Sep)), reset_prec(Ctxt2), fun lay/2),
                          ArgsD=lay_elems(fun wrangler_prettypr_0:par/1, As, Args, Ctxt1),
			  OpEndLoc = get_end_loc_with_comment(N),
			  ArgsD1=make_args(Args, ArgsD,Ctxt1,OpEndLoc,'(',')'),
			  beside(lay(N, Ctxt1),ArgsD1)
                  end,
                D1 = beside(floating(text("?")), D),
                maybe_parentheses_1(D1, Node, Ctxt);
        parentheses ->
                Body =wrangler_syntax:parentheses_body(Node),
                D = lay(Body, reset_check_bracket(reset_prec(Ctxt))),
                lay_parentheses(D);
        fake_parentheses ->
                Body =wrangler_syntax:fake_parentheses_body(Node),
                lay(Body, reset_prec(Ctxt));
	query_expr ->
                Ctxt1 = reset_prec(Ctxt),
                Body = wrangler_syntax:query_expr_body(Node),
                D = lay(Body, reset_check_bracket(Ctxt1)),
                append_keywords("query", "end", D, Body, Ctxt1);
        receive_expr ->
                Ctxt1 = reset_check_bracket(reset_prec(Ctxt)),
                Cs=wrangler_syntax:receive_expr_clauses(Node),
                D1 = lay_clauses(Cs, receive_expr, Ctxt1),
                case wrangler_syntax:receive_expr_timeout(Node) of
                    none ->
                        append_keywords("receive", "end", D1, Cs, Ctxt);
                    T ->
                        D3 = lay(T, Ctxt1),
                        A = wrangler_syntax:receive_expr_action(Node),
                        As=seq(A, floating(text(", ")), Ctxt1, fun lay/2),
                        D4 = lay_elems(fun wrangler_prettypr_0:sep/1, As, A, Ctxt),
                        AStartLoc=get_start_loc_with_comment(hd(A)),
                        {{HeadStartLn, HeadStartCol}, {HeadLastLn, _}} = wrangler_misc:get_start_end_loc_with_comment(T),
                        D5 =append_clause_body(D4, D3, Ctxt1, {AStartLoc, {HeadStartCol, HeadLastLn}}),
                        D2= append_leading_keyword("receive", D1, Cs, Ctxt),
                        D6 = append_keywords("after", "end", D5, [T|A], Ctxt),
                        {EndLn, _} = case Cs of 
                                         [] -> wrangler_syntax:get_pos(Node);
                                         _ ->get_end_loc(Cs)
                                     end,
                        case HeadStartLn == EndLn andalso EndLn/=0 of
                            true ->
                                wrangler_prettypr_0:horizontal([D2,D6]);
                            false ->
                                above(D2, D6)
                        end
                end;
	record_access ->
                {PrecL, Prec, PrecR} = inop_prec('#'),
                Arg =wrangler_syntax:record_access_argument(Node),
                Field =wrangler_syntax:record_access_field(Node),
                D1 = lay(Arg, reset_check_bracket(set_prec(Ctxt, PrecL))),
                D2 = beside(floating(text(".")), 
                            lay(Field, reset_check_bracket(set_prec(Ctxt, PrecR)))),
                D3 = case wrangler_syntax:record_access_type(Node) of
                         none -> D2;
                         T ->
                             beside(beside(floating(text("#")), 
                                           lay(T, reset_check_bracket(reset_prec(Ctxt)))), D2)
                     end,
                maybe_parentheses(beside(D1, D3), Prec, Ctxt);
             record_expr ->
                {PrecL, Prec, _} = inop_prec('#'),
                Ctxt1 = reset_check_bracket(reset_prec(Ctxt)),
           Arg = wrangler_syntax:record_expr_argument(Node),
           Type = wrangler_syntax:record_expr_type(Node),
           Fields = wrangler_syntax:record_expr_fields(Node),
           D1 = lay(Type, Ctxt1),
           Sep = get_separator(Fields, Ctxt, ","),
           Fs =seq(Fields, floating(text(Sep)), Ctxt1, fun lay/2),
           D2 = beside(floating(text("#")), D1),
           D3 = case Arg of
                    none -> D2;
                    A ->
                        Ctxt2 = reset_check_bracket(set_prec(Ctxt, PrecL)),
                        AD = maybe_parentheses_1(lay(A, Ctxt2), A, Ctxt2),
                        beside(AD, D2)
                end,
           D4 = case Fields of
                    [] ->
                        beside(D3, beside(text("{"), text("}")));
                    _ ->
                        FieldsD = lay_elems(fun wrangler_prettypr_0:par/1, Fs, Fields, Ctxt1),
                        ExprStartLoc = get_start_loc(Node),
                        TypeEndLoc = get_end_loc(Type),
                        FieldsD1 = make_args(Fields,FieldsD,Ctxt,TypeEndLoc,'{','}'),
                        LeftBracketLoc = get_keyword_loc_after('{', Ctxt1, TypeEndLoc),
                        append_elems(fun horizontal_1/1, {D3, {ExprStartLoc, TypeEndLoc}},
                                     {FieldsD1, {LeftBracketLoc, LeftBracketLoc}})
                end,
           maybe_parentheses(D4, Prec, Ctxt);
        record_field ->
         Ctxt1 = reset_check_bracket(reset_prec(Ctxt)),
         D1 = lay(wrangler_syntax:record_field_name(Node), Ctxt1),
         case wrangler_syntax:record_field_value(Node) of
             none -> D1;
             V -> D2 = lay(V, Ctxt1),
                  {LStart, LEnd} = get_start_end_loc(wrangler_syntax:record_field_name(Node)),
                  {RStart, REnd} = get_start_end_loc(V),
                  EqLoc = get_keyword_loc_after('=', Ctxt, LEnd),
                  LeftEq=append_elems(fun wrangler_prettypr_0:horizontal/1,
                                      {D1, {LStart, LEnd}}, {text("="), {EqLoc, EqLoc}}),
                  %%append_elems(fun ([Doc1,Doc2]) ->
                  %%                     follow(Doc1, Doc2, Ctxt#ctxt.break_indent)
                  %%             end, {LeftEq, {LStart, EqLoc}},
                  %%             {D2,{RStart, REnd}})
                  append_elems(fun wrangler_prettypr_0:horizontal/1,
                               {LeftEq, {LStart, EqLoc}},
                               {D2,{RStart, REnd}})
         end;
	record_index_expr ->
                {Prec, PrecR} = preop_prec('#'),
                Type =wrangler_syntax:record_index_expr_type(Node),
                Field = wrangler_syntax:record_index_expr_field(Node),
                D1 = lay(Type, reset_check_bracket(reset_prec(Ctxt))),
                D2 = lay(Field, reset_check_bracket(set_prec(Ctxt, PrecR))),
                D3 = beside(beside(floating(text("#")), D1),
                            beside(floating(text(".")), D2)),
                maybe_parentheses(D3, Prec, Ctxt);
	rule ->
                Ctxt1 = reset_check_bracket(reset_prec(Ctxt)),
                D1 = lay(wrangler_syntax:rule_name(Node), Ctxt1),
                D2 = lay_clauses(wrangler_syntax:rule_clauses(Node), {rule, D1}, Ctxt1),
                beside(D2, floating(text(".")));
	size_qualifier ->
                Ctxt1 = reset_check_bracket(set_prec(Ctxt, max_prec())),
                Body = wrangler_syntax:size_qualifier_body(Node),
                Arg = wrangler_syntax:size_qualifier_argument(Node),
                D1 = lay(Body, Ctxt1),
                D2 = lay(Arg, Ctxt1),
                beside(D1, beside(text(":"), D2));
      	try_expr ->
	   Ctxt1 = reset_check_bracket(reset_prec(Ctxt)),
     Body = wrangler_syntax:try_expr_body(Node),
     {BodyStart, BodyEnd} = get_start_end_loc(Body),
     TryLoc = get_keyword_loc_before('try', Ctxt1, BodyStart),
     Bs =seq(Body,floating(text(",")),Ctxt1,fun lay/2),
     D1 = lay_body_elems(Bs, Body, Ctxt1),
     {_NodeStart, NodeEnd} = get_start_end_loc(Node),
     EndLoc = get_keyword_loc_before('end', Ctxt1, NodeEnd),
     Es0 = [{text("end"), {EndLoc, NodeEnd}}],
     Es1 = {append_leading_keyword("try", D1, Body, Ctxt1), {TryLoc, BodyEnd}},
     Es2 = case wrangler_syntax:try_expr_after(Node) of
	       [] -> Es0;
	       As ->
		   AsDocs= seq(As,floating(text(",")),Ctxt1,fun lay/2),
		   D2 = lay_elems(fun wrangler_prettypr_0:sep/1, AsDocs, As, Ctxt),
		   {AsStart, AsEnd} = get_start_end_loc(As),
		   AfterLoc=get_keyword_loc_before('after', Ctxt1, AsStart),
		   [{append_leading_keyword("after", D2, As, Ctxt1), {AfterLoc, AsEnd}}
		    |Es0]
	   end,
     Es3 = case wrangler_syntax:try_expr_handlers(Node) of
	       [] -> Es2;
	       Hs ->
		   D3 = lay_clauses(Hs,try_expr,Ctxt1),
		   {HsStart, HsEnd} = get_start_end_loc(Hs),
		   CatchLoc = get_keyword_loc_before('catch', Ctxt1, HsStart),
		   [{append_leading_keyword("catch", D3, Hs, Ctxt1), {CatchLoc, HsEnd}}|Es2]
	   end,
     case wrangler_syntax:try_expr_clauses(Node) of
	 [] ->
	     lay_body_elems_2([Es1|Es3],Ctxt1,[],{{1,1},{1,1}});
	 Cs ->
             D4 = lay_clauses(Cs, try_expr, Ctxt1),
	     {CsStart, CsEnd} = get_start_end_loc(Cs),
             OfLoc=get_keyword_loc_before('of', Ctxt1, CsStart),
             Es4 = {append_leading_keyword("of", D4, Cs, Ctxt1),{OfLoc, CsEnd}},
             lay_body_elems_2([Es1,Es4|Es3], Ctxt1, [], {{1,1},{1,1}})
     end;
	char ->
	   V = wrangler_syntax:char_value(Node),
	   case is_integer(V) and (V > 127) of
	       true -> {ok, [Num], _} = io_lib:fread("~u", integer_to_list(V)),
		       [CharStr] = io_lib:fwrite("~.8B", [Num]),
		       text("$\\" ++ CharStr);
	       _ when is_atom(V) ->
		      text(atom_to_list(V));
	       _ -> text(wrangler_syntax:char_literal(Node))
	   end; %% "
       warning_marker ->
            E = wrangler_syntax:warning_marker_info(Node),
            beside(text("%% WARNING: "), lay_error_info(E, reset_prec(Ctxt)));
	type -> empty();  %% tempory fix!!
      	typed_record_field -> empty(); %% tempory fix!!!
        function_clause ->
              lay_2(wrangler_syntax:function_clause(Node), Ctxt);
        empty_node->
            empty()
    end.

lay_list(Node, Ctxt) ->
    Ctxt1 = reset_check_bracket(reset_prec(Ctxt)),
    Node1 = wrangler_syntax:compact_list(Node),
    PrefixElems = wrangler_syntax:list_prefix(Node1),
    Sep = get_separator(PrefixElems, Ctxt, ","),
    D0 = seq(PrefixElems, floating(text(Sep)), Ctxt1, fun lay/2),
    D1 = lay_elems(fun wrangler_prettypr_0:par/1, D0, PrefixElems, Ctxt),
    {{StartLn, StartCol}, {EndLn, EndCol}} = get_start_end_loc(Node),
    {PrefixStart={PrefixStartLn, PrefixStartCol},
     PrefixEnd= {PrefixEndLn, _PrefixEndCol}} =
        case PrefixElems of
            [] ->
                {{0,0},0,0};
            _ ->
                {get_start_loc_with_comment(hd(PrefixElems)),
                 get_end_loc_with_comment(lists:last(PrefixElems))}
        end,
    N = case StartLn of
            0 -> 0;
            _ ->PrefixStartLn - StartLn
        end,
    Offset = case StartCol of 
                 0 -> 0;
                 _ ->
                     PrefixStartCol - StartCol
             end,
    case wrangler_syntax:list_suffix(Node1) of
        none ->
            D2 =case N =< 0 of
                    true ->
                        Pad = PrefixStartCol-StartCol-1,
                        case Pad>0 of
                            true ->
                                %% beside(text("["), beside(text(spaces(Pad)), D1));
                                beside(text("["), D1);
                            false ->
                                beside(text("["), D1)
                        end;
                    _ ->
                        above(text("["), nest(Offset, D1))
                end,
            N1 = EndLn - PrefixEndLn,
            case N1 =< 0 orelse EndLn==0 orelse PrefixEndLn==0 of
                true ->
                    beside(D2, text("]"));
                false ->
                    above(D2, nest(EndCol - StartCol, text("]")))
            end;
	S ->
            D2 = lay(S, Ctxt1),
            {SuffixStart, SuffixEnd={SuffixEndLn,_}} = wrangler_misc:get_start_end_loc_with_comment(S),
            {BarLn, BarCol} = get_keyword_loc_before('|', Ctxt1, SuffixStart),
            BarD2=append_elems(fun wrangler_prettypr_0:horizontal/1,
                               {text("|"), {{BarLn, BarCol}, {BarLn, BarCol}}},
                               {D2, {SuffixStart, SuffixEnd}}),
            D1BarD2=append_elems(fun wrangler_prettypr_0:par/1, {D1, {PrefixStart, PrefixEnd}},
                                 {BarD2,{{BarLn, BarCol}, SuffixEnd}}),
            D3 = case N =< 0 of
                     true ->
                         Pad = PrefixStartCol-StartCol-1,
                         case Pad>0 of
                             true ->  
                                 %% this cause problem when the first element is removed.
                                 %% beside(text("["), beside(text(spaces(Pad)), D1BarD2));
                                 beside(text("["), D1BarD2);
                             false ->
                                 beside(text("["), D1BarD2)
                         end;
                     _ ->
                         above(text("["), nest(Offset, D1BarD2))
                 end,
            N1 = EndLn - SuffixEndLn,
            case N1 =< 0 orelse EndLn==0 orelse SuffixEndLn==0 of
                true ->
                    beside(D3, text("]"));
                false ->
                    above(D3, nest(EndCol - StartCol, text("]")))
            end
    end.

lay_generator(Ctxt, Pat, Body, Arrow) ->
    Ctxt1 = reset_check_bracket(reset_prec(Ctxt)),
    D1 = lay(Pat,Ctxt1),
    D2 = lay(Body,Ctxt1),
    D1EndLn = get_end_line_with_comment(Pat),
    BodyStartLoc={D2StartLn, _} = get_start_loc_with_comment(Body),
    {_, D1StartCol} = get_start_loc(Pat),
    {_, D2StartCol} = get_start_loc(Body),
    case (D1EndLn==0) or (D2StartLn==0) of
	true ->
	    par([D1,beside(text(Arrow),D2)],Ctxt#ctxt.break_indent);
	_ ->
	    case D2StartLn-D1EndLn of
		0 -> beside(D1,beside(text(" "++Arrow++" "),D2));
		1 -> 
                    {ArrowLn, ArrowCol} = 
                        get_keyword_loc_before(list_to_atom(Arrow), Ctxt, BodyStartLoc ),
                    if ArrowLn ==D1EndLn ->
                            Offset = D2StartCol -D1StartCol,
                            above(beside(D1, text(" "++Arrow)),nest(Offset,D2));
                       true ->
                            above(D1,nest(ArrowCol-D1StartCol,beside(text(Arrow++" "),D2)))
                    end;
		_ -> par([D1,beside(text(Arrow++" "),D2)],Ctxt#ctxt.break_indent)
	    end
    end.


lay_comp(Node, Ctxt, Fun1, Fun2, Tok1, Tok2) ->
    Ctxt1 = reset_check_bracket(reset_prec(Ctxt)),
    Temp = wrangler_syntax:Fun1(Node),
    D1 = lay(Temp,Ctxt1),
    Body = wrangler_syntax:Fun2(Node),
    Sep = get_separator(Body,Ctxt,","),
    Es = seq(Body,floating(text(Sep)),Ctxt1,fun lay/2),
    D2 = lay_elems(fun wrangler_prettypr_0:par/1,Es,Body,Ctxt),
    {TempStart,TempEnd} = get_start_end_loc(Temp),
    {BodyStart,BodyEnd} = get_start_end_loc(Body),
    {BarLn,BarCol} =
        get_keyword_loc_before('||',Ctxt,BodyStart),
    BarD2 = append_elems(fun wrangler_prettypr_0:horizontal/1,
                         {text("||"),{{BarLn,BarCol},{BarLn,BarCol + 1}}},
                         {D2,{BodyStart,BodyEnd}}),
    D1BarD2 =
        append_elems(fun wrangler_prettypr_0:par/1, {D1,{TempStart,TempEnd}},
                     {BarD2,{{BarLn,BarCol},BodyEnd}}),
    beside(floating(text(Tok1)),beside(D1BarD2,floating(text(Tok2)))).

lay_parentheses(D) ->
    beside(floating(text("(")),beside(D,floating(text(")")))).

maybe_parentheses(D,Prec,Ctxt) ->
    case Ctxt#ctxt.prec of
        P when P > Prec -> lay_parentheses(D);
        _ -> 
            D
    end.  

maybe_parentheses_2(D, Node, Prec, Ctxt) ->
    case Ctxt#ctxt.prec of
        P when P > Prec -> lay_parentheses(D);
        _ -> 
            maybe_parentheses_1(D, Node, Ctxt)
    end. 
    
maybe_parentheses_1(D, Node, Ctxt) ->
    Str=wrangler_prettypr_0:format(D),
    case already_has_parentheses(Str) of 
        true ->
            D;
        false ->
            case need_parentheses(Node, Ctxt) of 
                true ->		
                    lay_parentheses(D); 
                false ->
                    D
            end
    end.
    

already_has_parentheses("")->
    false;
already_has_parentheses(Str) ->
     case hd(Str)==$\( andalso lists:last(Str)==$\) of 
         true ->
             SubStr =lists:sublist(Str, 2, length(Str)-2),
             true==well_formed_parentheses(SubStr);
         false ->
             false
     end.
well_formed_parentheses(Str) ->
    Pars =[C||C<-Str, C==$\( orelse C==$\)],
    well_formed_parentheses(Pars, {0,0}).
well_formed_parentheses([], {L, R})-> 
    case {L, R} of 
        {0,0} -> true;
        _ -> {false, {L, R}}
    end;
well_formed_parentheses([C|Cs], {L, R}) -> 
    case C of 
        $\( -> 
           well_formed_parentheses(Cs, {L+1, R});
        $\) ->
            case L>0 of
                true->
                    well_formed_parentheses(Cs, {L-1, R});
                false ->
                    well_formed_parentheses(Cs, {L, R+1})
            end
    end.
                    
                
lay_qualified_name([S| Ss1] = Ss,Ctxt) ->
    Ctxt1 = reset_check_bracket(Ctxt),
    case wrangler_syntax:type(S) of
      atom ->
	  case wrangler_syntax:atom_value(S) of
	    '' -> beside(text("."),lay_qualified_name_1(Ss1,Ctxt1));
	    _ -> lay_qualified_name_1(Ss,Ctxt1)
	  end;
      _ -> lay_qualified_name_1(Ss,Ctxt1)
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

lay_clauses(Cs,Type,Ctxt) ->
    CsDocs = seq(Cs,floating(text(";")),
		 Ctxt#ctxt{clause = Type},fun lay/2),
    lay_body_elems(CsDocs, Cs, Ctxt).
    

%% Note that for the clause-making functions, the guard argument
%% can be `none', which has different interpretations in different
%% contexts.


make_fun_clause(P,G,B, CsNode, Ctxt,SameLine, HeadStartLoc) ->
    make_fun_clause(none,P,G,B,CsNode, Ctxt,SameLine, HeadStartLoc).

make_fun_clause(N,P,G,B, CsNode, Ctxt, SameLine, HeadStartLoc={_Ln, _Col}) ->
    Pats =wrangler_syntax:clause_patterns(CsNode),
    D = make_fun_clause_head(N,P,Pats,Ctxt,HeadStartLoc),
    make_case_clause(D,G,B,CsNode,Ctxt,SameLine).
    

make_fun_clause_head(N,P, Pats, Ctxt,FunNameLoc = {StartLine, StartCol}) ->
    D =make_args(Pats, P,Ctxt,FunNameLoc,'(',')'),
    {LeftBracketLine,LeftBracketCol} = get_keyword_loc_after('(',Ctxt,FunNameLoc),
    if N == none -> D;
       true ->
            case LeftBracketLine==StartLine of
                true ->
                    beside(N,D);
                false ->
                    above(N,nest(LeftBracketCol-StartCol,D))
            end
    end.

make_args([], _, _, _, LeftBracket,RightBracket) ->
    beside(text(atom_to_list(LeftBracket)),text(atom_to_list(RightBracket)));
make_args(Pats, P,Ctxt,FunNameLoc,LeftBracket,RightBracket) ->
    {LeftBracketLine,LeftBracketCol} = 
	get_keyword_loc_after(LeftBracket,Ctxt,FunNameLoc),
    {{PatStartLine, PatStartCol}, {PatEndLine, _PatEndCol}} =
        wrangler_misc:get_start_end_loc_with_comment(Pats),
    {RightBracketLine,RightBracketCol} =
	get_right_bracket_loc(Ctxt#ctxt.tokens,{LeftBracketLine,LeftBracketCol},
                              LeftBracket, RightBracket),
    D0 = case {RightBracketLine,RightBracketCol}=={0,0} orelse 
	     RightBracketLine==PatEndLine orelse PatEndLine==0
	 of
	     true ->
		 beside(P,text(atom_to_list(RightBracket)));
	     _ ->
		 above(P,nest(RightBracketCol - PatStartCol,text(atom_to_list(RightBracket))))
	 end,
    case {LeftBracketLine,LeftBracketCol}=={0,0} orelse 
	LeftBracketLine==PatStartLine orelse PatStartLine==0 of
   	true ->
	    beside(text(atom_to_list(LeftBracket)),D0);
	_ ->
	    above(text(atom_to_list(LeftBracket)),nest(PatStartCol-LeftBracketCol,D0))
    end.

make_rule_clause(N,P,G,B, CsNode, Ctxt, SameLine) ->
    Pats =wrangler_syntax:clause_patterns(CsNode),
    D = make_fun_clause_head(N,P,Pats, Ctxt,{0,0}),
    append_rule_body(B,append_guard(G,D,Ctxt),Ctxt, SameLine).

make_case_clause(P,G,B,CsNode,Ctxt,SameLine) ->
    append_clause_body(B,append_guard(G,P,CsNode,Ctxt),Ctxt, SameLine).

make_if_clause(_P,G,B,Ctxt, SameLine) ->
    %% We ignore the patterns; they should be empty anyway.
    G1 = if G == none -> text("true");
	    true -> G
	 end,
    append_clause_body(B,G1,Ctxt, SameLine).


append_rule_body(B,D,Ctxt, SameLine) ->
    append_clause_body(B,D, ':-',Ctxt, SameLine).

append_clause_body(B,D,Ctxt, SameLine) ->
    append_clause_body(B,D,'->',Ctxt, SameLine).

append_clause_body(B,D, Symbol,Ctxt, _SameLine={{BodyStartLn,BodyStartCol}, 
					       {HeadStartCol, HeadLastLn}}) ->
    S=text(" "++atom_to_list(Symbol)),
    S1=text(" "++atom_to_list(Symbol)++" "),
    case  BodyStartLn == 0 orelse HeadLastLn == 0 of
	true ->
	    sep([beside(D,S),nest(Ctxt#ctxt.break_indent,B)]);
	false ->
            case BodyStartLn-HeadLastLn  of 
		0 ->
		    beside(beside(D,S1),B);
		N when N>=1->
		    Offset=BodyStartCol-HeadStartCol,
                    {SLn, SCol} = get_keyword_loc_before(Symbol,Ctxt,
							 {BodyStartLn, BodyStartCol}),
                    D1=case SLn-HeadLastLn=<0 orelse SLn==0 of
			   true ->
			       beside(D, S);
			   false when SLn-HeadLastLn>1->
                               SD=nest(SCol-HeadStartCol, text(atom_to_list(Symbol))),
			       vertical([D, white_lines(SLn-HeadLastLn-1), SD]);
                           _ ->
                               SD=nest(SCol-HeadStartCol, text(atom_to_list(Symbol))),
                               vertical([D,SD])
                       end,
                    case SLn==BodyStartLn of 
			true ->
			    wrangler_prettypr_0:horizontal([D1, B]);
                        _ when BodyStartLn-SLn>1 andalso SLn/=0 ->
                            vertical([D1, white_lines(BodyStartLn-SLn-1), nest(Offset,B)]);
                        _ ->
                            vertical([D1, nest(Offset, B)])
		    end;
		_ ->
		    sep([beside(D, S), nest(Ctxt#ctxt.break_indent, B)])
	    end
    end.

append_guard(none,D,_) -> D;
append_guard(G,D,Ctxt) ->
    par([D,follow(text("when"),G,Ctxt#ctxt.sub_indent)],
	Ctxt#ctxt.break_indent).


append_guard(none,D,_CsNode,_) -> 
      D;
append_guard(G,D,CsNode,Ctxt) ->
    Guard= wrangler_syntax:clause_guard(CsNode),
    case revert_clause_guard(Guard) of 
        [[E]] -> case wrangler_syntax:type(E) == nil of
                     true -> D;
                     _ -> append_guard_1(G, D, CsNode, Ctxt)
                 end;
        _ -> append_guard_1(G, D, CsNode, Ctxt)
    end.
append_guard_1(G, D, CsNode, Ctxt) ->
    Guard= wrangler_syntax:clause_guard(CsNode),
    {{StartLn, StartCol},{_EndLn, _EndCol}}=get_start_end_loc(Guard),
    {WhenLn, WhenCol}=get_prev_keyword_loc(Ctxt#ctxt.tokens, {StartLn, StartCol}, 'when'),
    Pats = wrangler_syntax:clause_patterns(CsNode),
    {{_PStartLn, _PStartCol}, {PEndLn, _PEndCol}}=get_start_end_loc(Pats),
    {_,CsStartCol} = get_start_loc_with_comment(CsNode),
    D1= case WhenLn-PEndLn==1 of 
	    true ->
		above(D, nest(WhenCol-CsStartCol, text("when")));
	    false when WhenLn/=0 andalso WhenLn==PEndLn->
		wrangler_prettypr_0:horizontal([D, text("when")]);
	    _ ->
		par([D, text("when")], Ctxt#ctxt.break_indent)
	end,
    case WhenLn/=0 andalso WhenLn==StartLn of
	true ->
	    wrangler_prettypr_0:horizontal([D1, G]);
	false when StartLn-WhenLn==1 ->
	    above(D1, nest(StartCol-CsStartCol, G));
	_ ->
	    par([D1, G], Ctxt#ctxt.break_indent)
    end.

append_elems(Fun, {D1, {{_D1StartLn, D1StartCol}, {D1EndLn, D1EndCol}}}, 
	     {D2, {{D2StartLn, D2StartCol}, _D2End}}) -> 
    case D1EndLn==0 orelse D2StartLn==0 orelse D2StartLn<D1EndLn of
	true ->
	    Fun([D1,D2]);
	false when D1EndLn==D2StartLn ->
	    Gap = D2StartCol -D1EndCol-1,
	    S = text(empty_str(Gap)),
            beside(D1, beside(S, D2));
	_ ->
	    Nest=D2StartCol-D1StartCol,
	    above(D1, nest(Nest, D2))
    end.

append_keywords(KeyWord1, KeyWord2, CsD, Node, Ctxt) ->
    {{CsStartLn, CsStartCol}, {CsEndLn, CsEndCol}} = get_start_end_loc(Node),
    {KeyWord1Line, KeyWord1Col} =
	get_keyword_loc_before(list_to_atom(KeyWord1), Ctxt,
			       {CsStartLn, CsStartCol}),
    {KeyWord2Line, _KeyWord2Col} =
	get_keyword_loc_after(list_to_atom(KeyWord2), Ctxt,
			      {CsEndLn, CsEndCol}),
    KeyWord1CsD = case KeyWord1Line == 0 of
		      false when KeyWord1Line==CsStartLn ->
			  Gap =CsStartCol-(KeyWord1Col+length(KeyWord1)),
			  Space=text(empty_str(Gap)),
			  [horizontal([text(KeyWord1), Space, CsD])];
		      false when CsStartLn-KeyWord1Line>=1 ->
			  [above(text(KeyWord1), nest(CsStartCol-KeyWord1Col, CsD))];
		      _ ->
			 case lists:member(KeyWord1, ["if", "fun"]) of
			     true ->
				 [follow(text(KeyWord1), CsD, Ctxt#ctxt.break_indent)];
			     false ->
				 [text(KeyWord1), nest(Ctxt#ctxt.sub_indent, CsD)]
			 end
		  end,
    case KeyWord2Line == 0 of
	false when KeyWord2Line==CsEndLn ->
	    wrangler_prettypr_0:horizontal(KeyWord1CsD ++ [text(KeyWord2)]);
	false when KeyWord2Line-CsEndLn>=1 ->
	    above(sep(KeyWord1CsD), text(KeyWord2));
                  %% nest(KeyWord2Col-KeyWord1Col, text(KeyWord2)));
	_ ->
	    sep(KeyWord1CsD ++ [text(KeyWord2)])
    end.

append_leading_keyword(KeyWord, _, [],_Ctxt) ->
    text(KeyWord);
append_leading_keyword(KeyWord, CsD, Node, Ctxt) ->
    {CsStartLn, CsStartCol} = case is_list(Node) of
				  true -> get_start_loc(hd(Node));
				  false -> get_start_loc(Node)
			      end,
    {KeyWordLine, KeyWordCol} =
	get_keyword_loc_before(list_to_atom(KeyWord), Ctxt,
			       {CsStartLn, CsStartCol}),
    case KeyWordLine == 0 of
	false when KeyWordLine==CsStartLn ->
	    Gap =case CsStartCol-(KeyWordCol+length(KeyWord)) of
                     G when G>1 ->
                         G;
                     _ -> 1
                 end,           
	    Space=text(empty_str(Gap)),
	    horizontal([text(KeyWord), Space, CsD]);
	false when CsStartLn-KeyWordLine>=1 ->
	    above(text(KeyWord), nest(CsStartCol-KeyWordCol, CsD));
	_ ->
	    sep([text(KeyWord), nest(Ctxt#ctxt.sub_indent, CsD)])
    end.
   

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
    lay(wrangler_syntax:abstract(T),Ctxt).

seq_1([H|T],Separator,Ctxt,Fun) ->
    case T of
        [] -> [maybe_parentheses_1(Fun(H,Ctxt), H, Ctxt)];
	_->
	    [maybe_append(Separator,maybe_parentheses_1(Fun(H,Ctxt),H, Ctxt))|
             seq_1(T,Separator,Ctxt,Fun)]
    end;
seq_1([],_,_,_) -> [empty()].


seq([H|T],Separator,Ctxt,Fun) ->
    case T of
        [] -> [Fun(H,Ctxt)];
	_->
	    [maybe_append(Separator,Fun(H,Ctxt))|
             seq(T,Separator,Ctxt,Fun)]
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

horizontal([D]) -> D;
horizontal([D| Ds]) -> beside(beside(D, nil()),horizontal(Ds));
horizontal([]) -> [].

horizontal_1([D]) -> D;
horizontal_1([D| Ds]) -> beside(D,horizontal_1(Ds));
horizontal_1([]) -> [].


lay_elems(_Fun, _ElemDocs,[], _Ctxt) -> null;
lay_elems(Fun, ElemDocs,Elems,Ctxt) ->
    ARanges = [wrangler_misc:get_start_end_loc_with_comment(E) || E <- Elems],
    case lists:all(fun(R) -> R=={{0,0},{0,0}} end, ARanges) of 
	true ->
	    Fun(ElemDocs);
	false ->
            lay_elems_1(lists:zip(ElemDocs,ARanges), Ctxt,[],{{1,1},{1,1}}, 1)
    end.

lay_elems_1([], _Ctxt, Acc, _LastLine, _LastOffset) ->
    Docs = lists:map(fun (Ds) -> horizontal(Ds) end, Acc),
    vertical(lists:reverse(Docs));
lay_elems_1([{D, SE={{_SLn, SCol}, {_ELn, _ECol}}}|Ts], Ctxt, [], _LastLn, _StartOffset) ->
    lay_elems_1(Ts, Ctxt, [[D]], SE, SCol);
lay_elems_1([{D, SE={{SLn, SCol}, {_ELn, _ECol}}}| Ts], Ctxt, [H| T], 
	     _LastLoc={{_LastSLn, _LastSCol}, {LastELn, LastECol}}, StartOffset) ->
    case SLn == 0 orelse LastELn == 0 orelse SLn =< LastELn of
	true ->
	    lay_elems_1(Ts, Ctxt, [H ++ [D]| T], SE, StartOffset);
	false  when SLn-LastELn==1->
	    lay_elems_1(Ts, Ctxt, [[nest(SCol-StartOffset, D)], H|T], SE, StartOffset);
        _ ->
            case SLn-LastELn>1 andalso 
                real_white_line(Ctxt#ctxt.tokens, {LastELn, LastECol}, {SLn, SCol}) of
                true->
                    lay_elems_1(Ts, Ctxt, [[above(white_lines(SLn-LastELn-1),
                                                       nest(SCol-StartOffset, D))], H|T],
                                SE, StartOffset);
                false ->
                    lay_elems_1(Ts, Ctxt, [H ++ [D]| T], SE, StartOffset)
            end
    end.

  
lay_body_elems(_ElemDocs,[], _Ctxt) -> null;
lay_body_elems(ElemDocs,Elems, Ctxt) ->
    lay_body_elems_1(lists:zip(ElemDocs, Elems), Ctxt, [], {{1,1},{1,1}}).
   

lay_body_elems_1([], _Ctxt, Acc, _LastRange) ->
    Docs = lists:map(fun (Ds) -> wrangler_prettypr_0:horizontal(Ds) end, Acc),
    vertical(lists:reverse(Docs));
lay_body_elems_1([{D, Elem}| Ts],  Ctxt,[], _LastLoc) ->
    {SLoc, ELoc} = wrangler_misc:get_start_end_loc_with_comment(Elem),
    lay_body_elems_1(Ts,  Ctxt,[[D]], {SLoc, ELoc});
lay_body_elems_1([{D, Elem}| Ts], Ctxt, [H| T], 
		 _LastLoc={{_LastSLn, _LastSCol}, {LastELn, LastECol}}) ->
    Range={{SLn, SCol},{_ELn, _ECol}} = wrangler_misc:get_start_end_loc_with_comment(Elem),
    As = wrangler_syntax:get_ann(Elem),
    case lists:keysearch(layout, 1, As) of 
        {value, {layout, horizontal}} ->
            lay_body_elems_1(Ts,  Ctxt,[H ++ [D]| T], Range);
        {value, {layout, vertical}} ->
            lay_body_elems_1(Ts,  Ctxt,[[D],H|T], Range);
         false ->
            case SLn == 0 orelse LastELn == 0 orelse SLn < LastELn of
                true -> 
                    lay_body_elems_1(Ts,  Ctxt,[[D],H|T], Range);
                false -> 
                    case SLn - LastELn of
                        0 ->
                            lay_body_elems_1(Ts,  Ctxt,[H ++ [D]| T], Range);
                        1 ->
                            lay_body_elems_1(Ts,  Ctxt,[[D], H|T], Range);
                        N ->
                            case N>1 andalso 
                                real_white_line(Ctxt#ctxt.tokens, {LastELn, LastECol}, {SLn, SCol}) of
                                true->
                                    lay_body_elems_1(Ts,  Ctxt, [[above(white_lines(N-1),D)], H|T],Range);
                                false ->
                                    lay_body_elems_1(Ts,  Ctxt,[[D],H|T], Range)
                            end
                    end
            end
    end.


lay_body_elems_2([], _Ctxt, Acc, _LastRange) ->
    Docs = lists:map(fun (Ds) -> wrangler_prettypr_0:horizontal(Ds) end, Acc),
    vertical(lists:reverse(Docs));
lay_body_elems_2([{D, {SLoc, ELoc}}| Ts],  Ctxt,[], _LastLoc) ->
    lay_body_elems_2(Ts,  Ctxt,[[D]], {SLoc, ELoc});
lay_body_elems_2([{D,Range={{SLn, SCol}, {_ELn, _ECol}}}| Ts], Ctxt, [H| T], 
		 _LastLoc={{_LastSLn, _LastSCol}, {LastELn, LastECol}}) ->
    case SLn == 0 orelse LastELn == 0 orelse SLn < LastELn of
        true -> 
            lay_body_elems_2(Ts,  Ctxt,[[D],H|T], Range);
        false -> 
            case SLn - LastELn of
                0 ->
                    lay_body_elems_2(Ts,  Ctxt,[H ++ [D]| T], Range);
                        1 ->
                    lay_body_elems_2(Ts,  Ctxt,[[D], H|T], Range);
                N ->
                    case N>1 andalso 
                        real_white_line(Ctxt#ctxt.tokens, {LastELn, LastECol}, {SLn, SCol}) of
                        true->
                            lay_body_elems_2(Ts,  Ctxt, [[above(white_lines(N-1),D)], H|T],Range);
                        false ->
                            lay_body_elems_2(Ts,  Ctxt,[[D],H|T], Range)
                    end
            end
    end.
      

empty_str(N) when N<1 
                  -> "";
empty_str(N) -> 
    lists:append(lists:duplicate(N, " ")). 

nil() -> text("").    
    
white_lines(N) ->
    case N>1 of 
        true ->
            above(text(""), 
                  white_lines(N-1));
        _ ->
            text("")
    end.

real_white_line(Toks, _Loc1, _Loc2) when Toks==[] ->
    false;
real_white_line(Toks, Loc1, Loc2) ->
    Toks1 = lists:dropwhile(fun(T)->
                                    token_loc(T)=<Loc1
                            end, Toks),
    case Toks1 of
        [] ->
            false;
        [_T|Toks2] ->
            Toks3=lists:takewhile(fun(T)->
                                          token_loc(T)<Loc2
                                  end, Toks2),
            lists:all(fun(T) ->
                              is_whitespace_or_comment(T) orelse
                                  size(T)==2
                      end, Toks3)
    end.
    
    

get_prev_keyword_loc(FormToks, StartPos, KeyWord)->
    Ts1 = lists:takewhile(
	    fun(T) ->
		    token_loc(T)=<StartPos
	    end, FormToks),
    Ts2=lists:dropwhile(fun(T)->
				element(1, T)/=KeyWord
			end, lists:reverse(Ts1)),
    case Ts2 of 
	[] ->
	    {0,0};
	[T1|_] ->
	    token_loc(T1)
    end.

get_keyword_loc_after(_Keyword, #ctxt{tokens=Toks}, _Pos) when Toks==[] ->
    {0,0};
get_keyword_loc_after(Keyword,#ctxt{tokens=Toks}, Pos) ->
    FirstLoc = token_loc(hd(Toks)),
    LastLoc  = token_loc(lists:last(Toks)),
    case FirstLoc >= Pos orelse LastLoc =< Pos of 
        true -> {0,0};
        false ->
            get_keyword_loc_after_1(Keyword, Toks, Pos)
    end.
get_keyword_loc_after_1(Keyword, Toks,Pos)->
    Toks1 = lists:dropwhile(fun (T) ->
                                    token_loc(T) =< Pos 
                            end, Toks),
    case Toks1 of
	[] ->
	    {0,0};
	_ ->
	    {Toks2, Toks3} = lists:splitwith(
                               fun (T) ->
                                       token_val(T) =/=Keyword 
                               end, Toks1),
	    case Toks3 of 
		[] ->
		    {0,0};   
		_ ->
		    case lists:all(fun(T)-> is_whitespace_or_comment(T) orelse 
			 token_val(T)=='(' orelse token_val(T)==')'end, Toks2) of
                        true ->
                            token_loc(hd(Toks3));
                        _ ->
                            {0,0}
		  end
	    end
    end.



get_keyword_loc_before(_Keyword, #ctxt{tokens=Toks}, _Pos) when Toks==[] ->
    {0,0};
get_keyword_loc_before(Keyword,#ctxt{tokens=Toks}, Pos) ->
    FirstLoc = token_loc(hd(Toks)),
    LastLoc  = token_loc(lists:last(Toks)),
    case FirstLoc >= Pos orelse LastLoc =< Pos of 
        true -> {0,0};
        false ->
            get_keyword_loc_before_1(Keyword, Toks, Pos)
    end.
    
get_keyword_loc_before_1(Keyword, Toks,Pos)->
    Toks1 = lists:takewhile(fun (T) -> token_loc(T)< Pos end, Toks),
    case Toks1 of
	[] ->
	    {0,0};
	_ ->
	    {Toks2, Toks3} = lists:splitwith(
                               fun (T) -> 
                                       token_val(T) =/=Keyword 
                               end, lists:reverse(Toks1)),
	    case Toks3 of 
		[] ->
		    {0,0};   
		_ ->
		    case lists:all(fun(T)-> 
                                           is_whitespace_or_comment(T) orelse 
                                               token_val(T)=='(' orelse 
                                               token_val(T)==')'
                                   end, Toks2) of
			true ->
			    token_loc(hd(Toks3));
			_ ->
			    {0,0}
		    end
	    end
    end.


get_right_bracket_loc(Toks,LeftLoc,LeftBracket,RightBracket) ->
    Toks1 = lists:dropwhile(fun (T) -> 
				    token_loc(T) =< LeftLoc orelse
					is_whitespace_or_comment(T)
                            end, Toks),
    case Toks1 of
	[] ->
	    {0,0};
	_ ->	
	    get_right_bracket_loc_1(Toks1,0,LeftBracket,RightBracket)
    end.

get_right_bracket_loc_1([],_,_,_) ->
    {0,0};
get_right_bracket_loc_1([T|Toks1],UnBalanced,LeftBracket,RightBracket) ->
    case token_val(T) of
	LeftBracket ->
	    get_right_bracket_loc_1(Toks1,UnBalanced + 1,LeftBracket,RightBracket);
	RightBracket ->
	    case UnBalanced of
		0 ->
		    token_loc(T);
		_ ->
		    get_right_bracket_loc_1(Toks1,UnBalanced-1,LeftBracket,RightBracket)
	    end;
	_ ->
	    get_right_bracket_loc_1(Toks1,UnBalanced,LeftBracket,RightBracket)
    end.

		    
get_separator(_NodeList, [], Default) ->
    Default;
get_separator([],_, Default) ->
    Default;
get_separator(NodeList, Ctxt, Default) when is_list(NodeList) -> 
    Toks = Ctxt#ctxt.tokens,
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
get_separator(_Node, _Ctxt, Default) ->
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


need_parentheses(Node, Ctxt) ->
    case Ctxt#ctxt.check_bracket of 
        true ->
            has_parentheses(Node, Ctxt#ctxt.tokens, "((", "))");
        false ->
            As = wrangler_syntax:get_ann(Node),
            case lists:keysearch(with_bracket, 1, As) of
                {value, {with_bracket, true}} ->
                    true;
                {value, {with_bracket, false}} ->
                    false;
                _ ->
                    has_parentheses(Node, Ctxt#ctxt.tokens, "(", ")")
            end
    end.

%% the proper way should be to modify the paser to keep the 
%% the brackets!!!

has_parentheses(Node, Toks) ->
    has_parentheses(Node, Toks, "(", ")").

has_parentheses(Node, Toks, Left, Right)->		  
    {StartLoc,EndLoc} = get_start_end_loc(Node),
    Toks0 = [T||T<-Toks, not is_whitespace_or_comment(T)],
    {Toks1, Toks2} = lists:splitwith(
                       fun(T) ->
                               token_loc(T)<StartLoc
                       end, Toks0),
    {Toks21, Ts2} = lists:splitwith(
              fun(T) -> 
                      token_loc(T) =< EndLoc 
              end, Toks2),
    Ts1Str=wrangler_misc:concat_toks(lists:reverse(Toks1)),
    Ts2Str=wrangler_misc:concat_toks(Ts2),
    Str21 =wrangler_misc:concat_toks(Toks21),
    case well_formed_parentheses(Str21) of
        true ->
            lists:prefix(Left, Ts1Str) andalso
                lists:prefix(Right, Ts2Str);
        {false, {L, R}} ->
            StrL =lists:append(lists:duplicate(R, "(")),
            StrR= lists:append(lists:duplicate(L, ")")),
            lists:prefix(Right++StrR , Ts2Str) andalso
                lists:prefix(Left++StrL, Ts1Str)
    end.
  
   
%%===============================================================
is_whitespace({whitespace, _, _}) ->
    true;
is_whitespace(_) ->
    false.
	
is_whitespace_or_comment({whitespace, _, _}) ->
    true;
is_whitespace_or_comment({comment, _, _}) ->
    true;
is_whitespace_or_comment(_) -> false.


token_loc(T) ->
    case T of
      {_, L, _V} -> L;
      {_, L1} -> L1
    end.

token_val(T) ->
    case T of
      {_, _, V} -> V;
      {V, _} -> V
    end.

remove_trailing_whitespace(Str, TabWidth, FileFormat) ->
    {ok, Toks, _} = wrangler_scan_with_layout:string(Str, {1,1}, TabWidth, FileFormat),
    remove_trailing_whitespace(Toks, []).
remove_trailing_whitespace([], Acc) ->
    wrangler_misc:concat_toks(lists:reverse(Acc));
remove_trailing_whitespace([{',',L},{whitespace, _, ' '}, {whitespace, L1, '\r'}| S], Acc) ->
    remove_trailing_whitespace(S, [{whitespace, L1, '\r'},{',',L}| Acc]);
remove_trailing_whitespace([{',',L},{whitespace, _, ' '}, {whitespace, L1, '\n'}| S], Acc) ->
    remove_trailing_whitespace(S, [{whitespace, L1, '\n'},{',',L}| Acc]);
remove_trailing_whitespace([C| S], Acc) ->
    remove_trailing_whitespace(S, [C| Acc]).


remove_trailing_whites(Str) ->
    Str1=lists:dropwhile(fun(S)->
                                 lists:member(S, [$\n, $\r, $\t, $\s])
                         end, lists:reverse(Str)),
    lists:reverse(Str1).
    
get_leading_whites(OriginalToks) ->
    Ts1=lists:takewhile(fun(Ts) ->
                                all_whites(Ts)
                        end, OriginalToks),
    wrangler_misc:concat_toks(lists:append(Ts1)).

get_trailing_whites(Str) ->   
    lists:reverse(lists:takewhile(fun(S) ->
                                          lists:member(S, [$\n, $\r, $\t, $\s])
                                  end, lists:reverse(Str))).
    

%% =====================================================================

get_start_loc(Node) ->
    {L, _} = get_start_end_loc(Node),
    L.

get_end_loc(Node) ->
    {_, L} = get_start_end_loc(Node),
    L.
get_start_end_loc(Node)->
    wrangler_misc:start_end_loc(Node).

get_start_line_with_comment(Node) ->
    {{L, _}, _} = wrangler_misc:get_start_end_loc_with_comment(Node),
    L.
   
get_end_line_with_comment(Node) ->
    {_, {L, _}} = wrangler_misc:get_start_end_loc_with_comment(Node),
    L.

get_start_loc_with_comment(Node) ->
    {Start, _}= wrangler_misc:get_start_end_loc_with_comment(Node),
    Start.

get_end_loc_with_comment(Node) ->
    {_, End}=wrangler_misc:get_start_end_loc_with_comment(Node),
    End.

 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

repair_new_form_str("", NewFormStr, _TabWidth, _FileFormat) -> NewFormStr;
repair_new_form_str(OldFormStr, NewFormStr, TabWidth, FileFormat)->
    {ok, OldToks0, _} = wrangler_scan_with_layout:string(OldFormStr, {1,1}, TabWidth, FileFormat),
    OldToksByLine =group_toks_by_line(OldToks0),
    Str1 = get_leading_whites(OldToksByLine),
    Str2 = get_trailing_whites(OldFormStr),
    NewFormStr1=Str1++remove_trailing_whites(NewFormStr)++Str2,
    {ok, NewToks0, _} = wrangler_scan_with_layout:string(NewFormStr1, {1,1}, TabWidth, FileFormat),
    NewToksByLine = group_toks_by_line(NewToks0),
    DiffByLine = levenshtein_dist(OldToksByLine, NewToksByLine, TabWidth),
    repair_form_layout(DiffByLine, TabWidth).
    
 
repair_form_layout(DiffByLine, TabWidth) ->
    repair_form_layout(DiffByLine, none, TabWidth, []).
repair_form_layout([], _, _TabWidth, Acc) ->
    wrangler_misc:concat_toks(lists:append(lists:reverse(Acc)));
repair_form_layout([{'*', LineToks}|Lines], PrevDiff, TabWidth, Acc) ->
    case all_whites(LineToks) of
        true ->
            repair_form_layout(Lines, PrevDiff, TabWidth,[LineToks|Acc]);
        false ->
            repair_form_layout(Lines, '*', TabWidth,[LineToks|Acc])
    end;
repair_form_layout([{'d', _LineToks}|Lines], _PrevDiff, TabWidth, Acc) ->
    repair_form_layout(Lines, 'd', TabWidth, Acc);
repair_form_layout([{'i', LineToks}|Lines], _PrevDiff, TabWidth, Acc) ->
    repair_form_layout(Lines, 'i', TabWidth, [LineToks|Acc]);
repair_form_layout([{'s', OldLineToks, NewLineToks}|Lines], PrevDiff, TabWidth, Acc) ->
    case has_editing_change(OldLineToks, NewLineToks) of
        true ->
            case Lines of 
                [{i, Toks}|Lines1] ->
                    case wrangler_misc:concat_toks(remove_whites(Toks)) of
                        S when S=="->" orelse S=="of" ->
                            case remove_loc_and_whites(OldLineToks)--
                                remove_loc_and_whites(NewLineToks) of
                                [T={A, _}] when A=='->' orelse A=='of' ->
                                    case has_layout_change(OldLineToks, NewLineToks, TabWidth)==false andalso 
                                        has_editing_change(OldLineToks, NewLineToks) == false of
                                        true ->
                                            repair_form_layout(Lines1, '*', TabWidth, [OldLineToks|Acc]);
                                        false ->
                                            NewLineToks1 = insert_token_at_end(NewLineToks, T),
                                            NewLineToks2 = recover_tab_keys(OldLineToks, NewLineToks1, TabWidth),
                                            repair_form_layout(Lines1, 's', TabWidth, [NewLineToks2|Acc])
                                    end;
                                _ ->
                                    NewLineToks1 = recover_tab_keys(OldLineToks, NewLineToks, TabWidth),
                                    repair_form_layout(Lines, 's', TabWidth, [NewLineToks1|Acc])
                            end;
                        _ ->
                            NewLineToks1 = recover_tab_keys(OldLineToks, NewLineToks, TabWidth),
                            repair_form_layout(Lines, 's', TabWidth, [NewLineToks1|Acc])
                    end;
                _ ->
                    NewLineToks1 = recover_tab_keys(OldLineToks, NewLineToks, TabWidth),
                    repair_form_layout(Lines, 's', TabWidth, [NewLineToks1|Acc])
            end;
        false ->
            %% layout change.
            case PrevDiff of 
                '*' ->
                    repair_form_layout(Lines, '*', TabWidth, [OldLineToks|Acc]);
                _ ->
                    case all_whites_or_comments(OldLineToks) of 
                        true ->
                            Lines1 = lists:dropwhile(
                                       fun(L) ->
                                               all_whites_or_comments(element(2,L))
                                       end, Lines),
                            case Lines1 of
                                [{'s', Toks1, Toks2}|_] ->
                                    case has_layout_change(Toks1, Toks2, TabWidth) of 
                                        true ->
                                            NewLineToks1 = recover_tab_keys(OldLineToks, NewLineToks, TabWidth),
                                            repair_form_layout(Lines, 's', TabWidth, [NewLineToks1|Acc]);
                                        false ->
                                            repair_form_layout(Lines, '*', TabWidth, [OldLineToks|Acc])
                                    end;
                                _ ->
                                    repair_form_layout(Lines, '*', TabWidth, [OldLineToks|Acc])
                            end;                                
                        false ->
                            NewLineToks1 = recover_tab_keys(OldLineToks, NewLineToks, TabWidth),
                            repair_form_layout(Lines, 's', TabWidth, [NewLineToks1|Acc])
                    end
            end
    end.

has_editing_change(Toks1, Toks2) ->
    remove_loc_and_whites(Toks1) =/=
        remove_loc_and_whites(Toks2).

has_layout_change(Toks1, Toks2, TabWidth) ->
    Toks11 = expand_tab_keys(Toks1, TabWidth),
    Toks21 = expand_tab_keys(Toks2, TabWidth),
    Whites1 = lists:takewhile(fun(T) -> is_whitespace(T) end, Toks11),
    Whites2 = lists:takewhile(fun(T) -> is_whitespace(T) end, Toks21),
    length(Whites1)/=length(Whites2).
                                      
                                    
    
recover_tab_keys(OldToks, NewToks, TabWidth)->
    TabToks =lists:takewhile(fun(T)-> is_tab_token(T) end, OldToks),
    {WhiteToks, Toks1}=lists:splitwith(fun(T) -> 
                                               case T of
                                                   {whitespace, _L, ' '} ->
                                                       true;
                                                   _ -> false
                                               end
                                       end, NewToks),
    recover_tab_keys(TabToks, WhiteToks, TabWidth, [])++Toks1.

recover_tab_keys([], WhiteSpaces, _TabWidth, Acc) ->
    lists:reverse(Acc)++WhiteSpaces;
recover_tab_keys([T={whitespace, _, '\t'}], WhiteSpaces, TabWidth, Acc) ->
    case length(WhiteSpaces)>=TabWidth of
        true ->
            recover_tab_keys([], lists:nthtail(TabWidth, WhiteSpaces), TabWidth, [T|Acc]);
        false ->
            lists:reverse(Acc)++WhiteSpaces
    end;
recover_tab_keys([T={whitespace, _, '\t'}|Ts], WhiteSpaces, TabWidth, Acc) ->
    case length(WhiteSpaces)>=TabWidth of
        true ->
            recover_tab_keys(Ts, lists:nthtail(TabWidth, WhiteSpaces), TabWidth, [T|Acc]);
        false ->
           lists:reverse(Acc)++WhiteSpaces
    end.

group_toks_by_line(Toks) ->
    Toks1 =[case T  of 
		{K, {L,_C}, V} -> {K, L, V};
		{K, {L,_C}} -> {K, L}
	    end || T <-Toks],
    group_toks_by_line_1(Toks1,[]).

group_toks_by_line_1([],Acc) -> lists:reverse(Acc);
group_toks_by_line_1(Toks = [T| _Ts],Acc) ->
    L = element(2,T),
    {Toks1, Toks2} = 
	lists:partition(fun (T1) ->
				element(2,T1) == L
			end,
			Toks),
    group_toks_by_line_1(Toks2,[Toks1|Acc]).


insert_token_at_end(Toks, T) ->
    {Toks1, Toks2} = lists:splitwith(
                       fun(Tok) ->
                               is_whitespace_or_comment(Tok)
                       end, lists:reverse(Toks)),
    lists:reverse(Toks1++[T|Toks2]).
    
                        
is_tab_token({whitespace, _, '\t'}) ->
    true;
is_tab_token(_) ->
    false.

                                    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

calc_levenshtein_dist(OldToks, NewToks, TabWidth) ->
    {Matrix, OldLen, NewLen} = calc_matrix(OldToks, NewToks,TabWidth),
    {value, {_, Val}} = lists:keysearch({OldLen, NewLen}, 1, Matrix),
    Val.
    
levenshtein_dist(OldToks, NewToks, TabWidth) ->
    {Matrix, OldLen, NewLen} = calc_matrix(OldToks, NewToks,TabWidth),
    get_edit_ops(Matrix, OldToks, NewToks, OldLen, NewLen, TabWidth, []). 
    
calc_matrix(OldToks, NewToks, TabWidth) ->
    OldLen = length(OldToks),
    NewLen = length(NewToks),
    Is = lists:seq(0, OldLen),
    Js = lists:seq(0, NewLen),
    InitAcc = [{{I, 0}, I} || I <- Is] 
        ++ [{{0, J}, J} || J <- Js],
    Matrix = levenshtein_dist(OldToks, NewToks, OldLen,
                              NewLen, TabWidth, {1, 1}, InitAcc),
    {Matrix, OldLen, NewLen}.


levenshtein_dist(_OldToks, _NewToks, _OldLen, NewLen, _TabWidth, {_I, J}, Acc)
  when J>NewLen ->  Acc;
levenshtein_dist(OldToks, NewToks, OldLen, NewLen, TabWidth, {I, J}, Acc)
  when I>OldLen ->
    levenshtein_dist(OldToks, NewToks, OldLen, NewLen, TabWidth, {1, J+1}, Acc);
levenshtein_dist(OldToks, NewToks, OldLen, NewLen, TabWidth, {I, J}, Acc) ->
    Cost = case same(lists:nth(I, OldToks), lists:nth(J, NewToks), TabWidth) of
               true ->
                   0;
               false ->
                   1
           end,
    {value, {_, Del}} = lists:keysearch({I-1, J}, 1,Acc),
    {value, {_, Ins}} = lists:keysearch({I, J-1}, 1,Acc),
    {value, {_, Sub}} = lists:keysearch({I-1, J-1},1, Acc),
    Min = lists:min([Del+1, Ins+1, Sub+Cost]),
    levenshtein_dist(OldToks, NewToks, OldLen, NewLen, TabWidth, {I+1, J}, 
                                               [{{I,J},Min}|Acc]).

get_edit_ops(_Matrix, _OldToks, _NewToks, I, J, _TabWidth, Acc) 
  when I=<0 andalso J=<0 ->
    Acc;
get_edit_ops(Matrix, OldToks, NewToks, I, J, TabWidth, Acc) 
  when I=<0 ->
    Jth = lists:nth(J, NewToks),
    get_edit_ops(Matrix, OldToks, NewToks, I, J-1, TabWidth, [{'i', Jth}|Acc]);
get_edit_ops(Matrix, OldToks, NewToks, I, J, TabWidth, Acc) 
  when  J=<0 ->
    Ith = lists:nth(I, OldToks),
    get_edit_ops(Matrix, OldToks, NewToks, I-1, J, TabWidth, [{'d', Ith}|Acc]);
get_edit_ops(Matrix, OldToks, NewToks, I, J, TabWidth,Acc) ->
    Ith = lists:nth(I, OldToks),
    Jth = lists:nth(J, NewToks),
    case same(Ith, Jth, TabWidth) of
        true ->
            get_edit_ops(Matrix, OldToks, NewToks, I-1, J-1, TabWidth, [{'*', Ith}|Acc]);
        false ->
            {value, {_, Del}} = lists:keysearch({I-1, J}, 1,Matrix),
            {value, {_, Ins}} = lists:keysearch({I, J-1}, 1,Matrix),
            {value, {_, Sub}} = lists:keysearch({I-1, J-1},1, Matrix),
                   case lists:min([Del, Ins, Sub]) of
                Ins ->
                    get_edit_ops(Matrix, OldToks, NewToks, I, J-1, TabWidth, [{'i', Jth}|Acc]);
                Del ->
                    get_edit_ops(Matrix, OldToks, NewToks, I-1, J, TabWidth, [{'d', Ith}|Acc]);
                Sub ->
                    get_edit_ops(Matrix, OldToks, NewToks, I-1, J-1, TabWidth, [{'s', Ith, Jth}|Acc])
            end
    end.


same(Toks1, Toks2, TabWidth) ->
    case all_whites(Toks1) andalso all_whites(Toks2) of 
	true ->
	    true;
	false ->
	    same_toks_1(Toks1, Toks2, TabWidth)
    end.
same_toks_1(Toks1, Toks2, TabWidth) ->
    Toks11=expand_tab_keys(Toks1, TabWidth),
    Toks21=expand_tab_keys(Toks2, TabWidth),
    Toks12=remove_whites_after_first_non_white_token(remove_locs(Toks11)),
    Toks22=remove_whites_after_first_non_white_token(remove_locs(Toks21)),
    Toks12==Toks22.


all_whites_or_comments(Toks) ->
     lists:all(fun(T) ->
                       is_whitespace_or_comment(T) 
               end, Toks).
    
all_whites(Toks) ->
    lists:all(fun(T) ->
		      is_whitespace(T) 
	      end, Toks).

expand_tab_keys(Toks, TabWidth) ->
    lists:append([case T of 
		      {whitespace, L, '\t'} ->
			  lists:duplicate(TabWidth, {whitespace, L, ' '});
		      _ -> [T]
		  end || T<-Toks]).


remove_whites_after_first_non_white_token(Toks) ->
    {Toks1, Toks2}=lists:splitwith(fun(T) ->
					   is_whitespace_or_comment(T)
				   end, Toks),
    Toks1++[T||T<-Toks2, not is_whitespace(T)].

remove_loc(T) ->
    case T of
        {K, _L, V} -> {K, 0, V};
        {K, _L} -> {K, 0};
	 _ -> T
    end.

remove_locs(Toks) when is_list(Toks) ->
    [remove_loc(T)|| T<-Toks];
remove_locs(Others) ->
    Others.

remove_loc_and_whites(Toks) ->
    [remove_loc(T)||T<-Toks,
         not is_whitespace(T)].

remove_whites(Toks) ->
    [T||T<-Toks, not is_whitespace(T)].

remove_locs_whites_and_comments(Toks) ->
    [remove_loc(T)||T<-Toks,
         not is_whitespace_or_comment(T)].

revert_clause_guard(E)->
    case  wrangler_syntax:type(E) of
        disjunction -> wrangler_syntax:revert_clause_disjunction(E);
        conjunction ->
            %% Only the top level expression is
            %% unfolded here; no recursion.
            [wrangler_syntax:revert(wrangler_syntax:conjunction_body(E))];
        _ ->
            [[E]]       % a single expression
    end.
%%% known problems:
%% patchFroIdLines([Line, X| []]) ->
%%     a:patchTheFroId(Line).

%% negative indent is a problem.

lay_spec_args([FunSpec, TypeSpecs], Ctxt)->
    D1 = lay_fun_spec(FunSpec, Ctxt),
    D2 = lay_type_specs(TypeSpecs,Ctxt),
    [D1, D2].

lay_fun_spec(FunSpec, Ctxt)->
    Toks = Ctxt#ctxt.tokens,
    HasArity=lists:member('/', [element(1, T)||T<-Toks]),
    case wrangler_syntax:type(FunSpec) of 
        tuple ->
            case wrangler_syntax:tuple_elements(FunSpec) of 
                [F,A] ->
                    D1 = lay(F, Ctxt),
                    case HasArity of 
                        true ->
                            D2 = lay(A, Ctxt),
                            beside(beside(D1, beside(text("/"), D2)), text("::"));
                        false ->
                            D1
                    end;
                [M,F,A] ->
                    D1 = lay(M, Ctxt),
                    D2 = lay(F, Ctxt),
                    case HasArity of 
                        true ->
                            D3 = lay(A, Ctxt),
                            D23= beside(D2, beside(text("/"), D3)),
                            beside(beside(D1, beside(text(":"), D23)), text("::"));
                        false ->
                            beside(D1, beside(text(":"), D2))
                    end
            end;
        _ -> %% this should not happen
            lay(FunSpec, Ctxt)
    end.

lay_type_specs(TypeSpecs, Ctxt) ->
    SpecCs =wrangler_syntax:list_elements(TypeSpecs),
    lay_spec_clauses(SpecCs, Ctxt).

lay_spec_clauses([C|Cs], Ctxt) ->
    Ds=[lay_sig_type(C, Ctxt)|[beside(text("; "), lay_sig_type(C1, Ctxt))||C1<-Cs]],
    wrangler_prettypr_0:par(Ds, -2).

lay_sig_type(SigType,Ctxt) ->
    T1 = wrangler_syntax:revert(SigType), 
    case T1 of 
        {type, _Line, bounded_fun, [T, Gs]} ->
            lay_guard_type(lay_fun_type(T, Ctxt), Gs, Ctxt);
        {type, _Line, 'fun', _T} ->
            lay_fun_type(T1,Ctxt)
    end.

lay_guard_type(Before, Gs, Ctxt) ->
    Ds = seq(Gs, floating(text(",")), Ctxt, fun lay_constraint/2),
    %%wrangler_io:format("Gs:\n~p\n", [Gs]),
    D1 = lay_elems(fun wrangler_prettypr_0:par/1, Ds, Gs, Ctxt),
    par([beside(Before, text(" when")), D1]).

lay_constraint({type,_Line,constraint,[_Tag,[A |As]]}, Ctxt) ->
    D1=lay(A, Ctxt),
    D2=lay_type_args_1(As, Ctxt),                  
    wrangler_prettypr_0:horizontal([D1, text("::"), D2]).


lay_fun_type({type, _Line, 'fun', [FType, Ret]}, Ctxt) ->
    D1 = beside(text("("),beside(lay_type_args(FType, Ctxt), text(")"))),
   %% D1 =beside(lay_type_args(FType, Ctxt), text(")")),
    D2 = lay_type(Ret, Ctxt),
    par([beside(D1, text(" -> ")), D2], Ctxt#ctxt.break_indent).


lay_type_args({type, _Line, any}, _Ctxt) ->
    text("(...)");
lay_type_args({type, _Line, product, Ts}, Ctxt) ->
    lay_type_args_1(Ts, Ctxt).

lay_type_args_1(Ts, Ctxt) ->
    Ds=seq(Ts, floating(text(",")), Ctxt, fun lay_type_0/2),
    lay_elems(fun wrangler_prettypr_0:par/1, Ds, Ts, Ctxt).


lay_type_0(T, Ctxt) -> 
   %% wrangler_io:format("T:\n~p\n", [T]),
    lay_type(T, Ctxt).

lay_type({ann_type,_Line,[V,T]}, Ctxt) ->
     lay_field_type_1(lay(V, Ctxt), T, Ctxt);
lay_type({paren_type,_Line,[T]}, Ctxt) ->
    D = lay_type(T, Ctxt),
    beside(text("("), beside(D, text(")")));
lay_type({type,_Line,union,[T|Ts]}, Ctxt) ->
    Ds = [lay_type(T, Ctxt) | [lay_union_elem(T1, Ctxt)||T1<-Ts]],
    wrangler_prettypr_0:par(Ds);
lay_type({type,_Line,list,[T]}, Ctxt) ->
    D = lay_type(T, Ctxt),
    beside(text("["), beside(D, text("]")));
lay_type({type,_Line,nonempty_list,[T]}, Ctxt) ->
    D = wrangler_prettypr_0:par([lay_type(T, Ctxt), text(","), text("...")]),
    beside(text("["), beside(D, text("]")));
lay_type({type,_Line,nil,[]}, _Ctxt) ->
    beside(text("["), text("]"));
lay_type({type,_Line,tuple,any}, _Ctxt) ->
    text("tuple()");
lay_type({type,_Line,tuple,Ts}, Ctxt) ->
    Ds = seq(Ts, floating(text(",")), reset_prec(Ctxt), fun lay_type/2),
    D1 =lay_elems(fun wrangler_prettypr_0:par/1, Ds, Ts, Ctxt),
    beside(text("{"), beside(D1, text("}")));    
lay_type({type,_Line,record,[{atom,_,N}|Fs]}, Ctxt) ->
     lay_record_type(N, Fs,Ctxt);
lay_type({type,_Line,range,[I1,I2]}, Ctxt) ->
    D1 = lay(I1, Ctxt),
    D2 = lay(I2, Ctxt),
    beside(D1, beside(text(".."), D2));
lay_type({type,_Line,binary,[I1,I2]}, Ctxt) ->
    lay_binary_type(I1, I2, Ctxt);
lay_type(_T={type,_Line,'fun',[]}, _Ctxt) ->
    text("fun()");
lay_type({type,_,'fun',[{type,_,any},_]}=FunType, Ctxt) ->
    D1=lay_fun_type(FunType, Ctxt),
    beside(text("fun"),beside(text("("),  beside(D1, text(")"))));
lay_type(_T={type,_Line,'fun',[{type,_,product,_},_]}=FunType, Ctxt) ->
    D1 = lay_fun_type(FunType, Ctxt),
    beside(text("fun"),beside(text("("),  beside(D1, text(")"))));
lay_type({type,_Line,T,Ts}, Ctxt) ->
    D1 =text(atom_to_list(T)),
    D2 = lay_type_args_1(Ts, Ctxt),
    beside(beside(D1, text("(")), beside(D2, text(")")));
lay_type(_T={remote_type,_Line,[M,F,Ts]}, Ctxt) ->
    D1 = beside(lay(M, Ctxt), beside(text(":"), lay(F, Ctxt))),
    D2 = lay_type_args_1(Ts, Ctxt),
    beside(beside(D1, text("(")), beside(D2, text(")")));
lay_type({atom, _, T}, _Ctxt) ->
    text(atom_to_list(T));
lay_type(E, Ctxt)->
    lay(E, Ctxt).


lay_binary_type(I1, I2, Ctxt) ->
    B = [[] || {integer,_,0} <- [I1]] =:= [],
    U = [[] || {integer,_,0} <- [I2]] =:= [],
    Ctxt1 = reset_check_bracket(set_prec(Ctxt, max_prec())),
    E1 = [beside(text("_:"),lay(I1, Ctxt1)) || B],
    E2 = [beside(text("_:_*"),lay(I2, Ctxt1)) || U],
    case E1++E2 of 
        [D1, D2] -> wrangler_prettypr_0:horizontal(
                      [text("<<"), D1, text(", "), D2, text(">>")]);
        [D] ->
            wrangler_prettypr_0:horizontal(
              [text("<<"), D, text(">>")]);
        [] ->
            wrangler_prettypr_0:horizontal(
              [text("<<"), text(">>")])
    end.

lay_record_type(N, Fs, Ctxt) ->
    beside(beside(lay_record_name(N), text("{")),
       beside(lay_record_field_types(Fs, Ctxt), text("}"))).

lay_record_name(Name) ->
    text("#"++atom_to_list(Name)).

lay_record_field_types(Fs, Ctxt) ->
    Ds = seq(Fs, floating(text(",")), reset_check_bracket(reset_prec(Ctxt)),
             fun lay_field_type/2),
    lay_elems(fun wrangler_prettypr_0:par/1,Ds, Fs, Ctxt).

  
lay_field_type({type,_Line,field_type,[Name,Type]}, Ctxt) ->
    lay_field_type_1(lay(Name, Ctxt), Type, Ctxt).


lay_field_type_1(B, {type,_,union,Ts}, Ctxt) ->
    Ds = lay_union_type(Ts, Ctxt),
    Ds1=par(Ds, Ctxt#ctxt.break_indent),
    wrangler_prettypr_0:horizontal([B, text("::"), Ds1]);
lay_field_type_1(B, Type, Ctxt) ->
    D = lay_type(Type, Ctxt),
    wrangler_prettypr_0:horizontal([B, text("::"), D]).


lay_union_type([T|Ts], Ctxt) ->
    [lay_type(T, Ctxt) | [lay_union_elem(T1, Ctxt)||T1<-Ts]].

lay_union_elem(T, Ctxt) ->
    beside(text(" | "),lay_type(T, Ctxt)).

spec_need_paren([])->true;
spec_need_paren(Toks) ->
    [_T, T1| _Ts] = lists:dropwhile(fun({atom, _, 'spec'}) ->
                                           false;
                                       (_) ->
                                            true
                                   end, Toks),
    element(1, T1) == '('.
    
