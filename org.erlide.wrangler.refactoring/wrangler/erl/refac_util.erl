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
%% Some utility functions used by the refactorer.
%%
%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%%
%% =====================================================================

-module(refac_util).   

-export([parse_annotate_file/3, parse_annotate_file/4,
	 parse_annotate_file/5, quick_parse_annotate_file/3,
	 write_refactored_files/1, write_refactored_files/3,
	 write_refactored_files/4,
	 write_refactored_files_for_preview/2, expand_files/2,
	 get_client_files/2, file_format/1, tokenize/3,
	 concat_toks/1, test_framework_used/1,
	 get_modules_by_file/1]).

-include("../include/wrangler.hrl").

%% =====================================================================
%% @doc Pretty-print the abstract syntax trees to a files, and add the previous 
%% version to history for undo purpose. <code>Files</code> is a list of three element 
%% tuples. The first element in the tuple is the original file name, the second element 
%% is the new file name if the filename has been changed by the refactoring, otherwise it 
%% should be the same as the first element, and the third element in the tuple is the 
%% AST represention of the file.

%% Note: This function should not longer be used.

-spec(write_refactored_files([{{filename(),filename()},syntaxTree()}]) -> 'ok').
write_refactored_files(Files) ->
    F = fun ({{File1, File2}, AST}) ->
		FileFormat = file_format(File1),
		if File1 /= File2 ->
		       file:delete(File1);
		   true -> ok
		end,
		file:write_file(File2, list_to_binary(refac_prettypr:print_ast(FileFormat, AST)))
	end,
    Res =lists:map(F, Files),
    case lists:all(fun(R) -> R == ok end, Res) of 
	true -> ok;
	_ -> throw({error, "Wrangler failed to rewrite the refactored files."})
    end.

write_refactored_files_for_preview(Files, LogMsg) ->
    F = fun(FileAST) ->
		case FileAST of 
		    {{FileName,NewFileName}, AST} ->
			FileFormat = file_format(FileName),
			SwpFileName = filename:rootname(FileName, ".erl") ++ ".erl.swp",  %% .erl.swp or .swp.erl?
			case file:write_file(SwpFileName, list_to_binary(refac_prettypr:print_ast(FileFormat, AST))) of 
			    ok -> {{FileName,NewFileName, false},SwpFileName};
			    {error,Reason} -> Msg = io_lib:format("Wrangler could not write to directory ~s: ~w \n",
								  [filename:dirname(FileName), Reason]),
					      throw({error, Msg})
			end;			
		    {{FileName,NewFileName, IsNew}, AST} ->
			FileFormat = file_format(FileName),
			SwpFileName = filename:rootname(FileName, ".erl") ++ ".erl.swp", 
			case file:write_file(SwpFileName, list_to_binary(refac_prettypr:print_ast(FileFormat, AST))) of 
			    ok -> {{FileName,NewFileName, IsNew},SwpFileName};
			    {error, Reason}  -> 
				Msg = io_lib:format("Wrangler could not write to directory ~s: ~w \n",
						    [filename:dirname(FileName), Reason]),
				throw({error, Msg})
			end
		end
	end,
    FilePairs = lists:map(F, Files),
    case lists:any(fun(R) -> R == error end, FilePairs) of 
	true -> lists:foreach(fun(P) ->
				      case P of 
					  error -> ok;
					  {_,SwpF} -> file:delete(SwpF)
				      end
			      end, FilePairs),
		throw({error, "Wrangler failed to output the refactoring result."});
	_ -> wrangler_preview_server:add_files({FilePairs, LogMsg})
    end.


write_refactored_files(FileName, AnnAST, Editor, Cmd) ->
    case Editor of
      emacs ->
	  refac_util:write_refactored_files_for_preview([{{FileName, FileName}, AnnAST}], Cmd),
	  {ok, [FileName]};
      eclipse ->
	  Content = refac_prettypr:print_ast(refac_util:file_format(FileName), AnnAST),
	  {ok, [{FileName, FileName, Content}]}
    end.

write_refactored_files(Results, Editor, Cmd) ->
    case Editor of
      emacs ->
	  refac_util:write_refactored_files_for_preview(Results, Cmd),
	  ChangedFiles = lists:map(fun ({{F, _F}, _AST}) -> F end, Results),
	  ?wrangler_io("The following files are to be changed by this refactoring:\n~p\n",
		       [ChangedFiles]),
	  {ok, ChangedFiles};
      eclipse ->
	  Res = lists:map(fun ({{OldFName, NewFName}, AST}) ->
				  {OldFName, NewFName,
				   refac_prettypr:print_ast(refac_util:file_format(OldFName), AST)}
			  end, Results),
	  {ok, Res}
    end.


%% =====================================================================
%% @doc Parse an Erlang file, and annotate the abstract syntax tree with static semantic 
%% information. As to the parameters, FName is the name of the file to parse;  ByPassPreP 
%% is a bool value, and 'true' means to use the parse defined in refac_epp_dodger 
%% (which does not expand macros), 'false' means to use the parse defined in refac_epp
%% (which expands macros); SeachPaths is the list of directories to search for related 
%% Erlang files. 
%% The following annotations are added to the AST generated by the parser.
%% <ul>
%%     <li> <code> {env, [Var]}</code>, representing the input enrironment of 
%%     the subtree. </li>
%%
%%     <li> <code> {bound, [Var]} </code>, representing the variables that are 
%%      bound in the subtree. </li>
%%
%%     <li> <code> {free, [Var]}</code>, representing the free variables in the 
%%     subtree </li>
%%   
%%     <li> <code> {range, {Pos, Pos}} </code>, representing the start and end location 
%%     of subtree in the program source. </li>
%%    
%%     <li> <code> {category, atom()} </code>, representing the kind of the syntex phrase 
%%      represented by the subtree. </li>
%%
%%     <li> <code> {def, [Pos]} </code>, representing the defining positions of the variable 
%%     represented by the subtree (only when the subtree does represent a variable). </li>
%%
%%     <li> <code> {fun_def, {Mod, FunName, Arity, Pos, Pos}} </code>, representing the binding 
%%     information of the function represented by the subtree (only when the subtree
%%     represents a function definition, a function application, or an arity qualifier).
%%      </li>
%% </ul>
%%  <code>Var</code>  is a two-element tuple whose first element is an atom representing 
%%   the variable name, second element representing the variable's defining position. 
%%
%% @type syntaxTree(). An abstract syntax tree. The <code>erl_parse</code> "parse tree" 
%%  representation is a subset of the <code>syntaxTree()</code> representation.
%% 
%%  For the data structures used by the AST nodes, please refer to <a href="refac_syntax.html"> refac_syntax </a>.

-spec(parse_annotate_file(FName::filename(), ByPassPreP::boolean(), SearchPaths::[dir()])
                           -> {ok, {syntaxTree(), moduleInfo()}}).
parse_annotate_file(FName, ByPassPreP, SearchPaths) ->
    parse_annotate_file(FName, ByPassPreP, SearchPaths, ?DEFAULT_TABWIDTH).

-spec(parse_annotate_file(FName::filename(), ByPassPreP::boolean(), SearchPaths::[dir()], TabWidth::integer())
      -> {ok, {syntaxTree(), moduleInfo()}}).
parse_annotate_file(FName, ByPassPreP, SearchPaths, TabWidth) ->
    FileFormat = file_format(FName),
    case whereis(wrangler_ast_server) of
      undefined ->        %% this should not happen with Wrangler + Emacs.
	  ?wrangler_io("wrangler_ast_aserver is not defined\n", []),
	  parse_annotate_file(FName, ByPassPreP, SearchPaths, TabWidth, FileFormat);
      _ ->
	  wrangler_ast_server:get_ast({FName, ByPassPreP, SearchPaths, TabWidth, FileFormat})
    end.
     


-spec(parse_annotate_file(FName::filename(), ByPassPreP::boolean(), SearchPaths::[dir()], integer(), atom())
      -> {ok, {syntaxTree(), moduleInfo()}}).
parse_annotate_file(FName, true, SearchPaths, TabWidth, FileFormat) ->
    case refac_epp_dodger:parse_file(FName, [{tab, TabWidth}, {format, FileFormat}]) of
      {ok, Forms} ->
	  Dir = filename:dirname(FName),
	  DefaultIncl2 = [filename:join(Dir, X) || X <- refac_misc:default_incls()],
	  Includes = SearchPaths ++ DefaultIncl2,
	  {Info0, Ms} = case refac_epp:parse_file(FName, Includes, [], TabWidth, FileFormat) of
			  {ok, Fs, {MDefs, MUses}} ->
			      ST = refac_recomment:recomment_forms(Fs, []),
			      Info1 = refac_syntax_lib:analyze_forms(ST),
			      Ms1 = {dict:from_list(MDefs), dict:from_list(MUses)},
			      {Info1, Ms1};
			  _ -> {[], {dict:from_list([]), dict:from_list([])}}
			end,
	  Comments = refac_comment_scan:file(FName),
	  SyntaxTree = refac_recomment:recomment_forms(Forms, Comments),
	  Info = refac_syntax_lib:analyze_forms(SyntaxTree),
	  Info2 = merge_module_info(Info0, Info),
	  AnnAST0 = annotate_bindings(FName, SyntaxTree, Info2, Ms, TabWidth),
	  AnnAST = refac_type_annotation:type_ann_ast(FName, Info2, AnnAST0, SearchPaths, TabWidth),
	  {ok, {AnnAST, Info2}};
      {error, Reason} -> erlang:error(Reason)
    end;
parse_annotate_file(FName, false, SearchPaths, TabWidth, FileFormat) ->
    Dir = filename:dirname(FName),
    DefaultIncl2 = [filename:join(Dir, X) || X <- refac_misc:default_incls()],
    Includes = SearchPaths ++ DefaultIncl2,
    case refac_epp:parse_file(FName, Includes, [], TabWidth, FileFormat) of
      {ok, Forms, Ms} -> Forms1 = lists:filter(fun (F) ->
						       case F of
							 {attribute, _, file, _} -> false;
							 {attribute, _, type, {{record, _}, _, _}} -> false;
							 _ -> true
						       end
					       end, Forms),
			 %% I wonder whether the all the following is needed;
			 %% we should never perform a transformation on an AnnAST from resulted from refac_epp;
			 SyntaxTree = refac_recomment:recomment_forms(Forms1, []),
			 Info = refac_syntax_lib:analyze_forms(SyntaxTree),
			 AnnAST0 = annotate_bindings(FName, SyntaxTree, Info, Ms, TabWidth),
			 {ok, {AnnAST0, Info}};
      {error, Reason} -> erlang:error(Reason)
    end.


quick_parse_annotate_file(FName, SearchPaths, TabWidth) ->
    FileFormat = file_format(FName),
    case refac_epp_dodger:parse_file(FName, [{tab, TabWidth}, {format, FileFormat}]) of
      {ok, Forms} ->
	  Dir = filename:dirname(FName),
	  DefaultIncl2 = [filename:join(Dir, X) || X <- refac_misc:default_incls()],
	  Includes = SearchPaths ++ DefaultIncl2,
	  Ms = case refac_epp:parse_file(FName, Includes, [], TabWidth, FileFormat) of
		 {ok, _, {MDefs, MUses}} ->
		     {dict:from_list(MDefs), dict:from_list(MUses)};
		 _ -> []
	       end,
	  SyntaxTree = refac_recomment:recomment_forms(Forms, []),
	  Info = refac_syntax_lib:analyze_forms(SyntaxTree),
	  AnnAST = annotate_bindings(FName, SyntaxTree, Info, Ms, TabWidth),
	  {ok, {AnnAST, Info}};
      {error, Reason} -> erlang:error(Reason)
    end.

%% =====================================================================
-spec(tokenize(File::filename(), WithLayout::boolean(), TabWidth::integer()) -> [token()]).
tokenize(File, WithLayout, TabWidth) ->
    {ok, Bin} = file:read_file(File),
    S = erlang:binary_to_list(Bin),
    case WithLayout of 
	true -> 
	    {ok, Ts, _} = refac_scan_with_layout:string(S, {1,1}, TabWidth, file_format(File)),
	    Ts;
	_ -> {ok, Ts, _} = refac_scan:string(S, {1,1}, TabWidth,file_format(File)),
	     Ts
    end.

merge_module_info(Info1, Info2) ->
    Info = lists:usort(Info1 ++ Info2),
    F = fun(Attr) ->
		lists:usort(lists:append(
			      [Vs||{Attr1,Vs} <- Info, 
				   Attr1==Attr]))
	end,
    M = case lists:keysearch(module, 1, Info) of
		 {value, R} ->
		     R;
		 _ -> {module, []}
	     end,
    NewInfo=[M, {exports,F(exports)}, {module_imports, F(module_imports)},
	     {imports, F(imports)}, {attributes,F(attributes)},
	     {records, F(records)}, {errors, F(errors)}, {warnings, F(warnings)},
	     {functions, F(functions)}, {rules, F(rules)}],
    [{A,V}||{A, V}<-NewInfo, V=/=[]].
	     
  
annotate_bindings(FName, AST, Info, Ms, TabWidth) ->
    Toks = tokenize(FName, true, TabWidth),
    AnnAST0 = refac_syntax_lib:annotate_bindings(add_tokens(add_range(AST, Toks), Toks), ordsets:new(), Ms),
    add_category(refac_annotate_ast:add_fun_define_locations(AnnAST0, Info)).
 

%% Attach tokens to each form in the AST.
-spec add_tokens(syntaxTree(), [token()]) -> syntaxTree(). 		 
add_tokens(SyntaxTree, Toks) ->
    Fs = refac_syntax:form_list_elements(SyntaxTree),
    rewrite(SyntaxTree, refac_syntax:form_list(do_add_tokens(Toks, Fs))).

do_add_tokens(Toks, Fs) ->
    do_add_tokens(Toks, lists:reverse(Fs), []).

do_add_tokens(_, [], NewFs) ->
    NewFs;
do_add_tokens(Toks, [F| Fs], NewFs) ->
    {StartPos, RemFs} =
	case refac_syntax:type(F) of
	  error_marker ->
	      case Fs of
		[] -> {{1, 1}, []};
		_ -> Fs1 = lists:dropwhile(fun (F1) ->
						   lists:member(refac_syntax:type(F1), [comment, error_marker])
					   end, Fs),
		     case Fs1 of
		       [] ->
			   {{1, 1}, []};
		       _ -> {_, {Line, _}} = get_range(hd(Fs1)),
			    {{Line + 1, 1}, Fs1}
		     end
	      end;
	  _ -> case refac_syntax:get_precomments(F) of
		 [] -> {Start, _End} = get_range(F),
		       {Start, Fs};
		 [Com| _Tl] -> {refac_syntax:get_pos(Com), Fs}
	       end
	end,
    {Toks1, Toks2} = lists:splitwith(fun (T) ->
					     element(2, T) < StartPos
				     end, Toks),
    {Toks11, Toks12} = lists:splitwith(fun (T) ->
					       element(1, T) == whitespace
				       end, lists:reverse(Toks1)),
    {FormToks, RemainToks} =
	case Toks12 of
	  [] -> {Toks, []};
	  [H| _T] -> Line1 = element(1, element(2, H)),
		     {Toks13, Toks14} = lists:splitwith(fun (T) ->
								element(1, element(2, T)) == Line1
							end, lists:reverse(Toks11)),
		     {Toks14 ++ Toks2, lists:reverse(Toks12) ++ Toks13}
	end,
    F1 = refac_syntax:add_ann({toks, FormToks}, F),
    do_add_tokens(RemainToks, RemFs, [F1| NewFs]).


-spec add_range(syntaxTree(), [token()]) -> syntaxTree(). 
add_range(AST, Toks) ->
    ast_traverse_api:full_buTP(fun do_add_range/2, AST, Toks).
do_add_range(Node, Toks) ->
    
    {L, C} = case refac_syntax:get_pos(Node) of
	       {Line, Col} -> {Line, Col};
	       Line -> {Line, 0}
	     end,
    case refac_syntax:type(Node) of
      variable ->
	  Len = length(refac_syntax:variable_literal(Node)),
	  refac_syntax:add_ann({range, {{L, C}, {L, C + Len - 1}}}, Node);
      atom ->
	  Lit = refac_syntax:atom_literal(Node),
	  case hd(Lit) of
	    39 -> Toks1 = lists:dropwhile(fun (T) -> token_loc(T) =< {L, C} end, Toks),
		  case Toks1   %% this should not happen;
		      of
		    [] -> Len = length(atom_to_list(refac_syntax:atom_value(Node))),  %% This is problematic!!
			  refac_syntax:add_ann({range, {{L, C}, {L, C + Len - 1}}}, Node);
		    _ -> {L2, C2} = token_loc(hd(Toks1)),
			 refac_syntax:add_ann({range, {{L, C}, {L2, C2 - 1}}}, Node)
		  end;
	    _ -> Len = length(atom_to_list(refac_syntax:atom_value(Node))),  %% This is problematic!!
		 refac_syntax:add_ann({range, {{L, C}, {L, C + Len - 1}}}, Node)
	  end;
      operator ->
	  Len = length(atom_to_list(refac_syntax:atom_value(Node))),
	  refac_syntax:add_ann({range, {{L, C}, {L, C + Len - 1}}}, Node);
      char -> refac_syntax:add_ann({range, {{L, C}, {L, C}}}, Node);
      integer ->
	  Len = length(refac_syntax:integer_literal(Node)),
	  refac_syntax:add_ann({range, {{L, C}, {L, C + Len - 1}}}, Node);
      string ->
	  Toks1 = lists:dropwhile(fun (T) -> token_loc(T) < {L, C} end, Toks),
	  {Toks21, Toks22} = lists:splitwith(fun (T) -> is_string(T) orelse is_whitespace_or_comment(T) end, Toks1),
	  Toks3 = lists:filter(fun (T) -> is_string(T) end, Toks21),
	  case Toks3 of
	    [] ->
		Len = length(refac_syntax:string_literal(Node)),
		Toks23 = lists:takewhile(fun (T) -> token_loc(T) < {L, C + Len - 1} end, Toks22),
		Toks31 = lists:filter(fun (T) -> is_string(T) end, Toks23),
		case Toks31 of
		  [] ->
		      refac_syntax:add_ann({range, {{L, C}, {L, C + Len - 1}}}, Node);
		  _ ->
		      Node1 = refac_syntax:add_ann({range, {{L, C}, {L, C + Len - 1}}}, Node),
		      refac_syntax:add_ann({toks, Toks31}, Node1)
		end;
	    _ -> Toks4 = lists:takewhile(fun (T) -> is_whitespace_or_comment(T) end, lists:reverse(Toks21)),
		 {L3, C3} = case Toks4 of
			      [] -> token_loc(hd(Toks22));
			      _ -> token_loc(lists:last(Toks4))
			    end,
		 R = {token_loc(hd(Toks21)), {L3, C3 - 1}},
		 Node1 = refac_syntax:add_ann({range, R}, Node),
		 refac_syntax:add_ann({toks, Toks3}, Node1)
	  end;
      float ->
	  refac_syntax:add_ann({range, {{L, C}, {L, C}}}, Node); %% This is problematic.
      underscore -> refac_syntax:add_ann({range, {{L, C}, {L, C}}}, Node);
      eof_marker -> refac_syntax:add_ann({range, {{L, C}, {L, C}}}, Node);
      nil -> refac_syntax:add_ann({range, {{L, C}, {L, C + 1}}}, Node);
      module_qualifier ->
	  M = refac_syntax:module_qualifier_argument(Node),
	  F = refac_syntax:module_qualifier_body(Node),
	  {S1, _E1} = get_range(M),
	  {_S2, E2} = get_range(F),
	  refac_syntax:add_ann({range, {S1, E2}}, Node);
      list ->
	  LP = refac_misc:ghead("refac_util:do_add_range,list", refac_syntax:list_prefix(Node)),
	  {{L1, C1}, {L2, C2}} = get_range(LP),
	  case refac_syntax:list_suffix(Node) of
	    none -> refac_syntax:add_ann({range, {{L1, C1 - 1}, {L2, C2 + 1}}}, Node);
	    Tail -> {_S2, {L3, C3}} = get_range(Tail), refac_syntax:add_ann({range, {{L1, C1 - 1}, {L3, C3}}}, Node)
	  end;
      application ->
	  O = refac_syntax:application_operator(Node),
	  Args = refac_syntax:application_arguments(Node),
	  {S1, E1} = get_range(O),
	  {S3, E3} = case Args of
		       [] -> {S1, E1};
		       _ -> La = refac_misc:glast("refac_util:do_add_range, application", Args),
			    {_S2, E2} = get_range(La),
			    {S1, E2}
		     end,
	  E31 = extend_backwards(Toks, E3, ')'),
	  refac_syntax:add_ann({range, {S3, E31}}, Node);
      case_expr ->
	  A = refac_syntax:case_expr_argument(Node),
	  Lc = refac_misc:glast("refac_util:do_add_range,case_expr", refac_syntax:case_expr_clauses(Node)),
	  {S1, _E1} = get_range(A),
	  {_S2, E2} = get_range(Lc),
	  S11 = extend_forwards(Toks, S1, 'case'),
	  E21 = extend_backwards(Toks, E2, 'end'),
	  refac_syntax:add_ann({range, {S11, E21}}, Node);
      clause ->
	  P = refac_syntax:get_pos(Node),
	  Body = refac_misc:glast("refac_util:do_add_range, clause", refac_syntax:clause_body(Node)),
	  {_S2, E2} = get_range(Body),
	  refac_syntax:add_ann({range, {P, E2}}, Node);
      catch_expr ->
	  B = refac_syntax:catch_expr_body(Node),
	  {S, E} = get_range(B),
	  S1 = extend_forwards(Toks, S, 'catch'),
	  refac_syntax:add_ann({range, {S1, E}}, Node);
      if_expr ->
	  Cs = refac_syntax:if_expr_clauses(Node),
	  add_range_to_list_node(Node, Toks, Cs, "refac_util:do_add_range, if_expr",
				 "refac_util:do_add_range, if_expr", 'if', 'end');
      cond_expr ->
	  Cs = refac_syntax:cond_expr_clauses(Node),
	  add_range_to_list_node(Node, Toks, Cs, "refac_util:do_add_range, cond_expr",
				 "refac_util:do_add_range, cond_expr", 'cond', 'end');
      infix_expr ->
	  Left = refac_syntax:infix_expr_left(Node),
	  Right = refac_syntax:infix_expr_right(Node),
	  {S1, _E1} = get_range(Left),
	  {_S2, E2} = get_range(Right),
	  refac_syntax:add_ann({range, {S1, E2}}, Node);
      prefix_expr ->
	  Op = refac_syntax:prefix_expr_operator(Node),
	  Ar = refac_syntax:prefix_expr_argument(Node),
	  {S1, _E1} = get_range(Op),
	  {_S2, E2} = get_range(Ar),
	  %% E21 = extend_backwards(Toks, E2, ')'),  %% the parser should keey the parathesis!
	  refac_syntax:add_ann({range, {S1, E2}}, Node);
      conjunction ->
	  B = refac_syntax:conjunction_body(Node),
	  add_range_to_body(Node, B, "refac_util:do_add_range,conjunction",
			    "refac_util:do_add_range,conjunction");
      disjunction ->
	  B = refac_syntax:disjunction_body(Node),
	  add_range_to_body(Node, B, "refac_util:do_add_range, disjunction",
			    "refac_util:do_add_range,disjunction");
      function ->
	  F = refac_syntax:function_name(Node),
	  Cs = refac_syntax:function_clauses(Node),
	  Lc = refac_misc:glast("refac_util:do_add_range,function", Cs),
	  {S1, _E1} = get_range(F),
	  {_S2, E2} = get_range(Lc),
	  refac_syntax:add_ann({range, {S1, E2}}, Node);
      fun_expr ->
	  Cs = refac_syntax:fun_expr_clauses(Node),
	  S = refac_syntax:get_pos(Node),
	  Lc = refac_misc:glast("refac_util:do_add_range, fun_expr", Cs),
	  {_S1, E1} = get_range(Lc),
	  E11 = extend_backwards(Toks, E1,
				 'end'),   %% S starts from 'fun', so there is no need to extend forwards/
	  refac_syntax:add_ann({range, {S, E11}}, Node);
      arity_qualifier ->
	  B = refac_syntax:arity_qualifier_body(Node),
	  A = refac_syntax:arity_qualifier_argument(Node),
	  {S1, _E1} = get_range(B),
	  {_S2, E2} = get_range(A),
	  refac_syntax:add_ann({range, {S1, E2}}, Node);
      implicit_fun ->
	  S = refac_syntax:get_pos(Node),
	  N = refac_syntax:implicit_fun_name(Node),
	  {_S1, E1} = get_range(N),
	  refac_syntax:add_ann({range, {S, E1}}, Node);
      attribute ->
	  Name = refac_syntax:attribute_name(Node),
	  Args = refac_syntax:attribute_arguments(Node),
	  case Args of
	    none -> {S1, E1} = get_range(Name),
		    S11 = extend_forwards(Toks, S1, '-'),
		    refac_syntax:add_ann({range, {S11, E1}}, Node);
	    _ -> case length(Args) > 0 of
		   true -> Arg = refac_misc:glast("refac_util:do_add_range,attribute", Args),
			   {S1, _E1} = get_range(Name),
			   {_S2, E2} = get_range(Arg),
			   S11 = extend_forwards(Toks, S1, '-'),
			   refac_syntax:add_ann({range, {S11, E2}}, Node);
		   _ -> {S1, E1} = get_range(Name),
			S11 = extend_forwards(Toks, S1, '-'),
			refac_syntax:add_ann({range, {S11, E1}}, Node)
		 end
	  end;
      generator ->
	  P = refac_syntax:generator_pattern(Node),
	  B = refac_syntax:generator_body(Node),
	  {S1, _E1} = get_range(P),
	  {_S2, E2} = get_range(B),
	  refac_syntax:add_ann({range, {S1, E2}}, Node);
      tuple ->
	  Es = refac_syntax:tuple_elements(Node),
	  case length(Es) of
	    0 -> refac_syntax:add_ann({range, {{L, C}, {L, C + 1}}}, Node);
	    _ ->
		add_range_to_list_node(Node, Toks, Es, "refac_util:do_add_range, tuple",
				       "refac_util:do_add_range, tuple",
				       '{', '}')
	  end;
      list_comp ->
	  T = refac_syntax:list_comp_template(Node),
	  B = refac_misc:glast("refac_util:do_add_range,list_comp", refac_syntax:list_comp_body(Node)),
	  {S1, _E1} = get_range(T),
	  {_S2, E2} = get_range(B),
	  S11 = extend_forwards(Toks, S1, '['),
	  E21 = extend_backwards(Toks, E2, ']'),
	  refac_syntax:add_ann({range, {S11, E21}}, Node);
      block_expr ->
	  Es = refac_syntax:block_expr_body(Node),
	  add_range_to_list_node(Node, Toks, Es, "refac_util:do_add_range, block_expr",
				 "refac_util:do_add_range, block_expr", 'begin', 'end');
      receive_expr ->
	  case refac_syntax:receive_expr_timeout(Node) of
	    none ->
		Cs = refac_syntax:receive_expr_clauses(Node),
		case length(Cs) of
		  0 -> refac_syntax:add_ann({range, {L, C}, {L, C}}, Node);
		  _ ->
		      add_range_to_list_node(Node, Toks, Cs, "refac_util:do_add_range, receive_expr1",
					     "refac_util:do_add_range, receive_expr1", 'receive', 'end')
		end;
	    _E ->
		Cs = refac_syntax:receive_expr_clauses(Node),
		A = refac_syntax:receive_expr_action(Node),
		case length(Cs) of
		  0 ->
		      {_S2, E2} = get_range(refac_misc:glast("refac_util:do_add_range, receive_expr2", A)),
		      refac_syntax:add_ann({range, {{L, C}, E2}}, Node);
		  _ ->
		      Hd = refac_misc:ghead("refac_util:do_add_range,receive_expr2", Cs),
		      {S1, _E1} = get_range(Hd),
		      {_S2, E2} = get_range(refac_misc:glast("refac_util:do_add_range, receive_expr3", A)),
		      S11 = extend_forwards(Toks, S1, 'receive'),
		      E21 = extend_backwards(Toks, E2, 'end'),
		      refac_syntax:add_ann({range, {S11, E21}}, Node)
		end
	  end;
      try_expr ->
	  B = refac_syntax:try_expr_body(Node),
	  After = refac_syntax:try_expr_after(Node),
	  {S1, _E1} = get_range(refac_misc:ghead("refac_util:do_add_range, try_expr", B)),
	  {_S2, E2} = case After of
			[] ->
			    Handlers = refac_syntax:try_expr_handlers(Node),
			    get_range(refac_misc:glast("refac_util:do_add_range, try_expr", Handlers));
			_ ->
			    get_range(refac_misc:glast("refac_util:do_add_range, try_expr", After))
		      end,
	  S11 = extend_forwards(Toks, S1, 'try'),
	  E21 = extend_backwards(Toks, E2, 'end'),
	  refac_syntax:add_ann({range, {S11, E21}}, Node);
      binary ->
	  Fs = refac_syntax:binary_fields(Node),
	  case Fs == [] of
	    true -> refac_syntax:add_ann({range, {{L, C}, {L, C + 3}}}, Node);
	    _ -> %% this should be changed when the parser is able 
		%% to include location info for binary type qualifiers.
		Hd = refac_misc:ghead("do_add_range:binary", Fs),
		{S1, _E1} = get_range(Hd),
		S11 = extend_forwards(Toks, S1, "<<"),
		E21 = extend_backwards(Toks, S1, ">>"),
		refac_syntax:add_ann({range, {S11, E21}}, Node)
	  end;
      binary_field ->
	  Body = refac_syntax:binary_field_body(Node),
	  Types = refac_syntax:binary_field_types(Node),
	  {S1, E1} = get_range(Body),
	  {_S2, E2} = if Types == [] -> {S1, E1};
			 true -> get_range(refac_misc:glast("refac_util:do_add_range,binary_field", Types))
		      end,
	  case E2 > E1  %%Temporal fix; need to change refac_syntax to make the pos info correct.
	      of
	    true ->
		refac_syntax:add_ann({range, {S1, E2}}, Node);
	    false ->
		refac_syntax:add_ann({range, {S1, E1}}, Node)
	  end;
      match_expr ->
	  P = refac_syntax:match_expr_pattern(Node),
	  B = refac_syntax:match_expr_body(Node),
	  {S1, _E1} = get_range(P),
	  {_S2, E2} = get_range(B),
	  refac_syntax:add_ann({range, {S1, E2}}, Node);
      form_list ->
	  Es = refac_syntax:form_list_elements(Node),
	  
	  add_range_to_body(Node, Es, "refac_util:do_add_range, form_list",
			    "refac_util:do_add_range, form_list");
      parentheses ->
	  B = refac_syntax:parentheses_body(Node),
	  {S, E} = get_range(B),
	  S1 = extend_forwards(Toks, S, '('),
	  E1 = extend_backwards(Toks, E, ')'),
	  refac_syntax:add_ann({range, {S1, E1}}, Node);
      class_qualifier ->
	  A = refac_syntax:class_qualifier_argument(Node),
	  B = refac_syntax:class_qualifier_body(Node),
	  {S1, _E1} = get_range(A),
	  {_S2, E2} = get_range(B),
	  refac_syntax:add_ann({range, {S1, E2}}, Node);
      qualified_name ->
	  Es = refac_syntax:qualified_name_segments(Node),
	  
	  add_range_to_body(Node, Es, "refac_util:do_add_range, qualified_name",
			    "refac_util:do_add_range, qualified_name");
      query_expr ->
	  B = refac_syntax:query_expr_body(Node),
	  {S, E} = get_range(B),
	  refac_syntax:add_ann({range, {S, E}}, Node);
      record_field ->
	  Name = refac_syntax:record_field_name(Node),
	  {S1, E1} = get_range(Name),
	  Value = refac_syntax:record_field_value(Node),
	  case Value of
	    none -> refac_syntax:add_ann({range, {S1, E1}}, Node);
	    _ -> {_S2, E2} = get_range(Value), refac_syntax:add_ann({range, {S1, E2}}, Node)
	  end;
      typed_record_field ->   %% This is not correct; need to be fixed later!
	  Field = refac_syntax:typed_record_field(Node),
	  {S1, _E1} = get_range(Field),
	  Type = refac_syntax:typed_record_type(Node),
	  {_S2, E2} = get_range(Type),
	  refac_syntax:add_ann({range, {S1, E2}}, Node);
      record_expr ->
	  Arg = refac_syntax:record_expr_argument(Node),
	  Type = refac_syntax:record_expr_type(Node),
	  Fields = refac_syntax:record_expr_fields(Node),
	  {S1, E1} = case Arg of
		       none -> get_range(Type);
		       _ -> get_range(Arg)
		     end,
	  case Fields of
	    [] -> E11 = extend_backwards(Toks, E1, '}'),
		  refac_syntax:add_ann({range, {S1, E11}}, Node);
	    _ ->
		{_S2, E2} = get_range(refac_misc:glast("refac_util:do_add_range,record_expr", Fields)),
		E21 = extend_backwards(Toks, E2, '}'),
		refac_syntax:add_ann({range, {S1, E21}}, Node)
	  end;
      record_access ->
	  Arg = refac_syntax:record_access_argument(Node),
	  Field = refac_syntax:record_access_field(Node),
	  {S1, _E1} = get_range(Arg),
	  {_S2, E2} = get_range(Field),
	  refac_syntax:add_ann({range, {S1, E2}}, Node);
      record_index_expr ->
	  Type = refac_syntax:record_index_expr_type(Node),
	  Field = refac_syntax:record_index_expr_field(Node),
	  {S1, _E1} = get_range(Type),
	  {_S2, E2} = get_range(Field),
	  refac_syntax:add_ann({range, {S1, E2}}, Node);
      comment ->
	  T = refac_syntax:comment_text(Node),
	  Lines = length(T),
	  refac_syntax:add_ann({range, {{L, C}, {L + Lines - 1,
						 length(refac_misc:glast("refac_util:do_add_range,comment", T))}}},
			       Node);
      macro ->
	  Name = refac_syntax:macro_name(Node),
	  Args = refac_syntax:macro_arguments(Node),
	  {_S1, E1} = get_range(Name),
	  case Args of
	    none -> refac_syntax:add_ann({range, {{L, C}, E1}}, Node);
	    Ls ->
		case Ls of
		  [] -> E21 = extend_backwards(Toks, E1, ')'),
			refac_syntax:add_ann({range, {{L, C}, E21}}, Node);
		  _ ->
		      La = refac_misc:glast("refac_util:do_add_range,macro", Ls),
		      {_S2, E2} = get_range(La),
		      E21 = extend_backwards(Toks, E2, ')'),
		      refac_syntax:add_ann({range, {{L, C}, E21}}, Node)
		end
	  end;
      size_qualifier ->
	  Body = refac_syntax:size_qualifier_body(Node),
	  Arg = refac_syntax:size_qualifier_argument(Node),
	  {S1, _E1} = get_range(Body),
	  {_S2, E2} = get_range(Arg),
	  refac_syntax:add_ann({range, {S1, E2}}, Node);
      error_marker ->
	  refac_syntax:add_ann({range, {{L, C}, {L, C}}}, Node);
      type ->   %% This is not correct, and need to be fixed!!
	  refac_syntax:add_ann({range, {{L, C}, {L, C}}}, Node);
      _ ->
	  ?wrangler_io("Unhandled syntax category:\n~p\n", [refac_syntax:type(Node)]),
	  Node
    end.


get_range(Node) ->
     As = refac_syntax:get_ann(Node),
     case lists:keysearch(range, 1, As) of
       {value, {range, {S, E}}} -> {S, E};
       _ -> {?DEFAULT_LOC,
 	   ?DEFAULT_LOC} 
     end.


add_range_to_list_node(Node, Toks, Es, Str1, Str2, KeyWord1, KeyWord2) ->
    Hd = refac_misc:ghead(Str1, Es),
    La = refac_misc:glast(Str2, Es),
    {S1, _E1} = get_range(Hd),
    {_S2, E2} = get_range(La),
    S11 = extend_forwards(Toks, S1, KeyWord1),
    E21 = extend_backwards(Toks, E2, KeyWord2),
    refac_syntax:add_ann({range, {S11, E21}}, Node).


add_range_to_body(Node, B, Str1, Str2) ->
    H = refac_misc:ghead(Str1, B),
    La = refac_misc:glast(Str2, B),
    {S1, _E1} = get_range(H),
    {_S2, E2} = get_range(La),
    refac_syntax:add_ann({range, {S1, E2}}, Node).
   
extend_forwards(Toks, StartLoc, Val) ->
    Toks1 = lists:takewhile(fun (T) -> token_loc(T) < StartLoc end, Toks),
    Toks2 = lists:dropwhile(fun (T) -> token_val(T) =/= Val end, lists:reverse(Toks1)),
    case Toks2 of
      [] -> StartLoc;
      _ -> token_loc(hd(Toks2))
    end.

extend_backwards(Toks, EndLoc, Val) ->
    Toks1 = lists:dropwhile(fun (T) -> token_loc(T) =< EndLoc end, Toks),
    Toks2 = lists:dropwhile(fun (T) -> token_val(T) =/= Val end, Toks1),
    case Toks2 of
      [] -> EndLoc;
      _ ->
	  {Ln, Col} = token_loc(hd(Toks2)),
	  {Ln, Col + length(atom_to_list(Val)) - 1}
    end.

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

	
is_whitespace_or_comment({whitespace, _, _}) ->
    true;
is_whitespace_or_comment({comment, _, _}) ->
    true;
is_whitespace_or_comment(_) -> false.
	
    
is_string({string, _, _}) ->
    true;
is_string(_) -> false.


%% =====================================================================
% @doc Attach syntax category information to AST nodes.
%% =====================================================================

-spec(add_category(Node::syntaxTree()) -> syntaxTree()).
add_category(Node) ->
    case refac_syntax:type(Node) of
      form_list ->
	  Es = refac_syntax:form_list_elements(Node),
	  Es1 = lists:map(fun (E) -> add_category(E) end, Es),
	  Node1 = rewrite(Node, refac_syntax:form_list(Es1)),
	  refac_syntax:add_ann({category, form_list}, Node1);
      attribute -> add_category(Node, attribute);
      function -> add_category(Node, function);
      rule -> add_category(Node, rule);
      error_marker -> add_category(Node, error_marker);
      warning_marker -> add_category(Node, warning_marker);
      eof_marker -> add_category(Node, eof_marker);
      comment -> add_category(Node, comment);
      %%  macro -> add_category(Node, macro);
      _ -> add_category(Node, unknown)
    end.

add_category(Node, C) -> {Node1, _} = ast_traverse_api:stop_tdTP(fun do_add_category/2, Node, C),
			 Node1.
do_add_category(Node, C) ->
    if is_list(Node) -> {lists:map(fun (E) -> add_category(E, C) end, Node), true};
       true ->
	   case refac_syntax:type(Node) of
	     clause ->
		 B = refac_syntax:clause_body(Node),
		 P = refac_syntax:clause_patterns(Node),
		 G = refac_syntax:clause_guard(Node),
		 B1 = add_category(B, expression),
		 P1 = add_category(P, pattern),
		 G1 = case G of
			none -> none;
			_ -> add_category(G, guard_expression)
		      end,
		 Node1 = rewrite(Node, refac_syntax:clause(P1, G1, B1)),
		 {refac_syntax:add_ann({category, clause}, Node1), true};
	     match_expr ->
		 P = refac_syntax:match_expr_pattern(Node),
		 B = refac_syntax:match_expr_body(Node),
		 P1 = add_category(P, pattern),
		 B1 = add_category(B, C),
		 Node1 = rewrite(Node, refac_syntax:match_expr(P1, B1)),
		 {refac_syntax:add_ann({category, C}, Node1), true};
	     operator -> {refac_syntax:add_ann({category, operator}, Node), true}; %% added to fix bug 13/09/2008.
	     application ->
		 Op = refac_syntax:application_operator(Node),
		 Args = refac_syntax:application_arguments(Node),
		 Op1 = add_category(Op, application_op),
		 Args1 = add_category(Args, C),
		 Node1 = rewrite(Node, refac_syntax:application(Op1, Args1)),
		 {refac_syntax:add_ann({category, C}, Node1), true};
	     arity_qualifier ->
		 Fun = add_category(refac_syntax:arity_qualifier_body(Node), arity_qualifier),
		 A = add_category(refac_syntax:arity_qualifier_argument(Node), arity_qualifier),
		 Node1 = refac_syntax:arity_qualifier(Fun, A),
		 {refac_syntax:add_ann({category, C}, Node1), true};
	     macro ->
		 Name = refac_syntax:macro_name(Node),
		 Args = refac_syntax:macro_arguments(Node),
		 Name1 = add_category(Name, macro_name),
		 Args1 = case Args of
			   none -> none;
			   _ -> add_category(Args, expression) %% should 'expression' be 'macro_args'?
			 end,
		 Node1 = rewrite(Node, refac_syntax:macro(Name1, Args1)),
		 {refac_syntax:add_ann({category, C}, Node1), true};
	     record_access ->
		 Argument = refac_syntax:record_access_argument(Node),
		 Type = refac_syntax:record_access_type(Node),
		 Field = refac_syntax:record_access_field(Node),
		 Argument1 = add_category(Argument, C),
		 Type1 = case Type of
			   none -> none;
			   _ -> add_category(Type, record_type)
			 end,
		 Field1 = add_category(Field, record_field),
		 Node1 = rewrite(Node, refac_syntax:record_access(Argument1, Type1, Field1)),
		 {refac_syntax:add_ann({category, C}, Node1), true};
	     record_expr ->
		 Argument = refac_syntax:record_expr_argument(Node),
		 Type = refac_syntax:record_expr_type(Node),
		 Fields = refac_syntax:record_expr_fields(Node),
		 Argument1 = case Argument of
			       none -> none;
			       _ -> add_category(Argument, C)
			     end,
		 Type1 = add_category(Type, record_type),
		 Fields1 = add_category(Fields, C),
		 Node1 = rewrite(Node, refac_syntax:record_expr(Argument1, Type1, Fields1)),
		 {refac_syntax:add_ann({category, C}, Node1), true};
	     record_index_expr ->
		 Type = refac_syntax:record_index_expr_type(Node),
		 Field = refac_syntax:record_index_expr_field(Node),
		 Type1 = add_category(Type, record_type),
		 Field1 = add_category(Field, C),
		 Node1 = rewrite(Node, refac_syntax:record_index_expr(Type1, Field1)),
		 {refac_syntax:add_ann({category, C}, Node1), true};
	     record_field ->
		 Name = refac_syntax:record_field_name(Node),
		 Name1 = add_category(Name, record_field),
		 Value = refac_syntax:record_field_value(Node),
		 Value1 = case Value of
			    none -> none;
			    _ -> add_category(Value, C)
			  end,
		 Node1 = rewrite(Node, refac_syntax:record_field(Name1, Value1)),
		 {refac_syntax:add_ann({category, record_field}, Node1), true};
	     generator ->
		 P = refac_syntax:generator_pattern(Node),
		 B = refac_syntax:generator_body(Node),
		 P1 = add_category(P, pattern),
		 B1 = add_category(B, expression),
		 Node1 = rewrite(Node, refac_syntax:generator(P1, B1)),
		 {refac_syntax:add_ann({category, generator}, Node1), true};
	     attribute ->
		 case refac_syntax:atom_value(refac_syntax:attribute_name(Node)) of
		   define ->
		       Name = refac_syntax:attribute_name(Node),
		       Args = refac_syntax:attribute_arguments(Node),
		       MacroHead = refac_misc:ghead("Refac_util:do_add_category:MacroHead", Args),
		       MacroBody = tl(Args),
		       MacroHead1 = case refac_syntax:type(MacroHead) of
				      application ->
					  Operator = add_category(refac_syntax:application_operator(MacroHead), macro_name),
					  Arguments = add_category(refac_syntax:application_arguments(MacroHead), attribute),
					  rewrite(MacroHead, refac_syntax:application(Operator, Arguments));
				      _ -> add_category(MacroHead, macro_name)
				    end,
		       MacroBody1 = add_category(MacroBody, attribute),
		       Node1 = rewrite(Node, refac_syntax:attribute(Name, [MacroHead1| MacroBody1])),
		       {refac_syntax:add_ann({category, attribute}, Node1), true};
		   _ -> {refac_syntax:add_ann({category, C}, Node), false}
		 end;
	     %% TO ADD: other cases such as fields. Refer to the Erlang Specification.
	     binary_field -> {refac_syntax:add_ann({category, binary_field}, Node), false};
	     size_qualifier -> {refac_syntax:add_ann({category, size_qualifier}, Node), false};
	     _ -> {refac_syntax:add_ann({category, C}, Node), false}
	   end
    end.



rewrite(Tree, Tree1) ->
    refac_syntax:copy_attrs(Tree, Tree1).

%% =====================================================================
%% @doc Recursively collect all the files with the given file extension 
%%  in the specified directoris/files.

-spec(expand_files(FileDirs::[filename()|dir()], Ext::string()) -> [filename()]).
expand_files(FileDirs, Ext) ->
    expand_files(FileDirs, Ext, []).

expand_files([FileOrDir | Left], Ext, Acc) ->
    case filelib:is_dir(FileOrDir) of
      true ->
	  {ok, List} = file:list_dir(FileOrDir),
	  NewFiles = [filename:join(FileOrDir, X)
		      || X <- List, filelib:is_file(filename:join(FileOrDir, X)), filename:extension(X) == Ext],
	  NewDirs = [filename:join(FileOrDir, X) || X <- List, filelib:is_dir(filename:join(FileOrDir, X))],
	  expand_files(NewDirs ++ Left, Ext, NewFiles ++ Acc);
      false ->
	  case filelib:is_regular(FileOrDir) of
	    true ->
		case filename:extension(FileOrDir) == Ext of
		  true -> expand_files(Left, Ext, [FileOrDir | Acc]);
		  false -> expand_files(Left, Ext, Acc)
		end;
	    _ -> expand_files(Left, Ext, Acc)
	  end
    end;
expand_files([], _Ext, Acc) -> ordsets:from_list(Acc).


%%===============================================================================
%% @doc Return the list of files (Erlang modules) which make use of the functions 
%% defined in File.

-spec(get_client_files(File::filename(), SearchPaths::[dir()]) -> [filename()]).
get_client_files(File, SearchPaths) ->
    File1 = filename:absname(filename:join(filename:split(File))),
    ClientFiles = wrangler_modulegraph_server:get_client_files(File1, SearchPaths),
    case ClientFiles of
      [] ->
	  ?wrangler_io("\nWARNING: this module does not have "
		       "any client modules, please check the "
		       "search paths to ensure that this is "
		       "correct!\n", []);
      _ -> ok
    end,
    HeaderFiles = expand_files(SearchPaths, ".hrl"),
    ClientFiles ++ HeaderFiles.


%% =====================================================================
%% @doc The a list of files to a list of two-element tuples, with the first 
%% element of the tuple being the module name, and the second element 
%% binding the directory name of the file to which the module belongs.

-spec(get_modules_by_file(Files::[filename()]) -> [{atom(), dir()}]).
get_modules_by_file(Files) ->
    get_modules_by_file(Files, []).

get_modules_by_file([File | Left], Acc) ->
    BaseName = filename:basename(File, ".erl"),
    Dir = filename:dirname(File),
    get_modules_by_file(Left, [{list_to_atom(BaseName), Dir} | Acc]);
get_modules_by_file([], Acc) -> lists:reverse(Acc).


-spec file_format(filename()) ->dos|mac|unix. 		 
file_format(File) ->  
    {ok, Bin} = file:read_file(File),
    S = erlang:binary_to_list(Bin),
    LEs = scan_line_endings(S),
    case LEs of 
	[] -> unix;    %% default fileformat;
	_ ->  case lists:all(fun(E) -> E=="\r\n" end, LEs) of 
		  true -> dos;
		  _ -> case lists:all(fun(E) -> E=="\r" end, LEs)  of
			   true ->
			       mac;
			   _ -> case lists:all(fun(E)-> E=="\n" end, LEs) of
				    true -> unix;
				    _ -> throw({error, File ++ " uses a mixture of line endings,"
						" please normalise it to one of the standard file "
						"formats (i.e. unix/dos/mac) before performing any refactorings."})
				end
		       end
	      end
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

-spec test_framework_used(filename()) ->[atom()]. 			 
test_framework_used(FileName) ->
    case refac_epp_dodger:parse_file(FileName, []) of
      {ok, Forms} ->
	  Strs = lists:flatmap(fun (F) ->
				       case refac_syntax:type(F) of
					   attribute ->
					       Name = refac_syntax:attribute_name(F),
					       Args = refac_syntax:attribute_arguments(F),
					       case refac_syntax:type(Name) of
					       atom ->
						       AName = refac_syntax:atom_value(Name),
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
   

-spec(concat_toks(Toks::[token()]) ->string()).
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
