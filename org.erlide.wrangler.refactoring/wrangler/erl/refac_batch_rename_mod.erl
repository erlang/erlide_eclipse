%% Copyright (c) 2009, Huiqing Li, Simon Thompson
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
%% Refactoring: Rename a  collection of module names in batch mode.
%
%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%% 
%% =====================================================================
%%
%% @doc Rename a collect of module names in batch mode. 
%% <p> This refactoring is supposed to be run from the Erlang shell. For example, 
%% to rename all those module names which match the regular expression "foo_*" to 
%% "foo_*_1_0" in the directory <code> c:/distel/test </code>, just type the following command:
%% <code> refac_batch_rename_mod:batch_rename_mod("foo_*, "foo_*_1_0", ["c:/distel/test"]) </code>.
%% </p>
%% <p> This refactoring has a global effect. </p>
%% <p> The following <em> side-conditions </em> apply to this refactoring:
%% <li> The new module names should not conflict with each other, or any existing module names 
%% in the same scope which will not be renamed. </li>
%% <li> This refactorings assumes that the file basename is always the same as the module name. </li>
%% </p>
%% @end
%%%%
%% =====================================================================

%% @TODO: Add side-condition checkings.
-module(refac_batch_rename_mod).

-export([batch_rename_mod/3]).

-include("../include/wrangler.hrl").

%% =====================================================================
%% @spec batch_rename_mod(OldNamePattern::string(), NewNamePattern::string(), 
%%                        SearchPaths::[string()])-> ok | {error, string()}
%%   

%%-spec(batch_rename_mod/3::(string(), string(), [dir()])-> {ok, string()} | {error, string()}).
batch_rename_mod(OldNamePattern, NewNamePattern,SearchPaths) ->
    ?wrangler_io("\n[CMD: batch_rename_mod, ~p, ~p, ~p]\n", [OldNamePattern, NewNamePattern, SearchPaths]),
    %% Get all the erlang file which will be affected by this refactoring.
    Files = lists:append([[filename:join([Pwd,File])||
			      File <- filelib:wildcard("*.erl", Pwd)]   
			  || Pwd <- SearchPaths]),
    Mods = lists:map(fun({M, _Dir}) -> M end, refac_util:get_modules_by_file(Files)),
    ?wrangler_io("Mods:\n~p\n", [Mods]),
    Old_New_Mod_Names = lists:map(fun(M) -> {list_to_atom(M), 
					     list_to_atom(get_new_name(M, OldNamePattern, NewNamePattern))} end, Mods),
    ?wrangler_io("Old_New_Mod_Names:\n~p\n", [Old_New_Mod_Names]),
    HeaderFiles = refac_util:expand_files(SearchPaths, ".hrl"),
    %% Refactor both .erl and .hrl files, but does not change .hrl file name.
    Results = batch_rename_mod(Files++HeaderFiles, Old_New_Mod_Names),  
    refac_util:write_refactored_files(Results),
    ChangedFiles = lists:map(fun({{F, _F}, _AST}) -> F end, Results),
    ?wrangler_io("\n The following files have been changed by this refactoring:\n~p\n", [ChangedFiles]),
    {ok, "Refactoring finished."}.
    %%{ok, ChangedFiles}.


batch_rename_mod(Files, Old_New_Mod_Names) ->
    case Files of 
	[] -> [];
	[F|Fs] ->  
	    ?wrangler_io("The current file under refactoring is:\n~p\n",[F]),
	    {ok, {AnnAST, Info}} = refac_util:parse_annotate_file(F,true, []),
	    {AnnAST1, Changed} = do_rename_mod(AnnAST, Old_New_Mod_Names),
	    if Changed ->
		    NewFileName = 
			case get_module_name(Info) of 
			    {ok, OldModName} ->
				case lists:keysearch(OldModName, 1, Old_New_Mod_Names) of 
				    {value, {OldModName, NewModName}}  ->
					if OldModName == NewModName -> F;
					   true -> 
						filename:join([filename:dirname(F), atom_to_list(NewModName)++".erl"])
					end;
				    false -> {error, "Could not infer new module name"}
				end;
			    _ ->  F
			end,				   
		    [{{F,NewFileName}, AnnAST1}|batch_rename_mod(Fs, Old_New_Mod_Names)];
	       true -> batch_rename_mod(Fs, Old_New_Mod_Names)
	    end
    end.


do_rename_mod(Tree, Old_New_Mod_Names) ->
     refac_util:stop_tdTP(fun do_rename_mod_1/2, Tree, Old_New_Mod_Names).

do_rename_mod_1(Tree, Old_New_Mod_Names) ->
    case refac_syntax:type(Tree) of
        attribute -> 
 	    AttrName = refac_syntax:attribute_name(Tree),
 	    case refac_syntax:atom_value(AttrName) of 
		module -> Args = refac_syntax:attribute_arguments(Tree),
			  F = fun (Arg) -> OldModName = refac_syntax:atom_value(Arg),
					   {value, {OldModName, NewModName}} = 
					       lists:keysearch(OldModName, 1, Old_New_Mod_Names),
					   refac_syntax:copy_attrs(Arg, refac_syntax:atom(NewModName))
			      end,
			  Args1 = lists:map(F, Args),
			  Tree1 = refac_syntax:copy_attrs(Tree, refac_syntax:attribute(AttrName, Args1)),
			  {Tree1, true};
 		import -> Args = refac_syntax:attribute_arguments(Tree),
 			  case Args of 
 			      [H|T] -> OldModName = refac_syntax:atom_value(H), 
				       case lists:keysearch(OldModName, 1, Old_New_Mod_Names) of 
					   {value, {OldModName, NewModName}} ->
					       H1 = refac_syntax:copy_attrs(H, refac_syntax:atom(NewModName)),
					       Tree1 = refac_syntax:copy_attrs(Tree, 
									       refac_syntax:attribute(AttrName, [H1|T])),
					       {Tree1, true};
					   false -> {Tree, false}
				       end;
 			      _ -> {Tree, false}
 			  end;
 		_ -> {Tree, false}
 	    end;
        module_qualifier ->
 	    Mod = refac_syntax:module_qualifier_argument(Tree),
 	    Fun = refac_syntax:module_qualifier_body(Tree),
 	    case refac_syntax:type(Mod) of 
 		atom ->  OldModName = refac_syntax:atom_value(Mod),
 			 case lists:keysearch(OldModName, 1, Old_New_Mod_Names) of 
			     {value, {OldModName, NewModName}}->
 				 Mod1 = refac_syntax:copy_attrs(Mod, refac_syntax:atom(NewModName)),
 				 Tree1= refac_syntax:copy_attrs(Tree, refac_syntax:module_qualifier(Mod1, Fun)),
 				 {Tree1, true};
 			     false -> {Tree, false}
 			 end;
 		_ -> {Tree, false}
 	    end;
        _ -> {Tree, false}
     end.

get_new_name(Name, RegExp, NewRegExp) ->
    L = length(Name),
    case match(Name, sh_to_awk(RegExp)) of 
	{match, 1, L} -> Sub = get_sub(Name, RegExp),
			 get_new_name(Sub, NewRegExp);
	_ -> Name
    end.

get_new_name(Sub, NewRegExp) ->
    Index = string:str(NewRegExp, "*"),    
    case Index of 
	0 -> NewRegExp;
	N -> Prefix = string:sub_string(NewRegExp, 1, N-1), 
	     case Sub of 
		 [] -> throw({error,"Can not infer new module names, please check the new module name pattern specified!"});
		 _  -> Sub1 = hd(Sub),
		       get_new_name(tl(Sub), Prefix++Sub1++string:sub_string(NewRegExp,N+1))
	     end
    end.
	     
get_sub(Name, RegExp) ->
     get_sub(Name, RegExp, []).
get_sub(Name, RegExp, Sub) ->
      Index1 = string:str(RegExp, "*"), %% TODO: how about '?'
      case Index1 of 
  	0 -> Sub;
  	N -> SubName = string:sub_string(Name, N),
	     SubRegExp = string:sub_string(RegExp, N),
	     S = get_sub1(SubName, SubRegExp, SubName),
 	     get_sub(string:sub_string(SubName, length(S)+1), string:sub_string(SubRegExp,2), Sub++[S])
      end.

get_sub1(_SubName, _SubRegExp, []) -> [];
get_sub1(SubName, SubRegExp, Sub) ->
    Str = Sub ++ string:sub_string(SubRegExp, 2),
    L = length(SubName),
    case match(SubName, sh_to_awk(Str)) of 
	{match,1, L} -> Sub;
	nomatch -> get_sub1(SubName, SubRegExp, string:sub_string(Sub, 1, length(Sub)-1))
    end.

get_module_name(ModInfo) ->				      
    case lists:keysearch(module, 1, ModInfo) of
	{value, {module, ModName}} -> {ok, ModName};
	false ->
	    {error, "Can not get the current module name."}
    end.





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Some functions from regexp module; this module is deprecated; but I cound not find 
%% the corresponding functions in the 're' module;
%% Shall be removed when I have found the lib functions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reg(S) -> reg1(S).

reg1(S0) ->
    {L,S1} = reg2(S0),
    reg1p(S1, L).

reg1p([$||S0], L) ->
    {R,S1} = reg2(S0),
    reg1p(S1, {'or',L,R});
reg1p(S, L) -> {L,S}.


reg2(S0) ->
    {L,S1} = reg3(S0),
    reg2p(S1, L).

reg2p([C|S0], L) when C =/= $|, C =/= $) ->
    {R,S1} = reg3([C|S0]),
    reg2p(S1, {concat,L,R});
reg2p(S, L) -> {L,S}.

reg3(S0) ->
    {L,S1} = reg4(S0),
    reg3p(S1, L).

reg3p([$*|S], L) -> reg3p(S, {kclosure,L});
reg3p([$+|S], L) -> reg3p(S, {pclosure,L});
reg3p([$?|S], L) -> reg3p(S, {optional,L});
reg3p(S, L) -> {L,S}.

reg4([$(|S0]) ->
    case reg(S0) of
	{R,[$)|S1]} -> {R,S1};
	{_R,_S} -> throw({error,{unterminated,"("}})
    end;
reg4([$\\,O1,O2,O3|S]) when
  O1 >= $0, O1 =< $7, O2 >= $0, O2 =< $7, O3 >= $0, O3 =< $7 ->
    {(O1*8 + O2)*8 + O3 - 73*$0,S};
reg4([$\\,C|S]) -> {escape_char(C),S};
reg4([$\\]) -> throw({error,{unterminated,"\\"}});
reg4([$^|S]) -> {bos,S};
reg4([$$|S]) -> {eos,S};
reg4([$.|S]) -> {{comp_class,"\n"},S};
reg4("[^" ++ S0) ->
    case char_class(S0) of
	{Cc,[$]|S1]} -> {{comp_class,Cc},S1};
	{_Cc,_S} -> throw({error,{unterminated,"["}})
    end;
reg4([$[|S0]) ->
    case char_class(S0) of
	{Cc,[$]|S1]} -> {{char_class,Cc},S1};
	{_Cc,_S1} -> throw({error,{unterminated,"["}})
    end;

reg4([C|S]) when C =/= $*, C =/= $+, C =/= $?, C =/= $] -> {C,S};
reg4([C|_S]) -> throw({error,{illegal,[C]}});
reg4([]) -> {epsilon,[]}.

escape_char($n) -> $\n;				%\n = LF
escape_char($r) -> $\r;				%\r = CR
escape_char($t) -> $\t;				%\t = TAB
escape_char($v) -> $\v;				%\v = VT
escape_char($b) -> $\b;				%\b = BS
escape_char($f) -> $\f;				%\f = FF
escape_char($e) -> $\e;				%\e = ESC
escape_char($s) -> $\s;				%\s = SPACE
escape_char($d) -> $\d;				%\d = DEL
escape_char(C) -> C.

char_class([$]|S]) -> char_class(S, [$]]);
char_class(S) -> char_class(S, []).

char($\\, [O1,O2,O3|S]) when
  O1 >= $0, O1 =< $7, O2 >= $0, O2 =< $7, O3 >= $0, O3 =< $7 ->
    {(O1*8 + O2)*8 + O3 - 73*$0,S};
char($\\, [C|S]) -> {escape_char(C),S};
char(C, S) -> {C,S}.

char_class([C1|S0], Cc) when C1 =/= $] ->
    case char(C1, S0) of
	{Cf,[$-,C2|S1]} when C2 =/= $] ->
	    case char(C2, S1) of
		{Cl,S2} when Cf < Cl -> char_class(S2, [{Cf,Cl}|Cc]); 
		{Cl,_S2} -> throw({error,{char_class,[Cf,$-,Cl]}})
	    end;
	{C,S1} -> char_class(S1, [C|Cc])
    end;
char_class(S, Cc) -> {Cc,S}.

re_apply(S, St, RE) -> re_apply(RE, [], S, St).

re_apply(epsilon, More, S, P) ->		%This always matches
    re_apply_more(More, S, P);
re_apply({'or',RE1,RE2}, More, S, P) ->
    re_apply_or(re_apply(RE1, More, S, P),
		re_apply(RE2, More, S, P));
re_apply({concat,RE1,RE2}, More, S0, P) ->
    re_apply(RE1, [RE2|More], S0, P);
re_apply({kclosure,CE}, More, S, P) ->
    %% Be careful with the recursion, explicitly do one call before
    %% looping.
    re_apply_or(re_apply_more(More, S, P),
		re_apply(CE, [{kclosure,CE}|More], S, P));
re_apply({pclosure,CE}, More, S, P) ->
    re_apply(CE, [{kclosure,CE}|More], S, P);
re_apply({optional,CE}, More, S, P) ->
    re_apply_or(re_apply_more(More, S, P),
		re_apply(CE, More, S, P));
re_apply(bos, More, S, 1) -> re_apply_more(More, S, 1);
re_apply(eos, More, [$\n|S], P) -> re_apply_more(More, S, P);
re_apply(eos, More, [], P) -> re_apply_more(More, [], P);
re_apply({char_class,Cc}, More, [C|S], P) ->
    case in_char_class(C, Cc) of
	true -> re_apply_more(More, S, P+1);
	false -> nomatch
    end;
re_apply({comp_class,Cc}, More, [C|S], P) ->
    case in_char_class(C, Cc) of
	true -> nomatch;
	false -> re_apply_more(More, S, P+1)
    end;
re_apply(C, More, [C|S], P) when is_integer(C) ->
    re_apply_more(More, S, P+1);
re_apply(_RE, _More, _S, _P) -> nomatch.

%% re_apply_more([RegExp], String, Length) -> re_app_res().

re_apply_more([RE|More], S, P) -> re_apply(RE, More, S, P);
re_apply_more([], S, P) -> {match,P,S}.

%% in_char_class(Char, Class) -> bool().

in_char_class(C, [{C1,C2}|_Cc]) when C >= C1, C =< C2 -> true;
in_char_class(C, [C|_Cc]) -> true;
in_char_class(C, [_|Cc]) -> in_char_class(C, Cc);
in_char_class(_C, []) -> false.

%% re_apply_or(Match1, Match2) -> re_app_res().
%%  If we want the best match then choose the longest match, else just
%%  choose one by trying sequentially.

re_apply_or({match,P1,S1},   {match,P2,_S2}) when P1 >= P2 -> {match,P1,S1};
re_apply_or({match,_P1,_S1}, {match,P2,S2}) -> {match,P2,S2};
re_apply_or(nomatch, R2) -> R2;
re_apply_or(R1, nomatch) -> R1.

%% sh_to_awk(ShellRegExp)
%%  Convert a sh style regexp into a full AWK one. The main difficulty is
%%  getting character sets right as the conventions are different.

sh_to_awk(Sh) -> "^(" ++ sh_to_awk_1(Sh). %Fix the beginning

sh_to_awk_1([$*|Sh]) ->				%This matches any string
    ".*" ++ sh_to_awk_1(Sh);
sh_to_awk_1([$?|Sh]) ->				%This matches any character
    [$.|sh_to_awk_1(Sh)];
sh_to_awk_1([$[,$^,$]|Sh]) ->			%This takes careful handling
    "\\^" ++ sh_to_awk_1(Sh);
sh_to_awk_1("[^" ++ Sh) -> [$[|sh_to_awk_2(Sh, true)];
sh_to_awk_1("[!" ++ Sh) -> "[^" ++ sh_to_awk_2(Sh, false);
sh_to_awk_1([$[|Sh]) -> [$[|sh_to_awk_2(Sh, false)];
sh_to_awk_1([C|Sh]) ->
    %% Unspecialise everything else which is not an escape character.
    case special_char(C) of
	true -> [$\\,C|sh_to_awk_1(Sh)];
	false -> [C|sh_to_awk_1(Sh)]
    end;
sh_to_awk_1([]) -> ")$". 		%Fix the end

sh_to_awk_2([$]|Sh], UpArrow) -> [$]|sh_to_awk_3(Sh, UpArrow)];
sh_to_awk_2(Sh, UpArrow) -> sh_to_awk_3(Sh, UpArrow).

sh_to_awk_3([$]|Sh], true) -> "^]" ++ sh_to_awk_1(Sh);
sh_to_awk_3([$]|Sh], false) -> [$]|sh_to_awk_1(Sh)];
sh_to_awk_3([C|Sh], UpArrow) -> [C|sh_to_awk_3(Sh, UpArrow)];
sh_to_awk_3([], true) -> [$^|sh_to_awk_1([])];
sh_to_awk_3([], false) -> sh_to_awk_1([]).

%% -type special_char(char()) -> bool().
%%  Test if a character is a special character.

special_char($|) -> true;
special_char($*) -> true;
special_char($+) -> true;
special_char($?) -> true;
special_char($() -> true;
special_char($)) -> true;
special_char($\\) -> true;
special_char($^) -> true;
special_char($$) -> true;
special_char($.) -> true;
special_char($[) -> true;
special_char($]) -> true;
special_char($") -> true;
special_char(_C) -> false.

%% parse(RegExp) -> {ok,RE} | {error,E}.
%%  Parse the regexp described in the string RegExp.

parse(S) ->
    case catch reg(S) of
	{R,[]} -> {ok,R};
	{_R,[C|_]} -> {error,{illegal,[C]}};
	{error,E} -> {error,E}
    end.

%% -type match(String, RegExp) -> matchres().
%%  Find the longest match of RegExp in String.

match(S, RegExp) when is_list(RegExp) ->
    case parse(RegExp) of
	{ok,RE} -> match(S, RE);
	{error,E} -> {error,E}
    end;
match(S, RE) ->
    case match(RE, S, 1, 0, -1) of
	{Start,Len} when Len >= 0 ->
	    {match,Start,Len};
	{_Start,_Len} -> nomatch
    end.

match(RE, S, St, Pos, L) ->
    case first_match(RE, S, St) of
	{St1,L1} ->
	    Nst = St1 + 1,
	    if L1 > L -> match(RE, lists:nthtail(Nst-St, S), Nst, St1, L1);
	       true -> match(RE, lists:nthtail(Nst-St, S), Nst, Pos, L)
	    end;
	nomatch -> {Pos,L}
    end.

first_match(RE, S, St) when S =/= [] ->
    case re_apply(S, St, RE) of
	{match,P,_Rest} -> {St,P-St};
	nomatch -> first_match(RE, tl(S), St+1)
    end;
first_match(_RE, [], _St) -> nomatch.


