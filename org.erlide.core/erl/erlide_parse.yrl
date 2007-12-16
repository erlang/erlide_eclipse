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
%%     $Id$
%%

%% Definition of the Erlang grammar.

%% Note that this grammar is *not* the regular Erlang grammar!
%% It is tailored so that it will return useful data even in the presence of
%% incomplete or incorrect code.

Nonterminals
form
attribute attr_val
%function function_clauses function_clause 
function_head function_body
clause_args clause_guard clause_body
expr expr_100 expr_150 expr_160 expr_200 expr_300 expr_400 expr_500
expr_600 expr_700 expr_800 expr_900
expr_max
list tail
list_comprehension lc_expr lc_exprs
binary_comprehension 
tuple
%struct
record_expr record_tuple record_field record_fields
if_expr if_clause if_clauses case_expr cr_clause cr_clauses receive_expr
fun_expr fun_clause fun_clauses
try_expr try_catch try_clause try_clauses query_expr
function_call argument_list
exprs guard
atomic strings
prefix_op mult_op add_op list_op comp_op
rule rule_clauses rule_clause 
%rule_args rule_guard 
rule_body
binary bin_elements bin_element bit_expr 
opt_bit_size_expr bit_size_expr opt_bit_type_list bit_type_list bit_type
%%top_type top_types type typed_expr typed_attr_val arg_types arg_type
%%typed_exprs typed_record_fields
.

Terminals
char integer float atom string var macro

'(' ')' ',' '->' ':-' '{' '}' '[' ']' '|' '||' '<-' ';' ':' '#' '.'
'after' 'begin' 'case' 'try' 'catch' 'end' 'fun' 'if' 'of' 'receive' 'when'
'andalso' 'orelse' 'query'
'bnot' 'not'
'*' '/' 'div' 'rem' 'band' 'and'
'+' '-' 'bor' 'bxor' 'bsl' 'bsr' 'or' 'xor'
'++' '--'
'==' '/=' '=<' '<' '>=' '>' '=:=' '=/=' '<='
'<<' '>>'
'!' '=' 
%% '::'
dot.

Rootsymbol form.

form -> attribute : '$1'.
form -> function_head : '$1'.
form -> function_body : '$1'.
form -> rule : '$1'.

attribute -> '-' atom dot : build_attribute('$2', none, span('$1', '$3')).
attribute -> '-' atom '(' attr_val ')' dot : build_attribute('$2', '$4', span('$1', '$6')).
%% this doesn't seem to work yet /2007-09-19
%%attribute -> '-' atom '(' typed_attr_val ')' dot : build_typed_attribute('$2','$4', span('$1', '$6')).

%typed_attr_val -> expr ',' typed_record_fields : ['$1' , '$3'].
%typed_attr_val -> expr '::' top_types          : ['$1' , '$3'].

%typed_record_fields -> '{' typed_exprs '}' : {tuple,line('$1'),'$2'}.

%typed_exprs -> typed_expr                 : ['$1'].
%typed_exprs -> typed_expr ',' typed_exprs : ['$1'|'$3'].
%typed_exprs -> expr ',' typed_exprs       : ['$1'|'$3'].
%typed_exprs -> typed_expr ',' exprs       : ['$1'|'$3'].

%typed_expr -> expr '::' top_type          : {typed,'$1','$3'}.

%top_types -> top_type                     : ['$1'].
%top_types -> top_type ',' top_types       : ['$1'|'$3'].

%top_type -> type                          : '$1'.
%top_type -> type '|' top_type             : lift_unions('$1','$3').

%type -> atom                              : {type, atom, [normalise('$1')]}.
%type -> atom '(' ')'                      : build_type('$1', []).
%type -> atom '(' top_type ')'             : build_type('$1', ['$3']).
%type -> atom '(' top_type ',' top_type ')': build_type('$1', ['$3', '$5']).
%type -> '[' ']'                           : {type, nil, []}.
%type -> '[' top_type ']'                  : {type, list, ['$2']}.
%type -> '[' top_type ',' '.' '.' '.' ']'  : {type, nonempty_list, ['$2']}.
%type -> '(' '(' ')' '->' top_type ')'     : {type, 'fun', [[], '$5']}.
%type -> '(' '(' arg_types ')' '->' top_type ')' : {type, 'fun', ['$3', '$6']}.
%type -> '{' '}'                           : {type, tuple, []}.
%type -> '{' top_types '}'                 : {type, tuple, '$2'}.
%type -> '#' atom '{' '}'                  : {type, record, [normalise('$2')]}.
%type -> integer                           : {type, integer, [normalise('$1')]}.
%type -> '(' integer '.' '.' integer ')'   : 
%			{type, range, [normalise('$2'), normalise('$5')]}.

%arg_types -> arg_type                     : ['$1'].
%arg_types -> arg_type ',' arg_types       : ['$1'|'$3'].

%arg_type -> var '::' top_type             : '$3'.
%arg_type -> top_type                      : '$1'.

attr_val -> exprs : '$1'.

function_head -> atom clause_args clause_guard : {clause_head, span('$1', '$3'), element(3, '$1'), '$2', '$3'}.

function_body -> '->' exprs ';' : {clause_body, '$2'}.
function_body -> '->' exprs dot : {clause_body, '$2'}.

clause_args -> argument_list : element(1, '$1').

clause_guard -> 'when' guard : '$2'.
clause_guard -> '$empty' : [].

clause_body -> '->' exprs: '$2'.


expr -> 'catch' expr : {'catch',span('$1','$2'),'$2'}.
expr -> expr_100 : '$1'.

expr_100 -> expr_150 '=' expr_100 : {match,span('$1','$3'),'$1','$3'}.
expr_100 -> expr_150 '!' expr_100 : mkop('$1', '$2', '$3').
expr_100 -> expr_150 : '$1'.

expr_150 -> expr_160 'orelse' expr_150 : mkop('$1', '$2', '$3').
expr_150 -> expr_160 : '$1'.

expr_160 -> expr_200 'andalso' expr_160 : mkop('$1', '$2', '$3').
expr_160 -> expr_200 : '$1'.

expr_200 -> expr_300 comp_op expr_300 :
    mkop('$1', '$2', '$3').
expr_200 -> expr_300 : '$1'.

expr_300 -> expr_400 list_op expr_300 :
    mkop('$1', '$2', '$3').
expr_300 -> expr_400 : '$1'.

expr_400 -> expr_400 add_op expr_500 :
    mkop('$1', '$2', '$3').
expr_400 -> expr_500 : '$1'.

expr_500 -> expr_500 mult_op expr_600 :
    mkop('$1', '$2', '$3').
expr_500 -> expr_600 : '$1'.

expr_600 -> prefix_op expr_700 :
    mkop('$1', '$2').
expr_600 -> expr_700 : '$1'.

expr_700 -> function_call : '$1'.
expr_700 -> record_expr : '$1'.
expr_700 -> expr_800 : '$1'.

expr_800 -> expr_900 ':' expr_max :
    {remote,span('$1','$3'),'$1','$3'}.
expr_800 -> expr_900 : '$1'.

expr_900 -> '.' atom :
    {record_field,span('$1','$2'),{atom,span('$1','$2'),''},'$2'}.
expr_900 -> expr_900 '.' atom :
    {record_field,span('$1','$3'),'$1','$3'}.
expr_900 -> expr_max : '$1'.

expr_max -> var : '$1'.
expr_max -> atomic : '$1'.
expr_max -> list : '$1'.
expr_max -> binary : '$1'.
expr_max -> list_comprehension : '$1'.
expr_max -> binary_comprehension : '$1'.
expr_max -> tuple : '$1'.
%%expr_max -> struct : '$1'.
expr_max -> '(' expr ')' : '$2'.
expr_max -> 'begin' exprs 'end' : {block,span('$1','$2'),'$2'}.
expr_max -> if_expr : '$1'.
expr_max -> case_expr : '$1'.
expr_max -> receive_expr : '$1'.
expr_max -> fun_expr : '$1'.
expr_max -> try_expr : '$1'.
expr_max -> query_expr : '$1'.


list -> '[' ']' : {nil,pos('$1')}.
list -> '[' expr tail : {cons,span('$1','$3'),'$2','$3'}.

tail -> ']' : {nil,pos('$1')}.
tail -> '|' expr ']' : '$2'.
tail -> ',' expr tail : {cons,span('$1','$3'),'$2','$3'}.


binary -> '<<' '>>' : {bin,span('$1','$2'),[]}.
binary -> '<<' bin_elements '>>' : {bin,span('$1','$3'),'$2'}.

bin_elements -> bin_element : ['$1'].
bin_elements -> bin_element ',' bin_elements : ['$1'|'$3'].

bin_element -> bit_expr opt_bit_size_expr opt_bit_type_list :
    {bin_element,pos('$1'),'$1','$2','$3'}.

bit_expr -> prefix_op expr_max : mkop('$1', '$2').
bit_expr -> expr_max : '$1'.

opt_bit_size_expr -> ':' bit_size_expr : '$2'.
opt_bit_size_expr -> '$empty' : default.

opt_bit_type_list -> '/' bit_type_list : '$2'.
opt_bit_type_list -> '$empty' : default.

bit_type_list -> bit_type '-' bit_type_list : ['$1' | '$3'].
bit_type_list -> bit_type : ['$1'].

bit_type -> atom             : '$1'.
bit_type -> atom ':' integer : { '$1', '$3' }.

bit_size_expr -> expr_max : '$1'.


list_comprehension -> '[' expr '||' lc_exprs ']' :
    {lc,span('$1','$5'),'$2','$4'}.
binary_comprehension -> '<<' binary '||' lc_exprs '>>' :
	{bc,span('$1','$5'),'$2','$4'}.

lc_exprs -> lc_expr : ['$1'].
lc_exprs -> lc_expr ',' lc_exprs : ['$1'|'$3'].

lc_expr -> expr : '$1'.
lc_expr -> expr '<-' expr : {generate,span('$1','$3'),'$1','$3'}.
lc_expr -> binary '<=' expr_max : {b_generate,span('$1','$3'),'$1','$3'}.


tuple -> '{' '}' : {tuple,span('$1','$2'),[]}.
tuple -> '{' exprs '}' : {tuple,span('$1','$3'),'$2'}.


%%struct -> atom tuple :
%%    {struct,span('$1','$3'),element(3, '$1'),element(3, '$2')}.


%% N.B. This is called from expr_700.
%% N.B. Field names are returned as the complete object, even if they are
%% always atoms for the moment, this might change in the future.

record_expr -> '#' atom '.' atom :
    {record_index,span('$1','$4'),element(3, '$2'),'$4'}.
record_expr -> '#' atom record_tuple :
    {record,span('$1','$3'),element(3, '$2'),'$3'}.
record_expr -> expr_max '#' atom '.' atom :
    {record_field,span('$1','$5'),'$1',element(3, '$3'),'$5'}.
record_expr -> expr_max '#' atom record_tuple :
    {record,span('$1','$4'),'$1',element(3, '$3'),'$4'}.

record_tuple -> '{' '}' : [].
record_tuple -> '{' record_fields '}' : '$2'.

record_fields -> record_field : ['$1'].
record_fields -> record_field ',' record_fields : ['$1' | '$3'].

record_field -> var '=' expr : {record_field,span('$1','$3'),'$1','$3'}.
record_field -> atom '=' expr : {record_field,span('$1','$3'),'$1','$3'}.

%% N.B. This is called from expr_700.

function_call -> expr_800 argument_list :
    {call,span('$1','$2'),'$1',element(1, '$2')}.


if_expr -> 'if' if_clauses 'end' : {'if',span('$1','$3'),'$2'}.

if_clauses -> if_clause : ['$1'].
if_clauses -> if_clause ';' if_clauses : ['$1' | '$3'].

if_clause -> guard clause_body :
    {clause,hd(hd('$1')),[],'$1','$2'}.
%%    {clause,span('$1','$2'),[],'$1','$2'}.
%% 'guard' is just a list, can't use span on it


case_expr -> 'case' expr 'of' cr_clauses 'end' :
    {'case',span('$1','$5'),'$2','$4'}.

cr_clauses -> cr_clause : ['$1'].
cr_clauses -> cr_clause ';' cr_clauses : ['$1' | '$3'].

cr_clause -> expr clause_guard clause_body :
    {clause,span('$1','$3'),['$1'],'$2','$3'}.

receive_expr -> 'receive' cr_clauses 'end' :
    {'receive',span('$1','$3'),'$2'}.
receive_expr -> 'receive' 'after' expr clause_body 'end' :
    {'receive',span('$1','$5'),[],'$3','$4'}.
receive_expr -> 'receive' cr_clauses 'after' expr clause_body 'end' :
    {'receive',span('$1','$6'),'$2','$4','$5'}.


fun_expr -> 'fun' atom '/' integer :
    {'fun',span('$1','$4'),{function,element(3, '$2'),element(3, '$4')}}.
fun_expr -> 'fun' var '/' integer :
    {'fun',span('$1','$4'),{function,element(3, '$2'),element(3, '$4')}}.
fun_expr -> 'fun' atom ':' atom '/' integer :
    {'fun',span('$1','$6'),{function,element(3, '$2'),element(3, '$4'),element(3,'$6')}}.
fun_expr -> 'fun' var ':' var '/' integer :
    {'fun',span('$1','$6'),{function,element(3, '$2'),element(3, '$4'),element(3,'$6')}}.
fun_expr -> 'fun' atom ':' var '/' integer :
    {'fun',span('$1','$6'),{function,element(3, '$2'),element(3, '$4'),element(3,'$6')}}.
fun_expr -> 'fun' var ':' atom '/' integer :
    {'fun',span('$1','$6'),{function,element(3, '$2'),element(3, '$4'),element(3,'$6')}}.
fun_expr -> 'fun' fun_clauses 'end' :
    build_fun(span('$1','$3'), '$2').

fun_clauses -> fun_clause : ['$1'].
fun_clauses -> fun_clause ';' fun_clauses : ['$1' | '$3'].

fun_clause -> argument_list clause_guard clause_body :
    {Args,Pos} = '$1',
    {clause,Pos,'fun',Args,'$2','$3'}.

try_expr -> 'try' exprs 'of' cr_clauses try_catch :
    build_try(pos('$1'),'$2','$4','$5').
try_expr -> 'try' exprs try_catch :
    build_try(pos('$1'),'$2',[],'$3').
%% TODO use span here, but how?

try_catch -> 'catch' try_clauses 'end' :
    {'$2',[]}.
try_catch -> 'catch' try_clauses 'after' exprs 'end' :
    {'$2','$4'}.
try_catch -> 'after' exprs 'end' :
    {[],'$2'}.

try_clauses -> try_clause : ['$1'].
try_clauses -> try_clause ';' try_clauses : ['$1' | '$3'].

try_clause -> expr clause_guard clause_body :
    L = pos('$1'),
    {clause,L,[{tuple,L,[{atom,L,throw},'$1',{var,L,'_'}]}],'$2','$3'}.
try_clause -> atom ':' expr clause_guard clause_body :
    L = pos('$1'),
    {clause,L,[{tuple,L,['$1','$3',{var,L,'_'}]}],'$4','$5'}.
try_clause -> var ':' expr clause_guard clause_body :
    L = pos('$1'),
    {clause,L,[{tuple,L,['$1','$3',{var,L,'_'}]}],'$4','$5'}.

query_expr -> 'query' list_comprehension 'end' :
    {'query',span('$1','$3'),'$2'}.


argument_list -> '(' ')' : {[],span('$1','$2')}.
argument_list -> '(' exprs ')' : {'$2',span('$1','$3')}.


exprs -> expr : ['$1'].
exprs -> expr ',' exprs : ['$1' | '$3'].

guard -> exprs : ['$1'].
guard -> exprs ';' guard : ['$1'|'$3'].

atomic -> char : '$1'.
atomic -> integer : '$1'.
atomic -> float : '$1'.
atomic -> atom : '$1'.
atomic -> strings : '$1'.
atomic -> macro : '$1'.

strings -> string : '$1'.
strings -> string strings :
    {string,span('$1','$2'),element(3, '$1') ++ element(3, '$2')}.

prefix_op -> '+' : '$1'.
prefix_op -> '-' : '$1'.
prefix_op -> 'bnot' : '$1'.
prefix_op -> 'not' : '$1'.

mult_op -> '/' : '$1'.
mult_op -> '*' : '$1'.
mult_op -> 'div' : '$1'.
mult_op -> 'rem' : '$1'.
mult_op -> 'band' : '$1'.
mult_op -> 'and' : '$1'.

add_op -> '+' : '$1'.
add_op -> '-' : '$1'.
add_op -> 'bor' : '$1'.
add_op -> 'bxor' : '$1'.
add_op -> 'bsl' : '$1'.
add_op -> 'bsr' : '$1'.
add_op -> 'or' : '$1'.
add_op -> 'xor' : '$1'.

list_op -> '++' : '$1'.
list_op -> '--' : '$1'.

comp_op -> '==' : '$1'.
comp_op -> '/=' : '$1'.
comp_op -> '=<' : '$1'.
comp_op -> '<' : '$1'.
comp_op -> '>=' : '$1'.
comp_op -> '>' : '$1'.
comp_op -> '=:=' : '$1'.
comp_op -> '=/=' : '$1'.

rule -> rule_clauses dot : build_rule('$1', '$2').

rule_clauses -> rule_clause : ['$1'].
rule_clauses -> rule_clause ';' rule_clauses : ['$1'|'$3'].

rule_clause -> atom clause_args clause_guard rule_body :
    {clause,span('$1','$4'),element(3, '$1'),'$2','$3','$4'}.


%rule_args -> argument_list : element(1, '$1').

%rule_guard -> 'when' guard : '$2'.
%rule_guard -> '$empty' : [].

rule_body -> ':-' lc_exprs: '$2'.

Expect 2. %% conflicts inherited from the original erl_parse

Erlang code.

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
%%     $Id$
%%

-export([parse_form/1,parse_exprs/1,parse_term/1]).
-export([normalise/1,abstract/1,tokens/1,tokens/2]).
-export([abstract/2, package_segments/1]).
-export([inop_prec/1,preop_prec/1,func_prec/0,max_prec/0]).

%% The following directive is needed for (significantly) faster compilation
%% of the generated .erl file by the HiPE compiler.  Please do not remove.
-compile([inline,{hipe,[{regalloc,linear_scan}]}]).

-compile(export_all).

%% mkop(Op, Arg) -> {op,Line,Op,Arg}.
%% mkop(Left, Op, Right) -> {op,Line,Op,Left,Right}.

mkop(L, {Op,_Pos}, R) -> {op,span(L, R),Op,L,R}.

mkop({Op,Pos}, A) -> {op,Pos,Op,A}.

%% keep track of line info in tokens
line(Tup) -> element(2, element(1, Tup)).

pos(Tup) -> element(2, Tup).

span(Tup1, []) when is_tuple(Tup1) ->
  %%TODO we need the actual position here...
    span(Tup1, Tup1);
span(Tup1, L) when is_tuple(Tup1), is_list(L) ->
    span(Tup1, lists:last(L));
span(Tup1, Tup2) when is_tuple(Tup1), is_tuple(Tup2) ->
    {StartPos, _Len1} = element(2, Tup1),
    {_, C1} = StartPos,
    {EndPos, Len2} = element(2, Tup2),
    {_, C2} = EndPos,
    {StartPos, C2+Len2-C1}.

%% Entry points compatible to old erl_parse.
%% These really suck and are only here until Calle gets multiple
%% entry points working.

parse_form(Tokens) ->
    parse(Tokens).

parse_exprs(Tokens) ->
    case parse([{atom,0,f},{'(',0},{')',0},{'->',0}|Tokens]) of
    {ok,{function,_Lf,f,0,[{clause,_Lc,[],[],Exprs}]}} ->
        {ok,Exprs};
    {error,E} -> {error,E}
    end.

parse_term(Tokens) ->
    case parse([{atom,0,f},{'(',0},{')',0},{'->',0}|Tokens]) of
    {ok,{function,_Lf,f,0,[{clause,_Lc,[],[],[Expr]}]}} ->
        case catch normalise(Expr) of
        {'EXIT',_R} ->
            {error,{line(Expr),?MODULE,"bad term"}};
        Term -> {ok,Term}
        end;
    {ok,{function,_Lf,f,0,[{clause,_Lc,[],[],[_E1,E2|_Es]}]}} ->
        {error,{line(E2),?MODULE,"bad term"}};
    {error,E} -> {error,E}
    end.

build_type({atom,_,any}, []) -> {type, any, []};
build_type({atom,_,atom}, []) -> {type, atom, []};
build_type({atom,_,binary}, []) -> {type, binary, []};
build_type({atom,_,bool}, []) -> {type, bool, []};
build_type({atom,_,byte}, []) -> {type, byte, []};
build_type({atom,_,char}, []) -> {type, char, []};
build_type({atom,_,float}, []) -> {type, float, []};
build_type({atom,_,function}, []) -> {type, 'fun', []};
build_type({atom,_,identifier}, []) -> {type, identifier, []};
build_type({atom,_,integer}, []) -> {type, integer, []};
build_type({atom,_,list}, []) -> {type, list, []};
build_type({atom,_,mfa}, []) -> {type, mfa, []};
build_type({atom,_,neg_integer}, []) -> {type, neg_integer, []};
build_type({atom,_,non_neg_integer}, []) -> {type, non_neg_integer, []};
build_type({atom,_,none}, []) -> {type, none, []};
build_type({atom,_,nonempty_list}, [C,T]) -> {type, cons, [C,T]};
build_type({atom,_,nonempty_possibly_improper_list}, []) -> 
    {type, cons, []};
build_type({atom,_,nonempty_posssibly_improper_list}, [C,T]) -> 
    {type, cons, [C, T]};
build_type({atom,_,number}, []) -> {type, number, []};
build_type({atom,_,pid}, []) -> {type, pid, []};
build_type({atom,_,port}, []) -> {type, port, []};
build_type({atom,_,pos_integer}, []) -> {type, pos_integer, []};
build_type({atom,_,possibly_improper_list}, [C,T]) -> 
    {type, pos_improper_list, [C,T]};
build_type({atom,_,possibly_improper_list}, []) -> 
    {type, pos_improper_list, []};
build_type({atom,_,ref}, []) -> {type, ref, []};
build_type({atom,_,string}, []) -> {type, string, []};
build_type({atom,_,tuple}, []) -> {type, tuple, []};
build_type({atom,_,Other}, []) -> {type, var, [Other]}.

build_typed_attribute({atom,_La,record}, [{atom,_Ln,RecordName},RecTuple], Span) ->
    {attribute,Span,record,{RecordName,record_tuple(RecTuple)}};
build_typed_attribute({atom,_La,spec}, [{op,_Lo,'/',{atom,_La,FunName},
				                   {integer,_Li,FunArity}},
				       	TypeSpec], Span)  ->
    {attribute,Span,type_spec,{{FunName,FunArity},TypeSpec}};
build_typed_attribute({atom,La,_},_, _) ->
    error_bad_decl(La,spec).

lift_unions(T1, {type, union, List}) ->
    {type, union, [T1|List]};
lift_unions(T1, T2 = {type, _, _}) ->
    {type, union, [T1, T2]}.


%% build_attribute(AttrName, AttrValue, Span) ->
%%    {attribute,Line,module,Module}
%%    {attribute,Line,export,Exports}
%%    {attribute,Line,import,Imports}
%%    {attribute,Line,record,{Name,Inits}}
%%    {attribute,Line,file,{Name,Line}}
%%    {attribute,Line,Name,Val}

build_attribute({atom,_La,module}, Val, Span) ->
    case Val of
    [{atom,_Lm,Module}] ->
        {attribute,Span,module,Module};
    [{atom,_Lm,Module},ExpList] ->
        {attribute,Span,module,{Module,var_list(ExpList)}};
    [Name] ->
        case package_segments(Name) of
        error ->
            error_bad_decl(Span, module);
        Module ->
            {attribute,Span,module,Module}
        end;
    [Name,ExpList] ->
        case package_segments(Name) of
        error ->
            error_bad_decl(Span, module);
        Module ->
            {attribute,Span,module,{Module,var_list(ExpList)}}
        end;
    Other ->
        {attribute, Span, module, Other}
    end;
build_attribute({atom,_La,export}, Val, Span) ->
    case Val of
    [ExpList] ->
        {attribute,Span,export,farity_list(ExpList)};
    Other ->
        {attribute, Span, export, Other}
    end;
build_attribute({atom,_La,import}, Val, Span) ->
    case Val of
    [Name] ->
        case package_segments(Name) of
        error ->
            error_bad_decl(Span, import);
        Module ->
            {attribute,Span,import,Module}
        end;
    [{atom,_Lm,Mod},ImpList] ->
        {attribute,Span,import,{Mod,farity_list(ImpList)}};
    [Name, ImpList] ->
        case package_segments(Name) of
        error ->
            error_bad_decl(Span, import);
        Module ->
            {attribute,Span,import,{Module,farity_list(ImpList)}}
        end;
    Other ->
        {attribute, Span, import, Other}
    end;
build_attribute({atom,_La,record}, Val, Span) ->
    case Val of
    [{atom,_Ln,Record},RecTuple] ->
        {attribute,Span,record,{Record,record_tuple(RecTuple)}};
    Other ->
        {attribute, Span, record, Other}
    end;
build_attribute({atom,_La,file}, Val, Span) ->
    case Val of
    [{string,_Ln,Name},{integer,_Ll,Line}] ->
        {attribute,Span,file,{Name,Line}};
    Other ->
        {attribute, Span, file, Other}
    end;
build_attribute({atom,_La,Attr}, Val, Span) ->
    %%io:format("¤¤ ~p ~n", [{Attr, Val, Span}]),
    case Val of
    [Expr] ->
        {attribute,Span,Attr,term(Expr)};
    Other ->
        {attribute, Span, Attr, Other}
    end.

var_list({cons,_Lc,{var,_,V},Tail}) ->
    [V|var_list(Tail)];
var_list({nil,_Ln}) -> [];
var_list(Other) ->
    return_error(line(Other), "bad variable list").

error_bad_decl(L, S) ->
    return_error(L, io_lib:format("bad ~w declaration", [S])).

farity_list({cons,_Lc,{op,_Lo,'/',{atom,_La,A}=Ax,{integer,_Li,I}=Ix},Tail}) ->
    [{farity, span(Ax, Ix), A, I} | farity_list(Tail)];
farity_list({nil,_Ln, _}) -> [];
farity_list({nil,_Ln}) -> [];
farity_list(Other) ->
    return_error(line(Other), "bad function arity").

record_tuple({tuple,_Lt,Fields}=_T) ->
    %%io:format("--->~p ~n", [T]),
    record_fields(Fields);
record_tuple(Other) ->
    return_error(line(Other), "bad record declaration").

record_fields([{atom,La,A}|Fields]) ->
    [{record_field,La,{atom,La,A}}|record_fields(Fields)];
record_fields([{match,_Lm,{atom,La,A},Expr}|Fields]) ->
    [{record_field,La,{atom,La,A},Expr}|record_fields(Fields)];
record_fields([{typed,Expr,TypeInfo}|Fields]) ->
    [Field] = record_fields([Expr]),
    TypeInfo1 = 
	case Expr of
	    {match, _, _, _} -> TypeInfo; %% If we have an initializer.
	    {atom, _, _} -> lift_unions({type, atom, ['undefined']}, TypeInfo)
	end, 
    [{typed_record_field,Field,TypeInfo1}|record_fields(Fields)];
record_fields([Other|_Fields]) ->
    return_error(line(Other), "bad record field");
record_fields([]) -> [].

term(Expr) ->
    %%io:format("-- ~p~n", [catch normalise(Expr)]),
    case catch normalise(Expr) of
    {'EXIT',_R} -> return_error(line(Expr), "bad attribute");
    Term -> Term
    end.

package_segments(Name) ->
    package_segments(Name, [], []).

package_segments({record_field, _, F1, F2}, Fs, As) ->
    package_segments(F1, [F2 | Fs], As);
package_segments({atom, _, A}, [F | Fs], As) ->
    package_segments(F, Fs, [A | As]);
package_segments({atom, _, A}, [], As) ->
    lists:reverse([A | As]);
package_segments(_, _, _) ->
    error.

%% build_function([Clause]) -> {function,Line,Name,Arity,[Clause]}

build_function(Cs, Dot) ->
    Name = element(3, hd(Cs)),
    Arity = length(element(4, hd(Cs))),
    {function,span(hd(Cs), Dot),Name,Arity,check_clauses(Cs, Name, Arity)}.

%% build_rule([Clause]) -> {rule,Line,Name,Arity,[Clause]'}

build_rule(Cs, Dot) ->
    Name = element(3, hd(Cs)),
    Arity = length(element(4, hd(Cs))),
    {rule,span(hd(Cs), Dot),Name,Arity,check_clauses(Cs, Name, Arity)}.

%% build_fun(Line, [Clause]) -> {'fun',Line,{clauses,[Clause]}}.

build_fun(Line, Cs) ->
    Arity = length(element(4, hd(Cs))),
    {'fun',Line,{clauses,check_clauses(Cs, 'fun', Arity)}}.

check_clauses(Cs, Name, Arity) ->
     mapl(fun ({clause,L,N,As,G,B}) when N =:= Name, length(As) =:= Arity ->
         {clause,L,As,G,B};
         ({clause,L,_N,_As,_G,_B}) ->
         return_error(L, "head mismatch") end, Cs).

build_try(L,Es,Scs,{Ccs,As}) ->
    {'try',L,Es,Scs,Ccs,As}.

%% mapl(F,List)
%% an alternative map which always maps from left to right
%% and makes it possible to interrupt the mapping with throw on
%% the first occurence from left as expected.
%% can be removed when the jam machine (and all other machines)
%% uses the standardized (Erlang 5.0) evaluation order (from left to right)
mapl(F, [H|T]) ->
    V = F(H),
    [V | mapl(F,T)];
mapl(_, []) ->
    [].

%% normalise(AbsTerm)
%% abstract(Term)
%%  Convert between the abstract form of a term and a term.

normalise({char,_,C}) -> C;
normalise({integer,_,I}) -> I;
normalise({float,_,F}) -> F;
normalise({atom,_,A}) -> A;
normalise({string,_,S}) -> S;
normalise({nil,_}) -> [];
normalise({bin,_,Fs}) ->
    {value, B, _} =
    eval_bits:expr_grp(Fs, [],
               fun(E, _) ->
                   {value, normalise(E), []}
               end, [], true),
    B;
normalise({var,_, V}) -> V; %% TODO vars aren't terms! but how to handle them?
normalise({cons,_,Head,Tail}) ->
    [normalise(Head)|normalise(Tail)];
normalise({tuple,_,Args}) ->
    list_to_tuple(normalise_list(Args));
%% Atom dot-notation, as in 'foo.bar.baz'
normalise({record_field,_,_,_}=A) ->
    case package_segments(A) of
    error -> erlang:fault({badarg, A});
    As -> list_to_atom(packages:concat(As))
    end;
%% Special case for unary +/-.
normalise({op,_,'+',{char,_,I}}) -> I;
normalise({op,_,'+',{integer,_,I}}) -> I;
normalise({op,_,'+',{float,_,F}}) -> F;
normalise({op,_,'-',{char,_,I}}) -> -I;        %Weird, but compatible!
normalise({op,_,'-',{integer,_,I}}) -> -I;
normalise({op,_,'-',{float,_,F}}) -> -F;
normalise(X) -> erlang:fault({badarg, X}).

normalise_list([H|T]) ->
    [normalise(H)|normalise_list(T)];
normalise_list([]) ->
    [].

abstract(T) when is_integer(T) -> {integer,0,T};
abstract(T) when is_float(T) -> {float,0,T};
abstract(T) when is_atom(T) -> {atom,0,T};
abstract([]) -> {nil,0};
abstract(B) when is_binary(B) ->
    {bin, 0, lists:map(fun(Byte) ->
                   {bin_element, 0,
                {integer, 0, Byte}, default, default}
               end,
               binary_to_list(B))};
abstract([C|T]) when is_integer(C), 0 =< C, C < 256 ->
    abstract_string(T, [C]);
abstract([H|T]) ->
    {cons,0,abstract(H),abstract(T)};
abstract(Tuple) when is_tuple(Tuple) ->
    {tuple,0,abstract_list(tuple_to_list(Tuple))}.

abstract_string([C|T], String) when is_integer(C), 0 =< C, C < 256 ->
    abstract_string(T, [C|String]);
abstract_string([], String) ->
    {string, 0, lists:reverse(String)};
abstract_string(T, String) ->
    not_string(String, abstract(T)).

not_string([C|T], Result) ->
    not_string(T, {cons, 0, {integer, 0, C}, Result});
not_string([], Result) ->
    Result.

abstract_list([H|T]) ->
    [abstract(H)|abstract_list(T)];
abstract_list([]) ->
    [].

%%% abstract/2 keeps the line number
abstract(T, Line) when is_integer(T) -> {integer,Line,T};
abstract(T, Line) when is_float(T) -> {float,Line,T};
abstract(T, Line) when is_atom(T) -> {atom,Line,T};
abstract([], Line) -> {nil,Line};
abstract(B, Line) when is_binary(B) ->
    {bin, Line, lists:map(fun(Byte) ->
                   {bin_element, Line,
                {integer, Line, Byte}, default, default}
               end,
               binary_to_list(B))};
abstract([C|T], Line) when is_integer(C), 0 =< C, C < 256 ->
    abstract_string(T, [C], Line);
abstract([H|T], Line) ->
    {cons,Line,abstract(H, Line),abstract(T, Line)};
abstract(Tuple, Line) when tuple(Tuple) ->
    {tuple,Line,abstract_list(tuple_to_list(Tuple), Line)}.

abstract_string([C|T], String, Line) when is_integer(C), 0 =< C, C < 256 ->
    abstract_string(T, [C|String], Line);
abstract_string([], String, Line) ->
    {string, Line, lists:reverse(String)};
abstract_string(T, String, Line) ->
    not_string(String, abstract(T, Line), Line).

not_string([C|T], Result, Line) ->
    not_string(T, {cons, Line, {integer, Line, C}, Result}, Line);
not_string([], Result, _Line) ->
    Result.

abstract_list([H|T], Line) ->
    [abstract(H, Line)|abstract_list(T, Line)];
abstract_list([], _Line) ->
    [].

%% tokens(AbsTerm) -> [Token]
%% tokens(AbsTerm, More) -> [Token]
%%  Generate a list of tokens representing the abstract term.

tokens(Abs) ->
    tokens(Abs, []).

tokens({char,L,C}, More) -> [{char,L,C}|More];
tokens({integer,L,N}, More) -> [{integer,L,N}|More];
tokens({float,L,F}, More) -> [{float,L,F}|More];
tokens({atom,L,A}, More) -> [{atom,L,A}|More];
tokens({var,L,V}, More) -> [{var,L,V}|More];
tokens({string,L,S}, More) -> [{string,L,S}|More];
tokens({nil,L}, More) -> [{'[',L},{']',L}|More];
tokens({cons,L,Head,Tail}, More) ->
    [{'[',L}|tokens(Head, tokens_tail(Tail, More))];
tokens({tuple,L,[]}, More) ->
    [{'{',L},{'}',L}|More];
tokens({tuple,L,[E|Es]}, More) ->
    [{'{',L}|tokens(E, tokens_tuple(Es, pos(E), More))].

tokens_tail({cons,L,Head,Tail}, More) ->
    [{',',L}|tokens(Head, tokens_tail(Tail, More))];
tokens_tail({nil,L}, More) ->
    [{']',L}|More];
tokens_tail(Other, More) ->
    L = pos(Other),
    [{'|',L}|tokens(Other, [{']',L}|More])].

tokens_tuple([E|Es], Line, More) ->
    [{',',Line}|tokens(E, tokens_tuple(Es, pos(E), More))];
tokens_tuple([], Line, More) ->
    [{'}',Line}|More].

%% Give the relative precedences of operators.

inop_prec('=') -> {150,100,100};
inop_prec('!') -> {150,100,100};
inop_prec('orelse') -> {160,150,150};
inop_prec('andalso') -> {200,160,160};
inop_prec('==') -> {300,200,300};
inop_prec('/=') -> {300,200,300};
inop_prec('=<') -> {300,200,300};
inop_prec('<') -> {300,200,300};
inop_prec('>=') -> {300,200,300};
inop_prec('>') -> {300,200,300};
inop_prec('=:=') -> {300,200,300};
inop_prec('=/=') -> {300,200,300};
inop_prec('++') -> {400,300,300};
inop_prec('--') -> {400,300,300};
inop_prec('+') -> {400,400,500};
inop_prec('-') -> {400,400,500};
inop_prec('bor') -> {400,400,500};
inop_prec('bxor') -> {400,400,500};
inop_prec('bsl') -> {400,400,500};
inop_prec('bsr') -> {400,400,500};
inop_prec('or') -> {400,400,500};
inop_prec('xor') -> {400,400,500};
inop_prec('*') -> {500,500,600};
inop_prec('/') -> {500,500,600};
inop_prec('div') -> {500,500,600};
inop_prec('rem') -> {500,500,600};
inop_prec('band') -> {500,500,600};
inop_prec('and') -> {500,500,600};
inop_prec('#') -> {800,700,800};
inop_prec(':') -> {900,800,900};
inop_prec('.') -> {900,900,1000}.

preop_prec('catch') -> {0,100};
preop_prec('+') -> {600,700};
preop_prec('-') -> {600,700};
preop_prec('bnot') -> {600,700};
preop_prec('not') -> {600,700};
preop_prec('#') -> {700,800}.

func_prec() -> {800,700}.

max_prec() -> 1000.

c() ->
    yecc:yecc("erlide_parse.yrl", "erlide_parse.erl"),
    c:c(erlide_parse),
    erlide_parse:p("h(g) "),
    erlide_parse:p("->ok. ").

test() ->
    {ok, S} = file:read_file("test.erl"),
    Text = binary_to_list(S),
    {ok, _L, _} = erlide_scan:string(Text),
    %%io:format("~p~n===============---============~n", [L]),
    %%io:format("~p~n", [erlide_parse:parse(L)]).
    io:format("~p~n", [erlide_epp_dodger:parse_string(Text)]).

p(Str) ->
    {ok, L, _} = erlide_scan:string(Str),
    io:format("~p~n===============---============~n", [L]),
    io:format("~p~n", [erlide_parse:parse(L)]).
%    erlide_parse:parse_form(L).




