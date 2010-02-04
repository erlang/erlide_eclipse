-module(erlide_otp_docs_x).

%% a try to use the OTP sgml docs, but conversion to xml is needed and
%% the conversion can't be done automatically (but I may be wrong)

-compile(export_all).

-record(module_doc, {name, summary, description, sections, funcs, seealso}).
-record(func_doc, {name, arity, sign, summary, type, desc}).

g() ->
    g("h:/stdlib/io_lib.xml").

g(FN) ->
    {ok, Bin} = file:read_file(FN),
    %% skip until after comment
    L = binary_to_list(Bin),
    L1 = case string:str(L, "-->") of
	     0 ->
		 L;
	     N ->
		 string:substr(L, N+3)
	 end,
    {simple_xml, [Data]} = erlide_xml_simple:simple_xml(L1),
    MD = convert(Data),
    lists:flatten(format_func_doc(get_func_doc(MD, "write", 1))).


convert(L) when is_list(L) ->
    lists:flatmap(fun convert/1, L);
convert({erlref, [], Data}) ->
    mk_md(convert(Data));
convert({module, _, [{text, Name}]}) ->
    [{module, Name}];
convert({modulesummary, _, [{text, Name}]}) ->
    [{summary, Name}];
convert({description, _, C}) ->
    [{description, C}];
convert({section, _, C}) ->
    [{section, C}];
convert({funcs, _, C}) ->
    [{funcs, convert_fun(C)}];
convert(_) ->
    [].

convert_fun(L) when is_list(L) ->
    lists:map(fun convert_fun/1, L);
convert_fun({func, _, Data}) ->
    mk_fd(convert_fun(Data));
convert_fun({name, _, [{text, Name}]}) ->
    {name, Name};
convert_fun({Tag, _, C}) ->
    {Tag, C}.


    
mk_md(L) ->
    {value, {_, Name}} = lists:keysearch(module, 1, L),
    {value, {_, Summary}} = lists:keysearch(summary, 1, L),
    {value, {_, Desc}} = lists:keysearch(description, 1, L),
    {value, {_, Funcs}} = lists:keysearch(funcs, 1, L), %% gets only first one!
    Sects = [S || {section, S} <- L],
    #module_doc{name=Name, summary=Summary, description=Desc, sections=Sects, funcs=Funcs }.

mk_fd(L) ->
    Sign = [S || {name, S} <- L],
    Type = case [S || {type, S} <- L] of
	       [] ->
		   [];
	       [H] ->
		   H
	   end,
    [Summary] = [S || {fsummary, S} <- L],
    [Desc] = [S || {desc, S} <- L],
    {F, A} = get_fun_name(hd(Sign)),
    #func_doc{name=F, arity=A, sign=Sign, summary=Summary, type = Type, desc=Desc }.

get_func_doc(ModuleDoc, F, A) ->
    #module_doc{funcs=Funcs} = ModuleDoc,
    case [FX || FX <- Funcs,
		FX#func_doc.name==F, 
		FX#func_doc.arity==A] of
	[] ->
	    false;
	[Doc] ->
	    Doc
    end.


get_fun_name(Doc) ->
    {match, S1, E1} = regexp:first_match(Doc, "^[^(]+\\("),
    {match, M2} = regexp:matches(Doc, ","),
    {string:substr(Doc, S1, E1-1),length(M2)+1}.
    
format_func_doc(false) ->
    "";
format_func_doc(#func_doc{name=N, arity=A, summary=M, sign=S, type=_T, desc=_D}) ->
    ["<H1>",N,"/",integer_to_list(A),"</H1>",
     "<div>", to_html(M), "</div>",
     [["<strong><code>", X, "</code></strong>"] || X <-S],
%%     [["<p><b>", X, "</b></p>"] || X <-T],
     "<p> bla bla </p>"
    ].

to_html(L) when is_list(L) ->
    [to_html(X) || X<-L];
to_html({text, T}) ->
    T;
to_html(_O) ->
    "?".


