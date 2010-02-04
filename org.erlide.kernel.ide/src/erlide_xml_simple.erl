-module(erlide_xml_simple).

-export([simple_xml/1]).

%%%%%%%%%%%%%%%%%%%%
%% author: Ulf Wiger

-define(WHITESPACE(H), H==$\s; H==$\r; H==$\n; H==$\t).
-define(bad_xml(T), erlang:error({bad_xml, string:substr(T, 1, 25)})).

simple_xml(Bin) when is_binary(Bin) ->
    {simple_xml, simple_xml(strip(binary_to_list(Bin)), [])};
simple_xml(Str) ->
    {simple_xml, simple_xml(strip(Str), [])}.

simple_xml("<" ++ Str, Acc) ->
    {Str1, Acc1} = xml_tag(strip(Str), [], Acc),
    simple_xml(strip(Str1), Acc1);
simple_xml([], Acc) ->
    lists:reverse(Acc).

xml_tag("/>" ++ T, TagAcc, Acc) ->
    {strip(T), [{lists:reverse(TagAcc), [], []}|Acc]};
xml_tag(">" ++ Str, TagAcc, Acc) ->
    xml_content(strip(Str), lists:reverse(TagAcc), [], [], Acc);
xml_tag([H|T], TagAcc, Acc) when ?WHITESPACE(H) ->
    xml_attributes(strip(T), [], [], lists:reverse(TagAcc), Acc);
xml_tag([H|T], TagAcc, Acc) ->
    xml_tag(T, [H|TagAcc], Acc).


xml_attributes("=" ++ T, TagAcc, AAcc, Tag, Acc) ->
    xml_attr_value(strip(T), lists:reverse(TagAcc), AAcc, Tag, Acc);
xml_attributes([H|T], TagAcc, AAcc, Tag, Acc) when ?WHITESPACE(H) ->
    case strip(T) of
	"=" ++ T1 ->
	    xml_attr_value(strip(T1), lists:reverse(TagAcc), AAcc, Tag, Acc);
	_ ->
	    ?bad_xml(T)
    end;
xml_attributes([H|T], TagAcc, AAcc, Tag, Acc) ->
    xml_attributes(T, [H|TagAcc], AAcc, Tag, Acc).

xml_attr_value("\"" ++ T1, ATag, AAcc, Tag, Acc) ->
    {Str, T2} = scan_string(T1, []),
    case strip(T2) of
	"/>" ++ T3 ->
	    {strip(T3), [{Tag, [{ATag, Str}|AAcc], []}|Acc]};
	">" ++ T3 ->
	    xml_content(
	      strip(T3), Tag, [], lists:reverse(
				    [{ATag, Str}|AAcc]), Acc);
	T3 ->
	    xml_attributes(T3, [], [{ATag, Str}|AAcc], Tag, Acc)
    end;
xml_attr_value(T, _, _, _, _) ->
    ?bad_xml(T).


xml_content("</" ++ Str, Tag, CAcc, Attrs, Acc) ->
    Str1 = strip_prefix(Tag ++ ">", Str),
    {Str1, [{list_to_atom(Tag), Attrs, lists:reverse(CAcc)}|Acc]};
xml_content("<" ++ Str, Tag, CAcc, Attrs, Acc) ->
    {Str1, CAcc1} = xml_tag(Str, [], CAcc),
    xml_content(Str1, Tag, CAcc1, Attrs, Acc);
xml_content([H|T], Tag, CAcc, Attrs, Acc) ->
    xml_text(T, [H], Tag, CAcc, Attrs, Acc).

xml_text("<" ++ _ = Str, TAcc, Tag, CAcc, Attrs, Acc) ->
    case lists:reverse(strip(TAcc)) of
	[] ->
	    xml_content(Str, Tag, CAcc, Attrs, Acc);
	Txt ->
	    xml_content(Str, Tag, [{text, Txt}|CAcc],
			Attrs, Acc)
    end;
xml_text([H|T], TAcc, Tag, CAcc, Attrs, Acc) ->
    xml_text(T, [H|TAcc], Tag, CAcc, Attrs, Acc).

strip_prefix([H|T1], [H|T2]) ->
    strip_prefix(T1, T2);
strip_prefix([], T) ->
    T;
strip_prefix(_, T) ->
    ?bad_xml(T).


strip([H|T]) when ?WHITESPACE(H) ->
    strip(T);
strip(Str) ->
    Str.
    

scan_string("\"" ++ T, Acc) ->
    {lists:reverse(Acc), T};
scan_string([H|T], Acc) ->
    scan_string(T, [H|Acc]);
scan_string([], Acc) ->
    ?bad_xml(lists:reverse(Acc)).


