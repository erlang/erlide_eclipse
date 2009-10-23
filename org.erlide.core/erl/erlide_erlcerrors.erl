%% Author: jakob
%% Created: May 11, 2006
%% Description: parses error output from erlc into something more like compile:file(X, [return])

-module(erlide_erlcerrors).

% -define(DEBUG, 1).

-include("erlide.hrl").

%%
%% Include files
%%

-define(SEVERITY_ERROR, 0).
-define(SEVERITY_WARNING, 1).
	
%%
%% Exported Functions
%%
-export([convert_erlc_errors/1]).

%%
%% API Functions
%%
convert_erlc_errors(Text) -> 
    ?D(Text),
    RevLines = erlide_text:split_lines(Text),
    ?D(RevLines),
    Errors = convert_errors(RevLines, []),
    ?D(Errors),
    Rel = sofs:relation(Errors),
    Fam = sofs:relation_to_family(Rel),
    Result = sofs:to_external(Fam),
    ?D(Result),
    Result.

%%
%% Local Functions
%%

convert_errors([], Acc) ->
    Acc;
convert_errors([{_Offset, PossibleErrorLine} | Rest], Acc) ->
    case convert_error(PossibleErrorLine) of
        Tuple when is_tuple(Tuple) ->
            convert_errors(Rest, [Tuple | Acc]);
        _ ->
            convert_errors(Rest, Acc)
    end.

convert_error(PossibleErrorLine) ->
    case string:tokens(PossibleErrorLine, ":") of
        [File, LineNoS, ErrText] ->
            convert_error(File, LineNoS, ErrText, ?SEVERITY_ERROR);
        [File, LineNoS, " Warning", WarnText] ->
            convert_error(File, LineNoS, WarnText, ?SEVERITY_WARNING);
        _ ->
            not_an_error_line
    end.
        
convert_error(File, [Digit | _] = LineNoS, Text, Severity) when $0 =< Digit, Digit =< $9 ->
    case lists:suffix(".erl", File) orelse lists:suffix(".hrl", File) of
        true ->
            {File, {list_to_integer(LineNoS), module, string:strip(Text, right, $\n), Severity}};
        false ->
            not_an_error_line
    end;
convert_error(_, _, _, _) ->
    not_an_error_line.


