%% Author: jakob
%% Created: Mar 23, 2006
%% Description: TODO: Add description to erlide_comment
-module(erlide_comment).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([toggle_comment/3]).

%-define(DEBUG, 1). %

-include("erlide.hrl").

%%
%% API Functions
%%

toggle_comment(Text, From, Length) ->
    ?D({Text, From, Length}),
    {_, _, Lines} = erlide_text:get_text_and_lines(Text, From, Length),
    ?D(Lines),
    LineF = case lists:all(fun(L) -> is_comment_line(L) end, Lines) of
                   true ->
                       fun(L) -> uncomment_line(L) end;
                   false ->
                       fun(L) -> comment_line(L) end
            end,
    lists:flatten(lists:map(LineF, Lines)).

%%
%% Local Functions
%%

is_comment_line("") -> false;
is_comment_line(" " ++ Rest) -> is_comment_line(Rest);
is_comment_line("\t" ++ Rest) -> is_comment_line(Rest);
is_comment_line("%" ++ _) -> true;
is_comment_line(_) -> false.

uncomment_line(Line) -> uncomment_line_x(Line).

uncomment_line_x("%% " ++ Rest) -> Rest;
uncomment_line_x("%%" ++ Rest) -> Rest;
uncomment_line_x("%" ++ Rest) -> Rest;
uncomment_line_x(" " ++ Rest) -> uncomment_line_x(Rest);
uncomment_line_x(Line) -> Line.

comment_line("") -> "";
comment_line(Line) -> ["%% ", Line].
