%% Author: jakob
%% Created: 18 dec 2010
%% Description: TODO: Add description to erlide_np_records
-module(erlide_np_records).

%%
%% Include files
%%

-include("erlide_scanner.hrl").
-include("erlide_search_server.hrl").

%%
%% Exported Functions
%%
-export([check_fields/2]).

%%
%% API Functions
%%


% check refs to record fields
% 

check_fields(Tokens, RecordName) ->
    check_fields(record_name, Tokens, RecordName, [], '').

%%
%% Local Functions
%%

check_fields(_State, [], _RecordName, Fields, _PrevRecordName) ->
    {[], Fields};
check_fields(_State, [#token{kind='}'} | Rest], _RecordName, Fields, _PrevRecordName) ->
    {Rest, Fields};
check_fields(_State, [#token{kind='#'} | Rest], RecordName, Fields, PrevRecordName) -> % 1
    check_fields(record_want_name, Rest, RecordName, Fields, PrevRecordName);
check_fields(record_want_name, [#token{kind=atom, value=NewRecordName} | Rest], RecordName, Fields, _PrevRecordName) -> % 2
    check_fields(record_name, Rest, NewRecordName, Fields, RecordName);
check_fields(record_want_name, [#token{kind=macro, value=NewRecordName} | Rest], RecordName, Fields, _PrevRecordName) -> % 2
    check_fields(record_name, Rest, NewRecordName, Fields, RecordName);
check_fields(record_want_name, [#token{kind='?'} | Rest], RecordName, Fields, _PrevRecordName) -> % 2
    check_fields(record_name, Rest, '?', Fields, RecordName);
check_fields(record_name, [#token{kind=Dot} | Rest], RecordName, Fields, PrevRecordName) % 3 
  when Dot=:='.'; Dot=:=dot->
    check_fields(record_want_dot_field, Rest, RecordName, Fields, PrevRecordName);
check_fields(record_want_dot_field, [#token{kind=atom, value=FieldName, offset=Offset, length=Length} | Rest],
             RecordName, Fields, _PrevRecordName) -> % 4
    NewFields = [{Offset, Length, #record_field_ref{field=FieldName, record=RecordName}} | Fields],
    {Rest, NewFields};
check_fields(record_name, [#token{kind='{'} | Rest], RecordName, Fields, PrevRecordName) -> % 5
    {NewRest, NewFields} = check_fields(record_want_field, Rest, RecordName, Fields, PrevRecordName),
    check_fields(no_record, NewRest, PrevRecordName, NewFields, PrevRecordName);
check_fields(State, [#token{kind='{'} | Rest], RecordName, Fields, PrevRecordName) -> % 6
    {NewRest, NewFields} = check_fields(no_record, Rest, RecordName, Fields, PrevRecordName),
    check_fields(State, NewRest, RecordName, NewFields, PrevRecordName);
check_fields(record_want_field, [#token{kind=atom, value=FieldName, offset=Offset, length=Length} | Rest],
             RecordName, Fields, PrevRecordName) -> % 7
    NewFields = [{Offset, Length, #record_field_ref{field=FieldName, record=RecordName}} | Fields],
    check_fields(record_field, Rest, RecordName, NewFields, PrevRecordName);
check_fields(no_record, [#token{kind=','} | Rest], RecordName, Fields, PrevRecordName) -> % 8
    check_fields(record_want_field, Rest, RecordName, Fields, PrevRecordName);
check_fields(record_field, [#token{kind='='} | Rest], RecordName, Fields, PrevRecordName) -> % 9
    check_fields(no_record, Rest, RecordName, Fields, PrevRecordName);
check_fields(_State, [_ | Rest], RecordName, Fields, PrevRecordName) -> % 10
    check_fields(no_record, Rest, RecordName, Fields, PrevRecordName).
