-module(erlide_cache).

-include("erlide.hrl").
-include_lib("kernel/include/file.hrl").

-export([
         check_and_renew_cached/5
         ]).

-type date() :: {{1900..2038, 1..12, 1..31}, {0..24, 0..59, 0..59}}.
-define(NO_DATE, {{1900, 1, 1}, {0, 0, 0}}).

-spec check_and_renew_cached(file:name_all(), file:name_all(), non_neg_integer(),
                             fun((any()) -> any()), boolean()) -> {'cached'|'renewed'|'dont_use_cache', any()}.

check_and_renew_cached(SourceFileName, _CacheFileName, _Version, RenewFun, false) ->
    Term = RenewFun(SourceFileName),
    {dont_use_cache, Term};
check_and_renew_cached(SourceFileName, CacheFileName, Version, RenewFun, true) ->
    ?D(check_and_renew_cached),
    case check_cached(SourceFileName, CacheFileName, Version) of
        {cache, Cached} ->
            ?D({from_cache, CacheFileName}),
            R = {cached, Cached},
            ?D(got_cached),
            R;
        {no_cache, SourceModDate} ->
            ?D(SourceModDate),
            Term = RenewFun(SourceFileName),
            ?D({renewing, CacheFileName}),
            write_cache(SourceModDate, Version, CacheFileName, Term),
            {renewed, Term}
    end.

-spec read_cache_date_and_version(string()) -> {date(), integer()}.
read_cache_date_and_version(CacheFileName) ->
    ?D("check "++CacheFileName),
    case file:open(CacheFileName, [read, binary]) of
        {ok, F} ->
            {ok, BinDateAndVersion} = file:read(F, 7),
            file:close(F),
            <<BinDate:5/binary, Version:16/integer-big>> = BinDateAndVersion,
            {bin_to_date(BinDate), Version};
        _Err ->
            {?NO_DATE, 0}
    end.

check_cached(none, CacheFileName, Version) ->
    ?D(CacheFileName),
    SourceModDate = ?NO_DATE,
    check_cached_aux(SourceModDate, CacheFileName, Version);
check_cached(SourceFileName, CacheFileName, Version) ->
    ?D({SourceFileName, CacheFileName}),
    SourceModDate = case file:read_file_info(SourceFileName) of
                        {ok, Info} ->
                            Info#file_info.mtime;
                        {error, enoent} ->
                            ?NO_DATE
                    end,
    ?D(SourceModDate),
    check_cached_aux(SourceModDate, CacheFileName, Version).

check_cached_aux(SourceModDate, CacheFileName, Version) ->
    ?D({SourceModDate, CacheFileName, Version}),
    SV = read_cache_date_and_version(CacheFileName),
    ?D(SV),
    case same_date_and_version(SV, {SourceModDate, Version}) of
        true ->
            {cache, read_cache(CacheFileName)};
        _ ->
            {no_cache, SourceModDate}
    end.

same_date_and_version({{Date, {H, M, S1}}, V}, {{Date, {H, M, S2}}, V}) ->
    S1 div 2 =:= S2 div 2;
same_date_and_version(_, _) ->
    false.

%% renew_cached(SourceFileName, CacheFileName, Version, Term) ->
%%     SourceModDate = case file:read_file_info(SourceFileName) of
%%                         {ok, Info} ->
%%                             Info#file_info.mtime;
%%                         {error, enoent} ->
%%                             {{1900, 1, 1}, {0, 0, 0}}
%%                     end,
%%     ?D(SourceFileName),
%%     renew_cache(SourceModDate, Version, CacheFileName, Term).

read_cache(CacheFileName) ->
    {ok, B} = file:read_file(CacheFileName),
    ?D(CacheFileName),
    <<_:5/binary, _:16/integer-big, BinTerm/binary>> = B,
    binary_to_term(BinTerm).

-spec bin_to_date(binary()) -> date().
bin_to_date(<<Y:15/integer-big, Mo:4, D:5, H:5, M:6, S:5>>) ->
    {{Y, Mo, D}, {H, M, S*2}}.

-spec date_to_bin(date()) -> binary().
date_to_bin({{Y, Mo, D}, {H, M, S}}) ->
    <<Y:15/integer-big, Mo:4, D:5, H:5, M:6, (S div 2):5>>.

-spec write_cache(date(), non_neg_integer(), file:name_all(), any()) -> 'ok'.
write_cache(SourceFileModDate, Version, CacheFileName, Term) ->
    ?D({write, CacheFileName, SourceFileModDate, Version}),
    BinDate = date_to_bin(SourceFileModDate),
    B = term_to_binary(Term, [compressed]),
    _Delete = file:delete(CacheFileName),
    touch_path(CacheFileName),
    _Write = file:write_file(CacheFileName, <<BinDate/binary, Version:16/integer-big, B/binary>>),
    ?D(_Write).

touch_path(Path) ->
    touch_path(filename:split(Path), []).

touch_path([], _) ->
    ok;
touch_path([_], _) ->
    ok;
touch_path([H|T], Acc) ->
    Crt = filename:join(Acc, H),
    _Err = file:make_dir(Crt),
    touch_path(T, Crt).

