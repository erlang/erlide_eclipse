-module(erlide_cache).

-include("erlide.hrl").
-include_lib("kernel/include/file.hrl").

-export([
         check_and_renew_cached/5
         ]).

check_and_renew_cached(SourceFileName, CacheFileName, Version,
           RenewFun, UseCache) ->
    ?D({SourceFileName, CacheFileName, Version,
           RenewFun, UseCache}),
    check_and_renew_cached(SourceFileName, CacheFileName, Version,
         RenewFun, fun(D) -> D end, UseCache).

check_and_renew_cached(SourceFileName, _CacheFileName, _Version,
                       RenewFun, _CachedFun, false) ->
    Term = RenewFun(SourceFileName),
    {dont_use_cache, Term};
check_and_renew_cached(SourceFileName, CacheFileName,
                       Version, RenewFun, CachedFun,
                       true) ->
    ?D(check_and_renew_cached),
    case check_cached(SourceFileName, CacheFileName, Version) of
        {cache, Cached} ->
            ?D({from_cache, CacheFileName}),
            R = {cached, CachedFun(Cached)},
            ?D(got_cached),
            R;
        {no_cache, SourceModDate} ->
            ?D(SourceModDate),
            Term = RenewFun(SourceFileName),
            ?D({renewing, CacheFileName}),
            renew_cache(SourceModDate, Version, CacheFileName, Term),
            {renewed, Term}
    end.

read_cache_date_and_version(CacheFileName) ->
    case file:open(CacheFileName, [read, binary]) of
        {ok, F} ->
            {ok, BinDateAndVersion} = file:read(F, 7),
            file:close(F),
            <<BinDate:5/binary, Version:16/integer-big>> = BinDateAndVersion,
            {bin_to_date(BinDate), Version};
        _ ->
            {{{0, 0, 0}, {0, 0, 0}}, 0}
    end.

check_cached(none, CacheFileName, Version) ->
    ?D(CacheFileName),
    SourceModDate = {{1900, 1, 1}, {0, 0, 0}},
    check_cached_aux(SourceModDate, CacheFileName, Version);
check_cached(SourceFileName, CacheFileName, Version) ->
    ?D({SourceFileName, CacheFileName}),
    SourceModDate = case file:read_file_info(SourceFileName) of
                        {ok, Info} ->
                            Info#file_info.mtime;
                        {error, enoent} ->
                            {{1900, 1, 1}, {0, 0, 0}}
                    end,
    ?D(SourceModDate),
    check_cached_aux(SourceModDate, CacheFileName, Version).

check_cached_aux(SourceModDate, CacheFileName, Version) ->
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

renew_cached(SourceFileName, CacheFileName, Version, Term) ->
    SourceModDate = case file:read_file_info(SourceFileName) of
                        {ok, Info} ->
                            Info#file_info.mtime;
                        {error, enoent} ->
                            {{1900, 1, 1}, {0, 0, 0}}
                    end,
    ?D(SourceFileName),
    renew_cache(SourceModDate, Version, CacheFileName, Term).

read_cache(CacheFileName) ->
    {ok, B} = file:read_file(CacheFileName),
    ?D(CacheFileName),
    <<_:5/binary, _:16/integer-big, BinTerm/binary>> = B,
    binary_to_term(BinTerm).


bin_to_date(<<Y:15/integer-big, Mo:4, D:5, H:5, M:6, S:5>>) ->
    {{Y, Mo, D}, {H, M, S*2}}.
date_to_bin({{Y, Mo, D}, {H, M, S}}) ->
    <<Y:15/integer-big, Mo:4, D:5, H:5, M:6, (S div 2):5>>.

renew_cache(SourceFileModDate, Version, CacheFileName, Term) ->
    ?D(SourceFileModDate),
    BinDate = date_to_bin(SourceFileModDate),
    B = term_to_binary(Term, [compressed]),
    _Delete = file:delete(CacheFileName),
    touch_path(CacheFileName),
    _Write = file:write_file(CacheFileName, <<BinDate/binary, Version:16/integer-big, B/binary>>),
    ?D(_Write),
    ?D(CacheFileName).

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

