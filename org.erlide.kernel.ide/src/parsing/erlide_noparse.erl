%% Description: Split source files into functions and clauses and collects references
%% Author: jakob
%% Created: Mar 23, 2006

-module(erlide_noparse).

%%
%% Exported Functions
%%

%% called from Java
-export([initial_parse/5, reparse/2, remove_cache_files/2]).

%% called from Erlang
-export([get_module_refs/4]).

%%
%% Include files
%%

%% -define(DEBUG, 1).

-define(CACHE_VERSION, 29).
-define(SERVER, erlide_noparse).

-include("erlide.hrl").
-include("erlide_noparse.hrl").
-include("erlide_scanner.hrl").
-include("erlide_scanner_server.hrl").
-include("erlide_search.hrl").

%%
%% API Functions
%%

-spec initial_parse(atom(), string(), string(), boolean(), boolean()) ->
          {ok, #model{}, cached | renewing | dont_use_cache, [#ref{}]}
              | {error, term(), term()}.

initial_parse(ScannerName, ModuleFileName, StateDir, UseCache,
              UpdateSearchServer) ->
    try
        BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
        RefsFileName = BaseName ++ ".refs",
        RenewFun = fun(_F) ->
                           Tokens = get_tokens(ScannerName, ModuleFileName, StateDir),
                           {Model, Refs} =
                               do_parse(ScannerName, RefsFileName, Tokens, StateDir,
                                        UpdateSearchServer),
                           {Model, Refs}
                   end,
        CacheFileName = BaseName ++ ".noparse",
        {Cached, {Model, Refs}} = erlide_util:check_and_renew_cached(
                                    ModuleFileName, CacheFileName, ?CACHE_VERSION,
                                    RenewFun, UseCache),
        {ok, Model, Cached, Refs}
    catch
        error:Reason ->
            {error, Reason, erlang:get_stacktrace()}
    end.

-spec reparse(atom(), boolean()) ->
          {ok, #model{}, cached |�renewing | dont_use_cache, [#ref{}]}
              | {error, term(), term()}.
reparse(ScannerName, UpdateSearchServer) ->
    try
        Tokens = erlide_scanner_server:getTokens(ScannerName),
        {Model, _Refs} = do_parse(ScannerName, "", Tokens, "", UpdateSearchServer),
        {ok, Model}
    catch
        error:Reason ->
            {error, Reason, erlang:get_stacktrace()}
    end.

-spec get_module_refs(atom(), string(), string(), boolean()) -> [#ref{}].
get_module_refs(ScannerName, ModulePath, StateDir, UpdateSearchServer) ->
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    RefsFileName = BaseName ++ ".refs",
    %% TODO: shouldn't we check that .refs is up-to-date? using renew
    %% function etc. would probably be more straight-forward...
    ?D(RefsFileName),
    case file:read_file(RefsFileName) of
        {ok, Binary} ->
            binary_to_term(Binary);
        _ ->
            {ok, _, _, Refs}�= initial_parse(ScannerName, ModulePath, StateDir,
                                             true, UpdateSearchServer),
            Refs
    end.

-spec remove_cache_files(atom(), string) -> ok |�{error, term()}.
%% remove all cache files for an erlang module in erlide
remove_cache_files(ScannerName, StateDir) ->
    BaseName = filename:join(StateDir, atom_to_list(ScannerName)),
    ScannerCacheFileName = BaseName ++ ".scan",
    file:delete(ScannerCacheFileName),
    RefsFileName = BaseName ++ ".refs",
    file:delete(RefsFileName),
    CacheFileName = BaseName ++ ".noparse",
    file:delete(CacheFileName).

%%
%% Internal functions
%%

get_tokens(ScannerName, ModuleFileName, StateDir) ->
    case whereis(ScannerName) of
        undefined ->
            {ok, InitialTextBin} = file:read_file(ModuleFileName),
            InitialText = binary_to_list(InitialTextBin),
            {{_Cached, Module}, _Text} = erlide_scanner:initial_scan(ScannerName, ModuleFileName, InitialText, StateDir, true),
            erlide_scanner:get_all_tokens(Module);
        _ ->
      erlide_scanner_server:getTokens(ScannerName)
    end.

do_parse(ScannerName, RefsFileName, Tokens, StateDir, UpdateSearchServer) ->
    {Forms, Comments, References} = erlide_np:parse(Tokens),
    Model = #model{forms=Forms, comments=Comments},
    CompactModel = erlide_np_util:compact_model(Model),
    case StateDir of
        "" -> ok;
        _ ->
            file:write_file(RefsFileName, term_to_binary(References, [compressed]))
    end,
    update_search_server(UpdateSearchServer, ScannerName, References),
    {CompactModel, References}.

update_search_server(true, ScannerName, Refs) ->
    erlide_search_server:add_module_refs(ScannerName, Refs);
update_search_server(_, _, _) ->
    ok.

