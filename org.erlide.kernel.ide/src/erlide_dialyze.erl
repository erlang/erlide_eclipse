%% Author: jakob
%% Created: 17 feb 2010
%% Description: TODO: Add description to erlide_dialyze
-module(erlide_dialyze).

%%
%% Include files
%%

%% -define(DEBUG, 1).

-include("erlide.hrl").
-include_lib("kernel/include/file.hrl").

%%
%% Exported Functions
%%
-export([dialyze/5, format_warning/1, check_plt/1, get_plt_files/1, update_plt_with_additional_paths/2,
         start_dialyze/6, start_update_plt_with_additional_paths/3]).


%%
%% API Functions
%%

dialyze(Files, PltFiles, Includes, FromSource, NoCheckPLT) ->
    From = case FromSource of
               true -> src_code;
               false -> byte_code
           end,
    Plt = case PltFiles of
              [] ->
                  dialyzer_plt:new();
              [Plt1] ->
                  ?D(Plt1),
                  dialyzer_plt:from_file(Plt1);
              _ ->
                  Plts = [dialyzer_plt:from_file(F) || F <- PltFiles],
                  dialyzer_plt:merge_plts_or_report_conflicts(PltFiles, Plts)
          end,
    ?D(before),
    R = (catch do_analysis(Files, none, Plt, none, succ_typings, Includes, NoCheckPLT, From, none)),
    case R of
        {ErrorOrExit, E} when ErrorOrExit =:= 'EXIT'; ErrorOrExit =:= error ->
            {error, flat(E)};
        Result ->
            Result
    end.

start_dialyze(JPid, Files, PltFiles, Includes, FromSource, NoCheckPLT) ->
    From = case FromSource of
               true -> src_code;
               false -> byte_code
           end,
    Plt = case PltFiles of
              [] ->
                  dialyzer_plt:new();
              [Plt1] ->
                  ?D(Plt1),
                  dialyzer_plt:from_file(Plt1);
              _ ->
                  Plts = [dialyzer_plt:from_file(F) || F <- PltFiles],
                  dialyzer_plt:merge_plts_or_report_conflicts(PltFiles, Plts)
          end,
    ?D(before),
    R = (catch do_analysis(Files, none, Plt, none, succ_typings, Includes, NoCheckPLT, From, JPid)),
    case R of
        {ErrorOrExit, E} when ErrorOrExit =:= 'EXIT'; ErrorOrExit =:= error ->
            {error, flat(E)};
        Result ->
            Result
    end.


dialyze(Files, Plts, Includes, FromSource, NoCheckPLT, x) ->
    From = case FromSource of
               true -> src_code;
               false -> byte_code
           end,
    PltOption = case Plts of
                    [Plt] ->
                        {init_plt, Plt};
                    _ ->
                        {plts, Plts}
                end,
    case catch dialyzer:run([{files_rec, Files}, 
                             PltOption, 
                             {from, From},
                             {include_dirs, Includes},
                             {check_plt, not NoCheckPLT}]) of
        {_ErrorOrExit, E} ->
            {error, flat(E)};
        Result ->
            Result
    end.

format_warning(Msg) ->
    dialyzer:format_warning(Msg).

check_plt(Plt) ->
    dialyzer:run([{analysis_type, plt_check},
                  {init_plt, Plt}]).

% stolen from dialyzer.h

-define(RET_NOTHING_SUSPICIOUS, 0).
-define(RET_INTERNAL_ERROR, 1).
-define(RET_DISCREPANCIES, 2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% some parts stolen from dialyzer_cl:plt_common/3

update_plt_with_additional_paths(FileName, Paths) ->
    ?D(Paths),
    case update_plt(FileName, Paths, [], []) of
        {differ, Md5, DiffMd5, ModDeps} ->
            ?D({differ, Md5, DiffMd5, ModDeps}),
            %%             report_failed_plt_check(Opts, DiffMd5),
            {AnalFiles, _RemovedMods, ModDeps1} = 
                expand_dependent_modules(Md5, DiffMd5, ModDeps, Paths),
            Plt = clean_plt(FileName, sets:from_list([])),
            case AnalFiles =:= [] of
                true ->
                    %% Only removed stuff. Just write the PLT.
                    dialyzer_plt:to_file(FileName, Plt, ModDeps, 
                                         {Md5, ModDeps}),
                    [];
                %%                     {?RET_NOTHING_SUSPICIOUS, []};
                false ->
                    ?D({AnalFiles, FileName, ModDeps1}),
                    do_analysis(AnalFiles, FileName, Plt, {Md5, ModDeps1}, plt_build, false)
            end;
        ok ->
            %%             case Opts#options.output_plt of
            %%                 none -> ok;
            %%                 OutPlt ->
            %%                     {ok, Binary} = file:read_file(InitPlt),
            %%                     file:write_file(OutPlt, Binary)
            %%             end,
            %%             case Opts#options.report_mode of
            %%                 quiet -> ok;
            %%                 _ -> io:put_chars(" yes\n")
            %%             end,
            [];
        %%             {?RET_NOTHING_SUSPICIOUS, []};
        {old_version, Md5} ->
            PltInfo = {Md5, dict:new()},
            Files = [F || {F, _} <- Md5],
            do_analysis(Files, FileName, dialyzer_plt:new(), PltInfo, plt_build, false);
        {error, no_such_file} ->
            Msg = io_lib:format("Could not find the PLT: ~s\n~s",
                                [FileName, default_plt_error_msg()]),
            error(Msg);
        {error, not_valid} ->
            Msg = io_lib:format("The file: ~s is not a valid PLT file\n~s",
                                [FileName, default_plt_error_msg()]),
            error(Msg);
        {error, read_error} ->
            Msg = io_lib:format("Could not read the PLT: ~s\n~s",
                                [FileName, default_plt_error_msg()]),
            error(Msg);
        {error, {no_file_to_remove, F}} ->
            Msg = io_lib:format("Could not remove the file ~s from the PLT: ~s\n",
                                [F, FileName]),
            error(Msg)
    end.

start_update_plt_with_additional_paths(JPid, FileName, Paths) ->
    ?D(Paths),
    case update_plt(FileName, Paths, [], []) of
        {differ, Md5, DiffMd5, ModDeps} ->
            ?D({differ, Md5, DiffMd5, ModDeps}),
            %%             report_failed_plt_check(Opts, DiffMd5),
            {AnalFiles, _RemovedMods, ModDeps1} = 
                expand_dependent_modules(Md5, DiffMd5, ModDeps, Paths),
            Plt = clean_plt(FileName, sets:from_list([])),
            case AnalFiles =:= [] of
                true ->
                    %% Only removed stuff. Just write the PLT.
                    dialyzer_plt:to_file(FileName, Plt, ModDeps, 
                                         {Md5, ModDeps}),
                    [];
                %%                     {?RET_NOTHING_SUSPICIOUS, []};
                false ->
                    ?D({AnalFiles, FileName, ModDeps1}),
                    do_analysis(AnalFiles, FileName, Plt, {Md5, ModDeps1}, plt_build, JPid)
            end;
        ok ->
            [];
        {old_version, Md5} ->
            PltInfo = {Md5, dict:new()},
            Files = [F || {F, _} <- Md5],
            do_analysis(Files, FileName, dialyzer_plt:new(), PltInfo, plt_build, JPid);
        {error, no_such_file} ->
            Msg = io_lib:format("Could not find the PLT: ~s\n~s",
                                [FileName, default_plt_error_msg()]),
            error(Msg);
        {error, not_valid} ->
            Msg = io_lib:format("The file: ~s is not a valid PLT file\n~s",
                                [FileName, default_plt_error_msg()]),
            error(Msg);
        {error, read_error} ->
            Msg = io_lib:format("Could not read the PLT: ~s\n~s",
                                [FileName, default_plt_error_msg()]),
            error(Msg);
        {error, {no_file_to_remove, F}} ->
            Msg = io_lib:format("Could not remove the file ~s from the PLT: ~s\n",
                                [F, FileName]),
            error(Msg)
    end.

get_plt_files(PltFiles) ->
    Files = string:tokens(PltFiles, ","),
    case get_plt_files(Files, []) of
        [L | _]=R when is_list(L) ->
            {ok, R};
        _ ->
            no_files_found
    end.

%%
%% Local Functions
%%

flat({{dialyzer_error, E}, _}) ->
    flat(E);
flat({dialyzer_error, E}) ->
    flat(E);
flat(L) ->
    lists:flatten(L).

get_plt_files([], Acc) -> 
    lists:reverse(Acc);
get_plt_files([File | Rest], Acc) ->
    case filename:extension(File) of
        ".plt" ->
            get_plt_files(Rest, [File | Acc]);
        _ ->
            case file:read_file(File) of
                {ok, B} ->
                    L = erlide_util:split_lines(binary_to_list(B)),
                    get_plt_files(L ++ Rest, Acc);
                _ ->
                    get_plt_files(Rest, [File | Acc])
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% stolen from dialyzer.hrl

%%--------------------------------------------------------------------
%% Warning classification
%%--------------------------------------------------------------------

-define(WARN_RETURN_NO_RETURN, warn_return_no_exit).
-define(WARN_RETURN_ONLY_EXIT, warn_return_only_exit).
-define(WARN_NOT_CALLED, warn_not_called).
-define(WARN_NON_PROPER_LIST, warn_non_proper_list).
-define(WARN_FUN_APP, warn_fun_app).
-define(WARN_MATCHING, warn_matching).
-define(WARN_OPAQUE, warn_opaque).
-define(WARN_FAILING_CALL, warn_failing_call).
-define(WARN_BIN_CONSTRUCTION, warn_bin_construction).
-define(WARN_CONTRACT_TYPES, warn_contract_types).
-define(WARN_CONTRACT_SYNTAX, warn_contract_syntax).
-define(WARN_CONTRACT_NOT_EQUAL, warn_contract_not_equal).
-define(WARN_CONTRACT_SUBTYPE, warn_contract_subtype).
-define(WARN_CONTRACT_SUPERTYPE, warn_contract_supertype).
-define(WARN_CONTRACT_RANGE, warn_contract_range).
-define(WARN_CALLGRAPH, warn_callgraph).
-define(WARN_UNMATCHED_RETURN, warn_umatched_return).
-define(WARN_RACE_CONDITION, warn_race_condition).
-define(WARN_BEHAVIOUR, warn_behaviour).

%%
%% The following type has double role:
%%   1. It is the set of warnings that will be collected.
%%   2. It is also the set of tags for warnings that will be returned.
%%
-type dial_warn_tag() :: ?WARN_RETURN_NO_RETURN | ?WARN_RETURN_ONLY_EXIT
| ?WARN_NOT_CALLED | ?WARN_NON_PROPER_LIST
| ?WARN_MATCHING | ?WARN_OPAQUE | ?WARN_FUN_APP
| ?WARN_FAILING_CALL | ?WARN_BIN_CONSTRUCTION
| ?WARN_CONTRACT_TYPES | ?WARN_CONTRACT_SYNTAX
| ?WARN_CONTRACT_NOT_EQUAL | ?WARN_CONTRACT_SUBTYPE
| ?WARN_CONTRACT_SUPERTYPE | ?WARN_CALLGRAPH
| ?WARN_UNMATCHED_RETURN | ?WARN_RACE_CONDITION
| ?WARN_BEHAVIOUR | ?WARN_CONTRACT_RANGE.

%%
%% This is the representation of each warning as they will be returned
%% to dialyzer's callers
%%
-type file_line()    :: {file:filename(), non_neg_integer()}.
-type dial_warning() :: {dial_warn_tag(), file_line(), {atom(), [term()]}}.

-record(analysis, {analysis_pid            :: pid(),
type       = succ_typings, %  :: anal_type(),
defines    = [],        % :: [dial_define()],
doc_plt                         :: dialyzer_plt:plt(),
files          = []         :: [file:filename()],
include_dirs   = []         :: [file:filename()],
start_from     = byte_code,  %    :: start_from(),
plt                             :: dialyzer_plt:plt(),
use_contracts  = true           :: boolean(),
race_detection = false      :: boolean(),
behaviours_chk = false          :: boolean(),
callgraph_file = ""             :: file:filename()}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% stolen from dialyzer_plt

-type file_md5() :: {file:filename(), binary()}.
%%-type plt_info() :: {[file_md5()], dict()}.

-record(file_plt, {version = ""                :: string(),
                   file_md5_list = []          :: [file_md5()],
                   info = dict:new()           :: dict(),
                   contracts = dict:new()      :: dict(),
                   types = dict:new()          :: dict(),
                   exported_types = sets:new() :: set(),
                   mod_deps                    :: mod_deps(),
                   implementation_md5 = []     :: [file_md5()]}).

-type mod_deps() :: dict().
-type err_rsn() :: 'not_valid' | 'no_such_file' | 'read_error'.
-type md5_diff()    :: [{'differ', atom()} | {'removed', atom()}].
-type check_error() :: err_rsn() | {'no_file_to_remove', file:filename()}.

-spec update_plt(file:filename(), [file:filename()], [file:filename()], [file:filename()]) ->
          'ok'
              | {'error', check_error()}
              | {'differ', [file_md5()], md5_diff(), mod_deps()}
              | {'old_version', [file_md5()]}.

update_plt(FileName, Paths, RemoveFiles, AddFiles) ->
    case get_record_from_file(FileName) of
        {ok, #file_plt{file_md5_list = Md5, mod_deps = ModDeps} = Rec} ->
            case check_version(Rec) of
                ok ->
                    case compute_new_md5(Md5, Paths, RemoveFiles, AddFiles) of
                        ok ->
                            ok;
                        {differ, NewMd5, DiffMd5} ->
                            {differ, NewMd5, DiffMd5, ModDeps};
                        {error, _What} = Err -> Err
                    end;
                error ->
                    case compute_new_md5(Md5, Paths, RemoveFiles, AddFiles) of
                        ok -> {old_version, Md5};
                        {differ, NewMd5, _DiffMd5} -> {old_version, NewMd5};
                        {error, _What} = Err -> Err
                    end
            end;
        Error -> Error
    end.

-define(VSN, "2.4.2").

check_version(#file_plt{version = ?VSN, implementation_md5 = ImplMd5}) ->
    case compute_new_md5(ImplMd5, [], [], []) of
        ok -> ok;
        {differ, _, _} -> error;
        {error, _} -> error
    end;
check_version(#file_plt{}) -> error.

get_record_from_file(FileName) ->
    case file:read_file(FileName) of
        {ok, Bin} ->
            try binary_to_term(Bin) of
                #file_plt{} = FilePLT -> {ok, FilePLT};
                _ -> {error, not_valid}
            catch
                _:_ -> {error, not_valid}
            end;
        {error, enoent} ->
            {error, no_such_file};
        {error, _} ->
            {error, read_error}
    end.

compute_new_md5(Md5, Paths, [], []) ->
    compute_new_md5_1(Md5, Paths, [], []);
compute_new_md5(Md5, Paths, RemoveFiles0, AddFiles0) ->
    %% Assume that files are first removed and then added. Files that
    %% are both removed and added will be checked for consistency in the
    %% normal way. If they have moved, we assume that they differ.
    RemoveFiles = RemoveFiles0 -- AddFiles0,
    AddFiles = AddFiles0 -- RemoveFiles0,
    InitDiffList = init_diff_list(RemoveFiles, AddFiles),
    case init_md5_list(Md5, RemoveFiles, AddFiles) of
        {ok, NewMd5} -> compute_new_md5_1(NewMd5, Paths, [], InitDiffList);
        {error, _What} = Error -> Error
    end.

compute_new_md5_1([{File0, Md5} = Entry|Entries], Paths, NewList, Diff) ->
    File = find_file(Paths, File0),
    case compute_md5_from_file(File) of
        Md5 -> compute_new_md5_1(Entries, Paths, [Entry|NewList], Diff);
        file_not_found ->
            ?D(Entry),
            compute_new_md5_1(Entries, Paths, [Entry|NewList], Diff);
        NewMd5 ->
            ?D(File),
            ModName = beam_file_to_module(File),
            compute_new_md5_1(Entries, Paths, [{File, NewMd5}|NewList], [{differ, ModName}|Diff])
    end;
compute_new_md5_1([], _Paths, _NewList, []) ->
    ok;
compute_new_md5_1([], _Paths, NewList, Diff) ->
    {differ, lists:keysort(1, NewList), Diff}.

%% -spec compute_implementation_md5() -> [file_md5()].
%% 
%% compute_implementation_md5() ->
%%     Dir = code:lib_dir(hipe),
%%     Files1 = ["erl_bif_types.beam", "erl_types.beam"],
%%     Files2 = [filename:join([Dir, "ebin", F]) || F <- Files1],
%%     compute_md5_from_files(Files2, []).
%% 
%% -spec compute_md5_from_files([file:filename()], [file:filename()]) -> [file_md5()].
%% 
%% compute_md5_from_files(Files, Paths) ->
%%     lists:keysort(1, [{F, compute_md5_from_file(F, Paths)} || F <- Files]).

compute_md5_from_file(File) ->
    case filelib:is_regular(File) of
        false ->
            file_not_found;
        true ->
            case dialyzer_utils:get_abstract_code_from_beam(File) of
                error ->
                    Msg = io_lib:format("Could not get abstract code for file: ~s (please recompile it with +debug_info)\n", [File]),
                    throw({dialyzer_error, Msg});
                {ok, Abs} ->
                    erlang:md5(term_to_binary(Abs))
            end
    end.

find_file(Paths, File) ->
    find_file(Paths, File, filename:basename(File)).

find_file([], File, _Name) ->
    File;
find_file([Dir | Rest], File, Name) ->
    NewFile = filename:join([Dir, Name]),
    case filelib:is_regular(NewFile) of
        true ->
            NewFile;
        false ->
            find_file(Rest, File, Name)
    end.

init_diff_list(RemoveFiles, AddFiles) ->
    RemoveSet0 = sets:from_list([beam_file_to_module(F) || F <- RemoveFiles]),
    AddSet0 = sets:from_list([beam_file_to_module(F) || F <- AddFiles]),
    DiffSet = sets:intersection(AddSet0, RemoveSet0),
    RemoveSet = sets:subtract(RemoveSet0, DiffSet),
    %% Added files and diff files will appear as diff files from the md5 check.
    [{removed, F} || F <- sets:to_list(RemoveSet)].

init_md5_list(Md5, RemoveFiles, AddFiles) ->
    Files = [{remove, F} || F <- RemoveFiles] ++ [{add, F} || F <- AddFiles],
    DiffFiles = lists:keysort(2, Files),
    Md5Sorted = lists:keysort(1, Md5),
    init_md5_list_1(Md5Sorted, DiffFiles, []).

init_md5_list_1([{File, _Md5}|Md5Left], [{remove, File}|DiffLeft], Acc) ->
    init_md5_list_1(Md5Left, DiffLeft, Acc);
init_md5_list_1([{File, _Md5} = Entry|Md5Left], [{add, File}|DiffLeft], Acc) ->
    init_md5_list_1(Md5Left, DiffLeft, [Entry|Acc]);
init_md5_list_1([{File1, _Md5} = Entry|Md5Left] = Md5List,
                [{Tag, File2}|DiffLeft] = DiffList, Acc) ->
    case File1 < File2 of
        true -> init_md5_list_1(Md5Left, DiffList, [Entry|Acc]);
        false ->
            %% Just an assert.
            true = File1 > File2,
            case Tag of
                add -> init_md5_list_1(Md5List, DiffLeft, [{File2, <<>>}|Acc]);
                remove -> {error, {no_file_to_remove, File2}}
            end
    end;
init_md5_list_1([], DiffList, Acc) ->
    AddFiles = [{F, <<>>} || {add, F} <- DiffList],
    {ok, lists:reverse(Acc, AddFiles)};
init_md5_list_1(Md5List, [], Acc) ->
    {ok, lists:reverse(Acc, Md5List)}.

beam_file_to_module(Filename) ->
    list_to_atom(filename:basename(Filename, ".beam")).

%% stolen from dialyzer_plt

expand_dependent_modules(Md5, DiffMd5, ModDeps, Paths) ->
    ChangedMods = sets:from_list([M || {differ, M} <- DiffMd5]),
    ?D(ChangedMods),
    RemovedMods = sets:from_list([M || {removed, M} <- DiffMd5]),
    ?D(RemovedMods),
    BigSet = sets:union(ChangedMods, RemovedMods),
    BigList = sets:to_list(BigSet),
    ?D(BigList),
    ExpandedSet = expand_dependent_modules_1(BigList, BigSet, ModDeps),
    ?D(ExpandedSet),
    NewModDeps = dialyzer_callgraph:strip_module_deps(ModDeps, BigSet),
    ?D(NewModDeps),
    AnalyzeMods = sets:subtract(ExpandedSet, RemovedMods),
    ?D(AnalyzeMods),
    FilterFun = fun(File) ->
                        Mod = list_to_atom(filename:basename(File, ".beam")),
                        sets:is_element(Mod, AnalyzeMods)
                end,
    {[find_file(Paths, F) || {F, _} <- Md5, FilterFun(F)],
     RemovedMods, NewModDeps}.

clean_plt(PltFile, RemovedMods) ->
    %% Clean the plt from the removed modules.
    Plt = dialyzer_plt:from_file(PltFile),
    R = sets:fold(fun(M, AccPlt) -> dialyzer_plt:delete_module(AccPlt, M) end,
                  Plt, RemovedMods),
    R.

expand_dependent_modules_1([Mod|Mods], Included, ModDeps) ->
    case dict:find(Mod, ModDeps) of
        {ok, Deps} ->
            NewDeps = sets:subtract(sets:from_list(Deps), Included), 
            case sets:size(NewDeps) =:= 0 of
                true -> expand_dependent_modules_1(Mods, Included, ModDeps);
                false -> 
                    NewIncluded = sets:union(Included, NewDeps),
                    expand_dependent_modules_1(sets:to_list(NewDeps) ++ Mods, 
                                               NewIncluded, ModDeps)
            end;
        error ->
            expand_dependent_modules_1(Mods, Included, ModDeps)
    end;
expand_dependent_modules_1([], Included, _ModDeps) ->
    Included.

-record(cl_state,
        {backend_pid                      :: pid(),
         erlang_mode     = false          :: boolean(),
         external_calls  = []             :: [mfa()],
         external_types  = []             :: [mfa()],
         legal_warnings  = ordsets:new()  :: [dial_warn_tag()],
         mod_deps        = dict:new()     :: dict(),
         output          = standard_io    :: io:device(),
         output_format   = formatted,%      :: format(),
         filename_opt    = basename,%       :: fopt(),
         output_plt      = none           :: 'none' | file:filename(),
         plt_info        = none           :: 'none' | dialyzer_plt:plt_info(),
         report_mode     = normal,%         :: rep_mode(),
         return_status= ?RET_NOTHING_SUSPICIOUS,% :: dial_ret(),
         stored_warnings = []             :: [dial_warning()],
         unknown_behaviours = []          :: [dialyzer_behaviours:behaviour()],
         progress_fun = fun(_, _) -> ok end
        }).

get_progress_fun(false) ->
    fun(_, _) ->
            ok
    end;
get_progress_fun(JPid) when is_pid(JPid) ->
    fun(W, M) ->
            JPid ! {W, M},
            ok
    end.

do_analysis(Files, FileName, Plt, PltInfo, AnalysisType, JPid) ->
    do_analysis(Files, FileName, Plt, PltInfo, AnalysisType, [], true, byte_code, JPid).

do_analysis(Files, FileName, Plt, PltInfo, AnalysisType, IncludeDirs, _NoCheckPLT, From, JPid) ->
    assert_writable(FileName),
    hipe_compile(Files, true),
    %%     report_analysis_start(Options),
    State0 = new_state(),
    State1 = init_output(State0),
    DefaultWarns =
        %%         [?WARN_RETURN_NO_RETURN, ?WARN_RETURN_ONLY_EXIT
        %%                       , ?WARN_NOT_CALLED, ?WARN_NON_PROPER_LIST
        %%                       , ?WARN_MATCHING, ?WARN_OPAQUE, ?WARN_FUN_APP
        %%                       , ?WARN_FAILING_CALL, ?WARN_BIN_CONSTRUCTION
        %%                       , ?WARN_CONTRACT_TYPES, ?WARN_CONTRACT_SYNTAX
        %%                       , ?WARN_CONTRACT_NOT_EQUAL, ?WARN_CONTRACT_SUBTYPE
        %%                       , ?WARN_CONTRACT_SUPERTYPE, ?WARN_CALLGRAPH
        %%                       , ?WARN_UNMATCHED_RETURN, ?WARN_RACE_CONDITION
        %%                       , ?WARN_BEHAVIOUR, ?WARN_CONTRACT_RANGE], 
        [?WARN_RETURN_NO_RETURN,
         ?WARN_NOT_CALLED,
         ?WARN_NON_PROPER_LIST,
         ?WARN_FUN_APP,
         ?WARN_MATCHING,
         ?WARN_OPAQUE,
         ?WARN_CALLGRAPH,
         ?WARN_FAILING_CALL,
         ?WARN_BIN_CONSTRUCTION,
         ?WARN_CALLGRAPH,
         ?WARN_CONTRACT_RANGE,
         ?WARN_CONTRACT_TYPES,
         ?WARN_CONTRACT_SYNTAX],
    DefaultWarns1 = ordsets:from_list(DefaultWarns),
    State2 = State1#cl_state{legal_warnings = DefaultWarns1,
                             output_plt = FileName,
                             plt_info = PltInfo,
                             erlang_mode = true,
                             report_mode = normal,
                             progress_fun = get_progress_fun(JPid)},
    InitAnalysis = #analysis{type = AnalysisType,
                             defines = [],
                             include_dirs = IncludeDirs,
                             files = Files,
                             start_from = From,
                             plt = Plt,
                             use_contracts = true,
                             callgraph_file = ""},
    State3 = start_analysis(State2, InitAnalysis),
    %%     {T1, _} = statistics(wall_clock),
    case JPid of
        false -> ok;
        _ ->
            %% TODO value below initializes progress monitor. How big should it be? 
            JPid ! {start, 10}
    end,
    Return = (catch cl_loop(State3)),
    %%     {T2, _} = statistics(wall_clock),
    %%     report_elapsed_time(T1, T2, Options),
    case JPid of
        false ->
            Return;
        _ ->
            JPid ! {stop, Return},
            ok
    end.

assert_writable(none) ->
    ok;
assert_writable(PltFile) ->
    case check_if_writable(PltFile) of
        true -> ok;
        false ->
            Msg = io_lib:format("    The PLT file ~s is not writable", [PltFile]),
            error(Msg)
    end.

-define(MIN_FILES_FOR_NATIVE_COMPILE, 20).

-spec hipe_compile([file:filename()], boolean()) -> 'ok'.

hipe_compile(Files, ErlangMode) ->
    NoNative = (get(dialyzer_options_native) =:= false),
    FewFiles = (length(Files) < ?MIN_FILES_FOR_NATIVE_COMPILE),
    case NoNative orelse FewFiles orelse ErlangMode of
        true -> ok;
        false ->
            case erlang:system_info(hipe_architecture) of
                undefined -> ok;
                _ ->
                    Mods = [lists, dict, gb_sets, gb_trees, ordsets, sets,
                            cerl, cerl_trees, erl_types, erl_bif_types,
                            dialyzer_analysis_callgraph, dialyzer_codeserver,
                            dialyzer_dataflow, dialyzer_dep, dialyzer_plt,
                            dialyzer_succ_typings, dialyzer_typesig],
                    %%       report_native_comp(Options),
                    {_T1, _} = statistics(wall_clock),
                    native_compile(Mods),
                    {_T2, _} = statistics(wall_clock)
            %%       report_elapsed_time(_T1, _T2, Options)
            end
    end.

native_compile(Mods) ->
    case erlang:system_info(schedulers) of
        %% N when N > 1 ->
        %%   Parent = self(),
        %%   Pids = [spawn(fun () -> Parent ! {self(), hc(M)} end) || M <- Mods],
        %%   lists:foreach(fun (Pid) -> receive {Pid, Res} -> Res end end, Pids);
        _ -> % 1 ->
            lists:foreach(fun (Mod) -> hc(Mod) end, Mods)
    end.

hc(Mod) ->
    case code:ensure_loaded(Mod) of
        {module, Mod} -> ok;
        {error, sticky_directory} -> ok
    end,
    case code:is_module_native(Mod) of
        true -> ok;
        false ->
            {ok, Mod} = hipe:c(Mod),
            ok
    end.

new_state() ->
    #cl_state{}.

check_if_writable(PltFile) ->
    case filelib:is_regular(PltFile) of
        true -> is_writable_file_or_dir(PltFile);
        false ->
            case filelib:is_dir(PltFile) of
                true -> false;
                false ->
                    DirName = filename:dirname(PltFile),
                    filelib:is_dir(DirName) andalso is_writable_file_or_dir(DirName)
            end
    end.

is_writable_file_or_dir(PltFile) ->
    case file:read_file_info(PltFile) of
        {ok, #file_info{access = A}} ->
            (A =:= write) orelse (A =:= read_write);
        {error, _} ->
            false
    end.

progress(What, Msg, #cl_state{progress_fun=Fun}) ->
    Fun(progress, {self(), What, Msg}).

-define(LOG_CACHE_SIZE, 10).

%%-spec cl_loop(#cl_state{}) -> 
cl_loop(State) ->
    cl_loop(State, []).

cl_loop(State, LogCache) ->
    BackendPid = State#cl_state.backend_pid,
    receive
        {BackendPid, log, LogMsg} ->
            ?D({log, LogMsg}),
            progress(log, LogMsg, State),
            %%io:format(State#cl_state.output ,"Log: ~s\n", [LogMsg]),
            cl_loop(State, lists:sublist([LogMsg|LogCache], ?LOG_CACHE_SIZE));
        {BackendPid, warnings, Warnings} ->
            ?D({warnings, Warnings}),
            progress(warnings, Warnings, State),
            NewState = store_warnings(State, Warnings),
            cl_loop(NewState, LogCache);
        {BackendPid, unknown_behaviours, Behaviours} ->
            ?D({unknown_behaviours, Behaviours}),
            progress(unknown_behaviours, Behaviours, State),
            NewState = store_unknown_behaviours(State, Behaviours),
            cl_loop(NewState, LogCache);
        {BackendPid, done, NewPlt, _NewDocPlt} ->
            ?D(done),
            progress(done, NewPlt, State),
            return_value(State, NewPlt);
        {BackendPid, ext_calls, ExtCalls} ->
            ?D({ext_calls}), %% , ExtCalls}),
            progress(ext_calls, ExtCalls, State),
            cl_loop(State#cl_state{external_calls = ExtCalls}, LogCache);
        {BackendPid, ext_types, ExtTypes} ->
            ?D({ext_types}), %% , ExtTypes}),
            progress(ext_types, ExtTypes, State),
            cl_loop(State#cl_state{external_types = ExtTypes}, LogCache);
        {BackendPid, mod_deps, ModDeps} ->
            % ?D({mod_deps, ModDeps}),
            progress(mod_deps, dict:size(ModDeps), State),
            NewState = State#cl_state{mod_deps = ModDeps},
            cl_loop(NewState, LogCache);
        {'EXIT', BackendPid, {error, Reason}} ->
            ?D({Reason}),
            Msg = failed_anal_msg(Reason, LogCache),
            progress('EXIT', Msg, State),
            error(State, Msg);
        {'EXIT', BackendPid, Reason} when Reason =/= 'normal' ->
            ?D({Reason}),
            Msg = failed_anal_msg(io_lib:format("~P", [Reason, 12]), LogCache),
            progress('EXIT', Msg, State),
            error(State, Msg);
        _Other ->
            ?D({'_Other'}),
            %% io:format("Received ~p\n", [_Other]),
            progress(other, _Other, State),
            cl_loop(State, LogCache)
    end.


-spec failed_anal_msg(string(), [_]) -> nonempty_string().

failed_anal_msg(Reason, LogCache) ->
    Msg = "Analysis failed with error: " ++ Reason ++ "\n",
    case LogCache =:= [] of
        true -> Msg;
        false ->
            Msg ++ "Last messages in the log cache:\n  " ++ format_log_cache(LogCache)
    end.

%%
%% formats the log cache (treating it as a string) for pretty-printing
%%
format_log_cache(LogCache) ->
    Str = lists:append(lists:reverse(LogCache)),
    string:join(string:tokens(Str, "\n"), "\n  ").


-spec store_warnings(#cl_state{}, [dial_warning()]) -> #cl_state{}.

store_warnings(#cl_state{stored_warnings = StoredWarnings} = St, Warnings) ->
    St#cl_state{stored_warnings = StoredWarnings ++ Warnings}.

-spec store_unknown_behaviours(#cl_state{}, [dialyzer_behaviours:behaviour()]) -> #cl_state{}.

store_unknown_behaviours(#cl_state{unknown_behaviours = Behs} = St, Beh) ->
    St#cl_state{unknown_behaviours = Beh ++ Behs}.


return_value(State = #cl_state{erlang_mode = ErlangMode,
                               mod_deps = ModDeps,
                               output_plt = OutputPlt,
                               plt_info = PltInfo,
                               stored_warnings = StoredWarnings},
             Plt) ->
    case OutputPlt =:= none of
        true -> ok;
        false -> dialyzer_plt:to_file(OutputPlt, Plt, ModDeps, PltInfo)
    end,
    RetValue =
        case StoredWarnings =:= [] of
            true -> ?RET_NOTHING_SUSPICIOUS;
            false -> ?RET_DISCREPANCIES
        end,
    case ErlangMode of
        false ->
            print_warnings(State),
            print_ext_calls(State),
            print_ext_types(State),
            print_unknown_behaviours(State),
            %%             maybe_close_output_file(State),
            {RetValue, []};
        true -> 
            {RetValue, process_warnings(StoredWarnings)}
    end.

%%print_unknown_behaviours(#cl_state{report_mode = quiet}) ->
%%  ok;
print_unknown_behaviours(#cl_state{output = Output,
                                   external_calls = Calls,
                                   external_types = Types,
                                   stored_warnings = Warnings,
                                   unknown_behaviours = DupBehaviours,
                                   legal_warnings = LegalWarnings,
                                   output_format = Format}) ->
    case ordsets:is_element(?WARN_BEHAVIOUR, LegalWarnings)
             andalso DupBehaviours =/= [] of
        false -> ok;
        true ->
            Behaviours = lists:usort(DupBehaviours),
            case Warnings =:= [] andalso Calls =:= [] andalso Types =:= [] of
                true -> io:nl(Output); %% Need to do a newline first
                false -> ok
            end,
            case Format of
                formatted ->
                    io:put_chars(Output, "Unknown behaviours (behaviour_info(callbacks)"
                                     " does not return any specs):\n"),
                    do_print_unknown_behaviours(Output, Behaviours, "  ");
                raw ->
                    io:put_chars(Output, "%% Unknown behaviours:\n"),
                    do_print_unknown_behaviours(Output, Behaviours, "%%  ")
            end
    end.

do_print_unknown_behaviours(Output, [B|T], Before) ->
    io:format(Output, "~s~p\n", [Before,B]),
    do_print_unknown_behaviours(Output, T, Before);
do_print_unknown_behaviours(_, [], _) ->
    ok.

print_warnings(#cl_state{stored_warnings = []}) ->
    ok;
print_warnings(#cl_state{output = Output,
                         output_format = Format,
                         filename_opt = FOpt,
                         stored_warnings = Warnings}) ->
    PrWarnings = process_warnings(Warnings),
    case PrWarnings of
        [] -> ok;
        [_|_] ->
            S = case Format of
                    formatted ->
                        [dialyzer:format_warning(W, FOpt) || W <- PrWarnings];
                    raw ->
                        [io_lib:format("~p. \n", [W]) || W <- PrWarnings]
                end,
            io:format(Output, "\n~s", [S])
    end.

-spec process_warnings([dial_warning()]) -> [dial_warning()].

process_warnings(Warnings) ->
    Warnings1 = lists:keysort(2, Warnings), %% Sort on file/line
    remove_duplicate_warnings(Warnings1, []).

remove_duplicate_warnings([Duplicate, Duplicate|Left], Acc) ->
    remove_duplicate_warnings([Duplicate|Left], Acc);
remove_duplicate_warnings([NotDuplicate|Left], Acc) ->
    remove_duplicate_warnings(Left, [NotDuplicate|Acc]);
remove_duplicate_warnings([], Acc) ->
    lists:reverse(Acc).

print_ext_calls(#cl_state{report_mode = quiet}) ->
    ok;
print_ext_calls(#cl_state{output = Output,
                          external_calls = Calls,
                          stored_warnings = Warnings,
                          output_format = Format}) ->
    case Calls =:= [] of
        true -> ok;
        false ->
            case Warnings =:= [] of
                true -> io:nl(Output); %% Need to do a newline first
                false -> ok
            end,
            case Format of
                formatted ->
                    io:put_chars(Output, "Unknown functions:\n"),
                    do_print_ext_calls(Output, Calls, "  ");
                raw ->
                    io:put_chars(Output, "%% Unknown functions:\n"),
                    do_print_ext_calls(Output, Calls, "%%  ")
            end
    end.

do_print_ext_calls(Output, [{M,F,A}|T], Before) ->
    io:format(Output, "~s~p:~p/~p\n", [Before,M,F,A]),
    do_print_ext_calls(Output, T, Before);
do_print_ext_calls(_, [], _) ->
    ok.

print_ext_types(#cl_state{report_mode = quiet}) ->
    ok;
print_ext_types(#cl_state{output = Output,
                          external_calls = Calls,
                          external_types = Types,
                          stored_warnings = Warnings,
                          output_format = Format}) ->
    case Types =:= [] of
        true -> ok;
        false ->
            case Warnings =:= [] andalso Calls =:= [] of
                true -> io:nl(Output); %% Need to do a newline first
                false -> ok
            end,
            case Format of
                formatted ->
                    io:put_chars(Output, "Unknown types:\n"),
                    do_print_ext_types(Output, Types, "  ");
                raw ->
                    io:put_chars(Output, "%% Unknown types:\n"),
                    do_print_ext_types(Output, Types, "%%  ")
            end
    end.

do_print_ext_types(Output, [{M,F,A}|T], Before) ->
    io:format(Output, "~s~p:~p/~p\n", [Before,M,F,A]),
    do_print_ext_types(Output, T, Before);
do_print_ext_types(_, [], _) ->
    ok.

init_output(State0) ->
    State = State0#cl_state{output_format = raw, filename_opt = basename},
    State.

-spec error(string()) -> no_return().

error(Msg) ->
    throw({dialyzer_error, Msg}).

-spec error(#cl_state{}, string()) -> no_return().

error(State, Msg) ->
    case State#cl_state.output of
        standard_io -> ok;
        Outfile -> io:format(Outfile, "\n~s\n", [Msg])
    end,
    %%     maybe_close_output_file(State),
    throw({dialyzer_error, Msg}).


-spec start_analysis(#cl_state{}, #analysis{}) -> #cl_state{}.

start_analysis(State, Analysis) ->
    Self = self(),
    LegalWarnings = State#cl_state.legal_warnings,
    Fun = fun() -> 
                  dialyzer_analysis_callgraph:start(Self, LegalWarnings, Analysis)
          end,
    erlang:process_flag(trap_exit, true),
    BackendPid = spawn_link(Fun),
    State#cl_state{backend_pid = BackendPid}.

default_plt_error_msg() ->
    "Use the options:\n"++
        "   --build_plt   to build a new PLT; or\n"++
        "   --add_to_plt  to add to an existing PLT\n"++
        "\n"++
        "For example, use a command like the following:\n"++
        "   dialyzer --build_plt --apps erts kernel stdlib mnesia\n"++
        "Note that building a PLT such as the above may take 20 mins or so\n"++
        "\n"++
        "If you later need information about other applications, say crypto,\n"++
        "you can extend the PLT by the command:\n"++
        "  dialyzer --add_to_plt --apps crypto\n"++
        "For applications that are not in Erlang/OTP use an absolute file name.\n".
