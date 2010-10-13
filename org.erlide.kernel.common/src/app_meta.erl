-module(app_meta).

-export([
         read/1,
         write/2,
         meta_to_app/1,
         app_to_meta/1,
         merge_app_to_meta/2,
         check_app_meta/2
        ]).

-export([test/0]).

-include("app_meta.hrl").

%%FIXME inlined #meta in #app_meta, code probably broken now

-spec read(string()) -> #app_meta{}.
read(Path) ->
    {ok, [Term]} = file:consult(Path),
    case Term of
        {application, _, _} ->
            app_to_meta(Term);
        _ ->
            Term
    end.

-spec write(any(), string()) -> 'ok' | {'error', atom()}.
write(Term, Path) ->
    Text = io_lib:format("~p~n", [format(Term)]),
    file:write_file(Path, Text).

-spec meta_to_app(#app_meta{}) -> {application, string(), list()}.
meta_to_app(#app_meta{name=Name}=Meta) ->
    {application, Name, remove_defaults(clean(format(Meta)), format(#app_meta{}))}.

-spec app_to_meta({application, string(), list()}) -> #app_meta{}.
app_to_meta({application, Name, Meta}) ->
    {app_meta, Name, meta(Meta)}.

-spec merge_app_to_meta({application, string(), list()}, #app_meta{}) -> #app_meta{}.
merge_app_to_meta(App, Meta) ->
    lists:foldl(App, fun meta/2, Meta).

check_app_meta(App, Meta) ->
    lists:append(lists:foldl(App, fun check_meta/2, Meta)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

meta(Term) ->
    parse(Term, fun meta/2, #app_meta{}).

parse(Term, Fun, Init) when is_list(Term) ->
    lists:foldl(Fun, Init, Term).

meta({description, Description}, Meta) ->
    Meta#app_meta{description=Description};
meta({id, Id}, Meta) ->
    Meta#app_meta{id=Id};
meta({vsn, Vsn}, Meta) ->
    Meta#app_meta{vsn=Vsn};
meta({modules, Modules}, Meta) ->
    Meta#app_meta{modules=Modules};
meta({maxT, MaxT}, Meta) ->
    Meta#app_meta{maxT=MaxT};
meta({registered, Registered}, Meta) ->
    Meta#app_meta{registered=Registered};
meta({included_applications, Included_applications}, Meta) ->
    Meta#app_meta{included_applications=Included_applications};
meta({applications, Applications}, Meta) ->
    Meta#app_meta{applications=Applications};
meta({env, Env}, Meta) ->
    Meta#app_meta{env=Env};
meta({mod, Mod}, Meta) ->
    Meta#app_meta{mod=Mod};
meta({start_phases, Start_phases}, Meta) ->
    Meta#app_meta{start_phases=Start_phases};
meta({otp_version, Otp_version}, Meta) ->
    Meta#app_meta{otp_version=Otp_version};
meta({layout, Layout}, Meta) ->
    Meta#app_meta{layout=parse(Layout, fun layout/2, #layout{})};
meta({compiler_options, Compiler_options}, Meta) ->
    Meta#app_meta{compiler_options=Compiler_options};
meta(_, Meta) ->
    Meta.

check(_K, V, V) ->
    [];
%% check(modules, source, V0) ->
%%     check(modules, get_all_modules(), V0);
check(K, V, V0) when is_list(V), is_list(V0) ->
    case clean(V) of
        V0 ->
            [];
        V1 ->
            case lists:subtract(V1, V) of
                [] ->
                    [];
                _ ->
                    [{mismatch, K, V1, V0}]
            end
    end;
check(K, V, V0) ->
    [{mismatch, K, V, V0}].


check_meta({description, Description}, #app_meta{description=Description0}) ->
    check(description, Description, Description0);
check_meta({id, Id}, Meta) ->
    Meta#app_meta{id=Id};
check_meta({vsn, Vsn}, Meta) ->
    Meta#app_meta{vsn=Vsn};
check_meta({modules, Modules}, Meta) ->
    Meta#app_meta{modules=Modules};
check_meta({maxT, MaxT}, Meta) ->
    Meta#app_meta{maxT=MaxT};
check_meta({registered, Registered}, Meta) ->
    Meta#app_meta{registered=Registered};
check_meta({included_applications, Included_applications}, Meta) ->
    Meta#app_meta{included_applications=Included_applications};
check_meta({applications, Applications}, Meta) ->
    Meta#app_meta{applications=Applications};
check_meta({env, Env}, Meta) ->
    Meta#app_meta{env=Env};
check_meta({mod, Mod}, Meta) ->
    Meta#app_meta{mod=Mod};
check_meta({start_phases, Start_phases}, Meta) ->
    Meta#app_meta{start_phases=Start_phases};
check_meta({otp_version, Otp_version}, Meta) ->
    Meta#app_meta{otp_version=Otp_version};
check_meta({layout, Layout}, Meta) ->
    Meta#app_meta{layout=parse(Layout, fun layout/2, #layout{})};
check_meta({compiler_options, Compiler_options}, Meta) ->
    Meta#app_meta{compiler_options=Compiler_options};
check_meta(_, Meta) ->
    Meta.

layout({src, Src}, Layout) ->
    Layout#layout{src=Src};
layout({include, Include}, Layout) ->
    Layout#layout{include=Include};
layout({ebin, Ebin}, Layout) ->
    Layout#layout{ebin=Ebin};
layout(_, Layout) ->
    Layout.

format({application, Name, Meta}) ->
    {application, Name, format(Meta)};
format(#app_meta{name=Name, description=Description, id=Id, vsn=Vsn,
                modules=Modules, maxT=MaxT, registered=Registered,
                included_applications=Included_applications,
                applications=Applications, env=Env,
                mod=Mod, start_phases=Start_phases,
                otp_version=Otp_version, layout=Layout,
                compiler_options=Compiler_options}) ->
    {app_meta, Name, [{description,Description}, {id,Id}, { vsn,Vsn},
                     {modules,Modules}, {maxT,MaxT}, {registered,Registered},
                     {included_applications,Included_applications},
                     {applications,Applications}, {env,Env},
                     {mod,Mod}, {start_phases,Start_phases},
                     {otp_version,Otp_version}, {layout,format(Layout)},
                     {compiler_options,Compiler_options}]};
format(#layout{src=Src, include=Include, ebin=Ebin }) ->
    [{src, Src}, {include, Include}, {ebin, Ebin}].

clean(L) ->
    lists:flatmap(fun(X)-> clean1(X, L) end, L).

clean1({modules, M}, _) ->
    [{modules, lists:flatmap(fun get_name/1, M)}];
clean1({applications, M}, _) ->
    [{applications, lists:flatmap(fun get_name/1, M)}];
clean1({included_applications, M}, _) ->
    [{included_applications, lists:flatmap(fun get_name/1, M)}];
clean1({otp_version, _}, _) ->
    [];
clean1({layout, _}, _) ->
    [];
clean1({compiler_options, _}, _) ->
    [];
clean1(X, _) ->
    [X].

get_all_modules(Ms) ->
    Files = lists:append(lists:map(fun(X) -> {ok,Y}=file:list_dir(X), Y end, Ms)),
    %% TODO what do we do with yrl files?
    Mods = lists:filter(fun(X) -> filename:extension(X) == ".erl" end, Files),
    [list_to_atom(filename:basename(X, ".erl")) || X<-Mods].

get_name({X, _}) ->
    [X];
get_name(X) when is_atom(X) ->
    [X];
get_name(_) ->
    [].

remove_defaults(L, Ref)->
    Fun = fun(X={K, V}) ->
                  case lists:keysearch(K, 1, Ref) of
                      {value, {K, V}} -> [];
                      _ -> [X]
                  end
          end,
    lists:flatmap(Fun, L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test() ->
    X = read(".meta"),
    {_, Name, Options} =X,
    New = meta(Options),
    {format(New), meta_to_app({app_meta, Name, New})}.
