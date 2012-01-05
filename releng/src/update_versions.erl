%% -*- erlang -*-
%%! -smp enable -sname update_versions

%% must be called from the root of the git workspace!

%% TODO redesign as below:
%% check:
%% - get projects info
%% - compute new plugin versions
%% - compute new feature versions
%% modify:
%% - update project information
%% - update CHANGES
%% commit:
%% - commit 


%-mode(compile).
-module(update_versions).
-compile([export_all]).

-define(DBG(X), io:format("~p~n", [X])).

check() ->
    main(["check"]).
info() ->
    main(["info"]).
   

main([]) ->
    usage();
main([CmdStr]) ->
    try
    	Cmd = list_to_atom(CmdStr),
        io:format("Please ignore any \"fatal:...\" messages below~n"),
        Base = get_latest_tag(),
        ?MODULE:Cmd(Base, projects_info(Base)),
        io:format("~nDone!~n")
    catch
        _:E ->
            io:format("ERROR: ~p~n   ~p~n~n", [E, erlang:get_stacktrace()]),
            usage()
    end.

usage() ->
    io:format("Check which plugin versions need to be updated and add commit info to the CHANGES file\n"
     "Usage: ~s base cmd\n"
     "    cmd = check    : check which plugins need version updates\n"
     "          modify   : modify the plugins versions where needed, do not commit\n"
     "          commit   : as above and commit changes\n"
     "          info     : print info about current projects and features/plugins\n", 
     [script_name()]).

script_name() ->
    try
        escript:script_name()
    catch 
        _:_ -> 
            "update_versions"
    end.

-record(version, {major=0, 
                  minor=0, 
                  micro=0, 
                  q=""
                 }).
-record(plugin, {name, 
                 crt_version=#version{}, 
                 old_version=#version{}, 
                 new_version=#version{}, 
                 changed=nothing, 
                 code_changed=false
                }).
-record(feature, {name, 
                  crt_version=#version{}, 
                  old_version=#version{}, 
                  new_version=#version{}, 
                  features=[], 
                  plugins=[], 
                  changed=nothing, 
                  children_changed=nothing
                 }).

info(_Base, {Features, _Plugins} ) ->
    S = summary(Features),
    io:format("~p~n", [S]),
    ok.
    
check(_Base, {Features, Plugins} ) ->
    %% io:format("FEATURES ~p~nPLUGINS ~p~n", [Features, Plugins]),

    Fun1 = fun(#plugin{changed=C, code_changed=CC})->
                  (C=/=nothing) or CC
          end,
    ChangedPlugins = lists:filter(Fun1, Plugins),

    Fun3 = fun(#plugin{name=Id, crt_version=OldV, changed=C, code_changed=CC}) ->
                  %io:format("> ~s: ~p ~p ~n", [Id, C, CC]),
                   Ch = if CC -> micro; true -> nothing end,
                   NewV = inc_version(OldV, max_change(C, Ch)),
                   Old = version_string(OldV),
                   New = version_string(NewV),
                   io:format("P ~.40s ~18s -> ~.18s~n", [Id, Old, New]),
                   ok
           end,
    lists:foreach(Fun3, ChangedPlugins),

    Fun = fun(#feature{name=_N, changed=C, children_changed=CC})->
                  %io:format("> ~s: ~p ~p ~n", [N, C, CC]),
                  (max_change(C, CC) == CC) and (CC=/=nothing)
          end,
    ChangedFeatures = lists:filter(Fun, Features),
    
    Fun2 = fun(#feature{name=Id, crt_version=OldV, children_changed=CC}) ->
                   Old = version_string(OldV),
                   NewV = inc_version(OldV, CC),
                   New = version_string(NewV),
                   io:format("F ~.40s ~18s -> ~.18s~n", [Id, Old, New]),
                   ok
           end,
    lists:foreach(Fun2, ChangedFeatures),

    {ChangedFeatures, ChangedPlugins}.

modify(Base, Projects) ->
    {CFs, CPs} = check(Base, Projects),
    io:format("modify\n"),

    Fun2 = fun(#feature{name=Id, crt_version=OldV, children_changed=CC}) ->
                   Name = atom_to_list(Id)++"/feature.xml",
                   io:format("fff::: ~p~n", [Name]),
                   Old = version_string(OldV),
                   NewV = inc_version(OldV, CC),
                   New = version_string(NewV),
                   io:format("~p~n", ["sed -e 's/version=\""++Old++"\"/version=\""++New++"\"/' "++Name++" > "++Name++"1"]),
                   %os:cmd("sed -e 's/version=\""++Old++"\"/version=\""++New++"\"/' "++Name++" > "++Name++"1"),
                   %os:cmd("mv "++Name++"1 "++Name),
                   ok
           end,
    lists:foreach(Fun2, CFs),

    Fun3 = fun(#plugin{name=Id, crt_version=OldV, changed=C, code_changed=CC}) ->
                   Name = atom_to_list(Id)++"/META-INF/MANIFEST.MF",
                   io:format("ppp::: ~p~n", [Name]),
                   Ch = if CC -> micro; true -> nothing end,
                   NewV = inc_version(OldV, max_change(C, Ch)),
                   Old = version_string(OldV),
                   New = version_string(NewV),
                   io:format("~p~n", [("sed -e 's/Bundle-Version: "++Old++"/Bundle-Version: "++New++"/' "++Name++" > "++Name++"1")]),
                   %os:cmd("sed -e 's/Bundle-Version: "++Old++"/Bundle-Version: "++New++"/' "++Name++" > "++Name++"1"),
                   %os:cmd("mv "++Name++"1 "++Name),
                   ok
           end,
    lists:foreach(Fun3, CPs),
    
    %% TODO print version numbers and date
    %% TODO check if already has this set of changes 
    %% update_CHANGES(Base, crt_branch()),
    
    ok.

commit(Base, Projects) ->
    modify(Base, Projects),
    io:format("commiting...\n"),
    os:cmd("git commit -am \"updated plugin versions\""),
    ok.

crt_branch() ->
    Branches = [string:strip(X) || X <- string:tokens(os:cmd("git branch"), [$\n])],
    [Crt] = lists:filter(fun("*"++_) -> true; (_) -> false end, Branches),
    string:substr(Crt, 3).


projects_info(Base) ->
    Crt = crt_branch(),
    Changed = changed_projects(Base, Crt),
    io:format("Current tag: ~s~n   Base tag: ~s~n", [Crt, Base]),
    {ok, Dirs} = file:list_dir("."),
    update_features(sort(lists:flatten([parse_project(Name, Base, Changed) || Name <- lists:sort(Dirs)]))).

-define(D(X), io:format("- ~p~n", [X])).

changed_projects(Base, Crt) ->
    Projects = lists:usort(string:tokens(os:cmd("git log --name-only "++Base++".."++Crt++" --oneline"), "\n")),
    Names = [ hd(string:tokens(X, " ")) || X <- Projects],
    Erlide = lists:filter(fun("org.erlide"++_) ->
                                  true; 
                             (_) ->
                                  false 
                          end, Names),
    lists:usort([ hd(string:tokens(X, "/")) || X <- Erlide]).
    
parse_project(Name, Base, Changed) ->
    case get_feature_project(Name, Base) of
        [] ->
            get_plugin_project(Name, Base, Changed);
        F ->
            F
    end.

get_feature_project(Name, Base) ->
    case is_project(Name) andalso filelib:is_file(Name++"/feature.xml") of
        true ->
            [get_feature_content(Name, Base)];
        false ->
            []
    end.

get_plugin_project(Name, Base, Changed) ->
    case is_project(Name) andalso filelib:is_file(Name++"/META-INF/MANIFEST.MF") of
        true ->
            [get_plugin_content(Name, Base, Changed)];
        false ->
            []
    end.

update_features(FP) ->
    update_features(FP, []).

update_features({[], Ps}, Rs) ->
    {lists:reverse(Rs), Ps};
update_features({[#feature{}=F|T], Ps}, Rs) ->
    F1 = changed_children(F, Ps),
    update_features({T, Ps}, [F1 | Rs]).

changed_children(F, Ps) ->
    Fun = fun(P, Fx) ->
                  case lists:member(P#plugin.name, Fx#feature.plugins) andalso P#plugin.code_changed of
                      true ->
                          Fx#feature{children_changed = max_change(Fx#feature.children_changed, P#plugin.changed)};
                      _ ->                 
                          Fx
                  end
          end,
    lists:foldl(Fun, F, Ps).


is_project(Name) ->
    filelib:is_file(Name++"/.project").

get_feature_content(Name, Base) ->
    {XML, _} = xmerl_scan:file(Name++"/feature.xml", [{space, normalize}]),
    Version = version(element(9, hd(xmerl_xpath:string("/feature/attribute::version", XML)))),
    Includes = just_names(xmerl_xpath:string("/feature/includes/attribute::id", XML)),
    Plugins = just_names(xmerl_xpath:string("/feature/plugin/attribute::id", XML)),

    Z = lists:flatten(string:join(read_old_file(Name++"/feature.xml", Base), "\n")),
    Old = case Z of
            "" ->
                version("0.0.0");
            _ ->
                {OldXml, _} = xmerl_scan:string(Z),
                version(element(9, hd(xmerl_xpath:string("/feature/attribute::version", OldXml))))
    end,

    #feature{name=list_to_atom(Name), 
             crt_version=Version, 
             old_version=Old, 
             features=Includes, 
             plugins=Plugins, 
             changed=what_changed(Old, Version)}.

get_plugin_content(Name, Base, Changed) ->
    FN = Name++"/META-INF/MANIFEST.MF",
    Version = get_plugin_version(string:tokens(read_file(FN), "\n")),
    Old = get_plugin_version(read_old_file(FN, Base)),
    #plugin{name=list_to_atom(Name), 
            crt_version=Version, 
            old_version=Old, 
            changed=what_changed(Old, Version), 
            code_changed=lists:member(Name, Changed)}.

read_file(Name) ->
    {ok, Bin} = file:read_file(Name),
    binary_to_list(Bin).

read_old_file(Name, Base) ->
    Str = os:cmd("git show  "++Base++":"++Name),
    string:tokens(Str, "\n").

get_plugin_version([]) ->
    version("0.0.0");
get_plugin_version([L | Lines]) ->
    case string:str(L, "Bundle-Version: ") of
        1 ->
            Str = string:sub_string(L, length("Bundle-Version: ")+1),
            version(clean(Str));
        _ ->
            get_plugin_version(Lines)
    end.

clean(Str) ->
    string:strip(string:strip(Str, right, $\n), right, $\r).

just_names(L) ->
    [list_to_atom(element(9, X)) || X<-L].

version(Str) ->
    L = [try list_to_integer(N) catch _:_ -> N end || N <- string:tokens(Str, ".")],
    L1 = L ++ lists:duplicate(4-length(L), ""),
    list_to_tuple([version | L1]).

sort(L) ->
    IsFeature = fun(#feature{}) -> true; (_) -> false end,
    {F,P} = lists:partition(IsFeature, L),
    {reorder(F, P), lists:sort(P)}.

inc_version(#version{major=V}=Ver, major) ->
    Ver#version{major=V+1, minor=0, micro=0};
inc_version(#version{minor=V}=Ver, minor) ->
    Ver#version{minor=V+1, micro=0};
inc_version(#version{micro=V}=Ver, micro) ->
    Ver#version{micro=V+1};
inc_version(Ver, _) ->
    Ver.

version_string(#version{major=Mj, minor=Mn, micro=Mr, q=Q}) ->
    Str = lists:flatten(io_lib:format("~w.~w.~w", [Mj, Mn, Mr])),
    case Q of
        "" ->
            Str;
        _ ->
            Str++"."++Q
    end.

what_changed(#version{major=OM}, #version{major=NM}) when OM=/=NM ->
    major;
what_changed(#version{minor=Om}, #version{minor=Nm}) when Om=/=Nm ->
    minor;
what_changed(#version{micro=Ou}, #version{micro=Nu}) when Ou=/=Nu ->
    micro;
what_changed(_, _) ->
    nothing.

max_change(major, _) ->
    major;
max_change(_, major) ->
    major;
max_change(minor, _) ->
    minor;
max_change(_, minor) ->
    minor;
max_change(micro, _) ->
    micro;
max_change(_, micro) ->
    micro;
max_change(_, _) ->
    nothing.

reorder(F, P) ->
    Fun = fun(#feature{name=Id, features=[]}) ->
            [{Id, nothing}];
              (#feature{name=Id, features=Other}) ->
            [{Id, X} || X<-Other]
    end,
    Pairs = lists:flatmap(Fun, F),
    {ok, Order0} = topo_sort(Pairs),
    Order = lists:reverse(Order0),
    lists:flatmap(fun(nothing)-> []; (Id)-> [find_change(lists:keyfind(Id, #feature.name, F), F, P)] end, Order).

find_change(#feature{features=Fs, plugins=Ps}=X, F, P) ->
    Fun = fun(#feature{changed=C0}, C) ->
                max_change(C, C0);
              (#plugin{changed=C0}, C) ->
                max_change(C, C0)
        end,
    All = [lists:keyfind(Id, #feature.name, F) || Id<-Fs]++[lists:keyfind(Id, #plugin.name, P) || Id<-Ps],
    Change = lists:foldl(Fun, nothing, All),
    X#feature{children_changed=Change}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (C) 1998, Ericsson Computer Science Laboratory
%-doc([{author,'Joe Armstrong'},
%      {title,"Topological sort of a partial order."},
%      {keywords, [topological,sort,partial,order]},
%      {date,981102}]).

topo_sort(Pairs) ->
    iterate(Pairs, [], all(Pairs)).

iterate([], L, All) ->
    {ok, remove_duplicates(L ++ subtract(All, L))};
iterate(Pairs, L, All) ->
    case subtract(lhs(Pairs), rhs(Pairs)) of
    []  ->
        {cycle, Pairs};
    Lhs ->
        iterate(remove_pairs(Lhs, Pairs), L ++ Lhs, All)
    end.

all(L) -> lhs(L) ++ rhs(L).
lhs(L) -> lists:map(fun({X,_}) -> X end, L).
rhs(L) -> lists:map(fun({_,Y}) -> Y end, L).

subtract(L1, L2) ->  lists:filter(fun(X) -> not lists:member(X, L2) end, L1).

remove_duplicates([H|T]) ->
  case lists:member(H, T) of
      true  -> remove_duplicates(T);
      false -> [H|remove_duplicates(T)]
  end;
remove_duplicates([]) ->
    [].

remove_pairs(L1, L2) -> lists:filter(fun({X,_Y}) -> not lists:member(X, L1) end, L2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% end toposort

update_CHANGES(New, Crt) ->
    Lines = read_file("CHANGES"),
    Log = os:cmd("git log "++New++".."++Crt++" --oneline"),
    Lines1 = ["List of user visible changes between $NEW_ and $VER_ ($(date +%Y%m%d))", "", Log, Lines],
    {ok, F} = file:open("CHANGES", [write]),
    io:format(F, "~s~n", [string:join(Lines1, "\n")]),
    file:close(F),
    ok.

summary(Features) ->
    Fs = [{N,F++P} || #feature{name=N,plugins=P,features=F}<-Features],
    Fs.

get_latest_tag() ->
    string:strip(os:cmd("git describe --abbrev=0"), both, $\n).

tag_as_string({A,B,C}) ->
    lists:flatten(io_lib:format("v~p.~p.~p", [A,B,C])).

parse_tag("v"++Tag) ->
    list_to_tuple([list_to_integer(T) || T<-string:tokens(Tag, ".")]);
parse_tag(_) ->
    {0, 0, 0}.

