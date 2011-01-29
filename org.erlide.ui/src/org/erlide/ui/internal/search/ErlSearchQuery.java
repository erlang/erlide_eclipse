package org.erlide.ui.internal.search;

import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.search.ui.ISearchQuery;
import org.eclipse.search.ui.ISearchResult;
import org.eclipse.search.ui.text.Match;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.search.ErlSearchScope;
import org.erlide.core.search.ModuleLineFunctionArityRef;
import org.erlide.runtime.backend.ErlideBackend;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

import erlang.ErlangSearchPattern;
import erlang.ErlideSearchServer;

public class ErlSearchQuery implements ISearchQuery {
    private final ErlangSearchPattern pattern;
    private final ErlSearchScope scope;
    private final ErlSearchScope externalScope;
    private final Map<String, IErlModule> pathToModuleMap;
    private ErlangSearchResult fSearchResult;
    private List<ModuleLineFunctionArityRef> fResult;

    private String stateDirCached = null;
    private final String scopeDescription;

    public ErlSearchQuery(final ErlangSearchPattern pattern, final ErlSearchScope scope,
            final ErlSearchScope externalScope, final String scopeDescription) {
        this.pattern = pattern;
        this.scope = scope;
        this.externalScope = externalScope == null ? new ErlSearchScope()
                : externalScope;
        this.scopeDescription = scopeDescription;
        pathToModuleMap = Maps.newHashMap();
        setupPathToModuleMap();
    }

    private void setupPathToModuleMap() {
        addToModuleMap(scope);
        addToModuleMap(externalScope);
    }

    private void addToModuleMap(final ErlSearchScope theScope) {
        for (final IErlModule module : theScope.getModules()) {
            final String path = module.getFilePath();
            if (path != null) {
                pathToModuleMap.put(path, module);
            }
        }
    }

    // private void setupPathToModuleMap() {
    // for (final IErlModule i : scope.getModules()) {
    // final String path = i.getLocation().toPortableString();
    // pathToModuleMap.put(path, module);
    // }
    // }
    // for (final IErlModule i : externalScope) {
    // pathToModuleMap.put(i.getFilePath(), i);
    // }
    // }

    public boolean canRerun() {
        return true;
    }

    public boolean canRunInBackground() {
        return false;
    }

    public String getLabel() {
        return pattern.labelString();
    }

    public ISearchResult getSearchResult() {
        if (fSearchResult == null) {
            fSearchResult = new ErlangSearchResult(this);
        }
        return fSearchResult;
    }

    public IStatus run(final IProgressMonitor monitor)
            throws OperationCanceledException {
        final ErlideBackend backend = ErlangCore.getBackendManager()
                .getIdeBackend();
        fResult = ErlideSearchServer.findRefs(backend, pattern, scope,
                externalScope, getStateDir());
        final List<Match> l = Lists.newArrayListWithCapacity(fResult.size());
        final List<ErlangSearchElement> result = Lists
                .newArrayListWithCapacity(fResult.size());
        for (final ModuleLineFunctionArityRef ref : fResult) {
            final Match m = SearchUtil.createMatch(ref, pathToModuleMap);
            l.add(m);
            result.add((ErlangSearchElement) m.getElement());
        }
        fSearchResult = (ErlangSearchResult) getSearchResult();
        fSearchResult.setResult(result);
        fSearchResult.addMatches(l.toArray(new Match[l.size()]));
        return Status.OK_STATUS;
    }

    private String getStateDir() {
        if (stateDirCached == null) {
            stateDirCached = ErlangPlugin.getDefault().getStateLocation()
                    .toString();
        }
        return stateDirCached;
    }

    public ErlangSearchPattern getPattern() {
        return pattern;
    }

    public String getScopeDescription() {
        return scopeDescription;
    }
}
