package org.erlide.ui.internal.search;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.search.ui.ISearchQuery;
import org.eclipse.search.ui.ISearchResult;
import org.eclipse.search.ui.text.Match;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.search.ModuleLineFunctionArityRef;
import org.erlide.runtime.backend.ErlideBackend;

import com.google.common.collect.Lists;

import erlang.ErlangSearchPattern;
import erlang.ErlideSearchServer;

public class ErlSearchQuery implements ISearchQuery {
    private final ErlangSearchPattern pattern;
    private final Collection<IResource> scope;
    private final Collection<IErlModule> externalScope;
    private final Map<String, IErlModule> pathToModuleMap;
    private ErlangSearchResult fSearchResult;
    private List<ModuleLineFunctionArityRef> fResult;

    private String stateDirCached = null;
    private final String scopeDescription;

    public ErlSearchQuery(final ErlangSearchPattern pattern,
            final Collection<IResource> scope,
            final Collection<IErlModule> externalScope,
            final String scopeDescription) {
        this.pattern = pattern;
        this.scope = scope;
        this.externalScope = externalScope;
        this.scopeDescription = scopeDescription;
        pathToModuleMap = new HashMap<String, IErlModule>();
        setupPathToModuleMap();
    }

    private void setupPathToModuleMap() {
        for (final IResource i : scope) {
            final IErlElement element = ErlangCore.getModel().findElement(i);
            if (element instanceof IErlModule) {
                final IErlModule module = (IErlModule) element;
                final String path = i.getLocation().toPortableString();
                pathToModuleMap.put(path, module);
            }
        }
        for (final IErlModule i : externalScope) {
            pathToModuleMap.put(i.getFilePath(), i);
        }
    }

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
