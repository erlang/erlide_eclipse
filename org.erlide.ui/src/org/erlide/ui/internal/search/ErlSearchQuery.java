package org.erlide.ui.internal.search;

import java.util.Collection;
import java.util.List;

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
import org.erlide.core.search.ModuleLineFunctionArityRef;
import org.erlide.runtime.backend.ErlideBackend;

import com.google.common.collect.Lists;

import erlang.ErlangSearchPattern;
import erlang.ErlideSearchServer;

public class ErlSearchQuery implements ISearchQuery {
    private final ErlangSearchPattern pattern;
    private final Collection<IResource> scope;
    private ErlangSearchResult fSearchResult;
    private List<ModuleLineFunctionArityRef> fResult;

    private String stateDirCached = null;
    private final String scopeDecsription;

    public ErlSearchQuery(final ErlangSearchPattern pattern,
            final Collection<IResource> scope, final String scopeDecsription) {
        this.pattern = pattern;
        this.scope = scope;
        this.scopeDecsription = scopeDecsription;
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
                getStateDir());
        final List<Match> l = Lists.newArrayListWithCapacity(fResult.size());
        final List<ErlangSearchElement> result = Lists
                .newArrayListWithCapacity(fResult.size());
        for (final ModuleLineFunctionArityRef ref : fResult) {
            final Match m = SearchUtil.createMatch(ref);
            l.add(m);
            result.add((ErlangSearchElement) m.getElement());
        }
        getSearchResult();
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

    public String getScopeDecsription() {
        return scopeDecsription;
    }
}
