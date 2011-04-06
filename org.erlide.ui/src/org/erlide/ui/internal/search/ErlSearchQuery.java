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
import org.erlide.core.backend.Backend;
import org.erlide.core.backend.BackendCore;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.rpc.RpcException;
import org.erlide.core.rpc.RpcResultCallback;
import org.erlide.core.services.search.ErlSearchScope;
import org.erlide.core.services.search.ErlangSearchPattern;
import org.erlide.core.services.search.ErlideSearchServer;
import org.erlide.core.services.search.ModuleLineFunctionArityRef;
import org.erlide.ui.ErlideUIPlugin;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

public class ErlSearchQuery implements ISearchQuery {
    private final ErlangSearchPattern pattern;
    private final ErlSearchScope scope;
    private final Map<String, IErlModule> pathToModuleMap;
    private ErlangSearchResult fSearchResult;
    private List<ModuleLineFunctionArityRef> fResult;

    private String stateDirCached = null;
    private final String scopeDescription;
    private Backend fSearchBackend;

    public ErlSearchQuery(final ErlangSearchPattern pattern,
            final ErlSearchScope scope, final String scopeDescription) {
        this.pattern = pattern;
        this.scope = scope;
        this.scopeDescription = scopeDescription;
        pathToModuleMap = Maps.newHashMap();
        setupPathToModuleMap();
    }

    private void setupPathToModuleMap() {
        addToModuleMap(scope);
    }

    private void addToModuleMap(final ErlSearchScope theScope) {
        for (final IErlModule module : theScope.getModules()) {
            final String path = module.getFilePath();
            if (path != null) {
                pathToModuleMap.put(path, module);
            }
        }
    }

    public boolean canRerun() {
        return true;
    }

    public boolean canRunInBackground() {
        return true;
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
        fSearchBackend = BackendCore.getBackendManager().getIdeBackend();
        final RpcResultCallback callback = new RpcResultCallback() {

            public void start(final OtpErlangObject msg) {
                // TODO Auto-generated method stub

            }

            public void stop(final OtpErlangObject msg) {
                // TODO Auto-generated method stub

            }

            public void progress(final OtpErlangObject msg) {
                // TODO Auto-generated method stub

            }

        };
        try {
            ErlideSearchServer.startFindRefs(fSearchBackend, pattern, scope,
                    getStateDir(), callback);
        } catch (final RpcException e) {
            return new Status(IStatus.ERROR, ErlideUIPlugin.PLUGIN_ID,
                    "Search error", e);
        }
        addMatches();
        return Status.OK_STATUS;
    }

    public void cancel() throws RpcException {
        // ska vanta med denna committa det andra forst!
        // ErlideSearchServer.cancelSearch(fSearchBackend, fSearchDeamonPid);
    }

    private void addMatches() {
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
