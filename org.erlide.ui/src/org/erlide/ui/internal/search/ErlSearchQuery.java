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
import org.erlide.backend.BackendCore;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.services.search.ErlSearchScope;
import org.erlide.core.services.search.ErlangSearchPattern;
import org.erlide.core.services.search.ErlideSearchServer;
import org.erlide.core.services.search.ModuleLineFunctionArityRef;
import org.erlide.jinterface.rpc.IRpcResultCallback;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.ui.internal.ErlideUIPlugin;

import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

public class ErlSearchQuery implements ISearchQuery {
    private final ErlangSearchPattern pattern;
    private final ErlSearchScope scope;
    private final Map<String, IErlModule> pathToModuleMap;
    private ErlangSearchResult fSearchResult;

    private String stateDirCached = null;
    private final String scopeDescription;

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

    @Override
    public boolean canRerun() {
        return true;
    }

    @Override
    public boolean canRunInBackground() {
        return true;
    }

    @Override
    public String getLabel() {
        return pattern.labelString();
    }

    @Override
    public ISearchResult getSearchResult() {
        if (fSearchResult == null) {
            fSearchResult = new ErlangSearchResult(this);
        }
        return fSearchResult;
    }

    @Override
    public IStatus run(final IProgressMonitor monitor)
            throws OperationCanceledException {
        final Object locker = new Object();
        final IRpcResultCallback callback = new IRpcResultCallback() {

            @Override
            public void start(final OtpErlangObject msg) {
                if (fSearchResult != null) {
                    fSearchResult.removeAll();
                }
                final OtpErlangLong progressMaxL = (OtpErlangLong) msg;
                int progressMax;
                try {
                    progressMax = progressMaxL.intValue();
                } catch (final OtpErlangRangeException e) {
                    progressMax = 10;
                }
                monitor.beginTask("Searching", progressMax);
            }

            @Override
            public void stop(final OtpErlangObject msg) {
                monitor.done();
                synchronized (locker) {
                    locker.notifyAll();
                }
            }

            @Override
            public void progress(final OtpErlangObject msg) {
                final OtpErlangTuple t = (OtpErlangTuple) msg;
                final OtpErlangPid backgroundSearchPid = (OtpErlangPid) t
                        .elementAt(0);
                final OtpErlangLong progressL = (OtpErlangLong) t.elementAt(1);
                final OtpErlangObject resultO = t.elementAt(2);
                int progress = 1;
                try {
                    progress = progressL.intValue();
                    final List<ModuleLineFunctionArityRef> result = Lists
                            .newArrayList();
                    SearchUtil.addSearchResult(result, resultO);
                    addMatches(result);
                } catch (final OtpErlangRangeException e) {
                }
                monitor.worked(progress);
                if (monitor.isCanceled()) {
                    try {
                        ErlideSearchServer.cancelSearch(BackendCore
                                .getBackendManager().getIdeBackend(),
                                backgroundSearchPid);
                    } catch (final RpcException e) {
                    }
                }
            }

        };
        try {
            ErlideSearchServer.startFindRefs(BackendCore.getBackendManager()
                    .getIdeBackend(), pattern, scope, getStateDir(), callback);
        } catch (final RpcException e) {
            return new Status(IStatus.ERROR, ErlideUIPlugin.PLUGIN_ID,
                    "Search error", e);
        }
        synchronized (locker) {
            try {
                locker.wait();
            } catch (final InterruptedException e) {
            }
        }
        return Status.OK_STATUS;
    }

    private void addMatches(final List<ModuleLineFunctionArityRef> chunk) {
        final List<Match> l = Lists.newArrayListWithCapacity(chunk.size());
        final List<ErlangSearchElement> resultAdded = Lists
                .newArrayListWithCapacity(chunk.size());
        for (final ModuleLineFunctionArityRef ref : chunk) {
            final Match m = SearchUtil.createMatch(ref, pathToModuleMap);
            l.add(m);
            resultAdded.add((ErlangSearchElement) m.getElement());
        }
        fSearchResult = (ErlangSearchResult) getSearchResult();
        final List<ErlangSearchElement> result = fSearchResult.getResult();
        result.addAll(resultAdded);
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
