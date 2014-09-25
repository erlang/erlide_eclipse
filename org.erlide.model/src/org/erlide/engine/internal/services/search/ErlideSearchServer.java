package org.erlide.engine.internal.services.search;

import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.services.search.ErlSearchScope;
import org.erlide.engine.services.search.ErlangSearchPattern;
import org.erlide.engine.services.search.SearchServerService;
import org.erlide.runtime.api.IOtpRpc;
import org.erlide.runtime.rpc.IRpcResultCallback;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.util.ErlLogger;
import org.erlide.util.Util;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlideSearchServer implements SearchServerService {

    private static final int SEARCH_LONG_TIMEOUT = 50000;

    private final IOtpRpc backend;

    public ErlideSearchServer(final IOtpRpc backend) {
        this.backend = backend;
    }

    private OtpErlangList getModulesFromScope(final ErlSearchScope scope) {
        final OtpErlangObject[] result = new OtpErlangObject[scope.size()];
        int i = 0;
        for (final IErlModule module : scope.getModules()) {
            result[i] = make2Tuple(module.getScannerName(), module.getFilePath());
            i++;
        }
        return new OtpErlangList(result);
    }

    private OtpErlangTuple make2Tuple(final String scannerModuleName, final String path) {
        return new OtpErlangTuple(new OtpErlangObject[] {
                new OtpErlangAtom(scannerModuleName), new OtpErlangString(path) });
    }

    @Override
    public void startFindRefs(final ErlangSearchPattern pattern,
            final ErlSearchScope scope, final String stateDir,
            final IRpcResultCallback callback, final boolean updateSearchServer)
            throws RpcException {
        final OtpErlangList modules = getModulesFromScope(scope);
        backend.async_call_result(callback, "erlide_search_server", "start_find_refs",
                "xxxso", pattern.getSearchObject(), modules, stateDir, updateSearchServer);
    }

    @Override
    public OtpErlangObject findRefs(final ErlangSearchPattern pattern,
            final ErlSearchScope scope, final String stateDir,
            final boolean updateSearchServer) throws RpcException {
        final OtpErlangList modules = getModulesFromScope(scope);
        final OtpErlangObject searchObject = pattern.getSearchObject();
        ErlLogger.debug("searchObject %s", searchObject);
        final OtpErlangObject r = backend.call(SEARCH_LONG_TIMEOUT,
                "erlide_search_server", "find_refs", "xxso", searchObject, modules,
                stateDir, updateSearchServer);
        if (Util.isOk(r)) {
            return r;
        }
        return null;
    }

    @Override
    public void cancelSearch(final OtpErlangPid searchDeamonPid) throws RpcException {
        backend.call("erlide_search_server", "cancel_find_refs", "x", searchDeamonPid);
    }

}
