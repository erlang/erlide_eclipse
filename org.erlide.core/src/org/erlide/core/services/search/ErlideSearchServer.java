package org.erlide.core.services.search;

import org.erlide.backend.IBackend;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.jinterface.ErlLogger;
import org.erlide.jinterface.rpc.IRpcResultCallback;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.utils.Util;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlideSearchServer {

    private static final int SEARCH_LONG_TIMEOUT = 50000;

    private static OtpErlangList getModulesFromScope(final ErlSearchScope scope) {
        final OtpErlangObject result[] = new OtpErlangObject[scope.size()];
        int i = 0;
        for (final IErlModule module : scope.getModules()) {
            result[i] = make2Tuple(module.getScannerName(),
                    module.getFilePath());
            i++;
        }
        return new OtpErlangList(result);
    }

    private static OtpErlangTuple make2Tuple(final String scannerModuleName,
            final String path) {
        return new OtpErlangTuple(
                new OtpErlangObject[] { new OtpErlangAtom(scannerModuleName),
                        new OtpErlangString(path) });
    }

    public static void startFindRefs(final IBackend backend,
            final ErlangSearchPattern pattern, final ErlSearchScope scope,
            final String stateDir, final IRpcResultCallback callback)
            throws RpcException {
        final OtpErlangList modules = getModulesFromScope(scope);
        ErlLogger.debug("startFindRefs " + pattern.getSearchObject() + "    "
                + modules);
        backend.async_call_result(callback, "erlide_search_server",
                "start_find_refs", "xxxs", pattern.getSearchObject(), modules,
                stateDir);
    }

    public static OtpErlangObject findRefs(final IBackend backend,
            final ErlangSearchPattern pattern, final ErlSearchScope scope,
            final String stateDir) throws RpcException {
        final OtpErlangList modules = getModulesFromScope(scope);
        ErlLogger.debug("findRefs " + pattern.getSearchObject() + "    "
                + modules);
        final OtpErlangObject r = backend.call(SEARCH_LONG_TIMEOUT,
                "erlide_search_server", "find_refs", "xxs",
                pattern.getSearchObject(), modules, stateDir);
        if (Util.isOk(r)) {
            return r;
        }
        return null;
    }

    public static void cancelSearch(final IBackend backend,
            final OtpErlangPid searchDeamonPid) throws RpcException {
        backend.call("erlide_search_server", "cancel_find_refs", "x",
                searchDeamonPid);
    }

}
