package org.erlide.engine.internal.services.parsing;

import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.erlang.IErlFunction;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.runtime.api.IOtpRpc;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.runtime.rpc.RpcTimeoutException;
import org.erlide.util.ErlLogger;
import org.erlide.util.Util;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlideNoparse {

    private static final String ERLIDE_NOPARSE = "erlide_noparse";

    public static OtpErlangTuple initialParse(final IOtpRpc b,
            final String scannerModuleName, final String moduleFileName,
            final String initialText, final String stateDir, final boolean updateRefs) {
        OtpErlangTuple res = null;
        try {
            res = (OtpErlangTuple) b.call(200000, ERLIDE_NOPARSE, "initial_parse",
                    "asssoo", scannerModuleName, moduleFileName, initialText, stateDir,
                    true, updateRefs);
        } catch (final RpcTimeoutException e) {
            ErlLogger.warn(e);
        } catch (final RpcException e) {
            ErlLogger.warn(e);
        }
        return res;
    }

    public static OtpErlangTuple reparse(final IOtpRpc b,
            final String scannerModuleName, final boolean updateSearchServer) {
        OtpErlangTuple res = null;
        try {
            res = (OtpErlangTuple) b.call(20000, ERLIDE_NOPARSE, "reparse", "ao",
                    scannerModuleName, updateSearchServer);
        } catch (final RpcTimeoutException e) {
            ErlLogger.warn(e);
        } catch (final RpcException e) {
            ErlLogger.warn(e);
        }
        return res;
    }

    public static IErlFunction getFunction(final IErlModule module, final String name,
            final int arity) {
        try {
            for (final IErlElement e : module.getChildren()) {
                if (e instanceof IErlFunction) {
                    final IErlFunction function = (IErlFunction) e;
                    if (function.getName().equals(name) && function.getArity() == arity) {
                        return function;
                    }
                }
            }
        } catch (final ErlModelException e) {
            ErlLogger.warn(e);
        }
        return null;
    }

    public static void removeCacheFiles(final IOtpRpc backend,
            final String scannerModuleName, final String stateDir) {
        try {
            final OtpErlangObject res = backend.call(20000, ERLIDE_NOPARSE,
                    "remove_cache_files", "as", scannerModuleName, stateDir);
            if (!Util.isOk(res)) {
                ErlLogger.error("remove_cache_files %s %s", scannerModuleName,
                        res.toString());
            }
        } catch (final RpcException e) {
            ErlLogger.error(e);
        }
    }
}
