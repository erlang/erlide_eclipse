package org.erlide.core.internal.model.erlang;

import org.erlide.backend.IBackend;
import org.erlide.core.model.erlang.IErlFunction;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.IErlElement;
import org.erlide.jinterface.ErlLogger;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.jinterface.rpc.RpcTimeoutException;
import org.erlide.utils.Util;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlideNoparse {

    private static final String ERLIDE_NOPARSE = "erlide_noparse";

    public static OtpErlangTuple initialParse(final IBackend b,
            final String scannerModuleName, final String moduleFileName,
            final String stateDir, final boolean useCaches,
            final boolean updateRefs) {
        OtpErlangTuple res = null;
        try {
            res = (OtpErlangTuple) b.call(200000, ERLIDE_NOPARSE,
                    "initial_parse", "assoo", scannerModuleName,
                    moduleFileName, stateDir, useCaches, updateRefs);
            if (res.arity() > 2) {
                // ErlLogger.debug("initialParse " + res.elementAt(2));
            }
        } catch (final RpcTimeoutException e) {
            if (!b.isStopped()) {
                ErlLogger.warn(e);
            }
        } catch (final RpcException e) {
            ErlLogger.warn(e);
        }
        return res;
    }

    public static OtpErlangTuple reparse(final IBackend b,
            final String scannerModuleName) {
        OtpErlangTuple res = null;
        try {
            res = (OtpErlangTuple) b.call(20000, ERLIDE_NOPARSE, "reparse",
                    "a", scannerModuleName);
        } catch (final RpcTimeoutException e) {
            if (!b.isStopped()) {
                ErlLogger.warn(e);
            }
        } catch (final RpcException e) {
            ErlLogger.warn(e);
        }
        return res;
    }

    // public static void destroy(final Backend b, final String module) {
    // try {
    // b.call("erlide_noparse_server", "destroy", "a", module);
    // } catch (final Exception e) {
    // ErlLogger.warn(e);
    // }
    // }

    public static IErlFunction getFunction(final IErlModule module,
            final String name, final int arity) {
        try {
            for (final IErlElement e : module.getChildren()) {
                if (e instanceof IErlFunction) {
                    final IErlFunction function = (IErlFunction) e;
                    if (function.getName().equals(name)
                            && function.getArity() == arity) {
                        return function;
                    }
                }
            }
        } catch (final ErlModelException e) {
            ErlLogger.warn(e);
        }
        return null;
    }

    public static void removeCacheFiles(final IBackend backend,
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
