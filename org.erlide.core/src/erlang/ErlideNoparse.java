package erlang;

import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlFunction;
import org.erlide.core.erlang.IErlModule;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.backend.util.Util;
import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlideNoparse {

    private static final String ERLIDE_NOPARSE = "erlide_noparse";

    public static OtpErlangTuple initialParse(final Backend b,
            final String scannerModuleName, final String moduleFileName,
            final String stateDir, final boolean updateCaches,
            final boolean updateRefs) {
        OtpErlangTuple res = null;
        try {
            res = (OtpErlangTuple) b.call(200000, ERLIDE_NOPARSE,
                    "initial_parse", "assoo", scannerModuleName,
                    moduleFileName, stateDir, updateCaches, updateRefs);
            if (res.arity() > 2) {
                // ErlLogger.debug("initialParse " + res.elementAt(2));
            }
        } catch (final BackendException e) {
            ErlLogger.warn(e);
        }
        return res;
    }

    public static OtpErlangTuple reparse(final Backend b,
            final String scannerModuleName) {
        OtpErlangTuple res = null;
        try {
            res = (OtpErlangTuple) b.call(20000, ERLIDE_NOPARSE, "reparse",
                    "a", scannerModuleName);
        } catch (final BackendException e) {
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

    public static void removeCacheFiles(final Backend backend,
            final String scannerModuleName, final String stateDir) {
        try {
            final OtpErlangObject res = backend.call(20000, ERLIDE_NOPARSE,
                    "remove_cache_files", "as", scannerModuleName, stateDir);
            if (!Util.isOk(res)) {
                ErlLogger.error("remove_cache_files %s %s", scannerModuleName,
                        res.toString());
            }
        } catch (final BackendException e) {
            ErlLogger.error(e);
        }
    }
}
