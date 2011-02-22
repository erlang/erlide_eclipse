package erlang;

import java.util.Collection;
import java.util.List;

import org.eclipse.core.runtime.IPath;
import org.erlide.backend.Backend;
import org.erlide.backend.BackendException;
import org.erlide.backend.rpc.RpcCallSite;
import org.erlide.backend.util.Util;
import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.collect.Lists;

public class ErlideDialyze {

    private static final int LONG_TIMEOUT = 20000;
    private static final int FILE_TIMEOUT = 20000;
    private static final int INCLUDE_TIMEOUT = 4000;

    public static OtpErlangObject dialyze(final Backend backend,
            final Collection<String> files, final Collection<String> pltPaths,
            final Collection<IPath> includeDirs, final boolean fromSource) {
        final List<String> incs = Lists.newArrayList();
        for (final IPath p : includeDirs) {
            incs.add(p.toString());
        }
        try {
            final int timeout = files.size() * FILE_TIMEOUT
                    + includeDirs.size() * INCLUDE_TIMEOUT + LONG_TIMEOUT;
            final OtpErlangObject result = backend.call(timeout,
                    "erlide_dialyze", "dialyze", "lslslso", files, pltPaths,
                    incs, fromSource);
            ErlLogger.debug("result %s", result.toString());
            return result;
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }
        return null;
    }

    public static String formatWarning(final RpcCallSite backend,
            final OtpErlangObject warning) {
        try {
            final OtpErlangObject result = backend.call("erlide_dialyze",
                    "format_warning", "x", warning);
            return Util.stringValue(result);
        } catch (final BackendException e) {
            e.printStackTrace();
        }
        return warning.toString();
    }

    public static OtpErlangObject checkPlt(final Backend backend,
            final String plt) {
        try {
            return backend.call("erlide_dialyze", "check_plt", "s", plt);
        } catch (final BackendException e) {
            ErlLogger.debug(e);
        }
        return null;
    }

    public static List<String> getPltFiles(final Backend backend,
            final String pltFiles) throws BackendException {
        final OtpErlangObject o = backend.call("erlide_dialyze",
                "get_plt_files", "s", pltFiles);
        if (Util.isOk(o)) {
            final OtpErlangTuple t = (OtpErlangTuple) o;
            final OtpErlangObject e1 = t.elementAt(1);
            if (e1 instanceof OtpErlangList) {
                final OtpErlangList l = (OtpErlangList) e1;
                final List<String> result = Lists.newArrayListWithCapacity(l
                        .arity());
                for (final OtpErlangObject i : l) {
                    result.add(Util.stringValue(i));
                }
                return result;
            }
        }
        return null;
    }
}
