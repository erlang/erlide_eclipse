package erlang;

import java.util.Collection;
import java.util.List;

import org.eclipse.core.runtime.IPath;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.backend.util.Util;
import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.google.common.collect.Lists;

public class ErlideDialyze {

    private static final int LONG_TIMEOUT = 20000;
    private static final int FILE_TIMEOUT = 20000;
    private static final int INCLUDE_TIMEOUT = 4000;

    public static OtpErlangObject dialyze(final Backend backend,
            final Collection<String> files, final List<String> pltPaths,
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

    public static String formatWarning(final Backend backend,
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
}
