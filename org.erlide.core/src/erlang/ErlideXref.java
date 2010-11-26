package erlang;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.backend.util.Util;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.runtime.backend.ErlideBackend;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlideXref {
    private static final String ERLIDE_XREF = "erlide_xref";

    public static void addDirs(final Backend backend,
            final Collection<String> dirs) {
        try {
            backend.call(ERLIDE_XREF, "add_dirs", "ls", dirs);
        } catch (final BackendException e) {
            ErlLogger.warn(e);
        }
    }

    public static List<String> modules(final ErlideBackend backend) {
        final ArrayList<String> result = new ArrayList<String>();
        try {
            final OtpErlangObject res = backend
                    .call(ERLIDE_XREF, "modules", "");
            if (Util.isOk(res)) {
                final OtpErlangTuple t = (OtpErlangTuple) res;
                final OtpErlangList l = (OtpErlangList) t.elementAt(1);
                for (final OtpErlangObject i : l) {
                    if (i instanceof OtpErlangAtom) {
                        final OtpErlangAtom m = (OtpErlangAtom) i;
                        result.add(m.atomValue());
                    }
                }
            }
        } catch (final BackendException e) {
            ErlLogger.error(e); // TODO report error
        }
        return result;
    }

    public static void setScope(final ErlideBackend backend,
            final List<String> scope) {
        final List<String> mods = modules(backend);
        removeModules(backend, mods);
        addDirs(backend, scope);
    }

    private static void removeModules(final ErlideBackend backend,
            final List<String> mods) {
        try {
            backend.call(ERLIDE_XREF, "remove_modules", "ls", mods);
        } catch (final BackendException e) {
            ErlLogger.error(e); // TODO report error
        }
    }

}
