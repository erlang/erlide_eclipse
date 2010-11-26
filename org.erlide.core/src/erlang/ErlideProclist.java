package erlang;

import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.NoBackendException;
import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;

public class ErlideProclist {
    public static final String MODULE_NAME = "erlide_proclist";

    public static void processListInit(final Backend b) {
        if (b == null) {
            return;
        }
        try {
            b.call(MODULE_NAME, "process_list_init", "");
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }
    }

    public static OtpErlangList getProcessList(final Backend b) {
        if (b == null) {
            return new OtpErlangList();
        }
        try {
            final OtpErlangObject result = b.call(MODULE_NAME, "process_list",
                    "");
            return (OtpErlangList) result;
        } catch (final NoBackendException e) {
            ErlLogger.debug(e);
        } catch (final Exception e) {
            ErlLogger.warn(e);
        }
        return new OtpErlangList();
    }

    public static OtpErlangObject getProcessInfo(final Backend b,
            final OtpErlangPid pid) {
        if (b == null) {
            return new OtpErlangAtom("error");
        }
        try {
            return b.call(MODULE_NAME, "get_process_info", "p", pid);
        } catch (final NoBackendException e) {
            ErlLogger.debug(e);
        } catch (final Exception e) {
            ErlLogger.warn(e);
        }
        return new OtpErlangAtom("error");
    }

}
