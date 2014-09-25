package org.erlide.engine.internal.services.proclist;

import org.erlide.engine.services.proclist.ProclistService;
import org.erlide.runtime.api.IOtpRpc;
import org.erlide.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;

public class ErlideProclist implements ProclistService {
    public static final String MODULE_NAME = "erlide_proclist";

    @Override
    public void processListInit(final IOtpRpc b) {
        if (b == null) {
            return;
        }
        try {
            b.call(MODULE_NAME, "process_list_init", "");
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }
    }

    @Override
    public OtpErlangList getProcessList(final IOtpRpc b) {
        if (b == null) {
            return new OtpErlangList();
        }
        try {
            final OtpErlangObject result = b.call(MODULE_NAME, "process_list", "");
            return (OtpErlangList) result;
        } catch (final Exception e) {
            ErlLogger.warn(e);
        }
        return new OtpErlangList();
    }

    @Override
    public OtpErlangObject getProcessInfo(final IOtpRpc b, final OtpErlangPid pid) {
        if (b == null) {
            return new OtpErlangAtom("error");
        }
        try {
            return b.call(MODULE_NAME, "get_process_info", "p", pid);
        } catch (final Exception e) {
            ErlLogger.warn(e);
        }
        return new OtpErlangAtom("error");
    }

}
