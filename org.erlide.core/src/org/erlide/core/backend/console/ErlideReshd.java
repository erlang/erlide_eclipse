package org.erlide.core.backend.console;

import org.erlide.core.backend.Backend;
import org.erlide.core.backend.BackendException;
import org.erlide.core.backend.BackendUtil;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;

public class ErlideReshd {

    public static OtpErlangPid start(final Backend fBackend) {
        try {
            final OtpErlangObject r = fBackend.call("erlide_shell", "start",
                    "p", fBackend.getEventPid());
            final OtpErlangPid server = (OtpErlangPid) BackendUtil.ok(r);
            return server;
        } catch (final BackendException e) {
            return null;
        }
    }
}
