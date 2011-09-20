package org.erlide.core.backend.console;

import org.erlide.core.backend.BackendUtils;
import org.erlide.core.backend.IBackend;
import org.erlide.core.rpc.RpcException;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;

public class ErlideReshd {

    public static OtpErlangPid start(final IBackend fBackend) {
        try {
            final OtpErlangObject r = fBackend.call("erlide_shell", "start",
                    "p", fBackend.getEventPid());
            final OtpErlangPid server = (OtpErlangPid) BackendUtils.ok(r);
            return server;
        } catch (final RpcException e) {
            return null;
        }
    }
}
