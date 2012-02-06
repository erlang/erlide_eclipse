package org.erlide.backend.console;

import org.erlide.backend.BackendUtils;
import org.erlide.backend.IBackend;
import org.erlide.jinterface.rpc.RpcException;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;

public class ErlideReshd {

    public static OtpErlangPid start(final IBackend backend) {
        try {
            final OtpErlangObject r = backend.call("erlide_shell", "start",
                    "p", backend.getEventPid());
            final OtpErlangPid server = (OtpErlangPid) BackendUtils.ok(r);
            return server;
        } catch (final RpcException e) {
            return null;
        }
    }
}
