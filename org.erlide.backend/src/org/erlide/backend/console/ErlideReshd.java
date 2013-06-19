package org.erlide.backend.console;

import org.erlide.backend.BackendUtils;
import org.erlide.backend.api.IBackend;
import org.erlide.runtime.rpc.RpcException;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;

public class ErlideReshd {

    public static OtpErlangPid start(final IBackend runtime) {
        try {
            final OtpErlangObject r = runtime.getRpcSite().call("erlide_shell",
                    "start", "p", runtime.getEventMbox().self());
            final OtpErlangPid server = (OtpErlangPid) BackendUtils.ok(r);
            return server;
        } catch (final RpcException e) {
            return null;
        }
    }
}
