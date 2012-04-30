package org.erlide.launch.debug;

import org.erlide.backend.IBackend;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class DebugHelper {

    public static BackendEvalResult eval(final IBackend b, final String string,
            final OtpErlangObject bindings) {
        final BackendEvalResult result = new BackendEvalResult();
        try {
            OtpErlangObject r1;
            // ErlLogger.debug("eval %s %s", string, bindings);
            if (bindings == null) {
                r1 = b.call("erlide_backend", "eval", "s", string);
            } else {
                r1 = b.call("erlide_backend", "eval", "sx", string, bindings);
            }
            // value may be something else if exception is thrown...
            final OtpErlangTuple t = (OtpErlangTuple) r1;
            final boolean ok = !"error".equals(((OtpErlangAtom) t.elementAt(0))
                    .atomValue());
            if (ok) {
                result.setValue(t.elementAt(1), t.elementAt(2));
            } else {
                result.setError(t.elementAt(1));
            }
        } catch (final Exception e) {
            result.setError("rpc failed");
        }
        return result;
    }

}
