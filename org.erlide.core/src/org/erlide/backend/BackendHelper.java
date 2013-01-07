package org.erlide.backend;

import org.erlide.runtime.IRpcSite;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.utils.Asserts;
import org.erlide.utils.ErlLogger;
import org.erlide.utils.Util;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class BackendHelper {

    private final IRpcSite target;

    public BackendHelper(final IRpcSite target) {
        Asserts.isNotNull(target);
        this.target = target;
    }

    public BackendHelper() {
        this(BackendCore.getBackendManager().getIdeBackend().getRpcSite());
    }

    public String format_error(final OtpErlangObject object) {
        final OtpErlangTuple err = (OtpErlangTuple) object;
        final OtpErlangAtom mod = (OtpErlangAtom) err.elementAt(1);
        final OtpErlangObject arg = err.elementAt(2);

        String res;
        try {
            OtpErlangObject r = target.call(mod.atomValue(), "format_error",
                    "x", arg);
            r = target.call("lists", "flatten", "x", r);
            res = ((OtpErlangString) r).stringValue();
        } catch (final Exception e) {
            ErlLogger.error(e);
            res = err.toString();
        }
        return res;
    }

    /**
     * @param string
     * @return
     */
    public OtpErlangObject parseConsoleInput(final String string)
            throws BackendException {
        OtpErlangObject r1 = null;
        try {
            r1 = target.call("erlide_backend", "parse_string", "s", string);
        } catch (final Exception e) {
            throw new BackendException("Could not parse string \"" + string
                    + "\": " + e.getMessage());
        }
        final OtpErlangTuple t1 = (OtpErlangTuple) r1;
        if (Util.isOk(t1)) {
            return t1.elementAt(1);
        }
        throw new BackendException("Could not parse string \"" + string
                + "\": " + t1.elementAt(1).toString());
    }

    public OtpErlangObject concreteSyntax(final OtpErlangObject val) {
        try {
            return target.call("erlide_syntax", "concrete", "x", val);
        } catch (final RpcException e) {
            return null;
        }
    }

}
