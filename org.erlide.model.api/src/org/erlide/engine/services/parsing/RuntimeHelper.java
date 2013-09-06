package org.erlide.engine.services.parsing;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.nullValue;

import org.erlide.runtime.api.IRpcSite;
import org.erlide.runtime.api.ParserException;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.util.ErlLogger;
import org.erlide.util.Util;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class RuntimeHelper {

    private final IRpcSite target;

    public RuntimeHelper(final IRpcSite target) {
        assertThat(target, is(not(nullValue())));
        this.target = target;
    }

    public String formatError(final OtpErlangObject object) {
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
            throws ParserException {
        OtpErlangObject r1 = null;
        try {
            r1 = target.call("erlide_backend", "parse_string", "s", string);
        } catch (final Exception e) {
            throw new ParserException("Could not parse string \"" + string
                    + "\": " + e.getMessage());
        }
        final OtpErlangTuple t1 = (OtpErlangTuple) r1;
        if (Util.isOk(t1)) {
            return t1.elementAt(1);
        }
        throw new ParserException("Could not parse string \"" + string + "\": "
                + t1.elementAt(1).toString());
    }

    public OtpErlangObject concreteSyntax(final OtpErlangObject val) {
        try {
            return target.call("erlide_syntax", "concrete", "x", val);
        } catch (final RpcException e) {
            return null;
        }
    }

}
