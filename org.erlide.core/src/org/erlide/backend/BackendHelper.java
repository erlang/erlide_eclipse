package org.erlide.backend;

import org.erlide.runtime.IRpcSite;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.utils.ErlLogger;
import org.erlide.utils.Util;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class BackendHelper {

    public static String format_error(final IRpcSite b,
            final OtpErlangObject object) {
        final OtpErlangTuple err = (OtpErlangTuple) object;
        final OtpErlangAtom mod = (OtpErlangAtom) err.elementAt(1);
        final OtpErlangObject arg = err.elementAt(2);

        String res;
        try {
            OtpErlangObject r = b.call(mod.atomValue(), "format_error", "x",
                    arg);
            r = b.call("lists", "flatten", "x", r);
            res = ((OtpErlangString) r).stringValue();
        } catch (final Exception e) {
            ErlLogger.error(e);
            res = err.toString();
        }
        return res;
    }

    public static String format(final IRpcSite b, final String fmt,
            final OtpErlangObject... args) {
        try {
            final String r = b.call("erlide_backend", "format", "slx", fmt,
                    args).toString();
            return r.substring(1, r.length() - 1);
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }
        return "error";
    }

    /**
     * @param string
     * @return OtpErlangobject
     * @throws ErlangParseException
     */
    public static OtpErlangObject parseTerm(final IRpcSite b,
            final String string) throws BackendException {
        OtpErlangObject r1 = null;
        try {
            r1 = b.call("erlide_backend", "parse_term", "s", string);
        } catch (final Exception e) {
            throw new BackendException("Could not parse term \"" + string
                    + "\"");
        }
        final OtpErlangTuple t1 = (OtpErlangTuple) r1;
        if (Util.isOk(t1)) {
            return t1.elementAt(1);
        }
        throw new BackendException("Could not parse term \"" + string + "\": "
                + t1.elementAt(1).toString());
    }

    /**
     * @param string
     * @return
     * @throws BackendException
     */
    public static OtpErlangObject scanString(final IRpcSite b,
            final String string) throws BackendException {
        OtpErlangObject r1 = null;
        try {
            r1 = b.call("erlide_backend", "scan_string", "s", string);
        } catch (final Exception e) {
            throw new BackendException("Could not tokenize string \"" + string
                    + "\": " + e.getMessage());
        }
        final OtpErlangTuple t1 = (OtpErlangTuple) r1;
        if (Util.isOk(t1)) {
            return t1.elementAt(1);
        }
        throw new BackendException("Could not tokenize string \"" + string
                + "\": " + t1.elementAt(1).toString());
    }

    /**
     * @param string
     * @return
     */
    public static OtpErlangObject parseConsoleInput(final IRpcSite b,
            final String string) throws BackendException {
        OtpErlangObject r1 = null;
        try {
            r1 = b.call("erlide_backend", "parse_string", "s", string);
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

    public static String prettyPrint(final IRpcSite b, final String text)
            throws BackendException {
        OtpErlangObject r1 = null;
        try {
            r1 = b.call("erlide_backend", "pretty_print", "s", text + ".");
        } catch (final Exception e) {
            throw new BackendException("Could not parse string \"" + text
                    + "\": " + e.getMessage());
        }
        return ((OtpErlangString) r1).stringValue();
    }

    public static OtpErlangObject concreteSyntax(final IRpcSite b,
            final OtpErlangObject val) {
        try {
            return b.call("erlide_syntax", "concrete", "x", val);
        } catch (final RpcException e) {
            return null;
        }
    }

    public static OtpErlangObject convertErrors(final IRpcSite b,
            final String lines) throws RpcException {
        OtpErlangObject res;
        res = b.call("erlide_erlcerrors", "convert_erlc_errors", "s", lines);
        return res;
    }

    public static void startTracer(final IRpcSite b, final OtpErlangPid tracer) {
        try {
            ErlLogger.debug("Start tracer to %s", tracer);
            b.call("erlide_backend", "start_tracer", "ps", tracer);
        } catch (final RpcException e) {
        }
    }

    public static void startTracer(final IRpcSite b, final String logname) {
        try {
            ErlLogger.debug("Start tracer to %s", logname);
            b.call("erlide_backend", "start_tracer", "s", logname);
        } catch (final RpcException e) {
        }
    }

}
