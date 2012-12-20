package org.erlide.backend;

import org.erlide.jinterface.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class BeamLoader {

    public static boolean loadBeam(final IBackend backend,
            final String moduleName, final OtpErlangBinary bin) {
        OtpErlangObject r = null;
        try {
            r = backend.call("code", "is_sticky", "a", moduleName);
            // TODO handle sticky directories
            if (!((OtpErlangAtom) r).booleanValue()) {
                r = backend.call("code", "load_binary", "asb", moduleName,
                        moduleName + ".erl", bin);
            } else {
                ErlLogger.warn("sticky:: %s", moduleName);
                r = null;
            }
        } catch (final Exception e) {
            ErlLogger.warn(e);
        }
        if (r != null) {
            final OtpErlangTuple t = (OtpErlangTuple) r;
            if (((OtpErlangAtom) t.elementAt(0)).atomValue()
                    .compareTo("module") == 0) {
                return true;
            }
            // code couldn't be loaded
            // maybe here we should throw exception?
            return false;
        }
        // binary couldn't be extracted
        return false;
    }

}
