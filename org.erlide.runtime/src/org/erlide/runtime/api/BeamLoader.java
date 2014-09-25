package org.erlide.runtime.api;

import java.util.ArrayList;
import java.util.List;

import org.erlide.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class BeamLoader {

    public static boolean loadBeam(final IOtpRpc backend, final String moduleName,
            final OtpErlangBinary bin) {
        OtpErlangObject r = null;
        try {
            r = backend.call("code", "is_sticky", "a", moduleName);
            // TODO handle sticky directories
            if (!((OtpErlangAtom) r).booleanValue()) {
                r = backend.call("code", "load_binary", "asb", moduleName, moduleName
                        + ".erl", bin);
            } else {
                ErlLogger.warn("sticky:: %s", moduleName);
                r = null;
            }
        } catch (final Exception e) {
            ErlLogger.warn(e);
        }
        if (r instanceof OtpErlangTuple) {
            final OtpErlangTuple t = (OtpErlangTuple) r;
            if (((OtpErlangAtom) t.elementAt(0)).atomValue().compareTo("module") == 0) {
                return true;
            }
            // code couldn't be loaded
            // maybe here we should throw exception?
            return false;
        }
        // binary couldn't be extracted
        return false;
    }

    public static void reloadAllCode(final IOtpRpc backend) {
        try {
            final OtpErlangList loaded = (OtpErlangList) backend.call("code",
                    "all_loaded", "");
            final List<OtpErlangAtom> mine = new ArrayList<OtpErlangAtom>();
            for (final OtpErlangObject elem : loaded) {
                final OtpErlangTuple t = (OtpErlangTuple) elem;
                final OtpErlangAtom mod = (OtpErlangAtom) t.elementAt(0);
                if (mod.atomValue().startsWith("erlide_")) {
                    // ErlLogger.debug(">>> HAD " + mod + "   " +
                    // t.elementAt(1));
                    mine.add(mod);
                }
            }
            for (final OtpErlangAtom mod : mine) {
                // ErlLogger.debug(">>> reload " + mod);
                backend.call("c", "l", "x", mod);
            }
        } catch (final Exception e) {
            ErlLogger.error(e);
        }
    }

}
