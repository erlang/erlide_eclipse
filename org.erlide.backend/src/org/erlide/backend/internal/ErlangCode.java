package org.erlide.backend.internal;

import org.erlide.runtime.api.IOtpRpc;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.util.ErlLogger;
import org.erlide.util.erlang.Bindings;
import org.erlide.util.erlang.ErlUtils;

import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;

public final class ErlangCode {

    private static final String CODE = "code";

    private ErlangCode() {
    }

    public static void addPathA(final IOtpRpc backend, final String path) {
        try {
            backend.call(CODE, "add_patha", "s", path);
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }
    }

    public static void addPathZ(final IOtpRpc backend, final String path) {
        try {
            backend.call(CODE, "add_pathz", "s", path);
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }
    }

    public static void removePath(final IOtpRpc backend, final String path0) {
        String path = path0;
        try {
            // workaround for bug in code:del_path
            try {
                final OtpErlangObject rr = backend.call("filename", "join", "x",
                        new OtpErlangList(new OtpErlangString(path)));
                path = ((OtpErlangString) rr).stringValue();
            } catch (final Exception e) {
                // ignore
            }
            backend.call(CODE, "del_path", "s", path);
        } catch (final Exception e) {
        }
    }

    public static OtpErlangObject loadBinary(final IOtpRpc b, final String beamf,
            final OtpErlangBinary code) throws RpcException {
        OtpErlangObject result;
        result = b.call(CODE, "load_binary", "asb", beamf, beamf, code);
        return result;
    }

    public static void delete(final IOtpRpc fBackend, final String moduleName) {
        try {
            fBackend.call(CODE, "delete", "a", moduleName);
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }
    }

    public static void load(final IOtpRpc backend, final String name0) {
        String name = name0;
        if (name.endsWith(".beam")) {
            name = name.substring(0, name.length() - 5);
        }
        try {
            backend.call("c", "l", "a", name);
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }
    }

    public static boolean isEmbedded(final IOtpRpc backend) {
        try {
            final OtpErlangObject r = backend.call(CODE, "ensure_loaded", "a",
                    "funny_module_name_that_nobody_would_use");
            final Bindings b = ErlUtils.match("{error, What}", r);
            if (b.getAtom("What").equals("embedded")) {
                return true;
            }
        } catch (final Exception e) {
            // ignore errors
        }
        return false;
    }
}
