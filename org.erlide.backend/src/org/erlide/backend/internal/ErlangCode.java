package org.erlide.backend.internal;

import org.erlide.runtime.Bindings;
import org.erlide.runtime.ErlUtils;
import org.erlide.runtime.IRpcSite;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.utils.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;

public final class ErlangCode {

    private ErlangCode() {
    }

    public static void addPathA(final IRpcSite backend, final String path) {
        try {
            backend.call("code", "add_patha", "s", path);
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }
    }

    public static void addPathZ(final IRpcSite backend, final String path) {
        try {
            backend.call("code", "add_pathz", "s", path);
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }
    }

    public static void removePath(final IRpcSite backend, String path) {
        try {
            // workaround for bug in code:del_path
            try {
                final OtpErlangObject rr = backend.call("filename", "join",
                        "x", new OtpErlangList(new OtpErlangString(path)));
                path = ((OtpErlangString) rr).stringValue();
            } catch (final Exception e) {
                // ignore
            }
            backend.call("code", "del_path", "s", path);
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }
    }

    public static OtpErlangObject loadBinary(final IRpcSite b,
            final String beamf, final OtpErlangBinary code) throws RpcException {
        OtpErlangObject result;
        result = b.call("code", "load_binary", "asb", beamf, beamf, code);
        return result;
    }

    public static void delete(final IRpcSite fBackend, final String moduleName) {
        try {
            fBackend.call("code", "delete", "a", moduleName);
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }
    }

    public static void load(final IRpcSite backend, String name) {
        if (name.endsWith(".beam")) {
            name = name.substring(0, name.length() - 5);
        }
        try {
            backend.call("c", "l", "a", name);
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }
    }

    public static boolean isEmbedded(final IRpcSite backend) {
        try {
            final OtpErlangObject r = backend.call("code", "ensure_loaded",
                    "a", "funny_module_name_that_nobody_would_use");
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
