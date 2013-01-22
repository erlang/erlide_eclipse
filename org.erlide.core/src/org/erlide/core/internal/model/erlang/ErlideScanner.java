package org.erlide.core.internal.model.erlang;

import java.util.ArrayList;
import java.util.List;

import org.erlide.backend.BackendCore;
import org.erlide.backend.BackendException;
import org.erlide.backend.IBackend;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.model.erlang.ErlToken;
import org.erlide.runtime.IRpcSite;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.runtime.rpc.RpcTimeoutException;
import org.erlide.utils.ErlLogger;
import org.erlide.utils.Util;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlideScanner {
    private static final String ERLIDE_SCANNER = "erlide_scanner_server";
    private static final Object ENCODING = System.getProperty(
            "erlide.encoding.__test__", "latin1");

    public static void initialScan(final String module, final String path,
            final String initialText, final boolean logging) {
        final String stateDir = ErlangPlugin.getDefault().getStateLocation()
                .toString();
        final IBackend backend = BackendCore.getBackendManager()
                .getIdeBackend();
        try {
            final String loggingOnOff = logging ? "on" : "off";
            backend.getRpcSite().call(ERLIDE_SCANNER, "initialScan", "asssoa",
                    module, path, initialText, stateDir, true, loggingOnOff);
        } catch (final RpcTimeoutException e) {
            if (!backend.isStopped()) {
                ErlLogger.warn(e);
            }
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }
    }

    public static void create(final String module) {
        final IBackend backend = BackendCore.getBackendManager()
                .getIdeBackend();
        try {
            backend.getRpcSite().call(ERLIDE_SCANNER, "create", "a", module);
        } catch (final RpcTimeoutException e) {
            if (!backend.isStopped()) {
                ErlLogger.warn(e);
            }
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }
    }

    public static void addref(final String module) {
        final IBackend backend = BackendCore.getBackendManager()
                .getIdeBackend();
        try {
            backend.getRpcSite().call(ERLIDE_SCANNER, "addref", "a", module);
        } catch (final RpcTimeoutException e) {
            if (!backend.isStopped()) {
                ErlLogger.warn(e);
            }
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }
    }

    public static void dispose(final String module) {
        final IBackend backend = BackendCore.getBackendManager()
                .getIdeBackend();
        try {
            backend.getRpcSite().call(ERLIDE_SCANNER, "dispose", "a", module);
        } catch (final RpcTimeoutException e) {
            if (!backend.isStopped()) {
                ErlLogger.warn(e);
            }
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }
    }

    @SuppressWarnings("boxing")
    public static ErlToken getTokenAt(final String module, final int offset) {
        OtpErlangObject r1 = null;
        try {
            r1 = BackendCore.getBackendManager().getIdeBackend().getRpcSite()
                    .call(ERLIDE_SCANNER, "getTokenAt", "ai", module, offset);
            // ErlLogger.debug("getTokenAt -> " + r1);
        } catch (final Exception e) {
            // e.printStackTrace();
            return null;
        }
        if (r1 == null || !(r1 instanceof OtpErlangTuple)) {
            return null;
        }
        final OtpErlangTuple t1 = (OtpErlangTuple) r1;
        if (Util.isOk(t1)) {
            final OtpErlangObject ot = t1.elementAt(1);
            if (ot instanceof OtpErlangTuple) {
                final OtpErlangTuple tt = (OtpErlangTuple) ot;
                return new ErlToken(tt);
            }
        }
        return null;
    }

    @SuppressWarnings("boxing")
    public static void replaceText(final String module, final int offset,
            final int removeLength, final String newText) {
        final IBackend backend = BackendCore.getBackendManager()
                .getIdeBackend();
        try {
            // ErlLogger.debug("replaceText %s %d %d <length %d>", module,
            // offset,
            // removeLength, newTextLen);
            // ErlLogger.debug("replaceText %s %d %d \"%s\"", module, offset,
            // removeLength, newText);
            final OtpErlangObject r = backend.getRpcSite().call(ERLIDE_SCANNER,
                    "replaceText", "aiis", module, offset, removeLength,
                    newText);
            if (r instanceof OtpErlangTuple) {
                ErlLogger.error("GOT::" + r.toString());
            }
        } catch (final RpcTimeoutException e) {
            if (!backend.isStopped()) {
                ErlLogger.warn(e);
            }
        } catch (final RpcException e) {
            ErlLogger.debug(e);
        }
    }

    /**
     * @param string
     * @param offset
     * @return
     * @throws BackendException
     */
    public static List<ErlToken> lightScanString(final String string,
            final int offset) throws BackendException {
        OtpErlangObject r1 = null;
        final IRpcSite backend = BackendCore.getBackendManager()
                .getIdeBackend().getRpcSite();
        try {
            r1 = backend.call("erlide_scanner", "light_scan_string", "ba",
                    string, ENCODING);
        } catch (final Exception e) {
            throw new BackendException("Could not parse string \"" + string
                    + "\": " + e.getMessage());
        }
        if (r1 == null) {
            return null;
        }

        if (!(r1 instanceof OtpErlangTuple)) {
            throw new BackendException("Could not parse string \"" + string
                    + "\": weird return value " + r1);
        }
        final OtpErlangTuple t1 = (OtpErlangTuple) r1;

        List<ErlToken> toks = null;
        if (!(t1.elementAt(0) instanceof OtpErlangAtom)) {
            throw new BackendException("Could not parse string \"" + string
                    + "\": funny return value" + t1);
        }
        if (Util.isOk(t1)) {
            if (t1.elementAt(1) instanceof OtpErlangList) {
                final OtpErlangList l = (OtpErlangList) t1.elementAt(1);
                if (l != null) {
                    toks = new ArrayList<ErlToken>(l.arity() + 1);
                    for (final OtpErlangObject o : l) {
                        final OtpErlangTuple t = (OtpErlangTuple) o;
                        final ErlToken tk = new ErlToken(t);
                        tk.fixOffset(offset);
                        toks.add(tk);
                    }
                    return toks;
                }
            } else if (t1.elementAt(1) instanceof OtpErlangBinary) {
                final OtpErlangBinary b = (OtpErlangBinary) t1.elementAt(1);
                final byte[] bytes = b.binaryValue();
                toks = new ArrayList<ErlToken>(bytes.length / 10);
                for (int i = 0; i < bytes.length; i += 10) {
                    final ErlToken tk = new ErlToken(bytes, i);
                    tk.fixOffset(offset);
                    toks.add(tk);
                }
                return toks;
            }
        }
        throw new BackendException("Could not parse string \"" + string
                + "\": " + t1.elementAt(1).toString());
    }

    public static String checkAll(final String module, final String text) {
        if (module == null) {
            return "";
        }
        try {
            final OtpErlangObject o = BackendCore.getBackendManager()
                    .getIdeBackend().getRpcSite()
                    .call(ERLIDE_SCANNER, "check_all", "as", module, text);
            return o.toString();
        } catch (final RpcException e) {
            return "";
        }

    }

    public static String getText(final String scannerName) {
        try {
            final OtpErlangObject o = BackendCore.getBackendManager()
                    .getIdeBackend().getRpcSite()
                    .call(ERLIDE_SCANNER, "getText", "a", scannerName);
            return Util.stringValue(o);
        } catch (final RpcException e) {
            return "";
        }
    }

    public static boolean dumpLog(final String scannerName,
            final String dumpLocationFilename) {
        try {
            final IBackend backend = BackendCore.getBackendManager()
                    .getIdeBackend();
            final OtpErlangObject object = backend.getRpcSite().call(
                    ERLIDE_SCANNER, "dump_log", "as", scannerName,
                    dumpLocationFilename);
            return Util.isOk(object);
        } catch (final RpcException e) {
            ErlLogger.warn(e);
        }
        return false;
    }

}
