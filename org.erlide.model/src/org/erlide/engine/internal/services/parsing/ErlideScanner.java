package org.erlide.engine.internal.services.parsing;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.nullValue;

import java.util.ArrayList;
import java.util.List;

import org.erlide.engine.ErlangEngine;
import org.erlide.engine.services.parsing.ErlToken;
import org.erlide.engine.services.parsing.InternalScanner;
import org.erlide.engine.services.parsing.ScannerException;
import org.erlide.engine.services.parsing.SimpleScannerService;
import org.erlide.runtime.api.IOtpRpc;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.runtime.rpc.RpcTimeoutException;
import org.erlide.util.ErlLogger;
import org.erlide.util.Util;

import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlideScanner implements SimpleScannerService, InternalScanner {
    private static final String ERLIDE_SCANNER = "erlide_scanner";
    private static final Object ENCODING = System.getProperty("erlide.encoding.__test__",
            "latin1");
    private static final boolean USE_CACHE = true;

    private final IOtpRpc backend;

    public ErlideScanner(final IOtpRpc backend) {
        this.backend = backend;
    }

    public void initialScan(final String module, final String path,
            final String initialText) {
        final String stateDir = ErlangEngine.getInstance().getStateDir();
        try {
            backend.call(ERLIDE_SCANNER, "initial_scan", "assso", module, path,
                    initialText == null ? "" : initialText, stateDir, USE_CACHE);
        } catch (final RpcTimeoutException e) {
            ErlLogger.debug(e);
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }
    }

    @Override
    public void create(final String module) {
        try {
            backend.call(ERLIDE_SCANNER, "create", "a", module);
        } catch (final RpcTimeoutException e) {
            ErlLogger.debug(e);
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }
    }

    public void addref(final String module) {
        try {
            backend.call(ERLIDE_SCANNER, "addref", "a", module);
        } catch (final RpcTimeoutException e) {
            ErlLogger.debug(e);
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }
    }

    public void dispose(final String module) {
        try {
            backend.call(ERLIDE_SCANNER, "dispose", "a", module);
        } catch (final RpcTimeoutException e) {
            ErlLogger.debug(e);
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }
    }

    @SuppressWarnings("boxing")
    public ErlToken getTokenAt(final String module, final int offset) {
        OtpErlangObject r1 = null;
        try {
            r1 = backend.call(ERLIDE_SCANNER, "get_token_at", "ai", module, offset);
        } catch (final Exception e) {
            return null;
        }
        if (!(r1 instanceof OtpErlangTuple)) {
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
    public void replaceText(final String module, final int offset,
            final int removeLength, final String newText) {
        assertThat(newText, is(not(nullValue())));
        try {
            final OtpErlangObject r = backend.call(ERLIDE_SCANNER, "replace_text",
                    "aiis", module, offset, removeLength, newText);
            if (r instanceof OtpErlangTuple) {
                ErlLogger.error("replace_text %s @ %d GOT:: %s", module, offset,
                        r.toString());
            }
        } catch (final RpcTimeoutException e) {
            ErlLogger.debug(e);
        } catch (final RpcException e) {
            ErlLogger.debug(e);
        }
    }

    @Override
    public List<ErlToken> lightScanString(final String string, final int offset)
            throws ScannerException {
        OtpErlangObject r1 = null;
        try {
            r1 = backend.call("erlide_scanner", "light_scan_string", "ba", string,
                    ENCODING);
        } catch (final Exception e) {
            throw new ScannerException("Could not parse string \"" + string + "\": "
                    + e.getMessage());
        }
        if (r1 == null) {
            return null;
        }

        if (!(r1 instanceof OtpErlangTuple)) {
            throw new ScannerException("Could not parse string \"" + string
                    + "\": weird return value " + r1);
        }
        final OtpErlangTuple t1 = (OtpErlangTuple) r1;

        List<ErlToken> toks = null;
        if (Util.isOk(t1)) {
            if (t1.elementAt(1) instanceof OtpErlangBinary) {
                final OtpErlangBinary b = (OtpErlangBinary) t1.elementAt(1);
                final byte[] bytes = b.binaryValue();
                toks = new ArrayList<ErlToken>(bytes.length / 10);
                for (int i = 0; i < bytes.length; i += 10) {
                    final ErlToken tk = new ErlToken(bytes, i, offset);
                    toks.add(tk);
                }
                return toks;
            }
            throw new ScannerException("unexpected token format");
        }
        throw new ScannerException("Could not parse string \"" + string + "\": "
                + t1.toString());
    }

    @Override
    public OtpErlangObject checkAll(final String module, final String text,
            final boolean getTokens) {
        if (module == null) {
            return null;
        }
        try {
            final OtpErlangObject o = backend.call(ERLIDE_SCANNER, "check_all", "aso",
                    module, text, getTokens);
            return o;
        } catch (final RpcException e) {
            ErlLogger.warn(e);
        }
        return null;

    }

}
