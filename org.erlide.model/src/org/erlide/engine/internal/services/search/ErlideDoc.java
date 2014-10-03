package org.erlide.engine.internal.services.search;

import java.util.Collection;
import java.util.List;

import org.erlide.engine.ErlangEngine;
import org.erlide.engine.services.search.OpenService;
import org.erlide.engine.services.search.OtpDocService;
import org.erlide.runtime.api.IOtpRpc;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.util.ErlLogger;
import org.erlide.util.ErlangFunctionCall;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlideDoc implements OtpDocService {

    private static final String ERLIDE_OTP_DOC = "erlide_otp_doc";
    private final IOtpRpc backend;

    public ErlideDoc(final IOtpRpc backend) {
        this.backend = backend;
    }

    @Override
    public OtpErlangObject getProposalsWithDoc(final IOtpRpc b, final String mod,
            final String prefix, final String stateDir) {
        OtpErlangObject res = null;
        try {
            res = b.call(ERLIDE_OTP_DOC, "get_proposals", "ass", mod, prefix, stateDir);
        } catch (final RpcException e) {
            ErlLogger.warn(e);
        }
        return res;
    }

    @Override
    public OtpErlangObject getModules(final IOtpRpc b, final String prefix,
            final List<String> projectModules, final boolean includes) {
        OtpErlangObject res = null;
        try {
            final String what = includes ? "includes" : "modules";
            res = b.call(ERLIDE_OTP_DOC, "get_modules", "slsa", prefix, projectModules,
                    what);
        } catch (final RpcException e) {
            ErlLogger.warn(e);
        }
        return res;
    }

    @Override
    public OtpErlangObject getOtpDoc(final IOtpRpc b,
            final ErlangFunctionCall functionCall, final String stateDir) {
        OtpErlangObject res = null;
        final OtpErlangTuple input = new OtpErlangTuple(new OtpErlangObject[] {
                new OtpErlangAtom("external"),
                new OtpErlangAtom(functionCall.getModule()),
                new OtpErlangAtom(functionCall.getName()),
                new OtpErlangInt(functionCall.getArity()), new OtpErlangString("") });
        try {
            res = b.call(ERLIDE_OTP_DOC, "get_doc", "sxs", functionCall.getModule(),
                    input, stateDir);
        } catch (final RpcException e) {
            ErlLogger.warn(e);
        }
        return res;
    }

    @Override
    @SuppressWarnings("boxing")
    public OtpErlangObject getOtpDoc(final IOtpRpc b, final int offset,
            final String stateDir, final String module,
            final Collection<OtpErlangObject> imports, final String externalModules,
            final OtpErlangList pathVars) {
        OtpErlangObject res = null;
        try {
            final OtpErlangObject input = backend.call(
                    "erlide_open",
                    "open",
                    "aix",
                    module,
                    offset,
                    ErlangEngine.getInstance().getService(OpenService.class)
                            .mkContext(externalModules, null, pathVars, null, imports));
            res = b.call(ERLIDE_OTP_DOC, "get_doc", "sxs", module, input, stateDir);
        } catch (final RpcException e) {
            ErlLogger.warn(e);
        }
        return res;
    }

    @Override
    public String getOtpDocLocation(final IOtpRpc b) {
        // OtpErlangObject res = null;
        // try {
        // // commented out since target doesn't exist
        // // res = b.rpcx("erlide_otp_doc", "get_otp_doc_location", "");
        // // return CoreUtil.stringValue(res);
        // } catch (final RpcException e) {
        // ErlLogger.warn(e);
        // } catch (final BackendException e) {
        // ErlLogger.warn(e);
        // }
        return "";
    }
}
