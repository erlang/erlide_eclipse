package org.erlide.core.services.search;

import java.util.Collection;
import java.util.List;

import org.erlide.runtime.IRpcSite;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.utils.ErlLogger;
import org.erlide.utils.ErlangFunctionCall;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlideDoc {
    public static OtpErlangObject getProposalsWithDoc(final IRpcSite b,
            final String mod, final String prefix, final String stateDir) {
        OtpErlangObject res = null;
        try {
            res = b.call("erlide_otp_doc", "get_proposals", "ass", mod, prefix,
                    stateDir);
        } catch (final RpcException e) {
            ErlLogger.warn(e);
        }
        return res;
    }

    public static OtpErlangObject getModules(final IRpcSite b,
            final String prefix, final List<String> projectModules,
            final boolean includes) {
        OtpErlangObject res = null;
        try {
            final String what = includes ? "includes" : "modules";
            res = b.call("erlide_otp_doc", "get_modules", "slsa", prefix,
                    projectModules, what);
        } catch (final RpcException e) {
            ErlLogger.warn(e);
        }
        return res;
    }

    public static OtpErlangObject getOtpDoc(final IRpcSite backend,
            final ErlangFunctionCall functionCall, final String stateDir) {
        OtpErlangObject res = null;
        final OtpErlangTuple input = new OtpErlangTuple(new OtpErlangObject[] {
                new OtpErlangAtom("external"),
                new OtpErlangAtom(functionCall.getModule()),
                new OtpErlangAtom(functionCall.getName()),
                new OtpErlangInt(functionCall.getArity()),
                new OtpErlangString("") });
        try {
            res = backend.call("erlide_otp_doc", "get_doc", "sxs",
                    functionCall.getModule(), input, stateDir);
        } catch (final RpcException e) {
            ErlLogger.warn(e);
        }
        return res;
    }

    @SuppressWarnings("boxing")
    public static OtpErlangObject getOtpDoc(final IRpcSite ide,
            final IRpcSite b, final int offset, final String stateDir,
            final String module, final Collection<OtpErlangObject> imports,
            final String externalModules, final OtpErlangList pathVars) {
        OtpErlangObject res = null;
        try {
            final OtpErlangObject input = ide.call("erlide_open", "open",
                    "aix", module, offset, ErlideOpen.mkContext(
                            externalModules, null, pathVars, null, imports));
            res = b.call("erlide_otp_doc", "get_doc", "sxs", module, input,
                    stateDir);
        } catch (final RpcException e) {
            ErlLogger.warn(e);
        }
        return res;
    }

    public static String getOtpDocLocation(final IRpcSite b) {
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
