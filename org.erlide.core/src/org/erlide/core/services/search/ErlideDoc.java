package org.erlide.core.services.search;

import java.util.Collection;
import java.util.List;

import org.erlide.backend.IBackend;
import org.erlide.jinterface.ErlLogger;
import org.erlide.jinterface.rpc.RpcException;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

public class ErlideDoc {
    public static OtpErlangObject getProposalsWithDoc(final IBackend b,
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

    public static OtpErlangObject getModules(final IBackend b,
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

    @SuppressWarnings("boxing")
    public static OtpErlangObject getOtpDoc(final IBackend ide,
            final IBackend b, final int offset, final String stateDir,
            final String module, final Collection<OtpErlangObject> imports,
            final String externalModules, final OtpErlangList pathVars) {
        OtpErlangObject res = null;
        // ErlLogger.debug("getDoc:: %s %s %s", module, offset, imports);
        try {
            final OtpErlangObject input = ide.call("erlide_open", "open",
                    "aix", module, offset, ErlideOpen.mkContext(
                            externalModules, null, pathVars, null, imports));
            // ErlLogger.debug("%s", input.toString());
            res = b.call("erlide_otp_doc", "get_doc", "sxs", module, input,
                    stateDir);
        } catch (final RpcException e) {
            ErlLogger.warn(e);
        }
        return res;
    }

    public static String getOtpDocLocation(final IBackend b) {
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
