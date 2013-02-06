package org.erlide.core.services.search;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IPath;
import org.erlide.core.model.erlang.FunctionRef;
import org.erlide.core.model.root.IErlProject;
import org.erlide.runtime.Bindings;
import org.erlide.runtime.ErlUtils;
import org.erlide.runtime.IRpcSite;
import org.erlide.runtime.rpc.IRpcFuture;
import org.erlide.utils.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

public final class ErlangXref {

    public static void start(final IRpcSite b) {
        try {
            b.call("erlide_xref", "start", "");
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }

    }

    public static void stop(final IRpcSite b) {
        try {
            b.call("erlide_xref", "stop", "");
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }

    }

    public static IRpcFuture addProject(final IRpcSite b,
            final IErlProject project) {
        try {
            final IPath outputLocation = project.getWorkspaceProject()
                    .getFolder(project.getOutputLocation()).getLocation();
            final String loc = outputLocation.toString();
            return b.async_call("erlide_xref", "add_project", "s", loc);
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }
        return null;
    }

    public static void update(final IRpcSite b) {
        try {
            b.call("erlide_xref", "update", "");
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }
    }

    @SuppressWarnings("boxing")
    public static FunctionRef[] functionUse(final IRpcSite b, final String mod,
            final String fun, final int arity) {
        try {
            final OtpErlangObject r = b.call("erlide_xref", "function_use",
                    "aai", mod, fun, arity);
            final Bindings bind = ErlUtils.match("{ok, L}", r);
            if (bind == null) {
                return new FunctionRef[0];
            }
            final OtpErlangList l = (OtpErlangList) bind.get("L");
            final List<FunctionRef> result = new ArrayList<FunctionRef>();
            for (final OtpErlangObject e : l) {
                result.add(new FunctionRef(e));
            }
            return result.toArray(new FunctionRef[result.size()]);
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }
        return null;
    }

    private ErlangXref() {
    }

    public static FunctionRef[] functionUse(final IRpcSite b,
            final FunctionRef ref) {
        return functionUse(b, ref.module, ref.function, ref.arity);
    }

}
