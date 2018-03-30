package org.erlide.engine.services.search;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IPath;
import org.erlide.engine.model.erlang.FunctionRef;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.runtime.rpc.IOtpRpc;
import org.erlide.runtime.rpc.RpcFuture;
import org.erlide.util.ErlLogger;
import org.erlide.util.erlang.OtpBindings;
import org.erlide.util.erlang.OtpErlang;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

public class ErlangXref {

    private static final String ERLIDE_XREF = "erlide_xref";
    private final IOtpRpc backend;

    public ErlangXref(final IOtpRpc backend) {
        this.backend = backend;
    }

    public void start() {
        try {
            backend.call(ErlangXref.ERLIDE_XREF, "start", "");
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }

    }

    public void stop() {
        try {
            backend.call(ErlangXref.ERLIDE_XREF, "stop", "");
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }

    }

    public RpcFuture addProject(final IErlProject project) {
        try {
            final IPath outputLocation = project.getWorkspaceProject()
                    .getFolder(project.getProperties().getOutputDir()).getLocation();
            final String loc = outputLocation.toString();
            return backend.async_call(ErlangXref.ERLIDE_XREF, "add_project", "s", loc);
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }
        return null;
    }

    public void update() {
        try {
            backend.call(ErlangXref.ERLIDE_XREF, "update", "");
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }
    }

    @SuppressWarnings("boxing")
    public FunctionRef[] functionUse(final String mod, final String fun,
            final int arity) {
        try {
            final OtpErlangObject r = backend.call(ErlangXref.ERLIDE_XREF, "function_use", "aai",
                    mod.substring(0, mod.length() - 4), fun, arity);
            final OtpBindings bind = OtpErlang.match("{ok, L}", r);
            if (bind == null) {
                return new FunctionRef[0];
            }
            final OtpErlangList l = (OtpErlangList) bind.get("L");
            final List<FunctionRef> result = new ArrayList<>();
            for (final OtpErlangObject e : l) {
                result.add(new FunctionRef(e));
            }
            return result.toArray(new FunctionRef[result.size()]);
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }
        return null;
    }

    public FunctionRef[] functionUse(final FunctionRef ref) {
        return functionUse(ref.module, ref.function, ref.arity);
    }

}
