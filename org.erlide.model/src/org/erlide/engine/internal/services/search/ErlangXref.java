package org.erlide.engine.internal.services.search;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IPath;
import org.erlide.engine.model.erlang.FunctionRef;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.services.search.XrefService;
import org.erlide.runtime.api.IOtpRpc;
import org.erlide.runtime.rpc.RpcFuture;
import org.erlide.util.ErlLogger;
import org.erlide.util.erlang.Bindings;
import org.erlide.util.erlang.ErlUtils;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

public class ErlangXref implements XrefService {

    private static final String ERLIDE_XREF = "erlide_xref";
    private final IOtpRpc backend;

    public ErlangXref(final IOtpRpc backend) {
        this.backend = backend;
    }

    @Override
    public void start() {
        try {
            backend.call(ERLIDE_XREF, "start", "");
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }

    }

    @Override
    public void stop() {
        try {
            backend.call(ERLIDE_XREF, "stop", "");
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }

    }

    @Override
    public RpcFuture addProject(final IErlProject project) {
        try {
            final IPath outputLocation = project.getWorkspaceProject()
                    .getFolder(project.getProperties().getOutputDir()).getLocation();
            final String loc = outputLocation.toString();
            return backend.async_call(ERLIDE_XREF, "add_project", "s", loc);
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }
        return null;
    }

    @Override
    public void update() {
        try {
            backend.call(ERLIDE_XREF, "update", "");
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }
    }

    @Override
    @SuppressWarnings("boxing")
    public FunctionRef[] functionUse(final String mod, final String fun, final int arity) {
        try {
            final OtpErlangObject r = backend.call(ERLIDE_XREF, "function_use", "aai",
                    mod, fun, arity);
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

    @Override
    public FunctionRef[] functionUse(final FunctionRef ref) {
        return functionUse(ref.module, ref.function, ref.arity);
    }

}
