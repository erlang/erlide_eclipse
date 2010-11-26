package erlang;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IPath;
import org.erlide.core.erlang.IErlProject;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.rpc.RpcFuture;
import org.erlide.jinterface.util.Bindings;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.jinterface.util.ErlUtils;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

public final class ErlangXref {

    public static void start(final Backend b) {
        try {
            b.call("erlide_xref", "start", "");
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }

    }

    public static void stop(final Backend b) {
        try {
            b.call("erlide_xref", "stop", "");
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }

    }

    public static RpcFuture addProject(final Backend b,
            final IErlProject project) {
        try {
            final IPath outputLocation = project.getProject()
                    .getFolder(project.getOutputLocation()).getLocation();
            final String loc = outputLocation.toString();
            return b.async_call("erlide_xref", "add_project", "s", loc);
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }
        return null;
    }

    public static void update(final Backend b) {
        try {
            b.call("erlide_xref", "update", "");
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }
    }

    @SuppressWarnings("boxing")
    public static FunctionRef[] functionUse(final Backend b, final String mod,
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
            for (final OtpErlangObject e : l.elements()) {
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

    public static FunctionRef[] functionUse(final Backend b,
            final FunctionRef ref) {
        return functionUse(b, ref.module, ref.function, ref.arity);
    }

}
