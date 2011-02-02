package erlang;

import java.util.Collection;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.util.BeamUtil;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.rpc.RpcFuture;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.ErlideBackend;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.google.common.collect.Lists;

public class ErlideBuilder {

    public static RpcFuture compileErl(final Backend backend, final IPath fn,
            final String outputdir, final Collection<IPath> includedirs,
            final OtpErlangList compilerOptions) {
        final List<String> incs = Lists.newArrayList();
        for (final IPath p : includedirs) {
            incs.add(p.toString());
        }
        try {
            return backend.async_call("erlide_builder", "compile", "sslsx",
                    fn.toString(), outputdir, incs, compilerOptions);
        } catch (final Exception e) {
            ErlLogger.debug(e);
            return null;
        }
    }

    public static OtpErlangList getSourceClashes(final Backend backend,
            final String[] dirList) throws BackendException {
        final OtpErlangObject res = backend.call("erlide_builder",
                "source_clash", "ls", (Object) dirList);
        if (res instanceof OtpErlangList) {
            return (OtpErlangList) res;
        }
        throw new BackendException(
                "bad result from erlide_builder:source_clash: " + res);
    }

    public static OtpErlangList getCodeClashes(final Backend b)
            throws BackendException {
        final OtpErlangList res = (OtpErlangList) b.call("erlide_builder",
                "code_clash", null);
        return res;
    }

    public static void loadModule(final IProject project, final String module) {
        try {
            final BackendManager backendManager = ErlangCore
                    .getBackendManager();
            for (final ErlideBackend b : backendManager
                    .getExecutionBackends(project)) {
                ErlLogger.debug(":: loading %s in %s", module, b.getInfo()
                        .toString());
                if (b.isDistributed()) {
                    b.call("erlide_builder", "load", "ao", module,
                            b.doLoadOnAllNodes());
                } else {
                    BeamUtil.loadModuleViaInput(b, project, module);
                }
                backendManager.moduleLoaded(b, project, module);
            }
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }
    }

    public static RpcFuture compileYrl(final Backend backend, final String fn,
            final String output) {
        try {
            return backend.async_call("erlide_builder", "compile_yrl", "ss",
                    fn, output);
        } catch (final Exception e) {
            ErlLogger.debug(e);
            return null;
        }
    }

}
