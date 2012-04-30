package org.erlide.core.internal.services.builder;

import java.util.Collection;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.erlide.backend.BackendCore;
import org.erlide.backend.IBackend;
import org.erlide.backend.IBackendManager;
import org.erlide.jinterface.ErlLogger;
import org.erlide.jinterface.rpc.IRpcFuture;
import org.erlide.jinterface.rpc.RpcException;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.google.common.collect.Lists;

public class InternalErlideBuilder {

    public static IRpcFuture compileErl(final IBackend backend, final IPath fn,
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

    public static OtpErlangList getSourceClashes(final IBackend backend,
            final String[] dirList) throws RpcException {
        final OtpErlangObject res = backend.call("erlide_builder",
                "source_clash", "ls", (Object) dirList);
        if (res instanceof OtpErlangList) {
            return (OtpErlangList) res;
        }
        throw new RpcException("bad result from erlide_builder:source_clash: "
                + res);
    }

    public static OtpErlangList getCodeClashes(final IBackend b)
            throws RpcException {
        final OtpErlangList res = (OtpErlangList) b.call("erlide_builder",
                "code_clash", null);
        return res;
    }

    public static void loadModule(final IProject project, final String module) {
        try {
            final IBackendManager backendManager = BackendCore
                    .getBackendManager();
            for (final IBackend b : backendManager
                    .getExecutionBackends(project)) {
                ErlLogger.debug(":: loading %s in %s", module, b
                        .getRuntimeInfo().toString());
                if (b.isDistributed()) {
                    b.call("erlide_builder", "load", "ao", module,
                            b.doLoadOnAllNodes());
                }
                backendManager.moduleLoaded(b, project, module);
            }
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }
    }

    public static IRpcFuture compileYrl(final IBackend backend,
            final String fn, final String output) {
        try {
            return backend.async_call("erlide_builder", "compile_yrl", "ss",
                    fn, output);
        } catch (final Exception e) {
            ErlLogger.debug(e);
            return null;
        }
    }

}
