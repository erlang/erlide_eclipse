package org.erlide.dialyzer.builder;

import java.util.Collection;
import java.util.List;

import org.eclipse.core.runtime.IPath;
import org.erlide.runtime.api.IOtpRpc;
import org.erlide.runtime.rpc.RpcFuture;
import org.erlide.runtime.rpc.IRpcResultCallback;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.util.ErlLogger;
import org.erlide.util.Util;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.google.common.collect.Lists;

public class ErlideDialyze {

    private static final String ERLIDE_DIALYZE = "erlide_dialyze";
    private static final int LONG_TIMEOUT = 60000;
    // private static final int FILE_TIMEOUT = 20000;
    // private static final int INCLUDE_TIMEOUT = 40000;
    private static final int UPDATE_TIMEOUT = LONG_TIMEOUT * 10;

    public static RpcFuture dialyze(final IOtpRpc backend,
            final Collection<String> files, final Collection<String> pltPaths,
            final Collection<IPath> includeDirs, final boolean fromSource,
            final Object noCheckPLT) throws RpcException {
        final List<String> incs = Lists.newArrayList();
        for (final IPath p : includeDirs) {
            incs.add(p.toString());
        }
        return backend.async_call(ERLIDE_DIALYZE, "dialyze", "lslslsoo", files, pltPaths,
                incs, fromSource, noCheckPLT);
    }

    public static void startDialyzer(final IOtpRpc backend,
            final Collection<String> files, final Collection<String> pltPaths,
            final Collection<IPath> includeDirs, final boolean fromSource,
            final Object noCheckPLT, final IRpcResultCallback callback)
            throws RpcException {
        final List<String> incs = Lists.newArrayList();
        for (final IPath p : includeDirs) {
            incs.add(p.toString());
        }
        backend.async_call_result(callback, ERLIDE_DIALYZE, "start_dialyze", "xlslslsoo",
                files, pltPaths, incs, fromSource, noCheckPLT);
        // ErlLogger.debug("result %s", result.toString());
    }

    public static List<String> formatWarnings(final IOtpRpc backend,
            final OtpErlangList warnings) {
        final List<String> result = Lists.newArrayList();
        try {
            final OtpErlangList l = (OtpErlangList) backend.call(ERLIDE_DIALYZE,
                    "format_warnings", "x", warnings);
            for (final OtpErlangObject o : l) {
                result.add(Util.stringValue(o).trim());
            }
        } catch (final RpcException e) {
            ErlLogger.error(e);
        }
        return result;
    }

    public static OtpErlangObject checkPlt(final IOtpRpc backend, final String plt,
            final List<String> ebinDirs) throws RpcException {
        if (ebinDirs == null) {
            return backend.call(UPDATE_TIMEOUT, ERLIDE_DIALYZE, "check_plt", "s", plt);
        }
        return backend.call(UPDATE_TIMEOUT, ERLIDE_DIALYZE,
                "update_plt_with_additional_paths", "sls", plt, ebinDirs);
    }

    public static void startCheckPlt(final IOtpRpc backend, final String plt,
            final List<String> ebinDirs, final IRpcResultCallback callback)
            throws RpcException {
        backend.async_call_result(callback, ERLIDE_DIALYZE,
                "start_update_plt_with_additional_paths", "xsls", plt, ebinDirs);
    }
}
