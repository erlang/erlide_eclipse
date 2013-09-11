package org.erlide.dialyzer.builder;

import java.io.File;
import java.util.Collection;
import java.util.List;

import org.eclipse.core.runtime.IPath;
import org.erlide.runtime.api.IRpcSite;
import org.erlide.runtime.rpc.IRpcFuture;
import org.erlide.runtime.rpc.IRpcResultCallback;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.util.ErlLogger;
import org.erlide.util.Util;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.collect.Lists;

public class ErlideDialyze {

    private static final String ERLIDE_DIALYZE = "erlide_dialyze";
    private static final int LONG_TIMEOUT = 60000;
    // private static final int FILE_TIMEOUT = 20000;
    // private static final int INCLUDE_TIMEOUT = 40000;
    private static final int UPDATE_TIMEOUT = LONG_TIMEOUT * 10;

    public static IRpcFuture dialyze(final IRpcSite backend,
            final Collection<String> files, final Collection<String> pltPaths,
            final Collection<IPath> includeDirs, final boolean fromSource,
            final Object noCheckPLT) throws RpcException {
        final List<String> incs = Lists.newArrayList();
        for (final IPath p : includeDirs) {
            incs.add(p.toString());
        }
        return backend.async_call(ERLIDE_DIALYZE, "dialyze", "lslslsoo", files,
                pltPaths, incs, fromSource, noCheckPLT);
    }

    public static void startDialyzer(final IRpcSite backend,
            final Collection<String> files, final Collection<String> pltPaths,
            final Collection<IPath> includeDirs, final boolean fromSource,
            final Object noCheckPLT, final IRpcResultCallback callback)
            throws RpcException {
        final List<String> incs = Lists.newArrayList();
        for (final IPath p : includeDirs) {
            incs.add(p.toString());
        }
        backend.async_call_result(callback, ERLIDE_DIALYZE, "start_dialyze",
                "xlslslsoo", files, pltPaths, incs, fromSource, noCheckPLT);
        // ErlLogger.debug("result %s", result.toString());
    }

    public static List<String> formatWarnings(final IRpcSite backend,
            final OtpErlangList warnings) {
        final List<String> result = Lists.newArrayList();
        try {
            final OtpErlangList l = (OtpErlangList) backend.call(
                    ERLIDE_DIALYZE, "format_warnings", "x", warnings);
            for (final OtpErlangObject o : l) {
                result.add(Util.stringValue(o).trim());
            }
        } catch (final RpcException e) {
            ErlLogger.error(e);
        }
        return result;
    }

    public static OtpErlangObject checkPlt(final IRpcSite backend,
            final String plt, final List<String> ebinDirs) throws RpcException {
        if (ebinDirs == null) {
            return backend.call(UPDATE_TIMEOUT, ERLIDE_DIALYZE, "check_plt",
                    "s", plt);
        }
        return backend.call(UPDATE_TIMEOUT, ERLIDE_DIALYZE,
                "update_plt_with_additional_paths", "sls", plt, ebinDirs);
    }

    public static List<String> getPltFiles(final IRpcSite backend,
            final String pltFiles) throws RpcException {
        final OtpErlangObject o = backend.call(ERLIDE_DIALYZE, "get_plt_files",
                "s", pltFiles);
        if (Util.isOk(o)) {
            final OtpErlangTuple t = (OtpErlangTuple) o;
            final OtpErlangObject e1 = t.elementAt(1);
            if (e1 instanceof OtpErlangList) {
                final OtpErlangList l = (OtpErlangList) e1;
                final List<String> result = Lists.newArrayListWithCapacity(l
                        .arity());
                for (final OtpErlangObject i : l) {
                    final String pltFilePath = Util.stringValue(i);
                    if (new File(pltFilePath).exists()) {
                        result.add(pltFilePath);
                    }
                }
                return result;
            }
        }
        return null;
    }

    public static void startCheckPlt(final IRpcSite backend, final String plt,
            final List<String> ebinDirs, final IRpcResultCallback callback)
            throws RpcException {
        backend.async_call_result(callback, ERLIDE_DIALYZE,
                "start_update_plt_with_additional_paths", "xsls", plt, ebinDirs);
    }
}
