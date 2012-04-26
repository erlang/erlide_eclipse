package org.erlide.core.services.builder;

import java.util.Collection;
import java.util.List;

import org.eclipse.core.runtime.IPath;
import org.erlide.backend.IBackend;
import org.erlide.jinterface.ErlLogger;
import org.erlide.jinterface.rpc.IRpcResultCallback;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.jinterface.rpc.RpcTimeoutException;
import org.erlide.utils.Util;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.collect.Lists;

public class ErlideDialyze {

    private static final int LONG_TIMEOUT = 60000;
    private static final int FILE_TIMEOUT = 20000;
    private static final int INCLUDE_TIMEOUT = 40000;
    private static final int UPDATE_TIMEOUT = LONG_TIMEOUT * 10;

    public static OtpErlangObject dialyze(final IBackend backend,
            final Collection<String> files, final Collection<String> pltPaths,
            final Collection<IPath> includeDirs, final boolean fromSource,
            final Object noCheckPLT) {
        final List<String> incs = Lists.newArrayList();
        for (final IPath p : includeDirs) {
            incs.add(p.toString());
        }
        try {
            final int timeout = files.size() * FILE_TIMEOUT
                    + includeDirs.size() * INCLUDE_TIMEOUT + LONG_TIMEOUT;
            final OtpErlangObject result = backend.call(timeout,
                    "erlide_dialyze", "dialyze", "lslslsoo", files, pltPaths,
                    incs, fromSource, noCheckPLT);
            // ErlLogger.debug("result %s", result.toString());
            return result;
        } catch (final RpcTimeoutException e) {
            if (!backend.isStopped()) {
                ErlLogger.warn(e);
            }
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }
        return null;
    }

    public static void startDialyzer(final IBackend backend,
            final Collection<String> files, final Collection<String> pltPaths,
            final Collection<IPath> includeDirs, final boolean fromSource,
            final Object noCheckPLT, final IRpcResultCallback callback)
            throws RpcException {
        final List<String> incs = Lists.newArrayList();
        for (final IPath p : includeDirs) {
            incs.add(p.toString());
        }
        backend.async_call_result(callback, "erlide_dialyze", "start_dialyze",
                "xlslslsoo", files, pltPaths, incs, fromSource, noCheckPLT);
        // ErlLogger.debug("result %s", result.toString());
    }

    public static String formatWarning(final IBackend backend,
            final OtpErlangObject warning) {
        try {
            final OtpErlangObject result = backend.call("erlide_dialyze",
                    "format_warning", "x", warning);
            return Util.stringValue(result);
        } catch (final RpcException e) {
            e.printStackTrace();
        }
        return warning.toString();
    }

    public static OtpErlangObject checkPlt(final IBackend backend,
            final String plt, final List<String> ebinDirs) throws RpcException {
        if (ebinDirs == null) {
            return backend.call(UPDATE_TIMEOUT, "erlide_dialyze", "check_plt",
                    "s", plt);
        } else {
            return backend.call(UPDATE_TIMEOUT, "erlide_dialyze",
                    "update_plt_with_additional_paths", "sls", plt, ebinDirs);
        }
    }

    public static List<String> getPltFiles(final IBackend backend,
            final String pltFiles) throws RpcException {
        final OtpErlangObject o = backend.call("erlide_dialyze",
                "get_plt_files", "s", pltFiles);
        if (Util.isOk(o)) {
            final OtpErlangTuple t = (OtpErlangTuple) o;
            final OtpErlangObject e1 = t.elementAt(1);
            if (e1 instanceof OtpErlangList) {
                final OtpErlangList l = (OtpErlangList) e1;
                final List<String> result = Lists.newArrayListWithCapacity(l
                        .arity());
                for (final OtpErlangObject i : l) {
                    result.add(Util.stringValue(i));
                }
                return result;
            }
        }
        return null;
    }

    public static void startCheckPlt(final IBackend backend, final String plt,
            final List<String> ebinDirs, final IRpcResultCallback callback)
            throws RpcException {
        backend.async_call_result(callback, "erlide_dialyze",
                "start_update_plt_with_additional_paths", "xsls", plt, ebinDirs);
    }
}
