package org.erlide.dialyzer.builder;

import java.util.Collection;
import java.util.List;

import org.eclipse.core.runtime.IPath;
import org.erlide.runtime.rpc.IOtpRpc;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.runtime.rpc.RpcFuture;
import org.erlide.util.ErlLogger;
import org.erlide.util.Util;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.google.common.collect.Lists;

public class ErlideDialyze {

    private static final String ERLIDE_DIALYZE = "erlide_dialyze";

    public static RpcFuture dialyze(final IOtpRpc backend, final Collection<String> files,
            final Collection<String> pltPaths, final Collection<IPath> includeDirs,
            final boolean fromSource, final Object noCheckPLT) throws RpcException {
        final List<String> incs = Lists.newArrayList();
        for (final IPath p : includeDirs) {
            incs.add(p.toString());
        }
        return backend.async_call(ErlideDialyze.ERLIDE_DIALYZE, "dialyze", "lslslsoo",
                files, pltPaths, incs, fromSource, noCheckPLT);
    }

    public static List<String> formatWarnings(final IOtpRpc backend,
            final OtpErlangList warnings) {
        final List<String> result = Lists.newArrayList();
        try {
            final OtpErlangList l = (OtpErlangList) backend
                    .call(ErlideDialyze.ERLIDE_DIALYZE, "format_warnings", "x", warnings);
            for (final OtpErlangObject o : l) {
                result.add(Util.stringValue(o).trim());
            }
        } catch (final RpcException e) {
            ErlLogger.error(e);
        }
        return result;
    }
}
