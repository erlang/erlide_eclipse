package org.erlide.engine.internal.services.codeassist;

import java.util.Collection;
import java.util.SortedSet;
import java.util.TreeSet;

import org.erlide.engine.services.codeassist.ContextAssistService;
import org.erlide.engine.services.codeassist.RecordCompletion;
import org.erlide.runtime.api.IOtpRpc;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.util.ErlLogger;
import org.erlide.util.Util;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlideContextAssist implements ContextAssistService {

    private final IOtpRpc backend;

    public ErlideContextAssist(final IOtpRpc backend) {
        this.backend = backend;
    }

    @Override
    public Collection<String> getVariables(final String src, final String prefix) {
        final SortedSet<String> result = new TreeSet<String>();
        try {
            final OtpErlangObject res = backend.call("erlide_content_assist",
                    "get_variables", "ss", src, prefix);
            if (Util.isOk(res)) {
                final OtpErlangTuple t = (OtpErlangTuple) res;
                final OtpErlangList l = (OtpErlangList) t.elementAt(1);
                for (final OtpErlangObject i : l) {
                    result.add(Util.stringValue(i));
                }
            }
        } catch (final RpcException e) {
            ErlLogger.error(e);
        }
        return result;
    }

    @Override
    public RecordCompletion checkRecordCompletion(final IOtpRpc buildBackend,
            final String prefix) {
        try {
            final OtpErlangObject res = buildBackend.call("erlide_content_assist",
                    "check_record", "s", prefix);
            if (Util.isOk(res)) {
                final OtpErlangTuple t = (OtpErlangTuple) res;
                final OtpErlangTuple r = (OtpErlangTuple) t.elementAt(1);
                return new RecordCompletion(r);
            }
        } catch (final RpcException e) {
            ErlLogger.error(e);
        } catch (final OtpErlangRangeException e) {
            ErlLogger.error(e);
        }
        return null;
    }

    @Override
    @SuppressWarnings("boxing")
    public OtpErlangList getFunctionHead(final String name, final int arity) {
        try {
            final OtpErlangObject res = backend.call("erlide_content_assist",
                    "get_function_head", "ai", name, arity);
            if (res instanceof OtpErlangList) {
                return (OtpErlangList) res;
            }
        } catch (final RpcException e) {
            ErlLogger.error(e);
        }
        return null;
    }

}
