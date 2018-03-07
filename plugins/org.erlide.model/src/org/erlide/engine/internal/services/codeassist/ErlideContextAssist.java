package org.erlide.engine.internal.services.codeassist;

import java.util.Collection;
import java.util.SortedSet;
import java.util.TreeSet;

import org.erlide.runtime.rpc.IOtpRpc;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.util.ErlLogger;
import org.erlide.util.Util;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlideContextAssist {

    // -define(NO_RECORD, 0).
    // -define(RECORD_NAME, 1).
    // -define(RECORD_FIELD, 2).
    public static final int RECORD_NAME = 1;
    public static final int RECORD_FIELD = 2;

    private final IOtpRpc backend;

    public ErlideContextAssist(final IOtpRpc backend) {
        this.backend = backend;
    }

    public Collection<String> getVariables(final String src, final String prefix) {
        final SortedSet<String> result = new TreeSet<>();
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

}
