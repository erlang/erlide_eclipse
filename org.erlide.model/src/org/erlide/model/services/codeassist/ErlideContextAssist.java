package org.erlide.model.services.codeassist;

import java.util.Collection;
import java.util.SortedSet;
import java.util.TreeSet;

import org.erlide.runtime.api.IRpcSite;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.util.ErlLogger;
import org.erlide.util.Util;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlideContextAssist implements ContextAssistService {

    @Override
    public Collection<String> getVariables(final IRpcSite b, final String src,
            final String prefix) {
        final SortedSet<String> result = new TreeSet<String>();
        try {
            final OtpErlangObject res = b.call("erlide_content_assist",
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
    public RecordCompletion checkRecordCompletion(final IRpcSite b,
            final String substring) {
        try {
            final OtpErlangObject res = b.call("erlide_content_assist",
                    "check_record", "s", substring);
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
    public OtpErlangList getFunctionHead(final IRpcSite b, final String name,
            final int arity) {
        try {
            final OtpErlangObject res = b.call("erlide_content_assist",
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
