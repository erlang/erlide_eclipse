package org.erlide.core.services.codeassist;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

import org.erlide.backend.IBackend;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.utils.Util;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlideContextAssist {

    public static Collection<String> getVariables(final IBackend b,
            final String src, final String prefix) {
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
            e.printStackTrace();
        }
        return result;
    }

    // -define(NO_RECORD, 0).
    // -define(RECORD_NAME, 1).
    // -define(RECORD_FIELD, 2).
    public static final int RECORD_NAME = 1;
    public static final int RECORD_FIELD = 2;

    public static class RecordCompletion {

        private final int kind;
        private final String name;
        private String prefix;
        private final List<String> fields;

        public RecordCompletion(final OtpErlangTuple r)
                throws OtpErlangRangeException {
            final OtpErlangLong kindL = (OtpErlangLong) r.elementAt(0);
            final OtpErlangAtom nameA = (OtpErlangAtom) r.elementAt(1);
            final OtpErlangAtom prefixA = (OtpErlangAtom) r.elementAt(2);
            final OtpErlangList fieldL = (OtpErlangList) r.elementAt(3);
            kind = kindL.intValue();
            name = nameA.atomValue();
            prefix = prefixA.atomValue();
            if ("><".equals(prefix)) {
                prefix = "'";
            } else if ("<>".equals(prefix)) {
                prefix = "";
            }
            fields = new ArrayList<String>(fieldL.arity());
            for (final OtpErlangObject object : fieldL) {
                final OtpErlangAtom f = (OtpErlangAtom) object;
                getFields().add(f.atomValue());
            }
        }

        public boolean isNameWanted() {
            return kind == RECORD_NAME;
        }

        public boolean isFieldWanted() {
            return kind == RECORD_FIELD;
        }

        public String getName() {
            return name;
        }

        public String getPrefix() {
            return prefix;
        }

        public List<String> getFields() {
            return fields;
        }

    }

    public static RecordCompletion checkRecordCompletion(final IBackend b,
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
            e.printStackTrace();
        } catch (final OtpErlangRangeException e) {
            e.printStackTrace();
        }
        return null;
    }

    @SuppressWarnings("boxing")
    public static OtpErlangList getFunctionHead(final IBackend b,
            final String name, final int arity) {
        try {
            final OtpErlangObject res = b.call("erlide_content_assist",
                    "get_function_head", "ai", name, arity);
            if (res instanceof OtpErlangList) {
                return (OtpErlangList) res;
            }
        } catch (final RpcException e) {
            e.printStackTrace();
        }
        return null;
    }

}
