package org.erlide.engine.services.codeassist;

import java.util.ArrayList;
import java.util.List;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class RecordCompletion {

    private final int kind;
    private final String name;
    private String prefix;
    private final List<String> fields;

    public RecordCompletion(final OtpErlangTuple r) throws OtpErlangRangeException {
        final OtpErlangLong kindL = (OtpErlangLong) r.elementAt(0);
        final OtpErlangAtom nameA = (OtpErlangAtom) r.elementAt(1);
        final OtpErlangAtom prefixA = (OtpErlangAtom) r.elementAt(2);
        final OtpErlangList fieldL = (OtpErlangList) r.elementAt(3);
        kind = kindL.intValue();
        name = nameA.atomValue();
        prefix = prefixA.atomValue();
        // TODO we probably need another way to signal this...
        if (prefix.endsWith("><")) {
            prefix = "'" + prefix.substring(0, prefix.length() - 2);
        } else if ("<>".equals(prefix)) {
            prefix = "";
        } else {
            prefix = prefixA.toString();
            if (prefix.endsWith("'")) {
                prefix = prefix.substring(0, prefix.length() - 1);
            }
        }
        fields = new ArrayList<String>(fieldL.arity());
        for (final OtpErlangObject object : fieldL) {
            final OtpErlangAtom f = (OtpErlangAtom) object;
            getFields().add(f.atomValue());
        }
    }

    public boolean isNameWanted() {
        return kind == ContextAssistService.RECORD_NAME;
    }

    public boolean isFieldWanted() {
        return kind == ContextAssistService.RECORD_FIELD;
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
