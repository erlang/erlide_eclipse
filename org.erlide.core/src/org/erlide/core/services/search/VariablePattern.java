package org.erlide.core.services.search;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class VariablePattern extends ErlangSearchPattern {

    private final String functionName;
    private final int arity;
    private final String head;
    private final String name;

    public VariablePattern(final String functionName, final int arity,
            final String head, final String name, final LimitTo limitTo) {
        super(limitTo);
        this.functionName = functionName;
        this.arity = arity;
        this.head = head;
        this.name = name;
    }

    @Override
    public OtpErlangObject getSearchObject() {
        final OtpErlangObject t = makeSPatternObject(VARIABLE_DEF_ATOM,
                VARIABLE_REF_ATOM, name);
        return new OtpErlangTuple(new OtpErlangObject[] {
                VARIABLE_PATTERN_ATOM, t, new OtpErlangAtom(functionName),
                new OtpErlangLong(arity), new OtpErlangString(head) });
    }

    @Override
    public String patternString() {
        return "<variable>";
    }

    @Override
    public SearchFor getSearchFor() {
        return SearchFor.VARIABLE;
    }

    @Override
    public String labelString() {
        return name;
    }

}
