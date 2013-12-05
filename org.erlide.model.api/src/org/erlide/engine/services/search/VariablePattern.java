package org.erlide.engine.services.search;

import org.erlide.engine.model.erlang.IErlModule;

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
    private final IErlModule module;

    public VariablePattern(final String functionName, final int arity, final String head,
            final String name, final LimitTo limitTo, final IErlModule module) {
        super(limitTo);
        this.functionName = functionName;
        this.arity = arity;
        this.head = head;
        this.name = name;
        this.module = module;
    }

    @Override
    public OtpErlangObject getSearchObject() {
        final OtpErlangObject t = makeSPatternObject(VARIABLE_DEF_ATOM,
                VARIABLE_REF_ATOM, name);
        return new OtpErlangTuple(new OtpErlangObject[] { VARIABLE_PATTERN_ATOM, t,
                new OtpErlangAtom(functionName), new OtpErlangLong(arity),
                new OtpErlangString(head) });
    }

    @Override
    public String patternString() {
        return name;
    }

    @Override
    public SearchFor getSearchFor() {
        return SearchFor.VARIABLE;
    }

    @Override
    public String labelString() {
        return name;
    }

    @Override
    public ErlSearchScope reduceScope(final ErlSearchScope scope) {
        if (scope.getModules().contains(module)) {
            return new ErlSearchScope(module);
        }
        return scope;
    }
}
