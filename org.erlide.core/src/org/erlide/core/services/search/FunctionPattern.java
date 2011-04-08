package org.erlide.core.services.search;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;

public class FunctionPattern extends ErlangSearchPattern {

    private final String module;
    private final String name;
    private final int arity;
    private final boolean matchAnyFunctionDefinition;

    public FunctionPattern(final String module, final String name,
            final int arity, final ErlangSearchPattern.LimitTo limitTo,
            final boolean matchAnyFunctionDefinition) {
        super(limitTo);
        this.module = module;
        this.name = name;
        this.arity = arity;
        this.matchAnyFunctionDefinition = matchAnyFunctionDefinition;
    }

    @Override
    public OtpErlangObject getSearchObject() {
        if (module == null || module.length() == 0) {
            return makeFAPatternObject(FUNCTION_DEF_ATOM, FUNCTION_CALL_ATOM,
                    name, arity);
        } else {
            final OtpErlangAtom defA = matchAnyFunctionDefinition ? FUNCTION_DEF_ATOM
                    : FUNCTION_DEF_MOD_ATOM;
            return makeMFAPatternObject(defA, EXTERNAL_CALL_ATOM, module, name,
                    arity, matchAnyFunctionDefinition);
        }
    }

    @Override
    public String patternString() {
        final String s = name + "/" + arity;
        if (module == null || limitTo != LimitTo.REFERENCES) {
            return s;
        } else {
            return module + ":" + s;
        }
    }

    @Override
    public SearchFor getSearchFor() {
        return SearchFor.FUNCTION;
    }

    @Override
    public String labelString() {
        final String s = name + "/" + arity;
        if (module == null || limitTo != LimitTo.REFERENCES) {
            return s;
        } else {
            return module + ":" + s;
        }
    }

}
