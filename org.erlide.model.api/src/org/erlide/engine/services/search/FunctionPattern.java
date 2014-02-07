package org.erlide.engine.services.search;

import org.erlide.engine.model.erlang.IErlModule;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;

public class FunctionPattern extends ErlangSearchPattern {

    private final String moduleName;
    private final String name;
    private final int arity;
    private final boolean matchAnyFunctionDefinition;
    private final boolean local;
    private final IErlModule module;

    public FunctionPattern(final String moduleName, final String name, final int arity,
            final LimitTo limitTo, final boolean matchAnyFunctionDefinition,
            final IErlModule module, final boolean local) {
        super(limitTo);
        this.moduleName = moduleName;
        this.name = name;
        this.arity = arity;
        this.matchAnyFunctionDefinition = matchAnyFunctionDefinition;
        this.module = module;
        this.local = local;
    }

    @Override
    public OtpErlangObject getSearchObject() {
        if (moduleName == null || moduleName.length() == 0) {
            return makeFAPatternObject(FUNCTION_DEF_ATOM, FUNCTION_CALL_ATOM, name, arity);
        }
        final OtpErlangAtom defA = matchAnyFunctionDefinition ? FUNCTION_DEF_ATOM
                : FUNCTION_DEF_MOD_ATOM;
        return makeMFAPatternObject(defA, EXTERNAL_CALL_ATOM, moduleName, name, arity,
                matchAnyFunctionDefinition);
    }

    @Override
    public String patternString() {
        final String s = name + "/" + arity;
        if (moduleName == null || limitTo != LimitTo.REFERENCES) {
            return s;
        }
        return moduleName + ":" + s;
    }

    @Override
    public SearchFor getSearchFor() {
        return SearchFor.FUNCTION;
    }

    @Override
    public String labelString() {
        final String s = name + "/" + arity;
        if (moduleName == null || limitTo != LimitTo.REFERENCES) {
            return s;
        }
        return moduleName + ":" + s;
    }

    @Override
    public ErlSearchScope reduceScope(final ErlSearchScope scope) {
        if (local && scope.getModules().contains(module)) {
            return new ErlSearchScope(module);
        }
        return scope;
    }

}
