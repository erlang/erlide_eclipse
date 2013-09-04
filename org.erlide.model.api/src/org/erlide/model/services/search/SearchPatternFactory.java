package org.erlide.model.services.search;

import org.erlide.engine.ErlangEngine;
import org.erlide.model.erlang.IErlAttribute;
import org.erlide.model.erlang.IErlFunction;
import org.erlide.model.erlang.IErlFunctionClause;
import org.erlide.model.erlang.IErlMacroDef;
import org.erlide.model.erlang.IErlModule;
import org.erlide.model.erlang.IErlRecordDef;
import org.erlide.model.root.IErlElement;
import org.erlide.util.StringUtils;
import org.erlide.util.SystemConfiguration;
import org.erlide.util.Util;

public class SearchPatternFactory {

    public static ErlangSearchPattern getSearchPattern(
            final SearchFor searchFor, final String moduleName,
            final String name, final int arity, final LimitTo limitTo,
            final IErlModule module) {
        switch (searchFor) {
        case FUNCTION:
            return new FunctionPattern(moduleName, name, arity, limitTo, true,
                    null, false);
        case INCLUDE:
            return new IncludePattern(name, limitTo);
        case MACRO:
            return new MacroPattern(name, limitTo);
        case RECORD:
            return new RecordPattern(name, limitTo);
        case TYPE:
            return new TypeRefPattern(moduleName, name, limitTo);
        case RECORD_FIELD:
            return new RecordFieldPattern(moduleName, name, limitTo);
        case VARIABLE:
            return new VariablePattern("", -1, "", name, limitTo, module);
        }
        return null;
    }

    public static ErlangSearchPattern getSearchPatternFromErlElementAndLimitTo(
            final IErlElement element, final LimitTo limitTo) {
        if (element instanceof IErlFunction) {
            final IErlFunction function = (IErlFunction) element;
            final String withoutExtension = SystemConfiguration
                    .withoutExtension(function.getModuleName());
            return new FunctionPattern(withoutExtension,
                    function.getFunctionName(), function.getArity(), limitTo,
                    true, ErlangEngine.getInstance().getModelUtilService()
                            .getModule(function), !function.isExported());
        } else if (element instanceof IErlMacroDef) {
            final IErlMacroDef m = (IErlMacroDef) element;
            final String unquoted = StringUtils.unquote(m.getDefinedName());
            return new MacroPattern(unquoted, limitTo);
        } else if (element instanceof IErlRecordDef) {
            final IErlRecordDef r = (IErlRecordDef) element;
            final String unquoted = StringUtils.unquote(r.getDefinedName());
            return new RecordPattern(unquoted, limitTo);
        } else if (element instanceof IErlFunctionClause) {
            final IErlFunctionClause clause = (IErlFunctionClause) element;
            return getSearchPatternFromErlElementAndLimitTo(
                    (IErlElement) clause.getParent(), limitTo);
        } else if (element instanceof IErlAttribute) {
            final IErlAttribute a = (IErlAttribute) element;
            if (a.getName().startsWith("include")) {
                final String s = Util.stringValue(a.getValue());
                return new IncludePattern(s, limitTo);
            }
        }
        return null;
    }

}
