package org.erlide.core.services.search;

import org.erlide.core.model.erlang.IErlAttribute;
import org.erlide.core.model.erlang.IErlFunction;
import org.erlide.core.model.erlang.IErlFunctionClause;
import org.erlide.core.model.erlang.IErlMacroDef;
import org.erlide.core.model.erlang.IErlRecordDef;
import org.erlide.core.model.root.IErlElement;
import org.erlide.utils.StringUtils;
import org.erlide.utils.SystemConfiguration;
import org.erlide.utils.Util;

public class SearchPatternFactory {

    public static ErlangSearchPattern getSearchPattern(
            final SearchFor searchFor, final String module, final String name,
            final int arity, final LimitTo limitTo) {
        switch (searchFor) {
        case FUNCTION:
            return new FunctionPattern(module, name, arity, limitTo, true);
        case INCLUDE:
            return new IncludePattern(name, limitTo);
        case MACRO:
            return new MacroPattern(name, limitTo);
        case RECORD:
            return new RecordPattern(name, limitTo);
        case TYPE:
            return new TypeRefPattern(module, name, limitTo);
        case RECORD_FIELD:
            return new RecordFieldPattern(module, name, limitTo);
        case VARIABLE:
            return null; // FIXME
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
                    true);
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
