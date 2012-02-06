package org.erlide.core.services.search;

import java.util.EnumSet;

import org.erlide.core.model.erlang.IErlAttribute;
import org.erlide.core.model.erlang.IErlFunction;
import org.erlide.core.model.erlang.IErlFunctionClause;
import org.erlide.core.model.erlang.IErlMacroDef;
import org.erlide.core.model.erlang.IErlRecordDef;
import org.erlide.core.model.root.IErlElement;
import org.erlide.utils.StringUtils;
import org.erlide.utils.SystemUtils;
import org.erlide.utils.Util;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public abstract class ErlangSearchPattern {

    protected static final OtpErlangAtom EXTERNAL_CALL_ATOM = new OtpErlangAtom(
            "external_call");
    protected static final OtpErlangAtom FUNCTION_CALL_ATOM = new OtpErlangAtom(
            "function_call");
    protected static final OtpErlangAtom FUNCTION_DEF_ATOM = new OtpErlangAtom(
            "function_def");
    protected static final OtpErlangAtom FUNCTION_DEF_MOD_ATOM = new OtpErlangAtom(
            "function_def_mod");
    static final OtpErlangAtom INCLUDE_REF_ATOM = new OtpErlangAtom(
            "include_ref");
    protected static final OtpErlangAtom MACRO_DEF_ATOM = new OtpErlangAtom(
            "macro_def");
    protected static final OtpErlangAtom MACRO_REF_ATOM = new OtpErlangAtom(
            "macro_ref");
    protected static final OtpErlangAtom RECORD_DEF_ATOM = new OtpErlangAtom(
            "record_def");
    protected static final OtpErlangAtom RECORD_REF_ATOM = new OtpErlangAtom(
            "record_ref");
    protected static final OtpErlangAtom TYPE_DEF_ATOM = new OtpErlangAtom(
            "type_def");
    protected static final OtpErlangAtom TYPE_REF_ATOM = new OtpErlangAtom(
            "type_ref");
    protected static final OtpErlangAtom VARIABLE_PATTERN_ATOM = new OtpErlangAtom(
            "var_pattern");
    protected static final OtpErlangAtom VARIABLE_DEF_ATOM = new OtpErlangAtom(
            "var_def");
    protected static final OtpErlangAtom VARIABLE_REF_ATOM = new OtpErlangAtom(
            "var_ref");
    protected static final OtpErlangAtom RECORD_FIELD_DEF_ATOM = new OtpErlangAtom(
            "record_field_def");
    protected static final OtpErlangAtom RECORD_FIELD_REF_ATOM = new OtpErlangAtom(
            "record_field_ref");

    // search for
    public enum SearchFor {
        FUNCTION, MACRO, RECORD, INCLUDE, TYPE, VARIABLE, RECORD_FIELD;
    }

    public EnumSet<SearchFor> allSearchFor = EnumSet.allOf(SearchFor.class);

    // limit to
    public enum LimitTo {
        REFERENCES, DEFINITIONS, ALL_OCCURRENCES
    }

    public EnumSet<LimitTo> allLimitTo = EnumSet.allOf(LimitTo.class);

    protected final LimitTo limitTo;

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

    protected ErlangSearchPattern(final LimitTo limitTo) {
        this.limitTo = limitTo;
    }

    abstract public OtpErlangObject getSearchObject();

    protected OtpErlangObject makeSSPatternObject(final OtpErlangAtom defAtom,
            final OtpErlangAtom refAtom, final String s1, final String s2) {
        OtpErlangObject defs = null, refs = null;
        if (limitTo != LimitTo.REFERENCES) {
            defs = make3Tuple(defAtom, s1, s2);
        }
        if (limitTo != LimitTo.DEFINITIONS) {
            refs = make3Tuple(refAtom, s1, s2);
        }
        return returnRefsAndOrDefs(refs, defs);
    }

    protected static OtpErlangObject makeIncludePatternObject(final String s) {
        return new OtpErlangTuple(new OtpErlangObject[] { INCLUDE_REF_ATOM,
                new OtpErlangString(s) });
    }

    protected OtpErlangObject makeMFAPatternObject(final OtpErlangAtom defAtom,
            final OtpErlangAtom refAtom, final String m, final String f,
            final int a, final boolean matchAnyFunctionDefinition) {
        OtpErlangObject refs = null, defs = null;
        if (limitTo != LimitTo.REFERENCES) {
            if (matchAnyFunctionDefinition) {
                defs = make3Tuple(defAtom, f, a);
            } else {
                defs = make4Tuple(defAtom, m, f, a);
            }
        }
        if (limitTo != LimitTo.DEFINITIONS) {
            refs = make4Tuple(refAtom, m, f, a);
        }
        return returnRefsAndOrDefs(refs, defs);
    }

    protected OtpErlangObject makeFAPatternObject(final OtpErlangAtom defAtom,
            final OtpErlangAtom refAtom, final String f, final int a) {
        OtpErlangObject refs = null, defs = null;
        if (limitTo != LimitTo.REFERENCES) {
            defs = make3Tuple(defAtom, f, a);
        }
        if (limitTo != LimitTo.DEFINITIONS) {
            refs = make3Tuple(refAtom, f, a);
        }
        return returnRefsAndOrDefs(refs, defs);
    }

    protected OtpErlangObject makeSPatternObject(final OtpErlangAtom defAtom,
            final OtpErlangAtom refAtom, final String s) {
        return makeSPatternObject(defAtom, refAtom, s, s);
    }

    protected OtpErlangObject makeSPatternObject(final OtpErlangAtom defAtom,
            final OtpErlangAtom refAtom, final String defS, final String refS) {
        OtpErlangObject defs = null, refs = null;
        if (limitTo != LimitTo.REFERENCES) {
            defs = make2Tuple(defAtom, defS);
        }
        if (limitTo != LimitTo.DEFINITIONS) {
            refs = make2Tuple(refAtom, refS);
        }
        return returnRefsAndOrDefs(refs, defs);
    }

    private OtpErlangObject returnRefsAndOrDefs(final OtpErlangObject refs,
            final OtpErlangObject defs) {
        if (limitTo == LimitTo.ALL_OCCURRENCES) {
            return new OtpErlangList(new OtpErlangObject[] { refs, defs });
        } else if (limitTo == LimitTo.REFERENCES) {
            return refs;
        } else {
            return defs;
        }
    }

    private static OtpErlangTuple make2Tuple(final OtpErlangAtom atom,
            final String s) {
        return new OtpErlangTuple(new OtpErlangObject[] { atom,
                new OtpErlangAtom(s) });
    }

    private static final OtpErlangAtom UNDEFINED = new OtpErlangAtom(
            "undefined");

    private static OtpErlangObject make3Tuple(final OtpErlangAtom atom,
            final String s, final int a) {
        if (a >= 0) {
            return new OtpErlangTuple(new OtpErlangObject[] { atom,
                    new OtpErlangAtom(s), new OtpErlangLong(a) });
        }
        return new OtpErlangTuple(new OtpErlangObject[] { atom,
                new OtpErlangAtom(s), UNDEFINED });

    }

    private static OtpErlangObject make3Tuple(final OtpErlangAtom atom,
            final String a1, final String a2) {
        return new OtpErlangTuple(new OtpErlangObject[] { atom,
                new OtpErlangAtom(a1), new OtpErlangAtom(a2) });
    }

    private static OtpErlangObject make4Tuple(final OtpErlangAtom atom,
            final String s1, final String s2, final int a) {
        if (a >= 0) {
            return new OtpErlangTuple(new OtpErlangObject[] { atom,
                    new OtpErlangAtom(s1), new OtpErlangAtom(s2),
                    new OtpErlangLong(a) });
        }
        return new OtpErlangTuple(new OtpErlangObject[] { atom,
                new OtpErlangAtom(s1), new OtpErlangAtom(s2), UNDEFINED });
    }

    public LimitTo getLimitTo() {
        return limitTo;
    }

    public abstract String patternString();

    public abstract SearchFor getSearchFor();

    public abstract String labelString();

    public static ErlangSearchPattern getSearchPatternFromErlElementAndLimitTo(
            final IErlElement element, final LimitTo limitTo) {
        if (element instanceof IErlFunction) {
            final IErlFunction function = (IErlFunction) element;
            final String withoutExtension = SystemUtils
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
