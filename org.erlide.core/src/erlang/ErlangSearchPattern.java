package erlang;

import org.erlide.core.search.FunctionPattern;
import org.erlide.core.search.IncludePattern;
import org.erlide.core.search.MacroPattern;
import org.erlide.core.search.RecordPattern;
import org.erlide.core.search.TypeRefPattern;

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

	// search for
	public static final int SEARCHFOR_FUNCTION = 1;
	public static final int SEARCHFOR_MACRO = 2;
	public static final int SEARCHFOR_RECORD = 3;
	public static final int SEARCHFOR_INCLUDE = 4;
	public static final int SEARCHFOR_TYPE = 5;
	// public static final int SEARCHFOR_VARIABLE = 6;

	// limit to
	public static int REFERENCES = 0;
	public static int DEFINITIONS = 1;
	public static int ALL_OCCURRENCES = 2;

	// private final int searchFor;
	protected final int limitTo;

	public static ErlangSearchPattern getSearchPattern(final int searchFor,
			final String module, final String name, final int arity,
			final int limitTo) {
		switch (searchFor) {
		case SEARCHFOR_FUNCTION:
			return new FunctionPattern(module, name, arity, limitTo);
		case SEARCHFOR_INCLUDE:
			return new IncludePattern(name, limitTo);
		case SEARCHFOR_MACRO:
			return new MacroPattern(name, limitTo);
		case SEARCHFOR_RECORD:
			return new RecordPattern(name, limitTo);
		case SEARCHFOR_TYPE:
			return new TypeRefPattern(module, name, limitTo);
		}
		return null;
	}

	protected ErlangSearchPattern(final int limitTo) {
		this.limitTo = limitTo;
	}

	abstract public OtpErlangObject getSearchObject();

	protected OtpErlangObject makeSSPatternObject(final OtpErlangAtom defAtom,
			final OtpErlangAtom refAtom, final String s1, final String s2) {
		OtpErlangObject defs = null, refs = null;
		if (limitTo != REFERENCES) {
			defs = make3Tuple(defAtom, s1, s2);
		}
		if (limitTo != DEFINITIONS) {
			refs = make3Tuple(refAtom, s1, s2);
		}
		return returnRefsAndOrDefs(refs, defs);
	}

	protected OtpErlangObject makeIncludePatternObject(final String s) {
		return new OtpErlangTuple(new OtpErlangObject[] { INCLUDE_REF_ATOM,
				new OtpErlangString(s) });
	}

	protected OtpErlangObject makeMFAPatternObject(final OtpErlangAtom defAtom,
			final OtpErlangAtom refAtom, final String m, final String f,
			final int a) {
		OtpErlangObject refs = null, defs = null;
		if (limitTo != REFERENCES) {
			defs = make3Tuple(defAtom, f, a);
		}
		if (limitTo != DEFINITIONS) {
			refs = make4Tuple(refAtom, m, f, a);
		}
		return returnRefsAndOrDefs(refs, defs);
	}

	protected OtpErlangObject makeFAPatternObject(final OtpErlangAtom defAtom,
			final OtpErlangAtom refAtom, final String f, final int a) {
		OtpErlangObject refs = null, defs = null;
		if (limitTo != REFERENCES) {
			defs = make3Tuple(defAtom, f, a);
		}
		if (limitTo != DEFINITIONS) {
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
		if (limitTo != REFERENCES) {
			defs = make2Tuple(defAtom, defS);
		}
		if (limitTo != DEFINITIONS) {
			refs = make2Tuple(refAtom, refS);
		}
		return returnRefsAndOrDefs(refs, defs);
	}

	private OtpErlangObject returnRefsAndOrDefs(final OtpErlangObject refs,
			final OtpErlangObject defs) {
		if (limitTo == ALL_OCCURRENCES) {
			return new OtpErlangList(new OtpErlangObject[] { refs, defs });
		} else if (limitTo == REFERENCES) {
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

	private static OtpErlangObject make3Tuple(final OtpErlangAtom atom,
			final String s, final int a) {
		return new OtpErlangTuple(new OtpErlangObject[] { atom,
				new OtpErlangAtom(s), new OtpErlangLong(a) });
	}

	private OtpErlangObject make3Tuple(final OtpErlangAtom atom,
			final String a1, final String a2) {
		return new OtpErlangTuple(new OtpErlangObject[] { atom,
				new OtpErlangAtom(a1), new OtpErlangAtom(a2) });
	}

	private OtpErlangObject make4Tuple(final OtpErlangAtom atom,
			final String s1, final String s2, final int a) {
		return new OtpErlangTuple(new OtpErlangObject[] { atom,
				new OtpErlangAtom(s1), new OtpErlangAtom(s2),
				new OtpErlangLong(a) });
	}

	public int getLimitTo() {
		return limitTo;
	}

	public abstract String patternString();

	public abstract int getSearchFor();

	public abstract String labelString();

}
