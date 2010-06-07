package org.erlide.core.search;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlangSearchPattern {

	static final OtpErlangAtom EXTERNAL_CALL_ATOM = new OtpErlangAtom(
			"external_call");
	static final OtpErlangAtom FUNCTION_CALL_ATOM = new OtpErlangAtom(
			"function_call");
	static final OtpErlangAtom FUNCTION_DEF_ATOM = new OtpErlangAtom(
			"function_def");
	static final OtpErlangAtom INCLUDE_REF_ATOM = new OtpErlangAtom(
			"include_ref");
	static final OtpErlangAtom MACRO_DEF_ATOM = new OtpErlangAtom("macro_def");
	static final OtpErlangAtom MACRO_REF_ATOM = new OtpErlangAtom("macro_ref");
	static final OtpErlangAtom RECORD_DEF_ATOM = new OtpErlangAtom("record_def");
	static final OtpErlangAtom RECORD_REF_ATOM = new OtpErlangAtom("record_ref");
	static final OtpErlangAtom TYPE_DEF_ATOM = new OtpErlangAtom("type_def");
	static final OtpErlangAtom TYPE_REF_ATOM = new OtpErlangAtom("type_ref");

	// search for
	public static final int SEARCHFOR_FUNCTION = 1;
	public static final int SEARCHFOR_MACRO = 2;
	public static final int SEARCHFOR_RECORD = 3;
	public static final int SEARCHFOR_INCLUDE = 4;
	public static final int SEARCHFOR_TYPE = 5;

	// limit to
	public static int REFERENCES = 0;
	public static int DEFINITIONS = 1;
	public static int ALL_OCCURRENCES = 2;

	private final int searchFor;
	private final String module;
	private final String name;
	private final int arity;
	private final int limitTo;

	public ErlangSearchPattern(final int searchFor, final String module,
			final String name, final int arity, final int limitTo) {
		super();
		this.searchFor = searchFor;
		this.module = module;
		this.name = name;
		this.arity = arity;
		this.limitTo = limitTo;
	}

	public OtpErlangObject getSearchObject() {
		switch (searchFor) {
		case SEARCHFOR_FUNCTION:
			if (module == null || module.length() == 0) {
				return makeFAPatternObject(FUNCTION_DEF_ATOM,
						FUNCTION_CALL_ATOM, name, arity);
			} else {
				return makeMFAPatternObject(FUNCTION_DEF_ATOM,
						EXTERNAL_CALL_ATOM, module, name, arity);
			}
		case SEARCHFOR_MACRO:
			return makeSPatternObject(MACRO_DEF_ATOM, MACRO_REF_ATOM, name, "?"
					+ name);
		case SEARCHFOR_RECORD:
			return makeSPatternObject(RECORD_DEF_ATOM, RECORD_REF_ATOM, name);
		case SEARCHFOR_INCLUDE:
			return makeIncludePatternObject(name);
		case SEARCHFOR_TYPE:
			return makeSSPatternObject(TYPE_DEF_ATOM, TYPE_REF_ATOM,
					module == null ? "_" : module, name);
		}
		return null;
	}

	private OtpErlangObject makeSSPatternObject(final OtpErlangAtom defAtom,
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

	private OtpErlangObject makeIncludePatternObject(final String s) {
		return new OtpErlangTuple(new OtpErlangObject[] { INCLUDE_REF_ATOM,
				new OtpErlangString(s) });
	}

	private OtpErlangObject makeMFAPatternObject(final OtpErlangAtom defAtom,
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

	private OtpErlangObject makeFAPatternObject(final OtpErlangAtom defAtom,
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

	private OtpErlangObject makeSPatternObject(final OtpErlangAtom defAtom,
			final OtpErlangAtom refAtom, final String s) {
		return makeSPatternObject(defAtom, refAtom, s, s);
	}

	private OtpErlangObject makeSPatternObject(final OtpErlangAtom defAtom,
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

	public String patternString() {
		switch (searchFor) {
		case SEARCHFOR_FUNCTION:
			String s = name + "/" + arity;
			if (module == null || limitTo != REFERENCES) {
				return s;
			} else {
				return module + ":" + s;
			}
		case SEARCHFOR_MACRO:
			return name;
		case SEARCHFOR_RECORD:
			return name;
		case SEARCHFOR_INCLUDE:
			return name;
		case SEARCHFOR_TYPE:
			if (module != null && module.length() != 0) {
				return module + ":" + name;
			}
			return name;
		}
		return null;
	}

	@Override
	public String toString() {
		return "ErlangSearchPattern [arity=" + arity + ", limitTo=" + limitTo
				+ ", module=" + module + ", name=" + name + ", searchFor="
				+ searchFor + "]";
	}

	public int getLimitTo() {
		return limitTo;
	}

	public int getSearchFor() {
		return searchFor;
	}

	public String labelString() {
		String s = name;
		switch (searchFor) {
		case SEARCHFOR_FUNCTION:
			s = name + "/" + arity;
			if (module == null || limitTo != REFERENCES) {
				return s;
			} else {
				return module + ":" + s;
			}
		case SEARCHFOR_MACRO:
			if (!s.startsWith("?")) {
				s = "?" + s;
			}
			return s;
		case SEARCHFOR_RECORD:
			if (!s.startsWith("#")) {
				s = "#" + s;
			}
			if (!s.endsWith("{}")) {
				s = s + "{}";
			}
			return s;
		case SEARCHFOR_INCLUDE:
			if (!s.startsWith("-include")) {
				s = "-include(" + s;
			}
			if (!s.endsWith(").")) {
				s = s + ").";
			}
			return s;
		case SEARCHFOR_TYPE:
			if (module != null && module.length() != 0) {
				return module + ":" + name;
			}
			return name;
		}
		return null;

	}

}
