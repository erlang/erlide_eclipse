/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.jinterface;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangDouble;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlUtils {

	/**
	 * @param term
	 * @param pattern
	 * @return Map of matched variables. Null means no match, an empty list
	 *         means no variables were bound.
	 */
	public static HashMap<String, OtpErlangObject> match(
			OtpErlangObject pattern, OtpErlangObject term,
			HashMap<String, OtpErlangObject> bindings) {
		// System.out.println("matching \n " + pattern.toString() + "\n "
		// + term.toString() + "\n B=" + bindings);

		if (pattern instanceof OtpVariable) {
			return matchVariable(pattern, term, bindings);
		}
		final String pcn = pattern.getClass().getName();
		if (!pcn.equals(term.getClass().getName())) {
			return null;
		}

		if (pcn.endsWith("OtpErlangAtom")) {
			return matchAtom((OtpErlangAtom) pattern, (OtpErlangAtom) term,
					bindings);
		} else if (pcn.endsWith("OtpErlangLong")) {
			return matchLong((OtpErlangLong) pattern, (OtpErlangLong) term,
					bindings);
		} else if (pcn.endsWith("OtpErlangList")) {
			return matchList((OtpErlangList) pattern, (OtpErlangList) term,
					bindings);
		} else if (pcn.endsWith("OtpErlangTuple")) {
			return matchTuple((OtpErlangTuple) pattern, (OtpErlangTuple) term,
					bindings);
		}
		return null;
	}

	/**
	 * @param pattern
	 * @param term
	 * @param bindings
	 * @return
	 */
	private static HashMap<String, OtpErlangObject> matchVariable(
			OtpErlangObject pattern, OtpErlangObject term,
			HashMap<String, OtpErlangObject> bindings) {
		// System.out.println("match variable");

		@SuppressWarnings("unchecked")
		final HashMap<String, OtpErlangObject> result = (HashMap<String, OtpErlangObject>) bindings
				.clone();

		final OtpVariable v = (OtpVariable) pattern;
		final OtpErlangObject old = bindings.get(v.getName());
		// no previous binding
		if (old == null) {
			result.put(v.getName(), term);
			return result;
		} else {
			return old.equals(term) ? result : null;
		}
	}

	/**
	 * @param pattern
	 * @param term
	 * @return
	 */
	private static HashMap<String, OtpErlangObject> matchAtom(
			OtpErlangAtom pattern, OtpErlangAtom term,
			HashMap<String, OtpErlangObject> bindings) {
		// System.out.println("match atom");

		if (pattern.atomValue().equals(term.atomValue())) {
			return bindings;
		} else {
			return null;
		}
	}

	/**
	 * @param pattern
	 * @param term
	 * @param bindings
	 * @return
	 */
	private static HashMap<String, OtpErlangObject> matchLong(
			OtpErlangLong pattern, OtpErlangLong term,
			HashMap<String, OtpErlangObject> bindings) {
		// System.out.println("match long");

		try {
			if (pattern.intValue() == term.intValue()) {
				return bindings;
			} else {
				return null;
			}
		} catch (final OtpErlangRangeException e) {
			e.printStackTrace();
			return null;
		}
	}

	/**
	 * @param pattern
	 * @param term
	 * @param bindings
	 * @return
	 */
	private static HashMap<String, OtpErlangObject> matchList(
			OtpErlangList pattern, OtpErlangList term,
			HashMap<String, OtpErlangObject> bindings) {
		// System.out.println("match list");
		if (pattern.arity() != term.arity()) {
			return null;
		}

		@SuppressWarnings("unchecked")
		HashMap<String, OtpErlangObject> result = (HashMap<String, OtpErlangObject>) bindings
				.clone();
		for (int i = 0; i < pattern.arity(); i++) {
			result = match(pattern.elementAt(i), term.elementAt(i), result);
			if (result == null) {
				return null;
			}
		}

		return result;
	}

	/**
	 * @param pattern
	 * @param term
	 * @param bindings
	 * @return
	 */
	private static HashMap<String, OtpErlangObject> matchTuple(
			OtpErlangTuple pattern, OtpErlangTuple term,
			HashMap<String, OtpErlangObject> bindings) {
		// System.out.println("match tuple");
		if (pattern.arity() != term.arity()) {
			return null;
		}

		@SuppressWarnings("unchecked")
		HashMap<String, OtpErlangObject> result = (HashMap<String, OtpErlangObject>) bindings
				.clone();
		for (int i = 0; i < pattern.arity(); i++) {
			result = match(pattern.elementAt(i), term.elementAt(i), result);
			if (result == null) {
				return null;
			}
		}

		return result;
	}

	@SuppressWarnings("unchecked")
	protected HashMap mergeBindings(HashMap m1, HashMap m2) {
		final HashMap res = (HashMap) m1.clone();
		return res;
	}

	public static OtpErlangObject java2erlang(Object o) {
		if (o instanceof OtpErlangObject) {
			return (OtpErlangObject) o;
		} else if (o instanceof String) {
			return new OtpErlangString((String) o);
		} else if (o instanceof Integer) {
			return new OtpErlangInt((Integer) o);
		} else if (o instanceof Long) {
			return new OtpErlangLong((Long) o);
		} else if (o instanceof Double) {
			return new OtpErlangDouble((Double) o);
		} else if (o instanceof Boolean) {
			return new OtpErlangAtom((Boolean) o ? "true" : "false");
		} else if (o instanceof List<?>) {
			Object[] v = ((List<?>) o).toArray(new Object[] {});
			OtpErlangObject[] vv = new OtpErlangObject[v.length];
			for (int i = 0; i < v.length; i++) {
				vv[i] = java2erlang(v[i]);
			}
			return new OtpErlangList(vv);
		} else if (o instanceof Object[]) {
			Object[] v = ((Object[]) o);
			OtpErlangObject[] vv = new OtpErlangObject[v.length];
			for (int i = 0; i < v.length; i++) {
				vv[i] = java2erlang(v[i]);
			}
			return new OtpErlangTuple(vv);
		} else {
			return new OtpErlangLong(registerTarget(o));
		}
	}

	public static Object erlang2java(OtpErlangObject o) {
		try {
			if (o instanceof OtpErlangString) {
				return ((OtpErlangString) o).stringValue();
			} else if (o instanceof OtpErlangInt) {
				return ((OtpErlangInt) o).intValue();
			} else if (o instanceof OtpErlangLong) {
				return ((OtpErlangLong) o).longValue();
			} else if (o instanceof OtpErlangDouble) {
				return ((OtpErlangDouble) o).doubleValue();
			} else if (o instanceof OtpErlangAtom) {
				String a = ((OtpErlangAtom) o).atomValue();
				if ("true".equals(a)) {
					return true;
				} else if ("false".equals(a)) {
					return false;
				} else {
					return 0;
				}
			} else if (o instanceof OtpErlangList) {
				OtpErlangObject[] v = ((OtpErlangList) o).elements();
				Object[] vv = new Object[v.length];
				for (int i = 0; i < v.length; i++) {
					vv[i] = erlang2java(v[i]);
				}
				return Arrays.asList(vv);
			} else if (o instanceof OtpErlangTuple) {
				OtpErlangObject[] v = ((OtpErlangTuple) o).elements();
				Object[] vv = new Object[v.length];
				for (int i = 0; i < v.length; i++) {
					vv[i] = erlang2java(v[i]);
				}
				return vv;
			} else {
				return o;
			}
		} catch (Exception e) {
			return o;
		}
	}

	private static Map<Long, Object> objects = new HashMap<Long, Object>();;
	private static long refid = 1;

	public static Long registerTarget(Object o) {
		long ref = refid++;
		objects.put(ref, o);
		System.out.println("    >>" + ref + " " + o);
		return ref;
	}

	public static Object getTarget(Long ref) {
		System.out.println("    <<" + ref + " " + objects.get(ref));
		return objects.get(ref);
	}

	public static Object getTarget(OtpErlangLong ref) {
		long r;
		try {
			r = ref.longValue();
		} catch (OtpErlangRangeException e) {
			return null;
		}
		System.out.println("    <<" + ref + " " + objects.get(r));
		return objects.get(r);
	}

	public static void unregisterTarget(Long ref) {
		objects.remove(ref);
	}

	public static Class erlang2javaType(Object o) {
		if (o instanceof OtpErlangObject) {
			return OtpErlangObject.class;
		} else if (o instanceof OtpErlangString) {
			return String.class;
		} else if (o instanceof OtpErlangInt) {
			return Integer.class;
		} else if (o instanceof OtpErlangLong) {
			return Long.class;
		} else if (o instanceof OtpErlangDouble) {
			return Double.class;
		} else if (o instanceof OtpErlangAtom) {
			return Boolean.class;
		} else if (o instanceof OtpErlangList) {
			return List.class;
		} else if (o instanceof OtpErlangTuple) {
			return Object[].class;
		} else {
			return null;// ??new OtpErlangLong(registerTarget(o));
		}
	}
}