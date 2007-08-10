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

import java.util.HashMap;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
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
}