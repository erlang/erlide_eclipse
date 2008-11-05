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
import java.util.List;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlUtils {

	/*
	 * for example, format("{hello, ~s, [~a, _]}", "myname", "mykey")
	 */
	public static OtpErlangObject format(String fmt, Object... args) {
		return null;
	}

	public static Bindings match(OtpErlangObject pattern, OtpErlangObject term) {
		return match(pattern, term, new Bindings());
	}

	public static Bindings match(OtpErlangObject pattern, OtpErlangObject term,
			Bindings bindings) {
		// ErlLogger.debug("matching \n " + pattern.toString() + "\n "
		// + term.toString() + "\n B=" + bindings);

		if (pattern instanceof OtpVariable) {
			return matchVariable((OtpVariable) pattern, term, bindings);
		}
		final Class<?> pc = pattern.getClass();
		if (!pc.equals(term.getClass())) {
			return null;
		}

		if (pc == OtpErlangAtom.class) {
			return matchAtom((OtpErlangAtom) pattern, (OtpErlangAtom) term,
					bindings);
		} else if (pc == OtpErlangLong.class) {
			return matchLong((OtpErlangLong) pattern, (OtpErlangLong) term,
					bindings);
		} else if (pc == OtpErlangList.class) {
			return matchList((OtpErlangList) pattern, (OtpErlangList) term,
					bindings);
		} else if (pc == OtpErlangTuple.class) {
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
	private static Bindings matchVariable(OtpVariable var,
			OtpErlangObject term, Bindings bindings) {
		// System.out.println("match variable");

		final Bindings result = new Bindings(bindings);

		final OtpErlangObject old = bindings.get(var.getName());
		// no previous binding
		if (old == null) {
			result.put(var.getName(), term);
			return result;
		}
		return old.equals(term) ? result : null;
	}

	/**
	 * @param pattern
	 * @param term
	 * @return
	 */
	private static Bindings matchAtom(OtpErlangAtom pattern,
			OtpErlangAtom term, Bindings bindings) {
		// System.out.println("match atom");

		if (pattern.atomValue().equals(term.atomValue())) {
			return bindings;
		}
		return null;
	}

	/**
	 * @param pattern
	 * @param term
	 * @param bindings
	 * @return
	 */
	private static Bindings matchLong(OtpErlangLong pattern,
			OtpErlangLong term, Bindings bindings) {
		// System.out.println("match long");

		try {
			if (pattern.intValue() == term.intValue()) {
				return bindings;
			}
			return null;
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
	private static Bindings matchList(OtpErlangList pattern,
			OtpErlangList term, Bindings bindings) {
		// System.out.println("match list");
		if (pattern.arity() != term.arity()) {
			return null;
		}

		Bindings result = new Bindings(bindings);
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
	private static Bindings matchTuple(OtpErlangTuple pattern,
			OtpErlangTuple term, Bindings bindings) {
		// System.out.println("match tuple");
		if (pattern.arity() != term.arity()) {
			return null;
		}

		Bindings result = new Bindings(bindings);
		for (int i = 0; i < pattern.arity(); i++) {
			result = match(pattern.elementAt(i), term.elementAt(i), result);
			if (result == null) {
				return null;
			}
		}

		return result;
	}

	public OtpErlangObject format(String fmt, OtpErlangObject... args) {
		OtpErlangObject result;
		try {
			result = TermParser.parse(fmt);
			result = fill(result, Arrays.asList(args));
		} catch (Exception e) {
			result = null;
		}
		return result;
	}

	private OtpErlangObject fill(OtpErlangObject result,
			List<OtpErlangObject> asList) {
		// TODO implement
		if (result instanceof OtpErlangList) {
		} else if (result instanceof OtpErlangTuple) {
		}
		return result;
	}
}