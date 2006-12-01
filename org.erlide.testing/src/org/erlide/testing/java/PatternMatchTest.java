/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.testing.java;

import java.util.HashMap;

import junit.framework.TestCase;

import org.erlide.jinterface.ErlUtils;
import org.erlide.jinterface.OtpVariable;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 *
 *
 * @author Vlad Dumitrescu
 */
public class PatternMatchTest extends TestCase {

	public void testMatch() {
		OtpErlangObject p = new OtpErlangList(
				new OtpErlangObject[] { new OtpVariable("V"),
						new OtpErlangTuple(new OtpVariable("W")) });
		OtpErlangObject t1 = new OtpErlangList(new OtpErlangObject[] {
				new OtpErlangAtom("a"),
				new OtpErlangTuple(new OtpErlangAtom("b")) });
		OtpErlangObject t2 = new OtpErlangList(new OtpErlangObject[] {
				new OtpErlangAtom("a"),
				new OtpErlangTuple(new OtpErlangAtom("a")) });
		HashMap<String, OtpErlangObject> b = new HashMap<String, OtpErlangObject>();
		HashMap<String, OtpErlangObject> r;
		@SuppressWarnings("unused")
		HashMap<String, OtpErlangObject> r2;

		r = match(p, t1, b);

		r2 = match(p, t1, r);

		r2 = match(p, t2, r);

		r2 = match(p, t2, b);
	}

	/**
	 * @param p
	 * @param t1
	 * @param b
	 * @return
	 */
	private HashMap<String, OtpErlangObject> match(OtpErlangObject p, OtpErlangObject t1, HashMap<String, OtpErlangObject> b) {
		// System.out.println("-----------------------");
		HashMap<String, OtpErlangObject> r;
		// System.out.println("matching \n " + p.toString() + "\n " +
		// t1.toString() + "\n
		// B=" + b);
		r = ErlUtils.match(p, t1, b);
		// System.out.println("R=" + r);
		return r;
	}

}
