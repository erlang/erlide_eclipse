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

import org.erlide.jinterface.Bindings;
import org.erlide.jinterface.ErlUtils;
import org.erlide.jinterface.OtpVariable;
import org.erlide.jinterface.TermParser;
import org.junit.Test;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * 
 * 
 * @author Vlad Dumitrescu
 */
public class PatternMatchTest {

	@Test
	public void testFormatParser() throws Exception {
		OtpErlangObject res = new TermParser().parse("{hej,ho}");
	}

	@Test
	public void testMatch() {
		OtpErlangObject p = new OtpErlangList(new OtpVariable("V"),
				new OtpErlangTuple(new OtpVariable("W")));
		OtpErlangObject t1 = new OtpErlangList(new OtpErlangAtom("a"),
				new OtpErlangTuple(new OtpErlangAtom("b")));
		OtpErlangObject t2 = new OtpErlangList(new OtpErlangAtom("a"),
				new OtpErlangTuple(new OtpErlangAtom("a")));
		Bindings b = new Bindings();
		Bindings r;
		Bindings r2;

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
	private Bindings match(OtpErlangObject p, OtpErlangObject t1, Bindings b) {
		// ErlLogger.debug("-----------------------");
		Bindings r;
		// ErlLogger.debug("matching \n " + p.toString() + "\n " +
		// t1.toString() +" \n
		// B=" + b);
		r = ErlUtils.match(p, t1, b);
		// ErlLogger.debug("R=" + r);
		return r;
	}

}
