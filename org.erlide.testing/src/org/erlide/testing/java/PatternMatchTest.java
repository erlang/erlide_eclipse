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
import org.erlide.jinterface.TermParser;
import org.junit.Test;

import com.ericsson.otp.erlang.OtpErlangObject;

/**
 * 
 * 
 * @author Vlad Dumitrescu
 */
public class PatternMatchTest {

	@Test
	public void testFormatParser() throws Exception {
		OtpErlangObject res = TermParser.parse("{hej, ho}");
	}

	@Test
	public void testMatch() throws Exception {
		OtpErlangObject p = TermParser.parse("[W, V]");
		OtpErlangObject t1 = TermParser.parse("[a, b]");
		OtpErlangObject t2 = TermParser.parse("[a, a]");
		Bindings b = new Bindings();
		Bindings r;
		Bindings r2;

		r = match(p, t1, b);

		r2 = match(p, t1, r);

		r2 = match(p, t2, r);

		r2 = match(p, t2, b);
	}

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

	private Bindings match(String p, OtpErlangObject t1, Bindings b)
			throws Exception {
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
