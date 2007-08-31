/*******************************************************************************
 * Copyright (c) 2006 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.testing.erlang;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;

public class ErlangTest extends AbstractErlangTest {

	public void testRpc() {
		OtpErlangAtom a = new OtpErlangAtom("1");
		OtpErlangLong b = new OtpErlangLong(2);
		OtpErlangLong c = new OtpErlangLong(3);
		OtpErlangList l1 = new OtpErlangList(a, b, c);

		OtpErlangList l2 = new OtpErlangList(c, b, a);
		OtpErlangObject rr = runErlangTest("lists", "reverse", l1);
		assertTrue(rr.equals(l2));
	}

	public void testStringResult() {
		OtpErlangLong a = new OtpErlangLong(65);
		OtpErlangLong b = new OtpErlangLong(66);
		OtpErlangLong c = new OtpErlangLong(67);
		OtpErlangList l1 = new OtpErlangList(a, b, c);

		OtpErlangList l2 = new OtpErlangList(c, b, a);
		OtpErlangObject rr = runErlangTest("lists", "reverse", l1);

		assertFalse(rr.equals(l2));
	}

	public void testOk() {
		erlangTest("erlang", "now", NO_ARGS);
	}

	public void testFail() {
		erlangTest("erlang", "hrlp", NO_ARGS);
	}

}
