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

import static org.junit.Assert.assertTrue;

import org.junit.Test;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;

public class RpcTest extends AbstractErlangTest {

	@Test
	public void testRpc() {
		OtpErlangAtom a = new OtpErlangAtom("1");
		OtpErlangLong b = new OtpErlangLong(2);
		OtpErlangLong c = new OtpErlangLong(3);
		OtpErlangList l1 = new OtpErlangList(a, b, c);

		OtpErlangList l2 = new OtpErlangList(c, b, a);
		OtpErlangObject rr = runErlangTest("lists", "reverse", l1);
		assertTrue(rr.equals(l2));
	}

	@Test
	public void testStringResult() {
		OtpErlangString l1 = new OtpErlangString("abc");

		OtpErlangString l2 = new OtpErlangString("cba");
		OtpErlangObject rr = runErlangTest("lists", "reverse", l1);

		assertTrue(rr.equals(l2));
	}

}
