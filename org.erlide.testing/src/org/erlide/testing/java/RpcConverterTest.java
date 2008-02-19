/*******************************************************************************
 * Copyright (c) 2008 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution.
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.testing.java;

import static org.erlide.jinterface.rpc.RpcConverter.java2erlang;
import static org.junit.Assert.assertTrue;

import org.erlide.jinterface.rpc.RpcException;
import org.junit.Test;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;

public class RpcConverterTest {

	@Test
	public void cvtIntegerOk_1() throws RpcException {
		short value = 3;
		OtpErlangObject result = java2erlang(new Short(value), "i");
		OtpErlangObject expect = new OtpErlangLong(3);
		assertTrue(expect.equals(result));
	}

	@Test
	public void cvtIntegerOk_2() throws RpcException {
		int value = 3;
		OtpErlangObject result = java2erlang(new Integer(value), "x");
		OtpErlangObject expect = new OtpErlangLong(value);
		assertTrue(expect.equals(result));
	}

	@Test(expected = RpcException.class)
	public void cvtIntegerFail_1() throws RpcException {
		int value = 3;
		OtpErlangObject expect = new OtpErlangLong(value);
		OtpErlangObject result = java2erlang(new Integer(value), "s");
		assertTrue(expect.equals(result));
	}

	@Test
	public void cvtListOk_1() throws RpcException {
		OtpErlangObject result = java2erlang(new String[] { "a" }, "ls");
		OtpErlangObject expect = new OtpErlangList(
				new OtpErlangObject[] { new OtpErlangString("a") });
		assertTrue(expect.equals(result));
	}

	@Test(expected = RpcException.class)
	public void cvtListFail_1() throws RpcException {
		OtpErlangObject result = java2erlang(new String[] { "a" }, "li");
		OtpErlangObject expect = new OtpErlangList(
				new OtpErlangObject[] { new OtpErlangString("a") });
		assertTrue(expect.equals(result));
	}

	@Test
	public void cvtStringOk_2() throws RpcException {
		OtpErlangObject result = java2erlang("a", "a");
		OtpErlangObject expect = new OtpErlangAtom("a");
		assertTrue(expect.equals(result));
	}

	@Test
	public void cvtStringOk_3() throws RpcException {
		OtpErlangObject result = java2erlang("abc", "b");
		OtpErlangObject expect = new OtpErlangBinary("abc".getBytes());
		assertTrue(expect.equals(result));
	}

	@Test
	public void cvtStringOk_1() throws RpcException {
		OtpErlangObject result = java2erlang("a", "s");
		OtpErlangObject expect = new OtpErlangString("a");
		assertTrue(expect.equals(result));
	}

	@Test(expected = RpcException.class)
	public void cvtStringFail_1() throws RpcException {
		OtpErlangObject result = java2erlang(new String[] { "a" }, "li");
		OtpErlangObject expect = new OtpErlangList(
				new OtpErlangObject[] { new OtpErlangString("a") });
		assertTrue(expect.equals(result));
	}

	@SuppressWarnings("null")
	public void cvtTemplate() throws RpcException {
		OtpErlangObject result = java2erlang(null, "x");
		OtpErlangObject expect = null;
		assertTrue(expect.equals(result));
	}

}
