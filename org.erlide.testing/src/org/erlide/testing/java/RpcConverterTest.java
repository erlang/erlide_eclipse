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

import java.math.BigInteger;
import java.util.Arrays;

import org.erlide.jinterface.rpc.RpcConverter;
import org.erlide.jinterface.rpc.RpcException;
import org.junit.Test;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBigLong;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangDouble;
import com.ericsson.otp.erlang.OtpErlangFloat;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;

public class RpcConverterTest {

	public void test(Object o, String sig, OtpErlangObject expect)
			throws RpcException {
		OtpErlangObject result = java2erlang(o, sig);
		assertTrue(expect.equals(result));
	}

	@SuppressWarnings("boxing")
	@Test
	public void cvtIntegerOk_1() throws RpcException {
		test(397, "i", new OtpErlangLong(397));
	}

	@SuppressWarnings("boxing")
	@Test
	public void cvtIntegerOk_2() throws RpcException {
		test(3, "x", new OtpErlangLong(3));
	}

	@Test
	public void cvtIntegerOk_3() throws RpcException {
		BigInteger bigInteger = new BigInteger("39799999999999999999999", 10);
		test(bigInteger, "i", new OtpErlangBigLong(bigInteger));
	}

	@SuppressWarnings("boxing")
	@Test(expected = RpcException.class)
	public void cvtIntegerFail_1() throws RpcException {
		test(3, "s", new OtpErlangLong(3));
	}

	@Test
	public void cvtListOk_1() throws RpcException {
		test(new String[] { "a" }, "ls", new OtpErlangList(
				new OtpErlangObject[] { new OtpErlangString("a") }));
	}

	@SuppressWarnings("boxing")
	@Test
	public void cvtListOk_2() throws RpcException {
		test(new Object[] { "a", 35 }, "lx", new OtpErlangList(
				new OtpErlangObject[] { new OtpErlangString("a"),
						new OtpErlangLong(35) }));
	}

	@Test(expected = RpcException.class)
	public void cvtListFail_1() throws RpcException {
		test(new String[] { "a" }, "li", new OtpErlangList(
				new OtpErlangObject[] { new OtpErlangString("a") }));
	}

	@SuppressWarnings("boxing")
	@Test(expected = RpcException.class)
	public void cvtListFail_2() throws RpcException {
		test(new Object[] { "a", 35 }, "ls", new OtpErlangList(
				new OtpErlangObject[] { new OtpErlangString("a"),
						new OtpErlangLong(35) }));
	}

	@Test
	public void cvtStringOk_2() throws RpcException {
		test("astring", "s", new OtpErlangString("astring"));
	}

	@Test
	public void cvtStringOk_3() throws RpcException {
		test("astring", "b", new OtpErlangBinary("astring".getBytes()));
	}

	@Test
	public void cvtStringOk_1() throws RpcException {
		test("astring", "a", new OtpErlangAtom("astring"));
	}

	@Test(expected = RpcException.class)
	public void cvtStringFail_1() throws RpcException {
		test("astring", "p", new OtpErlangString("astring"));
	}

	@SuppressWarnings("boxing")
	@Test
	public void cvtFloatOk_1() throws RpcException {
		test(3.14f, "d", new OtpErlangFloat(3.14f));
	}

	@SuppressWarnings("boxing")
	@Test
	public void cvtDoubleOk_1() throws RpcException {
		test(3.14d, "d", new OtpErlangDouble(3.14d));
	}

	@Test
	public void parseSignature_1() throws RpcException {
		String sig = "aslsilpfd";
		String[] result = RpcConverter.parseSignature(sig, 7);
		String[] expect = new String[] { "a", "s", "ls", "i", "lp", "f", "d" };
		assertTrue(Arrays.equals(expect, result));
	}

	@Test
	public void parseSignature_2() throws RpcException {
		String sig = "llxi";
		String[] result = RpcConverter.parseSignature(sig, 2);
		String[] expect = new String[] { "llx", "i" };
		System.out.println(Arrays.toString(result));
		System.out.println(Arrays.toString(expect));
		assertTrue(Arrays.equals(expect, result));
	}

	@Test
	public void parseSignature_3() throws RpcException {
		String sig = "tax.d";
		String[] result = RpcConverter.parseSignature(sig, 2);
		String[] expect = new String[] { "tax", "d" };
		System.out.println(Arrays.toString(result));
		System.out.println(Arrays.toString(expect));
		assertTrue(Arrays.equals(expect, result));
	}

}
