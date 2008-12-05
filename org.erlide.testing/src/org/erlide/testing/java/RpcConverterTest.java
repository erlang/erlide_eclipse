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

import static org.junit.Assert.assertTrue;

import java.math.BigInteger;
import java.util.Arrays;

import org.erlide.jinterface.rpc.IConvertible;
import org.erlide.jinterface.rpc.RpcConverter;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.jinterface.rpc.Signature;
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

	private void test(Object o, String sig, OtpErlangObject expect)
			throws RpcException {
		OtpErlangObject result = RpcConverter.java2erlang(o, sig);
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
	public void cvtStringOk_1() throws RpcException {
		test("astring", "a", new OtpErlangAtom("astring"));
	}

	@Test
	public void cvtStringOk_2() throws RpcException {
		test("astring", "s", new OtpErlangString("astring"));
	}

	@Test
	public void cvtStringOk_4() throws RpcException {
		test("", "s", new OtpErlangString(""));
	}

	@Test
	public void cvtStringOk_3() throws RpcException {
		test("astring", "b", new OtpErlangBinary("astring".getBytes()));
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
	public void parseSignature_0() throws RpcException {
		String sig = null;
		Signature[] result = Signature.parse(sig);
		assertTrue(result == null);
	}

	@Test
	public void parseSignature_1() throws RpcException {
		String sig = "aslsilpfd";
		Signature[] result = Signature.parse(sig);
		String expect = "[a, s, l(s), i, l(p), f, d]";
		assertTrue(Arrays.toString(result).equals(expect));
	}

	@Test
	public void parseSignature_2() throws RpcException {
		String sig = "llxi";
		Signature[] result = Signature.parse(sig);
		String expect = "[l(l(x)), i]";
		assertTrue(Arrays.toString(result).equals(expect));
	}

	@Test
	public void parseSignature_3() throws RpcException {
		String sig = "2axd";
		Signature[] result = Signature.parse(sig);
		String expect = "[t(a,x), d]";
		assertTrue(Arrays.toString(result).equals(expect));
	}

	@Test
	public void parseSignature_4() throws RpcException {
		String sig = "l3axl2sad";
		Signature[] result = Signature.parse(sig);
		String expect = "[l(t(a,x,l(t(s,a)))), d]";
		assertTrue(Arrays.toString(result).equals(expect));
	}

	@Test
	public void parseSignature_5() throws RpcException {
		String sig = "32sadax";
		Signature[] result = Signature.parse(sig);
		String expect = "[t(t(s,a),d,a), x]";
		assertTrue(Arrays.toString(result).equals(expect));
	}

	@SuppressWarnings("boxing")
	@Test
	public void cvtBoolOk_1() throws RpcException {
		test(true, "o", new OtpErlangAtom("true"));
	}

	@SuppressWarnings("boxing")
	@Test
	public void cvtBoolOk_2() throws RpcException {
		test(false, "o", new OtpErlangAtom("false"));
	}

	@SuppressWarnings("boxing")
	@Test(expected = RpcException.class)
	public void cvtBoolFail_1() throws RpcException {
		test(true, "i", new OtpErlangAtom("true"));
	}

	@Test
	public void cvtConvertible_1() throws RpcException {
		IConvertible x = new IConvertible() {
			public OtpErlangObject toErlangObject() {
				return new OtpErlangAtom("__kalle__");
			}
		};
		test(x, "j", new OtpErlangAtom("__kalle__"));
	}

	static class Cvt {
		public static String fromErlangObject(OtpErlangObject obj) {
			return "hej";
		}
	}

	@Test
	public void cvtConvertible_2() throws RpcException {
		Object x = "hej";
		OtpErlangObject obj = new OtpErlangAtom("hej d�");
		Object y = RpcConverter.erlang2java(obj, Cvt.class);
		assertTrue(x.equals(y));
	}

}
