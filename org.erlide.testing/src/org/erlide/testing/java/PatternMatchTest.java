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

import junit.framework.Assert;

import org.erlide.jinterface.Bindings;
import org.erlide.jinterface.ErlUtils;
import org.erlide.jinterface.ParserException;
import org.erlide.jinterface.TermParser;
import org.junit.Test;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;

/**
 * 
 * 
 * @author Vlad Dumitrescu
 */
public class PatternMatchTest {

	@Test
	public void testFormatParser_simple_1() throws Exception {
		OtpErlangObject value = ErlUtils.format("~a", "hej");
		OtpErlangObject expected = ErlUtils.parse("hej");
		Assert.assertEquals(expected, value);
	}

	@Test
	public void testFormatParser_simple_2() throws Exception {
		OtpErlangObject value = ErlUtils.format("~s", "hej");
		OtpErlangObject expected = ErlUtils.parse("\"hej\"");
		Assert.assertEquals(expected, value);
	}

	@Test
	public void testFormatParser_simple_3() throws Exception {
		OtpErlangObject value = ErlUtils.format("~x");
		OtpErlangObject expected = ErlUtils.parse("~x");
		Assert.assertEquals(expected, value);
	}

	@Test
	public void testFormatParser_list() throws Exception {
		OtpErlangObject value = ErlUtils.format("[~a,2,~a]", "hej", "brr");
		OtpErlangObject expected = ErlUtils.parse("[hej,2,brr]");
		Assert.assertEquals(expected, value);
	}

	@Test
	public void testFormatParser_tuple() throws Exception {
		OtpErlangObject value = ErlUtils.format("{~a,2,~a}", "hej", "brr");
		OtpErlangObject expected = ErlUtils.parse("{hej,2,brr}");
		Assert.assertEquals(expected, value);
	}

	@Test
	public void testFormatParser_full() throws Exception {
		OtpErlangObject value = ErlUtils.format("[~a,{2,~a},5]", "hej", "brr");
		OtpErlangObject expected = ErlUtils.parse("[hej,{2,brr},5]");
		Assert.assertEquals(expected, value);
	}

	@Test
	public void testMatch_novar() throws Exception {
		OtpErlangObject p = ErlUtils.parse("[a, {b}]");
		OtpErlangObject t1 = ErlUtils.parse("[a, {b}]");
		Bindings r = ErlUtils.match(p, t1);
		Assert.assertNotNull(r);
	}

	@Test
	public void testMatch() throws Exception {
		Bindings r = ErlUtils.match("[W, V]", "[a, b]");
		Assert.assertEquals(r.get("W"), new OtpErlangAtom("a"));
		Assert.assertEquals(r.get("V"), new OtpErlangAtom("b"));
	}

	@Test
	public void testMatch_0() throws Exception {
		Bindings b = new Bindings();
		b.put("W", new OtpErlangAtom("a"));
		Bindings r = ErlUtils.match("[W, V]", "[a, b]", b);
		Assert.assertNotNull(r);
		Assert.assertEquals(r.get("V"), new OtpErlangAtom("b"));
	}

	@Test
	public void testMatch_1() throws Exception {
		Bindings r = ErlUtils.match("[W, V]", "[\"a\", {[1, 2]}]");
		Assert.assertEquals(r.get("W"), new OtpErlangString("a"));
		Assert.assertEquals(r.get("V"), ErlUtils.parse("{[1, 2]}"));
	}

	@Test
	public void testMatch_same() throws Exception {
		Bindings r = ErlUtils.match("[W, {V}]", "[a, {a}]");
		Assert.assertEquals(r.get("W"), new OtpErlangAtom("a"));
	}

	@Test
	public void testMatch_any() throws Exception {
		Bindings r = ErlUtils.match("[_, {_}]", "[a, {b}]");
		Assert.assertNotNull(r);
	}

	@Test
	public void testMatch_same_fail() throws Exception {
		Bindings r = ErlUtils.match("[W, {W}]", "[a, {b}]");
		Assert.assertNull(r);
	}

	@Test
	public void testMatch_sig_a() throws Exception {
		Bindings r = ErlUtils.match("W:a", "zzz");
		Assert.assertEquals(r.get("W"), new OtpErlangAtom("zzz"));
	}

	@Test
	public void testMatch_sig_i() throws Exception {
		Bindings r = ErlUtils.match("W:i", "222");
		Assert.assertEquals(r.get("W"), new OtpErlangLong(222));
	}

	@Test
	public void testMatch_sig_fail() throws Exception {
		Bindings r = ErlUtils.match("W:i", "zzz");
		Assert.assertNull(r);
	}

	@Test
	public void testMatch_ellipsis_1() throws Exception {
		OtpErlangObject r = TermParser.parse("[x|_]");
		Assert.assertNotNull(r);
	}

	@Test
	public void testMatch_ellipsis_2() throws Exception {
		Bindings r = ErlUtils.match("[X | T]", "[x,y,z]");
		Assert.assertNotNull(r);
		Assert.assertEquals(new OtpErlangAtom("x"), r.get("X"));
		Assert.assertEquals(TermParser.parse("[y,z]"), r.get("T"));
	}

	@Test()
	public void testMatch_ellipsis_4() throws Exception {
		Bindings r = ErlUtils.match("[X | y]", "[x,y,z]");
		Assert.assertNull(r);
	}

	@Test(expected = ParserException.class)
	public void testMatch_ellipsis_5() throws Exception {
		Bindings r = ErlUtils.match("[X | Y, Z]", "[x,y,z]");
	}

	@Test
	public void testMatch_t() throws Exception {
		Bindings r = ErlUtils.match("[W:a, V:i]", "[a, 1]");
		Assert.assertEquals(r.getAs("W", String.class), "a");
		Assert.assertEquals(r.getAs("V", Integer.class), Integer.valueOf(1));
	}

}
